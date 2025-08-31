package edu.uta.spl

abstract class CodeGenerator(tc: TypeChecker) {
  def typechecker: TypeChecker = tc
  def st: SymbolTable = tc.st
  def code(e: Program): IRstmt
  def allocate_variable(name: String, var_type: Type, fname: String): IRexp
}

class Code(tc: TypeChecker) extends CodeGenerator(tc) {
  var name_counter = 0

  def new_name(name: String): String = {
    name_counter += 1
    name + "_" + name_counter
  }

  var addedCode: List[IRstmt] = Nil

  def addCode(code: IRstmt*): Unit = {
    addedCode ++= code
  }

  def allocate_variable(name: String, var_type: Type, fname: String): IRexp = {
    val entry = st.lookup(fname)
    if (entry == null) throw new Error("No current function: " + fname)
    entry match {
      case Some(FuncDeclaration(rtp, params, label, level, min_offset)) =>
        st.insert(name, VarDeclaration(var_type, level, min_offset))
        st.replace(fname, FuncDeclaration(rtp, params, label, level, min_offset - 4))
        Mem(Binop("PLUS", Reg("fp"), IntValue(min_offset)))
      case _ => throw new Error("Expected FuncDeclaration for function: " + fname)
    }
  }

  def access_variable(name: String, level: Int): IRexp = {
    val entry = st.lookup(name)
    if (entry == null) throw new Error("Undefined variable: " + name)
    entry match {
      case Some(VarDeclaration(_, var_level, offset)) =>
        var res: IRexp = Reg("fp")
        for (_ <- var_level + 1 to level)
          res = Mem(Binop("PLUS", res, IntValue(-8)))
        Mem(Binop("PLUS", res, IntValue(offset)))
      case _ => throw new Error("Expected VarDeclaration for variable: " + name)
    }
  }

  def code(e: Expr, level: Int, fname: String): IRexp = e match {
    case IntConst(n) => IntValue(n)
    case FloatConst(f) => FloatValue(f)
    case StringConst(s) => StringValue(s)
    case BooleanConst(b) => IntValue(if (b) 1 else 0)
    case NullExp() => IntValue(0)
    case LvalExp(v) => code(v, level, fname)
    case BinOpExp(op, l, r) => Binop(op.toUpperCase, code(l, level, fname), code(r, level, fname))
    case UnOpExp(op, e) =>
      val ce = code(e, level, fname)
      op match {
        case "-" => Binop("MINUS", IntValue(0), ce)
        case "not" => Binop("EQ", ce, IntValue(0))
        case _ => throw new Error("Unknown unary op: " + op)
      }
    case CallExp(name, args) =>
      val entry = st.lookup(name)
      if (entry == null) throw new Error("Unknown function: " + name)
      entry match {
        case Some(FuncDeclaration(_, _, _, func_level, _)) =>
          val sl =
            if (func_level == level + 1) Reg("fp")
            else {
              var sl_exp: IRexp = Reg("fp")
              for (_ <- 0 to (level - func_level))
                sl_exp = Mem(Binop("PLUS", sl_exp, IntValue(-8)))
              sl_exp
            }
          Call(name, sl, args.map(code(_, level, fname)))
        case _ => throw new Error("Expected FuncDeclaration for: " + name)
      }
    case ArrayGen(len, v) =>
      val A = allocate_variable(new_name("A"), typechecker.typecheck(e), fname)
      val L = allocate_variable(new_name("L"), IntType(), fname)
      val V = allocate_variable(new_name("V"), typechecker.typecheck(v), fname)
      val I = allocate_variable(new_name("I"), IntType(), fname)
      val loop = new_name("loop")
      val exit = new_name("exit")
      ESeq(Seq(List(
        Move(L, code(len, level, fname)),
        Move(A, Allocate(Binop("PLUS", L, IntValue(1)))),
        Move(V, code(v, level, fname)),
        Move(Mem(A), L),
        Move(I, IntValue(0)),
        Label(loop),
        CJump(Binop("GEQ", I, L), exit),
        Move(Mem(Binop("PLUS", A, Binop("TIMES", Binop("PLUS", I, IntValue(1)), IntValue(4)))), V),
        Move(I, Binop("PLUS", I, IntValue(1))),
        Jump(loop),
        Label(exit)
      )), A)
    case RecordExp(fields) =>
      val r = allocate_variable(new_name("record"), typechecker.typecheck(e), fname)
      val assigns = fields.zipWithIndex.map {
        case (Bind(_, expr), i) =>
          Move(Mem(Binop("PLUS", r, IntValue(i * 4))), code(expr, level, fname))
      }
      ESeq(Seq(assigns), r)
    case TupleExp(elems) =>
      val r = allocate_variable(new_name("tuple"), typechecker.typecheck(e), fname)
      val assigns = elems.zipWithIndex.map {
        case (expr, i) => Move(Mem(Binop("PLUS", r, IntValue(i * 4))), code(expr, level, fname))
      }
      ESeq(Seq(assigns), r)
    case ArrayExp(elems) =>
      val r = allocate_variable(new_name("array"), typechecker.typecheck(e), fname)
      val init = List(Move(Mem(r), IntValue(elems.length)))
      val assigns = elems.zipWithIndex.map {
        case (expr, i) => Move(Mem(Binop("PLUS", r, IntValue((i + 1) * 4))), code(expr, level, fname))
      }
      ESeq(Seq(init ++ assigns), r)
  }

  def code(e: Lvalue, level: Int, fname: String): IRexp = e match {
    case Var(x) => access_variable(x, level)
    case ArrayDeref(arr, idx) =>
      val a = code(arr, level, fname)
      val i = code(idx, level, fname)
      Mem(Binop("PLUS", a, Binop("TIMES", Binop("PLUS", i, IntValue(1)), IntValue(4))))
    case RecordDeref(r, a) =>
      val cr = code(r, level, fname)
      typechecker.expandType(typechecker.typecheck(r)) match {
        case RecordType(cl) =>
          val i = cl.map(_.name).indexOf(a)
          Mem(Binop("PLUS", cr, IntValue(i * 4)))
        case _ => throw new Error("Unknown record: " + e)
      }
    case TupleDeref(tuple, i) =>
      val base = code(tuple, level, fname)
      Mem(Binop("PLUS", base, IntValue(i * 4)))
  }



def code(e: Stmt, level: Int, fname: String, exit_label: String): IRstmt = e match {
    case AssignSt(dst, src) =>
      Move(code(dst, level, fname), code(src, level, fname))

    case ForSt(variable, initial, step, limit, body) =>
      val loop = new_name("loop")
      val exit = new_name("exit")
      val v = allocate_variable(variable, IntType(), fname)
      Seq(List(
        Move(v, code(initial, level, fname)),
        Label(loop),
        CJump(Binop("GT", v, code(limit, level, fname)), exit),
        code(body, level, fname, exit),
        Move(v, Binop("PLUS", v, code(step, level, fname))),
        Jump(loop),
        Label(exit)
      ))

    case WhileSt(cond, body) =>
      val loop = new_name("loop")
      val exit = new_name("exit")
      Seq(List(
        Label(loop),
        CJump(Binop("EQ", code(cond, level, fname), IntValue(0)), exit),
        code(body, level, fname, exit),
        Jump(loop),
        Label(exit)
      ))

    case IfSt(cond, thenStmt, elseStmt) =>
      val exit = new_name("cont")
      val alt = new_name("exit")
      Seq(List(
        CJump(Binop("EQ", code(cond, level, fname), IntValue(0)), alt),
        code(thenStmt, level, fname, exit),
        Jump(exit),
        Label(alt),
        code(elseStmt, level, fname, exit),
        Label(exit)
      ))

    case ReturnValueSt(e) =>
      Seq(List(
        Move(Reg("a0"), code(e, level, fname)),
        Move(Reg("ra"), Mem(Binop("PLUS", Reg("fp"), IntValue(-4)))),
        Move(Reg("sp"), Reg("fp")),
        Move(Reg("fp"), Mem(Reg("fp"))),
        Return()
      ))

    case ReturnSt() =>
      Seq(List(
        Move(Reg("ra"), Mem(Binop("PLUS", Reg("fp"), IntValue(-4)))),
        Move(Reg("sp"), Reg("fp")),
        Move(Reg("fp"), Mem(Reg("fp"))),
        Return()
      ))

    case ExitSt() => Jump(exit_label)

    case PrintSt(exprs) =>
      Seq(exprs.flatMap {
        case StringConst(s) => List(SystemCall("WRITE_STRING", StringValue(s)))
        case e => List(SystemCall("WRITE_INT", code(e, level, fname)))
      } :+ SystemCall("WRITE_STRING", StringValue("\n")))

    case CallSt(name, args) =>
      val entry = st.lookup(name)
      if (entry == null) throw new Error("Unknown procedure: " + name)
      entry match {
        case Some(FuncDeclaration(_, _, _, func_level, _)) =>
          val sl =
            if (func_level == level + 1) Reg("fp")
            else {
              var sl_exp: IRexp = Reg("fp")
              for (_ <- 0 to (level - func_level))
                sl_exp = Mem(Binop("PLUS", sl_exp, IntValue(-8)))
              sl_exp
            }
          CallP(name, sl, args.map(code(_, level, fname)))
        case _ => throw new Error("Expected FuncDeclaration for procedure: " + name)
      }


    case ReadSt(args) =>
      Seq(args.map { lval => Move(code(lval, level, fname), Reg("a0")) } ++
        args.map { _ => SystemCall("READ_INT", Reg("a0")) })

    case BlockSt(decls, stmts) =>
      st.begin_scope()
      val dcode = decls.map(code(_, fname, level))
      val scode = stmts.map(code(_, level, fname, exit_label))
      st.end_scope()
      Seq(dcode ++ scode)
  }

  def code(e: Definition, fname: String, level: Int): IRstmt = e match {
    case VarDef(name, typ, expr) =>
      val actualType = if (typ.isInstanceOf[AnyType]) typechecker.typecheck(expr) else typ
      val v = allocate_variable(name, actualType, fname)
      Move(v, code(expr, level, fname))


    case FuncDef(f, ps, ot, b) =>
      val flabel = if (f == "main") f else new_name(f)
      st.insert(f, FuncDeclaration(ot, ps, flabel, level + 1, -12))
      st.begin_scope()
      ps.zipWithIndex.foreach {
        case (Bind(v, tp), i) => st.insert(v, VarDeclaration(tp, level + 1, (ps.length - i) * 4))
      }
      val body = code(b, level + 1, f, "")
      st.end_scope()
      val entry = st.lookup(f)
      if (entry == null) throw new Error("Unknown function: " + f)
      entry match {
        case Some(FuncDeclaration(_, _, _, _, offset)) =>
          addCode(Label(flabel),
            Move(Mem(Reg("sp")), Reg("fp")),
            Move(Reg("fp"), Reg("sp")),
            Move(Mem(Binop("PLUS", Reg("fp"), IntValue(-4))), Reg("ra")),
            Move(Mem(Binop("PLUS", Reg("fp"), IntValue(-8))), Reg("v0")),
            Move(Reg("sp"), Binop("PLUS", Reg("sp"), IntValue(offset))),
            body,
            Move(Reg("ra"), Mem(Binop("PLUS", Reg("fp"), IntValue(-4)))),
            Move(Reg("sp"), Reg("fp")),
            Move(Reg("fp"), Mem(Reg("fp"))),
            Return())
          Seq(List())
        case _ => throw new Error("Expected FuncDeclaration after insertion: " + f)
      }
    case TypeDef(name, tp) =>
      st.insert(name, TypeDeclaration(tp))
      Seq(List())
  }

  def code(e: Program): IRstmt = e match {
    case Program(BlockSt(decls, stmts)) =>
      st.begin_scope()


      decls.foreach {
        case TypeDef(name, tp) => st.insert(name, TypeDeclaration(tp))
        case _ => ()
      }


      val res = code(FuncDef("main", List(), NoType(), BlockSt(decls, stmts)), "", 0)

      st.end_scope()
      Seq(res :: addedCode)
  }




}
