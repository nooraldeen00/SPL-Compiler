package edu.uta.spl

abstract class TypeChecker {
  var trace_typecheck = false
  var st = new SymbolTable

  def expandType(tp: Type): Type
  def typecheck(e: Expr): Type
  def typecheck(e: Lvalue): Type
  def typecheck(e: Stmt, expected_type: Type): Unit
  def typecheck(e: Definition): Unit
  def typecheck(e: Program): Unit
}

class TypeCheck extends TypeChecker {
  def error(msg: String): Type = {
    System.err.println("*** Typechecking Error: " + msg)
    System.err.println("*** Symbol Table: " + st)
    System.exit(1)
    null
  }

  def expandType(tp: Type): Type = tp match {
    case NamedType(nm) =>
      st.lookup(nm) match {
        case Some(TypeDeclaration(t)) => expandType(t)
        case _ => error("Undeclared type: " + tp)
      }
    case _ => tp
  }

  def typeEquivalence(tp1: Type, tp2: Type): Boolean =
    if (tp1 == tp2 || tp1.isInstanceOf[AnyType] || tp2.isInstanceOf[AnyType])
      true
    else expandType(tp1) match {
      case ArrayType(t1) => expandType(tp2) match {
        case ArrayType(t2) => typeEquivalence(t1, t2)
        case _ => false
      }
      case RecordType(fs1) => expandType(tp2) match {
        case RecordType(fs2) =>
          fs1.length == fs2.length &&
            (fs1 zip fs2).forall { case (Bind(n1, t1), Bind(n2, t2)) =>
              n1 == n2 && typeEquivalence(t1, t2)
            }
        case _ => false
      }
      case TupleType(ts1) => expandType(tp2) match {
        case TupleType(ts2) =>
          ts1.length == ts2.length &&
            (ts1 zip ts2).forall { case (t1, t2) => typeEquivalence(t1, t2) }
        case _ => false
      }
      case _ => tp2 match {
        case NamedType(_) => typeEquivalence(tp1, expandType(tp2))
        case _ => false
      }
    }

  var level = -1

  def trace[T](e: Any, result: => T): T = {
    if (trace_typecheck) {
      level += 1
      println(" " * (3 * level) + "** " + e)
    }
    val res = result
    if (trace_typecheck) {
      print(" " * (3 * level))
      if (e.isInstanceOf[Stmt] || e.isInstanceOf[Definition]) println("->")
      else println("-> " + res)
      level -= 1
    }
    res
  }

  def typecheck(e: Expr): Type = trace(e, e match {
    case BinOpExp(op, l, r) =>
      val ltp = typecheck(l)
      val rtp = typecheck(r)
      if (!typeEquivalence(ltp, rtp))
        error("Incompatible types in binary operation: " + e)
      else if (op == "and" || op == "or") {
        if (typeEquivalence(ltp, BooleanType())) BooleanType()
        else error("AND/OR operation requires booleans: " + e)
      } else if (op == "eq" || op == "neq")
        BooleanType()
      else if (!typeEquivalence(ltp, IntType()) && !typeEquivalence(ltp, FloatType()))
        error("Arithmetic/comparison operations require numeric types: " + e)
      else if (List("gt", "lt", "geq", "leq").contains(op))
        BooleanType()
      else ltp

    case UnOpExp(op, operand) =>
      val t = typecheck(operand)
      op match {
        case "not" =>
          if (typeEquivalence(t, BooleanType())) BooleanType()
          else error("NOT requires boolean: " + operand)
        case "-" =>
          if (typeEquivalence(t, IntType()) || typeEquivalence(t, FloatType())) t
          else error("Unary - requires numeric type: " + operand)
        case _ => error("Unknown unary operator: " + op)
      }

    case IntConst(_)      => IntType()
    case FloatConst(_)    => FloatType()
    case BooleanConst(_)  => BooleanType()
    case StringConst(_)   => StringType()
    case NullExp()        => AnyType()
    case LvalExp(v)       => typecheck(v)

    case CallExp(name, args) =>
      val entry = st.lookup(name)
      if (entry == null) error("Undefined function: " + name)
      entry match {
        case Some(FuncDeclaration(outt, params, _, _, _)) =>
          if (params.length != args.length)
            error("Incorrect number of arguments for: " + name)
          val argTypes = args.map(typecheck)
          val paramTypes = params.map(_.value)
          if ((argTypes zip paramTypes).forall { case (a, p) => typeEquivalence(a, p) })
            outt
          else error("Argument type mismatch in call to: " + name)
        case _ => error("Expected a function declaration: " + name)

      }

    case ArrayExp(elements) =>
      if (elements.isEmpty)
        error("Cannot infer type of empty array literal")
      val types = elements.map(typecheck)
      val first = types.head
      if (!types.forall(t => typeEquivalence(t, first)))
        error("Array elements must all have the same type")
      ArrayType(first)

    case ArrayGen(length, value) =>
      if (!typeEquivalence(typecheck(length), IntType()))
        error("Array length must be an integer")
      ArrayType(typecheck(value))

    case TupleExp(elements) =>
      TupleType(elements.map(typecheck))

    case RecordExp(fields) =>
      RecordType(fields.map(f => Bind(f.name, typecheck(f.value))))

    case _ => throw new Error("Unknown expression: " + e)
  })

  def typecheck(e: Lvalue): Type = trace(e, e match {
    case Var(name) =>
      val entry = st.lookup(name)
      if (entry == null)
        error("Undefined variable: " + name)
      entry match {
        case Some(VarDeclaration(t, _, _)) => t
        case Some(_) => error(name + " is not a variable")
        case _ => error("Unexpected error in variable lookup: " + name)
      }


    case ArrayDeref(array, index) =>
      val atype = expandType(typecheck(array))
      val idxType = typecheck(index)
      if (!typeEquivalence(idxType, IntType()))
        error("Array index must be integer: " + index)
      atype match {
        case ArrayType(elemType) => elemType
        case _ => error("Attempt to index non-array: " + array)
      }

    case RecordDeref(record, attr) =>
      expandType(typecheck(record)) match {
        case RecordType(fields) =>
          fields.find(_.name == attr) match {
            case Some(Bind(_, t)) => t
            case None => error("Field " + attr + " not found")
          }
        case _ => error("Field access on non-record type")
      }

    case TupleDeref(tuple, index) =>
      expandType(typecheck(tuple)) match {
        case TupleType(types) =>
          if (index < 0 || index >= types.length)
            error("Tuple index out of bounds: " + index)
          types(index)
        case _ => error("Tuple deref on non-tuple value")
      }

    case _ => throw new Error("Invalid lvalue: " + e)
  })

  def typecheck(e: Stmt, expected_type: Type): Unit = trace(e, e match {
    case AssignSt(dst, src) =>
      if (!typeEquivalence(typecheck(dst), typecheck(src)))
        error("Assignment type mismatch")

    case ReturnValueSt(value) =>
      val t = typecheck(value)
      if (!typeEquivalence(t, expected_type))
        error("Incorrect return type")

    case ReturnSt() =>
      if (!typeEquivalence(expected_type, NoType()))
        error("Missing return value in non-void function")

    case IfSt(cond, thenp, elsep) =>
      if (!typeEquivalence(typecheck(cond), BooleanType()))
        error("If condition must be boolean")
      typecheck(thenp, expected_type)
      if (elsep != null)
        typecheck(elsep, expected_type)

    case WhileSt(cond, body) =>
      if (!typeEquivalence(typecheck(cond), BooleanType()))
        error("While condition must be boolean")
      typecheck(body, expected_type)

    case ForSt(v, init, step, limit, body) =>
      if (!typeEquivalence(typecheck(init), IntType()) ||
        !typeEquivalence(typecheck(step), IntType()) ||
        !typeEquivalence(typecheck(limit), IntType()))
        error("For loop expressions must be integers")
      st.begin_scope()
      st.insert(v, VarDeclaration(IntType(), 0, 0))
      typecheck(body, expected_type)
      st.end_scope()

    case BlockSt(decls, stmts) =>
      st.begin_scope()
      decls.foreach(typecheck)
      stmts.foreach(typecheck(_, expected_type))
      st.end_scope()

    case ReadSt(vars) =>
      vars.foreach { v =>
        val t = typecheck(v)
        if (!(typeEquivalence(t, IntType()) || typeEquivalence(t, FloatType()) || typeEquivalence(t, BooleanType())))
          error("Invalid type for read: " + t)
      }

    case PrintSt(exprs) =>
      exprs.foreach(typecheck)

    case CallSt(name, args) =>
      val entry = st.lookup(name)
      if (entry == null) error("Undefined procedure: " + name)
      entry match {
        case Some(FuncDeclaration(_, params, _, _, _)) =>
          if (params.length != args.length)
            error("Incorrect number of arguments for: " + name)
          val argTypes = args.map(typecheck)
          val paramTypes = params.map(_.value)
          if (!(argTypes zip paramTypes).forall { case (a, p) => typeEquivalence(a, p) })
            error("Argument type mismatch in call to: " + name)
        case _ => error("Expected procedure declaration for: " + name)
      }



    case LoopSt(body) =>
      typecheck(body, expected_type)

    case ExitSt() => ()

    case _ => throw new Error("Unknown statement: " + e)
  })

  def typecheck(e: Definition): Unit = trace(e, e match {
    case TypeDef(name, tp) =>
      st.insert(name, TypeDeclaration(tp))

    case VarDef(name, tp, value) =>
      val vtype = typecheck(value)
      val finalType = if (tp.isInstanceOf[AnyType]) vtype else tp
      if (!typeEquivalence(finalType, vtype))
        error("Variable declaration type mismatch: " + name)
      st.insert(name, VarDeclaration(finalType, 0, 0))

    case FuncDef(name, params, rettype, body) =>
      st.insert(name, FuncDeclaration(rettype, params, "", 0, 0))
      st.begin_scope()
      params.foreach { case Bind(v, tp) => st.insert(v, VarDeclaration(tp, 0, 0)) }
      typecheck(body, rettype)
      st.end_scope()

    case _ => throw new Error("Unknown definition: " + e)
  })

  def typecheck(e: Program): Unit = {
    typecheck(e.body, NoType())
  }
}
