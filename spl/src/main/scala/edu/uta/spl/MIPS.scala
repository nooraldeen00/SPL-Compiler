package edu.uta.spl

case class Register(reg: String) {
  override def toString: String = reg
}

class RegisterPool {
  val all_registers = List(
    "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9",
    "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"
  )

  var available_registers: List[Register] = all_registers.map(Register)

  def is_temporary(reg: Register): Boolean =
    reg match { case Register(n) => all_registers.contains(n) }

  def get(): Register =
    available_registers match {
      case reg :: rs =>
        available_registers = rs
        reg
      case _ => throw new Error("*** Run out of registers")
    }

  def recycle(reg: Register) {
    if (available_registers.contains(reg))
      throw new Error("*** Register has already been recycled: " + reg)
    if (is_temporary(reg))
      available_registers = reg :: available_registers
  }

  def used(): List[Register] = {
    for (reg <- all_registers if !available_registers.contains(Register(reg)))
      yield Register(reg)
  }
}

abstract class MipsGenerator {
  def clear()
  def emit(e: IRstmt)
  def initialCode()
}

class Mips extends MipsGenerator {
  def mips_label(s: String) {
    SPL.out.println(s + ":")
  }

  def mips(op: String) {
    SPL.out.println("        " + op)
  }

  def mips(op: String, args: String) {
    SPL.out.print("        " + op)
    for (_ <- op.length to 10)
      SPL.out.print(" ")
    SPL.out.println(args)
  }

  var rpool = new RegisterPool

  def clear {
    rpool = new RegisterPool
  }

  var name_counter = 0

  def new_label(): String = {
    name_counter += 1
    "L_" + name_counter
  }

  def emit(e: IRexp): Register = e match {
    case IntValue(n) =>
      val reg = rpool.get()
      mips("li", reg + ", " + n)
      reg

    case FloatValue(f) =>
      val reg = rpool.get()
      mips("li", reg + ", " + f.toInt)
      reg

    case StringValue(s) =>
      val label = "S_" + Math.abs(s.hashCode)
      mips(".data")
      mips_label(label)
      mips(".asciiz", "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"")
      mips(".text")
      val reg = rpool.get()
      mips("la", reg + ", " + label)
      reg

    case Reg(r) =>
      Register("$" + r)

    case Mem(Binop("PLUS", Reg(r), IntValue(n))) =>
      val reg = rpool.get()
      mips("lw", reg + ", " + n + "($" + r + ")")
      reg

    case Mem(e) =>
      val addr = emit(e)
      val reg = rpool.get()
      mips("lw", reg + ", 0(" + addr + ")")
      rpool.recycle(addr)
      reg

    case Binop(op, l, r) =>
      val lreg = emit(l)
      val rreg = emit(r)
      val res = rpool.get()
      op match {
        case "PLUS" => mips("addu", res + ", " + lreg + ", " + rreg)
        case "MINUS" => mips("subu", res + ", " + lreg + ", " + rreg)
        case "TIMES" => mips("mul", res + ", " + lreg + ", " + rreg)
        case "DIV" => mips("div", lreg + ", " + rreg); mips("mflo", res.toString)
        case "MOD" => mips("div", lreg + ", " + rreg); mips("mfhi", res.toString)
        case "AND" =>
          val label = new_label()
          mips("beq", lreg + ", $zero, " + label)
          mips("move", res + ", " + rreg)
          mips_label(label)
        case "OR" =>
          mips("or", res + ", " + lreg + ", " + rreg)
        case "EQ" => mips("seq", res + ", " + lreg + ", " + rreg)
        case "NEQ" => mips("sne", res + ", " + lreg + ", " + rreg)
        case "LT" => mips("slt", res + ", " + lreg + ", " + rreg)
        case "LEQ" =>
          mips("slt", res + ", " + rreg + ", " + lreg)
          mips("xori", res + ", " + res + ", 1")
        case "GT" => mips("slt", res + ", " + rreg + ", " + lreg)
        case "GEQ" =>
          mips("slt", res + ", " + lreg + ", " + rreg)
          mips("xori", res + ", " + res + ", 1")
        case _ => throw new Error("*** Unknown binary operator: " + op)
      }
      rpool.recycle(lreg)
      rpool.recycle(rreg)
      res

    case Call(f, sl, args) =>
      val used_regs = rpool.used()
      val size = (used_regs.length + args.length) * 4
      if (size > 0)
        mips("subu", "$sp, $sp, " + size)
      var i = size
      for (r <- used_regs) {
        mips("sw", r + ", " + i + "($sp)")
        i -= 4
      }
      i = args.length * 4
      for (a <- args) {
        val reg = emit(a)
        mips("sw", reg + ", " + i + "($sp)")
        rpool.recycle(reg)
        i -= 4
      }
      val sreg = emit(sl)
      mips("move", "$v0, " + sreg)
      rpool.recycle(sreg)
      mips("jal", f)
      i = size
      for (r <- used_regs) {
        mips("lw", r + ", " + i + "($sp)")
        i -= 4
      }
      if (size > 0)
        mips("addu", "$sp, $sp, " + size)
      val res = rpool.get()
      mips("move", res + ", $a0")
      res
  }

  def emit(e: IRstmt) {
    e match {
      case Move(Mem(Binop("PLUS", Reg(r), IntValue(n))), u) =>
        val src = emit(u)
        mips("sw", src + ", " + n + "($" + r + ")")
        rpool.recycle(src)

      case Move(Mem(addr), src) =>
        val areg = emit(addr)
        val sreg = emit(src)
        mips("sw", sreg + ", 0(" + areg + ")")
        rpool.recycle(areg)
        rpool.recycle(sreg)

      case Move(dst, src) =>
        val dreg = emit(dst)
        val sreg = emit(src)
        mips("move", dreg + ", " + sreg)
        rpool.recycle(dreg)
        rpool.recycle(sreg)

      case Label(name) =>
        mips_label(name)

      case Jump(label) =>
        mips("j", label)

      case CJump(cond, label) =>
        val reg = emit(cond)
        mips("beq", reg + ", $zero, " + label)
        rpool.recycle(reg)

      case CallP(f, sl, args) =>
        val used_regs = rpool.used()
        val size = (used_regs.length + args.length) * 4
        if (size > 0)
          mips("subu", "$sp, $sp, " + size)
        var i = size
        for (r <- used_regs) {
          mips("sw", r + ", " + i + "($sp)")
          i -= 4
        }
        i = args.length * 4
        for (a <- args) {
          val reg = emit(a)
          mips("sw", reg + ", " + i + "($sp)")
          rpool.recycle(reg)
          i -= 4
        }
        val sreg = emit(sl)
        mips("move", "$v0, " + sreg)
        rpool.recycle(sreg)
        mips("jal", f)
        i = size
        for (r <- used_regs) {
          mips("lw", r + ", " + i + "($sp)")
          i -= 4
        }
        if (size > 0)
          mips("addu", "$sp, $sp, " + size)

      case SystemCall(op, arg) =>
        val reg = emit(arg)
        op match {
          case "WRITE_INT" =>
            mips("move", "$a0, " + reg)
            mips("li", "$v0, 1")
            mips("syscall")
          case "WRITE_STRING" =>
            mips("move", "$a0, " + reg)
            mips("li", "$v0, 4")
            mips("syscall")
          case "READ_INT" =>
            mips("li", "$v0, 5")
            mips("syscall")
            mips("move", reg + ", $v0")
        }
        rpool.recycle(reg)

      case Return() =>
        mips("jr", "$ra")

      case Seq(stmts) =>
        stmts.foreach(emit)
    }
  }

  def initialCode() {
    mips(".globl", "main")
    mips(".data")
    mips_label("ENDL_")
    mips(".asciiz", "\"\\n\"")
    mips(".text")
  }
}
