# 🖥️ SPL Compiler
<img width="300" height="300" alt="spl" src="https://github.com/user-attachments/assets/4e7a7ab8-36f9-4a72-9ea2-32ec93a8ad68" />

A complete compiler for SPL (Simple Programming Language) built step-by-step through multiple assignments.
The project demonstrates the full compilation pipeline: lexing → parsing → AST construction → type checking → intermediate representation → MIPS code generation.

# 📚 Assignments Completed

## 1️⃣ Scanner (Lexical Analysis)

- Implemented a scanner in JFlex (spl.lex) to tokenize SPL source code.

- Recognizes keywords, identifiers, literals, operators, and delimiters.

- Provides detailed error handling with line and column reporting.

- Verified against provided test suite (tests/*.spl) using:

   scala lib/spl.jar 1 tests/hello.spl

- Compared results with reference implementation (spl-solution.jar).

## 2️⃣ Parser and AST (Syntactic Analysis)

- Extended CUP grammar (spl.cup) to construct Abstract Syntax Trees (ASTs).

- AST nodes defined in AST.scala (e.g., BinOpExp, LvalExp, Program, BlockSt).

- Integrated semantic actions into grammar rules to build trees (e.g. expr ::= expr PLUS expr).

- Implemented utility functions to manage lists (append, add) and optional values (AnyType).

- Stored the top-level AST with SPL.setAST(new Program(...)).

- Validated ASTs against the solution output:

   scala lib/spl.jar 2 tests/hello.spl
   scala spl-solution.jar 2 tests/hello.spl

   scala lib/spl.jar 3 tests/hello.spl
   scala spl-solution.jar 3 tests/hello.spl

## 3️⃣ Type Checking (Static Semantics)

- Implemented type rules in TypeCheck.scala using a SymbolTable of Declarations:

- VarDeclaration, TypeDeclaration, FuncDeclaration.

- Enforced name equivalence, scope rules, and type safety.

- Disallowed mixed-type operations (e.g., int + float).

- Added support for scoped variables in for-loops.

- Tested error cases (undeclared variables, wrong argument counts, invalid type ops).

- Compared tracing and results with the solution:

   scala lib/spl.jar 4 tests/hello.spl
   scala spl-solution.jar 4 tests/hello.spl

  ## 4️⃣ Intermediate Representation (IR Generation)

- Added IR generation in Code.scala, targeting SPL ASTs.

- Defined IR nodes in IR.scala such as:

   Seq, Move, Mem, Binop, CJump, Label, Jump, SystemCall.

- Modeled heap structures for arrays, records, tuples, and strings.

- Implemented stack allocation for local/global variables.

- Added IR support for:

   Loops (while, for) → labeled jumps + conditional branches

   Function calls/procedures with proper static link resolution

   System calls for I/O (READ_INT, WRITE_INT, WRITE_STRING)

- Verified IR output:

   scala lib/spl.jar 5 tests/hello.spl
   scala spl-solution.jar 5 tests/hello.spl

## 5️⃣ MIPS Code Generation (Backend)

- Implemented MIPS code emission in MIPS.scala + Code.scala.

- Lowered IR trees into executable MIPS assembly.

- Supported:

   Stack frames with fp, sp, and return conventions

   Heap allocation via gp (arrays, records, tuples)

   Register usage conventions (ra, v0, a0)

   Correct call/return protocol for nested procedures

- Output runs on SPIM / MARS simulators.


# 🛠️ Build & Run

## Build with Maven

mvn clean install

# Run Compiler Stages

## Scanner
   
   scala lib/spl.jar 1 tests/hello.spl

## Parser
   
   scala lib/spl.jar 2 tests/hello.spl
   
## AST
   
   scala lib/spl.jar 3 tests/hello.spl

## Type Checking
   
   scala lib/spl.jar 4 tests/hello.spl

## IR
   
   scala lib/spl.jar 5 tests/hello.spl

## MIPS Code Generation

   scala lib/spl.jar 6 tests/hello.spl -o hello.s


# 🧠 Learning Outcomes

Through these assignments I learned:

🔹 How compilers are structured: front-end, middle-end, back-end

🔹 Implementing a scanner with JFlex and parser with CUP

🔹 Building and traversing ASTs

🔹 Designing a type system and enforcing static semantics

🔹 Lowering to Intermediate Representation (IR)

🔹 Emitting real MIPS assembly and managing low-level details like frames, registers, and heap layout

🔹 Debugging using unit tests, solution comparisons, and GDB-style tracing

# 🔮 Future Work

- Control flow graphs + optimization passes (constant folding, dead code elimination)

- Register allocation strategies

- Extended language features (pattern matching, advanced types)

- Richer error messages and IDE integration

# ⚡ 
This SPL project demonstrates a full compiler pipeline in action — from raw text to executable assembly. It showcases my systems programming, compiler theory, and debugging skills across multiple stages of compiler construction.
  
