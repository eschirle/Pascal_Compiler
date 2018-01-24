//  Pascal Compiler written by Evan Schirle


// From SBT: ~run-main Project


object Project {
      object MyParsersNoWhitespace {
        import fastparse.all._

        val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
        val integer : Parser[Expr] = digits.map (n => CstI (n))
        val negInt : Parser[Expr]  = ("(-"~digits~")").map(n => CstI(0-n))

        val keywords : List[String] = List ("begin", "end", "true", "false", "integer", "string", "real", "boolean", "longint", "char")
        val dataType : Parser[Typ] = P( ("integer" | "string" | "real" | "boolean" | "longint" | "char").!.map(s=>Typ(s)) )
        val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
        
        val ident : Parser[String] = P ((alpha ~ (alpha | "_" | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
        val variable : Parser[Var] = ident.map (s => Var (s))
        val string : Parser[Expr] = P ("'" ~ CharsWhile(_!=''', min=0).! ~"'").map(s => Str(s)) 

        val boolean : Parser[Expr] = P(("true" | "false").!).map(s=>Bool(s))
      }

//--------------------------//
//  PARSING SECTION         //
//--------------------------//
      object MyParsers {

        val White = fastparse.WhitespaceApi.Wrapper {
          import fastparse.all._
          NoTrace ((" "|"\n").rep)
        }

        import fastparse.noApi._
        import White._
        import MyParsersNoWhitespace._

        val prgmDecl : Parser[prgmDec] = P("program" ~ ident ~ ";").map(s=>prgmDec(s))

        val atom : Parser[Expr] = P (negInt | integer | functCall | variable)    //add solving expressions with function calls
        val parens : Parser[Expr] = P (atom 
                            | ("(" ~ addSub ~ ")") 
                            )
        val mult : Parser[Expr] = P (parens ~ (("*"|"/"|"%").! ~ parens).rep.map (s => s.toList)).map (foldAssocLeft)
        val addSub : Parser[Expr] = P (mult ~ (("+" | "-").! ~ mult).rep.map (s => s.toList)).map (foldAssocLeft)

        val equality : Parser[Expr] = P(addSub ~ ("=" | "<=" | ">=" | "<>" | "<" | ">").! ~ addSub).map{case (e1, op, e2) => Prim(op, e1, e2)}

        val expr : Parser[Expr] = P(addSub | boolean | equality | string | functCall | variable | atom)

    //Variable Declaration Parsing

        val varDeclarations : Parser[List[VarDec]] = P(("var" ~ (varDecAsgn | varDeclaration).rep.map(v=>v.toList)) |  "".!.map(s =>(List[VarDec]())) )

        val varDeclaration :  Parser[VarDec] = P (ident ~ ":" ~ dataType ~ ";").map( {case (s, t) => NonAsgn(s, t)} )
        val varDecAsgn :      Parser[VarDec] = P (ident ~ ":" ~ dataType ~ "=" ~ expr ~ ";").map( {case(s, t, e) => ValAsgn(s, t, e)} )

    //Statements. 

        val assignment : Parser[Stmt] = P (variable ~ ":=" ~ expr).map{case (v, e) => Asgn(v, e)}

        val printStmt : Parser[Stmt] = P("writeln("~(expr)~")").map(p => Print(p))   //print can print a list of things separated by ","

        val cond : Parser[Expr] = P ("(".? ~ (equality | boolean) ~ ")".?)
        
        val ifStmt : Parser[Stmt] = P("if" ~ cond ~ "then" ~ statement ~ "else" ~ statement).map{case (e, s1, s2) => IfElse (e, s1, s2)}

        val whileStmt : Parser[Stmt] = P("while" ~ cond ~ "do" ~ statement).map({case (e, s) => While(e, s)})

        val forStmt : Parser[Stmt] = P ("for" ~ "(".? ~ variable ~ ":=" ~ atom ~ "to" ~ atom ~ ")".? ~ "do" ~ statement).map{case(v, lo, hi, s) => For(v, lo, hi, s)}

        val blockStmt : Parser[Stmt] = P("begin" ~ (statement~";").rep.map(l => Block(l.toList)) ~ "end")

        val statement : Parser[Stmt] = P (assignment| printStmt | ifStmt | whileStmt | forStmt | blockStmt | procCall)

        val statements : Parser[List[Stmt]] = P( (statement ~ ";").rep(1).map(s=>s.toList))


    //PARSING FUNCTION DECLARATIONS & Calls

        val param   :  Parser[(Var, Typ)] = P (variable ~ ":" ~ dataType).map{ case(v, t) => (v, t)}
        val params  :  Parser[List[(Var, Typ)]] = P ( param.rep(sep=",").map(p => p.toList) )
        
        val args    :  Parser[List[Expr]]  = P ( arg.rep(sep = ",").map(a=>a.toList))
        val arg     :  Parser[Expr] = P (expr)

        //function declarations... no local variables. 
        val functDec : Parser[Func] = P ("function" ~ ident ~ "(" ~ params ~ ")" ~ ":" ~ dataType ~ ";" ~ statement ~ ";").map(makeFunc)
        val procDec  : Parser[Func] = P ("procedure"~ ident ~ "(" ~ params ~ ")" ~ ";" ~ statement ~ ";").map(makeProc)
        val functDecs: Parser[List[Func]] = P( (functDec | procDec).rep.map(s=>s.toList) )

        val functCall: Parser[Expr] = P (ident ~ "(" ~ args ~ ")").map(makeFuncCall)
        val procCall : Parser[Stmt] = P (ident ~ "(" ~ args ~ ")").map(makeProcCall)

    //Main parser for programs

        val mainBody : Parser[Stmt] = P ("begin" ~ statements ~ "end.").map(ss => Block(ss))

        val prgm : Parser[Program] = P(prgmDecl ~ functDecs ~ varDeclarations ~ mainBody).map{case (pd, fd, vd, body) => Program (pd, fd, vd, body) }

    }
    
//--------------------------//
//  Abstract Syntax         //
//--------------------------//
    sealed trait Expr
    case class CstI (n : Int)                               extends Expr
    case class Var (nm : String)                            extends Expr
    case class Prim (nm : String, e1 : Expr, e2 : Expr)     extends Expr
    case class Str (nm:String)                              extends Expr
    case class Bool (truthness:String)                      extends Expr
    case class FuncCall (nm: String, args: List[Expr])      extends Expr

    sealed trait Stmt
    case class Asgn (v: Var, e : Expr)                      extends Stmt
    case class Block (ss : List[Stmt])                      extends Stmt
    case class IfElse (e: Expr, s1 : Stmt, s2 : Stmt)       extends Stmt
    case class While (e: Expr, s : Stmt)                    extends Stmt
    case class For (v: Var, lo: Expr, hi: Expr, s: Stmt)    extends Stmt
    case class Print (e: Expr)                              extends Stmt
    //case class PrintLiteralString (s : String)              extends Stmt      
    case class ProcCall (nm: String, args: List[Expr])      extends Stmt

    sealed trait Func       //function and procedure definitions. 
    case class Procedure (nm: String, p: List[(Var, Typ)], body: Stmt)              extends Func        //can currently parse procedures but cannot compile. 
    case class Function (nm: String, p: List[(Var, Typ)], ret: Typ, body: Stmt)     extends Func

    sealed trait VarDec
    case class NonAsgn (nm: String, typ: Typ)               extends VarDec
    case class ValAsgn (nm: String, typ: Typ, e: Expr)      extends VarDec

    case class Typ(nm: String)

    case class prgmDec (nm:String)
    case class Program(pd: prgmDec, fd: List[Func], vd: List[VarDec], body: Stmt)


//functions for mapping stuff

    def makeFuncCall(p : (String, List[Expr])) : Expr = {
        p match{
            case (nm, Nil)  => FuncCall(nm, List[Expr]())
            case (nm, args) => FuncCall(nm, args)
        }
    }

    def makeProcCall(p : (String, List[Expr])) : Stmt = {
        p match{
            case (nm, Nil)  => ProcCall(nm, List[Expr]())
            case (nm, args) => ProcCall(nm, args)
        }
    }

    def makeFunc(p : (String, List[(Var, Typ)], Typ, Stmt)) : Func = {
        p match{
            case (nm, params, ret, body) => Function(nm, params, ret, body)
        }
    }

    def makeProc(p : (String, List[(Var, Typ)], Stmt)) : Func = {
        p match{
            case (nm, params, body) => Procedure(nm, params, body)
        }
    }

    def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
        p match {
            case (e1, Nil) => e1
            case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
        }
    }

//--------------------------//
//  Pretty printing         //
//--------------------------//

    def ppExpr (e : Expr) : String = {
        e match {
            case CstI (i)                     => i.toString
            case Var (x)                      => x
            case Prim (op, e1, e2)            => "(%s %s %s)".format (ppExpr (e1), op, ppExpr (e2))
            case FuncCall(nm, args)           => "%s(%s)".format(nm, ppArgs(args))
            case Str (s)                      => "\"%s\"".format(s)
            case Bool(s)                      => s
        }
    }

    def ppBlock (indent : String, s : Stmt) : String = {
        val newIndent = indent + "  "
        s match {
            case Block (ss) => {
                val sb = new StringBuilder
                for (s <- ss) {
                    sb.append (ppStmt (newIndent, s))
                }
                sb.toString
            }
            case _ => {
                "%s".format (ppStmt (newIndent, s))
            }
        }
    }

    def ppStmt (indent : String, s : Stmt) : String = {
        s match {
            case Asgn (v, e)           => 
                "%s%s := %s;\n".format (indent, ppExpr(v), ppExpr (e))
            case IfElse (e, s1, s2)         => 
                "%sif (%s) {\n%s%s} else {\n%s%s}\n".format (indent, ppExpr (e), ppBlock (indent, s1), indent, ppBlock (indent, s2), indent)
            case Block (ss) => {
                "%s{\n%s%s}\n".format (indent, ppBlock (indent, s), indent)
            }
            case For (v, low, high, s) => {
                "%sfor (%s := %s to %s) {\n%s%s}\n".format (indent, ppExpr(v), ppExpr (low), ppExpr (high), ppBlock (indent, s), indent)
            }
            case While (e, s)           => 
                "%swhile (%s) {\n%s%s}\n".format (indent, ppExpr (e), ppBlock (indent, s), indent)
            case Print (e)              => 
                "%sprint (%s);\n".format (indent, ppExpr (e))
            case ProcCall(nm, args)     =>
                "%s(%s)".format(nm, ppArgs(args))
        }
    }

    def ppPDec(pDec : prgmDec) : String = {
        pDec match{
            case prgmDec(nm) => nm
        }
    }

    def ppType(t : Typ) : String = {
        t match{
            case Typ(nm) => nm
        }
    }

    def ppParams(p : List[(Var, Typ)]) : String = {
        val sb = new StringBuilder
        
        for (param <- p){
            param match{
                case (v, t) =>  sb.append("%s %s,".format( ppType(t), ppExpr(v) ))
            }
        }
        sb.toString
    }

    def ppArgs(aa : List[Expr]) : String = {
        val sb = new StringBuilder
        for (a <- aa){
            sb.append(ppExpr(a))
        }
        sb.toString
    }

    def ppFDef(indent : String, f : Func) : String = {
        //val newIndent = indent + "  "
        val sb = new StringBuilder
        f match{
            case Function(nm, params, retTyp, body) => {
                sb.append("def %s(%s) returns %s:\n".format( nm, ppParams(params), ppType(retTyp) ) )
                sb.append("%s%s".format(indent, ppStmt(indent, body)))
            }
            case Procedure(nm, params, body)    =>{
                sb.append("def void %s(%s):\n".format( nm, ppParams(params) ) )
                sb.append("%s%s".format(indent, ppStmt(indent, body)))
            }
        }
        sb.toString
    }

    def ppVarDecs(indent : String, vd : List[VarDec]) : String = {
        val sb = new StringBuilder
        sb.append("var\n")
        for (v <- vd){
            v match{
                case NonAsgn(nm, t) => sb.append( "%s%s %s;\n".format(indent, ppType(t), nm) )
                case ValAsgn(nm, t, e) => sb.append( "%s%s %s = %s\n".format(indent, ppType(t), nm, ppExpr(e) ))
            }
        }
        sb.toString
    }

    def ppStmts(indent : String, ss : List[Stmt]) : String = {
        val sb = new StringBuilder        
        for (s <- ss){
            sb.append(ppStmt(indent, s))
        }
        sb.toString
    }

    def ppProgram(indent : String, p : Program) : String = {
        p match{
            case Program(pd, fd, vd, body) => {
                val sb = new StringBuilder
                sb.append("Program %s;\n".format(ppPDec(pd)))
                for (f <- fd){
                    sb.append("%s\n".format(ppFDef(indent, f)))
                }
                sb.append("%s".format(ppVarDecs(indent, vd)))
                sb.append("main \n%s\n".format(ppStmt(indent, body)))
                sb.toString
            }
        }
    }

//--------------------------//
//  Generate Assembly       //
//--------------------------//

    type Env = Map[String,String]
    type FuncEnv = Map[String,Func]

    val emptyEnv : Env = Map.empty
    val emptyFenv: FuncEnv = Map.empty


    var labelCounter : Int = 0
    def newLabel () : String = {
        labelCounter = labelCounter + 1
        "lab%03d".format (labelCounter)
    }

  
    //Generate assembly...
    def compileExpr(e : Expr, env : Env, fenv : FuncEnv) : String = {
        e match{
            case CstI(i)    => "\tpushq\t$%d\n".format(i)
            case Var(x)     =>
                env.get(x) match{
                    case None => throw new RuntimeException("unable to find variable %s in environment".format(x))
                    case Some (lab) =>
                        "\tpushq\t%s\n".format (lab)
                }
            case Prim(op, e1, e2) => {
                val insts1 = compileExpr(e1, env, fenv)
                val insts2 = compileExpr(e2, env, fenv)
                val push = "\tpushq\t%rax\n"
                def pop (reg : String) = "\tpopq\t%%%s\n".format(reg)
                val instsOp : String = op match {
                    case  "+" => "\taddq\t%rbx, %rax\n"
                    case  "-" => "\tsubq\t%rbx, %rax\n"
                    case  "*" => "\timulq\t%rbx, %rax\n"
                    case  "/" => {  //integer division. 
                        "\tmovq\t%rax, -8(%rbp)\n" +
                        "\tmovq\t%rbx, -4(%rbp)\n" +
                        "\tmovl\t-8(%rbp), %eax\n" +
                        "\tcltd\n" +
                        "\tidivl\t-4(%rbp)\n"
                    }
                    case  "=" => {
                        "\tcmpq\t%rbx, %rax\n" +
                        "\tsete\t%al\n" +
                        "\tmovzbl\t%al, %eax\n"
                    }
                    case "<" => {
                        "\tcmpq\t%rbx, %rax\n" +
                        "\tsets\t%al\n" +
                        "\tmovzbl\t%al, %eax\n"
                    }
                    case ">" => {
                        "\tcmpq\t%rax, %rbx\n" +
                        "\tsets\t%al\n" +
                        "\tmovzbl\t%al, %eax\n"
                    }
                    case ">=" => {
                        "\tcmpq\t%rbx, %rax\n" +
                        "\tsetge\t%al\n" +
                        "\tmovzbl\t%al, %eax\n"
                    }
                    case "<=" => {
                        "\tcmpq\t%rbx, %rax\n" +
                        "\tsetle\t%al\n" +
                        "\tmovzbl\t%al, %eax\n"
                    }
                    case "<>" => {
                        "\tcmpq\t%rbx, %rax\n" +
                        "\tsetne\t%al\n" +
                        "\tmovzbl\t%al, %eax\n"
                    }
                    case "%" => {
                        //remainder operator. 
                        "\tmovq\t%rbx, -8(%rbp)\n" +
                        "\tmovq\t%rax, -4(%rbp)\n" +
                        "\tmovl\t-8(%rbp), %eax\n" +
                        "\tcltd\n" +
                        "\tidivl\t-4(%rbp)\n" +
                        "\tmovl\t%edx, %eax\n" 
                    }
                    case   _ => throw new RuntimeException ("unknown primitive " + op)
                }
                "%s%s%s%s%s%s".format (insts1, insts2, pop ("rbx"), pop ("rax"), instsOp, push)
            }
            //function calls
            case FuncCall(nm, es) => {
                es.reverse.map (e => compileExpr (e, env, fenv)).mkString +
                "\tcall\t%s\n".format (nm) + 
                "\taddq\t$%d, %%rsp\n".format (es.length * 8) +
                "\tpushq\t%rax\n"
            }
            case Bool(s)    =>{
                throw new RuntimeException("cannot compile booleans")
            }
            case Str(s)     =>{
                throw new RuntimeException("cannot compile strings...")
            }
        }
    }

    def compileStmt(s : Stmt, env : Env, fenv : FuncEnv) : String = {
        s match {
            case Asgn(v, e)        =>  {
                env.get (v.nm) match {
                    case None => {
                        //if the variable hasn't been declared but is the same name as the function, it is the return value
                        if(getFuncNames(fenv).contains(v.nm)){
                            //add the return variable to env. 
                            val env2 : Env = env + ((v.nm, "(%s)".format(v.nm)))

                            ppStmt ("// ", s) + 
                            compileExpr (e, env2, fenv) +
                            "\tpopq\t%rax\n" +
                            "\tpopq\t%rbp\n" + 
                            "\tret\n"
                        }else{
                            throw new RuntimeException ("unable to find variable %s in environment".format (v.nm))
                        }
                    }
                    case Some (lab) => 
                        ppStmt ("// ", s) + 
                        compileExpr (e, env, fenv) + 
                        "\tpopq\t%rax\n" +
                        "\tmovq\t%%rax, %s\n".format (lab)
                }
            }
            case Block (ss)         => {
                def loop (ss2 : List[Stmt]) : String = {
                    ss2 match {
                        case Nil       => ""
                        case s2 :: ss3 => compileStmt (s2, env, fenv) + loop (ss3)
                    }
                }
                loop (ss)
            }

            case IfElse(e, s1, s2)  => {
                val label1 = newLabel ()
                val label2 = newLabel ()
                val label3 = newLabel ()
                "// %s\n".format (ppExpr (e)) + 
                compileExpr (e, env, fenv) +
                "\tpopq\t%rax\n" + 
                "\ttestq\t%rax, %rax\n" + 
                "\tjne\t%s\n".format (label1) +
                "\tjmp\t%s\n".format (label2) +
                "%s:\n".format (label1) +
                compileStmt (s1, env, fenv) +
                "\tjmp\t%s\n".format (label3) +
                "%s:\n".format (label2) +
                compileStmt (s2, env, fenv) +
                "%s:\n".format (label3) 
            }

            case While (e, s)       => {
                val label1 = newLabel ()
                val label2 = newLabel ()
                "// while (%s)\n".format (ppExpr (e)) +
                "\tjmp\t%s\n".format (label2) +
                "%s:\n".format (label1) +
                compileStmt (s, env, fenv) +
                "%s:\n".format (label2) +
                compileExpr (e, env, fenv) + 
                "\tpopq\t%rax\n" + 
                "\ttestq\t%rax, %rax\n" + 
                "\tjne\t%s\n".format (label1)
            }

            case For (v, low, high, s)  => {
                val label1 = newLabel ()
                val label2 = newLabel ()
                "// for (%s := %s to %s)\n".format (ppExpr(v), ppExpr (low), ppExpr (high)) +
                compileExpr (low, env, fenv) + 
                "\tpopq\t%rax\n" + 
                "\tmovq\t%%rax, (%s)\n".format (v.nm) +
                "\tjmp\t%s\n".format (label2) +
                "%s:\n".format (label1) +
                compileStmt (s, env, fenv) +
                "\tmovq\t(%s), %%rax\n".format (v.nm) +
                "\taddq\t$1, %rax\n" +
                "\tmovq\t%%rax, (%s)\n".format (v.nm) +
                "%s:\n".format (label2) +
                compileExpr (high, env, fenv) + 
                "\tpopq\t%rbx\n" + 
                "\tmovq\t(%s), %%rax\n".format (v.nm) +
                "\tcmpq\t%rbx, %rax\n" + 
                "\tjle\t%s\n".format (label1)
            }

            case Print (e)          => {
                ppStmt ("// ", s) + 
                compileExpr (e, env, fenv) +
                "\tpopq\t%rsi\n" +
                "\tmovl\t$.output, %edi\n" + 
                "\tmovl\t$0, %eax\n" +
                "\tcall\tprintf\n"
            }

            //Procedure Calls still need 
            case ProcCall(nm, args) => {
                //procedure call
                //args.reverse.map (e => compileExpr (e, env, fenv)).mkString +
                //"\tcall\t%s\n".format (nm) + 
                //"\taddq\t$%d, %%rsp\n".format (args.length * 8)
                throw new RuntimeException("cannot compile procedures")
            }
        }
    }

    def header () : String = {
        "\t.text\n" +
        "\t.globl\t%s\n".format ("main") +
        "\t.type\t%s, @function\n".format ("main") +
        "%s:\n".format ("main") + 
        "\tpushq\t%rbp\n" + 
        "\tmovq\t%rsp, %rbp\n" 
    }

    def footer (env : Env) : String = {
        "// footer" +
        "\n" +
        "\t.section .rodata\n" + 
        "\t.output:\n" + 
        "\t.string \"%d\\n\"\n" +
        "\n" +
        (for ((nm1, _) <- env) yield {
            "\t.globl\t%s\n".format (nm1) +
            "\t.data\n".format (nm1) +
            "\t.align\t8\n" +
            "\t.size\t%s, 8\n".format (nm1) +
            "%s:\n".format (nm1) +
            "\t.quad\t0\n" +
            "\n"
        }).mkString
    }

    def initVars (varDecs : List[VarDec]) : String = {
        (for (v <- varDecs) yield{
            v match{
                case NonAsgn(nm, typ)       => ""
                case ValAsgn(nm, typ, e)    => {
                    "// %s := %s\n".format(nm, ppExpr(e)) + 
                    compileExpr (e, emptyEnv, emptyFenv) + 
                    "\tpopq\t%rax\n" +
                    "\tmovq\t%%rax, (%s)\n".format (nm)
                }
            }
        }).mkString
    }

    def compileFunc (f : Function, env : Env, fenv : FuncEnv) : String = {
                val header = {
                    "\t.text\n" +
                    "\t.globl\t%s\n".format (f.nm) +
                    "\t.type\t%s, @function\n".format (f.nm) +
                    "%s:\n".format (f.nm) + 
                    "\tpushq\t%rbp\n" + 
                    "\tmovq\t%rsp, %rbp\n" 
                }
                val footer = {
                    "\tpopq\t%rbp\n" + 
                    "\tret\n"
                }
                var env2 : Env = env
                for (((v, t), i) <- f.p.zipWithIndex) {
                    env2 = env2 + ( (v.nm, "%d(%%rbp)".format ((i + 2) * 8)) ) 
                }
                header + 
                compileStmt (f.body, env2, fenv) + 
                footer           
    }

    def compileAll(prog: Program, env: Env, fenv : FuncEnv) : String = {
        header() + 
        initVars(prog.vd) +              //initialize global variables
        compileStmt(prog.body, env, fenv) + 
        //stuff for main method        
        "\tpopq\t%rbp\n" + 
        "\tret\n" +
        "\n" + 
        prog.fd.map(f => compileFunc (f.asInstanceOf[Function], env, fenv)).mkString("\n") +
        footer(env)
    }

    //get names of variables from var section
    def getName(v : VarDec) : String = {
        v match {
            case ValAsgn(nm, typ, e) => nm
            case NonAsgn(nm, typ) => nm
        }
    }

    def getFuncNames(fenv : FuncEnv) : List[String] = { 
        (for ((nm, _) <- fenv) yield{
            nm
        }).toList
    }


//import some stuff.
    import java.io.File
    import fastparse.all.{Parsed,Parser}

//File Reading
    def readFile (filename : String) : String = {
        val source : scala.io.BufferedSource = io.Source.fromFile (filename)
        try source.getLines.mkString ("\n") finally source.close ()
    }

//--------------------------//
//  Compile                 //
//--------------------------//

    def invokeAssemblerLinker(asmFilename : String) : Unit = {
        import scala.sys.process.{Process}
        val pb = Process (List ("gcc", "-o", asmFilename.replace (".s", ""), asmFilename))
        import scala.language.postfixOps
        val result : String = (pb !!)
        println ("Running assembler: %s".format (result))   
    }

    def compile (prog : Program, filename: String) : Unit = {
        val fenv : FuncEnv = (for (f <- prog.fd) yield (f.asInstanceOf[Function].nm, f) ).toMap
        val vars : List[String] = for(vdec <- prog.vd) yield { getName(vdec) }          //set up environment with declared global variables. 
        val env : Env = (for (v <- vars) yield (v, "(%s)".format (v))).toMap
        println ("Variables: %s".format (env.mkString (", ")))
        println ("Compiling:")
        val asm : String = compileAll (prog, env, fenv)
        val asmFilename = filename.replace (".p", ".s")
        val fw = new java.io.FileWriter (asmFilename)
        fw.write (asm)
        fw.close
        println ("Wrote to %s".format (asmFilename))
        invokeAssemblerLinker (asmFilename)
        // println (asm)
    }


//--------------------------//
//  TESTING                 //
//--------------------------//
    

//test compiling, assembly, file writing
    def testComp(p: Parser[Program], filename: String) : Unit = {
        val input : String = readFile (filename)
        val result : fastparse.core.Parsed[Program, Char, String] = p.parse (input.toLowerCase()) 
        result match {
            case Parsed.Success (prog, successIndex) => {
                println ("Successfully parsed file \"%s\".\nResult is %s.\nIndex is %d.".format (filename, prog, successIndex))
                //println ("Pretty printing:")
                //print (ppProgram ("  ", prog))
                compile (prog, filename)
            }
            case Parsed.Failure (lastParser, index, extra) => {
                println ("Failed to parse file \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (filename, lastParser, index, extra))
            }
        }
    }


// MAIN

    // Test the compiler using testComp ,which takes a Parser and a Pascal file as its parameters. 

    def main (args : Array[String]) {
        println ("=" * 80)

        testComp(MyParsers.prgm, "input/test.p")
        println ("=" * 80)

        testComp(MyParsers.prgm, "input/test2.p")
        println ("=" * 80)

        testComp(MyParsers.prgm, "input/print1-100.p")
        println ("=" * 80)

        testComp(MyParsers.prgm, "input/fact1-20.p")
        println ("=" * 80)

        testComp(MyParsers.prgm, "input/notDiv3-5.p")
        println ("=" * 80)
    }

}
