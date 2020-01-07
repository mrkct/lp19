import scala.io.Source;
import scala.util.parsing.combinator._;
import scala.collection.mutable.Map;



class ArnoldC extends JavaTokenParsers {

    final val Start = "IT'S SHOWTIME"
    final val End = "YOU HAVE BEEN TERMINATED"
    final val Print = "TALK TO THE HAND"
    final val DeclStart = "HEY CHRISTMAS TREE"
    final val DeclEnd = "YOU SET US UP"
    final val AssignStart = "GET TO THE CHOPPER"
    final val AssignMid = "HERE IS MY INVITATION"
    final val AssignEnd = "ENOUGH TALK"
    final val Branch = "BECAUSE I'M GOING TO SAY PLEASE"
    final val Else = "BULLSHIT"
    final val BranchEnd = "YOU HAVE NO RESPECT FOR LOGIC"
    final val While = "STICK AROUND"
    final val EndWhile = "CHILL"

    final val Plus = "GET UP"
    final val Minus = "GET DOWN"
    final val Multiply = "YOU'RE FIRED"
    final val Divide = "HE HAD TO SPLIT"
    final val Equal = "YOU ARE NOT YOU YOU ARE ME"
    final val Greater = "LET OFF SOME STEAM BENNET"
    final val Or = "CONSIDER THAT A DIVORCE"
    final val And = "KNOCK KNOCK"

    sealed trait Statement
    sealed trait EvalsToInt

    case class VNumber(x: Double) extends Statement with EvalsToInt
    case class VToken(x: String) extends Statement with EvalsToInt
    case class VString(x: String) extends Statement

    case class SPrint(x: Statement) extends Statement
    case class SAssign(variable: VToken, value: Statement with EvalsToInt) extends Statement
    case class SEvaluate(operations: List[Any]) extends Statement with EvalsToInt
    case class SBranch(condition: VToken, branchTrue: List[Statement], branchFalse: List[Statement]) extends Statement
    case class SWhile(condition: VToken, block: List[Statement]) extends Statement

    def program = Start ~> rep(statement) <~ End;
    def statement: Parser[Statement] = sPrint | sDeclaration | sAssignment | sConditional | sLoop;

    def sPrint = Print ~> token ^^ {x => SPrint(x)}
    def sDeclaration = (DeclStart ~> tVariable) ~ (DeclEnd ~> tNumber) ^^ {
        case variable ~ value => SAssign(variable, value)
    };
    def sAssignment = 
        AssignStart ~> tVariable ~ AssignMid ~ intToken ~ rep(sOperator) <~ AssignEnd ^^ {
        case variable ~ _ ~ first ~ operations => SAssign(variable, SEvaluate(first::operations.flatten))
    };

    def sOperator = tOperator ~ intToken ^^ {case a ~ b => List(a, b)}

    def tOperator = Plus | Minus | Multiply | Divide | Equal | Greater | Or | And;

    def sConditional: Parser[SBranch] = 
        Branch ~> tVariable ~ "[" ~ rep(statement) ~ "]" ~ Else ~ "[" ~ rep(statement) <~ "]" <~ BranchEnd ^^ {
        case condition ~ _ ~ branchTrue ~ _ ~ _ ~ _ ~ branchFalse => SBranch(condition, branchTrue, branchFalse)
    };

    def sLoop = While ~> tVariable ~ "[" ~ rep(statement) <~ "]" <~ EndWhile ^^ {
        case condition ~ _ ~ block => SWhile(condition, block)
    };

    def token: Parser[Statement] = tStringLiteral | tNumber | tVariable;
    def intToken: Parser[Statement with EvalsToInt] = tNumber | tVariable;

    def tVariable: Parser[VToken] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ {x => VToken(x)};
    def tStringLiteral: Parser[Statement] = stringLiteral ^^ {x => VString(x.substring(1, x.length-1))};
    def tNumber: Parser[VNumber] = (decimalNumber | wholeNumber) ^^ {x => VNumber(x.toDouble)};

    def evalExpression(expr: List[Any], vars: Map[String, Double]): Double = {
        expr.head match {
            case VNumber(top) => evalExpression(expr.tail, vars, top)
            case VToken(v) => evalExpression(expr.tail, vars, vars(v))
        }
    }

    def evalExpression(expr: List[Any], vars: Map[String, Double], top: Double): Double = {
        
        implicit def double2bool(a: Double): Boolean = {a != 0}
        implicit def bool2double(a: Boolean): Double = if(a) 1 else 0
        
        implicit def token2double(a: VToken): Double = {
            val VToken(name) = a
            vars(name)
        }

        implicit def number2double(a: VNumber): Double = {
            val VNumber(x) = a
            x
        }

        implicit def number2bool(a: VNumber): Boolean = double2bool(number2double(a))


        return expr match {
            case List() => top
            case op::VToken(x)::rest => evalExpression(op::VNumber(vars(x))::rest, vars, top)
            case Plus::(b: VNumber)::rest => evalExpression(rest, vars, top + b)
            case Minus::(b: VNumber)::rest => evalExpression(rest, vars, top - b)
            case Multiply::(b: VNumber)::rest => evalExpression(rest, vars, top * b)
            case Divide::(b: VNumber)::rest => evalExpression(rest, vars, top / b)
            case Equal::(b: VNumber)::rest => evalExpression(rest, vars, top == number2double(b))
            case Greater::(b: VNumber)::rest => evalExpression(rest, vars, top > b)
            case Or::(b: VNumber)::rest => evalExpression(rest, vars, top || b)
            case And::(b: VNumber)::rest => evalExpression(rest, vars, top && b)
            case what => throw new Exception(s"wtf $what")
        }
    }

    def eval(program: List[Statement]): Unit = {
        var vars = Map[String, Double]()
        eval(program, vars)
    }

    def eval(program: List[Statement], variables: Map[String, Double]): Unit = {
        program.foreach {
            case SPrint(VToken(x)) => println(variables(x))
            case SPrint(VString(x)) => println(x)
            case SPrint(VNumber(x)) => println(x)
            case SAssign(VToken(v), VNumber(x)) => {variables(v) = x}
            case SAssign(VToken(v), VToken(x)) => {variables(v) = variables(x)}
            case SAssign(VToken(v), SEvaluate(expr)) => {variables(v) = evalExpression(expr, variables)}
            case SBranch(VToken(v), onTrue, onFalse) => {
                if (variables(v) != 0) {
                    eval(onTrue, variables)
                } else {
                    eval(onFalse, variables)
                }
            }
            case SWhile(VToken(v), block) => {
                while (variables(v) != 0) {
                    eval(block, variables)
                }
            }
            case e => println(s"non gestito $e")
        }
    }

    def eval(sourceCode: String): Unit = {
        parse(program, sourceCode) match {
            case Success(e, _) => eval(e)
            case error => println(error)
        }
    }
}

object ArnoldC {
    def main(args: Array[String]) = {
        val arnold = new ArnoldC
        args foreach (x => {
            val content = Source.fromFile(x).getLines.mkString("\n")
            arnold.eval(content)
        });
    }
}