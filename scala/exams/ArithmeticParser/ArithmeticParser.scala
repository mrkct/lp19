import scala.util.parsing.combinator._;


object ArithmeticParser extends JavaTokenParsers {

    sealed trait Expr {
        def step(): Expr;
    }

    case class Val(a: Double) extends Expr {
        def step(): Expr = { return Val(a); }
        override def toString(): String = { return a.toString; }
    }

    case class Sum(a: Expr, b: Expr) extends Expr {
        def step(): Expr = {
            return (a, b) match {
                case (Val(x), Val(y)) => Val(x+y)
                case (x, y) => Sum(x.step, y.step)
            };
        }
        override def toString(): String = { return s"(${a.toString} + ${b.toString})"}
    }

    case class Sub(a: Expr, b: Expr) extends Expr {
        def step(): Expr = {
            return (a, b) match {
                case (Val(x), Val(y)) => Val(x-y)
                case (x, y) => Sub(x.step, y.step)
            };
        }
        override def toString(): String = { return s"(${a.toString} - ${b.toString})"}
    }

    case class Mul(a: Expr, b: Expr) extends Expr {
        def step(): Expr = {
            return (a, b) match {
                case (Val(x), Val(y)) => Val(x*y)
                case (x, y) => Mul(x.step, y.step)
            };
        }
        override def toString(): String = { return s"(${a.toString} * ${b.toString})"}
    }

    case class Div(a: Expr, b: Expr) extends Expr {
        def step(): Expr = {
            return (a, b) match {
                case (Val(x), Val(y)) => Val(x/y)
                case (x, y) => Div(x.step, y.step)
            };
        }
        override def toString(): String = { return s"(${a.toString} / ${b.toString})"}
    }

    def toExpr(x: Any): Expr = {
        return x match {
            case y: String => Val(y.toDouble)
            case y: Expr => y
        };
    }

    def intNum = """\d+""".r;
    def operator = "+" | "-" | "*" | "/";
    def expr: Parser[Any] = "(" ~> (intNum | expr) ~ operator ~ (intNum | expr) <~ ")" ^^ {
        case (a: Any) ~ "+" ~ (b: Any) => Sum(toExpr(a), toExpr(b))
        case (a: Any) ~ "-" ~ (b: Any) => Sub(toExpr(a), toExpr(b))
        case (a: Any) ~ "*" ~ (b: Any) => Mul(toExpr(a), toExpr(b))
        case (a: Any) ~ "/" ~ (b: Any) => Div(toExpr(a), toExpr(b))
    };

    

    def solveByStep(expression: String): Unit = {
        def solve(e: Expr): Unit = {
            println(s"$e");
            e match {
                case Val(x) => {}
                case _ => solve(e.step)
            }
        }
        parse(expr, expression) match {
            case Success(e: Expr, _) => solve(e)
            case Success(e, _) => println(s"wtf, parser returned this: $e")
            case Error(msg, _) => println(s"Error while parsing: $msg")
            case Failure(msg, _) => println(s"Failed to parse expresion: $msg")
        };
    }

    def main(args: Array[String]) = {
        val sample = Array(
            "(1 + 2)", 
            "((1 * 5) + 3)",
            "(((5 + 5) + 5) + 5)", 
            "(((3 - 1) * 5) / (5 + ((3 + 3) - 12)))", 
            "((2 + 7) + ((3 + 9) + 4))", 
            "((1 * 7) + (7 * ((3 + 9) + 5)))", 
            "((5*(7-2))+(((15/3)+2)-((1422*2)-(3500/4))))"
        );
        val expressions = if (args.length > 0) args else sample;
        expressions foreach solveByStep;
    }
}