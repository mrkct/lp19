import scala.util.parsing.combinator._;
import scala.io.Source;
import scala.util.Try;


object Desk extends JavaTokenParsers {
    private def isInt(str: String): Boolean = {
        Try(str.toInt).isSuccess
    }

    def eval(parsedProgram: (List[String], Map[String, Int])): Int = {
        val (tokens, values) = parsedProgram;
        return tokens.foldLeft(0) {
            case (acc, x) if isInt(x) => acc + x.toInt
            case (acc, variable) => acc + values(variable)
        };
    }

    def deskProgram = "print" ~> expression ~ "where" ~ rep1sep(initialization, ",") ^^ {
        case expr ~ "where" ~ vars => (expr, vars.toMap)
    };

    def initialization = varName ~ "=" ~ wholeNumber ^^ {
        case name ~ "=" ~ value => (name, value.toInt)
    };

    def expression = repsep(token, "+");
    def token = varName | wholeNumber;
    def varName = "[a-zA-Z][a-zA-Z0-9]*".r;

    def evalDeskProgram(code: String): Option[Int] = {
        parse(deskProgram, code) match {
            case Success(r, _) => Some(eval(r))
            case error => None
        };
    }

    def main(args: Array[String]) = {
        args foreach {
            filename => {
                val file = Source.fromFile(filename);
                val content = file.getLines.mkString("\n");
                file.close;
                parse(deskProgram, content) match {
                    case Success(r, _) => println(s"$content ==> $r")
                    case error => println(s"$error")
                };
            }
        }
    }
}