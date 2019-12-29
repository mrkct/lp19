import scala.io.Source;
import scala.util.parsing.combinator._;


object Arithmetic extends JavaTokenParsers {
    override def skipWhitespace = false;

    sealed trait Operand;
    case class Sum() extends Operand;
    case class Sub() extends Operand;
    case class Mul() extends Operand;
    case class Div() extends Operand;
    case class Equ() extends Operand;

    case class Row(whitespace: String, value: String, operand: Operand);

    def operation = repsep(operand, "\n") ~ "\n" ~ last_operand ~ separator ~ result ^^ {
        case rows ~ _ ~ last ~ separator ~ result => {
            val allRows = rows :+ last;
            
            // Controllo che il separatore abbia la lunghezza corretta
            // Calcola la riga con 
            //val longestRow = allRows.foldLeft(0)({
            //    case (acc, Row(whitespace, value, _)) => acc max whitespace.concat(value).length
            //});
            val longestRow = allRows.head match {
                case Row(whitespace, value, _) => whitespace.concat(value).length
            };
            if (!allRows.forall({case Row(ws, x, _) => ws.concat(x).length == longestRow})) {
                throw new Exception(
                    s"Not all rows are right aligned"
                );
            }
            if (separator.length != 2 + longestRow) {
                throw new Exception(
                    s"the separator line is not the correct length (expected $longestRow, found $separator.length)"
                );
            }
            val (_, expectedResult) = allRows.foldLeft((Equ() : Operand, 0))({
                case ((prevOp, acc), Row(_, value, op)) => {
                    val x = value.toInt;
                    (op, prevOp match {
                        case Equ() => x
                        case Sum() => acc + x
                        case Sub() => acc - x
                        case Mul() => acc * x
                        case Div() => acc / x
                    })
                }
            });
            result.toInt == expectedResult
        }

    };
    def result = " *".r ~> number;
    def separator = "-+".r <~ "\n";
    def last_operand = " *".r ~ number <~ " =" <~ "\n" ^^ {
        case whitespace ~ value => Row(whitespace, value, Equ())
    };
    def operand = " *".r ~ number ~ " " ~ operator ^^ {
        case whitespace ~ value ~ _ ~ (operator: Operand) => Row(whitespace, value, operator)
    };

    def operator = ("+" | "-" | "*" | "/") ^^ {
        case "+" => Sum()
        case "-" => Sub()
        case "/" => Div()
        case "*" => Mul()
    };
    def number = "[0-9]+".r;

    def main(args: Array[String]) = {
        if (args.length < 1) {
            println("usage: scala Arithmetic.scala <filename>");
            System.exit(0);
        }
        val file = Source.fromFile(args(0));
        val content = file.getLines.mkString("\n");
        file.close;
        println(s"File content:\n$content");

        println(parse(operation, content));
    }
}