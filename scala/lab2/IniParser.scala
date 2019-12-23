import scala.io.Source;
import scala.io.BufferedSource;
import scala.util.parsing.combinator._;


class IniParser(filename: String) extends JavaTokenParsers {
    override val skipWhitespace = false;

    def ini = repsep(section, "\n") ^^ {case sections => {
        (sections map {case (x, y) => x -> y}).toMap
    }};
    def section = sectionName ~ repsep(parameter, "\n") ^^ { case section ~ values => {
        (section, (values map {case (x, y) => x -> y}).toMap)
    }};
    def sectionName = "[" ~> word <~ "]\n";
    def parameter = word ~ "=" ~ word <~ opt(comment) ^^ {case key ~ _ ~ value => (key.trim, value.trim)};
    def comment = ";[^\n]*".r;
    def word = "[a-zA-Z0-9\t ]+".r;

    val file = Source.fromFile(filename);
    val sections = try {
        parseFile(file);
    } finally (file.close);

    private def parseFile(file: BufferedSource): Map[String, Map[String, String]] = {
        val fileContent = file.getLines.mkString("\n");
        val result = parse(ini, fileContent);

        result match {
            case Error(msg, _) => throw new Exception(s"Fatal error: $msg")
            case Failure(msg, _) => throw new Exception(s"Parse fail: $msg")
            case _ => 
        };

        return result.get;
    }

    def apply(section: String): Map[String, String] = {
        return sections(section);
    }

    override def toString(): String = { return sections.toString; }
}

object IniParser {
    def main(args: Array[String]) = {
        val filename = if (args.length > 0) args(0) else "sample.ini";
        val parser = new IniParser(filename);
        for (section <- parser.sections.keys) {
            println(s"Section [$section] contains: ");
            for (parameter <- parser(section).keys) {
                val value = parser(section)(parameter);
                println(s"\t$parameter: $value");
            }
        }
    }
}