import scala.util.parsing.combinator.JavaTokenParsers;
import scala.io.Source;


object JSONParser extends JavaTokenParsers {
    def json = jsonObject;
    def jsonObject: Parser[Map[Any, Any]] = "{" ~> repsep(jsonField, ",") <~ "}" ^^ {
        case content => content.toMap
    }
    def jsonField: Parser[(Any, Any)] = jsonKey ~ ":" ~ jsonToken ^^ {
        case key ~ _ ~ value => (key, value)
    }
    def jsonToken: Parser[Any] = jsonInt | jsonString | jsonObject | jsonArray
    def jsonKey = jsonInt | jsonString;

    def jsonInt = wholeNumber ^^ {case x => x.toInt}
    def jsonString = stringLiteral ^^ {case x => x.substring(1, x.length-1)}
    def jsonArray: Parser[Any] = "[" ~> repsep(jsonToken, ",") <~ "]";
    def main(args: Array[String]) = {
        List(
            "{}", 
            """{"a": "b"}""", 
            """{"a": 1, "b": 2, "c": 3, "d": "Hello"}""", 
            """{"a": 1, "b": "Ciao", "c": { 0: 1, 1: 2, 2: 3 }}""", 
            """{"a": [1, {"x": [2, 3, 4]}, 5, "sei"]}""", 
            """
            {
                "name": "Marco",
                "age": 21,
                "idk": [1, 2, "three", "4"],
                "abc": {
                    "qwe": "RTY",
                    54: 55,
                    3: ["q", "w", "e", "r", "t", "y"]
                }
            }
            """
        ) foreach {x => {
            println(s"${parse(json, x)}")
        }}
    }
}
