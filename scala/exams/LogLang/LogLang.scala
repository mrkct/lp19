import scala.util.parsing.combinator._;
import scala.io.Source;
import scala.util.Try;
import java.nio.file.Files;
import java.nio.file.Paths;


object LogLang extends JavaTokenParsers {
    
    sealed trait Operation {
        def execute(): Boolean;
    }

    case class Remove(filename: String) extends Operation {
        def execute(): Boolean = {
            Try(
                Files.delete(Paths.get(filename))
            ).isSuccess
        }
    }

    case class Backup(filename: String, newname: String) extends Operation {
        def execute(): Boolean = {
            Try(
                Files.copy(Paths.get(filename), Paths.get(newname))
            ).isSuccess
        }
    }

    case class Rename(oldname: String, newname: String) extends Operation {
        def execute(): Boolean = {
            Try(
                Files.move(Paths.get(oldname), Paths.get(newname))
            ).isSuccess
        }
    }

    case class Merge(a: String, b: String, into: String) extends Operation {
        def execute(): Boolean = {
            Try({
                val first = Source.fromFile(a);
                val second = Source.fromFile(b);
                val content = first.getLines.mkString("\n") + second.getLines.mkString("\n");
                first.close
                second.close

                Files.write(Paths.get(into), content.getBytes);
            }).isSuccess
        }
    }

    case class Task(name: String, operations: List[Operation]) extends Operation {
        def execute(): Boolean = {
            println(s"Task $name");
            operations.foldLeft(1) {
                case (acc, x) => {
                    val result = x.execute();
                    println(s"\t[op$acc] $result");
                    acc + 1
                } 
            };
            true
        }
    }

    def program = rep(task);
    def task = "task" ~> taskName ~ ("{" ~> rep(operation) <~ "}") ^^ {
        case name ~ operations => Task(name, operations)
    };

    def taskName = """[a-zA-Z][a-zA-Z0-9]*""".r;

    def operation = opRemove | opBackup | opRename | opMerge;

    def opRemove = "remove" ~> stringLiteral ^^ {
        case file => Remove(file)
    };
    def opBackup = "backup" ~> stringLiteral ~ stringLiteral ^^ {
        case file ~ backup => Backup(file, backup)
    };
    def opRename = "rename" ~> stringLiteral ~ stringLiteral ^^ {
        case from ~ to => Rename(from, to)
    };
    def opMerge = "merge" ~> stringLiteral ~ stringLiteral ~ stringLiteral ^^ {
        case a ~ b ~ into => Merge(a, b, into)
    };

    override def stringLiteral = super.stringLiteral ^^ {case x => x.substring(1, x.length - 1)};

    def main(args: Array[String]) = {
        if (args.length < 1) {
            println(s"usage: scala LogLang.scala <filename>");
            System.exit(0);
        }
        val file = Source.fromFile(args(0));
        val content = file.getLines.mkString("\n");
        file.close;
        parse(program, content) match {
            case Success(x, _) => x.foreach (_.execute)
            case error => println(error)
        };
    }
}