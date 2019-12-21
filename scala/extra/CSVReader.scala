import scala.io.Source;


class CSVReader(filename: String) {
    val file = Source.fromFile(filename);
    val columns = try {
            parse(file.getLines)
    } finally file.close
    
    private def parse(lines: Iterator[String]): List[(List[String], Int)] = {
        var result: Array[(Array[String], Int)] = Array();
        for (line <- lines) {
            line.split(", ").zipWithIndex.foreach {
                case (item, i) => {
                    if (result.length <= i) {
                        result :+= (Array(item), item.length);
                    } else {
                        val (items, maxLen) = result(i);
                        result(i) = (items :+ item, if (item.length > maxLen) item.length else maxLen);
                    }
                }
            };
        }
        return (
            (result map {case (items, len) => (items.toList, len)}).toList
        );
    }

    private def repeat(start: String, toRepeat: String, count: Int): String = {
        if (count <= 0)
            return start;
        return repeat(start.concat(toRepeat), toRepeat, count-1);
    }
    
    def prettyPrint() = {
        // Una riga di '-' ripetuti per la larghezza della tabella
        val tableMarginH = repeat("", "-", columns.foldLeft(1) {
            case (acc, (_, len)) => 4 + len + acc
        });

        // Stampa una riga, riga è una lista di (item, columnWidth)
        def printRow(row: List[(String, Int)]) = {
            row foreach {case (item, padding) => {
                val spaces = 1 + padding - item.length;
                print(repeat(s"| $item", " ", spaces + 1));
            }};
            println("|");
        }

        println(tableMarginH);
        val header = columns map {case (items, padding) => (items.head, padding)};
        printRow(header);
        println(tableMarginH);

        def printItems(columns: List[(List[String], Int)]): Unit = {
            if (! (columns exists {case (items, pad) => items.length == 0})) {
                printRow(columns map {case (items, pad) => (items.head, pad)});
                printItems(columns map {case (items, pad) => (items.tail, pad)});
            }
        }

        printItems(columns map {case (items, pad) => (items.tail, pad)});
        println(tableMarginH);
    }
}

object CSVReader {
    def main(args: Array[String]): Unit = {
        args foreach {
            arg =>
                println(s"Content of $arg");
                val csv = new CSVReader(arg);
                csv.prettyPrint();
        }
    }
}