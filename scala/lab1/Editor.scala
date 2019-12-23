class Editor{
    private var cursor = 0;
    private var text = "";

    private def clamp(x: Int): Int = { 
        val lower = 0;
        val upper = text.length;
        return (lower max x min upper); 
    };

    def x() = {
        if (text.length != 0) {
            if (cursor == text.length) {
                text = text.substring(0, cursor-1);
            } else {
                text = text.substring(0, cursor) + text.substring(cursor + 1);
            }
            cursor = clamp(cursor - 1);
        }
        print();
    }

    def xw() = {
        val from = cursor;
        val to = text.indexOf(' ', from+1);
        if (to == -1) {
            text = text.substring(0, from);
        } else {
            text = text.substring(0, from) + text.substring(to+1);
            cursor = clamp(cursor - 1);
        }
    }

    def i(c: Char) = {
        text = text.substring(0, cursor) + c + text.substring(cursor);
        cursor += 1;
    }

    def iw(word: String) = {
        val added = word + ' ';
        if (cursor == text.length) {
            text += added;
        } else {
            text = text.substring(0, cursor+1) + added + text.substring(cursor+1);
        }
        cursor += added.length;
    }

    private def moveCursor(positions: Int) = {
        cursor = clamp(cursor + positions);
    }

    def l(pos: Int = 1) = {moveCursor(pos);}
    def h(pos: Int = 1) = {moveCursor(-pos);}

    override def toString(): String = {
        return s"[$cursor]: |$text|";
    }
}

object Editor {
    def main(args: Array[String]): Unit = {
        val editor = new Editor;
        "Ciao come stai??? " foreach {x => editor.i(x)};
        println(editor);
        editor.x;
        editor.x;
        println(editor);
        editor.iw("Io sto bene");
        println(editor);
        editor.h(60);
        editor.l(4);
        editor.iw("marco");
        println(editor);
        editor.xw();
        println(editor);
    }
}