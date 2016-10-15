import scala.io.Source

/*
TODO Прочитайте содержимое данного файла.
В случае неудачи верните сообщение соответствующего исключения.

It assumes, that file is run as script with `scala script.sc`
 */
def readThisWorksheet(): String = {
    val worksheetFile = System.getProperty("sun.java.command").split(" ")(1)
    try {
        Source.fromFile(worksheetFile).getLines.mkString("\n")
    } catch {
        case e: Exception => e.getMessage
    }
}

println(readThisWorksheet())