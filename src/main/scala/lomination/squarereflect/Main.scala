import lomination.squarereflect.codec.BoardParser
import lomination.squarereflect.console.ConsoleApp
import scala.util.Success
import scala.util.Failure


@main def main: Unit =
  ConsoleApp.runMenu

// @main def main(boardName: String): Unit =
//   BoardParser(s"boards/$boardName.srb") match
//     case Success(board) => ConsoleApp.run(board)
//     case Failure(exception) =>
//       println("Parsing board failed:")
//       println(exception.getMessage)
