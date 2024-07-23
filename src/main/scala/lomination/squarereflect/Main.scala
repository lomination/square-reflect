import lomination.squarereflect.Game
import lomination.squarereflect.codec.BoardParser
import lomination.squarereflect.console.ConsoleApp

@main def main(): Unit =
  val board = BoardParser("boards/Board1.srb")
  if (board.isSuccess)
    val game = Game.init(board.get)
    val app = ConsoleApp(game)
    app.run
  else
    println("Parsing board failed")
    println(board.failed.get.getMessage)
