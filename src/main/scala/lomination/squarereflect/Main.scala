import lomination.squarereflect.*
import lomination.squarereflect.console.Writable
import lomination.squarereflect.console.Writer.{given Writable[Game]}

@main def main(): Unit =
  val board: Board = Board(
    Seq(
      Seq(Empty, End(), Empty),
      Seq(Empty, Empty, Empty),
      Seq(Empty, Empty, Empty),
      Seq(Empty, Empty, Block()),
      Seq(Block(), Empty, Empty),
      Seq(Empty, Empty, Empty),
      Seq(Empty, Kill(), Empty)
    )
  )
  val game: Game = Game.init(board, (Position(0, 0), Direction.South))
  println(game.write)
