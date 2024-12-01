package lomination.squarereflect

import munit.FunSuite
import lomination.squarereflect.*
import lomination.squarereflect.Direction.*
// import lomination.squarereflect.console.Style.{Fg, Reset}

class TileTest extends FunSuite {
  test("Empty 0 behaviour") {
    val board = FastBoardBuilder(
      s"""|0,0,0;
          |0,0,0;
          |0,0,0""".stripMargin
    )
    val starts = Seq((Position(1, 0), South), (Position(2, 1), West), (Position(1, 2), North), (Position(0, 1), East))
    for ((pos, dir) <- starts) yield
      // println(s"${Fg(255, 199, 6)}Testing direction $dir (${(dir.ordinal + 2) % 4})$Reset")
      val game     = Game(board, Seq(Player.init(pos, dir))).compute(2)
      val player   = game.players(0)
      val expected = Player(Seq(pos, pos.next(dir), pos.next(dir).next(dir)), IsMoving(dir), 0, 0, None)
      assert(clue(player) == clue(expected))
      assert(clue(board) == clue(game.board))
  }

  test("Block 1-D-F behaviour") {
    val board = FastBoardBuilder(
      s"""|0,  0,  0;
          |0,1-D-F,0;
          |0,  0,  0""".stripMargin
    )
    val starts = Seq((Position(1, 0), South), (Position(2, 1), West), (Position(1, 2), North), (Position(0, 1), East))
    for ((pos, dir) <- starts) yield
      // println(s"${Fg(255, 199, 6)}Testing direction $dir (${(dir.ordinal + 2) % 4})$Reset")
      val game     = Game(board, Seq(Player.init(pos, dir))).compute(2)
      val player   = game.players(0)
      val expected = Player(Seq(pos), IsStopped, 0, 0, None)
      assert(clue(player) == clue(expected))
      assert(clue(board) == clue(game.board))
  }

  test("Block 1-D-T behaviour") {
    val board = FastBoardBuilder(
      s"""|0,  0,  0;
          |0,1-D-T,0;
          |0,  0,  0""".stripMargin,
      Block()
    )
    val starts = Seq((Position(1, 0), South), (Position(2, 1), West), (Position(1, 2), North), (Position(0, 1), East))
    for ((pos, dir) <- starts) yield
      // println(s"${Fg(255, 199, 6)}Testing direction $dir (${(dir.ordinal + 2) % 4})$Reset")
      val game           = Game(board, Seq(Player.init(pos, dir))).compute(3)
      val player         = game.players(0)
      val expectedPlayer = Player(Seq(pos, Position(1, 1)), IsStopped, 0, 0, None)
      assert(clue(player) == clue(expectedPlayer))
      assert(clue(game.board(0, 0)) == clue(Empty))
      assert(clue(game.board(pos.next(dir).next(dir))) == clue(Block(Default, true)))
  }

  test("Angle 2-N-D-F behaviour") {
    val board = FastBoardBuilder(
      s"""|0,   0,   0;
          |0,2-N-D-F,0;
          |0,   0,   0""".stripMargin
    )
    val starts = Seq((Position(1, 0), South), (Position(2, 1), West), (Position(1, 2), North), (Position(0, 1), East))
    for ((pos, dir) <- starts) yield
      // println(s"${Fg(255, 199, 6)}Testing direction $dir (${(dir.ordinal + 2) % 4})$Reset")
      val game   = Game(board, Seq(Player.init(pos, dir))).compute(2)
      val player = game.players(0)
      val expected =
        dir match
          case South => Player(Seq(pos, Position(1, 1), Position(2, 1)), IsMoving(East), 0, 0, None)
          case West  => Player(Seq(pos, Position(1, 1), Position(1, 0)), IsMoving(North), 0, 0, None)
          case North => Player(Seq(pos), IsStopped, 0, 0, None)
          case East  => Player(Seq(pos), IsStopped, 0, 0, None)
      assert(clue(player) == clue(expected))
      assert(clue(board) == clue(game.board))
  }

  test("Angle 2-N-D-T behaviour") {
    val board = FastBoardBuilder(
      s"""|0,   0,   0;
          |0,2-N-D-T,0;
          |0,   0,   0""".stripMargin,
      Block()
    )
    val starts = Seq((Position(1, 0), South), (Position(2, 1), West), (Position(1, 2), North), (Position(0, 1), East))
    for ((pos, dir) <- starts) yield
      // println(s"${Fg(255, 199, 6)}Testing direction $dir (${(dir.ordinal + 2) % 4})$Reset")
      val game   = Game(board, Seq(Player.init(pos, dir))).compute(3)
      val player = game.players(0)
      val expected =
        dir match
          case South => Player(Seq(pos, Position(1, 1), Position(1, 2), Position(2, 2)), IsMoving(East), 0, 0, None)
          case West  => Player(Seq(pos, Position(1, 1), Position(0, 1), Position(0, 0)), IsMoving(North), 0, 0, None)
          case North => Player(Seq(pos, Position(1, 1)), IsStopped, 0, 0, None)
          case East  => Player(Seq(pos, Position(1, 1)), IsStopped, 0, 0, None)
      assert(clue(player) == clue(expected))
      assert(clue(game(Position(1, 1))) == clue(Empty))
      assert(clue(game(Position(1, 1).next(dir))) == clue(Angle(North, Default, true)))
  }

}
