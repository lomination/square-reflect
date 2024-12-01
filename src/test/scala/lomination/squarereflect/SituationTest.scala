package lomination.squarereflect

import munit.FunSuite

import lomination.squarereflect.Direction.*

class SituationTest extends FunSuite {
  test("Double block") {
    val board = FastBoardBuilder(
      s"""|0,  0,  1-D-F;
          |0,1-D-F,  0  """.stripMargin
    )
    val game     = Game(board, Seq(Player.init(Position(0, 0), East))).compute(3).move(0, South).computeOnce
    val expected = Player(Seq(Position(0, 0), Position(1, 0)), IsStopped, 0, 0, None)
    assert(clue(game.players(0)) == clue(expected))
  }

  test("Angle with block behind") {
    val board = FastBoardBuilder(
      """|0,0,2-S-D-F,3-D;
         |0,0, 1-D-F,  0""".stripMargin
    )
    val game     = Game(board, Seq(Player.init(Position(0, 0), East))).compute(4)
    val expected = Player(Seq(Position(0, 0), Position(1, 0), Position(2, 0)), IsStopped, 0, 0, None)
    assert(clue(game.players(0)) == clue(expected))
    val game2 = game.move(0, East).computeOnce
    assert(game2.players(0).hasFinished)
  }

  test("Double angle") {
    val board = FastBoardBuilder(
      """|0,0,2-S-D-F, 0 ;
         |0,0,2-N-D-F,3-D""".stripMargin
    )
    val game = Game(board, Seq(Player.init(Position(0, 0), East))).compute(5)
    val expected = Player(
      Seq(Position(0, 0), Position(1, 0), Position(2, 0), Position(2, 1), Position(3, 1)),
      HasFinished,
      0,
      0,
      None
    )
    assert(clue(game.players(0)) == clue(expected))
  }

}
