package lomination.squarereflect

import lomination.squarereflect.{Kill, Player, Tile}

/** Represents a square reflect game.
  * @param board
  *   the board which the game is played on.
  * @param players
  *   the players of the game.
  */
class Game(val board: Board, val players: Seq[Player]):

  /** This game's board's width. */
  lazy val width: Int = board.width

  /** This game's board's height. */
  lazy val height: Int = board.height

  /** Returns the tile at the given position. If the given position is out of this game's board, returns the board's
    * default tile.
    */
  def apply(position: Position): Tile = board(position)

  /** Returns the tile at the given position. If the given position is out of this game's board, returns the board's
    * default tile.
    */
  def apply(x: Int, y: Int): Tile = board(x, y)

  def lookAt(x: Int, y: Int): Option[Tile] =
    val isVisible = board.lightLevel.isDefined && players
      .forall(_.position.distTo(x, y) > board.lightLevel.get.toDouble)
    if (isVisible) None
    else Some(apply(x, y))

  def lookAt(position: Position): Option[Tile] =
    lookAt(position.x, position.y)

  /** Returns a new game containing the updated board and players after 1 game tick. */
  def computeOnce: Game =
    players.foldLeft(Game(board, Seq[Player]())) { (game: Game, player: Player) =>
      if (player.isMoving)
        val update = game.board(player.nextPosition)(player, game.board)
        Game(update.board, game.players :+ update.player)
      else Game(game.board, game.players :+ player)
    }

  /** Returns a new game containing the updated board and players after the given number of iterations of game ticks. */
  def compute(iterations: Int): Game =
    if (iterations <= 0 || players.forall(!_.isMoving)) this
    else this.computeOnce.compute(iterations - 1)

  /** May not end */
  def computeUntilStopped: Game =
    if (players.exists(!_.isMoving)) this
    else this.computeOnce.computeUntilStopped

  /** Updates the players' direction to the given (if some) */
  def move(moves: Seq[Option[Direction]]): Game =
    if (moves.sizeIs != players)
      throw java.lang.IllegalArgumentException(
        s"Invalid size of the given seq 'moves' (${moves.size} instead of ${players.size})"
      )
    else
      Game(
        board,
        (players zip moves).map(pm => if (pm._1.isStopped && pm._2.isDefined) pm._1.move(pm._2.get) else pm._1)
      )

  /** Returns
    *
    * @param playerId
    * @param direction
    * @return
    */
  def move(playerId: Int, direction: Direction): Game =
    val newPlayers: Seq[Player] =
      for (i <- 0 until players.length) yield
        val player = players(i)
        if (i == playerId && player.isStopped)
          player.setState(IsMoving(direction))
        else
          player
    Game(board, newPlayers)

object Game:

  /** Inits a game with the given board and new players at the boards' start positions */
  def init(board: Board): Game =
    Game(board, board.starts.map(start => Player.init(start._1, start._2)))

case class Board(
    rows: Seq[Seq[Tile]],
    starts: Seq[(Position, Direction)],
    outsideTile: Tile,
    lightLevel: Option[Int],
    info: BoardInfo
):

  lazy val width: Int = rows(0).size

  lazy val height: Int = rows.size

  def apply(x: Int, y: Int): Tile =
    if (0 <= x && x < width && 0 <= y && y < height)
      rows(y)(x)
    else
      outsideTile

  def apply(position: Position): Tile =
    apply(position.x, position.y)

  def setTileAt(position: Position, tile: Tile): Board =
    val rows: Seq[Seq[Tile]] =
      for (y <- 0 until height)
        yield
          if (y != position.y) this.rows(y)
          else for (x <- 0 until width) yield if (x == position.x) tile else this.rows(y)(x)
    Board(rows, starts, outsideTile, lightLevel, info)

/** Represents the new states of the player and the tile after a `tile.apply(player)`
  *
  * @param player
  *   the new player. If None, the player is not modified
  * @param tile
  *   the new tile. If None, the tile is not modified
  */
case class Update(player: Player, board: Board)

case class BoardInfo(name: String, author: String, date: String)

object BoardInfo:
  def empty: BoardInfo = BoardInfo("", "", "")
