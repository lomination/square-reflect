package lomination.squarereflect


import lomination.squarereflect.Kill

class Game(val board: Board, val players: Seq[Player]):

  lazy val width: Int = board.width

  lazy val height: Int = board.height

  def apply(position: Position): Tile = board(position)

  def apply(x: Int, y: Int): Tile = board(x, y)

  def computeOnce: Game =
    players.foldLeft(Game(board, Seq())) {
      (game: Game, player: Player) =>
        if (player.isMoving)
          val update: Update = game.board(player.position)(player)
          Game(update.boardUpt.getOrElse(identity[Board])(game.board), game.players :+ update.player)
        else
          Game(game.board, game.players :+ player)
    }
  
  def compute(iterations: Int): Game =
    if (iterations <= 0 || players.forall(!_.isMoving)) this
    else this.computeOnce.compute(iterations - 1)

  def computeUntilStopped: Game =
    if (players.exists(!_.isMoving)) this
    else this.computeOnce.computeUntilStopped

  def move(moves: Seq[Option[Direction]]): Game =
    if (moves.sizeIs != players)
      throw java.lang.IllegalArgumentException(s"Invalid size of the given seq 'moves' (${moves.size} instead of ${players.size})")
    else
      Game(board, (players zip moves).map( pm => if (pm._1.isStopped && pm._2.isDefined) pm._1.move(pm._2.get) else pm._1 ))    
  
  def move(playerId: Int, direction: Direction): Game =
    val newPlayers: Seq[Player] =
      for (i <- 0 until players.length) yield
        val player = players(i)
        if (i == playerId && player.state == IsStopped)
          player.setState(IsMoving(direction))
        else
          player
    Game(board, newPlayers)



object Game:

  def init(board: Board): Game =
    Game(board, board.starts.map(start => Player.init(start._1, start._2)))


case class Board(rows: Seq[Seq[Tile]], starts: Seq[(Position, Direction)], outsideTile: Tile, lightLevel: Option[Int], info: BoardInfo):

  lazy val width: Int = rows(0).size

  lazy val height: Int = rows.size

  def apply(position: Position): Tile =
    if (0 <= position.x && position.x < width && 0 <= position.y && position.y < height)
      rows(position.y)(position.x)
    else
      outsideTile

  def apply(x: Int, y: Int): Tile =
    if (0 <= x && x < width && 0 <= y && y < height)
      rows(y)(x)
    else
      outsideTile

  def setTileAt(position: Position, f: Tile => Tile): Board =
    val rows: Seq[Seq[Tile]] =
      for (y <- 0 until height) yield
        if (y == position.y) this.rows(y)
        else for (x <- 0 until width) yield if (x == position.x) f(this.rows(y)(x)) else this.rows(y)(x)
    Board(rows, starts, outsideTile, lightLevel, info)

  def setTileAt(position: Position, tile: Tile): Board =
    setTileAt(position, _ => tile)


/** Represents the new states of the player and the tile after a `tile.apply(player)`
 * 
 * @param player
 *   the new player. If None, the player is not modified
 * @param tile
 *   the new tile. If None, the tile is not modified
*/
case class Update(player: Player, boardUpt: Option[Board => Board]):
  def apply(board: Board): Board =
    boardUpt match
      case Some(f) => f(board)
      case None => board


object Update:
  def apply(player: Player, position: Position, f: Tile => Tile) = new Update(player, Some(_.setTileAt(position, f)))
  def apply(player: Player, position: Position, tile: Tile) = new Update(player, Some(_.setTileAt(position, tile)))
  def apply(player: Player) = new Update(player, None)


case class BoardInfo(name: String, author: String, date: String)
