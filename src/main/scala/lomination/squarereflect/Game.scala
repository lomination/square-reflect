package lomination.squarereflect

case class Game(board: Board, players: Seq[Player]):
  lazy val width: Int = board.width
  lazy val height: Int = board.height
  def play(times: Int): Game =
    if (times <= 0) this
    else players.foldLeft(Game(board, Seq())) { (game: Game, player: Player) =>
      if (player.isMoving)
        val update: Update = game.board(player.positions.last)(player)
        val newBoard: Board =
          if (update.tile.isEmpty) game.board
          else board.setTileAt(player.positions.last, update.tile.get)
        Game(newBoard, game.players :+ update.player)
      else
        Game(game.board, game.players :+ player)
    }.play(times - 1)
  def play: Game = play(1)

  def move(moves: Seq[Option[Direction]]): Game =
    if (moves.sizeIs != players) throw java.lang.IllegalArgumentException("14982710391")
    Game(
      board,
      (players zip moves).map( pm => if (pm._1.isStopped && pm._2.isDefined) pm._1.move(pm._2.get) else pm._1 )
    )

object Game:
  def init(board: Board, starts: Seq[(Position, Direction)]): Game =
    Game(board, starts.map((start: (Position, Direction)) => Player.init(start._1, start._2)))
  def init(board: Board, start: (Position, Direction)): Game =
    Game(board, Seq(Player.init(start._1, start._2)))



/** Represents the new states of the player and the tile after a `tile.apply(player)`
 * 
 * @param player
 *   the new player. If None, the player is not modified
 * @param tile
 *   the new tile. If None, the tile is not modified
*/
case class Update(player: Player, boardUpt: Option[Board => Board]):
  def this(player: Player, position: Position, f: Tile => Tile) = this(player, Some(_.setTileAt(position, f)))
  def this(player: Player, position: Position, tile: Tile) = this(player, Some(_.setTileAt(position, tile)))
  def this(player: Player) = this(player, None)
  def apply(board: Board): Board =
    boardUpt match
      case Some(f) => f(board)
      case None => board
