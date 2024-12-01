package lomination.squarereflect

import lomination.squarereflect.*
import lomination.squarereflect.Direction.{East, North, South, West}

/** Tiles' super class
  *
  * @param direction
  *   the direction of this tile
  * @param mode
  *   the type of this tile
  */
abstract class Tile:
  /** This tile's id. Depends on its subtype (Block, Angle, ...) */
  val id: Int

  /** This tile's direction. */
  val direction: Direction

  /** This tile's type. Determines whether this tile is always available and whether it will appear/disappear. */
  val mode: Mode

  /** Determines whether this tile can be pushed */
  val pushable: Boolean

  /** Determines if a player can interract with this tile */
  def isAvailable: Boolean = mode.isAvailable

  /** Returns the given player after having applied this tile's behaviour on it */
  def apply(player: Player, board: Board): Update

// Mode (trait)              { def isAvailable }
// ├── Default               { def isAvailable }
// └── NonDefault (abstract) { def isAvailable; val count; def use }
//     ├── Ghost             { def isAvailable; val count; def use }
//     └── Weak              { def isAvailable; val count; def use }

object Tile:
  def unapply(tile: Tile): Option[(Int, Direction, Mode, Boolean)] =
    Some((tile.id, tile.direction, tile.mode, tile.pushable))

sealed trait Mode:
  def isAvailable: Boolean

object Default extends Mode:
  def isAvailable: Boolean = true

sealed abstract class NonDefault extends Mode:
  val count: Int
  def isStable: Boolean = count <= 0
  def use: Mode

object NonDefault:
  def unapply(nonDefault: NonDefault): Option[Int] = Some(nonDefault.count)

case class Ghost(count: Int) extends NonDefault:
  def isAvailable: Boolean = count <= 0
  def use: Ghost           = Ghost(count - 1)

case class Weak(count: Int) extends NonDefault:
  def isAvailable: Boolean = count > 0
  def use: Weak            = Weak(count - 1)

// ----------- Tiles -------------- //

case object Empty extends Tile:
  /** The value is 0 */
  val id: Int = 0

  /** The value is `North` */
  val direction: Direction = North

  /** The value is `Default` */
  val mode: Mode = Default

  /** The value is `false` */
  val pushable: Boolean                           = false
  def apply(player: Player, board: Board): Update = Update(player.forward, board)

case class Block(mode: Mode = Default, pushable: Boolean = false) extends Tile:
  /** The value is 1 */
  val id: Int              = 1
  val direction: Direction = North
  override def apply(player: Player, board: Board): Update =
    val dir: Direction = player.state.asInstanceOf[IsMoving].direction
    if (pushable && isAvailable && board(player.nextPosition.next(dir)) == Empty)
      Update(player.forward, board.setTileAt(player.nextPosition, Empty).setTileAt(player.nextPosition.next(dir), this))
    else
      val newPlayer: Player = if (isAvailable) player.setState(IsStopped) else player.forward
      mode match
        case Default =>
          Update(newPlayer, board)
        case nd @ NonDefault(count) if count > 1 =>
          Update(newPlayer, board.setTileAt(player.nextPosition, Block(nd.use, pushable)))
        case Ghost(_) =>
          Update(newPlayer, board.setTileAt(player.nextPosition, Block(Default, pushable)))
        case Weak(_) =>
          Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Angle(direction: Direction, mode: Mode = Default, pushable: Boolean = false) extends Tile:
  /** The value is 2 */
  val id: Int = 2
  override def apply(player: Player, board: Board): Update =
    val dir: Direction = player.state.asInstanceOf[IsMoving].direction
    if (pushable && isAvailable && board(player.nextPosition.next(dir)) == Empty)
      Update(player.forward, board.setTileAt(player.nextPosition, Empty).setTileAt(player.nextPosition.next(dir), this))
    else
      val newPlayer: Player =
        if (isAvailable)
          if (player.state == IsMoving(direction) || player.state == IsMoving(direction.next))
            player.setState(IsStopped)
          else if (player.state == IsMoving(direction.opposite))
            player.forward.setState(IsMoving(direction.next))
          else // if (player.state == IsMoving(direction.previous))
            player.forward.setState(IsMoving(direction))
        else
          player.forward
      mode match
        case Default =>
          Update(newPlayer, board)
        case nd @ NonDefault(count) if count > 1 =>
          Update(newPlayer, board.setTileAt(player.nextPosition, Angle(direction, nd.use, pushable)))
        case Ghost(_) =>
          Update(newPlayer, board.setTileAt(player.nextPosition, Angle(direction, Default, pushable)))
        case Weak(_) =>
          Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

    // mode match
    //   case Default =>
    //     Update(newPlayer)
    //   case nd: NonDefault if nd.isAvailable =>
    //     Update(newPlayer, newPlayer.position, Angle(direction, nd.use))
    //   case nd: NonDefault /*if !nd.isAvailable*/ =>
    //     Update(player.forward, player.position.next, Angle(direction, nd.use))

case class End(mode: Mode = Default) extends Tile:
  /** The value is 3 */
  val id: Int              = 3
  val direction: Direction = North
  val pushable: Boolean    = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player = if (isAvailable) player.forward.setState(HasFinished) else player.forward
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, End(nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, End(Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Kill(mode: Mode = Default) extends Tile:
  /** The value is 4 */
  val id: Int              = 4
  val direction: Direction = North
  val pushable: Boolean    = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player = if (isAvailable) player.forward.setState(IsDead) else player.forward
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Kill(nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Kill(Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Cannon(mode: Mode = Default) extends Tile:
  /** The value is 5 */
  val id: Int              = 5
  val direction: Direction = North
  val pushable: Boolean    = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player = if (isAvailable) player.forward.setState(IsStopped) else player.forward
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Cannon(nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Cannon(Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Portal(portalId: Char, pairPosition: Position, mode: Mode = Default) extends Tile:
  /** The value is 6 */
  val id: Int              = 6
  val direction: Direction = North
  val pushable: Boolean    = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player =
      if (isAvailable) player.forward.setPositions(player.positions :+ pairPosition) else player.forward
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Portal(portalId, pairPosition, nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Portal(portalId, pairPosition, Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Arrow(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 7 */
  val id: Int           = 7
  val pushable: Boolean = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player = if (isAvailable) player.forward.setState(IsMoving(direction)) else player.forward
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Arrow(direction, nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Arrow(direction, Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Blocker(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 8 */
  val id: Int           = 8
  val pushable: Boolean = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player =
      if (player.state == IsMoving(direction.opposite)) player.setState(IsStopped)
      else player.forward
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Blocker(direction, nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Blocker(direction, Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Tunnel(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 9 */
  val id: Int           = 9
  val pushable: Boolean = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player =
      if (player.state == IsMoving(direction.next) || player.state == IsMoving(direction.previous))
        player.setState(IsStopped)
      else player
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Tunnel(direction, nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Tunnel(direction, Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class Spike(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 10 */
  val id: Int           = 10
  val pushable: Boolean = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player =
      if (player.state == direction) player.forward.setState(IsDead)
      else if (player.state == direction.opposite) player.setState(IsStopped)
      else player
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Spike(direction, nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Spike(direction, Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case class DoubleSpike(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 11 */
  val id: Int           = 11
  val pushable: Boolean = false
  override def apply(player: Player, board: Board): Update =
    val newPlayer: Player =
      if (player.state == direction || player.state == direction.next) player.forward.setState(IsDead)
      else player.setState(IsStopped)
    mode match
      case Default =>
        Update(newPlayer, board)
      case nd @ NonDefault(count) if count > 1 =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Spike(direction, nd.use)))
      case Ghost(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Spike(direction, Default)))
      case Weak(_) =>
        Update(newPlayer, board.setTileAt(player.nextPosition, Empty))

case object Key extends Tile:
  /** The value is 12 */
  val id: Int              = 12
  val direction: Direction = North
  val mode: Mode           = Default
  val pushable: Boolean    = false
  override def apply(player: Player, board: Board): Update =
    Update(player.setKeys(player.keys + 1).forward, board.setTileAt(player.nextPosition, Empty))

case class Lock(pushable: Boolean = false) extends Tile:
  /** The value is 13 */
  val id: Int              = 13
  val direction: Direction = North
  val mode: Mode           = Default
  override def apply(player: Player, board: Board): Update =
    val dir: Direction = player.state.asInstanceOf[IsMoving].direction
    if (pushable && board(player.nextPosition.next(dir)) == Empty)
      Update(player.forward, board.setTileAt(player.nextPosition, Empty).setTileAt(player.nextPosition.next(dir), this))
    else if (player.keys > 0)
      Update(player.setKeys(player.keys - 1).forward, board.setTileAt(player.nextPosition, Empty))
    else
      Update(player.setState(IsStopped), board)

case class SsorStart(direction: Direction) extends Tile:
  /** The value is 14 */
  val id: Int    = 14
  val mode: Mode = Default
  val pushable   = false
  override def apply(player: Player, board: Board): Update =
    if (player.state == IsMoving(direction.opposite))
      Update(player.forward.setState(IsMoving(direction)), board)
    else
      Update(player.setState(IsStopped), board)

case class SsorEnd(direction: Direction) extends Tile:
  /** The value is 15 */
  val id: Int    = 15
  val mode: Mode = Default
  val pushable   = false
  override def apply(player: Player, board: Board): Update =
    if (player.state == IsMoving(direction))
      Update(player.forward.setState(HasFinished), board)
    else
      Update(player.setState(IsStopped), board)
