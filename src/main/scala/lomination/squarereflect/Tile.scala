package lomination.squarereflect


import lomination.squarereflect.*
import lomination.squarereflect.Direction.{North, South, East, West}


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
  def apply(player: Player): Update = Update(player.forward)

    

// Mode (trait)              { def isAvailable }
// ├── Default               { def isAvailable }
// └── NonDefault (abstract) { def isAvailable; val count; def use }
//     ├── Ghost             { def isAvailable; val count; def use }
//     └── Weak              { def isAvailable; val count; def use }

sealed trait Mode:
  def isAvailable: Boolean

object Default extends Mode:
  def isAvailable: Boolean = true

sealed abstract class NonDefault(val count: Int) extends Mode:
  def use: Mode
  def isUsed: Boolean = count <= 0

object NonDefault:
  def unapply(nonDefault: NonDefault): Option[Int] = Some(nonDefault.count)

case class Ghost(override val count: Int) extends NonDefault(count):
  def isAvailable: Boolean = count > 0
  def use: Ghost = Ghost(count - 1)

case class Weak(override val count: Int) extends NonDefault(count):
  def isAvailable: Boolean = count <= 0
  def use: Weak = Weak(count - 1)


// ----------- Tiles -------------- //


case object Empty extends Tile:
  /** The value is 0 */
  val id: Int = 0
  val direction: Direction = North
  val mode: Mode = Default
  val pushable: Boolean = false


case class Block(mode: Mode = Default) extends Tile:
  /** The value is 1 */
  val id: Int = 1
  val direction: Direction = North
  val pushable: Boolean = false
  override def apply(player: Player): Update =  
    val newPlayer: Player = player.setState(IsStopped)    
    mode match
      case Default =>
        Update(player.setState(IsStopped))
      case Ghost(1) =>
        Update(player.forward, player.nextPosition, Block())
      case Weak(1) =>
        Update(newPlayer, player.nextPosition, Empty)
      case nd: NonDefault if nd.isAvailable =>
        Update(player.setState(IsStopped), player.nextPosition, Block(nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Block(nd.use))


case class Angle(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 2 */
  val id: Int = 2
  val pushable: Boolean = false
  override def apply(player: Player): Update =      
    val newPlayer: Player =
      if (player.state == IsMoving(direction) || player.state == IsMoving(direction.next))
        player.setState(IsStopped)
      else if (player.state == IsMoving(direction.opposite))
        player.forward.setState(IsMoving(direction.next))
      else if (player.state == IsMoving(direction.previous))
        player.forward.setState(IsMoving(direction))
      else // should not happen (apply should always be on moving players)
        player
    mode match
      case Default =>
        Update(newPlayer)
      case nd: NonDefault if nd.isAvailable =>
        Update(newPlayer, newPlayer.position, Angle(direction, nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Angle(direction, nd.use))


case class End(mode: Mode = Default) extends Tile:
  /** The value is 3 */
  val id: Int = 3
  val direction: Direction = North
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    mode match
      case Default =>
        Update(player.forward.setState(HasFinished))
      case nd: NonDefault if nd.isAvailable =>
        Update(player.forward.setState(HasFinished), player.nextPosition, End(nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, End(nd.use))


case class Kill(mode: Mode = Default) extends Tile:
  /** The value is 4 */
  val id: Int = 4
  val direction: Direction = North
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    mode match
      case Default => 
        Update(player.forward.setState(IsDead))
      case nd: NonDefault if nd.isAvailable =>
        Update(player.forward.setState(IsDead), player.nextPosition, Kill(nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Kill(nd.use))


case class Cannon(mode: Mode = Default) extends Tile:
  /** The value is 5 */
  val id: Int = 5
  val direction: Direction = North
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    mode match
      case Default =>
        Update(player.forward.setState(IsStopped))
      case nd: NonDefault if nd.isAvailable =>
        Update(player.forward.setState(IsStopped), player.nextPosition, Cannon(nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Cannon(nd.use))


case class Portal(portalId: Char, pairPosition: Position, mode: Mode = Default) extends Tile:
  /** The value is 6 */
  val id: Int = 6
  val direction: Direction = North
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    mode match
      case Default =>
        Update(player.forward.setPositions(player.positions :+ pairPosition))
      case nd: NonDefault if nd.isAvailable =>
        Update(player.forward.setPositions(player.positions :+ pairPosition), player.nextPosition, Portal(portalId, pairPosition, nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Portal(portalId, pairPosition, nd.use))


case class Arrow(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 7 */
  val id: Int = 7
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    mode match
      case Default =>
        Update(player.forward.setState(IsMoving(direction)))
      case nd: NonDefault if nd.isAvailable =>
        Update(player.forward.setState(IsMoving(direction)), player.nextPosition, Arrow(direction, nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Arrow(direction, nd.use))


case class Blocker(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 8 */
  val id: Int = 8
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    val newPlayer: Player =
      if (player.state == IsMoving(direction.opposite)) player.setState(IsStopped)
      else player.forward
    mode match
      case Default =>
        Update(newPlayer)
      case nd: NonDefault if nd.isAvailable =>
        Update(newPlayer, player.nextPosition, Blocker(direction, nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Blocker(direction, nd.use))


case class Tunnel(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 9 */
  val id: Int = 9
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    val newPlayer: Player =
      if (player.state == IsMoving(direction.next) || player.state == IsMoving(direction.previous)) player.setState(IsStopped)
      else player
    mode match
      case Default =>
        Update(newPlayer)
      case nd: NonDefault if nd.isAvailable =>
        Update(newPlayer, player.nextPosition, Tunnel(direction, nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player.forward, player.nextPosition, Tunnel(direction, nd.use))


case class Spike(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 10 */
  val id: Int = 10
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    val newPlayer: Player =
      if (player.state == direction) player.forward.setState(IsDead)
      else if (player.state == direction.opposite) player.setState(IsStopped)
      else player
    mode match
      case Default => 
        Update(newPlayer)
      case nd: NonDefault if nd.isAvailable =>
        Update(newPlayer, player.nextPosition, Spike(direction, nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player, player.nextPosition, Spike(direction, nd.use))


case class DoubleSpike(direction: Direction, mode: Mode = Default) extends Tile:
  /** The value is 11 */
  val id: Int = 11
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    val newPlayer: Player =
      if (player.state == direction || player.state == direction.next) player.forward.setState(IsDead)
      else player.setState(IsStopped)
    mode match
      case Default =>
        Update(newPlayer)
      case nd: NonDefault if nd.isAvailable =>
        Update(newPlayer, player.nextPosition, DoubleSpike(direction, nd.use))
      case nd: NonDefault /*if !nd.isAvailable*/ =>
        Update(player, player.nextPosition, DoubleSpike(direction, nd.use)) 


case object Key extends Tile:
  /** The value is 12 */
  val id: Int = 12
  val direction: Direction = North
  val mode: Mode = Default
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    Update(player.setKeys(player.keys + 1).forward, player.nextPosition, Empty)


case object Lock extends Tile:
  /** The value is 13 */
  val id: Int = 13
  val direction: Direction = North
  val mode: Mode = Default
  val pushable: Boolean = false
  override def apply(player: Player): Update =
    if (player.keys > 0)
      Update(player.setKeys(player.keys + 1).forward, player.nextPosition, Empty)
    else
      Update(player.setState(IsStopped))


case class SsorStart(direction: Direction) extends Tile:
  /** The value is 14 */
  val id: Int = 14
  val mode: Mode = Default
  val pushable = false
  override def apply(player: Player): Update =
    if (player.state == IsMoving(direction.opposite))
      Update(player.forward.setState(IsMoving(direction)))
    else
      Update(player.setState(IsStopped))


case class SsorEnd(direction: Direction) extends Tile:
  /** The value is 15 */
  val id: Int = 15
  val mode: Mode = Default
  val pushable = false
  override def apply(player: Player): Update =
    if (player.state == IsMoving(direction.opposite))
      Update(player.forward.setState(HasFinished))
    else
      Update(player.setState(IsStopped))
