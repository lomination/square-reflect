package lomination.squarereflect


/**
  * Tiles' super class
  *
  * @param direction
  *   the direction of this tile
  * @param tileType
  *   the type of this tile
  */
abstract class Tile(val id: Int, val direction: Direction, val tileType: TileType):
  /** Determines if a player can interract with this tile */
  def isAvailable: Boolean = tileType.isAvailable
  /** Returns the given player after having applied this tile's behaviour on it */
  def apply(player: Player): Update = Update(player.forward, None)


// TileType { def isAvailable }
// ├── Default { def isAvailable }
// └── NonDefault { def isAvailable; val count; def use }
//     ├── Ghost  { def isAvailable; val count; def use }
//     └── Weak  { def isAvailable; val count; def use }

sealed trait TileType:
  def isAvailable: Boolean

object Default extends TileType:
  def isAvailable: Boolean = true

sealed abstract class NonDefault(val count: Int) extends TileType:
  def use: TileType

object NonDefault:
  def unapply(nonDefault: NonDefault): Option[Int] = Some(nonDefault.count)

case class Ghost(override val count: Int) extends NonDefault(count):
  def isAvailable: Boolean = count >= 0
  def use: Ghost = Ghost(count - 1)

case class Weak(override val count: Int) extends NonDefault(count):
  def isAvailable: Boolean = count <= 0
  def use: Weak = Weak(count - 1)


// ----------- Tiles -------------- //


case object Empty extends Tile(0, Direction.North, Default)


case class Block(override val tileType: TileType = Default) extends Tile(1, Direction.North, tileType):
  override def apply(player: Player): Update =      
    if (tileType.isAvailable)
      val newTile: Option[Tile] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Block(nd.use))
        case _ => None
      Update(player.setPositions(player.positions.dropRight(1)).setPlayerState(State.IsStopped), newTile)
    else
      super.apply(player)


case class Angle(override val direction: Direction, override val tileType: TileType = Default) extends Tile(2, direction, tileType):
  override def apply(player: Player): Update =      
    if (tileType.isAvailable)
      val newTile: Option[Angle] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Angle(direction, nd.use))
        case _ => None
      if (player.state == State.IsMoving(direction) || player.state == State.IsMoving(direction.next))
        Update(player.setPositions(player.positions.dropRight(1)).setPlayerState(State.IsStopped), newTile)
      else if (player.state == State.IsMoving(direction.opposite))
        Update(player.setPlayerState(State.IsMoving(direction.next)).forward, newTile)
      else // if (player.state == State.IsMoving(direction.previous))
        Update(player.setPlayerState(State.IsMoving(direction)).forward, newTile)
    else
      super.apply(player)


case class End(override val tileType: TileType = Default) extends Tile(3, Direction.North, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable)
      val newTile: Option[End] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(End(nd.use))
        case _ => None
      Update(player.setPlayerState(State.HasFinished), newTile)
    else
      super.apply(player)


case class Kill(override val tileType: TileType = Default) extends Tile(4, Direction.North, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable)
      val newTile: Option[Kill] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Kill(nd.use))
        case _ => None
      Update(player.setPlayerState(State.IsDead), newTile)
    else
      super.apply(player)


case class Cannon(override val tileType: TileType = Default) extends Tile(5, Direction.North, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable)
      val newTile: Option[Cannon] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Cannon(nd.use))
        case _ => None
      Update(player.setPlayerState(State.IsStopped), newTile)
    else
      super.apply(player)


case class Portal(portalId: Char, pairPosition: Position, override val tileType: TileType = Default) extends Tile(6, Direction.North, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable)
      val newTile: Option[Cannon] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Cannon(nd.use))
        case _ => None
      Update(player.setPositions(player.positions :+ pairPosition).forward, newTile)
    else
      super.apply(player)


case class Arrow(override val direction: Direction, override val tileType: TileType = Default) extends Tile(7, direction, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable)
      val newTile: Option[Arrow] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Arrow(direction, nd.use))
        case _ => None
      Update(player.setPlayerState(State.IsMoving(direction)).forward, newTile)
    else
      super.apply(player)


case class Blocker(override val direction: Direction, override val tileType: TileType = Default) extends Tile(8, direction, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable && player.state == State.IsMoving(direction.opposite))
      val newTile: Option[Blocker] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Blocker(direction, nd.use))
        case _ => None
      Update(player.setPlayerState(State.IsMoving(direction)), newTile)
    else
      super.apply(player)


case class Tunnel(override val direction: Direction, override val tileType: TileType = Default) extends Tile(9, direction, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable && (player.state == State.IsMoving(direction.next) || player.state == State.IsMoving(direction.previous)))
      val newTile: Option[Tunnel] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Tunnel(direction, nd.use))
        case _ => None
      Update(player.setPlayerState(State.IsMoving(direction)), newTile)
    else
      super.apply(player)


case class Spike(override val direction: Direction, override val tileType: TileType = Default) extends Tile(10, direction, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable && (player.state == direction || player.state == direction.opposite))
      val newTile: Option[Spike] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Kill(nd.use))
        case _ => None
      if (player.state == direction)
        // spike side => player is dead
        Update(player.setPlayerState(State.IsDead), newTile)
      else
        // wall ide => player is stopped at the prev pos
        Update(player.setPlayerState(State.IsStopped).setPositions(player.positions.dropRight(1)), newTile)
    else
      super.apply(player)


case class DoubleSpike(override val direction: Direction, override val tileType: TileType = Default) extends Tile(11, direction, tileType):
  override def apply(player: Player): Update =
    if (tileType.isAvailable)
      val newTile: Option[Spike] = tileType match
        case nd: NonDefault if nd.isAvailable => Some(Kill(nd.use))
        case _ => None
      if (player.state == direction || player.state == direction.next)
        // spike side => player is dead
        Update(player.setPlayerState(State.IsDead), newTile)
      else
        // wall side => player is stopped at the prev pos
        Update(player.setPlayerState(State.IsStopped).setPositions(player.positions.dropRight(1)), newTile)
    else
      super.apply(player)
