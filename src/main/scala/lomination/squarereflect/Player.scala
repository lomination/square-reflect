package lomination.squarereflect




case class Player(positions: Seq[Position], state: State, keys: Int, moves: Int, tile: Option[Tile]):
  /** The last position occupied by this player. Returns `this.positions.last` */
  lazy val position: Position = positions.last
  /** Returns a new player with the given positions as this.positions. */
  def setPositions(positions: Seq[Position]): Player =
    Player(positions, state, keys, moves, tile)
  // @inline def setPositions(f: Seq[Position] => Seq[Position]): Player = setPositions(f(positions))
  def setState(state: State): Player = Player(positions, state, keys, moves, tile)
  // @inline def setState(f: State => State): Player = setState(f(state))
  def setKeys(keys: Int): Player = Player(positions, state, keys, moves, tile)
  // @inline def setKeys(f: Int => Int): Player = setKeys(_ => f(keys))
  /** Whether this player is moving */
  def isMoving: Boolean = state.isInstanceOf[IsMoving]
  /** Whether this player is stopped */
  def isStopped: Boolean = state == IsStopped
  /** Whether this player is dead */
  def isDead: Boolean = state == IsDead
  /** Whether this player has finished */
  def hasFinished: Boolean = state == HasFinished
  /** Whether this player neither is dead no has finished */
  def isActive: Boolean = !hasFinished && !isDead
  /** Returns this player's next position, only if the player is moving. Else throws an exception. */
  def nextPosition: Position =
    state match
      case IsMoving(direction) => position.next(direction)
      case _ => throw UnsupportedOperationException("Cannot get the next position of a non-`IsMoving` player with `.nextPosition` method.")
  /** Returns this with the given direction as `this.direction`. Also increments this player's number of move by 1.
   * It should be used only if this player's stats is `IsStopped`. */
  def move(direction: Direction): Player =
    Player(positions, IsMoving(direction), keys, moves + 1, tile)
  /** Only id this player's state is `IsMoving`, returns this players with the next position (according to its
   * direction) appended to its positions. Else, throws an exception. */
  def forward: Player =
    state match
      case IsMoving(direction) =>
        setPositions(positions :+ position.next(direction))
      case _ =>
        throw UnsupportedOperationException("Cannot move a non-`IsMoving` player with `.forward` method.")


object Player:
  /** Initialises a new player. */
  def init(position: Position, direction: Direction): Player =
    Player(Seq(position), IsMoving(direction), 0, 0, None)


case class Position(x: Int, y: Int):
  infix def is(x: Int, y: Int): Boolean = this.x == x && this.y == y
  infix def is(that: Position): Boolean = this == that
  def distTo(that: Position): Double =
    math.sqrt(math.pow(this.x - that.x, 2) + math.pow(this.y - that.y, 2))
  def next(direction: Direction) =
    direction match
      case Direction.North => Position(x, y - 1)
      case Direction.South => Position(x, y + 1)
      case Direction.East => Position(x + 1, y)
      case Direction.West => Position(x - 1, y)


trait State

case class IsMoving(direction: Direction) extends State

case object IsStopped extends State

case object IsDead extends State

case object HasFinished extends State


enum Direction:
  case North, East, South, West
  def next: Direction = this match
    case North => East
    case East => South
    case South => West
    case West => North
  def opposite: Direction = this match
    case North => South
    case East => West
    case South => North
    case West => East
  def previous: Direction = this match
    case North => West
    case East => North
    case South => East
    case West => South
