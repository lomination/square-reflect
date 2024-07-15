package lomination.squarereflect

import scala.languageFeature.experimental.macros


case class Player(positions: Seq[Position], state: State, keys: Int, moves: Int, tile: Option[Tile]):
  def setPositions(positions: Seq[Position]): Player =
    Player(positions, state, keys, moves, tile)
  def setPlayerState(state: State): Player =
    Player(positions, state, keys, moves, tile)
  def setKeys(keys: Int): Player =
    Player(positions, state, keys, moves, tile)
  def clearPositions: Player =
    Player(Seq(positions.last), state, keys, moves, tile)
  def isMoving: Boolean = state.isInstanceOf[State.IsMoving]
  def isStopped: Boolean = state == State.IsStopped
  def isDead: Boolean = state == State.IsDead
  def hasFinished: Boolean = state == State.HasFinished
  def forward: Player =
    state match
      case State.IsMoving(direction) => setPositions(positions :+ positions.last.forward(direction))
      case _ => throw UnsupportedOperationException("Cannot move a non-`IsMoving` player with `.forward`")
  def move(direction: Direction): Player =
    Player(positions, State.IsMoving(direction), keys, moves + 1, tile)

object Player:
  def init(position: Position, direction: Direction): Player =
    Player(Seq(position), State.IsMoving(direction), 0, 0, None)

case class Position(x: Int, y: Int):
  infix def is(x: Int, y: Int): Boolean = this.x == x && this.y == y
  def forward(direction: Direction) =
    direction match
      case Direction.North => Position(x, y - 1)
      case Direction.South => Position(x, y + 1)
      case Direction.East => Position(x + 1, y)
      case Direction.West => Position(x - 1, y)

enum State:
  case IsMoving(direction: Direction)
  case IsStopped, IsDead, HasFinished

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
