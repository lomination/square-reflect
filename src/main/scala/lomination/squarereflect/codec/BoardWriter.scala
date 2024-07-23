package lomination.squarereflect.codec

import lomination.squarereflect.*

trait Writable[A]:
  extension (a: A) def write: String

object BoardWriter {
  def apply(board: Board): String = board.write

  given Writable[Board] with
    extension (board: Board)
      def write: String =
        val rows: String =
          board.rows
            .map(row =>
              row
                .drop(1)
                .foldLeft(Seq((1, row(0)))) { (acc, tile) =>
                  if (tile == acc.last._2)
                    acc.dropRight(1) :+ (acc.last._1 + 1, tile)
                  else
                    acc :+ (1, tile)
                }
                .map(tuple => tuple._2.write * tuple._1)
                .mkString(",")
            )
            .mkString(";")
        val starts: String =
          board.starts.map(start => s"${start._1.x}-${start._1.y}-${start._2.write}").mkString
        val outsideTile: String = board.outsideTile.write
        val lightLevel: String  = board.lightLevel.getOrElse("None").toString
        val info: BoardInfo     = board.info
        Seq(
          "encodingVersion = 0.1.0",
          s"rows = $rows",
          s"starts = $starts",
          s"outsideTile = $outsideTile",
          s"lightLevel = $lightLevel",
          s"name = ${info.name}",
          s"author = ${info.author}",
          s"date = ${info.date}"
        ).mkString("\n")

  given Writable[Tile] with
    extension (tile: Tile)
      def write: String =
        tile match
          case Empty                                => "0"
          case Block(mode)                          => s"1-${mode.write}"
          case Angle(direction, mode)               => s"2-${direction.write}-${mode.write}"
          case End(mode)                            => s"3-${mode.write}"
          case Kill(mode)                           => s"4-${mode.write}"
          case Cannon(mode)                         => s"5-${mode.write}"
          case Portal(portalId, pairPosition, mode) => s"6-${portalId}-${pairPosition.x}-${pairPosition.y}-${mode.write}"
          case Arrow(direction, mode)               => s"7-${direction.write}-${mode.write}"
          case Blocker(direction, mode)             => s"8-${direction.write}-${mode.write}"
          case Tunnel(direction, mode)              => s"9-${direction.write}-${mode.write}"
          case Spike(direction, mode)               => s"10-${direction.write}-${mode.write}"
          case DoubleSpike(direction, mode)         => s"11-${direction.write}-${mode.write}"
          case Key                                  => "12"
          case Lock                                 => "13"
          case SsorStart(direction)                 => s"14-${direction.write}"
          case SsorEnd(direction)                   => s"15-${direction.write}"

  given Writable[Direction] with
    extension (direction: Direction)
      def write: String =
        direction match
          case Direction.North => "N"
          case Direction.South => "S"
          case Direction.East  => "E"
          case Direction.West  => "W"

  given Writable[Mode] with
    extension (mode: Mode)
      def write: String =
        mode match
          case Default      => "D"
          case Ghost(count) => s"G$count"
          case Weak(count)  => s"W$count"

  given Writable[Boolean] with
    extension (boolean: Boolean)
      def write: String =
        boolean match
          case true  => "T"
          case false => "F"
}
