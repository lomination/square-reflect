package lomination.squarereflect.codec

import lomination.squarereflect.*
import scala.io.Source
import scala.util.{Try, Success, Failure}

object BoardParser {

  def apply(filename: String): Try[Board] =
    fromFile(filename)

  def fromFile(filename: String): Try[Board] =
    val lines   = Source.fromFile(filename).getLines
    val cfgLine = """(\w+) *= *([\S\s]*)""".r
    val cfg: Map[String, String] = lines.foldLeft(Map[String, String]()) { (map, line) =>
      line match
        case cfgLine(key, value) => map + (key -> value)
        case _                   => map
    }
    cfg.get("encodingVersion") match
      case Some("0.1.0") => `BoardParserV0.1.0`(cfg)
      case Some(version) => Failure(Exception(s"The given version `$version` is not supported."))
      case None          => Failure(Exception("The key `encodingVersion` is missing from the given board."))
}

object `BoardParserV0.1.0` {

  /** The requiered config keys to parse a board encoded with encoding version 0.1.0 */
  val requiredKeys: Seq[String] = Seq("rows", "starts", "outsideTile", "lightLevel", "name", "author", "date")

  /** Tries to parse the given config into a board */
  def apply(cfg: Map[String, String]): Try[Board] =
    if (requiredKeys.exists(!cfg.contains(_))) Failure(Exception("A required key in the board encoding version 0.1.0 is missing from the given board."))
    else
      val multipleTiles = """(\d+)\*([\S\s]+)""".r
      val rows: Seq[Seq[Tile]] = cfg("rows")
        .split(";")
        .toSeq
        .map(_.split(",").toSeq.foldLeft(Seq()) { (row, tile) =>
          tile match
            case multipleTiles(times, tile) => row ++ List.fill(times.toInt)(tile.toTile.get)
            case tile: String               => row :+ tile.toTile.get
        })
      val startR = """(\d+)-(\d+)-([NSEW])""".r
      val starts: Seq[(Position, Direction)] = cfg("starts").split(",").foldLeft(Seq()) { (acc, start) =>
        start match
          case startR(x, y, direction) => acc :+ (Position(x.toInt, y.toInt), direction.toDirection)
          case _                       => acc
      }
      val outsideTile: Tile       = cfg("outsideTile").toTile.get
      val lightLevel: Option[Int] = cfg("lightLevel").toIntOption
      val info: BoardInfo         = BoardInfo(cfg("name"), cfg("author"), cfg("date"))
      Success(Board(rows, starts, outsideTile, lightLevel, info))

  extension (s: String)
    def toTile: Try[Tile] =
      val block       = """1-(D|G\d+|W\d+)""".r
      val angle       = """2-([NSEW])-(D|G\d+|W\d+)""".r
      val end         = """3-(D|G\d+|W\d+)""".r
      val kill        = """4-(D|G\d+|W\d+)""".r
      val cannon      = """5-(D|G\d+|W\d+)""".r
      val portal      = """6-([\S\s])-(\d+)-(\d+)-(D|G\d+|W\d+)""".r
      val arrow       = """7-([NSEW])-(D|G\d+|W\d+)""".r
      val blocker     = """8-([NSEW])-(D|G\d+|W\d+)""".r
      val tunnel      = """9-([NSEW])-(D|G\d+|W\d+)""".r
      val spike       = """10-([NSEW])-(D|G\d+|W\d+)""".r
      val doubleSpike = """11-([NSEW])-(D|G\d+|W\d+)""".r
      val ssorStart   = """14-([NSEW])""".r
      val ssorEnd     = """15-([NSEW])""".r
      s match
        case "0"                          => Success(Empty)
        case block(mode)                  => Success(Block(mode.toMode))
        case angle(direction, mode)       => Success(Angle(direction.toDirection, mode.toMode))
        case end(mode)                    => Success(End(mode.toMode))
        case kill(mode)                   => Success(Kill(mode.toMode))
        case cannon(mode)                 => Success(Cannon(mode.toMode))
        case portal(portalId, x, y, mode) => Success(Portal(portalId(0), Position(x.toInt, y.toInt), mode.toMode))
        case arrow(direction, mode)       => Success(Arrow(direction.toDirection, mode.toMode))
        case blocker(direction, mode)     => Success(Blocker(direction.toDirection, mode.toMode))
        case tunnel(direction, mode)      => Success(Tunnel(direction.toDirection, mode.toMode))
        case spike(direction, mode)       => Success(Spike(direction.toDirection, mode.toMode))
        case doubleSpike(direction, mode) => Success(DoubleSpike(direction.toDirection, mode.toMode))
        case "12"                         => Success(Key)
        case "13"                         => Success(Lock)
        case ssorStart(direction)         => Success(SsorStart(direction.toDirection))
        case ssorEnd(direction)           => Success(SsorEnd(direction.toDirection))
        case _                            => Failure(Exception(s"Tile not found: `$s`"))

    def toMode: Mode =
      val ghost = """G(\d+)""".r
      val weak  = """W(\d+)""".r
      s match
        case "D"          => Default
        case ghost(count) => Ghost(count.toInt)
        case weak(count)  => Weak(count.toInt)
        case _            => throw Exception()

    def toDirection: Direction =
      s match
        case "N" => Direction.North
        case "S" => Direction.South
        case "E" => Direction.East
        case "W" => Direction.West
        case _   => throw Exception()
}

// def fromSsor(board: String): Board =
//   def convertTile(tile: String): Tile =
//     def extract(dir: String) = tile.last match
//       case 1 => Direction.North
//       case 2 => Direction.East
//       case 3 => Direction.south
//       case 4 => Direction.West
//       case _ => throw Exception()
//     (s"$tile(0)", s"$tile(1)") match
//       case ("0", "0") => Empty
//       case ("0", "1") => Block()
//       case ("0", "2") => Block(Ghost(1))
//       case ("0", "3") => Key
//       case ("0", "4") => Lock
//       case ("0", "5") => Kill()
//       case ("?", dir) => Empty
//       case ("!", dir) => End()
//       case ("A", dir) => Angle(extract(dir), Default)
//       case ("a", dir) => Angle(extract(dir), Ghost(1))
//       case _          => Empty
//   val rows     = board.split("\n").map(line => line.split(",").map(tile => convertTile(tile)).toSeq).toSeq
//   val splitted = board.split("\n").map(line => line.split(","))
//   Board(rows, starts)
