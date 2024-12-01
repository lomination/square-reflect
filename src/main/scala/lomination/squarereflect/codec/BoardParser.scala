package lomination.squarereflect.codec

import lomination.squarereflect.*
import lomination.squarereflect.util.{toGrid, toPositionedSeq, toTry}
import scala.io.Source
import scala.util.{Failure, Success, Try}

/** The global object for parsing square reflect boards from every supported file format */
object BoardParser {

  /** Loads the given file and tries to parse it via sbr format. If it failes, then tries Ssor's format.
    *
    * @param filename
    *   the file to parse into a square reflect board
    * @return
    *   a try of board containing either the successfully parsed board or the exception wrapped in a failure
    */
  def apply(filename: String): Try[Board] =
    fromSrbFile(filename) match
      case s: Success[Board] => s
      case _                 => fromSsorFormatFile(filename)

  /** Loads the given file and tries to parse it via sbr format
    *
    * @param filename
    *   the file to parse into a square reflect board
    * @return
    *   a try of board containing either the successfully parsed board or the exception wrapped in a failure
    */
  def fromSrbFile(filename: String): Try[Board] =
    val lines   = Source.fromFile(filename).getLines
    val cfgLine = """(\w+) *= *([\S\s]*)""".r
    val cfg: Map[String, String] = lines.foldLeft(Map[String, String]()) { (map, line) =>
      line match
        case cfgLine(key, value) => map + (key -> value)
        case _                   => map
    }
    cfg.get("encodingVersion") match
      case Some("0.1.0") => `BoardParserV0.1.0`(cfg)
      case Some(version) => Failure(ParsingError(s"The given version `$version` is not supported."))
      case None          => Failure(ParsingError("The key `encodingVersion` is missing from the given board."))

  /** Loads the given file and tries to parse it via Ssor's format
    *
    * @param filename
    *   the file to parse into a square reflect board
    * @return
    *   a try of board containing either the successfully parsed board or the exception wrapped in a failure
    */
  def fromSsorFormatFile(filename: String): Try[Board] =
    BoardParserSsorFormat(filename: String)
}

/** The object for parsing square reflect boards from srb format */
object `BoardParserV0.1.0` {

  /** The requiered config keys to parse a board encoded with encoding version 0.1.0 */
  val requiredKeys: Seq[String] = Seq("rows", "starts", "outsideTile", "lightLevel", "name", "author", "date")

  /** Loads the
    *
    * @param filename
    * @return
    */
  def apply(filename: String): Try[Board] =
    val lines   = Source.fromFile(filename).getLines
    val cfgLine = """(\w+) *= *([\S\s]*)""".r
    val cfg: Map[String, String] = lines.foldLeft(Map[String, String]()) { (map, line) =>
      line match
        case cfgLine(key, value) => map + (key -> value)
        case _                   => map
    }
    cfg.get("encodingVersion") match
      case Some("0.1.0") => apply(cfg)
      case Some(version) => Failure(ParsingError(s"Cannot parse version $version using board parser version 0.1.0"))
      case None          => Failure(ParsingError("The key `encodingVersion` is missing from the given board."))

  /** Tries to parse the given config into a board */
  def apply(cfg: Map[String, String]): Try[Board] =
    if (requiredKeys.exists(!cfg.contains(_)))
      Failure(ParsingError("A required key in the board encoding version 0.1.0 is missing from the given board."))
    else
      val multipleTilesRegex = """(\d+)\*([\S\s]+)""".r
      val startRegex         = """(\d+)-(\d+)-([NSEW])""".r
      val rowsTry: Try[Seq[Seq[Tile]]] = cfg("rows")
        .split(";")
        .toSeq
        .map(
          _.split(",").toSeq
            .flatMap {
              case multipleTilesRegex(times, tile) => List.fill(times.toInt)(tile.toTile)
              case tile: String                    => Seq(tile.toTile)
            }
            .toTry
        )
        .toTry
      val startsTry: Try[Seq[(Position, Direction)]] = cfg("starts")
        .split(",")
        .toSeq
        .map {
          case startRegex(x, y, direction) => Success(Position(x.toInt, y.toInt), direction.toDirection)
          case input                       => Failure(ParsingError(s"Cannot parse `$input` as a start position."))
        }
        .toTry
      val outsideTileTry: Try[Tile] = cfg("outsideTile").toTile
      val lightLevel: Option[Int]   = cfg("lightLevel").toIntOption
      val info: BoardInfo           = BoardInfo(cfg("name"), cfg("author"), cfg("date"))
      for {
        rows        <- rowsTry
        starts      <- startsTry
        outsideTile <- outsideTileTry
      } yield Board(rows, starts, outsideTile, lightLevel, info)

  extension (s: String)
    def toTile: Try[Tile] =
      val block       = """1-(D|G\d+|W\d+)-(T|F)""".r
      val angle       = """2-([NSEW])-(D|G\d+|W\d+)-(T|F)""".r
      val end         = """3-(D|G\d+|W\d+)""".r
      val kill        = """4-(D|G\d+|W\d+)""".r
      val cannon      = """5-(D|G\d+|W\d+)""".r
      val portal      = """6-([\S\s])-(\d+)-(\d+)-(D|G\d+|W\d+)""".r
      val arrow       = """7-([NSEW])-(D|G\d+|W\d+)""".r
      val blocker     = """8-([NSEW])-(D|G\d+|W\d+)""".r
      val tunnel      = """9-([NSEW])-(D|G\d+|W\d+)""".r
      val spike       = """10-([NSEW])-(D|G\d+|W\d+)""".r
      val doubleSpike = """11-([NSEW])-(D|G\d+|W\d+)""".r
      val lock        = """13-(T|F)""".r
      val ssorStart   = """14-([NSEW])""".r
      val ssorEnd     = """15-([NSEW])""".r
      s match
        case "0"                              => Success(Empty)
        case block(mode, pushable)            => Success(Block(mode.toMode, pushable.toBoolean))
        case angle(direction, mode, pushable) => Success(Angle(direction.toDirection, mode.toMode, pushable.toBoolean))
        case end(mode)                        => Success(End(mode.toMode))
        case kill(mode)                       => Success(Kill(mode.toMode))
        case cannon(mode)                     => Success(Cannon(mode.toMode))
        case portal(portalId, x, y, mode)     => Success(Portal(portalId(0), Position(x.toInt, y.toInt), mode.toMode))
        case arrow(direction, mode)           => Success(Arrow(direction.toDirection, mode.toMode))
        case blocker(direction, mode)         => Success(Blocker(direction.toDirection, mode.toMode))
        case tunnel(direction, mode)          => Success(Tunnel(direction.toDirection, mode.toMode))
        case spike(direction, mode)           => Success(Spike(direction.toDirection, mode.toMode))
        case doubleSpike(direction, mode)     => Success(DoubleSpike(direction.toDirection, mode.toMode))
        case "12"                             => Success(Key)
        case lock(pushable)                   => Success(Lock(pushable.toBoolean))
        case ssorStart(direction)             => Success(SsorStart(direction.toDirection))
        case ssorEnd(direction)               => Success(SsorEnd(direction.toDirection))
        case _                                => Failure(ParsingError(s"Tile not found: `$s`"))

    def toMode: Mode =
      val ghost = """G(\d+)""".r
      val weak  = """W(\d+)""".r
      s match
        case "D"          => Default
        case ghost(count) => Ghost(count.toInt)
        case weak(count)  => Weak(count.toInt)
        case _            => throw ParsingError()

    def toDirection: Direction =
      s match
        case "N" => Direction.North
        case "S" => Direction.South
        case "E" => Direction.East
        case "W" => Direction.West
        case _   => throw ParsingError()

    def toBoolean: Boolean =
      s match
        case "T" => true
        case "F" => false
        case _   => throw ParsingError()
}

object BoardParserSsorFormat {

  def apply(filename: String): Try[Board] =
    val lines: Iterator[String]      = Source.fromFile(filename).getLines
    val rowsTry: Try[Seq[Seq[Tile]]] = lines.map(_.split(",").toSeq.map(_.trim.toTile).toTry).toSeq.toTry
    if (rowsTry.isFailure) Failure(rowsTry.failed.get)
    else
      val rows: Seq[Seq[Tile]] = rowsTry.get
      if (rows.exists(_.sizeCompare(rows(0)) != 0)) Failure(ParsingError("Not all lines have the same size"))
      else
        val positionedRows: Seq[(Tile, Position)] = rows.toPositionedSeq
        val portals: Seq[(Portal, Position)] =
          positionedRows.filter(_._1.isInstanceOf[Portal]).asInstanceOf[Seq[(Portal, Position)]]
        val computedRows: Try[Seq[Seq[Tile]]] =
          if (portals.isEmpty) Success(rows)
          else
            positionedRows
              .map {
                case (Portal(id, _, mode), position) =>
                  portals.collectFirst { case (Portal(id, _, _), p) if p != position => p } match
                    case Some(pos) => Success(Portal(id, pos, mode), position)
                    case None => Failure(ParsingError(s"The portal with id $id and position $position has no pair"))
                case (tile, position) =>
                  Success(tile, position)
              }
              .toTry
              .map(_.toGrid)
        if (computedRows.isFailure) Failure(computedRows.failed.get)
        else
          val finalRows: Seq[Seq[Tile]] = computedRows.get
          val starts: Seq[(Position, Direction)] =
            positionedRows.filter { _._1.isInstanceOf[SsorStart] } map { case (SsorStart(direction), position) =>
              (position, direction)
            }
          if (starts.sizeIs == 0) Failure(ParsingError("No start tile not found"))
          else
            Success(Board(finalRows, starts, Block(), None, BoardInfo.empty))

  extension (s: String)
    def toTile: Try[Tile] =
      val ssorStart  = """"\?([1-4])"""".r
      val ssorEnd    = """"!([1-4])"""".r
      val angle      = """"A([1-4])"""".r
      val ghostAngle = """"a([1-4])"""".r
      val portal     = """"p([a-e])"""".r
      s match
        case "\"00\""              => Success(Empty)
        case "\"01\""              => Success(Block())
        case "\"02\""              => Success(Block(Ghost(1)))
        case "\"03\""              => Success(Key)
        case "\"04\""              => Success(Lock())
        case "\"05\""              => Success(Kill())
        case "\"06\""              => Success(Cannon())
        case ssorStart(direction)  => Success(SsorStart(direction.toDirection))
        case ssorEnd(direction)    => Success(SsorEnd(direction.toDirection))
        case angle(direction)      => Success(Angle(direction.toDirection.next))
        case ghostAngle(direction) => Success(Angle(direction.toDirection.next, Ghost(1)))
        case portal(portalId)      => Success(Portal(portalId(0), Position(-1, -1)))
        case _                     => Failure(ParsingError(s"Given tile $s not found"))

    def toDirection: Direction =
      Direction.fromOrdinal(s.toInt - 1)
}
