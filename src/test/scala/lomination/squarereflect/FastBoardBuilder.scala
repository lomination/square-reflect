package lomination.squarereflect

import scala.util.Try

import lomination.squarereflect.{Empty, Tile}
import lomination.squarereflect.util.toTry
import lomination.squarereflect.codec.ParsingError
import lomination.squarereflect.codec.`BoardParserV0.1.0`.toTile

object FastBoardBuilder:
  /** x and y: start position */
  def apply(rows: String, outsideTile: Tile = Empty): Board =
    val t: Try[Seq[Seq[Tile]]] = rows.split(";").toSeq.map(_.split(",").toSeq.map(_.trim.toTile).toTry).toTry
    if (t.isSuccess)
      Board(t.get, Seq(), outsideTile, None, BoardInfo.empty)
    else
      throw ParsingError("Failed to parse board in this test", t.failed.get)
