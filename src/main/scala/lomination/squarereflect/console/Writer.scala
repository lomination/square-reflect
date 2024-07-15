package lomination.squarereflect.console

import lomination.squarereflect.*
import lomination.squarereflect.console.*
import scala.io.AnsiColor.{BLINK, WHITE, BLUE, GREEN, INVISIBLE, BLACK, RESET, BLACK_B, RED_B, GREEN_B, YELLOW_B, BLUE_B, MAGENTA_B, CYAN_B, WHITE_B}
import lomination.squarereflect.console.Colour.white

trait Writable[A]:
  extension (a: A) def write: RichString

object Writer:
  given Writable[Game] with
    extension (game: Game)
      def write: RichString =
        // def idToColour(id: Int): String =
        //   id match
        //     case 0 => RED_B
        //     case 1 => YELLOW_B
        //     case 2 => GREEN_B
        //     case 3 => CYAN_B
        //     case 4 => BLUE_B
        //     case 5 => MAGENTA_B
        //     case _ => WHITE_B
        (
          for (y <- 0 until game.height) yield (
            if (!game.players.exists(_.positions.last.y == y)) then
              // write the full row
              val row = game.board.rows(y).map(_.write)
              Seq(BLACK_B + game.board.rows(y).map(_.write)(0)) ++ game.board.rows(y).map(_.write).drop(1)
            else
              for (x <- 0 until game.width) yield
                val writtenTile = game.board.rows(y)(x).write
                game.players.indexWhere(_.positions.last.x == x) match
                  case -1 => BLACK_B + writtenTile // no player here
                  case id: Int => // player here
                    val color = writtenTile.substring(0, writtenTile.size - 3)
                    val left = writtenTile(writtenTile.size - 3)
                    val right = writtenTile(writtenTile.size - 1)
                    val bgColor = idToColour(id)
                    bgColor + color + left + BLACK + "•" + color + right
          ).mkString
        ).mkString(RESET + "\n") + RESET

  given Writable[Tile] with
    extension (tile: Tile)
      def write: String =
        val colour = tile.tileType match
          case Default                   => Format.default
          case g: Ghost if g.isAvailable => Format.default
          case g: Ghost                  => Format(white) 
          case w: Weak if w.isAvailable  => GREEN
          case w: Weak                   => INVISIBLE
        val writtenTile = tile match
          case Empty             => """   """
          case Block(_)          => """[ ]"""
          case Angle(North, _)   => """|\,"""
          case Angle(South, _)   => """|/`"""
          case Angle(East, _)    => """`\|"""
          case Angle(West, _)    => """,/|"""
          case End(_)            => """END"""
          case Kill(_)           => """ X """
          case Cannon(_)         => """(+)"""
          case Portal(id, _, _)  => s"""($id)"""
          case Arrow(North, _)   => """ ↑ """
          case Arrow(South, _)   => """ ↓ """
          case Arrow(East, _)    => """ → """
          case Arrow(West, _)    => """ ← """
          case Blocker(North, _) => """BLN"""
          case Blocker(South, _) => """BLS"""
          case Blocker(East, _)  => """BLE"""
          case Blocker(West, _)  => """BLW"""
          case Tunnel(North, _)  => """TUN"""
          case Tunnel(South, _)  => """TUS"""
          case Tunnel(East, _)   => """TUE"""
          case Tunnel(West, _)   => """TUW"""
        colour + writtenTile
