package lomination.squarereflect.console

import org.jline.terminal.*
import lomination.squarereflect.*
import lomination.squarereflect.console.Style.*

case class ConsoleApp(game: Game):

  def run: Game = run(System.currentTimeMillis)

  def run(lastTick: Long): Game =
    if (System.currentTimeMillis >= lastTick + 500)
      if (game.players(0).hasFinished) // (game.players.forall(p => !p.isActive))
        display("You won!")
        game
      else if (game.players(0).isDead)
        display("You losed!")
        game
      else if (game.players(0).isMoving)
        display
        print("MOVIIIIIIIIIIIIIIIIIIIIIIIING")
        ConsoleApp(game.computeOnce).run
      else // if (game.players(0).isStopped)
        display
        print("\u001b[2K" + "> ")
        val in = scala.io.StdIn.readLine.toLowerCase
        if (Seq("n", "north", "up", "w", "^[[A").contains(in))
          ConsoleApp(game.move(0, Direction.North).computeOnce).run
        else if (Seq("s", "south", "down", "s", "^[[B").contains(in))
          ConsoleApp(game.move(0, Direction.South).computeOnce).run
        else if (Seq("e", "east", "right", "d", "^[[C").contains(in))
          ConsoleApp(game.move(0, Direction.East).computeOnce).run
        else if (Seq("w", "west", "left", "a", "^[[D").contains(in))
          ConsoleApp(game.move(0, Direction.West).computeOnce).run
        else if (Seq("quit", "q", "^[").contains(in))
          game
        else
          run(lastTick)
    else
      Thread.sleep(50)
      run(lastTick)

  def display(msg: String): Unit =
    print(s"\u001b[${game.width + 99}D\u001b[${game.height + 1}A")
    println(drawGame(game))

  def display: Unit = display("")

  def drawGame(game: Game): String =
    (
      for y <- 0 until game.height
      yield (
        for x <- 0 until game.width yield
          val pos: Position = Position(x, y)
          val tile: String = drawTile(game(x, y))
          // Equals either 0 or 1. When 0 the tile will have a black background, else a white one.
          val paving: Int = (x + 1) * (if (y % 2 == 0) y + 1 else y + 2) % 2
          if (game.board.lightLevel.isDefined && game.players.forall(_.position.distTo(pos) >= game.board.lightLevel.get.toDouble))
            // the tile cannot be seen
            val v = 255 * paving
            s"${Bg(v,v,v)}${Fg(v,v,v)}$tile$Reset"
          else
            val style: String = game(x, y).mode match
              case Default                   => s"${Fg(0,0,0)}"
              case g: Ghost if g.isAvailable => s"${Fg(0,0,0)}"
              case g: Ghost                  => s"${Fg(128,128,128)}"
              case w: Weak if w.isAvailable  => s"${Fg(0,0,0)}$Blink"
              case w: Weak                   => s"$Invisible"
            if (game.players.exists(_.position == pos))
              val bg: Style = bFromHue(game.players.indexWhere(_.position == pos).toFloat / (game.players.size + 1).toFloat)
              s"$style${tile(0)}$Reset$style$bg$FBlack•$Reset$style$bg$fg${tile(2)}$Reset"
            else s"$style$BWhite$fg$tile$Reset"
      ).mkString
    ).mkString("\n")

  def drawTile(tile: Tile): String =
    tile match
      case Empty            => "   "
      case Block(_)         => "[ ]"
      case Angle(dir, _)    => Seq("""|\,""", "|/`", """`\|""", ",/|")(dir.ordinal)
      case End(_)           => " ⚑ "
      case Kill(_)          => " X "
      case Cannon(_)        => "(+)"
      case Portal(id, _, _) => s"($id)"
      case Arrow(dir, _)    => Seq(" ↑ ", " → ", " ↓ ", " ← ")(dir.ordinal)
      case Blocker(dir, _)  => Seq(" ⇈ ", " ⇉ ", " ⇊ ", " ⇇ ")(dir.ordinal)
      case Tunnel(dir, _)   => Seq("⇄", "⇅")(dir.ordinal % 2)
      case Key              => " K "
      case Lock             => "[L]"
      case t: Tile =>
        val id: String = s"0${t.id}".slice(s"0${t.id}".size - 2, s"0${t.id}".size)
        val dir: Char  = t.direction.toString()(0)
        s"$id$dir"

object ConsoleApp:

  def init(board: Board): ConsoleApp =
    ConsoleApp(Game.init(board))
