package lomination.squarereflect.console

// import org.jline.terminSal.*
import java.io.File
import scala.io.StdIn.readLine
import scala.util.{Success, Failure}
import lomination.squarereflect.*
import lomination.squarereflect.console.Event.*
import lomination.squarereflect.codec.BoardParser

object ConsoleApp:
  /** The defined controls of the console application.
    *   - n/e/s/w ti move up/right/dows/left
    *   - q to quit
    *   - z or u to undo the last performed move
    *   - r to restart
    */
  val controls: Map[String, Event] = Map(
    ("n" -> Move(Direction.North)),
    ("s" -> Move(Direction.South)),
    ("e" -> Move(Direction.East)),
    ("w" -> Move(Direction.West)),
    ("q" -> Quit),
    ("z" -> Undo),
    ("u" -> Undo),
    ("r" -> Reset)
  )

  /** Runs the menu */
  def run: Unit =
    val board = runMenu
    run(board)

  /** Runs the game on the given board.
    *
    * @param board
    *   the board to play on
    */
  def run(board: Board): Unit =
    val game = Game.init(board)
    ConsoleApp.run(Seq(game, game), getTime)

  /** Runs the game from the given game state.
    *
    * @param game
    *   the initial game state
    */
  def run(game: Game): Unit =
    ConsoleApp.run(Seq(game, game), getTime)

  /** Runs the game. This may not end if the player is in an infinite loop. */
  private def run(games: Seq[Game], tick: Long): Unit =
    val nextTick = tick + 400
    Thread.sleep(Math.max(nextTick - getTime, 0))
    val lastGame = games.last
    lastGame.players(0).state match
      case IsMoving(_) =>
        display(lastGame, "")
        run(games.dropRight(1) :+ lastGame.computeOnce, nextTick)
      case IsStopped =>
        display(lastGame, "")
        getInput match
          case Move(direction) =>
            run(games :+ lastGame.move(0, direction).computeOnce, getTime - 500)
          case Reset =>
            run(Seq(games(0), games(0)), getTime)
          case Undo =>
            run(games.dropRight(1), getTime)
          case Quit =>
            ()
      case HasFinished =>
        display(lastGame, "You won!")
      case IsDead =>
        display(lastGame, "You losed!")

  /** Runs the game menu to choose a board. */
  def runMenu: Board =
    val workingDir = s"${System.getProperty("user.dir")}/boards/"
    val boards = new File(workingDir).listFiles.filter(_.isFile).map(_.getName).toSeq
    boards.zipWithIndex.foreach { case (file, i) => println(s"${i+1} - $file") }
    print("Choose a board:\n> ")
    readLine.toLowerCase.trim.toIntOption match
      case Some(i) =>
        BoardParser(workingDir + boards(i - 1)) match
          case Success(value) => value
          case Failure(exception) => println(exception.getMessage)
      case None => ()

  /** Asks inputs to the user. Then returns the corresponding event or retry. */
  def getInput: Event =
    print("\u001b[1A\u001b[2K> ")
    val in = readLine.toLowerCase.trim
    controls.get(in) match
      case Some(event) => println("\u001b[1A"); event
      case None        => getInput

  /** Returns the current time in milliseconds
    *
    * @return
    *   ```
    *   System.currentTimeMillis
    *   ```
    */
  @inline private def getTime: Long = System.currentTimeMillis

  /** Prints the given game state to the default output stream
    *
    * @param game
    *   the game state to print
    * @param msg
    *   an additional message to print
    */
  def display(game: Game, msg: String = ""): Unit =
    print(s"\u001b[${game.height + 1}A")
    println(drawGame(game))
    println(msg)

  def drawGame(game: Game): String =
    import lomination.squarereflect.console.Style.*
    (
      for y <- 0 until game.height
      yield (
        for x <- 0 until game.width yield
          game.lookAt(x, y) match
            case None => 
              val bg: Style = Bg(255, 255, 255)
              s"$bg   $Reset"
            case Some(tile) =>
              val written: String = drawTile(tile)
              val bg: Style = tile.pushable match
                case true => Bg(255, 199, 6)
                case _    =>  Bg(0, 0, 0)
              val fg: Style = tile.mode match
                case Default                   => Nothing
                case g: Ghost if g.isAvailable => Nothing
                case g: Ghost                  => Fg(102, 102, 102)
                case w: Weak if w.isAvailable  => Blink
                case w: Weak                   => Fg(0, 0, 0)
              if (game.players.exists(_.position is (x, y)))
                val colour = fgFromHue(game.players.indexWhere(_.position is (x, y)).toFloat / game.players.size.toFloat)
                s"$bg$fg${written(0)}$Reset$bg$colour•$Reset$bg$fg${written(2)}$Reset"
              else
                s"$bg$fg$written$Reset"
      ).mkString
    ).mkString("\n")

  def drawTile(tile: Tile): String =
    tile match
      case Empty            => "   "
      case Block(_, _)      => "[ ]"
      case Angle(dir, _, _) => Seq("""|\,""", "|/`", """`\|""", ",/|")(dir.ordinal)
      case End(_)           => " ⚑ "
      case Kill(_)          => " X "
      case Cannon(_)        => "(+)"
      case Portal(id, _, _) => s"($id)"
      case Arrow(dir, _)    => Seq(" ↑ ", " → ", " ↓ ", " ← ")(dir.ordinal)
      case Blocker(dir, _)  => Seq(" ⇈ ", " ⇉ ", " ⇊ ", " ⇇ ")(dir.ordinal)
      case Tunnel(dir, _)   => Seq(" ⇄ ", " ⇅ ")(dir.ordinal % 2)
      case Key              => " K "
      case Lock(_)          => "[L]"
      case Tile(id, dir, _, _) =>
        val i: String = if (id.toString.size == 1) s"0$id" else s"$id"
        val d: Char   = dir.toString()(0)
        s"$i$d"

enum Event:
  case Move(direction: Direction)
  case Quit
  case Undo
  case Reset
