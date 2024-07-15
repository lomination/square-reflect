package lomination.squarereflect


case class Board(rows: Seq[Seq[Tile]], name: Option[String] = None, light: Option[LightLevel] = None):
  lazy val width: Int = rows(0).size
  lazy val height: Int = rows.size
  def apply(position: Position): Tile = rows(position.y)(position.x)
  def apply(x: Int, y: Int): Tile = rows(y)(x)
  def setTileAt(position: Position, f: Tile => Tile): Board =
    val rows: Seq[Seq[Tile]] =
      for (y <- 0 until height) yield
        if (y == position.y) this.rows(y)
        else for (x <- 0 until width) yield if (x == position.x) f(this.rows(y)(x)) else this.rows(y)(x)
    Board(rows, name, light)
  def setTileAt(position: Position, tile: Tile): Board =
    setTileAt(position, _ => tile)

case class LightLevel(level: Int) extends AnyVal
