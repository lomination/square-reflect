package lomination.squarereflect.console

import lomination.squarereflect.{
  Empty,
  Block,
  Angle,
  End,
  Kill,
  Cannon,
  Portal,
  Arrow,
  Blocker,
  Tunnel,
  Tile
}

case class ConsoleTile(tile: Tile, display: String):
  def write: String =
    display

object ConsoleTile:
  def from(tile: Tile): ConsoleTile =
    tile match
      case t @ Empty                      => ConsoleTile(t, """   """)
      case t @ Block(tType)               => ConsoleTile(t, """[ ]""")
      case t @ Angle(dir, tType)          => ConsoleTile(t, """|\,""")
      case t @ End(tType)                 => ConsoleTile(t, """END""")
      case t @ Kill(tType)                => ConsoleTile(t, """ X """)
      case t @ Cannon(tType)              => ConsoleTile(t, """(+)""")
      case t @ Portal(id, pairPos, tType) => ConsoleTile(t, s"""($id)""")
      case t @ Arrow(dir, tType)          => ConsoleTile(t, """ ↑ """)
      case t @ Blocker(dir, tType)        => ConsoleTile(t, """BLN""")
      case t @ Tunnel(dir, tType)         => ConsoleTile(t, """TUN""")



      