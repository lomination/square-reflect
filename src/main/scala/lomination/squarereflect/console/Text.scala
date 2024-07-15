package lomination.squarereflect.console

case class Text(content: String, fore: Color, back: Color, style: Style):
  implicit def toString: String =
    Seq(
      s"\u001b[38;5;${fore._1};${fore._2};${fore._3}m",
      s"\u001b[48;5;${back._1};${back._2};${back._3}m",
      writeStyle,
      content,
      Console.RESET
    ).mkString
  def writeStyle: String = style match
    case Style.Default => ""
    case Style.Blink => "\u001b[5m"
    case Style.Invisible => "\u001b[8m"
  def reset: Text = Text(content, Color.white, Color.black, Style.Default)
  def setFore(fore: Color): Text = Text(content, fore, back, style)
  def setBack(back: Color): Text = Text(content, fore, back, style)
  def setStyle(style: Style): Text = Text(content, fore, back, style)

enum Style:
  case Default, Blink, Invisible


extension (string: String)
  def s(fore: Color = Color.white, back: Color = Color.black, style: Style = Style.Default): Text =
    Text(string, fore, back, style)

case class Color(r: Int, g: Int, b: Int)

object Color {
  lazy val white: Color = Color(255, 255, 255)
  lazy val lightGray: Color = Color(192, 192, 192)
  lazy val gray: Color = Color(128, 128, 128)
  lazy val darkGray: Color = Color(64, 64, 64)
  lazy val black: Color = Color(0, 0, 0)
  lazy val red: Color = Color(255, 0, 0)
  lazy val pink: Color = Color(255, 175, 175)
  lazy val orange: Color = Color(255, 200, 0)
  lazy val yellow: Color = Color(255, 255, 0)
  lazy val green: Color = Color(0, 255, 0)
  lazy val magenta: Color = Color(255, 0, 255)
  lazy val cyan: Color = Color(0, 255, 255)
  lazy val blue: Color = Color(0, 0, 255)
  def fromHue(hue: Float): Color =
    import java.awt.Color as JColor
    val jColor = JColor(JColor.HSBtoRGB(hue, 1f, 1f))
    Color(jColor.getRed, jColor.getGreen, jColor.getBlue)
}
