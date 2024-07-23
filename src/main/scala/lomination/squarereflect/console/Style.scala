package lomination.squarereflect.console

enum Style:
  case Nothing
  case Reset
  case Reversed
  case Invisible
  case Blink
  case Fg(r: Int, g: Int, b: Int)
  case Bg(r: Int, g: Int, b: Int)
  override def toString: String =
    this match
      case Nothing => ""
      case Reset => "\u001b[0m"
      case Blink => "\u001b[5m"
      case Reversed => "\u001b[7m"
      case Fg(r, g, b) => s"\u001b[38;2;$r;$g;${b}m"
      case Bg(r, g, b) => s"\u001b[48;2;$r;$g;${b}m"

object Style:
  def fgFromHue(hue: Float): Style =
    val colour = java.awt.Color(java.awt.Color.HSBtoRGB(hue, 1f, 1f))
    Style.Fg(colour.getRed, colour.getGreen, colour.getBlue)
  def bFromHue(hue: Float): Style =
    val colour = java.awt.Color(java.awt.Color.HSBtoRGB(hue, 1f, 1f))
    Style.Bg(colour.getRed, colour.getGreen, colour.getBlue)
