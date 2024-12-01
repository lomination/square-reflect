package lomination.squarereflect.console

/** An enum of some ANSI graphic escape codes */
enum Style:

  /** Does not change anything */
  case Nothing

  /** Sets the printing style to the default one */
  case Reset

  /** Swap foreground and background colours */
  case Reversed

  /** Makes the text blink */
  case Blink

  /** Sets the foreground colour to the given rgb values
    *
    * @param r
    *   the red value of the desired foreground colour (between 0 and 255)
    * @param g
    *   the green value of the desired foreground colour (between 0 and 255)
    * @param b
    *   the blue value of the desired foreground colour (between 0 and 255)
    */
  case Fg(r: Int, g: Int, b: Int)

  /** Sets the foregound colour to black
    *
    * Has the same effect as
    * ```
    * Style.Fg(r: Int, g: Int, b: Int)
    * ```
    */
  case DefFg

  /** Sets the background colour to the given rgb values
    *
    * @param r
    *   the red value of the desired background colour (between 0 and 255)
    * @param g
    *   the green value of the desired background colour (between 0 and 255)
    * @param b
    *   the blue value of the desired background colour (between 0 and 255)
    */
  case Bg(r: Int, g: Int, b: Int)

  /** Sets the background colour to black
    *
    * Has the same effect as
    * ```
    * Style.Fg(r: Int, g: Int, b: Int)
    * ```
    */
  case DefBg

  override def toString: String =
    this match
      case Nothing     => ""
      case Reset       => "\u001b[0m"
      case Reversed    => "\u001b[7m"
      case Blink       => "\u001b[5m"
      case Fg(r, g, b) => s"\u001b[38;2;$r;$g;${b}m"
      case DefFg       => "\u001b[38;2;255;255;255m"
      case Bg(r, g, b) => s"\u001b[48;2;$r;$g;${b}m"
      case DefBg       => "\u001b[38;2;0;0;0m"

object Style:

  /** Converts a hue value to a foreground rgb colour
    *
    * @param hue
    *   the given hue value (between 0 and 1)
    */
  def fgFromHue(hue: Float): Style =
    val colour = java.awt.Color(java.awt.Color.HSBtoRGB(hue, 1f, 1f))
    Style.Fg(colour.getRed, colour.getGreen, colour.getBlue)

  /** Converts a hue value to a background rgb colour
    *
    * @param hue
    *   the given hue value (between 0 and 1)
    */
  def bgFromHue(hue: Float): Style =
    val colour = java.awt.Color(java.awt.Color.HSBtoRGB(hue, 1f, 1f))
    Style.Bg(colour.getRed, colour.getGreen, colour.getBlue)
