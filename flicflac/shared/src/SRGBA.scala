package game

final case class SRGBA(r: Double, g: Double, b: Double, a: Double) derives CanEqual

object SRGBA:

  def fromHexString(hex: String): SRGBA =
    if hex.startsWith("#") && hex.length == 9 then
      fromColorInts(
        Integer.parseInt(hex.substring(1, 3), 16),
        Integer.parseInt(hex.substring(3, 5), 16),
        Integer.parseInt(hex.substring(5, 7), 16),
        Integer.parseInt(hex.substring(7, 9), 16)
      )
    else
      SRGBA(0,0,0,1) // BLACK
    end if

  def fromColorInts(r: Int, g: Int, b: Int, a: Int): SRGBA =
    SRGBA((1.0 / 255) * r, (1.0 / 255) * g, (1.0 / 255) * b, (1.0 / 255) * a)

end SRGBA