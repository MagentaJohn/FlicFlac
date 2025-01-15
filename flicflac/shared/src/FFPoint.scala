package game

final case class ffPoint(x: Int, y: Int) derives CanEqual:
  def +(pt: ffPoint): ffPoint = ffPoint(x + pt.x, y + pt.y)
