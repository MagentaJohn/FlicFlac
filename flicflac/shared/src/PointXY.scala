package game

final case class PointXY(x: Int, y: Int) derives CanEqual:
  def +(pt: PointXY): PointXY = PointXY(x + pt.x, y + pt.y)
