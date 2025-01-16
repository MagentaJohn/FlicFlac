package game

import io.circe.Encoder
import io.circe.Decoder

final case class PointXY(x: Int, y: Int) 
  derives CanEqual, 
          Encoder.AsObject,
          Decoder:
    def +(pt: PointXY): PointXY = PointXY(x + pt.x, y + pt.y)
