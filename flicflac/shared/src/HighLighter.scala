package shared

import io.circe.Encoder
import io.circe.Decoder

final case class HighLighter(
    val displayOn: Boolean,
    val currentPos: PointXY
) derives Encoder.AsObject,
      Decoder:

end HighLighter
