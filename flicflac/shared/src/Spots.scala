package shared

import io.circe.Encoder
import io.circe.Decoder

// 28/07/24 Tried to use Point instead of(Int,Int) but encoder/decoder throws compiler errors
final case class Spots(
    indices: Set[(Int, Int)]
) derives Encoder.AsObject,
      Decoder:


end Spots
