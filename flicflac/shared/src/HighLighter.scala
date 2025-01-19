package shared

import io.circe.Encoder
import io.circe.Decoder

final case class HighLighter(
    val displayOn: Boolean,
    val currentPos: PointXY
) derives Encoder.AsObject,
      Decoder:

  /*
  setPos repositions the highlighter hex as appropriate
   */
  def setPosAndShine(newPos: PointXY): HighLighter =
    copy(displayOn = true, currentPos = newPos)
  end setPosAndShine

  /*
  show enables or disables the display of the HighLighter hex
   */
  def shine(onOff: Boolean): HighLighter =
    copy(displayOn = onOff)
  end shine

end HighLighter
