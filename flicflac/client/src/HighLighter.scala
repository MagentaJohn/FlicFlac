package game

import indigo.*

final case class HighLighter(
    val displayOn: Boolean,
    val currentPos: ffPoint
):

  /*
  setPos repositions the highlighter hex as appropriate
   */
  def setPosAndShine(newPos: ffPoint): HighLighter =
    copy(displayOn = true, currentPos = newPos)
  end setPosAndShine

  /*
  show enables or disables the display of the HighLighter hex
   */
  def shine(onOff: Boolean): HighLighter =
    copy(displayOn = onOff)
  end shine

  /*
  paint generates a "SceneUpdateFragment" containing the new position of the Highligter Hex
   */

  def paint(hexBoard4: HexBoard4, fS: Double, pB: ffPoint): Layer =
    if displayOn then
      val layer = GameAssets.gHex(fS).modifyMaterial(_.withTint(mix(CM)))
      val paintPos = hexBoard4.getXsYs(currentPos)
      Layer(layer.moveTo(pB.x + paintPos.x, pB.y + paintPos.y))
    else
      // Blink effect
      Layer.empty
    end if
  end paint

end HighLighter
