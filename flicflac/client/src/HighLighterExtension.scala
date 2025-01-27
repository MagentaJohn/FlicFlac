package game

import shared.*

import indigo.*
import io.circe.Encoder
import io.circe.Decoder

extension (highLighter: HighLighter)

  /*
  setPos repositions the highlighter hex as appropriate
   */
  def setPosAndShine(model: FlicFlacGameModel, newPos: PointXY): HighLighter =
    model.highLighter.copy(displayOn = true, currentPos = newPos)
  end setPosAndShine

  /*
  show enables or disables the display of the HighLighter hex
   */
  def shine(model: FlicFlacGameModel, onOff: Boolean): HighLighter =
    model.highLighter.copy(displayOn = onOff)
  end shine

  /*
  paint generates a "SceneUpdateFragment" containing the new position of the Highligter Hex
   */

  def hlPaint(model: FlicFlacGameModel, hexBoard4: HexBoard4, fS: Double, pB: PointXY): Layer =
    val highLighter = model.highLighter
    if highLighter.displayOn then
      val layer = GameAssets.gHex(fS).modifyMaterial(_.withTint(RGBA.Cyan))
      val paintPos = hexBoard4.getXsYs(highLighter.currentPos)
      Layer(layer.moveTo(pB.x + paintPos.x, pB.y + paintPos.y))
    else
      // Blink effect
      Layer.empty
    end if
  end hlPaint

end extension
