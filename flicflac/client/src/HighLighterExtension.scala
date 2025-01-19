package game

import shared.*

import indigo.*
import io.circe.Encoder
import io.circe.Decoder

extension ( highLighter: HighLighter )

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
