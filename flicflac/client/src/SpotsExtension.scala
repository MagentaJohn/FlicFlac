package game

import shared.*

import indigo.*

extension (spots: Spots)

  def spPaint(model: FlicFlacGameModel): Layer =
    val dSF = hexBoard4.scalingFactor
    val pB = hexBoard4.pBase
    val layer = GameAssets.gSpot(dSF)
    var multiSpot = Layer.empty

    for pos <- model.possibleMoveSpots.indices do
      val pPos = hexBoard4.getXsYs(PointXY(pos._1, pos._2))
      val spotLayer = Layer(layer.moveTo(hexBoard4.pBase.x + pPos.x, hexBoard4.pBase.y + pPos.y))
      multiSpot = multiSpot |+| spotLayer
    end for
    multiSpot
  end spPaint

end extension
