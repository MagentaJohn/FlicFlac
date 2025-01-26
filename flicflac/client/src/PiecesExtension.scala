package game

import shared.*

import indigo.*
import io.circe.Encoder
import io.circe.Decoder

extension (pieces: Pieces)

  def piPaint(model: FlicFlacGameModel, fS: Double, bBlinkOn: Boolean, optDragPos: Option[Point]): Layer =
    var layerPieces = Layer.empty

    val pB = hexBoard4.pBase // extract GridBasePoint for later

    // first draw all the unselected pieces ...

    for p <- model.pieces.modelPieces do
      val id =
        if p.bCaptured then CK // CK for BLACK = "Captured/Killed"
        else p.pieceIdentity
        end if
      end id

      val layer = PieceAssets.getGraphic(p.pieceShape, id, p.bFlipped)
      val pSrc = p.pCurPos

      // this is the mechanism for blinking pieces

      val bPotentialBlinker = (p.pieceShape, model.gameState, model.ourPieceType) match
        case (CYLINDER, GameState.CYLINDER_TURN, CYLINDER) => true
        case (BLOCK, GameState.BLOCK_TURN, BLOCK)          => true
        case (_, _, _)                                     => false

      val bShow =
        if bPotentialBlinker then
          if p.bMoved == true then
            // true for steady, this piece has moved
            true
          else
            // this might be true or false depending on the blink timer
            bBlinkOn
        else
          // true for steady, it is not the turn of this piece
          true
        end if
      end bShow

      if p.selected(p) == false && bShow == true then
        val ffPos = hexBoard4.getXsYs(pSrc)
        val pPos = Point(ffPos.x, ffPos.y) // ..... converting FlicFlacPoint to IndigoPoint
        val pB1 = Point(pB.x, pB.y) // ............ converting FlicFlacPoint to IndigoPoint

        val newLayer = Layer(layer.moveTo(pB1 + pPos).scaleBy(fS, fS))
        layerPieces = layerPieces |+| newLayer
      end if
    end for
    // second draw the selected piece if it exists
    // ... (only expecting one for now, but perhaps game might allow more in future)

    for p <- model.pieces.modelPieces do
      if p.selected(p) == true then
        val layer = PieceAssets.getGraphic(p.pieceShape, p.pieceIdentity, p.bFlipped)
        val pSrc = p.pCurPos
        optDragPos match // watch out ... somehow optDragPos has already been scaled
          case Some(pos) =>
            val pC = Point(((PieceAssets.gWidth * fS) / 2).toInt, ((PieceAssets.gHeight * fS) / 2).toInt)
            val pPos = pos - pC
            val newLayer = Layer(layer.scaleBy(fS, fS).moveTo(pPos))
            layerPieces = layerPieces |+| newLayer
          case None =>
            val ffPos = hexBoard4.getXsYs(pSrc)
            val pPos = Point(ffPos.x, ffPos.y) // ..... converting FlicFlacPoint to IndigoPoint
            val pB1 = Point(pB.x, pB.y) // ............ converting FlicFlacPoint to IndigoPoint
            val newLayer = Layer(layer.moveTo(pB1 + pPos).scaleBy(fS, fS))
            layerPieces = layerPieces |+| newLayer
        end match
    end for

    layerPieces
  end piPaint

end extension
