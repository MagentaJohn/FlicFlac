package game

import io.circe.Encoder
import io.circe.Decoder

val CYLINDER = 0
val BLOCK = 1

final case class Pieces(
    modelPieces: Vector[Piece]
) derives Encoder.AsObject,
      Decoder:
  def newTurn(model: FlicFlacGameModel): Pieces =
    var newModelPieces = Vector.empty[Piece]
    for p1 <- model.pieces.modelPieces do
      val pNewCurPos =
        if p1.bCaptured then p1.pHomePos // if captured, return home & clear bCaptured flag
        else p1.pCurPos
      val p2 = p1.copy(
        bSelected = false,
        bCaptured = false,
        bCaptor = false,
        bMoved = false,
        pTurnStartPos = pNewCurPos,
        bTurnStartFlipState = p1.bFlipped,
        pCurPos = pNewCurPos
      )
      newModelPieces = newModelPieces :+ p2
    end for
    Pieces(newModelPieces)
  end newTurn

  def extraTurnScoring(model: FlicFlacGameModel): (Int, Int) =
    var additionalScore = (0, 0)
    for p <- model.pieces.modelPieces do
      if Piece.captured(p) then
        if p.pieceShape == CYLINDER then
          // blocks make capture
          additionalScore = (additionalScore._1, additionalScore._2 + 1)
        else
          // cylinders make capture
          additionalScore = (additionalScore._1 + 1, additionalScore._2)
        end if
      end if
    end for
    (model.gameScore._1 + additionalScore._1, model.gameScore._2 + additionalScore._2)
  end extraTurnScoring

end Pieces
