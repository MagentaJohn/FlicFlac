package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder

val CYLINDER = 0
val BLOCK = 1

// First 6 colors are used in modulo 6 fashion for pieces
val CB = 0 // CB for Blue
val CG = 1 // CR for Green
val CY = 2 // CG for Yellow
val CO = 3 // CY for Orange
val CR = 4 // CO for Red
val CP = 5 // CP for Purple
//-----------------------------------------------------
val CK = 6 // CK for Black (and CK=6 is used for Captured/Killed)
val CW = 7 // CW for White
val CC = 8 // CC for Cyan is a test color
val CM = 9 // CM for Magenta is a test color
val CX = 10 // CX indicates hex does not so not visible (indicated by transparency field = 0)

def mix(i: Int): RGBA =
  i match
    case CX => RGBA.fromHexString("#00000000") // Zero
    case CB => RGBA.fromHexString("#80C0FFFF") // Blue
    case CG => RGBA.fromHexString("#C0FFC0FF") // Green
    case CY => RGBA.fromHexString("#FFFFC0FF") // Yellow
    case CO => RGBA.fromHexString("#FFD070FF") // Orange
    case CR => RGBA.fromHexString("#FFC0C0FF") // Red
    case CP => RGBA.fromHexString("#CCCCFFFF") // Purple
    case CK => RGBA.fromHexString("#808080FF") // Black
    case CW => RGBA.fromHexString("#FFFFFFFF") // White
    case CC => RGBA.fromHexString("#00FFFFFF") // Cyan
    case CM => RGBA.fromHexString("#FF00FFFF") // Magenta
    case _  => RGBA.fromHexString("#FF00FFFF") // Magenta

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
