package shared

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

def mix(i: Int): SRGBA =
  i match
    case CX => SRGBA.fromHexString("#00000000") // Zero
    case CB => SRGBA.fromHexString("#80C0FFFF") // Blue
    case CG => SRGBA.fromHexString("#C0FFC0FF") // Green
    case CY => SRGBA.fromHexString("#FFFFC0FF") // Yellow
    case CO => SRGBA.fromHexString("#FFD070FF") // Orange
    case CR => SRGBA.fromHexString("#FFC0C0FF") // Red
    case CP => SRGBA.fromHexString("#CCCCFFFF") // Purple
    case CK => SRGBA.fromHexString("#808080FF") // Black
    case CW => SRGBA.fromHexString("#FFFFFFFF") // White
    case CC => SRGBA.fromHexString("#00FFFFFF") // Cyan
    case CM => SRGBA.fromHexString("#FF00FFFF") // Magenta
    case _  => SRGBA.fromHexString("#FF00FFFF") // Magenta

val pieceTypes: Vector[String] =
  Vector("Cyldr", "Block")

val pieceNames: Vector[String] = // Piece Names %6
  Vector("Blue  ", "Green ", "Yellow", "Orange", "Red   ", "Purple", "Grey  ")
final case class Pieces(
    modelPieces: Vector[Piece]
) derives Encoder.AsObject,
      Decoder:

  def summonPieces(hexBoard: HexBoard): Pieces =
    val size = hexBoard.boardSize
    val cy1 = hexBoard.getCylinderHomePos(size, CB)
    val cy2 = hexBoard.getCylinderHomePos(size, CG)
    val cy3 = hexBoard.getCylinderHomePos(size, CY)
    val cy4 = hexBoard.getCylinderHomePos(size, CO)
    val cy5 = hexBoard.getCylinderHomePos(size, CR)
    val cy6 = hexBoard.getCylinderHomePos(size, CP)
    val bk1 = hexBoard.getBlockHomePos(size, CB)
    val bk2 = hexBoard.getBlockHomePos(size, CG)
    val bk3 = hexBoard.getBlockHomePos(size, CY)
    val bk4 = hexBoard.getBlockHomePos(size, CO)
    val bk5 = hexBoard.getBlockHomePos(size, CR)
    val bk6 = hexBoard.getBlockHomePos(size, CP)

    val startingModelPieces: Vector[Piece] = Vector(
      Piece(CYLINDER, CB, cy1, cy1, cy1, false),
      Piece(CYLINDER, CG, cy2, cy2, cy2, false),
      Piece(CYLINDER, CY, cy3, cy3, cy3, false),
      Piece(CYLINDER, CO, cy4, cy4, cy4, false),
      Piece(CYLINDER, CR, cy5, cy5, cy5, false),
      Piece(CYLINDER, CP, cy6, cy6, cy6, false),
      Piece(BLOCK, CB, bk1, bk1, bk1, false),
      Piece(BLOCK, CG, bk2, bk2, bk2, false),
      Piece(BLOCK, CY, bk3, bk3, bk3, false),
      Piece(BLOCK, CO, bk4, bk4, bk4, false),
      Piece(BLOCK, CR, bk5, bk5, bk5, false),
      Piece(BLOCK, CP, bk6, bk6, bk6, false)
    )
    Pieces(startingModelPieces)
  end summonPieces

  def somePiece(hexBoard: HexBoard): Piece =
    Piece(CYLINDER, CB, PointXY(0, 0), PointXY(0, 0), PointXY(0, 0), false)
  end somePiece

  def findPieceByPos(model: FlicFlacGameModel, pos: PointXY): Option[Piece] =
    model.pieces.modelPieces.find(p => p.position(p) == pos)
  end findPieceByPos

  def findPieceSelected(model: FlicFlacGameModel): Option[Piece] =
    model.pieces.modelPieces.find(p => p.selected(p) == true)
  end findPieceSelected

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
      if p.captured(p) then
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
