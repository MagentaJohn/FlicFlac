package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

class ClientFlicFlacGameModel ( ffgm: FlicFlacGameModel ):

  def modify(
      previousModel: FlicFlacGameModel,
      possiblePiece: Option[Piece],
      possibleHighLighter: Option[HighLighter]
  ): Outcome[FlicFlacGameModel] =
    val m1 = modifyPiece(previousModel, possiblePiece)
    val m2 = modifyHighLighter(m1, possibleHighLighter)
    val m3 = modifyPossibleMoves(m2)
    val asJson = m3.asJson.noSpaces
    val gameCache = FlicFlacGameModel.getGameName(previousModel.ourName, previousModel.oppoName)
    org.scalajs.dom.window.localStorage.setItem(gameCache, asJson)
    Outcome(m3).addGlobalEvents(WebRtcEvent.SendData(m3))
  end modify

  def modifyPiece(previousModel: FlicFlacGameModel, possiblePiece: Option[Piece]): FlicFlacGameModel =
    possiblePiece match
      case Some(newPiece) =>
        var resultingVector: Vector[Piece] = Vector.empty
        for oldPiece <- previousModel.pieces.modelPieces do
          if (oldPiece.pieceShape == newPiece.pieceShape) && (oldPiece.pieceIdentity == newPiece.pieceIdentity) then resultingVector = resultingVector :+ newPiece
          else resultingVector = resultingVector :+ oldPiece
          end if
        end for
        val resultingPieces: Pieces = Pieces(resultingVector)
        previousModel.copy(pieces = resultingPieces)

      case None =>
        previousModel
  end modifyPiece

  def modifyPieces(previousModel: FlicFlacGameModel, newPieces: Pieces): Outcome[FlicFlacGameModel] =
    val m1 = previousModel.copy(pieces = newPieces)
    val asJson = m1.asJson.noSpaces
    val gameCache = FlicFlacGameModel.getGameName(previousModel.ourName, previousModel.oppoName)
    org.scalajs.dom.window.localStorage.setItem(gameCache, asJson)
    Outcome(m1).addGlobalEvents(WebRtcEvent.SendData(m1))
  end modifyPieces

  def modifyHighLighter(previousModel: FlicFlacGameModel, possibleHighLighter: Option[HighLighter]): FlicFlacGameModel =
    possibleHighLighter match
      case Some(newHighLighter) =>
        previousModel.copy(highLighter = newHighLighter)
      case None =>
        previousModel
  end modifyHighLighter

  def modifyPossibleMoves(previousModel: FlicFlacGameModel): FlicFlacGameModel =
    val newSpots = previousModel.possibleMoveSpots.calculatePossibleMoves(previousModel)
    previousModel.copy(possibleMoveSpots = newSpots)
  end modifyPossibleMoves

  def printPieces(model: FlicFlacGameModel): Unit =
    for p <- model.pieces.modelPieces do

      val sSelected = if Piece.selected(p) then "S" else "-"
      val sFlipped = if Piece.flipped(p) then "F" else "-"
      val sCaptured = if Piece.captured(p) then "C" else "-"
      val sMoved = if Piece.moved(p) then "M" else "-"

      val s = "@@@ " + PieceAssets.pieceTypes(p.pieceShape)
        + " " + PieceAssets.pieceNames(p.pieceIdentity % 6)
        + ": "
        + "CurPos(" + p.pCurPos.x + "," + p.pCurPos.y + ") "
        + "HomePos(" + p.pHomePos.x + "," + p.pHomePos.y + ") "
        + sSelected
        + sFlipped
        + sCaptured
        + sMoved
      scribe.trace(s)
    end for
  end printPieces

end ClientFlicFlacGameModel
