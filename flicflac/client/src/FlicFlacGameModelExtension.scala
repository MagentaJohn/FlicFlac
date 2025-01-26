package game

import shared.*

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

extension (flicFlacGameModel: FlicFlacGameModel)

  def creation(playerParams: FlicFlacPlayerParams): FlicFlacGameModel =
    scribe.debug("@@@ FlicFlacGameModel creation")

    val sOurName = playerParams.playPams1_Name1
    val sOppoName = playerParams.playPams2_Name2
    val iOurShape = playerParams.playPams3_PieceShape
    val boardSize = playerParams.playPams4_BoardSize
    val iWinningScore = playerParams.playPams5_ScoreToWin
    val iRandEventFreq = playerParams.playPams8_RandEventProb
    val score = (0, 0)
    // pieces
    val startingSpots: Spots = Spots(Set.empty)
    val turnTimer = sharedTurnTimer.copy(
      iTotalTurnTime = playerParams.playPams6_TurnTime,
      iCaptorsTurnTime = playerParams.playPams7_CaptorsTime
    )
    val highLighter = new HighLighter(false, PointXY(0, 0))

    // create the shared hexboard
    hexBoard.forge(boardSize)
    
    // derive the client hexboard, hexboard4
    hexBoard4.derive(hexBoard)


    FlicFlacGameModel(
      sOurName,
      sOppoName,
      boardSize,
      iOurShape,
      iWinningScore,
      iRandEventFreq,
      GameState.START_CON1,
      GameState.START_CON1,
      GameState.START_CON1,
      score,
      summonPieces(hexBoard),
      startingSpots,
      highLighter,
      turnTimer
    )
  end creation

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

  def findPieceByPos(model: FlicFlacGameModel, pos: PointXY): Option[Piece] =
    model.pieces.modelPieces.find(p => p.position(p) == pos)
  end findPieceByPos

  def findPieceSelected(model: FlicFlacGameModel): Option[Piece] =
    model.pieces.modelPieces.find(p => p.selected(p) == true)
  end findPieceSelected

  def modify(
      previousModel: FlicFlacGameModel,
      possiblePiece: Option[Piece],
      possibleHighLighter: Option[HighLighter]
  ): Outcome[FlicFlacGameModel] =
    val m1 = modifyPiece(previousModel, possiblePiece)
    val m2 = modifyHighLighter(m1, possibleHighLighter)
    val m3 = modifyPossibleMoves(m2)
    val asJson = m3.asJson.noSpaces
    val gameCache = getGameName(previousModel.ourName, previousModel.oppoName)
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
    val gameCache = getGameName(previousModel.ourName, previousModel.oppoName)
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
    val newSpots = previousModel.possibleMoveSpots.calculatePossibleMoves(hexBoard, previousModel)
    previousModel.copy(possibleMoveSpots = newSpots)
  end modifyPossibleMoves

  def reset(previousModel: FlicFlacGameModel): FlicFlacGameModel =
    scribe.debug("@@@ Reset model")

    val sOurName = previousModel.ourName
    val sOppoName = previousModel.oppoName
    val iBoardSize = previousModel.boardSize
    val iOurPieceType = previousModel.ourPieceType
    val iWinningScore = previousModel.winningScore
    val iRandEventFreq = previousModel.randEventFreq
    val score = (0, 0)
    val highLighter = new HighLighter(false, PointXY(0, 0))
    val emptySpots: Spots = Spots(Set.empty)
    val turnTime = previousModel.turnTimer.iTotalTurnTime
    val captorsTime = previousModel.turnTimer.iCaptorsTurnTime
    val turnTimer1 = TurnTimer(turnTime, captorsTime)
    val turnTimer2 = sharedTurnTimer.restartForTurn(turnTimer1)

    // adjust the shared hexboard
    hexBoard.forge(iBoardSize)
    
    // derive the client hexboard
    hexBoard4.derive(hexBoard)

    FlicFlacGameModel(
      sOurName,
      sOppoName,
      iBoardSize,
      iOurPieceType,
      iWinningScore,
      iRandEventFreq,
      GameState.START_CON1,
      GameState.START_CON1,
      GameState.CYLINDER_TURN,
      score,
      summonPieces(hexBoard),
      emptySpots,
      highLighter,
      turnTimer2
    )
  end reset

  def getGameName(ourName: String, oppoName: String): String =
    val sName: String =
      if ourName.compare(oppoName) < 0 then
        // we are the PeerJS initiator
        "FlicFlac-Game1"
      else
        // we are the PeerJS responder
        "FlicFlac-Game2"
      end if
    end sName
    // scribe.debug("@@@ getGameName: " + sName)
    sName
  end getGameName

  def getStartUpStates(): Set[GameState] =
    val startUpStateSet = Set(
      GameState.START_CON1,
      GameState.START_CON2,
      GameState.START_CON3,
      GameState.START_CON4,
      GameState.START_CON5,
      GameState.START_CON6,
      GameState.START_CON7,
      GameState.START_CON8
    )
    startUpStateSet
  end getStartUpStates

  def retrieve(startupData: FlicFlacStartupData): FlicFlacGameModel =
    val playerParams = FlicFlacPlayerParams.getParams(startupData)
    val ourName = playerParams.playPams1_Name1
    val oppoName = playerParams.playPams2_Name2

    val gameCache = getGameName(ourName, oppoName)
    val cacheOrNew = decode[FlicFlacGameModel](org.scalajs.dom.window.localStorage.getItem(gameCache)) match
      case Right(model: FlicFlacGameModel) =>
        // FIXME we should check for version number here and goto create if mismatch
        scribe.debug("@@@ Restored model")
        model.creation(playerParams)
      case Left(_) =>
        scribe.debug("@@@ Created model")
        val newFFGM = new FlicFlacGameModel()
        newFFGM.creation(playerParams)
    cacheOrNew
  end retrieve

  def printPieces(model: FlicFlacGameModel): Unit =
    for p <- model.pieces.modelPieces do

      val sSelected = if p.selected(p) then "S" else "-"
      val sFlipped = if p.flipped(p) then "F" else "-"
      val sCaptured = if p.captured(p) then "C" else "-"
      val sMoved = if p.moved(p) then "M" else "-"

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

end extension
