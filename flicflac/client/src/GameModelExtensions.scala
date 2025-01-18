package game

import io.circe.syntax.*
import io.circe.parser.decode

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

def retrieve(startupData: FlicFlacStartupData): FlicFlacGameModel =
  val playerParams = FlicFlacPlayerParams.getParams(startupData)
  val ourName = playerParams.playPams1_Name1
  val oppoName = playerParams.playPams2_Name2

  val gameCache = getGameName(ourName, oppoName)
  val cacheOrNew = decode[FlicFlacGameModel](org.scalajs.dom.window.localStorage.getItem(gameCache)) match
    case Right(model: FlicFlacGameModel) =>
      // FIXME we should check for version number here and goto create if mismatch
      scribe.debug("@@@ Restored model")
      creation(playerParams)
    case Left(_) =>
      scribe.debug("@@@ Created model")
      creation(playerParams)
  cacheOrNew
end retrieve

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
  val turnTimer = TurnTimer(playerParams.playPams6_TurnTime, playerParams.playPams7_CaptorsTime, false, false, 0, 0)
  val highLighter = new HighLighter(false, PointXY(0, 0))

  // create the hexboard
  hexBoard4.create(boardSize)

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
    summonPieces(hexBoard4),
    startingSpots,
    highLighter,
    turnTimer
  )
end creation

def summonPieces(hexBoard4: HexBoard4): Pieces =
  val cy1 = hexBoard4.getCylinderHomePos(CB)
  val cy2 = hexBoard4.getCylinderHomePos(CG)
  val cy3 = hexBoard4.getCylinderHomePos(CY)
  val cy4 = hexBoard4.getCylinderHomePos(CO)
  val cy5 = hexBoard4.getCylinderHomePos(CR)
  val cy6 = hexBoard4.getCylinderHomePos(CP)
  val bk1 = hexBoard4.getBlockHomePos(CB)
  val bk2 = hexBoard4.getBlockHomePos(CG)
  val bk3 = hexBoard4.getBlockHomePos(CY)
  val bk4 = hexBoard4.getBlockHomePos(CO)
  val bk5 = hexBoard4.getBlockHomePos(CR)
  val bk6 = hexBoard4.getBlockHomePos(CP)

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
  model.pieces.modelPieces.find(Piece.position(_) == pos)
end findPieceByPos

def findPieceSelected(model: FlicFlacGameModel): Option[Piece] =
  model.pieces.modelPieces.find(Piece.selected(_) == true)
end findPieceSelected

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
  val turnTimer1 = TurnTimer(turnTime, captorsTime, false, false, 0, 0)
  val turnTimer2 = TurnTimer.restartForTurn(turnTimer1)

  // create the hexboard
  hexBoard4.create(iBoardSize)

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
    summonPieces(hexBoard4),
    emptySpots,
    highLighter,
    turnTimer2
  )
end reset

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
