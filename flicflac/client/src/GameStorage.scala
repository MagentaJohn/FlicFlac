package game

import shared.*

import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode
import game.FlicFlacStartupData
import cats.instances.double
import game.hexBoard4

val GAME_INDEX_FILE = "FlicFlac-Index"
val GAME_PREFIX = "###-"

enum StepType:
  case ST_Start
  case ST_Forward
  case ST_Backward
  case ST_Finish
end StepType

// StorageIndex is cached key containing the names of currewnt and previous games
final case class StorageIndex(
    cachedEntries: List[String] // ........... List of names of the GameStorage files in cache
) derives Encoder.AsObject,
      Decoder

// MiniPiece is the reduced structure for each game piece (we don't need all the pieces details to display orientation and position)
final case class MiniPiece(
    qrPos: PointXY, // ..................... current position, QRS coords
    bFlipped: Boolean = false, // .......... piece normal is false, piece flipped is true
    bCaptured: Boolean = false // .......... piece is captured (or not)
) derives Encoder.AsObject,
      Decoder
end MiniPiece

// GameTurnEntry is the cache entry added to record each turn
final case class GameTurnEntry(
    moveNumber: Int,
    moveType: GameState,
    deployment: Vector[MiniPiece],
    score: (Int, Int)
) derives Encoder.AsObject,
      Decoder
end GameTurnEntry

// GameStorage is the class with helper functions to read and write to cache
final case class GameStorage(
    name: String, // ................. the name of the key file in the cache
    params: PlayerParams, // ......... the parameters used during game
    turnNo: Int, // .................. the current turn number when under review
    turns: List[GameTurnEntry] // .... the list of piece movements for each turn
) derives Encoder.AsObject,
      Decoder:

  def establishGameStorage(playerParams: PlayerParams): GameStorage =
    scribe.debug("@@@ establishGameStorage")

    val s1 = playerParams.playPams1_Name1
    val s2 = playerParams.playPams2_Name2

    val rootName = GAME_PREFIX + s1 + "-" + s2
    var index = 0
    var uniqueName = rootName + "-" + index.toString()
    var bSearching = true

    while bSearching == true do
      uniqueName = rootName + "-" + index.toString()
      val key = org.scalajs.dom.window.localStorage.getItem(uniqueName)
      if key == null then
        // key will be null when the cache does not hold key / uniqueName
        bSearching = false
      else
        // the key / uniqueName DOES exist so increment the index and try again
        index = index + 1
      end if
    end while
    // create/append the game name to game index file

    val possibleStorageIndex = decode[StorageIndex](org.scalajs.dom.window.localStorage.getItem(GAME_INDEX_FILE)) match
      case Right(si) =>
        val previousEntries = si.cachedEntries
        val updatedEntries = previousEntries :+ uniqueName
        val asJson2 = (StorageIndex(updatedEntries)).asJson.noSpaces
        org.scalajs.dom.window.localStorage.setItem(GAME_INDEX_FILE, asJson2)

      case Left(_) =>
        val firstEntryAsList = List(uniqueName)
        val asJson1 = (StorageIndex(firstEntryAsList)).asJson.noSpaces
        org.scalajs.dom.window.localStorage.setItem(GAME_INDEX_FILE, asJson1)

    // ensure the list of turns is empty - possible reset during game ...

    val gs1 = this.copy(name = uniqueName, params = playerParams, turns = List())
    val gs1AsJson = gs1.asJson.noSpaces
    org.scalajs.dom.window.localStorage.setItem(uniqueName, gs1AsJson)

    gs1
  end establishGameStorage

  def appendGameTurn(gs: GameStorage, ffgm: FlicFlacGameModel): GameStorage =

    val turnNumber = ffgm.turnNumber
    val turnType = ffgm.gameState

    var deployment: Vector[MiniPiece] = Vector.empty
    ffgm.pieces.modelPieces.foreach(p =>
      val qrs = HexBoard().getQRSfromAxAy(p.pCurPos.x, p.pCurPos.y)
      val qrPos = PointXY(qrs._1, qrs._2)
      val mp = new MiniPiece(qrPos, p.bFlipped, p.bCaptured)
      deployment = deployment :+ mp
    )
    val score = ffgm.gameScore

    val latestTurn = new GameTurnEntry(turnNumber, turnType, deployment, score)

    val updatedTurns = gs.turns :+ latestTurn
    val gs1 = gs.copy(turns = updatedTurns)
    val gs1AsJson = gs1.asJson.noSpaces
    org.scalajs.dom.window.localStorage.setItem(gs1.name, gs1AsJson)
    gs1
  end appendGameTurn

  def readGameStorage(storageName: String): Option[GameStorage] =
    val possibleGameStorage = decode[GameStorage](org.scalajs.dom.window.localStorage.getItem(storageName)) match
      case Right(gs: GameStorage) =>
        scribe.debug("@@@ readGameStorage:" + storageName + " has " + gs.turns.length + " turns")
        Some(gs)
      case Left(_) =>
        scribe.debug("@@@ readGameStorage:" + storageName + " not found")
        None
    possibleGameStorage
  end readGameStorage

  def traverseGameStorage(step: StepType): GameStorage =
    step match
      case StepType.ST_Start =>
        this.copy(turnNo = 1)

      case StepType.ST_Forward =>
        val nt = (turnNo + 1).min(turns.length)
        this.copy(turnNo = nt)

      case StepType.ST_Backward =>
        val nt = (turnNo - 1).max(1)
        this.copy(turnNo = nt)

      case StepType.ST_Finish =>
        this.copy(turnNo = turns.length)
  end traverseGameStorage

  def meldStorageToModel(startUpData: FlicFlacStartupData, model: FlicFlacGameModel): FlicFlacGameModel =

    scribe.debug("@@@ meldStorageToModel")
    val newBoardSize = this.params.playPams4_BoardSize
    val newWinningScore = this.params.playPams5_ScoreToWin
    val newTurnTime = this.params.playPams6_TurnTime
    val newCaptorsTime = this.params.playPams7_CaptorsTime
    val newRandEventFreq = this.params.playPams8_RandEventProb

    // lets ensure i1 which is internal turn number satisifes 0 <= i1 <= (turns.length -1)
    val i0 = (this.turnNo - 1).max(0)
    val i1 = (i0).min(this.turns.length - 1)

    val newGameState = this.turns(i1).moveType
    val newPieceType = // match the piece type to the game state in order to get the Score Panel to track the players
      if newGameState == GameState.BLOCK_TURN then
        // block
        BLOCK
      else
        // cylinder
        CYLINDER
      end if
    end newPieceType

    val newGameScore = this.turns(i1).score
    val newTurnNo = this.turns(i1).moveNumber

    var newPieces = Vector.empty[Piece]
    var pieceIndex = 0
    for p1 <- this.turns(i1).deployment do // p1 is a "miniPiece"

      val p2 = model.pieces.modelPieces(pieceIndex)

      val sh = p2.pieceShape
      val id = p2.pieceIdentity

      val q = p1.qrPos._1
      val r = p1.qrPos._2
      val s = -q - r
      val aXaY = hexBoard.getAxAyfromQRS(q, r, s)
      val xy = hexBoard.getAxAyfromQRS(q, r, s)
      val xy1 = PointXY(xy._1, xy._2)
      val xy0 = PointXY(0, 0)
      val bf = p1.bFlipped
      val bc = p1.bCaptured

      val p3 = new Piece(sh, id, xy1, xy0, xy0, false, bf, false, bc, false, false)

      newPieces = newPieces :+ p3

      pieceIndex = pieceIndex + 1
    end for

    model.copy(
      boardSize = newBoardSize,
      ourPieceType = newPieceType,
      winningScore = newWinningScore,
      randEventFreq = newRandEventFreq,
      gameState = newGameState,
      gameScore = newGameScore,
      turnNumber = newTurnNo,
      pieces = Pieces(newPieces),
      turnTimer = TurnTimer(newTurnTime, newCaptorsTime)
    )
  end meldStorageToModel

end GameStorage
