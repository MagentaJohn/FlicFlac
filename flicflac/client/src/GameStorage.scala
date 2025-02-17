package shared

import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

val GAME_INDEX_FILE = "FlicFlac-Index"
val GAME_PREFIX = "###-"

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
    turns: List[GameTurnEntry] // .... the list of piece movements for each turn
) derives Encoder.AsObject,
      Decoder:

  def establishGameStorage(s1: String, s2: String): GameStorage =
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

    val gs1 = this.copy(name = uniqueName, turns = List())
    val gs1AsJson = gs1.asJson.noSpaces
    org.scalajs.dom.window.localStorage.setItem(uniqueName, gs1AsJson)

    gs1
  end establishGameStorage

  def appendGameTurn(gs: GameStorage, ffgm: FlicFlacGameModel): GameStorage =

    val turnNumber = ffgm.turnNumber
    val turnType = ffgm.gameState

    var deployment: Vector[MiniPiece] = Vector.empty
    ffgm.pieces.modelPieces.foreach(p =>
      val qrs = HexBoard().getQRSfromXY(p.pCurPos.x, p.pCurPos.y)
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

end GameStorage
