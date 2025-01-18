package game

import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*

case class FlicFlacGameModel(
    ourName: String, // ................. Negotiated at startup - rx packets SWAP
    oppoName: String, // ................ Negotiated at startup - rx packets SWAP
    boardSize: Int, // ,................. Negotiated at startup
    ourPieceType: Int, // ............... Negotiated at startup - rx packets INVERT
    winningScore: Int, // ............... Negotiated at startup
    randEventFreq: Int, // .............. Negotiated at startup
    initiatorGameState: GameState, // ... Negotiated at startup
    responderGameState: GameState, // ... Negotiated at startup
    gameState: GameState, // ............ Updates
    gameScore: (Int, Int), // ........... Updates
    pieces: Pieces, // .................. Updates
    possibleMoveSpots: Spots, // ........ Updates
    highLighter: HighLighter, // ........ Updates
    turnTimer: TurnTimer // ............. Updates

) derives Encoder.AsObject,
      Decoder
