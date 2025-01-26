package shared

import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

enum GameState derives Encoder.AsObject, Decoder:
  case START_CON1 // a state included in the set return by getStartUpStates
  case START_CON2 // a state included in the set return by getStartUpStates
  case START_CON3 // a state included in the set return by getStartUpStates
  case START_CON4 // a state included in the set return by getStartUpStates
  case START_CON5 // a state included in the set return by getStartUpStates
  case START_CON6 // a state included in the set return by getStartUpStates
  case START_CON7 // a state included in the set return by getStartUpStates
  case START_CON8 // a state included in the set return by getStartUpStates
  case BLOCK_TURN
  case BLOCK_PAUSE
  case BLOCK_RESOLVE
  case CYLINDER_TURN
  case CYLINDER_PAUSE
  case CYLINDER_RESOLVE
  case FINISH
end GameState

enum PanelType:
  case P_INVISIBLE
  case P_HINT
  case P_ERROR
  case P_RESULTS
end PanelType

final case class FlicFlacGameModel(
    ourName: String = "Player1", // ......................................... Negotiated at startup - rx packets SWAP
    oppoName: String = "Player2", // ........................................ Negotiated at startup - rx packets SWAP
    boardSize: Int = BOARD_SIZE_XLARGE, // ,................................. Negotiated at startup
    ourPieceType: Int = CYLINDER, // ........................................ Negotiated at startup - rx packets INVERT
    winningScore: Int = 11, // .............................................. Negotiated at startup
    randEventFreq: Int = 1, // .............................................. Negotiated at startup
    initiatorGameState: GameState = GameState.START_CON1, // ................ Negotiated at startup
    responderGameState: GameState = GameState.START_CON1, // ................ Negotiated at startup
    gameState: GameState = GameState.CYLINDER_TURN, // ...................... Updates
    gameScore: (Int, Int) = (0, 0), // ...................................... Updates
    pieces: Pieces = Pieces(Vector.empty), // ............................... Updates
    possibleMoveSpots: Spots = Spots(Set.empty), // ......................... Updates
    highLighter: HighLighter = HighLighter(false, PointXY(0, 0)), // ........ Updates
    turnTimer: TurnTimer = TurnTimer(20, 10) // ............................. Updates

) derives Encoder.AsObject, Decoder:

end FlicFlacGameModel
