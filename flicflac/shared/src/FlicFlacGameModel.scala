package shared

import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

enum GameState derives Encoder.AsObject, Decoder:
  case START_CON1 // ........... default initial state for this model
  case START_CON2 // ........... peerJS has successfuly opened a node for this model
  case START_CON3 // ........... peerJS has successfull established a connection with remote node
  case START_CON4 // ........... playParams resolved by Responder, accepted by Initiator
  case START_CON5 // ........... Responder invokes CON5 (previously CON4), Initiator accepts
  case START_CON6 // ........... Responder invokes CON6 (previously CON5), Initiator accepts
  case START_CON7 // ........... Responder invokes CON7 (previously CON6), Initiator accepts
  case START_CON8 // ........... Responder invokes CYLINDER_TURN (previously CON7), queues StartGameEvent and switches scenes
  // ........................... Initiator accepts (waits for CYLINDER_TURN to arrive to queue StartGameEvent and switche scenes)
  case BLOCK_TURN // ........... Block Turn
  case BLOCK_PAUSE // .......... Not used
  case BLOCK_RESOLVE // ........ Not used
  case CYLINDER_TURN // ........ Cylinder Turn
  case CYLINDER_PAUSE // ....... Not used
  case CYLINDER_RESOLVE // ..... Not used
  case FINISH // ............... Game Finished and score displayed
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
    turnNumber: Int = 0, // ................................................. Updates
    pieces: Pieces = Pieces(Vector.empty), // ............................... Updates
    possibleMoveSpots: Spots = Spots(Set.empty), // ......................... Updates
    highLighter: HighLighter = HighLighter(false, PointXY(0, 0)), // ........ Updates - AI can ignore
    turnTimer: TurnTimer = TurnTimer(20, 10) // ............................. Updates

) derives Encoder.AsObject,
      Decoder:

  def convertRxGameModel(rxModel: FlicFlacGameModel): FlicFlacGameModel =
    val name1 = rxModel.ourName // .............................................. used to swap into oppoName
    val name2 = rxModel.oppoName // ............................................. used to swap into ourName
    val pieceType = (rxModel.ourPieceType & 1) ^ 1 // ........................... inverting piece type
    rxModel.copy(ourName = name2, oppoName = name1, ourPieceType = pieceType)
  end convertRxGameModel

end FlicFlacGameModel
