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

end FlicFlacGameModel
