package game

import io.circe.Encoder
import io.circe.Decoder

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
