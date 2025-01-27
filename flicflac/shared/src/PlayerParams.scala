package shared

import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

final case class PlayerParams(
    playPams1_Name1: String, // ........... default "Player1"
    playPams2_Name2: String, // ........... default "Player2"
    playPams3_PieceShape: Int, // ......... default CYLINDER if initiator, BLOCK if responder
    playPams4_BoardSize: Int, // .......... default 8
    playPams5_ScoreToWin: Int, // ......... default 11
    playPams6_TurnTime: Int, // ........... default 20 seconds
    playPams7_CaptorsTime: Int, // ........ default 10 seconds
    playPams8_RandEventProb: Int // ....... default 1 (in 100)
) derives Encoder.AsObject,
      Decoder

object PlayerParams:
  

end PlayerParams
