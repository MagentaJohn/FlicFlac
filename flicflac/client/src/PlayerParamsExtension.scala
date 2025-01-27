package game

import shared.*

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

extension (playParams: PlayerParams)

  def getParams(startupData: FlicFlacStartupData): PlayerParams =

    val name1 = startupData.flicFlacBootData.name1
    val name2 = startupData.flicFlacBootData.name2

    val cacheConfigOrDefault =
      decode[PlayerParams](org.scalajs.dom.window.localStorage.getItem("FlicFlac-Params")) match
        case Right(playerParams: PlayerParams) =>
          val newPlayerParams = playerParams.copy(playPams1_Name1 = name1, playPams2_Name2 = name2)
          newPlayerParams

        case Left(_) =>
          scribe.error("@@@ PlayerParams getParams failed - assert default")
          PlayerParams(
            "Player1", // ..... Our name
            "Player2", // ..... Opponents name
            CYLINDER, // ...... PieceShape
            8, // ............. BoardSize
            11, // ............ ScoreToWin
            20, // ............ TurnTime
            10, // ............. CaptorsTime
            1 // .............. cfgRandEventProb
          )
    scribe.debug("@@@ PlayerParams getParams " + cacheConfigOrDefault)
    cacheConfigOrDefault
  end getParams

end extension