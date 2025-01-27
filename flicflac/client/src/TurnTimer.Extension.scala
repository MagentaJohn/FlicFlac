package game

import shared.*

import indigo.*
import io.circe.Encoder
import io.circe.Decoder

extension (turnTimer: TurnTimer)

  def ttPaint(model: FlicFlacGameModel): Layer =
    // all measurements before scaling ...
    // for 0% ...
    // cap part is 25 high ...... and starts-ends at (70-95)
    // body part is 1195 high ... and starts-ends at (95-1220)
    // for 100% ...
    // cap part is 25 high ...... and starts-ends at (1170-1195)
    // body part is 25 high ..... and starts-ends at (1195-1220)

    // 0% = 70 ........ for cap top
    // 100% = 1170 .... for cap top
    // Body-Bottom length is 1170 (=1220 -25 -25) 25 for top and 25 for bottom
    // For N% ...
    // Cap Top(T) = 70 + N * 1170
    // White Rectangle Height = Cap Top -1

    val tt = model.turnTimer
    val bActive = tt.bActive
    val bCaptorsTurn = tt.bCaptorsTurn
    val iCaptorsTime =
      if bActive then tt.iCaptorsTurnTime * 10 // captors turn time in 10ths of seconds
      else 50 // we want to show 50% of the timer bar if inactive timer and captors turn
      end if
    end iCaptorsTime
    val iTotalTime =
      if bActive then tt.iTotalTurnTime * 10 // total turn time allowed in 10ths of seconds
      else 100 // we want 100% ot the timer bar if inactive and pieces turn
      end if
    end iTotalTime

    // It is possible for the captors time to be greater than the turn time so introduced the maximum
    val iMaxTime = math.max(iTotalTime, iCaptorsTime)
    val iCurrentTime = tt.iCurrentTime
    val iTurnExpires = tt.iThisTurnExpires

    val iTimeRemaining =
      if bActive then math.max(0, iTurnExpires - iCurrentTime)
      else if bCaptorsTurn then 50 // we want to show 50% of the timer bar if inactive timer and captors turn
      else 100 // we want 100% ot the timer bar if inactive and pieces turn
      end if
    end iTimeRemaining

    val iTimeSpent = iMaxTime - iTimeRemaining

    val dSF = hexBoard4.scalingFactor
    val scalableX = hexBoard4.boardSize match
      case BOARD_SIZE_SMALL  => 1050
      case BOARD_SIZE_MEDIUM => 1050
      case BOARD_SIZE_LARGE  => 1200
      case _                 => 1200 // BOARD_SIZE_XLARGE

    val bodyCropMark = hexBoard4.boardSize match
      case BOARD_SIZE_SMALL  => 270
      case BOARD_SIZE_MEDIUM => 160
      case BOARD_SIZE_LARGE  => 80
      case _                 => 0 // BOARD_SIZE_XLARGE

    // we need to adjust the body length to compensate for any cropping incurred by size reduction
    val iBodyLength = 1170 - bodyCropMark

    val T: Double = ((iBodyLength * iTimeSpent) / iMaxTime) + 70

    val iSliderXPos = (math.round(scalableX * dSF)).toInt + hexBoard4.pBase.x
    val iBodyTop = (math.round(95 * dSF)).toInt
    val iCapTop = (math.round(T * dSF)).toInt
    val iWidth = (math.round(52 * dSF)).toInt // changed from 50 to 52 to eliminate sporadic vertical lines

    val bCylinder = (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == CYLINDER)
    val bBlock = (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == BLOCK)

    val content1 =
      if (bCylinder == true) || (bBlock == true) then
        // magenta slider
        Layer.Content(GameAssets.gTimeSliderActiveBody(bodyCropMark, dSF).moveTo(iSliderXPos, iBodyTop))
      else
        // grey slider
        Layer.Content(GameAssets.gTimeSliderInactiveBody(bodyCropMark, dSF).moveTo(iSliderXPos, iBodyTop))
      end if
    end content1

    val content2 =
      if (bCylinder == true) || (bBlock == true) then
        // magenta slider
        Layer.Content(GameAssets.gTimeSliderActiveTop(dSF).moveTo(iSliderXPos, iCapTop))
      else
        // grey slider
        Layer.Content(GameAssets.gTimeSliderInactiveTop(dSF).moveTo(iSliderXPos, iCapTop))
      end if
    end content2

    val r3 = Rectangle(iSliderXPos, 0, iWidth, iCapTop)
    val content3 = Layer.Content(Shape.Box(r3, Fill.Color(RGBA.White)))
    val content4 = content1 |+| content2 |+| content3

    content4
  end ttPaint

end extension
