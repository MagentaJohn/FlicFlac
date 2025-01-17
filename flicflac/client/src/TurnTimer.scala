package game

import io.circe.Encoder
import io.circe.Decoder
final case class TurnTimer(
    val iTotalTurnTime: Int, // .............. the turn time in seconds as configured by Params
    val iCaptorsTurnTime: Int, // ............ the captors turn time in seconds as configured by Params
    val bActive: Boolean = false, // ......... indicates whether the time is active
    val bCaptorsTurn: Boolean = false, // .... indicates false for turn time and true for captors tome
    val iCurrentTime: Int = 0, // ............ the current time in 10ths of a second
    val iThisTurnExpires: Int = 0 // ......... the future time in 10ths of a second when turn expires
) derives Encoder.AsObject, Decoder
object TurnTimer:
  def restartForTurn(tt: TurnTimer): TurnTimer =
    val a1 = (tt.iTotalTurnTime > 0)
    val t2 = math.round(System.currentTimeMillis / 100).toInt // this is 10ths of a second
    val t3 = t2 + (10 * tt.iTotalTurnTime)
    tt.copy(iCurrentTime = t2, iThisTurnExpires = t3, bCaptorsTurn = false, bActive = a1)
  end restartForTurn

  def restartForCaptors(tt: TurnTimer): TurnTimer =
    val a1 = (tt.iCaptorsTurnTime > 0)
    val t2 = math.round(System.currentTimeMillis / 100).toInt // this is 10ths of a second
    val t3 = t2 + (10 * tt.iCaptorsTurnTime)
    tt.copy(iCurrentTime = t2, iThisTurnExpires = t3, bCaptorsTurn = true, bActive = a1)
  end restartForCaptors

  def update(tt: TurnTimer): Option[TurnTimer] =
    if tt.bActive then
      val newTime = math.round(System.currentTimeMillis / 100).toInt // this is 10ths of a second
      if newTime > tt.iCurrentTime then Some(tt.copy(iCurrentTime = newTime))
      else None // there is no update needed
      end if
    else None // there is no update needed
    end if
  end update

  def expired(tt: TurnTimer): Boolean =
    if tt.bActive && tt.iCurrentTime >= tt.iThisTurnExpires then
      // expired
      true
    else
      // either inactive or not expired
      false
    end if
  end expired

end TurnTimer
