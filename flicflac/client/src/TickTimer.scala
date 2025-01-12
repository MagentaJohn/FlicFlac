package game

import indigo.*
import indigoextras.ui.*
import scribe.*

val TT_ONE_SECOND = 10
val TT_TWO_SECONDS = 20
val TT_THREE_SECONDS = 30
val TT_TEN_SECONDS = 100

object TickTimer:

  val timerDivisor = 100 // makes these timers operate in 10ths of a second ... 0 means disabled, +20 = 2 seconds

  def start(expiryTime: Long): Long = // expiryTime is in 10ths of a second
    val timer = (System.currentTimeMillis() / timerDivisor) + expiryTime
    timer
  end start

  def stop(): Long =
    val timer = 0
    timer
  end stop

  def expired(timer: Long): Boolean =
    val currentTime = (System.currentTimeMillis() / timerDivisor)
    val expired = (timer > 0) && (currentTime >= timer)
    expired
  end expired

  def isActive(timer: Long): Boolean =
    val active = (timer > 0)
    active
  end isActive

  def isInactive(timer: Long): Boolean =
    val inactive = (timer == 0)
    inactive
  end isInactive
end TickTimer
