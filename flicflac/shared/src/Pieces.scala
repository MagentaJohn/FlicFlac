package shared

import io.circe.Encoder
import io.circe.Decoder

val CYLINDER = 0
val BLOCK = 1

// First 6 colors are used in modulo 6 fashion for pieces
val CB = 0 // CB for Blue
val CG = 1 // CR for Green
val CY = 2 // CG for Yellow
val CO = 3 // CY for Orange
val CR = 4 // CO for Red
val CP = 5 // CP for Purple
//-----------------------------------------------------
val CK = 6 // CK for Black (and CK=6 is used for Captured/Killed)
val CW = 7 // CW for White
val CC = 8 // CC for Cyan is a test color
val CM = 9 // CM for Magenta is a test color
val CX = 10 // CX indicates hex does not so not visible (indicated by transparency field = 0)

def mix(i: Int): SRGBA =
  i match
    case CX => SRGBA.fromHexString("#00000000") // Zero
    case CB => SRGBA.fromHexString("#80C0FFFF") // Blue
    case CG => SRGBA.fromHexString("#C0FFC0FF") // Green
    case CY => SRGBA.fromHexString("#FFFFC0FF") // Yellow
    case CO => SRGBA.fromHexString("#FFD070FF") // Orange
    case CR => SRGBA.fromHexString("#FFC0C0FF") // Red
    case CP => SRGBA.fromHexString("#CCCCFFFF") // Purple
    case CK => SRGBA.fromHexString("#808080FF") // Black
    case CW => SRGBA.fromHexString("#FFFFFFFF") // White
    case CC => SRGBA.fromHexString("#00FFFFFF") // Cyan
    case CM => SRGBA.fromHexString("#FF00FFFF") // Magenta
    case _  => SRGBA.fromHexString("#FF00FFFF") // Magenta

final case class Pieces(
    modelPieces: Vector[Piece]
) derives Encoder.AsObject,
      Decoder:

end Pieces
