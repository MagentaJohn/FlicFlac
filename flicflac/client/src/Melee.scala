package game

import shared.*

import indigo.*
import cats.instances.double
import game.PieceAssets.pieceNames

final case class Melee(model: FlicFlacGameModel):

  def combat(model: FlicFlacGameModel): Pieces =

    // Step A ... get 4 sets of links

    val allPieces = model.pieces.modelPieces
    var vectorHealth: Array[Int] = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    val (cylinders, blocks) = allPieces splitAt 6 // Cylinders are 0...5 & blocks are 6...11

    var allPiecesQRS = Vector.empty[(Int, Int, Int)]
    var allPiecesEmpowered = Vector.empty[(Boolean)]
    allPieces.foreach { piece =>
      val qrs = hexBoard4.getQRSfromAxAy(piece.pCurPos.x, piece.pCurPos.y)
      allPiecesQRS = allPiecesQRS :+ qrs
      if hexBoard4.getHexColor(piece.pCurPos) == piece.pieceIdentity then
        // body color same as hex color detected
        allPiecesEmpowered = allPiecesEmpowered :+ true
      else
        // body color does not match hex color
        allPiecesEmpowered = allPiecesEmpowered :+ false
      end if
    }

    allPieces.foreach { p1 =>
      if p1.pCurPos != p1.pHomePos then
        // the combat algorithm only applies to those pieces on the board, not to those pieces
        // that are in their home positions

        val index1 = (p1.pieceShape * 6) + p1.pieceIdentity
        val shape1 = PieceAssets.pieceTypes(p1.pieceShape)
        val color1 = (PieceAssets.pieceNames(index1 % 6)).trim
        val qrs1 = allPiecesQRS(index1)
        val setQRS1 = model.possibleMoveSpots.spotRingQRS(qrs1._1, qrs1._2, qrs1._3)
        allPieces.foreach { p2 =>
          val index2 = (p2.pieceShape * 6) + p2.pieceIdentity
          val shape2 = PieceAssets.pieceTypes(p2.pieceShape)
          val color2 = (PieceAssets.pieceNames(index2 % 6)).trim
          val qrs2 = allPiecesQRS(index2)
          if setQRS1.contains(qrs2) then
            val p1BodyColor = p1.pieceIdentity
            val p1FlyingColor1 = if p1.bFlipped then (p1.pieceIdentity + 4) % 6 else (p1.pieceIdentity + 1) % 6
            val p1FlyingColor2 = if p1.bFlipped then (p1.pieceIdentity + 5) % 6 else (p1.pieceIdentity + 2) % 6
            val p2BodyColor = p2.pieceIdentity
            val p2FlyingColor1 = if p2.bFlipped then (p2.pieceIdentity + 4) % 6 else (p2.pieceIdentity + 1) % 6
            val p2FlyingColor2 = if p2.bFlipped then (p2.pieceIdentity + 5) % 6 else (p2.pieceIdentity + 2) % 6

            if p1BodyColor == p2FlyingColor1
              || p1BodyColor == p2FlyingColor2
            then
              (p1.pieceShape, p2.pieceShape) match
                case (CYLINDER, CYLINDER) | (BLOCK, BLOCK) =>
                  // in this case statement, p1 is the supported, p2 is the supporter
                  if allPiecesEmpowered(index2) == true then
                    // thanks from p1, but the supporter p2 gets no health credit
                    vectorHealth(index1) += 2
                  else
                    // thanks from p1, but the supporter p2 gets no health credit
                    vectorHealth(index1) += 1
                  end if

                case (CYLINDER, BLOCK) | (BLOCK, CYLINDER) =>
                  // in this case statement, p1 is the DEFENDER and p2 is the ATTACKER
                  if p1FlyingColor1 != p2BodyColor && p1FlyingColor2 != p2BodyColor then
                    // defender is defenseless so subtract 20 from health to ensure capture
                    // well done attacker, you are guranteed an extra turn as one of the captors
                    vectorHealth(index1) -= 20
                  end if
                  if allPiecesEmpowered(index2) == true then
                    // defending piece flying correct colors, but attacker is also empowered
                    vectorHealth(index1) -= 2
                    vectorHealth(index2) += 2
                  else
                    // defending piece flying correct colors, and attacker is normal
                    vectorHealth(index1) -= 1
                    vectorHealth(index2) += 1
                  end if
              end match
              scribe.debug(
                "@@@ {" + shape1 + color1 + ":" + vectorHealth(index1) + "} {"
                  + shape2 + color2 + ":" + vectorHealth(index2) + "}"
              )
            end if // p1BodyColor == p2FlyingColor1 || p1BodyColor == p2FlyingColor2
          end if // setQRS1.contains(qrs2)
        }
      end if // p1.pCurPos != p1.pHomePos
    }
    // scribeCombat(allPieces, vectorHealth)

    val piecesWithCaptures = captured(allPieces, vectorHealth)
    (piecesWithCaptures)

  end combat

  /*  captured is used during the turn to mark unhealthy pieces as captured
   */

  def captured(allPieces: Vector[Piece], vectorHealth: Array[Int]): Pieces =
    var newPieces = Vector.empty[(Piece)]
    allPieces.foreach { piece =>
      val index = (piece.pieceShape * 6) + piece.pieceIdentity
      val newPiece =
        if vectorHealth(index) < 0 then
          // oh lordy - I have been captured
          piece.setCaptured(piece, true)
        else
          // aha - I am still free
          piece.setCaptured(piece, false)
        end if
      end newPiece
      newPieces = newPieces :+ newPiece
    }
    Pieces(newPieces)
  end captured

  /* detectCaptors is used at the end of a turn to revisits all captives to ...
  .. calculate the set of captors. If there has been no captures this turn ...
  .. an empty set is generated indicating it is ok to transition the next move ...
  .. to opponent
   */

  def detectCaptors(model: FlicFlacGameModel): Set[(Piece)] =
    val allPieces = model.pieces.modelPieces
    val (cylinders, blocks) = allPieces.splitAt(6)
    // second part of filter below discards our own pieces (ie kamikasi prisoners)
    val vPrisoners = allPieces.filter(p => (p.bCaptured == true) && (p.pieceShape != model.ourPieceType))
    var setCaptors = Set.empty[(Piece)]

    vPrisoners.foreach { p =>
      val qrs = hexBoard4.getQRSfromAxAy(p.pCurPos.x, p.pCurPos.y)
      val spotQRS = model.possibleMoveSpots.spotRingQRS(qrs._1, qrs._2, qrs._3)
      val prisonerColor = p.pieceIdentity
      val possibleCaptors = if model.gameState == GameState.CYLINDER_TURN then cylinders else blocks

      possibleCaptors.foreach { pp =>
        val qrs1 = hexBoard4.getQRSfromAxAy(pp.pCurPos.x, pp.pCurPos.y)
        if spotQRS.contains(qrs1) then
          val captorColor1 = if pp.bFlipped then (pp.pieceIdentity + 4) % 6 else (pp.pieceIdentity + 1) % 6
          val captorColor2 = if pp.bFlipped then (pp.pieceIdentity + 5) % 6 else (pp.pieceIdentity + 2) % 6
          if prisonerColor == captorColor1 || prisonerColor == captorColor2 then
            // add this piece to captors
            setCaptors = setCaptors + pp
          end if
        end if
      }
    }
    (setCaptors)
  end detectCaptors

  /* rewardCaptors is used at the end of the turn to reset the captors so that they ...
  .. can move again. It marks all other pieces as moved (even if they not moved)
  .. If a piece (including a captor) is captured then it returns to home position.

   */

  def rewardCaptors(model: FlicFlacGameModel, setCaptors: Set[(Piece)]): Pieces =
    var allPieces = Vector.empty[(Piece)]
    model.pieces.modelPieces.foreach { p =>
      if (p.pieceShape == CYLINDER && model.gameState == GameState.CYLINDER_TURN)
        || (p.pieceShape == BLOCK && model.gameState == GameState.BLOCK_TURN)
      then
        if setCaptors.contains(p) then
          val p1 = p.setCaptor(p, false)
          val p2 = p.setMoved(p1, false)
          if p.captured(p2) then
            val p3 = p.moveToHome(p2)
            val p4 = p.setTurnStartPos(p3, p3.pCurPos)
            val p5 = p.setCaptured(p4, false)
            allPieces = allPieces :+ p5
          else
            val p3 = p.setTurnStartPos(p2, p2.pCurPos)
            allPieces = allPieces :+ p3
          end if
        else
          val p1 = p.setMoved(p, true)
          if p.captured(p1) then
            val p2 = p.moveToHome(p1)
            val p3 = p.setTurnStartPos(p2, p2.pCurPos)
            val p4 = p.setCaptured(p3, false)
            allPieces = allPieces :+ p4
          else
            // add piece as just moved
            allPieces = allPieces :+ p1
          end if
        end if
      else
        if p.captured(p) then
          val p1 = p.moveToHome(p)
          val p2 = p.setTurnStartPos(p1, p1.pCurPos)
          val p3 = p.setCaptured(p2, false)
          allPieces = allPieces :+ p3
        else
          // add piece
          allPieces = allPieces :+ p
        end if
      end if
    }
    Pieces(allPieces)

  end rewardCaptors

  def scribeCombat(allPieces: Vector[Piece], vectorHealth: Array[Int]): Unit =
    val (cylinders, blocks) = allPieces splitAt 6 // Cylinders are 0...5 & blocks are 6...11
    var str1 = "Cyldrs "
    cylinders.foreach { p =>
      val color = PieceAssets.pieceNames(p.pieceIdentity)
      val index = (p.pieceShape * 6) + p.pieceIdentity
      str1 = str1 + "<" + color.trim + ":" + vectorHealth(index) + ">"
    }
    scribe.debug("@@@ " + str1)

    var str2 = "Blocks "
    blocks.foreach { p =>
      val color = PieceAssets.pieceNames(p.pieceIdentity)
      val index = (p.pieceShape * 6) + p.pieceIdentity
      str2 = str2 + "<" + color.trim + ":" + vectorHealth(index) + ">"
    }
    scribe.debug("@@@ " + str2)

  end scribeCombat
end Melee

object Melee {}

end Melee
