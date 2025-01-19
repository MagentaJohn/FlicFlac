package game

import shared.*

import indigo.*

extension(spots: Spots)

  def calculatePossibleMoves(model: FlicFlacGameModel): Spots =
    scribe.debug("@@@ Spots calculatePossibleMoves")

    val resultingSpots: Spots = Spots(Set.empty)
    FlicFlacGameModel.findPieceSelected(model) match
      case Some(piece) =>
        scribe.debug("@@@ Spots finds a selected piece")
        calculateSpots(model, piece)

      case None =>
        scribe.debug("@@@ Spots does not find a selected piece")
        Spots(Set.empty)
    end match
  end calculatePossibleMoves

  def calculateSpots(model: FlicFlacGameModel, piece: Piece): Spots =
    scribe.debug("@@@ Spots calculateSpots start")
    val bBlocks = ((piece.pieceShape == BLOCK) && (model.gameState == GameState.BLOCK_TURN))
    val bCylinders = ((piece.pieceShape == CYLINDER) && (model.gameState == GameState.CYLINDER_TURN))
    if bBlocks || bCylinders then
      if Piece.moved(piece) then
        scribe.debug("@@@ calculateSpots piece moved")
        // we know the piece has moved and want to provide the opportunity for it to return back to it's
        // turn starting position (PosA). However, after this piece moved another piece may also have moved and
        // occupied PosA. The next match checks for this ...
        model.pieces.modelPieces.find(p => (p.pCurPos == piece.pTurnStartPos)) match
          case Some(p) =>
            Spots(Set.empty)
          case None =>
            Spots(Set((piece.pTurnStartPos.x, piece.pTurnStartPos.y), (piece.pCurPos.x, piece.pCurPos.y))) // starting & current positions only
        end match
      else
        scribe.debug("@@@ calculateSpots piece not moved @ " + piece.pCurPos)
        spotify(model: FlicFlacGameModel, piece: Piece)
      end if
    else
      scribe.debug("@@@ calculateSpots out of turn " + model.gameState)
      Spots(Set.empty)
    end if
  end calculateSpots

  def spotify(model: FlicFlacGameModel, piece: Piece): Spots =

    val vPieces = model.pieces.modelPieces

    val ax = piece.pCurPos.x
    val ay = piece.pCurPos.y
    val qrs = hexBoard4.getQRSfromAxAy(ax, ay)
    val q = qrs._1
    val r = qrs._2
    val s = qrs._3

    if piece.pCurPos == piece.pHomePos then

      // we have a piece in the home position so display unoccupied starting places
      // these places vary according to board size
      var ss1 = Set.empty[(Int, Int)]

      piece.pieceShape match
        case CYLINDER =>
          if piece.pieceIdentity == CB || piece.pieceIdentity == CR || piece.pieceIdentity == CY then
            // spots for top left
            model.boardSize match
              case 5 => ss1 = Set((2, 9), (3, 8), (3, 7), (4, 6), (4, 5))
              case 6 => ss1 = Set((1, 9), (2, 8), (2, 7), (3, 6), (3, 5), (4, 4))
              case 7 => ss1 = Set((1, 9), (2, 8), (2, 7), (3, 6), (3, 5), (4, 4), (4, 3))
              case _ => ss1 = Set((0, 9), (1, 8), (1, 7), (2, 6), (2, 5), (3, 4), (3, 3), (4, 2))
          else
            // spots for bottom right
            model.boardSize match
              case 5 => ss1 = Set((4, 21), (5, 20), (5, 19), (6, 18), (6, 17))
              case 6 => ss1 = Set((4, 24), (4, 23), (5, 22), (5, 21), (6, 20), (6, 19))
              case 7 => ss1 = Set((4, 27), (5, 26), (5, 25), (6, 24), (6, 23), (7, 22), (7, 21))
              case _ => ss1 = Set((4, 30), (4, 29), (5, 28), (5, 27), (6, 26), (6, 25), (7, 24), (7, 23))
          end if
        case BLOCK =>
          if piece.pieceIdentity == CB || piece.pieceIdentity == CR || piece.pieceIdentity == CY then
            // spots for bottom left
            model.boardSize match
              case 5 => ss1 = Set((2, 17), (3, 18), (3, 19), (4, 20), (4, 21))
              case 6 => ss1 = Set((1, 19), (2, 20), (2, 21), (3, 22), (3, 23), (4, 24))
              case 7 => ss1 = Set((1, 21), (2, 22), (2, 23), (3, 24), (3, 25), (4, 26), (4, 27))
              case _ => ss1 = Set((0, 23), (1, 24), (1, 25), (2, 26), (2, 27), (3, 28), (3, 29), (4, 30))
          else
            // spots for top right
            model.boardSize match
              case 5 => ss1 = Set((4, 5), (5, 6), (5, 7), (6, 8), (6, 9))
              case 6 => ss1 = Set((4, 4), (4, 5), (5, 6), (5, 7), (6, 8), (6, 9))
              case 7 => ss1 = Set((4, 3), (5, 4), (5, 5), (6, 6), (6, 7), (7, 8), (7, 9))
              case _ => ss1 = Set((4, 2), (4, 3), (5, 4), (5, 5), (6, 6), (6, 7), (7, 8), (7, 9))
          end if
      end match

      val ss2 = ss1.filter { case (aX, aY) => hexBoard4.isThisHexFree(PointXY(aX, aY), vPieces) }
      val ss3 = ss2 + ((piece.pHomePos.x, piece.pHomePos.y))
      Spots(ss3)
    else if piece.bMoved then
      // we have a piece that has already moved this turn
      val ss1 = Set((piece.pTurnStartPos.x, piece.pTurnStartPos.y))
      Spots(ss1)
    else

      // we have a piece on the board trying to move so calculate valid moves from StartPos, Ring1,Ring2,Ring3
      // Inner Ring

      val setStartPos = Set((piece.pTurnStartPos.x, piece.pTurnStartPos.y))

      val setInnerRing = spotRingQRS(q, r, s)
      var setInnerRingAxAy = Set.empty[(Int, Int)]
      var setInnerRingQRS = Set.empty[(Int, Int, Int)]

      setInnerRing.foreach { case (q1, r1, s1) =>
        val aX1aY1 = hexBoard4.getAxAyfromQRS(q1, r1, s1)
        val q1r1s1 = (q1, r1, s1)
        if hexBoard4.isThisHexValid(q1, r1, s1)
          && hexBoard4.isThisHexFree(q1, r1, s1, vPieces)
        then
          setInnerRingAxAy = setInnerRingAxAy + aX1aY1
          setInnerRingQRS = setInnerRingQRS + q1r1s1
        end if
      }
      scribe.debug("@@@ spotify Ring1 free hex count: " + setInnerRingQRS.size)

      // Middle Ring

      var setMiddleRingAxAy = Set.empty[(Int, Int)]
      var setMiddleRingQRS = Set.empty[(Int, Int, Int)]
      val setInnerRingNotBlackQRS =
        setInnerRingQRS.filter((q2f, r2f, s2f) => hexBoard4.isThisHexBlack(q2f, r2f, s2f) == false)
      setInnerRingNotBlackQRS.foreach { case (q2, r2, s2) =>
        val set2M = spotRingQRS(q2, r2, s2)
        set2M.foreach { case (q2m, r2m, s2m) =>
          if hexBoard4.isThisHexValid(q2m, r2m, s2m)
            && hexBoard4.isThisHexBlack(q2m, r2m, s2m) == false
            && hexBoard4.isThisHexFree(q2m, r2m, s2m, vPieces)
          then
            val aX2aY2 = hexBoard4.getAxAyfromQRS(q2m, r2m, s2m)
            val q2r2s2 = (q2m, r2m, s2m)
            setMiddleRingAxAy = setMiddleRingAxAy + aX2aY2
            setMiddleRingQRS = setMiddleRingQRS + q2r2s2
        }
      }

      // Outer Ring

      var setOuterRingAxAy = Set.empty[(Int, Int)]
      var setOuterRingQRS = Set.empty[(Int, Int, Int)]
      val setMiddleRingNotBlackQRS =
        setMiddleRingQRS.filter((q3f, r3f, s3f) => hexBoard4.isThisHexBlack(q3f, r3f, s3f) == false)
      setMiddleRingNotBlackQRS.foreach { case (q3, r3, s3) =>
        val set3M = spotRingQRS(q3, r3, s3)
        set3M.foreach { case (q3m, r3m, s3m) =>
          if hexBoard4.isThisHexValid(q3m, r3m, s3m)
            && hexBoard4.isThisHexBlack(q3m, r3m, s3m) == false
            && hexBoard4.isThisHexFree(q3m, r3m, s3m, vPieces)
          then
            val aX3aY3 = hexBoard4.getAxAyfromQRS(q3m, r3m, s3m)
            val q3r3s3 = (q3m, r3m, s3m)
            setOuterRingAxAy = setOuterRingAxAy + aX3aY3
            setOuterRingQRS = setOuterRingQRS + q3r3s3
        }
      }
      Spots(setStartPos.union(setInnerRingAxAy).union(setMiddleRingAxAy).union(setOuterRingAxAy))
    end if

  end spotify

  def spotRingQRS(q: Int, r: Int, s: Int): Set[(Int, Int, Int)] =
    // Inner Ring offsets are ...
    // (0,-1,1) (1,-1,-0) (1, 0, -1) (0, 1, -1) (-1, 1, -0) (-1, 0, 1)
    val ring1 = Set(
      (q + 0, r - 1, s + 1),
      (q + 1, r - 1, s + 0),
      (q + 1, r + 0, s - 1),
      (q + 0, r + 1, s - 1),
      (q - 1, r + 1, s + 0),
      (q - 1, r + 0, s + 1)
    )
    ring1
  end spotRingQRS

  def spPaint(model: FlicFlacGameModel): Layer =
    val dSF = hexBoard4.scalingFactor
    val pB = hexBoard4.pBase
    val layer = GameAssets.gSpot(dSF)
    var multiSpot = Layer.empty

    for pos <- model.possibleMoveSpots.indices do
      val pPos = hexBoard4.getXsYs(PointXY(pos._1, pos._2))
      val spotLayer = Layer(layer.moveTo(hexBoard4.pBase.x + pPos.x, hexBoard4.pBase.y + pPos.y))
      multiSpot = multiSpot |+| spotLayer
    end for
    multiSpot
  end spPaint

end extension
