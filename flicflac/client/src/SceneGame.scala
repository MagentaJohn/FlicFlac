package game

import shared.*

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.platform.networking.Network

object SceneGame extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = GameSceneViewModel

  val name: SceneName = SceneName("Game")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set.empty

  var bBlinkOn = true
  var dMsg = "-----"

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =
    e =>
      try
        e match

          case StartLiveGame =>
            scribe.debug("@@@ StartLiveGame with BoardSize:" + model.boardSize)
            hexBoard.forge(model.boardSize)
            hexBoard4.derive(hexBoard) // ........................................ establish new hexboard
            val startingPieces = model.pieces.summonPieces(hexBoard) // .......... establish new starting positions
            val newModel = model.copy( // ........................................ create new model as first turn to be stored
              pieces = startingPieces,
              gameState = GameState.CYLINDER_TURN
            )
            gameStorage = gameStorage.appendGameTurn(gameStorage, newModel) // ... write starting positions as first turn stored
            model.modifyPieces(newModel, startingPieces) // ...................... update model and send to remote

          case e: FlicFlacGameUpdate.Info =>
            scribe.debug("@@@ FlicFlacGameUpdate.Info")
            model.modify(e.ffgm, None, None)
            if e.ffgm.turnNumber > model.turnNumber then
              // we only cache changes when a new turn is indicated
              gameStorage = gameStorage.appendGameTurn(gameStorage, e.ffgm)
            end if
            if e.ffgm.turnNumber == 0 && model.turnNumber > 0 then
              // the opponent has pressed the NEW GAME button so estyablish new cache
              val playerParams = new PlayerParams(
                model.ourName,
                model.oppoName,
                model.ourPieceType,
                model.boardSize,
                model.winningScore,
                model.turnTimer.iTotalTurnTime,
                model.turnTimer.iCaptorsTurnTime,
                model.randEventFreq
              )
              gameStorage = gameStorage.establishGameStorage(playerParams)
              scribe.debug("FIXME calling establish TP1")
            end if

            if e.ffgm.gameState == GameState.FINISH then
              val resultsMsg = constructResults(e.ffgm)
              Outcome(e.ffgm).addGlobalEvents(Freeze.PanelContent(PanelType.P_RESULTS, resultsMsg))
            else
              // game is ongoing
              Outcome(e.ffgm)
            end if

          case e: PointerEvent.PointerDown =>
            val clickPoint = e.position
            val ffClickPoint = PointXY(clickPoint.x, clickPoint.y)
            val hexPosn = hexBoard4.getAxAyFromDisplayXY(ffClickPoint, hexBoard4.scalingFactor)
            hexPosn match
              case Some(pos) =>
                // Pointer Down, Pos on Grid
                checkTurnValidAndThrow(model, "Pointer DOWN event") // throws exception if out of turn
                model.pieces.findPieceSelected(model) match
                  case Some(piece) =>
                    // Pointer Down, Pos on Grid, Piece Selected
                    if piece.pCurPos == pos then
                      // Pointer Down, Pos on Grid, Piece Selected, PiecePos=PointerPos <<##A##>>
                      dMsg = "##A##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.setPosAndShine(model, pos)
                      val modelA1 = model.copy(highLighter = newHL)
                      val updatedPiece = piece.setSelected(piece, true)
                      model.modify(modelA1, Some(updatedPiece), None)
                    else
                      // Pointer Down, Pos on Grid, Piece Selected, PiecePos!=PointerPos <<##B##>>
                      dMsg = "##B##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.shine(model, false)
                      Outcome(model.copy(highLighter = newHL))
                    end if

                  case None =>
                    // Pointer Down, Pos on Grid, No Piece Selected
                    model.pieces.findPieceByPos(model, pos) match
                      case Some(piece) =>
                        if ((piece.pieceShape == CYLINDER) && (model.gameState == GameState.CYLINDER_TURN))
                          || ((piece.pieceShape == BLOCK) && (model.gameState == GameState.BLOCK_TURN))
                        then
                          // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos and correct turn <<##C##>>
                          dMsg = "##C##"
                          scribe.debug("@@@ PointerEvent " + dMsg)
                          val newHL = model.highLighter.setPosAndShine(model, pos)
                          val updatedPiece = piece.setSelected(piece, true)
                          model.modify(model, Some(updatedPiece), Some(newHL))
                        else
                          // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos but incorrect turn <<##D##>>
                          dMsg = "##D##"
                          scribe.debug("@@@ PointerEvent " + dMsg)
                          Outcome(model)

                      case None =>
                        // Pointer Down, Pos on Grid, No Piece Selected, No Piece Found <<##E##>>
                        dMsg = "##E##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val newHL = model.highLighter.setPosAndShine(model, pos)
                        model.modify(model, None, Some(newHL))

                    end match // findPieceByPos
                end match // findPieceSelected

              case None =>
                // Pointer Down, Pos off Grid
                if checkTurnValid(model) then
                  model.pieces.findPieceSelected(model) match
                    case Some(piece) =>
                      // Pointer Down, Pos off Grid, Piece Selected <<##F##>>
                      dMsg = "##F##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.shine(model, false)
                      val updatedPiece = piece.setPosDeselect(piece, piece.pHomePos)
                      // clear any panel showing
                      model
                        .modify(model, Some(updatedPiece), Some(newHL))
                        .addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, ("", "")))

                    case None =>
                      // Pointer Down, Pos off Grid, No Piece Selected <<##G##>>
                      dMsg = "##G##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.shine(model, false)
                      // clear any panel showing
                      model.modify(model, None, Some(newHL)).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, ("", "")))
                  end match // findPieceSelected
                else
                  // although out of turn, the player is allowed to clear the local error panel (no msg sent to opponent)
                  Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, ("", "")))
                end if
            end match // hexXYCoordsFromDisplayXY

          case e: PointerEvent.PointerUp =>
            val clickPoint = e.position
            val ffClickPoint = PointXY(clickPoint.x, clickPoint.y)
            val hexPosn = hexBoard4.getAxAyFromDisplayXY(ffClickPoint, hexBoard4.scalingFactor)
            hexPosn match
              case Some(pos) =>
                // Pointer Up, Pos on Grid
                checkTurnValidAndThrow(model, "Pointer UP event") // throws exception if out of turn
                model.pieces.findPieceSelected(model) match
                  case Some(piece) =>
                    // Pointer Up, Pos on Grid, Piece Selected
                    if model.possibleMoveSpots.indices((pos.x, pos.y)) then
                      // Pointer Up, Pos on Grid, Piece Selected, Valid Move
                      val newHL = model.highLighter.shine(model, false)
                      if hexBoard.isThisHexBlack(pos) == true && piece.bMoved == false then
                        // we only flip piece if this is a new move
                        dMsg = "##H##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val updatedPiece = piece.setPosFlipDeselect(piece, pos)

                        model.modify(model, Some(updatedPiece), Some(newHL)).flatMap { um =>
                          val newPieces = Melee(um).combat(um, hexBoard)
                          um.modifyPieces(um, newPieces)
                        }
                      else
                        dMsg = "##I##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val updatedPiece = piece.setPosDeselect(piece, pos)

                        model.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                          val newPieces = Melee(updatedModel).combat(updatedModel, hexBoard)
                          model.modifyPieces(updatedModel, newPieces)
                        }
                      end if
                    else
                      // Pointer Up, Pos on Grid, Piece Selected
                      if pos == piece.pCurPos then
                        // Pointer Up, Pos on Grid, Piece Selected, No Move
                        dMsg = "##J##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val newHL = model.highLighter.shine(model, true)
                        val updatedPiece = piece.setSelected(piece, true)
                        model.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                          val newPieces = Melee(updatedModel).combat(updatedModel, hexBoard)
                          updatedModel.modifyPieces(updatedModel, newPieces)
                        }
                      else
                        // Pointer Up, Pos on Grid, Piece Selected, Invalid Move
                        dMsg = "##K##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val newHL = model.highLighter.shine(model, false)
                        val updatedPiece = piece.setPosDeselect(piece, piece.pCurPos)
                        model.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                          val newPieces = Melee(updatedModel).combat(updatedModel, hexBoard)
                          updatedModel.modifyPieces(updatedModel, newPieces)
                        }
                      end if
                    end if

                  case None =>
                    // Pointer Up, Pos on Grid, No piece selected
                    dMsg = "##L##"
                    scribe.debug("@@@ PointerEvent " + dMsg)

                    // FIXME 8 test lines to show magenta hex detail follows ...

                    val w = pos.x
                    val h = pos.y
                    val x = hexBoard4.hexArray(w)(h).x
                    val y = hexBoard4.hexArray(w)(h).y
                    val q = hexBoard4.hexArray(w)(h).q
                    val r = hexBoard4.hexArray(w)(h).r
                    val s = hexBoard4.hexArray(w)(h).s
                    scribe.debug(
                      "@@@ Magenta hexboard4: (ax,ay) x,y,q,r,s = (" + w + "," + h + ") : "
                        + x + "," + y + " : " + q + "," + r + "," + s
                    )
                    model.modify(model, None, None)

                end match // findPieceSelected

              case None =>
                if checkTurnValid(model) then
                  // Pointer Up, Pos off Grid
                  model.pieces.findPieceSelected(model) match
                    case Some(piece) =>
                      // Pointer Up, Pos off Grid, Piece Selected
                      dMsg = "##M##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.shine(model, false)
                      val updatedPiece = piece.setPosDeselect(piece, piece.pCurPos)
                      model.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                        val newPieces = Melee(updatedModel).combat(updatedModel, hexBoard)
                        updatedModel.modifyPieces(updatedModel, newPieces).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, ("", "")))
                      }

                    case None =>
                      // Pointer Up, Pos off Grid, No piece selected
                      dMsg = "##N##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.shine(model, false)
                      model.modify(model, None, Some(newHL)).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, ("", "")))
                  end match // findPieceSelected
                else
                  // although out of turn, the player is allowed to clear the local error panel (no msg sent to opponent)
                  Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, ("", "")))
                end if
            end match // hexXYCoordsFromDisplayXY

          case ButtonNewGameEvent =>
            scribe.debug("@@@ ButtonNewGameEvent")
            checkTurnValidAndThrow(model, "Button NEW GAME Event") // throws exception if out of turn
            if model.turnNumber == 0 then
              // avoid invalid storage and inhibit "new game" with reset if the game has not even started
              Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_HINT, ("*** FlicFlac Hint ***", "Game not started yet!")))
            else
              val newModel = model.reset(model)
              model.modify(newModel, None, None).addGlobalEvents(StartLiveGame)
            end if

          case ButtonPlusEvent =>
            scribe.debug("@@@ ButtonPlusEvent")
            val oldSF = hexBoard4.scalingFactor
            val newSF = increaseScaleFactor(oldSF)
            hexBoard4.calculateXsYs(newSF)
            hexBoard4.calculateGridPaintLayer()
            Outcome(model)

          case ButtonMinusEvent =>
            scribe.debug("@@@ ButtonMinusEvent")
            val oldSF = hexBoard4.scalingFactor
            val newSF = decreaseScaleFactor(oldSF)
            hexBoard4.calculateXsYs(newSF)
            hexBoard4.calculateGridPaintLayer()
            Outcome(model)

          case ViewportResize(gameViewPort) =>
            var dSF = 1.0
            if model.getStartUpStates().contains(model.gameState) then
              scribe.debug("@@@ ViewPortResize from scratch")
              val w = gameViewPort.width - hexBoard4.pBase.x
              val h = gameViewPort.height - hexBoard4.pBase.y
              dSF = GetScaleFactor(w, h, GameAssets.GetGameSceneDimensions(model.boardSize))
              scribe.debug("@@@ updateModel ViewportResize w:h->s " + w + ":" + h + "->" + dSF)
            else
              dSF = hexBoard4.scalingFactor
              scribe.debug("@@@ ViewPortResize from previous model sf=" + dSF)
            end if

            hexBoard4.calculateXsYs(dSF)
            hexBoard4.calculateGridPaintLayer()
            Outcome(model)

          case ButtonTurnEvent =>
            checkTurnValidAndThrow(model, "Button NEW TURN Event") // throws exception if out of turn
            scribe.debug("@@@ ButtonTurnEvent")
            val captors = Melee(model).detectCaptors(model, hexBoard)
            if captors.isEmpty then
              val emptySpots = Spots(Set.empty)
              val newScore = model.pieces.extraTurnScoring(model)
              val newTT = sharedTurnTimer.restartForTurn(model.turnTimer)
              val newPieces = model.pieces.newTurn(model)
              val cylinderScore = newScore._1
              val blockScore = newScore._2
              val newGameState =
                if (cylinderScore >= model.winningScore) && (cylinderScore >= blockScore + 2) then
                  scribe.debug("@@@ CYLINDERS WIN")
                  GameState.FINISH
                else if (blockScore >= model.winningScore) && (blockScore >= cylinderScore + 2) then
                  scribe.debug("@@@ BLOCKS WIN")
                  GameState.FINISH
                else if model.gameState == GameState.CYLINDER_TURN then
                  scribe.debug("@@@ BLOCK TURN @@@")
                  GameState.BLOCK_TURN
                else
                  scribe.debug("@@@ CYLINDER TURN @@@")
                  GameState.CYLINDER_TURN
                end if
              end newGameState
              val newTurnNumber = model.turnNumber + 1

              val newModel = model.copy(
                gameState = newGameState,
                pieces = newPieces,
                possibleMoveSpots = emptySpots,
                gameScore = newScore,
                turnNumber = newTurnNumber,
                turnTimer = newTT
              )

              gameStorage = gameStorage.appendGameTurn(gameStorage, newModel)

              scribe.debug("@@@ " + model.gameState.toString() + " -> " + newModel.gameState.toString())
              if newModel.gameState == GameState.FINISH then
                val results = constructResults(newModel)
                model
                  .modify(newModel, None, None)
                  .addGlobalEvents(Freeze.PanelContent(PanelType.P_RESULTS, results))
              else
                // game ongoing
                model.modify(newModel, None, None)
              end if
            else
              scribe.debug("@@@ CAPTORS @@@")
              val interimTurnNumber = model.turnNumber + 1
              val newModel1 = model.copy(turnNumber = interimTurnNumber)
              gameStorage = gameStorage.appendGameTurn(gameStorage, newModel1)
              model.modify(newModel1, None, None).addGlobalEvents(CaptorsEvent)
            end if

          case CaptorsEvent =>
            val emptySpots = Spots(Set.empty)
            val captors = Melee(model).detectCaptors(model, hexBoard)
            val newScore = model.pieces.extraTurnScoring(model)
            val newTT = sharedTurnTimer.restartForCaptors(model.turnTimer)
            val newPieces = Melee(model).rewardCaptors(model, captors)
            val newTurnNumber = model.turnNumber + 1

            val newModel2 =
              model.copy(pieces = newPieces, possibleMoveSpots = emptySpots, gameScore = newScore, turnNumber = newTurnNumber, turnTimer = newTT)

            gameStorage = gameStorage.appendGameTurn(gameStorage, newModel2)

            if newModel2.gameState == GameState.FINISH then
              val results = constructResults(newModel2)
              model
                .modify(newModel2, None, None)
                .addGlobalEvents(Freeze.PanelContent(PanelType.P_RESULTS, results))
            else
              // model ongoing
              model.modify(newModel2, None, None)
            end if

          // FIXME ... Keyboard Interface for testing purposes only ...
          case k: KeyboardEvent.KeyDown =>
            if k.keyCode == Key.ADD then Outcome(model).addGlobalEvents(ButtonPlusEvent)
            else if k.keyCode == Key.SUBTRACT then Outcome(model).addGlobalEvents(ButtonMinusEvent)
            else if k.keyCode == Key.ENTER then Outcome(model).addGlobalEvents(ButtonTurnEvent)
            else if k.keyCode == Key.F3 then Outcome(model).addGlobalEvents(SceneEvent.Previous)
            else if k.keyCode == Key.F4 then
              // ...
              Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, ("Error", "Test Error from GAME FKEY_F4")))
            else
              // ...
              Outcome(model)
            end if

          case FrameTick =>
            val t1 = System.currentTimeMillis / 100 // this is 10ths of a second
            val bNewBlinkOn = if (t1 % 10) > 0 then true else false
            if bNewBlinkOn != bBlinkOn then
              // update the global bBlinkOn
              bBlinkOn = bNewBlinkOn
            end if

            if sharedTurnTimer.expired(model.turnTimer) then
              val bCylinder = (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == CYLINDER)
              val bBlock = (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == BLOCK)
              if (bCylinder == true) || (bBlock == true) then
                // signal a button turn event to switch players
                Outcome(model).addGlobalEvents(ButtonTurnEvent)
              else
                // timer still running
                Outcome(model)
              end if
            else
              val possibleTT = sharedTurnTimer.update(model.turnTimer)
              possibleTT match
                case Some(tt) =>
                  Outcome(model.copy(turnTimer = tt))
                case None =>
                  Outcome(model)
              end match
            end if

          case _ =>
            Outcome(model)
      catch
        case h: HintException =>
          scribe.error("SceneGame updateModel " + h.getMessage())
          Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_HINT, ("*** FlicFlac Hint ***", h.getMessage())))

        case t: Throwable =>
          scribe.error("SceneGame updateModel " + t.getMessage())
          Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, ("Error", t.getMessage())))

  // end of GlobalEvent => Outcome[FlicFlacGameModel]

  end updateModel

  def checkTurnValidAndThrow(model: FlicFlacGameModel, errPos: String): Unit =
    val bBadCylinder = (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == BLOCK)
    val bBadBlock = (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == CYLINDER)
    if (bBadCylinder == true) || (bBadBlock == true) then
      // warn user palying out of turn
      throw new HintException(errPos + " ... Please wait for your turn")
    end if
  end checkTurnValidAndThrow

  def checkTurnValid(model: FlicFlacGameModel): Boolean =
    val bBadCylinder = (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == BLOCK)
    val bBadBlock = (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == CYLINDER)
    if (bBadCylinder == true) || (bBadBlock == true) then
      // warn user invalid event, playing out of turn
      return false
    else
      // valid event
      return true
    end if
  end checkTurnValid

  def increaseScaleFactor(oldSF: Double): Double =
    val newSF =
      if oldSF >= 0.9 then 1.0
      else if oldSF >= 0.8 then 0.9
      else if oldSF >= 0.75 then 0.8
      else if oldSF >= 0.67 then 0.75
      else if oldSF >= 0.5 then 0.67
      else 0.5
    scribe.debug("@@@ increaseScaleFactor to:" + newSF)
    newSF
  end increaseScaleFactor

  def decreaseScaleFactor(oldSF: Double): Double =
    val newSF =
      if oldSF <= 0.5 then 0.33
      else if oldSF <= 0.67 then 0.5
      else if oldSF <= 0.75 then 0.67
      else if oldSF <= 0.8 then 0.75
      else if oldSF <= 0.9 then 0.8
      else 0.9
    scribe.debug("@@@ decreaseScaleFactor to:" + newSF)
    newSF
  end decreaseScaleFactor

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    case FrameTick =>
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)

    case e: PointerEvent.PointerMove =>
      model.pieces.findPieceSelected(model) match
        case Some(p) =>
          viewModel.optDragPos = Some(e.position)

        case None =>
          viewModel.optDragPos = None
      end match
      Outcome(viewModel)

    case ButtonPlusEvent =>
      val newViewModel = viewModel.changeButtonBoundaries(model, viewModel.gameViewport)
      Outcome(newViewModel)

    case ButtonMinusEvent =>
      val newViewModel = viewModel.changeButtonBoundaries(model, viewModel.gameViewport)
      Outcome(newViewModel)

    case ViewportResize(gameViewPort) =>
      val newViewModel = viewModel.changeButtonBoundaries(model, gameViewPort)
      Outcome(newViewModel)

    case _ =>
      Outcome(viewModel)
  end updateViewModel

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val dSF = hexBoard4.scalingFactor

    val x10 = (70 * dSF).toInt
    val y10 = (1033 * dSF).toInt
    val x11 = (70 * dSF).toInt
    val y11 = (1066 * dSF).toInt
    val x12 = (90 * dSF).toInt
    val y12 = (53 * dSF).toInt

    val zoomLabel =
      TextBox("Zoom", 100, 70).alignCenter
        .withColor(RGBA.Black)
        .withFontSize(Pixels(30))
        .scaleBy(dSF, dSF)
        .moveTo(x10, y10)
    val sFactor = ((100 * dSF).toInt).toString()
    val zoomPercentage =
      TextBox(sFactor + "%", 100, 70).alignCenter
        .withColor(RGBA.Black)
        .withFontSize(Pixels(30))
        .scaleBy(dSF, dSF)
        .moveTo(x11, y11)

    val turnLabel = TextBox("Turn:" + (model.turnNumber + 1).toString(), 200, 40).alignLeft
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .scaleBy(dSF, dSF)
      .moveTo(x12, y12)

    val pB = hexBoard4.pBase // ................... for HighLighter

    val width = GameAssets.GetGameSceneDimensions(model.boardSize).width
    val height = GameAssets.GetGameSceneDimensions(model.boardSize).height

    val iHeight = (math.round(GameAssets.GetGameSceneDimensions(model.boardSize).height * dSF)).toInt
    val iLeftWidth = hexBoard4.pBase.x
    val iRightWidth = (math.round(GameAssets.GetGameSceneDimensions(model.boardSize).right - hexBoard4.pBase.x) * dSF).toInt
    val rLeft = Rectangle(0, 0, iLeftWidth, iHeight)
    val rRight = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth, iHeight))
    val rCorners = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth + hexBoard4.pBase.x, iHeight))

    val colorOurNameTitle =
      if (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == CYLINDER) then RGBA.Magenta
      else if (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == BLOCK) then RGBA.Magenta
      else RGBA.Black

    val youAre =
      TextBox(model.ourName + "    ", iRightWidth, 50) // adding 4 spaces to get a simple central alignment
        .bold.alignCenter
        .withColor(colorOurNameTitle)
        .withFontSize(Pixels(40))
        .scaleBy(dSF, dSF)
        .moveTo(iLeftWidth, 2)

    val diag =
      TextBox(dMsg, 200, 30)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(20))
        .moveTo(0, 0)

    val ourPieceShape = model.ourPieceType

// format: off

    Outcome(
      SceneUpdateFragment(LayerKeys.Background -> Layer.empty)
        |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(rLeft, Fill.Color(RGBA.White))))
        |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(rRight, Fill.Color(RGBA.White))))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(GameAssets.cornerLayers(rCorners, 1.0, RGBA.Magenta)))

// The diag fragment shows the diagnostic dMsg pointer handler events in the top LH corner
//        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(diag))

        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.turnButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(youAre))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(turnLabel))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> scorePanel.paint(model, ourPieceShape, bBlinkOn, dSF))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> paramsPanel.paint(model, dSF))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.plusButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(zoomLabel))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(zoomPercentage))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.minusButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.newGameButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> model.turnTimer.ttPaint(model))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> hexBoard4.hbPaint(model, dSF))
        |+| SceneUpdateFragment(LayerKeys.ForegroundHighL -> model.highLighter.hlPaint(model, hexBoard4, dSF, pB))
        |+| SceneUpdateFragment(LayerKeys.ForegroundSpots -> model.possibleMoveSpots.spPaint(model))
        |+| SceneUpdateFragment(LayerKeys.ForegroundPieces -> model.pieces.piPaint(model, dSF, bBlinkOn, viewModel.optDragPos))
    )
// format: on
  end present

  def coordXFromScore(score: Int): Int =
    if score < 10 then 150
    else 120
  end coordXFromScore

end SceneGame

final case class GameSceneViewModel(
    var optDragPos: Option[Point],
    gameViewport: GameViewport,
    newGameButton: Button,
    plusButton: Button,
    minusButton: Button,
    turnButton: Button
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[GameSceneViewModel] =
    for
      bn1 <- newGameButton.updateFromPointers(pointers)
      bn2 <- plusButton.updateFromPointers(pointers)
      bn3 <- minusButton.updateFromPointers(pointers)
      bn4 <- turnButton.updateFromPointers(pointers)
    yield this.copy(newGameButton = bn1, plusButton = bn2, minusButton = bn3, turnButton = bn4)

  def changeButtonBoundaries(model: FlicFlacGameModel, gvp: GameViewport): GameSceneViewModel =
    val dSF = hexBoard4.scalingFactor

    val newNewGameButton =
      Button(
        buttonAssets = GameAssets.buttonNewGameAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.newGameBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonNewGameEvent)

    val newPlusButton =
      Button(
        buttonAssets = GameAssets.buttonPlusAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.plusBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonPlusEvent)

    val newMinusButton =
      Button(
        buttonAssets = GameAssets.buttonMinusAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.minusBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonMinusEvent)

    val newTurnButton =
      Button(
        buttonAssets = GameAssets.buttonTurnAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.turnBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonTurnEvent)

    this.copy(
      // scalingFactor
      // optDragPos
      newGameButton = newNewGameButton,
      plusButton = newPlusButton,
      minusButton = newMinusButton,
      turnButton = newTurnButton
    )

  end changeButtonBoundaries

end GameSceneViewModel

object GameSceneViewModel:
  val turnBounds = Rectangle(10, 30, 90, 80)
  val plusBounds = Rectangle(5, 1025, 90, 80)
  val minusBounds = Rectangle(170, 1025, 90, 80)
  val newGameBounds = Rectangle(5, 1125, 240, 80)

  val initial: GameSceneViewModel =
    GameSceneViewModel(
      None, // ... we have no last position of the pointer recorded

      GameViewport(
        GameAssets.GetGameSceneDimensions(8).width,
        GameAssets.GetGameSceneDimensions(8).height
      ), // default model.size is 8
      Button(
        buttonAssets = GameAssets.buttonNewGameAssets(1.0),
        bounds = newGameBounds,
        depth = Depth(6)
      ).withUpActions(ButtonNewGameEvent),
      Button(
        buttonAssets = GameAssets.buttonPlusAssets(1.0),
        bounds = plusBounds,
        depth = Depth(6)
      ).withUpActions(ButtonPlusEvent),
      Button(
        buttonAssets = GameAssets.buttonMinusAssets(1.0),
        bounds = minusBounds,
        depth = Depth(6)
      ).withUpActions(ButtonMinusEvent),
      Button(
        buttonAssets = GameAssets.buttonTurnAssets(1.0),
        bounds = turnBounds,
        depth = Depth(6)
      ).withUpActions(ButtonTurnEvent)
    )
end GameSceneViewModel

def constructResults(model: FlicFlacGameModel): (String, String) =
  val cylinderScore = model.gameScore._1
  val blockScore = model.gameScore._2
  val (cylinderName, blockName) =
    if model.ourPieceType == CYLINDER then
      // we are cylinder
      (model.ourName, model.oppoName)
    else
      // we are block
      (model.oppoName, model.ourName)
    end if
  end val
  val results: (String, String) =
    if (cylinderScore >= model.winningScore) && (cylinderScore >= blockScore + 2) then
      (
        "*** " + cylinderName + " WINS ***",
        "Scores: " + cylinderName + ":" + cylinderScore + " " + blockName + ":" + blockScore + " ..."
      )
    else if (blockScore >= model.winningScore) && (blockScore >= cylinderScore + 2) then
      (
        "*** " + blockName + " WINS ***",
        "Scores: " + blockName + ":" + blockScore + " " + cylinderName + ":" + cylinderScore + " ..."
      )
    else ("???", "???")
    end if
  end results
  results
end constructResults

object FlicFlacGameUpdate:
  case class Info(ffgm: FlicFlacGameModel) extends GlobalEvent
end FlicFlacGameUpdate

class HintException(s: String) extends Exception(s) {}
