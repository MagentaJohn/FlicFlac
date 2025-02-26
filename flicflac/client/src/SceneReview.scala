package game

import shared.*

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.platform.networking.Network

object SceneReview extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:
  type SceneModel = FlicFlacGameModel
  type SceneViewModel = ReviewSceneViewModel

  def name: SceneName = SceneName("Review")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.reviewScene,
      (m, vm) => m.copy(reviewScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set.empty

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

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =
    e =>
      try
        e match

          case StartReviewGame =>
            scribe.debug("@@@ StartReviewGame")
            val startingPieces = model.pieces.summonPieces(hexBoard) // .......... establish new starting positions
            val newModel1 = model.copy( // ........................................ create new model as first turn to be stored
              pieces = startingPieces,
              gameState = GameState.CYLINDER_TURN
            )

            val newModel2 = gameStorage.meldStorageToModel(context.frameContext.startUpData, newModel1)
            Outcome(newModel2).addGlobalEvents(ButtonReviewStartEvent)

          case k: KeyboardEvent.KeyDown =>
            if k.keyCode == Key.UP_ARROW then Outcome(model).addGlobalEvents(ButtonReviewStartEvent)
            else if k.keyCode == Key.LEFT_ARROW then Outcome(model).addGlobalEvents(ButtonReviewBackwardEvent)
            else if k.keyCode == Key.RIGHT_ARROW then Outcome(model).addGlobalEvents(ButtonReviewForwardEvent)
            else if k.keyCode == Key.DOWN_ARROW then Outcome(model).addGlobalEvents(ButtonReviewFinishEvent)
            else Outcome(model)
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

          case ButtonReviewStartEvent =>
            scribe.debug("@@@ ButtonReviewStartEvent")
            gameStorage = gameStorage.traverseGameStorage(StepType.ST_Start)
            val newModel = gameStorage.meldStorageToModel(context.frameContext.startUpData, model)
            Outcome(newModel)

          case ButtonReviewBackwardEvent =>
            scribe.debug("@@@ ButtonReviewBackwardEvent")
            gameStorage = gameStorage.traverseGameStorage(StepType.ST_Backward)
            val newModel = gameStorage.meldStorageToModel(context.frameContext.startUpData, model)
            Outcome(newModel)

          case ButtonReviewForwardEvent =>
            scribe.debug("@@@ ButtonReviewForwardEvent")
            gameStorage = gameStorage.traverseGameStorage(StepType.ST_Forward)
            val newModel = gameStorage.meldStorageToModel(context.frameContext.startUpData, model)
            Outcome(newModel)

          case ButtonReviewFinishEvent =>
            scribe.debug("@@@ ButtonReviewFinishEvent")
            gameStorage = gameStorage.traverseGameStorage(StepType.ST_Finish)
            val newModel = gameStorage.meldStorageToModel(context.frameContext.startUpData, model)
            Outcome(newModel)

          case _ =>
            Outcome(model)
      catch
        case t: Throwable =>
          scribe.error("SceneReview updateModel " + t.getMessage())
          Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, ("Error", t.getMessage())))

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
    val x12 = (10 * dSF).toInt
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

    val width = GameAssets.GetGameSceneDimensions(8).width // default size 8
    val height = GameAssets.GetGameSceneDimensions(8).height // default size 8

    val iHeight = (math.round(GameAssets.GetGameSceneDimensions(model.boardSize).height * dSF)).toInt
    val iLeftWidth = hexBoard4.pBase.x
    val iRightWidth = (math.round(GameAssets.GetGameSceneDimensions(model.boardSize).right - hexBoard4.pBase.x) * dSF).toInt
    val rLeft = Rectangle(0, 0, iLeftWidth, iHeight)
    val rRight = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth, iHeight))
    val rCorners = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth + hexBoard4.pBase.x, iHeight))

    val g3Text = context.frameContext.startUpData._1.g3.drop(4) // remove the prefix "###-"
    val gameName =
      TextBox(g3Text + "    ", iRightWidth, 50) // adding 4 spaces to get a simple central alignment
        .bold.alignCenter
        .withColor(RGBA.Black)
        .withFontSize(Pixels(40))
        .scaleBy(dSF, dSF)
        .moveTo(iLeftWidth, 2)

    val turnLabel = TextBox("Turn:" + (model.turnNumber).toString() + "/" + (gameStorage.turns.length - 1).toString(), 250, 40).alignLeft
      .withColor(RGBA.Black)
      .bold
      .withFontSize(Pixels(40))
      .scaleBy(dSF, dSF)
      .moveTo(x12, y12)

    val ourPieceShape = gameStorage.params.playPams3_PieceShape

// format: off

    Outcome(
      SceneUpdateFragment(LayerKeys.Background -> Layer.empty)
        |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(rLeft, Fill.Color(RGBA.White))))
        |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(rRight, Fill.Color(RGBA.White))))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(GameAssets.cornerLayers(rCorners, 1.0, RGBA.Magenta)))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(gameName))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(turnLabel))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> scorePanel.paint(model, ourPieceShape, true, dSF))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> paramsPanel.paint(model, dSF))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.plusButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(zoomLabel))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(zoomPercentage))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.minusButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.startButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.backwardButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.forwardButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.finishButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> hexBoard4.hbPaint(model, dSF))
        |+| SceneUpdateFragment(LayerKeys.ForegroundPieces -> model.pieces.piPaint(model, dSF, true, viewModel.optDragPos))
    )
// format: on
  end present

end SceneReview

final case class ReviewSceneViewModel(
    var optDragPos: Option[Point],
    gameViewport: GameViewport,
    plusButton: Button,
    minusButton: Button,
    startButton: Button,
    backwardButton: Button,
    forwardButton: Button,
    finishButton: Button
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[ReviewSceneViewModel] =
    for
      bn1 <- plusButton.updateFromPointers(pointers)
      bn2 <- minusButton.updateFromPointers(pointers)
      bn3 <- startButton.updateFromPointers(pointers)
      bn4 <- backwardButton.updateFromPointers(pointers)
      bn5 <- forwardButton.updateFromPointers(pointers)
      bn6 <- finishButton.updateFromPointers(pointers)
    yield this.copy(plusButton = bn1, minusButton = bn2, startButton = bn3, backwardButton = bn4, forwardButton = bn5, finishButton = bn6)

  def changeButtonBoundaries(model: FlicFlacGameModel, gvp: GameViewport): ReviewSceneViewModel =
    val dSF = hexBoard4.scalingFactor

    val newPlusButton =
      Button(
        buttonAssets = GameAssets.buttonPlusAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(ReviewSceneViewModel.plusBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonPlusEvent)

    val newMinusButton =
      Button(
        buttonAssets = GameAssets.buttonMinusAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(ReviewSceneViewModel.minusBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonMinusEvent)

    val newStartButton =
      Button(
        buttonAssets = GameAssets.buttonReviewStartAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(ReviewSceneViewModel.startBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonReviewStartEvent)

    val newBackwardButton =
      Button(
        buttonAssets = GameAssets.buttonReviewBackwardAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(ReviewSceneViewModel.backwardBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonReviewBackwardEvent)

    val newForwardButton =
      Button(
        buttonAssets = GameAssets.buttonReviewForwardAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(ReviewSceneViewModel.forwardBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonReviewForwardEvent)

    val newFinishButton =
      Button(
        buttonAssets = GameAssets.buttonReviewFinishAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(ReviewSceneViewModel.finishBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonReviewFinishEvent)

    this.copy(
      // scalingFactor
      // optDragPos
      plusButton = newPlusButton,
      minusButton = newMinusButton,
      startButton = newStartButton,
      backwardButton = newBackwardButton,
      forwardButton = newForwardButton,
      finishButton = newFinishButton
    )

  end changeButtonBoundaries

end ReviewSceneViewModel

object ReviewSceneViewModel:
  val plusBounds = Rectangle(5, 1025, 90, 80)
  val minusBounds = Rectangle(170, 1025, 90, 80)
  val startBounds = Rectangle(10, 1125, 90, 80)
  val backwardBounds = Rectangle(100, 1125, 90, 80)
  val forwardBounds = Rectangle(190, 1125, 90, 80)
  val finishBounds = Rectangle(280, 1125, 240, 80)

  val initial: ReviewSceneViewModel =
    ReviewSceneViewModel(
      None, // ... we have no last position of the pointer recorded

      GameViewport(
        GameAssets.GetGameSceneDimensions(8).width,
        GameAssets.GetGameSceneDimensions(8).height
      ), // default model.size is 8
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
        buttonAssets = GameAssets.buttonReviewStartAssets(1.0),
        bounds = startBounds,
        depth = Depth(6)
      ).withUpActions(ButtonReviewStartEvent),
      Button(
        buttonAssets = GameAssets.buttonReviewBackwardAssets(1.0),
        bounds = backwardBounds,
        depth = Depth(6)
      ).withUpActions(ButtonReviewForwardEvent),
      Button(
        buttonAssets = GameAssets.buttonReviewForwardAssets(1.0),
        bounds = forwardBounds,
        depth = Depth(6)
      ).withUpActions(ButtonReviewBackwardEvent),
      Button(
        buttonAssets = GameAssets.buttonReviewFinishAssets(1.0),
        bounds = finishBounds,
        depth = Depth(6)
      ).withUpActions(ButtonReviewFinishEvent)
    )
end ReviewSceneViewModel
