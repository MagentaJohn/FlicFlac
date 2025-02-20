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
  type SceneViewModel = GameSceneViewModel

  def name: SceneName = SceneName("Review")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set.empty

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: SceneModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    e => Outcome(viewModel)

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =
    e =>
      e match

        case _ =>
          Outcome(model)

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: SceneModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =
    val width = GameAssets.GetGameSceneDimensions(8).width // default size 8
    val height = GameAssets.GetGameSceneDimensions(8).height // default size 8

    val textT1 = TextBox("*** FlicFlac Review ***", width, 80)
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(60))
      .alignCenter
      .moveTo(0, 0)

// format: off
    Outcome(
      SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black))))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(Batch(textT1)))
    )
// format: on    
  end present


end SceneReview

