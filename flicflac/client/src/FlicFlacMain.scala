package game

import shared.*

import indigo.*
import indigo.scenes.*
import scala.scalajs.js.annotation.JSExportTopLevel
import indigoextras.ui.*
import indigo.shared.assets.AssetType
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.events.MouseEvent
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.math.*
import io.circe.parser.decode
import scribe.*
import scribe.format.*

import org.scalajs.dom
import tyrian.TyrianSubSystem
import cats.effect.IO
import tyrian.TyrianIndigoBridge

object LayerKeys:
  val Background: BindingKey = BindingKey("Background")
  val Middleground: BindingKey = BindingKey("Middleground")
  val ForegroundHighL: BindingKey = BindingKey("ForegroundHighL")
  val ForegroundSpots: BindingKey = BindingKey("ForegroundSpots")
  val ForegroundPieces: BindingKey = BindingKey("ForegroundPieces")
  val Overlay: BindingKey = BindingKey("Overlay")
end LayerKeys

val hexBoard = new HexBoard()
val hexBoard4 = new HexBoard4()
val scorePanel = new ScorePanel()
val paramsPanel = new ParamsPanel()
val sharedTurnTimer = TurnTimer(0, 0, false, false, 0, 0)
var gameStorage = new GameStorage("", PlayerParams("", "", 0, 0, 0, 0, 0, 0), 1, List.empty)

case class FlicFlacGame(
    tyrianSubSystem: TyrianSubSystem[IO, Int, FlicFlacGameModel]
) extends IndigoGame[FlicFlacBootData, FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  // scribe reporting levels: fatal,error,warn,info,debug,trace
  // val myFormatter: Formatter = formatter"[$threadName] $positionAbbreviated - $message$newLine"
  Logger.root
    .clearHandlers()
    .withHandler(formatter = Formatter.simple)
    .withMinimumLevel(Level.Debug)
    .replace()

  val magnification = 1

  val config: GameConfig =
    // The default from indigo was the following line ...
    // GameConfig.default.withMagnification(magnification)
    // ... but Simon discovered we need to use our own config in order to control viewport
    // properly and avoid "scaling & chopping issues" ... JP 26/08/24
    FlicFlacConfig.config

  val assets: Set[AssetType] =
    GameAssets.get()

  def initialScene(flicFlacBootData: FlicFlacBootData): Option[SceneName] =
    scribe.debug("@@@ FlicFlacMain-initialScene()")
    Some(SceneParams.name)
  end initialScene

  def scenes(
      flicFlacBootData: FlicFlacBootData
  ): NonEmptyList[Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]] =
    scribe.debug("@@@ FlicFlacMain-scenes()")
    NonEmptyList(SceneParams, SceneGame, SceneReview)
  end scenes

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[FlicFlacBootData, FlicFlacGameModel]] =
    scribe.debug("@@@ FlicFlacMain-boot")
    scribe.debug("@@@ BootFlags: " + flags)
    val width = flags("width").toInt
    val height = flags("height").toInt
    val name1: String = flags("name1")
    val name2: String = flags("name2")
    val game3: String = flags("game3")
    Outcome {
      val flicFlacBootData: FlicFlacBootData =
        FlicFlacBootData.create(width, height, name1, name2, game3)
      // ViewConfig.default

      val config =
        FlicFlacConfig.config
          .withViewport(flicFlacBootData.viewport)

      val assetPath: String =
        flags.getOrElse("baseUrl", "")

      BootResult(config, flicFlacBootData)
        .withAssets(assets)
        .withSubSystems(tyrianSubSystem, SSPeerJS("SubSystemPeerJS"))
    }
  end boot

  def initialModel(flicFlacStartupData: FlicFlacStartupData): Outcome[FlicFlacGameModel] =
    val pp = new PlayerParams("", "", 0, 0, 0, 0, 0, 0) // dummy PlayerParams to get access to getParams
    val cachedParamsOrNew = pp.getParams(flicFlacStartupData)

    val reviewGameName = flicFlacStartupData.flicFlacBootData.g3
    if reviewGameName.length == 0 then
      scribe.debug("@@@ FlicFlacMain-initialModel() for live game")
      scribe.debug(s"@@@ PlayerParams: $cachedParamsOrNew")
      val initialFlicFlacGameModel = FlicFlacGameModel()
      val newTurnTime = cachedParamsOrNew.playPams6_TurnTime
      val newCaptorsTime = cachedParamsOrNew.playPams7_CaptorsTime
      val newTT = TurnTimer(newTurnTime, newCaptorsTime, false, false, 0, 0)
      val cachedGameOrNew = initialFlicFlacGameModel.retrieve(flicFlacStartupData)
      gameStorage = gameStorage.establishGameStorage(cachedParamsOrNew)
      val updatedGame = cachedGameOrNew.copy(turnTimer = newTT)

      Outcome(updatedGame)
        .addGlobalEvents(DetectParams)
        .addGlobalEvents(WebRtcEvent.MakePeerEntity)
    else
      scribe.debug("@@@ FlicFlacMain-initialModel() for review game")
      gameStorage = gameStorage.readGameStorage(reviewGameName) match
        case Some(gs) => gs
        case None =>
          scribe.error("@@@ GameStorage Database FlicFlac-Index corrupted")
          new GameStorage(reviewGameName, cachedParamsOrNew, 0, List.empty)

      // create the shared hexboard
      hexBoard.forge(gameStorage.params.playPams4_BoardSize)

      // derive the client hexboard, hexboard4
      hexBoard4.derive(hexBoard)

      val model1 = FlicFlacGameModel()
      val model2 = model1.creation(gameStorage.params)
      Outcome(model2)
        .addGlobalEvents(DetectParams)
    end if
  end initialModel

  def initialViewModel(
      flicFlacStartupData: FlicFlacStartupData,
      flicFlacGameModel: FlicFlacGameModel
  ): Outcome[FlicFlacViewModel] =
    scribe.debug("@@@ FlicFlacMain-initialViewModel()")
    val w = flicFlacStartupData.flicFlacBootData.gameViewPort.width
    val h = flicFlacStartupData.flicFlacBootData.gameViewPort.height
    val staticAssets = flicFlacStartupData.staticAssets

    Outcome(
      FlicFlacViewModel(
        staticAssets,
        GameSceneViewModel.initial,
        ReviewSceneViewModel.initial,
        flicFlacStartupData.flicFlacBootData.gameViewPort
      )
    )
  end initialViewModel

  def setup(
      flicFlacBootData: FlicFlacBootData,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[FlicFlacStartupData]] =
    scribe.debug("@@@ FlicFlacMain-setup()")
    val outCome = FlicFlacStartupData.initialise(flicFlacBootData)
    outCome
  end setup

  def updateModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =

    case _ =>
      Outcome(flicFlacGameModel)
  end updateModel

  def updateViewModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): GlobalEvent => Outcome[FlicFlacViewModel] =
    case FrameTick =>
      Outcome(flicFlacViewModel)

    case ViewportResize(gameViewPort) =>
      val w = gameViewPort.width
      val h = gameViewPort.height
      scribe.debug("@@@ FlicFlacMain-updateViewModel ViewportResize w:h " + w + ":" + h)
      Outcome(flicFlacViewModel.copy(theGameViewPort = gameViewPort))

    case ButtonPlayEvent =>
      scribe.debug("@@@ Main-ButtonGameEvent")
      Outcome(flicFlacViewModel)
        .addGlobalEvents(SceneEvent.JumpTo(SceneGame.name))
        .addGlobalEvents(ViewportResize(flicFlacViewModel.theGameViewPort))

    case _ =>
      Outcome(flicFlacViewModel)
  end updateViewModel

  def present(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): Outcome[SceneUpdateFragment] = // this technique supplied by DaveSmith
    Outcome(
      SceneUpdateFragment(
        LayerKeys.Background -> Layer.empty, // Initialising keys early (root level), in the desired order
        LayerKeys.Middleground -> Layer.empty,
        LayerKeys.ForegroundHighL -> Layer.empty,
        LayerKeys.ForegroundSpots -> Layer.empty,
        LayerKeys.ForegroundPieces -> Layer.empty,
        LayerKeys.Overlay -> Layer.empty
      )
    )

  scribe.debug("@@@ FlicFlacMain class FlicFlacGame Finish")
end FlicFlacGame

def GetScaleFactor(viewWidth: Int, viewHeight: Int, sceneDimensions: Rectangle): Double =
  val dsfx: Double = viewWidth.toDouble / sceneDimensions.width
  val dsfy: Double = viewHeight.toDouble / sceneDimensions.height
  val dToCheck = if dsfx > dsfy then dsfy else dsfx

  val dSF =
    if dToCheck >= 1.0 then 1.0
    else if dToCheck >= 0.9 then 0.9
    else if dToCheck >= 0.8 then 0.8
    else if dToCheck >= 0.75 then 0.75
    else if dToCheck >= 0.67 then 0.67
    else if dToCheck >= 0.5 then 0.5
    else if dToCheck >= 0.33 then 0.33
    else 0.25
  dSF
end GetScaleFactor

//final case class ViewModel()
final case class FlicFlacViewModel(
    staticAssets: StaticAssets,
    gameScene: GameSceneViewModel,
    reviewScene: ReviewSceneViewModel,
    theGameViewPort: GameViewport
)

case object ButtonSplashEvent extends GlobalEvent
case object ButtonRulesEvent extends GlobalEvent
case object ButtonPlayEvent extends GlobalEvent
case object ButtonNewGameEvent extends GlobalEvent
case object ButtonResultsEvent extends GlobalEvent
case object ButtonParamsEvent extends GlobalEvent
case object ButtonPlusEvent extends GlobalEvent
case object ButtonMinusEvent extends GlobalEvent
case object ButtonReviewStartEvent extends GlobalEvent
case object ButtonReviewBackwardEvent extends GlobalEvent
case object ButtonReviewForwardEvent extends GlobalEvent
case object ButtonReviewFinishEvent extends GlobalEvent
case object ButtonTurnEvent extends GlobalEvent
case object CaptorsEvent extends GlobalEvent
case object StartLiveGame extends GlobalEvent
case object StartReviewGame extends GlobalEvent

object Freeze:
  case class PanelContent(panelType: PanelType, content: (String, String)) extends GlobalEvent
end Freeze
