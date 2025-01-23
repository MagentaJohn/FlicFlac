package game

import shared.*

import indigo.*
import indigoextras.ui.*

object FlicFlacStartupData:

  scribe.debug("@@@ Object FlicFlacStartupData START")

  def initialise(
      flicFlacBootData: FlicFlacBootData
  ): Outcome[Startup[FlicFlacStartupData]] =
    Outcome(
      Startup.Success(createStartupData(flicFlacBootData))
    )

  def createStartupData(flicFlacBootData: FlicFlacBootData): FlicFlacStartupData =
    FlicFlacStartupData(
      flicFlacBootData = flicFlacBootData,
      staticAssets = StaticAssets(
        hexGraphic = GameAssets.gHex(1.0),
        buttonSplashAsset = GameAssets.buttonSplashAssets(1.0),
        buttonRulesAsset = GameAssets.buttonRulesAssets(1.0),
        buttonPlayAsset = GameAssets.buttonPlayAssets(1.0),
        buttonResultsAsset = GameAssets.buttonResultsAssets(1.0)
      )
    )
  scribe.debug("@@@ Object FlicFlacStartupData FINISH")
end FlicFlacStartupData

final case class FlicFlacStartupData(flicFlacBootData: FlicFlacBootData, staticAssets: StaticAssets)

final case class StaticAssets(
    hexGraphic: Graphic[Material.ImageEffects],
    buttonSplashAsset: ButtonAssets,
    buttonRulesAsset: ButtonAssets,
    buttonPlayAsset: ButtonAssets,
    buttonResultsAsset: ButtonAssets
)

final case class FlicFlacBootData(pixelWidth: Int, pixelHeight: Int, n1: String, n2: String, viewport: GameViewport):
  val width = pixelWidth
  val height = pixelHeight
  val name1 = n1
  val name2 = n2
  val gameViewPort = viewport
end FlicFlacBootData

object FlicFlacBootData:
  scribe.debug("@@@ Object FlicFlacBootData START")

  def create(w: Int, h: Int, n1: String, n2: String): FlicFlacBootData =
    FlicFlacBootData(w, h, n1, n2, GameViewport(w, h))

  scribe.debug("@@@ Object FlicFlacBootData FINISH")
end FlicFlacBootData

object FlicFlacConfig:
  scribe.debug("@@@ Object FlicFlacConfig START")
  val config: GameConfig =
    GameConfig(
      viewport = GameViewport(
        GameAssets.GetGameSceneDimensions(BOARD_SIZE_XLARGE).width,
        GameAssets.GetGameSceneDimensions(BOARD_SIZE_XLARGE).height
      ), // default model.boardSize is 8
      frameRateLimit = Option(FPS.`30`), // this is the slowest FPS indigo game engine allows ... JP 27/08/24
      clearColor = RGBA.fromHexString("#000000"),
      magnification = 1,
      transparentBackground = true,
      // the default setting from indigo is "ResizePreserveAspect" but Simon found that this corrupts the
      // css height attribute on a resize, which in turn chops off the bottom of the board.
      // The setting we discovered is simply "Resize" which keeps the width and height intact ... JP 27/08/24
      resizePolicy = ResizePolicy.Resize,
      advanced = AdvancedGameConfig(
        renderingTechnology = RenderingTechnology.WebGL2WithFallback,
        antiAliasing = false,
        premultipliedAlpha = true,
        batchSize = 256,
        autoLoadStandardShaders = true,
        disableContextMenu = true
      )
    )
  scribe.debug("@@@ Object FlicFlacConfig FINISH")
end FlicFlacConfig
