package game

import shared.*

import indigo.*

final case class ParamsPanel():
  val p0 = Point(0, 590) // ......... coords of params panel on the game scene when zoom at 100%
  val p1 = Point(130, 670) // ....... coords of winning score
  val p2 = Point(130, 755) // ....... coords of total turn time
  val p3 = Point(130, 840) // ....... coords of captors turn time
  val p4 = Point(130, 925) // ....... coords of random event probability

  def paint(model: FlicFlacGameModel, dSF: Double): Layer =

    val p0Scaled = p0 * dSF
    val p1Scaled = p1 * dSF
    val p2Scaled = p2 * dSF
    val p3Scaled = p3 * dSF
    val p4Scaled = p4 * dSF

    val paramsPanel = GameAssets.gParamsPanel(dSF).moveTo(p0Scaled)

    val param1 =
      TextBox((model.winningScore).toString(), 110, 70).bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .scaleBy(dSF, dSF)
        .moveTo(p1Scaled)

    val param2 =
      TextBox((model.turnTimer.iTotalTurnTime).toString(), 110, 70).bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .scaleBy(dSF, dSF)
        .moveTo(p2Scaled)

    val param3 =
      TextBox((model.turnTimer.iCaptorsTurnTime).toString(), 110, 70).bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .scaleBy(dSF, dSF)
        .moveTo(p3Scaled)

    val param4 =
      TextBox((model.randEventFreq).toString(), 110, 70).bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .scaleBy(dSF, dSF)
        .moveTo(p4Scaled)

    val layer0 = Layer(paramsPanel)
    val layer1 = Layer(param1)
    val layer2 = Layer(param2)
    val layer3 = Layer(param3)
    val layer4 = Layer(param4)

    (layer0 |+| layer1 |+| layer2 |+| layer3 |+| layer4)

  end paint

end ParamsPanel
