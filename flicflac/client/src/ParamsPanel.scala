package game

import indigo.*

final case class ParamsPanel():
  val p0 = Point(0, 590) // ......... coords of params panel on the game scene when zoom at 100%
  val p1 = Point(140, 670) // ....... coords of winning score
  val p2 = Point(140, 755) // ....... coords of total turn time
  val p3 = Point(140, 840) // ....... coords of captors turn time
  val p4 = Point(140, 925) // ....... coords of random event probability

  def show(model: FlicFlacGameModel, dSF: Double): Layer =

    val p0Scaled = p0 * dSF
    val p1Scaled = p1 * dSF
    val p2Scaled = p2 * dSF
    val p3Scaled = p3 * dSF
    val p4Scaled = p4 * dSF

    val paramsPanel = GameAssets.gParamsPanel(dSF).moveTo(p0Scaled)

    val param1 =
      TextBox((model.winningScore).toString(), 100, 70).bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .scaleBy(dSF, dSF)
        .moveTo(p1Scaled)

    val param2 =
      TextBox((model.turnTimer.iTotalTurnTime).toString(), 100, 70).bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .scaleBy(dSF, dSF)
        .moveTo(p2Scaled)

    val param3 =
      TextBox((model.turnTimer.iCaptorsTurnTime).toString(), 100, 70).bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .scaleBy(dSF, dSF)
        .moveTo(p3Scaled)
    val param4 =
      TextBox((model.randEventFreq).toString(), 100, 70).bold
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

  end show

end ParamsPanel
