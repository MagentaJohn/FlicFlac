package game

import shared.*

import tyrian.Html.*
import tyrian.*
import cats.effect.IO
import org.scalajs.dom
import scala.concurrent.duration.DurationDouble
import scala.scalajs.js.annotation.*
import scribe.*

enum Msg:
  case StartIndigo extends Msg
  case DoNothing extends Msg
  case RunGame extends Msg
  case RetryIndigo extends Msg
  case NavigateToUrl(url: String) extends Msg
end Msg

final case class TyrianModel(
    bridge: TyrianIndigoBridge[IO, Int, FlicFlacGameModel],
    renderUi: Boolean = true
)
object TyrianModel:
  val init: TyrianModel = TyrianModel(TyrianIndigoBridge())
end TyrianModel

object TyrianApp extends TyrianIOApp[Msg, TyrianModel]:

  override def router: Location => Msg = {
    case loc: Location.Internal =>
      scribe.info(s"loc.fullPath: ${loc.fullPath}")
      loc.fullPath.contains("runGame=true") match
        case true =>
          Msg.StartIndigo
        case false =>
          Msg.DoNothing
      end match
    case ext =>
      Msg.NavigateToUrl(ext.href)
  }

  def init(flags: Map[String, String]): (TyrianModel, Cmd[IO, Msg]) =
    (TyrianModel.init, Cmd.Emit(Msg.DoNothing))

  def update(model: TyrianModel): Msg => (TyrianModel, Cmd[IO, Msg]) = {
    // format: off
    case Msg.StartIndigo =>
      println("Starting Indigo")
      println(model)
      val task: IO[Msg] = IO.delay{
        if dom.document.getElementById("indigo-container") == null then
          Msg.RetryIndigo
        else
          val i1 = dom.window.location.href.indexOf("?N1=")
          val i2 = dom.window.location.href.indexOf("&N2=")

          val sName1A = dom.window.location.href.substring(i1+4)
          val i3 = sName1A.indexOf("&")
          val sName1 = sName1A.substring(0,i3)

          val sName2A = dom.window.location.href.substring(i2+4)
          val i4 = sName2A.indexOf("&")
          val sName2 = sName2A.substring(0,i4)

          scribe.info("@@@ NAME1:" +sName1)
          scribe.info("@@@ NAME2:" +sName2)

          FlicFlacGame(model.bridge.subSystem(IndigoGameId("indigo-container"))).launch(
            "indigo-container",
            Map[String, String](
              "width" -> dom.window.innerWidth.toString,
              "height" -> dom.window.innerHeight.toString,
              "name1" -> sName1,
              "name2" -> sName2
            )
          )
          Msg.DoNothing
      }
      (model.copy(renderUi = false), Cmd.Run(task))

    case Msg.RetryIndigo =>
      scribe.info("Retrying Indigo")
      (model, Cmd.emitAfterDelay(Msg.StartIndigo, 0.5.seconds))

    case Msg.DoNothing =>
      (model, Cmd.None)

    case Msg.NavigateToUrl(url) => {
        dom.console.error("external navigation not supported")
        (model, Cmd.None)
    }
    case Msg.RunGame =>
      (
        model.copy(renderUi = false),
//        Nav.loadUrl[IO](s"${dom.window.location}?runGame=true"),
        Nav.loadUrl[IO](s"${dom.window.location}&runGame=true"), // players names have already inserted ?
      )
    // format: on
  }

  end update

  def view(model: TyrianModel): Html[Msg] =
    if model.renderUi then
      div(id := "myapp")(
        button(id := "startGame", onClick(Msg.RunGame))("Start Game")
      )
    else div(id := "indigo-container")()
  def subscriptions(model: TyrianModel): Sub[IO, Msg] =
    Sub.None
end TyrianApp
