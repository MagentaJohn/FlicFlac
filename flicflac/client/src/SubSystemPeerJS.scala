package game

import shared.*

import scala.scalajs.js
import indigo.*
import io.github.quafadas.peerscalajs.Peer
import io.github.quafadas.peerscalajs.DataConnection
import scala.collection.mutable.Queue
import io.circe.syntax.*
import io.circe.parser.decode

/*
INITIATOR                                                            | RESPONDER
@@@-10 SubSystemPeerJS WebRtcEvent.MakePeerEntity using FIRSTNAME    | @@@-10 SubSystemPeerJS WebRtcEvent.MakePeerEntity using SECONDNAME
@@@-20 SubSystemPeerJS WebRtcEvent.CreatedPeerEntity                 | @@@-20 SubSystemPeerJS WebRtcEvent.CreatedPeerEntity
@@@-11 localPeer.on open                                             | @@@-11 localPeer.on open
                                                                     |
@@@-30 SubSystemPeerJS WebRtcEvent.Connect: FIRSTNAME -> SECONDNAME  |
@@@-50 SubSystemPeerJS WebRtcEvent.PeerCreatedConnection             |
                                                                     | @@@-12 localPeer.connection to FIRSTNAME
                                                                     | @@@-40 SubSystemPeerJS WebRtcEvent.IncomingPeerConnection
@@@-31 Connect.on open                                               |
                                                                     | @@@-50 SubSystemPeerJS WebRtcEvent.PeerCreatedConnection
@@@-60 SubSystemPeerJS WebRtcEvent.ConnectionOpen                    |
@@@-70 SubSystemPeerJS WebRtcEvent.SendData                          |
@@@-71 SendData ALPHA                                                |
                                                                     | @@@-41 IncomingPeerConnection.on data ALPHA
                                                                     | @@@-80 SubSystemPeerJS WebRtcEvent.ReceiveData
                                                                     | @@@-81 ReceiveData ALPHA
                                                                     | @@@-70 SubSystemPeerJS WebRtcEvent.SendData
                                                                     | @@@-71 SendData bravo
@@@-61 ConnectionOpen.on data bravo                                  |
@@@-80 SubSystemPeerJS WebRtcEvent.ReceiveData                       |
@@@-81 ReceiveData bravo                                             |
@@@-90 SubSystemPeerJS WebRtcEvent.Close                             |
@@@-32 Connect.on close                                              |
@@@-62 ConnectionOpen.on close                                       |
                                                                     | @@@-42 IncomingPeerConnection.on closed
 */

sealed trait WebRtcEvent extends GlobalEvent

object WebRtcEvent:
  case object MakePeerEntity extends WebRtcEvent // 10
  case class CreatedPeerEntity(peer: Peer) extends WebRtcEvent // 20
  case class Connect(s: String) extends WebRtcEvent with NetworkReceiveEvent // 30
  case class IncomingPeerConnection(conn: DataConnection) extends WebRtcEvent with NetworkReceiveEvent // 40
  case class PeerCreatedConnection(conn: DataConnection) extends WebRtcEvent // 50
  case object ConnectionOpen extends WebRtcEvent with NetworkReceiveEvent // 60
  case class SendData(ffgm: FlicFlacGameModel) extends WebRtcEvent with NetworkReceiveEvent // 70
  case class ReceivedData(data: js.Object) extends WebRtcEvent with NetworkReceiveEvent // 80
  case object Close extends WebRtcEvent with NetworkReceiveEvent // 90

end WebRtcEvent

val INITIATOR = true
val RESPONDER = false

var timerT1 = TickTimer.stop() // ... timerT1 used by INITIATOR to repeat connect request if previous fail

final case class SSPeerJS(initialMessage: String) extends SubSystem[FlicFlacGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None
  var remotePeerName: String = ""
  var latestUpdate: Option[FlicFlacGameUpdate.Info] = None
  val eventQueue: Queue[WebRtcEvent] = Queue.empty[WebRtcEvent]
  var peerJsPanel: (PanelType, (String, String)) = (PanelType.P_INVISIBLE, ("", ""))

  // peerJsPanel controls the appearance of the Error/Pause/Results panel. It can be triggered in two different ways 1)&2)
  // and it s cleared in one way 3)
  // 1) direct manipulation of the var peerJsPanel from the callbacks in this file
  // 2) indirectly on receipt of the message Frozen.PanelContent(T,msg) where T is one of P_ERROR, P_RESULTS
  // 3) generation of message Frozen.PanelContent(P_INVISIBLE,"") in one of the scenes ... which occurs when you click (maybe outside the board)

  type EventType = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData = FlicFlacGameModel
  val id: SubSystemId = SubSystemId("SubSystemPeerJS")

  val eventFilter: GlobalEvent => Option[GlobalEvent] = {
    case e: GlobalEvent => Some(e)
    case null           => None
  }
  def reference(flicFlacGameModel: FlicFlacGameModel): FlicFlacGameModel = flicFlacGameModel

  def initialModel: Outcome[Unit] =
    scribe.trace("@@@ SubSystemPeerJS initialModel")
    Outcome(())
  end initialModel

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] =
    e =>
      try
        e match
          case WebRtcEvent.MakePeerEntity =>
            scribe.trace("@@@-10 SubSystemPeerJS WebRtcEvent.MakePeerEntity using " + context.reference.ourName)
            val localPeer = Peer(id = context.reference.ourName)

            localPeer.on(
              "open",
              (_: Any) =>
                scribe.trace("@@@-11 localPeer.on open")
                if context.reference.ourName.compare(context.reference.oppoName) < 0 then
                  // we are the connection initiator so attempt to make the request
                  eventQueue.enqueue(WebRtcEvent.Connect(context.reference.oppoName))
                  // peer is established so bump Game State
                  setGameState(GameState.START_CON2, context.reference, INITIATOR)
                else
                  // we are the connection responder
                  setGameState(GameState.START_CON2, context.reference, RESPONDER)
                end if
            )

            localPeer.on(
              "connection",
              (c: DataConnection) =>
                remotePeerName = c.label // RESPONDER setting up remotePeerName
                scribe.trace("@@@-12 localPeer.connection to " + remotePeerName)

                // optionally, we can reject connection if c.label != context.reference.oppoName ...
                // ... this is the scenario where an unknown peer has connected to us

                eventQueue.enqueue(WebRtcEvent.IncomingPeerConnection(c))
            )
            localPeer.on(
              "disconnected",
              (_: Any) => scribe.trace("@@@-13 localPeer.on disconnected")
            )

            localPeer.on(
              "close",
              (_: Any) => scribe.trace("@@@-14 localPeer.on close")
            )

            localPeer.on(
              "error",
              (e: js.Object) =>
                scribe.error("@@@-19 LocalPeer.on error " + js.JSON.stringify(e))
                val eMsg = js.JSON.stringify(e)
                  + " HINT: Wait for \"" + context.reference.oppoName + "\" to start" // signalling ERROR
                peerJsPanel = (PanelType.P_HINT, ("HINT", eMsg))
                timerT1 = TickTimer.start(TT_TEN_SECONDS)
            )
            eventQueue.enqueue(WebRtcEvent.CreatedPeerEntity(localPeer))
            Outcome(())

          case WebRtcEvent.CreatedPeerEntity(p) =>
            scribe.trace("@@@-20 SubSystemPeerJS WebRtcEvent.CreatedPeerEntity")
            peer = Some(p)
            Outcome(())

          case WebRtcEvent.Connect(s) =>
            val ourname = peer.get.id
            remotePeerName = s // INITIATOR setting up remotePeerName
            scribe.trace("@@@-30 SubSystemPeerJS WebRtcEvent.Connect: " + ourname + " ->  " + remotePeerName)

            val connection = peer match
              case Some(p) =>
                val options = js.Dynamic.literal()
                options.label = ourname // the responder (section 40) will use this label to set his remotePeerName
                val conn = p.connect(remotePeerName, options)
                conn.on(
                  "open",
                  (_: Any) =>
                    scribe.trace("@@@-31 Connect.on open")
                    eventQueue.enqueue(WebRtcEvent.ConnectionOpen)
                )
                conn.on(
                  "close",
                  (c: DataConnection) => scribe.trace("@@@-32 Connect.on close")
                )
                conn.on(
                  "error",
                  (e: js.Object) =>
                    scribe.error("@@@-33 Connect.on error" + js.JSON.stringify(e))
                    val eMsg = js.JSON.stringify(e)
                    peerJsPanel = (PanelType.P_ERROR, ("Error", eMsg)) // signalling ERROR
                )

                conn

              case None =>
                scribe.fatal("@@@-39 Connect None ... Attempting to connect without a peer")
                peerJsPanel = (PanelType.P_ERROR, ("Error", "Connecting without a local peer in place")) // signalling ERROR
                val nullDataConnection = new DataConnection()
                nullDataConnection // likely to cause exception

            eventQueue.enqueue(WebRtcEvent.PeerCreatedConnection(connection))
            Outcome(())

          case WebRtcEvent.IncomingPeerConnection(c) =>
            scribe.trace("@@@-40 SubSystemPeerJS WebRtcEvent.IncomingPeerConnection")
            c.on(
              "data",
              (data: js.Object) =>
                scribe.trace("@@@-41 ConnectionOpen.on data")
                val str = js.JSON.stringify(data)
                val ffgm = decodeRxJsonObject(data, 48) // 48 is the error number
                val ffgm1 = convertRxGameModel(ffgm)
                latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm1))
            )
            c.on(
              "close",
              (c: DataConnection) => scribe.trace("@@@-42 IncomingPeerConnection.on closed ")
            )
            c.on(
              "error",
              (e: js.Object) =>
                scribe.error("@@@-49 IncomingPeerConnection.on error " + js.JSON.stringify(e))
                val eMsg = js.JSON.stringify(e)
                peerJsPanel = (PanelType.P_ERROR, ("Error", eMsg)) // signalling ERROR
            )
            eventQueue.enqueue(WebRtcEvent.PeerCreatedConnection(c))
            Outcome(())

          case WebRtcEvent.PeerCreatedConnection(connLocal: DataConnection) =>
            // successful connection as as RESPONDER so bump Game State
            scribe.trace("@@@-50 SubSystemPeerJS WebRtcEvent.PeerCreatedConnection")
            conn = Some(connLocal)
            setGameState(GameState.START_CON3, context.reference, RESPONDER)
            Outcome(())

          case WebRtcEvent.ConnectionOpen =>
            // successful connection as INITIATOR so stop timerT1 and bump Game State
            scribe.trace("@@@-60 SubSystemPeerJS WebRtcEvent.ConnectionOpen")
            timerT1 = TickTimer.stop()
            setGameState(GameState.START_CON3, context.reference, INITIATOR)
            conn.foreach { c =>
              c.on(
                "data",
                (data: js.Object) =>
                  scribe.trace("@@@-61 ConnectionOpen.on data ")
                  val ffgm = decodeRxJsonObject(data, 68) // 68 is the error number
                  val ffgm1 = convertRxGameModel(ffgm)
                  latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm1))
              )

              c.on(
                "close",
                (c: DataConnection) => scribe.trace("@@@-62 ConnectionOpen.on close ")
              )
              c.on(
                "error",
                (e: js.Object) =>
                  scribe.error("@@@-69 ConnectionOpen.on error " + js.JSON.stringify(e))
                  val eMsg = js.JSON.stringify(e)
                  peerJsPanel = (PanelType.P_ERROR, ("Error", eMsg)) // signalling ERROR
              )
            }
            Outcome(())

          case WebRtcEvent.SendData(ffgm) =>
            scribe.trace("@@@-70 SubSystemPeerJS WebRtcEvent.SendData")

            if TickTimer.isInactive(timerT1) then
              conn.foreach { c =>
                scribe.trace("@@@-71 SendData " + peer.get.id + "->" + remotePeerName)
                val toSendNoSpaces = ffgm.asJson.noSpaces
                val toSendJson = js.JSON.parse(toSendNoSpaces)
                c.send(toSendJson)
              }
            else
              // send nothing as connection not ready
              scribe.trace("@@@-72 SendData blocked as connection not open yet")
            end if
            Outcome(())

          case WebRtcEvent.ReceivedData(data: js.Object) =>
            scribe.trace("@@@-80 SubSystemPeerJS WebRtcEvent.ReceiveData")
// ********************************************************************************************************
// It appears that sections 41 and 61 do the same job as this one, which ends up as a duplication of effort
//
//            conn.foreach { c =>
//              scribe.trace("@@@-81 ReceiveData " + remotePeerName + "->" + peer.get.id)
//              val ffgm = decodeRxJsonObject(data, 88) // 88 is the error number
//              val ffgm1 = convertRxGameModel(ffgm)
//              latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm1))
//            }
// *********************************************************************************************************
            Outcome(())

          case WebRtcEvent.Close =>
            scribe.trace("@@@-90 SubSystemPeerJS WebRtcEvent.Close")
            conn.foreach(_.close())
            Outcome(())

          case Freeze.PanelContent(typeOfPanel, titleAndMessageToDisplay) =>
            peerJsPanel = (typeOfPanel, titleAndMessageToDisplay)
            scribe.trace("@@@ SubSystemPeerJS Freeze.PanelContent: " + typeOfPanel.toString())
            Outcome(())

          case _ =>
            if TickTimer.expired(timerT1) then
              timerT1 = TickTimer.stop()
              if context.reference.ourName.compare(context.reference.oppoName) < 0 then
                // we are the connection initiator and timerT1 has expired so attempt to make the connection request again
                eventQueue.enqueue(WebRtcEvent.Connect(context.reference.oppoName))
              end if
            end if

            val outcome0 = Outcome(()) // ... Assume no additional events to add

            val bEvents = !eventQueue.isEmpty
            val outcome1 =
              if eventQueue.isEmpty then
                // no events queueud
                outcome0
              else // ....................... Adding in queued events generated from non-call back PeerJs code
                val events = eventQueue.dequeueAll(_ => true)
                outcome0.addGlobalEvents(events*)
              end if
            end outcome1

            val outcome2 =
              latestUpdate match
                case Some(update) =>
                  latestUpdate = None
                  outcome1.addGlobalEvents(update) // ... Adding in event generated from a callback
                case None =>
                  outcome1

            outcome2
      catch
        case e: PeerJsException =>
          val errorMsg = e.getMessage()
          scribe.error("@@@ SubSystemPeerJS update PeerJsCustomException handler " + errorMsg)
          peerJsPanel = (PanelType.P_ERROR, ("Error", errorMsg))
          Outcome(())
        case e: Throwable =>
          val errorMsg = e.getMessage()
          scribe.error("@@@ SubSystemPeerJS update ThrowableException handler " + errorMsg)
          peerJsPanel = (PanelType.P_ERROR, ("Error", errorMsg))
          Outcome(())
  end update

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =

    val dSF = hexBoard4.scalingFactor
    val title = peerJsPanel._2._1
    val msg = peerJsPanel._2._2

    peerJsPanel match
      case (PanelType.P_ERROR, _) =>
        displayPanel(title, msg, dSF, PanelType.P_ERROR)
      case (PanelType.P_HINT, _) =>
        displayPanel(title, msg, dSF, PanelType.P_HINT)
      case (PanelType.P_RESULTS, _) =>
        displayPanel(title, msg, dSF, PanelType.P_RESULTS)
      case _ => // including P_INVISIBLE
        Outcome(SceneUpdateFragment.empty)
    end match

  end present

  def displayPanel(title: String, msg: String, dSF: Double, pType: PanelType): Outcome[SceneUpdateFragment] =
    val boxX = (260 * dSF).toInt
    val boxY = (176 * dSF).toInt

    val iSF = (10 * dSF).toInt // dSF is one of 1.0 0.9 0.8 0.75 0.67 0.5 0.33

    val (boxW, boxH, titleFontSize, msgFontSize, box4Yoffset, box5Yoffset) = iSF match
      case 10 => (((16 + (12 * (msg.length()))).max(1000)), 180, 60, 20, 100, 140)
      case 9  => (((14 + (11 * (msg.length()))).max(900)), 162, 54, 18, 90, 126)
      case 8  => (((13 + (10 * (msg.length()))).max(800)), 135, 48, 16, 80, 110)
      case 7  => (((12 + (9 * (msg.length()))).max(750)), 135, 45, 15, 75, 105)
      case 6  => (((10 + (7 * (msg.length()))).max(600)), 108, 36, 12, 65, 88)
      case 5  => (((8 + (6 * (msg.length()))).max(500)), 90, 30, 10, 50, 70)
      case _  => (((5 + (5 * (msg.length()))).max(400)), 60, 20, 8, 34, 45)

    val borderColour =
      if (pType == PanelType.P_RESULTS) || (pType == PanelType.P_HINT) then
        // sets border, title and text to black
        RGBA.Black
      else
        // sets border and title to red, but text remains black
        RGBA.Red

    val textError3 =
      if (pType == PanelType.P_RESULTS) || (pType == PanelType.P_HINT) then
        TextBox(title, boxW - 16, boxH - 16).alignCenter.bold
          .withColor(borderColour)
          .withFontSize(Pixels(titleFontSize))
          .moveTo(boxX + 8, boxY + 8)
      else
        TextBox("*** FlicFlac ERROR ***", boxW - 16, boxH - 16).alignCenter.bold
          .withColor(borderColour)
          .withFontSize(Pixels(titleFontSize))
          .moveTo(boxX + 8, boxY + 8)
      end if
    end textError3

    val textError4 = TextBox(msg, boxW - 16, boxH - 16)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(msgFontSize))
      .moveTo(boxX + 8, boxY + box4Yoffset)

    val textError5 =
      TextBox("... click on any part of the background to dismiss this notification.", boxW - 16, boxH - 16)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(msgFontSize))
        .moveTo(boxX + 8, boxY + box5Yoffset)

// format: off

    Outcome(SceneUpdateFragment(LayerKeys.Overlay -> Layer.Content(Shape.Box(Rectangle(boxX, boxY, boxW, boxH), Fill.Color(borderColour))))
        |+| SceneUpdateFragment(LayerKeys.Overlay -> Layer.Content(Shape.Box(Rectangle(boxX + 4, boxY + 4, boxW - 8, boxH - 8), Fill.Color(RGBA.Cyan))))
        |+| SceneUpdateFragment(LayerKeys.Overlay -> Layer.Content(Batch(textError3, textError4, textError5)))
    )

// format: on
  end displayPanel

  def decodeRxJsonObject(data: js.Object, errNo: Int): FlicFlacGameModel =
    val str = js.JSON.stringify(data)
    val ffgm = decode[FlicFlacGameModel](str)
      .fold(
        e =>
          scribe.error("@@@ " + errNo + " Error decoding data")
          throw new Exception("Error " + errNo + "decoding data")
        ,
        identity
      )
    ffgm
  end decodeRxJsonObject

  def setGameState(gs: GameState, ffgm: FlicFlacGameModel, initiator: Boolean): Unit =
    //
    latestUpdate match
      case Some(update) =>
        // peerJs has already modified the model, so we must update the modification
        val existingModel = update.ffgm
        if initiator then
          // initiator
          latestUpdate = Some(FlicFlacGameUpdate.Info(existingModel.copy(initiatorGameState = gs)))
        else
          // responder
          latestUpdate = Some(FlicFlacGameUpdate.Info(existingModel.copy(responderGameState = gs)))
        end if

      case None =>
        // peerJs has not modified the model so we need a new model to report
        if initiator then
          // initiator
          latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm.copy(initiatorGameState = gs)))
        else
          // responder
          latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm.copy(responderGameState = gs)))
        end if
  end setGameState

  def convertRxGameModel(rxModel: FlicFlacGameModel): FlicFlacGameModel =
    val name1 = rxModel.ourName // .............................................. used to swap into oppoName
    val name2 = rxModel.oppoName // ............................................. used to swap into ourName
    val pieceType = (rxModel.ourPieceType & 1) ^ 1 // ........................... inverting piece type
    rxModel.copy(ourName = name2, oppoName = name1, ourPieceType = pieceType)
  end convertRxGameModel

end SSPeerJS

class PeerJsException(cause: String) extends Exception(cause)
