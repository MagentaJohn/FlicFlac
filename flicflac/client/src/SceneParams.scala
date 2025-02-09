package game

import shared.*

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.platform.networking.Network
import game.SceneParams.lastTxGameModel

/*
  INTIATOR criteria .... ourName < oppoName
  RESPONDER criteria ... ourName > oppoName
  NB index1.html prohibits ourName == oppoName

  SceneParams and SubSystemPeerJs implement a simple state machine as follows:

  START_CON1 ...... both INITIATOR and RESPONDER generate a peerJS
  START_CON2 ...... both INITIATOR and RESPONDER peerJS established, INITIATOR (repeatedly) issues request to connect
  START_CON3 ...... both INITIATOR and RESPONDER independently confirm connection
  ................. INITIATOR sends proposed parameters and transitions to CON4 (triggered by peerJS)
  MsgA: Initiator===>>>Responder "proposed params"
  ................. RESPONDER averages game parameters and responds with modified params and transitions to CON4
  MsgB: Initiator<<<===Responder "modified params"
  START_CON4 ...... INITIATOR transmits modifed params with state CYLINDER_TURN, transitions to CYLINDER_TURN
  MsgC: Initiator===>>>Responder "modifed params"
  ................. RESPONDER transitions to CYLINDER_TURN
  CYLINDER_TURN ... both INITIATOR and RESPONDER display hexboard and the game starts
 */

object SceneParams extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:
  type SceneModel = FlicFlacGameModel
  type SceneViewModel = GameSceneViewModel

  def name: SceneName = SceneName("SceneParams")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set.empty

  var lastTxGameModel: Option[FlicFlacGameModel] = None
  val RETRANSMIT_TIMEOUT = 50 // 5 seconds
  val INITIAL_TIMEOUT = 70 // 7 seconds
  var timerP1 =
    TickTimer.start(INITIAL_TIMEOUT) // protocol timer to retranmsit last frame if nn response with RETRANSMIT_TIMEOUT

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
        case DetectParams =>
          val s1: String = org.scalajs.dom.window.localStorage.getItem("FlicFlac-Game1")
          val s2: String = org.scalajs.dom.window.localStorage.getItem("FlicFlac-Game2")

          if (s1 ne null) || (s2 ne null) then
            scribe.trace("@@@ SceneParams.DetectParams RESTORE GAME")
            Outcome(model).addGlobalEvents(SceneEvent.Next) // sidestep the bootloader
          else
            scribe.trace("@@@ SceneParams.DetectParams NEW GAME")
            Outcome(model)
          end if

        case e: FlicFlacGameUpdate.Info =>
          scribe.trace("@@@ SceneParams FlicFlacGameUpdate.Info")

          val n1 = context.frameContext.startUpData.flicFlacBootData.n1
          val n2 = context.frameContext.startUpData.flicFlacBootData.n2
          val newModel1 = e.ffgm.copy(ourName = n1, oppoName = n2)
          val bInitiator = (n1.compare(n2) < 0)
          val currentState = if bInitiator then model.initiatorGameState else model.responderGameState

          scribe.trace(
            "@@@ SceneParams Old: INITIATOR:" + model.initiatorGameState
              .toString() + " RESPONDER:" + model.responderGameState.toString()
          )

          if bInitiator == true then
            val outcome1 = initiatorStateMachine(context, currentState, newModel1)
            lastTxGameModel match
              case Some(t) =>
                scribe.trace(
                  "@@@ SceneParams New: INITIATOR:" + t.initiatorGameState
                    .toString() + " RESPONDER:" + t.responderGameState.toString()
                )
              case _ =>
                scribe.trace("@@@ SceneParams no change detected")
            end match
            outcome1
          else
            val outcome2 = responderStateMachine(context, currentState, newModel1)
            lastTxGameModel match
              case Some(t) =>
                scribe.trace(
                  "@@@ SceneParams New: INITIATOR:" + t.initiatorGameState
                    .toString() + " RESPONDER:" + t.responderGameState.toString()
                )
              case _ =>
                scribe.trace("@@@ SceneParams no change detected")
            end match
            outcome2
          end if

        case e: PointerEvent.PointerDown =>
          // this clears any panel showing
          Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, ("", "")))

        // Keyboard Interface for testing purposes only ...
        case k: KeyboardEvent.KeyDown =>
          if k.keyCode == Key.F3 then Outcome(model).addGlobalEvents(SceneEvent.Next)
          else if k.keyCode == Key.F4 then
            Outcome(model)
              .addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, ("Error", "Test Error from PARAMS FKEY_F4")))
          else Outcome(model)
          end if

        case FrameTick =>
          val n1 = context.frameContext.startUpData.flicFlacBootData.n1
          val n2 = context.frameContext.startUpData.flicFlacBootData.n2
          val initiator = (n1.compare(n2) < 0)

          if (initiator == true) && (model.initiatorGameState == GameState.START_CON3) && (lastTxGameModel == None) then
            timerP1 = TickTimer.start(RETRANSMIT_TIMEOUT)
            lastTxGameModel = Some(model)
            Outcome(model).addGlobalEvents(WebRtcEvent.SendData(model))
          else if TickTimer.expired(timerP1) then
            timerP1 = TickTimer.start(RETRANSMIT_TIMEOUT) // start the tx retransmission timeout (again)
            lastTxGameModel match
              case Some(lastTxModel) =>
                scribe.trace("@@@ TimerP1 expired so retransmission")
                Outcome(model).addGlobalEvents(WebRtcEvent.SendData(lastTxModel))
              case None =>
                scribe.trace("@@@ TimerP1 expired but nothing to transmit")
                Outcome(model)
            end match
          else
            // waiting for inbound response or retransmission timer
            Outcome(model)
          end if

        case _ =>
          Outcome(model)

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: SceneModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =
    val width = GameAssets.GetGameSceneDimensions(8).width // default size 8
    val height = GameAssets.GetGameSceneDimensions(8).height // default size 8

    val n1 = context.frameContext.startUpData.flicFlacBootData.n1
    val n2 = context.frameContext.startUpData.flicFlacBootData.n2
    val bInitiator = (n1.compare(n2) < 0)

    val gs = if bInitiator == true then model.initiatorGameState else model.responderGameState

    val textT1 = TextBox("*** FlicFlac BootLoader ***", width, 80)
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(60))
      .alignCenter
      .moveTo(0, 0)

    val topX = 10
    val topY = 90
    val fontWidth = 13
    val nameWidth = (Math.max(n1.length, n2.length) + 2) * fontWidth // we add a plus 2 for the : and safety
    val nameHeight = 30
    val nameGap = 10
    val boxGap = 10

    val textN1 = TextBox(n1 + ":", nameWidth, nameHeight)
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(20))
      .alignRight
      .moveTo(topX, topY)
    val textN2 = TextBox(n2 + ":", nameWidth, nameHeight)
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(20))
      .alignRight
      .moveTo(topX, topY + nameHeight + nameGap)
    val boxB1 = Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Yellow)).moveTo(topX + nameWidth + boxGap, topY)
    val boxB2 = Shape
      .Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Yellow))
      .moveTo(topX + nameWidth + boxGap, topY + nameHeight + nameGap)
    val boxB3 = Shape
      .Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Yellow))
      .moveTo(topX + nameWidth + boxGap + 24 + nameGap, topY + nameHeight + nameGap)

    val initiatorProgress = Math.min(model.initiatorGameState.ordinal + 1, 8)
    val responderProgress = Math.min(model.responderGameState.ordinal + 1, 8)

    var initiatorBoxes = Layer.empty
    for i <- 1 to initiatorProgress do
      val x = topX + nameWidth + boxGap + boxGap + ((24 + boxGap) * (i - 1))
      val y = topY
      initiatorBoxes = initiatorBoxes |+| Layer.Content(boxB1.moveTo(x, y))
    end for

    var responderBoxes = Layer.empty
    for i <- 1 to responderProgress do
      val x = topX + nameWidth + boxGap + boxGap + ((24 + boxGap) * (i - 1))
      val y = topY + nameHeight + nameGap
      responderBoxes = responderBoxes |+| Layer.Content(boxB1.moveTo(x, y))
    end for

// format: off
    Outcome(
      SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black))))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(Batch(textT1, textN1, textN2)))
        |+| SceneUpdateFragment(LayerKeys.ForegroundPieces -> initiatorBoxes)
        |+| SceneUpdateFragment(LayerKeys.ForegroundPieces -> responderBoxes)
    )
// format: on    
  end present

  def initiatorStateMachine(
      context: SceneContext[FlicFlacStartupData],
      currentState: GameState,
      responderModel: FlicFlacGameModel
  ): Outcome[FlicFlacGameModel] =
    responderModel.responderGameState match
      case GameState.START_CON1 =>
        Outcome(responderModel)
      case GameState.START_CON2 =>
        Outcome(responderModel)
      case GameState.START_CON3 =>
        Outcome(responderModel)
      case GameState.START_CON4 =>
        scribe.trace("@@@ SceneParams INITIATOR transitions to START_CON4")
        val newModel = responderModel.copy(initiatorGameState = GameState.START_CON4)
        lastTxGameModel = Some(newModel)
        responderModel.modify(newModel, None, None)

      case GameState.START_CON5 =>
        if currentState == GameState.START_CON4 then
          scribe.trace("@@@ SceneParams INITIATOR transitions to START_CON5")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON5)
          lastTxGameModel = Some(newModel)
          responderModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          lastTxGameModel = Some(newModel)
          responderModel.modify(newModel, None, None)
        end if

      case GameState.START_CON6 =>
        if currentState == GameState.START_CON5 then
          scribe.trace("@@@ SceneParams INITIATOR transitions to START_CON6")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON6)
          lastTxGameModel = Some(newModel)
          responderModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          lastTxGameModel = Some(newModel)
          responderModel.modify(newModel, None, None)
        end if

      case GameState.START_CON7 =>
        if currentState == GameState.START_CON6 then
          scribe.trace("@@@ SceneParams INITIATOR transitions to START_CON7")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON7)
          lastTxGameModel = Some(newModel)
          responderModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          lastTxGameModel = Some(newModel)
          responderModel.modify(newModel, None, None)
        end if

      case GameState.START_CON8 =>
        if currentState == GameState.START_CON7 then
          scribe.trace("@@@ SceneParams INITIATOR transitions to START_CON8")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON8)
          lastTxGameModel = Some(newModel)
          responderModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          responderModel.modify(newModel, None, None)
        end if

      case GameState.CYLINDER_TURN =>
        scribe.trace("@@@ SceneParams INITIATOR invokes SceneGame")
        val t1 = responderModel.turnTimer
        val t2 = sharedTurnTimer.restartForTurn(t1)
        val newModel = responderModel.copy(
          gameState = GameState.CYLINDER_TURN,
          initiatorGameState = GameState.CYLINDER_TURN,
          turnTimer = t2
        )

        lastTxGameModel = Some(newModel)
        timerP1 = TickTimer.stop()
        responderModel
          .modify(newModel, None, None)
          .addGlobalEvents(SceneEvent.Next)
          .addGlobalEvents(StartLiveGame)

      case _ =>
        val sError = "SceneParams INITIATOR error "
          + responderModel.initiatorGameState.toString() + " and " + responderModel.responderGameState.toString()
        scribe.trace("@@@ " + sError)
        val newModel = responderModel
        lastTxGameModel = Some(newModel)
        responderModel
          .modify(newModel, None, None)
          .addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, ("Error", sError)))
  end initiatorStateMachine

  def responderStateMachine(
      context: SceneContext[FlicFlacStartupData],
      currentState: GameState,
      initiatorModel: FlicFlacGameModel
  ): Outcome[FlicFlacGameModel] =
    initiatorModel.initiatorGameState match
      case GameState.START_CON1 =>
        Outcome(initiatorModel)
      case GameState.START_CON2 =>
        Outcome(initiatorModel)
      case GameState.START_CON3 =>
        scribe.trace("@@@ SceneParams RESPONDER transitions to START_CON4")

        // this is the RESPONDER - the ONE AND ONLY time the params marked with ### may be changed
        val pp = new PlayerParams("", "", 0, 0, 0, 0, 0, 0) // dummy PlayerParams to get access to getParams
        val originalPlayerParams = pp.getParams(context.frameContext.startUpData)
        val newBoardSize = initiatorModel.boardSize.min(originalPlayerParams.playPams4_BoardSize)
        val newScoreToWin = (initiatorModel.winningScore + originalPlayerParams.playPams5_ScoreToWin) / 2
        val newTurnTimer = (initiatorModel.turnTimer.iTotalTurnTime + originalPlayerParams.playPams6_TurnTime) / 2
        val newCaptorsTimer = (initiatorModel.turnTimer.iCaptorsTurnTime + originalPlayerParams.playPams7_CaptorsTime) / 2
        val newRandEventFreq = (initiatorModel.randEventFreq + originalPlayerParams.playPams8_RandEventProb) / 2

        val newModel = initiatorModel.copy(
          responderGameState = GameState.START_CON4,
          boardSize = newBoardSize, // ....................................................###
          winningScore = newScoreToWin, // ................................................###
          turnTimer = TurnTimer(newTurnTimer, newCaptorsTimer, false, false, 0, 0), // ....###
          randEventFreq = newRandEventFreq // .............................................###
        )
        lastTxGameModel = Some(newModel)
        initiatorModel.modify(newModel, None, None)

      case GameState.START_CON4 =>
        if currentState == GameState.START_CON4 then
          scribe.trace("@@@ SceneParams RESPONDER transitions to START_CON5")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON5)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        end if

      case GameState.START_CON5 =>
        if currentState == GameState.START_CON5 then
          scribe.trace("@@@ SceneParams RESPONDER transitions to START_CON6")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON6)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        end if

      case GameState.START_CON6 =>
        if currentState == GameState.START_CON6 then
          scribe.trace("@@@ SceneParams RESPONDER transitions to START_CON7")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON7)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        end if

      case GameState.START_CON7 =>
        if currentState == GameState.START_CON7 then
          scribe.trace("@@@ SceneParams RESPONDER transitions to START_CON8")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON8)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        else
          scribe.trace("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        end if

      case GameState.START_CON8 =>
        if currentState == GameState.START_CON8 then
          scribe.trace("@@@ SceneParams RESPONDER invokes SceneGame")

          val t1 = initiatorModel.turnTimer
          val t2 = sharedTurnTimer.restartForTurn(t1)
          val newModel = initiatorModel.copy(
            responderGameState = GameState.CYLINDER_TURN,
            gameState = GameState.CYLINDER_TURN,
            turnTimer = t2
          )

          lastTxGameModel = Some(newModel)
          timerP1 = TickTimer.stop()
          initiatorModel
            .modify(newModel, None, None)
            .addGlobalEvents(SceneEvent.Next)
            .addGlobalEvents(StartLiveGame)
        else
          scribe.trace("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          initiatorModel.modify(newModel, None, None)
        end if

      case GameState.CYLINDER_TURN =>
        scribe.trace("@@@ SceneParams RESPONDER detects inbound CYLINDER_TURN ???")
        val newModel = initiatorModel.copy(responderGameState = currentState)
        lastTxGameModel = Some(newModel)
        Outcome(newModel)

      case _ =>
        val sError = "SceneParams RESPONDER error "
          + initiatorModel.initiatorGameState.toString() + " and "
          + initiatorModel.responderGameState.toString()
        scribe.trace("@@@ " + sError)
        val newModel = initiatorModel
        lastTxGameModel = Some(newModel)
        initiatorModel
          .modify(newModel, None, None)
          .addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, ("Error", sError)))

  end responderStateMachine

end SceneParams

case object DetectParams extends GlobalEvent
