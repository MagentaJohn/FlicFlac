package game

import indigo.*

final case class ScorePanel():
  val p0 = Point(0, 130) // ....... coords of score panel on the game scene when 100% zoom
  val p1 = Point(14, 190) // ...... coords of cylinder name
  val p2 = Point(14, 370) // ...... coords of block name
  val p3a = Point(150, 250) // .... coords of cylinder score if single digit
  val p3b = Point(120, 250) // .... coords of cylinder score if 10 or more
  val p4a = Point(150, 430) // .... coords of block score if single digit
  val p4b = Point(120, 430) // .... coords of block score if 10 or more
  val p5 = Point(20, 230) // ...... coords of cylinder icon
  val p6 = Point(20, 410) // ...... coords of block icon

  def paint(model: FlicFlacGameModel, bBlinkOn: Boolean, dSF: Double): Layer =

    val p0Scaled = p0 * dSF
    val p1Scaled = p1 * dSF
    val p2Scaled = p2 * dSF
    val p3Scaled = coordsFromScore(model.gameScore._1, p3a, p3b) * dSF
    val p4Scaled = coordsFromScore(model.gameScore._2, p4a, p4b) * dSF
    val p5Scaled = p5 * dSF
    val p6Scaled = p6 * dSF

    val cylinderName =
      if model.ourPieceType == CYLINDER then model.ourName
      else model.oppoName
      end if
    end cylinderName

    val blockName =
      if model.ourPieceType == CYLINDER then model.oppoName
      else model.ourName
      end if
    end blockName

    val hybridScorePanel =
      if (bBlinkOn == true) && (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == CYLINDER) then
        // select magenta cylinder icon
        GameAssets.gScorePanelHighlightCylinder(dSF).moveTo(p0Scaled)
      else if (bBlinkOn == true) && (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == BLOCK) then
        // select magenta block icon
        GameAssets.gScorePanelHighlightBlock(dSF).moveTo(p0Scaled)
      else
        // normal panel
        GameAssets.gScorePanelHighlightOff(dSF).moveTo(p0Scaled)
      end if
    end hybridScorePanel

    val cylinderIcon =
      if model.gameState == GameState.CYLINDER_TURN then
        if model.ourPieceType == CYLINDER then
          findPieceSelected(model) match
            case Some(piece) =>
              // show detailed graphics icon for selected piece
              GameAssets.gScorePanelPieceAndFlip(piece.pieceShape, piece.pieceIdentity, piece.bFlipped, dSF).moveTo(p5Scaled)
            case None =>
              // invoke magenta icon
              GameAssets.gScorePanelMagentaCylinder(dSF).moveTo(p5Scaled)
        else
          // we are not the cylinder player so invoke grey icon
          GameAssets.gScorePanelGreyCylinder(dSF).moveTo(p5Scaled)
        end if
      else
        // it is not the cylinders turn so invoke grey icon
        GameAssets.gScorePanelGreyCylinder(dSF).moveTo(p5Scaled)
      end if
    end cylinderIcon

    val blockIcon =
      if model.gameState == GameState.BLOCK_TURN then
        if model.ourPieceType == BLOCK then
          findPieceSelected(model) match
            case Some(piece) =>
              // show detailed graphics icon for selected piece
              GameAssets.gScorePanelPieceAndFlip(piece.pieceShape, piece.pieceIdentity, piece.bFlipped, dSF).moveTo(p6Scaled)
            case None =>
              // invoke magenta icon
              GameAssets.gScorePanelMagentaBlock(dSF).moveTo(p6Scaled)
        else
          // we are not the block player so invoke grey icon
          GameAssets.gScorePanelGreyBlock(dSF).moveTo(p6Scaled)
        end if
      else
        // it is not the blocks turn so invoke grey icon
        GameAssets.gScorePanelGreyBlock(dSF).moveTo(p6Scaled)
      end if
    end blockIcon

    val cylinderPlayer =
      TextBox((cylinderName).toString(), 220, 50).alignCenter
        .withColor(RGBA.Black)
        .withFontSize(Pixels(40))
        .scaleBy(dSF, dSF)
        .moveTo(p1Scaled)

    val blockPlayer =
      TextBox((blockName).toString(), 220, 50).alignCenter
        .withColor(RGBA.Black)
        .withFontSize(Pixels(40))
        .scaleBy(dSF, dSF)
        .moveTo(p2Scaled)

    val cylinderScore =
      TextBox((model.gameScore._1).toString(), 150, 300)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(100))
        .scaleBy(dSF, dSF)
        .moveTo(p3Scaled)

    val blockScore =
      TextBox((model.gameScore._2).toString(), 150, 300)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(100))
        .scaleBy(dSF, dSF)
        .moveTo(p4Scaled)

    val content1 = Layer(hybridScorePanel)
    val content2 = Layer(cylinderPlayer)
    val content3 = Layer(blockPlayer)
    val content4 = Layer(cylinderIcon)
    val content5 = Layer(blockIcon)
    val content6 = Layer(cylinderScore)
    val content7 = Layer(blockScore)

    (content1 |+| content2 |+| content3 |+| content4 |+| content5 |+| content6 |+| content7)
  end paint

  def coordsFromScore(score: Int, coordsForOneDigit: Point, coordsForTwoDigits: Point): Point =
    if score < 10 then coordsForOneDigit
    else coordsForTwoDigits
    end if
  end coordsFromScore

end ScorePanel
