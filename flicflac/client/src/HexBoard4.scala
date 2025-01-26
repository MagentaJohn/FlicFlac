package game

import shared.*

import indigo.*

/*  class HH3 is the client class behind each individual hex cell of the client grid
    including the ones you cannot see.
 */

case class HH3(
    x: Int, // .... cartesian x coordinate of centre of hex
    y: Int, // .... cartesian y coordinate of centre of hex
    c: Int, // .... colour and visibility of hex
    q: Int, // .... cubic q Coord (-ve to +ve = left to right)
    r: Int, // .... cubic r Coord (-ve to +ve = top right to bottom left)
    s: Int, // .... cubic s Coord (-ve to +ve = bottom right to top left)
    xR: Int, // ... original copy of xS (ie with scale factor 1)
    yR: Int, // ... original copy of yS (ie with scale factor 1)
    xS: Int, // ... pixel x scaled offset from base point for origin of png to paint this hex
    yS: Int // .... pixel y scaled offset from base point for origin of png to paint this hex
)
end HH3


// format: off

  /** ******************* The Hex Board ***
    *
    * The implementation of this hexagonal grid is based on the information kindly supplied at
    * https://www.redblobgames.com/grids/hexagons/ Thankou! to Red Blob Games
    *
    * Please refer to my Documentation/HexInfo.pdf for a full description of the construction of the hexagonal grid
    *
    * This grid is "flat-top orientated" and uses a primary "cartesian coordinate" system (xCoord,yCoord) of "even-q" for
    * display purposes. This grid also carries a secondary "cube coordinate" system (qCoord, rCoord, sCoord) for
    * purposes of the gaming model
    *
    * The rows of the hexArray are interleaved with the odd rows being pushed down half a hexagon, eg the start of the
    * first four rows of the hexArray grid looks like this (so this is the top left hand corner with structure
    * abbrievated)...
    *
    * HH3(0,0,c,0,0,0,xP,yP),   HH3(2,0,c,2,-1,-1,xP,yP),   HH3(4,0,c,4,-2,-2,xP,yP),   HH3(6,0,c,6,-3,-3,xP,yP)
    *            HH3(1,1,c,1,0,-1,xP,yP),  HH3(3,1,c,3,-1,-2,xP,yP),   HH3(5,1,c,5,-2,-3,xP,yP),   HH3(7,1,c,7,-3,-4,xP,yP)
    * HH3(0,2,c,0,1,-1,xP,yP),  HH3(2,2,c,2,0,-2,xP,yP),    HH3(4,2,c,4,-1,-3,xP,yP),   HH3(6,2,c,6,-2,-4,xP,yP)
    *            HH3(1,3,c,1,1,-2,xP,yP),  HH3(3,3,c,3,0,-3,xP,yP),    HH3(5,3,c,5,-1,-4,xP,yP),   HH3(7,3,c,7,-2,-5,xP,yP)
    * HH3(0,4,c,0,2,-2,xP,yP),  HH3(2,4,c,2,1,-3,xP,yP),    HH3(4,4,c,4,0,-4,xP,yP),    HH3(6,4,c,6,-1,-5,xP,yP)
    *
    * NB The storage for this snippet would be HexArray(4,5) ... even though the xy coords range from (0,0) to (7,4)
    */
// format : on

/*
...... w is arrayWidth, the number of columns in "hexArray"
...... h is arrayHight, the number of rows in "hexArray"
...... aX,aY are coords used to access the cells of "hexArray"
...... x,y are cartesian coords of hex cell
...... q,r,s are cubic coords of hex cell
...... xP,yP are display coords for cell (and these are the coords that are scaled)
*/
case class HexBoard4(
  var boardSize: Int = BOARD_SIZE_XLARGE, // ..................... the size as supplied by the FlicFlacGameModel (default 8)
  val arrayWidth: Int = XLARGE_ARRAY_WIDTH, // ................... forcing arrayWidth=9 (calculated from sZ=8)
  val arrayHeight: Int = XLARGE_ARRAY_HEIGHT, // ................. forcing arrayHeight=34 (calculated from sZ=8)
  val graphicWidth: Int = 90, // ................................. the width of the graphic crop for each hex (90)
  val graphicHeight: Int = 80, // ................................ the width of the graphic crop for each hex (80)
  var pBase: PointXY = PointXY(200,0), // ........................ coords of invisible left hand corner(200,0)
  val xWidth: Int = 70, // ....................................... amount to add to a hex centre x coord to reach the vertical line of the next column (70)
  val yHeight: Int = 40, // ...................................... half the amount to add to a hex centre y coord to reach the next hexagon below (40)
  val xHalfway: Int = 10, // ..................................... xcoord of halfway along the top left diagonal line of first hex (10)
  var hexArray: Array[Array[HH3]] = Array.ofDim[HH3](9, 34), //... starting array size (9x34)
  var scalingFactor: Double = 1.0, // ............................ scaling factor as controlled by +/- buttons (1.0)
  var hexGridLayer: Layer.Content = Layer.empty // ............... Layer recalculated at start and on each scale change

):

  def derive(hexBoard:HexBoard): Unit =
    hexBoard4.boardSize = hexBoard.boardSize
    hexBoard4.pBase = hexBoard4.boardSize match
      case BOARD_SIZE_SMALL => PointXY(0, 0)
      case BOARD_SIZE_MEDIUM => PointXY(50, 0)
      case BOARD_SIZE_LARGE => PointXY(50, 0)
      case _ => PointXY(200, 0) // BOARD_SIZE_XLARGE

    // construct client version of hexboard which includes display coords
    constructBoard4(hexBoard.hexArray, hexBoard4.arrayWidth, hexBoard4.arrayHeight)

    // establish the paint positions for each hex
    calculateXsYs(hexBoard4.scalingFactor)

    // establish the first GridPaintLayer scaled to 1.0
    calculateGridPaintLayer()
  end derive

  // ########################################################

  /*
  constructBoard4 populates each hexArray cell
  the embedded while loops populate x,y,c,q,r,s,xP,yP, but not xP, yP
  Afterwards, calculateXpyP calcualtes the paint position according to scale
  for the graphic to paint the hex cell
   */
  def constructBoard4(hh: Array[Array[HH]],width: Int, height: Int): Unit =
    var row = 0
    while row < height do // array height
      var col = row & 1
      var n = 0
      while n < width do // array width      
        val c = hh(n)(row).c
        val q = hh(n)(row).q
        val r = hh(n)(row).r
        val s = hh(n)(row).s
        val xP = col * hexBoard4.xWidth
        val yP = row * hexBoard4.yHeight
        hexBoard4.hexArray(n)(row) = HH3(col, row, c, q, r, s, xP, yP, xP, yP)
        col += 2
        n += 1
      end while
      row += 1
    end while
  end constructBoard4

  /*
  calculateXsYs calculates the positions of the origins for the graphics used to paint each cell
  This function is invoked when a resize event occurs or the scale changes
   */
  def calculateXsYs(fS: Double): Unit =
    var y = 0
    while y < hexBoard4.arrayHeight do
      var x = 0
      while x < hexBoard4.arrayWidth do
        val hh = hexBoard4.hexArray(x)(y)
        val xS = math.round(hh.xR * fS).toInt
        val yS = math.round(hh.yR * fS).toInt
        hexBoard4.hexArray(x)(y) = HH3(hh.x, hh.y, hh.c, hh.q, hh.r, hh.s, hh.xR, hh.yR, xS, yS) // writing xS and yS away
        x += 1
      end while
      y += 1
    end while
    hexBoard4.scalingFactor = fS
  end calculateXsYs

  def calculateGridPaintLayer(): Unit =
    hexGridLayer = Layer.empty // start this combination with an empty layer
    var y = 0
    while y < hexBoard4.arrayHeight do
      var x = 0
      while x < hexBoard4.arrayWidth do
        val hh = hexBoard4.hexArray(x)(y)
        if hh.c != CX then
          // this hex is visible so paint it
          val layer = GameAssets.gHex(hexBoard4.scalingFactor).modifyMaterial(_.withTint(indigoMix(hh.c)))
          val scaledX = hh.xS + hexBoard4.pBase.x
          val scaledY = hh.yS + hexBoard4.pBase.y
          hexGridLayer = hexGridLayer |+| Layer(layer.moveTo(scaledX, scaledY))


// FIXME just for documentation
//
//          val tb = TextBox(hh.q+","+hh.r).alignLeft.bold
//            .withColor(RGBA.Black)
//            .withFontSize(Pixels(18))
//          hexGridLayer = hexGridLayer |+| Layer(tb.moveTo(scaledX+30,scaledY+30))
//
        end if

        x += 1
      end while
      y += 1
    end while
  end calculateGridPaintLayer

  def indigoMix(i: Int): RGBA =
    i match
      case CX => RGBA.fromHexString("#00000000") // Zero
      case CB => RGBA.fromHexString("#80C0FFFF") // Blue
      case CG => RGBA.fromHexString("#C0FFC0FF") // Green
      case CY => RGBA.fromHexString("#FFFFC0FF") // Yellow
      case CO => RGBA.fromHexString("#FFD070FF") // Orange
      case CR => RGBA.fromHexString("#FFC0C0FF") // Red
      case CP => RGBA.fromHexString("#CCCCFFFF") // Purple
      case CK => RGBA.fromHexString("#808080FF") // Black
      case CW => RGBA.fromHexString("#FFFFFFFF") // White
      case CC => RGBA.fromHexString("#00FFFFFF") // Cyan
      case CM => RGBA.fromHexString("#FF00FFFF") // Magenta
      case _  => RGBA.fromHexString("#FF00FFFF") // Magenta
  end indigoMix

  def getXsYs(pSrc: PointXY): PointXY =
    var pValidated = PointXY(0, 0)
    val x = pSrc.x
    val y = pSrc.y
    val pResult = PointXY(hexBoard4.hexArray(x)(y).xS, hexBoard4.hexArray(x)(y).yS)
    pResult
  end getXsYs

  // detected a valid hex (ie is it part of the board) using Array Coordinates (as a point)
  def isThisHexValid(pAxAy: PointXY): Boolean =
    (hexBoard4.hexArray(pAxAy.x)(pAxAy.y).c != CX)
  end isThisHexValid

  // detected a valid hex (ie is it part of the board) using Cubic Coordinates
  def isThisHexValid(q: Int, r: Int, s: Int): Boolean =
    val aXaY = getAxAyfromQRS(q, r, s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexValid(pAxAy)
  end isThisHexValid

  // detecting a black hex using Array Coordinates (as a point)
  def isThisHexBlack(pAxAy: PointXY): Boolean =
    (hexBoard4.hexArray(pAxAy.x)(pAxAy.y).c == CK)
  end isThisHexBlack

  // detecting a black hex using Cubic Coordinates (q,r,s)
  def isThisHexBlack(q: Int, r: Int, s: Int): Boolean =
    val aXaY = getAxAyfromQRS(q, r, s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexBlack(pAxAy)
  end isThisHexBlack

  // detect an occupied hex using Array Coordinates (as a point)
  def isThisHexFree(pAxAy: PointXY, vPieces: Vector[Piece]): Boolean =
    vPieces.find(p => p.pCurPos == pAxAy) match
      case Some(piece) => false
      case None        => true
  end isThisHexFree

// detect an occupied hex using Cubic Coordinates (q,r,s)
  def isThisHexFree(q: Int, r: Int, s: Int, vPieces: Vector[Piece]): Boolean =
    val aXaY = getAxAyfromQRS(q, r, s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexFree(pAxAy, vPieces)
  end isThisHexFree

// obtain color of this hex
  def getHexColor(pos: PointXY): Int =
    hexBoard4.hexArray(pos.x)(pos.y).c
  end getHexColor

    // convert from embedded qrs to embedded xy
  def getXYfromQRS(q: Int, r: Int, s: Int): (Int, Int) =
    val x = q
    val y = q + (2 * r)
    (x, y)
  end getXYfromQRS

  // convert from embedded xy to embedded qrs
  def getQRSfromXY(x: Int, y: Int): (Int, Int, Int) =
    val q = x
    val r = (y - x) / 2
    val s = -q - r
    (q, r, s)
  end getQRSfromXY

  // convert from array xy to embedded xy
  def getXYfromAxAy(aX: Int, aY: Int): (Int, Int) =
    val x = (2 * aX) + (aY & 1)
    val y = aY
    (x, y)
  end getXYfromAxAy

  // convert from embedded xy to array xy
  def getAxAyfromXY(x: Int, y: Int): (Int, Int) =
    val aX = (x - (x & 1)) / 2
    val aY = y
    (aX, aY)
  end getAxAyfromXY

  // convert from array xy to embedded qrs
  def getQRSfromAxAy(aX: Int, aY: Int): (Int, Int, Int) =
    val q = (2 * aX) + (aY & 1)
    val r = ((aY - (aY & 1)) / 2) - aX
    val s = -q - r
    (q, r, s)
  end getQRSfromAxAy

  // convert from embedded qrs to array xy
  def getAxAyfromQRS(q: Int, r: Int, s: Int): (Int, Int) =
    val aX = (q - (q & 1)) / 2
    val aY = q + (2 * r)
    (aX, aY)
  end getAxAyfromQRS

  /*
  hbPaint supplies the "SceneUpdateFragment" that contains all the graphics required to paint the hexboard
  Experience shows that this routine is time critical, so optimisation is key
   */

  def hbPaint(model: FlicFlacGameModel, dSF: Double): Layer =
    hexGridLayer
  end hbPaint

  /*
  getAxAyFromDisplayXY takes the mouse display coordinates (pDs) and converts them
  to a Point containing the X,Y indices into the underlying hex that matches pDs
  There is either Some(Point) or None
   */

  def getAxAyFromDisplayXY(pDs: PointXY, fS: Double): Option[PointXY] =
    // scribe.debug("hexXYFromDisplayXY START:" + pDs)
    val GWIDTH = hexBoard4.graphicWidth // ........................... The Hex graphic width without overlap of one pixel
    val GHEIGHT = hexBoard4.graphicHeight // ......................... The Hex graphic height without overlap of one pixel
    val pB = hexBoard4.pBase // ...................................... Base Corner (Top LHS) of Rectangle containing board
    val width = 16 * hexBoard4.xWidth // ............................. calculating board dimensions where xwidth is the small hexagon display width
    val height = 33 * hexBoard4.yHeight // ........................... calculating board dimensions where yHeight is only half the small hexagon display height
    val widthScaled = math.round((width * fS)).toInt // .............. scaling board dimensions
    val heightScaled = math.round((height * fS)).toInt // ............ scaling board dimensions
    val gWidthScaled = math.round(((GWIDTH / 2) * fS)).toInt // ...... scaling the dimensions of the original hex
    val gHeightScaled = math.round(((GHEIGHT / 2) * fS)).toInt // .... scaling the dimensions of the original hex
    val pC1 = PointXY(pB.x + gWidthScaled, pB.y + gHeightScaled) // .. PC1 is top LH corner of the detection rectangle
    val pC2 = PointXY(pC1.x + widthScaled, pC1.y + heightScaled) // .. pC2 is bottom RH corner of the detection rectangle
    val xH = (hexBoard4.xHalfway * fS).toInt // ...................... scaling the tiny offset required for detection grid alignment

    // scribe.debug("hexXYFromDisplayXY BOUNDARIES:: " + pC1 + " :: " + pC2)

    // The detection grid needs to start halfway up the top LH diagonal of the first hex which (before scaling) is 10,20)
    if (pDs.x >= pC1.x + xH) && (pDs.x < pC2.x - xH) && (pDs.y >= pC1.y) && (pDs.y < pC2.y) then
      // we know that point pDs is valid, ie it is in the detection rectangle
      val offsetX = pDs.x - pB.x - xH
      val xWidthScaled = math.round((hexBoard4.xWidth * fS)).toInt
      val x = (offsetX / xWidthScaled).toInt
      val yHeightScaled = math.round(hexBoard4.yHeight * fS).toInt
      val offsetY = pDs.y - pB.y - ((x & 1) * yHeightScaled)
      val y = ((offsetY / yHeightScaled) & 0xfffe) + (x & 1) // << this enforces  ((x & y are even) || (x & y are odd))

      // scribe.debug("hexXYFromDisplayXY OFFSETS X/Y " + offsetX + ":" + offsetY + " POS X/Y " + x + ":" + y + " W:" + xWidth + " H:" + yHeight)

      val c = hexBoard4.hexArray(x / 2)(y).c
      if c != CX then // ...................... exclude hexes from display if color is CX
        val pAxAy = PointXY(x / 2, y) // ........ x/2 because hexArray has even/odd columns
        // scribe.debug("hexXYFromDisplayXY FINISH:" + hexXYCoords)
        Some(pAxAy)
      else
        // scribe.debug("hexXYFromDisplayXY FINISHES with NONE (non-displayable hex)")
        None
      end if
    else
      // scribe.debug("hexXYFromDisplayXY FINISHES with NONE (outside detection grid)")
      None
    end if
  end getAxAyFromDisplayXY

end HexBoard4
