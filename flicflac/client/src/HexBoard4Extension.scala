package game

import shared.*

import indigo.*

var hexGridLayer: Layer.Content = Layer.empty // ... Layer recalculated at start and on each +/-

extension (hexBoard4: HexBoard4)

  def create(size: Int) : Unit =
    hexBoard4.boardSize = size
    hexBoard4.pBase = size match
      case 5 => PointXY(0, 0)
      case 6 => PointXY(50, 0)
      case 7 => PointXY(50, 0)
      case _ => PointXY(200, 0)
    
    // start with black board, populates q,r,s (for debugging the helper routine printBoard can follow this line)
    fillBoard(hexBoard4.arrayWidth, hexBoard4.arrayHeight, RGBA.Black) // This is also index CK but here we must interface to indigo

    // this is the pattern of the board
    colorBoardHexes(2, hexBoard4.arrayWidth, hexBoard4.arrayHeight )

    // trim off the four corners (uses q,r,s coords)
    trimBoard( hexBoard4.boardSize, hexBoard4.arrayWidth, hexBoard4.arrayHeight, CX )

    // establish extra hexes for homepositions of pieces
    establishHomeHexes( hexBoard4.boardSize, hexBoard4.arrayWidth, hexBoard4.arrayHeight)

    // establish the paint positions for each hex
    calculateXsYs(hexBoard4.scalingFactor)

    // establish the first GridPaintLayer scaled to 1.0
    calculateGridPaintLayer()
  end create

  // ########################################################

  /*
  createArrayOfHH creates an array of mini hexagons according to width and height parameters
  NB width and height are hexagonal width/height dimensions, not physical dimensions
   */
  def createArrayOfHH(width: Int, height: Int): Array[Array[HH3]] =
    Array.ofDim[HH3](width, height)

  /*
  fillBoard populates each hexArray cell
  the embedded while loops populate x,y,c,q,r,s,xP,yP, but not xP, yP
  Afterwards, calculateXpyP calcualtes the paint position according to scale
  for the graphic to paint the hex cell
   */
  def fillBoard(width: Int, height: Int, color: RGBA): Unit =
    var row = 0
    while row < height do // array height
      var col = row & 1
      var n = 0
      while n < width do  // array width
        val q = col
        val r = (row - col) / 2
        val s = (-row - col) / 2
        val xP = col * hexBoard4.xWidth
        val yP = row * hexBoard4.yHeight
        hexBoard4.hexArray(n)(row) = HH3(col, row, CW, q, r, s, xP, yP, xP, yP)
        col += 2
        n += 1
      end while
      row += 1
    end while
  end fillBoard

  /*
  colorBoardHexes generates the pattern colors for each mini hex using the
  color combinations in the row templates
   */
  def colorBoardHexes(row: Int, arrayWidth: Int, arrayHeight: Int): Unit =

    val rowTemplate5: Array[Vector[Int]] = Array(
      Vector(CB, CY, CY), Vector(CG, CO, CG), Vector(CK, CK, CK), Vector(CB, CR, CB), Vector(CG, CP, CP), Vector(CK, CK, CK),
      Vector(CY, CR, CR), Vector(CO, CP, CO), Vector(CK, CK, CK), Vector(CY, CB, CY), Vector(CO, CG, CG), Vector(CK, CK, CK),
      Vector(CR, CB, CB), Vector(CP, CG, CP), Vector(CK, CK, CK), Vector(CR, CY, CR), Vector(CP, CO, CO), Vector(CK, CK, CK)
    )

    val rowTemplate6: Array[Vector[Int]] = Array(
      Vector(CK, CK, CK), Vector(CY, CY, CB), Vector(CG, CO, CG), Vector(CK, CK, CK), Vector(CB, CR, CB), Vector(CP, CP, CG),
      Vector(CK, CK, CK), Vector(CR, CR, CY), Vector(CO, CP, CO), Vector(CK, CK, CK), Vector(CY, CB, CY), Vector(CG, CG, CO),
      Vector(CK, CK, CK), Vector(CB, CB, CR), Vector(CP, CG, CP), Vector(CK, CK, CK), Vector(CR, CY, CR), Vector(CO, CO, CP)
    )

    val rowTemplate7: Array[Vector[Int]] = Array(
      Vector(CP, CO, CO), Vector(CK, CK, CK), Vector(CB, CY, CY), Vector(CG, CO, CG), Vector(CK, CK, CK), Vector(CB, CR, CB),
      Vector(CG, CP, CP), Vector(CK, CK, CK), Vector(CY, CR, CR), Vector(CO, CP, CO), Vector(CK, CK, CK), Vector(CY, CB, CY),
      Vector(CO, CG, CG), Vector(CK, CK, CK), Vector(CR, CB, CB), Vector(CP, CG, CP), Vector(CK, CK, CK), Vector(CR, CY, CR)
    )

    val rowTemplate8: Array[Vector[Int]] = Array(
      Vector(CR, CY, CR), Vector(CO, CO, CP), Vector(CK, CK, CK), Vector(CY, CY, CB), Vector(CG, CO, CG), Vector(CK, CK, CK),
      Vector(CB, CR, CB), Vector(CP, CP, CG), Vector(CK, CK, CK), Vector(CR, CR, CY), Vector(CO, CP, CO), Vector(CK, CK, CK),
      Vector(CY, CB, CY), Vector(CG, CG, CO), Vector(CK, CK, CK), Vector(CB, CB, CR), Vector(CP, CG, CP), Vector(CK, CK, CK)
    )

    val rowTemplateX = hexBoard4.boardSize match
      case 5 => rowTemplate5
      case 6 => rowTemplate6
      case 7 => rowTemplate7
      case _ => rowTemplate8

    var col = 0
    var n = 0
    var thisRow = row
    while thisRow < arrayHeight - 2 do
      if (thisRow & 1) == 0 then
        col = 2
        n = 1
      else
        col = 1
        n = 0
      end if

      // we use -2 here to allow for top border
      while n < arrayWidth - 1 do
        val hexColor = rowTemplateX((thisRow - 2) % rowTemplateX.length)(n % 3)
        setHexColor(PointXY(n,thisRow),hexColor)
        col += 2
        n += 1
      end while
      thisRow += 1
    end while
  end colorBoardHexes

  /*
  getQRSofTopCentreHex supplies the cubic cordinates of the border top centre hex
   */
  def getQRSofTopCentreHex(boardSize: Int, width: Int, height: Int): (Int, Int, Int) =
    val x: Int = (width - 1) / 2
    //val y: Int = if ((width - 1) & 1) == 1 then 1 else 0
    val y = boardSize match
      case 5 => 3 // Small
      case 6 => 2 // Medium
      case 7 => 1 // Large
      case _ => 0 // Extra Large
    return (hexBoard4.hexArray(x)(y).q, hexBoard4.hexArray(x)(y).r, hexBoard4.hexArray(x)(y).s)
  end getQRSofTopCentreHex

  def getQRSofTopLeftHex(boardSize :Int, width: Int, height: Int): (Int, Int, Int) =
    val topQRS = getQRSofTopCentreHex(boardSize, width, height)
    val topLeft = (topQRS._1-boardSize, topQRS._2+boardSize, topQRS._3)
    topLeft
  end getQRSofTopLeftHex

  def getQRSofTopRightHex(boardSize :Int, width: Int, height: Int): (Int, Int, Int) =
    val topQRS = getQRSofTopCentreHex(boardSize, width, height)
    val topRight = (topQRS._1+boardSize, topQRS._2, topQRS._3-boardSize)
    topRight
  end getQRSofTopRightHex

  /*
  getQRSofBottomCentreHex supplies the cubic cordinates of the border bottom centre hex
   */
  def getQRSofBottomCentreHex(boardSize: Int, width: Int, height: Int): (Int, Int, Int) =
    val x: Int = (width - 1) / 2
    //val y: Int = if ((width - 1) & 1) == 1 then height - 1 else height - 2
    val y = boardSize match
      case 5 => height - 11 // Small
      case 6 => height - 8 // Medium
      case 7 => height - 5 // Large
      case _ => height - 2 // Extra Large

    return (hexBoard4.hexArray(x)(y).q, hexBoard4.hexArray(x)(y).r, hexBoard4.hexArray(x)(y).s)
  end getQRSofBottomCentreHex

  def getQRSofBottomLeftHex(boardSize :Int, width: Int, height: Int): (Int, Int, Int) =
    val bottomQRS = getQRSofBottomCentreHex(boardSize, width, height)
    val bottomLeft= (bottomQRS._1-boardSize, bottomQRS._2, bottomQRS._3+boardSize)
    bottomLeft
  end getQRSofBottomLeftHex

  def getQRSofBottomRightHex(boardSize :Int, width: Int, height: Int): (Int, Int, Int) =
    val bottomQRS = getQRSofBottomCentreHex(boardSize, width, height)
    val bottomRight = (bottomQRS._1+boardSize, bottomQRS._2-boardSize, bottomQRS._3)
    bottomRight
  end getQRSofBottomRightHex

  /*
  trimBoard sets the color of the border hexes of the rectangular array such
  that the remaining hexes that have not been touched, form a large hexagon. This function
  is used help form the inital state of the game board
   */
  def trimBoard(size: Int, width: Int, height: Int, color: Int): Unit =
    val topQRS = getQRSofTopCentreHex(size, width, height)
    val bottomQRS = getQRSofBottomCentreHex(size,width, height)
    val leftQRS = getQRSofTopLeftHex(size, width, height)
    val rightQRS = getQRSofTopRightHex(size, width, height)
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        val hh = hexBoard4.hexArray(x)(y)
        if (hh.s >= topQRS._3) || (hh.r <= topQRS._2) || (hh.s <= bottomQRS._3) || (hh.r >= bottomQRS._2) || (hh.q <= leftQRS._1) || (hh.q >= rightQRS._1)then
          setHexColor(PointXY(x,y),color)
        end if
        x += 1
      end while
      y += 1
    end while
  end trimBoard

  /* 
  establishHomeHexes sets up extra black hex for the starting point for cylinder pieces
   */
  def establishHomeHexes(size: Int, width: Int, height: Int): Unit =
    setHexColor(getCylinderHomePos(CB), CB)
    setHexColor(getCylinderHomePos(CG), CG)
    setHexColor(getCylinderHomePos(CY), CY)
    setHexColor(getCylinderHomePos(CO), CO)
    setHexColor(getCylinderHomePos(CR), CR)
    setHexColor(getCylinderHomePos(CP), CP)
    setHexColor(getBlockHomePos(CB), CB)
    setHexColor(getBlockHomePos(CG), CG)
    setHexColor(getBlockHomePos(CY), CY)
    setHexColor(getBlockHomePos(CO), CO)
    setHexColor(getBlockHomePos(CR), CR)
    setHexColor(getBlockHomePos(CP), CP)
  end establishHomeHexes


  /* 
  sethexColor sets the color of a hex
   */

  def setHexColor(pos: PointXY, col : Int) : Unit =
    val hh = hexBoard4.hexArray(pos.x)(pos.y)
    hexBoard4.hexArray(pos.x)(pos.y) = HH3(hh.x, hh.y, col, hh.q, hh.r, hh.s, hh.xR, hh.yR, hh.xS, hh.yS)
  end setHexColor

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
        hexBoard4.hexArray(x)(y) = HH3(hh.x,hh.y,hh.c,hh.q,hh.r,hh.s,hh.xR,hh.yR,xS,yS) // writing xS and yS away
        x += 1
      end while
      y += 1
    end while
    hexBoard4.scalingFactor = fS
  end calculateXsYs

  def calculateGridPaintLayer() : Unit =
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

  def getXsYs(pSrc: PointXY) : PointXY =
    var pValidated = PointXY(0,0)
    val x = pSrc.x
    val y = pSrc.y
    val pResult = PointXY(hexBoard4.hexArray(x)(y).xS, hexBoard4.hexArray(x)(y).yS)
    pResult
  end getXsYs

  // detected a valid hex (ie is it part of the board) using Array Coordinates (as a point)
  def isThisHexValid(pAxAy: PointXY) : Boolean =
    (hexBoard4.hexArray(pAxAy.x)(pAxAy.y).c != CX)
  end isThisHexValid

  // detected a valid hex (ie is it part of the board) using Cubic Coordinates
  def isThisHexValid(q: Int, r: Int, s: Int) : Boolean =
    val aXaY = getAxAyfromQRS(q,r,s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexValid(pAxAy)
  end isThisHexValid

  // detecting a black hex using Array Coordinates (as a point)
  def isThisHexBlack(pAxAy: PointXY) : Boolean =
    (hexBoard4.hexArray(pAxAy.x)(pAxAy.y).c == CK)
  end isThisHexBlack

  // detecting a black hex using Cubic Coordinates (q,r,s)
  def isThisHexBlack(q: Int, r: Int, s: Int) : Boolean =
    val aXaY = getAxAyfromQRS(q,r,s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexBlack(pAxAy)
  end isThisHexBlack

  // detect an occupied hex using Array Coordinates (as a point)
  def isThisHexFree(pAxAy: PointXY, vPieces: Vector[Piece]) : Boolean =
    vPieces.find(p=>p.pCurPos == pAxAy) match
      case Some(piece) => false
      case None => true
  end isThisHexFree

// detect an occupied hex using Cubic Coordinates (q,r,s)
  def isThisHexFree(q: Int, r: Int, s: Int, vPieces: Vector[Piece]) : Boolean =
    val aXaY = getAxAyfromQRS(q,r,s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexFree(pAxAy, vPieces)
  end isThisHexFree

// obtain color of this hex
  def getHexColor(pos: PointXY) : Int =
    hexBoard4.hexArray(pos.x)(pos.y).c
  end getHexColor

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
    //scribe.debug("hexXYFromDisplayXY START:" + pDs)
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

    //scribe.debug("hexXYFromDisplayXY BOUNDARIES:: " + pC1 + " :: " + pC2)

    // The detection grid needs to start halfway up the top LH diagonal of the first hex which (before scaling) is 10,20)
    if (pDs.x >= pC1.x + xH) && (pDs.x < pC2.x - xH) && (pDs.y >= pC1.y) && (pDs.y < pC2.y) then
      // we know that point pDs is valid, ie it is in the detection rectangle
      val offsetX = pDs.x - pB.x - xH
      val xWidthScaled = math.round((hexBoard4.xWidth * fS)).toInt
      val x = (offsetX / xWidthScaled).toInt
      val yHeightScaled = math.round(hexBoard4.yHeight * fS).toInt
      val offsetY = pDs.y - pB.y - ((x & 1) * yHeightScaled)
      val y = ((offsetY / yHeightScaled) & 0xfffe) + (x & 1) // << this enforces  ((x & y are even) || (x & y are odd))

      //scribe.debug("hexXYFromDisplayXY OFFSETS X/Y " + offsetX + ":" + offsetY + " POS X/Y " + x + ":" + y + " W:" + xWidth + " H:" + yHeight)

      val c = hexBoard4.hexArray(x / 2)(y).c
      if (c != CX) then // ...................... exclude hexes from display if color is CX
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


  def getCylinderHomePos(id: Int): PointXY =
    val p1 = hexBoard4.boardSize match
      case 5 =>
        if (id == CR) then PointXY(2,2) else PointXY(1,2)
      case 6 => PointXY(1,1)
      case 7 => PointXY(1,1)
      case _ => PointXY(0,1)
    
    val aW = hexBoard4.arrayWidth
    val aH = hexBoard4.arrayHeight
    val p4 = hexBoard4.boardSize match
      case 5 =>
        if (id == CG) then PointXY(aW-2, aH-10) else PointXY(aW-3, aH-10)
      case 6 => PointXY(aW-3, aH-7)
      case 7 => PointXY(aW-2, aH-5)
      case _ => PointXY(aW-2, aH-3)

    id match
        case CB => p1 + PointXY(1,3) // .......Blue
        case CR => p1 + PointXY(1,2) // .......Red
        case CY => p1 + PointXY(2,1) // .......Yellow
        case CO => p4 + PointXY(-1,-1) // .....Orange
        case CG => p4 + PointXY(-1,-2) // .....Green
        case CP => p4 + PointXY(0,-3) // ......Purple
    end match
  end getCylinderHomePos

  def getBlockHomePos(id: Int): PointXY =
    val aW = hexBoard4.arrayWidth
    val aH = hexBoard4.arrayHeight
    val p2 = hexBoard4.boardSize match
      case 5 =>
        if (id==CG) then PointXY(aW-2,2) else PointXY(aW-3,2)
      case 6 => PointXY(aW-3,1)
      case 7 => PointXY(aW-2,1)
      case _ => PointXY(aW-2,1)
    
    val p3 = hexBoard4.boardSize match
      case 5 =>
        if (id==CR) then PointXY(2, aH-10) else PointXY(1, aH-10)
      case 6 => PointXY(1, aH-7)
      case 7 => PointXY(1, aH-5)
      case _ => PointXY(0, aH-3)

    id match
        case CB => p3 + PointXY(1,-3) // .....Blue
        case CR => p3 + PointXY(1,-2) // .....Red
        case CY => p3 + PointXY(2,-1) // .....Yellow
        case CO => p2 + PointXY(-1,1) // .....Orange
        case CG => p2 + PointXY(-1,2) // .....Green
        case CP => p2 + PointXY(0,3) // ......Purple
    end match
  end getBlockHomePos

  // convert from embedded qrs to embedded xy
  def getXYfromQRS(q: Int, r: Int, s:Int) : (Int, Int) =
    val x = q
    val y = q + (2*r)
    (x,y)
  end getXYfromQRS

  // convert from embedded xy to embedded qrs
  def getQRSfromXY(x: Int, y: Int) : (Int, Int, Int) =
    val q = x
    val r = (y-x)/2
    val s = -q -r
    (q,r,s)
  end getQRSfromXY

  // convert from array xy to embedded xy
  def getXYfromAxAy(aX: Int, aY: Int) : (Int, Int) =
    val x = (2*aX) + (aY&1)
    val y = aY
    (x,y)
  end getXYfromAxAy

  // convert from embedded xy to array xy
  def getAxAyfromXY(x: Int, y: Int) : (Int, Int) =
    val aX = (x - (x&1))/2
    val aY = y
    (aX,aY)
  end getAxAyfromXY

  // convert from array xy to embedded qrs
  def getQRSfromAxAy(aX: Int, aY: Int) : (Int, Int, Int) =
    val q = (2 * aX) + (aY & 1)
    val r = ((aY - (aY&1))/2) - aX
    val s = -q -r
    (q,r,s)
  end getQRSfromAxAy

  // convert from embedded qrs to array xy
  def getAxAyfromQRS(q: Int, r: Int, s:Int) : (Int, Int) =
    val aX = (q -  (q&1))/2
    val aY = q + (2*r)
    (aX,aY)
  end getAxAyfromQRS

end extension
