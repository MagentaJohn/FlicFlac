package shared

val BOARD_SIZE_SMALL = 5
val BOARD_SIZE_MEDIUM = 6
val BOARD_SIZE_LARGE = 7
val BOARD_SIZE_XLARGE = 8 // default board size

val XLARGE_ARRAY_WIDTH = 9
val XLARGE_ARRAY_HEIGHT = 34

/*  class HH is the class behind each individual hex cell of the grid
    including the ones you cannot see.
 */

case class HH(
    x: Int, // .... cartesian x coordinate of centre of hex
    y: Int, // .... cartesian y coordinate of centre of hex
    c: Int, // .... colour and visibility of hex
    q: Int, // .... cubic q Coord (-ve to +ve = left to right)
    r: Int, // .... cubic r Coord (-ve to +ve = top right to bottom left)
    s: Int, // .... cubic s Coord (-ve to +ve = bottom right to top left)
)
end HH


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
    * HH(0,0,c,0,0,0),   HH(2,0,c,2,-1,-1),   HH(4,0,c,4,-2,-2),   HH(6,0,c,6,-3,-3)
    *            HH(1,1,c,1,0,-1),  HH(3,1,c,3,-1,-2),   HH(5,1,c,5,-2,-3),   HH(7,1,c,7,-3,-4)
    * HH(0,2,c,0,1,-1),  HH(2,2,c,2,0,-2),    HH(4,2,c,4,-1,-3),   HH(6,2,c,6,-2,-4)
    *            HH(1,3,c,1,1,-2),  HH(3,3,c,3,0,-3),    HH(5,3,c,5,-1,-4),   HH(7,3,c,7,-2,-5)
    * HH(0,4,c,0,2,-2),  HH(2,4,c,2,1,-3),    HH(4,4,c,4,0,-4),    HH(6,4,c,6,-1,-5)
    *
    * NB The storage for this snippet would be HexArray(4,5) ... even though the xy coords range from (0,0) to (7,4)
    */
// format : on

/* 
HexBoard includes the master var "hexArray", with original dimensions of 9x34. These dimensions are sufficient for all
four boardsizes. For each board size the appropriate hexagons are masked out as invalid. The dimensions and coordinate
system for hexArray does not change
 */

case class HexBoard(
  var boardSize: Int = BOARD_SIZE_XLARGE, // ..................................................... the size as supplied by the FlicFlacGameModel (default 8)
  val arrayWidth: Int = XLARGE_ARRAY_WIDTH, // ................................................... forcing arrayWidth=9 (calculated from sZ=8)
  val arrayHeight: Int = XLARGE_ARRAY_HEIGHT, // ................................................. forcing arrayHeight=34 (calculated from sZ=8)
  var hexArray: Array[Array[HH]] = Array.ofDim[HH](XLARGE_ARRAY_WIDTH, XLARGE_ARRAY_HEIGHT), //... starting array size (9x34)
) :

  def forge(size: Int): Unit = 
    // set the size
    boardSize = size

    // start with white board, populates q,r,s
    fillBoard(hexArray, arrayWidth, arrayHeight)

    // this is the pattern of the board
    colorBoardHexes(boardSize, hexArray, 2, arrayWidth, arrayHeight)

    // trim off the four corners (uses q,r,s coords)
    trimBoard(boardSize, hexArray, arrayWidth, arrayHeight, CX)

    // establish extra hexes for homepositions of pieces
    establishHomeHexes(boardSize, hexArray, arrayWidth, arrayHeight)

  end forge

  /*
  fillBoard populates each hexArray cell
  the embedded while loops populate x,y,c,q,r,s,xP,yP, but not xP, yP
  Afterwards, calculateXpyP calcualtes the paint position according to scale
  for the graphic to paint the hex cell
   */
  def fillBoard(hexArray: Array[Array[HH]], width: Int, height: Int): Unit =
    var row = 0
    while row < height do // array height
      var col = row & 1
      var n = 0
      while n < width do // array width
        val q = col
        val r = (row - col) / 2
        val s = (-row - col) / 2
        hexArray(n)(row) = HH(col, row, CW, q, r, s)
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
  def colorBoardHexes(boardSize: Int, hexArray: Array[Array[HH]], startingRow: Int, arrayWidth: Int, arrayHeight: Int): Unit =

    val rowTemplate8A: Array[Vector[Int]] = Array(
      Vector (CR, CY, CR, CR, CY, CR, CR, CY, CR),
      Vector (CO, CO, CP, CO, CO, CP, CO, CO, CP),
      Vector (CK, CK, CK, CK, CK, CK, CK, CK, CK),
      Vector (CY, CY, CB, CY, CY, CB, CY, CY, CB),
      Vector (CG, CO, CG, CG, CO, CG, CG, CO, CG),
      Vector (CK, CK, CK, CK, CK, CK, CK, CK, CK),

      Vector (CB, CR, CB, CB, CR, CB, CB, CR, CB),
      Vector (CP, CP, CG, CP, CP, CG, CP, CP, CG),
      Vector (CK, CK, CK, CK, CK, CK, CK, CK, CK),
      Vector (CR, CR, CY, CR, CR, CY, CR, CR, CY),
      Vector (CO, CP, CO, CO, CP, CO, CO, CP, CO),
      Vector (CK, CK, CK, CK, CK, CK, CK, CK, CK),

      Vector (CY, CB, CY, CY, CB, CY, CY, CB, CY),
      Vector (CG, CG, CO, CG, CG, CO, CG, CG, CO),
      Vector (CK, CK, CK, CK, CK, CK, CK, CK, CK),
      Vector (CB, CB, CR, CB, CB, CR, CB, CB, CR),
      Vector (CP, CG, CP, CP, CG, CP, CP, CG, CP),
      Vector (CK, CK, CK, CK, CK, CK, CK, CK, CK),
    )

    val tOffsets: (Int, Int, Int) = boardSize match
      case BOARD_SIZE_SMALL => (2, 12, 16) // = 2,12,16
      case BOARD_SIZE_MEDIUM => (2, 2, 0) // = 2,2,0
      case BOARD_SIZE_LARGE => (2, 10, 1) // = 2,10,1
      case _ => (2, 0, 0) // BOARD_SIZE_XLARGE = 2,0,0

    val startingRow = tOffsets._1
    val verticalOffset = tOffsets._2
    val horizontalOffset = tOffsets._3

    var col = 0
    var n = 0
    var thisRow = startingRow
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
        val hexColor = rowTemplate8A((thisRow + verticalOffset- 2) % rowTemplate8A.length)((n + horizontalOffset) % 9)
        setHexColor(hexArray, PointXY(n, thisRow), hexColor)
        col += 2
        n += 1
      end while
      thisRow += 1
    end while
  end colorBoardHexes

  /*
  sethexColor sets the color of a hex
   */

  def setHexColor(hexArray: Array[Array[HH]], pos: PointXY, col: Int): Unit =
    val hh = hexArray(pos.x)(pos.y)
    hexArray(pos.x)(pos.y) = HH(hh.x, hh.y, col, hh.q, hh.r, hh.s)
  end setHexColor

  /*
  trimBoard sets the color of the border hexes of the rectangular array such
  that the remaining hexes that have not been touched, form a large hexagon. This function
  is used help form the inital state of the game board
   */
  def trimBoard(boardSize: Int, hexArray: Array[Array[HH]], width: Int, height: Int, color: Int): Unit =
    val qrs = boardSize match
      case BOARD_SIZE_SMALL => (5, 13, -2, 6, -15,- 7)
      case BOARD_SIZE_MEDIUM => (3, 13,-2, 8, -16, -6)
      case BOARD_SIZE_LARGE => (3, 15, -3, 9, -18, -6)
      case _ => (1, 15, -3, 11, -19, -5) // default is XLARGE

      var y = 0
      while y < height do
        var x = 0
        while x < width do
          val hh = hexArray(x)(y)
          if (hh.q < qrs._1) || (hh.q > qrs._2) || (hh.r < qrs._3) || (hh.r > qrs._4) || (hh.s < qrs._5) || (hh.s > qrs._6) then 
            setHexColor(hexArray, PointXY(x, y), color)
          end if
          x += 1
        end while
        y += 1
      end while
  end trimBoard


  def getCylinderHomePos(boardSize: Int, id: Int): PointXY =

    boardSize match
      case BOARD_SIZE_SMALL =>
        id match
          case CB => PointXY(2, 5) // .......Blue
          case CR => PointXY(3, 4) // .......Red
          case CY => PointXY(3, 3) // .......Yellow
          case CO => PointXY(5, 23) // ......Orange
          case CG => PointXY(6, 22) // ......Green
          case CP => PointXY(6, 21) // ......Purple
        end match

      case BOARD_SIZE_MEDIUM =>
        id match
          case CB => PointXY(2, 4) // .......Blue
          case CR => PointXY(2, 3) // .......Red
          case CY => PointXY(3, 2) // .......Yellow
          case CO => PointXY(5, 26) // ......Orange
          case CG => PointXY(5, 25) // ......Green
          case CP => PointXY(6, 24) // ......Purple
        end match

      case BOARD_SIZE_LARGE => 
        id match
          case CB => PointXY(2, 4) // .......Blue
          case CR => PointXY(2, 3) // .......Red
          case CY => PointXY(3, 2) // .......Yellow
          case CO => PointXY(6, 28) // ......Orange
          case CG => PointXY(6, 27) // ......Green
          case CP => PointXY(7, 26) // ......Purple
        end match

      case _ => // this is BOARD_SIZE_XLARGE, the default
        id match
          case CB => PointXY(1, 4) // .......Blue
          case CR => PointXY(1, 3) // .......Red
          case CY => PointXY(2, 2) // .......Yellow
          case CO => PointXY(6, 30) // ......Orange
          case CG => PointXY(6, 29) // ......Green
          case CP => PointXY(7, 28) // ......Purple
        end match
      end match
  end getCylinderHomePos

  def getBlockHomePos(boardSize: Int, id: Int): PointXY =
    boardSize match
      case BOARD_SIZE_SMALL =>
        id match
          case CB => PointXY(2, 21) // .......Blue
          case CR => PointXY(3, 22) // .......Red
          case CY => PointXY(3, 23) // .......Yellow
          case CO => PointXY(5, 3) // ........Orange
          case CG => PointXY(6, 4) // ........Green
          case CP => PointXY(6, 5) // ........Purple
        end match

      case BOARD_SIZE_MEDIUM =>
        id match
          case CB => PointXY(2, 24) // .......Blue
          case CR => PointXY(2, 25) // .......Red
          case CY => PointXY(3, 26) // .......Yellow
          case CO => PointXY(5, 2) // ........Orange
          case CG => PointXY(5, 3) // ........Green
          case CP => PointXY(6, 4) // ........Purple
        end match

      case BOARD_SIZE_LARGE => 
        id match
          case CB => PointXY(2, 26) // .......Blue
          case CR => PointXY(2, 27) // .......Red
          case CY => PointXY(3, 28) // .......Yellow
          case CO => PointXY(6, 2) // ........Orange
          case CG => PointXY(6, 3) // ........Green
          case CP => PointXY(7, 4) // ........Purple
        end match

      case _ => // this is BOARD_SIZE_XLARGE, the default
        id match
          case CB => PointXY(1, 28) // .......Blue
          case CR => PointXY(1, 29) // .......Red
          case CY => PointXY(2, 30) // .......Yellow
          case CO => PointXY(6, 2) // ........Orange
          case CG => PointXY(6, 3) // ........Green
          case CP => PointXY(7, 4) // ........Purple
        end match
      end match
  end getBlockHomePos

  /*
  establishHomeHexes sets up extra black hex for the starting point for cylinder pieces
   */
  def establishHomeHexes(boardSize: Int, hexArray: Array[Array[HH]], width: Int, height: Int): Unit =
    setHexColor(hexArray, getCylinderHomePos(boardSize, CB), CB)
    setHexColor(hexArray, getCylinderHomePos(boardSize, CG), CG)
    setHexColor(hexArray, getCylinderHomePos(boardSize, CY), CY)
    setHexColor(hexArray, getCylinderHomePos(boardSize, CO), CO)
    setHexColor(hexArray, getCylinderHomePos(boardSize, CR), CR)
    setHexColor(hexArray, getCylinderHomePos(boardSize, CP), CP)
    setHexColor(hexArray, getBlockHomePos(boardSize, CB), CB)
    setHexColor(hexArray, getBlockHomePos(boardSize, CG), CG)
    setHexColor(hexArray, getBlockHomePos(boardSize, CY), CY)
    setHexColor(hexArray, getBlockHomePos(boardSize, CO), CO)
    setHexColor(hexArray, getBlockHomePos(boardSize, CR), CR)
    setHexColor(hexArray, getBlockHomePos(boardSize, CP), CP)
  end establishHomeHexes

  /**********************************
  Some core hexboard helper functions
  ***********************************/

  // detected a valid hex (ie is it part of the board) using Array Coordinates (as a point)
  def isThisHexValid(hexArray: Array[Array[HH]], pAxAy: PointXY): Boolean =
    (hexArray(pAxAy.x)(pAxAy.y).c != CX)
  end isThisHexValid

  // detected a valid hex (ie is it part of the board) using Cubic Coordinates
  def isThisHexValid(hexArray: Array[Array[HH]], q: Int, r: Int, s: Int): Boolean =
    val aXaY = getAxAyfromQRS(q, r, s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexValid(hexArray, pAxAy)
  end isThisHexValid

  // detecting a black hex using Array Coordinates (as a point)
  def isThisHexBlack(hexArray: Array[Array[HH]], pAxAy: PointXY): Boolean =
    (hexArray(pAxAy.x)(pAxAy.y).c == CK)
  end isThisHexBlack

  // detecting a black hex using Cubic Coordinates (q,r,s)
  def isThisHexBlack(hexArray: Array[Array[HH]], q: Int, r: Int, s: Int): Boolean =
    val aXaY = getAxAyfromQRS(q, r, s)
    val pAxAy = PointXY(aXaY._1, aXaY._2)
    isThisHexBlack(hexArray, pAxAy)
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
  def getHexColor(hexArray: Array[Array[HH]], pos: PointXY): Int =
    hexArray(pos.x)(pos.y).c
  end getHexColor

  /**************************************************
  6 Helper Coordinate Conversion Functions ...
  **************************************************/

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

end HexBoard
