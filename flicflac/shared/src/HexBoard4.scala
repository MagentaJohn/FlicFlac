package shared

val BOARD_SIZE_SMALL = 5
val BOARD_SIZE_MEDIUM = 6
val BOARD_SIZE_LARGE = 7
val BOARD_SIZE_XLARGE = 8 // default board size

/*  class HH is the class behind each individual hex cell of the grid
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
  val arrayWidth: Int = 9, // .................................... forcing arrayWidth=9 (calculated from sZ=3)
  val arrayHeight: Int = 34, // .................................. forcing arrayHeight=34 (calculated from sZ=3)
  val graphicWidth: Int = 90, // ................................. the width of the graphic crop for each hex (90)
  val graphicHeight: Int = 80, // ................................ the width of the graphic crop for each hex (80)
  var pBase: PointXY = PointXY(200,0), // ........................ coords of invisible left hand corner(200,0)
  val xWidth: Int = 70, // ....................................... amount to add to a hex centre x coord to reach the vertical line of the next column (70)
  val yHeight: Int = 40, // ...................................... half the amount to add to a hex centre y coord to reach the next hexagon below (40)
  val xHalfway: Int = 10, // ..................................... xcoord of halfway along the top left diagonal line of first hex (10)
  var hexArray: Array[Array[HH3]] = Array.ofDim[HH3](9, 34), //... starting array size (9x34)
  var scalingFactor: Double = 1.0 // ............................. scaling factor as controlled by +/- buttons (1.0)
)
end HexBoard4
