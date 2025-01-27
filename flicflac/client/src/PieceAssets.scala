package game

import shared.*

import indigo.*

// Common Assets needed to build each piece
object PieceAssets:
  val gWidth = 90 // .......... GWIDTH pixel height of graphic (also known as iHexPixelWidth)
  val gHeight = 80 // ......... GHEIGHT pixel height of graphic (also known as iHexPixelHeight)
  val blocksAssetName = AssetName(GameAssets.blAssetName)
  val cylindersAssetName = AssetName(GameAssets.cyAssetName)

  def getGraphic(shape: Int, id: Int, flipped: Boolean): Graphic[Material.ImageEffects] =
    val safeId = id % (6 + 1) // there are six main colours + 1 more, which is for grey (the captured color)
    val pieceAssetName = if shape == CYLINDER then cylindersAssetName else blocksAssetName
    val verticalOffset = if flipped then gHeight else 0
    val pieceRect = Rectangle(gWidth * safeId, 0 + verticalOffset, gWidth + 1, gHeight + 1)
    val pieceGraphic = Graphic(pieceRect, 4, Material.ImageEffects(pieceAssetName)) // Pieces on Layer 4
    pieceGraphic
  end getGraphic

end PieceAssets
