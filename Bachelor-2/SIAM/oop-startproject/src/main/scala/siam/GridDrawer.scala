package siam
import gamelib.*
import utilities.*

import java.awt.Graphics2D
import scala.jdk.CollectionConverters.*

object GridDrawer:
  def draw(
            grid: SiamGrid[Piece],
            panel: GridPanel,
            cellWidth: Int,
            cellHeight: Int,
            padding: Int
          ): Unit =
    panel.clear()
    var cells: List[Cell] = List()
    grid.forEach((row, column, piece) =>
      cells = PieceCell(
        piece,
        (column * cellHeight)+padding,
        (row * cellWidth)+padding,
        cellWidth
      ) :: cells
    )
    cells = cells ::: listButton

    panel.addCells(cells.asJava)
