package Table

import Cells._
import scala.Array._

class Table(width: Int, height: Int) {
	private val table = ofDim[Cell](width, height)
	fillTableWithEmptyCells()
	
	def getCell(ix: Int, iy: Int): Option[Cell] = {
		if (IndicesAreIncorrect(ix, iy))
			return None
		Some(table(ix)(iy))
	}
	
	def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
		if (IndicesAreIncorrect(ix, iy))
			throw new IllegalArgumentException
		
		table(ix)(iy) = cell
	}
	
	private def fillTableWithEmptyCells(): Unit = {
		for (i <- 0 until width)
			for ( j <- 0 until height)
				table(i)(j) = new EmptyCell();
	}
	
	private def IndicesAreIncorrect(ix: Int, iy: Int) : Boolean =
		ix >= width || ix < 0 || iy >= height || iy < 0
}