package Cells

import Table.Table

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
	def getReferencedCell: Option[Cell] = table.getCell(ix, iy)
	
	override def toString: String = {
		val referencedCell = getReferencedCell
		
		referencedCell match {
			case None => "outOfRange"
			case Some(cell: ReferenceCell) =>
				if (cell.getReferencedCell.get == this) "cyclic" else cell.getReferencedCell.get.toString
			case _ => referencedCell.get.toString
		}
	}
}
