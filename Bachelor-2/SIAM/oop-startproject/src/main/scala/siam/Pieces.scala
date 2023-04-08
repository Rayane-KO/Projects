package siam
import java.awt.Graphics2D

/**
  * PIECES
  * @param row rij van de piece
  * @param col kolom van de piece
  * @param owner van wie is de piece
  * enkel voor de EmptyPiece is dit anders omdat het een leeg piece voorstelt
  * de rij en kolom van een leeg piece is niet belangrijk
  */

final case class Elephant(var row: Int, var col: Int, owner: Player) extends Piece {
  var direction = "up"
}

final case class Rhino(var row: Int, var col: Int, owner: Player) extends Piece {
  var direction = "down"
}

case object EmptyPiece extends Piece {
  var row: Int = invalid
  var col: Int = invalid
  val owner: Player = null // lege pieces behoren tot geen enkele speler
  var direction = "none"
}

final case class Mountain(var row: Int, var col: Int) extends Piece{
  var direction = "none"
  val owner: Player = null // bergen behoren tot geen enkele speler
}

/*object Test:
  def main(args: Array[String]): Unit = {
    var elephant = new Elephant(0, 0)
    var rhino = new Rhino(0, 0)
    val row = elephant.row
    print(s"$row")
}*/
