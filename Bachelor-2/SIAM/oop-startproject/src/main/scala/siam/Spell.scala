package siam

trait Rotating {
  private def quarterTurn(piece: Piece): String =
    piece.direction match
      case "up" => "right"
      case "right" => "down"
      case "down" => "left"
      case "left" => "up"
  def rotate(piece: Piece): Unit =
      piece.rotate(quarterTurn(piece))
}

trait RestrictedMovement {
  private var oldRow: Int = invalid
  private var oldCol: Int = invalid
  def setup(piece: Piece): Unit =
    oldRow = piece.row
    oldCol = piece.col
  def restrict(piece: Piece): Unit =
    if (piece.row != oldRow || piece.col != oldCol){
      piece.movementCounter -= 1
      oldRow = piece.row
      oldCol = piece.col
    }

  def reset(): Unit =
    oldRow = invalid
    oldCol = invalid
}

trait Death {
  protected val countdown = 2  //protected zal handig zijn om nieuwe spreuken te maken (zodat we countdown kunnen wijzigen in subtypes)
  private var counter = countdown
  def kill(piece: Piece): Unit =
    if counter == 0 then piece.kill()
    else counter -= 1

  def reset(): Unit =
    counter = countdown
}

sealed abstract class Spell {
  def apply(piece: Piece): Unit =
    if piece.underSpell == null then piece.addSpell(this)
    else println("piece has already a spell!")

  def execute(piece: Piece): Unit
  def init(): Unit
}

case class UncontrolledRotation(name: String) extends Spell with Rotating {
  override def execute(piece: Piece): Unit =
    rotate(piece)

  override def init(): Unit = println(name)
}

case class LimitedMovement(name: String) extends Spell with RestrictedMovement {
  override def apply(piece: Piece): Unit =
    setup(piece)
    super.apply(piece)
  override def execute(piece: Piece): Unit =
    restrict(piece)

  override def init(): Unit =
    reset()
}

case class SuddenDeath(name: String) extends Spell with Death {
  override def execute(piece: Piece): Unit =
    kill(piece)

  override def init(): Unit =
    reset()
}

case class Combined(name: String) extends Spell with Rotating with RestrictedMovement {
  override def execute(piece: Piece): Unit =
    rotate(piece)
    restrict(piece)

  override def init(): Unit =
    reset() // gaat de reset methode oproepen van RestrictedMovement

}






/*
object Test:
  def main(args: Array[String]): Unit = {
    var elephant = new Elephant(0, 0)
    println(s"${elephant.direction}")
    UncontrolledRotation.apply(elephant)
    println(s"${elephant.direction}")
  }*/