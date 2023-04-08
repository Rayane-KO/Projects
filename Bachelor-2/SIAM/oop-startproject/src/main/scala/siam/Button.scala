package siam

/**
  * Button
  * stelt een knop voor
  */
abstract class Button{
  var x: Int
  var y: Int
  var image = "none"
  var width: Int
  var height: Int
  var visible = true // als het visible is, wordt het getekend

  def checkClick(clickX: Int, clickY: Int): Boolean =
    if visible then clickX >= x && clickX <= x + width && clickY >= y && clickY <= y + height else false

  def flipState(): Unit = 
    if visible then visible = false else visible = true
}

//// alle knoppen die nodig zijn voor het spel \\\\\
// knoppen voor de functionaliteiten nl. bewegen, draaien, piece toevoegen en spreuk zetten
case class MoveButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class RotateButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class AddButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class SpellsButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
// knoppen om een richting te kiezen
case class UpButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class DownButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class LeftButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class RightButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
// knop dat de huidige speler voorstelt
case class PlayerButton(var x: Int, var y: Int, var width: Int, var height: Int, var player: Player) extends Button
// knoppen voor de spreuken
case class LimitedMButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class UncontrolledRButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class UnexpectedDButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button
case class CombinedButton(var x: Int, var y: Int, var width: Int, var height: Int) extends Button

