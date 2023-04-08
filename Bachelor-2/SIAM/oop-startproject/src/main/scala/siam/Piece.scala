package siam
/**
  * PIECE
  */
abstract class Piece {
  var row: Int
  var col: Int
  var direction: String
  val owner: Player
  var dead = false
  var underSpell: Spell = null // als er een spreuk werd gebruikt op de piece, dan wordt het hier onthouden
  var movementCounter = 2 // wanneer een piece de 'beperkte bewegingsvrijheid' spreuk heeft kan het maar 2 keer bewegen

  def addSpell(spell: Spell): Unit =
    this.underSpell = spell

  def removeSpell(): Unit =
    this.underSpell = null
    this.movementCounter = 2

  def rotate(newDir: String): Unit =
    this.direction = newDir
  
  def move(newRow: Int, newCol: Int): Unit =
    this.row = newRow
    this.col = newCol

  def kill(): Unit =
    this.dead = true

  def revive(): Unit =
    this.dead = false
}
