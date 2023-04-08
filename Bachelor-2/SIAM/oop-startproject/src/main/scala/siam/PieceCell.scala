package siam
import gamelib.AssetsLoader
import java.awt.Graphics2D
/**
  * PIECECELL
  * @param piece de piece die zal getekend worden
  * @param x de x positie van de afbeelding
  * @param y de y positie van de afbeelding
  * @param size de grootte van de afbeelding
  * afhankelijk van het type piece en of het wel of niet onder een spreuk is
  * zal het een ander afbeelding tekenen
  */
class PieceCell(piece: Piece, x: Int, y: Int, size: Int) extends gamelib.Cell:
  val direction: String = piece.direction
  var image_up: String = "none"
  var image_down: String = "none"
  var image_left: String = "none"
  var image_right: String = "none"
  var image_to_draw: String = "none"
  var spell_to_draw: String = "none"
  var empty: Boolean = false

  override def draw(g: Graphics2D): Unit = 
    if (piece.underSpell != null){
      piece.underSpell match
        case limitedMovement: LimitedMovement => spell_to_draw = "beperkte_bewegingsvrijheid.png"
        case suddenDeath: SuddenDeath => spell_to_draw = "onverwachte_dood.png"
        case uncontrolledRotation: UncontrolledRotation => spell_to_draw = "ongecontroleerd_draaien.png"
        case combined: Combined => spell_to_draw = "combined.png"
    }
    piece match
      case elephant: Elephant =>
        image_up = "olifant_up.png"
        image_down = "olifant_down.png"
        image_left = "olifant_left.png"
        image_right = "olifant_right.png"

      case rhino: Rhino =>
        image_up = "neushoorn_up.png"
        image_down = "neushoorn_down.png"
        image_left = "neushoorn_left.png"
        image_right = "neushoorn_right.png"

      case mountain: Mountain =>
        image_to_draw = "mountain.png"

      case EmptyPiece =>
        empty = true

      if (!empty) {
        direction match
          case "up" => image_to_draw = image_up
          case "down" => image_to_draw = image_down
          case "left" => image_to_draw = image_left
          case "right" => image_to_draw = image_right
          case "none" => image_to_draw = "mountain.png"

        g.drawImage(AssetsLoader.loadImage(image_to_draw), x, y, size, size, null)
        if spell_to_draw != "none" then g.drawImage(AssetsLoader.loadImage(spell_to_draw), x, y, size/5, size/5, null)
      }