package siam

import gamelib.AssetsLoader
import java.awt.Graphics2D

/**
  * BUTTONCELL
  * @param button de knop die getekend moet worden
  * @param x horizontale positie van de knop op het scherm
  * @param y verticale positie van de knop op het scherm
  * @param width breedte van de knop
  * @param height hoogte van de knop
  */
class ButtonCell(button: Button, x: Int, y: Int, width: Int, height: Int) extends gamelib.Cell{
  var image = "none"
  override def draw(g: Graphics2D): Unit =
    if (button.visible) { // teken enkel als de knop zichtbaar is
      button match
        case move: MoveButton =>
          image = "movebutton.png"

        case rotate: RotateButton =>
          image = "rotatebutton.png"

        case playerb: PlayerButton =>
          playerb.player.name match
            case "Elephants" => image = "elephant_turn.png"
            case "Rhinos" => image = "rhino_turn.png"
          
        case spells: SpellsButton =>
          image = "spellsbutton.png"

        case limited: LimitedMButton =>
          image = "beperkte_bewegingsvrijheid.png"

        case uncontrolled: UncontrolledRButton =>
          image = "ongecontroleerd_draaien.png"

        case unexpected: UnexpectedDButton =>
          image = "onverwachte_dood.png"

        case combined: CombinedButton =>
          image = "combined.png"

        case add: AddButton =>
          image = "addbutton.png"

        case upButton: UpButton =>
          image = "arrow_up.png"

        case downButton: DownButton =>
          image = "arrow_down.png"

        case leftButton: LeftButton =>
          image = "arrow_left.png"

        case rightButton: RightButton =>
          image = "arrow_right.png"
          
      if image != "none" then g.drawImage(AssetsLoader.loadImage(image), x, y, width, height, null)
    }
}
