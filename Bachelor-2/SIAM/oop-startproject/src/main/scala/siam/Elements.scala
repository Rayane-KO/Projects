package siam
import gamelib.*
import utilities.*

import java.awt.Graphics2D
import scala.jdk.CollectionConverters.*

// grootte van Siam Game
val rows = 5
val cols = 5
val actual_width = 100 * rows
val actual_height = 100 * cols
val width = actual_width + 200
val height = actual_height + 200
val padding = 100

// veel gebruikte waarde
val invalid = -1

// scherm
val window = new GUI(width, height, rows, cols, padding)
val panel = window.getGridPanel
val utils = new WindowUtils(width, height, rows, cols, padding)

// het bord
val grid = SiamGrid[Piece](rows, cols, EmptyPiece)

var listButton: List[gamelib.Cell] = List()

// spreuken
val limitedMovement = new LimitedMovement("LimitedMovement")
val uncontrolledRotation = new UncontrolledRotation("UncontrolledRotation")
val suddenDeath = new SuddenDeath("SuddenDeath")
val combinedSpell = new Combined("CombinedSpell")
val spells = List(limitedMovement, uncontrolledRotation, suddenDeath, combinedSpell)

// bergen
var mountain1 = new Mountain(2, 1)
var mountain2 = new Mountain(2, 2)
var mountain3 = new Mountain(2, 3)

// spelers 
val Player1 = new Player("Elephants", grid)
val Player2 = new Player("Rhinos", grid)
var currentPlayer = Player2 // rhinos beginnen eerst

// bergen en knoppen toevoegen zodat ze zichtbaar zijn op het scherm
def setup(): Unit =
  grid.add(mountain1.row, mountain1.col, mountain1)
  grid.add(mountain2.row, mountain2.col, mountain2)
  grid.add(mountain3.row, mountain3.col, mountain3)
  listButton = buttonCells ::: listButton
  Player1.access(Player2)
  Player2.access(Player1)

// grootte van de knoppen
val buttonWidth = 100
val buttonHeight = 50
val squareButton = 70
// knoppen voor toevoegen, bewegen, draaien en spreuken plaatsen
val mButton = new MoveButton(230, 610, buttonWidth, buttonHeight)
val rButton = new RotateButton(360, 610, buttonWidth, buttonHeight)
val aButton = new AddButton(100, 610, buttonWidth, buttonHeight)
val spButton = new SpellsButton(490, 610, buttonWidth, buttonHeight)
val listActions = List(aButton, mButton, rButton, spButton)
// knoppen voor de richtingen
val dButton = new DownButton(width / 2 - 30, height - squareButton, squareButton, squareButton)
val uButton = new UpButton(width / 2 - 30, 0, squareButton, squareButton)
val lButton = new LeftButton(0, height / 2, squareButton, squareButton)
val riButton = new RightButton(width - 90, height / 2, squareButton, squareButton)
// huidige speler
var pButton = new PlayerButton(width - buttonWidth, 0, buttonWidth, squareButton, currentPlayer)
// knoppen voor de spreuken
val limButton = new LimitedMButton(100, 0, squareButton, squareButton)
val uncButton = new UncontrolledRButton(230, 0, squareButton, squareButton)
val unexButton = new UnexpectedDButton(360, 0, squareButton, squareButton)
val combButton = new CombinedButton(490, 0, squareButton, squareButton)

val buttons = List(mButton, rButton, aButton, dButton, uButton, lButton, riButton, pButton, spButton, limButton, uncButton, unexButton, combButton)
val buttonCells = buttons.map(button => ButtonCell(button, button.x, button.y, button.width, button.height))


// booleans om te weten welke actie we willen ondernemen:
// 1. een piece toevoegen
// 2. een piece bewegen
// 3. een piece draaien
// 4. een spreuk op een piece plaatsen
var add = false
var move = false
var rotate = false
var spell = false
