package siam
import gamelib.*
import utilities.*
import java.awt.Graphics2D
import scala.jdk.CollectionConverters.*
/**
  * SIAM GAME
  * @author Rayane Kouidane 0587073
  * HOW TO PLAY:
  *   - als het jouw beurt is dan kan je een actie kiezen met de knoppen add, move, rotate en spells
  *   - add zal een dier toevoegen, move zal een dier bewegen, rotate zal een dier draaien en spells is om spreuken te gebruiken op dieren
  *   - eerst klik je op een actie, dan klik je op welk dier je een actie wilt toepassen en als laatst klik je op een richting of spreuk
  */
  object SiamGame:
    @main def run(): Unit =
      setup()

      def checkWinner(): String =
        if (Player1.winner != "NONE") Player1.winner
        else if (Player2.winner != "NONE") Player2.winner
        else "NONE"

      // flip de visible state van alle richtingsknoppen
      def directionsState(): Unit =
        dButton.flipState()
        uButton.flipState()
        riButton.flipState()
        lButton.flipState()

      // flip de visible state van alle spreukknoppen
      def spellsState(): Unit =
        uncButton.flipState()
        unexButton.flipState()
        limButton.flipState()
        combButton.flipState()

      def flipButtonExcept(pos: Int): Unit =
        for(index <- 0 until 4){
          if index != pos then listActions(index).flipState()
        }

      // verander van speler
      def flip(): Unit =
        if currentPlayer == Player1 then currentPlayer = Player2 else currentPlayer = Player1
        pButton.player = currentPlayer
        currentPlayer.checkSpells()

      // hierin worden de clicks onthouden
      var frstX = invalid
      var frstY = invalid
      var scndX = invalid
      var scndY = invalid
      var thrdX = invalid
      var thrdY = invalid

      // kijkt welke richting de speler heeft gekozen en draait de piece
      def checkRotation(row: Int, col: Int): Unit =
        if (lButton.checkClick(thrdX, thrdY)) currentPlayer.rotatePiece(row, col, "left")
        else if (riButton.checkClick(thrdX, thrdY)) currentPlayer.rotatePiece(row, col, "right")
        else if (uButton.checkClick(thrdX, thrdY)) currentPlayer.rotatePiece(row, col, "up")
        else if (dButton.checkClick(thrdX, thrdY)) currentPlayer.rotatePiece(row, col, "down")

      var direction = "none"
      def checkDirection(x: Int, y: Int): Unit =
        direction = "none"
        if lButton.checkClick(x, y) then direction = "left"
        else if riButton.checkClick(x, y) then direction = "right"
        else if uButton.checkClick(x, y) then direction = "up"
        else if dButton.checkClick(x, y) then direction = "down"

      // controleert of een bepaalde speler een piece op positie row, col mag gebruiken
      def checkYours(row: Int, col: Int, player: Player): Boolean =
        var p1 = "Elephants"
        var p2 = "Rhinos"
        if (spell){
          p1 = p2
          p2 = "Elephants"
        }
        (grid.get(row, col).getClass == Elephant(invalid, invalid, null).getClass && player.name == p1) || (grid.get(row, col).getClass == Rhino(invalid, invalid, null).getClass && player.name == p2)

      def chooseFunc(function: Int): Unit =
        add = false
        move = false
        rotate = false
        spell = false
        function match
          case 0 => add = true
          case 1 => move = true
          case 2 => rotate = true
          case 3 => spell = true

        flipButtonExcept(function)
        if function == 3 then spellsState() else directionsState()

      def drawGrid(): Unit =
        GridDrawer.draw(grid, panel, (width - 200) / cols, (height - 200) / rows, 100)

      directionsState()
      spellsState()
      drawGrid()
      panel.setPressListener((x: Int, y: Int) => {
        if (checkWinner() == "NONE") {
          if aButton.checkClick(x, y) then chooseFunc(0)
          else if mButton.checkClick(x, y) then chooseFunc(1)
          else if rButton.checkClick(x, y) then chooseFunc(2)
          else if spButton.checkClick(x, y) then chooseFunc(3)

          if (add || move || rotate || spell) {
            if (frstX == invalid && frstY == invalid) {
              frstX = x
              frstY = y
            } else if (scndX == invalid && scndY == invalid) {
              scndX = x
              scndY = y
            } else if (thrdX == invalid && thrdY == invalid) {
              thrdX = x
              thrdY = y
            }
            if (thrdX != invalid && thrdY != invalid && scndX != invalid && scndY != invalid) {
              val (col, row) = utils.determineCell(scndX, scndY)
              if (row < rows && row >= 0 && col < cols && col >= 0) {
                if (add) {
                  if (currentPlayer.canAdd) {
                    if (row == 0 || col == 0 || row == rows - 1 || col == cols - 1) {
                      if (grid.get(row, col) == EmptyPiece) { // piece op een lege plaats zetten
                        currentPlayer.placePiece(row, col)
                        checkRotation(row, col)
                        flip()
                      } else { // piece plaatsen en andere pieces duwen
                        checkDirection(thrdX, thrdY)
                        if (direction != "none") {
                        val check = currentPlayer.checkPush(row, col, direction, true)
                        if (check) {
                            currentPlayer.pushPiece(row, col, direction, true)
                            currentPlayer.placePiece(row, col)
                            currentPlayer.rotatePiece(row, col, direction)
                            flip()
                          }
                        }
                      }
                      add = false
                      flipButtonExcept(0)
                      directionsState()
                    }
                  }
                }
                else if (move) {
                  if (checkYours(row, col, currentPlayer)) {
                    if (grid.get(row, col) != EmptyPiece && grid.get(row, col).movementCounter > 0) {
                      checkDirection(thrdX, thrdY)
                      if (direction != "none"){
                        val check = currentPlayer.checkPush(row, col, direction, false)
                        if (check) {
                          currentPlayer.pushPiece(row, col, direction, false)
                          flip()
                        }
                      }
                    }
                    move = false
                    flipButtonExcept(1)
                    directionsState()
                  }
                }
                else if (rotate) {
                  if (grid.get(row, col) != EmptyPiece && checkYours(row, col, currentPlayer)) {
                    checkRotation(row, col)
                    flip()
                  }
                  rotate = false
                  flipButtonExcept(2)
                  directionsState()
                }
                else if (spell) {
                  var spellPos = invalid
                  if (grid.get(row, col) != EmptyPiece) { // spreuk kan enkel uitgevoerd worden op tegengestelde speler
                    if (checkYours(row, col, currentPlayer)) {
                      if limButton.checkClick(thrdX, thrdY) then spellPos = 0
                      else if unexButton.checkClick(thrdX, thrdY) then spellPos = 2
                      else if uncButton.checkClick(thrdX, thrdY) then spellPos = 1
                      else if combButton.checkClick(thrdX, thrdY) then spellPos = 3
                      if (spellPos != invalid) {
                        currentPlayer.useSpell(spells(spellPos), row, col)
                        flip()
                      }
                    }
                  }
                    flipButtonExcept(3)
                    spellsState()
                    spell = false
                  }
              }
            }
          }
          if (frstX != invalid && frstY != invalid && scndX != invalid && scndY != invalid && thrdX != invalid && thrdY != invalid) {
            frstX = invalid
            frstY = invalid
            scndX = invalid
            scndY = invalid
            thrdX = invalid
            thrdY = invalid
          }
          drawGrid()
        }
        else {
          drawGrid()
          println(s"WINNER IS PLAYER: ${checkWinner()}")
        }
      })
      while true do Thread.sleep(0)