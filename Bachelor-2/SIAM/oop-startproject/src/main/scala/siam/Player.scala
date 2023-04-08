package siam
/**
  * PLAYER
  * @param name afhankelijk van zijn naam krijgt de speler andere pieces
  * @param grid stelt het bord voor waarop we spelen
  */
class Player(val name: String, val grid: SiamGrid[Piece]) {
  private var availableSpells = List(new UncontrolledRotation("UncontrolledRotation"), new LimitedMovement("LimitedMovement"), new SuddenDeath("SuddenDeath"), new Combined("CombinedSpell"))
  private val elephants: List[Piece] = List.fill(5)(new Elephant(invalid,invalid, this))
  private val rhinos: List[Piece] = List.fill(5)(new Rhino(invalid,invalid, this))
  var winner = "NONE"
  private var enemy: Player = _ // zal enkel dienen om de spreuken van de andere speler te kunnen wijzigen

  private var pieces: List[Piece] = // als de speler 'elephants' is dan krijgt hij/zij olifanten en andersom
    if name == "Elephants" then elephants else rhinos

  def canAdd: Boolean =
    pieces.nonEmpty

  def getPiece: Piece =
    pieces.head

  def access(player: Player): Unit =
    enemy = player

  def placePiece(row: Int, col: Int): Unit =
    if (canAdd) {
      val piece = getPiece
      if (pieces.contains(piece)) {
        piece.row = row
        piece.col = col
        grid.add(row, col, piece)
        pieces = pieces.filter(_ != piece) // remove the piece from the list of available pieces
      }
    }
    else println(s"You have no more pieces: $pieces")

  def rotatePiece(row: Int, col: Int, newDir: String): Unit =
    grid.get(row, col).rotate(newDir)

  private def opposite(direction: String): String =
    direction match
      case "up" => "down"
      case "down" => "up"
      case "left" => "right"
      case "right" => "left"

  def checkSpellOwner(piece: Piece): Unit =
    piece.underSpell.init()
    if piece.owner == this then enemy.availableSpells = piece.underSpell :: enemy.availableSpells
    else availableSpells = piece.underSpell :: availableSpells
    piece.removeSpell()

  // zal door de matrix lopen en de staat van cursed pieces updaten
  def checkSpells(): Unit =
    grid.forEach((row, col, piece) =>
      if (piece.underSpell != null) {
        piece.underSpell.execute(piece)
      }
      if (piece.dead) {
        checkSpellOwner(piece)
        piece.revive()
        piece.owner.pieces = piece :: piece.owner.pieces // geef de piece terug aan zijn owner
        grid.delete(row, col)
      })

    // methode om een piece te bewegen van een positie (row, col)
    // naar een andere positie (newRow, newCol) op het bord
  def gridMove(row: Int, col: Int, newRow: Int, newCol: Int): Unit =
    if (grid.get(newRow, newCol) == EmptyPiece) {
      grid.add(newRow, newCol, grid.get(row, col))
      grid.get(newRow, newCol).move(newRow, newCol)
      grid.delete(row, col)
    }


// movePiece zal pieces op ons bord laten bewegen,
// het controleert ook of er een berg van het bord werd geduwd of niet
  def movePiece(row: Int, col: Int, direction: String): Unit =
    var fall = false
    var mFall = false
    var winnerRow = row
    var winnerCol = col
    val piece = grid.get(row, col)
    piece match
      case mountain: Mountain =>
        direction match {
          case "up" => if row - 1 >= 0 then gridMove(row, col, row - 1, col) else {
            mFall = true
            winnerRow = row + 1
          }
          case "down" => if row + 1 < grid.getRows then gridMove(row, col, row + 1, col) else {
            mFall = true
            winnerRow = row - 1
          }
          case "right" => if col+1 < grid.getCols then gridMove(row, col, row, col+1) else {
            mFall = true
            winnerCol = col - 1
          }
          case "left" => if col - 1 >= 0 then gridMove(row, col, row - 1, col-1) else {
            mFall = true
            winnerCol = col + 1
          }
        }
        if (mFall) {
          grid.delete(row, col)
          grid.get(winnerRow, winnerCol) match
            case elephant: Elephant => winner = "ELEPHANTS"
            case rhino: Rhino => winner = "RHINOS"
        }
      case _ =>
        direction match {
          case "up" => if row - 1 >= 0 then gridMove(row, col, row - 1, col) else fall = true
          case "down" => if row + 1 < grid.getRows then gridMove(row, col, row + 1, col) else fall = true
          case "left" => if col - 1 >= 0 then gridMove(row, col, row, col - 1) else fall = true
          case "right" => if col + 1 < grid.getCols then gridMove(row, col, row, col + 1) else fall = true
        }
    if (fall) {
      val piece = grid.get(row, col)
      if (piece.underSpell != null){
        checkSpellOwner(piece)
      }
      piece.owner.pieces = piece :: piece.owner.pieces
      grid.delete(row, col)
    }

  private var checkRow = invalid
  private var checkCol = invalid
  // checkPush controleert of het mogelijk is om andere pieces te duwen
  // afhankelijk van hoeveel dieren in dezelfde en tegengestelde richting wijzen en ook
  // hoeveel bergen er op de weg staan
  def checkPush(row: Int, col: Int, direction: String, mType: Boolean): Boolean =
    var currentRow = row
    var currentCol = col
    val currentDir = direction
    var dCounter = 0
    var odCounter = 0
    var mCounter = 0
    while (currentRow < grid.getRows && currentRow >= 0 && currentCol < grid.getCols && currentCol >= 0 && grid.get(currentRow, currentCol) != EmptyPiece && !(dCounter == 1 && odCounter == 1)) {
      // dCounter == 1 && odCounter == 1: wanneer je de situatie hebt dat er na de pijl die je wilt bewegen een tegengestelde pijl is. bv: /->\ <- -> je kan niet duwen
      checkRow = currentRow
      checkCol = currentCol
      val pieceDirection = grid.get(currentRow, currentCol).direction
      if pieceDirection == opposite(currentDir) then odCounter += 1
      else if pieceDirection == currentDir then dCounter += 1
      else if pieceDirection == "none" then mCounter += 1
      direction match
        case "up" => currentRow -= 1

        case "down" => currentRow += 1

        case "left" => currentCol -= 1

        case "right" => currentCol += 1
    }
    if mType then dCounter += 1 // als we een dier toevoegen dan moeten we die ook bijtellen (mType: movementType => add of move)
    if (odCounter == 1 || odCounter == 0) && dCounter == 0 then true // als we een dier willen bewegen in tegengestelde richting
    else if (mCounter != 0) {
      if dCounter >= odCounter + mCounter then true else false
    }
    else if dCounter > odCounter then true else false // als er geen bergen dan zijn dan check je gewoon of er genoeg dieren zijn om te duwen

  // pushPiece zal met checkPush kijken of we pieces kunnen duwen en
  // als het mogelijk is zal het de pieces duwen
  def pushPiece(row: Int, col: Int, direction: String, mType: Boolean): Unit =
    if (checkPush(row, col, direction, mType)) {
      direction match
        case "up" =>
          for (i <- checkRow to row) {
            movePiece(i, col, "up")
          }
        case "down" =>
          for (i <- checkRow to row by -1) {
            movePiece(i, col, "down")
          }
        case "left" =>
          for (j <- checkCol to col) {
            movePiece(row, j, "left")
          }
        case "right" =>
          for (j <- checkCol to col by -1) {
            movePiece(row, j, "right")
          }
    }
    checkRow = invalid
    checkCol = invalid

  def useSpell(spell: Spell, pieceRow: Int, pieceCol: Int): Unit =
    if (availableSpells.contains(spell)){
      val piece = grid.get(pieceRow, pieceCol)
      spell.apply(piece)
      availableSpells = availableSpells.filter(_ != spell) // verwijder de spreuk van de lijst van beschikbare spreuken
    }
    else println(s"Spell: $spell is not available")
}


/*
object Test:
  def main(args: Array[String]): Unit = {
    var elephant = new Elephant(0, 0)
    var rhino = new Rhino(0, 0)
    val grid = new SiamGrid(5, 5)
    var els = List(elephant, rhino)
    val Player1 = new Player(els, grid)
    Player1.placePiece(1, 1)
    grid.displayGrid()
    println()
    Player1.placePiece(2, 2)
    grid.displayGrid()
    println()
    Player1.movePiece(1, 1, -1, 0)
    grid.displayGrid()
    println()
    println(els)
  }
*/