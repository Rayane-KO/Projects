package siam

import scala.reflect.ClassTag

/**
  * SIAMGRID
  *
  * @param rows aantal rijen van het bord
  * @param cols aantal kolommen van het bord
  * stelt het spelbord voor
  */
class SiamGrid[T: ClassTag](rows: Int, cols: Int, nullValue: T):
  val matrix: Array[Array[T]] = Array.fill(rows, cols)(nullValue)

  def getRows: Int = rows
  def getCols: Int = cols

  def add(row: Int, col: Int, el: T): Unit =
    matrix(row)(col) = el

  def delete(row: Int, col: Int): Unit =
    matrix(row)(col) = nullValue

  def get(row: Int, col: Int): T =
    matrix(row)(col)

  // Een methode om de cellen te overlopen
  def forEach(f: (Int, Int, T) => Unit): Unit =
    for i <- 0 until rows do for j <- 0 until cols do f(i, j, matrix(i)(j))

/*object Test:
  def main(args: Array[String]): Unit = {
    val grid = new SiamGrid(5, 5)
    val elephant = new Elephant(1, 2)
    grid.add(elephant.row, elephant.col, elephant)
    for (row <- grid.matrix){
      for(element <- row){
        print(s"$element ")
      }
      println()
    }
  }
*/