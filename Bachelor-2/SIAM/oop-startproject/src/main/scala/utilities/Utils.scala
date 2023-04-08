package utilities

/** Meegeven code
  * @param rows
  *   het aantal rows in totaal
  * @param columns
  *   het aantal kolommen in totaal
  * @param padding
  *   de gebruike padding (aangenomen dat er een gelijke hoeveelheid padding is
  *   langst alle kanten)
  */
class WindowUtils(
    width: Int,
    height: Int,
    rows: Int,
    columns: Int,
    padding: Int
):
  private val cellWidth: Int = (width - 2*padding) / columns
  private val cellHeight: Int = (height - 2*padding) / rows

  /** Bekijk welke rij en kolom is aangeklikt in het grid.
    *
    * @param x
    *   de x positie op het scherm
    * @param y
    *   de y positie op het scherm
    */
  def determineCell(x: Int, y: Int): (Int, Int) =
    ((x - padding) / cellWidth, (y - padding) / cellHeight)
