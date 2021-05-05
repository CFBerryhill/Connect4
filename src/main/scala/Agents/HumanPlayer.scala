package Agents

import Engine.Gameboard
import scala.io.StdIn.readLine

class HumanPlayer(val board : Gameboard, val id : String) extends Agent{

  /**
   *
   * @return col for move, keep trying until valid move given
   */
  override def find_move(): Int = {
    //board.print_board()
    println("Where would you like to drop your tile? (0-" + (board.cols-1) + ")")
    val col =  {
      var c = readLine().toInt
      while(!(c >= 0 && c <= board.cols-1 && !board.col_full(c))) {
        println("move not valid, please try again")
        c = readLine().toInt
      }
      c
    }
    col
  }
}
