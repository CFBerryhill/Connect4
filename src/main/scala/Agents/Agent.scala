package Agents

import Engine.Gameboard

trait Agent {

  val board : Gameboard
  val id : String

  //if moves.size%2 == 0 before I move, then I'm the first player. else, im the second player

  def make_move(): Unit = {
    board.drop_tile(find_move())
  }

  def find_move() : Int

  /**
   * BEFORE MOVING, this will determine if you are the first player.
   *
   * @return
   */
  def am_first_player() : Boolean = {
    board.moves.size() % 2 == 0
  }

}
