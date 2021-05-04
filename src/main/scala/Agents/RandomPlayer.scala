package Agents

import Engine.Gameboard

import scala.util.Random

class RandomPlayer(val board : Gameboard) extends Agent{

  val id = 'r'

  def find_move() : Int = {

    val rand = new Random()

    val moves = board.valid_moves()

    //random valid move
    moves(rand.nextInt(moves.length))
  }

}
