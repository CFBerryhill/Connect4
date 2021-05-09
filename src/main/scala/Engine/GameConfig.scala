package Engine

import Agents.Agent

case class GameConfig(val board : Gameboard, val player1 : Agent, val player2: Agent, trial_num: Int) {

  val board_drctry : String = "/" + board.rows + "x" + board.cols
  val connect_drctry : String = "/" + board.connect
  val player_drctry : String = "/" + player1.id + "_vs_" + player2.id

  val full_drctry : String = board_drctry + connect_drctry + player_drctry

  val filename : String = full_drctry + "/" + trial_num

  def get_players() : Array[Agent] = {
    Array(player1,player2)
  }

  def get_winner(): String = {
    board.game_over()
    board.winner match {
      case 1 => "PLAYER 1"
      case 2 => "PLAYER 2"
      case -1 => "Draw"
      case _ => "Not over"
    }
  }

  def get_game_info() : String = {
    "Players: " + player1.id + "," + player2.id +
    "\nFirst player: " + player1.id +
    "\nWinner:" + get_winner() +
    board.info()
  }
}
