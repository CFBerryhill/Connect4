package Agents.MinMax

import Agents.Agent
import Engine.Gameboard

class MinMaxSimplePlayer(val board : Gameboard) extends Agent {

  val id ="Simple"

  //essentially infinity
  val MAXSCORE : Int = Integer.MAX_VALUE /4;

  var wlflag : Boolean = false

  def find_move() : Int = {
    min_max()
  }

  def min_max() : Int = { //min maxes 1 move ahead

    val player : Char = {
      if(board.moves.size() % 2 == 0) {
        'X'
      } else {
        'O'
      }
    }

    val validmoves = board.valid_moves()
    val movevals = for { i <- validmoves.indices
                         brdcpy = board.copy_board()
                         m = board_value(brdcpy.drop_tile(validmoves(i)), player)
                         } yield m

    validmoves(movevals.indexOf(movevals.max))
  }

  def board_value(brd : Gameboard, player : Char) : Int = {
    var sum = 0
    for(r <- 0 until brd.get_rows()) {
      for(c <- 0 until brd.get_cols()) {
        if(wlflag) {
          wlflag = false
          return sum
        }
        sum += connect_sum(brd, player, r,c,1,0)
        sum += connect_sum(brd, player, r,c,0,1)
        sum += connect_sum(brd, player, r,c,1,1)
        sum += connect_sum(brd, player, r,c,1,-1)
      }
    }
    sum
  }

  /**
   *
   * assumes being called before
   *
   * @param gboard what board are we doing a sum on?
   * @param player which player are we checking for the win? <'O', 'X'>
   * @param startRow together with startCol, where do we begin checking?
   * @param startCol together with startRow, where do we begin checking?
   * @param deltR direction of row checking <-1, 0, 1>
   * @param deltC direction of col checking <-1, 0, 1>
   * @return true if player has won
   */
  def connect_sum(gboard : Gameboard, player : Char, startRow: Int, startCol : Int, deltR : Int, deltC : Int) : Int = {
    var r = startRow
    var c = startCol
    var cnt = 0
    var encnt = 0
    for (count <- 0 until gboard.get_connect()) {
      if (r < gboard.get_rows() && r >= 0 && c < gboard.get_cols() && c >= 0) {
        val test = gboard.board(r)(c)
        //either our player (+1), empty space (+0), or blocked (return 0)
        if(cnt != 0 && encnt!= 0) { //blocked short circuit
          return 0
        } else if (test.equals(player)) {
          cnt += 1
        } else if(test.equals('_')) {
          //nothing happens
        } else { // enemy tile
          encnt += 1
        }
        r += deltR
        c += deltC
      } else { //out of bounds
        return 0
      }
    }
    //infinite 0 or n^2
    if(encnt != 0 && cnt != 0) { //blocked
      0
    } else if(cnt == gboard.get_connect()) { //friend win
      wlflag  = true
      MAXSCORE
    } else if(encnt == gboard.get_connect()) { //enemy win
      wlflag = true
      -1 * MAXSCORE
    } else if (cnt == 0 && encnt != 0) { //enemy block
      -1 * encnt * encnt
    } else if(encnt == 0 && cnt != 0) { //friend block
      cnt*cnt
    } else {
      0
    }
  }



  def is_terminal_play(brd : Gameboard, col : Int) : Boolean = {
    brd.drop_tile(col)
    if(brd.winner == 1 || brd.winner == 2) {
      true
    }

    false
  }

}
