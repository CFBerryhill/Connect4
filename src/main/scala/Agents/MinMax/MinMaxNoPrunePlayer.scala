package Agents.MinMax

import Agents.Agent
import Engine.Gameboard
class MinMaxNoPrunePlayer(val board : Gameboard) extends Agent {


  val id ='p'

  //essentially infinity
  val MAXSCORE : Int = Integer.MAX_VALUE / 4

  var wlflag : Boolean = false

  def find_move() : Int = {
    min_max()
  }

  def min_max() : Int = { //min maxes 1 move ahead

    val validmoves = board.valid_moves()

    val player : Char = {
      if(board.moves.size() % 2 == 0) {
        'X'
      } else {
        'O'
      }
    }

    val movevals = for { i <- validmoves.indices
                         brdcpy = board.copy_board()
                         m = min_max_recurse(brdcpy.drop_tile(validmoves(i)), 7, player)
                         } yield m

    validmoves(movevals.indexOf(movevals.max))
  }

  //depth limited minmax
  def min_max_recurse(board: Gameboard, depth : Int, maxplayer: Char): Int = {
//    if depth = 0 or node is a terminal node then
//    return the heuristic value of node

    val maximizingplayer : Boolean = (maxplayer.equals('X') && board.moves.size() % 2 == 0) || maxplayer.equals('O') && board.moves.size() % 2 != 0
    val firstplayer : Boolean = maxplayer.equals('X')
    var rtvalue = 0


    if(depth == 0 || is_terminal_play(board)) {
      return board_value(board, maxplayer)
    }

    //if the player we're maximizing for was the first player ('X'), and there are an even number of moves, this is the maximizing players turn.


    //    if maximizingPlayer then
    //      value := −∞
    //    for each child of node do
    //      value := max(value, minimax(child, depth − 1, FALSE))
    //    return value
    if(maximizingplayer) {
      rtvalue = Int.MaxValue * -1
      val validmoves = board.valid_moves()

      for (i <- validmoves.indices) {
        val child = board.copy_board().drop_tile(validmoves(i))
        rtvalue = math.max(rtvalue, min_max_recurse(child, depth - 1, maxplayer))
      }
      rtvalue
    } else {
      //    else (* minimizing player *)
      //    value := +∞
      //    for each child of node do
      //      value := min(value, minimax(child, depth − 1, TRUE))
      //    return value
      rtvalue = Int.MaxValue
      val validmoves = board.valid_moves()

      for (i <- validmoves.indices) {
        val child = board.copy_board().drop_tile(validmoves(i))
        rtvalue = math.min(rtvalue, min_max_recurse(child, depth - 1, maxplayer))
      }
      rtvalue
    }
  }

  //evaluate board for player
  def board_value(brd : Gameboard, player : Char) : Int = {

    var sum = 0;
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




  def is_terminal_play(brd : Gameboard) : Boolean = {
    brd.game_over()
    brd.winner != 0
  }

}
