package Agents.MinMax

import Agents.Agent
import Engine.Gameboard
import Agents.MinMax

import java.util
import scala.annotation.tailrec
import scala.util.Random
class MinMaxTruePlayer(val board : Gameboard) extends Agent {


  val id ='a'

  //essentially infinity
  val MAX_SCORE : Int = Integer.MAX_VALUE / 4

  val MAX_DEPTH : Int = board.connect*2

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

    val root : Gameboard = board.copy_board()
    val validmoves = board.valid_moves()

    var depth = 1
    val movevals = Array.fill(validmoves.length)(0)

    //need a better iterative deepening end decision

    while(depth < MAX_DEPTH && !finished(movevals)){

      for (i <- validmoves.indices) {
        val child = root.copy_board().drop_tile(validmoves(i))
        movevals(i) = min_max_recurse(child, depth, Int.MinValue, Int.MaxValue, player)
      }

      depth += 1
    }

    val m = max_moves(movevals)
    val rand = new Random()

    validmoves(m(rand.nextInt(m.length)))
  }

  //depth limited minmax
  def min_max_recurse(board : Gameboard, depth : Int, alpha : Int, beta : Int, maxplayer: Char): Int = {
    //    if depth = 0 or node is a terminal node then
    //    return the heuristic value of node

    val maximizingplayer : Boolean = (maxplayer.equals('X') && board.moves.size() % 2 == 0) || maxplayer.equals('O') && board.moves.size() % 2 != 0

    if(depth == 0 || is_terminal_play(board)) {
      return board_value(board, maxplayer)
    }

    //if the player we're maximizing for was the first player ('X'), and there are an even number of moves, this is the maximizing players turn.

    if(maximizingplayer) {
      @tailrec
      def max_loop(rt : Int, alpha : Int, children : List[Gameboard]): Int = {
        children match {
          case Nil => rt
          case c::rest => val new_rt = math.max(rt, min_max_recurse(c, depth-1, alpha, beta, maxplayer))
                          val new_alpha = math.max(new_rt, alpha)
                          if(new_alpha >= beta) {
                            new_rt
                          } else {
                            max_loop(new_rt, new_alpha, rest)
                          }
        }
      }
      max_loop(Int.MaxValue  * -1, alpha, make_children(board).toList)
    } else {
      @tailrec
      def min_loop(rt : Int, beta : Int, children : List[Gameboard]): Int = {
        children match {
          case Nil => rt
          case c::rest => val new_rt = math.min(rt, min_max_recurse(c, depth-1, alpha, beta, maxplayer))
                          val new_beta = math.min(new_rt, beta)
                          if(new_beta <= alpha) {
                            new_rt
                          } else {
                            min_loop(new_rt, new_beta, rest)
                          }
        }
      }

      min_loop(Int.MaxValue, beta, make_children(board).toList)
    }
  }

  /**
   *
   * need to do MATH to improve this
   *
   * @param movevals
   * @return
   */
  def finished(movevals : Array[Int]) : Boolean = {

    if(movevals.length <= 1) {
      return true
    }

    val sorted = movevals.sortWith(_ > _)

    val max1 = sorted(0)
    val max2 = sorted(1)

    val delta = math.abs(max1 - max2)

    delta >= MAX_SCORE * 0.8
  }

  def max_moves(movevals : Array[Int]) : Array[Int] = {

    if(movevals.length <= 1) {
      return movevals
    }

    val max = movevals.max

    val maxes = for{i <- movevals.indices
                                  if movevals(i) == max
                                    m = i
                                } yield m

    maxes.toArray
  }

  def make_children(board : Gameboard): Array[Gameboard] = {
    val validmoves = board.valid_moves()
    val result = for {i <- validmoves.indices
                      c = board.copy_board().drop_tile(validmoves(i))
                      } yield c

    result.toArray
  }

  //evaluate board for player
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
      MAX_SCORE
    } else if(encnt == gboard.get_connect()) { //enemy win
      wlflag = true
      -1 * MAX_SCORE
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
