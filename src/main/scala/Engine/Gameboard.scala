package Engine

import Agents.Agent

import java.util
import scala.util.control.Breaks.{break, breakable}

class Gameboard(val rows : Int, val cols : Int, val connect : Int) {

  val tokens : Array[Char] = Array('X','O')

  //initialize this as full of 0
  //0 is empty, 1 is agent 1 tile, 2 is agent 2 tile
  val board : Array[Array[Char]] = Array.fill(rows)(Array.fill(cols)('_'))

  //keeps track of moves made on board in (col, agent) for easy replication
  //player 1 is first player, player 2 is second player
  val moves : util.ArrayList[(Int, Char)] = new util.ArrayList[(Int, Char)]()

  var winner = 0 //0 when no winner, -1 when drawn, 1 or 2 depending on players

  def get_connect() : Int = {
    connect
  }

  def get_rows() : Int = {
    rows
  }

  def get_cols() : Int = {
    cols
  }

  /**
   * drops into column 'col' but row 0, then row 1, and so on until column is full.
   * assumes check for valid move is done before entering
   *
   * @param col   what col are we dropping into?
   * @param agent what number player are they?
   * @return
   */
  def drop_tile(col : Int) : Gameboard = {
    var dropped : Boolean = false
    var r = 0
    while(!dropped) {
      if(board(r)(col).equals('_')) {
        board(r)(col) = tokens(moves.size()%2)
        dropped = true
      }
      r+=1
    }
    moves.add((col,tokens(moves.size()%2)))
    this
  }

  /**
   * is column 'col' full? i.e., are all rows filled in column 'col'
   * @param col
   * @return true if col full, false otherwise
   */
  def col_full(col : Int) : Boolean = {
    board(rows-1)(col) != '_'
  }

  def get_col(col : Int) : Array[Char] = {
    val rt = Array.fill(6) {'_'}

    for(i <- board.indices) {
      rt(i) = board(i)(col)
    }

    rt
  }

  /**
   *
   * @param col intended move
   * @return valid move?
   */
  def valid_move(col : Int) : Boolean = {
    !col_full(col)
  }

  def valid_moves() : Array[Int] = {
    val moves = new util.ArrayList[Int]()
    for(i <- board(0).indices) {
      if(valid_move(i)) {
        moves.add(i)
      }
    }

    //to Array[Int]
    val rt : Array[Int] = {
      val a = Array.fill(moves.size){-1}
      for(i<- 0 until moves.size ) {
        a(i) = moves.get(i)
      }
      a
    }

    //return
    rt
  }

  /**
   * checks for full board
   * checks for win condition
   *
   * @return bool game_pver
   */
  def game_over() : Boolean = {
    //check for full board
    if((0 until cols).forall(col_full)){
      winner = -1
      true
    } else {
      //did player 1 win?
      val p1win = for {r <- 0 until rows
                         c <- 0 until cols
                         w = did_winner(r, c, tokens(0))
                         } yield w

      //did player 2 win?
      val p2win = for {r <- 0 until rows
                       c <- 0 until cols
                       w = did_winner(r, c, tokens(1))
                       } yield w

      if(p1win.contains(tokens(0))) {
        winner = 1
        true
      } else if(p2win.contains(tokens(1))) {
        winner = 2
        true
      } else {
        false
      }


    }
  }

  def did_winner(r: Int, c: Int, player: Char) : Char = {
    if(
    //check vertically
    did_win(player, r, c, 1, 0) ||
    //check horizontally
    did_win(player, r, c, 0, 1) ||
    //check diagonally L->R
    did_win(player, r, c, 1, 1) ||
    //check diagonally R->L
    did_win(player, r, c, 1, -1))
    {
      winner = player
      player
    }
    else
    {
      '0' //no one won
    }
  }

  /**
   *
   * @param player which player are we checking for the win? <1,2>
   * @param startRow together with startCol, where do we begin checking?
   * @param startCol together with startRow, where do we begin checking?
   * @param deltR direction of row checking <-1, 0, 1>
   * @param deltC direction of col checking <-1, 0, 1>
   * @return true if player has won
   */
  def did_win(player : Char, startRow: Int, startCol : Int, deltR : Int, deltC : Int) : Boolean = {
      var r = startRow
      var c = startCol
      for (count <- 0 until connect) {
        if (r < rows && r >= 0 && c < cols && c >= 0) {
          val test = board(r)(c)
          if (!test.equals(player)) {
            return false
          }
          r += deltR
          c += deltC
        } else {
          return false
        }
    }
    true
  }

  def reset_board() : Unit = {
    for(r<-0 until rows) {
      for(c<-0 until cols) {
        board(r)(c) = '_'
      }
    }

    moves.clear()

    winner = 0
  }

  def copy_board() : Gameboard = {
    val rt = new Gameboard(rows,cols,connect)
    for(i<- 0 until moves.size) {
      rt.drop_tile(moves.get(i)._1)
    }
    rt
  }

  override def toString: String = {
    var string = ""
    for (r <- board.length - 1 to 0 by -1) {
      for (c <- board(r).indices) {
        string += board(r)(c).toString + "  "
      }
      string += "\n"
    }
    string
  }

  /**
   *
   *
   * @return Number of moves: n
   *         Sequence of moves: [(a,X),(b,O)...]
   *         Final Board:
   */
  def info() : String = {
    "\nNumber of Moves: " + moves.size +
    "\nSequence of Moves: " + moves.toString +
    "\nFinal Board:\n" + toString
  }

  /**
   * print in correct row order but reverse col order
   * for ease of tile dropping
   */
  def print_board() : Unit = {
    print(toString())
  }
}
