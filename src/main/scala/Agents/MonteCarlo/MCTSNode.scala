package Agents.MonteCarlo

import Agents.MinMax.MinMaxNode
import Engine.Gameboard

import scala.collection.mutable.ListBuffer
import scala.util.Random

class MCTSNode(val board : Gameboard, val move : Option[Int], val parent : Option[MCTSNode]) {

  var wins = 0
  var visits = 0
  val t = 1.0
  var tree_ratio: Double = (wins + t) / (visits + (2*t))

  val epsilon = 0.01

  val game_over : Boolean = board.game_over()

  val depth : Int = {
    var n = this
    var cnt = 0
    while(n.has_parent()) {
      n = n.parent.get
      cnt += 1
    }
    cnt
  }

  var children: ListBuffer[MCTSNode] = new ListBuffer[MCTSNode]
  val validmoves : Array[Int] = board.valid_moves()

  def expand_node() : Unit = {
    for(i<-validmoves) {
      children += new MCTSNode(board.copy_board().drop_tile(i), Some(i), Some(this))
    }
  }

  def number_of_descendents() : Int = {
    var sum = 1
    for(i <- children) {
      sum += i.number_of_descendents()
    }
    sum
  }

  //    replace ratio wins/visits with some function that gives unvisited nodes a score of .5
  //
  //    eg. pick t = 1, set f(w,v) = ( w + t ) / ( v + 2t )
  //
  //    at even depth, pick random node in { argmax( f(w,v) ) }
  //
  //    at odd depth, pick random node in { argmin( f(w,v) ) }
  def tree_policy_child(): Option[MCTSNode] = {
    for(i <- children) {
      i.update_treeval()
    }
    val rand = new Random(System.currentTimeMillis())

    //epsilon greedy
    if(board.game_over() || children.isEmpty) {
      None
    } else if(rand.nextInt(100) <= epsilon*100) {
      //println("exploring")
      Some(children(rand.nextInt(children.length)))
    } else {
      val bleh = arg_minmax(children, depth % 2 == 0)
      Some(bleh(rand.nextInt(bleh.length)))
    }
  }

  def arg_minmax(kinder: ListBuffer[MCTSNode], max : Boolean) : List[MCTSNode] = {
    if(kinder.length <= 1) {
      return kinder.toList
    }

    if(max) {
      val max = {
        var mx = kinder.head
        for (i <- kinder) {
          if (mx.tree_ratio < i.tree_ratio) {
            mx = i
          }
        }
        mx
      }

      val maxes = for {i <- kinder.indices
                       if kinder(i).tree_ratio == max.tree_ratio
                       m = kinder(i)
                       } yield m

      maxes.toList
    } else {
      val min = {
        var mn = kinder.head
        for (i <- kinder) {
          if (mn.tree_ratio > i.tree_ratio) {
            mn = i
          }
        }
        mn
      }

      val mins = for {i <- kinder.indices
                       if kinder(i).tree_ratio == min.tree_ratio
                       m = kinder(i)
                       } yield m

      mins.toList
    }
  }

  //???
  def simulation_policy_child(): MCTSNode =  {
//    simulation policy:   each move in playout is uniform random, over all legal moves
    val rand  = new Random()
    val move = validmoves(rand.nextInt(validmoves.length))
    new MCTSNode(board.copy_board().drop_tile(move), Some(move), Some(this))
  }

  /**
   *  r == 0 game non terminal
   *  r == 1 we won
   *  r == -1 we lost or drew
   */
  def update_stats(result : Int): Unit = {
    visits += 1
    if(result == 1) {
      wins += 1
    }
  }

  def update_treeval() : Unit = {
    tree_ratio = (wins + t) / (visits + (2*t))
  }

  def is_leaf(): Boolean = {
    children.isEmpty || game_over
  }

  def terminal_node() : Boolean = {
    game_over
  }

  def has_parent(): Boolean = {
    parent.isDefined
  }
}
