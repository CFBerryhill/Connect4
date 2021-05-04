package Agents.MonteCarlo

import Agents.Agent
import Engine.Gameboard

import scala.util.Random

class MCTSPlayer(val board : Gameboard) extends Agent {

  val id = 'm'

  def find_move(): Int = {
    MCTS()
  }

  def MCTS() : Int = {
    val player : Int = {
      if(board.moves.size() % 2 == 0) {
        1
      } else {
        2
      }
    }
//    root_node  = Node(None, None)
    val root = new MCTSNode(board, None, None)
    val rand = new Random(System.currentTimeMillis())
//  while resouces remain:
    //===================CAP===================
    val cap = Math.pow(board.cols,board.connect) //cols^(board*2)
    var rollout_cnt = 0
    while(tree_expandable(root) && rollout_cnt < cap) { //while nodes < Math.pow(board.cols, board.rows || No nodes left to explore
      //n, s = root_node, copy.deepcopy(state)
      var n = root
      val s = root.board.copy_board()

      //while n is not leaf
      while(!n.is_leaf()) {
        n = n.tree_policy_child().get
        s.drop_tile(n.move.get)
      }

      //expand ???
      n.expand_node()
      n.tree_policy_child() match {
        case None =>  //n is a terminal board, s should also be terminal at this point. dont update either
        case Some(c) => n = c
                        s.drop_tile(c.move.get)
      }

      //simulate from deepcopy
      while(!s.game_over()) {
        val validmoves = s.valid_moves()
        val move = validmoves(rand.nextInt(validmoves.length))
        s.drop_tile(move)
      }

      //results
      val result : Int = {
        if(s.winner == player) {
          1
        } else {
          0
        }
      }

      //propegate
      while(n.has_parent()) {
        n.update_stats(result)
        n = n.parent.get
      }
      rollout_cnt += 1
      //println(rollout_cnt)
    }
    //return best move of child
    best_move(root)
  }

  def tree_expandable(node : MCTSNode) : Boolean = {

    //unexpanded node
    if(node.is_leaf() && !node.terminal_node()) {
      return true
    }

    val bools = for{ i<-node.children
      m = tree_expandable(i)
    } yield m

    bools.contains(true) //empty => false
                         //contains true => not fully explored => true
                         //!contains true => no expandable node => false
  }


  def best_move(root : MCTSNode) : Int = {
    //returns best move of given node children
    var max = root.children.head
    for(i <- root.children) {
      i.update_treeval()
      //println("Move: " + i.move.get + " Visits: " + i.visits + " Wins: " + i.wins + " Ratio: " + i.tree_ratio)
      if(i.tree_ratio > max.tree_ratio) {
        max = i
      }
    }
    //println("Selected Move: " + max.move.get)
    max.move.get
  }


}
