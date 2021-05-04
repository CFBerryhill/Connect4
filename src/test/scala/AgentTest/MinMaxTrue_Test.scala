package GameTest

import Agents.MinMax.MinMaxTruePlayer

import collection.mutable.Stack
import org.scalatest._
import _root_.Engine.Gameboard
import flatspec._
import matchers._

class MinMaxTrue_Test extends AnyFlatSpec with should.Matchers {

  val rows = 3
  val cols = 3
  val connect = 3

  val gameboard = new Gameboard(rows,cols,connect)

  it should "explore the tree in DFS" in {
    val player = new MinMaxTruePlayer(gameboard)

    player.min_max()

  }

}