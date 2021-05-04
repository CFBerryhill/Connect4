//package GameTest
//
//import collection.mutable.Stack
//import org.scalatest._
//import _root_.Engine.Gameboard
//import flatspec._
//import matchers._
//
//class Gameboard_Test extends AnyFlatSpec with should.Matchers {
//
//  val rows = 6
//  val cols = 7
//  val connect = 4
//
//  it should "return columns correctly" in {
//    val board = new Gameboard(rows,cols, connect)
//    assert(board.get_col(0).length == rows)
//  }
//
//  it should "drop tiles in all columns" in {
//    val board = new Gameboard(rows,cols, connect)
//    for(i <- 0 until cols) {
//      board.drop_tile(i)
//      assert(board.get_col(i)(0) == '1')
//    }
//  }
//
//  it should "see vertical game win" in {
//    val board = new Gameboard(rows,cols, connect)
//
//    //for all cols
//    for(c<-0 to 6) {
//      //drop vertically 3 times, no win
//      for (i <- 0 to 2) {
//        board.drop_tile(c)
//        board.game_over()
//        assert(board.winner == '0') //<--early win?
//      }
//      //drop vertically 1 more time, win
//      board.drop_tile(c)
//      board.game_over()
//      assert(board.winner == 1)
//      //clear board
//      board.reset_board()
//    }
//  }
//
//  it should "see horizontal game win" in {
//    val board = new Gameboard(rows,cols, connect)
//
//    //drop horizontally 3 times, no win
//    for (i <- 0 to 2) {
//      board.drop_tile(i)
//      board.game_over()
//      assert(board.winner == 0) //<--early win?
//      board.drop_tile(connect-i)
//    }
//    //drop vertically 1 more time, win
//    board.drop_tile(3)
//    board.game_over()
//    assert(board.winner == 1)
//    //clear board
//    board.reset_board()
//
//  }
//
////  /**
////   * 0 0 0 1
////   * 0 0 1 2
////   * 0 1 2 2
////   * 1 2 2 2
////   */
////  it should "see R->L diagonal game win" in {
////    val board = new Gameboard(rows,cols, connect)
////
////    for(i<-0 until connect) {
////      for(count <- 0 until i) {
////        board.drop_tile(i,'2')
////      }
////      board.drop_tile(i,'1')
////    }
////    assert(board.game_end('1','2') == '1')
////  }
////
////  /**
////   * 1 0 0 0
////   * 2 1 0 0
////   * 2 2 1 0
////   * 2 2 2 1
////   */
////  it should "see L->R diagonal game win" in {
////    val board = new Gameboard(rows,cols, connect)
////    for(i <- connect-1 to 0 by -1) {
////      for(count <- 0 until i) {
////        board.drop_tile(count,'2')
////      }
////    }
////    for(count <- 0 until 4) {
////      board.drop_tile(count,'1')
////    }
////    assert(board.game_end('1','2') == '1')
////  }
//
////  it should "see the draw" in {
////    val board = new Gameboard(rows,cols, connect)
////    var cnt = '0'
////    for(r<-0 until rows) {
////      for(c<-0 until cols) {
////        board.drop_tile(c, )
////        cnt = cnt + 1
////      }
////    }
////    assert(board.game_end() == -1)
////  }
//
//
//}