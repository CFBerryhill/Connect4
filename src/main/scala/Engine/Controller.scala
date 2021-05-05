package Engine

import Agents.MinMax.{MinMaxSimplePlayer, MinMaxTruePlayer}
import Agents.MonteCarlo.MCTSPlayer
import Agents.{Agent, HumanPlayer, RandomPlayer}

import java.io.{File, FileWriter}
import java.util
import scala.annotation.tailrec
import scala.io.Source._
import scala.io.StdIn.readLine

object Controller extends App {

  val stand : Gameboard = new Gameboard(6,7,4)

  val agents = Array("m", "s", "r", "a")

  def main(): Unit = {

    val games = loop()

      for(i <- games.indices) {

        val gconfig = games(i)

        gconfig.board.reset_board()

        val players: Array[Agent] = {
          gconfig.get_players()
        }

        var cnt = 0

        val output = new File("results/" + gconfig.full_drctry)
        if (!output.exists()) {
          output.mkdirs()
        }
        val fileWriter = new FileWriter(new File("results/" + gconfig.filename + ".txt"))

        println("Game starting, player " + gconfig.player1.id + " going first vs " + gconfig.player2.id + " trial #" +gconfig.trial_num)
        while (!gconfig.board.game_over()) {
          //fileWriter.write(gconfig.board.toString + "\n")
          gconfig.board.print_board()
          players(cnt % 2).make_move()
          cnt += 1
          println("MOVE MADE")
        }
        println("Player " + gconfig.board.winner + " Won!")
        println(gconfig.get_game_info())

        new java.io.File("results/" + gconfig.full_drctry).mkdirs




        fileWriter.write(gconfig.get_game_info())
        fileWriter.close()
      }
  }

  @tailrec
  def loop() : Array[GameConfig] = {
    println("Welcome to Casey's Connect 4! " +
      "Would you like to play against a human, an artifical agent, run tests, or battle 2 agents? (0, 1, 2, 3)")

    val mode = readLine()

    if (mode.equals("0")) {
      val p1 : Agent = new HumanPlayer(stand, 'X')
      val p2 : Agent = new HumanPlayer(stand, '0')
      Array(GameConfig(stand, p1, p2,0))
    } else if (mode.equals("1")) {
      val p1 : Agent = new HumanPlayer(stand, 'h')
      val p2 : Agent = get_player_from_controller(stand)
      Array(GameConfig(stand,p1,p2,0))
    } else if (mode.equals("2")) {
      parse_test_config()
    } else if(mode.equals("3")) {
      val board : Gameboard = get_board_from_player()
      val p1 : Agent = get_player_from_controller(board)
      val p2 : Agent = get_player_from_controller(board)
      Array(GameConfig(board,p1,p2,0))
    }else {
      println("Invalid, please try again")
      loop()
    }
  }

  def get_player_from_controller(board : Gameboard): Agent = {
    println("Which Agent? : Random (0) : MinMaxSimple (1) : MinMaxTruePlayer (2) : MonteCarloPlayer (3)")
    val i = readLine().toInt
    if(i == 0) {
      println("random player selected")
      new RandomPlayer(board)
    } else if (i == 1) {
      println("minmaxsimple selected")
      new MinMaxSimplePlayer(board)
    } else if (i == 2) {
      println("minmaxtrueplayer selected")
      new MinMaxTruePlayer(board)
    } else if (i == 3) {
      println("montecarloplayer selected")
      new MCTSPlayer(board)
    } else {
      println("Invalid, please try again")
      get_player_from_controller(board)
    }
  }

  @tailrec
  def get_board_from_player(): Gameboard = {
    try {
      println("What size board? (RxC)")
      val b = readLine().split('x')
      val dims = for {i <- b
                      d = i.toInt
                      } yield d

      println("Connect Length? (Simple integer)")
      val c = readLine().toInt

      new Gameboard(dims(0), dims(1), c)
    } catch {
      case _: Throwable => println("Unexpected input, try again")
                           get_board_from_player()
    }

  }

  main()

  def parse_test_config() : Array[GameConfig] = {
    val rt : util.ArrayList[GameConfig] = new util.ArrayList[GameConfig]()

    val bufferedsource = fromFile("testconfig.txt")

    val iter = bufferedsource.getLines()

    val trials = iter.next().toInt

    while (iter.hasNext) {
      val dimensions = iter.next().split('x')
      val cncts = iter.next().split(',')
      val boards: Array[Gameboard] = {
        val r = dimensions(0).toInt
        val c = dimensions(1).toInt
        val rt: IndexedSeq[Gameboard] = for {i <- cncts.indices
                                             gbs = new Gameboard(r, c, cncts(i).toInt)
                                             } yield gbs
        rt.toArray
      }
      val agnts = iter.next().split(',')

      for (i <- agnts.indices) {
        for (j <- agnts.indices) {
          for(k <- boards.indices) {
            for(l <- 0 until trials) {
              rt.add(GameConfig(boards(k), match_char(agnts(i), boards(k)), match_char(agnts(j), boards(k)), l))
            }
          }
        }
      }
    }

    bufferedsource.close()

    //toArray
    val toArray : Array[GameConfig] = {
      val r = new Array[GameConfig](rt.size())
      for(i <- 0 until rt.size()) {
        r(i) = rt.get(i)
      }
      r
    }
    toArray
  }

  def match_char(player : String, board : Gameboard) : Agent = {
    player match {
      case "m" => new MCTSPlayer(board)
      case "s" => new MinMaxSimplePlayer(board)
      case "r" => new RandomPlayer(board)
      case "a" => new MinMaxTruePlayer(board)
    }
  }

}