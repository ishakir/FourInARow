import scala.annotation.tailrec

/**
 * Created by imran on 29/11/15.
 */
object Main extends App {
  override def main (args: Array[String]) {
    recurse(io.Source.stdin.getLines(), State())
  }

  @tailrec
  def recurse(inputs: Iterator[String], state: State): Unit = {
    if(!inputs.hasNext) return
    else {
      val line: String = inputs.next()
      System.err.println(s"Received '$line'")

      val (responses, newState) = process(line, state)

      // Log out the current state
//      System.err.println(newState)

      // Print out actions to stdout
      for(response <- responses) println(response)

      // Aaaand continue
      recurse(inputs, newState)
    }
  }

  def println(str: String) = {
    System.err.println(s"Sending: '$str'")
    print(str)
  }

  def process(input: String, state: State) = {
    try {
      val args = input.split(" ")
      (args(0).toLowerCase(), args(1).toLowerCase()) match {
        case ("settings", "timebank") => (Seq(), new State(state.settings.timebank(args(2).toInt), state.game, state.allGroups))
        case ("settings", "time_per_move") => (Seq(), new State(state.settings.timePerMove(args(2).toInt), state.game, state.allGroups))
        case ("settings", "your_bot") => {
          val settings = state.settings
          val newState = if(settings.myBotName != args(2)) {
            new State(settings.theirBotName(settings.myBotName).myBotName(args(2)), state.game, state.allGroups)
          } else {
            new State(state.settings.myBotName(args(2)), state.game, state.allGroups)
          }
          (Seq(), newState)
        }
        case ("settings", "your_botid") => (Seq(), new State(state.settings.myBotId(args(2).toInt), state.game, state.allGroups))
        case ("settings", "field_columns") => (Seq(), new State(state.settings.columns(args(2).toInt), state.game, state.allGroups))
        case ("settings", "field_rows") => (Seq(), new State(state.settings.rows(args(2).toInt), state.game, state.allGroups))
        case ("settings", "player_names") => {
          val settings = state.settings
          val names = args(2).split(",")
          val newSettings = if(settings.myBotName == "") {
            settings.myBotName(names(0)).theirBotName(names(1))
          } else {
            settings.theirBotName(names.find(str => str != settings.myBotName).get)
          }
          (Seq(), new State(newSettings, state.game, state.allGroups))
        }
        case ("update", "game") => args(2).toLowerCase() match {
          case "round" => {
            // We really don't care
            (Seq(), state)
          }
          case "field" => {
            val board = Board(args(3), state.settings)
            if(state.game.currentRoundNumber == 0) {
              (Seq(), new State(state.settings, state.game.nextRound(board), new AllGroups(board)))
            } else {
              (Seq(), new State(state.settings, state.game.nextRound(board), state.allGroups))
            }
          }
        }
        case ("action", "move") => {
          val column = new Move(state.game.currentRound.board, state.allGroups).chooseColumn()
          (Seq(s"place_disc $column"), state)
        }
      }
    } catch {
      case e: Exception => {
        System.err.println("Caught exception!")
        e.printStackTrace(System.err)
        (Seq(), state)
      }
    }
  }
}
