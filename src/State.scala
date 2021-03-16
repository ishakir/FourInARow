object Settings {
  def apply() = new Settings(0, 0, "", "", 0, 0, 0)
}
class Settings(val timebank: Int, val timePerMove: Int, val myBotName: String,
               val theirBotName: String, val myBotId: Int, val columns: Int, val rows: Int) {
  def timebank(newTimebank: Int): Settings = new Settings(newTimebank, timePerMove, myBotName, theirBotName, myBotId, columns, rows)
  def timePerMove(newTimePerMove: Int): Settings = new Settings(timebank, newTimePerMove, myBotName, theirBotName, myBotId, columns, rows)
  def myBotName(newMyBotName: String): Settings = new Settings(timebank, timePerMove, newMyBotName, theirBotName, myBotId, columns, rows)
  def theirBotName(newTheirBotName: String): Settings = new Settings(timebank, timePerMove, myBotName, newTheirBotName, myBotId, columns, rows)
  def myBotId(newMyBotId: Int): Settings = new Settings(timebank, timePerMove, myBotName, theirBotName, newMyBotId, columns, rows)
  def columns(newColumns: Int): Settings = new Settings(timebank, timePerMove, myBotName, theirBotName, myBotId, newColumns, rows)
  def rows(newRows: Int): Settings = new Settings(timebank, timePerMove, myBotName, theirBotName, myBotId, columns, newRows)
  override val toString =
    s"""{
       |    "timebank": $timebank,
       |    "time_per_move": $timePerMove,
       |    "my_bot_name": $myBotName,
       |    "their_bot_name": $theirBotName,
       |    "my_bot_id": $myBotId,
       |    "columns": $columns,
       |    "rows": $rows
       |  }""".stripMargin
}

class Game(val rounds: List[Round]) {
  lazy val currentRound = rounds.head
  lazy val currentRoundNumber = if(rounds.isEmpty) 0 else currentRound.number
  def nextRound(board: IndexedSeq[IndexedSeq[Cell]]) = new Game(new Round(currentRoundNumber + 1, board) :: rounds)
  override val toString =
    s"""[
       |    ${rounds.mkString(",\n    ")}
       |  ]""".stripMargin
}

class Cell
case object Me extends Cell
case object Opponent extends Cell
case object Empty extends Cell

object Board {
  def apply(field: String, settings: Settings): IndexedSeq[IndexedSeq[Cell]] = {
    val numberBoard: IndexedSeq[IndexedSeq[String]] = field.split(";").toIndexedSeq.map(str => str.split(",").toIndexedSeq)
    numberBoard.map(arr =>
      arr.map(str => str.toInt match {
        case settings.myBotId => Me
        case 0 => Empty
        case _ => Opponent
      })
    ).asInstanceOf[IndexedSeq[IndexedSeq[Cell]]]
  }
}
class Round(val number: Int, val board: IndexedSeq[IndexedSeq[Cell]]) {
  val boardString = board.map(row =>
    "        " + row.map(cell => cell match {
      case Me => "M"
      case Opponent => "O"
      case Empty => "."
    }).mkString("")
  ).mkString("\n")
  override val toString =
    s"""{
       |      "number": $number,
       |      "board": \n$boardString
       |    }""".stripMargin
}

object State {
  def apply() = new State(Settings(), new Game(Nil), null)
}
class State(val settings: Settings, val game: Game, val allGroups: AllGroups) {
  override val toString =
    s"""{
      |  "settings": $settings,
      |  "game": $game
      |}""".stripMargin
}
