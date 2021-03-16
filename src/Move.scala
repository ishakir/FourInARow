import scala.annotation.tailrec
import scala.util.Random

class AllGroups(board: IndexedSeq[IndexedSeq[Cell]]) {
  lazy val rowIndices: Seq[Int] = board.zipWithIndex.map { case (content, index) => index }.toSeq
  lazy val columnIndices: Seq[Int] = board(0).zipWithIndex.map { case (content, index) => index }.toSeq

  lazy val allDirections: Seq[(Int, Int)] = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  lazy val allGroupsOfFour: Set[Set[(Int, Int)]] = rowIndices.flatMap { row =>
    columnIndices.flatMap { col =>
      allDirections.flatMap { case (rowDir, colDir) =>
        val rowIndices = (0 to 3).map(a => row + a * rowDir)
        val colIndices = (0 to 3).map(a => col + a * colDir)
        val result = rowIndices zip colIndices
        try {
          result.foreach { case(i, j) => board(i)(j) }
          Some(result.toSet)
        } catch {
          case e: IndexOutOfBoundsException => None
        }
      }
    }
  }.toSet
}

/**
 * Created by imran on 29/11/15.
 */
class Move(board: IndexedSeq[IndexedSeq[Cell]], allGroups: AllGroups) {
  lazy val rowIndices: Seq[Int] = board.zipWithIndex.map { case (content, index) => index }.toSeq
  lazy val columnIndices: Seq[Int] = board(0).zipWithIndex.map { case (content, index) => index }.toSeq

  def landingIndex(col: Int) = board.map(row => row(col)).zipWithIndex.reverse.find { case (cell, index) => cell == Empty }.map(x => x._2)
  lazy val columnToLandingPosition: Map[Int, (Int, Int)] = columnIndices.flatMap(col => {
    landingIndex(col).map(row => col -> (row, col))
  }).toMap

  lazy val allWinners: Set[Int] = allGroups.allGroupsOfFour.flatMap(completionColumn(_, Me))
  lazy val allBlockers: Set[Int] = allGroups.allGroupsOfFour.flatMap(completionColumn(_, Opponent))

  def chooseColumn(): Int = {
    if(!allWinners.isEmpty) {
      System.err.println(s"Found winners '$allWinners'")
      allWinners.head
    } else if(!allBlockers.isEmpty) {
      System.err.println(s"Found blockers '$allBlockers'")
      allBlockers.head
    } else {
      chooseRandomly()
    }
  }

  @tailrec
  private def chooseRandomly(): Int = {
    def isSatisfactory(col: Int) = {
      val landingPosition = columnToLandingPosition.get(col).get
      val move = new Move(board.updated(landingPosition._1, board(landingPosition._1).updated(col, Me)), allGroups)
      move.allBlockers.size == 0
    }
    System.err.println("Fell back to random choice")
    val shuffled = Random.shuffle(columnToLandingPosition.keySet)
    val potential = shuffled.head
    System.err.println(s"Choosing the head of '$shuffled'")
    System.err.println(s"Which is '$potential'")
    if(isSatisfactory(potential)) potential else chooseRandomly()
  }

  def completionColumn(four: Set[(Int, Int)], cellType: Cell): Option[Int] = {
    val values = four.groupBy(get)
    if(values.get(cellType).isDefined && values.get(cellType).get.size == 3 && values.get(Empty).isDefined && values.get(Empty).get.size == 1) {
      val rowCol = values.get(Empty).get.head
      if(isLanding(rowCol)) Some(rowCol._2) else None
    } else None
  }

  def get(rowCol: (Int, Int)): Cell = rowCol match { case (row, col) =>
      board(row)(col)
  }

  def isLanding(rowCol: (Int, Int)): Boolean = rowCol match { case (row, col) =>
    columnToLandingPosition.get(col).isDefined && columnToLandingPosition.get(col).get == rowCol
  }
}
