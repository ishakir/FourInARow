/**
 * Created by imran on 29/11/15.
 */
object Test extends App {

  def assert(b: Boolean): Unit = {
    if(!b) { throw new IllegalStateException("Test failed!") } else println("Test passed!")
  }

  def assertNever(b: () => Boolean) = {
    assert((1 to 100).map(i => !b()).exists(identity))
  }

  val settings = new Settings(5, 10, "test_bot", "lame_test_bot", 1, 4, 4)
  def move(boardStr: String) = {
    val board = Board(boardStr.replace("\n", ";"), settings)
    new Move(board, new AllGroups(board))
  }

  val emptyBoard =
    """ |0,0,0,0
        |0,0,0,0
        |0,0,0,0
        |0,0,0,0""".stripMargin

  val differentLandingsBoard =
    """ |0,0,0,1
        |0,0,0,1
        |0,0,0,1
        |0,0,1,1""".stripMargin

  val twoWinnersTwoBlockers =
    """ |0,0,0,0
        |1,2,2,1
        |1,2,2,1
        |1,2,2,1""".stripMargin

  val twoBlockers =
    """ |0,0,0,0
        |0,2,2,0
        |0,2,2,0
        |0,2,2,0""".stripMargin

  val diagonalWinner =
    """ |1,0,0,0
        |2,1,0,0
        |2,2,0,0
        |2,2,2,1""".stripMargin

  val diagonalWinnerOnEnd =
    """ |0,0,0,0
        |2,1,0,0
        |2,2,1,0
        |2,2,2,1""".stripMargin

  val winnerInMiddle =
    """ |0,0,0,0
        |0,0,0,0
        |0,0,0,0
        |1,0,1,1""".stripMargin

  val willHandOpponentWin =
    """ |0,0,0,0
        |0,0,0,0
        |2,0,2,2
        |1,0,2,1""".stripMargin

  assert(move(emptyBoard).rowIndices == Seq(0, 1, 2, 3))
  assert(move(emptyBoard).columnIndices == Seq(0, 1, 2, 3))

  assert(
    move(emptyBoard).columnToLandingPosition == Map(
      0 -> (3, 0),
      1 -> (3, 1),
      2 -> (3, 2),
      3 -> (3, 3)
    )
  )

  assert(
    move(differentLandingsBoard).columnToLandingPosition == Map(
      0 -> (3, 0),
      1 -> (3, 1),
      2 -> (2, 2)
    )
  )


  assert(move(twoWinnersTwoBlockers).allWinners == Set(0, 3))
  assert(move(twoWinnersTwoBlockers).allBlockers == Set(1, 2))

  assert(Set(0,3).contains(move(twoWinnersTwoBlockers).chooseColumn()))
  assert(Set(1,2).contains(move(twoBlockers).chooseColumn()))

  assert(move(diagonalWinner).chooseColumn() == 2)
  assert(move(diagonalWinnerOnEnd).chooseColumn() == 0)
  assert(move(winnerInMiddle).chooseColumn() == 1)

  assertNever(() => move(willHandOpponentWin).chooseColumn() == 1)
  assertNever(() => move(differentLandingsBoard).chooseColumn() == 3)

}
