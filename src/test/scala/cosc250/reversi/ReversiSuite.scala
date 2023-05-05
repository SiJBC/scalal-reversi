package cosc250.reversi

/** A place for you to write some boid tests.
  *
  * Boids are an immutable class containing functions. That makes them
  * relatively straightforward to test -- except that the values within them are
  * doubles, which are hard to compare exactly. Instead, test if they are close
  * (i.e. within a certain amount +/- what you're looking for).
  */
class ReversiSuite extends munit.FunSuite {

  // A place for you to write tests. Some suggested tests to start with have been sketched below

  test("Counts pieces") {
    assertEquals(
      2,
      GameState(
        None,
        Map((3, 3) -> Player.Black, (3, 4) -> Player.Black),
        Player.White
      ).blackPieces
    )
    assertEquals(
      2,
      GameState(
        None,
        Map((3, 3) -> Player.White, (3, 4) -> Player.White),
        Player.White
      ).whitePieces
    )

  }

  test(
    "Should be able to detect if a move is valid and there is an Northern chain of opponent pieces"
  ) {
    assertEquals(
      true,
      GameState(
        Some((4, 3), Player.White),
        Map(
          ((3, 3), Player.Black),
          ((4, 4), Player.Black),
          ((3, 4), Player.White),
          ((4, 3), Player.White)
        ),
        Player.Black
      ).isValidMove((4, 2))
    )
  }

  test(
    "Should be able to detect if a move is valid and there is an Southern chain of opponent pieces"
  ) {
    assertEquals(
      true,
      GameState(
        Some((4, 3), Player.White),
        Map(
          ((3, 3), Player.Black),
          ((4, 4), Player.Black),
          ((3, 4), Player.White),
          ((4, 3), Player.White)
        ),
        Player.Black
      ).isValidMove((3, 5))
    )
  }

  test(
    "Should be able to detect if a move is valid and there is an Eastern chain of opponent pieces"
  ) {
    assertEquals(
      true,
      GameState(
        Some((4, 3), Player.White),
        Map(
          ((3, 3), Player.Black),
          ((4, 4), Player.Black),
          ((3, 4), Player.White),
          ((4, 3), Player.White)
        ),
        Player.Black
      ).isValidMove((5, 3))
    )
  }

  test(
    "Should be able to detect if a move is valid and there is an Western chain of opponent pieces"
  ) {
    assertEquals(
      true,
      GameState(
        Some((4, 3), Player.White),
        Map(
          ((3, 3), Player.Black),
          ((4, 4), Player.Black),
          ((3, 4), Player.White),
          ((4, 3), Player.White)
        ),
        Player.Black
      ).isValidMove((5, 3))
    )
  }

  test("Should be able to count the score for one player") {
    ???
  }

  // You'll need to write some additional tests

}
