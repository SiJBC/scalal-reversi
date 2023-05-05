package cosc250.reversi

import scala.collection.immutable.Queue
import scala.annotation.tailrec

enum Player:
  case Black
  case White

enum Direction:
  case North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest

/** The board size is always 8 by 8 */
val boardSize = 8

/** A location on the board. Zero-indexed */
type Location = (Int, Int)

/** The state of the board
  *
  * @param lastMove
  *   \- the location of the last move
  * @param board
  *   \- maps the locations of pieces on the board (note that if a piece has not
  *   been played in a square, it won't be in the map)
  * @param turn
  *   \- whose turn it is next
  */
case class GameState(
    lastMove: Option[(Location, Player)],
    board: Map[Location, Player],
    turn: Player
) {
  // can you write a method that will return Location (int,int) from the lastMove as a tuple

  val openingCoords: Seq[Location] = Seq((3, 3), (3, 4), (4, 4), (4, 3))

  /** The number of pieces on the board by colour */
  /** The number of black pieces */
  def blackPieces: Int = board.count(_._2 == Player.Black)

  /** The number of white pieces */
  def whitePieces: Int = board.count(_._2 == Player.White)
  def player: Player = turn
  def allSquaresOfPlayer(player: Player): Seq[Location] = {
    board.filter((square, player) => player == player).toSeq.map(_._1)
  }

  /** True if neither player can play a move */
  def gameOver: Boolean = boardFull

  /** Whether a particular move is valid */
  def isValidMove(location: Location): Boolean = {
    val opponent = if (player == Player.Black) Player.White else Player.Black
    Direction.values.exists((direction) =>
      // n value is 1 so that can check the neighbour square is an opponent piece
      (staticMapCheck(board, location, direction, opponent, 1)) &
        // n value is 1 so that can check that there is a chain of opponent pieces in that direction
        (tailRecursiveMapCheck(board, location, direction, opponent, 1) > 1) &
        // n value is the length of the chain of opponent pieces so that can check that the next square is a player piece
        (staticMapCheck(
          board,
          location,
          direction,
          player,
          tailRecursiveMapCheck(board, location, direction, opponent, 1)
        ))
    )
  }

  /** Performs a move */
  def move(location: Location): GameState =
    ???

  def isOpeningMove: Boolean = board.size < 4
  def boardLength: Int = board.size
  def boardFull: Boolean = board.size == (blackPieces + whitePieces)
  def moveCount: Int = blackPieces + whitePieces
  def truthyFalsyLocation(
      location: Location
  ): Boolean = {
    board.get(location) match {
      case Some(Player.White) => true
      case Some(Player.Black) => true
      case None               => false
    }
  }
  def neighbours(location: Location): Seq[Location] = {
    val x = location._1
    val y = location._2
    (x, y) match {
      case (0, 0) => Seq((x + 1, y), (x, y + 1), (x + 1, y + 1))
      case (8, 8) => Seq((x - 1, y), (x, y - 1), (x - 1, y - 1))
      case (_, 0) =>
        Seq((x - 1, y), (x + 1, y), (x, y + 1), (x - 1, y + 1), (x + 1, y + 1))
      case (0, _) =>
        Seq((x, y - 1), (x, y + 1), (x + 1, y), (x + 1, y - 1), (x + 1, y + 1))
      case (_, _) =>
        Seq(
          (x - 1, y),
          (x + 1, y),
          (x, y - 1),
          (x, y + 1),
          (x - 1, y - 1),
          (x - 1, y + 1),
          (x + 1, y - 1),
          (x + 1, y + 1)
        )
    }
  }

  /** Check a square on the board contains a player the square's position is
    * relative to a predetermined location*
    */
  def staticMapCheck(
      map: Map[Location, Player],
      location: Location,
      direction: Direction,
      player: Player,
      n: Int
  ): Boolean = {
    direction match {
      case Direction.North => {
        if (board.get((location._1, location._2 - n)) == Some(player)) {
          true
        } else {
          false
        }
      }
      case Direction.South => {
        if (board.get((location._1, location._2 + n)) == Some(player)) {
          true
        } else {
          false
        }
      }
      case Direction.East => {
        if (board.get((location._1 + n, location._2)) == Some(player)) {
          true
        } else {
          false
        }
      }
      case Direction.West => {
        if (board.get((location._1 - n, location._2)) == Some(player)) {
          true
        } else {
          false
        }
      }

      case Direction.NorthEast => {
        if (board.get((location._1 + n, location._2 - n)) == Some(player)) {
          true
        } else {
          false
        }
      }
      case Direction.NorthWest => {
        if (board.get((location._1 - n, location._2 - n)) == Some(player)) {
          true
        } else {
          false
        }
      }
      case Direction.SouthEast => {
        if (board.get((location._1 + n, location._2 + n)) == Some(player)) {
          true
        } else {
          false
        }
      }
      case Direction.SouthWest => {
        if (board.get((location._1 - n, location._2 + n)) == Some(player)) {
          true
        } else {
          false
        }
      }
    }
  }

  /** Recursively traverse the board Map to check for chains of pieces * */
  @tailrec
  private def tailRecursiveMapCheck(
      map: Map[Location, Player],
      location: Location,
      direction: Direction,
      player: Player,
      count: Int
  ): Int = {
    direction match {
      case Direction.North => {
        if (map.get((location._1, location._2 - 1)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1, location._2 - 1),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
      case Direction.South => {
        if (map.get((location._1, location._2 + 1)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1, location._2 + 1),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
      case Direction.East => {
        if (map.get((location._1 + 1, location._2)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1 + 1, location._2),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
      case Direction.West => {
        if (map.get((location._1 - 1, location._2)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1 - 1, location._2),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
      case Direction.NorthEast => {
        if (map.get((location._1 + 1, location._2 - 1)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1 + 1, location._2 + 1),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
      case Direction.NorthWest => {
        if (map.get((location._1 - 1, location._2 - 1)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1 - 1, location._2 + 1),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
      case Direction.SouthEast => {
        if (map.get((location._1 + 1, location._2 + 1)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1 + 1, location._2 - 1),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
      case Direction.SouthWest => {
        if (map.get((location._1 - 1, location._2 + 1)) == Some(player)) {
          tailRecursiveMapCheck(
            map,
            (location._1 - 1, location._2 - 1),
            direction,
            player,
            count + 1
          )
        } else {
          count
        }
      }
    }
  }

  def nextMove: Location = {
    if (board.size < 4) {
      val player = lastMove.map(_._2).getOrElse(Player.Black)
      val validMoves =
        openingCoords.filter(!truthyFalsyLocation(_))
      validMoves(0)
    } else {
      val opponent = if (turn == Player.Black) Player.White else Player.Black
      val opponentPieces = board.filter(_._2 == opponent).keys.toSeq
      val possibleMoves =
        opponentPieces.flatMap(neighbours).filter(!truthyFalsyLocation(_))
      val validMoves = possibleMoves
        .filter(isValidMove)
      validMoves(0)
    }
  }

  def nextPlayer: Player = {
    if (lastMove.isEmpty) {
      Player.White
    } else {
      if (lastMove.map(_._2).get == Player.Black) {
        Player.White
      } else {
        Player.Black
      }
    }
  }

  def returnNeighbours(location: Location): Seq[Location] =
    neighbours(location).filter(truthyFalsyLocation(_))

  def filterNeighboursByPlayer(
      location: Location,
      player: Player
  ): Seq[Location] =
    returnNeighbours(location).filter(board.get(_).get == player)

  def directionReturn(
      locationOne: Location,
      locationTwo: Location
  ): Direction = {
    if (locationOne._1 == locationTwo._1) {
      if (locationOne._2 < locationTwo._2) {
        Direction.South
      } else {
        Direction.North
      }
    } else if (locationOne._2 == locationTwo._2) {
      if (locationOne._1 > locationTwo._1) {
        Direction.West
      } else {
        Direction.East
      }
    } else if (locationOne._1 > locationTwo._1) {
      if (locationOne._2 < locationTwo._2) {
        Direction.SouthWest
      } else {
        Direction.NorthWest
      }
    } else {
      if (locationOne._2 < locationTwo._2) {
        Direction.SouthEast
      } else {
        Direction.NorthEast
      }
    }
  }

  def returnAllDirectionRecursions(
      locations: Seq[Location],
      player: Player,
      location: Location,
      direction: Direction,
      board: Map[Location, Player]
  ): Seq[Location] = {
    val recursiveSquare = {
      direction match {
        case Direction.East      => (location._1 + 1, location._2)
        case Direction.West      => (location._1 - 1, location._2)
        case Direction.North     => (location._1, location._2 - 1)
        case Direction.South     => (location._1, location._2 + 1)
        case Direction.SouthEast => (location._1 + 1, location._2 + 1)
        case Direction.SouthWest => (location._1 - 1, location._2 + 1)
        case Direction.NorthEast => (location._1 + 1, location._2 - 1)
        case Direction.NorthWest => (location._1 - 1, location._2 - 1)
      }
    }
    if (board.get(recursiveSquare) == None) {
      Seq()
    } else if (board.get(recursiveSquare) == Some(player)) {
      locations
    } else {
      Seq(recursiveSquare) ++ returnAllDirectionRecursions(
        locations,
        player,
        recursiveSquare,
        direction,
        board
      )
    }
  }

  def updateBoardAfterFlip(
      nextMoveLocation: Location
  ): Map[Location, Player] = {
    val opponent = if (turn == Player.Black) Player.White else Player.Black
    val neighboursOpponentSquares =
      filterNeighboursByPlayer(nextMoveLocation, opponent)
    val tempBoard = board + (nextMoveLocation -> turn)

    val neighboursOpponentSquaresAndDirectionAndNeighbourChains =
      neighboursOpponentSquares
        .zip(
          neighboursOpponentSquares.map((square) =>
            directionReturn(nextMoveLocation, square)
          )
        )

    if (board.size > 5) {
      println("here: 428")
      println(board)
      println(nextMoveLocation)
      println(neighboursOpponentSquaresAndDirectionAndNeighbourChains)
    }

    val piecesToUpdate =
      neighboursOpponentSquaresAndDirectionAndNeighbourChains
        .map((_1, _2) =>
          returnAllDirectionRecursions(
            Seq(nextMoveLocation),
            turn,
            nextMoveLocation,
            _2,
            tempBoard
          )
        )
        .filter(_.size > 1)
        .flatten

    val newBoard = tempBoard.map((_1, _2) => {
      if (piecesToUpdate.contains(_1)) {
        (_1, turn)
      } else {
        (_1, _2)
      }
    })
    newBoard
    // board
  }

  def updateBoardMap(
      nextMoveLocation: Location,
      turn: Player
  ): Map[Location, Player] = {
    val updatedSquares =
      neighbours(nextMoveLocation).filter(square =>
        truthyFalsyLocation(square) && board.get(square) == Some(
          turn
        )
      )
    val updatedBoardMap = updateBoardAfterFlip(nextMoveLocation)
    updatedBoardMap + (nextMoveLocation -> turn)
  }

  def generateNewGame: GameState = {
    val nextMoveLocation = nextMove
    val nextMovePlayer = lastMove.map(_._2).get
    val newBoard = updateBoardMap(nextMoveLocation, nextMovePlayer)
    GameState(Some(nextMoveLocation, nextMovePlayer), newBoard, nextMovePlayer)
  }
}

object GameState {
  def newGame = GameState(None, Map.empty, Player.Black)
}

/** A game is a sequence of game-states (so it remembers past moves). The most
  * recent move is at the end.
  */
type Game = Seq[GameState]

/** Creates a new game, containing just the start game state */
def newGame: Seq[GameState] = Seq(GameState.newGame)

/** Called by the UI on each animation tick to make your AI play the game */
def play(state: Seq[GameState], lookAhead: Int): Seq[GameState] = {
  val lastState = state.last
  val nextMoveLocation = lastState.nextMove
  val nextMovePlayer = lastState.nextPlayer
  val newBoard = lastState.updateBoardMap(
    nextMoveLocation,
    lastState.turn
  )

  val newState = GameState(
    Some(nextMoveLocation, nextMovePlayer),
    newBoard,
    nextMovePlayer
  )
  state :+ newState
}

/** Called by the UI when the user clicks back in the game history */
def rewindTo(state: Seq[GameState], move: Int): Seq[GameState] =
  state.slice(0, move + 1)
