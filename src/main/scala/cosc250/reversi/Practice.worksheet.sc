type Location = (Int, Int)
enum Player:
  case Black, White

// make a map for me with a Map[Location, Player]

val map = Map(
  ((7, 1), Player.White),
  ((6, 1), Player.White),
  ((5, 1), Player.White),
  ((4, 1), Player.Black),
  ((8, 2), Player.White),
  ((8, 3), Player.White),
  ((8, 4), Player.White),
  ((8, 5), Player.Black)
)
println(map.get((1, 1)))

val list = Seq(1, 2, 3, 4, 5)
println(list)

list :+ 6

val board = Map(
  ((3, 3), Player.Black),
  ((4, 4), Player.Black),
  ((3, 4), Player.Black),
  ((4, 3), Player.White),
  ((4, 2), Player.White),
  ((4, 1), Player.White),
  ((4, 0), Player.Black)
)

val boardB = Map(
  ((3, 3), Player.Black),
  ((3, 4), Player.White),
  ((4, 4), Player.Black),
  ((4, 3), Player.White),
  ((2, 4), Player.Black)
)

def recursivelyEast(
    board: Map[Location, Player],
    location: Location,
    player: Player
): Seq[Location] = {
  val eastSquare = (location._1 + 1, location._2)
  if (board.get(eastSquare) == None) {
    Seq()
  } else if (board.get(eastSquare) == Some(player)) {
    Seq()
  } else {
    Seq(eastSquare) ++ recursivelyEast(board, eastSquare, player)
  }
}
println(boardB)
recursivelySouth(boardB, (3, 4), Player.White)

recursivelyEast(boardB, (2, 4), Player.Black)

def recursivelySouth(
    board: Map[Location, Player],
    location: Location,
    player: Player
): Seq[Location] = {
  val southSquare = (location._1, location._2 + 1)
  if (board.get(southSquare) == None) {
    Seq()
  } else if (board.get(southSquare) == Some(player)) {
    Seq()
  } else {
    Seq(southSquare) ++ recursivelySouth(board, southSquare, player)
  }
}

def recursivelyNorth(
    board: Map[Location, Player],
    location: Location,
    player: Player
): Seq[Location] = {
  val northSquare = (location._1, location._2 - 1)
  if (board.get(northSquare) == None) {
    Seq()
  } else if (board.get(northSquare) == Some(player)) {
    Seq(northSquare)
  } else {
    Seq(northSquare) ++ recursivelyNorth(board, northSquare, player)
  }
}

println(recursivelySouth(board, (4, 1), Player.White))
println(recursivelyNorth(board, (4, 4), Player.Black))
println(recursivelySouth(board, (4, 0), Player.Black))
