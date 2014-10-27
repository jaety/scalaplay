package jt.scalaplay

case class Grid(pieces: Seq[Option[Grid.PlacedPiece]]) {
    import Grid._


    def add(piece: PlacedPiece) : Option[Grid] = {
        if (valid(piece)) Some(Grid(pieces.take(piece.place) ++ Seq(Some(piece)) ++ pieces.drop(piece.place+1)))
        else None
    }

    def valid(p: PlacedPiece) = {
        pieces(p.place) == None && 
        up(p.place).map(_.down matches p.up).getOrElse(true) &&
        right(p.place).map(_.left matches p.right).getOrElse(true) &&
        down(p.place).map(_.up matches p.down).getOrElse(true) &&
        left(p.place).map(_.right matches p.left).getOrElse(true)
    }
    def up(site: Int) : Option[PlacedPiece] = {
        val index = site - 3
        if (index >= 0) pieces(index) else None
    }
    def down(site: Int) : Option[PlacedPiece] = {
        val index = site + 3
        if (index <= 8) pieces(index) else None
    }
    def left(site: Int) : Option[PlacedPiece] = {
        if (site % 3 == 0) None else pieces(site-1)
    }
    def right(site: Int) : Option[PlacedPiece] = {
        if (site % 3 == 2) None else pieces(site+1)
    }

    def full = pieces.forall(_ != None)
    def count = pieces.map(x => if (x == None) 0 else 1).sum

    def prettyPrint = {
        val strs = pieces.map(_.map(_.prettyPrint).getOrElse("--------"))
        val lines = strs.grouped(3).map(_.mkString(","))
        lines.mkString("\n") + "\n" + "*"*(8*3+2)
    }
}

object Grid {

    type Suit = Char
    object Suits {
        val Club = "C"
        val Spade = "S"
        val Diamond = "D"
        val Heart = "H"
    }
    type Side = Char
    object Side {
        val Out = "O"
        val In  = "I"
    }

    case class Edge(suit: Suit, side: Side) {
        def matches(other: Edge) = suit == other.suit && side != other.side

        def prettyPrint = s"$suit$side"
    }
    case class Piece(edges: Seq[Edge])
    case class PlacedPiece(piece: Piece, upIndex: Int, place: Int) {
        def up    = piece.edges(upIndex)
        def right = piece.edges((upIndex + 1) % 4)
        def down  = piece.edges((upIndex + 2) % 4)
        def left  = piece.edges((upIndex + 3) % 4)

        def prettyPrint = Seq(up,right,down,left).map(_.prettyPrint).mkString("")
    }

    def piece(s: String) = Piece(s.grouped(2).map(t => Edge(t(0), t(1))).toIndexedSeq)

    val place_order = Seq(4,1,5,2,3,0,7,6,8)
    def nextGrids(grid: Grid, pieces: Seq[Piece]) : Seq[(Piece, Seq[Piece], Grid)] = for {
        piece <- pieces
        up    <- if (grid.count == 0) Seq(0) else (0 to 3)
        placed_piece = PlacedPiece(piece, up, place_order(grid.count))
        nextGrid <- (grid add placed_piece).toSeq
    } yield {
        (piece, pieces.filter(_ != piece), nextGrid)
    }

    def empty = Grid(Seq.fill(9)(None))

    def solve(grid: Grid, pieces: Seq[Piece]) : Seq[Grid] = {

        if (grid.count < 2) println(grid.prettyPrint)

        if (grid.full) Seq(grid)
        else {
            for {
                (piece, otherPieces, nextGrid) <- nextGrids(grid, pieces)
                solution <- solve(nextGrid, otherPieces)
            } yield {
                solution
            }
        }
    }

    def solve(pieces: Seq[Piece]) : Seq[Grid] = solve(empty, pieces)

    val pieces = Seq(
        "H+D+D-H-",
        "D+C+C-D-",
        "H+S+S-C-",
        "S+D+S-H-",
        "S+D+H-D-",
        "C+H+S-H-",
        "H+D+C-C-",
        "S+S+H-C-",
        "C+H+D-C-"
    ).map(piece)

    def main(args: Array[String]) {
        val solution = solve(pieces) // .take(1)
        solution.map(_.prettyPrint).foreach(println)
    }
}

