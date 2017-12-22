package org.skyluc.mahjongsim.model

object BaseModel {

  // TODO: nextPosition might not be needed. To check
  sealed abstract class Position(key: Char) {
    def nextPosition: Position
  }
  case object East extends Position('E') {
    override def nextPosition = South
  }
  case object South extends Position('S') {
    override def nextPosition = West
  }
  case object West extends Position('W') {
    override def nextPosition = North
  }
  case object North extends Position('N') {
    override def nextPosition = East
  }

  trait Tile {
    def copy: Int

    def key: Char

    protected def shortId: String

    val id: String = s"$shortId$copy"

    override def toString = id
  }

  case object NoTile extends Tile {
    override val copy = 0
    override val key = 'Z'
    protected override val shortId = "  "
    override val id = "   "
  }

  trait NumberedTile extends Tile {
    def number: Int
    protected override def shortId: String = s"$key$number"
  }

  trait NamedTile extends Tile {
    def name: Char
    protected override def shortId: String = s"$key$name"
  }

  case class DotTile(override val number: Int, override val copy: Int) extends NumberedTile {
    override def key = 'D'
  }

  case class BambooTile(override val number: Int, override val copy: Int) extends NumberedTile {
    override def key = 'B'
  }

  case class CharacterTile(override val number: Int, override val copy: Int) extends NumberedTile {
    override def key = 'C'
  }

  case class DragonTile(override val name: Char, override val copy: Int) extends NamedTile {
    override def key = 'R'
 }

  case class WindTile(override val name: Char, override val copy: Int) extends NamedTile {
    override def key = 'W'
  }

  trait Combination {
    def tiles: List[Tile]
  }
  
  val TileSet: List[Tile] = {
    val numbered = for {
      number <- 1 to 9
      copy <- 1 to 4
    } yield {
      List(DotTile(number, copy), BambooTile(number, copy), CharacterTile(number, copy))
    }
    val named = for {
      copy <- 1 to 4
    } yield {
      List(DragonTile('G', copy), DragonTile('R', copy), DragonTile('W', copy),
        WindTile('E', copy), WindTile('S', copy), WindTile('W', copy), WindTile('N', copy))
    }
    (numbered ++ named).to[List].flatten.sortBy(_.id)
  }

  val rng = new util.Random()

  def shuffledTileSet: List[Tile] =
    rng.shuffle(TileSet)

}