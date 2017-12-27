package org.skyluc.mahjongsim.model

import scala.annotation.tailrec

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

  case class Chow(t1: Tile, t2: Tile, t3: Tile) extends Combination {
    override val tiles = List(t1, t2, t3)
  }
  
  case class Pung(t1: Tile, t2: Tile, t3: Tile) extends Combination {
    override val tiles = List(t1, t2, t3)
  }
  
  case class Kong(t1: Tile, t2: Tile, t3: Tile, t4: Tile) extends Combination {
    override val tiles = List(t1, t2, t3, t4)
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

  def isMahjong(tiles: List[Tile], combinations: List[Combination]): Boolean = {
    val used = combinations.flatMap(_.tiles)
    val notUsed = tiles.filterNot(used.contains(_))

    val (bamboo, character, dot, dragon, wind) = groupTiles(notUsed)

    val bambooMod3 = bamboo.length % 3
    val characterMod3 = character.length % 3
    val dotMod3 = dot.length % 3
    val dragonMod3 = dragon.length % 3
    val windMod3 = wind.length % 3
    
    if (bambooMod3 == 2){
      if (characterMod3 == 0 && dotMod3 == 0 && dragonMod3 == 0 && windMod3 == 0) {
        isAllPungsAndChowsPlusPairNumbered(bamboo) &&
          isAllPungsAndChowsNumbered(character) &&
          isAllPungsAndChowsNumbered(dot) &&
          isAllPungsNamed(dragon) &&
          isAllPungsNamed(wind)
      } else {
        false
      }
    } else if (characterMod3 == 2){
      if (bambooMod3 == 0 && dotMod3 == 0 && dragonMod3 == 0 && windMod3 == 0) {
        isAllPungsAndChowsPlusPairNumbered(character) &&
          isAllPungsAndChowsNumbered(bamboo) &&
          isAllPungsAndChowsNumbered(dot) &&
          isAllPungsNamed(dragon) &&
          isAllPungsNamed(wind)
      } else {
        false
      }
    } else if (dotMod3 == 2){
      if (bambooMod3 == 0 && characterMod3 == 0 && dragonMod3 == 0 && windMod3 == 0) {
        isAllPungsAndChowsPlusPairNumbered(dot) &&
          isAllPungsAndChowsNumbered(bamboo) &&
          isAllPungsAndChowsNumbered(character) &&
          isAllPungsNamed(dragon) &&
          isAllPungsNamed(wind)
      } else {
        false
      }
    } else if (dragonMod3 == 2){
      if (bambooMod3 == 0 && characterMod3 == 0 && dotMod3 == 0 && windMod3 == 0) {
        isAllPungsPlusPairNamed(dragon) &&
          isAllPungsAndChowsNumbered(bamboo) &&
          isAllPungsAndChowsNumbered(character) &&
          isAllPungsAndChowsNumbered(dot) &&
          isAllPungsNamed(wind)
      } else {
        false
      }
    } else if (windMod3 == 2){
      if (bambooMod3 == 0 && characterMod3 == 0 && dotMod3 == 0 && dragonMod3 == 0) {
        isAllPungsPlusPairNamed(wind) &&
          isAllPungsAndChowsNumbered(bamboo) &&
          isAllPungsAndChowsNumbered(character) &&
          isAllPungsAndChowsNumbered(dot) &&
          isAllPungsNamed(dragon)
      } else {
        false
      }
    } else {
      false
    }
  }

  def groupTiles(tiles: List[Tile]): (List[BambooTile], List[CharacterTile], List[DotTile], List[DragonTile], List[WindTile]) = {
    @tailrec def groupTiles(
      tiles: List[Tile],
      bamboo: List[BambooTile],
      character: List[CharacterTile],
      dot: List[DotTile],
      dragon: List[DragonTile],
      wind: List[WindTile]): (List[BambooTile], List[CharacterTile], List[DotTile], List[DragonTile], List[WindTile]) = {
        tiles match {
          case Nil =>
            (bamboo, character, dot, dragon, wind)
          case head :: tail =>
            head match {
              case t: BambooTile =>
                groupTiles(tail, t :: bamboo, character, dot, dragon, wind)
              case t: CharacterTile =>
                groupTiles(tail, bamboo, t :: character, dot, dragon, wind)
              case t: DotTile =>
                groupTiles(tail, bamboo, character, t :: dot, dragon, wind)
              case t: DragonTile =>
                groupTiles(tail, bamboo, character, dot, t :: dragon, wind)
              case t: WindTile =>
                groupTiles(tail, bamboo, character, dot, dragon, t :: wind)
            }
        }
      }
    groupTiles(tiles, Nil, Nil, Nil, Nil, Nil)
  }

  def pungsAndChowsNumbered(tiles: List[NumberedTile]): List[Combination] = {
    val possible = tiles.combinations(3)
    possible.flatMap{ c =>
      val t1 = c.head
      val t2 = c.tail.head
      val t3 = c.tail.tail.head
      if (t1.number == t2.number && t1.number == t3.number) {
        Some(Pung(t1, t2, t3))
      } else if (t1.number + 1 == t2.number && t1.number + 2 == t3.number ||
        t1.number + 1 == t3.number && t1.number + 2 == t2.number ||
        t2.number + 1 == t1.number && t2.number + 2 == t3.number ||
        t2.number + 1 == t3.number && t2.number + 2 == t1.number ||
        t3.number + 1 == t1.number && t3.number + 2 == t2.number ||
        t3.number + 1 == t2.number && t3.number + 2 == t1.number) {
        Some(Chow(t1, t2, t3))
      } else {
        None
      }
    }.to[List]
  }

  def pungsNamed(tiles: List[NamedTile]): List[Pung] = {
    val possible = tiles.combinations(3)
    possible.flatMap{ c =>
      val t1 = c.head
      val t2 = c.tail.head
      val t3 = c.tail.tail.head
      if (t1.name == t2.name && t1.name == t3.name) {
        Some(Pung(t1, t2, t3))
      } else {
        None
      }
    }.to[List]
  }

  def kongsNamed(tiles: List[NamedTile]): List[Kong] = {
    val possible = tiles.combinations(4)
    possible.flatMap{ c =>
      val t1 = c.head
      val t2 = c.tail.head
      val t3 = c.tail.tail.head
      val t4 = c.tail.tail.tail.head
      if (t1.name == t2.name && t1.name == t3.name && t1.name == t4.name) {
        Some(Kong(t1, t2, t3, t4))
      } else {
        None
      }
    }.to[List]   
  }

  // prerequisite: n * 3 tiles, of same tile type
  def isAllPungsAndChowsNumbered(tiles: List[NumberedTile]): Boolean = {
    if (tiles.isEmpty) {
      true
    } else {
      val combinations = pungsAndChowsNumbered(tiles)
      val combinationsNeeded = tiles.length / 3
      val cc = combinations.combinations(combinationsNeeded)
      cc.exists{ c =>
        c.flatMap(_.tiles).distinct.length == tiles.length
      }
    }
  }

  // prerequisite: n * 3 tiles, of same tile type
  def isAllPungsNamed(tiles: List[NamedTile]): Boolean = {
    if (tiles.isEmpty) {
      true
    } else {
      val combinations = pungsNamed(tiles)
      val combinationsNeeded = tiles.length / 3
      val cc = combinations.combinations(combinationsNeeded)
      cc.exists{ c =>
        c.flatMap(_.tiles).distinct.length == tiles.length
      }
    }
  }

  // prerequisite: n * 3 + 2 tiles, of same tile type
  def isAllPungsAndChowsPlusPairNumbered(tiles: List[NumberedTile]): Boolean = {
    tiles match {
      case p1 :: p2 :: Nil =>
        p1.number == p2.number
      case _ =>
        val combinations = pungsAndChowsNumbered(tiles)
        val combinationsNeeded = tiles.length / 3
        val cc = combinations.combinations(combinationsNeeded)
        cc.exists{ c =>
          tiles.diff(c.flatMap(_.tiles)) match {
            case p1 :: p2 :: Nil =>
              p1.number == p2.number            
            case _ =>
              false
          }
        }
    }
  }

  // prerequisite: n * 3 + 2 tiles, of same tile type
  def isAllPungsPlusPairNamed(tiles: List[NamedTile]): Boolean = {
    tiles match {
      case p1 :: p2 :: Nil =>
        p1.name == p2.name
      case _ =>
        val combinations = pungsNamed(tiles)
        val combinationsNeeded = tiles.length / 3
        val cc = combinations.combinations(combinationsNeeded)
        cc.exists{ c =>
          tiles.diff(c.flatMap(_.tiles)) match {
            case p1 :: p2 :: Nil =>
              p1.name == p2.name            
            case _ =>
              false
          }
        }
    }
  }
}