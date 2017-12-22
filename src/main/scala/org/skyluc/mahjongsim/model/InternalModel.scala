package org.skyluc.mahjongsim.model

import org.skyluc.mahjongsim.model.BaseModel._

object InternalModel {

  case class Hand(
    position: Position,
    tiles: Vector[Tile],
    combinations: List[Combination]
  ) {
    override def toString(): String = {
      val cs = combinations.map {
        _.tiles.mkString("-")
      }.mkString(" ")
      val used = combinations.flatMap(_.tiles)
      val others = tiles.filterNot(t => used.contains(t)).mkString(" ")
      s"Position $position\n$cs $others"
    }
  }

  object Hand {
    def apply(position: Position): Hand = {
      Hand(position, Vector(), List())
    }
  }

  case class Stack(bottom: Tile, top: Tile)

  sealed trait WallIndex {
    def stackIndex: Int
    def nextForward: WallIndex
    def nextBackward: WallIndex
  }

  case class StackTopIndex(override val stackIndex: Int) extends WallIndex {
    override def nextForward = StackBottomIndex(stackIndex)
    override def nextBackward = StackBottomIndex(stackIndex)

    override def toString: String = s"top$stackIndex"
  }

  case class StackBottomIndex(override val stackIndex: Int) extends WallIndex {
    override def nextForward = if (stackIndex == 67) {
      StackTopIndex(0)
    } else {
      StackTopIndex(stackIndex + 1)
    }

    override def nextBackward = if (stackIndex == 0) {
      StackTopIndex(67)
    } else {
      StackTopIndex(stackIndex - 1) 
    }

    override def toString: String = s"bottom$stackIndex"
  }

  case class Walls(
    stacks: Vector[Stack],
    nextDrawIndex: WallIndex,
    nextReplacementIndex: WallIndex,
    moreDraw: Boolean) {

    private def apply(index: WallIndex): Tile = {
      index match {
        case StackTopIndex(i) =>
          stacks(i).top
        case StackBottomIndex(i) =>
          stacks(i).bottom
      }
    }

    def nextDrawTile: Tile = {
      apply(nextDrawIndex)
    }

    def nextDraw4Tiles: (Tile, Tile, Tile, Tile) = {
      (
        apply(nextDrawIndex),
        apply(nextDrawIndex.nextForward),
        apply(nextDrawIndex.nextForward.nextForward),
        apply(nextDrawIndex.nextForward.nextForward.nextForward)
      )
    }

    def draw4(): Walls = {
      // TODO: check in right state ?
      copy(nextDrawIndex = nextDrawIndex.nextForward.nextForward.nextForward.nextForward)
    }

    def draw(): Walls = {
      // TODO: check more draw
      (nextDrawIndex, nextReplacementIndex) match {
        case (StackTopIndex(i), StackTopIndex(j)) if (i == j) =>
          copy(nextDrawIndex = nextDrawIndex.nextForward, nextReplacementIndex = nextReplacementIndex.nextBackward)
        case (StackBottomIndex(i), StackBottomIndex(j)) if (i == j) =>
          copy(moreDraw = false)
        case (StackBottomIndex(i), StackBottomIndex(j)) if (i == j - 1 || j == 0 && i == 67) =>
          copy(nextDrawIndex = nextReplacementIndex)
        case _ =>
          copy(nextDrawIndex = nextDrawIndex.nextForward)
      }
    }

    override def toString: String = {
      if (moreDraw) {

        val nextTopDrawIndex = nextDrawIndex match {
          case StackTopIndex(i) =>
            i
          case StackBottomIndex(i) =>
            i + 1
        }
        val limitTopReplacementIndex = nextReplacementIndex match {
          case StackTopIndex(i) =>
            i + 1
          case StackBottomIndex(i) =>
            i
        }

        val topInWall: List[Boolean] = if (nextDrawIndex.stackIndex > nextReplacementIndex.stackIndex) {
          List.fill(limitTopReplacementIndex)(true) ++
            List.fill(nextTopDrawIndex - limitTopReplacementIndex)(false) ++
            List.fill(68 - nextTopDrawIndex)(true)
        } else {
          List.fill(nextTopDrawIndex)(false) ++
            List.fill(limitTopReplacementIndex - nextTopDrawIndex)(true) ++
            List.fill(68 - limitTopReplacementIndex)(false)
        }

        val bottomInWall: List[Boolean] = if (nextDrawIndex.stackIndex > nextReplacementIndex.stackIndex) {
          List.fill(nextReplacementIndex.stackIndex + 1)(true) ++
            List.fill(nextDrawIndex.stackIndex - nextReplacementIndex.stackIndex - 1)(false) ++
            List.fill(68 - nextDrawIndex.stackIndex)(true)
        } else {
          List.fill(nextDrawIndex.stackIndex)(false) ++
          List.fill(nextReplacementIndex.stackIndex - nextDrawIndex.stackIndex + 1)(true) ++
          List.fill(67 - nextReplacementIndex.stackIndex)(false)
        }

        val topStrings = stacks.zip(topInWall).map(t =>
          if (t._2) {
            t._1.top.toString
          } else {
            "   "
          }
        )

        val bottomStrings = stacks.zip(bottomInWall).map(t =>
          if (t._2) {
            t._1.bottom.toString
          } else {
            "   "
          }
        )

        s"""
Next draw index: $nextDrawIndex. Next replacement index: $nextReplacementIndex

East
${topStrings.take(17).reverse.mkString(" ")}
${bottomStrings.take(17).reverse.mkString(" ")}
South
${topStrings.drop(51).take(17).reverse.mkString(" ")}
${bottomStrings.drop(51).take(17).reverse.mkString(" ")}
West
${topStrings.drop(34).take(17).reverse.mkString(" ")}
${bottomStrings.drop(34).take(17).reverse.mkString(" ")}
North
${topStrings.drop(17).take(17).reverse.mkString(" ")}
${bottomStrings.drop(17).take(17).reverse.mkString(" ")}
"""
      } else {
        "No more draw"
      }
    }

  }

  object Walls {
    def apply(shuffledTiles: List[Tile], diceRoll: Int): Walls = {
      val stacks: Vector[Stack] = shuffledTiles.grouped(2).map(l => Stack(l(0), l(1))).to[Vector]

      val wallFirstStack = diceRoll % 4 match {
        case 0 =>
          17
        case 1 =>
          0
        case 2 =>
          51
        case 3 =>
          34
      }
      val firstDrawIndex = StackTopIndex(wallFirstStack + diceRoll)

      Walls(stacks, firstDrawIndex, firstDrawIndex.nextBackward.nextBackward, true)
    }
  }

  case class GameState(
    /**
     * wallTiles(0) is top left most tile in front of the east position
     */
    walls: Walls,
    lastDiscardedTile: Option[Tile],
    nextPosition: Position,
    east: Hand,
    south: Hand,
    west: Hand,
    north: Hand
  ) {

    import GameState._

    override def toString: String = {
      s"""
$walls

$east
$north
$west
$south
"""
    }

    def deal4(position: Position, tile1: Tile, tile2: Tile, tile3: Tile, tile4: Tile): GameState = { 
      // TODO: check valid move
      val t = copy(walls = walls.draw4())
      t.updatePosition(position){ p =>
        p.copy(tiles = p.tiles ++ List(tile1, tile2, tile3, tile4))
      }
    }

    def deal1(position: Position, tile: Tile): GameState = {
      // TODO: check valid move
      val t = copy(walls = walls.draw())
      t.updatePosition(position){ p =>
        p.copy(tiles = p.tiles :+ tile)
      }
    }

    def simpleDraw(position: Position, draw: Tile, discard: Tile): GameState = {
      // TODO: check valid move
      val t = copy(walls = walls.draw(), lastDiscardedTile = Some(discard))
      if (draw == discard) {
        t
      } else {
        t.updatePosition(position){ h =>
          val discarded = h.tiles.filterNot(_ == discard)
          h.copy(tiles = discarded :+ draw)
        }
      }
    }

    def nextDrawTile: Tile = {
      // TODO: check game not finished
      walls.nextDrawTile
    }

    def nextDraw4Tiles: (Tile, Tile, Tile, Tile) = {
      // TODO: check game still in deal phase ?
      walls.nextDraw4Tiles
    }

    def moreDraw: Boolean =
      walls.moreDraw

    private def updatePosition(position: Position)(f: Hand => Hand): GameState = {
      position match {
        case East =>
          copy(east = f(east))
        case South =>
          copy(south = f(south))
        case West =>
          copy(west = f(west))
        case North =>
          copy(north = f(north))
      }
    }
  }

  object GameState {

    def apply(shuffledTiles: List[Tile], diceRoll: Int): GameState = {
      GameState(Walls(shuffledTiles, diceRoll), None, East, Hand(East), Hand(South), Hand(West), Hand(North))
    }

  }
}