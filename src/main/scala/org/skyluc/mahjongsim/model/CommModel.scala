package org.skyluc.mahjongsim.model

import BaseModel._

object CommModel {
  case class Start(shuffledTileSet: List[Tile], d1: Int, d2: Int, d3: Int)
  case class Deal4(t1: Tile, t2: Tile, t3: Tile, t4: Tile)
  case class Deal1(t: Tile)

  case class Draw(t: Tile)
  case class Discard(t: Tile)
  case object Mahjong

}