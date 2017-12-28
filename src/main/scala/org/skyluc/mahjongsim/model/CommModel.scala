package org.skyluc.mahjongsim.model

import BaseModel._

object CommModel {
  case class Start(shuffledTileSet: List[Tile], d1: Int, d2: Int, d3: Int)
  case class Deal4(t1: Tile, t2: Tile, t3: Tile, t4: Tile)
  case class Deal1(t: Tile)

  case class DrawTile(draw: Tile)
  case class SimpleDraw(discard: Tile)
  case class DrawKong(kong: Kong)

  case class ReplacementTile(draw: Tile)
  case class SimpleReplacement(discard: Tile)
  case class ReplacementKong(kong: Kong)

  case object DrawMahjong
  case object ReplacementMahjong

}