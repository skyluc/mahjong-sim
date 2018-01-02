package org.skyluc.mahjongsim.runtime

import akka.actor._
import org.skyluc.mahjongsim.model.BaseModel._

class RecorderActor extends Actor {

  import RecorderActor._

  def receive: Receive = {
    case Deal1(position, tile) =>
      log(s"D1   ${position.key} $tile")
    case Deal4(position, tile1, tile2, tile3, tile4) =>
      log(s"D4   ${position.key} $tile1 $tile2 $tile3 $tile4")
    case SimpleDiscard(position, draw, discard) =>
      log(s"SD   ${position.key} $draw $discard")  
    case SimpleConcealedKong(position, draw, kong) =>
      log(s"SCK  ${position.key} $draw $kong")
    case DrawMahjong(position, draw) =>
      log(s"SM   ${position.key} $draw")
    case ReplacementDiscard(position, draw, discard) =>
      log(s"RD  ${position.key} $draw $discard")
    case ReplacementConcealedKong(position, draw, kong) =>
      log(s"RCK  ${position.key} $draw $kong")
    case ReplacementMahjong(position, draw) =>
      log(s"RM   ${position.key} $draw")
    case BigMeldedKong(position, claim, kong) =>
      log(s"BMK  ${position.key} $claim $kong")
    case MeldedPung(position, claim, pung, discard) =>
      log(s"MP   ${position.key} $claim $pung $discard")
    case MeldedChow(position, claim, chow, discard) =>
      log(s"MC   ${position.key} $claim $chow $discard")
    case DiscardMahjong(position, draw) =>
      log(s"DM   ${position.key} $draw")
  }

  private def log(move: String): Unit = {
    println(s"------------ $move")
  }

}

object RecorderActor {

  case class Deal1(position: Position, tile: Tile)
  case class Deal4(position: Position, tile1: Tile, tile2: Tile, tile3: Tile, tile4: Tile)
  case class SimpleDiscard(position: Position, draw: Tile, discard: Tile)
  case class SimpleConcealedKong(position: Position, draw: Tile, kong: Kong)
  case class ReplacementDiscard(position: Position, draw: Tile, discard: Tile)
  case class ReplacementConcealedKong(position: Position, draw: Tile, kong: Kong)
  case class BigMeldedKong(position: Position, discard: Tile, kong: Kong)
  case class MeldedPung(position: Position, claim: Tile, pung: Pung, discard: Tile)
  case class MeldedChow(position: Position, claim: Tile, chow: Chow, discard: Tile)

  case class DrawMahjong(position: Position, draw: Tile)
  case class ReplacementMahjong(position: Position, draw: Tile)
  case class DiscardMahjong(position: Position, draw: Tile)

  def props: Props = Props(classOf[RecorderActor])
}