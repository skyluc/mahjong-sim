package org.skyluc.mahjongsim.runtime

import akka.actor.{Actor, Props}

// import org.skyluc.mahjongsim.model.PlayerModel._
import org.skyluc.mahjongsim.model.BaseModel._
import org.skyluc.mahjongsim.model.CommModel
import org.skyluc.mahjongsim.log.Logger


class PlayerActor(position2: Position) extends Actor {
  import context._
  import PlayerActor._

  val rng = new scala.util.Random()

  def receive = deal(PlayerView(position2, Nil, List(), None))
  
  def deal(state: PlayerView): Receive = {
    case CommModel.Deal4(t1, t2, t3, t4) =>
      become(deal(state.deal4(t1, t2, t3, t4)))
    case CommModel.Deal1(t) =>
      become(play(state.deal1(t)))
    case _ =>
      println("bad")
  }

  def play(state: PlayerView): Receive = {
    case CommModel.DrawTile(draw) =>
      val all = state.tiles :+ draw
      if (isMahjong(all, state.combinations)) {
        sender ! CommModel.DrawMahjong
        become(mahjong(state.mahjong(draw)))
      } else {
        val moves = state.drawMoves(draw)
        moves(rng.nextInt(moves.length)) match {
          case KongMove(kong) =>
            sender ! CommModel.DrawKong(kong)
            become(playReplacement(state.simpleKong(draw, kong)))
          case DiscardMove(discard)=>
            sender ! CommModel.SimpleDraw(discard)
            become(play(state.simpleDraw(draw, discard)))
        }
      }
    case CommModel.Discarded(byPrevious, discard) =>
      val all = state.tiles :+ discard
      if (isMahjong(all, state.combinations)) {
        sender ! CommModel.DiscardedMahjong
        become(waitClaimResponse(state, discard, DiscardMahjong))
      } else {
        val moves = state.discardMoves(byPrevious, discard)
        val move = moves(rng.nextInt(moves.length))
        move match {
          case KongMove(kong) =>
            sender ! CommModel.BigMeldedKong(kong)
          case PungMove(pung) =>
            sender ! CommModel.MeldedPung(pung)
          case ChowMove(chow) =>
            sender ! CommModel.MeldedChow(chow)
          case NoClaim =>
            sender ! CommModel.NoClaim
        }
        become(waitClaimResponse(state, discard, move))
      }
  }

  def playReplacement(state: PlayerView): Receive = {
    case CommModel.ReplacementTile(replacement) =>
      val all = state.tiles :+ replacement
      if (isMahjong(all, state.combinations)) {
        sender ! CommModel.ReplacementMahjong
        become(mahjong(state.mahjong(replacement)))
      } else {
        val moves = state.drawMoves(replacement)
        moves(rng.nextInt(moves.length)) match {
          case KongMove(kong) =>
            sender ! CommModel.ReplacementKong(kong)
            become(playReplacement(state.simpleKong(replacement, kong)))
          case DiscardMove(discard)=>
            sender ! CommModel.SimpleReplacement(discard)
            become(play(state.simpleDraw(replacement, discard)))
        }
      }
  }

  def waitClaimResponse(state: PlayerView, claim: Tile, move: Move): Receive = {
    val r: Receive = {
      case CommModel.ClaimAccepted =>
        move match {
            case KongMove(kong) =>
              become(playReplacement(state.bigMeldedKong(claim, kong)))
            case PungMove(pung) =>
              val discard = state.pickADiscardForMeldedPungOrChow(claim, pung)
              sender ! CommModel.MeldedPungDiscard(discard)
              become(play(state.meldedPung(claim, pung, discard)))
            case ChowMove(chow) =>
              val discard = state.pickADiscardForMeldedPungOrChow(claim, chow)
              sender ! CommModel.MeldedChowDiscard(discard)
              become(play(state.meldedChow(claim, chow, discard)))
            case DiscardMahjong =>
              become(mahjong(state.mahjong(claim)))
            case NoClaim =>
              become(play(state))      
        }
      case CommModel.ClaimRefused =>
        become(play(state))
    }
    r
  }

  def mahjong(state: PlayerView): Receive = {
    val r: Receive = {
      case _ =>
      println(s"BAD!!! - I won the game")
    }
    r
  }

}

object PlayerActor {

  def log(state: PlayerView): PlayerView = {
    Logger.log(state)
    state
  }
  
  case class PlayerView(
    position: Position,
    tiles: List[Tile],
    combinations: List[Combination],
    mahjongTile: Option[Tile]) {

    override def toString(): String = {
      val cs = combinations.map {
        _.tiles.map(_.id).mkString("-")
      }.mkString(" ")
      val used = combinations.flatMap(_.tiles)
      val others = tiles.filterNot(used.contains(_)).mkString(" ")
      val plusTile = mahjongTile.map(t => s" +$t").getOrElse("")
      s"Player $position\n$cs $others$plusTile"
    }

    def deal4(t1: Tile, t2: Tile, t3: Tile, t4: Tile): PlayerView = {
      log(copy(tiles = tiles ++ List(t1, t2, t3, t4)))
    }

    def deal1(t: Tile): PlayerView = {
      log(copy(tiles = tiles :+ t))
    }

    def simpleDraw(draw: Tile, discard: Tile): PlayerView = {
      if (draw == discard) {
        log(this)
      } else {
        log(copy(tiles = tiles.filterNot(_ == discard) :+ draw))
      }
    }

    // TODO: find better name
    def simpleKong(draw: Tile, kong: Kong): PlayerView = {
      log(copy(tiles = tiles :+ draw, combinations = combinations :+ kong))
    }

    def bigMeldedKong(claim: Tile, kong: Kong): PlayerView = {
      log(copy(tiles = tiles :+ claim, combinations = combinations :+ kong))
    }

    def meldedPung(claim: Tile, pung: Pung, discard: Tile): PlayerView = {
      log(copy(tiles = tiles.filterNot(_ == discard) :+ claim, combinations = combinations :+ pung))
    }

    def meldedChow(claim: Tile, chow: Chow, discard: Tile): PlayerView = {
      log(copy(tiles = tiles.filterNot(_ == discard) :+ claim, combinations = combinations :+ chow))
    }

    def mahjong(tile: Tile): PlayerView = {
      log(copy(mahjongTile = Some(tile)))
    }

    // TODO: find better name
    // TODO: move mahjong detection here ?
    // TODO: support for small melded kong
    def drawMoves(draw: Tile): List[Move] = {
      val available = notInCombination :+ draw

      val (bamboo, character, dot, dragon, wind) = groupTiles(available)
      val kongs = kongsNumbered(bamboo) ++ kongsNumbered(character) ++ kongsNumbered(dot) ++ kongsNamed(dragon) ++ kongsNamed(wind)

      // TODO: normally, should choose from all possible moves
      val allMoves = kongs.map(KongMove(_)) ++ available.map(DiscardMove(_))

      if (kongs.isEmpty) {
        available.map(DiscardMove(_))
      } else {
        kongs.map(KongMove(_))
      }
    }

    def notInCombination: List[Tile] = {
      val used = combinations.flatMap(_.tiles)
      tiles.filterNot(used.contains(_))
    }

    // TODO: move mahjong detection here ?
    def discardMoves(byPrevious: Boolean, discard: Tile): List[Move] = {
      val used = combinations.flatMap(_.tiles)
      val available = tiles.filterNot(used.contains(_)) :+ discard

      val (bamboo, character, dot, dragon, wind) = groupTiles(available)
      val bigMeldedKongs = (kongsNumbered(bamboo) ++ kongsNumbered(character) ++ kongsNumbered(dot) ++ kongsNamed(dragon) ++ kongsNamed(wind)).
        filter(_.tiles.contains(discard))
      val meldedPungs = (pungsNumbered(bamboo) ++ pungsNumbered(character) ++ pungsNumbered(dot) ++ pungsNamed(dragon) ++ pungsNamed(wind)).
        filter(_.tiles.contains(discard))

      val meldedChows = if (byPrevious) {
        (chowsNumbered(bamboo) ++ chowsNumbered(character) ++ chowsNumbered(dot)).
          filter(_.tiles.contains(discard))
      } else {
        Nil
      }

      val cs = bigMeldedKongs.map(KongMove(_)) ++ meldedPungs.map(PungMove(_)) ++ meldedChows.map(ChowMove(_))

      if (cs.isEmpty) {
        List(NoClaim)
      } else {
        cs
      }

    }

    // TODO: better name
    def pickADiscardForMeldedPungOrChow(claim: Tile, Combination: Combination): Tile = {
      val newAll = tiles :+ claim
      val toBeUsed = combinations.flatMap(_.tiles) ++ Combination.tiles
      val available = tiles.diff(toBeUsed)

      available(rng.nextInt(available.length))
    }
  }

  trait Move {
  }

  case class KongMove(kong: Kong) extends Move {

  }

  case class DiscardMove(discard: Tile) extends Move {

  }

  case class PungMove(pung: Pung) extends Move

  case class ChowMove(chow: Chow) extends Move

  case object NoClaim extends Move

  case object DiscardMahjong extends Move

  def props(position: Position): Props = Props(classOf[PlayerActor], position)

}