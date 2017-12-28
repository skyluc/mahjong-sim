package org.skyluc.mahjongsim.runtime

import akka.actor.{Actor, Props}

// import org.skyluc.mahjongsim.model.PlayerModel._
import org.skyluc.mahjongsim.model.BaseModel._
import org.skyluc.mahjongsim.model.CommModel


class PlayerActor(position: Position) extends Actor {
  import context._
  import PlayerActor._

  val rng = new scala.util.Random()

  def receive = deal(PlayerView(Nil, List()))
  
  def deal(state: PlayerView): Receive = {
    case CommModel.Deal4(t1, t2, t3, t4) =>
      become(deal(state.deal4(t1, t2, t3, t4)))
    case CommModel.Deal1(t) =>
      become(play(state.deal1(t)))
    case _ =>
      println("bad")
  }

  def play(state: PlayerView): Receive = debug(state) {
    case CommModel.DrawTile(draw) =>
      val all = state.tiles :+ draw
      if (isMahjong(all, state.combinations)) {
        sender ! CommModel.DrawMahjong
        become(mahjong(state, draw))
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
    case _ =>
      println("bad")
  }

  def playReplacement(state: PlayerView): Receive = debug(state) {
    case CommModel.ReplacementTile(replacement) =>
      val all = state.tiles :+ replacement
      if (isMahjong(all, state.combinations)) {
        sender ! CommModel.ReplacementMahjong
        become(mahjong(state, replacement))
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
    case _ =>
      println("bad")
  }

  def mahjong(state: PlayerView, mahjongTile: Tile): Receive = {
    println(state.toString(position, mahjongTile))
    val r: Receive = {
      case _ =>
      println(s"BAD!!! - I won the game")
    }
    r
  }

  private def debug(state: PlayerView)(r: Receive): Receive = {
    println(state.toString(position))
    r orElse {
      case o =>
        println(s"BAD!! - $o")
    }
  }

}

object PlayerActor {
  
  case class PlayerView(
    tiles: List[Tile],
    combinations: List[Combination]) {

    def toString(position: Position): String = {
      val cs = combinations.map {
        _.tiles.map(_.id).mkString("-")
      }.mkString(" ")
      val used = combinations.flatMap(_.tiles)
      val others = tiles.filterNot(used.contains(_)).mkString(" ")
      s"Player $position\n$cs $others"
    }

    def toString(position: Position, mahjongTile: Tile): String = {
      s"${toString(position)} +${mahjongTile.id}"
    }

    def deal4(t1: Tile, t2: Tile, t3: Tile, t4: Tile): PlayerView = {
      copy(tiles = tiles ++ List(t1, t2, t3, t4))
    }

    def deal1(t: Tile): PlayerView = {
      copy(tiles = tiles :+ t)
    }

    def simpleDraw(draw: Tile, discard: Tile): PlayerView = {
      if (draw == discard) {
        this
      } else {
        copy(tiles = tiles.filterNot(_ == discard) :+ draw)
      }
    }

    // TODO: find better name
    def simpleKong(draw: Tile, kong: Kong): PlayerView = {
      copy(tiles = tiles :+ draw, combinations :+ kong)
    }

    // TODO: find better name
    def drawMoves(draw: Tile): List[Move] = {
      val used = combinations.flatMap(_.tiles)
      val available = tiles.filterNot(used.contains(_)) :+ draw

      val (bamboo, character, dot, dragon, wind) = groupTiles(available)
      val kongs = kongsNumbered(bamboo) ++ kongsNumbered(character) ++ kongsNumbered(dot) ++ kongsNamed(dragon) ++ kongsNamed(wind)

      val allMoves = kongs.map(KongMove(_)) ++ available.map(DiscardMove(_))

      if (kongs.isEmpty) {
        available.map(DiscardMove(_))
      } else {
        kongs.map(KongMove(_))
      }
    }
  }

  trait Move {
  }

  case class KongMove(kong: Kong) extends Move {

  }

  case class DiscardMove(discard: Tile) extends Move {

  }

  def props(position: Position): Props = Props(classOf[PlayerActor], position)

}