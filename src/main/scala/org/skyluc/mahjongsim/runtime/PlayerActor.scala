package org.skyluc.mahjongsim.runtime

import akka.actor.{Actor, Props}

// import org.skyluc.mahjongsim.model.PlayerModel._
import org.skyluc.mahjongsim.model.BaseModel._
import org.skyluc.mahjongsim.model.CommModel


class PlayerActor(position: Position) extends Actor {
  import context._
  import PlayerActor._

  val rng = new scala.util.Random()

  def receive = deal(PlayerView(Vector(), List()))
  
  def deal(state: PlayerView): Receive = {
    case CommModel.Deal4(t1, t2, t3, t4) =>
      become(deal(state.copy(tiles = state.tiles ++ List(t1, t2, t3, t4))))
    case CommModel.Deal1(t) =>
      become(play(state.copy(tiles = state.tiles :+ t)))
    case _ =>
      println("bad")
  }

  def play(state: PlayerView): Receive = debug(state) {
    case CommModel.Draw(draw) =>
      val all = state.tiles :+ draw
      val discard = all(rng.nextInt(14))
      val newTiles = all.filterNot(_ == discard)
      sender ! CommModel.Discard(discard)
      become(play(state.copy(tiles = newTiles)))
    case _ =>
      println("bad")
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

  trait Combination {
    def indexes: List[Int]
  }
  
  case class PlayerView(
    tiles: Vector[Tile],
    combinations: List[Combination]) {

    def toString(position: Position): String = {
      val cs = combinations.map {
        _.indexes.map(tiles(_).id).mkString("-")
      }.mkString(" ")
      val used = combinations.flatMap(_.indexes)
      val others = tiles.zipWithIndex.filterNot(t => used.contains(t._2)).map(_._1.id).mkString(" ")
      s"Player $position\n$cs $others"
    }
  }

  def props(position: Position): Props = Props(classOf[PlayerActor], position)

}