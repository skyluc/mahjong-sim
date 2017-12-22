package org.skyluc.mahjongsim

import model.BaseModel._
import model.CommModel._ // likely should not be needed

import akka.actor.{ActorSystem, Props}

object MahjongSim {

  def main(args: Array[String]) {


    val system = ActorSystem("majhong")

    val player1 = system.actorOf(runtime.PlayerActor.props(East))
    val player2 = system.actorOf(runtime.PlayerActor.props(South))
    val player3 = system.actorOf(runtime.PlayerActor.props(West))
    val player4 = system.actorOf(runtime.PlayerActor.props(North))

    val game = system.actorOf(runtime.GameActor.props(player1, player2, player3, player4))

    game ! Start(shuffledTileSet, 3, 4, 5)

    val lock = system.whenTerminated

    import scala.concurrent._
    import scala.concurrent.duration._

    try {
      Await.ready(lock, 2.seconds)
    } catch {
      case e: Exception =>
        println("Timed out")
        system.terminate()
    }
  }
}