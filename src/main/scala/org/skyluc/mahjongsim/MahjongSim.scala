package org.skyluc.mahjongsim

import model.BaseModel._
import model.CommModel._ // likely should not be needed

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask

object MahjongSim {

  def main(args: Array[String]) {

    val rng = new scala.util.Random()


    val system = ActorSystem("majhong")

    val recorderManager = system.actorOf(runtime.RecorderManagerActor.props())
    val gameManager = system.actorOf(runtime.GameManagerActor.props(recorderManager))

    import scala.concurrent._
    import scala.concurrent.duration._
    import akka.util.Timeout

    implicit val timeout: Timeout = 2.seconds

    val future = (gameManager ? runtime.GameManagerActor.Run(100)).mapTo[runtime.GameManagerActor.Result]

    try {
      val result = Await.result(future, 2.seconds)
      println(result)
      Thread.sleep(100)
      system.terminate
    } catch {
      case e: Exception =>
        println("Timed out")
        system.terminate()
    }
  }
}