package org.skyluc.mahjongsim.runtime

import akka.actor._
import org.skyluc.mahjongsim.model.BaseModel._
import org.skyluc.mahjongsim.model.CommModel

class GameManagerActor(recorderManager: ActorRef) extends Actor {
  import context._

  import GameManagerActor._

  def receive: Receive = notRunning
  
  def notRunning: Receive = {
    case Run(nb) =>
      self ! Init
      become(init(nb, 0, 0, sender, System.currentTimeMillis))
  }

  def init(toRun: Int, draw: Int, mahjong: Int, callback: ActorRef, startTime: Long): Receive = {
    case Init =>
      if (toRun > 0) {
        recorderManager ! RecorderManagerActor.NewRecorder
      }
      else {
        callback ! Result(draw, mahjong, System.currentTimeMillis - startTime)
        become(notRunning)
      }
    case RecorderManagerActor.Recorder(recorder) =>
      val playerEast = system.actorOf(PlayerActor.props(East))
      val playerSouth = system.actorOf(PlayerActor.props(South))
      val playerWest = system.actorOf(PlayerActor.props(West))
      val playerNorth = system.actorOf(PlayerActor.props(North))

      val game = actorOf(GameActor.props(playerEast, playerSouth, playerWest, playerNorth, recorder))

      game ! CommModel.Start(shuffledTileSet, rng.nextInt(6), rng.nextInt(6), rng.nextInt(6))
      become(waitForEndGame(toRun, draw, mahjong, playerEast, playerSouth, playerWest, playerNorth, game, recorder, callback, startTime))
  }

  def waitForEndGame(
    toRun: Int,
    draw: Int,
    mahjong: Int,
    playerEast: ActorRef,
    playerSouth: ActorRef,
    playerWest: ActorRef,
    playerNorth: ActorRef,
    game: ActorRef,
    recorder: ActorRef,
    callback: ActorRef,
    startTime: Long): Receive = {

    case CommModel.GameFinished(isMahjong) =>
      recorder ! RecorderActor.GameDone
      playerEast ! PoisonPill
      playerSouth ! PoisonPill
      playerWest ! PoisonPill
      playerNorth ! PoisonPill
      game ! PoisonPill
      self ! Init
      if (isMahjong)
        become(init(toRun - 1, draw, mahjong + 1, callback, startTime))
      else
        become(init(toRun - 1, draw + 1, mahjong, callback, startTime))

  }
}

object GameManagerActor {

  case object Init

  case class Run(nb: Int)

  case class Result(draw: Int, mahjong: Int, runningTime: Long)

  def props(recorderManager: ActorRef): Props = Props(classOf[GameManagerActor], recorderManager)
}

class RecorderManagerActor extends Actor {
  import RecorderManagerActor._

  def receive: Receive = {
    case NewRecorder =>
      sender ! Recorder(context.actorOf(RecorderActor.props))
  }
}

object RecorderManagerActor {

  case object NewRecorder
  case class Recorder(recorder: ActorRef)

  def props(): Props = Props(classOf[RecorderManagerActor])
}