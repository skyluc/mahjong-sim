package org.skyluc.mahjongsim.runtime

import akka.actor.{Actor, ActorRef, Props}

import org.skyluc.mahjongsim.model.InternalModel._
import org.skyluc.mahjongsim.model.CommModel
import org.skyluc.mahjongsim.model.BaseModel._

// TODO: stealing for Mahjong

class GameActor(pEast: ActorRef, pSouth: ActorRef, pWest: ActorRef, pNorth: ActorRef) extends Actor {
  import GameActor._
  import context._

  case object PlayerEast extends Player(East, pEast) {
    override def nextPlayer = PlayerSouth
  }
  case object PlayerSouth extends Player(South, pSouth) {
    override def nextPlayer = PlayerWest
  }
  case object PlayerWest extends Player(West, pWest) {
    override def nextPlayer = PlayerNorth
  }
  case object PlayerNorth extends Player(North, pNorth) {
    override def nextPlayer = PlayerEast
  }

  def receive = notStarted()
  
  def notStarted(): Receive = {
    case CommModel.Start(shuffledTileSet, d1, d2, d3) =>
      become(dealAll(GameState(shuffledTileSet, d1 + d2 + d3)))
  }

  def dealAll(state: GameState): Receive = debug(state) {
    deal4(state, PlayerEast, 12)
  }

  def deal4(state: GameState, player: Player, count: Int): Receive = {
    self ! GameActor.Deal4(player, count)
    deal(state)
  }

  def deal1(state: GameState, player: Player, count: Int): Receive = {
    self ! GameActor.Deal1(player, count)
    deal(state)
  }

  def deal(state: GameState): Receive = {
    case GameActor.Deal4(_, 0) =>
      become(deal1(state, PlayerEast, 4))
    case GameActor.Deal4(player, count) =>
      val (tile1, tile2, tile3, tile4) = state.nextDraw4Tiles

      player.actor ! CommModel.Deal4(tile1, tile2, tile3, tile4)

      become(deal4(
        state.deal4(player.position, tile1, tile2, tile3, tile4),
        player.nextPlayer,
        count - 1))
    case GameActor.Deal1(_, 0) =>
      become(startPlay(state))
    case GameActor.Deal1(player, count) =>
      player.actor ! CommModel.Deal1(state.nextDrawTile)
      become(deal1(
        state.deal1(player.position, state.nextDrawTile),
        player.nextPlayer,
        count - 1)
      )
  }

  def startPlay(state: GameState): Receive = debug(state) {
    val draw = state.nextDrawTile
    PlayerEast.actor ! CommModel.Draw(draw)
    waitForDrawResponse(state, PlayerEast, draw)
  }

  def nextDraw(state: GameState, player: Player): Receive = debug(state) {
    if (state.moreDraw) { // TODO: need a done function
      val draw = state.nextDrawTile
      val nextPlayer = player.nextPlayer
      nextPlayer.actor ! CommModel.Draw(draw)
      waitForDrawResponse(state, nextPlayer, draw)
    } else {
      system.terminate
      debug(state) {
        case Nil =>
          // should not reach here
      }
    }
  }

  def waitForDrawResponse(state: GameState, player: Player, draw: Tile): Receive = {
    case CommModel.Discard(discard) =>
      become(nextDraw(state.simpleDraw(player.position, draw, discard), player))
    case CommModel.Mahjong =>
      println(s"Mahjong ${player.position} +$draw")
      become(debug(state.simpleMahjong(player.position, draw)) {
        case Nil =>
          // should not reach here
      })
      system.terminate
      
  }

  def play(state: GameState): Receive = {
    case GameActor.Play =>
      println("play")
      system.terminate()
  }

  private def debug(state: GameState)(r: Receive): Receive = {
    println(state)
    r orElse {
      case o =>
        println(s"BAD!!!!!! - $o")
    }
  }

}

object GameActor {

  abstract class Player(val position: Position, val actor: ActorRef) {
    def nextPlayer: Player
  }

  case class Deal4(player: Player, count: Int)
  case class Deal1(player: Player, count: Int)
  case object Play

  def props(pEast: ActorRef, pSouth: ActorRef, pNorth: ActorRef, pWest: ActorRef): Props =
    Props(classOf[GameActor], pEast, pSouth, pNorth, pWest)
}