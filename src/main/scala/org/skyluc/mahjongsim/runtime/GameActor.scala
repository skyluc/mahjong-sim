package org.skyluc.mahjongsim.runtime

import akka.actor.{Actor, ActorRef, Props}

import org.skyluc.mahjongsim.model.InternalModel._
import org.skyluc.mahjongsim.model.CommModel
import org.skyluc.mahjongsim.model.BaseModel._

// TODO: stealing for Mahjong, on small melded kong
// TODO: check no calling kong on exhausted wall

class GameActor(pEast: ActorRef, pSouth: ActorRef, pWest: ActorRef, pNorth: ActorRef, recorder: ActorRef) extends Actor {
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

  def actorToPlayer: Map[ActorRef, Player] = collection.immutable.HashMap((pEast, PlayerEast), (pSouth, PlayerSouth), (pWest, PlayerWest), (pNorth, PlayerNorth))

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

      recorder ! RecorderActor.Deal4(player.position, tile1, tile2, tile3, tile4)

      become(deal4(
        state.deal4(player.position, tile1, tile2, tile3, tile4),
        player.nextPlayer,
        count - 1))
    case GameActor.Deal1(_, 0) =>
      become(startPlay(state))
    case GameActor.Deal1(player, count) =>
      player.actor ! CommModel.Deal1(state.nextDrawTile)

      recorder ! RecorderActor.Deal1(player.position, state.nextDrawTile)

      become(deal1(
        state.deal1(player.position, state.nextDrawTile),
        player.nextPlayer,
        count - 1)
      )
  }

  def startPlay(state: GameState): Receive = debug(state) {
    val draw = state.nextDrawTile
    PlayerEast.actor ! CommModel.DrawTile(draw)
    waitForDrawResponse(state, PlayerEast, draw)
  }

  def nextDraw(state: GameState, player: Player): Receive = debug(state) {
    if (state.moreDraw) {
      val draw = state.nextDrawTile
      val nextPlayer = player.nextPlayer
      nextPlayer.actor ! CommModel.DrawTile(draw)
      waitForDrawResponse(state, nextPlayer, draw)
    } else {
      self ! Done
      debug(state) {
        case Nil =>
          // should not reach here
      }
    }
  }

  def waitForDrawResponse(state: GameState, player: Player, draw: Tile): Receive = {
    case CommModel.SimpleDraw(discard) =>
      recorder ! RecorderActor.SimpleDiscard(player.position, draw, discard)
      become(proposeDiscarded(state.simpleDraw(player.position, draw, discard), player, discard))
    case CommModel.DrawKong(kong) =>
      recorder ! RecorderActor.SimpleConcealedKong(player.position, draw, kong)
      become(replacement(state.drawKong(player.position, draw, kong), player))
    case CommModel.DrawMahjong =>
      println(s"Mahjong ${player.position} +$draw")
      recorder ! RecorderActor.DrawMahjong(player.position, draw)
      self ! Done
      become(mahjong(state.drawMahjong(player.position, draw)))
  }

  def replacement(state: GameState, player: Player): Receive = debug(state) {
    // TODO: check end of game. Can you declare kong with no more tiles ?
    val replacement = state.nextReplacementTile
    player.actor ! CommModel.ReplacementTile(replacement)
    waitForReplacementResponse(state, player, replacement)
  }

  def waitForReplacementResponse(state: GameState, player: Player, draw: Tile): Receive = {
    case CommModel.SimpleReplacement(discard) =>
      recorder ! RecorderActor.ReplacementDiscard(player.position, draw, discard)
      become(proposeDiscarded(state.simpleReplacement(player.position, draw, discard), player, discard))
    case CommModel.ReplacementKong(kong) =>
      recorder ! RecorderActor.ReplacementConcealedKong(player.position, draw, kong)
      become(replacement(state.replacementKong(player.position, draw, kong), player))
    case CommModel.ReplacementMahjong =>
      recorder ! RecorderActor.ReplacementMahjong(player.position, draw)
      println(s"Mahjong ${player.position} +$draw")
      become(mahjong(state.replacementMahjong(player.position, draw)))
      self ! Done
  }

  def proposeDiscarded(state: GameState, player: Player, discard: Tile): Receive = debug(state) {
    player.nextPlayer.actor ! CommModel.Discarded(true, discard)
    player.nextPlayer.nextPlayer.actor ! CommModel.Discarded(false, discard)
    player.nextPlayer.nextPlayer.nextPlayer.actor ! CommModel.Discarded(false, discard)
    waitForDiscardResponse(state, player, discard, Nil)
  }

  def waitForDiscardResponse(state: GameState, player: Player, discard: Tile, responses: List[DiscardResponse]): Receive = {
    case CommModel.NoClaim =>
      become(handleDiscardResponses(state, player, discard, NoClaim(actorToPlayer(sender)) :: responses))
    case CommModel.MeldedChow(chow) =>
      become(handleDiscardResponses(state, player, discard, MeldedChow(actorToPlayer(sender), chow) :: responses))
    case CommModel.MeldedPung(pung) =>
      become(handleDiscardResponses(state, player, discard, MeldedPung(actorToPlayer(sender), pung) :: responses))
    case CommModel.BigMeldedKong(kong) =>
      become(handleDiscardResponses(state, player, discard, BigMeldedKong(actorToPlayer(sender), kong) :: responses))
    case CommModel.DiscardedMahjong =>
      become(handleDiscardResponses(state, player, discard, DiscardMahjong(actorToPlayer(sender)) :: responses))

  }

  def handleDiscardResponses(state: GameState, previous: Player, discard: Tile, responses: List[DiscardResponse]): Receive = {
    if (responses.length == 3) {
      val ordered = responses.sortBy(_.order(previous))
      ordered.head match {
        case NoClaim(player) =>
          player.actor ! CommModel.ClaimRefused
          ordered.tail.foreach(_.player.actor ! CommModel.ClaimRefused)
          nextDraw(state, previous)
        case DiscardMahjong(player) =>
          player.actor ! CommModel.ClaimAccepted
          ordered.tail.foreach(_.player.actor ! CommModel.ClaimRefused)
          recorder ! RecorderActor.DiscardMahjong(player.position, discard)
          mahjong(state.discardMahjong(player.position, discard))
        case MeldedChow(player, chow) =>
          player.actor ! CommModel.ClaimAccepted
          ordered.tail.foreach(_.player.actor ! CommModel.ClaimRefused)
          waitForMeldedChowDiscard(state, player, discard, chow)
        case MeldedPung(player, pung) =>
          player.actor ! CommModel.ClaimAccepted
          ordered.tail.foreach(_.player.actor ! CommModel.ClaimRefused)
          waitForMeldedPungDiscard(state, player, discard, pung)
        case BigMeldedKong(player, kong) =>
          player.actor ! CommModel.ClaimAccepted
          ordered.tail.foreach(_.player.actor ! CommModel.ClaimRefused)
          recorder ! RecorderActor.BigMeldedKong(player.position, discard, kong)
          replacement(state.bigMeldedKong(player.position, discard, kong), player)
      }
    } else {
      waitForDiscardResponse(state, previous, discard, responses)
    }
  }

  def waitForMeldedChowDiscard(state: GameState, player: Player, claim: Tile, chow: Chow): Receive = {
    case CommModel.MeldedChowDiscard(discard) =>
      recorder ! RecorderActor.MeldedChow(player.position, claim, chow, discard)
      become(proposeDiscarded(state.meldedChow(player.position, claim, chow, discard), player, discard))
  }

  def waitForMeldedPungDiscard(state: GameState, player: Player, claim: Tile, pung: Pung): Receive = {
    case CommModel.MeldedPungDiscard(discard) =>
      recorder ! RecorderActor.MeldedPung(player.position, claim, pung, discard)
      become(proposeDiscarded(state.meldedPung(player.position, claim, pung, discard), player, discard))
  }

  def mahjong(state: GameState): Receive = debug(state) {
    case Nil =>
      // should not reach here
  }

  private def debug(state: GameState)(r: Receive): Receive = {
    println(state) 
    r orElse {
      case Done =>
        system.terminate()
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
  case object Done

  sealed trait DiscardResponse {
    def player: Player
    def order(currentPlayer: Player): Int
  }
  case class NoClaim(override val player: Player) extends DiscardResponse {
    override def order(currentPlayer: Player): Int = 100
  }
  case class MeldedPung(override val player: Player, pung: Pung) extends DiscardResponse {
    override def order(currentPlayer: Player): Int = 50
  }
  case class MeldedChow(override val player: Player, chow: Chow) extends DiscardResponse {
    override def order(currentPlayer: Player): Int = 80
  }
  case class BigMeldedKong(override val player: Player, kong: Kong) extends DiscardResponse {
    override def order(currentPlayer: Player): Int = 20
  }
  case class DiscardMahjong(override val player: Player) extends DiscardResponse {
    override def order(currentPlayer: Player): Int = {
      if (player == currentPlayer.nextPlayer) {
        1
      } else if (player == currentPlayer.nextPlayer.nextPlayer) {
        2
      } else {
        3
      }
    }
  }

  def props(pEast: ActorRef, pSouth: ActorRef, pNorth: ActorRef, pWest: ActorRef, recorder: ActorRef): Props =
    Props(classOf[GameActor], pEast, pSouth, pNorth, pWest, recorder)
}