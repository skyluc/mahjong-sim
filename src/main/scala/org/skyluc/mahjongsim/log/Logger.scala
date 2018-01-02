package org.skyluc.mahjongsim.log

import org.skyluc.mahjongsim.model.InternalModel.GameState
import org.skyluc.mahjongsim.runtime.PlayerActor.PlayerView

object Logger {
  final val GAME_LOGS = false
  final val PLAYER_LOGS = true
  final val RECORDER_LOGS = true

  def log(state: GameState): Unit = {
    if (GAME_LOGS)
      println(state)
  }

  def log(state: PlayerView): Unit = {
    if (PLAYER_LOGS)
      println(state)
  }

  def logRecorder(log: String): Unit = {
    if (RECORDER_LOGS)
      println(log)
  }
}