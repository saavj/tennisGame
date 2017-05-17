package com.saacole.tennisgame

trait Player {
  def winBall(): Unit
  var score: Int
  var name: String
}

object Player {

  class PlayerInstance(var name: String) extends Player {
    override var score: Int = 0
    override def winBall(): Unit = score = score + 1
  }

  def apply(name: String): Player = new PlayerInstance(name)
}

class TennisGame(player1: Player, player2: Player) {
  def score: String =
    if (player1.score > 3 && player1.score - player2.score > 1) s"${player1.name} won"
    else if (player2.score > 3 && player2.score - player1.score > 1) s"${player2.name} won"
    else if (player1.score > 2 && player2.score > 2) {

      if (player1.score == player2.score) "deuce"
      else if (player1.score - player2.score == 1) s"advantage ${player1.name}"
      else if (player1.score - player2.score == -1) s"advantage ${player2.name}"
      else "other"

    } else s"${parseScore(player1.score)}, ${parseScore(player2.score)}"

  def parseScore(int: Int): String = int match {
    case 0 => "love"
    case 1 => "fifteen"
    case 2 => "thirty"
    case 3 => "forty"
    case _ => "other"
  }
}
