package com.example.akkatest.game

import com.example.akkatest.game.TicTacToeGame.SIZE

/**
  * Classic tic-tac-toe game with 3x3 field
  *
  * @author Denis Pakhomov.
  * @version 1.0
  */
class TicTacToeGame(val field: Vector[Vector[Mark]]) {

  def this() { this(Vector.fill(SIZE, SIZE)(Empty)) }

  def makeMove(move: Move): Either[GameError, TicTacToeGame] = {

    move match {
      case _ if result.isDefined => Left(GameOver)
      case _ if move.figure != nextMark => Left(InvalidMark)
      case _ if move.x >= SIZE || move.y >= SIZE => Left(OutOfRange)
      case _ if field(move.x)(move.y) != Empty => Left(AlreadyMarked)
      case _ =>
        val updatedField = field.updated(move.x, field(move.x).updated(move.y, move.figure))
        Right(new TicTacToeGame(updatedField))
    }
  }

  def result: Option[GameResult] = {
    if (isWin(X)) Some(Win(X))
    else if (isWin(O)) Some(Win(O))
    else if (field.forall(row => row.forall(cell => !cell.equals(Empty)))) Some(Draw)
    else None
  }

  def nextMark: Mark = {
    field.map(row => row.count(cell => cell != Empty)).sum % 2 match {
      case 0 => X
      case 1 => O
    }
  }

  def isWin(mark: Mark): Boolean = {
    val rowCheck = field.exists(row => row.forall(cell => cell == mark))
    val columnCheck = (0 until SIZE).exists(i => field.forall(col => col(i) == mark))
    val leftDiagonal = (0 until SIZE).forall(i => field(i)(i) == mark)
    val rightDiagonal = (0 until SIZE).forall(i => field(i)(SIZE - 1 - i) == mark)

    rowCheck || columnCheck || leftDiagonal || rightDiagonal
  }
}

object TicTacToeGame {
  val SIZE = 3
}

sealed trait GameResult
case object Draw extends GameResult
case class Win(figure: Mark) extends GameResult

sealed trait Mark
case object X extends Mark
case object O extends Mark
case object Empty extends Mark

sealed trait GameError
case object InvalidMark extends GameError
case object OutOfRange extends GameError
case object AlreadyMarked extends GameError
case object GameOver extends GameError

case class Move(figure: Mark, x: Int, y: Int)