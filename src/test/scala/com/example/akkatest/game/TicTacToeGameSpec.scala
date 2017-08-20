package com.example.akkatest.game

import org.scalatest._

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class TicTacToeGameSpec extends FlatSpec with Matchers {

  "Tic-tac-toe game" should "starts with empty field and X mark" in {
    val game = new TicTacToeGame()

    assert(game.field.forall(row => row.forall(cell => cell.equals(Empty))))
    game.nextMark shouldBe X
    game.result shouldBe None
  }

  it should "be won by left diagonal" in {
    val game = new TicTacToeGame(Vector(
      Vector(X, O, O),
      Vector(Empty, X, Empty),
      Vector(Empty, Empty, X)
    ))

    assert(game.result.contains(Win(X)))
  }

  it should "be won by right diagonal" in {
    val game = new TicTacToeGame(Vector(
      Vector(O, O, X),
      Vector(Empty, X, Empty),
      Vector(X, Empty, Empty)
    ))

    assert(game.result.contains(Win(X)))
  }

  it should "be won by row" in {
    val game = new TicTacToeGame(Vector(
      Vector(X, X, X),
      Vector(O, O, Empty),
      Vector(Empty, Empty, Empty)
    ))

    assert(game.result.contains(Win(X)))
  }

  it should "be won by column" in {
    val game = new TicTacToeGame(Vector(
      Vector(X, O, O),
      Vector(X, Empty, Empty),
      Vector(X, Empty, Empty)
    ))

    assert(game.result.contains(Win(X)))
  }

  it should "be draw if all cells is not empty" in {
    val game = new TicTacToeGame(Vector(
      Vector(X, O, O),
      Vector(O, X, X),
      Vector(X, O, O)
    ))

    assert(game.result.contains(Draw))
  }

  it should "update field and switch figure with turn" in {
    val game = new TicTacToeGame()

    val result = game.makeMove(Move(X, 0, 0))

    assert(result.right.exists(game => game.field == Vector(
      Vector(X, Empty, Empty),
      Vector(Empty, Empty, Empty),
      Vector(Empty, Empty, Empty)
    )))
    assert(result.right.exists(game => game.nextMark == O))
  }

  it should "returns InvalidPlayer when figure is invalid" in {
    val game = new TicTacToeGame(Vector(
      Vector(X, Empty, Empty),
      Vector(Empty, Empty, Empty),
      Vector(Empty, Empty, Empty))
    )
    val result = game.makeMove(Move(X, 1, 0))

    assert(result.left.toOption.contains(InvalidMark))
  }

  it should "returns AlreadyMarked when cell is not empty" in {
    val game = new TicTacToeGame(Vector(
      Vector(X, Empty, Empty),
      Vector(Empty, Empty, Empty),
      Vector(Empty, Empty, Empty))
    )
    val result = game.makeMove(Move(O, 0, 0))

    assert(result.left.toOption.contains(AlreadyMarked))
  }

  it should "returns OutOfRange when turns coordinates more than field size" in {
    val game = new TicTacToeGame()
    val result = game.makeMove(Move(X, 3, 0))

    assert(result.left.toOption.contains(OutOfRange))
  }

  it should "returns OutOfRange when turn coordinates are more than field size" in {
    val game = new TicTacToeGame()
    val result = game.makeMove(Move(X, 3, 0))

    assert(result.left.toOption.contains(OutOfRange))
  }

  it should "returns GameOver when turn coordinates more than field size" in {
    val game = new TicTacToeGame(Vector(
      Vector(X, O, O),
      Vector(X, Empty, Empty),
      Vector(X, Empty, Empty)
    ))
    val result = game.makeMove(Move(O, 3, 0))

    assert(result.left.toOption.contains(GameOver))
  }
}
