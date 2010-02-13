import prolog._

object PrologInScalaSample extends InternalDSLProlog {
  case class Succeed(env: ProofEnv, continuation: List[Command]) extends Exception

  def main(args: Array[String]) {
    tak
    queen
    primes
    knight
  }

  def tak() {
    implicit val context = new Context

    val tak = Functor("tak")

    tak('X, 'Y, 'Z, 'Z) :- (
      'X =< 'Y, CUT, 'Z === 'A
      )
    tak('X, 'Y, 'Z, 'A) :- (
      'X1 is 'X - 1,
      'Y1 is 'Y - 1,
      'Z1 is 'Z - 1,
      tak('X1, 'Y, 'Z, 'A1),
      tak('Y1, 'Z, 'X, 'A2),
      tak('Z1, 'X, 'Y, 'A3),
      tak('A1, 'A2, 'A3, 'A)
      )

    // ?- tak(4, 2, 0, X).
    var beginTime = System.nanoTime
    val answer1 = Var("answer1")
    prove(tak(4, 2, 0, answer1)) {
      (env, continuation) => println("解: " + answer1.toString(env))
    }
    println("時間: " + (System.nanoTime - beginTime) / 1000000000.0)

    // ?- tak(7, 5, 1, X).
    beginTime = System.nanoTime
    val answer2 = Var("answer2")
    prove(tak(7, 5, 1, answer2)) {
      (env, continuation) => println("解: " + answer2.toString(env))
    }
    println("時間: " + (System.nanoTime - beginTime) / 1000000000.0)

    // ?- tak(18, 12, 6, X).
    beginTime = System.nanoTime
    val answer3 = Var("answer3")
    prove(tak(18, 12, 6, answer3)) {
      (env, continuation) => println("解: " + answer3.toString(env))
    }
    println("時間: " + (System.nanoTime - beginTime) / 1000000000.0)
  }

  def queen() {
    implicit val context = new Context

    val queen = Functor("queen")
    val queen2 = Functor("queen2")
    val qperm = Functor("qperm")
    val qdelete = Functor("qdelete")
    val safe = Functor("safe")
    val nodiag = Functor("nodiag")

    queen('Data, 'Out) :- queen2('Data, EMPTY, 'Out)
    queen2(EMPTY, '_, EMPTY) :- ()
    queen2('H :: 'T, 'History, 'Q :: 'M) :- (
            qdelete('Q, 'H, 'T, 'L1),
            nodiag('History, 'Q, 1),
            queen2('L1, 'Q :: 'History, 'M)
            )
    qperm(EMPTY, EMPTY) :- ()
    qperm('X :: 'Y, 'U :: 'V) :- (
            qdelete('U, 'X, 'Y, 'Z),
            qperm('Z, 'V)
            )
    qdelete('A, 'A, 'L, 'L) :- ()
    qdelete('X, 'A, 'H :: 'T, 'A :: 'R) :- qdelete('X, 'H, 'T, 'R)
    safe(EMPTY) :- ()
    safe('N :: 'L) :- (
            nodiag('L, 'N, 1),
            safe('L)
            )
    nodiag(EMPTY, '_, '_) :- ()
    nodiag('N :: 'L, 'B, 'D) :- (
            'D =\= ('N - 'B),
            'D =\= ('B - 'N),
            'D1 is ('D + 1),
            nodiag('L, 'B, 'D1)
            )

    val result = Var("result")
    var beginTime = System.nanoTime
    prove(queen(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: EMPTY, result)) {
      (env, commands) => println("解: " + result.toString(env))
    }
    println("時間: " + (System.nanoTime - beginTime) / 1000000000.0)
  }

  def primes() {
    implicit val context = new Context

    val primes = Functor("primes")
    val integers = Functor("integers")
    val sift = Functor("sift")
    val remove = Functor("remove")

    primes('Limit, 'Ps) :- (
            integers(2, 'Limit, 'Is),
            sift('Is, 'Ps)
            )
    integers('Low, 'High, 'Low :: 'Rest) :- (
            'Low =< 'High, CUT,
            'M is ('Low + 1),
            integers('M, 'High, 'Rest)
            )
    integers('_, '_, EMPTY) :- ()
    sift(EMPTY, EMPTY) :- ()
    sift('I :: 'Is, 'I :: 'Ps) :- (
            remove('I, 'Is, 'Ne),
            sift('Ne, 'Ps)
            )
    remove('_, EMPTY, EMPTY) :- ()
    remove('P, 'I :: 'Is, 'Nis0) :- (
            ('I mod 'P) =\= 0,  // 'i mod 'p =\= 0 は 'i mod ('p =\= 0) と解釈されることに注意
            CUT,
            'Nis0 === ('I :: 'Nis),
            remove('P, 'Is, 'Nis)
            )
    remove('P, '_ :: 'Is, 'Nis) :- remove('P, 'Is, 'Nis)

    val result = Var("result")
    var beginTime = System.nanoTime
    prove(primes(98, result)) {
      (env, commands) => println("解: " + result.toString(env))
    }
    println("時間: " + (System.nanoTime - beginTime) / 1000000000.0)
  }

  def knight() {
    implicit val context = new Context

    val board = Functor("board")
    val row = Functor("row")
    val cell = Functor("cell")

    cell('RowIndex, 'ColumnIndex, 'Board, 'Cell) :- (
            ARG('RowIndex, 'Board, 'Row),
            ARG('ColumnIndex, 'Row, 'Cell)
            )

    val knight5 = Functor("knight5")
    val tour = Functor("tour")
    val next = Functor("next")
    val move = Functor("move")

    knight5('Tour) :- (
            'Board === board(
              row('_, '_, '_, '_, '_),
              row('_, '_, '_, '_, '_),
              row('_, '_, '_, '_, '_),
              row('_, '_, '_, '_, '_),
              row('_, '_, '_, '_, '_)
              ),
            tour(1, 1, 25, 'Tour, 'Board)
            )
    tour('I, 'J, 'N, ('I - 'J) :: 'Tour, 'Board) :- (
            move('I, 'J, 'Board),
            'N1 is 'N - 1,
            next('I, 'J, 'I1, 'J1),
            tour('I1, 'J1, 'N1, 'Tour, 'Board)
            )
    tour('I, 'J, 1, ('I - 'J) :: EMPTY, 'Board) :- move('I, 'J, 'Board)
    move('I, 'J, 'Board) :- (
            cell('I, 'J, 'Board, 'Cell),
            VAR('Cell),
            'Cell === "v"
            )
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I - 2, 'J1 is 'J - 1)
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I - 2, 'J1 is 'J + 1)
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I - 1, 'J1 is 'J - 2)
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I - 1, 'J1 is 'J + 2)
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I + 1, 'J1 is 'J - 2)
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I + 1, 'J1 is 'J + 2)
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I + 2, 'J1 is 'J - 1)
    next('I, 'J, 'I1, 'J1) :- ('I1 is 'I + 2, 'J1 is 'J + 1)

    val result = Var("result")
    var beginTime = System.nanoTime
    try {
      prove(knight5(result)) {
        (env, commands) => throw new Succeed(env, commands)
      }
      println("解なし")
    } catch {
      case e : Succeed => e match { case Succeed(env, commands) => println("解: " + result.toString(env)) }
    }
    println("時間: " + (System.nanoTime - beginTime) / 1000000000.0)
  }
}
