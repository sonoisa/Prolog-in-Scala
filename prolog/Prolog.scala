/*
 * Copyright (c) 2010 Isao Sonobe <sonoisa (AT) muse (DOT) ocn (DOT) ne (DOT) jp>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
package prolog

/**
 * Prolog環境
 */
trait Prolog {
  import Term._
  import Clause._
  import error._

  /**
   * 解を探索する。
   *
   * @param goals ゴール
   * @param callback 探索に成功したときに呼び出される処理
   */
  def prove(goals: Term*)(callback: (ProofEnv, List[Command]) => Unit): Unit = {
    var idInClauseForVar = new scala.collection.mutable.HashMap[Variable, Variable]
    val newGoals = goals.map{ _.assignIdInClauseToVar(idInClauseForVar) }
    val initialVariableTrail = VariableTrail()
    val initialEnv = Env(idInClauseForVar.size, Rollback(initialVariableTrail) :: Nil)
    val initialProofEnv = new ProofEnv(Map.empty ++ idInClauseForVar, initialEnv)
    val goalList = newGoals.toList
    evaluate(
      Search(goalList.map(g => TermInstance(g, initialEnv)),
          goalList match {
            case Nil => Nil
            case goal :: _ => goal.clauses
          },
        initialVariableTrail) ::
        Rollback(initialVariableTrail) :: Nil,
        initialProofEnv, initialVariableTrail, callback)
  }

  /**
   * 探索処理を実行する。
   *
   * @param initialCommands 実行する処理
   * @param initialEnv クエリに使用された変数の環境
   * @param initialVariableTrail 環境に対して行われた全変更
   * @param callback 探索に成功したときに呼び出される処理
   * @return 処理後の継続
   */
  final def evaluate(initialCommands: List[Command], initialEnv: ProofEnv, initialVariableTrail: VariableTrail, callback: (ProofEnv, List[Command]) => Unit) :Unit = {
    def evaluate(commands: List[Command]): Unit = {
      commands match {
        case command :: remainingCommands =>
          // 処理を実行する。
          evaluate(command.execute(remainingCommands, initialEnv, initialVariableTrail, callback))
        case Nil => ; // 処理完了
      }
    }
    evaluate(initialCommands)
  }

  /** スタックトレースを出力する。 */
  val TRACE = PredefAtom("trace", (atom, arguments, goals, env, trail, remainingGoals, remainingClauses, remainingCommands) => {
    Trace(arguments, env) ::
      Search(remainingGoals, clausesToBePossible(remainingGoals), trail) /* 次のゴールへ移る。 */ ::
      remainingCommands
  })

  /** 単一化する述語 */
  val UNIFY = PredefAtom("=", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      // 単一化
      case lhs :: rhs :: Nil => {
        val (succeed, newChangedVariables) = unifyAll(lhs, env, rhs, env)
        if (succeed) {
          val newTrail = trail.createCheckPoint(newChangedVariables)  // 状態のロールバックはremainingCommandsの先頭にあるRollbackで行われる。
          Search(remainingGoals, clausesToBePossible(remainingGoals), newTrail) /* 次のゴールへ移る。 */ :: remainingCommands
        } else {
          remainingCommands // バックトラック
        }
      }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 引数を取得する述語 */
  val ARG = PredefAtom("arg", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case num :: term :: result :: Nil =>
        (env.dereference(num), env.dereference(term)) match {
          case (TermInstance(Num(index), _), TermInstance(Compound(functor, terms), termsEnv)) => {
            if (index > 0 && index <= terms.size) {
              val elem = terms(index - 1)
              val (succeed, newChangedVariables) = unifyAll(result, env, elem, termsEnv)
              if (succeed) {
                val newTrail = trail.createCheckPoint(newChangedVariables)  // 状態のロールバックはremainingCommandsの先頭にあるRollbackで行われる。
                Search(remainingGoals, clausesToBePossible(remainingGoals), newTrail) /* 次のゴールへ移る。 */ :: remainingCommands
              } else {
                remainingCommands // バックトラック
              }
            } else {
              remainingCommands // バックトラック
            }
          }
          case _ => throw new InstantiationError("instantiation_error '%s'.".format(atom.name))
        }
      case _ => throw new InstantiationError("instantiation_error '%s'.".format(atom.name))
    })

  /** 数式を評価する述語 */
  val IS = PredefAtom("is", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case term :: expr :: Nil =>
        val result = Num(calc(expr, env))
        val (succeed, newChangedVariables) = unifyAll(term, env, result, env)
        if (succeed) {
          val newTrail = trail.createCheckPoint(newChangedVariables)  // 状態のロールバックはremainingCommandsの先頭にあるRollbackで行われる。
          Search(remainingGoals, clausesToBePossible(remainingGoals), newTrail) /* 次のゴールへ移る。 */ :: remainingCommands
        } else {
          remainingCommands // バックトラック
        }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 数式を評価し、同値を判定する述語 */
  val EQ = PredefAtom("=:=", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case lhs :: rhs :: Nil => {
        if (calc(lhs, env) == calc(rhs, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 数式を評価し、非同値を判定する述語 */
  val NE = PredefAtom("=\\=", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case lhs :: rhs :: Nil => {
        if (calc(lhs, env) != calc(rhs, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 数式を評価し、以上を判定する述語 */
  val GE = PredefAtom(">=", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case lhs :: rhs :: Nil => {
        if (calc(lhs, env) >= calc(rhs, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 数式を評価し、超過を判定する述語 */
  val GT = PredefAtom(">", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case lhs :: rhs :: Nil => {
        if (calc(lhs, env) > calc(rhs, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 数式を評価し、以下を判定する述語 */
  val LE = PredefAtom("<=", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case lhs :: rhs :: Nil => {
        if (calc(lhs, env) <= calc(rhs, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 数式を評価し、未満を判定する述語 */
  val LT = PredefAtom("<", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case lhs :: rhs :: Nil => {
        if (calc(lhs, env) < calc(rhs, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new SyntaxError("Unbalanced operator '%s'.".format(atom.name))
    })

  /** 変数であることを判定する述語 */
  val VAR = PredefAtom("var", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case term :: Nil => {
        if (isVar(term, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
    })

  /** 変数でないことを判定する述語 */
  val NONVAR = PredefAtom("nonvar", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case term :: Nil => {
        if (isNonvar(term, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
    })

  /** 数値であることを判定する述語 */
  val NUM = PredefAtom("num", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case term :: Nil => {
        if (isNum(term, env))
          Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands // 次のゴールへ移る。
        else
          remainingCommands // バックトラック
      }
      case _ => throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
    })

  /** 標準出力に項の内容を出力する述語 */
  val WRITE = PredefAtom("write", (atom, arguments, _, env, trail, remainingGoals, _, remainingCommands) =>
    arguments match {
      case term :: Nil =>
        println(term.toString(env))
        Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands /* 次のゴールへ移る。 */
      case _ =>
        throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
    }
  )

  /** 定義済みアトムtrue */
  val TRUE = PredefAtom("true", (atom, arguments, _, _, trail, remainingGoals, _, remainingCommands) =>
    if (arguments.isEmpty)
      Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: remainingCommands /* 次のゴールへ移る。 */
    else
      throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
  )

  /** 定義済みアトムfalse */
  val FALSE = PredefAtom("false", (atom, arguments, _, _, _, _, _, remainingCommands) =>
    if (arguments.isEmpty)
      remainingCommands /* バックトラック */
    else
      throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
  )

  /** 定義済みアトムfail */
  val FAIL = PredefAtom("fail", (atom, arguments, _, _, _, _, _, remainingCommands) =>
    if (arguments.isEmpty)
      remainingCommands /* バックトラック */
    else
      throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
  )

  /** カット */
  val CUT = PredefAtom("!", (atom, arguments, _, env, trail, remainingGoals, _, _) => {
    if (arguments.isEmpty)
      // カットの場合、backPointまでスタックを捨て（枝狩り）、次のゴールへ移る。
      Search(remainingGoals, clausesToBePossible(remainingGoals), trail) :: env.backpoint
    else
      throw new RuntimeException("Undefined procedure '%s'.".format(atom.name))
  })

  /** 定義済み述語+ */
  val ADD = PredefAtom("+", (atom, _, _, _, _, _, _, _) => { throw new RuntimeException("Undefined procedure '%s'.".format(atom.name)) })

  /** 定義済み述語- */
  val SUB = PredefAtom("-", (atom, _, _, _, _, _, _, _) => { throw new RuntimeException("Undefined procedure '%s'.".format(atom.name)) })

  /** 定義済み述語* */
  val MUL = PredefAtom("*", (atom, _, _, _, _, _, _, _) => { throw new RuntimeException("Undefined procedure '%s'.".format(atom.name)) })

  /** 定義済み述語/ */
  val DIV = PredefAtom("/", (atom, _, _, _, _, _, _, _) => { throw new RuntimeException("Undefined procedure '%s'.".format(atom.name)) })

  /** 定義済み述語mod */
  val MOD = PredefAtom("mod", (atom, _, _, _, _, _, _, _) => { throw new RuntimeException("Undefined procedure '%s'.".format(atom.name)) })

  /** 定義済み述語. */
  val CONS = PredefAtom(".", (atom, _, _, _, _, _, _, _) => { throw new RuntimeException("Undefined procedure '%s'.".format(atom.name)) })

  /** 定義済みアトム[] */
  val EMPTY = PredefAtom("[]", (atom, _, _, _, _, _, _, _) => { throw new RuntimeException("Undefined procedure '%s'.".format(atom.name)) })

  /**
   * 環境の代入を適用した項を数式として評価した結果を返す。
   * 数式には+と-、*、/が使用できる。
   *
   * @param term 数式
   * @param env 環境
   * @return 評価結果値
   */
  private def calc(term: Term, env: Env): Int = env.dereference(term) match {
    case TermInstance(Num(n), e) => n
    case TermInstance(Compound(ADD, lhs :: rhs :: Nil), e) => calc(lhs, e) + calc(rhs, e)
    case TermInstance(Compound(SUB, lhs :: rhs :: Nil), e) => calc(lhs, e) - calc(rhs, e)
    case TermInstance(Compound(MUL, lhs :: rhs :: Nil), e) => calc(lhs, e) * calc(rhs, e)
    case TermInstance(Compound(DIV, lhs :: rhs :: Nil), e) => {
      val denom = calc(rhs, e)
      if (denom == 0) {
        throw new ArithmeticException("zero divisor.")
      }
      calc(lhs, e) / denom
    }
    case TermInstance(Compound(MOD, lhs :: rhs :: Nil), e) => {
      val denom = calc(rhs, e)
      if (denom == 0) {
        throw new ArithmeticException("zero divisor.")
      }
      calc(lhs, e) % denom
    }
    case TermInstance(t, e) => throw new ArithmeticException("'%s' is not a function.".format(t.toString(e)))
  }
}
