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
 * 処理
 */
sealed abstract class Command {
  /**
   * 処理を実行する。
   *
   * @param remainingCommands 処理開始時点の継続
   * @param initialEnv クエリに使用された変数の環境
   * @param initialVariableTrail 環境に対して行われた全変更
   * @param callback 探索に成功したときに呼び出される処理
   * @return 処理後の継続
   */
  def execute(remainingCommands: List[Command], initialEnv: ProofEnv, initialVariableTrail: VariableTrail, callback: (ProofEnv, List[Command]) => Unit): List[Command]
}

/**
 * 探索する。
 *
 * @param goals ゴール
 * @param remainingClauses 先頭ゴールに適用するルールの残り
 * @param trail 環境に対して行う変更の記録先
 */
final case class Search(goals: List[TermInstance], remainingClauses: List[Clause], trail: VariableTrail) extends Command {
  /**
   * 探索する。
   *
   * @param remainingCommands 処理開始時点の継続
   * @param initialEnv クエリに使用された変数の環境
   * @param initialVariableTrail 環境に対して行われた全変更
   * @param callback 探索に成功したときに呼び出される処理
   * @return 処理後の継続
   */
  override def execute(remainingCommands: List[Command], initialEnv: ProofEnv, initialVariableTrail: VariableTrail, callback: (ProofEnv, List[Command]) => Unit): List[Command] = {
    goals match {
      case Nil =>
        // 解を発見した。
        callback(initialEnv, remainingCommands)
        remainingCommands
      case goals @ (TermInstance(goal, env) :: remainingGoals) =>
        goal.execute(goals, env, trail, remainingGoals, remainingClauses, remainingCommands)
    }
  }

  override def toString(): String = {
    val sb = new StringBuilder
    sb.append("Search(\n")
    sb.append(goals.mkString("  ", ",\n  ", "\n"))
    sb.append(remainingClauses.mkString("    ", ",\n    ", "\n  "))
    sb.append(trail)
    sb.append(")")
    sb.toString()
  }
}

/**
 * 環境に対して行われた変更を取り消す。
 *
 * @param trail 環境に対して行われた変更
 */
final case class Rollback(trail: VariableTrail) extends Command {
  /**
   * 環境に対して行われた変更を取り消す。
   *
   * @param remainingCommands 処理開始時点の継続
   * @param initialEnv クエリに使用された変数の環境
   * @param initialVariableTrail 環境に対して行われた全変更
   * @param callback 探索に成功したときに呼び出される処理
   * @return 処理後の継続
   */
  override def execute(remainingCommands: List[Command], initialEnv: ProofEnv, initialVariableTrail: VariableTrail, callback: (ProofEnv, List[Command]) => Unit): List[Command] = {
    trail.rollback()  // 環境に対して行われた変更を元に戻す。
    remainingCommands // 処理を継続する。
  }

  override def toString(): String = "Rollback(" + trail.toString() + ")"
}

/**
 * スタックトレースを出力する。
 */
final case class Trace(terms: List[Term], env: Env) extends Command {
  /**
   * スタックトレースを出力する。
   *
   * @param remainingCommands 処理開始時点の継続
   * @param initialEnv クエリに使用された変数の環境
   * @param initialVariableTrail 環境に対して行われた全変更
   * @param callback 探索に成功したときに呼び出される処理
   * @return 処理後の継続
   */
  override def execute(remainingCommands: List[Command], initialEnv: ProofEnv, initialVariableTrail: VariableTrail, callback: (ProofEnv, List[Command]) => Unit): List[Command] = {
    println(terms.mkString("", ", ", ""))
    println(remainingCommands.mkString(" ::\n"))
    println("  env: " + initialEnv)
    println("  variable trail: " + initialVariableTrail)
    println()

    remainingCommands // 処理を継続する。
  }

  override def toString(): String = "Trace(" + terms.mkString("", ", ", "") + ")"
}
