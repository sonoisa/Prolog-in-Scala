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
 * ホーン節
 *
 * @param head 頭部
 * @param body 本体部
 * @param countOfVariable 変数数
 */
abstract class Clause(val head: Term, val body: List[Term], val countOfVariable: Int) {
  /**
   * 本体部の項と環境の組にして返す。
   *
   * @param env 環境
   * @return 本体部の項と環境の組のリスト
   */
  def getBodyInstance(env: Env): List[TermInstance] = body.map(term => TermInstance(term, env))
}

/**
 * ホーン節
 */
object Clause {
  /** プログラム中で節を一意に特定するID */
  private var clauseId = -1

  /**
   * プログラム中で節を一意に特定するIDを返す。
   * 実行する度に0から1つずつカウントアップする。
   *
   * @return プログラム中で節を一意に特定するID
   */
  private def getNextClauseId(): Int = { clauseId += 1; clauseId }

  /**
   * 節を生成し、返す。
   * 頭部と本体部の変数を、節内で変数を一意に特定するIDを持った変数(VariableInClauseImpl)に置き換えたものを、節に保持する。
   *
   * @param head 頭部
   * @param body 本体部
   * @return 節
   */
  def apply(head: Term, body: List[Term]): Clause = {
    // Map[変数, idInClauseが振られた変数]
    var idInClauseForVar = new scala.collection.mutable.HashMap[Variable, Variable]
    val newHead = head.assignIdInClauseToVar(idInClauseForVar)
    val newBody = body.map{ _.assignIdInClauseToVar(idInClauseForVar) }
    new ClauseImpl(newHead, newBody, idInClauseForVar.size, getNextClauseId())
  }

  /**
   * ホーン節
   *
   * @param head 頭部
   * @param body 本体部
   * @param countOfVariable 変数数
   * @param clauseId プログラム中で節を一意に特定するID
   */
  private class ClauseImpl(head: Term, body: List[Term], countOfVariable: Int, val clauseId: Int) extends Clause(head, body, countOfVariable) {
    override def toString(): String = clauseId.toString() + ":" + head.toString() + " :- " + body.mkString("(", ", ", ")")
  }

  /**
   * 先頭のゴールと単一化できる可能性のある節を返す。
   *
   * @param goals ゴール
   * @return 先頭のゴールと単一化できる可能性のある節
   */
  def clausesToBePossible(goals: List[TermInstance]): List[Clause] = goals match {
    case Nil => Nil
    case _ => goals.head.term.clauses
  }
}
