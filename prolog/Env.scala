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
 * 環境(代入、カット後の戻り位置となる継続)
 *
 * @param backpoint カット後の戻り位置となる継続
 */
abstract class Env(val backpoint: List[Command]) {
  /**
   * 変数に代入を適用した結果を返す。
   *
   * @param variable 代入を適用する変数
   * @return 変数に代入を適用した結果
   */
  def apply(variable: Variable): Option[TermInstance]

  /**
   * 代入を追加する。
   *
   * @param variable 代入を適用する変数
   * @param representation 代入を適用した結果
   */
  def update(variable: Variable, representation: TermInstance): Unit

  /**
   * 代入を削除する。
   *
   * @param variable 削除する代入の変数
   */
  def delete(variable: Variable): Unit

  /**
   * 代入を適用した結果を返す。
   *  
   * @param term 項
   * @return 代入を適用した結果
   */
  def dereference(term: Term): TermInstance
}

/**
 * 代入環境のコンパニオンオブジェクト
 */
object Env {
  /**
   * 環境を生成し、返す。
   *
   * @param size 変数数
   * @param backpoint カット後の戻り位置となる継続
   * @return 代入環境
   */
  def apply(size: Int, backpoint: List[Command]): Env = new EnvImpl(size, backpoint)

  /**
   * 環境(代入、カット後の戻り位置となる継続)
   *
   * @param size 変数数
   * @param backpoint カット後の戻り位置となる継続
   */
  private class EnvImpl(size: Int, backpoint: List[Command]) extends Env(backpoint) {
    /** 代入。インデックスがvariable.idInClauseの要素にその変数への適用結果が格納される。 */
    private var subst = new Array[TermInstance](size)

    override def toString(): String = {
      "%x".format(hashCode) + subst.filter(_ != null).map{ case TermInstance(rep, env) => rep.toSignatureString() + "@" + "%x".format(env.hashCode) }.mkString("(", ", ", ")")
    }
    override def apply(variable: Variable): Option[TermInstance] = {
      val te = subst(variable.idInClause)
      if (te == null) {
        None
      } else {
        Some(te)
      }
    }
    override def update(variable: Variable, representation: TermInstance): Unit = subst(variable.idInClause) = representation
    override def delete(variable: Variable): Unit = subst(variable.idInClause) = null

    override def dereference(term: Term): TermInstance = {
      def repRec(term: Term, env: Env): TermInstance = term match {
        case v @ Variable(_, _, _) => env(v) match {
          case None => TermInstance(term, env)
          case Some(TermInstance(t, e)) => repRec(t, e)
        }
        case _ => TermInstance(term, env)
      }
      repRec(term, this)
    }
  }
}
