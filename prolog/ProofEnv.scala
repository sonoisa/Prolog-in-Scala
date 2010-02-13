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
 * クエリーの環境
 *
 * @param idInClauseForVar クエリーのゴールに使用された変数に対応する内部変数(節内で変数を一意に特定するIDを持った変数)
 * @env クエリーのゴールの環境
 */
class ProofEnv(val idInClauseForVar: Map[Variable, Variable], val env: Env) {
  /**
   * クエリーのゴールに使用された変数に対応する内部変数を返す。
   * なければ、引数で与えられた項を返す。
   *
   * @param term 探す項
   * @return 引数で与えられた項(変数)に対応する内部変数と環境の組
   */
  def apply(term: Term): TermInstance = term match {
    case v @ Variable(_, _, _) => idInClauseForVar.get(v) match {
      case None => TermInstance(term, env)
      case Some(v) => env.dereference(v)
    }
    case _ => TermInstance(term, env)
  }

  override def toString(): String = env.toString()
}
