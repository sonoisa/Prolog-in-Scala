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
 * 変数やアトム、関数子の定義を保持する文脈
 */
class Context {
  /** 変数名に対応する変数 */
  private var varForName: Map[String, Variable] = Map.empty

  /** 名称に対応するアトム、関数子 */
  private var postdefAtomForName: Map[String, PostdefAtom] = Map.empty

  /**
   * 変数名に対応する変数を返す。
   *
   * @param name 名称
   * @return 変数名に対応する変数
   */
  def getVar(name: String): Option[Variable] = varForName.get(name)

  /**
   * 変数名に対応する変数を追加する。
   *
   * @param name 名称
   * @param variable 変数
   */
  def putVar(name: String, variable: Variable): Unit = varForName += (name -> variable)

  /**
   * 名称に対応するユーザにより定義されたアトムや関数子を返す。
   *
   * @param name 名称
   * @return ユーザにより定義されたアトムや関数子
   */
  def getPostdefAtom(name: String): Option[PostdefAtom] = postdefAtomForName.get(name)

  /**
   * 名称に対応するユーザにより定義されたアトムや関数子を追加する。
   *
   * @param name 名称
   * @param atom ユーザにより定義されたアトムや関数子
   * @throws IllegalArgumentException 組み込みのアトムや関数子にある名称の場合
   */
  def putPostdefAtom(name: String, atom: PostdefAtom): Unit = {
    if (PredefAtom.hasAtom(name)) {
      throw new IllegalArgumentException("Cannot overload procedure %s.".format(name))
    }
    postdefAtomForName += (name -> atom)
  }
}
