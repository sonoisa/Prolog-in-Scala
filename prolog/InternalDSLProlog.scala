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
 * Prologの内部DSL用拡張
 */
trait InternalDSLProlog extends Prolog {
  /**
   * IntからNumへの暗黙的変換
   *
   * @param num 整数
   * @return 数値
   */
  implicit def int2Num(num: Int): Num = Num(num)

  /**
   * SymbolからVariableへの暗黙的変換
   *
   * @param symbol 変数名を表す記号
   * @return 変数
   */
  implicit def symbol2Variable(symbol: Symbol)(implicit context: Context): Variable = Var(symbol.name)(context)

  /**
   * StringからPostdefAtomへの暗黙的変換
   *
   * @param name ユーザにより定義されたアトムや関数子の名称
   * @return ユーザにより定義されたアトムや関数子
   */
  implicit def string2PostdefAtom(name: String)(implicit context: Context): PostdefAtom = Atom(name)(context)

  /**
   * Termへの演算追加
   *
   * @param term 項
   * @return 演算の追加された項
   */
  implicit def term2RichTerm(term: Term) = new RichTerm(term)

  /**
   * VariableとしてのSymbolへの演算追加
   *
   * @param symbol 変数名を表す記号
   * @return 演算の追加された項
   */
  implicit def symbol2RichTerm(symbol: Symbol)(implicit context: Context) = new RichTerm(Var(symbol.name)(context))

  /**
   * NumとしてのIntへの演算追加
   *
   * @param num 整数
   * @return 演算の追加された項
   */
  implicit def int2RichTerm(num: Int) = new RichTerm(Num(num))

  /**
   * 演算の追加された項
   *
   * @param term 元の項
   */
  class RichTerm(term: Term) {
    /**
     * 二項演算子 +
     *
     * @param rhs 右辺
     * @return 複合項 ADD(this, rhs)
     */
    def +(rhs: Term): Term = ADD(term, rhs)

    /**
     * 二項演算子 -
     *
     * @param rhs 右辺
     * @return 複合項 SUB(this, rhs)
     */
    def -(rhs: Term): Term = SUB(term, rhs)

    /**
     * 二項演算子 *
     *
     * @param rhs 右辺
     * @return 複合項 MUL(this, rhs)
     */
    def *(rhs: Term): Term = MUL(term, rhs)

    /**
     * 二項演算子 /
     *
     * @param rhs 右辺
     * @return 複合項 DIV(this, rhs)
     */
    def /(rhs: Term): Term = DIV(term, rhs)

    /**
     * 二項演算子 mod
     *
     * @param rhs 右辺
     * @return 複合項 MOD(this, rhs)
     */
    def mod(rhs: Term): Term = MOD(term, rhs)

    /**
     * 単一化を意味する二項演算子 ===
     * Scalaでは = はメソッドとして定義できないため、 === にする。
     *
     * @param rhs 右辺
     * @return 述語 UNIFY(this, rhs)
     */
    def ===(rhs: Term): Term = UNIFY(term, rhs)

    /**
     * 数式評価と単一化を意味する二項演算子 is
     *
     * @param rhs 右辺
     * @return 述語 IS(this, rhs)
     */
    def is(rhs: Term): Term = IS(term, rhs)

    /**
     * 数式評価と等号の充足確認を意味する二項演算子 =:=
     *
     * @param rhs 右辺
     * @return 述語 EQ(this, rhs)
     */
    def =:=(rhs: Term): Term = EQ(term, rhs)

    /**
     * 数式評価と不等号の充足確認を意味する二項演算子 =\=
     *
     * @param rhs 右辺
     * @return 述語 NE(this, rhs)
     */
    def =\=(rhs: Term): Term = NE(term, rhs)

    /**
     * 数式評価と以上の充足確認を意味する二項演算子 >=
     *
     * @param rhs 右辺
     * @return 述語 GE(this, rhs)
     */
    def >=(rhs: Term): Term = GE(term, rhs)

    /**
     * 数式評価と超過の充足確認を意味する二項演算子 >
     *
     * @param rhs 右辺
     * @return 述語 GT(this, rhs)
     */
    def >(rhs: Term): Term = GT(term, rhs)

    /**
     * 数式評価と以下の充足確認を意味する二項演算子 =<
     *
     * @param rhs 右辺
     * @return 述語 LE(this, rhs)
     */
    def =<(rhs: Term): Term = LE(term, rhs)

    /**
     * 数式評価と未満の充足確認を意味する二項演算子 <
     *
     * @param rhs 右辺
     * @return 述語 LT(this, rhs)
     */
    def <(rhs: Term): Term = LT(term, rhs)

    /**
     * リスト要素の連結を意味する二項演算子 ::
     * Scalaでは | にすると左結合になり、リストを表すcons構造にならないため :: にする。
     *
     * @param lhs 左辺
     * @return 複合項 CONS(lhs, this)
     */
    def ::(lhs: Term): Term = CONS(lhs, term)
  }

  /**
   * 節の生成操作の追加された項への暗黙的変換
   *
   * @param postdefCompound ユーザにより定義された複合項
   * @return 節の生成操作の追加された項
   */
  implicit def postdefCompound2RichPostdefCompound(postdefCompound: PostdefCompound) = new {
    /**
     * この複合項を頭部に持つ節を生成し、頭部がこの複合項と単一化される可能性のある節として登録する。
     *
     * @param body 本体部
     * @return 節
     */
    def :-(body: Term*): Clause = {
      val clause = Clause(postdefCompound, body.toList)
      postdefCompound.functor.assertz(clause)
      clause
    }
  }

  /**
   * 節の生成操作の追加された項への暗黙的変換
   *
   * @param postdefAtom ユーザにより定義されたアトム
   * @return 節の生成操作の追加された項
   */
  implicit def postdefAtom2RichPostdefAtom(postdefAtom: PostdefAtom) = new {
    /**
     * このアトムを頭部に持つ節を生成し、頭部がこの複合項と単一化される可能性のある節として登録する。
     *
     * @param body 本体部
     * @return 節
     */
    def :-(body: Term*): Clause = {
      val clause = Clause(postdefAtom, body.toList)
      postdefAtom.assertz(clause)
      clause
    }
  }

  /**
   * 節の生成操作の追加された項への暗黙的変換
   *
   * @param name ユーザにより定義されたアトムの名称
   * @return 節の生成操作の追加された項
   */
  implicit def string2RichPostdefAtom(name: String)(implicit context: Context) = new {
    /**
     * このアトムを頭部に持つ節を生成し、頭部がこの複合項と単一化される可能性のある節として登録する。
     *
     * @param body 本体部
     * @return 節
     */
    def :-(body: Term*): Clause = {
      val atom = Atom(name)(context)
      val clause = Clause(atom, body.toList)
      atom.assertz(clause)
      clause
    }
  }
}
