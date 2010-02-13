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

import error._

/**
 * 項
 */
sealed abstract class Term {
  /**
   * 環境の代入が適用された項を表す文字列を返す。
   * 循環参照は"**"と出力する。
   *
   * @param env 環境
   * @return 環境の代入が適用された項を表す文字列
   */
  def toString(env: Env): String = {
    def toRepresentationString(term: Term, env: Env, visittedTerms: Set[TermInstance]): String = {
      val te @ TermInstance(rep, repEnv) = env.dereference(term)
      if (visittedTerms.contains(te)) {
        // 循環参照の場合
        "**"
      } else {
        rep match {
          case v @ Variable(_, _, _) => v.toString() + "@" + "%x".format(repEnv.hashCode)
          case Atom(name) => name
          case c @ Compound(functor, terms) =>
            val vt = visittedTerms + te
            functor.name + terms.map(t => toRepresentationString(t, repEnv, vt)).mkString("(", ", ", ")")
          case Num(n) => n.toString
        }
      }
    }
    toRepresentationString(this, env, Set.empty)
  }

  /**
   * 環境の代入が適用された項を表す文字列を返す。
   * 循環参照は"**"と出力する。
   *
   * @param env 環境
   * @return 環境の代入が適用された項を表す文字列
   */
  def toString(env: ProofEnv): String = {
    val TermInstance(t, e) = env(this)
    t.toString(e)
  }

  /**
   * 項の木構造を表す文字列を返す。(デバッグ用)
   * 文字列の親子関係は字下げの深さで表す。
   *
   * @param env 環境
   * @return 項の木構造を表す文字列
   */
  def toDetailString(env: Env): String = toDetailString(0, env, Set.empty)

  /**
   * 項の木構造を表す文字列を返す。(デバッグ用)
   *
   * @param indentLevel 字下げレベル
   * @param env 環境
   * @param visittedTerms 既に出力された項(循環参照を発見するために使用される)
   * @return 項の木構造を表す文字列
   */
  private def toDetailString(indentLevel: Int, env: Env, visittedTerms: Set[(Term, Env)]): String = {
    val sb = new StringBuilder
    sb.append("  " * indentLevel)
    sb.append(toSignatureString)
    env.dereference(this) match { case TermInstance(t, e) => sb.append(" -> %s".format(t.toSignatureString)) }
    sb.append('\n')
    children.foreach(t => sb.append(t.toDetailString(indentLevel + 1, env, visittedTerms)))
    sb.toString
  }

  /**
   * 項を表す文字列を返す。
   * 代入の適用や再帰的に内容を出力することはしない。
   *
   * @return 項を表す文字列
   */
  def toSignatureString(): String

  /**
   * 複合項の場合、子要素を返す。
   *
   * @return 子要素
   */
  def children(): List[Term]

  /**
   * 頭部がこの項と単一化される可能性のある節を返す。
   *
   * @return 頭部がこの項と単一化される可能性のある節
   */
  def clauses(): List[Clause]

  /**
   * 項に含まれる変数をidInClauseForVarで与えられたidInClauseを持つ変数に置き換えた項を返す。
   * もし、変数がidInClauseForVarのキーになかった場合、その変数のidInClauseに0からの連番を振った変数を生成し、idInClauseForVarにその情報を追加する。
   *
   * @param idInClauseForVar Map[変数, idInClauseを振った変数]。キーにない変数が現れた場合、その変数のidInClauseに0からの連番を振った変数を生成し、idInClauseForVarにその情報を追加する。
   */
  def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term

  /**
   * この項をゴールとして処理する。
   *
   * @param goals ゴール
   * @param env このゴールの環境
   * @param trail 環境に対して行う変更の記録先
   * @param remainingGoals 残りのゴール
   * @param remainingClauses 先頭ゴールに適用するルールの残り
   * @param remainingCommands 処理開始時点の継続
   * @return 処理後の継続
   */
  def execute(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
              remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command]

  /**
   * この項とremainingClausesの先頭の節の頭部を単一化する。
   * 単一化できた場合、ゴールの先頭を節の本体部に置き換え、探索する継続を返す。
   * 単一化できなかった場合、次の節を試みる継続を返す。
   * 次の節がない場合は、バックトラックする継続を返す。
   *
   * @param goals ゴール
   * @param env このゴールの環境
   * @param trail 環境に対して行う変更の記録先
   * @param remainingGoals 残りのゴール
   * @param remainingClauses 先頭ゴールに適用するルールの残り
   * @param remainingCommands 処理開始時点の継続
   * @return 処理後の継続
   */
  protected def unify(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
                          remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command] = {
    remainingClauses match {
      case Nil => remainingCommands // 適用できるルールが尽きたらバックトラックする。
      case clause :: newRemainingClauses => {
        val head = clause.head
        val newEnv = Env(clause.countOfVariable, remainingCommands)
        val (succeed, newChangedVariables) = Term.unifyAll(head, newEnv, this, env)
        if (succeed) {
          // 最左ゴールと頭部の単一化に成功した場合は、探索木を1階層降りる。
          val newTrail = trail.createCheckPoint(newChangedVariables)
          val body = clause.getBodyInstance(newEnv)
          val newGoals = body ::: remainingGoals
          Search(newGoals, Clause.clausesToBePossible(newGoals), newTrail) /* 一つ下の階層を探索する。 */ ::
            (newRemainingClauses match {
              case Nil =>
                remainingCommands // 次のルールがない場合はトラックバックする。
              case _ =>
                Rollback(newTrail) /* 一つ下の階層からバックトラックする場合、環境の変更を元に戻す。 */ ::
                  Search(goals, newRemainingClauses, trail) /* 同じ階層の次のルール */ ::
                  remainingCommands
            })
        } else {
          newRemainingClauses match {
            case Nil =>
              remainingCommands // 次のルールがない場合はトラックバックする。
            case _ =>
              Search(goals, newRemainingClauses, trail) /* 同じ階層の次のルール */ ::
                remainingCommands
          }
        }
      }
    }
  }
}

/**
 * 項
 */
object Term {
  /**
   * 単一化する。
   * 単一化に成功した場合は、env1とenv2に最汎単一化代入を追加する。
   *
   * @param term1 単一化する項
   * @param env1 term1の環境(単一化に成功した場合、最汎単一化代入が追加される)
   * @param term2 単一化する項
   * @param env2 term2の環境(単一化に成功した場合、最汎単一化代入が追加される)
   * @return (true:単一化に成功、false:単一化に失敗, 新たに束縛された変数と環境の組)
   */
  def unifyAll(term1: Term, env1: Env, term2: Term, env2: Env): (Boolean, List[TermInstance]) = {
    // 新たに束縛された変数と環境の組
    var changedVariables = List[TermInstance]()

    def unifyTerm(term1: Term, env1: Env, term2: Term, env2: Env): Boolean = {
      val te1 @ TermInstance(rep1, repEnv1) = env1.dereference(term1)
      val te2 @ TermInstance(rep2, repEnv2) = env2.dereference(term2)

      if (rep1.eq(rep2) && repEnv1.eq(repEnv2)) {
        true
      } else {
        (rep1, rep2) match {
          case (Atom(n1), Atom(n2)) if n1 == n2 => true
          case (Num(n), Num(m)) if n == m => true
          case (Compound(p1, terms1), Compound(p2, terms2)) if p1.eq(p2) && terms1.size == terms2.size =>
            unifyTerms(terms1, repEnv1, terms2, repEnv2)
          case (v @ Variable(_, _, _), _) =>
            repEnv1(v) = te2
            changedVariables ::= te1
            true
          case (_, v @ Variable(_, _, _)) =>
            repEnv2(v) = te1
            changedVariables ::= te2
            true
          case _ => false
        }
      }
    }

    /**
     * 項のリストを単一化する。
     * 
     * 前提条件: term1とterm2のサイズが等しい。
     * 前提条件: term1のサイズは1以上。
     */
    def unifyTerms(terms1: List[Term], env1: Env, terms2: List[Term], env2: Env): Boolean = {
      if (terms1.isEmpty) {
        true
      } else if (unifyTerm(terms1.head, env1, terms2.head, env2)) {
        unifyTerms(terms1.tail, env1, terms2.tail, env2)
      } else {
        false
      }
    }

    if (unifyTerm(term1, env1, term2, env2)) {
      (true, changedVariables)
    } else {
      changedVariables.foreach{ case TermInstance(t, e) => e.delete(t.asInstanceOf[Variable] /* 性能のため... */) }
      (false, Nil)
    }
  }

  /**
   * 代入を適用した項が変数であるか返す。
   *
   * @param term 項
   * @param env 環境
   * @return true:変数である。false:変数でない。
   */
  def isVar(term: Term, env: Env): Boolean = env.dereference(term) match {
    case TermInstance(Variable(_, _, _), _) => true
    case _ => false
  }

  /**
   * 代入を適用した項が変数でないか返す。
   *
   * @param term 項
   * @param env 環境
   * @return true:変数でない。false:変数である。
   */
  def isNonvar(term: Term, env: Env): Boolean = !isVar(term, env)

  /**
   * 代入を適用した項が数値であるか返す。
   *
   * @param term 項
   * @param env 環境
   * @return true:数値である。false:数値でない。
   */
  def isNum(term: Term, env: Env): Boolean = env.dereference(term) match {
    case TermInstance(Num(_), _) => true
    case _ => false
  }
}

/**
 * 変数
 *
 * @param name 変数名(オプション)
 * @param id プログラム中で変数を一意に特定するID
 * @param idInClause 節内で変数を一意に特定するID
 */
abstract case class Variable(val name: Option[String], val id: Int, val idInClause: Int) extends Term {
  override def toSignatureString(): String = toString()
  override def children(): List[Term] = Nil
  override def clauses(): List[Clause] = Nil
  override def execute(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
                       remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command] = {
    val TermInstance(rep, repEnv) = env.dereference(this)
    if (Term.isVar(rep, repEnv)) {
      throw new InfiniteLoopException("42.")
    } else {
      rep.execute(goals, env, trail, remainingGoals, remainingClauses, remainingCommands)
    }
  }
}

/**
 * 変数
 */
object Var {
  /** プログラム中で変数を一意に特定するID */
  private var variableId = -1

  /**
   * プログラム中で変数を一意に特定するIDを返す。
   * 実行する度に0から1つずつカウントアップする。
   *
   * @return プログラム中で変数を一意に特定するID
   */
  private def getNextVariableId(): Int = { variableId += 1; variableId }

  /**
   * 未設定を意味する節内で変数を一意に特定するID
   */
  val UNDEFINED_ID = -1

  /**
   * 無名変数を生成し、返す。
   *
   * @return 無名変数
   */
  def apply(): Variable = new VariableImpl(None, getNextVariableId())

  /**
   * 名前付き変数を返す。
   * contextに同名のオブジェクトがある場合は、それを返す。
   * なければ新たにオブジェクト生成し、contextに登録する。
   * ただし、名称が_で開始される場合、無名関数とみなし、contextには登録しない。
   *
   * @param name 名称
   * @param context 変数やアトム、関数子の定義を保持する文脈
   * @return 名前付き変数
   */
  def apply(name: String)(implicit context: Context): Variable = {
    context.getVar(name) match {
      case Some(v) => v
      case None => {
        val newVar = new VariableImpl(Some(name), getNextVariableId())
        if (!name.startsWith("_")) context.putVar(name, newVar)
        newVar
      }
    }
  }

  /**
   * 節内で変数を一意に特定するIDは未設定である変数
   *
   * @param name 変数名(オプション)
   * @param id プログラム中で変数を一意に特定するID
   */
  private final class VariableImpl(name: Option[String], id: Int) extends Variable(name, id, UNDEFINED_ID) {
    override def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term = {
      idInClauseForVar.getOrElse(this, {
          val idInClause = idInClauseForVar.size
          val varWithIdInClause = new VariableInClauseImpl(this, idInClause)
          idInClauseForVar += (this -> varWithIdInClause)
          varWithIdInClause
        })
    }

    override def toString(): String = name.getOrElse("_" + id)
  }

  /**
   * 節内で変数を一意に特定するIDを持った変数
   *
   * @param name 変数名(オプション)
   * @param id プログラム中で変数を一意に特定するID
   * @param idInClause 節内で変数を一意に特定するID
   */
  private final class VariableInClauseImpl(variable: Variable, idInClause: Int) extends Variable(variable.name, variable.id, idInClause) {
    override def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term = this
    override def toString(): String = name.getOrElse("_" + id)
  }
}

/**
 * 複合項
 *
 * @param functor 関数子
 * @param terms 項
 */
abstract case class Compound(val functor: Atom, val terms:List[Term]) extends Term {
  override def toString(): String = functor.toString() + terms.mkString("(", ", ", ")")
  override def toSignatureString(): String = functor.toString()
  override def children(): List[Term] = terms
}

/**
 * ユーザにより定義された関数子に属する複合項
 *
 * @param functor 関数子
 * @param terms 項
 */
abstract class PostdefCompound(override val functor: PostdefAtom, terms: List[Term]) extends Compound(functor, terms)

/**
 * 組込みの関数子に属する複合項
 *
 * @param functor 関数子
 * @param terms 項
 */
abstract class PredefCompound(override val functor: PredefAtom, terms: List[Term]) extends Compound(functor, terms)

/**
 * アトムや関数子
 *
 * @param name 名称
 */
abstract case class Atom(name: String) extends Term {
  override def toSignatureString(): String = name
  override def children(): List[Term] = Nil
  override def toString(): String = name
}

/**
 * アトムや関数子
 */
object Atom {
  /**
   * アトムや関数子を返す。
   * contextに同名のオブジェクトがある場合は、それを返す。
   * なければ生成し、返す。
   *
   * @param name 名称
   * @param context 変数やアトム、関数子の定義を保持する文脈
   * @return アトムや関数子
   */
  def apply(name: String)(implicit context: Context): PostdefAtom = context.getPostdefAtom(name) match {
    case Some(a) => a
    case None => {
      val newAtom = new PostdefAtomWithoutArityImpl(name)
      context.putPostdefAtom(name, newAtom)
      newAtom
    }
  }

  /**
   * アトムやアリティが0の関数子
   * 各アリティに対応する関数子の情報を保持する。
   *
   * @param name 名称
   */
  private final class PostdefAtomWithoutArityImpl(name: String) extends PostdefAtom(name) {
    /** 各アリティに対応する関数子 */
    private var functorForArity: Map[Int, PostdefAtom] = Map.empty

    override def functor(arity: Int): PostdefAtom = {
      if (arity == 0) {
        this
      } else {
        functorForArity.getOrElse(arity, {
            val functorWithArity = new PostdefAtomWithArityImpl(name, this)
            functorForArity += (arity -> functorWithArity)
            functorWithArity
          })
      }
    }
  }

  /**
   * アリティが1以上の関数子
   *
   * @param name 名称
   * @param originalFunctor 各アリティに対応する関数子の情報を保持する関数子
   */
  private final class PostdefAtomWithArityImpl(name: String, originalFunctor: PostdefAtom) extends PostdefAtom(name) {
    override def functor(arity: Int): PostdefAtom = originalFunctor.functor(arity)
  }
}

/**
 * 関数子
 */
object Functor {
  /**
   * アトムや関数子を返す。
   * contextに同名のオブジェクトがある場合は、それを返す。
   * なければ生成し、返す。
   *
   * @param name 名称
   * @param context 変数やアトム、関数子の定義を保持する文脈
   * @return アトムや関数子
   */
  def apply(name: String)(implicit context: Context): PostdefAtom =  Atom(name)(context)
}

/**
 * ユーザにより定義されたアトムや関数子
 *
 * @param name 名称
 */
abstract class PostdefAtom(name: String) extends Atom(name) {
  /**
   * 引数で与えられたアリティを持つ関数子を返す。
   *
   * @param arity アリティ
   * @return 関数子
   */
  def functor(arity: Int): PostdefAtom

  /** この関数子をヘッドに持つホーン節のリスト */
  private var _clauses: List[Clause] = Nil
  
  override def clauses(): List[Clause] = _clauses

  /**
   * この関数子に属する頭部に持つ節を単一化される可能性のあるルールとして追加する。
   * ルールの末尾に追加する。
   * 適用の優先順位が最も低くなる。
   * 
   * @param clause 追加する節
   */
  def assertz(clause: Clause): Unit = {
    clause.head match {
      case c @ Compound(f, _) => assert(f.eq(this))
      case a => assert(a.eq(this))
    }
    _clauses = (clause :: _clauses.reverse).reverse
  }

  /**
   * このアトムを返す。
   *
   * @return このアトム
   */
  def apply(): PostdefAtom = this

  /**
   * この関数子に属する複合項を生成し、返す。
   *
   * @param term 先頭の項
   * @param terms 2番目以降の項
   * @return この関数子に属する複合項
   */
  def apply(term: Term, terms: Term*): PostdefCompound = new PostdefCompoundImpl(functor(terms.size + 1), term :: terms.toList)

  override def execute(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
                       remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command] = {
    unify(goals, env, trail, remainingGoals, remainingClauses, remainingCommands)
  }

  override def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term = this

  /**
   * ユーザにより定義された関数子に属する複合項
   *
   * @param functor 関数子
   * @param terms 項
   */
  private final class PostdefCompoundImpl(functor: PostdefAtom, terms: List[Term]) extends PostdefCompound(functor, terms) {
    override def clauses(): List[Clause] = functor.clauses
    override def execute(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
                         remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command] = {
      unify(goals, env, trail, remainingGoals, remainingClauses, remainingCommands)
    }
    override def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term = {
      new PostdefCompoundImpl(functor, terms.map{ _.assignIdInClauseToVar(idInClauseForVar) })
    }
  }
}

/**
 * 組込みのアトムや関数子
 *
 * @param name 名称
 * @param procedure 評価処理
 */
abstract class PredefAtom(name: String,
    val procedure: (PredefAtom, List[Term], List[TermInstance], Env, VariableTrail, List[TermInstance], List[Clause], List[Command]) => List[Command]
    ) extends Atom(name) {
  /**
   * このアトムや関数子を返す。
   *
   * @return このアトムや関数子
   */
  def apply(): PredefAtom

  /**
   * この関数子に属する複合項を生成し、返す。
   *
   * @param term 先頭の項
   * @param terms 2番目以降の項
   * @return この関数子に属する複合項
   */
  def apply(term: Term, terms: Term*): PredefCompound
}

/**
 * 組込みのアトムや関数子
 */
object PredefAtom {
  /** 名称に対応する組込みのアトムや関数子 */
  private var predefAtomForName: Map[String, PredefAtom] = Map.empty

  /**
   * 名称に対応する組込みのアトムや関数子を返す。
   *
   * @param name 名称
   * @return 名称に対応する組込みのアトムや関数子
   */
  def getAtom(name: String): Option[PredefAtom] = predefAtomForName.get(name)

  /**
   * 名称に対応する組込みのアトムや関数子を保持しているかどうかを返す。
   *
   * @param name 名称
   * @return true:保持している。false:保持していない。
   */
  def hasAtom(name: String): Boolean = predefAtomForName.contains(name)

  /**
   * 組込みのアトムや関数子を生成し、返す。
   *
   * @param name 名称
   * @param procedure 評価処理
   * @return 組込みのアトムや関数子
   * @throws IllegalArgumentException 既に組み込みのアトムや関数子にある名称の場合
   */
  def apply(name: String,
      procedure: (PredefAtom, List[Term], List[TermInstance], Env, VariableTrail, List[TermInstance], List[Clause], List[Command]) => List[Command]
      ): PredefAtom = {
    if (predefAtomForName.contains(name)) {
      throw new IllegalArgumentException("Cannot overload procedure '%s'.".format(name))
    }
    val a = new PredefAtomImpl(name, procedure)
    predefAtomForName += (name -> a)
    a
  }

  /**
   * 組込みのアトムや関数子
   *
   * @param name 名称
   * @param procedure 評価処理
   */
  private final class PredefAtomImpl(name: String,
        procedure: (PredefAtom, List[Term], List[TermInstance], Env, VariableTrail, List[TermInstance], List[Clause], List[Command]) => List[Command]
        ) extends PredefAtom(name, procedure) {
    override def clauses(): List[Clause] = Nil
    override def apply(): PredefAtom = this
    override def apply(term: Term, terms: Term*): PredefCompound = new PredefCompoundImpl(this, term :: terms.toList)
    override def execute(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
                         remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command] = {
      procedure(this, Nil, goals, env, trail, remainingGoals, remainingClauses, remainingCommands)
    }
    override def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term = this
  }

  /**
   * 組込みの関数子に属する複合項
   *
   * @param functor 関数子
   * @param terms 項
   */
  private final class PredefCompoundImpl(functor: PredefAtom, terms: List[Term]) extends PredefCompound(functor, terms) {
    override def clauses(): List[Clause] = Nil
    override def execute(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
                         remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command] = {
      functor.procedure(functor, terms, goals, env, trail, remainingGoals, remainingClauses, remainingCommands)
    }
    override def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term = {
      new PredefCompoundImpl(functor, terms.map{ _.assignIdInClauseToVar(idInClauseForVar) })
    }
  }
}

/**
 * 整数
 *
 * @param number 整数
 */
case class Num(number: Int) extends Term {
  override def toSignatureString(): String = number.toString
  override def children(): List[Term] = Nil
  override def toString(): String = number.toString
  override def clauses(): List[Clause] = Nil
  override def execute(goals: List[TermInstance], env: Env, trail: VariableTrail, remainingGoals: List[TermInstance],
                       remainingClauses: List[Clause], remainingCommands: List[Command]): List[Command] = {
    throw new RuntimeException("Callable expected, but was '%s'.".format(this.toString))
  }
  override def assignIdInClauseToVar(idInClauseForVar: scala.collection.mutable.Map[Variable, Variable]): Term = this
}
