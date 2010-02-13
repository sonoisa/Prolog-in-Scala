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
 * 環境の変更履歴(variable trail法)
 *
 * @param changedVariables 変更された変数と環境の組
 */
final class VariableTrail(var changedVariables: List[TermInstance]) {
  /** 後続の変更履歴(次のチェックポイント) */
  private var successor: Option[VariableTrail] = None

  /**
   * 後続の変更履歴を生成し返す。
   *
   * @param changedVariables 後続の変更履歴に保持する変更された変数と環境の組
   * @return 後続の変更履歴
   */
  def createCheckPoint(changedVariables: List[TermInstance]): VariableTrail = {
    val newVariableTrail = new VariableTrail(changedVariables)
    successor = Some(newVariableTrail)
    newVariableTrail
  }

  /**
   * 後続の変更履歴のものを含めた環境の変更を取り消す。
   */
  def rollback(): Unit = {
    def rollback(trail: VariableTrail): Unit = {
      trail.changedVariables.foreach{ case TermInstance(t, e) => e.delete(t.asInstanceOf[Variable] /* 性能のため... */) }
      trail.changedVariables = Nil
      trail.successor match {
        case None => ;
        case Some(h) =>
          rollback(h)
      }
    }
    rollback(this)
    successor = None
  }

  override def toString(): String = {
    val sb = new StringBuilder
    def toString(trail: VariableTrail): Unit = {
      sb.append('(')
      sb.append(trail.changedVariables.map{ case TermInstance(t, e) => t.toSignatureString() + "@" + "%x".format(e.hashCode) }.mkString(", "))
      sb.append(')')
      trail.successor match {
        case None => ;
        case Some(h) => toString(h)
      }
    }
    toString(this)
    sb.toString
  }
}

/**
 * 環境の変更履歴
 */
object VariableTrail {
  /**
   * 環境の変更履歴を生成し、返す。
   */
  def apply(): VariableTrail = new VariableTrail(Nil)
}
