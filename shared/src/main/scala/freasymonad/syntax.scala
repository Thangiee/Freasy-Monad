package freasymonad

import scala.language.implicitConversions
import scala.meta._
import scala.meta.contrib._

private[freasymonad] object syntax {
  implicit class NameSyntax[N <: Name](val n: N) extends AnyVal {
    def asArg: Term.Arg = n.asTerm
    def asPatVarTerm: Pat.Var.Term = Pat.Var.Term(n.asTerm)

    def update(f: String => String): Name = Name.Indeterminate(f(n.value))
    def capitalize: Name = update(_.capitalize)
  }

  implicit class TreeSyntax(val t: Tree) extends AnyVal {
    def ===(that: Tree): Boolean = t.isEqual[Structurally](that)
  }
}

