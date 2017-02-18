package freasymonad

import scala.language.implicitConversions
import scala.meta._
import scala.meta.contrib._

private[freasymonad] object syntax {
  implicit class NameSyntax[N <: Name](val n: N) extends AnyVal {
    def asTerm: Term.Name = Term.Name(n.value)
    def asType: Type.Name = Type.Name(n.value)
    def asArg: Term.Arg = asTerm
    def asCtorRef: Ctor.Ref.Name = Ctor.Ref.Name(n.value)
    def asPatVarTerm: Pat.Var.Term = Pat.Var.Term(n.asTerm)

    def update(f: String => String): Name = Name.Indeterminate(f(n.value))
    def capitalize: Name = update(_.capitalize)
  }

  implicit class TreeSyntax(val t: Tree) extends AnyVal {
    def ===(that: Tree): Boolean = t.equal[Structurally](that)
  }
}

