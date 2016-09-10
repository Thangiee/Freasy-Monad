package com.thangiee.freasymonad.injector

import com.intellij.psi.PsiClass
import com.thangiee.freasymonad.injector.freeInjector._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScTypeAlias}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}

object freeInjector {
  implicit class RichFunc(val value: ScFunction) {
    val typeParamsTxt: String = value.typeParametersClause.map(_.getText).getOrElse("")
    val paramsTxt    : String = value.paramClauses.text
    val argsText     : String = s"(${value.parameters.map(_.name).mkString(", ")})"

    val returnTypeOrAny : ScType      = value.returnType.getOrAny
    val returnInnerTypes: Seq[ScType] = returnTypeOrAny match {
      case tpe: ScParameterizedType => tpe.typeArguments
      case _ => Seq.empty
    }
    val returnInnerTypesTxt    : Seq[String] = returnInnerTypes.map(_.canonicalText)
    val firstReturnInnerTypeTxt: String      = returnInnerTypesTxt.headOption.getOrElse("Any")

    def appendTypeParam(tp: String): String =
      if (typeParamsTxt.isEmpty) s"[$tp]" else s"[$tp, ${typeParamsTxt.tail.dropRight(1)}]"
  }
}

class freeInjector extends SyntheticMembersInjector {

  override def needsCompanionObject(source: ScTypeDefinition): Boolean = {
    source.findAnnotationNoAliases("freasymonad.free") != null
  }

  override def injectInners(source: ScTypeDefinition): Seq[String] = {
    source match {
      case obj: ScObject =>
        ScalaPsiUtil.getCompanionModule(obj) match {
          case Some(scTrait: ScTraitImpl) if scTrait.findAnnotationNoAliases("freasymonad.free") != null =>
            val typeAlias: Option[ScTypeAlias] = scTrait.aliases.headOption
            val typeAliasName: String = typeAlias.map(_.name).getOrElse("")

            // absolute path to trait companion obj
            val absPath: String = {
              val path = scTrait.getPath
              if (path.isEmpty) scTrait.name else s"${scTrait.getPath}.${scTrait.name}"
            }

            val sealedTrait: Option[PsiClass] = scTrait.getAllInnerClasses().collectFirst {
              case clazz if clazz.hasModifierProperty("sealed") => clazz
            }
            val sealedTraitName: String = sealedTrait.map(_.getName).getOrElse("")
            val funcs: Seq[ScFunction] = scTrait.functions.filter(_.returnTypeOrAny.canonicalText.contains(typeAliasName))
            val implicitInjectParam: String = s"(implicit I: cats.free.Inject[$absPath.$sealedTraitName, F])"
            def Free(fn: ScFunction): String = s"cats.free.Free[F, ${fn.firstReturnInnerTypeTxt}]"

            val sealedTraitADT = {
              val caseClasses = funcs.map { fn =>
                s"case class ${fn.name.capitalize}${fn.typeParamsTxt}${fn.paramsTxt} extends $sealedTraitName[${fn.firstReturnInnerTypeTxt}]"
              }
              s"object $sealedTraitName { ${caseClasses.mkString("\n")} }"
            }

            val opsObj =
              s"""
                |object ops {
                |  ${typeAlias.map(_.text).getOrElse("")}
                |  ${funcs.map(_.text).mkString("\n")}
                |}
              """.stripMargin

            val injectOpsObj = {
              val ops = funcs.map { fn =>
                s"def ${fn.name}${fn.appendTypeParam("F[_]")}${fn.paramsTxt}$implicitInjectParam: ${Free(fn)}) = ???"
              }
              s"object injectOps { ${ops.mkString("\n")} }"
            }

            val injectClass = {
              val ops = funcs.map { fn =>
                s"def ${fn.name}${fn.typeParamsTxt}${fn.paramsTxt}: ${Free(fn)} = ???"
              }

              s"class Inject[F[_]]$implicitInjectParam { ${ops.mkString("\n")} }"
            }

            val injectClassCompanion = {
              s"""
                 |object Inject {
                 |  implicit def injectOps[F[_]]$implicitInjectParam: $absPath.Inject[F] = ???
                 |}
               """.stripMargin
            }

            val interpTrait = {
              val absFuncs = funcs.filter(_.isAbstractMember)
              val funcsToBeImpl = absFuncs.map { fn =>
                s"def ${fn.name}${fn.typeParamsTxt}${fn.paramsTxt}: M[${fn.firstReturnInnerTypeTxt}]"
              }

              s"""
                |trait Interp[M[_]] {
                |  import cats._
                |  val interpreter: $absPath.$sealedTraitName ~> M = {
                |    def apply[A](fa: $absPath.$sealedTraitName[A]): M[A]
                |  }
                |  ${funcsToBeImpl.mkString("\n")}
                |  def run[A](op: $absPath.ops.$typeAliasName[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = ???
                |}
              """.stripMargin
            }

            val sealedTraitSig = sealedTrait.map(_.getText).getOrElse("")
            Seq(sealedTraitSig, sealedTraitADT, opsObj, injectOpsObj, injectClass, injectClassCompanion, interpTrait)

          case _ => Seq.empty
        }
      case _ => Seq.empty
    }
  }

}
