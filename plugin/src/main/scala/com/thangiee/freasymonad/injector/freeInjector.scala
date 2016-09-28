package com.thangiee.freasymonad.injector

import com.intellij.psi.PsiClass
import com.thangiee.freasymonad.injector.freeInjector._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.types.result.TypingContext
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}

object freeInjector {
  implicit class RichFunc(val value: ScFunction) {
    val typeParamsTxt: String = value.typeParametersClause.map(_.getText).getOrElse("")
    val paramsTxt    : String = value.paramClauses.text
    val argsText     : String = s"(${value.parameters.map(_.name).mkString(", ")})"

    val returnTypeOrAny: ScType      = value.returnType.getOrAny

    def appendTypeParam(tp: String): String =
      if (typeParamsTxt.isEmpty) s"[$tp]" else s"[$tp, ${typeParamsTxt.tail.dropRight(1)}]"
  }

  implicit class RichScVal(scValue: ScValue) {
    val returnTypeOrAny: ScType = scValue.getType(TypingContext.empty).getOrAny
  }

  implicit class RichScType(scType: ScType) {
    val innerTypes : Seq[ScType] = scType match {
      case tpe: ScParameterizedType => tpe.typeArguments
      case _ => Seq.empty
    }
    val innerTypesTxt    : Seq[String] = innerTypes.map(_.canonicalText)
    val firstInnerTypeTxt: String      = innerTypesTxt.headOption.getOrElse("Any")
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

            val (opsFunc, nonOpsFunc) = scTrait.functions.partition(_.returnTypeOrAny.canonicalText.split('[').head.contains(typeAliasName))

            val (opsVal, nonOpsVal) =
              scTrait.allVals.map(_._1)
                .map(ScalaPsiUtil.nameContext)
                .collect {
                  case v: ScPatternDefinition => v // concrete val
                  case v: ScValueDeclaration  => v // abstract val
                }
                .partition(_.returnTypeOrAny.canonicalText.split('[').head.contains(typeAliasName))

            val implicitInjectParam: String = s"(implicit I: cats.free.Inject[$absPath.$sealedTraitName, F])"
            def Free(A: ScType): String = s"cats.free.Free[F, ${A.firstInnerTypeTxt}]"

            val sealedTraitADT = {
              val caseClasses = opsFunc.map { fn =>
                s"case class ${fn.name.capitalize}${fn.typeParamsTxt}${fn.paramsTxt} extends $sealedTraitName[${fn.returnTypeOrAny.firstInnerTypeTxt}]"
              }
              s"object $sealedTraitName { ${caseClasses.mkString("\n")} }"
            }

            val opsObj = {
              val typeAliasText = typeAlias.map(_.text).map { alias =>
                val i = alias.indexOf("""Free[""") + 5
                s"${alias.substring(0, i)}$absPath.${alias.substring(i, alias.length)}"
              }
              s"""
                 |object ops {
                 |  ${typeAliasText.getOrElse("")}
                 |  ${nonOpsVal.map(_.text).mkString("\n")}
                 |  ${opsVal.map(_.text).mkString("\n")}
                 |  ${nonOpsFunc.map(_.text).mkString("\n")}
                 |  ${opsFunc.map(_.text).mkString("\n")}
                 |}
              """.stripMargin
            }

            val injectOpsObj = {
              val ops = opsFunc.map { fn =>
                s"def ${fn.name}${fn.appendTypeParam("F[_]")}${fn.paramsTxt}$implicitInjectParam: ${Free(fn.returnTypeOrAny)}) = ???"
              }
              val opsValToDef = opsVal.map { v =>
                s"def ${v.declaredNames.mkString("")}[F[_]]: ${Free(v.returnTypeOrAny)} = ???"
              }
              s"""
                 |object injectOps {
                 |  ${nonOpsVal.map(_.text).mkString("\n")}
                 |  ${nonOpsFunc.map(_.text).mkString("\n")}
                 |  ${ops.mkString("\n")}
                 |  ${opsValToDef.mkString("\n")}
                 |}
              """.stripMargin
            }

            val injectClass = {
              val ops = opsFunc.map { fn =>
                s"def ${fn.name}${fn.typeParamsTxt}${fn.paramsTxt}: ${Free(fn.returnTypeOrAny)} = ???"
              }
              val opsVal2 = opsVal.map { v =>
                s"val ${v.declaredNames.mkString("")}: ${Free(v.returnTypeOrAny)} = ???"
              }
              s"""
                 |class Inject[F[_]]$implicitInjectParam {
                 |  ${nonOpsVal.map(_.text).mkString("\n")}
                 |  ${opsVal2.mkString("\n")}
                 |  ${nonOpsFunc.map(_.text).mkString("\n")}
                 |  ${ops.mkString("\n")}
                 |}
              """.stripMargin
            }

            val injectClassCompanion = {
              s"""
                 |object Inject {
                 |  implicit def injectOps[F[_]]$implicitInjectParam: $absPath.Inject[F] = ???
                 |}
               """.stripMargin
            }

            val interpTrait = {
              val funcsToBeImpl: Seq[String] =
                opsFunc.filter(_.isAbstractMember)
                  .map { fn =>
                    s"def ${fn.name}${fn.typeParamsTxt}${fn.paramsTxt}: M[${fn.returnTypeOrAny.firstInnerTypeTxt}]"
                  } ++
                opsVal.collect { case absVal: ScValueDeclaration => absVal }
                  .map { v =>
                    s"def ${v.declaredNames.mkString("")}: M[${v.returnTypeOrAny.firstInnerTypeTxt}]"
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
