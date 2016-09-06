package com.thangiee.freasymonad.injector

import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.types.api.Any
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}

class freeInjector extends SyntheticMembersInjector {

  override def needsCompanionObject(source: ScTypeDefinition): Boolean = {
    source.findAnnotationNoAliases("freasymonad.free") != null
  }

  override def injectInners(source: ScTypeDefinition): Seq[String] = {
    source match {
      case obj: ScObject =>
        ScalaPsiUtil.getCompanionModule(obj) match {
          case Some(scTrait: ScTraitImpl) if scTrait.findAnnotationNoAliases("freasymonad.free") != null =>
            val typeAliasName: String = scTrait.aliases.headOption.map(_.name).getOrElse("???")
            val sealTraitName: String = scTrait.getAllInnerClasses().headOption.map(_.getName).getOrElse("???")

            val absFuncs: Seq[ScFunction] = scTrait.functions.filter(_.isAbstractMember)

            val funcsReturnType: Seq[ScType] = absFuncs.map(_.returnType.getOrAny)
            val funcsInnerReturnType: Seq[ScType] = funcsReturnType.collect {
              case tpe: ScParameterizedType => tpe.typeArguments.headOption.getOrElse(Any)
              case _ => Any
            }
            val funcsName: Seq[String] = absFuncs.map(_.name)
            val funcsTypeParamsTxt: Seq[String] = absFuncs.map(_.typeParametersClause.map(_.getText).getOrElse(""))
            val funcsNameWithTp: Seq[String] = (funcsName zip funcsTypeParamsTxt).map{ case (name, tp) => name + tp }
            val funcsParams: Seq[String] = absFuncs.map(_.paramClauses.text)
            val funcsArgs: Seq[String] = absFuncs.map(_.parameters.map(_.name).mkString(", "))

            val grammarADTClasses = (funcsNameWithTp zip funcsParams zip funcsInnerReturnType).map { case ((fName, params), rt) =>
              s"case class ${fName.capitalize}$params extends $sealTraitName[${rt.canonicalText}]"
            }

            val patternMatchCases = (funcsName zip funcsTypeParamsTxt zip funcsArgs).map { case ((fName, tp), args) =>
              s"case ${fName.capitalize}$tp($args) => $fName($args)"
            }

            val funcsToBeImpl = (funcsNameWithTp zip funcsParams zip funcsInnerReturnType).map { case ((fName, params), rt) =>
              s"def $fName$params: M[${rt.canonicalText}]"
            }

            Seq(
              s"""
                |object all extends ${scTrait.name} {
                |  object $sealTraitName {
                |    ${grammarADTClasses.mkString("\n")}
                |  }
                |  trait Interpreter[M[_]] {
                |    val interpreter = new ($sealTraitName ~> M) {
                |      def apply[A](fa: $sealTraitName[A]): M[A] = fa match {
                |        ${patternMatchCases.mkString("\n")}
                |      }
                |    }
                |    ${funcsToBeImpl.mkString("\n")}
                |    def run[A](op: $typeAliasName[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = op.foldMap(interpreter)
                |  }
                |}
              """.stripMargin
            )
          case _ => Seq.empty
        }
      case _ => Seq.empty
    }
  }

}
