package com.thangiee.freasymonad.injector

import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScTypeAlias}
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
            val typeAlias: Option[ScTypeAlias] = scTrait.aliases.headOption
            val typeAliasName: String = typeAlias.map(_.name).getOrElse("")

            val sealedTrait = scTrait.getAllInnerClasses().collectFirst {
              case clazz if clazz.hasModifierProperty("sealed") => clazz
            }
            val sealedTraitName: String = sealedTrait.map(_.getName).getOrElse("")

            val (absFuncs, concreteFuncs): (Seq[ScFunction], Seq[ScFunction]) = scTrait.functions.partition(_.isAbstractMember)

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
              s"case class ${fName.capitalize}$params extends $sealedTraitName[${rt.canonicalText}]"
            }

            val patternMatchCases = (funcsName zip funcsTypeParamsTxt zip funcsArgs).map { case ((fName, tp), args) =>
              s"case ${fName.capitalize}$tp($args) => $fName($args)"
            }

            val funcsToBeImpl = (funcsNameWithTp zip funcsParams zip funcsInnerReturnType).map { case ((fName, params), rt) =>
              s"def $fName$params: M[${rt.canonicalText}]"
            }

            val sealedTraitSig = sealedTrait.map(_.getText).getOrElse("")

            val sealedTraitADT =
              s"""
                 |object $sealedTraitName {
                 |  ${grammarADTClasses.mkString("\n")}
                 |}
               """.stripMargin

            val opsObj =
              s"""
                |object ops {
                |  ${typeAlias.map(_.text).getOrElse("")}
                |  ${absFuncs.map(_.text).mkString("\n")}
                |  ${concreteFuncs.map(_.text).mkString("\n")})
                |}
              """.stripMargin

            val interpTrait =
              s"""
                |trait Interp[M[_]] {
                |  import ${scTrait.getPath}.${scTrait.name}.ops._
                |  import cats._
                |  val interpreter = new ($sealedTraitName ~> M) {
                |    def apply[A](fa: $sealedTraitName[A]): M[A] = fa match {
                |      ${patternMatchCases.mkString("\n")}
                |    }
                |  }
                |  ${funcsToBeImpl.mkString("\n")}
                |  def run[A](op: $typeAliasName[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = op.foldMap(interpreter)
                |}
              """.stripMargin

            Seq(sealedTraitSig, sealedTraitADT, opsObj, interpTrait)

          case _ => Seq.empty
        }
      case _ => Seq.empty
    }
  }

}
