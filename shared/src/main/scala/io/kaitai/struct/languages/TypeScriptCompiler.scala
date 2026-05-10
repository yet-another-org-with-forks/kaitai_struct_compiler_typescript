package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{AnyType, ArrayType, BitsType, BooleanType, BytesType, CalcFloatType, CalcIntType, CalcKaitaiStructType, EnumType, FloatType, IntType, KaitaiStreamType, KaitaiStructType, OwnedKaitaiStreamType, StrType, SwitchType, UserType}
import io.kaitai.struct.format.EnumValueSpec
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.TypeScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, ImportList, RuntimeConfig, Utils}

class TypeScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends ECMAScriptCompiler(typeProvider, config) {
  override val static: ECMAScriptCompilerStatic = TypeScriptCompiler
  override val translator: TypeScriptTranslator = new TypeScriptTranslator(typeProvider, importList, config)

  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.ts"

  override def classHeader(name: List[String]): Unit = {
    if (name.length > 1) {
      out.puts(s"export namespace ${static.types2class(name.dropRight(1))} {")
      out.inc
    }

    out.puts(s"export class ${type2class(name.last)} extends ${static.kstructNameFull(config)} {")
    out.inc
    classPrivateMembers()
    out.puts
  }

  override def classFooter(name: List[String]): Unit = {
    universalFooter

    if (name.length > 1) {
      universalFooter
    }
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(BigInt, EnumValueSpec)]): Unit = {
    out.puts(s"export namespace ${static.types2class(curClass)} {")
    out.inc

    out.puts(s"export enum ${type2class(enumName)} {")
    out.inc
    enumColl.foreach { case (id, label) =>
      out.puts(s"${Utils.upperUnderscoreCase(label.name)} = ${translator.doIntLiteral(id)},")
    }
    out.dec
    out.puts("}")

    out.dec
    out.puts("}")

    out.puts
  }

  override def renderParam(name: String, typeName: String, isOptional: Boolean = false): String =
    s"$name${if (isOptional) "?" else ""}: $typeName"

  override def renderMethodHeader(
    name: String,
    params: Seq[String],
    returnType: Option[String],
    accessModifier: Option[String] = None
  ): String = {
    val accessPrefix = accessModifier.map(_ + " ").getOrElse("")
    val returnSuffix = returnType.map(t => s": $t").getOrElse("")
    s"$accessPrefix$name(${params.mkString(", ")})$returnSuffix {"
  }

  override def renderVariableDeclaration(
    varName: String,
    varType: String,
    isConst: Boolean = false,
    value: String = ""
  ): String = {
    val assignType = if (isConst) "const" else "let"
    val assignValue = if (value.nonEmpty) s" = $value" else ""
    s"$assignType $varName: $varType$assignValue;"
  }

  override def renderMemberDeclaration(
    memberName: String,
    memberType: String,
    isOptional: Boolean = false,
    value: String = ""
  ): String = {
    val modifier = if (isOptional) {
      "?"
    } else if (!isOptional && value != "") {
      ""
    } else {
      "!"
    }
    val assignValue = if (value.nonEmpty) s" = $value" else ""
    s"$memberName$modifier: $memberType$assignValue;"
  }
}

object TypeScriptCompiler extends ECMAScriptCompilerStatic {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new TypeScriptCompiler(tp, config)

  // We must use non-null assertions for all member accesses because we cannot detect where they are not needed.
  override val memberAccess: String = "!."

  override def importExternalTypeDeclaration(importList: ImportList, name: String): Unit = {
    importList.add(s"""import { $name } from "./$name.js";""")
  }

  override def importProcessor(importList: ImportList, name: List[String], config: RuntimeConfig): Unit = {
    val procClass = type2class(name.last)
    if (name.length == 1) {
      val path = if (config.typescriptOpaque.isEmpty) "./" else config.typescriptOpaque
      importList.add(s"""import { $procClass } from "$path$procClass.js";""")
    } else {
      val pkgName = if (name.init.isEmpty) "" else name.init.mkString("-")
      importList.add(s"""import { $procClass } from "$pkgName";""")
    }
  }

  override def kaitaiType2NativeType(attrType: DataType, isNullable: Boolean = false, config: RuntimeConfig): String = {
    val baseType = attrType match {
      case CalcIntType => "number"
      case CalcFloatType => "number"
      case _: IntType | _: FloatType | _: BitsType => "number"
      case _: BooleanType => "boolean"
      case _: StrType => "string"
      case _: BytesType => "Uint8Array"
      case t: UserType => userTypeName(t)
      case t: EnumType => enumTypeName(t)
      case AnyType => "any"
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructNameFull(config)
      case at: ArrayType => s"Array<${kaitaiType2NativeType(at.elType, isNullable = false, config)}>"
      case st: SwitchType => kaitaiType2NativeType(st.combinedType, isNullable = false, config)
    }
    if (isNullable) s"$baseType | undefined" else baseType
  }

  override def castIfNeeded(expr: String, exprType: DataType, targetType: DataType, config: RuntimeConfig): String = {
    val targetTypeComb = targetType.asCombined
    if (targetTypeComb != exprType) {
      // In TypeScript, upcasting can be performed implicitly, without the need for explicit conversion.
      //
      // It's not that important whether we detect *all* upcasting scenarios here. If we do not
      // detect one, it only means that an unnecessary (but harmless) type cast will be generated.
      val isUpcast =
        (exprType, targetTypeComb) match {
          case (_, AnyType) => true
          case (_: UserType, KaitaiStructType | CalcKaitaiStructType(_)) => true
          case _ => false
        }
      if (isUpcast) {
        return expr
      }
      s"(($expr) as ${kaitaiType2NativeType(targetType, isNullable = false, config)})"
    } else {
      expr
    }
  }
}
