package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.format.EnumValueSpec
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.JavaScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, ImportList, RuntimeConfig, Utils}

/**
 * The current implementation similar to TypeScript with the following differences:
 * - Any type/type-cast declarations are removed
 * - No not-null assertion for member access
 * - Frozen objects instead of enums
 * - Nested class/enum declarations instead of namespaces
 * - Output extension is .js instead of .ts
 */
class JavaScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends ECMAScriptCompiler(typeProvider, config) {

  override def static: ECMAScriptCompilerStatic = JavaScriptCompiler
  override def innerClasses: Boolean = true
  override def innerEnums: Boolean = true

  override val translator = new JavaScriptTranslator(typeProvider, importList, config)

  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.js"

  override def classHeader(name: List[String]): Unit = {
    if (name.length > 1) {
      out.puts(s"static ${type2class(name.last)} = class extends ${static.kstructNameFull(config)} {")
    } else {
      out.puts(s"export class ${type2class(name.last)} extends ${static.kstructNameFull(config)} {")
    }

    out.inc
    classPrivateMembers()
    out.puts
  }

  override def classFooter(name: List[String]): Unit =
    universalFooter

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(BigInt, EnumValueSpec)]): Unit = {
    out.puts(s"static ${type2class(enumName)} = Object.freeze({")
    out.inc

    // Name to ID mapping
    enumColl.foreach { case (id, label) =>
      out.puts(s"${Utils.upperUnderscoreCase(label.name)}: ${translator.doIntLiteral(id)},")
    }
    out.puts

    // ID to name mapping
    enumColl.foreach { case (id, label) =>
      val idStr = if (id < 0) {
        "\"" + id.toString + "\""
      } else {
        id.toString
      }
      out.puts(s"""$idStr: "${Utils.upperUnderscoreCase(label.name)}",""")
    }

    out.dec
    out.puts("});")
    out.puts
  }

  override def renderParam(name: String, typeName: String, isOptional: Boolean = false): String =
    name

  override def renderMethodHeader(
     name: String,
     params: Seq[String],
     returnType: Option[String],
     accessModifier: Option[String] = None
   ): String = {
    s"$name(${params.mkString(", ")}) {"
  }

  override def renderVariableDeclaration(
    varName: String,
    varType: String,
    isConst: Boolean = false,
    value: String = ""
  ): String = {
    val assignType = if (isConst) "const" else "let"
    val assignValue = if (value.nonEmpty) s" = $value" else ""
    s"$assignType $varName$assignValue;"
  }

  override def renderMemberDeclaration(
    memberName: String,
    memberType: String,
    isOptional: Boolean = false,
    value: String = ""
  ): String = {
    val assignValue = if (value.nonEmpty) s" = $value" else ""
    s"$memberName$assignValue;"
  }
}

object JavaScriptCompiler extends ECMAScriptCompilerStatic {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JavaScriptCompiler(tp, config)

  override val memberAccess: String = "."

  override def importExternalTypeDeclaration(importList: ImportList, name: String): Unit = {
    importList.add(s"""import { $name } from "./$name.js";""")
  }

  override def importProcessor(importList: ImportList, name: List[String], config: RuntimeConfig): Unit = {
    val procClass = type2class(name.last)
    if (name.length == 1) {
      val path = if (config.javascriptOpaque.isEmpty) "./" else config.javascriptOpaque
      importList.add(s"""import { $procClass } from "$path$procClass.js";""")
    } else {
      val pkgName = if (name.init.isEmpty) "" else name.init.mkString("-")
      importList.add(s"""import { $procClass } from "$pkgName";""")
    }
  }

  override def kaitaiType2NativeType(attrType: DataType, isNullable: Boolean = false, config: RuntimeConfig): String =
    ""

  override def castIfNeeded(expr: String, exprType: DataType, targetType: DataType, config: RuntimeConfig): String =
    expr
}
