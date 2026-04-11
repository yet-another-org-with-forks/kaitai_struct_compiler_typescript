package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.languages.{JavaScriptCompiler, TypeScriptCompilerStatic}
import io.kaitai.struct.{ImportList, RuntimeConfig}

/**
 * The current implementation extends TypeScriptTranslator with the following differences:
 * - Any type/type-cast declarations are removed
 * - No not-null assertion for member access
 */
class JavaScriptTranslator(provider: TypeProvider, importList: ImportList, config: RuntimeConfig)
  extends TypeScriptTranslator(provider, importList, config) {
  override def compilerStatic: TypeScriptCompilerStatic = JavaScriptCompiler

  def this(provider: TypeProvider, importList: ImportList) =
    this(provider, importList, RuntimeConfig())

  override def doCast(value: Ast.expr, typeName: DataType): String =
    translate(value)
}
