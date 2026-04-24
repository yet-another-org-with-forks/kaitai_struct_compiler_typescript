package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.languages.{ECMAScriptCompilerStatic, TypeScriptCompiler}
import io.kaitai.struct.{ImportList, RuntimeConfig}

class TypeScriptTranslator(provider: TypeProvider, importList: ImportList, config: RuntimeConfig)
  extends ECMAScriptTranslator(provider, importList, config) {
  protected def compilerStatic: ECMAScriptCompilerStatic = TypeScriptCompiler

  override def doCast(value: Ast.expr, typeName: DataType): String =
    compilerStatic.castIfNeeded(translate(value), AnyType, typeName, config)
}
