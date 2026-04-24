package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.languages.{ECMAScriptCompilerStatic, JavaScriptCompiler}
import io.kaitai.struct.{ImportList, RuntimeConfig}

class JavaScriptTranslator(provider: TypeProvider, importList: ImportList, config: RuntimeConfig)
  extends ECMAScriptTranslator(provider, importList, config) {
  override def compilerStatic: ECMAScriptCompilerStatic = JavaScriptCompiler

  // Type-casting is not necessary for JavaScript.
  override def doCast(value: Ast.expr, typeName: DataType): String =
    translate(value)
}
