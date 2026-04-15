package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.TypeScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, ExternalType, ImportList, RuntimeConfig, Utils}

import scala.collection.mutable.ListBuffer

class TypeScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalDoc
    with UniversalFooter
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FetchInstances
    with EveryWriteIsExpression
    with GenericChecks
    with SwitchIfOps {

  protected def static: TypeScriptCompilerStatic = TypeScriptCompiler
  protected def memberAccess: String = static.memberAccess
  protected def kstructName: String = static.kstructName
  protected def kstreamName: String = static.kstreamName

  override val translator: TypeScriptTranslator = new TypeScriptTranslator(typeProvider, importList, config)

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "  "

  override def innerClasses: Boolean = false
  override def innerEnums: Boolean = false

  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.ts"

  override def results(topClass: ClassSpec): Map[String, String] =
    Map(outFileName(topClass.nameAsStr) -> (outHeader.result + outImports(topClass) + out.result))

  override def outImports(topClass: ClassSpec): String =
    importList.toList.distinct.mkString("", "\n", "\n\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts
    importList.add(s"""import { $kstructName, $kstreamName } from "kaitai-struct";""")
  }

  override def fileFooter(name: String): Unit = {
    out.puts
    out.puts(s"export default ${type2class(name)};")
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit = {
    val className = type2class(extType.name.head)
    static.importClass(importList, List(className))
  }

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

  protected def classPrivateMembers(): Unit = {
    val isHybrid = typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) => true
      case _ => false
    }

    if (isHybrid)
      out.puts(renderMemberDeclaration("#_is_le", "boolean", isOptional = true))

    if (config.readStoresPos)
      out.puts(renderMemberDeclaration("_debug", s"Record<string, $kstructName.Debug>", isOptional = false, "{}"))
  }

  override def classFooter(name: List[String]): Unit = {
    universalFooter

    if (name.length > 1) {
      universalFooter
    }
  }

  override def classConstructorHeader(
     name: List[String],
     parentClassName: DataType,
     rootClassName: List[String],
     isHybrid: Boolean,
     params: List[ParamDefSpec]
   ): Unit = {
    val mainParamsList = List(
      renderParam("_io", tsType(KaitaiStreamType)),
      renderParam("_parent", tsType(parentClassName), isOptional = true),
      renderParam("_root", tsType(CalcUserType(rootClassName, None)), isOptional = true),
      if (isHybrid) renderParam("_is_le", "boolean", isOptional = true) else ""
    )
    val additionalParamsList = params.map((p) => renderParam(paramName(p.id), tsType(p.dataType), isOptional = true));
    val constructorArgs = (mainParamsList ++ additionalParamsList).filter(_.nonEmpty).mkString(", ")

    out.puts(s"constructor($constructorArgs) {")
    out.inc
    out.puts("super(_io);")
    out.puts("this._parent = _parent;")

    if (name == rootClassName) {
      out.puts("this._root = _root ?? this;")
    } else {
      out.puts("this._root = _root;")
    }

    if (isHybrid)
      out.puts("this.#_is_le = _is_le;")

    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    out.puts
  }

  override def classConstructorFooter: Unit = {
    universalFooter
  }

  override def runRead(name: List[String]): Unit = {
    out.puts("this._read();")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if (this.#_is_le === true) {")
    out.inc
    out.puts("this._readLe();")
    out.dec
    out.puts("} else if (this.#_is_le === false) {")
    out.inc
    out.puts("this._readBe();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts(s"throw new $kstreamName.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def runWriteCalc(): Unit = {
    out.puts
    out.puts("if (this.#_is_le === true) {")
    out.inc
    out.puts("this._writeLe();")
    out.dec
    out.puts("} else if (this.#_is_le === false) {")
    out.inc
    out.puts("this._writeBe();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts(s"throw new $kstreamName.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => Utils.upperCamelCase(e.toSuffix)
      case None => ""
    }
    out.puts(renderMethodHeader(s"_read$suffix", Nil, Some("void")))
    out.inc
  }

  override def readFooter(): Unit = {
    if (config.readWrite) {
      out.puts("this._dirty = false;")
    }
    universalFooter
  }

  override def fetchInstancesHeader(): Unit = {
    out.puts
    out.puts(renderMethodHeader("_fetchInstances", Nil, Some("void")))
    out.inc
  }

  override def fetchInstancesFooter(): Unit =
    universalFooter

  override def attrInvokeFetchInstances(baseExpr: Ast.expr, exprType: DataType, dataType: DataType): Unit = {
    out.puts(s"${expression(baseExpr)}${memberAccess}_fetchInstances();")
  }

  override def attrInvokeInstance(instName: InstanceIdentifier): Unit = {
    out.puts(s"this.${publicMemberName(instName)};")
  }

  override def writeHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    out.puts
    endian match {
      case Some(e) =>
        out.puts(renderMethodHeader(s"_write_Seq${Utils.upperUnderscoreCase(e.toSuffix)}", Nil, Some("void"), Some("private")))
        out.inc
      case None =>
        val args = List(renderParam("io", tsType(KaitaiStreamType), isOptional = true))
        out.puts(renderMethodHeader("_write_Seq", args, Some("void")))
        out.inc
        out.puts("super._write_Seq(io);")
    }
  }

  override def checkHeader(): Unit = {
    out.puts
    out.puts(renderMethodHeader("_check", Nil, Some("void")))
    out.inc
  }

  override def checkFooter(): Unit = {
    out.puts("this._dirty = false;")
    universalFooter
  }

  override def writeInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(renderMethodHeader(s"_write${idToSetterStr(instName)}", Nil, Some("void"), Some("private")))
    out.inc
    instanceClearWriteFlag(instName)
  }

  override def writeInstanceFooter(): Unit =
    universalFooter

  override def checkInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (this.#_enabled${idToSetterStr(instName)}) {")
    out.inc
  }

  override def checkInstanceFooter(): Unit =
    condIfFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    // At the time of writing, `_root` and `_parent` are considered non-nullable, but in reality
    // they can always be `null`.
    val isNullableCorrected =
      if (attrName == RootIdentifier || attrName == ParentIdentifier) {
        true
      } else {
        isNullable
      }
    attrName match {
      case IoIdentifier => // already declared in class header
      case _ =>
        out.puts(renderMemberDeclaration(s"${publicMemberName(attrName)}", tsType(attrType, isNullableCorrected), isNullableCorrected))
    }
  }

  override def attributeDeclarationDoc(id: Identifier, doc: DocSpec): Unit = {
    universalDoc(doc)
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = { }

  override def attributeSetter(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = { }

  override def attributeDoc(id: Identifier, doc: DocSpec): Unit = { }

  override def attrSetProperty(base: Ast.expr, propName: Identifier, value: String): Unit = {
    out.puts(s"${expression(base)}.${publicMemberName(propName)} = $value;")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    // JSDoc docstring style: https://jsdoc.app/about-getting-started
    out.puts
    out.puts("/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    // https://jsdoc.app/tags-see
    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", s"@see $text")
      case UrlRef(url, text) =>
        out.putsLines(" * ", s"@see {@link $url|$text}")
    }

    out.puts(" */")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (this.#_is_le) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, rep: RepeatSpec): String = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName.$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName.processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) expression(rotValue) else s"8 - (${expression(rotValue)})"
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        static.importClass(importList, name)
        val procClass = type2class(name.last)
        val procVarName = s"_process_${idToStr(varSrc)}"
        out.puts(s"const $procVarName = new $procClass(${args.map(expression).mkString(", ")});")
        s"$procVarName.decode($srcExpr)"
    }
  }

  override def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dataType: BytesType, exprTypeOpt: Option[DataType]): Unit = {
    val srcExpr = expression(Identifier.itemExpr(varSrc, rep))
    val expr = proc match {
      case ProcessXor(xorValue) =>
        val argStr = if (translator.inSubIOWriteBackHandler) "_processXorArg" else expression(xorValue)
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName.$procName($srcExpr, $argStr)"
      case ProcessZlib =>
        s"$kstreamName.unprocessZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val argStr = if (translator.inSubIOWriteBackHandler) "_processRotateArg" else expression(rotValue)
        val rotateExpr = if (!isLeft) argStr else s"8 - ($argStr)"
        s"$kstreamName.processRotateLeft($srcExpr, $rotateExpr, 1)"
      case ProcessCustom(name, args) =>
        val procName = s"_process_${idToStr(varSrc)}"
        if (!translator.inSubIOWriteBackHandler) {
          static.importClass(importList, name)
          val procClass = type2class(name.last)
          out.puts(s"const $procName = new $procClass(${args.map(expression).mkString(", ")});")
        }
        s"$procName.encode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, isRaw = false, dataType, dataType)
  }

  override def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"const _processXorArg = ${expression(xorValue)};")
      case ProcessRotate(_, rotValue) =>
        out.puts(s"const _processRotateArg = ${expression(rotValue)};")
      case ProcessZlib => // no process arguments
      case ProcessCustom(name, args) =>
        static.importClass(importList, name)
        val procClass = type2class(name.last)
        out.puts(s"const _process_${idToStr(varSrc)} = new $procClass(${args.map(expression).mkString(", ")});")
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val ioName = s"_io_${idToStr(varName)}"
    val args = getRawIdExpr(varName, rep)
    out.puts(s"const $ioName = new $kstreamName($args);")
    ioName
  }

  override def allocateIOFixed(varName: Identifier, size: String): String = {
    val ioName = s"_io_${idToStr(varName)}"
    out.puts(s"const $ioName = new $kstreamName(new Uint8Array($size));")
    ioName
  }

  override def exprIORemainingSize(io: String): String =
    s"$io.size - $io.pos"

  override def subIOWriteBackHeader(subIO: String, rep: RepeatSpec, process: Option[ProcessExpr]): String = {
    val parentIoName = "parent"
    out.puts(s"$subIO.writeBackHandler = new $kstreamName.WriteBackHandler(_pos2, ($parentIoName) => {")
    out.inc
    translator.inSubIOWriteBackHandler = true
    parentIoName
  }

  override def subIOWriteBackFooter(subIO: String): Unit = {
    translator.inSubIOWriteBackHandler = false
    out.dec
    out.puts("});")
  }

  override def addChildIO(io: String, childIO: String): Unit =
    out.puts(s"$io.addChildStream($childIO);")

  protected def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[$memberName.length - 1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"const io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"const _pos = $io.pos;")

  override def pushPosForSubIOWriteBackHandler(io: String): Unit =
    out.puts(s"const _pos2 = $io.pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def seekRelative(io: String, relPos: String): Unit =
    out.puts(s"$io.seek($io.pos + ($relPos));")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, io: Option[String], rep: RepeatSpec): Unit = {
    val debugName = attrDebugName(attrId, rep, end = false)

    val ioProps = io match {
      case None => ""
      case Some(x) => s"start: $x.pos, ioOffset: $x.byteOffset"
    }

    val enumNameProps = attrType match {
      case t: EnumType => s"""enumName: "${static.types2class(t.enumSpec.get.name)}""""
      case _ => ""
    }

    out.puts(s"$debugName = { $ioProps${if (ioProps != "" && enumNameProps != "") ", " else ""}$enumNameProps };")
  }

  override def attrDebugArrInit(id: Identifier, attrType: DataType): Unit =
    out.puts(s"this._debug.${idToStr(id)}.arr = [];")

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    val debugName = attrDebugName(attrId, rep, end = true)
    out.puts(s"$debugName.end = $io.pos;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter: Unit = universalFooter

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit =
    out.puts(s"${privateMemberName(id)} = [];")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts("let i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    out.puts(s"for (let i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def condRepeatCommonHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"for (let i = 0; i < ${privateMemberName(id)}.length; i++) {")
    out.inc
  }

  override def condRepeatExprFooter: Unit = universalFooter

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("{")
    out.inc
    out.puts(renderVariableDeclaration(translator.doName(Identifier.ITERATOR), tsType(dataType)))
    out.puts("let i = 0;")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    if (isRaw) {
      val tmpName = translator.doName(Identifier.ITERATOR2)
      out.puts(s"const $tmpName = $expr;")
      out.puts(s"${privateMemberName(id)}.push($tmpName);")
    } else {
      val tmpName = translator.doName(Identifier.ITERATOR)
      out.puts(s"$tmpName = $expr;")
      out.puts(s"${privateMemberName(id)}.push($tmpName);")
    }
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"const $id = $expr;")

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }

  override def blockScopeFooter: Unit = universalFooter

  override def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io.readBytesTerm($term, $include, $consume, $eosError)"
        } else {
          s"$io.readBytesTermMulti(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val (parent, root) = if (t.isExternal(typeProvider.nowClass)) {
          ("undefined", "undefined")
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "undefined"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          (parent, "this._root")
        }
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => ", this.#_is_le"
          case _ => ""
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${static.tsUserTypeName(t)}($io, $parent, $root$addEndian$addParams)"
    }
  }

  override def createSubstreamFixedSize(id: Identifier, blt: BytesLimitType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String = {
    val ioName = s"_io_${idToStr(id)}"
    handleAssignmentTempVar(KaitaiStreamType, ioName, s"$io.substream(${translator.translate(blt.size)})")
    ioName
  }

  override def extraRawAttrForUserTypeFromBytes(id: Identifier, ut: UserTypeFromBytes, condSpec: ConditionalSpec): List[AttrSpec] = {
    if (config.zeroCopySubstream) {
      ut.bytes match {
        case BytesLimitType(sizeExpr, None, _, None, None) =>
          // substream will be used, no need for store raws
          List()
        case _ =>
          // buffered implementation will be used, fall back to raw storage
          super.extraRawAttrForUserTypeFromBytes(id, ut, condSpec)
      }
    } else {
      // zero-copy streams disabled, fall back to raw storage
      super.extraRawAttrForUserTypeFromBytes(id, ut, condSpec)
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) if terminator.forall(term => padByte != (term.last & 0xff)) =>
        s"$kstreamName.bytesStripRight($expr0, $padByte)"
      case _ => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName.bytesTerminate($expr1, $t, $include)"
        } else {
          s"$kstreamName.bytesTerminateMulti($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    out.puts(s"$id._read();")
  }

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    out.puts("try {")
    out.inc
    tryBlock()
    out.dec
    out.puts("} finally {")
    out.inc
    finallyBlock()
    out.dec
    out.puts("}")
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: BooleanType | _: EnumType | _: StrType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    out.puts(s"switch (${expression(on)}) {")
    out.inc
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit =
    switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}: {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
    out.puts("}")
  }

  override def switchElseStart(): Unit = {
    out.puts("default: {")
    out.inc
  }

  override def switchEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"const ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  private def switchCmpExpr(condition: Ast.expr): String =
    expression(Ast.expr.Compare(NAME_SWITCH_ON, Ast.cmpop.Eq, condition))

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    out.puts("else {")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    // Actually instance members always nullable
    out.puts(renderMemberDeclaration(s"#${idToStr(attrName)}", tsType(attrType, isNullable = true), isOptional = true))
  }

  override def instanceWriteFlagDeclaration(attrName: InstanceIdentifier): Unit = {
    out.puts(renderMemberDeclaration(s"#_shouldWrite${idToSetterStr(attrName)}", "boolean", isOptional = false, "false"))
    out.puts(renderMemberDeclaration(s"#_enabled${idToSetterStr(attrName)}", "boolean", isOptional = false, "true"))
  }

  override def instanceSetWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"this.#_shouldWrite${idToSetterStr(instName)} = this.#_enabled${idToSetterStr(instName)};")
  }

  override def instanceClearWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"this.#_shouldWrite${idToSetterStr(instName)} = false;")
  }

  override def instanceEnabledSetter(instName: InstanceIdentifier): Unit = {
    out.puts(renderMethodHeader(s"set${idToSetterStr(instName)}Enabled", List(renderParam("v", "boolean")), Some("void")))
    out.inc
    out.puts("this._dirty = true;")
    out.puts(s"this.#_enabled${idToSetterStr(instName)} = v;")
    universalFooter
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"get ${publicMemberName(instName)}() {")
    out.inc
  }

  override def instanceFooter: Unit = universalFooter

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== undefined)")
    out.inc
    instanceReturn(instName, dataType, isNullable = false)
    out.dec
  }

  override def instanceCheckWriteFlagAndWrite(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (this.#_shouldWrite${idToSetterStr(instName)}) {")
    out.inc
    out.puts(s"this._write${idToSetterStr(instName)}();")
    out.dec
    out.puts("}")
  }

  override def instanceReturnNullIfDisabled(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (!this.#_enabled${idToSetterStr(instName)}) {")
    out.inc
    out.puts("return undefined;")
    out.dec
    out.puts("}")
    out.puts
  }

  override def instanceHasValueIfHeader(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== undefined) {")
    out.inc
  }

  override def instanceHasValueIfFooter(): Unit = condIfFooter

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceInvalidate(instName: InstanceIdentifier): Unit = {
    out.puts(renderMethodHeader(s"_invalidate${idToSetterStr(instName)}", Nil, Some("void")))
    out.inc
    out.puts(s"${privateMemberName(instName)} = undefined;")
    out.dec
    out.puts("}")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
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

  override def internalEnumIntType(basedOn: IntType): DataType = basedOn

  override def debugClassSequence(seq: List[AttrSpec]): Unit = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts("static " + renderMemberDeclaration(s"_seqFields", "string[]", isOptional = false, s"[$seqStr]"))
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts(renderMethodHeader("toString", Nil, Some("string")))
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    universalFooter
  }

  override def attrPrimitiveWrite(
     io: String,
     valueExpr: Ast.expr,
     dataType: DataType,
     defEndian: Option[FixedEndian],
     exprTypeOpt: Option[DataType]
   ): Unit = {
    val expr = expression(valueExpr)
    val stmt = dataType match {
      case t: ReadableType =>
        s"$io.write${Utils.capitalize(t.apiCall(defEndian))}($expr)"
      case BitsType1(bitEndian) =>
        s"$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1, ${translator.boolToInt(valueExpr)})"
      case BitsType(width: Int, bitEndian) =>
        s"$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width, $expr)"
      case _: BytesType =>
        s"$io.writeBytes($expr)"
    }
    out.puts(s"$stmt;")
  }

  override def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit =
    out.puts(s"$io.writeBytesLimit(${expression(expr)}, $size, $term, $padRight);")

  override def attrUserTypeInstreamWrite(io: String, valueExpr: Ast.expr, dataType: DataType, exprType: DataType): Unit =
    out.puts(s"${expression(valueExpr)}${memberAccess}_write_Seq($io);")

  override def exprStreamToByteArray(io: String): String =
    s"$io.toByteArray()"

  override def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))
    out.puts(s"if (${expression(checkExpr)}) {")
    out.inc
    out.puts(s"throw new $kstructName.ConsistencyError($msgStr, ${expression(expected)}, ${expression(actual)});")
    out.dec
    out.puts("}")
  }

  override def attrObjectsEqualCheck(actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val actualStr = expression(actual)
    val expectedStr = expression(expected)
    val msgStr = expression(Ast.expr.Str(msg))
    out.puts(s"if ($actualStr !== $expectedStr) {")
    out.inc
    out.puts(s"throw new $kstructName.ConsistencyError($msgStr, $expectedStr, $actualStr);")
    out.dec
    out.puts("}")
  }

  override def attrParentParamCheck(actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean], msg: String): Unit = {
    /** @note Must be kept in sync with [[TypeScriptCompiler.parseExpr]] */
    val (expectedParent, dependsOnIo) = ut.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => ("undefined", false)
      case Some(fp) =>
        (expression(fp), userExprDependsOnIo(fp))
      case None => ("this", false)
    }
    if (shouldDependOnIo.exists(shouldDepend => dependsOnIo != shouldDepend))
      return

    val msgStr = expression(Ast.expr.Str(msg))
    out.puts(s"if (${expression(actualParentExpr)} !== $expectedParent) {")
    out.inc
    out.puts(s"throw new $kstructName.ConsistencyError($msgStr, $expectedParent, ${expression(actualParentExpr)});")
    out.dec
    out.puts("}")
  }

  override def attrIsEofCheck(io: String, expectedIsEof: Boolean, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))
    val eofExpr = s"$io.isEof()"
    val ifExpr = if (expectedIsEof) s"!($eofExpr)" else eofExpr
    out.puts(s"if ($ifExpr) {")
    out.inc
    out.puts(s"throw new $kstructName.ConsistencyError($msgStr, 0, ${exprIORemainingSize(io)});")
    out.dec
    out.puts("}")
  }

  override def condIfIsEofHeader(io: String, wantedIsEof: Boolean): Unit = {
    val eofExpr = s"$io.isEof()"
    val ifExpr = if (!wantedIsEof) s"!($eofExpr)" else eofExpr
    out.puts(s"if ($ifExpr) {")
    out.inc
  }

  override def condIfIsEofFooter: Unit = condIfFooter

  private def tsType(attrType: DataType, isNullable: Boolean = false): String =
    static.tsType(attrType, isNullable, config)

  override def castIfNeeded(expr: String, exprType: DataType, targetType: DataType): String =
    static.castIfNeeded(expr, exprType, targetType, config)

  override def idToStr(id: Identifier): String =
    static.idToStr(id)

  override def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case _ => idToStr(id)
    }

  private def idToSetterStr(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case _ => Utils.upperCamelCase(idToStr(id))
    }

  override def privateMemberName(id: Identifier): String =
    static.privateMemberName(id)

  override def localTemporaryName(id: Identifier): String =
    s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String =
    static.ksErrorName(err)

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr] = None
  ): Unit =
    attrValidate(attr, s"!(${translator.translate(checkExpr)})", err, useIo, actual, expected)

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    useIo: Boolean
  ): Unit = {
    val enumRef = static.tsEnumTypeName(et)
    attrValidate(attr, s"!Object.prototype.hasOwnProperty.call($enumRef, ${translator.translate(valueExpr)})", err, useIo, valueExpr, None)
  }

  private def attrValidate(
    attr: AttrLikeSpec,
    failCondExpr: String,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr]
  ): Unit = {
    val errArgsStr = expected.map(expression) ++ List(
      expression(actual),
      if (useIo) expression(Ast.expr.InternalName(IoIdentifier)) else "undefined",
      expression(Ast.expr.Str(attr.path.mkString("/", "/", "")))
    )
    out.puts(s"if ($failCondExpr) {")
    out.inc
    val errObj = s"new ${ksErrorName(err)}(${errArgsStr.mkString(", ")})"
    if (attrDebugNeeded(attr.id)) {
      val debugName = attrDebugName(attr.id, attr.cond.repeat, end = true)
      out.puts(s"const _err = $errObj;")
      out.puts(s"$debugName.validationError = _err;")
      out.puts("throw _err;")
    } else {
      out.puts(s"throw $errObj;")
    }
    out.dec
    out.puts("}")
  }

  private def attrDebugName(attrId: Identifier, rep: RepeatSpec, end: Boolean): String = {
    val arrIndexExpr = rep match {
      case NoRepeat => ""
      case _: RepeatExpr => ".arr[i]"
      case RepeatEos | _: RepeatUntil => s".arr[${privateMemberName(attrId)}.length${if (end) " - 1" else ""}]"
    }

    s"this._debug.${idToStr(attrId)}$arrIndexExpr"
  }

  protected def renderParam(name: String, typeName: String, isOptional: Boolean = false): String =
    s"$name${if (isOptional) "?" else ""}: $typeName"

  protected def renderMethodHeader(
    name: String,
    params: Seq[String],
    returnType: Option[String],
    accessModifier: Option[String] = None
  ): String = {
    val accessPrefix = accessModifier.map(_ + " ").getOrElse("")
    val returnSuffix = returnType.map(t => s": $t").getOrElse("")
    s"$accessPrefix$name(${params.mkString(", ")})$returnSuffix {"
  }

  protected def renderVariableDeclaration(
   varName: String,
   varType: String,
   isConst: Boolean = false,
   value: String = ""
 ): String = {
    val assignType = if (isConst) "const" else "let"
    val assignValue = if (value.nonEmpty) s" = $value" else ""
    s"$assignType $varName: $varType$assignValue;"
  }

  protected def renderMemberDeclaration(
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

trait TypeScriptCompilerStatic extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {

  // We must use non-null assertions for all member accesses because we cannot detect where they are not needed.
  val memberAccess: String = "!."

  def importClass(importList: ImportList, name: List[String]): Unit = {
    val procClass = type2class(name.last)
    val nameInit = name.init
    val pkgName = if (nameInit.isEmpty) "" else nameInit.mkString("-")
    if (pkgName.isEmpty) {
      importList.add(s"""import { $procClass } from "./$procClass.js";""")
    } else {
      importList.add(s"""import { $procClass } from "$pkgName";""")
    }
  }

  def tsType(attrType: DataType, isNullable: Boolean = false, config: RuntimeConfig): String = {
    val baseType = attrType match {
      case CalcIntType => "number"
      case CalcFloatType => "number"
      case _: IntType | _: FloatType | _: BitsType => "number"
      case _: BooleanType => "boolean"
      case _: StrType => "string"
      case _: BytesType => "Uint8Array"
      case t: UserType => tsUserTypeName(t)
      case t: EnumType => tsEnumTypeName(t)
      case AnyType => "any"
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructNameFull(config)
      case at: ArrayType => s"Array<${tsType(at.elType, isNullable = false, config)}>"
      case st: SwitchType => tsType(st.combinedType, isNullable = false, config)
    }
    if (isNullable) s"$baseType | undefined" else baseType
  }

  def castIfNeeded(expr: String, exprType: DataType, targetType: DataType, config: RuntimeConfig): String = {
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
      s"(($expr) as ${tsType(targetType, isNullable = false, config)})"
    } else {
      expr
    }
  }

  def tsUserTypeName(t: UserType): String = {
    val resolvedName = t.classSpec.map(_.name).getOrElse(t.name)
    types2class(resolvedName)
  }

  def tsEnumTypeName(t: EnumType): String = {
    val resolvedName = t.enumSpec.map(_.name).getOrElse(t.name)
    types2class(resolvedName)
  }

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_${Utils.lowerCamelCase(name)}"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
      case IoStorageIdentifier(innerId) => s"_io_${idToStr(innerId)}"
      case OuterSizeIdentifier(innerId) => s"${idToStr(innerId)}_OuterSize"
      case InnerSizeIdentifier(innerId) => s"${idToStr(innerId)}_InnerSize"
    }

  def privateMemberName(id: Identifier): String =
    id match {
      case IoIdentifier => s"this._io"
      case InstanceIdentifier(_) => s"this.#${idToStr(id)}"
      case _ => s"this.${idToStr(id)}"
    }

  override def kstreamName: String = "KaitaiStream"

  override def kstructName: String = "KaitaiStruct"

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => s"$kstreamName.EOFError"
    case _ => s"$kstreamName.${err.name}"
  }

  def types2class(types: List[String]): String =
    types.map(type2class).mkString(".")

  def kstructNameFull(config: RuntimeConfig): String = {
    kstructName + ((config.autoRead, config.readWrite) match {
      case (_, true) => ".ReadWrite"
      case (false, false) => ".ReadOnly"
      case (true, false) => ""
    })
  }
}

object TypeScriptCompiler extends TypeScriptCompilerStatic {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new TypeScriptCompiler(tp, config)
}
