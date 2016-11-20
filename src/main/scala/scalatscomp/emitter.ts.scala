package scalatscomp
object Emitter {
  sealed abstract class TempFlags
  object TempFlags {
    case object Auto extends TempFlags
    case object CountMask extends TempFlags
    case object _i extends TempFlags
  }
  val id = ((s: SourceFile) => s)
  val nullTransformers: Array[Transformer] = Array((_underscore_ => id))
  def emitFiles(resolver: EmitResolver,
                host: EmitHost,
                targetSourceFile: SourceFile,
                emitOnlyDtsFiles: Boolean): EmitResult = {
    val delimiters = createDelimiterMap()
    val brackets = createBracketsMap()
    val extendsHelper =
      """\nvar __extends = (this && this.__extends) || function (d, b) {\n    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];\n    function __() { this.constructor = d; }\n    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());\n};"""
    val assignHelper =
      """\nvar __assign = (this && this.__assign) || Object.assign || function(t) {\n    for (var s, i = 1, n = arguments.length; i < n; i++) {\n        s = arguments[i];\n        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))\n            t[p] = s[p];\n    }\n    return t;\n};"""
    val decorateHelper =
      """\nvar __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {\n    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;\n    if (typeof Reflect === \"object\" && typeof Reflect.decorate === \"function\") r = Reflect.decorate(decorators, target, key, desc);\n    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;\n    return c > 3 && r && Object.defineProperty(target, key, r), r;\n};"""
    val metadataHelper =
      """\nvar __metadata = (this && this.__metadata) || function (k, v) {\n    if (typeof Reflect === \"object\" && typeof Reflect.metadata === \"function\") return Reflect.metadata(k, v);\n};"""
    val paramHelper =
      """\nvar __param = (this && this.__param) || function (paramIndex, decorator) {\n    return function (target, key) { decorator(target, key, paramIndex); }\n};"""
    val awaiterHelper =
      """\nvar __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {\n    return new (P || (P = Promise))(function (resolve, reject) {\n        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }\n        function rejected(value) { try { step(generator[\"throw\"](value)); } catch (e) { reject(e); } }\n        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }\n        step((generator = generator.apply(thisArg, _arguments)).next());\n    });\n};"""
    val generatorHelper =
      """\nvar __generator = (this && this.__generator) || function (thisArg, body) {\n    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t;\n    return { next: verb(0), \"throw\": verb(1), \"return\": verb(2) };\n    function verb(n) { return function (v) { return step([n, v]); }; }\n    function step(op) {\n        if (f) throw new TypeError(\"Generator is already executing.\");\n        while (_) try {\n            if (f = 1, y && (t = y[op[0] & 2 ? \"return\" : op[0] ? \"throw\" : \"next\"]) && !(t = t.call(y, op[1])).done) return t;\n            if (y = 0, t) op = [0, t.value];\n            switch (op[0]) {\n                case 0: case 1: t = op; break;\n                case 4: _.label++; return { value: op[1], done: false };\n                case 5: _.label++; y = op[1]; op = [0]; continue;\n                case 7: op = _.ops.pop(); _.trys.pop(); continue;\n                default:\n                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }\n                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }\n                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }\n                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }\n                    if (t[2]) _.ops.pop();\n                    _.trys.pop(); continue;\n            }\n            op = body.call(thisArg, _);\n        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }\n        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };\n    }\n};"""
    val exportStarHelper =
      """\nfunction __export(m) {\n    for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];\n}"""
    val umdHelper =
      """\n(function (dependencies, factory) {\n    if (typeof module === 'object' && typeof module.exports === 'object') {\n        var v = factory(require, exports); if (v !== undefined) module.exports = v;\n    }\n    else if (typeof define === 'function' && define.amd) {\n        define(dependencies, factory);\n    }\n})"""
    val superHelper = """\nconst _super = name => super[name];"""
    val advancedSuperHelper =
      """\nconst _super = (function (geti, seti) {\n    const cache = Object.create(null);\n    return name => cache[name] || (cache[name] = { get value() { return geti(name); }, set value(v) { seti(name, v); } });\n})(name => super[name], (name, value) => super[name] = value);"""
    val compilerOptions = host.getCompilerOptions()
    val languageVersion = getEmitScriptTarget(compilerOptions)
    val moduleKind = getEmitModuleKind(compilerOptions)
    val sourceMapDataList: Array[SourceMapData] =
      (if ((compilerOptions.sourceMap || compilerOptions.inlineSourceMap))
         Array()
       else undefined)
    val emittedFilesList: Array[String] =
      (if (compilerOptions.listEmittedFiles) Array() else undefined)
    val emitterDiagnostics = createDiagnosticCollection()
    val newLine = host.getNewLine()
    val transformers: Array[Transformer] =
      (if (emitOnlyDtsFiles) nullTransformers
       else getTransformers(compilerOptions))
    val writer = createTextWriter(newLine)
    const fresh1 = writer
    val write = fresh1.write
    val writeLine = fresh1.writeLine
    val increaseIndent = fresh1.increaseIndent
    val decreaseIndent = fresh1.decreaseIndent
    val sourceMap = createSourceMapWriter(host, writer)
    const fresh2 = sourceMap
    val emitNodeWithSourceMap = fresh2.emitNodeWithSourceMap
    val emitTokenWithSourceMap = fresh2.emitTokenWithSourceMap
    val comments = createCommentWriter(host, writer, sourceMap)
    const fresh3 = comments
    val emitNodeWithComments = fresh3.emitNodeWithComments
    val emitBodyWithDetachedComments = fresh3.emitBodyWithDetachedComments
    val emitTrailingCommentsOfPosition = fresh3.emitTrailingCommentsOfPosition
    var nodeIdToGeneratedName: Array[String] = zeroOfMyType
    var autoGeneratedIdToGeneratedName: Array[String] = zeroOfMyType
    var generatedNameSet: Map[String] = zeroOfMyType
    var tempFlags: TempFlags = zeroOfMyType
    var currentSourceFile: SourceFile = zeroOfMyType
    var currentText: String = zeroOfMyType
    var currentFileIdentifiers: Map[String] = zeroOfMyType
    var extendsEmitted: Boolean = zeroOfMyType
    var assignEmitted: Boolean = zeroOfMyType
    var decorateEmitted: Boolean = zeroOfMyType
    var paramEmitted: Boolean = zeroOfMyType
    var awaiterEmitted: Boolean = zeroOfMyType
    var isOwnFileEmit: Boolean = zeroOfMyType
    var emitSkipped = false
    val sourceFiles = getSourceFilesToEmit(host, targetSourceFile)
    performance.mark("beforeTransform")
    const fresh4 = transformFiles(resolver, host, sourceFiles, transformers)
    val transformed = fresh4.transformed
    val emitNodeWithSubstitution = fresh4.emitNodeWithSubstitution
    val emitNodeWithNotification = fresh4.emitNodeWithNotification
    performance.measure("transformTime", "beforeTransform")
    performance.mark("beforePrint")
    forEachTransformedEmitFile(host, transformed, emitFile, emitOnlyDtsFiles)
    performance.measure("printTime", "beforePrint")
    (sourceFiles).foreach { fresh5 =>
      val sourceFile = zeroOfMyType = fresh5 {
        disposeEmitNodes(sourceFile)

      }
    }
    return Map(
      "emitSkipped" -> emitSkipped,
      "diagnostics" -> emitterDiagnostics.getDiagnostics(),
      "emittedFiles" -> emittedFilesList,
      "sourceMaps" -> sourceMapDataList)
    def emitFile(jsFilePath: String,
                 sourceMapFilePath: String,
                 declarationFilePath: String,
                 sourceFiles: Array[SourceFile],
                 isBundledEmit: Boolean) = {
      if (((!host.isEmitBlocked(jsFilePath)) && (!compilerOptions.noEmit))) {
        if ((!emitOnlyDtsFiles)) {
          printFile(jsFilePath, sourceMapFilePath, sourceFiles, isBundledEmit)

        }

      } else {
        (emitSkipped = true)

      }
      if (declarationFilePath) {
        (emitSkipped = (writeDeclarationFile(
            declarationFilePath,
            getOriginalSourceFiles(sourceFiles),
            isBundledEmit,
            host,
            resolver,
            emitterDiagnostics,
            emitOnlyDtsFiles) || emitSkipped))

      }
      if (((!emitSkipped) && emittedFilesList)) {
        if ((!emitOnlyDtsFiles)) {
          emittedFilesList.push(jsFilePath)

        }
        if (sourceMapFilePath) {
          emittedFilesList.push(sourceMapFilePath)

        }
        if (declarationFilePath) {
          emittedFilesList.push(declarationFilePath)

        }

      }

    }
    def printFile(jsFilePath: String,
                  sourceMapFilePath: String,
                  sourceFiles: Array[SourceFile],
                  isBundledEmit: Boolean) = {
      sourceMap
        .initialize(jsFilePath, sourceMapFilePath, sourceFiles, isBundledEmit)
      (nodeIdToGeneratedName = Array())
      (autoGeneratedIdToGeneratedName = Array())
      (generatedNameSet = createMap[String]())
      (isOwnFileEmit = (!isBundledEmit))
      if ((isBundledEmit && moduleKind)) {
        (sourceFiles).foreach { fresh6 =>
          val sourceFile = zeroOfMyType = fresh6 {
            emitEmitHelpers(sourceFile)

          }
        }

      }
      forEach(sourceFiles, printSourceFile)
      writeLine()
      val sourceMappingURL = sourceMap.getSourceMappingURL()
      if (sourceMappingURL) {
        write(s"""//# ${"sourceMappingURL"}=${sourceMappingURL}""")

      }
      if ((compilerOptions.sourceMap && (!compilerOptions.inlineSourceMap))) {
        writeFile(
          host,
          emitterDiagnostics,
          sourceMapFilePath,
          sourceMap.getText(),
          false)

      }
      if (sourceMapDataList) {
        sourceMapDataList.push(sourceMap.getSourceMapData())

      }
      writeFile(
        host,
        emitterDiagnostics,
        jsFilePath,
        writer.getText(),
        compilerOptions.emitBOM)
      sourceMap.reset()
      comments.reset()
      writer.reset()
      (tempFlags = TempFlags.Auto)
      (currentSourceFile = undefined)
      (currentText = undefined)
      (extendsEmitted = false)
      (assignEmitted = false)
      (decorateEmitted = false)
      (paramEmitted = false)
      (awaiterEmitted = false)
      (isOwnFileEmit = false)

    }
    def printSourceFile(node: SourceFile) = {
      (currentSourceFile = node)
      (currentText = node.text)
      (currentFileIdentifiers = node.identifiers)
      sourceMap.setSourceFile(node)
      comments.setSourceFile(node)
      pipelineEmitWithNotification(EmitContext.SourceFile, node)

    }
    def emit(node: Node) = {
      pipelineEmitWithNotification(EmitContext.Unspecified, node)

    }
    def emitIdentifierName(node: Identifier) = {
      pipelineEmitWithNotification(EmitContext.IdentifierName, node)

    }
    def emitExpression(node: Expression) = {
      pipelineEmitWithNotification(EmitContext.Expression, node)

    }
    def pipelineEmitWithNotification(emitContext: EmitContext, node: Node) = {
      emitNodeWithNotification(emitContext, node, pipelineEmitWithComments)

    }
    def pipelineEmitWithComments(emitContext: EmitContext, node: Node) = {
      if ((emitContext === EmitContext.SourceFile)) {
        pipelineEmitWithSourceMap(emitContext, node)
        return

      }
      emitNodeWithComments(emitContext, node, pipelineEmitWithSourceMap)

    }
    def pipelineEmitWithSourceMap(emitContext: EmitContext, node: Node) = {
      if (((emitContext === EmitContext.SourceFile) || (emitContext === EmitContext.IdentifierName))) {
        pipelineEmitWithSubstitution(emitContext, node)
        return

      }
      emitNodeWithSourceMap(emitContext, node, pipelineEmitWithSubstitution)

    }
    def pipelineEmitWithSubstitution(emitContext: EmitContext, node: Node) = {
      emitNodeWithSubstitution(emitContext, node, pipelineEmitForContext)

    }
    def pipelineEmitForContext(emitContext: EmitContext, node: Node): Unit = {
      emitContext match {
        case EmitContext.SourceFile =>
          return pipelineEmitInSourceFileContext(node)
        case EmitContext.IdentifierName =>
          return pipelineEmitInIdentifierNameContext(node)
        case EmitContext.Unspecified =>
          return pipelineEmitInUnspecifiedContext(node)
        case EmitContext.Expression =>
          return pipelineEmitInExpressionContext(node)
        case _ =>
      }

    }
    def pipelineEmitInSourceFileContext(node: Node): Unit = {
      val kind = node.kind
      kind match {
        case SyntaxKind.SourceFile =>
          return emitSourceFile(node.asInstanceOf[SourceFile])
        case _ =>
      }

    }
    def pipelineEmitInIdentifierNameContext(node: Node): Unit = {
      val kind = node.kind
      kind match {
        case SyntaxKind.Identifier =>
          return emitIdentifier(node.asInstanceOf[Identifier])
        case _ =>
      }

    }
    def pipelineEmitInUnspecifiedContext(node: Node): Unit = {
      val kind = node.kind
      kind match {
        case SyntaxKind.TemplateHead | SyntaxKind.TemplateMiddle |
            SyntaxKind.TemplateTail =>
          return emitLiteral(node.asInstanceOf[LiteralExpression])
        case SyntaxKind.Identifier =>
          return emitIdentifier(node.asInstanceOf[Identifier])
        case SyntaxKind.ConstKeyword | SyntaxKind.DefaultKeyword |
            SyntaxKind.ExportKeyword | SyntaxKind.VoidKeyword |
            SyntaxKind.PrivateKeyword | SyntaxKind.ProtectedKeyword |
            SyntaxKind.PublicKeyword | SyntaxKind.StaticKeyword |
            SyntaxKind.AbstractKeyword | SyntaxKind.AsKeyword |
            SyntaxKind.AnyKeyword | SyntaxKind.AsyncKeyword |
            SyntaxKind.AwaitKeyword | SyntaxKind.BooleanKeyword |
            SyntaxKind.ConstructorKeyword | SyntaxKind.DeclareKeyword |
            SyntaxKind.GetKeyword | SyntaxKind.IsKeyword |
            SyntaxKind.ModuleKeyword | SyntaxKind.NamespaceKeyword |
            SyntaxKind.NeverKeyword | SyntaxKind.ReadonlyKeyword |
            SyntaxKind.RequireKeyword | SyntaxKind.NumberKeyword |
            SyntaxKind.SetKeyword | SyntaxKind.StringKeyword |
            SyntaxKind.SymbolKeyword | SyntaxKind.TypeKeyword |
            SyntaxKind.UndefinedKeyword | SyntaxKind.FromKeyword |
            SyntaxKind.GlobalKeyword | SyntaxKind.OfKeyword =>
          writeTokenText(kind)
          return
        case SyntaxKind.QualifiedName =>
          return emitQualifiedName(node.asInstanceOf[QualifiedName])
        case SyntaxKind.ComputedPropertyName =>
          return emitComputedPropertyName(
            node.asInstanceOf[ComputedPropertyName])
        case SyntaxKind.TypeParameter =>
          return emitTypeParameter(node.asInstanceOf[TypeParameterDeclaration])
        case SyntaxKind.Parameter =>
          return emitParameter(node.asInstanceOf[ParameterDeclaration])
        case SyntaxKind.Decorator =>
          return emitDecorator(node.asInstanceOf[Decorator])
        case SyntaxKind.PropertySignature =>
          return emitPropertySignature(node.asInstanceOf[PropertySignature])
        case SyntaxKind.PropertyDeclaration =>
          return emitPropertyDeclaration(
            node.asInstanceOf[PropertyDeclaration])
        case SyntaxKind.MethodSignature =>
          return emitMethodSignature(node.asInstanceOf[MethodSignature])
        case SyntaxKind.MethodDeclaration =>
          return emitMethodDeclaration(node.asInstanceOf[MethodDeclaration])
        case SyntaxKind.Constructor =>
          return emitConstructor(node.asInstanceOf[ConstructorDeclaration])
        case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
          return emitAccessorDeclaration(
            node.asInstanceOf[AccessorDeclaration])
        case SyntaxKind.CallSignature =>
          return emitCallSignature(node.asInstanceOf[CallSignatureDeclaration])
        case SyntaxKind.ConstructSignature =>
          return emitConstructSignature(
            node.asInstanceOf[ConstructSignatureDeclaration])
        case SyntaxKind.IndexSignature =>
          return emitIndexSignature(
            node.asInstanceOf[IndexSignatureDeclaration])
        case SyntaxKind.TypePredicate =>
          return emitTypePredicate(node.asInstanceOf[TypePredicateNode])
        case SyntaxKind.TypeReference =>
          return emitTypeReference(node.asInstanceOf[TypeReferenceNode])
        case SyntaxKind.FunctionType =>
          return emitFunctionType(node.asInstanceOf[FunctionTypeNode])
        case SyntaxKind.ConstructorType =>
          return emitConstructorType(node.asInstanceOf[ConstructorTypeNode])
        case SyntaxKind.TypeQuery =>
          return emitTypeQuery(node.asInstanceOf[TypeQueryNode])
        case SyntaxKind.TypeLiteral =>
          return emitTypeLiteral(node.asInstanceOf[TypeLiteralNode])
        case SyntaxKind.ArrayType =>
          return emitArrayType(node.asInstanceOf[ArrayTypeNode])
        case SyntaxKind.TupleType =>
          return emitTupleType(node.asInstanceOf[TupleTypeNode])
        case SyntaxKind.UnionType =>
          return emitUnionType(node.asInstanceOf[UnionTypeNode])
        case SyntaxKind.IntersectionType =>
          return emitIntersectionType(node.asInstanceOf[IntersectionTypeNode])
        case SyntaxKind.ParenthesizedType =>
          return emitParenthesizedType(
            node.asInstanceOf[ParenthesizedTypeNode])
        case SyntaxKind.ExpressionWithTypeArguments =>
          return emitExpressionWithTypeArguments(
            node.asInstanceOf[ExpressionWithTypeArguments])
        case SyntaxKind.ThisType =>
          return emitThisType()
        case SyntaxKind.LiteralType =>
          return emitLiteralType(node.asInstanceOf[LiteralTypeNode])
        case SyntaxKind.ObjectBindingPattern =>
          return emitObjectBindingPattern(
            node.asInstanceOf[ObjectBindingPattern])
        case SyntaxKind.ArrayBindingPattern =>
          return emitArrayBindingPattern(
            node.asInstanceOf[ArrayBindingPattern])
        case SyntaxKind.BindingElement =>
          return emitBindingElement(node.asInstanceOf[BindingElement])
        case SyntaxKind.TemplateSpan =>
          return emitTemplateSpan(node.asInstanceOf[TemplateSpan])
        case SyntaxKind.SemicolonClassElement =>
          return emitSemicolonClassElement()
        case SyntaxKind.Block =>
          return emitBlock(node.asInstanceOf[Block])
        case SyntaxKind.VariableStatement =>
          return emitVariableStatement(node.asInstanceOf[VariableStatement])
        case SyntaxKind.EmptyStatement =>
          return emitEmptyStatement()
        case SyntaxKind.ExpressionStatement =>
          return emitExpressionStatement(
            node.asInstanceOf[ExpressionStatement])
        case SyntaxKind.IfStatement =>
          return emitIfStatement(node.asInstanceOf[IfStatement])
        case SyntaxKind.DoStatement =>
          return emitDoStatement(node.asInstanceOf[DoStatement])
        case SyntaxKind.WhileStatement =>
          return emitWhileStatement(node.asInstanceOf[WhileStatement])
        case SyntaxKind.ForStatement =>
          return emitForStatement(node.asInstanceOf[ForStatement])
        case SyntaxKind.ForInStatement =>
          return emitForInStatement(node.asInstanceOf[ForInStatement])
        case SyntaxKind.ForOfStatement =>
          return emitForOfStatement(node.asInstanceOf[ForOfStatement])
        case SyntaxKind.ContinueStatement =>
          return emitContinueStatement(node.asInstanceOf[ContinueStatement])
        case SyntaxKind.BreakStatement =>
          return emitBreakStatement(node.asInstanceOf[BreakStatement])
        case SyntaxKind.ReturnStatement =>
          return emitReturnStatement(node.asInstanceOf[ReturnStatement])
        case SyntaxKind.WithStatement =>
          return emitWithStatement(node.asInstanceOf[WithStatement])
        case SyntaxKind.SwitchStatement =>
          return emitSwitchStatement(node.asInstanceOf[SwitchStatement])
        case SyntaxKind.LabeledStatement =>
          return emitLabeledStatement(node.asInstanceOf[LabeledStatement])
        case SyntaxKind.ThrowStatement =>
          return emitThrowStatement(node.asInstanceOf[ThrowStatement])
        case SyntaxKind.TryStatement =>
          return emitTryStatement(node.asInstanceOf[TryStatement])
        case SyntaxKind.DebuggerStatement =>
          return emitDebuggerStatement(node.asInstanceOf[DebuggerStatement])
        case SyntaxKind.VariableDeclaration =>
          return emitVariableDeclaration(
            node.asInstanceOf[VariableDeclaration])
        case SyntaxKind.VariableDeclarationList =>
          return emitVariableDeclarationList(
            node.asInstanceOf[VariableDeclarationList])
        case SyntaxKind.FunctionDeclaration =>
          return emitFunctionDeclaration(
            node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.ClassDeclaration =>
          return emitClassDeclaration(node.asInstanceOf[ClassDeclaration])
        case SyntaxKind.InterfaceDeclaration =>
          return emitInterfaceDeclaration(
            node.asInstanceOf[InterfaceDeclaration])
        case SyntaxKind.TypeAliasDeclaration =>
          return emitTypeAliasDeclaration(
            node.asInstanceOf[TypeAliasDeclaration])
        case SyntaxKind.EnumDeclaration =>
          return emitEnumDeclaration(node.asInstanceOf[EnumDeclaration])
        case SyntaxKind.ModuleDeclaration =>
          return emitModuleDeclaration(node.asInstanceOf[ModuleDeclaration])
        case SyntaxKind.ModuleBlock =>
          return emitModuleBlock(node.asInstanceOf[ModuleBlock])
        case SyntaxKind.CaseBlock =>
          return emitCaseBlock(node.asInstanceOf[CaseBlock])
        case SyntaxKind.ImportEqualsDeclaration =>
          return emitImportEqualsDeclaration(
            node.asInstanceOf[ImportEqualsDeclaration])
        case SyntaxKind.ImportDeclaration =>
          return emitImportDeclaration(node.asInstanceOf[ImportDeclaration])
        case SyntaxKind.ImportClause =>
          return emitImportClause(node.asInstanceOf[ImportClause])
        case SyntaxKind.NamespaceImport =>
          return emitNamespaceImport(node.asInstanceOf[NamespaceImport])
        case SyntaxKind.NamedImports =>
          return emitNamedImports(node.asInstanceOf[NamedImports])
        case SyntaxKind.ImportSpecifier =>
          return emitImportSpecifier(node.asInstanceOf[ImportSpecifier])
        case SyntaxKind.ExportAssignment =>
          return emitExportAssignment(node.asInstanceOf[ExportAssignment])
        case SyntaxKind.ExportDeclaration =>
          return emitExportDeclaration(node.asInstanceOf[ExportDeclaration])
        case SyntaxKind.NamedExports =>
          return emitNamedExports(node.asInstanceOf[NamedExports])
        case SyntaxKind.ExportSpecifier =>
          return emitExportSpecifier(node.asInstanceOf[ExportSpecifier])
        case SyntaxKind.MissingDeclaration =>
          return
        case SyntaxKind.ExternalModuleReference =>
          return emitExternalModuleReference(
            node.asInstanceOf[ExternalModuleReference])
        case SyntaxKind.JsxText =>
          return emitJsxText(node.asInstanceOf[JsxText])
        case SyntaxKind.JsxOpeningElement =>
          return emitJsxOpeningElement(node.asInstanceOf[JsxOpeningElement])
        case SyntaxKind.JsxClosingElement =>
          return emitJsxClosingElement(node.asInstanceOf[JsxClosingElement])
        case SyntaxKind.JsxAttribute =>
          return emitJsxAttribute(node.asInstanceOf[JsxAttribute])
        case SyntaxKind.JsxSpreadAttribute =>
          return emitJsxSpreadAttribute(node.asInstanceOf[JsxSpreadAttribute])
        case SyntaxKind.JsxExpression =>
          return emitJsxExpression(node.asInstanceOf[JsxExpression])
        case SyntaxKind.CaseClause =>
          return emitCaseClause(node.asInstanceOf[CaseClause])
        case SyntaxKind.DefaultClause =>
          return emitDefaultClause(node.asInstanceOf[DefaultClause])
        case SyntaxKind.HeritageClause =>
          return emitHeritageClause(node.asInstanceOf[HeritageClause])
        case SyntaxKind.CatchClause =>
          return emitCatchClause(node.asInstanceOf[CatchClause])
        case SyntaxKind.PropertyAssignment =>
          return emitPropertyAssignment(node.asInstanceOf[PropertyAssignment])
        case SyntaxKind.ShorthandPropertyAssignment =>
          return emitShorthandPropertyAssignment(
            node.asInstanceOf[ShorthandPropertyAssignment])
        case SyntaxKind.EnumMember =>
          return emitEnumMember(node.asInstanceOf[EnumMember])
        case _ =>
      }
      if (isExpression(node)) {
        return pipelineEmitWithSubstitution(EmitContext.Expression, node)

      }

    }
    def pipelineEmitInExpressionContext(node: Node): Unit = {
      val kind = node.kind
      kind match {
        case SyntaxKind.NumericLiteral =>
          return emitNumericLiteral(node.asInstanceOf[NumericLiteral])
        case SyntaxKind.StringLiteral | SyntaxKind.RegularExpressionLiteral |
            SyntaxKind.NoSubstitutionTemplateLiteral =>
          return emitLiteral(node.asInstanceOf[LiteralExpression])
        case SyntaxKind.Identifier =>
          return emitIdentifier(node.asInstanceOf[Identifier])
        case SyntaxKind.FalseKeyword | SyntaxKind.NullKeyword |
            SyntaxKind.SuperKeyword | SyntaxKind.TrueKeyword |
            SyntaxKind.ThisKeyword =>
          writeTokenText(kind)
          return
        case SyntaxKind.ArrayLiteralExpression =>
          return emitArrayLiteralExpression(
            node.asInstanceOf[ArrayLiteralExpression])
        case SyntaxKind.ObjectLiteralExpression =>
          return emitObjectLiteralExpression(
            node.asInstanceOf[ObjectLiteralExpression])
        case SyntaxKind.PropertyAccessExpression =>
          return emitPropertyAccessExpression(
            node.asInstanceOf[PropertyAccessExpression])
        case SyntaxKind.ElementAccessExpression =>
          return emitElementAccessExpression(
            node.asInstanceOf[ElementAccessExpression])
        case SyntaxKind.CallExpression =>
          return emitCallExpression(node.asInstanceOf[CallExpression])
        case SyntaxKind.NewExpression =>
          return emitNewExpression(node.asInstanceOf[NewExpression])
        case SyntaxKind.TaggedTemplateExpression =>
          return emitTaggedTemplateExpression(
            node.asInstanceOf[TaggedTemplateExpression])
        case SyntaxKind.TypeAssertionExpression =>
          return emitTypeAssertionExpression(node.asInstanceOf[TypeAssertion])
        case SyntaxKind.ParenthesizedExpression =>
          return emitParenthesizedExpression(
            node.asInstanceOf[ParenthesizedExpression])
        case SyntaxKind.FunctionExpression =>
          return emitFunctionExpression(node.asInstanceOf[FunctionExpression])
        case SyntaxKind.ArrowFunction =>
          return emitArrowFunction(node.asInstanceOf[ArrowFunction])
        case SyntaxKind.DeleteExpression =>
          return emitDeleteExpression(node.asInstanceOf[DeleteExpression])
        case SyntaxKind.TypeOfExpression =>
          return emitTypeOfExpression(node.asInstanceOf[TypeOfExpression])
        case SyntaxKind.VoidExpression =>
          return emitVoidExpression(node.asInstanceOf[VoidExpression])
        case SyntaxKind.AwaitExpression =>
          return emitAwaitExpression(node.asInstanceOf[AwaitExpression])
        case SyntaxKind.PrefixUnaryExpression =>
          return emitPrefixUnaryExpression(
            node.asInstanceOf[PrefixUnaryExpression])
        case SyntaxKind.PostfixUnaryExpression =>
          return emitPostfixUnaryExpression(
            node.asInstanceOf[PostfixUnaryExpression])
        case SyntaxKind.BinaryExpression =>
          return emitBinaryExpression(node.asInstanceOf[BinaryExpression])
        case SyntaxKind.ConditionalExpression =>
          return emitConditionalExpression(
            node.asInstanceOf[ConditionalExpression])
        case SyntaxKind.TemplateExpression =>
          return emitTemplateExpression(node.asInstanceOf[TemplateExpression])
        case SyntaxKind.YieldExpression =>
          return emitYieldExpression(node.asInstanceOf[YieldExpression])
        case SyntaxKind.SpreadElementExpression =>
          return emitSpreadElementExpression(
            node.asInstanceOf[SpreadElementExpression])
        case SyntaxKind.ClassExpression =>
          return emitClassExpression(node.asInstanceOf[ClassExpression])
        case SyntaxKind.OmittedExpression =>
          return
        case SyntaxKind.AsExpression =>
          return emitAsExpression(node.asInstanceOf[AsExpression])
        case SyntaxKind.NonNullExpression =>
          return emitNonNullExpression(node.asInstanceOf[NonNullExpression])
        case SyntaxKind.JsxElement =>
          return emitJsxElement(node.asInstanceOf[JsxElement])
        case SyntaxKind.JsxSelfClosingElement =>
          return emitJsxSelfClosingElement(
            node.asInstanceOf[JsxSelfClosingElement])
        case SyntaxKind.PartiallyEmittedExpression =>
          return emitPartiallyEmittedExpression(
            node.asInstanceOf[PartiallyEmittedExpression])
        case _ =>
      }

    }
    def emitNumericLiteral(node: NumericLiteral) = {
      emitLiteral(node)
      if (node.trailingComment) {
        write(s""" /*${node.trailingComment}*/""")

      }

    }
    def emitLiteral(node: LiteralLikeNode) = {
      val text = getLiteralTextOfNode(node)
      if ((((compilerOptions.sourceMap || compilerOptions.inlineSourceMap)) && (((node.kind === SyntaxKind.StringLiteral) || isTemplateLiteralKind(
            node.kind))))) {
        writer.writeLiteral(text)

      } else {
        write(text)

      }

    }
    def emitIdentifier(node: Identifier) = {
      if ((getEmitFlags(node) & EmitFlags.UMDDefine)) {
        writeLines(umdHelper)

      } else {
        write(getTextOfNode(node, false))

      }

    }
    def emitQualifiedName(node: QualifiedName) = {
      emitEntityName(node.left)
      write(".")
      emit(node.right)

    }
    def emitEntityName(node: EntityName) = {
      if ((node.kind === SyntaxKind.Identifier)) {
        emitExpression(node.asInstanceOf[Identifier])

      } else {
        emit(node)

      }

    }
    def emitComputedPropertyName(node: ComputedPropertyName) = {
      write("[")
      emitExpression(node.expression)
      write("]")

    }
    def emitTypeParameter(node: TypeParameterDeclaration) = {
      emit(node.name)
      emitWithPrefix(" extends ", node.constraint)

    }
    def emitParameter(node: ParameterDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      writeIfPresent(node.dotDotDotToken, "...")
      emit(node.name)
      writeIfPresent(node.questionToken, "?")
      emitExpressionWithPrefix(" = ", node.initializer)
      emitWithPrefix(": ", node.`type`)

    }
    def emitDecorator(decorator: Decorator) = {
      write("@")
      emitExpression(decorator.expression)

    }
    def emitPropertySignature(node: PropertySignature) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      emit(node.name)
      writeIfPresent(node.questionToken, "?")
      emitWithPrefix(": ", node.`type`)
      write(";")

    }
    def emitPropertyDeclaration(node: PropertyDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      emit(node.name)
      emitWithPrefix(": ", node.`type`)
      emitExpressionWithPrefix(" = ", node.initializer)
      write(";")

    }
    def emitMethodSignature(node: MethodSignature) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      emit(node.name)
      writeIfPresent(node.questionToken, "?")
      emitTypeParameters(node, node.typeParameters)
      emitParameters(node, node.parameters)
      emitWithPrefix(": ", node.`type`)
      write(";")

    }
    def emitMethodDeclaration(node: MethodDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      writeIfPresent(node.asteriskToken, "*")
      emit(node.name)
      emitSignatureAndBody(node, emitSignatureHead)

    }
    def emitConstructor(node: ConstructorDeclaration) = {
      emitModifiers(node, node.modifiers)
      write("constructor")
      emitSignatureAndBody(node, emitSignatureHead)

    }
    def emitAccessorDeclaration(node: AccessorDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      write((if ((node.kind === SyntaxKind.GetAccessor)) "get " else "set "))
      emit(node.name)
      emitSignatureAndBody(node, emitSignatureHead)

    }
    def emitCallSignature(node: CallSignatureDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      emitTypeParameters(node, node.typeParameters)
      emitParameters(node, node.parameters)
      emitWithPrefix(": ", node.`type`)
      write(";")

    }
    def emitConstructSignature(node: ConstructSignatureDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      write("new ")
      emitTypeParameters(node, node.typeParameters)
      emitParameters(node, node.parameters)
      emitWithPrefix(": ", node.`type`)
      write(";")

    }
    def emitIndexSignature(node: IndexSignatureDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      emitParametersForIndexSignature(node, node.parameters)
      emitWithPrefix(": ", node.`type`)
      write(";")

    }
    def emitSemicolonClassElement() = {
      write(";")

    }
    def emitTypePredicate(node: TypePredicateNode) = {
      emit(node.parameterName)
      write(" is ")
      emit(node.`type`)

    }
    def emitTypeReference(node: TypeReferenceNode) = {
      emit(node.typeName)
      emitTypeArguments(node, node.typeArguments)

    }
    def emitFunctionType(node: FunctionTypeNode) = {
      emitTypeParameters(node, node.typeParameters)
      emitParametersForArrow(node, node.parameters)
      write(" => ")
      emit(node.`type`)

    }
    def emitConstructorType(node: ConstructorTypeNode) = {
      write("new ")
      emitTypeParameters(node, node.typeParameters)
      emitParametersForArrow(node, node.parameters)
      write(" => ")
      emit(node.`type`)

    }
    def emitTypeQuery(node: TypeQueryNode) = {
      write("typeof ")
      emit(node.exprName)

    }
    def emitTypeLiteral(node: TypeLiteralNode) = {
      write("{")
      emitList(node, node.members, ListFormat.TypeLiteralMembers)
      write("}")

    }
    def emitArrayType(node: ArrayTypeNode) = {
      emit(node.elementType)
      write("[]")

    }
    def emitTupleType(node: TupleTypeNode) = {
      write("[")
      emitList(node, node.elementTypes, ListFormat.TupleTypeElements)
      write("]")

    }
    def emitUnionType(node: UnionTypeNode) = {
      emitList(node, node.types, ListFormat.UnionTypeConstituents)

    }
    def emitIntersectionType(node: IntersectionTypeNode) = {
      emitList(node, node.types, ListFormat.IntersectionTypeConstituents)

    }
    def emitParenthesizedType(node: ParenthesizedTypeNode) = {
      write("(")
      emit(node.`type`)
      write(")")

    }
    def emitThisType() = {
      write("this")

    }
    def emitLiteralType(node: LiteralTypeNode) = {
      emitExpression(node.literal)

    }
    def emitObjectBindingPattern(node: ObjectBindingPattern) = {
      val elements = node.elements
      if ((elements.length === 0)) {
        write("{}")

      } else {
        write("{")
        emitList(node, elements, ListFormat.ObjectBindingPatternElements)
        write("}")

      }

    }
    def emitArrayBindingPattern(node: ArrayBindingPattern) = {
      val elements = node.elements
      if ((elements.length === 0)) {
        write("[]")

      } else {
        write("[")
        emitList(node, node.elements, ListFormat.ArrayBindingPatternElements)
        write("]")

      }

    }
    def emitBindingElement(node: BindingElement) = {
      emitWithSuffix(node.propertyName, ": ")
      writeIfPresent(node.dotDotDotToken, "...")
      emit(node.name)
      emitExpressionWithPrefix(" = ", node.initializer)

    }
    def emitArrayLiteralExpression(node: ArrayLiteralExpression) = {
      val elements = node.elements
      if ((elements.length === 0)) {
        write("[]")

      } else {
        val preferNewLine =
          (if (node.multiLine) ListFormat.PreferNewLine else ListFormat.None)
        emitExpressionList(
          node,
          elements,
          (ListFormat.ArrayLiteralExpressionElements | preferNewLine))

      }

    }
    def emitObjectLiteralExpression(node: ObjectLiteralExpression) = {
      val properties = node.properties
      if ((properties.length === 0)) {
        write("{}")

      } else {
        val indentedFlag = (getEmitFlags(node) & EmitFlags.Indented)
        if (indentedFlag) {
          increaseIndent()

        }
        val preferNewLine =
          (if (node.multiLine) ListFormat.PreferNewLine else ListFormat.None)
        val allowTrailingComma =
          (if ((languageVersion >= ScriptTarget.ES5))
             ListFormat.AllowTrailingComma
           else ListFormat.None)
        emitList(
          node,
          properties,
          ((ListFormat.ObjectLiteralExpressionProperties | allowTrailingComma) | preferNewLine))
        if (indentedFlag) {
          decreaseIndent()

        }

      }

    }
    def emitPropertyAccessExpression(node: PropertyAccessExpression) = {
      var indentBeforeDot = false
      var indentAfterDot = false
      if ((!((getEmitFlags(node) & EmitFlags.NoIndentation)))) {
        val dotRangeStart = node.expression.end
        val dotRangeEnd = (skipTrivia(currentText, node.expression.end) + 1)
        val dotToken = Map(
          "kind" -> SyntaxKind.DotToken,
          "pos" -> dotRangeStart,
          "end" -> dotRangeEnd).asInstanceOf[Node]
        (indentBeforeDot = needsIndentation(node, node.expression, dotToken))
        (indentAfterDot = needsIndentation(node, dotToken, node.name))

      }
      emitExpression(node.expression)
      increaseIndentIf(indentBeforeDot)
      val shouldEmitDotDot = ((!indentBeforeDot) && needsDotDotForPropertyAccess(
          node.expression))
      write((if (shouldEmitDotDot) ".." else "."))
      increaseIndentIf(indentAfterDot)
      emit(node.name)
      decreaseIndentIf(indentBeforeDot, indentAfterDot)

    }
    def needsDotDotForPropertyAccess(expression: Expression) = {
      if ((expression.kind === SyntaxKind.NumericLiteral)) {
        val text =
          getLiteralTextOfNode(expression.asInstanceOf[LiteralExpression])
        return (text.indexOf(tokenToString(SyntaxKind.DotToken)) < 0)

      } else if ((isPropertyAccessExpression(expression) || isElementAccessExpression(
                   expression))) {
        val constantValue = getConstantValue(expression)
        return ((isFinite(constantValue) && (Math.floor(constantValue) === constantValue)) && compilerOptions.removeComments)

      }

    }
    def emitElementAccessExpression(node: ElementAccessExpression) = {
      emitExpression(node.expression)
      write("[")
      emitExpression(node.argumentExpression)
      write("]")

    }
    def emitCallExpression(node: CallExpression) = {
      emitExpression(node.expression)
      emitTypeArguments(node, node.typeArguments)
      emitExpressionList(
        node,
        node.arguments,
        ListFormat.CallExpressionArguments)

    }
    def emitNewExpression(node: NewExpression) = {
      write("new ")
      emitExpression(node.expression)
      emitTypeArguments(node, node.typeArguments)
      emitExpressionList(
        node,
        node.arguments,
        ListFormat.NewExpressionArguments)

    }
    def emitTaggedTemplateExpression(node: TaggedTemplateExpression) = {
      emitExpression(node.tag)
      write(" ")
      emitExpression(node.template)

    }
    def emitTypeAssertionExpression(node: TypeAssertion) = {
      if (node.`type`) {
        write("<")
        emit(node.`type`)
        write(">")

      }
      emitExpression(node.expression)

    }
    def emitParenthesizedExpression(node: ParenthesizedExpression) = {
      write("(")
      emitExpression(node.expression)
      write(")")

    }
    def emitFunctionExpression(node: FunctionExpression) = {
      emitFunctionDeclarationOrExpression(node)

    }
    def emitArrowFunction(node: ArrowFunction) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      emitSignatureAndBody(node, emitArrowFunctionHead)

    }
    def emitArrowFunctionHead(node: ArrowFunction) = {
      emitTypeParameters(node, node.typeParameters)
      emitParametersForArrow(node, node.parameters)
      emitWithPrefix(": ", node.`type`)
      write(" =>")

    }
    def emitDeleteExpression(node: DeleteExpression) = {
      write("delete ")
      emitExpression(node.expression)

    }
    def emitTypeOfExpression(node: TypeOfExpression) = {
      write("typeof ")
      emitExpression(node.expression)

    }
    def emitVoidExpression(node: VoidExpression) = {
      write("void ")
      emitExpression(node.expression)

    }
    def emitAwaitExpression(node: AwaitExpression) = {
      write("await ")
      emitExpression(node.expression)

    }
    def emitPrefixUnaryExpression(node: PrefixUnaryExpression) = {
      writeTokenText(node.operator)
      if (shouldEmitWhitespaceBeforeOperand(node)) {
        write(" ")

      }
      emitExpression(node.operand)

    }
    def shouldEmitWhitespaceBeforeOperand(node: PrefixUnaryExpression) = {
      val operand = node.operand
      return ((operand.kind === SyntaxKind.PrefixUnaryExpression) && (((((node.operator === SyntaxKind.PlusToken) && ((((operand
        .asInstanceOf[PrefixUnaryExpression])
        .operator === SyntaxKind.PlusToken) || ((operand
        .asInstanceOf[PrefixUnaryExpression])
        .operator === SyntaxKind.PlusPlusToken))))) || (((node.operator === SyntaxKind.MinusToken) && ((((operand
        .asInstanceOf[PrefixUnaryExpression])
        .operator === SyntaxKind.MinusToken) || ((operand
        .asInstanceOf[PrefixUnaryExpression])
        .operator === SyntaxKind.MinusMinusToken))))))))

    }
    def emitPostfixUnaryExpression(node: PostfixUnaryExpression) = {
      emitExpression(node.operand)
      writeTokenText(node.operator)

    }
    def emitBinaryExpression(node: BinaryExpression) = {
      val isCommaOperator = (node.operatorToken.kind !== SyntaxKind.CommaToken)
      val indentBeforeOperator =
        needsIndentation(node, node.left, node.operatorToken)
      val indentAfterOperator =
        needsIndentation(node, node.operatorToken, node.right)
      emitExpression(node.left)
      increaseIndentIf(
        indentBeforeOperator,
        (if (isCommaOperator) " " else undefined))
      writeTokenText(node.operatorToken.kind)
      increaseIndentIf(indentAfterOperator, " ")
      emitExpression(node.right)
      decreaseIndentIf(indentBeforeOperator, indentAfterOperator)

    }
    def emitConditionalExpression(node: ConditionalExpression) = {
      val indentBeforeQuestion =
        needsIndentation(node, node.condition, node.questionToken)
      val indentAfterQuestion =
        needsIndentation(node, node.questionToken, node.whenTrue)
      val indentBeforeColon =
        needsIndentation(node, node.whenTrue, node.colonToken)
      val indentAfterColon =
        needsIndentation(node, node.colonToken, node.whenFalse)
      emitExpression(node.condition)
      increaseIndentIf(indentBeforeQuestion, " ")
      write("?")
      increaseIndentIf(indentAfterQuestion, " ")
      emitExpression(node.whenTrue)
      decreaseIndentIf(indentBeforeQuestion, indentAfterQuestion)
      increaseIndentIf(indentBeforeColon, " ")
      write(":")
      increaseIndentIf(indentAfterColon, " ")
      emitExpression(node.whenFalse)
      decreaseIndentIf(indentBeforeColon, indentAfterColon)

    }
    def emitTemplateExpression(node: TemplateExpression) = {
      emit(node.head)
      emitList(node, node.templateSpans, ListFormat.TemplateExpressionSpans)

    }
    def emitYieldExpression(node: YieldExpression) = {
      write((if (node.asteriskToken) "yield*" else "yield"))
      emitExpressionWithPrefix(" ", node.expression)

    }
    def emitSpreadElementExpression(node: SpreadElementExpression) = {
      write("...")
      emitExpression(node.expression)

    }
    def emitClassExpression(node: ClassExpression) = {
      emitClassDeclarationOrExpression(node)

    }
    def emitExpressionWithTypeArguments(node: ExpressionWithTypeArguments) = {
      emitExpression(node.expression)
      emitTypeArguments(node, node.typeArguments)

    }
    def emitAsExpression(node: AsExpression) = {
      emitExpression(node.expression)
      if (node.`type`) {
        write(" as ")
        emit(node.`type`)

      }

    }
    def emitNonNullExpression(node: NonNullExpression) = {
      emitExpression(node.expression)
      write("!")

    }
    def emitTemplateSpan(node: TemplateSpan) = {
      emitExpression(node.expression)
      emit(node.literal)

    }
    def emitBlock(node: Block) = {
      if (isSingleLineEmptyBlock(node)) {
        writeToken(SyntaxKind.OpenBraceToken, node.pos, node)
        write(" ")
        writeToken(SyntaxKind.CloseBraceToken, node.statements.end, node)

      } else {
        writeToken(SyntaxKind.OpenBraceToken, node.pos, node)
        emitBlockStatements(node)
        writeToken(SyntaxKind.CloseBraceToken, node.statements.end, node)

      }

    }
    def emitBlockStatements(node: BlockLike) = {
      if ((getEmitFlags(node) & EmitFlags.SingleLine)) {
        emitList(node, node.statements, ListFormat.SingleLineBlockStatements)

      } else {
        emitList(node, node.statements, ListFormat.MultiLineBlockStatements)

      }

    }
    def emitVariableStatement(node: VariableStatement) = {
      emitModifiers(node, node.modifiers)
      emit(node.declarationList)
      write(";")

    }
    def emitEmptyStatement() = {
      write(";")

    }
    def emitExpressionStatement(node: ExpressionStatement) = {
      emitExpression(node.expression)
      write(";")

    }
    def emitIfStatement(node: IfStatement) = {
      val openParenPos = writeToken(SyntaxKind.IfKeyword, node.pos, node)
      write(" ")
      writeToken(SyntaxKind.OpenParenToken, openParenPos, node)
      emitExpression(node.expression)
      writeToken(SyntaxKind.CloseParenToken, node.expression.end, node)
      emitEmbeddedStatement(node.thenStatement)
      if (node.elseStatement) {
        writeLine()
        writeToken(SyntaxKind.ElseKeyword, node.thenStatement.end, node)
        if ((node.elseStatement.kind === SyntaxKind.IfStatement)) {
          write(" ")
          emit(node.elseStatement)

        } else {
          emitEmbeddedStatement(node.elseStatement)

        }

      }

    }
    def emitDoStatement(node: DoStatement) = {
      write("do")
      emitEmbeddedStatement(node.statement)
      if (isBlock(node.statement)) {
        write(" ")

      } else {
        writeLine()

      }
      write("while (")
      emitExpression(node.expression)
      write(");")

    }
    def emitWhileStatement(node: WhileStatement) = {
      write("while (")
      emitExpression(node.expression)
      write(")")
      emitEmbeddedStatement(node.statement)

    }
    def emitForStatement(node: ForStatement) = {
      val openParenPos = writeToken(SyntaxKind.ForKeyword, node.pos)
      write(" ")
      writeToken(SyntaxKind.OpenParenToken, openParenPos, node)
      emitForBinding(node.initializer)
      write(";")
      emitExpressionWithPrefix(" ", node.condition)
      write(";")
      emitExpressionWithPrefix(" ", node.incrementor)
      write(")")
      emitEmbeddedStatement(node.statement)

    }
    def emitForInStatement(node: ForInStatement) = {
      val openParenPos = writeToken(SyntaxKind.ForKeyword, node.pos)
      write(" ")
      writeToken(SyntaxKind.OpenParenToken, openParenPos)
      emitForBinding(node.initializer)
      write(" in ")
      emitExpression(node.expression)
      writeToken(SyntaxKind.CloseParenToken, node.expression.end)
      emitEmbeddedStatement(node.statement)

    }
    def emitForOfStatement(node: ForOfStatement) = {
      val openParenPos = writeToken(SyntaxKind.ForKeyword, node.pos)
      write(" ")
      writeToken(SyntaxKind.OpenParenToken, openParenPos)
      emitForBinding(node.initializer)
      write(" of ")
      emitExpression(node.expression)
      writeToken(SyntaxKind.CloseParenToken, node.expression.end)
      emitEmbeddedStatement(node.statement)

    }
    def emitForBinding(node: (VariableDeclarationList | Expression)) = {
      if ((node !== undefined)) {
        if ((node.kind === SyntaxKind.VariableDeclarationList)) {
          emit(node)

        } else {
          emitExpression(node.asInstanceOf[Expression])

        }

      }

    }
    def emitContinueStatement(node: ContinueStatement) = {
      writeToken(SyntaxKind.ContinueKeyword, node.pos)
      emitWithPrefix(" ", node.label)
      write(";")

    }
    def emitBreakStatement(node: BreakStatement) = {
      writeToken(SyntaxKind.BreakKeyword, node.pos)
      emitWithPrefix(" ", node.label)
      write(";")

    }
    def emitReturnStatement(node: ReturnStatement) = {
      writeToken(SyntaxKind.ReturnKeyword, node.pos, node)
      emitExpressionWithPrefix(" ", node.expression)
      write(";")

    }
    def emitWithStatement(node: WithStatement) = {
      write("with (")
      emitExpression(node.expression)
      write(")")
      emitEmbeddedStatement(node.statement)

    }
    def emitSwitchStatement(node: SwitchStatement) = {
      val openParenPos = writeToken(SyntaxKind.SwitchKeyword, node.pos)
      write(" ")
      writeToken(SyntaxKind.OpenParenToken, openParenPos)
      emitExpression(node.expression)
      writeToken(SyntaxKind.CloseParenToken, node.expression.end)
      write(" ")
      emit(node.caseBlock)

    }
    def emitLabeledStatement(node: LabeledStatement) = {
      emit(node.label)
      write(": ")
      emit(node.statement)

    }
    def emitThrowStatement(node: ThrowStatement) = {
      write("throw")
      emitExpressionWithPrefix(" ", node.expression)
      write(";")

    }
    def emitTryStatement(node: TryStatement) = {
      write("try ")
      emit(node.tryBlock)
      emit(node.catchClause)
      if (node.finallyBlock) {
        writeLine()
        write("finally ")
        emit(node.finallyBlock)

      }

    }
    def emitDebuggerStatement(node: DebuggerStatement) = {
      writeToken(SyntaxKind.DebuggerKeyword, node.pos)
      write(";")

    }
    def emitVariableDeclaration(node: VariableDeclaration) = {
      emit(node.name)
      emitWithPrefix(": ", node.`type`)
      emitExpressionWithPrefix(" = ", node.initializer)

    }
    def emitVariableDeclarationList(node: VariableDeclarationList) = {
      write(
        (if (isLet(node)) "let "
         else (if (isConst(node)) "const "
               else "var ")))
      emitList(node, node.declarations, ListFormat.VariableDeclarationList)

    }
    def emitFunctionDeclaration(node: FunctionDeclaration) = {
      emitFunctionDeclarationOrExpression(node)

    }
    def emitFunctionDeclarationOrExpression(
        node: (FunctionDeclaration | FunctionExpression)) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      write((if (node.asteriskToken) "function* " else "function "))
      emitIdentifierName(node.name)
      emitSignatureAndBody(node, emitSignatureHead)

    }
    def emitSignatureAndBody(
        node: FunctionLikeDeclaration,
        emitSignatureHead: ((SignatureDeclaration) => Unit)) = {
      val body = node.body
      if (body) {
        if (isBlock(body)) {
          val indentedFlag = (getEmitFlags(node) & EmitFlags.Indented)
          if (indentedFlag) {
            increaseIndent()

          }
          if ((getEmitFlags(node) & EmitFlags.ReuseTempVariableScope)) {
            emitSignatureHead(node)
            emitBlockFunctionBody(body)

          } else {
            val savedTempFlags = tempFlags
            (tempFlags = 0)
            emitSignatureHead(node)
            emitBlockFunctionBody(body)
            (tempFlags = savedTempFlags)

          }
          if (indentedFlag) {
            decreaseIndent()

          }

        } else {
          emitSignatureHead(node)
          write(" ")
          emitExpression(body)

        }

      } else {
        emitSignatureHead(node)
        write(";")

      }

    }
    def emitSignatureHead(
        node: (FunctionDeclaration | FunctionExpression | MethodDeclaration | AccessorDeclaration | ConstructorDeclaration)) = {
      emitTypeParameters(node, node.typeParameters)
      emitParameters(node, node.parameters)
      emitWithPrefix(": ", node.`type`)

    }
    def shouldEmitBlockFunctionBodyOnSingleLine(body: Block) = {
      if ((getEmitFlags(body) & EmitFlags.SingleLine)) {
        return true

      }
      if (body.multiLine) {
        return false

      }
      if (((!nodeIsSynthesized(body)) && (!rangeIsOnSingleLine(
            body,
            currentSourceFile)))) {
        return false

      }
      if ((shouldWriteLeadingLineTerminator(
            body,
            body.statements,
            ListFormat.PreserveLines) || shouldWriteClosingLineTerminator(
            body,
            body.statements,
            ListFormat.PreserveLines))) {
        return false

      }
      var previousStatement: Statement = zeroOfMyType
      (body.statements).foreach { fresh7 =>
        val statement = zeroOfMyType = fresh7 {
          if (shouldWriteSeparatingLineTerminator(
                previousStatement,
                statement,
                ListFormat.PreserveLines)) {
            return false

          }
          (previousStatement = statement)

        }
      }
      return true

    }
    def emitBlockFunctionBody(body: Block) = {
      write(" {")
      increaseIndent()
      emitBodyWithDetachedComments(
        body,
        body.statements,
        (if (shouldEmitBlockFunctionBodyOnSingleLine(body))
           emitBlockFunctionBodyOnSingleLine
         else emitBlockFunctionBodyWorker))
      decreaseIndent()
      writeToken(SyntaxKind.CloseBraceToken, body.statements.end, body)

    }
    def emitBlockFunctionBodyOnSingleLine(body: Block) = {
      emitBlockFunctionBodyWorker(body, true)

    }
    def emitBlockFunctionBodyWorker(
        body: Block,
        emitBlockFunctionBodyOnSingleLine: Boolean) = {
      val statementOffset = emitPrologueDirectives(body.statements, true)
      val helpersEmitted = emitHelpers(body)
      if ((((statementOffset === 0) && (!helpersEmitted)) && emitBlockFunctionBodyOnSingleLine)) {
        decreaseIndent()
        emitList(
          body,
          body.statements,
          ListFormat.SingleLineFunctionBodyStatements)
        increaseIndent()

      } else {
        emitList(
          body,
          body.statements,
          ListFormat.MultiLineFunctionBodyStatements,
          statementOffset)

      }

    }
    def emitClassDeclaration(node: ClassDeclaration) = {
      emitClassDeclarationOrExpression(node)

    }
    def emitClassDeclarationOrExpression(
        node: (ClassDeclaration | ClassExpression)) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      write("class")
      emitNodeWithPrefix(" ", node.name, emitIdentifierName)
      val indentedFlag = (getEmitFlags(node) & EmitFlags.Indented)
      if (indentedFlag) {
        increaseIndent()

      }
      emitTypeParameters(node, node.typeParameters)
      emitList(node, node.heritageClauses, ListFormat.ClassHeritageClauses)
      val savedTempFlags = tempFlags
      (tempFlags = 0)
      write(" {")
      emitList(node, node.members, ListFormat.ClassMembers)
      write("}")
      if (indentedFlag) {
        decreaseIndent()

      }
      (tempFlags = savedTempFlags)

    }
    def emitInterfaceDeclaration(node: InterfaceDeclaration) = {
      console.log("interface decl!")
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      write("interface ")
      emit(node.name)
      emitTypeParameters(node, node.typeParameters)
      emitList(node, node.heritageClauses, ListFormat.HeritageClauses)
      write(" {")
      emitList(node, node.members, ListFormat.InterfaceMembers)
      write("}")

    }
    def emitTypeAliasDeclaration(node: TypeAliasDeclaration) = {
      emitDecorators(node, node.decorators)
      emitModifiers(node, node.modifiers)
      write("type ")
      emit(node.name)
      emitTypeParameters(node, node.typeParameters)
      write(" = ")
      emit(node.`type`)
      write(";")

    }
    def emitEnumDeclaration(node: EnumDeclaration) = {
      emitModifiers(node, node.modifiers)
      write("enum ")
      emit(node.name)
      val savedTempFlags = tempFlags
      (tempFlags = 0)
      write(" {")
      emitList(node, node.members, ListFormat.EnumMembers)
      write("}")
      (tempFlags = savedTempFlags)

    }
    def emitModuleDeclaration(node: ModuleDeclaration) = {
      emitModifiers(node, node.modifiers)
      write(
        (if ((node.flags & NodeFlags.Namespace)) "namespace "
         else "module "))
      emit(node.name)
      var body = node.body
      while ((body.kind === SyntaxKind.ModuleDeclaration)) {
        {
          write(".")
          emit((body.asInstanceOf[ModuleDeclaration]).name)
          (body = (body.asInstanceOf[ModuleDeclaration]).body)

        }
      }
      write(" ")
      emit(body)

    }
    def emitModuleBlock(node: ModuleBlock) = {
      if (isEmptyBlock(node)) {
        write("{ }")

      } else {
        val savedTempFlags = tempFlags
        (tempFlags = 0)
        write("{")
        increaseIndent()
        emitBlockStatements(node)
        write("}")
        (tempFlags = savedTempFlags)

      }

    }
    def emitCaseBlock(node: CaseBlock) = {
      writeToken(SyntaxKind.OpenBraceToken, node.pos)
      emitList(node, node.clauses, ListFormat.CaseBlockClauses)
      writeToken(SyntaxKind.CloseBraceToken, node.clauses.end)

    }
    def emitImportEqualsDeclaration(node: ImportEqualsDeclaration) = {
      emitModifiers(node, node.modifiers)
      write("import ")
      emit(node.name)
      write(" = ")
      emitModuleReference(node.moduleReference)
      write(";")

    }
    def emitModuleReference(node: ModuleReference) = {
      if ((node.kind === SyntaxKind.Identifier)) {
        emitExpression(node.asInstanceOf[Identifier])

      } else {
        emit(node)

      }

    }
    def emitImportDeclaration(node: ImportDeclaration) = {
      emitModifiers(node, node.modifiers)
      write("import ")
      if (node.importClause) {
        emit(node.importClause)
        write(" from ")

      }
      emitExpression(node.moduleSpecifier)
      write(";")

    }
    def emitImportClause(node: ImportClause) = {
      emit(node.name)
      if ((node.name && node.namedBindings)) {
        write(", ")

      }
      emit(node.namedBindings)

    }
    def emitNamespaceImport(node: NamespaceImport) = {
      write("* as ")
      emit(node.name)

    }
    def emitNamedImports(node: NamedImports) = {
      emitNamedImportsOrExports(node)

    }
    def emitImportSpecifier(node: ImportSpecifier) = {
      emitImportOrExportSpecifier(node)

    }
    def emitExportAssignment(node: ExportAssignment) = {
      write((if (node.isExportEquals) "export = " else "export default "))
      emitExpression(node.expression)
      write(";")

    }
    def emitExportDeclaration(node: ExportDeclaration) = {
      write("export ")
      if (node.exportClause) {
        emit(node.exportClause)

      } else {
        write("*")

      }
      if (node.moduleSpecifier) {
        write(" from ")
        emitExpression(node.moduleSpecifier)

      }
      write(";")

    }
    def emitNamedExports(node: NamedExports) = {
      emitNamedImportsOrExports(node)

    }
    def emitExportSpecifier(node: ExportSpecifier) = {
      emitImportOrExportSpecifier(node)

    }
    def emitNamedImportsOrExports(node: NamedImportsOrExports) = {
      write("{")
      emitList(node, node.elements, ListFormat.NamedImportsOrExportsElements)
      write("}")

    }
    def emitImportOrExportSpecifier(node: ImportOrExportSpecifier) = {
      if (node.propertyName) {
        emit(node.propertyName)
        write(" as ")

      }
      emit(node.name)

    }
    def emitExternalModuleReference(node: ExternalModuleReference) = {
      write("require(")
      emitExpression(node.expression)
      write(")")

    }
    def emitJsxElement(node: JsxElement) = {
      emit(node.openingElement)
      emitList(node, node.children, ListFormat.JsxElementChildren)
      emit(node.closingElement)

    }
    def emitJsxSelfClosingElement(node: JsxSelfClosingElement) = {
      write("<")
      emitJsxTagName(node.tagName)
      write(" ")
      emitList(node, node.attributes, ListFormat.JsxElementAttributes)
      write("/>")

    }
    def emitJsxOpeningElement(node: JsxOpeningElement) = {
      write("<")
      emitJsxTagName(node.tagName)
      writeIfAny(node.attributes, " ")
      emitList(node, node.attributes, ListFormat.JsxElementAttributes)
      write(">")

    }
    def emitJsxText(node: JsxText) = {
      writer.writeLiteral(getTextOfNode(node, true))

    }
    def emitJsxClosingElement(node: JsxClosingElement) = {
      write("</")
      emitJsxTagName(node.tagName)
      write(">")

    }
    def emitJsxAttribute(node: JsxAttribute) = {
      emit(node.name)
      emitWithPrefix("=", node.initializer)

    }
    def emitJsxSpreadAttribute(node: JsxSpreadAttribute) = {
      write("{...")
      emitExpression(node.expression)
      write("}")

    }
    def emitJsxExpression(node: JsxExpression) = {
      if (node.expression) {
        write("{")
        emitExpression(node.expression)
        write("}")

      }

    }
    def emitJsxTagName(node: JsxTagNameExpression) = {
      if ((node.kind === SyntaxKind.Identifier)) {
        emitExpression(node.asInstanceOf[Identifier])

      } else {
        emit(node)

      }

    }
    def emitCaseClause(node: CaseClause) = {
      write("case ")
      emitExpression(node.expression)
      write(":")
      emitCaseOrDefaultClauseStatements(node, node.statements)

    }
    def emitDefaultClause(node: DefaultClause) = {
      write("default:")
      emitCaseOrDefaultClauseStatements(node, node.statements)

    }
    def emitCaseOrDefaultClauseStatements(parentNode: Node,
                                          statements: NodeArray[Statement]) = {
      val emitAsSingleStatement = ((statements.length === 1) && (((nodeIsSynthesized(
          parentNode) || nodeIsSynthesized(statements(0))) || rangeStartPositionsAreOnSameLine(
          parentNode,
          statements(0),
          currentSourceFile))))
      if (emitAsSingleStatement) {
        write(" ")
        emit(statements(0))

      } else {
        emitList(
          parentNode,
          statements,
          ListFormat.CaseOrDefaultClauseStatements)

      }

    }
    def emitHeritageClause(node: HeritageClause) = {
      write(" ")
      writeTokenText(node.token)
      write(" ")
      emitList(node, node.types, ListFormat.HeritageClauseTypes)

    }
    def emitCatchClause(node: CatchClause) = {
      writeLine()
      val openParenPos = writeToken(SyntaxKind.CatchKeyword, node.pos)
      write(" ")
      writeToken(SyntaxKind.OpenParenToken, openParenPos)
      emit(node.variableDeclaration)
      writeToken(
        SyntaxKind.CloseParenToken,
        (if (node.variableDeclaration) node.variableDeclaration.end
         else openParenPos))
      write(" ")
      emit(node.block)

    }
    def emitPropertyAssignment(node: PropertyAssignment) = {
      emit(node.name)
      write(": ")
      val initializer = node.initializer
      if ((((getEmitFlags(initializer) & EmitFlags.NoLeadingComments)) === 0)) {
        val commentRange = getCommentRange(initializer)
        emitTrailingCommentsOfPosition(commentRange.pos)

      }
      emitExpression(initializer)

    }
    def emitShorthandPropertyAssignment(node: ShorthandPropertyAssignment) = {
      emit(node.name)
      if (node.objectAssignmentInitializer) {
        write(" = ")
        emitExpression(node.objectAssignmentInitializer)

      }

    }
    def emitEnumMember(node: EnumMember) = {
      emit(node.name)
      emitExpressionWithPrefix(" = ", node.initializer)

    }
    def emitSourceFile(node: SourceFile) = {
      writeLine()
      emitShebang()
      emitBodyWithDetachedComments(node, node.statements, emitSourceFileWorker)

    }
    def emitSourceFileWorker(node: SourceFile) = {
      val statements = node.statements
      val statementOffset = emitPrologueDirectives(statements)
      val savedTempFlags = tempFlags
      (tempFlags = 0)
      emitHelpers(node)
      emitList(node, statements, ListFormat.MultiLine, statementOffset)
      (tempFlags = savedTempFlags)

    }
    def emitPartiallyEmittedExpression(node: PartiallyEmittedExpression) = {
      emitExpression(node.expression)

    }
    def emitPrologueDirectives(statements: Array[Node],
                               startWithNewLine: Boolean): Int = {
      {
        var i = 0
        while ((i < statements.length)) {
          {
            if (isPrologueDirective(statements(i))) {
              if ((startWithNewLine || (i > 0))) {
                writeLine()

              }
              emit(statements(i))

            } else {
              return i

            }

          }
          (i += 1)
        }
      }
      return statements.length

    }
    def emitHelpers(node: Node) = {
      val emitFlags = getEmitFlags(node)
      var helpersEmitted = false
      if ((emitFlags & EmitFlags.EmitEmitHelpers)) {
        (helpersEmitted = emitEmitHelpers(currentSourceFile))

      }
      if ((emitFlags & EmitFlags.EmitExportStar)) {
        writeLines(exportStarHelper)
        (helpersEmitted = true)

      }
      if ((emitFlags & EmitFlags.EmitSuperHelper)) {
        writeLines(superHelper)
        (helpersEmitted = true)

      }
      if ((emitFlags & EmitFlags.EmitAdvancedSuperHelper)) {
        writeLines(advancedSuperHelper)
        (helpersEmitted = true)

      }
      return helpersEmitted

    }
    def emitEmitHelpers(node: SourceFile) = {
      if (compilerOptions.noEmitHelpers) {
        return false

      }
      if ((compilerOptions.importHelpers && ((isExternalModule(node) || compilerOptions.isolatedModules)))) {
        return false

      }
      var helpersEmitted = false
      if ((((languageVersion < ScriptTarget.ES2015)) && (((!extendsEmitted) && (node.flags & NodeFlags.HasClassExtends))))) {
        writeLines(extendsHelper)
        (extendsEmitted = true)
        (helpersEmitted = true)

      }
      if ((((compilerOptions.jsx !== JsxEmit.Preserve) && (!assignEmitted)) && ((node.flags & NodeFlags.HasJsxSpreadAttributes)))) {
        writeLines(assignHelper)
        (assignEmitted = true)

      }
      if (((!decorateEmitted) && (node.flags & NodeFlags.HasDecorators))) {
        writeLines(decorateHelper)
        if (compilerOptions.emitDecoratorMetadata) {
          writeLines(metadataHelper)

        }
        (decorateEmitted = true)
        (helpersEmitted = true)

      }
      if (((!paramEmitted) && (node.flags & NodeFlags.HasParamDecorators))) {
        writeLines(paramHelper)
        (paramEmitted = true)
        (helpersEmitted = true)

      }
      if ((((languageVersion < ScriptTarget.ES2017)) && (((!awaiterEmitted) && (node.flags & NodeFlags.HasAsyncFunctions))))) {
        writeLines(awaiterHelper)
        if ((languageVersion < ScriptTarget.ES2015)) {
          writeLines(generatorHelper)

        }
        (awaiterEmitted = true)
        (helpersEmitted = true)

      }
      if (helpersEmitted) {
        writeLine()

      }
      return helpersEmitted

    }
    def writeLines(text: String): Unit = {
      val lines =
        text.split(java.util.regex.Pattern.compile(raw"""\r\n|\r|\n""", "g")) {
          var i = 0
          while ((i < lines.length)) {
            {
              val line = lines(i)
              if (line.length) {
                if ((i > 0)) {
                  writeLine()

                }
                write(line)

              }

            }
            (i += 1)
          }
        }

    }
    def emitShebang() = {
      val shebang = getShebang(currentText)
      if (shebang) {
        write(shebang)
        writeLine()

      }

    }
    def emitModifiers(node: Node, modifiers: NodeArray[Modifier]) = {
      if ((modifiers && modifiers.length)) {
        emitList(node, modifiers, ListFormat.Modifiers)
        write(" ")

      }

    }
    def emitWithPrefix(prefix: String, node: Node) = {
      emitNodeWithPrefix(prefix, node, emit)

    }
    def emitExpressionWithPrefix(prefix: String, node: Node) = {
      emitNodeWithPrefix(prefix, node, emitExpression)

    }
    def emitNodeWithPrefix(prefix: String,
                           node: Node,
                           emit: ((Node) => Unit)) = {
      if (node) {
        write(prefix)
        emit(node)

      }

    }
    def emitWithSuffix(node: Node, suffix: String) = {
      if (node) {
        emit(node)
        write(suffix)

      }

    }
    def emitEmbeddedStatement(node: Statement) = {
      if (isBlock(node)) {
        write(" ")
        emit(node)

      } else {
        writeLine()
        increaseIndent()
        emit(node)
        decreaseIndent()

      }

    }
    def emitDecorators(parentNode: Node, decorators: NodeArray[Decorator]) = {
      emitList(parentNode, decorators, ListFormat.Decorators)

    }
    def emitTypeArguments(parentNode: Node,
                          typeArguments: NodeArray[TypeNode]) = {
      emitList(parentNode, typeArguments, ListFormat.TypeArguments)

    }
    def emitTypeParameters(
        parentNode: Node,
        typeParameters: NodeArray[TypeParameterDeclaration]) = {
      emitList(parentNode, typeParameters, ListFormat.TypeParameters)

    }
    def emitParameters(parentNode: Node,
                       parameters: NodeArray[ParameterDeclaration]) = {
      emitList(parentNode, parameters, ListFormat.Parameters)

    }
    def emitParametersForArrow(parentNode: Node,
                               parameters: NodeArray[ParameterDeclaration]) = {
      if ((((parameters && (parameters.length === 1)) && (parameters(0).`type` === undefined)) && (parameters(
            0).pos === parentNode.pos))) {
        emit(parameters(0))

      } else {
        emitParameters(parentNode, parameters)

      }

    }
    def emitParametersForIndexSignature(
        parentNode: Node,
        parameters: NodeArray[ParameterDeclaration]) = {
      emitList(parentNode, parameters, ListFormat.IndexSignatureParameters)

    }
    def emitList(parentNode: Node,
                 children: NodeArray[Node],
                 format: ListFormat,
                 start: Int,
                 count: Int) = {
      emitNodeList(emit, parentNode, children, format, start, count)

    }
    def emitExpressionList(parentNode: Node,
                           children: NodeArray[Node],
                           format: ListFormat,
                           start: Int,
                           count: Int) = {
      emitNodeList(emitExpression, parentNode, children, format, start, count)

    }
    def emitNodeList(emit: ((Node) => Unit),
                     parentNode: Node,
                     children: NodeArray[Node],
                     format: ListFormat,
                     start: Nothing = 0,
                     count: Nothing =
                       (if (children) (children.length - start) else 0)) = {
      val isUndefined = (children === undefined)
      if ((isUndefined && (format & ListFormat.OptionalIfUndefined))) {
        return

      }
      val isEmpty = (((isUndefined || (children.length === 0)) || (start >= children.length)) || (count === 0))
      if ((isEmpty && (format & ListFormat.OptionalIfEmpty))) {
        return

      }
      if ((format & ListFormat.BracketsMask)) {
        write(getOpeningBracket(format))

      }
      if (isEmpty) {
        if ((format & ListFormat.MultiLine)) {
          writeLine()

        } else if ((format & ListFormat.SpaceBetweenBraces)) {
          write(" ")

        }

      } else {
        val mayEmitInterveningComments = (((format & ListFormat.NoInterveningComments)) === 0)
        var shouldEmitInterveningComments = mayEmitInterveningComments
        if (shouldWriteLeadingLineTerminator(parentNode, children, format)) {
          writeLine()
          (shouldEmitInterveningComments = false)

        } else if ((format & ListFormat.SpaceBetweenBraces)) {
          write(" ")

        }
        if ((format & ListFormat.Indented)) {
          increaseIndent()

        }
        var previousSibling: Node = zeroOfMyType
        var shouldDecreaseIndentAfterEmit: Boolean = zeroOfMyType
        val delimiter = getDelimiter(format) {
          var i = 0
          while ((i < count)) {
            {
              val child = children((start + i))
              if (previousSibling) {
                write(delimiter)
                if (shouldWriteSeparatingLineTerminator(
                      previousSibling,
                      child,
                      format)) {
                  if ((((format & ((ListFormat.LinesMask | ListFormat.Indented)))) === ListFormat.SingleLine)) {
                    increaseIndent()
                    (shouldDecreaseIndentAfterEmit = true)

                  }
                  writeLine()
                  (shouldEmitInterveningComments = false)

                } else if ((previousSibling && (format & ListFormat.SpaceBetweenSiblings))) {
                  write(" ")

                }

              }
              if (shouldEmitInterveningComments) {
                val commentRange = getCommentRange(child)
                emitTrailingCommentsOfPosition(commentRange.pos)

              } else {
                (shouldEmitInterveningComments = mayEmitInterveningComments)

              }
              emit(child)
              if (shouldDecreaseIndentAfterEmit) {
                decreaseIndent()
                (shouldDecreaseIndentAfterEmit = false)

              }
              (previousSibling = child)

            }
            (i += 1)
          }
        }
        val hasTrailingComma = (((format & ListFormat.AllowTrailingComma)) && children.hasTrailingComma)
        if (((format & ListFormat.CommaDelimited) && hasTrailingComma)) {
          write(",")

        }
        if ((format & ListFormat.Indented)) {
          decreaseIndent()

        }
        if (shouldWriteClosingLineTerminator(parentNode, children, format)) {
          writeLine()

        } else if ((format & ListFormat.SpaceBetweenBraces)) {
          write(" ")

        }

      }
      if ((format & ListFormat.BracketsMask)) {
        write(getClosingBracket(format))

      }

    }
    def writeIfAny(nodes: NodeArray[Node], text: String) = {
      if ((nodes && (nodes.length > 0))) {
        write(text)

      }

    }
    def writeIfPresent(node: Node, text: String) = {
      if ((node !== undefined)) {
        write(text)

      }

    }
    def writeToken(token: SyntaxKind, pos: Int, contextNode: Node) = {
      return emitTokenWithSourceMap(contextNode, token, pos, writeTokenText)

    }
    def writeTokenText(token: SyntaxKind, pos: Int) = {
      val tokenString = tokenToString(token)
      write(tokenString)
      return (if ((pos < 0)) pos else (pos + tokenString.length))

    }
    def increaseIndentIf(value: Boolean,
                         valueToWriteWhenNotIndenting: String) = {
      if (value) {
        increaseIndent()
        writeLine()

      } else if (valueToWriteWhenNotIndenting) {
        write(valueToWriteWhenNotIndenting)

      }

    }
    def decreaseIndentIf(value1: Boolean, value2: Boolean) = {
      if (value1) {
        decreaseIndent()

      }
      if (value2) {
        decreaseIndent()

      }

    }
    def shouldWriteLeadingLineTerminator(parentNode: Node,
                                         children: NodeArray[Node],
                                         format: ListFormat) = {
      if ((format & ListFormat.MultiLine)) {
        return true

      }
      if ((format & ListFormat.PreserveLines)) {
        if ((format & ListFormat.PreferNewLine)) {
          return true

        }
        val firstChild = children(0)
        if ((firstChild === undefined)) {
          return (!rangeIsOnSingleLine(parentNode, currentSourceFile))

        } else if ((positionIsSynthesized(parentNode.pos) || nodeIsSynthesized(
                     firstChild))) {
          return synthesizedNodeStartsOnNewLine(firstChild, format)

        } else {
          return (!rangeStartPositionsAreOnSameLine(
            parentNode,
            firstChild,
            currentSourceFile))

        }

      } else {
        return false

      }

    }
    def shouldWriteSeparatingLineTerminator(previousNode: Node,
                                            nextNode: Node,
                                            format: ListFormat) = {
      if ((format & ListFormat.MultiLine)) {
        return true

      } else if ((format & ListFormat.PreserveLines)) {
        if (((previousNode === undefined) || (nextNode === undefined))) {
          return false

        } else if ((nodeIsSynthesized(previousNode) || nodeIsSynthesized(
                     nextNode))) {
          return (synthesizedNodeStartsOnNewLine(previousNode, format) || synthesizedNodeStartsOnNewLine(
            nextNode,
            format))

        } else {
          return (!rangeEndIsOnSameLineAsRangeStart(
            previousNode,
            nextNode,
            currentSourceFile))

        }

      } else {
        return nextNode.startsOnNewLine

      }

    }
    def shouldWriteClosingLineTerminator(parentNode: Node,
                                         children: NodeArray[Node],
                                         format: ListFormat) = {
      if ((format & ListFormat.MultiLine)) {
        return (((format & ListFormat.NoTrailingNewLine)) === 0)

      } else if ((format & ListFormat.PreserveLines)) {
        if ((format & ListFormat.PreferNewLine)) {
          return true

        }
        val lastChild = lastOrUndefined(children)
        if ((lastChild === undefined)) {
          return (!rangeIsOnSingleLine(parentNode, currentSourceFile))

        } else if ((positionIsSynthesized(parentNode.pos) || nodeIsSynthesized(
                     lastChild))) {
          return synthesizedNodeStartsOnNewLine(lastChild, format)

        } else {
          return (!rangeEndPositionsAreOnSameLine(
            parentNode,
            lastChild,
            currentSourceFile))

        }

      } else {
        return false

      }

    }
    def synthesizedNodeStartsOnNewLine(node: Node, format: ListFormat) = {
      if (nodeIsSynthesized(node)) {
        val startsOnNewLine = node.startsOnNewLine
        if ((startsOnNewLine === undefined)) {
          return (((format & ListFormat.PreferNewLine)) !== 0)

        }
        return startsOnNewLine

      }
      return (((format & ListFormat.PreferNewLine)) !== 0)

    }
    def needsIndentation(parent: Node, node1: Node, node2: Node): Boolean = {
      (parent = skipSynthesizedParentheses(parent))
      (node1 = skipSynthesizedParentheses(node1))
      (node2 = skipSynthesizedParentheses(node2))
      if (node2.startsOnNewLine) {
        return true

      }
      return ((((!nodeIsSynthesized(parent)) && (!nodeIsSynthesized(node1))) && (!nodeIsSynthesized(
        node2))) && (!rangeEndIsOnSameLineAsRangeStart(
        node1,
        node2,
        currentSourceFile)))

    }
    def skipSynthesizedParentheses(node: Node) = {
      while (((node.kind === SyntaxKind.ParenthesizedExpression) && nodeIsSynthesized(
               node))) {
        {
          (node = (node.asInstanceOf[ParenthesizedExpression]).expression)

        }
      }
      return node

    }
    def getTextOfNode(node: Node, includeTrivia: Boolean): String = {
      if (isGeneratedIdentifier(node)) {
        return getGeneratedIdentifier(node.asInstanceOf[Identifier])

      } else if ((isIdentifier(node) && ((nodeIsSynthesized(node) || (!node.parent))))) {
        return unescapeIdentifier(node.text)

      } else if (((node.kind === SyntaxKind.StringLiteral) && (node
                   .asInstanceOf[StringLiteral])
                   .textSourceNode)) {
        return getTextOfNode(
          (node.asInstanceOf[StringLiteral]).textSourceNode,
          includeTrivia)

      } else if ((isLiteralExpression(node) && ((nodeIsSynthesized(node) || (!node.parent))))) {
        return node.text

      }
      return getSourceTextOfNodeFromSourceFile(
        currentSourceFile,
        node,
        includeTrivia)

    }
    def getLiteralTextOfNode(node: LiteralLikeNode): String = {
      if (((node.kind === SyntaxKind.StringLiteral) && (node
            .asInstanceOf[StringLiteral])
            .textSourceNode)) {
        val textSourceNode = (node.asInstanceOf[StringLiteral]).textSourceNode
        if (isIdentifier(textSourceNode)) {
          return (("\"" + escapeNonAsciiCharacters(
            escapeString(getTextOfNode(textSourceNode)))) + "\"")

        } else {
          return getLiteralTextOfNode(textSourceNode)

        }

      }
      return getLiteralText(node, currentSourceFile, languageVersion)

    }
    def isSingleLineEmptyBlock(block: Block) = {
      return ((!block.multiLine) && isEmptyBlock(block))

    }
    def isEmptyBlock(block: BlockLike) = {
      return ((block.statements.length === 0) && rangeEndIsOnSameLineAsRangeStart(
        block,
        block,
        currentSourceFile))

    }
    def isUniqueName(name: String): Boolean = {
      return (((!resolver.hasGlobalName(name)) && (!hasProperty(
        currentFileIdentifiers,
        name))) && (!hasProperty(generatedNameSet, name)))

    }
    def isUniqueLocalName(name: String, container: Node): Boolean = {
      {
        var node = container
        while (isNodeDescendantOf(node, container)) {
          {
            if ((node.locals && hasProperty(node.locals, name))) {
              if ((node
                    .locals(name)
                    .flags & (((SymbolFlags.Value | SymbolFlags.ExportValue) | SymbolFlags.Alias)))) {
                return false

              }

            }

          }
          (node = node.nextContainer)
        }
      }
      return true

    }
    def makeTempVariableName(flags: TempFlags): String = {
      if ((flags && (!((tempFlags & flags))))) {
        val name = (if ((flags === TempFlags._i)) "_i" else "_n")
        if (isUniqueName(name)) {
          (tempFlags |= flags)
          return name

        }

      }
      while (true) {
        {
          val count = (tempFlags & TempFlags.CountMask)
          (tempFlags += 1)
          if (((count !== 8) && (count !== 13))) {
            val name =
              (if ((count < 26))
                 ("_" + String.fromCharCode((CharacterCodes.a + count)))
               else ("_" + ((count - 26))))
            if (isUniqueName(name)) {
              return name

            }

          }

        }
      }

    }
    def makeUniqueName(baseName: String): String = {
      if ((baseName.charCodeAt((baseName.length - 1)) !== CharacterCodes._underscore_)) {
        (baseName += "_")

      }
      var i = 1
      while (true) {
        {
          val generatedName = (baseName + i)
          if (isUniqueName(generatedName)) {
            return (generatedNameSet(generatedName) = generatedName)

          }
          (i += 1)

        }
      }

    }
    def generateNameForModuleOrEnum(
        node: (ModuleDeclaration | EnumDeclaration)) = {
      val name = getTextOfNode(node.name)
      return (if (isUniqueLocalName(name, node)) name
              else makeUniqueName(name))

    }
    def generateNameForImportOrExportDeclaration(
        node: (ImportDeclaration | ExportDeclaration)) = {
      val expr = getExternalModuleName(node)
      val baseName =
        (if ((expr.kind === SyntaxKind.StringLiteral))
           escapeIdentifier(
             makeIdentifierFromModuleName(
               (expr.asInstanceOf[LiteralExpression]).text))
         else "module")
      return makeUniqueName(baseName)

    }
    def generateNameForExportDefault() = {
      return makeUniqueName("default")

    }
    def generateNameForClassExpression() = {
      return makeUniqueName("class")

    }
    def generateNameForNode(node: Node): String = {
      node.kind match {
        case SyntaxKind.Identifier =>
          return makeUniqueName(getTextOfNode(node))
        case SyntaxKind.ModuleDeclaration | SyntaxKind.EnumDeclaration =>
          return generateNameForModuleOrEnum(
            node.asInstanceOf[(ModuleDeclaration | EnumDeclaration)])
        case SyntaxKind.ImportDeclaration | SyntaxKind.ExportDeclaration =>
          return generateNameForImportOrExportDeclaration(
            node.asInstanceOf[(ImportDeclaration | ExportDeclaration)])
        case SyntaxKind.FunctionDeclaration | SyntaxKind.ClassDeclaration |
            SyntaxKind.ExportAssignment =>
          return generateNameForExportDefault()
        case SyntaxKind.ClassExpression =>
          return generateNameForClassExpression()
        case _ =>
          return makeTempVariableName(TempFlags.Auto)
      }

    }
    def generateName(name: Identifier) = {
      name.autoGenerateKind match {
        case GeneratedIdentifierKind.Auto =>
          return makeTempVariableName(TempFlags.Auto)
        case GeneratedIdentifierKind.Loop =>
          return makeTempVariableName(TempFlags._i)
        case GeneratedIdentifierKind.Unique =>
          return makeUniqueName(name.text)
        case _ =>
      }
      Debug.fail("Unsupported GeneratedIdentifierKind.")

    }
    def getNodeForGeneratedName(name: Identifier) = {
      val autoGenerateId = name.autoGenerateId
      var node = name.asInstanceOf[Node]
      var original = node.original
      while (original) {
        {
          (node = original)
          if (((isIdentifier(node) && (node.autoGenerateKind === GeneratedIdentifierKind.Node)) && (node.autoGenerateId !== autoGenerateId))) {
            break()

          }
          (original = node.original)

        }
      }
      return node

    }
    def getGeneratedIdentifier(name: Identifier) = {
      if ((name.autoGenerateKind === GeneratedIdentifierKind.Node)) {
        val node = getNodeForGeneratedName(name)
        val nodeId = getNodeId(node)
        return (nodeIdToGeneratedName(nodeId) || ((
          nodeIdToGeneratedName(nodeId) =
            unescapeIdentifier(generateNameForNode(node)))))

      } else {
        val autoGenerateId = name.autoGenerateId
        return (autoGeneratedIdToGeneratedName(autoGenerateId) || ((
          autoGeneratedIdToGeneratedName(autoGenerateId) =
            unescapeIdentifier(generateName(name)))))

      }

    }
    def createDelimiterMap() = {
      val delimiters: Array[String] = Array()
      (delimiters(ListFormat.None) = "")
      (delimiters(ListFormat.CommaDelimited) = ",")
      (delimiters(ListFormat.BarDelimited) = " |")
      (delimiters(ListFormat.AmpersandDelimited) = " &")
      return delimiters

    }
    def getDelimiter(format: ListFormat) = {
      return delimiters((format & ListFormat.DelimitersMask))

    }
    def createBracketsMap() = {
      val brackets: Array[Array[String]] = Array()
      (brackets(ListFormat.Braces) = Array("{", "}"))
      (brackets(ListFormat.Parenthesis) = Array("(", ")"))
      (brackets(ListFormat.AngleBrackets) = Array("<", ">"))
      (brackets(ListFormat.SquareBrackets) = Array("[", "]"))
      return brackets

    }
    def getOpeningBracket(format: ListFormat) = {
      return brackets((format & ListFormat.BracketsMask))(0)

    }
    def getClosingBracket(format: ListFormat) = {
      return brackets((format & ListFormat.BracketsMask))(1)

    }

  }
  sealed abstract class ListFormat
  object ListFormat {
    case object None extends ListFormat
    case object SingleLine extends ListFormat
    case object MultiLine extends ListFormat
    case object PreserveLines extends ListFormat
    case object LinesMask extends ListFormat
    case object NotDelimited extends ListFormat
    case object BarDelimited extends ListFormat
    case object AmpersandDelimited extends ListFormat
    case object CommaDelimited extends ListFormat
    case object DelimitersMask extends ListFormat
    case object AllowTrailingComma extends ListFormat
    case object Indented extends ListFormat
    case object SpaceBetweenBraces extends ListFormat
    case object SpaceBetweenSiblings extends ListFormat
    case object Braces extends ListFormat
    case object Parenthesis extends ListFormat
    case object AngleBrackets extends ListFormat
    case object SquareBrackets extends ListFormat
    case object BracketsMask extends ListFormat
    case object OptionalIfUndefined extends ListFormat
    case object OptionalIfEmpty extends ListFormat
    case object Optional extends ListFormat
    case object PreferNewLine extends ListFormat
    case object NoTrailingNewLine extends ListFormat
    case object NoInterveningComments extends ListFormat
    case object Modifiers extends ListFormat
    case object HeritageClauses extends ListFormat
    case object TypeLiteralMembers extends ListFormat
    case object TupleTypeElements extends ListFormat
    case object UnionTypeConstituents extends ListFormat
    case object IntersectionTypeConstituents extends ListFormat
    case object ObjectBindingPatternElements extends ListFormat
    case object ArrayBindingPatternElements extends ListFormat
    case object ObjectLiteralExpressionProperties extends ListFormat
    case object ArrayLiteralExpressionElements extends ListFormat
    case object CallExpressionArguments extends ListFormat
    case object NewExpressionArguments extends ListFormat
    case object TemplateExpressionSpans extends ListFormat
    case object SingleLineBlockStatements extends ListFormat
    case object MultiLineBlockStatements extends ListFormat
    case object VariableDeclarationList extends ListFormat
    case object SingleLineFunctionBodyStatements extends ListFormat
    case object MultiLineFunctionBodyStatements extends ListFormat
    case object ClassHeritageClauses extends ListFormat
    case object ClassMembers extends ListFormat
    case object InterfaceMembers extends ListFormat
    case object EnumMembers extends ListFormat
    case object CaseBlockClauses extends ListFormat
    case object NamedImportsOrExportsElements extends ListFormat
    case object JsxElementChildren extends ListFormat
    case object JsxElementAttributes extends ListFormat
    case object CaseOrDefaultClauseStatements extends ListFormat
    case object HeritageClauseTypes extends ListFormat
    case object SourceFileStatements extends ListFormat
    case object Decorators extends ListFormat
    case object TypeArguments extends ListFormat
    case object TypeParameters extends ListFormat
    case object Parameters extends ListFormat
    case object IndexSignatureParameters extends ListFormat
  }
}
