package scalatscomp
object DeclarationEmitter {
  trait ModuleElementDeclarationEmitInfo {
    var node: Node
    var outputPos: Int
    var indent: Int
    var asynchronousOutput: String
    var subModuleElementDeclarationEmitInfo: Array[
        ModuleElementDeclarationEmitInfo]
    var isVisible: Boolean
  }
  trait DeclarationEmit {
    var reportedDeclarationError: Boolean
    var moduleElementDeclarationEmitInfo: Array[
        ModuleElementDeclarationEmitInfo]
    var synchronousDeclarationOutput: String
    var referencesOutput: String
  }
  type GetSymbolAccessibilityDiagnostic =
    ((SymbolAccessibilityResult) => SymbolAccessibilityDiagnostic)
  trait EmitTextWriterWithSymbolWriter
      extends EmitTextWriter with SymbolWriter {
    var getSymbolAccessibilityDiagnostic: GetSymbolAccessibilityDiagnostic
  }
  trait SymbolAccessibilityDiagnostic {
    var errorNode: Node
    var diagnosticMessage: DiagnosticMessage
    var typeName: DeclarationName
  }
  def getDeclarationDiagnostics(host: EmitHost, resolver: EmitResolver,
      targetSourceFile: SourceFile): Array[Diagnostic] = {
    val declarationDiagnostics = createDiagnosticCollection()
    forEachExpectedEmitFile(host, getDeclarationDiagnosticsFromFile,
        targetSourceFile)
    return declarationDiagnostics.getDiagnostics(
        (if (targetSourceFile) targetSourceFile.fileName else undefined))
    def getDeclarationDiagnosticsFromFile(obj: EmitFileNames,
        sources: Array[SourceFile], isBundledEmit: Boolean) = {
      const fresh1 = obj
      val declarationFilePath = fresh1.declarationFilePath
      emitDeclarations(host, resolver, declarationDiagnostics,
          declarationFilePath, sources, isBundledEmit, false)

    }

  }
  def emitDeclarations(host: EmitHost, resolver: EmitResolver,
      emitterDiagnostics: DiagnosticCollection, declarationFilePath: String,
      sourceFiles: Array[SourceFile], isBundledEmit: Boolean,
      emitOnlyDtsFiles: Boolean): DeclarationEmit = {
    val newLine = host.getNewLine()
    val compilerOptions = host.getCompilerOptions()
    var write: ((String) => Unit) = zeroOfMyType
    var writeLine: (() => Unit) = zeroOfMyType
    var increaseIndent: (() => Unit) = zeroOfMyType
    var decreaseIndent: (() => Unit) = zeroOfMyType
    var writeTextOfNode: ((String, Node) => Unit) = zeroOfMyType
    var writer: EmitTextWriterWithSymbolWriter = zeroOfMyType
    createAndSetNewTextWriterWithSymbolWriter()
    var enclosingDeclaration: Node = zeroOfMyType
    var resultHasExternalModuleIndicator: Boolean = zeroOfMyType
    var currentText: String = zeroOfMyType
    var currentLineMap: Array[Int] = zeroOfMyType
    var currentIdentifiers: Map[String] = zeroOfMyType
    var isCurrentFileExternalModule: Boolean = zeroOfMyType
    var reportedDeclarationError = false
    var errorNameNode: DeclarationName = zeroOfMyType
    val emitJsDocComments =
      (if (compilerOptions.removeComments) noop else writeJsDocComments)
    val emit = (if (compilerOptions.stripInternal) stripInternal else emitNode)
    var noDeclare: Boolean = zeroOfMyType
    var moduleElementDeclarationEmitInfo: Array[ModuleElementDeclarationEmitInfo] =
      Array()
    var asynchronousSubModuleDeclarationEmitInfo: Array[ModuleElementDeclarationEmitInfo] =
      zeroOfMyType
    var referencesOutput = ""
    var usedTypeDirectiveReferences: Map[String] = zeroOfMyType
    val emittedReferencedFiles: Array[SourceFile] = Array()
    var addedGlobalFileReference = false
    var allSourcesModuleElementDeclarationEmitInfo: Array[ModuleElementDeclarationEmitInfo] =
      Array()
    forEach(sourceFiles,
        (sourceFile => {
          if (isSourceFileJavaScript(sourceFile)) {
            return

          }
          if ((!compilerOptions.noResolve)) {
            forEach(sourceFile.referencedFiles,
                (fileReference => {
                  val referencedFile =
                    tryResolveScriptReference(host, sourceFile, fileReference)
                  if ((referencedFile && (!contains(emittedReferencedFiles,
                          referencedFile)))) {
                    if (writeReferencePath(referencedFile,
                            ((!isBundledEmit) && (!addedGlobalFileReference)),
                            emitOnlyDtsFiles)) {
                      (addedGlobalFileReference = true)

                    }
                    emittedReferencedFiles.push(referencedFile)

                  }

                }))

          }
          (resultHasExternalModuleIndicator = false)
          if (((!isBundledEmit) || (!isExternalModule(sourceFile)))) {
            (noDeclare = false)
            emitSourceFile(sourceFile)

          } else if (isExternalModule(sourceFile)) {
            (noDeclare = true)
            write(s"""declare module \"${getResolvedExternalModuleName(host,
                sourceFile)}\" {""")
            writeLine()
            increaseIndent()
            emitSourceFile(sourceFile)
            decreaseIndent()
            write("}")
            writeLine()

          }
          if (moduleElementDeclarationEmitInfo.length) {
            val oldWriter = writer
            forEach(moduleElementDeclarationEmitInfo,
                (aliasEmitInfo => {
                  if ((aliasEmitInfo.isVisible && (!aliasEmitInfo.asynchronousOutput))) {
                    Debug.assert(
                        (aliasEmitInfo.node.kind === SyntaxKind.ImportDeclaration))
                    createAndSetNewTextWriterWithSymbolWriter()
                    Debug.assert(
                        ((aliasEmitInfo.indent === 0) || (((aliasEmitInfo.indent === 1) && isBundledEmit)))) {
                      var i = 0
                      while ((i < aliasEmitInfo.indent)) {
                        {
                          increaseIndent()

                        }
                        (i += 1)
                      }
                    }
                    writeImportDeclaration(
                        aliasEmitInfo.node.asInstanceOf[ImportDeclaration])
                    (aliasEmitInfo.asynchronousOutput = writer.getText()) {
                      var i = 0
                      while ((i < aliasEmitInfo.indent)) {
                        {
                          decreaseIndent()

                        }
                        (i += 1)
                      }
                    }

                  }

                }))
            setWriter(oldWriter)
            (allSourcesModuleElementDeclarationEmitInfo =
              allSourcesModuleElementDeclarationEmitInfo.concat(
                  moduleElementDeclarationEmitInfo))
            (moduleElementDeclarationEmitInfo = Array())

          }
          if (((((!isBundledEmit) && isExternalModule(sourceFile)) && sourceFile.moduleAugmentations.length) && (!resultHasExternalModuleIndicator))) {
            write("export {};")
            writeLine()

          }

        }))
    if (usedTypeDirectiveReferences) {
      (usedTypeDirectiveReferences).keys.foreach { fresh2 =>
        val directive = zeroOfMyType = fresh2 {
          (referencesOutput += s"""/// <reference types=\"${directive}\" />${newLine}""")

        }
      }

    }
    return Map("reportedDeclarationError" -> reportedDeclarationError,
        "moduleElementDeclarationEmitInfo" -> allSourcesModuleElementDeclarationEmitInfo,
        "synchronousDeclarationOutput" -> writer.getText(),
        "referencesOutput" -> referencesOutput)
    def hasInternalAnnotation(range: CommentRange) = {
      val comment = currentText.substring(range.pos, range.end)
      return (comment.indexOf("@internal") >= 0)

    }
    def stripInternal(node: Node) = {
      if (node) {
        val leadingCommentRanges =
          getLeadingCommentRanges(currentText, node.pos)
        if (forEach(leadingCommentRanges, hasInternalAnnotation)) {
          return

        }
        emitNode(node)

      }

    }
    def createAndSetNewTextWriterWithSymbolWriter(): Unit = {
      val writer =
        createTextWriter(newLine).asInstanceOf[EmitTextWriterWithSymbolWriter]
      (writer.trackSymbol = trackSymbol)
      (writer.reportInaccessibleThisError = reportInaccessibleThisError)
      (writer.writeKeyword = writer.write)
      (writer.writeOperator = writer.write)
      (writer.writePunctuation = writer.write)
      (writer.writeSpace = writer.write)
      (writer.writeStringLiteral = writer.writeLiteral)
      (writer.writeParameter = writer.write)
      (writer.writeSymbol = writer.write)
      setWriter(writer)

    }
    def setWriter(newWriter: EmitTextWriterWithSymbolWriter) = {
      (writer = newWriter)
      (write = newWriter.write)
      (writeTextOfNode = newWriter.writeTextOfNode)
      (writeLine = newWriter.writeLine)
      (increaseIndent = newWriter.increaseIndent)
      (decreaseIndent = newWriter.decreaseIndent)

    }
    def writeAsynchronousModuleElements(nodes: Array[Node]) = {
      val oldWriter = writer
      forEach(nodes,
          (declaration => {
            var nodeToCheck: Node = zeroOfMyType
            if ((declaration.kind === SyntaxKind.VariableDeclaration)) {
              (nodeToCheck = declaration.parent.parent)

            } else if ((((declaration.kind === SyntaxKind.NamedImports) || (declaration.kind === SyntaxKind.ImportSpecifier)) || (declaration.kind === SyntaxKind.ImportClause))) {
              Debug.fail(
                  "We should be getting ImportDeclaration instead to write")

            } else {
              (nodeToCheck = declaration)

            }
            var moduleElementEmitInfo = forEach(
                moduleElementDeclarationEmitInfo,
                (declEmitInfo =>
                      (if ((declEmitInfo.node === nodeToCheck)) declEmitInfo
                       else undefined)))
            if (((!moduleElementEmitInfo) && asynchronousSubModuleDeclarationEmitInfo)) {
              (moduleElementEmitInfo = forEach(
                  asynchronousSubModuleDeclarationEmitInfo,
                  (declEmitInfo =>
                        (if ((declEmitInfo.node === nodeToCheck)) declEmitInfo
                         else undefined))))

            }
            if (moduleElementEmitInfo) {
              if ((moduleElementEmitInfo.node.kind === SyntaxKind.ImportDeclaration)) {
                (moduleElementEmitInfo.isVisible = true)

              } else {
                createAndSetNewTextWriterWithSymbolWriter() {
                  var declarationIndent = moduleElementEmitInfo.indent
                  while (declarationIndent) {
                    {
                      increaseIndent()

                    }
                    (declarationIndent -= 1)
                  }
                }
                if ((nodeToCheck.kind === SyntaxKind.ModuleDeclaration)) {
                  Debug.assert(
                      (asynchronousSubModuleDeclarationEmitInfo === undefined))
                  (asynchronousSubModuleDeclarationEmitInfo = Array())

                }
                writeModuleElement(nodeToCheck)
                if ((nodeToCheck.kind === SyntaxKind.ModuleDeclaration)) {
                  (moduleElementEmitInfo.subModuleElementDeclarationEmitInfo =
                    asynchronousSubModuleDeclarationEmitInfo)
                  (asynchronousSubModuleDeclarationEmitInfo = undefined)

                }
                (moduleElementEmitInfo.asynchronousOutput = writer.getText())

              }

            }

          }))
      setWriter(oldWriter)

    }
    def recordTypeReferenceDirectivesIfNecessary(
        typeReferenceDirectives: Array[String]): Unit = {
      if ((!typeReferenceDirectives)) {
        return

      }
      if ((!usedTypeDirectiveReferences)) {
        (usedTypeDirectiveReferences = createMap[String]())

      }
      (typeReferenceDirectives).foreach { fresh3 =>
        val directive = zeroOfMyType = fresh3 {
          if ((!((directiveinusedTypeDirectiveReferences)))) {
            (usedTypeDirectiveReferences(directive) = directive)

          }

        }
      }

    }
    def handleSymbolAccessibilityError(
        symbolAccessibilityResult: SymbolAccessibilityResult) = {
      if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.Accessible)) {
        if ((symbolAccessibilityResult && symbolAccessibilityResult.aliasesToMakeVisible)) {
          writeAsynchronousModuleElements(
              symbolAccessibilityResult.aliasesToMakeVisible)

        }

      } else {
        (reportedDeclarationError = true)
        val errorInfo =
          writer.getSymbolAccessibilityDiagnostic(symbolAccessibilityResult)
        if (errorInfo) {
          if (errorInfo.typeName) {
            emitterDiagnostics.add(
                createDiagnosticForNode(
                    (symbolAccessibilityResult.errorNode || errorInfo.errorNode),
                    errorInfo.diagnosticMessage,
                    getTextOfNodeFromSourceText(currentText,
                        errorInfo.typeName),
                    symbolAccessibilityResult.errorSymbolName,
                    symbolAccessibilityResult.errorModuleName))

          } else {
            emitterDiagnostics.add(
                createDiagnosticForNode(
                    (symbolAccessibilityResult.errorNode || errorInfo.errorNode),
                    errorInfo.diagnosticMessage,
                    symbolAccessibilityResult.errorSymbolName,
                    symbolAccessibilityResult.errorModuleName))

          }

        }

      }

    }
    def trackSymbol(symbol: Symbol, enclosingDeclaration: Node,
        meaning: SymbolFlags) = {
      handleSymbolAccessibilityError(resolver.isSymbolAccessible(symbol,
              enclosingDeclaration, meaning, true))
      recordTypeReferenceDirectivesIfNecessary(
          resolver.getTypeReferenceDirectivesForSymbol(symbol, meaning))

    }
    def reportInaccessibleThisError() = {
      if (errorNameNode) {
        (reportedDeclarationError = true)
        emitterDiagnostics.add(createDiagnosticForNode(errorNameNode,
                Diagnostics.The_inferred_type_of_0_references_an_inaccessible_this_type_A_type_annotation_is_necessary,
                declarationNameToString(errorNameNode)))

      }

    }
    def writeTypeOfDeclaration(
        declaration: (AccessorDeclaration | VariableLikeDeclaration),
        `type`: TypeNode,
        getSymbolAccessibilityDiagnostic: GetSymbolAccessibilityDiagnostic) = {
      (writer.getSymbolAccessibilityDiagnostic =
        getSymbolAccessibilityDiagnostic)
      write(": ")
      if (`type`) {
        emitType(`type`)

      } else {
        (errorNameNode = declaration.name)
        resolver.writeTypeOfDeclaration(declaration, enclosingDeclaration,
            (TypeFormatFlags.UseTypeOfFunction | TypeFormatFlags.UseTypeAliasValue),
            writer)
        (errorNameNode = undefined)

      }

    }
    def writeReturnTypeAtSignature(signature: SignatureDeclaration,
        getSymbolAccessibilityDiagnostic: GetSymbolAccessibilityDiagnostic) = {
      (writer.getSymbolAccessibilityDiagnostic =
        getSymbolAccessibilityDiagnostic)
      write(": ")
      if (signature.`type`) {
        emitType(signature.`type`)

      } else {
        (errorNameNode = signature.name)
        resolver.writeReturnTypeOfSignatureDeclaration(signature,
            enclosingDeclaration,
            (TypeFormatFlags.UseTypeOfFunction | TypeFormatFlags.UseTypeAliasValue),
            writer)
        (errorNameNode = undefined)

      }

    }
    def emitLines(nodes: Array[Node]) = {
      (nodes).foreach { fresh4 =>
        val node = zeroOfMyType = fresh4 {
          emit(node)

        }
      }

    }
    def emitSeparatedList(nodes: Array[Node], separator: String,
        eachNodeEmitFn: ((Node) => Unit), canEmitFn: ((Node) => Boolean)) = {
      var currentWriterPos = writer.getTextPos()
      (nodes).foreach { fresh5 =>
        val node = zeroOfMyType = fresh5 {
          if (((!canEmitFn) || canEmitFn(node))) {
            if ((currentWriterPos !== writer.getTextPos())) {
              write(separator)

            }
            (currentWriterPos = writer.getTextPos())
            eachNodeEmitFn(node)

          }

        }
      }

    }
    def emitCommaList(nodes: Array[Node], eachNodeEmitFn: ((Node) => Unit),
        canEmitFn: ((Node) => Boolean)) = {
      emitSeparatedList(nodes, ", ", eachNodeEmitFn, canEmitFn)

    }
    def writeJsDocComments(declaration: Node) = {
      if (declaration) {
        val jsDocComments = getJsDocCommentsFromText(declaration, currentText)
        emitNewLineBeforeLeadingComments(currentLineMap, writer, declaration,
            jsDocComments)
        emitComments(currentText, currentLineMap, writer, jsDocComments, false,
            true, newLine, writeCommentRange)

      }

    }
    def emitTypeWithNewGetSymbolAccessibilityDiagnostic(
        `type`: (TypeNode | EntityName),
        getSymbolAccessibilityDiagnostic: GetSymbolAccessibilityDiagnostic) = {
      (writer.getSymbolAccessibilityDiagnostic =
        getSymbolAccessibilityDiagnostic)
      emitType(`type`)

    }
    def emitType(`type`: (TypeNode | Identifier | QualifiedName)) = {
      `type`.kind match {
        case SyntaxKind.AnyKeyword | SyntaxKind.StringKeyword |
            SyntaxKind.NumberKeyword | SyntaxKind.BooleanKeyword |
            SyntaxKind.SymbolKeyword | SyntaxKind.VoidKeyword |
            SyntaxKind.UndefinedKeyword | SyntaxKind.NullKeyword |
            SyntaxKind.NeverKeyword | SyntaxKind.ThisType |
            SyntaxKind.LiteralType =>
          return writeTextOfNode(currentText, `type`)
        case SyntaxKind.ExpressionWithTypeArguments =>
          return emitExpressionWithTypeArguments(
              `type`.asInstanceOf[ExpressionWithTypeArguments])
        case SyntaxKind.TypeReference =>
          return emitTypeReference(`type`.asInstanceOf[TypeReferenceNode])
        case SyntaxKind.TypeQuery =>
          return emitTypeQuery(`type`.asInstanceOf[TypeQueryNode])
        case SyntaxKind.ArrayType =>
          return emitArrayType(`type`.asInstanceOf[ArrayTypeNode])
        case SyntaxKind.TupleType =>
          return emitTupleType(`type`.asInstanceOf[TupleTypeNode])
        case SyntaxKind.UnionType =>
          return emitUnionType(`type`.asInstanceOf[UnionTypeNode])
        case SyntaxKind.IntersectionType =>
          return emitIntersectionType(
              `type`.asInstanceOf[IntersectionTypeNode])
        case SyntaxKind.ParenthesizedType =>
          return emitParenType(`type`.asInstanceOf[ParenthesizedTypeNode])
        case SyntaxKind.FunctionType | SyntaxKind.ConstructorType =>
          return emitSignatureDeclarationWithJsDocComments(
              `type`.asInstanceOf[FunctionOrConstructorTypeNode])
        case SyntaxKind.TypeLiteral =>
          return emitTypeLiteral(`type`.asInstanceOf[TypeLiteralNode])
        case SyntaxKind.Identifier =>
          return emitEntityName(`type`.asInstanceOf[Identifier])
        case SyntaxKind.QualifiedName =>
          return emitEntityName(`type`.asInstanceOf[QualifiedName])
        case SyntaxKind.TypePredicate =>
          return emitTypePredicate(`type`.asInstanceOf[TypePredicateNode])
        case _ =>
      }
      def writeEntityName(entityName: (EntityName | Expression)) = {
        if ((entityName.kind === SyntaxKind.Identifier)) {
          writeTextOfNode(currentText, entityName)

        } else {
          val left =
            (if ((entityName.kind === SyntaxKind.QualifiedName))(
                 entityName.asInstanceOf[QualifiedName]).left
             else
               (entityName.asInstanceOf[PropertyAccessExpression]).expression)
          val right =
            (if ((entityName.kind === SyntaxKind.QualifiedName))(
                 entityName.asInstanceOf[QualifiedName]).right
             else (entityName.asInstanceOf[PropertyAccessExpression]).name)
          writeEntityName(left)
          write(".")
          writeTextOfNode(currentText, right)

        }

      }
      def emitEntityName(entityName: EntityNameOrEntityNameExpression) = {
        val visibilityResult = resolver.isEntityNameVisible(entityName,
            (if ((entityName.parent.kind === SyntaxKind.ImportEqualsDeclaration))
               entityName.parent
             else enclosingDeclaration))
        handleSymbolAccessibilityError(visibilityResult)
        recordTypeReferenceDirectivesIfNecessary(
            resolver.getTypeReferenceDirectivesForEntityName(entityName))
        writeEntityName(entityName)

      }
      def emitExpressionWithTypeArguments(
          node: ExpressionWithTypeArguments) = {
        if (isEntityNameExpression(node.expression)) {
          Debug.assert(
              ((node.expression.kind === SyntaxKind.Identifier) || (node.expression.kind === SyntaxKind.PropertyAccessExpression)))
          emitEntityName(node.expression)
          if (node.typeArguments) {
            write("<")
            emitCommaList(node.typeArguments, emitType)
            write(">")

          }

        }

      }
      def emitTypeReference(`type`: TypeReferenceNode) = {
        emitEntityName(`type`.typeName)
        if (`type`.typeArguments) {
          write("<")
          emitCommaList(`type`.typeArguments, emitType)
          write(">")

        }

      }
      def emitTypePredicate(`type`: TypePredicateNode) = {
        writeTextOfNode(currentText, `type`.parameterName)
        write(" is ")
        emitType(`type`.`type`)

      }
      def emitTypeQuery(`type`: TypeQueryNode) = {
        write("typeof ")
        emitEntityName(`type`.exprName)

      }
      def emitArrayType(`type`: ArrayTypeNode) = {
        emitType(`type`.elementType)
        write("[]")

      }
      def emitTupleType(`type`: TupleTypeNode) = {
        write("[")
        emitCommaList(`type`.elementTypes, emitType)
        write("]")

      }
      def emitUnionType(`type`: UnionTypeNode) = {
        emitSeparatedList(`type`.types, " | ", emitType)

      }
      def emitIntersectionType(`type`: IntersectionTypeNode) = {
        emitSeparatedList(`type`.types, " & ", emitType)

      }
      def emitParenType(`type`: ParenthesizedTypeNode) = {
        write("(")
        emitType(`type`.`type`)
        write(")")

      }
      def emitTypeLiteral(`type`: TypeLiteralNode) = {
        write("{")
        if (`type`.members.length) {
          writeLine()
          increaseIndent()
          emitLines(`type`.members)
          decreaseIndent()

        }
        write("}")

      }

    }
    def emitSourceFile(node: SourceFile) = {
      (currentText = node.text)
      (currentLineMap = getLineStarts(node))
      (currentIdentifiers = node.identifiers)
      (isCurrentFileExternalModule = isExternalModule(node))
      (enclosingDeclaration = node)
      emitDetachedComments(currentText, currentLineMap, writer,
          writeCommentRange, node, newLine, true)
      emitLines(node.statements)

    }
    def getExportDefaultTempVariableName(): String = {
      val baseName = "_default"
      if ((!((baseNameincurrentIdentifiers)))) {
        return baseName

      }
      var count = 0
      while (true) {
        {
          (count += 1)
          val name = ((baseName + "_") + count)
          if ((!((nameincurrentIdentifiers)))) {
            return name

          }

        }
      }

    }
    def emitExportAssignment(node: ExportAssignment) = {
      if ((node.expression.kind === SyntaxKind.Identifier)) {
        write((if (node.isExportEquals) "export = " else "export default "))
        writeTextOfNode(currentText, node.expression)

      } else {
        val tempVarName = getExportDefaultTempVariableName()
        if ((!noDeclare)) {
          write("declare ")

        }
        write("var ")
        write(tempVarName)
        write(": ")
        (writer.getSymbolAccessibilityDiagnostic =
          getDefaultExportAccessibilityDiagnostic)
        resolver.writeTypeOfExpression(node.expression, enclosingDeclaration,
            (TypeFormatFlags.UseTypeOfFunction | TypeFormatFlags.UseTypeAliasValue),
            writer)
        write(";")
        writeLine()
        write((if (node.isExportEquals) "export = " else "export default "))
        write(tempVarName)

      }
      write(";")
      writeLine()
      if ((node.expression.kind === SyntaxKind.Identifier)) {
        val nodes = resolver.collectLinkedAliases(
            node.expression.asInstanceOf[Identifier])
        writeAsynchronousModuleElements(nodes)

      }
      def getDefaultExportAccessibilityDiagnostic(): SymbolAccessibilityDiagnostic = {
        return Map(
            "diagnosticMessage" -> Diagnostics.Default_export_of_the_module_has_or_is_using_private_name_0,
            "errorNode" -> node)

      }

    }
    def isModuleElementVisible(node: Declaration) = {
      return resolver.isDeclarationVisible(node)

    }
    def emitModuleElement(node: Node, isModuleElementVisible: Boolean) = {
      if (isModuleElementVisible) {
        writeModuleElement(node)

      } else if (((node.kind === SyntaxKind.ImportEqualsDeclaration) || (((node.parent.kind === SyntaxKind.SourceFile) && isCurrentFileExternalModule)))) {
        var isVisible: Boolean = zeroOfMyType
        if ((asynchronousSubModuleDeclarationEmitInfo && (node.parent.kind !== SyntaxKind.SourceFile))) {
          asynchronousSubModuleDeclarationEmitInfo
            .push(Map("node" -> node, "outputPos" -> writer.getTextPos(),
                    "indent" -> writer.getIndent(), "isVisible" -> isVisible))

        } else {
          if ((node.kind === SyntaxKind.ImportDeclaration)) {
            val importDeclaration = node.asInstanceOf[ImportDeclaration]
            if (importDeclaration.importClause) {
              (isVisible = (((importDeclaration.importClause.name && resolver.isDeclarationVisible(
                    importDeclaration.importClause))) || isVisibleNamedBinding(
                    importDeclaration.importClause.namedBindings)))

            }

          }
          moduleElementDeclarationEmitInfo
            .push(Map("node" -> node, "outputPos" -> writer.getTextPos(),
                    "indent" -> writer.getIndent(), "isVisible" -> isVisible))

        }

      }

    }
    def writeModuleElement(node: Node) = {
      node.kind match {
        case SyntaxKind.FunctionDeclaration =>
          return writeFunctionDeclaration(
              node.asInstanceOf[FunctionLikeDeclaration])
        case SyntaxKind.VariableStatement =>
          return writeVariableStatement(node.asInstanceOf[VariableStatement])
        case SyntaxKind.InterfaceDeclaration =>
          return writeInterfaceDeclaration(
              node.asInstanceOf[InterfaceDeclaration])
        case SyntaxKind.ClassDeclaration =>
          return writeClassDeclaration(node.asInstanceOf[ClassDeclaration])
        case SyntaxKind.TypeAliasDeclaration =>
          return writeTypeAliasDeclaration(
              node.asInstanceOf[TypeAliasDeclaration])
        case SyntaxKind.EnumDeclaration =>
          return writeEnumDeclaration(node.asInstanceOf[EnumDeclaration])
        case SyntaxKind.ModuleDeclaration =>
          return writeModuleDeclaration(node.asInstanceOf[ModuleDeclaration])
        case SyntaxKind.ImportEqualsDeclaration =>
          return writeImportEqualsDeclaration(
              node.asInstanceOf[ImportEqualsDeclaration])
        case SyntaxKind.ImportDeclaration =>
          return writeImportDeclaration(node.asInstanceOf[ImportDeclaration])
        case _ =>
          Debug.fail("Unknown symbol kind")
      }

    }
    def emitModuleElementDeclarationFlags(node: Node) = {
      if ((node.parent.kind === SyntaxKind.SourceFile)) {
        val modifiers = getModifierFlags(node)
        if ((modifiers & ModifierFlags.Export)) {
          write("export ")

        }
        if ((modifiers & ModifierFlags.Default)) {
          write("default ")

        } else if (((node.kind !== SyntaxKind.InterfaceDeclaration) && (!noDeclare))) {
          write("declare ")

        }

      }

    }
    def emitClassMemberDeclarationFlags(flags: ModifierFlags) = {
      if ((flags & ModifierFlags.Private)) {
        write("private ")

      } else if ((flags & ModifierFlags.Protected)) {
        write("protected ")

      }
      if ((flags & ModifierFlags.Static)) {
        write("static ")

      }
      if ((flags & ModifierFlags.Readonly)) {
        write("readonly ")

      }
      if ((flags & ModifierFlags.Abstract)) {
        write("abstract ")

      }

    }
    def writeImportEqualsDeclaration(node: ImportEqualsDeclaration) = {
      emitJsDocComments(node)
      if (hasModifier(node, ModifierFlags.Export)) {
        write("export ")

      }
      write("import ")
      writeTextOfNode(currentText, node.name)
      write(" = ")
      if (isInternalModuleImportEqualsDeclaration(node)) {
        emitTypeWithNewGetSymbolAccessibilityDiagnostic(
            node.moduleReference.asInstanceOf[EntityName],
            getImportEntityNameVisibilityError)
        write(";")

      } else {
        write("require(")
        emitExternalModuleSpecifier(node)
        write(");")

      }
      writer.writeLine()
      def getImportEntityNameVisibilityError(): SymbolAccessibilityDiagnostic = {
        return Map(
            "diagnosticMessage" -> Diagnostics.Import_declaration_0_is_using_private_name_1,
            "errorNode" -> node, "typeName" -> node.name)

      }

    }
    def isVisibleNamedBinding(
        namedBindings: (NamespaceImport | NamedImports)): Boolean = {
      if (namedBindings) {
        if ((namedBindings.kind === SyntaxKind.NamespaceImport)) {
          return resolver.isDeclarationVisible(
              namedBindings.asInstanceOf[NamespaceImport])

        } else {
          return forEach((namedBindings.asInstanceOf[NamedImports]).elements,
              (namedImport => resolver.isDeclarationVisible(namedImport)))

        }

      }

    }
    def writeImportDeclaration(node: ImportDeclaration) = {
      emitJsDocComments(node)
      if (hasModifier(node, ModifierFlags.Export)) {
        write("export ")

      }
      write("import ")
      if (node.importClause) {
        val currentWriterPos = writer.getTextPos()
        if ((node.importClause.name && resolver.isDeclarationVisible(
                node.importClause))) {
          writeTextOfNode(currentText, node.importClause.name)

        }
        if ((node.importClause.namedBindings && isVisibleNamedBinding(
                node.importClause.namedBindings))) {
          if ((currentWriterPos !== writer.getTextPos())) {
            write(", ")

          }
          if ((node.importClause.namedBindings.kind === SyntaxKind.NamespaceImport)) {
            write("* as ")
            writeTextOfNode(currentText,
                (node.importClause.namedBindings.asInstanceOf[NamespaceImport]).name)

          } else {
            write("{ ")
            emitCommaList((node.importClause.namedBindings
                      .asInstanceOf[NamedImports])
                  .elements, emitImportOrExportSpecifier,
                resolver.isDeclarationVisible)
            write(" }")

          }

        }
        write(" from ")

      }
      emitExternalModuleSpecifier(node)
      write(";")
      writer.writeLine()

    }
    def emitExternalModuleSpecifier(
        parent: (ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration)) = {
      (resultHasExternalModuleIndicator = (resultHasExternalModuleIndicator || (parent.kind !== SyntaxKind.ModuleDeclaration)))
      var moduleSpecifier: Node = zeroOfMyType
      if ((parent.kind === SyntaxKind.ImportEqualsDeclaration)) {
        val node = parent.asInstanceOf[ImportEqualsDeclaration]
        (moduleSpecifier =
          getExternalModuleImportEqualsDeclarationExpression(node))

      } else if ((parent.kind === SyntaxKind.ModuleDeclaration)) {
        (moduleSpecifier = (parent.asInstanceOf[ModuleDeclaration]).name)

      } else {
        val node = parent.asInstanceOf[(ImportDeclaration | ExportDeclaration)]
        (moduleSpecifier = node.moduleSpecifier)

      }
      if ((((moduleSpecifier.kind === SyntaxKind.StringLiteral) && isBundledEmit) && ((compilerOptions.out || compilerOptions.outFile)))) {
        val moduleName =
          getExternalModuleNameFromDeclaration(host, resolver, parent)
        if (moduleName) {
          write("\"")
          write(moduleName)
          write("\"")
          return

        }

      }
      writeTextOfNode(currentText, moduleSpecifier)

    }
    def emitImportOrExportSpecifier(node: ImportOrExportSpecifier) = {
      if (node.propertyName) {
        writeTextOfNode(currentText, node.propertyName)
        write(" as ")

      }
      writeTextOfNode(currentText, node.name)

    }
    def emitExportSpecifier(node: ExportSpecifier) = {
      emitImportOrExportSpecifier(node)
      val nodes =
        resolver.collectLinkedAliases((node.propertyName || node.name))
      writeAsynchronousModuleElements(nodes)

    }
    def emitExportDeclaration(node: ExportDeclaration) = {
      emitJsDocComments(node)
      write("export ")
      if (node.exportClause) {
        write("{ ")
        emitCommaList(node.exportClause.elements, emitExportSpecifier)
        write(" }")

      } else {
        write("*")

      }
      if (node.moduleSpecifier) {
        write(" from ")
        emitExternalModuleSpecifier(node)

      }
      write(";")
      writer.writeLine()

    }
    def writeModuleDeclaration(node: ModuleDeclaration) = {
      emitJsDocComments(node)
      emitModuleElementDeclarationFlags(node)
      if (isGlobalScopeAugmentation(node)) {
        write("global ")

      } else {
        if ((node.flags & NodeFlags.Namespace)) {
          write("namespace ")

        } else {
          write("module ")

        }
        if (isExternalModuleAugmentation(node)) {
          emitExternalModuleSpecifier(node)

        } else {
          writeTextOfNode(currentText, node.name)

        }

      }
      while ((node.body && (node.body.kind !== SyntaxKind.ModuleBlock))) {
        {
          (node = node.body.asInstanceOf[ModuleDeclaration])
          write(".")
          writeTextOfNode(currentText, node.name)

        }
      }
      val prevEnclosingDeclaration = enclosingDeclaration
      if (node.body) {
        (enclosingDeclaration = node)
        write(" {")
        writeLine()
        increaseIndent()
        emitLines((node.body.asInstanceOf[ModuleBlock]).statements)
        decreaseIndent()
        write("}")
        writeLine()
        (enclosingDeclaration = prevEnclosingDeclaration)

      } else {
        write(";")

      }

    }
    def writeTypeAliasDeclaration(node: TypeAliasDeclaration) = {
      val prevEnclosingDeclaration = enclosingDeclaration
      (enclosingDeclaration = node)
      emitJsDocComments(node)
      emitModuleElementDeclarationFlags(node)
      write("type ")
      writeTextOfNode(currentText, node.name)
      emitTypeParameters(node.typeParameters)
      write(" = ")
      emitTypeWithNewGetSymbolAccessibilityDiagnostic(node.`type`,
          getTypeAliasDeclarationVisibilityError)
      write(";")
      writeLine()
      (enclosingDeclaration = prevEnclosingDeclaration)
      def getTypeAliasDeclarationVisibilityError(): SymbolAccessibilityDiagnostic = {
        return Map(
            "diagnosticMessage" -> Diagnostics.Exported_type_alias_0_has_or_is_using_private_name_1,
            "errorNode" -> node.`type`, "typeName" -> node.name)

      }

    }
    def writeEnumDeclaration(node: EnumDeclaration) = {
      emitJsDocComments(node)
      emitModuleElementDeclarationFlags(node)
      if (isConst(node)) {
        write("const ")

      }
      write("enum ")
      writeTextOfNode(currentText, node.name)
      write(" {")
      writeLine()
      increaseIndent()
      emitLines(node.members)
      decreaseIndent()
      write("}")
      writeLine()

    }
    def emitEnumMemberDeclaration(node: EnumMember) = {
      emitJsDocComments(node)
      writeTextOfNode(currentText, node.name)
      val enumMemberValue = resolver.getConstantValue(node)
      if ((enumMemberValue !== undefined)) {
        write(" = ")
        write(enumMemberValue.`toString`())

      }
      write(",")
      writeLine()

    }
    def isPrivateMethodTypeParameter(node: TypeParameterDeclaration) = {
      return ((node.parent.kind === SyntaxKind.MethodDeclaration) && hasModifier(
          node.parent, ModifierFlags.Private))

    }
    def emitTypeParameters(typeParameters: Array[TypeParameterDeclaration]) = {
      def emitTypeParameter(node: TypeParameterDeclaration) = {
        increaseIndent()
        emitJsDocComments(node)
        decreaseIndent()
        writeTextOfNode(currentText, node.name)
        if ((node.constraint && (!isPrivateMethodTypeParameter(node)))) {
          write(" extends ")
          if ((((node.parent.kind === SyntaxKind.FunctionType) || (node.parent.kind === SyntaxKind.ConstructorType)) || ((node.parent.parent && (node.parent.parent.kind === SyntaxKind.TypeLiteral))))) {
            Debug.assert(
                ((((((node.parent.kind === SyntaxKind.MethodDeclaration) || (node.parent.kind === SyntaxKind.MethodSignature)) || (node.parent.kind === SyntaxKind.FunctionType)) || (node.parent.kind === SyntaxKind.ConstructorType)) || (node.parent.kind === SyntaxKind.CallSignature)) || (node.parent.kind === SyntaxKind.ConstructSignature)))
            emitType(node.constraint)

          } else {
            emitTypeWithNewGetSymbolAccessibilityDiagnostic(node.constraint,
                getTypeParameterConstraintVisibilityError)

          }

        }
        def getTypeParameterConstraintVisibilityError(): SymbolAccessibilityDiagnostic = {
          var diagnosticMessage: DiagnosticMessage = zeroOfMyType
          node.parent.kind match {
            case SyntaxKind.ClassDeclaration =>
              (diagnosticMessage =
                Diagnostics.Type_parameter_0_of_exported_class_has_or_is_using_private_name_1)
            case SyntaxKind.InterfaceDeclaration =>
              (diagnosticMessage =
                Diagnostics.Type_parameter_0_of_exported_interface_has_or_is_using_private_name_1)
            case SyntaxKind.ConstructSignature =>
              (diagnosticMessage =
                Diagnostics.Type_parameter_0_of_constructor_signature_from_exported_interface_has_or_is_using_private_name_1)
            case SyntaxKind.CallSignature =>
              (diagnosticMessage =
                Diagnostics.Type_parameter_0_of_call_signature_from_exported_interface_has_or_is_using_private_name_1)
            case SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature =>
              if (hasModifier(node.parent, ModifierFlags.Static)) {
                (diagnosticMessage =
                  Diagnostics.Type_parameter_0_of_public_static_method_from_exported_class_has_or_is_using_private_name_1)

              } else if ((node.parent.parent.kind === SyntaxKind.ClassDeclaration)) {
                (diagnosticMessage =
                  Diagnostics.Type_parameter_0_of_public_method_from_exported_class_has_or_is_using_private_name_1)

              } else {
                (diagnosticMessage =
                  Diagnostics.Type_parameter_0_of_method_from_exported_interface_has_or_is_using_private_name_1)

              }
            case SyntaxKind.FunctionDeclaration =>
              (diagnosticMessage =
                Diagnostics.Type_parameter_0_of_exported_function_has_or_is_using_private_name_1)
            case _ =>
              Debug.fail(
                  ("This is unknown parent for type parameter: " + node.parent.kind))
          }
          return Map("diagnosticMessage" -> diagnosticMessage,
              "errorNode" -> node, "typeName" -> node.name)

        }

      }
      if (typeParameters) {
        write("<")
        emitCommaList(typeParameters, emitTypeParameter)
        write(">")

      }

    }
    def emitHeritageClause(typeReferences: Array[ExpressionWithTypeArguments],
        isImplementsList: Boolean) = {
      if (typeReferences) {
        write((if (isImplementsList) " implements " else " extends "))
        emitCommaList(typeReferences, emitTypeOfTypeReference)

      }
      def emitTypeOfTypeReference(node: ExpressionWithTypeArguments) = {
        if (isEntityNameExpression(node.expression)) {
          emitTypeWithNewGetSymbolAccessibilityDiagnostic(node,
              getHeritageClauseVisibilityError)

        } else if (((!isImplementsList) && (node.expression.kind === SyntaxKind.NullKeyword))) {
          write("null")

        } else {
          (writer.getSymbolAccessibilityDiagnostic =
            getHeritageClauseVisibilityError)
          resolver.writeBaseConstructorTypeOfClass(
              enclosingDeclaration.asInstanceOf[ClassLikeDeclaration],
              enclosingDeclaration,
              (TypeFormatFlags.UseTypeOfFunction | TypeFormatFlags.UseTypeAliasValue),
              writer)

        }
        def getHeritageClauseVisibilityError(): SymbolAccessibilityDiagnostic = {
          var diagnosticMessage: DiagnosticMessage = zeroOfMyType
          if ((node.parent.parent.kind === SyntaxKind.ClassDeclaration)) {
            (diagnosticMessage =
              (if (isImplementsList)
                 Diagnostics.Implements_clause_of_exported_class_0_has_or_is_using_private_name_1
               else
                 Diagnostics.Extends_clause_of_exported_class_0_has_or_is_using_private_name_1))

          } else {
            (diagnosticMessage =
              Diagnostics.Extends_clause_of_exported_interface_0_has_or_is_using_private_name_1)

          }
          return Map("diagnosticMessage" -> diagnosticMessage,
              "errorNode" -> node,
              "typeName" -> (node.parent.parent.asInstanceOf[Declaration]).name)

        }

      }

    }
    def writeClassDeclaration(node: ClassDeclaration) = {
      def emitParameterProperties(
          constructorDeclaration: ConstructorDeclaration) = {
        if (constructorDeclaration) {
          forEach(constructorDeclaration.parameters,
              (param => {
                if (hasModifier(param,
                        ModifierFlags.ParameterPropertyModifier)) {
                  emitPropertyDeclaration(param)

                }

              }))

        }

      }
      emitJsDocComments(node)
      emitModuleElementDeclarationFlags(node)
      if (hasModifier(node, ModifierFlags.Abstract)) {
        write("abstract ")

      }
      write("class ")
      writeTextOfNode(currentText, node.name)
      val prevEnclosingDeclaration = enclosingDeclaration
      (enclosingDeclaration = node)
      emitTypeParameters(node.typeParameters)
      val baseTypeNode = getClassExtendsHeritageClauseElement(node)
      if (baseTypeNode) {
        emitHeritageClause(Array(baseTypeNode), false)

      }
      emitHeritageClause(getClassImplementsHeritageClauseElements(node), true)
      write(" {")
      writeLine()
      increaseIndent()
      emitParameterProperties(getFirstConstructorWithBody(node))
      emitLines(node.members)
      decreaseIndent()
      write("}")
      writeLine()
      (enclosingDeclaration = prevEnclosingDeclaration)

    }
    def writeInterfaceDeclaration(node: InterfaceDeclaration) = {
      emitJsDocComments(node)
      emitModuleElementDeclarationFlags(node)
      write("interface ")
      writeTextOfNode(currentText, node.name)
      val prevEnclosingDeclaration = enclosingDeclaration
      (enclosingDeclaration = node)
      emitTypeParameters(node.typeParameters)
      emitHeritageClause(getInterfaceBaseTypeNodes(node), false)
      write(" {")
      writeLine()
      increaseIndent()
      emitLines(node.members)
      decreaseIndent()
      write("}")
      writeLine()
      (enclosingDeclaration = prevEnclosingDeclaration)

    }
    def emitPropertyDeclaration(node: Declaration) = {
      if (hasDynamicName(node)) {
        return

      }
      emitJsDocComments(node)
      emitClassMemberDeclarationFlags(getModifierFlags(node))
      emitVariableDeclaration(node.asInstanceOf[VariableDeclaration])
      write(";")
      writeLine()

    }
    def emitVariableDeclaration(
        node: (VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration)) = {
      if (((node.kind !== SyntaxKind.VariableDeclaration) || resolver
            .isDeclarationVisible(node))) {
        if (isBindingPattern(node.name)) {
          emitBindingPattern(node.name.asInstanceOf[BindingPattern])

        } else {
          writeTextOfNode(currentText, node.name)
          if ((((((node.kind === SyntaxKind.PropertyDeclaration) || (node.kind === SyntaxKind.PropertySignature)) || (((node.kind === SyntaxKind.Parameter) && (!isParameterPropertyDeclaration(
                  node.asInstanceOf[ParameterDeclaration])))))) && hasQuestionToken(
                  node))) {
            write("?")

          }
          if (((((node.kind === SyntaxKind.PropertyDeclaration) || (node.kind === SyntaxKind.PropertySignature))) && (node.parent.kind === SyntaxKind.TypeLiteral))) {
            emitTypeOfVariableDeclarationFromTypeLiteral(node)

          } else if (resolver.isLiteralConstDeclaration(node)) {
            write(" = ")
            resolver.writeLiteralConstValue(node, writer)

          } else if ((!hasModifier(node, ModifierFlags.Private))) {
            writeTypeOfDeclaration(node, node.`type`,
                getVariableDeclarationTypeVisibilityError)

          }

        }

      }
      def getVariableDeclarationTypeVisibilityDiagnosticMessage(
          symbolAccessibilityResult: SymbolAccessibilityResult) = {
        if ((node.kind === SyntaxKind.VariableDeclaration)) {
          return (if (symbolAccessibilityResult.errorModuleName)
                    (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                       Diagnostics.Exported_variable_0_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                     else
                       Diagnostics.Exported_variable_0_has_or_is_using_name_1_from_private_module_2)
                  else
                    Diagnostics.Exported_variable_0_has_or_is_using_private_name_1)

        } else if (((node.kind === SyntaxKind.PropertyDeclaration) || (node.kind === SyntaxKind.PropertySignature))) {
          if (hasModifier(node, ModifierFlags.Static)) {
            return (if (symbolAccessibilityResult.errorModuleName)
                      (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                         Diagnostics.Public_static_property_0_of_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                       else
                         Diagnostics.Public_static_property_0_of_exported_class_has_or_is_using_name_1_from_private_module_2)
                    else
                      Diagnostics.Public_static_property_0_of_exported_class_has_or_is_using_private_name_1)

          } else if ((node.parent.kind === SyntaxKind.ClassDeclaration)) {
            return (if (symbolAccessibilityResult.errorModuleName)
                      (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                         Diagnostics.Public_property_0_of_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                       else
                         Diagnostics.Public_property_0_of_exported_class_has_or_is_using_name_1_from_private_module_2)
                    else
                      Diagnostics.Public_property_0_of_exported_class_has_or_is_using_private_name_1)

          } else {
            return (if (symbolAccessibilityResult.errorModuleName)
                      Diagnostics.Property_0_of_exported_interface_has_or_is_using_name_1_from_private_module_2
                    else
                      Diagnostics.Property_0_of_exported_interface_has_or_is_using_private_name_1)

          }

        }

      }
      def getVariableDeclarationTypeVisibilityError(
          symbolAccessibilityResult: SymbolAccessibilityResult): SymbolAccessibilityDiagnostic = {
        val diagnosticMessage =
          getVariableDeclarationTypeVisibilityDiagnosticMessage(
              symbolAccessibilityResult)
        return (if ((diagnosticMessage !== undefined))
                  Map("diagnosticMessage" -> diagnosticMessage,
                      "errorNode" -> node, "typeName" -> node.name)
                else undefined)

      }
      def emitBindingPattern(bindingPattern: BindingPattern) = {
        val elements: Array[Node] = Array()
        (bindingPattern.elements).foreach { fresh6 =>
          val element = zeroOfMyType = fresh6 {
            if ((element.kind !== SyntaxKind.OmittedExpression)) {
              elements.push(element)

            }

          }
        }
        emitCommaList(elements, emitBindingElement)

      }
      def emitBindingElement(bindingElement: BindingElement) = {
        def getBindingElementTypeVisibilityError(
            symbolAccessibilityResult: SymbolAccessibilityResult): SymbolAccessibilityDiagnostic = {
          val diagnosticMessage =
            getVariableDeclarationTypeVisibilityDiagnosticMessage(
                symbolAccessibilityResult)
          return (if ((diagnosticMessage !== undefined))
                    Map("diagnosticMessage" -> diagnosticMessage,
                        "errorNode" -> bindingElement,
                        "typeName" -> bindingElement.name)
                  else undefined)

        }
        if (bindingElement.name) {
          if (isBindingPattern(bindingElement.name)) {
            emitBindingPattern(
                bindingElement.name.asInstanceOf[BindingPattern])

          } else {
            writeTextOfNode(currentText, bindingElement.name)
            writeTypeOfDeclaration(bindingElement, undefined,
                getBindingElementTypeVisibilityError)

          }

        }

      }

    }
    def emitTypeOfVariableDeclarationFromTypeLiteral(
        node: VariableLikeDeclaration) = {
      if (node.`type`) {
        write(": ")
        emitType(node.`type`)

      }

    }
    def isVariableStatementVisible(node: VariableStatement) = {
      return forEach(node.declarationList.declarations,
          (varDeclaration => resolver.isDeclarationVisible(varDeclaration)))

    }
    def writeVariableStatement(node: VariableStatement) = {
      emitJsDocComments(node)
      emitModuleElementDeclarationFlags(node)
      if (isLet(node.declarationList)) {
        write("let ")

      } else if (isConst(node.declarationList)) {
        write("const ")

      } else {
        write("var ")

      }
      emitCommaList(node.declarationList.declarations, emitVariableDeclaration,
          resolver.isDeclarationVisible)
      write(";")
      writeLine()

    }
    def emitAccessorDeclaration(node: AccessorDeclaration) = {
      if (hasDynamicName(node)) {
        return

      }
      val accessors = getAllAccessorDeclarations(
          (node.parent.asInstanceOf[ClassDeclaration]).members, node)
      var accessorWithTypeAnnotation: AccessorDeclaration = zeroOfMyType
      if ((node === accessors.firstAccessor)) {
        emitJsDocComments(accessors.getAccessor)
        emitJsDocComments(accessors.setAccessor)
        emitClassMemberDeclarationFlags(
            (getModifierFlags(node) | ((if (accessors.setAccessor) 0
                else ModifierFlags.Readonly) )))
        writeTextOfNode(currentText, node.name)
        if ((!hasModifier(node, ModifierFlags.Private))) {
          (accessorWithTypeAnnotation = node)
          var `type` = getTypeAnnotationFromAccessor(node)
          if ((!`type`)) {
            val anotherAccessor =
              (if ((node.kind === SyntaxKind.GetAccessor))
                 accessors.setAccessor
               else accessors.getAccessor)
            (`type` = getTypeAnnotationFromAccessor(anotherAccessor))
            if (`type`) {
              (accessorWithTypeAnnotation = anotherAccessor)

            }

          }
          writeTypeOfDeclaration(node, `type`,
              getAccessorDeclarationTypeVisibilityError)

        }
        write(";")
        writeLine()

      }
      def getTypeAnnotationFromAccessor(
          accessor: AccessorDeclaration): TypeNode = {
        if (accessor) {
          return (if ((accessor.kind === SyntaxKind.GetAccessor))
                    accessor.`type`
                  else
                    (if ((accessor.parameters.length > 0))
                       accessor.parameters(0).`type`
                     else undefined))

        }

      }
      def getAccessorDeclarationTypeVisibilityError(
          symbolAccessibilityResult: SymbolAccessibilityResult): SymbolAccessibilityDiagnostic = {
        var diagnosticMessage: DiagnosticMessage = zeroOfMyType
        if ((accessorWithTypeAnnotation.kind === SyntaxKind.SetAccessor)) {
          if (hasModifier(accessorWithTypeAnnotation.parent,
                  ModifierFlags.Static)) {
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 Diagnostics.Parameter_0_of_public_static_property_setter_from_exported_class_has_or_is_using_name_1_from_private_module_2
               else
                 Diagnostics.Parameter_0_of_public_static_property_setter_from_exported_class_has_or_is_using_private_name_1))

          } else {
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 Diagnostics.Parameter_0_of_public_property_setter_from_exported_class_has_or_is_using_name_1_from_private_module_2
               else
                 Diagnostics.Parameter_0_of_public_property_setter_from_exported_class_has_or_is_using_private_name_1))

          }
          return Map("diagnosticMessage" -> diagnosticMessage,
              "errorNode" -> accessorWithTypeAnnotation
                .parameters(0)
                .asInstanceOf[Node],
              "typeName" -> accessorWithTypeAnnotation.name)

        } else {
          if (hasModifier(accessorWithTypeAnnotation, ModifierFlags.Static)) {
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                    Diagnostics.Return_type_of_public_static_property_getter_from_exported_class_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                  else
                    Diagnostics.Return_type_of_public_static_property_getter_from_exported_class_has_or_is_using_name_0_from_private_module_1)
               else
                 Diagnostics.Return_type_of_public_static_property_getter_from_exported_class_has_or_is_using_private_name_0))

          } else {
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                    Diagnostics.Return_type_of_public_property_getter_from_exported_class_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                  else
                    Diagnostics.Return_type_of_public_property_getter_from_exported_class_has_or_is_using_name_0_from_private_module_1)
               else
                 Diagnostics.Return_type_of_public_property_getter_from_exported_class_has_or_is_using_private_name_0))

          }
          return Map("diagnosticMessage" -> diagnosticMessage,
              "errorNode" -> accessorWithTypeAnnotation.name
                .asInstanceOf[Node],
              "typeName" -> undefined)

        }

      }

    }
    def writeFunctionDeclaration(node: FunctionLikeDeclaration) = {
      if (hasDynamicName(node)) {
        return

      }
      if ((!resolver.isImplementationOfOverload(node))) {
        emitJsDocComments(node)
        if ((node.kind === SyntaxKind.FunctionDeclaration)) {
          emitModuleElementDeclarationFlags(node)

        } else if (((node.kind === SyntaxKind.MethodDeclaration) || (node.kind === SyntaxKind.Constructor))) {
          emitClassMemberDeclarationFlags(getModifierFlags(node))

        }
        if ((node.kind === SyntaxKind.FunctionDeclaration)) {
          write("function ")
          writeTextOfNode(currentText, node.name)

        } else if ((node.kind === SyntaxKind.Constructor)) {
          write("constructor")

        } else {
          writeTextOfNode(currentText, node.name)
          if (hasQuestionToken(node)) {
            write("?")

          }

        }
        emitSignatureDeclaration(node)

      }

    }
    def emitSignatureDeclarationWithJsDocComments(
        node: SignatureDeclaration) = {
      emitJsDocComments(node)
      emitSignatureDeclaration(node)

    }
    def emitSignatureDeclaration(node: SignatureDeclaration) = {
      val prevEnclosingDeclaration = enclosingDeclaration
      (enclosingDeclaration = node)
      var closeParenthesizedFunctionType = false
      if ((node.kind === SyntaxKind.IndexSignature)) {
        emitClassMemberDeclarationFlags(getModifierFlags(node))
        write("[")

      } else {
        if (((node.kind === SyntaxKind.ConstructSignature) || (node.kind === SyntaxKind.ConstructorType))) {
          write("new ")

        } else if ((node.kind === SyntaxKind.FunctionType)) {
          val currentOutput = writer.getText()
          if ((node.typeParameters && (currentOutput.charAt(
                  (currentOutput.length - 1)) === "<"))) {
            (closeParenthesizedFunctionType = true)
            write("(")

          }

        }
        emitTypeParameters(node.typeParameters)
        write("(")

      }
      emitCommaList(node.parameters, emitParameterDeclaration)
      if ((node.kind === SyntaxKind.IndexSignature)) {
        write("]")

      } else {
        write(")")

      }
      val isFunctionTypeOrConstructorType = ((node.kind === SyntaxKind.FunctionType) || (node.kind === SyntaxKind.ConstructorType))
      if ((isFunctionTypeOrConstructorType || (node.parent.kind === SyntaxKind.TypeLiteral))) {
        if (node.`type`) {
          write((if (isFunctionTypeOrConstructorType) " => " else ": "))
          emitType(node.`type`)

        }

      } else if (((node.kind !== SyntaxKind.Constructor) && (!hasModifier(node,
              ModifierFlags.Private)))) {
        writeReturnTypeAtSignature(node, getReturnTypeVisibilityError)

      }
      (enclosingDeclaration = prevEnclosingDeclaration)
      if ((!isFunctionTypeOrConstructorType)) {
        write(";")
        writeLine()

      } else if (closeParenthesizedFunctionType) {
        write(")")

      }
      def getReturnTypeVisibilityError(
          symbolAccessibilityResult: SymbolAccessibilityResult): SymbolAccessibilityDiagnostic = {
        var diagnosticMessage: DiagnosticMessage = zeroOfMyType
        node.kind match {
          case SyntaxKind.ConstructSignature =>
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 Diagnostics.Return_type_of_constructor_signature_from_exported_interface_has_or_is_using_name_0_from_private_module_1
               else
                 Diagnostics.Return_type_of_constructor_signature_from_exported_interface_has_or_is_using_private_name_0))
          case SyntaxKind.CallSignature =>
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 Diagnostics.Return_type_of_call_signature_from_exported_interface_has_or_is_using_name_0_from_private_module_1
               else
                 Diagnostics.Return_type_of_call_signature_from_exported_interface_has_or_is_using_private_name_0))
          case SyntaxKind.IndexSignature =>
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 Diagnostics.Return_type_of_index_signature_from_exported_interface_has_or_is_using_name_0_from_private_module_1
               else
                 Diagnostics.Return_type_of_index_signature_from_exported_interface_has_or_is_using_private_name_0))
          case SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature =>
            if (hasModifier(node, ModifierFlags.Static)) {
              (diagnosticMessage =
                (if (symbolAccessibilityResult.errorModuleName)
                   (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                      Diagnostics.Return_type_of_public_static_method_from_exported_class_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                    else
                      Diagnostics.Return_type_of_public_static_method_from_exported_class_has_or_is_using_name_0_from_private_module_1)
                 else
                   Diagnostics.Return_type_of_public_static_method_from_exported_class_has_or_is_using_private_name_0))

            } else if ((node.parent.kind === SyntaxKind.ClassDeclaration)) {
              (diagnosticMessage =
                (if (symbolAccessibilityResult.errorModuleName)
                   (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                      Diagnostics.Return_type_of_public_method_from_exported_class_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                    else
                      Diagnostics.Return_type_of_public_method_from_exported_class_has_or_is_using_name_0_from_private_module_1)
                 else
                   Diagnostics.Return_type_of_public_method_from_exported_class_has_or_is_using_private_name_0))

            } else {
              (diagnosticMessage =
                (if (symbolAccessibilityResult.errorModuleName)
                   Diagnostics.Return_type_of_method_from_exported_interface_has_or_is_using_name_0_from_private_module_1
                 else
                   Diagnostics.Return_type_of_method_from_exported_interface_has_or_is_using_private_name_0))

            }
          case SyntaxKind.FunctionDeclaration =>
            (diagnosticMessage =
              (if (symbolAccessibilityResult.errorModuleName)
                 (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                    Diagnostics.Return_type_of_exported_function_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                  else
                    Diagnostics.Return_type_of_exported_function_has_or_is_using_name_0_from_private_module_1)
               else
                 Diagnostics.Return_type_of_exported_function_has_or_is_using_private_name_0))
          case _ =>
            Debug.fail(("This is unknown kind for signature: " + node.kind))
        }
        return Map("diagnosticMessage" -> diagnosticMessage,
            "errorNode" -> (node.name.asInstanceOf[Node] || node))

      }

    }
    def emitParameterDeclaration(node: ParameterDeclaration) = {
      increaseIndent()
      emitJsDocComments(node)
      if (node.dotDotDotToken) {
        write("...")

      }
      if (isBindingPattern(node.name)) {
        emitBindingPattern(node.name.asInstanceOf[BindingPattern])

      } else {
        writeTextOfNode(currentText, node.name)

      }
      if (resolver.isOptionalParameter(node)) {
        write("?")

      }
      decreaseIndent()
      if ((((node.parent.kind === SyntaxKind.FunctionType) || (node.parent.kind === SyntaxKind.ConstructorType)) || (node.parent.parent.kind === SyntaxKind.TypeLiteral))) {
        emitTypeOfVariableDeclarationFromTypeLiteral(node)

      } else if ((!hasModifier(node.parent, ModifierFlags.Private))) {
        writeTypeOfDeclaration(node, node.`type`,
            getParameterDeclarationTypeVisibilityError)

      }
      def getParameterDeclarationTypeVisibilityError(
          symbolAccessibilityResult: SymbolAccessibilityResult): SymbolAccessibilityDiagnostic = {
        val diagnosticMessage: DiagnosticMessage =
          getParameterDeclarationTypeVisibilityDiagnosticMessage(
              symbolAccessibilityResult)
        return (if ((diagnosticMessage !== undefined))
                  Map("diagnosticMessage" -> diagnosticMessage,
                      "errorNode" -> node, "typeName" -> node.name)
                else undefined)

      }
      def getParameterDeclarationTypeVisibilityDiagnosticMessage(
          symbolAccessibilityResult: SymbolAccessibilityResult): DiagnosticMessage = {
        node.parent.kind match {
          case SyntaxKind.Constructor =>
            return (if (symbolAccessibilityResult.errorModuleName)
                      (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                         Diagnostics.Parameter_0_of_constructor_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                       else
                         Diagnostics.Parameter_0_of_constructor_from_exported_class_has_or_is_using_name_1_from_private_module_2)
                    else
                      Diagnostics.Parameter_0_of_constructor_from_exported_class_has_or_is_using_private_name_1)
          case SyntaxKind.ConstructSignature =>
            return (if (symbolAccessibilityResult.errorModuleName)
                      Diagnostics.Parameter_0_of_constructor_signature_from_exported_interface_has_or_is_using_name_1_from_private_module_2
                    else
                      Diagnostics.Parameter_0_of_constructor_signature_from_exported_interface_has_or_is_using_private_name_1)
          case SyntaxKind.CallSignature =>
            return (if (symbolAccessibilityResult.errorModuleName)
                      Diagnostics.Parameter_0_of_call_signature_from_exported_interface_has_or_is_using_name_1_from_private_module_2
                    else
                      Diagnostics.Parameter_0_of_call_signature_from_exported_interface_has_or_is_using_private_name_1)
          case SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature =>
            if (hasModifier(node.parent, ModifierFlags.Static)) {
              return (if (symbolAccessibilityResult.errorModuleName)
                        (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                           Diagnostics.Parameter_0_of_public_static_method_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                         else
                           Diagnostics.Parameter_0_of_public_static_method_from_exported_class_has_or_is_using_name_1_from_private_module_2)
                      else
                        Diagnostics.Parameter_0_of_public_static_method_from_exported_class_has_or_is_using_private_name_1)

            } else if ((node.parent.parent.kind === SyntaxKind.ClassDeclaration)) {
              return (if (symbolAccessibilityResult.errorModuleName)
                        (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                           Diagnostics.Parameter_0_of_public_method_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                         else
                           Diagnostics.Parameter_0_of_public_method_from_exported_class_has_or_is_using_name_1_from_private_module_2)
                      else
                        Diagnostics.Parameter_0_of_public_method_from_exported_class_has_or_is_using_private_name_1)

            } else {
              return (if (symbolAccessibilityResult.errorModuleName)
                        Diagnostics.Parameter_0_of_method_from_exported_interface_has_or_is_using_name_1_from_private_module_2
                      else
                        Diagnostics.Parameter_0_of_method_from_exported_interface_has_or_is_using_private_name_1)

            }
          case SyntaxKind.FunctionDeclaration =>
            return (if (symbolAccessibilityResult.errorModuleName)
                      (if ((symbolAccessibilityResult.accessibility === SymbolAccessibility.CannotBeNamed))
                         Diagnostics.Parameter_0_of_exported_function_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                       else
                         Diagnostics.Parameter_0_of_exported_function_has_or_is_using_name_1_from_private_module_2)
                    else
                      Diagnostics.Parameter_0_of_exported_function_has_or_is_using_private_name_1)
          case _ =>
            Debug.fail(
                ("This is unknown parent for parameter: " + node.parent.kind))
        }

      }
      def emitBindingPattern(bindingPattern: BindingPattern) = {
        if ((bindingPattern.kind === SyntaxKind.ObjectBindingPattern)) {
          write("{")
          emitCommaList(bindingPattern.elements, emitBindingElement)
          write("}")

        } else if ((bindingPattern.kind === SyntaxKind.ArrayBindingPattern)) {
          write("[")
          val elements = bindingPattern.elements
          emitCommaList(elements, emitBindingElement)
          if ((elements && elements.hasTrailingComma)) {
            write(", ")

          }
          write("]")

        }

      }
      def emitBindingElement(
          bindingElement: (BindingElement | OmittedExpression)) = {
        if ((bindingElement.kind === SyntaxKind.OmittedExpression)) {
          write(" ")

        } else if ((bindingElement.kind === SyntaxKind.BindingElement)) {
          if (bindingElement.propertyName) {
            writeTextOfNode(currentText, bindingElement.propertyName)
            write(": ")

          }
          if (bindingElement.name) {
            if (isBindingPattern(bindingElement.name)) {
              emitBindingPattern(
                  bindingElement.name.asInstanceOf[BindingPattern])

            } else {
              Debug.assert(
                  (bindingElement.name.kind === SyntaxKind.Identifier))
              if (bindingElement.dotDotDotToken) {
                write("...")

              }
              writeTextOfNode(currentText, bindingElement.name)

            }

          }

        }

      }

    }
    def emitNode(node: Node) = {
      node.kind match {
        case SyntaxKind.FunctionDeclaration | SyntaxKind.ModuleDeclaration |
            SyntaxKind.ImportEqualsDeclaration |
            SyntaxKind.InterfaceDeclaration | SyntaxKind.ClassDeclaration |
            SyntaxKind.TypeAliasDeclaration | SyntaxKind.EnumDeclaration =>
          return emitModuleElement(node,
              isModuleElementVisible(node.asInstanceOf[Declaration]))
        case SyntaxKind.VariableStatement =>
          return emitModuleElement(node,
              isVariableStatementVisible(node.asInstanceOf[VariableStatement]))
        case SyntaxKind.ImportDeclaration =>
          return emitModuleElement(node,
              (!(node.asInstanceOf[ImportDeclaration]).importClause))
        case SyntaxKind.ExportDeclaration =>
          return emitExportDeclaration(node.asInstanceOf[ExportDeclaration])
        case SyntaxKind.Constructor | SyntaxKind.MethodDeclaration |
            SyntaxKind.MethodSignature =>
          return writeFunctionDeclaration(
              node.asInstanceOf[FunctionLikeDeclaration])
        case SyntaxKind.ConstructSignature | SyntaxKind.CallSignature |
            SyntaxKind.IndexSignature =>
          return emitSignatureDeclarationWithJsDocComments(
              node.asInstanceOf[SignatureDeclaration])
        case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
          return emitAccessorDeclaration(
              node.asInstanceOf[AccessorDeclaration])
        case SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature =>
          return emitPropertyDeclaration(
              node.asInstanceOf[PropertyDeclaration])
        case SyntaxKind.EnumMember =>
          return emitEnumMemberDeclaration(node.asInstanceOf[EnumMember])
        case SyntaxKind.ExportAssignment =>
          return emitExportAssignment(node.asInstanceOf[ExportAssignment])
        case SyntaxKind.SourceFile =>
          return emitSourceFile(node.asInstanceOf[SourceFile])
        case _ =>
      }

    }
    def writeReferencePath(referencedFile: SourceFile,
        addBundledFileReference: Boolean,
        emitOnlyDtsFiles: Boolean): Boolean = {
      var declFileName: String = zeroOfMyType
      var addedBundledEmitReference = false
      if (isDeclarationFile(referencedFile)) {
        (declFileName = referencedFile.fileName)

      } else {
        forEachExpectedEmitFile(host, getDeclFileName, referencedFile,
            emitOnlyDtsFiles)

      }
      if (declFileName) {
        (declFileName = getRelativePathToDirectoryOrUrl(
            getDirectoryPath(normalizeSlashes(declarationFilePath)),
            declFileName, host.getCurrentDirectory(),
            host.getCanonicalFileName, false))
        (referencesOutput += s"""/// <reference path=\"${declFileName}\" />${newLine}""")

      }
      return addedBundledEmitReference
      def getDeclFileName(emitFileNames: EmitFileNames,
          _sourceFiles: Array[SourceFile], isBundledEmit: Boolean) = {
        if ((isBundledEmit && (!addBundledFileReference))) {
          return

        }
        Debug.assert(
            ((!(!emitFileNames.declarationFilePath)) || isSourceFileJavaScript(
                referencedFile)),
            "Declaration file is not present only for javascript files")
        (declFileName = (emitFileNames.declarationFilePath || emitFileNames.jsFilePath))
        (addedBundledEmitReference = isBundledEmit)

      }

    }

  }
  def writeDeclarationFile(declarationFilePath: String,
      sourceFiles: Array[SourceFile], isBundledEmit: Boolean, host: EmitHost,
      resolver: EmitResolver, emitterDiagnostics: DiagnosticCollection,
      emitOnlyDtsFiles: Boolean) = {
    val emitDeclarationResult =
      emitDeclarations(host, resolver, emitterDiagnostics, declarationFilePath,
          sourceFiles, isBundledEmit, emitOnlyDtsFiles)
    val emitSkipped = ((emitDeclarationResult.reportedDeclarationError || host
        .isEmitBlocked(declarationFilePath)) || host
        .getCompilerOptions()
        .noEmit)
    if ((!emitSkipped)) {
      val declarationOutput = (emitDeclarationResult.referencesOutput + getDeclarationOutput(
            emitDeclarationResult.synchronousDeclarationOutput,
            emitDeclarationResult.moduleElementDeclarationEmitInfo))
      writeFile(host, emitterDiagnostics, declarationFilePath,
          declarationOutput, host.getCompilerOptions().emitBOM, sourceFiles)

    }
    return emitSkipped
    def getDeclarationOutput(synchronousDeclarationOutput: String,
        moduleElementDeclarationEmitInfo: Array[
            ModuleElementDeclarationEmitInfo]) = {
      var appliedSyncOutputPos = 0
      var declarationOutput = ""
      forEach(moduleElementDeclarationEmitInfo,
          (aliasEmitInfo => {
            if (aliasEmitInfo.asynchronousOutput) {
              (declarationOutput += synchronousDeclarationOutput.substring(
                  appliedSyncOutputPos, aliasEmitInfo.outputPos))
              (declarationOutput += getDeclarationOutput(
                  aliasEmitInfo.asynchronousOutput,
                  aliasEmitInfo.subModuleElementDeclarationEmitInfo))
              (appliedSyncOutputPos = aliasEmitInfo.outputPos)

            }

          }))
      (declarationOutput += synchronousDeclarationOutput.substring(
          appliedSyncOutputPos))
      return declarationOutput

    }

  }
}
