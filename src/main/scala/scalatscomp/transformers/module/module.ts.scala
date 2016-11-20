package scalatscomp.transformers.module
object Module {
  def transformModule(context: TransformationContext) = {
    val transformModuleDelegates = createMap[((SourceFile) => SourceFile)](
        Map(ModuleKind.None -> transformCommonJSModule,
            ModuleKind.CommonJS -> transformCommonJSModule,
            ModuleKind.AMD -> transformAMDModule,
            ModuleKind.UMD -> transformUMDModule))
    const fresh1 = context
    val startLexicalEnvironment = fresh1.startLexicalEnvironment
    val endLexicalEnvironment = fresh1.endLexicalEnvironment
    val hoistVariableDeclaration = fresh1.hoistVariableDeclaration
    val compilerOptions = context.getCompilerOptions()
    val resolver = context.getEmitResolver()
    val host = context.getEmitHost()
    val languageVersion = getEmitScriptTarget(compilerOptions)
    val moduleKind = getEmitModuleKind(compilerOptions)
    val previousOnSubstituteNode = context.onSubstituteNode
    val previousOnEmitNode = context.onEmitNode
    (context.onSubstituteNode = onSubstituteNode)
    (context.onEmitNode = onEmitNode)
    context.enableSubstitution(SyntaxKind.Identifier)
    context.enableSubstitution(SyntaxKind.BinaryExpression)
    context.enableSubstitution(SyntaxKind.PrefixUnaryExpression)
    context.enableSubstitution(SyntaxKind.PostfixUnaryExpression)
    context.enableSubstitution(SyntaxKind.ShorthandPropertyAssignment)
    context.enableEmitNotification(SyntaxKind.SourceFile)
    var currentSourceFile: SourceFile = zeroOfMyType
    var externalImports: Array[(ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration)] =
      zeroOfMyType
    var exportSpecifiers: Map[Array[ExportSpecifier]] = zeroOfMyType
    var exportEquals: ExportAssignment = zeroOfMyType
    var bindingNameExportSpecifiersMap: Map[Array[ExportSpecifier]] =
      zeroOfMyType
    val bindingNameExportSpecifiersForFileMap =
      createMap[Map[Array[ExportSpecifier]]]()
    var hasExportStarsToExportValues: Boolean = zeroOfMyType
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      if (isDeclarationFile(node)) {
        return node

      }
      if ((isExternalModule(node) || compilerOptions.isolatedModules)) {
        (currentSourceFile = node)
        ((Map("externalImports" -> externalImports,
                    "exportSpecifiers" -> exportSpecifiers,
                    "exportEquals" -> exportEquals,
                    "hasExportStarsToExportValues" -> hasExportStarsToExportValues) =
                  collectExternalModuleInfo(node)))
        val transformModule = (transformModuleDelegates(moduleKind) || transformModuleDelegates(
              ModuleKind.None))
        val updated = transformModule(node)
        aggregateTransformFlags(updated)
        (currentSourceFile = undefined)
        (externalImports = undefined)
        (exportSpecifiers = undefined)
        (exportEquals = undefined)
        (hasExportStarsToExportValues = false)
        return updated

      }
      return node

    }
    def transformCommonJSModule(node: SourceFile) = {
      startLexicalEnvironment()
      val statements: Array[Statement] = Array()
      val statementOffset = addPrologueDirectives(statements, node.statements,
          (!compilerOptions.noImplicitUseStrict), visitor)
      addRange(statements,
          visitNodes(node.statements, visitor, isStatement, statementOffset))
      addRange(statements, endLexicalEnvironment())
      addExportEqualsIfNeeded(statements, false)
      val updated = updateSourceFile(node, statements)
      if (hasExportStarsToExportValues) {
        setEmitFlags(updated, (EmitFlags.EmitExportStar | getEmitFlags(node)))

      }
      return updated

    }
    def transformAMDModule(node: SourceFile) = {
      val define = createIdentifier("define")
      val moduleName = tryGetModuleNameFromFile(node, host, compilerOptions)
      return transformAsynchronousModule(node, define, moduleName, true)

    }
    def transformUMDModule(node: SourceFile) = {
      val define = createIdentifier("define")
      setEmitFlags(define, EmitFlags.UMDDefine)
      return transformAsynchronousModule(node, define, undefined, false)

    }
    def transformAsynchronousModule(node: SourceFile, define: Expression,
        moduleName: Expression, includeNonAmdDependencies: Boolean) = {
      const fresh2 =
        collectAsynchronousDependencies(node, includeNonAmdDependencies)
      val aliasedModuleNames = fresh2.aliasedModuleNames
      val unaliasedModuleNames = fresh2.unaliasedModuleNames
      val importAliasNames = fresh2.importAliasNames
      return updateSourceFile(node,
          Array(createStatement(createCall(define, undefined,
                      Array(
                          ((if (moduleName) Array(moduleName) else Array()) ): _*,
                          createArrayLiteral(Array(createLiteral("require"),
                                  createLiteral("exports"),
                                  aliasedModuleNames: _*,
                                  unaliasedModuleNames: _*)),
                          createFunctionExpression(undefined, undefined,
                              undefined, undefined,
                              Array(createParameter("require"),
                                  createParameter("exports"),
                                  importAliasNames: _*),
                              undefined,
                              transformAsynchronousModuleBody(node)))))))

    }
    def transformAsynchronousModuleBody(node: SourceFile) = {
      startLexicalEnvironment()
      val statements: Array[Statement] = Array()
      val statementOffset = addPrologueDirectives(statements, node.statements,
          (!compilerOptions.noImplicitUseStrict), visitor)
      addRange(statements,
          visitNodes(node.statements, visitor, isStatement, statementOffset))
      addRange(statements, endLexicalEnvironment())
      addExportEqualsIfNeeded(statements, true)
      val body = createBlock(statements, undefined, true)
      if (hasExportStarsToExportValues) {
        setEmitFlags(body, EmitFlags.EmitExportStar)

      }
      return body

    }
    def addExportEqualsIfNeeded(statements: Array[Statement],
        emitAsReturn: Boolean) = {
      if (exportEquals) {
        if (emitAsReturn) {
          val statement = createReturn(exportEquals.expression, exportEquals)
          setEmitFlags(statement,
              (EmitFlags.NoTokenSourceMaps | EmitFlags.NoComments))
          statements.push(statement)

        } else {
          val statement = createStatement(createAssignment(
                  createPropertyAccess(createIdentifier("module"), "exports"),
                  exportEquals.expression), exportEquals)
          setEmitFlags(statement, EmitFlags.NoComments)
          statements.push(statement)

        }

      }

    }
    def visitor(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.ImportDeclaration =>
          return visitImportDeclaration(node.asInstanceOf[ImportDeclaration])
        case SyntaxKind.ImportEqualsDeclaration =>
          return visitImportEqualsDeclaration(
              node.asInstanceOf[ImportEqualsDeclaration])
        case SyntaxKind.ExportDeclaration =>
          return visitExportDeclaration(node.asInstanceOf[ExportDeclaration])
        case SyntaxKind.ExportAssignment =>
          return visitExportAssignment(node.asInstanceOf[ExportAssignment])
        case SyntaxKind.VariableStatement =>
          return visitVariableStatement(node.asInstanceOf[VariableStatement])
        case SyntaxKind.FunctionDeclaration =>
          return visitFunctionDeclaration(
              node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.ClassDeclaration =>
          return visitClassDeclaration(node.asInstanceOf[ClassDeclaration])
        case SyntaxKind.ExpressionStatement =>
          return visitExpressionStatement(
              node.asInstanceOf[ExpressionStatement])
        case _ =>
          return node
      }

    }
    def visitImportDeclaration(
        node: ImportDeclaration): VisitResult[Statement] = {
      if ((!contains(externalImports, node))) {
        return undefined

      }
      val statements: Array[Statement] = Array()
      val namespaceDeclaration = getNamespaceDeclarationNode(node)
      if ((moduleKind !== ModuleKind.AMD)) {
        if ((!node.importClause)) {
          statements.push(createStatement(createRequireCall(node), node))

        } else {
          val variables: Array[VariableDeclaration] = Array()
          if ((namespaceDeclaration && (!isDefaultImport(node)))) {
            variables.push(
                createVariableDeclaration(
                    getSynthesizedClone(namespaceDeclaration.name), undefined,
                    createRequireCall(node)))

          } else {
            variables.push(
                createVariableDeclaration(getGeneratedNameForNode(node),
                    undefined, createRequireCall(node)))
            if ((namespaceDeclaration && isDefaultImport(node))) {
              variables.push(
                  createVariableDeclaration(
                      getSynthesizedClone(namespaceDeclaration.name),
                      undefined, getGeneratedNameForNode(node)))

            }

          }
          statements.push(createVariableStatement(undefined,
                  createConstDeclarationList(variables), node))

        }

      } else if ((namespaceDeclaration && isDefaultImport(node))) {
        statements.push(createVariableStatement(undefined,
                createVariableDeclarationList(Array(createVariableDeclaration(
                            getSynthesizedClone(namespaceDeclaration.name),
                            undefined, getGeneratedNameForNode(node), node)))))

      }
      addExportImportAssignments(statements, node)
      return singleOrMany(statements)

    }
    def visitImportEqualsDeclaration(
        node: ImportEqualsDeclaration): VisitResult[Statement] = {
      if ((!contains(externalImports, node))) {
        return undefined

      }
      setEmitFlags(node.name, EmitFlags.NoSubstitution)
      val statements: Array[Statement] = Array()
      if ((moduleKind !== ModuleKind.AMD)) {
        if (hasModifier(node, ModifierFlags.Export)) {
          statements.push(createStatement(createExportAssignment(node.name,
                      createRequireCall(node)), node))

        } else {
          statements.push(createVariableStatement(undefined,
                  createVariableDeclarationList(
                      Array(createVariableDeclaration(
                              getSynthesizedClone(node.name), undefined,
                              createRequireCall(node))), undefined,
                      (if ((languageVersion >= ScriptTarget.ES2015))
                         NodeFlags.Const
                       else NodeFlags.None)),
                  node))

        }

      } else {
        if (hasModifier(node, ModifierFlags.Export)) {
          statements
            .push(createStatement(createExportAssignment(node.name, node.name),
                    node))

        }

      }
      addExportImportAssignments(statements, node)
      return statements

    }
    def visitExportDeclaration(
        node: ExportDeclaration): VisitResult[Statement] = {
      if ((!contains(externalImports, node))) {
        return undefined

      }
      val generatedName = getGeneratedNameForNode(node)
      if (node.exportClause) {
        val statements: Array[Statement] = Array()
        if ((moduleKind !== ModuleKind.AMD)) {
          statements.push(createVariableStatement(undefined,
                  createVariableDeclarationList(
                      Array(createVariableDeclaration(generatedName, undefined,
                              createRequireCall(node)))),
                  node))

        }
        (node.exportClause.elements).foreach { fresh3 =>
          val specifier = zeroOfMyType = fresh3 {
            val exportedValue = createPropertyAccess(generatedName,
                (specifier.propertyName || specifier.name))
            statements.push(createStatement(createExportAssignment(
                        specifier.name, exportedValue), specifier))

          }
        }
        return singleOrMany(statements)

      } else {
        return createStatement(createCall(createIdentifier("__export"),
                undefined,
                Array((if ((moduleKind !== ModuleKind.AMD))
                         createRequireCall(node)
                       else generatedName))), node)

      }

    }
    def visitExportAssignment(
        node: ExportAssignment): VisitResult[Statement] = {
      if (node.isExportEquals) {
        return undefined

      }
      val statements: Array[Statement] = Array()
      addExportDefault(statements, node.expression, node)
      return statements

    }
    def addExportDefault(statements: Array[Statement], expression: Expression,
        location: TextRange): Unit = {
      tryAddExportDefaultCompat(statements)
      statements.push(createStatement(createExportAssignment(
                  createIdentifier("default"), expression), location))

    }
    def tryAddExportDefaultCompat(statements: Array[Statement]) = {
      val original = getOriginalNode(currentSourceFile)
      Debug.assert((original.kind === SyntaxKind.SourceFile))
      if ((!original.symbol.exports("____esModule"))) {
        if ((languageVersion === ScriptTarget.ES3)) {
          statements.push(
              createStatement(createExportAssignment(
                      createIdentifier("__esModule"), createLiteral(true))))

        } else {
          statements.push(
              createStatement(
                  createCall(createPropertyAccess(createIdentifier("Object"),
                          "defineProperty"), undefined,
                      Array(createIdentifier("exports"),
                          createLiteral("__esModule"),
                          createObjectLiteral(Array(createPropertyAssignment(
                                      "value", createLiteral(true))))))))

        }

      }

    }
    def addExportImportAssignments(statements: Array[Statement],
        node: (ImportEqualsDeclaration | ImportDeclaration)) = {
      if (isImportEqualsDeclaration(node)) {
        addExportMemberAssignments(statements, node.name)

      } else {
        val names = reduceEachChild(node, collectExportMembers, Array())
        (names).foreach { fresh4 =>
          val name = zeroOfMyType = fresh4 {
            addExportMemberAssignments(statements, name)

          }
        }

      }

    }
    def collectExportMembers(names: Array[Identifier],
        node: Node): Array[Identifier] = {
      if ((isAliasSymbolDeclaration(node) && isDeclaration(node))) {
        val name = node.name
        if (isIdentifier(name)) {
          names.push(name)

        }

      }
      return reduceEachChild(node, collectExportMembers, names)

    }
    def addExportMemberAssignments(statements: Array[Statement],
        name: Identifier): Unit = {
      if ((((!exportEquals) && exportSpecifiers) && hasProperty(
              exportSpecifiers, name.text))) {
        (exportSpecifiers(name.text)).foreach { fresh5 =>
          val specifier = zeroOfMyType = fresh5 {
            statements.push(
                startOnNewLine(createStatement(createExportAssignment(
                            specifier.name, name), specifier.name)))

          }
        }

      }

    }
    def addExportMemberAssignment(statements: Array[Statement],
        node: DeclarationStatement) = {
      if (hasModifier(node, ModifierFlags.Default)) {
        addExportDefault(statements, getDeclarationName(node), node)

      } else {
        statements.push(
            createExportStatement(node.name.asInstanceOf[Identifier],
                setEmitFlags(getSynthesizedClone(node.name),
                    EmitFlags.LocalName),
                node))

      }

    }
    def visitVariableStatement(
        node: VariableStatement): VisitResult[Statement] = {
      val originalKind = getOriginalNode(node).kind
      if ((((originalKind === SyntaxKind.ModuleDeclaration) || (originalKind === SyntaxKind.EnumDeclaration)) || (originalKind === SyntaxKind.ClassDeclaration))) {
        if ((!hasModifier(node, ModifierFlags.Export))) {
          return node

        }
        return setOriginalNode(createVariableStatement(undefined,
                node.declarationList), node)

      }
      val resultStatements: Array[Statement] = Array()
      if (hasModifier(node, ModifierFlags.Export)) {
        val variables = getInitializedVariables(node.declarationList)
        if ((variables.length > 0)) {
          val inlineAssignments = createStatement(
              inlineExpressions(map(variables, transformInitializedVariable)),
              node)
          resultStatements.push(inlineAssignments)

        }

      } else {
        resultStatements.push(node)

      }
      (node.declarationList.declarations).foreach { fresh6 =>
        val decl = zeroOfMyType = fresh6 {
          addExportMemberAssignmentsForBindingName(resultStatements, decl.name)

        }
      }
      return resultStatements

    }
    def addExportMemberAssignmentsForBindingName(
        resultStatements: Array[Statement], name: BindingName): Unit = {
      if (isBindingPattern(name)) {
        (name.elements).foreach { fresh7 =>
          val element = zeroOfMyType = fresh7 {
            if ((!isOmittedExpression(element))) {
              addExportMemberAssignmentsForBindingName(resultStatements,
                  element.name)

            }

          }
        }

      } else {
        if ((((!exportEquals) && exportSpecifiers) && hasProperty(
                exportSpecifiers, name.text))) {
          val sourceFileId = getOriginalNodeId(currentSourceFile)
          if ((!bindingNameExportSpecifiersForFileMap(sourceFileId))) {
            (
                bindingNameExportSpecifiersForFileMap(sourceFileId) =
                  createMap[Array[ExportSpecifier]]())

          }
          (
              bindingNameExportSpecifiersForFileMap(sourceFileId)(name.text) =
                exportSpecifiers(name.text))
          addExportMemberAssignments(resultStatements, name)

        }

      }

    }
    def transformInitializedVariable(node: VariableDeclaration): Expression = {
      val name = node.name
      if (isBindingPattern(name)) {
        return flattenVariableDestructuringToExpression(node,
            hoistVariableDeclaration, getModuleMemberName, visitor)

      } else {
        return createAssignment(getModuleMemberName(name),
            visitNode(node.initializer, visitor, isExpression))

      }

    }
    def visitFunctionDeclaration(
        node: FunctionDeclaration): VisitResult[Statement] = {
      val statements: Array[Statement] = Array()
      val name = (node.name || getGeneratedNameForNode(node))
      if (hasModifier(node, ModifierFlags.Export)) {
        val isAsync = hasModifier(node, ModifierFlags.Async)
        statements.push(setOriginalNode(createFunctionDeclaration(undefined,
                    (if (isAsync)
                       Array(createNode(SyntaxKind.AsyncKeyword).asInstanceOf[
                               Modifier])
                     else undefined),
                    node.asteriskToken, name, undefined, node.parameters,
                    undefined, node.body, node), node))
        addExportMemberAssignment(statements, node)

      } else {
        statements.push(node)

      }
      if (node.name) {
        addExportMemberAssignments(statements, node.name)

      }
      return singleOrMany(statements)

    }
    def visitClassDeclaration(
        node: ClassDeclaration): VisitResult[Statement] = {
      val statements: Array[Statement] = Array()
      val name = (node.name || getGeneratedNameForNode(node))
      if (hasModifier(node, ModifierFlags.Export)) {
        statements.push(setOriginalNode(createClassDeclaration(undefined,
                    undefined, name, undefined, node.heritageClauses,
                    node.members, node), node))
        addExportMemberAssignment(statements, node)

      } else {
        statements.push(node)

      }
      if ((node.name && (!((node.decorators && node.decorators.length))))) {
        addExportMemberAssignments(statements, node.name)

      }
      return singleOrMany(statements)

    }
    def visitExpressionStatement(
        node: ExpressionStatement): VisitResult[Statement] = {
      val original = getOriginalNode(node)
      val origKind = original.kind
      if (((origKind === SyntaxKind.EnumDeclaration) || (origKind === SyntaxKind.ModuleDeclaration))) {
        return visitExpressionStatementForEnumOrNamespaceDeclaration(node,
            original.asInstanceOf[(EnumDeclaration | ModuleDeclaration)])

      } else if ((origKind === SyntaxKind.ClassDeclaration)) {
        val classDecl = original.asInstanceOf[ClassDeclaration]
        if (classDecl.name) {
          val statements = Array(node)
          addExportMemberAssignments(statements, classDecl.name)
          return statements

        }

      }
      return node

    }
    def visitExpressionStatementForEnumOrNamespaceDeclaration(
        node: ExpressionStatement,
        original: (EnumDeclaration | ModuleDeclaration)): VisitResult[Statement] = {
      val statements: Array[Statement] = Array(node)
      if (((hasModifier(original,
              ModifierFlags.Export) && (original.kind === SyntaxKind.EnumDeclaration)) && isFirstDeclarationOfKind(
              original, SyntaxKind.EnumDeclaration))) {
        addVarForExportedEnumOrNamespaceDeclaration(statements, original)

      }
      addExportMemberAssignments(statements,
          original.name.asInstanceOf[Identifier])
      return statements

    }
    def addVarForExportedEnumOrNamespaceDeclaration(
        statements: Array[Statement],
        node: (EnumDeclaration | ModuleDeclaration)) = {
      val transformedStatement = createVariableStatement(undefined,
          Array(createVariableDeclaration(getDeclarationName(node), undefined,
                  createPropertyAccess(createIdentifier("exports"),
                      getDeclarationName(node)))),
          node)
      setEmitFlags(transformedStatement, EmitFlags.NoComments)
      statements.push(transformedStatement)

    }
    def getDeclarationName(node: DeclarationStatement) = {
      return (if (node.name)
                getSynthesizedClone(node.name.asInstanceOf[Identifier])
              else getGeneratedNameForNode(node))

    }
    def onEmitNode(emitContext: EmitContext, node: Node,
        emitCallback: ((EmitContext, Node) => Unit)): Unit = {
      if ((node.kind === SyntaxKind.SourceFile)) {
        (bindingNameExportSpecifiersMap =
          bindingNameExportSpecifiersForFileMap(getOriginalNodeId(node)))
        previousOnEmitNode(emitContext, node, emitCallback)
        (bindingNameExportSpecifiersMap = undefined)

      } else {
        previousOnEmitNode(emitContext, node, emitCallback)

      }

    }
    def onSubstituteNode(emitContext: EmitContext, node: Node) = {
      (node = previousOnSubstituteNode(emitContext, node))
      if ((emitContext === EmitContext.Expression)) {
        return substituteExpression(node.asInstanceOf[Expression])

      } else if (isShorthandPropertyAssignment(node)) {
        return substituteShorthandPropertyAssignment(node)

      }
      return node

    }
    def substituteShorthandPropertyAssignment(
        node: ShorthandPropertyAssignment): ObjectLiteralElementLike = {
      val name = node.name
      val exportedOrImportedName = substituteExpressionIdentifier(name)
      if ((exportedOrImportedName !== name)) {
        if (node.objectAssignmentInitializer) {
          val initializer = createAssignment(exportedOrImportedName,
              node.objectAssignmentInitializer)
          return createPropertyAssignment(name, initializer, node)

        }
        return createPropertyAssignment(name, exportedOrImportedName, node)

      }
      return node

    }
    def substituteExpression(node: Expression) = {
      node.kind match {
        case SyntaxKind.Identifier =>
          return substituteExpressionIdentifier(node.asInstanceOf[Identifier])
        case SyntaxKind.BinaryExpression =>
          return substituteBinaryExpression(
              node.asInstanceOf[BinaryExpression])
        case SyntaxKind.PostfixUnaryExpression |
            SyntaxKind.PrefixUnaryExpression =>
          return substituteUnaryExpression(
              node.asInstanceOf[
                  (PrefixUnaryExpression | PostfixUnaryExpression)])
        case _ =>
      }
      return node

    }
    def substituteExpressionIdentifier(node: Identifier): Expression = {
      return ((trySubstituteExportedName(node) || trySubstituteImportedName(
          node)) || node)

    }
    def substituteBinaryExpression(node: BinaryExpression): Expression = {
      val left = node.left
      if ((isIdentifier(left) && isAssignmentOperator(node.operatorToken.kind))) {
        if ((bindingNameExportSpecifiersMap && hasProperty(
                bindingNameExportSpecifiersMap, left.text))) {
          setEmitFlags(node, EmitFlags.NoSubstitution)
          var nestedExportAssignment: BinaryExpression = zeroOfMyType
          (bindingNameExportSpecifiersMap(left.text)).foreach { fresh8 =>
            val specifier = zeroOfMyType = fresh8 {
              (nestedExportAssignment =
                (if (nestedExportAssignment)
                   createExportAssignment(specifier.name,
                       nestedExportAssignment)
                 else createExportAssignment(specifier.name, node)))

            }
          }
          return nestedExportAssignment

        }

      }
      return node

    }
    def substituteUnaryExpression(
        node: (PrefixUnaryExpression | PostfixUnaryExpression)): Expression = {
      val operator = node.operator
      val operand = node.operand
      if ((isIdentifier(operand) && bindingNameExportSpecifiersForFileMap)) {
        if ((bindingNameExportSpecifiersMap && hasProperty(
                bindingNameExportSpecifiersMap, operand.text))) {
          setEmitFlags(node, EmitFlags.NoSubstitution)
          var transformedUnaryExpression: BinaryExpression = zeroOfMyType
          if ((node.kind === SyntaxKind.PostfixUnaryExpression)) {
            (transformedUnaryExpression = createBinary(operand,
                createToken(
                    (if ((operator === SyntaxKind.PlusPlusToken))
                       SyntaxKind.PlusEqualsToken
                     else SyntaxKind.MinusEqualsToken)),
                createLiteral(1), node))
            setEmitFlags(transformedUnaryExpression, EmitFlags.NoSubstitution)

          }
          var nestedExportAssignment: BinaryExpression = zeroOfMyType
          (bindingNameExportSpecifiersMap(operand.text)).foreach { fresh9 =>
            val specifier = zeroOfMyType = fresh9 {
              (nestedExportAssignment =
                (if (nestedExportAssignment)
                   createExportAssignment(specifier.name,
                       nestedExportAssignment)
                 else
                   createExportAssignment(specifier.name,
                       (transformedUnaryExpression || node))))

            }
          }
          return nestedExportAssignment

        }

      }
      return node

    }
    def trySubstituteExportedName(node: Identifier) = {
      val emitFlags = getEmitFlags(node)
      if ((((emitFlags & EmitFlags.LocalName)) === 0)) {
        val container = resolver.getReferencedExportContainer(node,
            (((emitFlags & EmitFlags.ExportName)) !== 0))
        if (container) {
          if ((container.kind === SyntaxKind.SourceFile)) {
            return createPropertyAccess(createIdentifier("exports"),
                getSynthesizedClone(node), node)

          }

        }

      }
      return undefined

    }
    def trySubstituteImportedName(node: Identifier): Expression = {
      if ((((getEmitFlags(node) & EmitFlags.LocalName)) === 0)) {
        val declaration = resolver.getReferencedImportDeclaration(node)
        if (declaration) {
          if (isImportClause(declaration)) {
            return createPropertyAccess(
                getGeneratedNameForNode(declaration.parent),
                createIdentifier("default"), node)

          } else if (isImportSpecifier(declaration)) {
            val name = (declaration.propertyName || declaration.name)
            return createPropertyAccess(
                getGeneratedNameForNode(declaration.parent.parent.parent),
                getSynthesizedClone(name), node)

          }

        }

      }
      return undefined

    }
    def getModuleMemberName(name: Identifier) = {
      return createPropertyAccess(createIdentifier("exports"), name, name)

    }
    def createRequireCall(
        importNode: (ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration)) = {
      val moduleName = getExternalModuleNameLiteral(importNode,
          currentSourceFile, host, resolver, compilerOptions)
      val args: Array[Expression] = Array()
      if (isDefined(moduleName)) {
        args.push(moduleName)

      }
      return createCall(createIdentifier("require"), undefined, args)

    }
    def createExportStatement(name: Identifier, value: Expression,
        location: TextRange) = {
      val statement = createStatement(createExportAssignment(name, value))
      (statement.startsOnNewLine = true)
      if (location) {
        setSourceMapRange(statement, location)

      }
      return statement

    }
    def createExportAssignment(name: Identifier, value: Expression) = {
      return createAssignment(createPropertyAccess(createIdentifier("exports"),
              getSynthesizedClone(name)), value)

    }
    trait AsynchronousDependencies {
      var aliasedModuleNames: Array[Expression]
      var unaliasedModuleNames: Array[Expression]
      var importAliasNames: Array[ParameterDeclaration]
    }
    def collectAsynchronousDependencies(node: SourceFile,
        includeNonAmdDependencies: Boolean): AsynchronousDependencies = {
      val aliasedModuleNames: Array[Expression] = Array()
      val unaliasedModuleNames: Array[Expression] = Array()
      val importAliasNames: Array[ParameterDeclaration] = Array()
      (node.amdDependencies).foreach { fresh10 =>
        val amdDependency = zeroOfMyType = fresh10 {
          if (amdDependency.name) {
            aliasedModuleNames.push(createLiteral(amdDependency.path))
            importAliasNames.push(createParameter(amdDependency.name))

          } else {
            unaliasedModuleNames.push(createLiteral(amdDependency.path))

          }

        }
      }
      (externalImports).foreach { fresh11 =>
        val importNode = zeroOfMyType = fresh11 {
          val externalModuleName = getExternalModuleNameLiteral(importNode,
              currentSourceFile, host, resolver, compilerOptions)
          val importAliasName =
            getLocalNameForExternalImport(importNode, currentSourceFile)
          if ((includeNonAmdDependencies && importAliasName)) {
            setEmitFlags(importAliasName, EmitFlags.NoSubstitution)
            aliasedModuleNames.push(externalModuleName)
            importAliasNames.push(createParameter(importAliasName))

          } else {
            unaliasedModuleNames.push(externalModuleName)

          }

        }
      }
      return Map("aliasedModuleNames" -> aliasedModuleNames,
          "unaliasedModuleNames" -> unaliasedModuleNames,
          "importAliasNames" -> importAliasNames)

    }
    def updateSourceFile(node: SourceFile, statements: Array[Statement]) = {
      val updated = getMutableClone(node)
      (updated.statements = createNodeArray(statements, node.statements))
      return updated

    }

  }
}
