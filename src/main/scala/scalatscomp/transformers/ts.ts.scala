package scalatscomp.transformers
object TS {
  val USE_NEW_TYPE_METADATA_FORMAT = false
  sealed abstract class TypeScriptSubstitutionFlags
  object TypeScriptSubstitutionFlags {
    case object ClassAliases extends TypeScriptSubstitutionFlags
    case object NamespaceExports extends TypeScriptSubstitutionFlags
    case object NonQualifiedEnumMembers extends TypeScriptSubstitutionFlags
  }
  def transformTypeScript(context: TransformationContext) = {
    const fresh1 = context
    val startLexicalEnvironment = fresh1.startLexicalEnvironment
    val endLexicalEnvironment = fresh1.endLexicalEnvironment
    val hoistVariableDeclaration = fresh1.hoistVariableDeclaration
    val resolver = context.getEmitResolver()
    val compilerOptions = context.getCompilerOptions()
    val languageVersion = getEmitScriptTarget(compilerOptions)
    val moduleKind = getEmitModuleKind(compilerOptions)
    val previousOnEmitNode = context.onEmitNode
    val previousOnSubstituteNode = context.onSubstituteNode
    (context.onEmitNode = onEmitNode)
    (context.onSubstituteNode = onSubstituteNode)
    context.enableSubstitution(SyntaxKind.PropertyAccessExpression)
    context.enableSubstitution(SyntaxKind.ElementAccessExpression)
    var currentSourceFile: SourceFile = zeroOfMyType
    var currentNamespace: ModuleDeclaration = zeroOfMyType
    var currentNamespaceContainerName: Identifier = zeroOfMyType
    var currentScope: (SourceFile | Block | ModuleBlock | CaseBlock) =
      zeroOfMyType
    var currentScopeFirstDeclarationsOfName: Map[Node] = zeroOfMyType
    var currentSourceFileExternalHelpersModuleName: Identifier = zeroOfMyType
    var enabledSubstitutions: TypeScriptSubstitutionFlags = zeroOfMyType
    var classAliases: Map[Identifier] = zeroOfMyType
    var applicableSubstitutions: TypeScriptSubstitutionFlags = zeroOfMyType
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      if (isDeclarationFile(node)) {
        return node

      }
      return visitNode(node, visitor, isSourceFile)

    }
    def saveStateAndInvoke[T](node: Node, f: ((Node) => T)): T = {
      val savedCurrentScope = currentScope
      val savedCurrentScopeFirstDeclarationsOfName =
        currentScopeFirstDeclarationsOfName
      onBeforeVisitNode(node)
      val visited = f(node)
      if ((currentScope !== savedCurrentScope)) {
        (currentScopeFirstDeclarationsOfName =
          savedCurrentScopeFirstDeclarationsOfName)

      }
      (currentScope = savedCurrentScope)
      return visited

    }
    def visitor(node: Node): VisitResult[Node] = {
      return saveStateAndInvoke(node, visitorWorker)

    }
    def visitorWorker(node: Node): VisitResult[Node] = {
      if ((node.kind === SyntaxKind.SourceFile)) {
        return visitSourceFile(node.asInstanceOf[SourceFile])

      } else if ((node.transformFlags & TransformFlags.TypeScript)) {
        return visitTypeScript(node)

      } else if ((node.transformFlags & TransformFlags.ContainsTypeScript)) {
        return visitEachChild(node, visitor, context)

      }
      return node

    }
    def sourceElementVisitor(node: Node): VisitResult[Node] = {
      return saveStateAndInvoke(node, sourceElementVisitorWorker)

    }
    def sourceElementVisitorWorker(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.ImportDeclaration =>
          return visitImportDeclaration(node.asInstanceOf[ImportDeclaration])
        case SyntaxKind.ImportEqualsDeclaration =>
          return visitImportEqualsDeclaration(
              node.asInstanceOf[ImportEqualsDeclaration])
        case SyntaxKind.ExportAssignment =>
          return visitExportAssignment(node.asInstanceOf[ExportAssignment])
        case SyntaxKind.ExportDeclaration =>
          return visitExportDeclaration(node.asInstanceOf[ExportDeclaration])
        case _ =>
          return visitorWorker(node)
      }

    }
    def namespaceElementVisitor(node: Node): VisitResult[Node] = {
      return saveStateAndInvoke(node, namespaceElementVisitorWorker)

    }
    def namespaceElementVisitorWorker(node: Node): VisitResult[Node] = {
      if (((((node.kind === SyntaxKind.ExportDeclaration) || (node.kind === SyntaxKind.ImportDeclaration)) || (node.kind === SyntaxKind.ImportClause)) || (((node.kind === SyntaxKind.ImportEqualsDeclaration) && ((
              node
                .asInstanceOf[ImportEqualsDeclaration])
            .moduleReference
            .kind === SyntaxKind.ExternalModuleReference))))) {
        return undefined

      } else if (((node.transformFlags & TransformFlags.TypeScript) || hasModifier(
              node, ModifierFlags.Export))) {
        return visitTypeScript(node)

      } else if ((node.transformFlags & TransformFlags.ContainsTypeScript)) {
        return visitEachChild(node, visitor, context)

      }
      return node

    }
    def classElementVisitor(node: Node): VisitResult[Node] = {
      return saveStateAndInvoke(node, classElementVisitorWorker)

    }
    def classElementVisitorWorker(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.Constructor =>
          return undefined
        case SyntaxKind.PropertyDeclaration | SyntaxKind.IndexSignature |
            SyntaxKind.GetAccessor | SyntaxKind.SetAccessor |
            SyntaxKind.MethodDeclaration =>
          return visitorWorker(node)
        case SyntaxKind.SemicolonClassElement =>
          return node
        case _ =>
          Debug.failBadSyntaxKind(node)
          return undefined
      }

    }
    def visitTypeScript(node: Node): VisitResult[Node] = {
      if ((hasModifier(node, ModifierFlags.Ambient) && isStatement(node))) {
        return createNotEmittedStatement(node)

      }
      node.kind match {
        case SyntaxKind.ExportKeyword | SyntaxKind.DefaultKeyword =>
          return (if (currentNamespace) undefined else node)
        case SyntaxKind.PublicKeyword | SyntaxKind.PrivateKeyword |
            SyntaxKind.ProtectedKeyword | SyntaxKind.AbstractKeyword |
            SyntaxKind.ConstKeyword | SyntaxKind.DeclareKeyword |
            SyntaxKind.ReadonlyKeyword | SyntaxKind.ArrayType |
            SyntaxKind.TupleType | SyntaxKind.TypeLiteral |
            SyntaxKind.TypePredicate | SyntaxKind.TypeParameter |
            SyntaxKind.AnyKeyword | SyntaxKind.BooleanKeyword |
            SyntaxKind.StringKeyword | SyntaxKind.NumberKeyword |
            SyntaxKind.NeverKeyword | SyntaxKind.VoidKeyword |
            SyntaxKind.SymbolKeyword | SyntaxKind.ConstructorType |
            SyntaxKind.FunctionType | SyntaxKind.TypeQuery |
            SyntaxKind.TypeReference | SyntaxKind.UnionType |
            SyntaxKind.IntersectionType | SyntaxKind.ParenthesizedType |
            SyntaxKind.ThisType | SyntaxKind.LiteralType |
            SyntaxKind.IndexSignature | SyntaxKind.Decorator |
            SyntaxKind.TypeAliasDeclaration | SyntaxKind.PropertyDeclaration |
            SyntaxKind.Constructor =>
          return visitConstructor(node.asInstanceOf[ConstructorDeclaration])
        case SyntaxKind.InterfaceDeclaration =>
          return createNotEmittedStatement(node)
        case SyntaxKind.ClassDeclaration =>
          return visitClassDeclaration(node.asInstanceOf[ClassDeclaration])
        case SyntaxKind.ClassExpression =>
          return visitClassExpression(node.asInstanceOf[ClassExpression])
        case SyntaxKind.HeritageClause =>
          return visitHeritageClause(node.asInstanceOf[HeritageClause])
        case SyntaxKind.ExpressionWithTypeArguments =>
          return visitExpressionWithTypeArguments(
              node.asInstanceOf[ExpressionWithTypeArguments])
        case SyntaxKind.MethodDeclaration =>
          return visitMethodDeclaration(node.asInstanceOf[MethodDeclaration])
        case SyntaxKind.GetAccessor =>
          return visitGetAccessor(node.asInstanceOf[GetAccessorDeclaration])
        case SyntaxKind.SetAccessor =>
          return visitSetAccessor(node.asInstanceOf[SetAccessorDeclaration])
        case SyntaxKind.FunctionDeclaration =>
          return visitFunctionDeclaration(
              node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.FunctionExpression =>
          return visitFunctionExpression(node.asInstanceOf[FunctionExpression])
        case SyntaxKind.ArrowFunction =>
          return visitArrowFunction(node.asInstanceOf[ArrowFunction])
        case SyntaxKind.Parameter =>
          return visitParameter(node.asInstanceOf[ParameterDeclaration])
        case SyntaxKind.ParenthesizedExpression =>
          return visitParenthesizedExpression(
              node.asInstanceOf[ParenthesizedExpression])
        case SyntaxKind.TypeAssertionExpression | SyntaxKind.AsExpression =>
          return visitAssertionExpression(
              node.asInstanceOf[AssertionExpression])
        case SyntaxKind.CallExpression =>
          return visitCallExpression(node.asInstanceOf[CallExpression])
        case SyntaxKind.NewExpression =>
          return visitNewExpression(node.asInstanceOf[NewExpression])
        case SyntaxKind.NonNullExpression =>
          return visitNonNullExpression(node.asInstanceOf[NonNullExpression])
        case SyntaxKind.EnumDeclaration =>
          return visitEnumDeclaration(node.asInstanceOf[EnumDeclaration])
        case SyntaxKind.VariableStatement =>
          return visitVariableStatement(node.asInstanceOf[VariableStatement])
        case SyntaxKind.VariableDeclaration =>
          return visitVariableDeclaration(
              node.asInstanceOf[VariableDeclaration])
        case SyntaxKind.ModuleDeclaration =>
          return visitModuleDeclaration(node.asInstanceOf[ModuleDeclaration])
        case SyntaxKind.ImportEqualsDeclaration =>
          return visitImportEqualsDeclaration(
              node.asInstanceOf[ImportEqualsDeclaration])
        case _ =>
          Debug.failBadSyntaxKind(node)
          return visitEachChild(node, visitor, context)
      }

    }
    def onBeforeVisitNode(node: Node) = {
      node.kind match {
        case SyntaxKind.SourceFile | SyntaxKind.CaseBlock |
            SyntaxKind.ModuleBlock | SyntaxKind.Block =>
          (currentScope =
            node.asInstanceOf[(SourceFile | CaseBlock | ModuleBlock | Block)])
          (currentScopeFirstDeclarationsOfName = undefined)
        case SyntaxKind.ClassDeclaration | SyntaxKind.FunctionDeclaration =>
          if (hasModifier(node, ModifierFlags.Ambient)) {
            break()

          }
          recordEmittedDeclarationInScope(node)
        case _ =>
      }

    }
    def visitSourceFile(node: SourceFile) = {
      (currentSourceFile = node)
      if (compilerOptions.alwaysStrict) {
        (node = ensureUseStrict(node))

      }
      if ((((node.flags & NodeFlags.EmitHelperFlags) && compilerOptions.importHelpers) && ((isExternalModule(
              node) || compilerOptions.isolatedModules)))) {
        startLexicalEnvironment()
        val statements: Array[Statement] = Array()
        val statementOffset =
          addPrologueDirectives(statements, node.statements, false, visitor)
        val externalHelpersModuleName =
          createUniqueName(externalHelpersModuleNameText)
        val externalHelpersModuleImport =
          createImportDeclaration(undefined, undefined,
              createImportClause(undefined,
                  createNamespaceImport(externalHelpersModuleName)),
              createLiteral(externalHelpersModuleNameText))
        (externalHelpersModuleImport.parent = node)
        (externalHelpersModuleImport.flags &= (~NodeFlags.Synthesized))
        statements.push(externalHelpersModuleImport)
        (currentSourceFileExternalHelpersModuleName = externalHelpersModuleName)
        addRange(statements,
            visitNodes(node.statements, sourceElementVisitor, isStatement,
                statementOffset))
        addRange(statements, endLexicalEnvironment())
        (currentSourceFileExternalHelpersModuleName = undefined)
        (node = updateSourceFileNode(node,
            createNodeArray(statements, node.statements)))
        (node.externalHelpersModuleName = externalHelpersModuleName)

      } else {
        (node = visitEachChild(node, sourceElementVisitor, context))

      }
      setEmitFlags(node, (EmitFlags.EmitEmitHelpers | getEmitFlags(node)))
      return node

    }
    def shouldEmitDecorateCallForClass(node: ClassDeclaration) = {
      if ((node.decorators && (node.decorators.length > 0))) {
        return true

      }
      val `constructor` = getFirstConstructorWithBody(node)
      if (`constructor`) {
        return forEach(`constructor`.parameters,
            shouldEmitDecorateCallForParameter)

      }
      return false

    }
    def shouldEmitDecorateCallForParameter(parameter: ParameterDeclaration) = {
      return ((parameter.decorators !== undefined) && (parameter.decorators.length > 0))

    }
    def visitClassDeclaration(
        node: ClassDeclaration): VisitResult[Statement] = {
      val staticProperties = getInitializedProperties(node, true)
      val hasExtendsClause = (getClassExtendsHeritageClauseElement(node) !== undefined)
      val isDecoratedClass = shouldEmitDecorateCallForClass(node)
      var classAlias: Identifier = zeroOfMyType
      var name = node.name
      if (((!name) && (staticProperties.length > 0))) {
        (name = getGeneratedNameForNode(node))

      }
      val statements: Array[Statement] = Array()
      if ((!isDecoratedClass)) {
        val classDeclaration = createClassDeclaration(undefined,
            visitNodes(node.modifiers, visitor, isModifier), name, undefined,
            visitNodes(node.heritageClauses, visitor, isHeritageClause),
            transformClassMembers(node, hasExtendsClause), node)
        setOriginalNode(classDeclaration, node)
        if ((staticProperties.length > 0)) {
          setEmitFlags(classDeclaration,
              (EmitFlags.NoTrailingSourceMap | getEmitFlags(classDeclaration)))

        }
        statements.push(classDeclaration)

      } else {
        (classAlias = addClassDeclarationHeadWithDecorators(statements, node,
            name, hasExtendsClause))

      }
      if (staticProperties.length) {
        addInitializedPropertyStatements(statements, staticProperties,
            getLocalName(node, true))

      }
      addClassElementDecorationStatements(statements, node, false)
      addClassElementDecorationStatements(statements, node, true)
      addConstructorDecorationStatement(statements, node, classAlias)
      if (isNamespaceExport(node)) {
        addExportMemberAssignment(statements, node)

      } else if (isDecoratedClass) {
        if (isDefaultExternalModuleExport(node)) {
          statements.push(createExportAssignment(undefined, undefined, false,
                  getLocalName(node)))

        } else if (isNamedExternalModuleExport(node)) {
          statements.push(createExternalModuleExport(name))

        }

      }
      return statements

    }
    def addClassDeclarationHeadWithDecorators(statements: Array[Statement],
        node: ClassDeclaration, name: Identifier,
        hasExtendsClause: Boolean) = {
      val location = moveRangePastDecorators(node)
      val classExpression: Expression = setOriginalNode(
          createClassExpression(undefined, name, undefined,
              visitNodes(node.heritageClauses, visitor, isHeritageClause),
              transformClassMembers(node, hasExtendsClause), location), node)
      if ((!name)) {
        (name = getGeneratedNameForNode(node))

      }
      var classAlias: Identifier = zeroOfMyType
      if ((resolver
            .getNodeCheckFlags(node) & NodeCheckFlags.ClassWithConstructorReference)) {
        enableSubstitutionForClassAliases()
        (classAlias = createUniqueName(
            (if ((node.name && (!isGeneratedIdentifier(node.name))))
               node.name.text
             else "default")))
        (classAliases(getOriginalNodeId(node)) = classAlias)

      }
      val declaredName = getDeclarationName(node, true)
      val transformedClassExpression = createVariableStatement(undefined,
          createLetDeclarationList(
              Array(createVariableDeclaration((classAlias || declaredName),
                      undefined, classExpression))),
          location)
      setCommentRange(transformedClassExpression, node)
      statements.push(setOriginalNode(transformedClassExpression, node))
      if (classAlias) {
        statements.push(setOriginalNode(createVariableStatement(undefined,
                    createLetDeclarationList(Array(createVariableDeclaration(
                                declaredName, undefined, classAlias))),
                    location), node))

      }
      return classAlias

    }
    def visitClassExpression(node: ClassExpression): Expression = {
      val staticProperties = getInitializedProperties(node, true)
      val heritageClauses =
        visitNodes(node.heritageClauses, visitor, isHeritageClause)
      val members = transformClassMembers(node,
          some(heritageClauses,
              (c => (c.token === SyntaxKind.ExtendsKeyword))))
      val classExpression = setOriginalNode(createClassExpression(undefined,
              node.name, undefined, heritageClauses, members, node), node)
      if ((staticProperties.length > 0)) {
        val expressions: Array[Expression] = Array()
        val temp = createTempVariable(hoistVariableDeclaration)
        if ((resolver
              .getNodeCheckFlags(node) & NodeCheckFlags.ClassWithConstructorReference)) {
          enableSubstitutionForClassAliases()
          (classAliases(getOriginalNodeId(node)) = getSynthesizedClone(temp))

        }
        setEmitFlags(classExpression,
            (EmitFlags.Indented | getEmitFlags(classExpression)))
        expressions
          .push(startOnNewLine(createAssignment(temp, classExpression)))
        addRange(expressions,
            generateInitializedPropertyExpressions(staticProperties, temp))
        expressions.push(startOnNewLine(temp))
        return inlineExpressions(expressions)

      }
      return classExpression

    }
    def transformClassMembers(node: (ClassDeclaration | ClassExpression),
        hasExtendsClause: Boolean) = {
      val members: Array[ClassElement] = Array()
      val `constructor` = transformConstructor(node, hasExtendsClause)
      if (`constructor`) {
        members.push(`constructor`)

      }
      addRange(members,
          visitNodes(node.members, classElementVisitor, isClassElement))
      return createNodeArray(members, node.members)

    }
    def transformConstructor(node: (ClassDeclaration | ClassExpression),
        hasExtendsClause: Boolean) = {
      val hasInstancePropertyWithInitializer =
        forEach(node.members, isInstanceInitializedProperty)
      val hasParameterPropertyAssignments = (node.transformFlags & TransformFlags.ContainsParameterPropertyAssignments)
      val `constructor` = getFirstConstructorWithBody(node)
      if (((!hasInstancePropertyWithInitializer) && (!hasParameterPropertyAssignments))) {
        return visitEachChild(`constructor`, visitor, context)

      }
      val parameters = transformConstructorParameters(`constructor`)
      val body =
        transformConstructorBody(node, `constructor`, hasExtendsClause)
      return startOnNewLine(setOriginalNode(createConstructor(undefined,
                  undefined, parameters, body, (`constructor` || node)),
              `constructor`))

    }
    def transformConstructorParameters(
        `constructor`: ConstructorDeclaration) = {
      return (if (`constructor`)
                visitNodes(`constructor`.parameters, visitor, isParameter)
              else Array().asInstanceOf[Array[ParameterDeclaration]])

    }
    def transformConstructorBody(node: (ClassExpression | ClassDeclaration),
        `constructor`: ConstructorDeclaration, hasExtendsClause: Boolean) = {
      val statements: Array[Statement] = Array()
      var indexOfFirstStatement = 0
      startLexicalEnvironment()
      if (`constructor`) {
        (indexOfFirstStatement =
          addPrologueDirectivesAndInitialSuperCall(`constructor`, statements))
        val propertyAssignments =
          getParametersWithPropertyAssignments(`constructor`)
        addRange(statements,
            map(propertyAssignments, transformParameterWithPropertyAssignment))

      } else if (hasExtendsClause) {
        statements.push(createStatement(createCall(createSuper(), undefined,
                    Array(createSpread(createIdentifier("arguments"))))))

      }
      val properties = getInitializedProperties(node, false)
      addInitializedPropertyStatements(statements, properties, createThis())
      if (`constructor`) {
        addRange(statements,
            visitNodes(`constructor`.body.statements, visitor, isStatement,
                indexOfFirstStatement))

      }
      addRange(statements, endLexicalEnvironment())
      return setMultiLine(createBlock(createNodeArray(statements,
                  (if (`constructor`) `constructor`.body.statements
                   else node.members)),
              (if (`constructor`) `constructor`.body else undefined)), true)

    }
    def addPrologueDirectivesAndInitialSuperCall(ctor: ConstructorDeclaration,
        result: Array[Statement]): Int = {
      if (ctor.body) {
        val statements = ctor.body.statements
        val index = addPrologueDirectives(result, statements, false, visitor)
        if ((index === statements.length)) {
          return index

        }
        val statement = statements(index)
        if (((statement.kind === SyntaxKind.ExpressionStatement) && isSuperCall(
                (statement.asInstanceOf[ExpressionStatement]).expression))) {
          result.push(visitNode(statement, visitor, isStatement))
          return (index + 1)

        }
        return index

      }
      return 0

    }
    def getParametersWithPropertyAssignments(
        node: ConstructorDeclaration): Array[ParameterDeclaration] = {
      return filter(node.parameters, isParameterWithPropertyAssignment)

    }
    def isParameterWithPropertyAssignment(parameter: ParameterDeclaration) = {
      return (hasModifier(parameter,
          ModifierFlags.ParameterPropertyModifier) && isIdentifier(
          parameter.name))

    }
    def transformParameterWithPropertyAssignment(
        node: ParameterDeclaration) = {
      Debug.assert(isIdentifier(node.name))
      val name = node.name.asInstanceOf[Identifier]
      val propertyName = getMutableClone(name)
      setEmitFlags(propertyName,
          (EmitFlags.NoComments | EmitFlags.NoSourceMap))
      val localName = getMutableClone(name)
      setEmitFlags(localName, EmitFlags.NoComments)
      return startOnNewLine(createStatement(createAssignment(
                  createPropertyAccess(createThis(), propertyName, node.name),
                  localName), moveRangePos(node, (-1))))

    }
    def getInitializedProperties(node: (ClassExpression | ClassDeclaration),
        isStatic: Boolean): Array[PropertyDeclaration] = {
      return filter(node.members,
          (if (isStatic) isStaticInitializedProperty
           else isInstanceInitializedProperty))

    }
    def isStaticInitializedProperty(member: ClassElement): Boolean = {
      return isInitializedProperty(member, true)

    }
    def isInstanceInitializedProperty(member: ClassElement): Boolean = {
      return isInitializedProperty(member, false)

    }
    def isInitializedProperty(member: ClassElement, isStatic: Boolean) = {
      return (((member.kind === SyntaxKind.PropertyDeclaration) && (isStatic === hasModifier(
          member, ModifierFlags.Static))) && ((
          member.asInstanceOf[PropertyDeclaration]).initializer !== undefined))

    }
    def addInitializedPropertyStatements(statements: Array[Statement],
        properties: Array[PropertyDeclaration],
        receiver: LeftHandSideExpression) = {
      (properties).foreach { fresh2 =>
        val property = zeroOfMyType = fresh2 {
          val statement =
            createStatement(transformInitializedProperty(property, receiver))
          setSourceMapRange(statement, moveRangePastModifiers(property))
          setCommentRange(statement, property)
          statements.push(statement)

        }
      }

    }
    def generateInitializedPropertyExpressions(
        properties: Array[PropertyDeclaration],
        receiver: LeftHandSideExpression) = {
      val expressions: Array[Expression] = Array()
      (properties).foreach { fresh3 =>
        val property = zeroOfMyType = fresh3 {
          val expression = transformInitializedProperty(property, receiver)
          (expression.startsOnNewLine = true)
          setSourceMapRange(expression, moveRangePastModifiers(property))
          setCommentRange(expression, property)
          expressions.push(expression)

        }
      }
      return expressions

    }
    def transformInitializedProperty(property: PropertyDeclaration,
        receiver: LeftHandSideExpression) = {
      val propertyName = visitPropertyNameOfClassElement(property)
      val initializer = visitNode(property.initializer, visitor, isExpression)
      val memberAccess =
        createMemberAccessForPropertyName(receiver, propertyName, propertyName)
      return createAssignment(memberAccess, initializer)

    }
    def getDecoratedClassElements(node: (ClassExpression | ClassDeclaration),
        isStatic: Boolean): Array[ClassElement] = {
      return filter(node.members,
          (if (isStatic) isStaticDecoratedClassElement
           else isInstanceDecoratedClassElement))

    }
    def isStaticDecoratedClassElement(member: ClassElement) = {
      return isDecoratedClassElement(member, true)

    }
    def isInstanceDecoratedClassElement(member: ClassElement) = {
      return isDecoratedClassElement(member, false)

    }
    def isDecoratedClassElement(member: ClassElement, isStatic: Boolean) = {
      return (nodeOrChildIsDecorated(member) && (isStatic === hasModifier(
          member, ModifierFlags.Static)))

    }
    trait AllDecorators {
      var decorators: Array[Decorator]
      var parameters: Array[Array[Decorator]]
    }
    def getDecoratorsOfParameters(node: FunctionLikeDeclaration) = {
      var decorators: Array[Array[Decorator]] = zeroOfMyType
      if (node) {
        val parameters = node.parameters {
          var i = 0
          while ((i < parameters.length)) {
            {
              val parameter = parameters(i)
              if ((decorators || parameter.decorators)) {
                if ((!decorators)) {
                  (decorators = new Array(parameters.length))

                }
                (decorators(i) = parameter.decorators)

              }

            }
            (i += 1)
          }
        }

      }
      return decorators

    }
    def getAllDecoratorsOfConstructor(
        node: (ClassExpression | ClassDeclaration)): AllDecorators = {
      val decorators = node.decorators
      val parameters =
        getDecoratorsOfParameters(getFirstConstructorWithBody(node))
      if (((!decorators) && (!parameters))) {
        return undefined

      }
      return Map("decorators" -> decorators, "parameters" -> parameters)

    }
    def getAllDecoratorsOfClassElement(
        node: (ClassExpression | ClassDeclaration),
        member: ClassElement): AllDecorators = {
      member.kind match {
        case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
          return getAllDecoratorsOfAccessors(node,
              member.asInstanceOf[AccessorDeclaration])
        case SyntaxKind.MethodDeclaration =>
          return getAllDecoratorsOfMethod(
              member.asInstanceOf[MethodDeclaration])
        case SyntaxKind.PropertyDeclaration =>
          return getAllDecoratorsOfProperty(
              member.asInstanceOf[PropertyDeclaration])
        case _ =>
          return undefined
      }

    }
    def getAllDecoratorsOfAccessors(node: (ClassExpression | ClassDeclaration),
        accessor: AccessorDeclaration): AllDecorators = {
      if ((!accessor.body)) {
        return undefined

      }
      const fresh4 = getAllAccessorDeclarations(node.members, accessor)
      val firstAccessor = fresh4.firstAccessor
      val secondAccessor = fresh4.secondAccessor
      val setAccessor = fresh4.setAccessor
      if ((accessor !== firstAccessor)) {
        return undefined

      }
      val decorators = (firstAccessor.decorators || ((secondAccessor && secondAccessor.decorators)))
      val parameters = getDecoratorsOfParameters(setAccessor)
      if (((!decorators) && (!parameters))) {
        return undefined

      }
      return Map("decorators" -> decorators, "parameters" -> parameters)

    }
    def getAllDecoratorsOfMethod(method: MethodDeclaration): AllDecorators = {
      if ((!method.body)) {
        return undefined

      }
      val decorators = method.decorators
      val parameters = getDecoratorsOfParameters(method)
      if (((!decorators) && (!parameters))) {
        return undefined

      }
      return Map("decorators" -> decorators, "parameters" -> parameters)

    }
    def getAllDecoratorsOfProperty(
        property: PropertyDeclaration): AllDecorators = {
      val decorators = property.decorators
      if ((!decorators)) {
        return undefined

      }
      return Map("decorators" -> decorators)

    }
    def transformAllDecoratorsOfDeclaration(node: Declaration,
        allDecorators: AllDecorators) = {
      if ((!allDecorators)) {
        return undefined

      }
      val decoratorExpressions: Array[Expression] = Array()
      addRange(decoratorExpressions,
          map(allDecorators.decorators, transformDecorator))
      addRange(decoratorExpressions,
          flatMap(allDecorators.parameters, transformDecoratorsOfParameter))
      addTypeMetadata(node, decoratorExpressions)
      return decoratorExpressions

    }
    def addClassElementDecorationStatements(statements: Array[Statement],
        node: ClassDeclaration, isStatic: Boolean) = {
      addRange(statements,
          map(generateClassElementDecorationExpressions(node, isStatic),
              expressionToStatement))

    }
    def generateClassElementDecorationExpressions(
        node: (ClassExpression | ClassDeclaration), isStatic: Boolean) = {
      val members = getDecoratedClassElements(node, isStatic)
      var expressions: Array[Expression] = zeroOfMyType
      (members).foreach { fresh5 =>
        val member = zeroOfMyType = fresh5 {
          val expression =
            generateClassElementDecorationExpression(node, member)
          if (expression) {
            if ((!expressions)) {
              (expressions = Array(expression))

            } else {
              expressions.push(expression)

            }

          }

        }
      }
      return expressions

    }
    def generateClassElementDecorationExpression(
        node: (ClassExpression | ClassDeclaration), member: ClassElement) = {
      val allDecorators = getAllDecoratorsOfClassElement(node, member)
      val decoratorExpressions =
        transformAllDecoratorsOfDeclaration(member, allDecorators)
      if ((!decoratorExpressions)) {
        return undefined

      }
      val prefix = getClassMemberPrefix(node, member)
      val memberName = getExpressionForPropertyName(member, true)
      val descriptor =
        (if ((languageVersion > ScriptTarget.ES3))
           (if ((member.kind === SyntaxKind.PropertyDeclaration))
              createVoidZero()
            else createNull())
         else undefined)
      val helper = createDecorateHelper(
          currentSourceFileExternalHelpersModuleName, decoratorExpressions,
          prefix, memberName, descriptor, moveRangePastDecorators(member))
      setEmitFlags(helper, EmitFlags.NoComments)
      return helper

    }
    def addConstructorDecorationStatement(statements: Array[Statement],
        node: ClassDeclaration, decoratedClassAlias: Identifier) = {
      val expression =
        generateConstructorDecorationExpression(node, decoratedClassAlias)
      if (expression) {
        statements.push(setOriginalNode(createStatement(expression), node))

      }

    }
    def generateConstructorDecorationExpression(
        node: (ClassExpression | ClassDeclaration),
        decoratedClassAlias: Identifier) = {
      val allDecorators = getAllDecoratorsOfConstructor(node)
      val decoratorExpressions =
        transformAllDecoratorsOfDeclaration(node, allDecorators)
      if ((!decoratorExpressions)) {
        return undefined

      }
      if (decoratedClassAlias) {
        val expression = createAssignment(decoratedClassAlias,
            createDecorateHelper(currentSourceFileExternalHelpersModuleName,
                decoratorExpressions, getDeclarationName(node)))
        val result = createAssignment(getDeclarationName(node), expression,
            moveRangePastDecorators(node))
        setEmitFlags(result, EmitFlags.NoComments)
        return result

      } else {
        val result = createAssignment(getDeclarationName(node),
            createDecorateHelper(currentSourceFileExternalHelpersModuleName,
                decoratorExpressions, getDeclarationName(node)),
            moveRangePastDecorators(node))
        setEmitFlags(result, EmitFlags.NoComments)
        return result

      }

    }
    def transformDecorator(decorator: Decorator) = {
      return visitNode(decorator.expression, visitor, isExpression)

    }
    def transformDecoratorsOfParameter(decorators: Array[Decorator],
        parameterOffset: Int) = {
      var expressions: Array[Expression] = zeroOfMyType
      if (decorators) {
        (expressions = Array())
        (decorators).foreach { fresh6 =>
          val decorator = zeroOfMyType = fresh6 {
            val helper = createParamHelper(
                currentSourceFileExternalHelpersModuleName,
                transformDecorator(decorator), parameterOffset,
                decorator.expression)
            setEmitFlags(helper, EmitFlags.NoComments)
            expressions.push(helper)

          }
        }

      }
      return expressions

    }
    def addTypeMetadata(node: Declaration,
        decoratorExpressions: Array[Expression]) = {
      if (USE_NEW_TYPE_METADATA_FORMAT) {
        addNewTypeMetadata(node, decoratorExpressions)

      } else {
        addOldTypeMetadata(node, decoratorExpressions)

      }

    }
    def addOldTypeMetadata(node: Declaration,
        decoratorExpressions: Array[Expression]) = {
      if (compilerOptions.emitDecoratorMetadata) {
        if (shouldAddTypeMetadata(node)) {
          decoratorExpressions.push(
              createMetadataHelper(currentSourceFileExternalHelpersModuleName,
                  "design:type", serializeTypeOfNode(node)))

        }
        if (shouldAddParamTypesMetadata(node)) {
          decoratorExpressions.push(
              createMetadataHelper(currentSourceFileExternalHelpersModuleName,
                  "design:paramtypes", serializeParameterTypesOfNode(node)))

        }
        if (shouldAddReturnTypeMetadata(node)) {
          decoratorExpressions.push(
              createMetadataHelper(currentSourceFileExternalHelpersModuleName,
                  "design:returntype", serializeReturnTypeOfNode(node)))

        }

      }

    }
    def addNewTypeMetadata(node: Declaration,
        decoratorExpressions: Array[Expression]) = {
      if (compilerOptions.emitDecoratorMetadata) {
        var properties: Array[ObjectLiteralElementLike] = zeroOfMyType
        if (shouldAddTypeMetadata(node)) {
          ((properties || ((properties = Array())))).push(
              createPropertyAssignment("type",
                  createArrowFunction(undefined, undefined, Array(), undefined,
                      undefined, serializeTypeOfNode(node))))

        }
        if (shouldAddParamTypesMetadata(node)) {
          ((properties || ((properties = Array())))).push(
              createPropertyAssignment("paramTypes",
                  createArrowFunction(undefined, undefined, Array(), undefined,
                      undefined, serializeParameterTypesOfNode(node))))

        }
        if (shouldAddReturnTypeMetadata(node)) {
          ((properties || ((properties = Array())))).push(
              createPropertyAssignment("returnType",
                  createArrowFunction(undefined, undefined, Array(), undefined,
                      undefined, serializeReturnTypeOfNode(node))))

        }
        if (properties) {
          decoratorExpressions
            .push(
                createMetadataHelper(
                    currentSourceFileExternalHelpersModuleName,
                    "design:typeinfo",
                    createObjectLiteral(properties, undefined, true)))

        }

      }

    }
    def shouldAddTypeMetadata(node: Declaration): Boolean = {
      val kind = node.kind
      return ((((kind === SyntaxKind.MethodDeclaration) || (kind === SyntaxKind.GetAccessor)) || (kind === SyntaxKind.SetAccessor)) || (kind === SyntaxKind.PropertyDeclaration))

    }
    def shouldAddReturnTypeMetadata(node: Declaration): Boolean = {
      return (node.kind === SyntaxKind.MethodDeclaration)

    }
    def shouldAddParamTypesMetadata(node: Declaration): Boolean = {
      val kind = node.kind
      return (((((kind === SyntaxKind.ClassDeclaration) || (kind === SyntaxKind.ClassExpression)) || (kind === SyntaxKind.MethodDeclaration)) || (kind === SyntaxKind.GetAccessor)) || (kind === SyntaxKind.SetAccessor))

    }
    def serializeTypeOfNode(node: Node): Expression = {
      node.kind match {
        case SyntaxKind.PropertyDeclaration | SyntaxKind.Parameter |
            SyntaxKind.GetAccessor =>
          return serializeTypeNode(
              (
                  node
                    .asInstanceOf[
                        (PropertyDeclaration | ParameterDeclaration | GetAccessorDeclaration)])
                .`type`)
        case SyntaxKind.SetAccessor =>
          return serializeTypeNode(
              getSetAccessorTypeAnnotationNode(
                  node.asInstanceOf[SetAccessorDeclaration]))
        case SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression |
            SyntaxKind.MethodDeclaration =>
          return createIdentifier("Function")
        case _ =>
          return createVoidZero()
      }

    }
    def getRestParameterElementType(node: TypeNode) = {
      if ((node && (node.kind === SyntaxKind.ArrayType))) {
        return (node.asInstanceOf[ArrayTypeNode]).elementType

      } else if ((node && (node.kind === SyntaxKind.TypeReference))) {
        return singleOrUndefined(
            (node.asInstanceOf[TypeReferenceNode]).typeArguments)

      } else {
        return undefined

      }

    }
    def serializeParameterTypesOfNode(node: Node): Expression = {
      val valueDeclaration =
        (if (isClassLike(node)) getFirstConstructorWithBody(node)
         else (if ((isFunctionLike(node) && nodeIsPresent(node.body))) node
               else undefined))
      val expressions: Array[Expression] = Array()
      if (valueDeclaration) {
        val parameters = valueDeclaration.parameters
        val numParameters = parameters.length {
          var i = 0
          while ((i < numParameters)) {
            {
              val parameter = parameters(i)
              if ((((i === 0) && isIdentifier(parameter.name)) && (parameter.name.text === "this"))) {
                continue

              }
              if (parameter.dotDotDotToken) {
                expressions.push(
                    serializeTypeNode(
                        getRestParameterElementType(parameter.`type`)))

              } else {
                expressions.push(serializeTypeOfNode(parameter))

              }

            }
            (i += 1)
          }
        }

      }
      return createArrayLiteral(expressions)

    }
    def serializeReturnTypeOfNode(node: Node): Expression = {
      if ((isFunctionLike(node) && node.`type`)) {
        return serializeTypeNode(node.`type`)

      } else if (isAsyncFunctionLike(node)) {
        return createIdentifier("Promise")

      }
      return createVoidZero()

    }
    def serializeTypeNode(node: TypeNode): Expression = {
      if ((node === undefined)) {
        return createIdentifier("Object")

      }
      node.kind match {
        case SyntaxKind.VoidKeyword =>
          return createVoidZero()
        case SyntaxKind.ParenthesizedType =>
          return serializeTypeNode(
              (node.asInstanceOf[ParenthesizedTypeNode]).`type`)
        case SyntaxKind.FunctionType | SyntaxKind.ConstructorType =>
          return createIdentifier("Function")
        case SyntaxKind.ArrayType | SyntaxKind.TupleType =>
          return createIdentifier("Array")
        case SyntaxKind.TypePredicate | SyntaxKind.BooleanKeyword =>
          return createIdentifier("Boolean")
        case SyntaxKind.StringKeyword =>
          return createIdentifier("String")
        case SyntaxKind.LiteralType =>
          (node.asInstanceOf[LiteralTypeNode]).literal.kind match {
            case SyntaxKind.StringLiteral =>
              return createIdentifier("String")
            case SyntaxKind.NumericLiteral =>
              return createIdentifier("Number")
            case SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword =>
              return createIdentifier("Boolean")
            case _ =>
              Debug.failBadSyntaxKind(
                  (node.asInstanceOf[LiteralTypeNode]).literal)
          }
        case SyntaxKind.NumberKeyword =>
          return createIdentifier("Number")
        case SyntaxKind.SymbolKeyword =>
          return (if ((languageVersion < ScriptTarget.ES2015))
                    getGlobalSymbolNameWithFallback()
                  else createIdentifier("Symbol"))
        case SyntaxKind.TypeReference =>
          return serializeTypeReferenceNode(
              node.asInstanceOf[TypeReferenceNode])
        case SyntaxKind.IntersectionType | SyntaxKind.UnionType => {
          val unionOrIntersection =
            node.asInstanceOf[UnionOrIntersectionTypeNode]
          var serializedUnion: Identifier = zeroOfMyType
          (unionOrIntersection.types).foreach { fresh7 =>
            val typeNode = zeroOfMyType = fresh7 {
              val serializedIndividual =
                serializeTypeNode(typeNode).asInstanceOf[Identifier]
              if ((serializedIndividual.kind !== SyntaxKind.Identifier)) {
                (serializedUnion = undefined)
                break()

              }
              if ((serializedIndividual.text === "Object")) {
                return serializedIndividual

              }
              if ((serializedUnion && (serializedUnion.text !== serializedIndividual.text))) {
                (serializedUnion = undefined)
                break()

              }
              (serializedUnion = serializedIndividual)

            }
          }
          if (serializedUnion) {
            return serializedUnion

          }

        }
        case SyntaxKind.TypeQuery | SyntaxKind.TypeLiteral |
            SyntaxKind.AnyKeyword | SyntaxKind.ThisType =>
        case _ =>
          Debug.failBadSyntaxKind(node)
      }
      return createIdentifier("Object")

    }
    def serializeTypeReferenceNode(node: TypeReferenceNode) = {
      resolver
        .getTypeReferenceSerializationKind(node.typeName, currentScope) match {
        case TypeReferenceSerializationKind.Unknown =>
          val serialized = serializeEntityNameAsExpression(node.typeName, true)
          val temp = createTempVariable(hoistVariableDeclaration)
          return createLogicalOr(
              createLogicalAnd(createStrictEquality(
                      createTypeOf(createAssignment(temp, serialized)),
                      createLiteral("function")), temp),
              createIdentifier("Object"))
        case TypeReferenceSerializationKind.TypeWithConstructSignatureAndValue =>
          return serializeEntityNameAsExpression(node.typeName, false)
        case TypeReferenceSerializationKind.VoidNullableOrNeverType =>
          return createVoidZero()
        case TypeReferenceSerializationKind.BooleanType =>
          return createIdentifier("Boolean")
        case TypeReferenceSerializationKind.NumberLikeType =>
          return createIdentifier("Number")
        case TypeReferenceSerializationKind.StringLikeType =>
          return createIdentifier("String")
        case TypeReferenceSerializationKind.ArrayLikeType =>
          return createIdentifier("Array")
        case TypeReferenceSerializationKind.ESSymbolType =>
          return (if ((languageVersion < ScriptTarget.ES2015))
                    getGlobalSymbolNameWithFallback()
                  else createIdentifier("Symbol"))
        case TypeReferenceSerializationKind.TypeWithCallSignature =>
          return createIdentifier("Function")
        case TypeReferenceSerializationKind.Promise =>
          return createIdentifier("Promise")
        case _ =>
          return createIdentifier("Object")
      }

    }
    def serializeEntityNameAsExpression(node: EntityName,
        useFallback: Boolean): Expression = {
      node.kind match {
        case SyntaxKind.Identifier =>
          val name = getMutableClone(node.asInstanceOf[Identifier])
          (name.flags &= (~NodeFlags.Synthesized))
          (name.original = undefined)
          (name.parent = currentScope)
          if (useFallback) {
            return createLogicalAnd(createStrictInequality(createTypeOf(name),
                    createLiteral("undefined")), name)

          }
          return name
        case SyntaxKind.QualifiedName =>
          return serializeQualifiedNameAsExpression(
              node.asInstanceOf[QualifiedName], useFallback)
        case _ =>
      }

    }
    def serializeQualifiedNameAsExpression(node: QualifiedName,
        useFallback: Boolean): Expression = {
      var left: Expression = zeroOfMyType
      if ((node.left.kind === SyntaxKind.Identifier)) {
        (left = serializeEntityNameAsExpression(node.left, useFallback))

      } else if (useFallback) {
        val temp = createTempVariable(hoistVariableDeclaration)
        (left = createLogicalAnd(createAssignment(temp,
                serializeEntityNameAsExpression(node.left, true)), temp))

      } else {
        (left = serializeEntityNameAsExpression(node.left, false))

      }
      return createPropertyAccess(left, node.right)

    }
    def getGlobalSymbolNameWithFallback(): Expression = {
      return createConditional(
          createStrictEquality(createTypeOf(createIdentifier("Symbol")),
              createLiteral("function")),
          createToken(SyntaxKind.QuestionToken), createIdentifier("Symbol"),
          createToken(SyntaxKind.ColonToken), createIdentifier("Object"))

    }
    def getExpressionForPropertyName(member: (ClassElement | EnumMember),
        generateNameForComputedPropertyName: Boolean): Expression = {
      val name = member.name
      if (isComputedPropertyName(name)) {
        return (if (generateNameForComputedPropertyName)
                  getGeneratedNameForNode(name)
                else (name.asInstanceOf[ComputedPropertyName]).expression)

      } else if (isIdentifier(name)) {
        return createLiteral(name.text)

      } else {
        return getSynthesizedClone(name)

      }

    }
    def visitPropertyNameOfClassElement(member: ClassElement): PropertyName = {
      val name = member.name
      if (isComputedPropertyName(name)) {
        var expression = visitNode(name.expression, visitor, isExpression)
        if (member.decorators) {
          val generatedName = getGeneratedNameForNode(name)
          hoistVariableDeclaration(generatedName)
          (expression = createAssignment(generatedName, expression))

        }
        return setOriginalNode(createComputedPropertyName(expression, name),
            name)

      } else {
        return name

      }

    }
    def visitHeritageClause(node: HeritageClause): HeritageClause = {
      if ((node.token === SyntaxKind.ExtendsKeyword)) {
        val types =
          visitNodes(node.types, visitor, isExpressionWithTypeArguments, 0, 1)
        return createHeritageClause(SyntaxKind.ExtendsKeyword, types, node)

      }
      return undefined

    }
    def visitExpressionWithTypeArguments(
        node: ExpressionWithTypeArguments): ExpressionWithTypeArguments = {
      val expression =
        visitNode(node.expression, visitor, isLeftHandSideExpression)
      return createExpressionWithTypeArguments(undefined, expression, node)

    }
    def shouldEmitFunctionLikeDeclaration(node: FunctionLikeDeclaration) = {
      return (!nodeIsMissing(node.body))

    }
    def visitConstructor(node: ConstructorDeclaration) = {
      if ((!shouldEmitFunctionLikeDeclaration(node))) {
        return undefined

      }
      return visitEachChild(node, visitor, context)

    }
    def visitMethodDeclaration(node: MethodDeclaration) = {
      if ((!shouldEmitFunctionLikeDeclaration(node))) {
        return undefined

      }
      val method = createMethod(undefined,
          visitNodes(node.modifiers, visitor, isModifier), node.asteriskToken,
          visitPropertyNameOfClassElement(node), undefined,
          visitNodes(node.parameters, visitor, isParameter), undefined,
          transformFunctionBody(node), node)
      setCommentRange(method, node)
      setSourceMapRange(method, moveRangePastDecorators(node))
      setOriginalNode(method, node)
      return method

    }
    def shouldEmitAccessorDeclaration(node: AccessorDeclaration) = {
      return (!((nodeIsMissing(node.body) && hasModifier(node,
          ModifierFlags.Abstract))))

    }
    def visitGetAccessor(node: GetAccessorDeclaration) = {
      if ((!shouldEmitAccessorDeclaration(node))) {
        return undefined

      }
      val accessor = createGetAccessor(undefined,
          visitNodes(node.modifiers, visitor, isModifier),
          visitPropertyNameOfClassElement(node),
          visitNodes(node.parameters, visitor, isParameter), undefined,
          (if (node.body) visitEachChild(node.body, visitor, context)
           else createBlock(Array())),
          node)
      setCommentRange(accessor, node)
      setSourceMapRange(accessor, moveRangePastDecorators(node))
      setOriginalNode(accessor, node)
      return accessor

    }
    def visitSetAccessor(node: SetAccessorDeclaration) = {
      if ((!shouldEmitAccessorDeclaration(node))) {
        return undefined

      }
      val accessor = createSetAccessor(undefined,
          visitNodes(node.modifiers, visitor, isModifier),
          visitPropertyNameOfClassElement(node),
          visitNodes(node.parameters, visitor, isParameter),
          (if (node.body) visitEachChild(node.body, visitor, context)
           else createBlock(Array())),
          node)
      setCommentRange(accessor, node)
      setSourceMapRange(accessor, moveRangePastDecorators(node))
      setOriginalNode(accessor, node)
      return accessor

    }
    def visitFunctionDeclaration(
        node: FunctionDeclaration): VisitResult[Statement] = {
      if ((!shouldEmitFunctionLikeDeclaration(node))) {
        return createNotEmittedStatement(node)

      }
      val func = createFunctionDeclaration(undefined,
          visitNodes(node.modifiers, visitor, isModifier), node.asteriskToken,
          node.name, undefined,
          visitNodes(node.parameters, visitor, isParameter), undefined,
          transformFunctionBody(node), node)
      setOriginalNode(func, node)
      if (isNamespaceExport(node)) {
        val statements: Array[Statement] = Array(func)
        addExportMemberAssignment(statements, node)
        return statements

      }
      return func

    }
    def visitFunctionExpression(node: FunctionExpression): Expression = {
      if (nodeIsMissing(node.body)) {
        return createOmittedExpression()

      }
      val func = createFunctionExpression(visitNodes(node.modifiers, visitor,
              isModifier), node.asteriskToken, node.name, undefined,
          visitNodes(node.parameters, visitor, isParameter), undefined,
          transformFunctionBody(node), node)
      setOriginalNode(func, node)
      return func

    }
    def visitArrowFunction(node: ArrowFunction) = {
      val func =
        createArrowFunction(visitNodes(node.modifiers, visitor, isModifier),
            undefined, visitNodes(node.parameters, visitor, isParameter),
            undefined, node.equalsGreaterThanToken, transformConciseBody(node),
            node)
      setOriginalNode(func, node)
      return func

    }
    def transformFunctionBody(
        node: (MethodDeclaration | AccessorDeclaration | FunctionDeclaration | FunctionExpression)): FunctionBody = {
      return transformFunctionBodyWorker(node.body)

    }
    def transformFunctionBodyWorker(body: Block, start: Nothing = 0) = {
      val savedCurrentScope = currentScope
      val savedCurrentScopeFirstDeclarationsOfName =
        currentScopeFirstDeclarationsOfName
      (currentScope = body)
      (currentScopeFirstDeclarationsOfName = createMap[Node]())
      startLexicalEnvironment()
      val statements = visitNodes(body.statements, visitor, isStatement, start)
      val visited = updateBlock(body, statements)
      val declarations = endLexicalEnvironment()
      (currentScope = savedCurrentScope)
      (currentScopeFirstDeclarationsOfName =
        savedCurrentScopeFirstDeclarationsOfName)
      return mergeFunctionBodyLexicalEnvironment(visited, declarations)

    }
    def transformConciseBody(node: ArrowFunction): ConciseBody = {
      return transformConciseBodyWorker(node.body, false)

    }
    def transformConciseBodyWorker(body: (Block | Expression),
        forceBlockFunctionBody: Boolean) = {
      if (isBlock(body)) {
        return transformFunctionBodyWorker(body)

      } else {
        startLexicalEnvironment()
        val visited: (Expression | Block) =
          visitNode(body, visitor, isConciseBody)
        val declarations = endLexicalEnvironment()
        val merged = mergeFunctionBodyLexicalEnvironment(visited, declarations)
        if ((forceBlockFunctionBody && (!isBlock(merged)))) {
          return createBlock(
              Array(createReturn(merged.asInstanceOf[Expression])))

        } else {
          return merged

        }

      }

    }
    def visitParameter(node: ParameterDeclaration) = {
      if (parameterIsThisKeyword(node)) {
        return undefined

      }
      val parameter =
        createParameterDeclaration(undefined, undefined, node.dotDotDotToken,
            visitNode(node.name, visitor, isBindingName), undefined, undefined,
            visitNode(node.initializer, visitor, isExpression),
            moveRangePastModifiers(node))
      setOriginalNode(parameter, node)
      setCommentRange(parameter, node)
      setSourceMapRange(parameter, moveRangePastModifiers(node))
      setEmitFlags(parameter.name, EmitFlags.NoTrailingSourceMap)
      return parameter

    }
    def visitVariableStatement(node: VariableStatement): Statement = {
      if (isNamespaceExport(node)) {
        val variables = getInitializedVariables(node.declarationList)
        if ((variables.length === 0)) {
          return undefined

        }
        return createStatement(inlineExpressions(map(variables,
                    transformInitializedVariable)), node)

      } else {
        return visitEachChild(node, visitor, context)

      }

    }
    def transformInitializedVariable(node: VariableDeclaration): Expression = {
      val name = node.name
      if (isBindingPattern(name)) {
        return flattenVariableDestructuringToExpression(node,
            hoistVariableDeclaration,
            getNamespaceMemberNameWithSourceMapsAndWithoutComments, visitor)

      } else {
        return createAssignment(
            getNamespaceMemberNameWithSourceMapsAndWithoutComments(name),
            visitNode(node.initializer, visitor, isExpression), node)

      }

    }
    def visitVariableDeclaration(node: VariableDeclaration) = {
      return updateVariableDeclaration(node,
          visitNode(node.name, visitor, isBindingName), undefined,
          visitNode(node.initializer, visitor, isExpression))

    }
    def visitParenthesizedExpression(
        node: ParenthesizedExpression): Expression = {
      val innerExpression = skipOuterExpressions(node.expression,
          (~OuterExpressionKinds.Assertions))
      if (isAssertionExpression(innerExpression)) {
        val expression = visitNode(node.expression, visitor, isExpression)
        return createPartiallyEmittedExpression(expression, node)

      }
      return visitEachChild(node, visitor, context)

    }
    def visitAssertionExpression(node: AssertionExpression): Expression = {
      val expression = visitNode(node.expression, visitor, isExpression)
      return createPartiallyEmittedExpression(expression, node)

    }
    def visitNonNullExpression(node: NonNullExpression): Expression = {
      val expression =
        visitNode(node.expression, visitor, isLeftHandSideExpression)
      return createPartiallyEmittedExpression(expression, node)

    }
    def visitCallExpression(node: CallExpression) = {
      return updateCall(node,
          visitNode(node.expression, visitor, isExpression), undefined,
          visitNodes(node.arguments, visitor, isExpression))

    }
    def visitNewExpression(node: NewExpression) = {
      return updateNew(node, visitNode(node.expression, visitor, isExpression),
          undefined, visitNodes(node.arguments, visitor, isExpression))

    }
    def shouldEmitEnumDeclaration(node: EnumDeclaration) = {
      return (((!isConst(node)) || compilerOptions.preserveConstEnums) || compilerOptions.isolatedModules)

    }
    def shouldEmitVarForEnumDeclaration(
        node: (EnumDeclaration | ModuleDeclaration)) = {
      return (isFirstEmittedDeclarationInScope(node) && (((!hasModifier(node,
          ModifierFlags.Export)) || isES6ExportedDeclaration(node))))

    }
    def addVarForEnumExportedFromNamespace(statements: Array[Statement],
        node: (EnumDeclaration | ModuleDeclaration)) = {
      val statement = createVariableStatement(undefined,
          Array(createVariableDeclaration(getDeclarationName(node), undefined,
                  getExportName(node))))
      setSourceMapRange(statement, node)
      statements.push(statement)

    }
    def visitEnumDeclaration(node: EnumDeclaration): VisitResult[Statement] = {
      if ((!shouldEmitEnumDeclaration(node))) {
        return undefined

      }
      val statements: Array[Statement] = Array()
      var emitFlags = EmitFlags.AdviseOnEmitNode
      recordEmittedDeclarationInScope(node)
      if (shouldEmitVarForEnumDeclaration(node)) {
        addVarForEnumOrModuleDeclaration(statements, node)
        if (((moduleKind !== ModuleKind.System) || (currentScope !== currentSourceFile))) {
          (emitFlags |= EmitFlags.NoLeadingComments)

        }

      }
      val parameterName = getNamespaceParameterName(node)
      val containerName = getNamespaceContainerName(node)
      val exportName = getExportName(node)
      val enumStatement = createStatement(
          createCall(createFunctionExpression(undefined, undefined, undefined,
                  undefined, Array(createParameter(parameterName)), undefined,
                  transformEnumBody(node, containerName)), undefined,
              Array(createLogicalOr(exportName,
                      createAssignment(exportName, createObjectLiteral())))),
          node)
      setOriginalNode(enumStatement, node)
      setEmitFlags(enumStatement, emitFlags)
      statements.push(enumStatement)
      if (isNamespaceExport(node)) {
        addVarForEnumExportedFromNamespace(statements, node)

      }
      return statements

    }
    def transformEnumBody(node: EnumDeclaration,
        localName: Identifier): Block = {
      val savedCurrentNamespaceLocalName = currentNamespaceContainerName
      (currentNamespaceContainerName = localName)
      val statements: Array[Statement] = Array()
      startLexicalEnvironment()
      addRange(statements, map(node.members, transformEnumMember))
      addRange(statements, endLexicalEnvironment())
      (currentNamespaceContainerName = savedCurrentNamespaceLocalName)
      return createBlock(createNodeArray(statements, node.members), undefined,
          true)

    }
    def transformEnumMember(member: EnumMember): Statement = {
      val name = getExpressionForPropertyName(member, false)
      return createStatement(
          createAssignment(createElementAccess(currentNamespaceContainerName,
                  createAssignment(
                      createElementAccess(currentNamespaceContainerName, name),
                      transformEnumMemberDeclarationValue(member))), name,
              member), member)

    }
    def transformEnumMemberDeclarationValue(member: EnumMember): Expression = {
      val value = resolver.getConstantValue(member)
      if ((value !== undefined)) {
        return createLiteral(value)

      } else {
        enableSubstitutionForNonQualifiedEnumMembers()
        if (member.initializer) {
          return visitNode(member.initializer, visitor, isExpression)

        } else {
          return createVoidZero()

        }

      }

    }
    def shouldEmitModuleDeclaration(node: ModuleDeclaration) = {
      return isInstantiatedModule(node,
          (compilerOptions.preserveConstEnums || compilerOptions.isolatedModules))

    }
    def isES6ExportedDeclaration(node: Node) = {
      return (isExternalModuleExport(node) && (moduleKind === ModuleKind.ES2015))

    }
    def recordEmittedDeclarationInScope(node: Node) = {
      val name = (node.symbol && node.symbol.name)
      if (name) {
        if ((!currentScopeFirstDeclarationsOfName)) {
          (currentScopeFirstDeclarationsOfName = createMap[Node]())

        }
        if ((!((nameincurrentScopeFirstDeclarationsOfName)))) {
          (currentScopeFirstDeclarationsOfName(name) = node)

        }

      }

    }
    def isFirstEmittedDeclarationInScope(node: Node) = {
      if (currentScopeFirstDeclarationsOfName) {
        val name = (node.symbol && node.symbol.name)
        if (name) {
          return (currentScopeFirstDeclarationsOfName(name) === node)

        }

      }
      return false

    }
    def shouldEmitVarForModuleDeclaration(node: ModuleDeclaration) = {
      return isFirstEmittedDeclarationInScope(node)

    }
    def addVarForEnumOrModuleDeclaration(statements: Array[Statement],
        node: (ModuleDeclaration | EnumDeclaration)) = {
      val statement = createVariableStatement(
          (if (isES6ExportedDeclaration(node))
             visitNodes(node.modifiers, visitor, isModifier)
           else undefined),
          Array(createVariableDeclaration(getDeclarationName(node, false,
                      true))))
      setOriginalNode(statement, node)
      if ((node.kind === SyntaxKind.EnumDeclaration)) {
        setSourceMapRange(statement.declarationList, node)

      } else {
        setSourceMapRange(statement, node)

      }
      setCommentRange(statement, node)
      setEmitFlags(statement, EmitFlags.NoTrailingComments)
      statements.push(statement)

    }
    def visitModuleDeclaration(
        node: ModuleDeclaration): VisitResult[Statement] = {
      if ((!shouldEmitModuleDeclaration(node))) {
        return createNotEmittedStatement(node)

      }
      Debug.assert(isIdentifier(node.name),
          "TypeScript module should have an Identifier name.")
      enableSubstitutionForNamespaceExports()
      val statements: Array[Statement] = Array()
      var emitFlags = EmitFlags.AdviseOnEmitNode
      recordEmittedDeclarationInScope(node)
      if (shouldEmitVarForModuleDeclaration(node)) {
        addVarForEnumOrModuleDeclaration(statements, node)
        if (((moduleKind !== ModuleKind.System) || (currentScope !== currentSourceFile))) {
          (emitFlags |= EmitFlags.NoLeadingComments)

        }

      }
      val parameterName = getNamespaceParameterName(node)
      val containerName = getNamespaceContainerName(node)
      val exportName = getExportName(node)
      var moduleArg = createLogicalOr(exportName,
          createAssignment(exportName, createObjectLiteral()))
      if ((hasModifier(node,
              ModifierFlags.Export) && (!isES6ExportedDeclaration(node)))) {
        val localName = getLocalName(node)
        (moduleArg = createAssignment(localName, moduleArg))

      }
      val moduleStatement = createStatement(
          createCall(createFunctionExpression(undefined, undefined, undefined,
                  undefined, Array(createParameter(parameterName)), undefined,
                  transformModuleBody(node, containerName)), undefined,
              Array(moduleArg)), node)
      setOriginalNode(moduleStatement, node)
      setEmitFlags(moduleStatement, emitFlags)
      statements.push(moduleStatement)
      return statements

    }
    def transformModuleBody(node: ModuleDeclaration,
        namespaceLocalName: Identifier): Block = {
      val savedCurrentNamespaceContainerName = currentNamespaceContainerName
      val savedCurrentNamespace = currentNamespace
      val savedCurrentScopeFirstDeclarationsOfName =
        currentScopeFirstDeclarationsOfName
      (currentNamespaceContainerName = namespaceLocalName)
      (currentNamespace = node)
      (currentScopeFirstDeclarationsOfName = undefined)
      val statements: Array[Statement] = Array()
      startLexicalEnvironment()
      var statementsLocation: TextRange = zeroOfMyType
      var blockLocation: TextRange = zeroOfMyType
      val body = node.body
      if ((body.kind === SyntaxKind.ModuleBlock)) {
        addRange(statements,
            visitNodes((body.asInstanceOf[ModuleBlock]).statements,
                namespaceElementVisitor, isStatement))
        (statementsLocation = (body.asInstanceOf[ModuleBlock]).statements)
        (blockLocation = body)

      } else {
        val result =
          visitModuleDeclaration(body.asInstanceOf[ModuleDeclaration])
        if (result) {
          if (isArray(result)) {
            addRange(statements, result)

          } else {
            statements.push(result)

          }

        }
        val moduleBlock =
          getInnerMostModuleDeclarationFromDottedModule(node).body
            .asInstanceOf[ModuleBlock]
        (statementsLocation = moveRangePos(moduleBlock.statements, (-1)))

      }
      addRange(statements, endLexicalEnvironment())
      (currentNamespaceContainerName = savedCurrentNamespaceContainerName)
      (currentNamespace = savedCurrentNamespace)
      (currentScopeFirstDeclarationsOfName =
        savedCurrentScopeFirstDeclarationsOfName)
      val block = createBlock(createNodeArray(statements, statementsLocation),
          blockLocation, true)
      if ((body.kind !== SyntaxKind.ModuleBlock)) {
        setEmitFlags(block, (getEmitFlags(block) | EmitFlags.NoComments))

      }
      return block

    }
    def getInnerMostModuleDeclarationFromDottedModule(
        moduleDeclaration: ModuleDeclaration): ModuleDeclaration = {
      if ((moduleDeclaration.body.kind === SyntaxKind.ModuleDeclaration)) {
        val recursiveInnerModule =
          getInnerMostModuleDeclarationFromDottedModule(
              moduleDeclaration.body.asInstanceOf[ModuleDeclaration])
        return (recursiveInnerModule || moduleDeclaration.body.asInstanceOf[
            ModuleDeclaration])

      }

    }
    def visitImportDeclaration(
        node: ImportDeclaration): VisitResult[Statement] = {
      if ((!node.importClause)) {
        return node

      }
      val importClause =
        visitNode(node.importClause, visitImportClause, isImportClause, true)
      return (if (importClause)
                updateImportDeclaration(node, undefined, undefined,
                    importClause, node.moduleSpecifier)
              else undefined)

    }
    def visitImportClause(node: ImportClause): VisitResult[ImportClause] = {
      val name =
        (if (resolver.isReferencedAliasDeclaration(node)) node.name
         else undefined)
      val namedBindings = visitNode(node.namedBindings,
          visitNamedImportBindings, isNamedImportBindings, true)
      return (if (((name || namedBindings)))
                updateImportClause(node, name, namedBindings)
              else undefined)

    }
    def visitNamedImportBindings(
        node: NamedImportBindings): VisitResult[NamedImportBindings] = {
      if ((node.kind === SyntaxKind.NamespaceImport)) {
        return (if (resolver.isReferencedAliasDeclaration(node)) node
                else undefined)

      } else {
        val elements =
          visitNodes(node.elements, visitImportSpecifier, isImportSpecifier)
        return (if (some(elements)) updateNamedImports(node, elements)
                else undefined)

      }

    }
    def visitImportSpecifier(
        node: ImportSpecifier): VisitResult[ImportSpecifier] = {
      return (if (resolver.isReferencedAliasDeclaration(node)) node
              else undefined)

    }
    def visitExportAssignment(
        node: ExportAssignment): VisitResult[Statement] = {
      return (if (resolver.isValueAliasDeclaration(node))
                visitEachChild(node, visitor, context)
              else undefined)

    }
    def visitExportDeclaration(
        node: ExportDeclaration): VisitResult[Statement] = {
      if ((!node.exportClause)) {
        return (if (resolver.moduleExportsSomeValue(node.moduleSpecifier)) node
                else undefined)

      }
      if ((!resolver.isValueAliasDeclaration(node))) {
        return undefined

      }
      val exportClause =
        visitNode(node.exportClause, visitNamedExports, isNamedExports, true)
      return (if (exportClause)
                updateExportDeclaration(node, undefined, undefined,
                    exportClause, node.moduleSpecifier)
              else undefined)

    }
    def visitNamedExports(node: NamedExports): VisitResult[NamedExports] = {
      val elements =
        visitNodes(node.elements, visitExportSpecifier, isExportSpecifier)
      return (if (some(elements)) updateNamedExports(node, elements)
              else undefined)

    }
    def visitExportSpecifier(
        node: ExportSpecifier): VisitResult[ExportSpecifier] = {
      return (if (resolver.isValueAliasDeclaration(node)) node else undefined)

    }
    def shouldEmitImportEqualsDeclaration(node: ImportEqualsDeclaration) = {
      return (resolver
        .isReferencedAliasDeclaration(node) || (((!isExternalModule(
          currentSourceFile)) && resolver
        .isTopLevelValueImportEqualsWithEntityName(node))))

    }
    def visitImportEqualsDeclaration(
        node: ImportEqualsDeclaration): VisitResult[Statement] = {
      if (isExternalModuleImportEqualsDeclaration(node)) {
        return (if (resolver.isReferencedAliasDeclaration(node))
                  visitEachChild(node, visitor, context)
                else undefined)

      }
      if ((!shouldEmitImportEqualsDeclaration(node))) {
        return undefined

      }
      val moduleReference = createExpressionFromEntityName(
          node.moduleReference.asInstanceOf[EntityName])
      setEmitFlags(moduleReference,
          (EmitFlags.NoComments | EmitFlags.NoNestedComments))
      if ((isNamedExternalModuleExport(node) || (!isNamespaceExport(node)))) {
        return setOriginalNode(createVariableStatement(
                visitNodes(node.modifiers, visitor, isModifier),
                createVariableDeclarationList(Array(createVariableDeclaration(
                            node.name, undefined, moduleReference))),
                node), node)

      } else {
        return setOriginalNode(createNamespaceExport(node.name,
                moduleReference, node), node)

      }

    }
    def isNamespaceExport(node: Node) = {
      return ((currentNamespace !== undefined) && hasModifier(node,
          ModifierFlags.Export))

    }
    def isExternalModuleExport(node: Node) = {
      return ((currentNamespace === undefined) && hasModifier(node,
          ModifierFlags.Export))

    }
    def isNamedExternalModuleExport(node: Node) = {
      return (isExternalModuleExport(node) && (!hasModifier(node,
          ModifierFlags.Default)))

    }
    def isDefaultExternalModuleExport(node: Node) = {
      return (isExternalModuleExport(node) && hasModifier(node,
          ModifierFlags.Default))

    }
    def expressionToStatement(expression: Expression) = {
      return createStatement(expression, undefined)

    }
    def addExportMemberAssignment(statements: Array[Statement],
        node: (ClassDeclaration | FunctionDeclaration)) = {
      val expression =
        createAssignment(getExportName(node), getLocalName(node, true))
      setSourceMapRange(expression, createRange(node.name.pos, node.end))
      val statement = createStatement(expression)
      setSourceMapRange(statement, createRange((-1), node.end))
      statements.push(statement)

    }
    def createNamespaceExport(exportName: Identifier, exportValue: Expression,
        location: TextRange) = {
      return createStatement(
          createAssignment(getNamespaceMemberName(exportName, false, true),
              exportValue), location)

    }
    def createExternalModuleExport(exportName: Identifier) = {
      return createExportDeclaration(undefined, undefined,
          createNamedExports(Array(createExportSpecifier(exportName))))

    }
    def getNamespaceMemberName(name: Identifier, allowComments: Boolean,
        allowSourceMaps: Boolean): Expression = {
      val qualifiedName = createPropertyAccess(currentNamespaceContainerName,
          getSynthesizedClone(name), name)
      var emitFlags: EmitFlags = zeroOfMyType
      if ((!allowComments)) {
        (emitFlags |= EmitFlags.NoComments)

      }
      if ((!allowSourceMaps)) {
        (emitFlags |= EmitFlags.NoSourceMap)

      }
      if (emitFlags) {
        setEmitFlags(qualifiedName, emitFlags)

      }
      return qualifiedName

    }
    def getNamespaceMemberNameWithSourceMapsAndWithoutComments(
        name: Identifier) = {
      return getNamespaceMemberName(name, false, true)

    }
    def getNamespaceParameterName(
        node: (ModuleDeclaration | EnumDeclaration)) = {
      val name = getGeneratedNameForNode(node)
      setSourceMapRange(name, node.name)
      return name

    }
    def getNamespaceContainerName(
        node: (ModuleDeclaration | EnumDeclaration)) = {
      return getGeneratedNameForNode(node)

    }
    def getLocalName(
        node: (FunctionDeclaration | ClassDeclaration | ClassExpression | ModuleDeclaration | EnumDeclaration),
        noSourceMaps: Boolean, allowComments: Boolean) = {
      return getDeclarationName(node, allowComments, (!noSourceMaps),
          EmitFlags.LocalName)

    }
    def getExportName(
        node: (FunctionDeclaration | ClassDeclaration | ClassExpression | ModuleDeclaration | EnumDeclaration),
        noSourceMaps: Boolean, allowComments: Boolean) = {
      if (isNamespaceExport(node)) {
        return getNamespaceMemberName(getDeclarationName(node), allowComments,
            (!noSourceMaps))

      }
      return getDeclarationName(node, allowComments, (!noSourceMaps),
          EmitFlags.ExportName)

    }
    def getDeclarationName(
        node: (FunctionDeclaration | ClassDeclaration | ClassExpression | ModuleDeclaration | EnumDeclaration),
        allowComments: Boolean, allowSourceMaps: Boolean,
        emitFlags: EmitFlags) = {
      if (node.name) {
        val name = getMutableClone(node.name.asInstanceOf[Identifier])
        (emitFlags |= getEmitFlags(node.name))
        if ((!allowSourceMaps)) {
          (emitFlags |= EmitFlags.NoSourceMap)

        }
        if ((!allowComments)) {
          (emitFlags |= EmitFlags.NoComments)

        }
        if (emitFlags) {
          setEmitFlags(name, emitFlags)

        }
        return name

      } else {
        return getGeneratedNameForNode(node)

      }

    }
    def getClassPrototype(node: (ClassExpression | ClassDeclaration)) = {
      return createPropertyAccess(getDeclarationName(node), "prototype")

    }
    def getClassMemberPrefix(node: (ClassExpression | ClassDeclaration),
        member: ClassElement) = {
      return (if (hasModifier(member, ModifierFlags.Static))
                getDeclarationName(node)
              else getClassPrototype(node))

    }
    def enableSubstitutionForNonQualifiedEnumMembers() = {
      if ((((enabledSubstitutions & TypeScriptSubstitutionFlags.NonQualifiedEnumMembers)) === 0)) {
        (enabledSubstitutions |= TypeScriptSubstitutionFlags.NonQualifiedEnumMembers)
        context.enableSubstitution(SyntaxKind.Identifier)

      }

    }
    def enableSubstitutionForClassAliases() = {
      if ((((enabledSubstitutions & TypeScriptSubstitutionFlags.ClassAliases)) === 0)) {
        (enabledSubstitutions |= TypeScriptSubstitutionFlags.ClassAliases)
        context.enableSubstitution(SyntaxKind.Identifier)
        (classAliases = createMap[Identifier]())

      }

    }
    def enableSubstitutionForNamespaceExports() = {
      if ((((enabledSubstitutions & TypeScriptSubstitutionFlags.NamespaceExports)) === 0)) {
        (enabledSubstitutions |= TypeScriptSubstitutionFlags.NamespaceExports)
        context.enableSubstitution(SyntaxKind.Identifier)
        context.enableSubstitution(SyntaxKind.ShorthandPropertyAssignment)
        context.enableEmitNotification(SyntaxKind.ModuleDeclaration)

      }

    }
    def isTransformedModuleDeclaration(node: Node): Boolean = {
      return (getOriginalNode(node).kind === SyntaxKind.ModuleDeclaration)

    }
    def isTransformedEnumDeclaration(node: Node): Boolean = {
      return (getOriginalNode(node).kind === SyntaxKind.EnumDeclaration)

    }
    def onEmitNode(emitContext: EmitContext, node: Node,
        emitCallback: ((EmitContext, Node) => Unit)): Unit = {
      val savedApplicableSubstitutions = applicableSubstitutions
      if (((enabledSubstitutions & TypeScriptSubstitutionFlags.NamespaceExports) && isTransformedModuleDeclaration(
              node))) {
        (applicableSubstitutions |= TypeScriptSubstitutionFlags.NamespaceExports)

      }
      if (((enabledSubstitutions & TypeScriptSubstitutionFlags.NonQualifiedEnumMembers) && isTransformedEnumDeclaration(
              node))) {
        (applicableSubstitutions |= TypeScriptSubstitutionFlags.NonQualifiedEnumMembers)

      }
      previousOnEmitNode(emitContext, node, emitCallback)
      (applicableSubstitutions = savedApplicableSubstitutions)

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
      if ((enabledSubstitutions & TypeScriptSubstitutionFlags.NamespaceExports)) {
        val name = node.name
        val exportedName = trySubstituteNamespaceExportedName(name)
        if (exportedName) {
          if (node.objectAssignmentInitializer) {
            val initializer =
              createAssignment(exportedName, node.objectAssignmentInitializer)
            return createPropertyAssignment(name, initializer, node)

          }
          return createPropertyAssignment(name, exportedName, node)

        }

      }
      return node

    }
    def substituteExpression(node: Expression) = {
      node.kind match {
        case SyntaxKind.Identifier =>
          return substituteExpressionIdentifier(node.asInstanceOf[Identifier])
        case SyntaxKind.PropertyAccessExpression =>
          return substitutePropertyAccessExpression(
              node.asInstanceOf[PropertyAccessExpression])
        case SyntaxKind.ElementAccessExpression =>
          return substituteElementAccessExpression(
              node.asInstanceOf[ElementAccessExpression])
        case _ =>
      }
      return node

    }
    def substituteExpressionIdentifier(node: Identifier): Expression = {
      return ((trySubstituteClassAlias(node) || trySubstituteNamespaceExportedName(
          node)) || node)

    }
    def trySubstituteClassAlias(node: Identifier): Expression = {
      if ((enabledSubstitutions & TypeScriptSubstitutionFlags.ClassAliases)) {
        if ((resolver
              .getNodeCheckFlags(node) & NodeCheckFlags.ConstructorReferenceInClass)) {
          val declaration = resolver.getReferencedValueDeclaration(node)
          if (declaration) {
            val classAlias = classAliases(declaration.id)
            if (classAlias) {
              val clone = getSynthesizedClone(classAlias)
              setSourceMapRange(clone, node)
              setCommentRange(clone, node)
              return clone

            }

          }

        }

      }
      return undefined

    }
    def trySubstituteNamespaceExportedName(node: Identifier): Expression = {
      if (((enabledSubstitutions & applicableSubstitutions) && (((getEmitFlags(
              node) & EmitFlags.LocalName)) === 0))) {
        val container = resolver.getReferencedExportContainer(node, false)
        if (container) {
          val substitute = ((((applicableSubstitutions & TypeScriptSubstitutionFlags.NamespaceExports) && (container.kind === SyntaxKind.ModuleDeclaration))) || (((applicableSubstitutions & TypeScriptSubstitutionFlags.NonQualifiedEnumMembers) && (container.kind === SyntaxKind.EnumDeclaration))))
          if (substitute) {
            return createPropertyAccess(getGeneratedNameForNode(container),
                node, node)

          }

        }

      }
      return undefined

    }
    def substitutePropertyAccessExpression(node: PropertyAccessExpression) = {
      return substituteConstantValue(node)

    }
    def substituteElementAccessExpression(node: ElementAccessExpression) = {
      return substituteConstantValue(node)

    }
    def substituteConstantValue(
        node: (PropertyAccessExpression | ElementAccessExpression)): LeftHandSideExpression = {
      val constantValue = tryGetConstEnumValue(node)
      if ((constantValue !== undefined)) {
        val substitute = createLiteral(constantValue)
        setSourceMapRange(substitute, node)
        setCommentRange(substitute, node)
        if ((!compilerOptions.removeComments)) {
          val propertyName =
            (if (isPropertyAccessExpression(node))
               declarationNameToString(node.name)
             else getTextOfNode(node.argumentExpression))
          (substitute.trailingComment = s""" ${propertyName} """)

        }
        setConstantValue(node, constantValue)
        return substitute

      }
      return node

    }
    def tryGetConstEnumValue(node: Node): Int = {
      if (compilerOptions.isolatedModules) {
        return undefined

      }
      return (if ((isPropertyAccessExpression(node) || isElementAccessExpression(
                      node)))
                resolver.getConstantValue(
                    node.asInstanceOf[
                        (PropertyAccessExpression | ElementAccessExpression)])
              else undefined)

    }

  }
}
