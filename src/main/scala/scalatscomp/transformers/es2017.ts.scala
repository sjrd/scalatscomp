package scalatscomp.transformers
object ES2017 {
  type SuperContainer =
    (ClassDeclaration | MethodDeclaration | GetAccessorDeclaration | SetAccessorDeclaration | ConstructorDeclaration)
  def transformES2017(context: TransformationContext) = {
    sealed abstract class ES2017SubstitutionFlags
    object ES2017SubstitutionFlags {
      case object AsyncMethodsWithSuper extends ES2017SubstitutionFlags
    }
    const fresh1 = context
    val startLexicalEnvironment = fresh1.startLexicalEnvironment
    val endLexicalEnvironment = fresh1.endLexicalEnvironment
    val resolver = context.getEmitResolver()
    val compilerOptions = context.getCompilerOptions()
    val languageVersion = getEmitScriptTarget(compilerOptions)
    var currentSourceFileExternalHelpersModuleName: Identifier = zeroOfMyType
    var enabledSubstitutions: ES2017SubstitutionFlags = zeroOfMyType
    var applicableSubstitutions: ES2017SubstitutionFlags = zeroOfMyType
    var currentSuperContainer: SuperContainer = zeroOfMyType
    val previousOnEmitNode = context.onEmitNode
    val previousOnSubstituteNode = context.onSubstituteNode
    (context.onEmitNode = onEmitNode)
    (context.onSubstituteNode = onSubstituteNode)
    var currentScope: (SourceFile | Block | ModuleBlock | CaseBlock) =
      zeroOfMyType
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      if (isDeclarationFile(node)) {
        return node

      }
      (currentSourceFileExternalHelpersModuleName =
        node.externalHelpersModuleName)
      return visitEachChild(node, visitor, context)

    }
    def visitor(node: Node): VisitResult[Node] = {
      if ((node.transformFlags & TransformFlags.ES2017)) {
        return visitorWorker(node)

      } else if ((node.transformFlags & TransformFlags.ContainsES2017)) {
        return visitEachChild(node, visitor, context)

      }
      return node

    }
    def visitorWorker(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.AsyncKeyword =>
          return undefined
        case SyntaxKind.AwaitExpression =>
          return visitAwaitExpression(node.asInstanceOf[AwaitExpression])
        case SyntaxKind.MethodDeclaration =>
          return visitMethodDeclaration(node.asInstanceOf[MethodDeclaration])
        case SyntaxKind.FunctionDeclaration =>
          return visitFunctionDeclaration(
            node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.FunctionExpression =>
          return visitFunctionExpression(node.asInstanceOf[FunctionExpression])
        case SyntaxKind.ArrowFunction =>
          return visitArrowFunction(node.asInstanceOf[ArrowFunction])
        case _ =>
          Debug.failBadSyntaxKind(node)
          return node
      }

    }
    def visitAwaitExpression(node: AwaitExpression): Expression = {
      return setOriginalNode(
        createYield(
          undefined,
          visitNode(node.expression, visitor, isExpression),
          node),
        node)

    }
    def visitMethodDeclaration(node: MethodDeclaration) = {
      if ((!isAsyncFunctionLike(node))) {
        return node

      }
      val method = createMethod(
        undefined,
        visitNodes(node.modifiers, visitor, isModifier),
        node.asteriskToken,
        node.name,
        undefined,
        visitNodes(node.parameters, visitor, isParameter),
        undefined,
        transformFunctionBody(node),
        node)
      setCommentRange(method, node)
      setSourceMapRange(method, moveRangePastDecorators(node))
      setOriginalNode(method, node)
      return method

    }
    def visitFunctionDeclaration(
        node: FunctionDeclaration): VisitResult[Statement] = {
      if ((!isAsyncFunctionLike(node))) {
        return node

      }
      val func = createFunctionDeclaration(
        undefined,
        visitNodes(node.modifiers, visitor, isModifier),
        node.asteriskToken,
        node.name,
        undefined,
        visitNodes(node.parameters, visitor, isParameter),
        undefined,
        transformFunctionBody(node),
        node)
      setOriginalNode(func, node)
      return func

    }
    def visitFunctionExpression(node: FunctionExpression): Expression = {
      if ((!isAsyncFunctionLike(node))) {
        return node

      }
      if (nodeIsMissing(node.body)) {
        return createOmittedExpression()

      }
      val func =
        createFunctionExpression(
          undefined,
          node.asteriskToken,
          node.name,
          undefined,
          visitNodes(node.parameters, visitor, isParameter),
          undefined,
          transformFunctionBody(node),
          node)
      setOriginalNode(func, node)
      return func

    }
    def visitArrowFunction(node: ArrowFunction) = {
      if ((!isAsyncFunctionLike(node))) {
        return node

      }
      val func =
        createArrowFunction(
          visitNodes(node.modifiers, visitor, isModifier),
          undefined,
          visitNodes(node.parameters, visitor, isParameter),
          undefined,
          node.equalsGreaterThanToken,
          transformConciseBody(node),
          node)
      setOriginalNode(func, node)
      return func

    }
    def transformFunctionBody(
        node: (MethodDeclaration | AccessorDeclaration | FunctionDeclaration | FunctionExpression))
      : FunctionBody = {
      return transformAsyncFunctionBody(node).asInstanceOf[FunctionBody]

    }
    def transformConciseBody(node: ArrowFunction): ConciseBody = {
      return transformAsyncFunctionBody(node)

    }
    def transformFunctionBodyWorker(body: Block, start: Nothing = 0) = {
      val savedCurrentScope = currentScope
      (currentScope = body)
      startLexicalEnvironment()
      val statements = visitNodes(body.statements, visitor, isStatement, start)
      val visited = updateBlock(body, statements)
      val declarations = endLexicalEnvironment()
      (currentScope = savedCurrentScope)
      return mergeFunctionBodyLexicalEnvironment(visited, declarations)

    }
    def transformAsyncFunctionBody(
        node: FunctionLikeDeclaration): (ConciseBody | FunctionBody) = {
      val nodeType =
        (if (node.original)(node.original
           .asInstanceOf[FunctionLikeDeclaration])
           .`type`
         else node.`type`)
      val promiseConstructor =
        (if ((languageVersion < ScriptTarget.ES2015))
           getPromiseConstructor(nodeType)
         else undefined)
      val isArrowFunction = (node.kind === SyntaxKind.ArrowFunction)
      val hasLexicalArguments = (((resolver.getNodeCheckFlags(node) & NodeCheckFlags.CaptureArguments)) !== 0)
      if ((!isArrowFunction)) {
        val statements: Array[Statement] = Array()
        val statementOffset = addPrologueDirectives(
          statements,
          (node.body.asInstanceOf[Block]).statements,
          false,
          visitor)
        statements.push(
          createReturn(
            createAwaiterHelper(
              currentSourceFileExternalHelpersModuleName,
              hasLexicalArguments,
              promiseConstructor,
              transformFunctionBodyWorker(
                node.body.asInstanceOf[Block],
                statementOffset))))
        val block = createBlock(statements, node.body, true)
        if ((languageVersion >= ScriptTarget.ES2015)) {
          if ((resolver.getNodeCheckFlags(node) & NodeCheckFlags.AsyncMethodWithSuperBinding)) {
            enableSubstitutionForAsyncMethodsWithSuper()
            setEmitFlags(block, EmitFlags.EmitAdvancedSuperHelper)

          } else if ((resolver.getNodeCheckFlags(node) & NodeCheckFlags.AsyncMethodWithSuper)) {
            enableSubstitutionForAsyncMethodsWithSuper()
            setEmitFlags(block, EmitFlags.EmitSuperHelper)

          }

        }
        return block

      } else {
        return createAwaiterHelper(
          currentSourceFileExternalHelpersModuleName,
          hasLexicalArguments,
          promiseConstructor,
          transformConciseBodyWorker(node.body, true).asInstanceOf[Block])

      }

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
    def getPromiseConstructor(`type`: TypeNode) = {
      val typeName = getEntityNameFromTypeNode(`type`)
      if ((typeName && isEntityName(typeName))) {
        val serializationKind =
          resolver.getTypeReferenceSerializationKind(typeName)
        if (((serializationKind === TypeReferenceSerializationKind.TypeWithConstructSignatureAndValue) || (serializationKind === TypeReferenceSerializationKind.Unknown))) {
          return typeName

        }

      }
      return undefined

    }
    def enableSubstitutionForAsyncMethodsWithSuper() = {
      if ((((enabledSubstitutions & ES2017SubstitutionFlags.AsyncMethodsWithSuper)) === 0)) {
        (enabledSubstitutions |= ES2017SubstitutionFlags.AsyncMethodsWithSuper)
        context.enableSubstitution(SyntaxKind.CallExpression)
        context.enableSubstitution(SyntaxKind.PropertyAccessExpression)
        context.enableSubstitution(SyntaxKind.ElementAccessExpression)
        context.enableEmitNotification(SyntaxKind.ClassDeclaration)
        context.enableEmitNotification(SyntaxKind.MethodDeclaration)
        context.enableEmitNotification(SyntaxKind.GetAccessor)
        context.enableEmitNotification(SyntaxKind.SetAccessor)
        context.enableEmitNotification(SyntaxKind.Constructor)

      }

    }
    def substituteExpression(node: Expression) = {
      node.kind match {
        case SyntaxKind.PropertyAccessExpression =>
          return substitutePropertyAccessExpression(
            node.asInstanceOf[PropertyAccessExpression])
        case SyntaxKind.ElementAccessExpression =>
          return substituteElementAccessExpression(
            node.asInstanceOf[ElementAccessExpression])
        case SyntaxKind.CallExpression =>
          if ((enabledSubstitutions & ES2017SubstitutionFlags.AsyncMethodsWithSuper)) {
            return substituteCallExpression(node.asInstanceOf[CallExpression])

          }
        case _ =>
      }
      return node

    }
    def substitutePropertyAccessExpression(node: PropertyAccessExpression) = {
      if (((enabledSubstitutions & ES2017SubstitutionFlags.AsyncMethodsWithSuper) && (node.expression.kind === SyntaxKind.SuperKeyword))) {
        val flags = getSuperContainerAsyncMethodFlags()
        if (flags) {
          return createSuperAccessInAsyncMethod(
            createLiteral(node.name.text),
            flags,
            node)

        }

      }
      return node

    }
    def substituteElementAccessExpression(node: ElementAccessExpression) = {
      if (((enabledSubstitutions & ES2017SubstitutionFlags.AsyncMethodsWithSuper) && (node.expression.kind === SyntaxKind.SuperKeyword))) {
        val flags = getSuperContainerAsyncMethodFlags()
        if (flags) {
          return createSuperAccessInAsyncMethod(
            node.argumentExpression,
            flags,
            node)

        }

      }
      return node

    }
    def substituteCallExpression(node: CallExpression): Expression = {
      val expression = node.expression
      if (isSuperProperty(expression)) {
        val flags = getSuperContainerAsyncMethodFlags()
        if (flags) {
          val argumentExpression =
            (if (isPropertyAccessExpression(expression))
               substitutePropertyAccessExpression(expression)
             else substituteElementAccessExpression(expression))
          return createCall(
            createPropertyAccess(argumentExpression, "call"),
            undefined,
            Array(createThis(), node.arguments: _*))

        }

      }
      return node

    }
    def isSuperContainer(node: Node): Boolean = {
      val kind = node.kind
      return (((((kind === SyntaxKind.ClassDeclaration) || (kind === SyntaxKind.Constructor)) || (kind === SyntaxKind.MethodDeclaration)) || (kind === SyntaxKind.GetAccessor)) || (kind === SyntaxKind.SetAccessor))

    }
    def onEmitNode(emitContext: EmitContext,
                   node: Node,
                   emitCallback: ((EmitContext, Node) => Unit)): Unit = {
      val savedApplicableSubstitutions = applicableSubstitutions
      val savedCurrentSuperContainer = currentSuperContainer
      if (((enabledSubstitutions & ES2017SubstitutionFlags.AsyncMethodsWithSuper) && isSuperContainer(
            node))) {
        (currentSuperContainer = node)

      }
      previousOnEmitNode(emitContext, node, emitCallback)
      (applicableSubstitutions = savedApplicableSubstitutions)
      (currentSuperContainer = savedCurrentSuperContainer)

    }
    def onSubstituteNode(emitContext: EmitContext, node: Node) = {
      (node = previousOnSubstituteNode(emitContext, node))
      if ((emitContext === EmitContext.Expression)) {
        return substituteExpression(node.asInstanceOf[Expression])

      }
      return node

    }
    def createSuperAccessInAsyncMethod(
        argumentExpression: Expression,
        flags: NodeCheckFlags,
        location: TextRange): LeftHandSideExpression = {
      if ((flags & NodeCheckFlags.AsyncMethodWithSuperBinding)) {
        return createPropertyAccess(
          createCall(
            createIdentifier("_super"),
            undefined,
            Array(argumentExpression)),
          "value",
          location)

      } else {
        return createCall(
          createIdentifier("_super"),
          undefined,
          Array(argumentExpression),
          location)

      }

    }
    def getSuperContainerAsyncMethodFlags() = {
      return ((currentSuperContainer !== undefined) && (resolver.getNodeCheckFlags(
        currentSuperContainer) & ((NodeCheckFlags.AsyncMethodWithSuper | NodeCheckFlags.AsyncMethodWithSuperBinding))))

    }

  }
}
