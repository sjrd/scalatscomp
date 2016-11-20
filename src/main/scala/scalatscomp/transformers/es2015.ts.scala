package scalatscomp.transformers
object ES2015 {
  sealed abstract class ES2015SubstitutionFlags
  object ES2015SubstitutionFlags {
    case object CapturedThis extends ES2015SubstitutionFlags
    case object BlockScopedBindings extends ES2015SubstitutionFlags
  }
  trait LoopOutParameter {
    var originalName: Identifier
    var outParamName: Identifier
  }
  sealed abstract class CopyDirection
  object CopyDirection {
    case object ToOriginal extends CopyDirection
    case object ToOutParameter extends CopyDirection
  }
  sealed abstract class Jump
  object Jump {
    case object Break extends Jump
    case object Continue extends Jump
    case object Return extends Jump
  }
  trait ConvertedLoopState {
    var labels: Map[String]
    var labeledNonLocalBreaks: Map[String]
    var labeledNonLocalContinues: Map[String]
    var nonLocalJumps: Jump
    var allowedNonLabeledJumps: Jump
    var argumentsName: Identifier
    var thisName: Identifier
    var containsLexicalThis: Boolean
    var hoistedLocalVariables: Array[Identifier]
    var loopOutParameters: Array[LoopOutParameter]
  }
  sealed abstract class SuperCaptureResult
  object SuperCaptureResult {
    case object NoReplacement extends SuperCaptureResult
    case object ReplaceSuperCapture extends SuperCaptureResult
    case object ReplaceWithReturn extends SuperCaptureResult
  }
  def transformES2015(context: TransformationContext) = {
    const fresh1 = context
    val startLexicalEnvironment = fresh1.startLexicalEnvironment
    val endLexicalEnvironment = fresh1.endLexicalEnvironment
    val hoistVariableDeclaration = fresh1.hoistVariableDeclaration
    val resolver = context.getEmitResolver()
    val previousOnSubstituteNode = context.onSubstituteNode
    val previousOnEmitNode = context.onEmitNode
    (context.onEmitNode = onEmitNode)
    (context.onSubstituteNode = onSubstituteNode)
    var currentSourceFile: SourceFile = zeroOfMyType
    var currentText: String = zeroOfMyType
    var currentParent: Node = zeroOfMyType
    var currentNode: Node = zeroOfMyType
    var enclosingVariableStatement: VariableStatement = zeroOfMyType
    var enclosingBlockScopeContainer: Node = zeroOfMyType
    var enclosingBlockScopeContainerParent: Node = zeroOfMyType
    var enclosingFunction: FunctionLikeDeclaration = zeroOfMyType
    var enclosingNonArrowFunction: FunctionLikeDeclaration = zeroOfMyType
    var enclosingNonAsyncFunctionBody: (FunctionLikeDeclaration | ClassElement) =
      zeroOfMyType
    var isInConstructorWithCapturedSuper: Boolean = zeroOfMyType
    var convertedLoopState: ConvertedLoopState = zeroOfMyType
    var enabledSubstitutions: ES2015SubstitutionFlags = zeroOfMyType
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      if (isDeclarationFile(node)) {
        return node

      }
      (currentSourceFile = node)
      (currentText = node.text)
      return visitNode(node, visitor, isSourceFile)

    }
    def visitor(node: Node): VisitResult[Node] = {
      return saveStateAndInvoke(node, dispatcher)

    }
    def dispatcher(node: Node): VisitResult[Node] = {
      return (if (convertedLoopState) visitorForConvertedLoopWorker(node)
              else visitorWorker(node))

    }
    def saveStateAndInvoke[T <: Node, U](node: T, f: ((T) => U)): U = {
      val savedEnclosingFunction = enclosingFunction
      val savedEnclosingNonArrowFunction = enclosingNonArrowFunction
      val savedEnclosingNonAsyncFunctionBody = enclosingNonAsyncFunctionBody
      val savedEnclosingBlockScopeContainer = enclosingBlockScopeContainer
      val savedEnclosingBlockScopeContainerParent =
        enclosingBlockScopeContainerParent
      val savedEnclosingVariableStatement = enclosingVariableStatement
      val savedCurrentParent = currentParent
      val savedCurrentNode = currentNode
      val savedConvertedLoopState = convertedLoopState
      val savedIsInConstructorWithCapturedSuper =
        isInConstructorWithCapturedSuper
      if (nodeStartsNewLexicalEnvironment(node)) {
        (isInConstructorWithCapturedSuper = false)
        (convertedLoopState = undefined)

      }
      onBeforeVisitNode(node)
      val visited = f(node)
      (isInConstructorWithCapturedSuper = savedIsInConstructorWithCapturedSuper)
      (convertedLoopState = savedConvertedLoopState)
      (enclosingFunction = savedEnclosingFunction)
      (enclosingNonArrowFunction = savedEnclosingNonArrowFunction)
      (enclosingNonAsyncFunctionBody = savedEnclosingNonAsyncFunctionBody)
      (enclosingBlockScopeContainer = savedEnclosingBlockScopeContainer)
      (enclosingBlockScopeContainerParent =
        savedEnclosingBlockScopeContainerParent)
      (enclosingVariableStatement = savedEnclosingVariableStatement)
      (currentParent = savedCurrentParent)
      (currentNode = savedCurrentNode)
      return visited

    }
    def returnCapturedThis(node: Node): Node = {
      return setOriginalNode(createReturn(createIdentifier("_this")), node)

    }
    def isReturnVoidStatementInConstructorWithCapturedSuper(
        node: Node): Boolean = {
      return ((isInConstructorWithCapturedSuper && (node.kind === SyntaxKind.ReturnStatement)) && (!(node
        .asInstanceOf[ReturnStatement])
        .expression))

    }
    def shouldCheckNode(node: Node): Boolean = {
      return (((((node.transformFlags & TransformFlags.ES2015)) !== 0) || (node.kind === SyntaxKind.LabeledStatement)) || ((isIterationStatement(
        node,
        false) && shouldConvertIterationStatementBody(node))))

    }
    def visitorWorker(node: Node): VisitResult[Node] = {
      if (isReturnVoidStatementInConstructorWithCapturedSuper(node)) {
        return returnCapturedThis(node.asInstanceOf[ReturnStatement])

      } else if (shouldCheckNode(node)) {
        return visitJavaScript(node)

      } else if (((node.transformFlags & TransformFlags.ContainsES2015) || ((isInConstructorWithCapturedSuper && (!isExpression(
                   node)))))) {
        return visitEachChild(node, visitor, context)

      } else {
        return node

      }

    }
    def visitorForConvertedLoopWorker(node: Node): VisitResult[Node] = {
      var result: VisitResult[Node] = zeroOfMyType
      if (shouldCheckNode(node)) {
        (result = visitJavaScript(node))

      } else {
        (result = visitNodesInConvertedLoop(node))

      }
      return result

    }
    def visitNodesInConvertedLoop(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.ReturnStatement =>
          (node =
            (if (isReturnVoidStatementInConstructorWithCapturedSuper(node))
               returnCapturedThis(node)
             else node))
          return visitReturnStatement(node.asInstanceOf[ReturnStatement])
        case SyntaxKind.VariableStatement =>
          return visitVariableStatement(node.asInstanceOf[VariableStatement])
        case SyntaxKind.SwitchStatement =>
          return visitSwitchStatement(node.asInstanceOf[SwitchStatement])
        case SyntaxKind.BreakStatement | SyntaxKind.ContinueStatement =>
          return visitBreakOrContinueStatement(
            node.asInstanceOf[BreakOrContinueStatement])
        case SyntaxKind.ThisKeyword =>
          return visitThisKeyword(node)
        case SyntaxKind.Identifier =>
          return visitIdentifier(node.asInstanceOf[Identifier])
        case _ =>
          return visitEachChild(node, visitor, context)
      }

    }
    def visitJavaScript(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.ExportKeyword =>
          return node
        case SyntaxKind.ClassDeclaration =>
          return visitClassDeclaration(node.asInstanceOf[ClassDeclaration])
        case SyntaxKind.ClassExpression =>
          return visitClassExpression(node.asInstanceOf[ClassExpression])
        case SyntaxKind.Parameter =>
          return visitParameter(node.asInstanceOf[ParameterDeclaration])
        case SyntaxKind.FunctionDeclaration =>
          return visitFunctionDeclaration(
            node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.ArrowFunction =>
          return visitArrowFunction(node.asInstanceOf[ArrowFunction])
        case SyntaxKind.FunctionExpression =>
          return visitFunctionExpression(node.asInstanceOf[FunctionExpression])
        case SyntaxKind.VariableDeclaration =>
          return visitVariableDeclaration(
            node.asInstanceOf[VariableDeclaration])
        case SyntaxKind.Identifier =>
          return visitIdentifier(node.asInstanceOf[Identifier])
        case SyntaxKind.VariableDeclarationList =>
          return visitVariableDeclarationList(
            node.asInstanceOf[VariableDeclarationList])
        case SyntaxKind.LabeledStatement =>
          return visitLabeledStatement(node.asInstanceOf[LabeledStatement])
        case SyntaxKind.DoStatement =>
          return visitDoStatement(node.asInstanceOf[DoStatement])
        case SyntaxKind.WhileStatement =>
          return visitWhileStatement(node.asInstanceOf[WhileStatement])
        case SyntaxKind.ForStatement =>
          return visitForStatement(node.asInstanceOf[ForStatement])
        case SyntaxKind.ForInStatement =>
          return visitForInStatement(node.asInstanceOf[ForInStatement])
        case SyntaxKind.ForOfStatement =>
          return visitForOfStatement(node.asInstanceOf[ForOfStatement])
        case SyntaxKind.ExpressionStatement =>
          return visitExpressionStatement(
            node.asInstanceOf[ExpressionStatement])
        case SyntaxKind.ObjectLiteralExpression =>
          return visitObjectLiteralExpression(
            node.asInstanceOf[ObjectLiteralExpression])
        case SyntaxKind.CatchClause =>
          return visitCatchClause(node.asInstanceOf[CatchClause])
        case SyntaxKind.ShorthandPropertyAssignment =>
          return visitShorthandPropertyAssignment(
            node.asInstanceOf[ShorthandPropertyAssignment])
        case SyntaxKind.ArrayLiteralExpression =>
          return visitArrayLiteralExpression(
            node.asInstanceOf[ArrayLiteralExpression])
        case SyntaxKind.CallExpression =>
          return visitCallExpression(node.asInstanceOf[CallExpression])
        case SyntaxKind.NewExpression =>
          return visitNewExpression(node.asInstanceOf[NewExpression])
        case SyntaxKind.ParenthesizedExpression =>
          return visitParenthesizedExpression(
            node.asInstanceOf[ParenthesizedExpression],
            true)
        case SyntaxKind.BinaryExpression =>
          return visitBinaryExpression(
            node.asInstanceOf[BinaryExpression],
            true)
        case SyntaxKind.NoSubstitutionTemplateLiteral |
            SyntaxKind.TemplateHead | SyntaxKind.TemplateMiddle |
            SyntaxKind.TemplateTail =>
          return visitTemplateLiteral(node.asInstanceOf[LiteralExpression])
        case SyntaxKind.TaggedTemplateExpression =>
          return visitTaggedTemplateExpression(
            node.asInstanceOf[TaggedTemplateExpression])
        case SyntaxKind.TemplateExpression =>
          return visitTemplateExpression(node.asInstanceOf[TemplateExpression])
        case SyntaxKind.YieldExpression =>
          return visitYieldExpression(node.asInstanceOf[YieldExpression])
        case SyntaxKind.SuperKeyword =>
          return visitSuperKeyword()
        case SyntaxKind.YieldExpression =>
          return visitEachChild(node, visitor, context)
        case SyntaxKind.MethodDeclaration =>
          return visitMethodDeclaration(node.asInstanceOf[MethodDeclaration])
        case SyntaxKind.SourceFile =>
          return visitSourceFileNode(node.asInstanceOf[SourceFile])
        case SyntaxKind.VariableStatement =>
          return visitVariableStatement(node.asInstanceOf[VariableStatement])
        case _ =>
          Debug.failBadSyntaxKind(node)
          return visitEachChild(node, visitor, context)
      }

    }
    def onBeforeVisitNode(node: Node) = {
      if (currentNode) {
        if (isBlockScope(currentNode, currentParent)) {
          (enclosingBlockScopeContainer = currentNode)
          (enclosingBlockScopeContainerParent = currentParent)

        }
        if (isFunctionLike(currentNode)) {
          (enclosingFunction = currentNode)
          if ((currentNode.kind !== SyntaxKind.ArrowFunction)) {
            (enclosingNonArrowFunction = currentNode)
            if ((!((getEmitFlags(currentNode) & EmitFlags.AsyncFunctionBody)))) {
              (enclosingNonAsyncFunctionBody = currentNode)

            }

          }

        }
        currentNode.kind match {
          case SyntaxKind.VariableStatement =>
            (enclosingVariableStatement =
              currentNode.asInstanceOf[VariableStatement])
          case SyntaxKind.VariableDeclarationList |
              SyntaxKind.VariableDeclaration | SyntaxKind.BindingElement |
              SyntaxKind.ObjectBindingPattern |
              SyntaxKind.ArrayBindingPattern =>
          case _ =>
            (enclosingVariableStatement = undefined)
        }

      }
      (currentParent = currentNode)
      (currentNode = node)

    }
    def visitSwitchStatement(node: SwitchStatement): SwitchStatement = {
      Debug.assert((convertedLoopState !== undefined))
      val savedAllowedNonLabeledJumps =
        convertedLoopState.allowedNonLabeledJumps
      (convertedLoopState.allowedNonLabeledJumps |= Jump.Break)
      val result = visitEachChild(node, visitor, context)
      (convertedLoopState.allowedNonLabeledJumps = savedAllowedNonLabeledJumps)
      return result

    }
    def visitReturnStatement(node: ReturnStatement): Statement = {
      Debug.assert((convertedLoopState !== undefined))
      (convertedLoopState.nonLocalJumps |= Jump.Return)
      return createReturn(
        createObjectLiteral(
          Array(
            createPropertyAssignment(
              createIdentifier("value"),
              (if (node.expression)
                 visitNode(node.expression, visitor, isExpression)
               else createVoidZero())))))

    }
    def visitThisKeyword(node: Node): Node = {
      Debug.assert((convertedLoopState !== undefined))
      if ((enclosingFunction && (enclosingFunction.kind === SyntaxKind.ArrowFunction))) {
        (convertedLoopState.containsLexicalThis = true)
        return node

      }
      return (convertedLoopState.thisName || ((convertedLoopState.thisName =
        createUniqueName("this"))))

    }
    def visitIdentifier(node: Identifier): Identifier = {
      if ((!convertedLoopState)) {
        return node

      }
      if (isGeneratedIdentifier(node)) {
        return node

      }
      if (((node.text !== "arguments") && (!resolver.isArgumentsLocalBinding(
            node)))) {
        return node

      }
      return (convertedLoopState.argumentsName || ((convertedLoopState.argumentsName =
        createUniqueName("arguments"))))

    }
    def visitBreakOrContinueStatement(
        node: BreakOrContinueStatement): Statement = {
      if (convertedLoopState) {
        val jump =
          (if ((node.kind === SyntaxKind.BreakStatement)) Jump.Break
           else Jump.Continue)
        val canUseBreakOrContinue = ((((node.label && convertedLoopState.labels) && convertedLoopState
            .labels(node.label.text))) || (((!node.label) && ((convertedLoopState.allowedNonLabeledJumps & jump)))))
        if ((!canUseBreakOrContinue)) {
          var labelMarker: String = zeroOfMyType
          if ((!node.label)) {
            if ((node.kind === SyntaxKind.BreakStatement)) {
              (convertedLoopState.nonLocalJumps |= Jump.Break)
              (labelMarker = "break")

            } else {
              (convertedLoopState.nonLocalJumps |= Jump.Continue)
              (labelMarker = "continue")

            }

          } else {
            if ((node.kind === SyntaxKind.BreakStatement)) {
              (labelMarker = s"""break-${node.label.text}""")
              setLabeledJump(
                convertedLoopState,
                true,
                node.label.text,
                labelMarker)

            } else {
              (labelMarker = s"""continue-${node.label.text}""")
              setLabeledJump(
                convertedLoopState,
                false,
                node.label.text,
                labelMarker)

            }

          }
          var returnExpression: Expression = createLiteral(labelMarker)
          if (convertedLoopState.loopOutParameters.length) {
            val outParams = convertedLoopState.loopOutParameters
            var expr: Expression = zeroOfMyType {
              var i = 0
              while ((i < outParams.length)) {
                {
                  val copyExpr = copyOutParameter(
                    outParams(i),
                    CopyDirection.ToOutParameter)
                  if ((i === 0)) {
                    (expr = copyExpr)

                  } else {
                    (expr = createBinary(expr, SyntaxKind.CommaToken, copyExpr))

                  }

                }
                (i += 1)
              }
            }
            (returnExpression =
              createBinary(expr, SyntaxKind.CommaToken, returnExpression))

          }
          return createReturn(returnExpression)

        }

      }
      return visitEachChild(node, visitor, context)

    }
    def visitClassDeclaration(node: ClassDeclaration): VisitResult[Statement] = {
      val modifierFlags = getModifierFlags(node)
      val isExported = (modifierFlags & ModifierFlags.Export)
      val isDefault = (modifierFlags & ModifierFlags.Default)
      val modifiers =
        (if ((isExported && (!isDefault)))
           filter(node.modifiers, isExportModifier)
         else undefined)
      val statement = createVariableStatement(
        modifiers,
        createVariableDeclarationList(
          Array(
            createVariableDeclaration(
              getDeclarationName(node, true),
              undefined,
              transformClassLikeDeclarationToExpression(node)))),
        node)
      setOriginalNode(statement, node)
      startOnNewLine(statement)
      if ((isExported && isDefault)) {
        val statements: Array[Statement] = Array(statement)
        statements.push(
          createExportAssignment(
            undefined,
            undefined,
            false,
            getDeclarationName(node, false)))
        return statements

      }
      return statement

    }
    def isExportModifier(node: Modifier) = {
      return (node.kind === SyntaxKind.ExportKeyword)

    }
    def visitClassExpression(node: ClassExpression): Expression = {
      return transformClassLikeDeclarationToExpression(node)

    }
    def transformClassLikeDeclarationToExpression(
        node: (ClassExpression | ClassDeclaration)): Expression = {
      if (node.name) {
        enableSubstitutionsForBlockScopedBindings()

      }
      val extendsClauseElement = getClassExtendsHeritageClauseElement(node)
      val classFunction = createFunctionExpression(
        undefined,
        undefined,
        undefined,
        undefined,
        (if (extendsClauseElement) Array(createParameter("_super"))
         else Array()),
        undefined,
        transformClassBody(node, extendsClauseElement))
      if ((getEmitFlags(node) & EmitFlags.Indented)) {
        setEmitFlags(classFunction, EmitFlags.Indented)

      }
      val inner = createPartiallyEmittedExpression(classFunction)
      (inner.end = node.end)
      setEmitFlags(inner, EmitFlags.NoComments)
      val outer = createPartiallyEmittedExpression(inner)
      (outer.end = skipTrivia(currentText, node.pos))
      setEmitFlags(outer, EmitFlags.NoComments)
      return createParen(
        createCall(
          outer,
          undefined,
          (if (extendsClauseElement)
             Array(
               visitNode(
                 extendsClauseElement.expression,
                 visitor,
                 isExpression))
           else Array())))

    }
    def transformClassBody(
        node: (ClassExpression | ClassDeclaration),
        extendsClauseElement: ExpressionWithTypeArguments): Block = {
      val statements: Array[Statement] = Array()
      startLexicalEnvironment()
      addExtendsHelperIfNeeded(statements, node, extendsClauseElement)
      addConstructor(statements, node, extendsClauseElement)
      addClassMembers(statements, node)
      val closingBraceLocation = createTokenRange(
        skipTrivia(currentText, node.members.end),
        SyntaxKind.CloseBraceToken)
      val localName = getLocalName(node)
      val outer = createPartiallyEmittedExpression(localName)
      (outer.end = closingBraceLocation.end)
      setEmitFlags(outer, EmitFlags.NoComments)
      val statement = createReturn(outer)
      (statement.pos = closingBraceLocation.pos)
      setEmitFlags(
        statement,
        (EmitFlags.NoComments | EmitFlags.NoTokenSourceMaps))
      statements.push(statement)
      addRange(statements, endLexicalEnvironment())
      val block =
        createBlock(createNodeArray(statements, node.members), undefined, true)
      setEmitFlags(block, EmitFlags.NoComments)
      return block

    }
    def addExtendsHelperIfNeeded(
        statements: Array[Statement],
        node: (ClassExpression | ClassDeclaration),
        extendsClauseElement: ExpressionWithTypeArguments): Unit = {
      if (extendsClauseElement) {
        statements.push(
          createStatement(
            createExtendsHelper(
              currentSourceFile.externalHelpersModuleName,
              getDeclarationName(node)),
            extendsClauseElement))

      }

    }
    def addConstructor(
        statements: Array[Statement],
        node: (ClassExpression | ClassDeclaration),
        extendsClauseElement: ExpressionWithTypeArguments): Unit = {
      val `constructor` = getFirstConstructorWithBody(node)
      val hasSynthesizedSuper = hasSynthesizedDefaultSuperCall(
        `constructor`,
        (extendsClauseElement !== undefined))
      val constructorFunction = createFunctionDeclaration(
        undefined,
        undefined,
        undefined,
        getDeclarationName(node),
        undefined,
        transformConstructorParameters(`constructor`, hasSynthesizedSuper),
        undefined,
        transformConstructorBody(
          `constructor`,
          node,
          extendsClauseElement,
          hasSynthesizedSuper),
        (`constructor` || node))
      if (extendsClauseElement) {
        setEmitFlags(constructorFunction, EmitFlags.CapturesThis)

      }
      statements.push(constructorFunction)

    }
    def transformConstructorParameters(
        `constructor`: ConstructorDeclaration,
        hasSynthesizedSuper: Boolean): Array[ParameterDeclaration] = {
      if ((`constructor` && (!hasSynthesizedSuper))) {
        return visitNodes(`constructor`.parameters, visitor, isParameter)

      }
      return Array()

    }
    def transformConstructorBody(
        `constructor`: (ConstructorDeclaration | undefined),
        node: (ClassDeclaration | ClassExpression),
        extendsClauseElement: ExpressionWithTypeArguments,
        hasSynthesizedSuper: Boolean) = {
      val statements: Array[Statement] = Array()
      startLexicalEnvironment()
      var statementOffset = (-1)
      if (hasSynthesizedSuper) {
        (statementOffset = 1)

      } else if (`constructor`) {
        (statementOffset = addPrologueDirectives(
          statements,
          `constructor`.body.statements,
          false,
          visitor))

      }
      if (`constructor`) {
        addDefaultValueAssignmentsIfNeeded(statements, `constructor`)
        addRestParameterIfNeeded(
          statements,
          `constructor`,
          hasSynthesizedSuper)
        Debug.assert(
          (statementOffset >= 0),
          "statementOffset not initialized correctly!")

      }
      val superCaptureStatus =
        declareOrCaptureOrReturnThisForConstructorIfNeeded(
          statements,
          `constructor`,
          (!(!extendsClauseElement)),
          hasSynthesizedSuper,
          statementOffset)
      if (((superCaptureStatus === SuperCaptureResult.ReplaceSuperCapture) || (superCaptureStatus === SuperCaptureResult.ReplaceWithReturn))) {
        (statementOffset += 1)

      }
      if (`constructor`) {
        val body = saveStateAndInvoke(
          `constructor`,
          (`constructor` => {
             (isInConstructorWithCapturedSuper = (superCaptureStatus === SuperCaptureResult.ReplaceSuperCapture))
             return visitNodes(
               `constructor`.body.statements,
               visitor,
               isStatement,
               statementOffset)

           }))
        addRange(statements, body)

      }
      if (((extendsClauseElement && (superCaptureStatus !== SuperCaptureResult.ReplaceWithReturn)) && (!((`constructor` && isSufficientlyCoveredByReturnStatements(
            `constructor`.body)))))) {
        statements.push(createReturn(createIdentifier("_this")))

      }
      addRange(statements, endLexicalEnvironment())
      val block = createBlock(
        createNodeArray(
          statements,
          (if (`constructor`) `constructor`.body.statements
           else node.members)),
        (if (`constructor`) `constructor`.body else node),
        true)
      if ((!`constructor`)) {
        setEmitFlags(block, EmitFlags.NoComments)

      }
      return block

    }
    def isSufficientlyCoveredByReturnStatements(
        statement: Statement): Boolean = {
      if ((statement.kind === SyntaxKind.ReturnStatement)) {
        return true

      } else if ((statement.kind === SyntaxKind.IfStatement)) {
        val ifStatement = statement.asInstanceOf[IfStatement]
        if (ifStatement.elseStatement) {
          return (isSufficientlyCoveredByReturnStatements(
            ifStatement.thenStatement) && isSufficientlyCoveredByReturnStatements(
            ifStatement.elseStatement))

        }

      } else if ((statement.kind === SyntaxKind.Block)) {
        val lastStatement = lastOrUndefined(
          (statement.asInstanceOf[Block]).statements)
        if ((lastStatement && isSufficientlyCoveredByReturnStatements(
              lastStatement))) {
          return true

        }

      }
      return false

    }
    def declareOrCaptureOrReturnThisForConstructorIfNeeded(
        statements: Array[Statement],
        ctor: (ConstructorDeclaration | undefined),
        hasExtendsClause: Boolean,
        hasSynthesizedSuper: Boolean,
        statementOffset: Int) = {
      if ((!hasExtendsClause)) {
        if (ctor) {
          addCaptureThisForNodeIfNeeded(statements, ctor)

        }
        return SuperCaptureResult.NoReplacement

      }
      if ((!ctor)) {
        statements.push(createReturn(createDefaultSuperCallOrThis()))
        return SuperCaptureResult.ReplaceWithReturn

      }
      if (hasSynthesizedSuper) {
        captureThisForNode(statements, ctor, createDefaultSuperCallOrThis())
        enableSubstitutionsForCapturedThis()
        return SuperCaptureResult.ReplaceSuperCapture

      }
      var firstStatement: Statement = zeroOfMyType
      var superCallExpression: Expression = zeroOfMyType
      val ctorStatements = ctor.body.statements
      if ((statementOffset < ctorStatements.length)) {
        (firstStatement = ctorStatements(statementOffset))
        if (((firstStatement.kind === SyntaxKind.ExpressionStatement) && isSuperCall(
              (firstStatement
                .asInstanceOf[ExpressionStatement])
                .expression))) {
          val superCall = (firstStatement
            .asInstanceOf[ExpressionStatement])
            .expression
            .asInstanceOf[CallExpression]
          (superCallExpression = setOriginalNode(
            saveStateAndInvoke(superCall, visitImmediateSuperCallInBody),
            superCall))

        }

      }
      if ((superCallExpression && (statementOffset === (ctorStatements.length - 1)))) {
        statements.push(createReturn(superCallExpression))
        return SuperCaptureResult.ReplaceWithReturn

      }
      captureThisForNode(statements, ctor, superCallExpression, firstStatement)
      if (superCallExpression) {
        return SuperCaptureResult.ReplaceSuperCapture

      }
      return SuperCaptureResult.NoReplacement

    }
    def createDefaultSuperCallOrThis() = {
      val actualThis = createThis()
      setEmitFlags(actualThis, EmitFlags.NoSubstitution)
      val superCall = createFunctionApply(
        createIdentifier("_super"),
        actualThis,
        createIdentifier("arguments"))
      return createLogicalOr(superCall, actualThis)

    }
    def visitParameter(node: ParameterDeclaration): ParameterDeclaration = {
      if (node.dotDotDotToken) {
        return undefined

      } else if (isBindingPattern(node.name)) {
        return setOriginalNode(
          createParameter(getGeneratedNameForNode(node), undefined, node),
          node)

      } else if (node.initializer) {
        return setOriginalNode(
          createParameter(node.name, undefined, node),
          node)

      } else {
        return node

      }

    }
    def shouldAddDefaultValueAssignments(
        node: FunctionLikeDeclaration): Boolean = {
      return (((node.transformFlags & TransformFlags.ContainsDefaultValueAssignments)) !== 0)

    }
    def addDefaultValueAssignmentsIfNeeded(
        statements: Array[Statement],
        node: FunctionLikeDeclaration): Unit = {
      if ((!shouldAddDefaultValueAssignments(node))) {
        return

      }
      (node.parameters).foreach { fresh2 =>
        val parameter = zeroOfMyType = fresh2 {
          const fresh3 = parameter
          val name = fresh3.name
          val initializer = fresh3.initializer
          val dotDotDotToken = fresh3.dotDotDotToken
          if (dotDotDotToken) {
            continue

          }
          if (isBindingPattern(name)) {
            addDefaultValueAssignmentForBindingPattern(
              statements,
              parameter,
              name,
              initializer)

          } else if (initializer) {
            addDefaultValueAssignmentForInitializer(
              statements,
              parameter,
              name,
              initializer)

          }

        }
      }

    }
    def addDefaultValueAssignmentForBindingPattern(
        statements: Array[Statement],
        parameter: ParameterDeclaration,
        name: BindingPattern,
        initializer: Expression): Unit = {
      val temp = getGeneratedNameForNode(parameter)
      if ((name.elements.length > 0)) {
        statements.push(
          setEmitFlags(
            createVariableStatement(
              undefined,
              createVariableDeclarationList(
                flattenParameterDestructuring(parameter, temp, visitor))),
            EmitFlags.CustomPrologue))

      } else if (initializer) {
        statements.push(
          setEmitFlags(
            createStatement(
              createAssignment(
                temp,
                visitNode(initializer, visitor, isExpression))),
            EmitFlags.CustomPrologue))

      }

    }
    def addDefaultValueAssignmentForInitializer(
        statements: Array[Statement],
        parameter: ParameterDeclaration,
        name: Identifier,
        initializer: Expression): Unit = {
      (initializer = visitNode(initializer, visitor, isExpression))
      val statement = createIf(
        createStrictEquality(getSynthesizedClone(name), createVoidZero()),
        setEmitFlags(
          createBlock(
            Array(
              createStatement(
                createAssignment(
                  setEmitFlags(getMutableClone(name), EmitFlags.NoSourceMap),
                  setEmitFlags(
                    initializer,
                    (EmitFlags.NoSourceMap | getEmitFlags(initializer))),
                  parameter))),
            parameter),
          ((EmitFlags.SingleLine | EmitFlags.NoTrailingSourceMap) | EmitFlags.NoTokenSourceMaps)),
        undefined,
        parameter)
      (statement.startsOnNewLine = true)
      setEmitFlags(
        statement,
        ((EmitFlags.NoTokenSourceMaps | EmitFlags.NoTrailingSourceMap) | EmitFlags.CustomPrologue))
      statements.push(statement)

    }
    def shouldAddRestParameter(node: ParameterDeclaration,
                               inConstructorWithSynthesizedSuper: Boolean) = {
      return (((node && node.dotDotDotToken) && (node.name.kind === SyntaxKind.Identifier)) && (!inConstructorWithSynthesizedSuper))

    }
    def addRestParameterIfNeeded(
        statements: Array[Statement],
        node: FunctionLikeDeclaration,
        inConstructorWithSynthesizedSuper: Boolean): Unit = {
      val parameter = lastOrUndefined(node.parameters)
      if ((!shouldAddRestParameter(
            parameter,
            inConstructorWithSynthesizedSuper))) {
        return

      }
      val declarationName = getMutableClone(
        parameter.name.asInstanceOf[Identifier])
      setEmitFlags(declarationName, EmitFlags.NoSourceMap)
      val expressionName = getSynthesizedClone(
        parameter.name.asInstanceOf[Identifier])
      val restIndex = (node.parameters.length - 1)
      val temp = createLoopVariable()
      statements.push(
        setEmitFlags(
          createVariableStatement(
            undefined,
            createVariableDeclarationList(
              Array(
                createVariableDeclaration(
                  declarationName,
                  undefined,
                  createArrayLiteral(Array())))),
            parameter),
          EmitFlags.CustomPrologue))
      val forStatement = createFor(
        createVariableDeclarationList(
          Array(
            createVariableDeclaration(
              temp,
              undefined,
              createLiteral(restIndex))),
          parameter),
        createLessThan(
          temp,
          createPropertyAccess(createIdentifier("arguments"), "length"),
          parameter),
        createPostfixIncrement(temp, parameter),
        createBlock(
          Array(
            startOnNewLine(
              createStatement(
                createAssignment(
                  createElementAccess(
                    expressionName,
                    createSubtract(temp, createLiteral(restIndex))),
                  createElementAccess(createIdentifier("arguments"), temp)),
                parameter)))))
      setEmitFlags(forStatement, EmitFlags.CustomPrologue)
      startOnNewLine(forStatement)
      statements.push(forStatement)

    }
    def addCaptureThisForNodeIfNeeded(statements: Array[Statement],
                                      node: Node): Unit = {
      if (((node.transformFlags & TransformFlags.ContainsCapturedLexicalThis) && (node.kind !== SyntaxKind.ArrowFunction))) {
        captureThisForNode(statements, node, createThis())

      }

    }
    def captureThisForNode(statements: Array[Statement],
                           node: Node,
                           initializer: (Expression | undefined),
                           originalStatement: Statement): Unit = {
      enableSubstitutionsForCapturedThis()
      val captureThisStatement = createVariableStatement(
        undefined,
        createVariableDeclarationList(
          Array(createVariableDeclaration("_this", undefined, initializer))),
        originalStatement)
      setEmitFlags(
        captureThisStatement,
        (EmitFlags.NoComments | EmitFlags.CustomPrologue))
      setSourceMapRange(captureThisStatement, node)
      statements.push(captureThisStatement)

    }
    def addClassMembers(statements: Array[Statement],
                        node: (ClassExpression | ClassDeclaration)): Unit = {
      (node.members).foreach { fresh4 =>
        val member = zeroOfMyType = fresh4 {
          member.kind match {
            case SyntaxKind.SemicolonClassElement =>
              statements.push(
                transformSemicolonClassElementToStatement(
                  member.asInstanceOf[SemicolonClassElement]))
            case SyntaxKind.MethodDeclaration =>
              statements.push(
                transformClassMethodDeclarationToStatement(
                  getClassMemberPrefix(node, member),
                  member.asInstanceOf[MethodDeclaration]))
            case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
              val accessors = getAllAccessorDeclarations(
                node.members,
                member.asInstanceOf[AccessorDeclaration])
              if ((member === accessors.firstAccessor)) {
                statements.push(
                  transformAccessorsToStatement(
                    getClassMemberPrefix(node, member),
                    accessors))

              }
            case SyntaxKind.Constructor =>
            case _ =>
              Debug.failBadSyntaxKind(node)
          }

        }
      }

    }
    def transformSemicolonClassElementToStatement(
        member: SemicolonClassElement) = {
      return createEmptyStatement(member)

    }
    def transformClassMethodDeclarationToStatement(
        receiver: LeftHandSideExpression,
        member: MethodDeclaration) = {
      val commentRange = getCommentRange(member)
      val sourceMapRange = getSourceMapRange(member)
      val func = transformFunctionLikeToExpression(member, member, undefined)
      setEmitFlags(func, EmitFlags.NoComments)
      setSourceMapRange(func, sourceMapRange)
      val statement = createStatement(
        createAssignment(
          createMemberAccessForPropertyName(
            receiver,
            visitNode(member.name, visitor, isPropertyName),
            member.name),
          func),
        member)
      setOriginalNode(statement, member)
      setCommentRange(statement, commentRange)
      setEmitFlags(statement, EmitFlags.NoSourceMap)
      return statement

    }
    def transformAccessorsToStatement(
        receiver: LeftHandSideExpression,
        accessors: AllAccessorDeclarations): Statement = {
      val statement = createStatement(
        transformAccessorsToExpression(receiver, accessors, false),
        getSourceMapRange(accessors.firstAccessor))
      setEmitFlags(statement, EmitFlags.NoComments)
      return statement

    }
    def transformAccessorsToExpression(
        receiver: LeftHandSideExpression,
        triple: AllAccessorDeclarations,
        startsOnNewLine: Boolean): Expression = {
      const fresh5 = triple
      val firstAccessor = fresh5.firstAccessor
      val getAccessor = fresh5.getAccessor
      val setAccessor = fresh5.setAccessor
      val target = getMutableClone(receiver)
      setEmitFlags(
        target,
        (EmitFlags.NoComments | EmitFlags.NoTrailingSourceMap))
      setSourceMapRange(target, firstAccessor.name)
      val propertyName = createExpressionForPropertyName(
        visitNode(firstAccessor.name, visitor, isPropertyName))
      setEmitFlags(
        propertyName,
        (EmitFlags.NoComments | EmitFlags.NoLeadingSourceMap))
      setSourceMapRange(propertyName, firstAccessor.name)
      val properties: Array[ObjectLiteralElementLike] = Array()
      if (getAccessor) {
        val getterFunction =
          transformFunctionLikeToExpression(getAccessor, undefined, undefined)
        setSourceMapRange(getterFunction, getSourceMapRange(getAccessor))
        setEmitFlags(getterFunction, EmitFlags.NoLeadingComments)
        val getter = createPropertyAssignment("get", getterFunction)
        setCommentRange(getter, getCommentRange(getAccessor))
        properties.push(getter)

      }
      if (setAccessor) {
        val setterFunction =
          transformFunctionLikeToExpression(setAccessor, undefined, undefined)
        setSourceMapRange(setterFunction, getSourceMapRange(setAccessor))
        setEmitFlags(setterFunction, EmitFlags.NoLeadingComments)
        val setter = createPropertyAssignment("set", setterFunction)
        setCommentRange(setter, getCommentRange(setAccessor))
        properties.push(setter)

      }
      properties.push(
        createPropertyAssignment("enumerable", createLiteral(true)),
        createPropertyAssignment("configurable", createLiteral(true)))
      val call = createCall(
        createPropertyAccess(createIdentifier("Object"), "defineProperty"),
        undefined,
        Array(
          target,
          propertyName,
          createObjectLiteral(properties, undefined, true)))
      if (startsOnNewLine) {
        (call.startsOnNewLine = true)

      }
      return call

    }
    def visitArrowFunction(node: ArrowFunction) = {
      if ((node.transformFlags & TransformFlags.ContainsLexicalThis)) {
        enableSubstitutionsForCapturedThis()

      }
      val func = transformFunctionLikeToExpression(node, node, undefined)
      setEmitFlags(func, EmitFlags.CapturesThis)
      return func

    }
    def visitFunctionExpression(node: FunctionExpression): Expression = {
      return transformFunctionLikeToExpression(node, node, node.name)

    }
    def visitFunctionDeclaration(
        node: FunctionDeclaration): FunctionDeclaration = {
      return setOriginalNode(
        createFunctionDeclaration(
          undefined,
          node.modifiers,
          node.asteriskToken,
          node.name,
          undefined,
          visitNodes(node.parameters, visitor, isParameter),
          undefined,
          transformFunctionBody(node),
          node),
        node)

    }
    def transformFunctionLikeToExpression(
        node: FunctionLikeDeclaration,
        location: TextRange,
        name: Identifier): FunctionExpression = {
      val savedContainingNonArrowFunction = enclosingNonArrowFunction
      if ((node.kind !== SyntaxKind.ArrowFunction)) {
        (enclosingNonArrowFunction = node)

      }
      val expression = setOriginalNode(
        createFunctionExpression(
          undefined,
          node.asteriskToken,
          name,
          undefined,
          visitNodes(node.parameters, visitor, isParameter),
          undefined,
          saveStateAndInvoke(node, transformFunctionBody),
          location),
        node)
      (enclosingNonArrowFunction = savedContainingNonArrowFunction)
      return expression

    }
    def transformFunctionBody(node: FunctionLikeDeclaration) = {
      var multiLine = false
      var singleLine = false
      var statementsLocation: TextRange = zeroOfMyType
      var closeBraceLocation: TextRange = zeroOfMyType
      val statements: Array[Statement] = Array()
      val body = node.body
      var statementOffset: Int = zeroOfMyType
      startLexicalEnvironment()
      if (isBlock(body)) {
        (statementOffset =
          addPrologueDirectives(statements, body.statements, false, visitor))

      }
      addCaptureThisForNodeIfNeeded(statements, node)
      addDefaultValueAssignmentsIfNeeded(statements, node)
      addRestParameterIfNeeded(statements, node, false)
      if (((!multiLine) && (statements.length > 0))) {
        (multiLine = true)

      }
      if (isBlock(body)) {
        (statementsLocation = body.statements)
        addRange(
          statements,
          visitNodes(body.statements, visitor, isStatement, statementOffset))
        if (((!multiLine) && body.multiLine)) {
          (multiLine = true)

        }

      } else {
        Debug.assert((node.kind === SyntaxKind.ArrowFunction))
        (statementsLocation = moveRangeEnd(body, (-1)))
        val equalsGreaterThanToken =
          (node.asInstanceOf[ArrowFunction]).equalsGreaterThanToken
        if (((!nodeIsSynthesized(equalsGreaterThanToken)) && (!nodeIsSynthesized(
              body)))) {
          if (rangeEndIsOnSameLineAsRangeStart(
                equalsGreaterThanToken,
                body,
                currentSourceFile)) {
            (singleLine = true)

          } else {
            (multiLine = true)

          }

        }
        val expression = visitNode(body, visitor, isExpression)
        val returnStatement = createReturn(expression, body)
        setEmitFlags(
          returnStatement,
          ((EmitFlags.NoTokenSourceMaps | EmitFlags.NoTrailingSourceMap) | EmitFlags.NoTrailingComments))
        statements.push(returnStatement)
        (closeBraceLocation = body)

      }
      val lexicalEnvironment = endLexicalEnvironment()
      addRange(statements, lexicalEnvironment)
      if ((((!multiLine) && lexicalEnvironment) && lexicalEnvironment.length)) {
        (multiLine = true)

      }
      val block = createBlock(
        createNodeArray(statements, statementsLocation),
        node.body,
        multiLine)
      if (((!multiLine) && singleLine)) {
        setEmitFlags(block, EmitFlags.SingleLine)

      }
      if (closeBraceLocation) {
        setTokenSourceMapRange(
          block,
          SyntaxKind.CloseBraceToken,
          closeBraceLocation)

      }
      setOriginalNode(block, node.body)
      return block

    }
    def visitExpressionStatement(
        node: ExpressionStatement): ExpressionStatement = {
      node.expression.kind match {
        case SyntaxKind.ParenthesizedExpression =>
          return updateStatement(
            node,
            visitParenthesizedExpression(
              node.expression.asInstanceOf[ParenthesizedExpression],
              false))
        case SyntaxKind.BinaryExpression =>
          return updateStatement(
            node,
            visitBinaryExpression(
              node.expression.asInstanceOf[BinaryExpression],
              false))
        case _ =>
      }
      return visitEachChild(node, visitor, context)

    }
    def visitParenthesizedExpression(
        node: ParenthesizedExpression,
        needsDestructuringValue: Boolean): ParenthesizedExpression = {
      if (needsDestructuringValue) {
        node.expression.kind match {
          case SyntaxKind.ParenthesizedExpression =>
            return createParen(
              visitParenthesizedExpression(
                node.expression.asInstanceOf[ParenthesizedExpression],
                true),
              node)
          case SyntaxKind.BinaryExpression =>
            return createParen(
              visitBinaryExpression(
                node.expression.asInstanceOf[BinaryExpression],
                true),
              node)
          case _ =>
        }

      }
      return visitEachChild(node, visitor, context)

    }
    def visitBinaryExpression(node: BinaryExpression,
                              needsDestructuringValue: Boolean): Expression = {
      Debug.assert(isDestructuringAssignment(node))
      return flattenDestructuringAssignment(
        context,
        node,
        needsDestructuringValue,
        hoistVariableDeclaration,
        visitor)

    }
    def visitVariableStatement(node: VariableStatement): Statement = {
      if ((convertedLoopState && (((getCombinedNodeFlags(node.declarationList) & NodeFlags.BlockScoped)) == 0))) {
        var assignments: Array[Expression] = zeroOfMyType
        (node.declarationList.declarations).foreach { fresh6 =>
          val decl = zeroOfMyType = fresh6 {
            hoistVariableDeclarationDeclaredInConvertedLoop(
              convertedLoopState,
              decl)
            if (decl.initializer) {
              var assignment: Expression = zeroOfMyType
              if (isBindingPattern(decl.name)) {
                (assignment = flattenVariableDestructuringToExpression(
                  decl,
                  hoistVariableDeclaration,
                  undefined,
                  visitor))

              } else {
                (assignment = createBinary(
                  decl.name.asInstanceOf[Identifier],
                  SyntaxKind.EqualsToken,
                  visitNode(decl.initializer, visitor, isExpression)))

              }
              ((assignments || ((assignments = Array())))).push(assignment)

            }

          }
        }
        if (assignments) {
          return createStatement(
            reduceLeft(
              assignments,
              ((acc, v) => createBinary(v, SyntaxKind.CommaToken, acc))),
            node)

        } else {
          return undefined

        }

      }
      return visitEachChild(node, visitor, context)

    }
    def visitVariableDeclarationList(
        node: VariableDeclarationList): VariableDeclarationList = {
      if ((node.flags & NodeFlags.BlockScoped)) {
        enableSubstitutionsForBlockScopedBindings()

      }
      val declarations = flatten(
        map(
          node.declarations,
          (if ((node.flags & NodeFlags.Let))
             visitVariableDeclarationInLetDeclarationList
           else visitVariableDeclaration)))
      val declarationList = createVariableDeclarationList(declarations, node)
      setOriginalNode(declarationList, node)
      setCommentRange(declarationList, node)
      if (((node.transformFlags & TransformFlags.ContainsBindingPattern) && ((isBindingPattern(
            node.declarations(0).name) || isBindingPattern(
            lastOrUndefined(node.declarations).name))))) {
        val firstDeclaration = firstOrUndefined(declarations)
        val lastDeclaration = lastOrUndefined(declarations)
        setSourceMapRange(
          declarationList,
          createRange(firstDeclaration.pos, lastDeclaration.end))

      }
      return declarationList

    }
    def shouldEmitExplicitInitializerForLetDeclaration(
        node: VariableDeclaration) = {
      val flags = resolver.getNodeCheckFlags(node)
      val isCapturedInFunction = (flags & NodeCheckFlags.CapturedBlockScopedBinding)
      val isDeclaredInLoop = (flags & NodeCheckFlags.BlockScopedBindingInLoop)
      val emittedAsTopLevel = (isBlockScopedContainerTopLevel(
          enclosingBlockScopeContainer) || ((((isCapturedInFunction && isDeclaredInLoop) && isBlock(
          enclosingBlockScopeContainer)) && isIterationStatement(
          enclosingBlockScopeContainerParent,
          false))))
      val emitExplicitInitializer = ((((!emittedAsTopLevel) && (enclosingBlockScopeContainer.kind !== SyntaxKind.ForInStatement)) && (enclosingBlockScopeContainer.kind !== SyntaxKind.ForOfStatement)) && (((!resolver.isDeclarationWithCollidingName(
          node)) || (((isDeclaredInLoop && (!isCapturedInFunction)) && (!isIterationStatement(
          enclosingBlockScopeContainer,
          false)))))))
      return emitExplicitInitializer

    }
    def visitVariableDeclarationInLetDeclarationList(
        node: VariableDeclaration) = {
      val name = node.name
      if (isBindingPattern(name)) {
        return visitVariableDeclaration(node)

      }
      if (((!node.initializer) && shouldEmitExplicitInitializerForLetDeclaration(
            node))) {
        val clone = getMutableClone(node)
        (clone.initializer = createVoidZero())
        return clone

      }
      return visitEachChild(node, visitor, context)

    }
    def visitVariableDeclaration(
        node: VariableDeclaration): VisitResult[VariableDeclaration] = {
      if (isBindingPattern(node.name)) {
        val recordTempVariablesInLine = ((!enclosingVariableStatement) || (!hasModifier(
            enclosingVariableStatement,
            ModifierFlags.Export)))
        return flattenVariableDestructuring(
          node,
          undefined,
          visitor,
          (if (recordTempVariablesInLine) undefined
           else hoistVariableDeclaration))

      }
      return visitEachChild(node, visitor, context)

    }
    def visitLabeledStatement(node: LabeledStatement): VisitResult[Statement] = {
      if (convertedLoopState) {
        if ((!convertedLoopState.labels)) {
          (convertedLoopState.labels = createMap[String]())

        }
        (convertedLoopState.labels(node.label.text) = node.label.text)

      }
      var result: VisitResult[Statement] = zeroOfMyType
      if ((isIterationStatement(node.statement, false) && shouldConvertIterationStatementBody(
            node.statement.asInstanceOf[IterationStatement]))) {
        (result = visitNodes(
          createNodeArray(Array(node.statement)),
          visitor,
          isStatement))

      } else {
        (result = visitEachChild(node, visitor, context))

      }
      if (convertedLoopState) {
        (convertedLoopState.labels(node.label.text) = undefined)

      }
      return result

    }
    def visitDoStatement(node: DoStatement) = {
      return convertIterationStatementBodyIfNecessary(node)

    }
    def visitWhileStatement(node: WhileStatement) = {
      return convertIterationStatementBodyIfNecessary(node)

    }
    def visitForStatement(node: ForStatement) = {
      return convertIterationStatementBodyIfNecessary(node)

    }
    def visitForInStatement(node: ForInStatement) = {
      return convertIterationStatementBodyIfNecessary(node)

    }
    def visitForOfStatement(node: ForOfStatement): VisitResult[Statement] = {
      return convertIterationStatementBodyIfNecessary(node, convertForOfToFor)

    }
    def convertForOfToFor(
        node: ForOfStatement,
        convertedLoopBodyStatements: Array[Statement]): ForStatement = {
      val expression = visitNode(node.expression, visitor, isExpression)
      val initializer = node.initializer
      val statements: Array[Statement] = Array()
      val counter = createLoopVariable()
      val rhsReference =
        (if ((expression.kind === SyntaxKind.Identifier))
           createUniqueName((expression.asInstanceOf[Identifier]).text)
         else createTempVariable(undefined))
      if (isVariableDeclarationList(initializer)) {
        if ((initializer.flags & NodeFlags.BlockScoped)) {
          enableSubstitutionsForBlockScopedBindings()

        }
        val firstOriginalDeclaration = firstOrUndefined(
          initializer.declarations)
        if ((firstOriginalDeclaration && isBindingPattern(
              firstOriginalDeclaration.name))) {
          val declarations = flattenVariableDestructuring(
            firstOriginalDeclaration,
            createElementAccess(rhsReference, counter),
            visitor)
          val declarationList =
            createVariableDeclarationList(declarations, initializer)
          setOriginalNode(declarationList, initializer)
          val firstDeclaration = declarations(0)
          val lastDeclaration = lastOrUndefined(declarations)
          setSourceMapRange(
            declarationList,
            createRange(firstDeclaration.pos, lastDeclaration.end))
          statements.push(createVariableStatement(undefined, declarationList))

        } else {
          statements.push(
            createVariableStatement(
              undefined,
              createVariableDeclarationList(
                Array(
                  createVariableDeclaration(
                    (if (firstOriginalDeclaration)
                       firstOriginalDeclaration.name
                     else createTempVariable(undefined)),
                    undefined,
                    createElementAccess(rhsReference, counter))),
                moveRangePos(initializer, (-1))),
              moveRangeEnd(initializer, (-1))))

        }

      } else {
        val assignment = createAssignment(
          initializer,
          createElementAccess(rhsReference, counter))
        if (isDestructuringAssignment(assignment)) {
          statements.push(
            createStatement(
              flattenDestructuringAssignment(
                context,
                assignment,
                false,
                hoistVariableDeclaration,
                visitor)))

        } else {
          ((assignment.asInstanceOf[BinaryExpression]).end = initializer.end)
          statements.push(
            createStatement(assignment, moveRangeEnd(initializer, (-1))))

        }

      }
      var bodyLocation: TextRange = zeroOfMyType
      var statementsLocation: TextRange = zeroOfMyType
      if (convertedLoopBodyStatements) {
        addRange(statements, convertedLoopBodyStatements)

      } else {
        val statement = visitNode(node.statement, visitor, isStatement)
        if (isBlock(statement)) {
          addRange(statements, statement.statements)
          (bodyLocation = statement)
          (statementsLocation = statement.statements)

        } else {
          statements.push(statement)

        }

      }
      setEmitFlags(
        expression,
        (EmitFlags.NoSourceMap | getEmitFlags(expression)))
      val body = createBlock(
        createNodeArray(statements, statementsLocation),
        bodyLocation)
      setEmitFlags(body, (EmitFlags.NoSourceMap | EmitFlags.NoTokenSourceMaps))
      val forStatement = createFor(
        createVariableDeclarationList(
          Array(
            createVariableDeclaration(
              counter,
              undefined,
              createLiteral(0),
              moveRangePos(node.expression, (-1))),
            createVariableDeclaration(
              rhsReference,
              undefined,
              expression,
              node.expression)),
          node.expression),
        createLessThan(
          counter,
          createPropertyAccess(rhsReference, "length"),
          node.expression),
        createPostfixIncrement(counter, node.expression),
        body,
        node)
      setEmitFlags(forStatement, EmitFlags.NoTokenTrailingSourceMaps)
      return forStatement

    }
    def visitObjectLiteralExpression(
        node: ObjectLiteralExpression): Expression = {
      val properties = node.properties
      val numProperties = properties.length
      var numInitialProperties = numProperties {
        var i = 0
        while ((i < numProperties)) {
          {
            val property = properties(i)
            if (((property.transformFlags & TransformFlags.ContainsYield) || (property.name.kind === SyntaxKind.ComputedPropertyName))) {
              (numInitialProperties = i)
              break()

            }

          }
          (i += 1)
        }
      }
      Debug.assert((numInitialProperties !== numProperties))
      val temp = createTempVariable(hoistVariableDeclaration)
      val expressions: Array[Expression] = Array()
      val assignment = createAssignment(
        temp,
        setEmitFlags(
          createObjectLiteral(
            visitNodes(
              properties,
              visitor,
              isObjectLiteralElementLike,
              0,
              numInitialProperties),
            undefined,
            node.multiLine),
          EmitFlags.Indented))
      if (node.multiLine) {
        (assignment.startsOnNewLine = true)

      }
      expressions.push(assignment)
      addObjectLiteralMembers(expressions, node, temp, numInitialProperties)
      expressions.push(
        (if (node.multiLine) startOnNewLine(getMutableClone(temp)) else temp))
      return inlineExpressions(expressions)

    }
    def shouldConvertIterationStatementBody(
        node: IterationStatement): Boolean = {
      return (((resolver.getNodeCheckFlags(node) & NodeCheckFlags.LoopWithCapturedBlockScopedBinding)) !== 0)

    }
    def hoistVariableDeclarationDeclaredInConvertedLoop(
        state: ConvertedLoopState,
        node: VariableDeclaration): Unit = {
      if ((!state.hoistedLocalVariables)) {
        (state.hoistedLocalVariables = Array())

      }
      visit(node.name)
      def visit(node: (Identifier | BindingPattern)) = {
        if ((node.kind === SyntaxKind.Identifier)) {
          state.hoistedLocalVariables.push((node.asInstanceOf[Identifier]))

        } else {
          ((node.asInstanceOf[BindingPattern]).elements).foreach { fresh7 =>
            val element = zeroOfMyType = fresh7 {
              if ((!isOmittedExpression(element))) {
                visit(element.name)

              }

            }
          }

        }

      }

    }
    def convertIterationStatementBodyIfNecessary(
        node: IterationStatement,
        convert: ((IterationStatement,
                   Array[Statement]) => IterationStatement))
      : VisitResult[Statement] = {
      if ((!shouldConvertIterationStatementBody(node))) {
        var saveAllowedNonLabeledJumps: Jump = zeroOfMyType
        if (convertedLoopState) {
          (saveAllowedNonLabeledJumps =
            convertedLoopState.allowedNonLabeledJumps)
          (convertedLoopState.allowedNonLabeledJumps = (Jump.Break | Jump.Continue))

        }
        val result =
          (if (convert) convert(node, undefined)
           else visitEachChild(node, visitor, context))
        if (convertedLoopState) {
          (convertedLoopState.allowedNonLabeledJumps =
            saveAllowedNonLabeledJumps)

        }
        return result

      }
      val functionName = createUniqueName("_loop")
      var loopInitializer: VariableDeclarationList = zeroOfMyType
      node.kind match {
        case SyntaxKind.ForStatement | SyntaxKind.ForInStatement |
            SyntaxKind.ForOfStatement =>
          val initializer = (node
            .asInstanceOf[(ForStatement | ForInStatement | ForOfStatement)])
            .initializer
          if ((initializer && (initializer.kind === SyntaxKind.VariableDeclarationList))) {
            (loopInitializer =
              initializer.asInstanceOf[VariableDeclarationList])

          }
        case _ =>
      }
      val loopParameters: Array[ParameterDeclaration] = Array()
      val loopOutParameters: Array[LoopOutParameter] = Array()
      if ((loopInitializer && ((getCombinedNodeFlags(loopInitializer) & NodeFlags.BlockScoped)))) {
        (loopInitializer.declarations).foreach { fresh8 =>
          val decl = zeroOfMyType = fresh8 {
            processLoopVariableDeclaration(
              decl,
              loopParameters,
              loopOutParameters)

          }
        }

      }
      val outerConvertedLoopState = convertedLoopState
      (convertedLoopState = Map("loopOutParameters" -> loopOutParameters))
      if (outerConvertedLoopState) {
        if (outerConvertedLoopState.argumentsName) {
          (convertedLoopState.argumentsName =
            outerConvertedLoopState.argumentsName)

        }
        if (outerConvertedLoopState.thisName) {
          (convertedLoopState.thisName = outerConvertedLoopState.thisName)

        }
        if (outerConvertedLoopState.hoistedLocalVariables) {
          (convertedLoopState.hoistedLocalVariables =
            outerConvertedLoopState.hoistedLocalVariables)

        }

      }
      var loopBody = visitNode(node.statement, visitor, isStatement)
      val currentState = convertedLoopState
      (convertedLoopState = outerConvertedLoopState)
      if (loopOutParameters.length) {
        val statements =
          (if (isBlock(loopBody))(loopBody
             .asInstanceOf[Block])
             .statements
             .slice()
           else Array(loopBody))
        copyOutParameters(
          loopOutParameters,
          CopyDirection.ToOutParameter,
          statements)
        (loopBody = createBlock(statements, undefined, true))

      }
      if ((!isBlock(loopBody))) {
        (loopBody = createBlock(Array(loopBody), undefined, true))

      }
      val isAsyncBlockContainingAwait = ((enclosingNonArrowFunction && (((getEmitFlags(
          enclosingNonArrowFunction) & EmitFlags.AsyncFunctionBody)) !== 0)) && (((node.statement.transformFlags & TransformFlags.ContainsYield)) !== 0))
      var loopBodyFlags: EmitFlags = 0
      if (currentState.containsLexicalThis) {
        (loopBodyFlags |= EmitFlags.CapturesThis)

      }
      if (isAsyncBlockContainingAwait) {
        (loopBodyFlags |= EmitFlags.AsyncFunctionBody)

      }
      val convertedLoopVariable = createVariableStatement(
        undefined,
        createVariableDeclarationList(
          Array(
            createVariableDeclaration(
              functionName,
              undefined,
              setEmitFlags(
                createFunctionExpression(
                  undefined,
                  (if (isAsyncBlockContainingAwait)
                     createToken(SyntaxKind.AsteriskToken)
                   else undefined),
                  undefined,
                  undefined,
                  loopParameters,
                  undefined,
                  loopBody.asInstanceOf[Block]),
                loopBodyFlags)))))
      val statements: Array[Statement] = Array(convertedLoopVariable)
      var extraVariableDeclarations: Array[VariableDeclaration] = zeroOfMyType
      if (currentState.argumentsName) {
        if (outerConvertedLoopState) {
          (outerConvertedLoopState.argumentsName = currentState.argumentsName)

        } else {
          ((extraVariableDeclarations || ((extraVariableDeclarations = Array()))))
            .push(
              createVariableDeclaration(
                currentState.argumentsName,
                undefined,
                createIdentifier("arguments")))

        }

      }
      if (currentState.thisName) {
        if (outerConvertedLoopState) {
          (outerConvertedLoopState.thisName = currentState.thisName)

        } else {
          ((extraVariableDeclarations || ((extraVariableDeclarations = Array()))))
            .push(
              createVariableDeclaration(
                currentState.thisName,
                undefined,
                createIdentifier("this")))

        }

      }
      if (currentState.hoistedLocalVariables) {
        if (outerConvertedLoopState) {
          (outerConvertedLoopState.hoistedLocalVariables =
            currentState.hoistedLocalVariables)

        } else {
          if ((!extraVariableDeclarations)) {
            (extraVariableDeclarations = Array())

          }
          (currentState.hoistedLocalVariables).foreach { fresh9 =>
            val identifier = zeroOfMyType = fresh9 {
              extraVariableDeclarations.push(
                createVariableDeclaration(identifier))

            }
          }

        }

      }
      if (loopOutParameters.length) {
        if ((!extraVariableDeclarations)) {
          (extraVariableDeclarations = Array())

        }
        (loopOutParameters).foreach { fresh10 =>
          val outParam = zeroOfMyType = fresh10 {
            extraVariableDeclarations.push(
              createVariableDeclaration(outParam.outParamName))

          }
        }

      }
      if (extraVariableDeclarations) {
        statements.push(
          createVariableStatement(
            undefined,
            createVariableDeclarationList(extraVariableDeclarations)))

      }
      val convertedLoopBodyStatements = generateCallToConvertedLoop(
        functionName,
        loopParameters,
        currentState,
        isAsyncBlockContainingAwait)
      var loop: IterationStatement = zeroOfMyType
      if (convert) {
        (loop = convert(node, convertedLoopBodyStatements))

      } else {
        (loop = getMutableClone(node).asInstanceOf[IterationStatement])
        (loop.statement = undefined)
        (loop = visitEachChild(loop, visitor, context))
        (loop.statement =
          createBlock(convertedLoopBodyStatements, undefined, true))
        (loop.transformFlags = 0)
        aggregateTransformFlags(loop)

      }
      statements.push(
        (if ((currentParent.kind === SyntaxKind.LabeledStatement))
           createLabel(
             (currentParent.asInstanceOf[LabeledStatement]).label,
             loop)
         else loop))
      return statements

    }
    def copyOutParameter(outParam: LoopOutParameter,
                         copyDirection: CopyDirection): BinaryExpression = {
      val source =
        (if ((copyDirection === CopyDirection.ToOriginal))
           outParam.outParamName
         else outParam.originalName)
      val target =
        (if ((copyDirection === CopyDirection.ToOriginal))
           outParam.originalName
         else outParam.outParamName)
      return createBinary(target, SyntaxKind.EqualsToken, source)

    }
    def copyOutParameters(outParams: Array[LoopOutParameter],
                          copyDirection: CopyDirection,
                          statements: Array[Statement]): Unit = {
      (outParams).foreach { fresh11 =>
        val outParam = zeroOfMyType = fresh11 {
          statements.push(
            createStatement(copyOutParameter(outParam, copyDirection)))

        }
      }

    }
    def generateCallToConvertedLoop(
        loopFunctionExpressionName: Identifier,
        parameters: Array[ParameterDeclaration],
        state: ConvertedLoopState,
        isAsyncBlockContainingAwait: Boolean): Array[Statement] = {
      val outerConvertedLoopState = convertedLoopState
      val statements: Array[Statement] = Array()
      val isSimpleLoop = (((!((state.nonLocalJumps & (~Jump.Continue)))) && (!state.labeledNonLocalBreaks)) && (!state.labeledNonLocalContinues))
      val call = createCall(
        loopFunctionExpressionName,
        undefined,
        map(parameters, (p => p.name.asInstanceOf[Identifier])))
      val callResult =
        (if (isAsyncBlockContainingAwait)
           createYield(createToken(SyntaxKind.AsteriskToken), call)
         else call)
      if (isSimpleLoop) {
        statements.push(createStatement(callResult))
        copyOutParameters(
          state.loopOutParameters,
          CopyDirection.ToOriginal,
          statements)

      } else {
        val loopResultName = createUniqueName("state")
        val stateVariable = createVariableStatement(
          undefined,
          createVariableDeclarationList(Array(
            createVariableDeclaration(loopResultName, undefined, callResult))))
        statements.push(stateVariable)
        copyOutParameters(
          state.loopOutParameters,
          CopyDirection.ToOriginal,
          statements)
        if ((state.nonLocalJumps & Jump.Return)) {
          var returnStatement: ReturnStatement = zeroOfMyType
          if (outerConvertedLoopState) {
            (outerConvertedLoopState.nonLocalJumps |= Jump.Return)
            (returnStatement = createReturn(loopResultName))

          } else {
            (returnStatement = createReturn(
              createPropertyAccess(loopResultName, "value")))

          }
          statements.push(
            createIf(
              createBinary(
                createTypeOf(loopResultName),
                SyntaxKind.EqualsEqualsEqualsToken,
                createLiteral("object")),
              returnStatement))

        }
        if ((state.nonLocalJumps & Jump.Break)) {
          statements.push(
            createIf(
              createBinary(
                loopResultName,
                SyntaxKind.EqualsEqualsEqualsToken,
                createLiteral("break")),
              createBreak()))

        }
        if ((state.labeledNonLocalBreaks || state.labeledNonLocalContinues)) {
          val caseClauses: Array[CaseClause] = Array()
          processLabeledJumps(
            state.labeledNonLocalBreaks,
            true,
            loopResultName,
            outerConvertedLoopState,
            caseClauses)
          processLabeledJumps(
            state.labeledNonLocalContinues,
            false,
            loopResultName,
            outerConvertedLoopState,
            caseClauses)
          statements.push(
            createSwitch(loopResultName, createCaseBlock(caseClauses)))

        }

      }
      return statements

    }
    def setLabeledJump(state: ConvertedLoopState,
                       isBreak: Boolean,
                       labelText: String,
                       labelMarker: String): Unit = {
      if (isBreak) {
        if ((!state.labeledNonLocalBreaks)) {
          (state.labeledNonLocalBreaks = createMap[String]())

        }
        (state.labeledNonLocalBreaks(labelText) = labelMarker)

      } else {
        if ((!state.labeledNonLocalContinues)) {
          (state.labeledNonLocalContinues = createMap[String]())

        }
        (state.labeledNonLocalContinues(labelText) = labelMarker)

      }

    }
    def processLabeledJumps(table: Map[String],
                            isBreak: Boolean,
                            loopResultName: Identifier,
                            outerLoop: ConvertedLoopState,
                            caseClauses: Array[CaseClause]): Unit = {
      if ((!table)) {
        return

      }
      (table).keys.foreach { fresh12 =>
        val labelText = zeroOfMyType = fresh12 {
          val labelMarker = table(labelText)
          val statements: Array[Statement] = Array()
          if (((!outerLoop) || ((outerLoop.labels && outerLoop.labels(
                labelText))))) {
            val label = createIdentifier(labelText)
            statements.push(
              (if (isBreak) createBreak(label) else createContinue(label)))

          } else {
            setLabeledJump(outerLoop, isBreak, labelText, labelMarker)
            statements.push(createReturn(loopResultName))

          }
          caseClauses.push(
            createCaseClause(createLiteral(labelMarker), statements))

        }
      }

    }
    def processLoopVariableDeclaration(
        decl: (VariableDeclaration | BindingElement),
        loopParameters: Array[ParameterDeclaration],
        loopOutParameters: Array[LoopOutParameter]) = {
      val name = decl.name
      if (isBindingPattern(name)) {
        (name.elements).foreach { fresh13 =>
          val element = zeroOfMyType = fresh13 {
            if ((!isOmittedExpression(element))) {
              processLoopVariableDeclaration(
                element,
                loopParameters,
                loopOutParameters)

            }

          }
        }

      } else {
        loopParameters.push(createParameter(name))
        if ((resolver.getNodeCheckFlags(decl) & NodeCheckFlags.NeedsLoopOutParameter)) {
          val outParamName = createUniqueName(("out_" + name.text))
          loopOutParameters.push(
            Map("originalName" -> name, "outParamName" -> outParamName))

        }

      }

    }
    def addObjectLiteralMembers(expressions: Array[Expression],
                                node: ObjectLiteralExpression,
                                receiver: Identifier,
                                start: Int) = {
      val properties = node.properties
      val numProperties = properties.length {
        var i = start
        while ((i < numProperties)) {
          {
            val property = properties(i)
            property.kind match {
              case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
                val accessors = getAllAccessorDeclarations(
                  node.properties,
                  property.asInstanceOf[AccessorDeclaration])
                if ((property === accessors.firstAccessor)) {
                  expressions.push(
                    transformAccessorsToExpression(
                      receiver,
                      accessors,
                      node.multiLine))

                }
              case SyntaxKind.PropertyAssignment =>
                expressions.push(
                  transformPropertyAssignmentToExpression(
                    property.asInstanceOf[PropertyAssignment],
                    receiver,
                    node.multiLine))
              case SyntaxKind.ShorthandPropertyAssignment =>
                expressions.push(
                  transformShorthandPropertyAssignmentToExpression(
                    property.asInstanceOf[ShorthandPropertyAssignment],
                    receiver,
                    node.multiLine))
              case SyntaxKind.MethodDeclaration =>
                expressions.push(
                  transformObjectLiteralMethodDeclarationToExpression(
                    property.asInstanceOf[MethodDeclaration],
                    receiver,
                    node.multiLine))
              case _ =>
                Debug.failBadSyntaxKind(node)
            }

          }
          (i += 1)
        }
      }

    }
    def transformPropertyAssignmentToExpression(property: PropertyAssignment,
                                                receiver: Expression,
                                                startsOnNewLine: Boolean) = {
      val expression = createAssignment(
        createMemberAccessForPropertyName(
          receiver,
          visitNode(property.name, visitor, isPropertyName)),
        visitNode(property.initializer, visitor, isExpression),
        property)
      if (startsOnNewLine) {
        (expression.startsOnNewLine = true)

      }
      return expression

    }
    def transformShorthandPropertyAssignmentToExpression(
        property: ShorthandPropertyAssignment,
        receiver: Expression,
        startsOnNewLine: Boolean) = {
      val expression = createAssignment(
        createMemberAccessForPropertyName(
          receiver,
          visitNode(property.name, visitor, isPropertyName)),
        getSynthesizedClone(property.name),
        property)
      if (startsOnNewLine) {
        (expression.startsOnNewLine = true)

      }
      return expression

    }
    def transformObjectLiteralMethodDeclarationToExpression(
        method: MethodDeclaration,
        receiver: Expression,
        startsOnNewLine: Boolean) = {
      val expression = createAssignment(
        createMemberAccessForPropertyName(
          receiver,
          visitNode(method.name, visitor, isPropertyName)),
        transformFunctionLikeToExpression(method, method, undefined),
        method)
      if (startsOnNewLine) {
        (expression.startsOnNewLine = true)

      }
      return expression

    }
    def visitCatchClause(node: CatchClause): CatchClause = {
      Debug.assert(isBindingPattern(node.variableDeclaration.name))
      val temp = createTempVariable(undefined)
      val newVariableDeclaration = createVariableDeclaration(
        temp,
        undefined,
        undefined,
        node.variableDeclaration)
      val vars =
        flattenVariableDestructuring(node.variableDeclaration, temp, visitor)
      val list = createVariableDeclarationList(
        vars,
        node.variableDeclaration,
        node.variableDeclaration.flags)
      val destructure = createVariableStatement(undefined, list)
      return updateCatchClause(
        node,
        newVariableDeclaration,
        addStatementToStartOfBlock(node.block, destructure))

    }
    def addStatementToStartOfBlock(block: Block, statement: Statement): Block = {
      val transformedStatements =
        visitNodes(block.statements, visitor, isStatement)
      return updateBlock(block, Array(statement).concat(transformedStatements))

    }
    def visitMethodDeclaration(
        node: MethodDeclaration): ObjectLiteralElementLike = {
      Debug.assert((!isComputedPropertyName(node.name)))
      val functionExpression = transformFunctionLikeToExpression(
        node,
        moveRangePos(node, (-1)),
        undefined)
      setEmitFlags(
        functionExpression,
        (EmitFlags.NoLeadingComments | getEmitFlags(functionExpression)))
      return createPropertyAssignment(node.name, functionExpression, node)

    }
    def visitShorthandPropertyAssignment(
        node: ShorthandPropertyAssignment): ObjectLiteralElementLike = {
      return createPropertyAssignment(
        node.name,
        getSynthesizedClone(node.name),
        node)

    }
    def visitYieldExpression(node: YieldExpression): Expression = {
      return visitEachChild(node, visitor, context)

    }
    def visitArrayLiteralExpression(node: ArrayLiteralExpression): Expression = {
      return transformAndSpreadElements(
        node.elements,
        true,
        node.multiLine,
        node.elements.hasTrailingComma)

    }
    def visitCallExpression(node: CallExpression) = {
      return visitCallExpressionWithPotentialCapturedThisAssignment(node, true)

    }
    def visitImmediateSuperCallInBody(node: CallExpression) = {
      return visitCallExpressionWithPotentialCapturedThisAssignment(
        node,
        false)

    }
    def visitCallExpressionWithPotentialCapturedThisAssignment(
        node: CallExpression,
        assignToCapturedThis: Boolean): (CallExpression | BinaryExpression) = {
      const fresh14 =
        createCallBinding(node.expression, hoistVariableDeclaration)
      val target = fresh14.target
      val thisArg = fresh14.thisArg
      if ((node.expression.kind === SyntaxKind.SuperKeyword)) {
        setEmitFlags(thisArg, EmitFlags.NoSubstitution)

      }
      var resultingCall: (CallExpression | BinaryExpression) = zeroOfMyType
      if ((node.transformFlags & TransformFlags.ContainsSpreadElementExpression)) {
        (resultingCall = createFunctionApply(
          visitNode(target, visitor, isExpression),
          visitNode(thisArg, visitor, isExpression),
          transformAndSpreadElements(node.arguments, false, false, false)))

      } else {
        (resultingCall = createFunctionCall(
          visitNode(target, visitor, isExpression),
          visitNode(thisArg, visitor, isExpression),
          visitNodes(node.arguments, visitor, isExpression),
          node))

      }
      if ((node.expression.kind === SyntaxKind.SuperKeyword)) {
        val actualThis = createThis()
        setEmitFlags(actualThis, EmitFlags.NoSubstitution)
        val initializer = createLogicalOr(resultingCall, actualThis)
        return (if (assignToCapturedThis)
                  createAssignment(createIdentifier("_this"), initializer)
                else initializer)

      }
      return resultingCall

    }
    def visitNewExpression(node: NewExpression): LeftHandSideExpression = {
      Debug.assert(
        (((node.transformFlags & TransformFlags.ContainsSpreadElementExpression)) !== 0))
      const fresh15 = createCallBinding(
        createPropertyAccess(node.expression, "bind"),
        hoistVariableDeclaration)
      val target = fresh15.target
      val thisArg = fresh15.thisArg
      return createNew(
        createFunctionApply(
          visitNode(target, visitor, isExpression),
          thisArg,
          transformAndSpreadElements(
            createNodeArray(Array(createVoidZero(), node.arguments: _*)),
            false,
            false,
            false)),
        undefined,
        Array())

    }
    def transformAndSpreadElements(elements: NodeArray[Expression],
                                   needsUniqueCopy: Boolean,
                                   multiLine: Boolean,
                                   hasTrailingComma: Boolean): Expression = {
      val numElements = elements.length
      val segments = flatten(
        spanMap(
          elements,
          partitionSpreadElement,
          ((partition, visitPartition, _start, end) =>
             visitPartition(
               partition,
               multiLine,
               (hasTrailingComma && (end === numElements))))))
      if ((segments.length === 1)) {
        val firstElement = elements(0)
        return (if (((needsUniqueCopy && isSpreadElementExpression(
                      firstElement)) && (firstElement.expression.kind !== SyntaxKind.ArrayLiteralExpression)))
                  createArraySlice(segments(0))
                else segments(0))

      }
      return createArrayConcat(segments.shift(), segments)

    }
    def partitionSpreadElement(node: Expression) = {
      return (if (isSpreadElementExpression(node)) visitSpanOfSpreadElements
              else visitSpanOfNonSpreadElements)

    }
    def visitSpanOfSpreadElements(
        chunk: Array[Expression]): VisitResult[Expression] = {
      return map(chunk, visitExpressionOfSpreadElement)

    }
    def visitSpanOfNonSpreadElements(
        chunk: Array[Expression],
        multiLine: Boolean,
        hasTrailingComma: Boolean): VisitResult[Expression] = {
      return createArrayLiteral(
        visitNodes(
          createNodeArray(chunk, undefined, hasTrailingComma),
          visitor,
          isExpression),
        undefined,
        multiLine)

    }
    def visitExpressionOfSpreadElement(node: SpreadElementExpression) = {
      return visitNode(node.expression, visitor, isExpression)

    }
    def visitTemplateLiteral(node: LiteralExpression): LeftHandSideExpression = {
      return createLiteral(node.text, node)

    }
    def visitTaggedTemplateExpression(node: TaggedTemplateExpression) = {
      val tag = visitNode(node.tag, visitor, isExpression)
      val temp = createTempVariable(hoistVariableDeclaration)
      val templateArguments: Array[Expression] = Array(temp)
      val cookedStrings: Array[Expression] = Array()
      val rawStrings: Array[Expression] = Array()
      val template = node.template
      if (isNoSubstitutionTemplateLiteral(template)) {
        cookedStrings.push(createLiteral(template.text))
        rawStrings.push(getRawLiteral(template))

      } else {
        cookedStrings.push(createLiteral(template.head.text))
        rawStrings.push(getRawLiteral(template.head))
        (template.templateSpans).foreach { fresh16 =>
          val templateSpan = zeroOfMyType = fresh16 {
            cookedStrings.push(createLiteral(templateSpan.literal.text))
            rawStrings.push(getRawLiteral(templateSpan.literal))
            templateArguments.push(
              visitNode(templateSpan.expression, visitor, isExpression))

          }
        }

      }
      return createParen(
        inlineExpressions(
          Array(
            createAssignment(temp, createArrayLiteral(cookedStrings)),
            createAssignment(
              createPropertyAccess(temp, "raw"),
              createArrayLiteral(rawStrings)),
            createCall(tag, undefined, templateArguments))))

    }
    def getRawLiteral(node: LiteralLikeNode) = {
      var text = getSourceTextOfNodeFromSourceFile(currentSourceFile, node)
      val isLast = ((node.kind === SyntaxKind.NoSubstitutionTemplateLiteral) || (node.kind === SyntaxKind.TemplateTail))
      (text = text.substring(1, (text.length - ((if (isLast) 1 else 2) ))))
      (text = text
        .replace(java.util.regex.Pattern.compile(raw"""\r\n?""", "g"), "\n"))
      return createLiteral(text, node)

    }
    def visitTemplateExpression(node: TemplateExpression): Expression = {
      val expressions: Array[Expression] = Array()
      addTemplateHead(expressions, node)
      addTemplateSpans(expressions, node)
      val expression = reduceLeft(expressions, createAdd)
      if (nodeIsSynthesized(expression)) {
        setTextRange(expression, node)

      }
      return expression

    }
    def shouldAddTemplateHead(node: TemplateExpression) = {
      Debug.assert((node.templateSpans.length !== 0))
      return ((node.head.text.length !== 0) || (node
        .templateSpans(0)
        .literal
        .text
        .length === 0))

    }
    def addTemplateHead(expressions: Array[Expression],
                        node: TemplateExpression): Unit = {
      if ((!shouldAddTemplateHead(node))) {
        return

      }
      expressions.push(createLiteral(node.head.text))

    }
    def addTemplateSpans(expressions: Array[Expression],
                         node: TemplateExpression): Unit = {
      (node.templateSpans).foreach { fresh17 =>
        val span = zeroOfMyType = fresh17 {
          expressions.push(visitNode(span.expression, visitor, isExpression))
          if ((span.literal.text.length !== 0)) {
            expressions.push(createLiteral(span.literal.text))

          }

        }
      }

    }
    def visitSuperKeyword(): LeftHandSideExpression = {
      return (if ((((enclosingNonAsyncFunctionBody && isClassElement(
                    enclosingNonAsyncFunctionBody)) && (!hasModifier(
                    enclosingNonAsyncFunctionBody,
                    ModifierFlags.Static))) && (currentParent.kind !== SyntaxKind.CallExpression)))
                createPropertyAccess(createIdentifier("_super"), "prototype")
              else createIdentifier("_super"))

    }
    def visitSourceFileNode(node: SourceFile): SourceFile = {
      val pair = span(node.statements, isPrologueDirective)
      val prologue = pair(0)
      val remaining = pair(1)
      val statements: Array[Statement] = Array()
      startLexicalEnvironment()
      addRange(statements, prologue)
      addCaptureThisForNodeIfNeeded(statements, node)
      addRange(
        statements,
        visitNodes(createNodeArray(remaining), visitor, isStatement))
      addRange(statements, endLexicalEnvironment())
      val clone = getMutableClone(node)
      (clone.statements = createNodeArray(statements, node.statements))
      return clone

    }
    def onEmitNode(emitContext: EmitContext,
                   node: Node,
                   emitCallback: ((EmitContext, Node) => Unit)) = {
      val savedEnclosingFunction = enclosingFunction
      if (((enabledSubstitutions & ES2015SubstitutionFlags.CapturedThis) && isFunctionLike(
            node))) {
        (enclosingFunction = node)

      }
      previousOnEmitNode(emitContext, node, emitCallback)
      (enclosingFunction = savedEnclosingFunction)

    }
    def enableSubstitutionsForBlockScopedBindings() = {
      if ((((enabledSubstitutions & ES2015SubstitutionFlags.BlockScopedBindings)) === 0)) {
        (enabledSubstitutions |= ES2015SubstitutionFlags.BlockScopedBindings)
        context.enableSubstitution(SyntaxKind.Identifier)

      }

    }
    def enableSubstitutionsForCapturedThis() = {
      if ((((enabledSubstitutions & ES2015SubstitutionFlags.CapturedThis)) === 0)) {
        (enabledSubstitutions |= ES2015SubstitutionFlags.CapturedThis)
        context.enableSubstitution(SyntaxKind.ThisKeyword)
        context.enableEmitNotification(SyntaxKind.Constructor)
        context.enableEmitNotification(SyntaxKind.MethodDeclaration)
        context.enableEmitNotification(SyntaxKind.GetAccessor)
        context.enableEmitNotification(SyntaxKind.SetAccessor)
        context.enableEmitNotification(SyntaxKind.ArrowFunction)
        context.enableEmitNotification(SyntaxKind.FunctionExpression)
        context.enableEmitNotification(SyntaxKind.FunctionDeclaration)

      }

    }
    def onSubstituteNode(emitContext: EmitContext, node: Node) = {
      (node = previousOnSubstituteNode(emitContext, node))
      if ((emitContext === EmitContext.Expression)) {
        return substituteExpression(node)

      }
      if (isIdentifier(node)) {
        return substituteIdentifier(node)

      }
      return node

    }
    def substituteIdentifier(node: Identifier) = {
      if ((enabledSubstitutions & ES2015SubstitutionFlags.BlockScopedBindings)) {
        val original = getParseTreeNode(node, isIdentifier)
        if ((original && isNameOfDeclarationWithCollidingName(original))) {
          return getGeneratedNameForNode(original)

        }

      }
      return node

    }
    def isNameOfDeclarationWithCollidingName(node: Identifier) = {
      val parent = node.parent
      parent.kind match {
        case SyntaxKind.BindingElement | SyntaxKind.ClassDeclaration |
            SyntaxKind.EnumDeclaration | SyntaxKind.VariableDeclaration =>
          return (((parent
            .asInstanceOf[Declaration])
            .name === node) && resolver.isDeclarationWithCollidingName(
            parent.asInstanceOf[Declaration]))
        case _ =>
      }
      return false

    }
    def substituteExpression(node: Node) = {
      node.kind match {
        case SyntaxKind.Identifier =>
          return substituteExpressionIdentifier(node.asInstanceOf[Identifier])
        case SyntaxKind.ThisKeyword =>
          return substituteThisKeyword(node.asInstanceOf[PrimaryExpression])
        case _ =>
      }
      return node

    }
    def substituteExpressionIdentifier(node: Identifier): Identifier = {
      if ((enabledSubstitutions & ES2015SubstitutionFlags.BlockScopedBindings)) {
        val declaration =
          resolver.getReferencedDeclarationWithCollidingName(node)
        if (declaration) {
          return getGeneratedNameForNode(declaration.name)

        }

      }
      return node

    }
    def substituteThisKeyword(node: PrimaryExpression): PrimaryExpression = {
      if ((((enabledSubstitutions & ES2015SubstitutionFlags.CapturedThis) && enclosingFunction) && (getEmitFlags(
            enclosingFunction) & EmitFlags.CapturesThis))) {
        return createIdentifier("_this", node)

      }
      return node

    }
    def getLocalName(
        node: (ClassDeclaration | ClassExpression | FunctionDeclaration),
        allowComments: Boolean,
        allowSourceMaps: Boolean) = {
      return getDeclarationName(
        node,
        allowComments,
        allowSourceMaps,
        EmitFlags.LocalName)

    }
    def getDeclarationName(
        node: (ClassDeclaration | ClassExpression | FunctionDeclaration),
        allowComments: Boolean,
        allowSourceMaps: Boolean,
        emitFlags: EmitFlags) = {
      if ((node.name && (!isGeneratedIdentifier(node.name)))) {
        val name = getMutableClone(node.name)
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

      }
      return getGeneratedNameForNode(node)

    }
    def getClassMemberPrefix(node: (ClassExpression | ClassDeclaration),
                             member: ClassElement) = {
      val expression = getLocalName(node)
      return (if (hasModifier(member, ModifierFlags.Static)) expression
              else createPropertyAccess(expression, "prototype"))

    }
    def hasSynthesizedDefaultSuperCall(`constructor`: ConstructorDeclaration,
                                       hasExtendsClause: Boolean) = {
      if (((!`constructor`) || (!hasExtendsClause))) {
        return false

      }
      val parameter = singleOrUndefined(`constructor`.parameters)
      if ((((!parameter) || (!nodeIsSynthesized(parameter))) || (!parameter.dotDotDotToken))) {
        return false

      }
      val statement = firstOrUndefined(`constructor`.body.statements)
      if ((((!statement) || (!nodeIsSynthesized(statement))) || (statement.kind !== SyntaxKind.ExpressionStatement))) {
        return false

      }
      val statementExpression =
        (statement.asInstanceOf[ExpressionStatement]).expression
      if (((!nodeIsSynthesized(statementExpression)) || (statementExpression.kind !== SyntaxKind.CallExpression))) {
        return false

      }
      val callTarget =
        (statementExpression.asInstanceOf[CallExpression]).expression
      if (((!nodeIsSynthesized(callTarget)) || (callTarget.kind !== SyntaxKind.SuperKeyword))) {
        return false

      }
      val callArgument = singleOrUndefined(
        (statementExpression.asInstanceOf[CallExpression]).arguments)
      if ((((!callArgument) || (!nodeIsSynthesized(callArgument))) || (callArgument.kind !== SyntaxKind.SpreadElementExpression))) {
        return false

      }
      val expression =
        (callArgument.asInstanceOf[SpreadElementExpression]).expression
      return (isIdentifier(expression) && (expression === parameter.name))

    }

  }
}
