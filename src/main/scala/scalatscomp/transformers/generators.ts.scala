package scalatscomp.transformers
object Generators {
  type Label = Int
  sealed abstract class OpCode
  object OpCode {
    case object Nop extends OpCode
    case object Statement extends OpCode
    case object Assign extends OpCode
    case object Break extends OpCode
    case object BreakWhenTrue extends OpCode
    case object BreakWhenFalse extends OpCode
    case object Yield extends OpCode
    case object YieldStar extends OpCode
    case object Return extends OpCode
    case object Throw extends OpCode
    case object Endfinally extends OpCode
  }
  type OperationArguments =
    ((Label) | (Label, Expression) | (Statement) | (Expression) | (Expression,
                                                                   Expression))
  sealed abstract class BlockAction
  object BlockAction {
    case object Open extends BlockAction
    case object Close extends BlockAction
  }
  sealed abstract class CodeBlockKind
  object CodeBlockKind {
    case object Exception extends CodeBlockKind
    case object With extends CodeBlockKind
    case object Switch extends CodeBlockKind
    case object Loop extends CodeBlockKind
    case object Labeled extends CodeBlockKind
  }
  sealed abstract class ExceptionBlockState
  object ExceptionBlockState {
    case object Try extends ExceptionBlockState
    case object Catch extends ExceptionBlockState
    case object Finally extends ExceptionBlockState
    case object Done extends ExceptionBlockState
  }
  trait CodeBlock {
    var kind: CodeBlockKind
  }
  trait ExceptionBlock extends CodeBlock {
    var state: ExceptionBlockState
    var startLabel: Label
    var catchVariable: Identifier
    var catchLabel: Label
    var finallyLabel: Label
    var endLabel: Label
  }
  trait LabeledBlock extends CodeBlock {
    var labelText: String
    var isScript: Boolean
    var breakLabel: Label
  }
  trait SwitchBlock extends CodeBlock {
    var isScript: Boolean
    var breakLabel: Label
  }
  trait LoopBlock extends CodeBlock {
    var continueLabel: Label
    var isScript: Boolean
    var breakLabel: Label
  }
  trait WithBlock extends CodeBlock {
    var expression: Identifier
    var startLabel: Label
    var endLabel: Label
  }
  sealed abstract class Instruction
  object Instruction {
    case object Next extends Instruction
    case object Throw extends Instruction
    case object Return extends Instruction
    case object Break extends Instruction
    case object Yield extends Instruction
    case object YieldStar extends Instruction
    case object Catch extends Instruction
    case object Endfinally extends Instruction
  }
  val instructionNames = createMap[String](
    Map(
      Instruction.Return -> "return",
      Instruction.Break -> "break",
      Instruction.Yield -> "yield",
      Instruction.YieldStar -> "yield*",
      Instruction.Endfinally -> "endfinally"))
  def transformGenerators(context: TransformationContext) = {
    const fresh1 = context
    val startLexicalEnvironment = fresh1.startLexicalEnvironment
    val endLexicalEnvironment = fresh1.endLexicalEnvironment
    val hoistFunctionDeclaration = fresh1.hoistFunctionDeclaration
    val hoistVariableDeclaration = fresh1.hoistVariableDeclaration
    val compilerOptions = context.getCompilerOptions()
    val languageVersion = getEmitScriptTarget(compilerOptions)
    val resolver = context.getEmitResolver()
    val previousOnSubstituteNode = context.onSubstituteNode
    (context.onSubstituteNode = onSubstituteNode)
    var currentSourceFile: SourceFile = zeroOfMyType
    var renamedCatchVariables: Map[Boolean] = zeroOfMyType
    var renamedCatchVariableDeclarations: Map[Identifier] = zeroOfMyType
    var inGeneratorFunctionBody: Boolean = zeroOfMyType
    var inStatementContainingYield: Boolean = zeroOfMyType
    var blocks: Array[CodeBlock] = zeroOfMyType
    var blockOffsets: Array[Int] = zeroOfMyType
    var blockActions: Array[BlockAction] = zeroOfMyType
    var blockStack: Array[CodeBlock] = zeroOfMyType
    var labelOffsets: Array[Int] = zeroOfMyType
    var labelExpressions: Array[Array[LiteralExpression]] = zeroOfMyType
    var nextLabelId = 1
    var operations: Array[OpCode] = zeroOfMyType
    var operationArguments: Array[OperationArguments] = zeroOfMyType
    var operationLocations: Array[TextRange] = zeroOfMyType
    var state: Identifier = zeroOfMyType
    var blockIndex = 0
    var labelNumber = 0
    var labelNumbers: Array[Array[Int]] = zeroOfMyType
    var lastOperationWasAbrupt: Boolean = zeroOfMyType
    var lastOperationWasCompletion: Boolean = zeroOfMyType
    var clauses: Array[CaseClause] = zeroOfMyType
    var statements: Array[Statement] = zeroOfMyType
    var exceptionBlockStack: Array[ExceptionBlock] = zeroOfMyType
    var currentExceptionBlock: ExceptionBlock = zeroOfMyType
    var withBlockStack: Array[WithBlock] = zeroOfMyType
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      if (isDeclarationFile(node)) {
        return node

      }
      if ((node.transformFlags & TransformFlags.ContainsGenerator)) {
        (currentSourceFile = node)
        (node = visitEachChild(node, visitor, context))
        (currentSourceFile = undefined)

      }
      return node

    }
    def visitor(node: Node): VisitResult[Node] = {
      val transformFlags = node.transformFlags
      if (inStatementContainingYield) {
        return visitJavaScriptInStatementContainingYield(node)

      } else if (inGeneratorFunctionBody) {
        return visitJavaScriptInGeneratorFunctionBody(node)

      } else if ((transformFlags & TransformFlags.Generator)) {
        return visitGenerator(node)

      } else if ((transformFlags & TransformFlags.ContainsGenerator)) {
        return visitEachChild(node, visitor, context)

      } else {
        return node

      }

    }
    def visitJavaScriptInStatementContainingYield(
        node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.DoStatement =>
          return visitDoStatement(node.asInstanceOf[DoStatement])
        case SyntaxKind.WhileStatement =>
          return visitWhileStatement(node.asInstanceOf[WhileStatement])
        case SyntaxKind.SwitchStatement =>
          return visitSwitchStatement(node.asInstanceOf[SwitchStatement])
        case SyntaxKind.LabeledStatement =>
          return visitLabeledStatement(node.asInstanceOf[LabeledStatement])
        case _ =>
          return visitJavaScriptInGeneratorFunctionBody(node)
      }

    }
    def visitJavaScriptInGeneratorFunctionBody(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.FunctionDeclaration =>
          return visitFunctionDeclaration(
            node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.FunctionExpression =>
          return visitFunctionExpression(node.asInstanceOf[FunctionExpression])
        case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
          return visitAccessorDeclaration(
            node.asInstanceOf[AccessorDeclaration])
        case SyntaxKind.VariableStatement =>
          return visitVariableStatement(node.asInstanceOf[VariableStatement])
        case SyntaxKind.ForStatement =>
          return visitForStatement(node.asInstanceOf[ForStatement])
        case SyntaxKind.ForInStatement =>
          return visitForInStatement(node.asInstanceOf[ForInStatement])
        case SyntaxKind.BreakStatement =>
          return visitBreakStatement(node.asInstanceOf[BreakStatement])
        case SyntaxKind.ContinueStatement =>
          return visitContinueStatement(node.asInstanceOf[ContinueStatement])
        case SyntaxKind.ReturnStatement =>
          return visitReturnStatement(node.asInstanceOf[ReturnStatement])
        case _ =>
          if ((node.transformFlags & TransformFlags.ContainsYield)) {
            return visitJavaScriptContainingYield(node)

          } else if ((node.transformFlags & ((TransformFlags.ContainsGenerator | TransformFlags.ContainsHoistedDeclarationOrCompletion)))) {
            return visitEachChild(node, visitor, context)

          } else {
            return node

          }
      }

    }
    def visitJavaScriptContainingYield(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.BinaryExpression =>
          return visitBinaryExpression(node.asInstanceOf[BinaryExpression])
        case SyntaxKind.ConditionalExpression =>
          return visitConditionalExpression(
            node.asInstanceOf[ConditionalExpression])
        case SyntaxKind.YieldExpression =>
          return visitYieldExpression(node.asInstanceOf[YieldExpression])
        case SyntaxKind.ArrayLiteralExpression =>
          return visitArrayLiteralExpression(
            node.asInstanceOf[ArrayLiteralExpression])
        case SyntaxKind.ObjectLiteralExpression =>
          return visitObjectLiteralExpression(
            node.asInstanceOf[ObjectLiteralExpression])
        case SyntaxKind.ElementAccessExpression =>
          return visitElementAccessExpression(
            node.asInstanceOf[ElementAccessExpression])
        case SyntaxKind.CallExpression =>
          return visitCallExpression(node.asInstanceOf[CallExpression])
        case SyntaxKind.NewExpression =>
          return visitNewExpression(node.asInstanceOf[NewExpression])
        case _ =>
          return visitEachChild(node, visitor, context)
      }

    }
    def visitGenerator(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.FunctionDeclaration =>
          return visitFunctionDeclaration(
            node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.FunctionExpression =>
          return visitFunctionExpression(node.asInstanceOf[FunctionExpression])
        case _ =>
          Debug.failBadSyntaxKind(node)
          return visitEachChild(node, visitor, context)
      }

    }
    def visitFunctionDeclaration(node: FunctionDeclaration): Statement = {
      if ((node.asteriskToken && (getEmitFlags(node) & EmitFlags.AsyncFunctionBody))) {
        (node = setOriginalNode(
          createFunctionDeclaration(
            undefined,
            undefined,
            undefined,
            node.name,
            undefined,
            node.parameters,
            undefined,
            transformGeneratorFunctionBody(node.body),
            node),
          node))

      } else {
        val savedInGeneratorFunctionBody = inGeneratorFunctionBody
        val savedInStatementContainingYield = inStatementContainingYield
        (inGeneratorFunctionBody = false)
        (inStatementContainingYield = false)
        (node = visitEachChild(node, visitor, context))
        (inGeneratorFunctionBody = savedInGeneratorFunctionBody)
        (inStatementContainingYield = savedInStatementContainingYield)

      }
      if (inGeneratorFunctionBody) {
        hoistFunctionDeclaration(node)
        return undefined

      } else {
        return node

      }

    }
    def visitFunctionExpression(node: FunctionExpression): Expression = {
      if ((node.asteriskToken && (getEmitFlags(node) & EmitFlags.AsyncFunctionBody))) {
        (node = setOriginalNode(
          createFunctionExpression(
            undefined,
            undefined,
            node.name,
            undefined,
            node.parameters,
            undefined,
            transformGeneratorFunctionBody(node.body),
            node),
          node))

      } else {
        val savedInGeneratorFunctionBody = inGeneratorFunctionBody
        val savedInStatementContainingYield = inStatementContainingYield
        (inGeneratorFunctionBody = false)
        (inStatementContainingYield = false)
        (node = visitEachChild(node, visitor, context))
        (inGeneratorFunctionBody = savedInGeneratorFunctionBody)
        (inStatementContainingYield = savedInStatementContainingYield)

      }
      return node

    }
    def visitAccessorDeclaration(node: AccessorDeclaration) = {
      val savedInGeneratorFunctionBody = inGeneratorFunctionBody
      val savedInStatementContainingYield = inStatementContainingYield
      (inGeneratorFunctionBody = false)
      (inStatementContainingYield = false)
      (node = visitEachChild(node, visitor, context))
      (inGeneratorFunctionBody = savedInGeneratorFunctionBody)
      (inStatementContainingYield = savedInStatementContainingYield)
      return node

    }
    def transformGeneratorFunctionBody(body: Block) = {
      val statements: Array[Statement] = Array()
      val savedInGeneratorFunctionBody = inGeneratorFunctionBody
      val savedInStatementContainingYield = inStatementContainingYield
      val savedBlocks = blocks
      val savedBlockOffsets = blockOffsets
      val savedBlockActions = blockActions
      val savedBlockStack = blockStack
      val savedLabelOffsets = labelOffsets
      val savedLabelExpressions = labelExpressions
      val savedNextLabelId = nextLabelId
      val savedOperations = operations
      val savedOperationArguments = operationArguments
      val savedOperationLocations = operationLocations
      val savedState = state
      (inGeneratorFunctionBody = true)
      (inStatementContainingYield = false)
      (blocks = undefined)
      (blockOffsets = undefined)
      (blockActions = undefined)
      (blockStack = undefined)
      (labelOffsets = undefined)
      (labelExpressions = undefined)
      (nextLabelId = 1)
      (operations = undefined)
      (operationArguments = undefined)
      (operationLocations = undefined)
      (state = createTempVariable(undefined))
      startLexicalEnvironment()
      val statementOffset =
        addPrologueDirectives(statements, body.statements, false, visitor)
      transformAndEmitStatements(body.statements, statementOffset)
      val buildResult = build()
      addRange(statements, endLexicalEnvironment())
      statements.push(createReturn(buildResult))
      (inGeneratorFunctionBody = savedInGeneratorFunctionBody)
      (inStatementContainingYield = savedInStatementContainingYield)
      (blocks = savedBlocks)
      (blockOffsets = savedBlockOffsets)
      (blockActions = savedBlockActions)
      (blockStack = savedBlockStack)
      (labelOffsets = savedLabelOffsets)
      (labelExpressions = savedLabelExpressions)
      (nextLabelId = savedNextLabelId)
      (operations = savedOperations)
      (operationArguments = savedOperationArguments)
      (operationLocations = savedOperationLocations)
      (state = savedState)
      return createBlock(statements, body, body.multiLine)

    }
    def visitVariableStatement(node: VariableStatement): Statement = {
      if ((node.transformFlags & TransformFlags.ContainsYield)) {
        transformAndEmitVariableDeclarationList(node.declarationList)
        return undefined

      } else {
        if ((getEmitFlags(node) & EmitFlags.CustomPrologue)) {
          return node

        }
        (node.declarationList.declarations).foreach { fresh2 =>
          val variable = zeroOfMyType = fresh2 {
            hoistVariableDeclaration(variable.name.asInstanceOf[Identifier])

          }
        }
        val variables = getInitializedVariables(node.declarationList)
        if ((variables.length === 0)) {
          return undefined

        }
        return createStatement(
          inlineExpressions(map(variables, transformInitializedVariable)))

      }

    }
    def visitBinaryExpression(node: BinaryExpression): Expression = {
      getExpressionAssociativity(node) match {
        case Associativity.Left =>
          return visitLeftAssociativeBinaryExpression(node)
        case Associativity.Right =>
          return visitRightAssociativeBinaryExpression(node)
        case _ =>
          Debug.fail("Unknown associativity.")
      }

    }
    def isCompoundAssignment(kind: BinaryOperator): Boolean = {
      return ((kind >= SyntaxKind.FirstCompoundAssignment) && (kind <= SyntaxKind.LastCompoundAssignment))

    }
    def getOperatorForCompoundAssignment(
        kind: CompoundAssignmentOperator): BitwiseOperatorOrHigher = {
      kind match {
        case SyntaxKind.PlusEqualsToken =>
          return SyntaxKind.PlusToken
        case SyntaxKind.MinusEqualsToken =>
          return SyntaxKind.MinusToken
        case SyntaxKind.AsteriskEqualsToken =>
          return SyntaxKind.AsteriskToken
        case SyntaxKind.AsteriskAsteriskEqualsToken =>
          return SyntaxKind.AsteriskAsteriskToken
        case SyntaxKind.SlashEqualsToken =>
          return SyntaxKind.SlashToken
        case SyntaxKind.PercentEqualsToken =>
          return SyntaxKind.PercentToken
        case SyntaxKind.LessThanLessThanEqualsToken =>
          return SyntaxKind.LessThanLessThanToken
        case SyntaxKind.GreaterThanGreaterThanEqualsToken =>
          return SyntaxKind.GreaterThanGreaterThanToken
        case SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken =>
          return SyntaxKind.GreaterThanGreaterThanGreaterThanToken
        case SyntaxKind.AmpersandEqualsToken =>
          return SyntaxKind.AmpersandToken
        case SyntaxKind.BarEqualsToken =>
          return SyntaxKind.BarToken
        case SyntaxKind.CaretEqualsToken =>
          return SyntaxKind.CaretToken
        case _ =>
      }

    }
    def visitRightAssociativeBinaryExpression(node: BinaryExpression) = {
      const fresh3 = node
      val left = fresh3.left
      val right = fresh3.right
      if (containsYield(right)) {
        var target: Expression = zeroOfMyType
        left.kind match {
          case SyntaxKind.PropertyAccessExpression =>
            (target = updatePropertyAccess(
              left.asInstanceOf[PropertyAccessExpression],
              cacheExpression(
                visitNode(
                  (left.asInstanceOf[PropertyAccessExpression]).expression,
                  visitor,
                  isLeftHandSideExpression)),
              (left.asInstanceOf[PropertyAccessExpression]).name))
          case SyntaxKind.ElementAccessExpression =>
            (target = updateElementAccess(
              left.asInstanceOf[ElementAccessExpression],
              cacheExpression(
                visitNode(
                  (left.asInstanceOf[ElementAccessExpression]).expression,
                  visitor,
                  isLeftHandSideExpression)),
              cacheExpression(
                visitNode(
                  (left
                    .asInstanceOf[ElementAccessExpression])
                    .argumentExpression,
                  visitor,
                  isExpression))))
          case _ =>
            (target = visitNode(left, visitor, isExpression))
        }
        val operator = node.operatorToken.kind
        if (isCompoundAssignment(operator)) {
          return createBinary(
            target,
            SyntaxKind.EqualsToken,
            createBinary(
              cacheExpression(target),
              getOperatorForCompoundAssignment(operator),
              visitNode(right, visitor, isExpression),
              node),
            node)

        } else {
          return updateBinary(
            node,
            target,
            visitNode(right, visitor, isExpression))

        }

      }
      return visitEachChild(node, visitor, context)

    }
    def visitLeftAssociativeBinaryExpression(node: BinaryExpression) = {
      if (containsYield(node.right)) {
        if (isLogicalOperator(node.operatorToken.kind)) {
          return visitLogicalBinaryExpression(node)

        } else if ((node.operatorToken.kind === SyntaxKind.CommaToken)) {
          return visitCommaExpression(node)

        }
        val clone = getMutableClone(node)
        (clone.left = cacheExpression(
          visitNode(node.left, visitor, isExpression)))
        (clone.right = visitNode(node.right, visitor, isExpression))
        return clone

      }
      return visitEachChild(node, visitor, context)

    }
    def visitLogicalBinaryExpression(node: BinaryExpression) = {
      val resultLabel = defineLabel()
      val resultLocal = declareLocal()
      emitAssignment(
        resultLocal,
        visitNode(node.left, visitor, isExpression),
        node.left)
      if ((node.operatorToken.kind === SyntaxKind.AmpersandAmpersandToken)) {
        emitBreakWhenFalse(resultLabel, resultLocal, node.left)

      } else {
        emitBreakWhenTrue(resultLabel, resultLocal, node.left)

      }
      emitAssignment(
        resultLocal,
        visitNode(node.right, visitor, isExpression),
        node.right)
      markLabel(resultLabel)
      return resultLocal

    }
    def visitCommaExpression(node: BinaryExpression) = {
      var pendingExpressions: Array[Expression] = Array()
      visit(node.left)
      visit(node.right)
      return inlineExpressions(pendingExpressions)
      def visit(node: Expression) = {
        if ((isBinaryExpression(node) && (node.operatorToken.kind === SyntaxKind.CommaToken))) {
          visit(node.left)
          visit(node.right)

        } else {
          if ((containsYield(node) && (pendingExpressions.length > 0))) {
            emitWorker(
              OpCode.Statement,
              Array(createStatement(inlineExpressions(pendingExpressions))))
            (pendingExpressions = Array())

          }
          pendingExpressions.push(visitNode(node, visitor, isExpression))

        }

      }

    }
    def visitConditionalExpression(node: ConditionalExpression): Expression = {
      if ((containsYield(node.whenTrue) || containsYield(node.whenFalse))) {
        val whenFalseLabel = defineLabel()
        val resultLabel = defineLabel()
        val resultLocal = declareLocal()
        emitBreakWhenFalse(
          whenFalseLabel,
          visitNode(node.condition, visitor, isExpression),
          node.condition)
        emitAssignment(
          resultLocal,
          visitNode(node.whenTrue, visitor, isExpression),
          node.whenTrue)
        emitBreak(resultLabel)
        markLabel(whenFalseLabel)
        emitAssignment(
          resultLocal,
          visitNode(node.whenFalse, visitor, isExpression),
          node.whenFalse)
        markLabel(resultLabel)
        return resultLocal

      }
      return visitEachChild(node, visitor, context)

    }
    def visitYieldExpression(node: YieldExpression): LeftHandSideExpression = {
      val resumeLabel = defineLabel()
      val expression = visitNode(node.expression, visitor, isExpression)
      if (node.asteriskToken) {
        emitYieldStar(expression, node)

      } else {
        emitYield(expression, node)

      }
      markLabel(resumeLabel)
      return createGeneratorResume()

    }
    def visitArrayLiteralExpression(node: ArrayLiteralExpression) = {
      return visitElements(node.elements, node.multiLine)

    }
    def visitElements(elements: NodeArray[Expression], _multiLine: Boolean) = {
      val numInitialElements = countInitialNodesWithoutYield(elements)
      val temp = declareLocal()
      var hasAssignedTemp = false
      if ((numInitialElements > 0)) {
        emitAssignment(
          temp,
          createArrayLiteral(
            visitNodes(
              elements,
              visitor,
              isExpression,
              0,
              numInitialElements)))
        (hasAssignedTemp = true)

      }
      val expressions = reduceLeft(
        elements,
        reduceElement,
        Array().asInstanceOf[Array[Expression]],
        numInitialElements)
      return (if (hasAssignedTemp)
                createArrayConcat(temp, Array(createArrayLiteral(expressions)))
              else createArrayLiteral(expressions))
      def reduceElement(expressions: Array[Expression], element: Expression) = {
        if ((containsYield(element) && (expressions.length > 0))) {
          emitAssignment(
            temp,
            (if (hasAssignedTemp)
               createArrayConcat(temp, Array(createArrayLiteral(expressions)))
             else createArrayLiteral(expressions)))
          (hasAssignedTemp = true)
          (expressions = Array())

        }
        expressions.push(visitNode(element, visitor, isExpression))
        return expressions

      }

    }
    def visitObjectLiteralExpression(node: ObjectLiteralExpression) = {
      val properties = node.properties
      val multiLine = node.multiLine
      val numInitialProperties = countInitialNodesWithoutYield(properties)
      val temp = declareLocal()
      emitAssignment(
        temp,
        createObjectLiteral(
          visitNodes(
            properties,
            visitor,
            isObjectLiteralElementLike,
            0,
            numInitialProperties),
          undefined,
          multiLine))
      val expressions = reduceLeft(
        properties,
        reduceProperty,
        Array().asInstanceOf[Array[Expression]],
        numInitialProperties)
      expressions.push(
        (if (multiLine) startOnNewLine(getMutableClone(temp)) else temp))
      return inlineExpressions(expressions)
      def reduceProperty(expressions: Array[Expression],
                         property: ObjectLiteralElementLike) = {
        if ((containsYield(property) && (expressions.length > 0))) {
          emitStatement(createStatement(inlineExpressions(expressions)))
          (expressions = Array())

        }
        val expression =
          createExpressionForObjectLiteralElementLike(node, property, temp)
        val visited = visitNode(expression, visitor, isExpression)
        if (visited) {
          if (multiLine) {
            (visited.startsOnNewLine = true)

          }
          expressions.push(visited)

        }
        return expressions

      }

    }
    def visitElementAccessExpression(node: ElementAccessExpression) = {
      if (containsYield(node.argumentExpression)) {
        val clone = getMutableClone(node)
        (clone.expression = cacheExpression(
          visitNode(node.expression, visitor, isLeftHandSideExpression)))
        (clone.argumentExpression =
          visitNode(node.argumentExpression, visitor, isExpression))
        return clone

      }
      return visitEachChild(node, visitor, context)

    }
    def visitCallExpression(node: CallExpression) = {
      if (forEach(node.arguments, containsYield)) {
        const fresh4 = createCallBinding(
          node.expression,
          hoistVariableDeclaration,
          languageVersion,
          true)
        val target = fresh4.target
        val thisArg = fresh4.thisArg
        return setOriginalNode(
          createFunctionApply(
            cacheExpression(
              visitNode(target, visitor, isLeftHandSideExpression)),
            thisArg,
            visitElements(node.arguments),
            node),
          node)

      }
      return visitEachChild(node, visitor, context)

    }
    def visitNewExpression(node: NewExpression) = {
      if (forEach(node.arguments, containsYield)) {
        const fresh5 = createCallBinding(
          createPropertyAccess(node.expression, "bind"),
          hoistVariableDeclaration)
        val target = fresh5.target
        val thisArg = fresh5.thisArg
        return setOriginalNode(
          createNew(
            createFunctionApply(
              cacheExpression(visitNode(target, visitor, isExpression)),
              thisArg,
              visitElements(node.arguments)),
            undefined,
            Array(),
            node),
          node)

      }
      return visitEachChild(node, visitor, context)

    }
    def transformAndEmitStatements(statements: Array[Statement],
                                   start: Nothing = 0) = {
      val numStatements = statements.length {
        var i = start
        while ((i < numStatements)) {
          {
            transformAndEmitStatement(statements(i))

          }
          (i += 1)
        }
      }

    }
    def transformAndEmitEmbeddedStatement(node: Statement) = {
      if (isBlock(node)) {
        transformAndEmitStatements(node.statements)

      } else {
        transformAndEmitStatement(node)

      }

    }
    def transformAndEmitStatement(node: Statement): Unit = {
      val savedInStatementContainingYield = inStatementContainingYield
      if ((!inStatementContainingYield)) {
        (inStatementContainingYield = containsYield(node))

      }
      transformAndEmitStatementWorker(node)
      (inStatementContainingYield = savedInStatementContainingYield)

    }
    def transformAndEmitStatementWorker(node: Statement): Unit = {
      node.kind match {
        case SyntaxKind.Block =>
          return transformAndEmitBlock(node.asInstanceOf[Block])
        case SyntaxKind.ExpressionStatement =>
          return transformAndEmitExpressionStatement(
            node.asInstanceOf[ExpressionStatement])
        case SyntaxKind.IfStatement =>
          return transformAndEmitIfStatement(node.asInstanceOf[IfStatement])
        case SyntaxKind.DoStatement =>
          return transformAndEmitDoStatement(node.asInstanceOf[DoStatement])
        case SyntaxKind.WhileStatement =>
          return transformAndEmitWhileStatement(
            node.asInstanceOf[WhileStatement])
        case SyntaxKind.ForStatement =>
          return transformAndEmitForStatement(node.asInstanceOf[ForStatement])
        case SyntaxKind.ForInStatement =>
          return transformAndEmitForInStatement(
            node.asInstanceOf[ForInStatement])
        case SyntaxKind.ContinueStatement =>
          return transformAndEmitContinueStatement(
            node.asInstanceOf[ContinueStatement])
        case SyntaxKind.BreakStatement =>
          return transformAndEmitBreakStatement(
            node.asInstanceOf[BreakStatement])
        case SyntaxKind.ReturnStatement =>
          return transformAndEmitReturnStatement(
            node.asInstanceOf[ReturnStatement])
        case SyntaxKind.WithStatement =>
          return transformAndEmitWithStatement(
            node.asInstanceOf[WithStatement])
        case SyntaxKind.SwitchStatement =>
          return transformAndEmitSwitchStatement(
            node.asInstanceOf[SwitchStatement])
        case SyntaxKind.LabeledStatement =>
          return transformAndEmitLabeledStatement(
            node.asInstanceOf[LabeledStatement])
        case SyntaxKind.ThrowStatement =>
          return transformAndEmitThrowStatement(
            node.asInstanceOf[ThrowStatement])
        case SyntaxKind.TryStatement =>
          return transformAndEmitTryStatement(node.asInstanceOf[TryStatement])
        case _ =>
          return emitStatement(visitNode(node, visitor, isStatement, true))
      }

    }
    def transformAndEmitBlock(node: Block): Unit = {
      if (containsYield(node)) {
        transformAndEmitStatements(node.statements)

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def transformAndEmitExpressionStatement(node: ExpressionStatement) = {
      emitStatement(visitNode(node, visitor, isStatement))

    }
    def transformAndEmitVariableDeclarationList(
        node: VariableDeclarationList): VariableDeclarationList = {
      (node.declarations).foreach { fresh6 =>
        val variable = zeroOfMyType = fresh6 {
          hoistVariableDeclaration(variable.name.asInstanceOf[Identifier])

        }
      }
      val variables = getInitializedVariables(node)
      val numVariables = variables.length
      var variablesWritten = 0
      var pendingExpressions: Array[Expression] = Array()
      while ((variablesWritten < numVariables)) {
        {
          {
            var i = variablesWritten
            while ((i < numVariables)) {
              {
                val variable = variables(i)
                if ((containsYield(variable.initializer) && (pendingExpressions.length > 0))) {
                  break()

                }
                pendingExpressions.push(transformInitializedVariable(variable))

              }
              (i += 1)
            }
          }
          if (pendingExpressions.length) {
            emitStatement(
              createStatement(inlineExpressions(pendingExpressions)))
            (variablesWritten += pendingExpressions.length)
            (pendingExpressions = Array())

          }

        }
      }
      return undefined

    }
    def transformInitializedVariable(node: VariableDeclaration) = {
      return createAssignment(
        getSynthesizedClone(node.name).asInstanceOf[Identifier],
        visitNode(node.initializer, visitor, isExpression))

    }
    def transformAndEmitIfStatement(node: IfStatement) = {
      if (containsYield(node)) {
        if ((containsYield(node.thenStatement) || containsYield(
              node.elseStatement))) {
          val endLabel = defineLabel()
          val elseLabel =
            (if (node.elseStatement) defineLabel() else undefined)
          emitBreakWhenFalse(
            (if (node.elseStatement) elseLabel else endLabel),
            visitNode(node.expression, visitor, isExpression))
          transformAndEmitEmbeddedStatement(node.thenStatement)
          if (node.elseStatement) {
            emitBreak(endLabel)
            markLabel(elseLabel)
            transformAndEmitEmbeddedStatement(node.elseStatement)

          }
          markLabel(endLabel)

        } else {
          emitStatement(visitNode(node, visitor, isStatement))

        }

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def transformAndEmitDoStatement(node: DoStatement) = {
      if (containsYield(node)) {
        val conditionLabel = defineLabel()
        val loopLabel = defineLabel()
        beginLoopBlock(conditionLabel)
        markLabel(loopLabel)
        transformAndEmitEmbeddedStatement(node.statement)
        markLabel(conditionLabel)
        emitBreakWhenTrue(
          loopLabel,
          visitNode(node.expression, visitor, isExpression))
        endLoopBlock()

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def visitDoStatement(node: DoStatement) = {
      if (inStatementContainingYield) {
        beginScriptLoopBlock()
        (node = visitEachChild(node, visitor, context))
        endLoopBlock()
        return node

      } else {
        return visitEachChild(node, visitor, context)

      }

    }
    def transformAndEmitWhileStatement(node: WhileStatement) = {
      if (containsYield(node)) {
        val loopLabel = defineLabel()
        val endLabel = beginLoopBlock(loopLabel)
        markLabel(loopLabel)
        emitBreakWhenFalse(
          endLabel,
          visitNode(node.expression, visitor, isExpression))
        transformAndEmitEmbeddedStatement(node.statement)
        emitBreak(loopLabel)
        endLoopBlock()

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def visitWhileStatement(node: WhileStatement) = {
      if (inStatementContainingYield) {
        beginScriptLoopBlock()
        (node = visitEachChild(node, visitor, context))
        endLoopBlock()
        return node

      } else {
        return visitEachChild(node, visitor, context)

      }

    }
    def transformAndEmitForStatement(node: ForStatement) = {
      if (containsYield(node)) {
        val conditionLabel = defineLabel()
        val incrementLabel = defineLabel()
        val endLabel = beginLoopBlock(incrementLabel)
        if (node.initializer) {
          val initializer = node.initializer
          if (isVariableDeclarationList(initializer)) {
            transformAndEmitVariableDeclarationList(initializer)

          } else {
            emitStatement(
              createStatement(
                visitNode(initializer, visitor, isExpression),
                initializer))

          }

        }
        markLabel(conditionLabel)
        if (node.condition) {
          emitBreakWhenFalse(
            endLabel,
            visitNode(node.condition, visitor, isExpression))

        }
        transformAndEmitEmbeddedStatement(node.statement)
        markLabel(incrementLabel)
        if (node.incrementor) {
          emitStatement(
            createStatement(
              visitNode(node.incrementor, visitor, isExpression),
              node.incrementor))

        }
        emitBreak(conditionLabel)
        endLoopBlock()

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def visitForStatement(node: ForStatement) = {
      if (inStatementContainingYield) {
        beginScriptLoopBlock()

      }
      val initializer = node.initializer
      if (isVariableDeclarationList(initializer)) {
        (initializer.declarations).foreach { fresh7 =>
          val variable = zeroOfMyType = fresh7 {
            hoistVariableDeclaration(variable.name.asInstanceOf[Identifier])

          }
        }
        val variables = getInitializedVariables(initializer)
        (node = updateFor(
          node,
          (if ((variables.length > 0))
             inlineExpressions(map(variables, transformInitializedVariable))
           else undefined),
          visitNode(node.condition, visitor, isExpression, true),
          visitNode(node.incrementor, visitor, isExpression, true),
          visitNode(node.statement, visitor, isStatement, false, liftToBlock)))

      } else {
        (node = visitEachChild(node, visitor, context))

      }
      if (inStatementContainingYield) {
        endLoopBlock()

      }
      return node

    }
    def transformAndEmitForInStatement(node: ForInStatement) = {
      if (containsYield(node)) {
        val keysArray = declareLocal()
        val key = declareLocal()
        val keysIndex = createLoopVariable()
        val initializer = node.initializer
        hoistVariableDeclaration(keysIndex)
        emitAssignment(keysArray, createArrayLiteral())
        emitStatement(
          createForIn(
            key,
            visitNode(node.expression, visitor, isExpression),
            createStatement(
              createCall(
                createPropertyAccess(keysArray, "push"),
                undefined,
                Array(key)))))
        emitAssignment(keysIndex, createLiteral(0))
        val conditionLabel = defineLabel()
        val incrementLabel = defineLabel()
        val endLabel = beginLoopBlock(incrementLabel)
        markLabel(conditionLabel)
        emitBreakWhenFalse(
          endLabel,
          createLessThan(keysIndex, createPropertyAccess(keysArray, "length")))
        var variable: Expression = zeroOfMyType
        if (isVariableDeclarationList(initializer)) {
          (initializer.declarations).foreach { fresh8 =>
            val variable = zeroOfMyType = fresh8 {
              hoistVariableDeclaration(variable.name.asInstanceOf[Identifier])

            }
          }
          (variable = getSynthesizedClone(initializer.declarations(0).name)
            .asInstanceOf[Identifier])

        } else {
          (variable = visitNode(initializer, visitor, isExpression))
          Debug.assert(isLeftHandSideExpression(variable))

        }
        emitAssignment(variable, createElementAccess(keysArray, keysIndex))
        transformAndEmitEmbeddedStatement(node.statement)
        markLabel(incrementLabel)
        emitStatement(createStatement(createPostfixIncrement(keysIndex)))
        emitBreak(conditionLabel)
        endLoopBlock()

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def visitForInStatement(node: ForInStatement) = {
      if (inStatementContainingYield) {
        beginScriptLoopBlock()

      }
      val initializer = node.initializer
      if (isVariableDeclarationList(initializer)) {
        (initializer.declarations).foreach { fresh9 =>
          val variable = zeroOfMyType = fresh9 {
            hoistVariableDeclaration(variable.name.asInstanceOf[Identifier])

          }
        }
        (node = updateForIn(
          node,
          initializer.declarations(0).name.asInstanceOf[Identifier],
          visitNode(node.expression, visitor, isExpression),
          visitNode(node.statement, visitor, isStatement, false, liftToBlock)))

      } else {
        (node = visitEachChild(node, visitor, context))

      }
      if (inStatementContainingYield) {
        endLoopBlock()

      }
      return node

    }
    def transformAndEmitContinueStatement(node: ContinueStatement): Unit = {
      val label =
        findContinueTarget((if (node.label) node.label.text else undefined))
      Debug.assert(
        (label > 0),
        "Expected continue statment to point to a valid Label.")
      emitBreak(label, node)

    }
    def visitContinueStatement(node: ContinueStatement): Statement = {
      if (inStatementContainingYield) {
        val label = findContinueTarget((node.label && node.label.text))
        if ((label > 0)) {
          return createInlineBreak(label, node)

        }

      }
      return visitEachChild(node, visitor, context)

    }
    def transformAndEmitBreakStatement(node: BreakStatement): Unit = {
      val label =
        findBreakTarget((if (node.label) node.label.text else undefined))
      Debug.assert(
        (label > 0),
        "Expected break statment to point to a valid Label.")
      emitBreak(label, node)

    }
    def visitBreakStatement(node: BreakStatement): Statement = {
      if (inStatementContainingYield) {
        val label = findBreakTarget((node.label && node.label.text))
        if ((label > 0)) {
          return createInlineBreak(label, node)

        }

      }
      return visitEachChild(node, visitor, context)

    }
    def transformAndEmitReturnStatement(node: ReturnStatement): Unit = {
      emitReturn(visitNode(node.expression, visitor, isExpression, true), node)

    }
    def visitReturnStatement(node: ReturnStatement) = {
      return createInlineReturn(
        visitNode(node.expression, visitor, isExpression, true),
        node)

    }
    def transformAndEmitWithStatement(node: WithStatement) = {
      if (containsYield(node)) {
        beginWithBlock(
          cacheExpression(visitNode(node.expression, visitor, isExpression)))
        transformAndEmitEmbeddedStatement(node.statement)
        endWithBlock()

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def transformAndEmitSwitchStatement(node: SwitchStatement) = {
      if (containsYield(node.caseBlock)) {
        val caseBlock = node.caseBlock
        val numClauses = caseBlock.clauses.length
        val endLabel = beginSwitchBlock()
        val expression =
          cacheExpression(visitNode(node.expression, visitor, isExpression))
        val clauseLabels: Array[Label] = Array()
        var defaultClauseIndex = (-1) {
          var i = 0
          while ((i < numClauses)) {
            {
              val clause = caseBlock.clauses(i)
              clauseLabels.push(defineLabel())
              if (((clause.kind === SyntaxKind.DefaultClause) && (defaultClauseIndex === (-1)))) {
                (defaultClauseIndex = i)

              }

            }
            (i += 1)
          }
        }
        var clausesWritten = 0
        var pendingClauses: Array[CaseClause] = Array()
        while ((clausesWritten < numClauses)) {
          {
            var defaultClausesSkipped = 0 {
              var i = clausesWritten
              while ((i < numClauses)) {
                {
                  val clause = caseBlock.clauses(i)
                  if ((clause.kind === SyntaxKind.CaseClause)) {
                    val caseClause = clause.asInstanceOf[CaseClause]
                    if ((containsYield(caseClause.expression) && (pendingClauses.length > 0))) {
                      break()

                    }
                    pendingClauses.push(
                      createCaseClause(
                        visitNode(
                          caseClause.expression,
                          visitor,
                          isExpression),
                        Array(
                          createInlineBreak(
                            clauseLabels(i),
                            caseClause.expression))))

                  } else {
                    (defaultClausesSkipped += 1)

                  }

                }
                (i += 1)
              }
            }
            if (pendingClauses.length) {
              emitStatement(
                createSwitch(expression, createCaseBlock(pendingClauses)))
              (clausesWritten += pendingClauses.length)
              (pendingClauses = Array())

            }
            if ((defaultClausesSkipped > 0)) {
              (clausesWritten += defaultClausesSkipped)
              (defaultClausesSkipped = 0)

            }

          }
        }
        if ((defaultClauseIndex >= 0)) {
          emitBreak(clauseLabels(defaultClauseIndex))

        } else {
          emitBreak(endLabel)

        }
        {
          var i = 0
          while ((i < numClauses)) {
            {
              markLabel(clauseLabels(i))
              transformAndEmitStatements(caseBlock.clauses(i).statements)

            }
            (i += 1)
          }
        }
        endSwitchBlock()

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def visitSwitchStatement(node: SwitchStatement) = {
      if (inStatementContainingYield) {
        beginScriptSwitchBlock()

      }
      (node = visitEachChild(node, visitor, context))
      if (inStatementContainingYield) {
        endSwitchBlock()

      }
      return node

    }
    def transformAndEmitLabeledStatement(node: LabeledStatement) = {
      if (containsYield(node)) {
        beginLabeledBlock(node.label.text)
        transformAndEmitEmbeddedStatement(node.statement)
        endLabeledBlock()

      } else {
        emitStatement(visitNode(node, visitor, isStatement))

      }

    }
    def visitLabeledStatement(node: LabeledStatement) = {
      if (inStatementContainingYield) {
        beginScriptLabeledBlock(node.label.text)

      }
      (node = visitEachChild(node, visitor, context))
      if (inStatementContainingYield) {
        endLabeledBlock()

      }
      return node

    }
    def transformAndEmitThrowStatement(node: ThrowStatement): Unit = {
      emitThrow(visitNode(node.expression, visitor, isExpression), node)

    }
    def transformAndEmitTryStatement(node: TryStatement) = {
      if (containsYield(node)) {
        beginExceptionBlock()
        transformAndEmitEmbeddedStatement(node.tryBlock)
        if (node.catchClause) {
          beginCatchBlock(node.catchClause.variableDeclaration)
          transformAndEmitEmbeddedStatement(node.catchClause.block)

        }
        if (node.finallyBlock) {
          beginFinallyBlock()
          transformAndEmitEmbeddedStatement(node.finallyBlock)

        }
        endExceptionBlock()

      } else {
        emitStatement(visitEachChild(node, visitor, context))

      }

    }
    def containsYield(node: Node) = {
      return (node && (((node.transformFlags & TransformFlags.ContainsYield)) !== 0))

    }
    def countInitialNodesWithoutYield(nodes: NodeArray[Node]) = {
      val numNodes = nodes.length {
        var i = 0
        while ((i < numNodes)) {
          {
            if (containsYield(nodes(i))) {
              return i

            }

          }
          (i += 1)
        }
      }
      return (-1)

    }
    def onSubstituteNode(emitContext: EmitContext, node: Node): Node = {
      (node = previousOnSubstituteNode(emitContext, node))
      if ((emitContext === EmitContext.Expression)) {
        return substituteExpression(node.asInstanceOf[Expression])

      }
      return node

    }
    def substituteExpression(node: Expression): Expression = {
      if (isIdentifier(node)) {
        return substituteExpressionIdentifier(node)

      }
      return node

    }
    def substituteExpressionIdentifier(node: Identifier) = {
      if ((renamedCatchVariables && hasProperty(
            renamedCatchVariables,
            node.text))) {
        val original = getOriginalNode(node)
        if ((isIdentifier(original) && original.parent)) {
          val declaration = resolver.getReferencedValueDeclaration(original)
          if (declaration) {
            val name = getProperty(
              renamedCatchVariableDeclarations,
              String(getOriginalNodeId(declaration)))
            if (name) {
              val clone = getMutableClone(name)
              setSourceMapRange(clone, node)
              setCommentRange(clone, node)
              return clone

            }

          }

        }

      }
      return node

    }
    def cacheExpression(node: Expression): Identifier = {
      var temp: Identifier = zeroOfMyType
      if (isGeneratedIdentifier(node)) {
        return node.asInstanceOf[Identifier]

      }
      (temp = createTempVariable(hoistVariableDeclaration))
      emitAssignment(temp, node, node)
      return temp

    }
    def declareLocal(name: String): Identifier = {
      val temp =
        (if (name) createUniqueName(name) else createTempVariable(undefined))
      hoistVariableDeclaration(temp)
      return temp

    }
    def defineLabel(): Label = {
      if ((!labelOffsets)) {
        (labelOffsets = Array())

      }
      val label = nextLabelId
      (nextLabelId += 1)
      (labelOffsets(label) = (-1))
      return label

    }
    def markLabel(label: Label): Unit = {
      Debug.assert((labelOffsets !== undefined), "No labels were defined.")
      (labelOffsets(label) = (if (operations) operations.length else 0))

    }
    def beginBlock(block: CodeBlock): Int = {
      if ((!blocks)) {
        (blocks = Array())
        (blockActions = Array())
        (blockOffsets = Array())
        (blockStack = Array())

      }
      val index = blockActions.length
      (blockActions(index) = BlockAction.Open)
      (blockOffsets(index) = (if (operations) operations.length else 0))
      (blocks(index) = block)
      blockStack.push(block)
      return index

    }
    def endBlock(): CodeBlock = {
      val block = peekBlock()
      Debug.assert((block !== undefined), "beginBlock was never called.")
      val index = blockActions.length
      (blockActions(index) = BlockAction.Close)
      (blockOffsets(index) = (if (operations) operations.length else 0))
      (blocks(index) = block)
      blockStack.pop()
      return block

    }
    def peekBlock() = {
      return lastOrUndefined(blockStack)

    }
    def peekBlockKind(): CodeBlockKind = {
      val block = peekBlock()
      return (block && block.kind)

    }
    def beginWithBlock(expression: Identifier): Unit = {
      val startLabel = defineLabel()
      val endLabel = defineLabel()
      markLabel(startLabel)
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.With,
          "expression" -> expression,
          "startLabel" -> startLabel,
          "endLabel" -> endLabel).asInstanceOf[WithBlock])

    }
    def endWithBlock(): Unit = {
      Debug.assert((peekBlockKind() === CodeBlockKind.With))
      val block = endBlock().asInstanceOf[WithBlock]
      markLabel(block.endLabel)

    }
    def isWithBlock(block: CodeBlock): Boolean = {
      return (block.kind === CodeBlockKind.With)

    }
    def beginExceptionBlock(): Label = {
      val startLabel = defineLabel()
      val endLabel = defineLabel()
      markLabel(startLabel)
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.Exception,
          "state" -> ExceptionBlockState.Try,
          "startLabel" -> startLabel,
          "endLabel" -> endLabel).asInstanceOf[ExceptionBlock])
      emitNop()
      return endLabel

    }
    def beginCatchBlock(variable: VariableDeclaration): Unit = {
      Debug.assert((peekBlockKind() === CodeBlockKind.Exception))
      val text = (variable.name.asInstanceOf[Identifier]).text
      val name = declareLocal(text)
      if ((!renamedCatchVariables)) {
        (renamedCatchVariables = createMap[Boolean]())
        (renamedCatchVariableDeclarations = createMap[Identifier]())
        context.enableSubstitution(SyntaxKind.Identifier)

      }
      (renamedCatchVariables(text) = true)
      (renamedCatchVariableDeclarations(getOriginalNodeId(variable)) = name)
      val exception = peekBlock().asInstanceOf[ExceptionBlock]
      Debug.assert((exception.state < ExceptionBlockState.Catch))
      val endLabel = exception.endLabel
      emitBreak(endLabel)
      val catchLabel = defineLabel()
      markLabel(catchLabel)
      (exception.state = ExceptionBlockState.Catch)
      (exception.catchVariable = name)
      (exception.catchLabel = catchLabel)
      emitAssignment(
        name,
        createCall(createPropertyAccess(state, "sent"), undefined, Array()))
      emitNop()

    }
    def beginFinallyBlock(): Unit = {
      Debug.assert((peekBlockKind() === CodeBlockKind.Exception))
      val exception = peekBlock().asInstanceOf[ExceptionBlock]
      Debug.assert((exception.state < ExceptionBlockState.Finally))
      val endLabel = exception.endLabel
      emitBreak(endLabel)
      val finallyLabel = defineLabel()
      markLabel(finallyLabel)
      (exception.state = ExceptionBlockState.Finally)
      (exception.finallyLabel = finallyLabel)

    }
    def endExceptionBlock(): Unit = {
      Debug.assert((peekBlockKind() === CodeBlockKind.Exception))
      val exception = endBlock().asInstanceOf[ExceptionBlock]
      val state = exception.state
      if ((state < ExceptionBlockState.Finally)) {
        emitBreak(exception.endLabel)

      } else {
        emitEndfinally()

      }
      markLabel(exception.endLabel)
      emitNop()
      (exception.state = ExceptionBlockState.Done)

    }
    def isExceptionBlock(block: CodeBlock): Boolean = {
      return (block.kind === CodeBlockKind.Exception)

    }
    def beginScriptLoopBlock(): Unit = {
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.Loop,
          "isScript" -> true,
          "breakLabel" -> (-1),
          "continueLabel" -> (-1)).asInstanceOf[LoopBlock])

    }
    def beginLoopBlock(continueLabel: Label): Label = {
      val breakLabel = defineLabel()
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.Loop,
          "isScript" -> false,
          "breakLabel" -> breakLabel,
          "continueLabel" -> continueLabel).asInstanceOf[LoopBlock])
      return breakLabel

    }
    def endLoopBlock(): Unit = {
      Debug.assert((peekBlockKind() === CodeBlockKind.Loop))
      val block = endBlock().asInstanceOf[SwitchBlock]
      val breakLabel = block.breakLabel
      if ((!block.isScript)) {
        markLabel(breakLabel)

      }

    }
    def beginScriptSwitchBlock(): Unit = {
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.Switch,
          "isScript" -> true,
          "breakLabel" -> (-1)).asInstanceOf[SwitchBlock])

    }
    def beginSwitchBlock(): Label = {
      val breakLabel = defineLabel()
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.Switch,
          "isScript" -> false,
          "breakLabel" -> breakLabel).asInstanceOf[SwitchBlock])
      return breakLabel

    }
    def endSwitchBlock(): Unit = {
      Debug.assert((peekBlockKind() === CodeBlockKind.Switch))
      val block = endBlock().asInstanceOf[SwitchBlock]
      val breakLabel = block.breakLabel
      if ((!block.isScript)) {
        markLabel(breakLabel)

      }

    }
    def beginScriptLabeledBlock(labelText: String) = {
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.Labeled,
          "isScript" -> true,
          "labelText" -> labelText,
          "breakLabel" -> (-1)).asInstanceOf[LabeledBlock])

    }
    def beginLabeledBlock(labelText: String) = {
      val breakLabel = defineLabel()
      beginBlock(
        Map(
          "kind" -> CodeBlockKind.Labeled,
          "isScript" -> false,
          "labelText" -> labelText,
          "breakLabel" -> breakLabel).asInstanceOf[LabeledBlock])

    }
    def endLabeledBlock() = {
      Debug.assert((peekBlockKind() === CodeBlockKind.Labeled))
      val block = endBlock().asInstanceOf[LabeledBlock]
      if ((!block.isScript)) {
        markLabel(block.breakLabel)

      }

    }
    def supportsUnlabeledBreak(block: CodeBlock): Boolean = {
      return ((block.kind === CodeBlockKind.Switch) || (block.kind === CodeBlockKind.Loop))

    }
    def supportsLabeledBreakOrContinue(block: CodeBlock): Boolean = {
      return (block.kind === CodeBlockKind.Labeled)

    }
    def supportsUnlabeledContinue(block: CodeBlock): Boolean = {
      return (block.kind === CodeBlockKind.Loop)

    }
    def hasImmediateContainingLabeledBlock(labelText: String, start: Int) = {
      {
        var j = start
        while ((j >= 0)) {
          {
            val containingBlock = blockStack(j)
            if (supportsLabeledBreakOrContinue(containingBlock)) {
              if ((containingBlock.labelText === labelText)) {
                return true

              }

            } else {
              break()

            }

          }
          (j -= 1)
        }
      }
      return false

    }
    def findBreakTarget(labelText: String): Label = {
      Debug.assert((blocks !== undefined))
      if (labelText) {
        {
          var i = (blockStack.length - 1)
          while ((i >= 0)) {
            {
              val block = blockStack(i)
              if ((supportsLabeledBreakOrContinue(block) && (block.labelText === labelText))) {
                return block.breakLabel

              } else if ((supportsUnlabeledBreak(block) && hasImmediateContainingLabeledBlock(
                           labelText,
                           (i - 1)))) {
                return block.breakLabel

              }

            }
            (i -= 1)
          }
        }

      } else {
        {
          var i = (blockStack.length - 1)
          while ((i >= 0)) {
            {
              val block = blockStack(i)
              if (supportsUnlabeledBreak(block)) {
                return block.breakLabel

              }

            }
            (i -= 1)
          }
        }

      }
      return 0

    }
    def findContinueTarget(labelText: String): Label = {
      Debug.assert((blocks !== undefined))
      if (labelText) {
        {
          var i = (blockStack.length - 1)
          while ((i >= 0)) {
            {
              val block = blockStack(i)
              if ((supportsUnlabeledContinue(block) && hasImmediateContainingLabeledBlock(
                    labelText,
                    (i - 1)))) {
                return block.continueLabel

              }

            }
            (i -= 1)
          }
        }

      } else {
        {
          var i = (blockStack.length - 1)
          while ((i >= 0)) {
            {
              val block = blockStack(i)
              if (supportsUnlabeledContinue(block)) {
                return block.continueLabel

              }

            }
            (i -= 1)
          }
        }

      }
      return 0

    }
    def createLabel(label: Label): Expression = {
      if ((label > 0)) {
        if ((labelExpressions === undefined)) {
          (labelExpressions = Array())

        }
        val expression = createSynthesizedNode(SyntaxKind.NumericLiteral)
          .asInstanceOf[LiteralExpression]
        if ((labelExpressions(label) === undefined)) {
          (labelExpressions(label) = Array(expression))

        } else {
          labelExpressions(label).push(expression)

        }
        return expression

      }
      return createNode(SyntaxKind.OmittedExpression)
        .asInstanceOf[OmittedExpression]

    }
    def createInstruction(instruction: Instruction): NumericLiteral = {
      val literal = createLiteral(instruction)
      (literal.trailingComment = instructionNames(instruction))
      return literal

    }
    def createInlineBreak(label: Label, location: TextRange): ReturnStatement = {
      Debug.assert((label > 0), s"""Invalid label: ${label}""")
      return createReturn(
        createArrayLiteral(
          Array(createInstruction(Instruction.Break), createLabel(label))),
        location)

    }
    def createInlineReturn(expression: Expression,
                           location: TextRange): ReturnStatement = {
      return createReturn(
        createArrayLiteral(
          (if (expression)
             Array(createInstruction(Instruction.Return), expression)
           else Array(createInstruction(Instruction.Return)))),
        location)

    }
    def createGeneratorResume(location: TextRange): LeftHandSideExpression = {
      return createCall(
        createPropertyAccess(state, "sent"),
        undefined,
        Array(),
        location)

    }
    def emitNop() = {
      emitWorker(OpCode.Nop)

    }
    def emitStatement(node: Statement): Unit = {
      if (node) {
        emitWorker(OpCode.Statement, Array(node))

      } else {
        emitNop()

      }

    }
    def emitAssignment(left: Expression,
                       right: Expression,
                       location: TextRange): Unit = {
      emitWorker(OpCode.Assign, Array(left, right), location)

    }
    def emitBreak(label: Label, location: TextRange): Unit = {
      emitWorker(OpCode.Break, Array(label), location)

    }
    def emitBreakWhenTrue(label: Label,
                          condition: Expression,
                          location: TextRange): Unit = {
      emitWorker(OpCode.BreakWhenTrue, Array(label, condition), location)

    }
    def emitBreakWhenFalse(label: Label,
                           condition: Expression,
                           location: TextRange): Unit = {
      emitWorker(OpCode.BreakWhenFalse, Array(label, condition), location)

    }
    def emitYieldStar(expression: Expression, location: TextRange): Unit = {
      emitWorker(OpCode.YieldStar, Array(expression), location)

    }
    def emitYield(expression: Expression, location: TextRange): Unit = {
      emitWorker(OpCode.Yield, Array(expression), location)

    }
    def emitReturn(expression: Expression, location: TextRange): Unit = {
      emitWorker(OpCode.Return, Array(expression), location)

    }
    def emitThrow(expression: Expression, location: TextRange): Unit = {
      emitWorker(OpCode.Throw, Array(expression), location)

    }
    def emitEndfinally(): Unit = {
      emitWorker(OpCode.Endfinally)

    }
    def emitWorker(code: OpCode,
                   args: OperationArguments,
                   location: TextRange): Unit = {
      if ((operations === undefined)) {
        (operations = Array())
        (operationArguments = Array())
        (operationLocations = Array())

      }
      if ((labelOffsets === undefined)) {
        markLabel(defineLabel())

      }
      val operationIndex = operations.length
      (operations(operationIndex) = code)
      (operationArguments(operationIndex) = args)
      (operationLocations(operationIndex) = location)

    }
    def build() = {
      (blockIndex = 0)
      (labelNumber = 0)
      (labelNumbers = undefined)
      (lastOperationWasAbrupt = false)
      (lastOperationWasCompletion = false)
      (clauses = undefined)
      (statements = undefined)
      (exceptionBlockStack = undefined)
      (currentExceptionBlock = undefined)
      (withBlockStack = undefined)
      val buildResult = buildStatements()
      return createCall(
        createHelperName(
          currentSourceFile.externalHelpersModuleName,
          "__generator"),
        undefined,
        Array(
          createThis(),
          setEmitFlags(
            createFunctionExpression(
              undefined,
              undefined,
              undefined,
              undefined,
              Array(createParameter(state)),
              undefined,
              createBlock(buildResult, undefined, (buildResult.length > 0))),
            EmitFlags.ReuseTempVariableScope)))

    }
    def buildStatements(): Array[Statement] = {
      if (operations) {
        {
          var operationIndex = 0
          while ((operationIndex < operations.length)) {
            {
              writeOperation(operationIndex)

            }
            (operationIndex += 1)
          }
        }
        flushFinalLabel(operations.length)

      } else {
        flushFinalLabel(0)

      }
      if (clauses) {
        val labelExpression = createPropertyAccess(state, "label")
        val switchStatement =
          createSwitch(labelExpression, createCaseBlock(clauses))
        (switchStatement.startsOnNewLine = true)
        return Array(switchStatement)

      }
      if (statements) {
        return statements

      }
      return Array()

    }
    def flushLabel(): Unit = {
      if ((!statements)) {
        return

      }
      appendLabel((!lastOperationWasAbrupt))
      (lastOperationWasAbrupt = false)
      (lastOperationWasCompletion = false)
      (labelNumber += 1)

    }
    def flushFinalLabel(operationIndex: Int): Unit = {
      if (isFinalLabelReachable(operationIndex)) {
        tryEnterLabel(operationIndex)
        (withBlockStack = undefined)
        writeReturn(undefined, undefined)

      }
      if ((statements && clauses)) {
        appendLabel(false)

      }
      updateLabelExpressions()

    }
    def isFinalLabelReachable(operationIndex: Int) = {
      if ((!lastOperationWasCompletion)) {
        return true

      }
      if (((!labelOffsets) || (!labelExpressions))) {
        return false

      }
      {
        var label = 0
        while ((label < labelOffsets.length)) {
          {
            if (((labelOffsets(label) === operationIndex) && labelExpressions(
                  label))) {
              return true

            }

          }
          (label += 1)
        }
      }
      return false

    }
    def appendLabel(markLabelEnd: Boolean): Unit = {
      if ((!clauses)) {
        (clauses = Array())

      }
      if (statements) {
        if (withBlockStack) {
          {
            var i = (withBlockStack.length - 1)
            while ((i >= 0)) {
              {
                val withBlock = withBlockStack(i)
                (statements = Array(
                  createWith(withBlock.expression, createBlock(statements))))

              }
              (i -= 1)
            }
          }

        }
        if (currentExceptionBlock) {
          const fresh10 = currentExceptionBlock
          val startLabel = fresh10.startLabel
          val catchLabel = fresh10.catchLabel
          val finallyLabel = fresh10.finallyLabel
          val endLabel = fresh10.endLabel
          statements.unshift(
            createStatement(
              createCall(
                createPropertyAccess(
                  createPropertyAccess(state, "trys"),
                  "push"),
                undefined,
                Array(
                  createArrayLiteral(
                    Array(
                      createLabel(startLabel),
                      createLabel(catchLabel),
                      createLabel(finallyLabel),
                      createLabel(endLabel)))))))
          (currentExceptionBlock = undefined)

        }
        if (markLabelEnd) {
          statements.push(
            createStatement(
              createAssignment(
                createPropertyAccess(state, "label"),
                createLiteral((labelNumber + 1)))))

        }

      }
      clauses.push(
        createCaseClause(createLiteral(labelNumber), (statements || Array())))
      (statements = undefined)

    }
    def tryEnterLabel(operationIndex: Int): Unit = {
      if ((!labelOffsets)) {
        return

      }
      {
        var label = 0
        while ((label < labelOffsets.length)) {
          {
            if ((labelOffsets(label) === operationIndex)) {
              flushLabel()
              if ((labelNumbers === undefined)) {
                (labelNumbers = Array())

              }
              if ((labelNumbers(labelNumber) === undefined)) {
                (labelNumbers(labelNumber) = Array(label))

              } else {
                labelNumbers(labelNumber).push(label)

              }

            }

          }
          (label += 1)
        }
      }

    }
    def updateLabelExpressions() = {
      if (((labelExpressions !== undefined) && (labelNumbers !== undefined))) {
        {
          var labelNumber = 0
          while ((labelNumber < labelNumbers.length)) {
            {
              val labels = labelNumbers(labelNumber)
              if ((labels !== undefined)) {
                (labels).foreach { fresh11 =>
                  val label = zeroOfMyType = fresh11 {
                    val expressions = labelExpressions(label)
                    if ((expressions !== undefined)) {
                      (expressions).foreach { fresh12 =>
                        val expression = zeroOfMyType = fresh12 {
                          (expression.text = String(labelNumber))

                        }
                      }

                    }

                  }
                }

              }

            }
            (labelNumber += 1)
          }
        }

      }

    }
    def tryEnterOrLeaveBlock(operationIndex: Int): Unit = {
      if (blocks) {
        {
          while (((blockIndex < blockActions.length) && (blockOffsets(
                   blockIndex) <= operationIndex))) {
            {
              val block = blocks(blockIndex)
              val blockAction = blockActions(blockIndex)
              if (isExceptionBlock(block)) {
                if ((blockAction === BlockAction.Open)) {
                  if ((!exceptionBlockStack)) {
                    (exceptionBlockStack = Array())

                  }
                  if ((!statements)) {
                    (statements = Array())

                  }
                  exceptionBlockStack.push(currentExceptionBlock)
                  (currentExceptionBlock = block)

                } else if ((blockAction === BlockAction.Close)) {
                  (currentExceptionBlock = exceptionBlockStack.pop())

                }

              } else if (isWithBlock(block)) {
                if ((blockAction === BlockAction.Open)) {
                  if ((!withBlockStack)) {
                    (withBlockStack = Array())

                  }
                  withBlockStack.push(block)

                } else if ((blockAction === BlockAction.Close)) {
                  withBlockStack.pop()

                }

              }

            }
            (blockIndex += 1)
          }
        }

      }

    }
    def writeOperation(operationIndex: Int): Unit = {
      tryEnterLabel(operationIndex)
      tryEnterOrLeaveBlock(operationIndex)
      if (lastOperationWasAbrupt) {
        return

      }
      (lastOperationWasAbrupt = false)
      (lastOperationWasCompletion = false)
      val opcode = operations(operationIndex)
      if ((opcode === OpCode.Nop)) {
        return

      } else if ((opcode === OpCode.Endfinally)) {
        return writeEndfinally()

      }
      val args = operationArguments(operationIndex)
      if ((opcode === OpCode.Statement)) {
        return writeStatement(args(0).asInstanceOf[Statement])

      }
      val location = operationLocations(operationIndex)
      opcode match {
        case OpCode.Assign =>
          return writeAssign(
            args(0).asInstanceOf[Expression],
            args(1).asInstanceOf[Expression],
            location)
        case OpCode.Break =>
          return writeBreak(args(0).asInstanceOf[Label], location)
        case OpCode.BreakWhenTrue =>
          return writeBreakWhenTrue(
            args(0).asInstanceOf[Label],
            args(1).asInstanceOf[Expression],
            location)
        case OpCode.BreakWhenFalse =>
          return writeBreakWhenFalse(
            args(0).asInstanceOf[Label],
            args(1).asInstanceOf[Expression],
            location)
        case OpCode.Yield =>
          return writeYield(args(0).asInstanceOf[Expression], location)
        case OpCode.YieldStar =>
          return writeYieldStar(args(0).asInstanceOf[Expression], location)
        case OpCode.Return =>
          return writeReturn(args(0).asInstanceOf[Expression], location)
        case OpCode.Throw =>
          return writeThrow(args(0).asInstanceOf[Expression], location)
        case _ =>
      }

    }
    def writeStatement(statement: Statement): Unit = {
      if (statement) {
        if ((!statements)) {
          (statements = Array(statement))

        } else {
          statements.push(statement)

        }

      }

    }
    def writeAssign(left: Expression,
                    right: Expression,
                    operationLocation: TextRange): Unit = {
      writeStatement(
        createStatement(createAssignment(left, right), operationLocation))

    }
    def writeThrow(expression: Expression,
                   operationLocation: TextRange): Unit = {
      (lastOperationWasAbrupt = true)
      (lastOperationWasCompletion = true)
      writeStatement(createThrow(expression, operationLocation))

    }
    def writeReturn(expression: Expression,
                    operationLocation: TextRange): Unit = {
      (lastOperationWasAbrupt = true)
      (lastOperationWasCompletion = true)
      writeStatement(
        createReturn(
          createArrayLiteral(
            (if (expression)
               Array(createInstruction(Instruction.Return), expression)
             else Array(createInstruction(Instruction.Return)))),
          operationLocation))

    }
    def writeBreak(label: Label, operationLocation: TextRange): Unit = {
      (lastOperationWasAbrupt = true)
      writeStatement(
        createReturn(
          createArrayLiteral(
            Array(createInstruction(Instruction.Break), createLabel(label))),
          operationLocation))

    }
    def writeBreakWhenTrue(label: Label,
                           condition: Expression,
                           operationLocation: TextRange): Unit = {
      writeStatement(
        createIf(
          condition,
          createReturn(
            createArrayLiteral(
              Array(createInstruction(Instruction.Break), createLabel(label))),
            operationLocation)))

    }
    def writeBreakWhenFalse(label: Label,
                            condition: Expression,
                            operationLocation: TextRange): Unit = {
      writeStatement(
        createIf(
          createLogicalNot(condition),
          createReturn(
            createArrayLiteral(
              Array(createInstruction(Instruction.Break), createLabel(label))),
            operationLocation)))

    }
    def writeYield(expression: Expression,
                   operationLocation: TextRange): Unit = {
      (lastOperationWasAbrupt = true)
      writeStatement(
        createReturn(
          createArrayLiteral(
            (if (expression)
               Array(createInstruction(Instruction.Yield), expression)
             else Array(createInstruction(Instruction.Yield)))),
          operationLocation))

    }
    def writeYieldStar(expression: Expression,
                       operationLocation: TextRange): Unit = {
      (lastOperationWasAbrupt = true)
      writeStatement(
        createReturn(
          createArrayLiteral(
            Array(createInstruction(Instruction.YieldStar), expression)),
          operationLocation))

    }
    def writeEndfinally(): Unit = {
      (lastOperationWasAbrupt = true)
      writeStatement(createReturn(
        createArrayLiteral(Array(createInstruction(Instruction.Endfinally)))))

    }

  }
}
