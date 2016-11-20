package scalatscomp.transformers
object ES2016 {
  def transformES2016(context: TransformationContext) = {
    const fresh1 = context
    val hoistVariableDeclaration = fresh1.hoistVariableDeclaration
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      if (isDeclarationFile(node)) {
        return node

      }
      return visitEachChild(node, visitor, context)

    }
    def visitor(node: Node): VisitResult[Node] = {
      if ((node.transformFlags & TransformFlags.ES2016)) {
        return visitorWorker(node)

      } else if ((node.transformFlags & TransformFlags.ContainsES2016)) {
        return visitEachChild(node, visitor, context)

      } else {
        return node

      }

    }
    def visitorWorker(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.BinaryExpression =>
          return visitBinaryExpression(node.asInstanceOf[BinaryExpression])
        case _ =>
          Debug.failBadSyntaxKind(node)
          return visitEachChild(node, visitor, context)
      }

    }
    def visitBinaryExpression(node: BinaryExpression): Expression = {
      val left = visitNode(node.left, visitor, isExpression)
      val right = visitNode(node.right, visitor, isExpression)
      if ((node.operatorToken.kind === SyntaxKind.AsteriskAsteriskEqualsToken)) {
        var target: Expression = zeroOfMyType
        var value: Expression = zeroOfMyType
        if (isElementAccessExpression(left)) {
          val expressionTemp = createTempVariable(hoistVariableDeclaration)
          val argumentExpressionTemp =
            createTempVariable(hoistVariableDeclaration)
          (target = createElementAccess(createAssignment(expressionTemp,
                  left.expression, left.expression),
              createAssignment(argumentExpressionTemp, left.argumentExpression,
                  left.argumentExpression),
              left))
          (value =
            createElementAccess(expressionTemp, argumentExpressionTemp, left))

        } else if (isPropertyAccessExpression(left)) {
          val expressionTemp = createTempVariable(hoistVariableDeclaration)
          (target = createPropertyAccess(createAssignment(expressionTemp,
                  left.expression, left.expression), left.name, left))
          (value = createPropertyAccess(expressionTemp, left.name, left))

        } else {
          (target = left)
          (value = left)

        }
        return createAssignment(target, createMathPow(value, right, node),
            node)

      } else if ((node.operatorToken.kind === SyntaxKind.AsteriskAsteriskToken)) {
        return createMathPow(left, right, node)

      } else {
        Debug.failBadSyntaxKind(node)
        return visitEachChild(node, visitor, context)

      }

    }

  }
}
