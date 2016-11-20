package scalatscomp.transformers
object ES5 {
  def transformES5(context: TransformationContext) = {
    val previousOnSubstituteNode = context.onSubstituteNode
    (context.onSubstituteNode = onSubstituteNode)
    context.enableSubstitution(SyntaxKind.PropertyAccessExpression)
    context.enableSubstitution(SyntaxKind.PropertyAssignment)
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      return node

    }
    def onSubstituteNode(emitContext: EmitContext, node: Node) = {
      (node = previousOnSubstituteNode(emitContext, node))
      if (isPropertyAccessExpression(node)) {
        return substitutePropertyAccessExpression(node)

      } else if (isPropertyAssignment(node)) {
        return substitutePropertyAssignment(node)

      }
      return node

    }
    def substitutePropertyAccessExpression(
        node: PropertyAccessExpression): Expression = {
      val literalName = trySubstituteReservedName(node.name)
      if (literalName) {
        return createElementAccess(node.expression, literalName, node)

      }
      return node

    }
    def substitutePropertyAssignment(
        node: PropertyAssignment): PropertyAssignment = {
      val literalName = (isIdentifier(node.name) && trySubstituteReservedName(
          node.name))
      if (literalName) {
        return updatePropertyAssignment(node, literalName, node.initializer)

      }
      return node

    }
    def trySubstituteReservedName(name: Identifier) = {
      val token = (name.originalKeywordKind || ((if (nodeIsSynthesized(name))
                                                   stringToToken(name.text)
                                                 else undefined) ))
      if (((token >= SyntaxKind.FirstReservedWord) && (token <= SyntaxKind.LastReservedWord))) {
        return createLiteral(name, name)

      }
      return undefined

    }

  }
}
