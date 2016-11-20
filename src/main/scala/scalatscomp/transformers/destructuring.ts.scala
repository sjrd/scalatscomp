package scalatscomp.transformers
object Destructuring {
  def flattenDestructuringAssignment(
      context: TransformationContext,
      node: BinaryExpression,
      needsValue: Boolean,
      recordTempVariable: ((Identifier) => Unit),
      visitor: ((Node) => VisitResult[Node])): Expression = {
    if (isEmptyObjectLiteralOrArrayLiteral(node.left)) {
      val right = node.right
      if (isDestructuringAssignment(right)) {
        return flattenDestructuringAssignment(
          context,
          right,
          needsValue,
          recordTempVariable,
          visitor)

      } else {
        return node.right

      }

    }
    var location: TextRange = node
    var value = node.right
    val expressions: Array[Expression] = Array()
    if (needsValue) {
      (value = ensureIdentifier(
        value,
        true,
        location,
        emitTempVariableAssignment,
        visitor))

    } else if (nodeIsSynthesized(node)) {
      (location = value)

    }
    flattenDestructuring(
      node,
      value,
      location,
      emitAssignment,
      emitTempVariableAssignment,
      visitor)
    if (needsValue) {
      expressions.push(value)

    }
    val expression = inlineExpressions(expressions)
    aggregateTransformFlags(expression)
    return expression
    def emitAssignment(name: Identifier,
                       value: Expression,
                       location: TextRange) = {
      val expression = createAssignment(name, value, location)
      setEmitFlags(expression, EmitFlags.NoNestedSourceMaps)
      aggregateTransformFlags(expression)
      expressions.push(expression)

    }
    def emitTempVariableAssignment(value: Expression, location: TextRange) = {
      val name = createTempVariable(recordTempVariable)
      emitAssignment(name, value, location)
      return name

    }

  }
  def flattenParameterDestructuring(node: ParameterDeclaration,
                                    value: Expression,
                                    visitor: ((Node) => VisitResult[Node])) = {
    val declarations: Array[VariableDeclaration] = Array()
    flattenDestructuring(
      node,
      value,
      node,
      emitAssignment,
      emitTempVariableAssignment,
      visitor)
    return declarations
    def emitAssignment(name: Identifier,
                       value: Expression,
                       location: TextRange) = {
      val declaration =
        createVariableDeclaration(name, undefined, value, location)
      setEmitFlags(declaration, EmitFlags.NoNestedSourceMaps)
      aggregateTransformFlags(declaration)
      declarations.push(declaration)

    }
    def emitTempVariableAssignment(value: Expression, location: TextRange) = {
      val name = createTempVariable(undefined)
      emitAssignment(name, value, location)
      return name

    }

  }
  def flattenVariableDestructuring(
      node: VariableDeclaration,
      value: Expression,
      visitor: ((Node) => VisitResult[Node]),
      recordTempVariable: ((Identifier) => Unit)) = {
    val declarations: Array[VariableDeclaration] = Array()
    var pendingAssignments: Array[Expression] = zeroOfMyType
    flattenDestructuring(
      node,
      value,
      node,
      emitAssignment,
      emitTempVariableAssignment,
      visitor)
    return declarations
    def emitAssignment(name: Identifier,
                       value: Expression,
                       location: TextRange,
                       original: Node) = {
      if (pendingAssignments) {
        pendingAssignments.push(value)
        (value = inlineExpressions(pendingAssignments))
        (pendingAssignments = undefined)

      }
      val declaration =
        createVariableDeclaration(name, undefined, value, location)
      (declaration.original = original)
      setEmitFlags(declaration, EmitFlags.NoNestedSourceMaps)
      declarations.push(declaration)
      aggregateTransformFlags(declaration)

    }
    def emitTempVariableAssignment(value: Expression, location: TextRange) = {
      val name = createTempVariable(recordTempVariable)
      if (recordTempVariable) {
        val assignment = createAssignment(name, value, location)
        if (pendingAssignments) {
          pendingAssignments.push(assignment)

        } else {
          (pendingAssignments = Array(assignment))

        }

      } else {
        emitAssignment(name, value, location, undefined)

      }
      return name

    }

  }
  def flattenVariableDestructuringToExpression(
      node: VariableDeclaration,
      recordTempVariable: ((Identifier) => Unit),
      nameSubstitution: ((Identifier) => Expression),
      visitor: ((Node) => VisitResult[Node])) = {
    val pendingAssignments: Array[Expression] = Array()
    flattenDestructuring(
      node,
      undefined,
      node,
      emitAssignment,
      emitTempVariableAssignment,
      visitor)
    val expression = inlineExpressions(pendingAssignments)
    aggregateTransformFlags(expression)
    return expression
    def emitAssignment(name: Identifier,
                       value: Expression,
                       location: TextRange,
                       original: Node) = {
      val left = ((nameSubstitution && nameSubstitution(name)) || name)
      emitPendingAssignment(left, value, location, original)

    }
    def emitTempVariableAssignment(value: Expression, location: TextRange) = {
      val name = createTempVariable(recordTempVariable)
      emitPendingAssignment(name, value, location, undefined)
      return name

    }
    def emitPendingAssignment(name: Expression,
                              value: Expression,
                              location: TextRange,
                              original: Node) = {
      val expression = createAssignment(name, value, location)
      (expression.original = original)
      setEmitFlags(expression, EmitFlags.NoNestedSourceMaps)
      pendingAssignments.push(expression)
      return expression

    }

  }
  def flattenDestructuring(
      root: (VariableDeclaration | ParameterDeclaration | BindingElement | BinaryExpression),
      value: Expression,
      location: TextRange,
      emitAssignment: ((Identifier, Expression, TextRange, Node) => Unit),
      emitTempVariableAssignment: ((Expression, TextRange) => Identifier),
      visitor: ((Node) => VisitResult[Node])) = {
    if ((value && visitor)) {
      (value = visitNode(value, visitor, isExpression))

    }
    if (isBinaryExpression(root)) {
      emitDestructuringAssignment(root.left, value, location)

    } else {
      emitBindingElement(root, value)

    }
    def emitDestructuringAssignment(
        bindingTarget: (Expression | ShorthandPropertyAssignment),
        value: Expression,
        location: TextRange) = {
      var target: Expression = zeroOfMyType
      if (isShorthandPropertyAssignment(bindingTarget)) {
        val initializer =
          (if (visitor)
             visitNode(
               bindingTarget.objectAssignmentInitializer,
               visitor,
               isExpression)
           else bindingTarget.objectAssignmentInitializer)
        if (initializer) {
          (value = createDefaultValueCheck(value, initializer, location))

        }
        (target = bindingTarget.name)

      } else if ((isBinaryExpression(bindingTarget) && (bindingTarget.operatorToken.kind === SyntaxKind.EqualsToken))) {
        val initializer =
          (if (visitor) visitNode(bindingTarget.right, visitor, isExpression)
           else bindingTarget.right)
        (value = createDefaultValueCheck(value, initializer, location))
        (target = bindingTarget.left)

      } else {
        (target = bindingTarget)

      }
      if ((target.kind === SyntaxKind.ObjectLiteralExpression)) {
        emitObjectLiteralAssignment(
          target.asInstanceOf[ObjectLiteralExpression],
          value,
          location)

      } else if ((target.kind === SyntaxKind.ArrayLiteralExpression)) {
        emitArrayLiteralAssignment(
          target.asInstanceOf[ArrayLiteralExpression],
          value,
          location)

      } else {
        val name = getMutableClone(target.asInstanceOf[Identifier])
        setSourceMapRange(name, target)
        setCommentRange(name, target)
        emitAssignment(name, value, location, undefined)

      }

    }
    def emitObjectLiteralAssignment(target: ObjectLiteralExpression,
                                    value: Expression,
                                    location: TextRange) = {
      val properties = target.properties
      if ((properties.length !== 1)) {
        (value =
          ensureIdentifier(value, true, location, emitTempVariableAssignment))

      }
      (properties).foreach { fresh1 =>
        val p = zeroOfMyType = fresh1 {
          if (((p.kind === SyntaxKind.PropertyAssignment) || (p.kind === SyntaxKind.ShorthandPropertyAssignment))) {
            val propName =
              (p.asInstanceOf[PropertyAssignment])
                .name
                .asInstanceOf[(Identifier | LiteralExpression)]
            val target =
              (if ((p.kind === SyntaxKind.ShorthandPropertyAssignment))
                 p.asInstanceOf[ShorthandPropertyAssignment]
               else
                 ((p
                   .asInstanceOf[PropertyAssignment])
                   .initializer || propName))
            emitDestructuringAssignment(
              target,
              createDestructuringPropertyAccess(value, propName),
              p)

          }

        }
      }

    }
    def emitArrayLiteralAssignment(target: ArrayLiteralExpression,
                                   value: Expression,
                                   location: TextRange) = {
      val elements = target.elements
      val numElements = elements.length
      if ((numElements !== 1)) {
        (value =
          ensureIdentifier(value, true, location, emitTempVariableAssignment))

      }
      {
        var i = 0
        while ((i < numElements)) {
          {
            val e = elements(i)
            if ((e.kind !== SyntaxKind.OmittedExpression)) {
              if ((e.kind !== SyntaxKind.SpreadElementExpression)) {
                emitDestructuringAssignment(
                  e,
                  createElementAccess(value, createLiteral(i)),
                  e)

              } else if ((i === (numElements - 1))) {
                emitDestructuringAssignment(
                  (e.asInstanceOf[SpreadElementExpression]).expression,
                  createArraySlice(value, i),
                  e)

              }

            }

          }
          (i += 1)
        }
      }

    }
    def emitBindingElement(
        target: (VariableDeclaration | ParameterDeclaration | BindingElement),
        value: Expression) = {
      val initializer =
        (if (visitor) visitNode(target.initializer, visitor, isExpression)
         else target.initializer)
      if (initializer) {
        (value =
          (if (value) createDefaultValueCheck(value, initializer, target)
           else initializer))

      } else if ((!value)) {
        (value = createVoidZero())

      }
      val name = target.name
      if (isBindingPattern(name)) {
        val elements = name.elements
        val numElements = elements.length
        if ((numElements !== 1)) {
          (value = ensureIdentifier(
            value,
            (numElements !== 0),
            target,
            emitTempVariableAssignment))

        }
        {
          var i = 0
          while ((i < numElements)) {
            {
              val element = elements(i)
              if (isOmittedExpression(element)) {
                continue

              } else if ((name.kind === SyntaxKind.ObjectBindingPattern)) {
                val propName = (element.propertyName || element.name
                    .asInstanceOf[Identifier])
                emitBindingElement(
                  element,
                  createDestructuringPropertyAccess(value, propName))

              } else {
                if ((!element.dotDotDotToken)) {
                  emitBindingElement(element, createElementAccess(value, i))

                } else if ((i === (numElements - 1))) {
                  emitBindingElement(element, createArraySlice(value, i))

                }

              }

            }
            (i += 1)
          }
        }

      } else {
        emitAssignment(name, value, target, target)

      }

    }
    def createDefaultValueCheck(value: Expression,
                                defaultValue: Expression,
                                location: TextRange): Expression = {
      (value =
        ensureIdentifier(value, true, location, emitTempVariableAssignment))
      return createConditional(
        createStrictEquality(value, createVoidZero()),
        createToken(SyntaxKind.QuestionToken),
        defaultValue,
        createToken(SyntaxKind.ColonToken),
        value)

    }
    def createDestructuringPropertyAccess(
        expression: Expression,
        propertyName: PropertyName): LeftHandSideExpression = {
      if (isComputedPropertyName(propertyName)) {
        return createElementAccess(
          expression,
          ensureIdentifier(
            propertyName.expression,
            false,
            propertyName,
            emitTempVariableAssignment))

      } else if (isLiteralExpression(propertyName)) {
        val clone = getSynthesizedClone(propertyName)
        (clone.text = unescapeIdentifier(clone.text))
        return createElementAccess(expression, clone)

      } else {
        if (isGeneratedIdentifier(propertyName)) {
          val clone = getSynthesizedClone(propertyName)
          (clone.text = unescapeIdentifier(clone.text))
          return createPropertyAccess(expression, clone)

        } else {
          return createPropertyAccess(
            expression,
            createIdentifier(unescapeIdentifier(propertyName.text)))

        }

      }

    }

  }
  def ensureIdentifier(value: Expression,
                       reuseIdentifierExpressions: Boolean,
                       location: TextRange,
                       emitTempVariableAssignment: ((Expression,
                                                     TextRange) => Identifier),
                       visitor: ((Node) => VisitResult[Node])) = {
    if ((isIdentifier(value) && reuseIdentifierExpressions)) {
      return value

    } else {
      if (visitor) {
        (value = visitNode(value, visitor, isExpression))

      }
      return emitTempVariableAssignment(value, location)

    }

  }
}
