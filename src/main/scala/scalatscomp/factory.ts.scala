package scalatscomp
object Factory {
  var NodeConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
  var SourceFileConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
  def createNode(kind: SyntaxKind,
                 location: TextRange,
                 flags: NodeFlags): Node = {
    val ConstructorForKind =
      (if ((kind === SyntaxKind.SourceFile))
         ((SourceFileConstructor || ((SourceFileConstructor =
           objectAllocator.getSourceFileConstructor()))))
       else
         ((NodeConstructor || ((NodeConstructor =
           objectAllocator.getNodeConstructor())))))
    val node =
      (if (location) new ConstructorForKind(kind, location.pos, location.end)
       else new ConstructorForKind(kind, (-1), (-1)))
    (node.flags = (flags | NodeFlags.Synthesized))
    return node

  }
  def updateNode[T <: Node](updated: T, original: T): T = {
    if ((updated !== original)) {
      setOriginalNode(updated, original)
      if (original.startsOnNewLine) {
        (updated.startsOnNewLine = true)

      }
      aggregateTransformFlags(updated)

    }
    return updated

  }
  def createNodeArray[T <: Node](elements: Array[T],
                                 location: TextRange,
                                 hasTrailingComma: Boolean): NodeArray[T] = {
    if (elements) {
      if (isNodeArray(elements)) {
        return elements

      }

    } else {
      (elements = Array())

    }
    val array = elements.asInstanceOf[NodeArray[T]]
    if (location) {
      (array.pos = location.pos)
      (array.end = location.end)

    } else {
      (array.pos = (-1))
      (array.end = (-1))

    }
    if (hasTrailingComma) {
      (array.hasTrailingComma = true)

    }
    return array

  }
  def createSynthesizedNode(kind: SyntaxKind, startsOnNewLine: Boolean): Node = {
    val node = createNode(kind, undefined)
    (node.startsOnNewLine = startsOnNewLine)
    return node

  }
  def createSynthesizedNodeArray[T <: Node](elements: Array[T]): NodeArray[T] = {
    return createNodeArray(elements, undefined)

  }
  def getSynthesizedClone[T <: Node](node: T): T = {
    val clone = createNode(node.kind, undefined, node.flags).asInstanceOf[T]
    setOriginalNode(clone, node)
    (node).keys.foreach { fresh1 =>
      val key = zeroOfMyType = fresh1 {
        if ((clone.`hasOwnProperty`(key) || (!node.`hasOwnProperty`(key)))) {
          continue

        }
        ((clone.asInstanceOf[Any])(key) = (node.asInstanceOf[Any])(key))

      }
    }
    return clone

  }
  def getMutableClone[T <: Node](node: T): T = {
    val clone = getSynthesizedClone(node)
    (clone.pos = node.pos)
    (clone.end = node.end)
    (clone.parent = node.parent)
    return clone

  }
  def createLiteral(textSource: (StringLiteral | Identifier),
                    location: TextRange): StringLiteral
  def createLiteral(value: String, location: TextRange): StringLiteral
  def createLiteral(value: Int, location: TextRange): NumericLiteral
  def createLiteral(value: Boolean, location: TextRange): BooleanLiteral
  def createLiteral(value: (String | Int | Boolean),
                    location: TextRange): PrimaryExpression
  def createLiteral(
      value: (String | Int | Boolean | StringLiteral | Identifier),
      location: TextRange): PrimaryExpression = {
    if ((typeof(value) === "number")) {
      val node = createNode(SyntaxKind.NumericLiteral, location, undefined)
        .asInstanceOf[LiteralExpression]
      (node.text = value.`toString`())
      return node

    } else if ((typeof(value) === "boolean")) {
      return createNode(
        (if (value) SyntaxKind.TrueKeyword else SyntaxKind.FalseKeyword),
        location,
        undefined).asInstanceOf[PrimaryExpression]

    } else if ((typeof(value) === "string")) {
      val node = createNode(SyntaxKind.StringLiteral, location, undefined)
        .asInstanceOf[StringLiteral]
      (node.text = value)
      return node

    } else if (value) {
      val node = createNode(SyntaxKind.StringLiteral, location, undefined)
        .asInstanceOf[StringLiteral]
      (node.textSourceNode = value)
      (node.text = value.text)
      return node

    }

  }
  var nextAutoGenerateId = 0
  def createIdentifier(text: String, location: TextRange): Identifier = {
    val node =
      createNode(SyntaxKind.Identifier, location).asInstanceOf[Identifier]
    (node.text = escapeIdentifier(text))
    (node.originalKeywordKind = stringToToken(text))
    (node.autoGenerateKind = GeneratedIdentifierKind.None)
    (node.autoGenerateId = 0)
    return node

  }
  def createTempVariable(
      recordTempVariable: (((Identifier) => Unit) | undefined),
      location: TextRange): Identifier = {
    val name =
      createNode(SyntaxKind.Identifier, location).asInstanceOf[Identifier]
    (name.text = "")
    (name.originalKeywordKind = SyntaxKind.Unknown)
    (name.autoGenerateKind = GeneratedIdentifierKind.Auto)
    (name.autoGenerateId = nextAutoGenerateId)
    (nextAutoGenerateId += 1)
    if (recordTempVariable) {
      recordTempVariable(name)

    }
    return name

  }
  def createLoopVariable(location: TextRange): Identifier = {
    val name =
      createNode(SyntaxKind.Identifier, location).asInstanceOf[Identifier]
    (name.text = "")
    (name.originalKeywordKind = SyntaxKind.Unknown)
    (name.autoGenerateKind = GeneratedIdentifierKind.Loop)
    (name.autoGenerateId = nextAutoGenerateId)
    (nextAutoGenerateId += 1)
    return name

  }
  def createUniqueName(text: String, location: TextRange): Identifier = {
    val name =
      createNode(SyntaxKind.Identifier, location).asInstanceOf[Identifier]
    (name.text = text)
    (name.originalKeywordKind = SyntaxKind.Unknown)
    (name.autoGenerateKind = GeneratedIdentifierKind.Unique)
    (name.autoGenerateId = nextAutoGenerateId)
    (nextAutoGenerateId += 1)
    return name

  }
  def getGeneratedNameForNode(node: Node, location: TextRange): Identifier = {
    val name =
      createNode(SyntaxKind.Identifier, location).asInstanceOf[Identifier]
    (name.original = node)
    (name.text = "")
    (name.originalKeywordKind = SyntaxKind.Unknown)
    (name.autoGenerateKind = GeneratedIdentifierKind.Node)
    (name.autoGenerateId = nextAutoGenerateId)
    (nextAutoGenerateId += 1)
    return name

  }
  def createToken[TKind <: SyntaxKind](token: TKind) = {
    return createNode(token).asInstanceOf[Token[TKind]]

  }
  def createSuper() = {
    val node =
      createNode(SyntaxKind.SuperKeyword).asInstanceOf[PrimaryExpression]
    return node

  }
  def createThis(location: TextRange) = {
    val node = createNode(SyntaxKind.ThisKeyword, location)
      .asInstanceOf[PrimaryExpression]
    return node

  }
  def createNull() = {
    val node =
      createNode(SyntaxKind.NullKeyword).asInstanceOf[PrimaryExpression]
    return node

  }
  def createComputedPropertyName(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.ComputedPropertyName, location)
      .asInstanceOf[ComputedPropertyName]
    (node.expression = expression)
    return node

  }
  def updateComputedPropertyName(node: ComputedPropertyName,
                                 expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createComputedPropertyName(expression, node), node)

    }
    return node

  }
  def createParameter(name: (String | Identifier | BindingPattern),
                      initializer: Expression,
                      location: TextRange) = {
    return createParameterDeclaration(
      undefined,
      undefined,
      undefined,
      name,
      undefined,
      undefined,
      initializer,
      location)

  }
  def createParameterDeclaration(decorators: Array[Decorator],
                                 modifiers: Array[Modifier],
                                 dotDotDotToken: DotDotDotToken,
                                 name: (String | Identifier | BindingPattern),
                                 questionToken: QuestionToken,
                                 `type`: TypeNode,
                                 initializer: Expression,
                                 location: TextRange,
                                 flags: NodeFlags) = {
    val node = createNode(SyntaxKind.Parameter, location, flags)
      .asInstanceOf[ParameterDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.dotDotDotToken = dotDotDotToken)
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.questionToken = questionToken)
    (node.`type` = `type`)
    (node.initializer =
      (if (initializer) parenthesizeExpressionForList(initializer)
       else undefined))
    return node

  }
  def updateParameterDeclaration(node: ParameterDeclaration,
                                 decorators: Array[Decorator],
                                 modifiers: Array[Modifier],
                                 name: BindingName,
                                 `type`: TypeNode,
                                 initializer: Expression) = {
    if ((((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.name !== name)) || (node.`type` !== `type`)) || (node.initializer !== initializer))) {
      return updateNode(
        createParameterDeclaration(
          decorators,
          modifiers,
          node.dotDotDotToken,
          name,
          node.questionToken,
          `type`,
          initializer,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createProperty(decorators: Array[Decorator],
                     modifiers: Array[Modifier],
                     name: (String | PropertyName),
                     questionToken: QuestionToken,
                     `type`: TypeNode,
                     initializer: Expression,
                     location: TextRange) = {
    val node = createNode(SyntaxKind.PropertyDeclaration, location)
      .asInstanceOf[PropertyDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.questionToken = questionToken)
    (node.`type` = `type`)
    (node.initializer = initializer)
    return node

  }
  def updateProperty(node: PropertyDeclaration,
                     decorators: Array[Decorator],
                     modifiers: Array[Modifier],
                     name: PropertyName,
                     `type`: TypeNode,
                     initializer: Expression) = {
    if ((((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.name !== name)) || (node.`type` !== `type`)) || (node.initializer !== initializer))) {
      return updateNode(
        createProperty(
          decorators,
          modifiers,
          name,
          node.questionToken,
          `type`,
          initializer,
          node),
        node)

    }
    return node

  }
  def createMethod(decorators: Array[Decorator],
                   modifiers: Array[Modifier],
                   asteriskToken: AsteriskToken,
                   name: (String | PropertyName),
                   typeParameters: Array[TypeParameterDeclaration],
                   parameters: Array[ParameterDeclaration],
                   `type`: TypeNode,
                   body: Block,
                   location: TextRange,
                   flags: NodeFlags) = {
    val node = createNode(SyntaxKind.MethodDeclaration, location, flags)
      .asInstanceOf[MethodDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.asteriskToken = asteriskToken)
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.typeParameters =
      (if (typeParameters) createNodeArray(typeParameters) else undefined))
    (node.parameters = createNodeArray(parameters))
    (node.`type` = `type`)
    (node.body = body)
    return node

  }
  def updateMethod(node: MethodDeclaration,
                   decorators: Array[Decorator],
                   modifiers: Array[Modifier],
                   name: PropertyName,
                   typeParameters: Array[TypeParameterDeclaration],
                   parameters: Array[ParameterDeclaration],
                   `type`: TypeNode,
                   body: Block) = {
    if ((((((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.name !== name)) || (node.typeParameters !== typeParameters)) || (node.parameters !== parameters)) || (node.`type` !== `type`)) || (node.body !== body))) {
      return updateNode(
        createMethod(
          decorators,
          modifiers,
          node.asteriskToken,
          name,
          typeParameters,
          parameters,
          `type`,
          body,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createConstructor(decorators: Array[Decorator],
                        modifiers: Array[Modifier],
                        parameters: Array[ParameterDeclaration],
                        body: Block,
                        location: TextRange,
                        flags: NodeFlags) = {
    val node = createNode(SyntaxKind.Constructor, location, flags)
      .asInstanceOf[ConstructorDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.typeParameters = undefined)
    (node.parameters = createNodeArray(parameters))
    (node.`type` = undefined)
    (node.body = body)
    return node

  }
  def updateConstructor(node: ConstructorDeclaration,
                        decorators: Array[Decorator],
                        modifiers: Array[Modifier],
                        parameters: Array[ParameterDeclaration],
                        body: Block) = {
    if (((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.parameters !== parameters)) || (node.body !== body))) {
      return updateNode(
        createConstructor(
          decorators,
          modifiers,
          parameters,
          body,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createGetAccessor(decorators: Array[Decorator],
                        modifiers: Array[Modifier],
                        name: (String | PropertyName),
                        parameters: Array[ParameterDeclaration],
                        `type`: TypeNode,
                        body: Block,
                        location: TextRange,
                        flags: NodeFlags) = {
    val node = createNode(SyntaxKind.GetAccessor, location, flags)
      .asInstanceOf[GetAccessorDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.typeParameters = undefined)
    (node.parameters = createNodeArray(parameters))
    (node.`type` = `type`)
    (node.body = body)
    return node

  }
  def updateGetAccessor(node: GetAccessorDeclaration,
                        decorators: Array[Decorator],
                        modifiers: Array[Modifier],
                        name: PropertyName,
                        parameters: Array[ParameterDeclaration],
                        `type`: TypeNode,
                        body: Block) = {
    if (((((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.name !== name)) || (node.parameters !== parameters)) || (node.`type` !== `type`)) || (node.body !== body))) {
      return updateNode(
        createGetAccessor(
          decorators,
          modifiers,
          name,
          parameters,
          `type`,
          body,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createSetAccessor(decorators: Array[Decorator],
                        modifiers: Array[Modifier],
                        name: (String | PropertyName),
                        parameters: Array[ParameterDeclaration],
                        body: Block,
                        location: TextRange,
                        flags: NodeFlags) = {
    val node = createNode(SyntaxKind.SetAccessor, location, flags)
      .asInstanceOf[SetAccessorDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.typeParameters = undefined)
    (node.parameters = createNodeArray(parameters))
    (node.body = body)
    return node

  }
  def updateSetAccessor(node: SetAccessorDeclaration,
                        decorators: Array[Decorator],
                        modifiers: Array[Modifier],
                        name: PropertyName,
                        parameters: Array[ParameterDeclaration],
                        body: Block) = {
    if ((((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.name !== name)) || (node.parameters !== parameters)) || (node.body !== body))) {
      return updateNode(
        createSetAccessor(
          decorators,
          modifiers,
          name,
          parameters,
          body,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createObjectBindingPattern(elements: Array[BindingElement],
                                 location: TextRange) = {
    val node = createNode(SyntaxKind.ObjectBindingPattern, location)
      .asInstanceOf[ObjectBindingPattern]
    (node.elements = createNodeArray(elements))
    return node

  }
  def updateObjectBindingPattern(node: ObjectBindingPattern,
                                 elements: Array[BindingElement]) = {
    if ((node.elements !== elements)) {
      return updateNode(createObjectBindingPattern(elements, node), node)

    }
    return node

  }
  def createArrayBindingPattern(elements: Array[ArrayBindingElement],
                                location: TextRange) = {
    val node = createNode(SyntaxKind.ArrayBindingPattern, location)
      .asInstanceOf[ArrayBindingPattern]
    (node.elements = createNodeArray(elements))
    return node

  }
  def updateArrayBindingPattern(node: ArrayBindingPattern,
                                elements: Array[ArrayBindingElement]) = {
    if ((node.elements !== elements)) {
      return updateNode(createArrayBindingPattern(elements, node), node)

    }
    return node

  }
  def createBindingElement(propertyName: (String | PropertyName),
                           dotDotDotToken: DotDotDotToken,
                           name: (String | BindingName),
                           initializer: Expression,
                           location: TextRange) = {
    val node = createNode(SyntaxKind.BindingElement, location)
      .asInstanceOf[BindingElement]
    (node.propertyName =
      (if ((typeof(propertyName) === "string")) createIdentifier(propertyName)
       else propertyName))
    (node.dotDotDotToken = dotDotDotToken)
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.initializer = initializer)
    return node

  }
  def updateBindingElement(node: BindingElement,
                           propertyName: PropertyName,
                           name: BindingName,
                           initializer: Expression) = {
    if ((((node.propertyName !== propertyName) || (node.name !== name)) || (node.initializer !== initializer))) {
      return updateNode(
        createBindingElement(
          propertyName,
          node.dotDotDotToken,
          name,
          initializer,
          node),
        node)

    }
    return node

  }
  def createArrayLiteral(elements: Array[Expression],
                         location: TextRange,
                         multiLine: Boolean) = {
    val node = createNode(SyntaxKind.ArrayLiteralExpression, location)
      .asInstanceOf[ArrayLiteralExpression]
    (node.elements = parenthesizeListElements(createNodeArray(elements)))
    if (multiLine) {
      (node.multiLine = true)

    }
    return node

  }
  def updateArrayLiteral(node: ArrayLiteralExpression,
                         elements: Array[Expression]) = {
    if ((node.elements !== elements)) {
      return updateNode(
        createArrayLiteral(elements, node, node.multiLine),
        node)

    }
    return node

  }
  def createObjectLiteral(properties: Array[ObjectLiteralElementLike],
                          location: TextRange,
                          multiLine: Boolean) = {
    val node = createNode(SyntaxKind.ObjectLiteralExpression, location)
      .asInstanceOf[ObjectLiteralExpression]
    (node.properties = createNodeArray(properties))
    if (multiLine) {
      (node.multiLine = true)

    }
    return node

  }
  def updateObjectLiteral(node: ObjectLiteralExpression,
                          properties: Array[ObjectLiteralElementLike]) = {
    if ((node.properties !== properties)) {
      return updateNode(
        createObjectLiteral(properties, node, node.multiLine),
        node)

    }
    return node

  }
  def createPropertyAccess(expression: Expression,
                           name: (String | Identifier),
                           location: TextRange,
                           flags: NodeFlags) = {
    val node = createNode(SyntaxKind.PropertyAccessExpression, location, flags)
      .asInstanceOf[PropertyAccessExpression]
    (node.expression = parenthesizeForAccess(expression))
    (((node.emitNode || ((node.emitNode = Map(
      ))))).flags |= EmitFlags.NoIndentation)
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    return node

  }
  def updatePropertyAccess(node: PropertyAccessExpression,
                           expression: Expression,
                           name: Identifier) = {
    if (((node.expression !== expression) || (node.name !== name))) {
      val propertyAccess =
        createPropertyAccess(expression, name, node, node.flags)
      (((propertyAccess.emitNode || ((propertyAccess.emitNode = Map(
        ))))).flags = getEmitFlags(node))
      return updateNode(propertyAccess, node)

    }
    return node

  }
  def createElementAccess(expression: Expression,
                          index: (Int | Expression),
                          location: TextRange) = {
    val node = createNode(SyntaxKind.ElementAccessExpression, location)
      .asInstanceOf[ElementAccessExpression]
    (node.expression = parenthesizeForAccess(expression))
    (node.argumentExpression =
      (if ((typeof(index) === "number")) createLiteral(index) else index))
    return node

  }
  def updateElementAccess(node: ElementAccessExpression,
                          expression: Expression,
                          argumentExpression: Expression) = {
    if (((node.expression !== expression) || (node.argumentExpression !== argumentExpression))) {
      return updateNode(
        createElementAccess(expression, argumentExpression, node),
        node)

    }
    return node

  }
  def createCall(expression: Expression,
                 typeArguments: Array[TypeNode],
                 argumentsArray: Array[Expression],
                 location: TextRange,
                 flags: NodeFlags) = {
    val node = createNode(SyntaxKind.CallExpression, location, flags)
      .asInstanceOf[CallExpression]
    (node.expression = parenthesizeForAccess(expression))
    if (typeArguments) {
      (node.typeArguments = createNodeArray(typeArguments))

    }
    (node.arguments = parenthesizeListElements(createNodeArray(argumentsArray)))
    return node

  }
  def updateCall(node: CallExpression,
                 expression: Expression,
                 typeArguments: Array[TypeNode],
                 argumentsArray: Array[Expression]) = {
    if ((((expression !== node.expression) || (typeArguments !== node.typeArguments)) || (argumentsArray !== node.arguments))) {
      return updateNode(
        createCall(
          expression,
          typeArguments,
          argumentsArray,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createNew(expression: Expression,
                typeArguments: Array[TypeNode],
                argumentsArray: Array[Expression],
                location: TextRange,
                flags: NodeFlags) = {
    val node = createNode(SyntaxKind.NewExpression, location, flags)
      .asInstanceOf[NewExpression]
    (node.expression = parenthesizeForNew(expression))
    (node.typeArguments =
      (if (typeArguments) createNodeArray(typeArguments) else undefined))
    (node.arguments =
      (if (argumentsArray)
         parenthesizeListElements(createNodeArray(argumentsArray))
       else undefined))
    return node

  }
  def updateNew(node: NewExpression,
                expression: Expression,
                typeArguments: Array[TypeNode],
                argumentsArray: Array[Expression]) = {
    if ((((node.expression !== expression) || (node.typeArguments !== typeArguments)) || (node.arguments !== argumentsArray))) {
      return updateNode(
        createNew(expression, typeArguments, argumentsArray, node, node.flags),
        node)

    }
    return node

  }
  def createTaggedTemplate(tag: Expression,
                           template: TemplateLiteral,
                           location: TextRange) = {
    val node = createNode(SyntaxKind.TaggedTemplateExpression, location)
      .asInstanceOf[TaggedTemplateExpression]
    (node.tag = parenthesizeForAccess(tag))
    (node.template = template)
    return node

  }
  def updateTaggedTemplate(node: TaggedTemplateExpression,
                           tag: Expression,
                           template: TemplateLiteral) = {
    if (((node.tag !== tag) || (node.template !== template))) {
      return updateNode(createTaggedTemplate(tag, template, node), node)

    }
    return node

  }
  def createParen(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.ParenthesizedExpression, location)
      .asInstanceOf[ParenthesizedExpression]
    (node.expression = expression)
    return node

  }
  def updateParen(node: ParenthesizedExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createParen(expression, node), node)

    }
    return node

  }
  def createFunctionExpression(modifiers: Array[Modifier],
                               asteriskToken: AsteriskToken,
                               name: (String | Identifier),
                               typeParameters: Array[TypeParameterDeclaration],
                               parameters: Array[ParameterDeclaration],
                               `type`: TypeNode,
                               body: Block,
                               location: TextRange,
                               flags: NodeFlags) = {
    val node = createNode(SyntaxKind.FunctionExpression, location, flags)
      .asInstanceOf[FunctionExpression]
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.asteriskToken = asteriskToken)
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.typeParameters =
      (if (typeParameters) createNodeArray(typeParameters) else undefined))
    (node.parameters = createNodeArray(parameters))
    (node.`type` = `type`)
    (node.body = body)
    return node

  }
  def updateFunctionExpression(node: FunctionExpression,
                               modifiers: Array[Modifier],
                               name: Identifier,
                               typeParameters: Array[TypeParameterDeclaration],
                               parameters: Array[ParameterDeclaration],
                               `type`: TypeNode,
                               body: Block) = {
    if (((((((node.name !== name) || (node.modifiers !== modifiers)) || (node.typeParameters !== typeParameters)) || (node.parameters !== parameters)) || (node.`type` !== `type`)) || (node.body !== body))) {
      return updateNode(
        createFunctionExpression(
          modifiers,
          node.asteriskToken,
          name,
          typeParameters,
          parameters,
          `type`,
          body,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createArrowFunction(modifiers: Array[Modifier],
                          typeParameters: Array[TypeParameterDeclaration],
                          parameters: Array[ParameterDeclaration],
                          `type`: TypeNode,
                          equalsGreaterThanToken: EqualsGreaterThanToken,
                          body: ConciseBody,
                          location: TextRange,
                          flags: NodeFlags) = {
    val node = createNode(SyntaxKind.ArrowFunction, location, flags)
      .asInstanceOf[ArrowFunction]
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.typeParameters =
      (if (typeParameters) createNodeArray(typeParameters) else undefined))
    (node.parameters = createNodeArray(parameters))
    (node.`type` = `type`)
    (node.equalsGreaterThanToken = (equalsGreaterThanToken || createToken(
        SyntaxKind.EqualsGreaterThanToken)))
    (node.body = parenthesizeConciseBody(body))
    return node

  }
  def updateArrowFunction(node: ArrowFunction,
                          modifiers: Array[Modifier],
                          typeParameters: Array[TypeParameterDeclaration],
                          parameters: Array[ParameterDeclaration],
                          `type`: TypeNode,
                          body: ConciseBody) = {
    if ((((((node.modifiers !== modifiers) || (node.typeParameters !== typeParameters)) || (node.parameters !== parameters)) || (node.`type` !== `type`)) || (node.body !== body))) {
      return updateNode(
        createArrowFunction(
          modifiers,
          typeParameters,
          parameters,
          `type`,
          node.equalsGreaterThanToken,
          body,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createDelete(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.DeleteExpression, location)
      .asInstanceOf[DeleteExpression]
    (node.expression = parenthesizePrefixOperand(expression))
    return node

  }
  def updateDelete(node: DeleteExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createDelete(expression, node), expression)

    }
    return node

  }
  def createTypeOf(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.TypeOfExpression, location)
      .asInstanceOf[TypeOfExpression]
    (node.expression = parenthesizePrefixOperand(expression))
    return node

  }
  def updateTypeOf(node: TypeOfExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createTypeOf(expression, node), expression)

    }
    return node

  }
  def createVoid(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.VoidExpression, location)
      .asInstanceOf[VoidExpression]
    (node.expression = parenthesizePrefixOperand(expression))
    return node

  }
  def updateVoid(node: VoidExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createVoid(expression, node), node)

    }
    return node

  }
  def createAwait(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.AwaitExpression, location)
      .asInstanceOf[AwaitExpression]
    (node.expression = parenthesizePrefixOperand(expression))
    return node

  }
  def updateAwait(node: AwaitExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createAwait(expression, node), node)

    }
    return node

  }
  def createPrefix(operator: PrefixUnaryOperator,
                   operand: Expression,
                   location: TextRange) = {
    val node = createNode(SyntaxKind.PrefixUnaryExpression, location)
      .asInstanceOf[PrefixUnaryExpression]
    (node.operator = operator)
    (node.operand = parenthesizePrefixOperand(operand))
    return node

  }
  def updatePrefix(node: PrefixUnaryExpression, operand: Expression) = {
    if ((node.operand !== operand)) {
      return updateNode(createPrefix(node.operator, operand, node), node)

    }
    return node

  }
  def createPostfix(operand: Expression,
                    operator: PostfixUnaryOperator,
                    location: TextRange) = {
    val node = createNode(SyntaxKind.PostfixUnaryExpression, location)
      .asInstanceOf[PostfixUnaryExpression]
    (node.operand = parenthesizePostfixOperand(operand))
    (node.operator = operator)
    return node

  }
  def updatePostfix(node: PostfixUnaryExpression, operand: Expression) = {
    if ((node.operand !== operand)) {
      return updateNode(createPostfix(operand, node.operator, node), node)

    }
    return node

  }
  def createBinary(left: Expression,
                   operator: (BinaryOperator | BinaryOperatorToken),
                   right: Expression,
                   location: TextRange) = {
    val operatorToken =
      (if ((typeof(operator) === "number")) createToken(operator)
       else operator)
    val operatorKind = operatorToken.kind
    val node = createNode(SyntaxKind.BinaryExpression, location)
      .asInstanceOf[BinaryExpression]
    (node.left = parenthesizeBinaryOperand(operatorKind, left, true, undefined))
    (node.operatorToken = operatorToken)
    (node.right =
      parenthesizeBinaryOperand(operatorKind, right, false, node.left))
    return node

  }
  def updateBinary(node: BinaryExpression,
                   left: Expression,
                   right: Expression) = {
    if (((node.left !== left) || (node.right !== right))) {
      return updateNode(
        createBinary(left, node.operatorToken, right, node),
        node)

    }
    return node

  }
  def createConditional(condition: Expression,
                        questionToken: QuestionToken,
                        whenTrue: Expression,
                        colonToken: ColonToken,
                        whenFalse: Expression,
                        location: TextRange) = {
    val node = createNode(SyntaxKind.ConditionalExpression, location)
      .asInstanceOf[ConditionalExpression]
    (node.condition = condition)
    (node.questionToken = questionToken)
    (node.whenTrue = whenTrue)
    (node.colonToken = colonToken)
    (node.whenFalse = whenFalse)
    return node

  }
  def updateConditional(node: ConditionalExpression,
                        condition: Expression,
                        whenTrue: Expression,
                        whenFalse: Expression) = {
    if ((((node.condition !== condition) || (node.whenTrue !== whenTrue)) || (node.whenFalse !== whenFalse))) {
      return updateNode(
        createConditional(
          condition,
          node.questionToken,
          whenTrue,
          node.colonToken,
          whenFalse,
          node),
        node)

    }
    return node

  }
  def createTemplateExpression(head: TemplateHead,
                               templateSpans: Array[TemplateSpan],
                               location: TextRange) = {
    val node = createNode(SyntaxKind.TemplateExpression, location)
      .asInstanceOf[TemplateExpression]
    (node.head = head)
    (node.templateSpans = createNodeArray(templateSpans))
    return node

  }
  def updateTemplateExpression(node: TemplateExpression,
                               head: TemplateHead,
                               templateSpans: Array[TemplateSpan]) = {
    if (((node.head !== head) || (node.templateSpans !== templateSpans))) {
      return updateNode(
        createTemplateExpression(head, templateSpans, node),
        node)

    }
    return node

  }
  def createYield(asteriskToken: AsteriskToken,
                  expression: Expression,
                  location: TextRange) = {
    val node = createNode(SyntaxKind.YieldExpression, location)
      .asInstanceOf[YieldExpression]
    (node.asteriskToken = asteriskToken)
    (node.expression = expression)
    return node

  }
  def updateYield(node: YieldExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(
        createYield(node.asteriskToken, expression, node),
        node)

    }
    return node

  }
  def createSpread(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.SpreadElementExpression, location)
      .asInstanceOf[SpreadElementExpression]
    (node.expression = parenthesizeExpressionForList(expression))
    return node

  }
  def updateSpread(node: SpreadElementExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createSpread(expression, node), node)

    }
    return node

  }
  def createClassExpression(modifiers: Array[Modifier],
                            name: Identifier,
                            typeParameters: Array[TypeParameterDeclaration],
                            heritageClauses: Array[HeritageClause],
                            members: Array[ClassElement],
                            location: TextRange) = {
    val node = createNode(SyntaxKind.ClassExpression, location)
      .asInstanceOf[ClassExpression]
    (node.decorators = undefined)
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.name = name)
    (node.typeParameters =
      (if (typeParameters) createNodeArray(typeParameters) else undefined))
    (node.heritageClauses = createNodeArray(heritageClauses))
    (node.members = createNodeArray(members))
    return node

  }
  def updateClassExpression(node: ClassExpression,
                            modifiers: Array[Modifier],
                            name: Identifier,
                            typeParameters: Array[TypeParameterDeclaration],
                            heritageClauses: Array[HeritageClause],
                            members: Array[ClassElement]) = {
    if ((((((node.modifiers !== modifiers) || (node.name !== name)) || (node.typeParameters !== typeParameters)) || (node.heritageClauses !== heritageClauses)) || (node.members !== members))) {
      return updateNode(
        createClassExpression(
          modifiers,
          name,
          typeParameters,
          heritageClauses,
          members,
          node),
        node)

    }
    return node

  }
  def createOmittedExpression(location: TextRange) = {
    val node = createNode(SyntaxKind.OmittedExpression, location)
      .asInstanceOf[OmittedExpression]
    return node

  }
  def createExpressionWithTypeArguments(typeArguments: Array[TypeNode],
                                        expression: Expression,
                                        location: TextRange) = {
    val node = createNode(SyntaxKind.ExpressionWithTypeArguments, location)
      .asInstanceOf[ExpressionWithTypeArguments]
    (node.typeArguments =
      (if (typeArguments) createNodeArray(typeArguments) else undefined))
    (node.expression = parenthesizeForAccess(expression))
    return node

  }
  def updateExpressionWithTypeArguments(node: ExpressionWithTypeArguments,
                                        typeArguments: Array[TypeNode],
                                        expression: Expression) = {
    if (((node.typeArguments !== typeArguments) || (node.expression !== expression))) {
      return updateNode(
        createExpressionWithTypeArguments(typeArguments, expression, node),
        node)

    }
    return node

  }
  def createTemplateSpan(expression: Expression,
                         literal: (TemplateMiddle | TemplateTail),
                         location: TextRange) = {
    val node =
      createNode(SyntaxKind.TemplateSpan, location).asInstanceOf[TemplateSpan]
    (node.expression = expression)
    (node.literal = literal)
    return node

  }
  def updateTemplateSpan(node: TemplateSpan,
                         expression: Expression,
                         literal: (TemplateMiddle | TemplateTail)) = {
    if (((node.expression !== expression) || (node.literal !== literal))) {
      return updateNode(createTemplateSpan(expression, literal, node), node)

    }
    return node

  }
  def createBlock(statements: Array[Statement],
                  location: TextRange,
                  multiLine: Boolean,
                  flags: NodeFlags): Block = {
    val block =
      createNode(SyntaxKind.Block, location, flags).asInstanceOf[Block]
    (block.statements = createNodeArray(statements))
    if (multiLine) {
      (block.multiLine = true)

    }
    return block

  }
  def updateBlock(node: Block, statements: Array[Statement]) = {
    if ((statements !== node.statements)) {
      return updateNode(
        createBlock(statements, node, node.multiLine, node.flags),
        node)

    }
    return node

  }
  def createVariableStatement(
      modifiers: Array[Modifier],
      declarationList: (VariableDeclarationList | Array[VariableDeclaration]),
      location: TextRange,
      flags: NodeFlags): VariableStatement = {
    val node = createNode(SyntaxKind.VariableStatement, location, flags)
      .asInstanceOf[VariableStatement]
    (node.decorators = undefined)
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.declarationList =
      (if (isArray(declarationList))
         createVariableDeclarationList(declarationList)
       else declarationList))
    return node

  }
  def updateVariableStatement(
      node: VariableStatement,
      modifiers: Array[Modifier],
      declarationList: VariableDeclarationList): VariableStatement = {
    if (((node.modifiers !== modifiers) || (node.declarationList !== declarationList))) {
      return updateNode(
        createVariableStatement(modifiers, declarationList, node, node.flags),
        node)

    }
    return node

  }
  def createVariableDeclarationList(
      declarations: Array[VariableDeclaration],
      location: TextRange,
      flags: NodeFlags): VariableDeclarationList = {
    val node = createNode(SyntaxKind.VariableDeclarationList, location, flags)
      .asInstanceOf[VariableDeclarationList]
    (node.declarations = createNodeArray(declarations))
    return node

  }
  def updateVariableDeclarationList(
      node: VariableDeclarationList,
      declarations: Array[VariableDeclaration]) = {
    if ((node.declarations !== declarations)) {
      return updateNode(
        createVariableDeclarationList(declarations, node, node.flags),
        node)

    }
    return node

  }
  def createVariableDeclaration(name: (String | BindingPattern | Identifier),
                                `type`: TypeNode,
                                initializer: Expression,
                                location: TextRange,
                                flags: NodeFlags): VariableDeclaration = {
    val node = createNode(SyntaxKind.VariableDeclaration, location, flags)
      .asInstanceOf[VariableDeclaration]
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.`type` = `type`)
    (node.initializer =
      (if ((initializer !== undefined))
         parenthesizeExpressionForList(initializer)
       else undefined))
    return node

  }
  def updateVariableDeclaration(node: VariableDeclaration,
                                name: BindingName,
                                `type`: TypeNode,
                                initializer: Expression) = {
    if ((((node.name !== name) || (node.`type` !== `type`)) || (node.initializer !== initializer))) {
      return updateNode(
        createVariableDeclaration(name, `type`, initializer, node, node.flags),
        node)

    }
    return node

  }
  def createEmptyStatement(location: TextRange) = {
    return createNode(SyntaxKind.EmptyStatement, location)
      .asInstanceOf[EmptyStatement]

  }
  def createStatement(expression: Expression,
                      location: TextRange,
                      flags: NodeFlags): ExpressionStatement = {
    val node = createNode(SyntaxKind.ExpressionStatement, location, flags)
      .asInstanceOf[ExpressionStatement]
    (node.expression = parenthesizeExpressionForExpressionStatement(expression))
    return node

  }
  def updateStatement(node: ExpressionStatement, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createStatement(expression, node, node.flags), node)

    }
    return node

  }
  def createIf(expression: Expression,
               thenStatement: Statement,
               elseStatement: Statement,
               location: TextRange) = {
    val node =
      createNode(SyntaxKind.IfStatement, location).asInstanceOf[IfStatement]
    (node.expression = expression)
    (node.thenStatement = thenStatement)
    (node.elseStatement = elseStatement)
    return node

  }
  def updateIf(node: IfStatement,
               expression: Expression,
               thenStatement: Statement,
               elseStatement: Statement) = {
    if ((((node.expression !== expression) || (node.thenStatement !== thenStatement)) || (node.elseStatement !== elseStatement))) {
      return updateNode(
        createIf(expression, thenStatement, elseStatement, node),
        node)

    }
    return node

  }
  def createDo(statement: Statement,
               expression: Expression,
               location: TextRange) = {
    val node =
      createNode(SyntaxKind.DoStatement, location).asInstanceOf[DoStatement]
    (node.statement = statement)
    (node.expression = expression)
    return node

  }
  def updateDo(node: DoStatement,
               statement: Statement,
               expression: Expression) = {
    if (((node.statement !== statement) || (node.expression !== expression))) {
      return updateNode(createDo(statement, expression, node), node)

    }
    return node

  }
  def createWhile(expression: Expression,
                  statement: Statement,
                  location: TextRange) = {
    val node = createNode(SyntaxKind.WhileStatement, location)
      .asInstanceOf[WhileStatement]
    (node.expression = expression)
    (node.statement = statement)
    return node

  }
  def updateWhile(node: WhileStatement,
                  expression: Expression,
                  statement: Statement) = {
    if (((node.expression !== expression) || (node.statement !== statement))) {
      return updateNode(createWhile(expression, statement, node), node)

    }
    return node

  }
  def createFor(initializer: ForInitializer,
                condition: Expression,
                incrementor: Expression,
                statement: Statement,
                location: TextRange) = {
    val node = createNode(SyntaxKind.ForStatement, location, undefined)
      .asInstanceOf[ForStatement]
    (node.initializer = initializer)
    (node.condition = condition)
    (node.incrementor = incrementor)
    (node.statement = statement)
    return node

  }
  def updateFor(node: ForStatement,
                initializer: ForInitializer,
                condition: Expression,
                incrementor: Expression,
                statement: Statement) = {
    if (((((node.initializer !== initializer) || (node.condition !== condition)) || (node.incrementor !== incrementor)) || (node.statement !== statement))) {
      return updateNode(
        createFor(initializer, condition, incrementor, statement, node),
        node)

    }
    return node

  }
  def createForIn(initializer: ForInitializer,
                  expression: Expression,
                  statement: Statement,
                  location: TextRange) = {
    val node = createNode(SyntaxKind.ForInStatement, location)
      .asInstanceOf[ForInStatement]
    (node.initializer = initializer)
    (node.expression = expression)
    (node.statement = statement)
    return node

  }
  def updateForIn(node: ForInStatement,
                  initializer: ForInitializer,
                  expression: Expression,
                  statement: Statement) = {
    if ((((node.initializer !== initializer) || (node.expression !== expression)) || (node.statement !== statement))) {
      return updateNode(
        createForIn(initializer, expression, statement, node),
        node)

    }
    return node

  }
  def createForOf(initializer: ForInitializer,
                  expression: Expression,
                  statement: Statement,
                  location: TextRange) = {
    val node = createNode(SyntaxKind.ForOfStatement, location)
      .asInstanceOf[ForOfStatement]
    (node.initializer = initializer)
    (node.expression = expression)
    (node.statement = statement)
    return node

  }
  def updateForOf(node: ForOfStatement,
                  initializer: ForInitializer,
                  expression: Expression,
                  statement: Statement) = {
    if ((((node.initializer !== initializer) || (node.expression !== expression)) || (node.statement !== statement))) {
      return updateNode(
        createForOf(initializer, expression, statement, node),
        node)

    }
    return node

  }
  def createContinue(label: Identifier,
                     location: TextRange): ContinueStatement = {
    val node = createNode(SyntaxKind.ContinueStatement, location)
      .asInstanceOf[ContinueStatement]
    if (label) {
      (node.label = label)

    }
    return node

  }
  def updateContinue(node: ContinueStatement, label: Identifier) = {
    if ((node.label !== label)) {
      return updateNode(createContinue(label, node), node)

    }
    return node

  }
  def createBreak(label: Identifier, location: TextRange): BreakStatement = {
    val node = createNode(SyntaxKind.BreakStatement, location)
      .asInstanceOf[BreakStatement]
    if (label) {
      (node.label = label)

    }
    return node

  }
  def updateBreak(node: BreakStatement, label: Identifier) = {
    if ((node.label !== label)) {
      return updateNode(createBreak(label, node), node)

    }
    return node

  }
  def createReturn(expression: Expression,
                   location: TextRange): ReturnStatement = {
    val node = createNode(SyntaxKind.ReturnStatement, location)
      .asInstanceOf[ReturnStatement]
    (node.expression = expression)
    return node

  }
  def updateReturn(node: ReturnStatement, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createReturn(expression, node), node)

    }
    return node

  }
  def createWith(expression: Expression,
                 statement: Statement,
                 location: TextRange) = {
    val node = createNode(SyntaxKind.WithStatement, location)
      .asInstanceOf[WithStatement]
    (node.expression = expression)
    (node.statement = statement)
    return node

  }
  def updateWith(node: WithStatement,
                 expression: Expression,
                 statement: Statement) = {
    if (((node.expression !== expression) || (node.statement !== statement))) {
      return updateNode(createWith(expression, statement, node), node)

    }
    return node

  }
  def createSwitch(expression: Expression,
                   caseBlock: CaseBlock,
                   location: TextRange): SwitchStatement = {
    val node = createNode(SyntaxKind.SwitchStatement, location)
      .asInstanceOf[SwitchStatement]
    (node.expression = parenthesizeExpressionForList(expression))
    (node.caseBlock = caseBlock)
    return node

  }
  def updateSwitch(node: SwitchStatement,
                   expression: Expression,
                   caseBlock: CaseBlock) = {
    if (((node.expression !== expression) || (node.caseBlock !== caseBlock))) {
      return updateNode(createSwitch(expression, caseBlock, node), node)

    }
    return node

  }
  def createLabel(label: (String | Identifier),
                  statement: Statement,
                  location: TextRange) = {
    val node = createNode(SyntaxKind.LabeledStatement, location)
      .asInstanceOf[LabeledStatement]
    (node.label =
      (if ((typeof(label) === "string")) createIdentifier(label) else label))
    (node.statement = statement)
    return node

  }
  def updateLabel(node: LabeledStatement,
                  label: Identifier,
                  statement: Statement) = {
    if (((node.label !== label) || (node.statement !== statement))) {
      return updateNode(createLabel(label, statement, node), node)

    }
    return node

  }
  def createThrow(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.ThrowStatement, location)
      .asInstanceOf[ThrowStatement]
    (node.expression = expression)
    return node

  }
  def updateThrow(node: ThrowStatement, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createThrow(expression, node), node)

    }
    return node

  }
  def createTry(tryBlock: Block,
                catchClause: CatchClause,
                finallyBlock: Block,
                location: TextRange) = {
    val node =
      createNode(SyntaxKind.TryStatement, location).asInstanceOf[TryStatement]
    (node.tryBlock = tryBlock)
    (node.catchClause = catchClause)
    (node.finallyBlock = finallyBlock)
    return node

  }
  def updateTry(node: TryStatement,
                tryBlock: Block,
                catchClause: CatchClause,
                finallyBlock: Block) = {
    if ((((node.tryBlock !== tryBlock) || (node.catchClause !== catchClause)) || (node.finallyBlock !== finallyBlock))) {
      return updateNode(
        createTry(tryBlock, catchClause, finallyBlock, node),
        node)

    }
    return node

  }
  def createCaseBlock(clauses: Array[CaseOrDefaultClause],
                      location: TextRange): CaseBlock = {
    val node =
      createNode(SyntaxKind.CaseBlock, location).asInstanceOf[CaseBlock]
    (node.clauses = createNodeArray(clauses))
    return node

  }
  def updateCaseBlock(node: CaseBlock, clauses: Array[CaseOrDefaultClause]) = {
    if ((node.clauses !== clauses)) {
      return updateNode(createCaseBlock(clauses, node), node)

    }
    return node

  }
  def createFunctionDeclaration(
      decorators: Array[Decorator],
      modifiers: Array[Modifier],
      asteriskToken: AsteriskToken,
      name: (String | Identifier),
      typeParameters: Array[TypeParameterDeclaration],
      parameters: Array[ParameterDeclaration],
      `type`: TypeNode,
      body: Block,
      location: TextRange,
      flags: NodeFlags) = {
    val node = createNode(SyntaxKind.FunctionDeclaration, location, flags)
      .asInstanceOf[FunctionDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.asteriskToken = asteriskToken)
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.typeParameters =
      (if (typeParameters) createNodeArray(typeParameters) else undefined))
    (node.parameters = createNodeArray(parameters))
    (node.`type` = `type`)
    (node.body = body)
    return node

  }
  def updateFunctionDeclaration(
      node: FunctionDeclaration,
      decorators: Array[Decorator],
      modifiers: Array[Modifier],
      name: Identifier,
      typeParameters: Array[TypeParameterDeclaration],
      parameters: Array[ParameterDeclaration],
      `type`: TypeNode,
      body: Block) = {
    if ((((((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.name !== name)) || (node.typeParameters !== typeParameters)) || (node.parameters !== parameters)) || (node.`type` !== `type`)) || (node.body !== body))) {
      return updateNode(
        createFunctionDeclaration(
          decorators,
          modifiers,
          node.asteriskToken,
          name,
          typeParameters,
          parameters,
          `type`,
          body,
          node,
          node.flags),
        node)

    }
    return node

  }
  def createClassDeclaration(decorators: Array[Decorator],
                             modifiers: Array[Modifier],
                             name: Identifier,
                             typeParameters: Array[TypeParameterDeclaration],
                             heritageClauses: Array[HeritageClause],
                             members: Array[ClassElement],
                             location: TextRange) = {
    val node = createNode(SyntaxKind.ClassDeclaration, location)
      .asInstanceOf[ClassDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.name = name)
    (node.typeParameters =
      (if (typeParameters) createNodeArray(typeParameters) else undefined))
    (node.heritageClauses = createNodeArray(heritageClauses))
    (node.members = createNodeArray(members))
    return node

  }
  def updateClassDeclaration(node: ClassDeclaration,
                             decorators: Array[Decorator],
                             modifiers: Array[Modifier],
                             name: Identifier,
                             typeParameters: Array[TypeParameterDeclaration],
                             heritageClauses: Array[HeritageClause],
                             members: Array[ClassElement]) = {
    if (((((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.name !== name)) || (node.typeParameters !== typeParameters)) || (node.heritageClauses !== heritageClauses)) || (node.members !== members))) {
      return updateNode(
        createClassDeclaration(
          decorators,
          modifiers,
          name,
          typeParameters,
          heritageClauses,
          members,
          node),
        node)

    }
    return node

  }
  def createImportDeclaration(decorators: Array[Decorator],
                              modifiers: Array[Modifier],
                              importClause: ImportClause,
                              moduleSpecifier: Expression,
                              location: TextRange): ImportDeclaration = {
    val node = createNode(SyntaxKind.ImportDeclaration, location)
      .asInstanceOf[ImportDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.importClause = importClause)
    (node.moduleSpecifier = moduleSpecifier)
    return node

  }
  def updateImportDeclaration(node: ImportDeclaration,
                              decorators: Array[Decorator],
                              modifiers: Array[Modifier],
                              importClause: ImportClause,
                              moduleSpecifier: Expression) = {
    if (((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.importClause !== importClause)) || (node.moduleSpecifier !== moduleSpecifier))) {
      return updateNode(
        createImportDeclaration(
          decorators,
          modifiers,
          importClause,
          moduleSpecifier,
          node),
        node)

    }
    return node

  }
  def createImportClause(name: Identifier,
                         namedBindings: NamedImportBindings,
                         location: TextRange): ImportClause = {
    val node =
      createNode(SyntaxKind.ImportClause, location).asInstanceOf[ImportClause]
    (node.name = name)
    (node.namedBindings = namedBindings)
    return node

  }
  def updateImportClause(node: ImportClause,
                         name: Identifier,
                         namedBindings: NamedImportBindings) = {
    if (((node.name !== name) || (node.namedBindings !== namedBindings))) {
      return updateNode(createImportClause(name, namedBindings, node), node)

    }
    return node

  }
  def createNamespaceImport(name: Identifier,
                            location: TextRange): NamespaceImport = {
    val node = createNode(SyntaxKind.NamespaceImport, location)
      .asInstanceOf[NamespaceImport]
    (node.name = name)
    return node

  }
  def updateNamespaceImport(node: NamespaceImport, name: Identifier) = {
    if ((node.name !== name)) {
      return updateNode(createNamespaceImport(name, node), node)

    }
    return node

  }
  def createNamedImports(elements: Array[ImportSpecifier],
                         location: TextRange): NamedImports = {
    val node =
      createNode(SyntaxKind.NamedImports, location).asInstanceOf[NamedImports]
    (node.elements = createNodeArray(elements))
    return node

  }
  def updateNamedImports(node: NamedImports,
                         elements: Array[ImportSpecifier]) = {
    if ((node.elements !== elements)) {
      return updateNode(createNamedImports(elements, node), node)

    }
    return node

  }
  def createImportSpecifier(propertyName: Identifier,
                            name: Identifier,
                            location: TextRange) = {
    val node = createNode(SyntaxKind.ImportSpecifier, location)
      .asInstanceOf[ImportSpecifier]
    (node.propertyName = propertyName)
    (node.name = name)
    return node

  }
  def updateImportSpecifier(node: ImportSpecifier,
                            propertyName: Identifier,
                            name: Identifier) = {
    if (((node.propertyName !== propertyName) || (node.name !== name))) {
      return updateNode(createImportSpecifier(propertyName, name, node), node)

    }
    return node

  }
  def createExportAssignment(decorators: Array[Decorator],
                             modifiers: Array[Modifier],
                             isExportEquals: Boolean,
                             expression: Expression,
                             location: TextRange) = {
    val node = createNode(SyntaxKind.ExportAssignment, location)
      .asInstanceOf[ExportAssignment]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.isExportEquals = isExportEquals)
    (node.expression = expression)
    return node

  }
  def updateExportAssignment(node: ExportAssignment,
                             decorators: Array[Decorator],
                             modifiers: Array[Modifier],
                             expression: Expression) = {
    if ((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.expression !== expression))) {
      return updateNode(
        createExportAssignment(
          decorators,
          modifiers,
          node.isExportEquals,
          expression,
          node),
        node)

    }
    return node

  }
  def createExportDeclaration(decorators: Array[Decorator],
                              modifiers: Array[Modifier],
                              exportClause: NamedExports,
                              moduleSpecifier: Expression,
                              location: TextRange) = {
    val node = createNode(SyntaxKind.ExportDeclaration, location)
      .asInstanceOf[ExportDeclaration]
    (node.decorators =
      (if (decorators) createNodeArray(decorators) else undefined))
    (node.modifiers =
      (if (modifiers) createNodeArray(modifiers) else undefined))
    (node.exportClause = exportClause)
    (node.moduleSpecifier = moduleSpecifier)
    return node

  }
  def updateExportDeclaration(node: ExportDeclaration,
                              decorators: Array[Decorator],
                              modifiers: Array[Modifier],
                              exportClause: NamedExports,
                              moduleSpecifier: Expression) = {
    if (((((node.decorators !== decorators) || (node.modifiers !== modifiers)) || (node.exportClause !== exportClause)) || (node.moduleSpecifier !== moduleSpecifier))) {
      return updateNode(
        createExportDeclaration(
          decorators,
          modifiers,
          exportClause,
          moduleSpecifier,
          node),
        node)

    }
    return node

  }
  def createNamedExports(elements: Array[ExportSpecifier],
                         location: TextRange) = {
    val node =
      createNode(SyntaxKind.NamedExports, location).asInstanceOf[NamedExports]
    (node.elements = createNodeArray(elements))
    return node

  }
  def updateNamedExports(node: NamedExports,
                         elements: Array[ExportSpecifier]) = {
    if ((node.elements !== elements)) {
      return updateNode(createNamedExports(elements, node), node)

    }
    return node

  }
  def createExportSpecifier(name: (String | Identifier),
                            propertyName: (String | Identifier),
                            location: TextRange) = {
    val node = createNode(SyntaxKind.ExportSpecifier, location)
      .asInstanceOf[ExportSpecifier]
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.propertyName =
      (if ((typeof(propertyName) === "string")) createIdentifier(propertyName)
       else propertyName))
    return node

  }
  def updateExportSpecifier(node: ExportSpecifier,
                            name: Identifier,
                            propertyName: Identifier) = {
    if (((node.name !== name) || (node.propertyName !== propertyName))) {
      return updateNode(createExportSpecifier(name, propertyName, node), node)

    }
    return node

  }
  def createJsxElement(openingElement: JsxOpeningElement,
                       children: Array[JsxChild],
                       closingElement: JsxClosingElement,
                       location: TextRange) = {
    val node =
      createNode(SyntaxKind.JsxElement, location).asInstanceOf[JsxElement]
    (node.openingElement = openingElement)
    (node.children = createNodeArray(children))
    (node.closingElement = closingElement)
    return node

  }
  def updateJsxElement(node: JsxElement,
                       openingElement: JsxOpeningElement,
                       children: Array[JsxChild],
                       closingElement: JsxClosingElement) = {
    if ((((node.openingElement !== openingElement) || (node.children !== children)) || (node.closingElement !== closingElement))) {
      return updateNode(
        createJsxElement(openingElement, children, closingElement, node),
        node)

    }
    return node

  }
  def createJsxSelfClosingElement(tagName: JsxTagNameExpression,
                                  attributes: Array[JsxAttributeLike],
                                  location: TextRange) = {
    val node = createNode(SyntaxKind.JsxSelfClosingElement, location)
      .asInstanceOf[JsxSelfClosingElement]
    (node.tagName = tagName)
    (node.attributes = createNodeArray(attributes))
    return node

  }
  def updateJsxSelfClosingElement(node: JsxSelfClosingElement,
                                  tagName: JsxTagNameExpression,
                                  attributes: Array[JsxAttributeLike]) = {
    if (((node.tagName !== tagName) || (node.attributes !== attributes))) {
      return updateNode(
        createJsxSelfClosingElement(tagName, attributes, node),
        node)

    }
    return node

  }
  def createJsxOpeningElement(tagName: JsxTagNameExpression,
                              attributes: Array[JsxAttributeLike],
                              location: TextRange) = {
    val node = createNode(SyntaxKind.JsxOpeningElement, location)
      .asInstanceOf[JsxOpeningElement]
    (node.tagName = tagName)
    (node.attributes = createNodeArray(attributes))
    return node

  }
  def updateJsxOpeningElement(node: JsxOpeningElement,
                              tagName: JsxTagNameExpression,
                              attributes: Array[JsxAttributeLike]) = {
    if (((node.tagName !== tagName) || (node.attributes !== attributes))) {
      return updateNode(
        createJsxOpeningElement(tagName, attributes, node),
        node)

    }
    return node

  }
  def createJsxClosingElement(tagName: JsxTagNameExpression,
                              location: TextRange) = {
    val node = createNode(SyntaxKind.JsxClosingElement, location)
      .asInstanceOf[JsxClosingElement]
    (node.tagName = tagName)
    return node

  }
  def updateJsxClosingElement(node: JsxClosingElement,
                              tagName: JsxTagNameExpression) = {
    if ((node.tagName !== tagName)) {
      return updateNode(createJsxClosingElement(tagName, node), node)

    }
    return node

  }
  def createJsxAttribute(name: Identifier,
                         initializer: (StringLiteral | JsxExpression),
                         location: TextRange) = {
    val node =
      createNode(SyntaxKind.JsxAttribute, location).asInstanceOf[JsxAttribute]
    (node.name = name)
    (node.initializer = initializer)
    return node

  }
  def updateJsxAttribute(node: JsxAttribute,
                         name: Identifier,
                         initializer: (StringLiteral | JsxExpression)) = {
    if (((node.name !== name) || (node.initializer !== initializer))) {
      return updateNode(createJsxAttribute(name, initializer, node), node)

    }
    return node

  }
  def createJsxSpreadAttribute(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.JsxSpreadAttribute, location)
      .asInstanceOf[JsxSpreadAttribute]
    (node.expression = expression)
    return node

  }
  def updateJsxSpreadAttribute(node: JsxSpreadAttribute,
                               expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createJsxSpreadAttribute(expression, node), node)

    }
    return node

  }
  def createJsxExpression(expression: Expression, location: TextRange) = {
    val node = createNode(SyntaxKind.JsxExpression, location)
      .asInstanceOf[JsxExpression]
    (node.expression = expression)
    return node

  }
  def updateJsxExpression(node: JsxExpression, expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(createJsxExpression(expression, node), node)

    }
    return node

  }
  def createHeritageClause(token: SyntaxKind,
                           types: Array[ExpressionWithTypeArguments],
                           location: TextRange) = {
    val node = createNode(SyntaxKind.HeritageClause, location)
      .asInstanceOf[HeritageClause]
    (node.token = token)
    (node.types = createNodeArray(types))
    return node

  }
  def updateHeritageClause(node: HeritageClause,
                           types: Array[ExpressionWithTypeArguments]) = {
    if ((node.types !== types)) {
      return updateNode(createHeritageClause(node.token, types, node), node)

    }
    return node

  }
  def createCaseClause(expression: Expression,
                       statements: Array[Statement],
                       location: TextRange) = {
    val node =
      createNode(SyntaxKind.CaseClause, location).asInstanceOf[CaseClause]
    (node.expression = parenthesizeExpressionForList(expression))
    (node.statements = createNodeArray(statements))
    return node

  }
  def updateCaseClause(node: CaseClause,
                       expression: Expression,
                       statements: Array[Statement]) = {
    if (((node.expression !== expression) || (node.statements !== statements))) {
      return updateNode(createCaseClause(expression, statements, node), node)

    }
    return node

  }
  def createDefaultClause(statements: Array[Statement], location: TextRange) = {
    val node = createNode(SyntaxKind.DefaultClause, location)
      .asInstanceOf[DefaultClause]
    (node.statements = createNodeArray(statements))
    return node

  }
  def updateDefaultClause(node: DefaultClause, statements: Array[Statement]) = {
    if ((node.statements !== statements)) {
      return updateNode(createDefaultClause(statements, node), node)

    }
    return node

  }
  def createCatchClause(variableDeclaration: (String | VariableDeclaration),
                        block: Block,
                        location: TextRange) = {
    val node =
      createNode(SyntaxKind.CatchClause, location).asInstanceOf[CatchClause]
    (node.variableDeclaration =
      (if ((typeof(variableDeclaration) === "string"))
         createVariableDeclaration(variableDeclaration)
       else variableDeclaration))
    (node.block = block)
    return node

  }
  def updateCatchClause(node: CatchClause,
                        variableDeclaration: VariableDeclaration,
                        block: Block) = {
    if (((node.variableDeclaration !== variableDeclaration) || (node.block !== block))) {
      return updateNode(
        createCatchClause(variableDeclaration, block, node),
        node)

    }
    return node

  }
  def createPropertyAssignment(name: (String | PropertyName),
                               initializer: Expression,
                               location: TextRange) = {
    val node = createNode(SyntaxKind.PropertyAssignment, location)
      .asInstanceOf[PropertyAssignment]
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.questionToken = undefined)
    (node.initializer =
      (if ((initializer !== undefined))
         parenthesizeExpressionForList(initializer)
       else undefined))
    return node

  }
  def updatePropertyAssignment(node: PropertyAssignment,
                               name: PropertyName,
                               initializer: Expression) = {
    if (((node.name !== name) || (node.initializer !== initializer))) {
      return updateNode(
        createPropertyAssignment(name, initializer, node),
        node)

    }
    return node

  }
  def createShorthandPropertyAssignment(
      name: (String | Identifier),
      objectAssignmentInitializer: Expression,
      location: TextRange) = {
    val node = createNode(SyntaxKind.ShorthandPropertyAssignment, location)
      .asInstanceOf[ShorthandPropertyAssignment]
    (node.name =
      (if ((typeof(name) === "string")) createIdentifier(name) else name))
    (node.objectAssignmentInitializer =
      (if ((objectAssignmentInitializer !== undefined))
         parenthesizeExpressionForList(objectAssignmentInitializer)
       else undefined))
    return node

  }
  def updateShorthandPropertyAssignment(
      node: ShorthandPropertyAssignment,
      name: Identifier,
      objectAssignmentInitializer: Expression) = {
    if (((node.name !== name) || (node.objectAssignmentInitializer !== objectAssignmentInitializer))) {
      return updateNode(
        createShorthandPropertyAssignment(
          name,
          objectAssignmentInitializer,
          node),
        node)

    }
    return node

  }
  def updateSourceFileNode(node: SourceFile, statements: Array[Statement]) = {
    if ((node.statements !== statements)) {
      val updated = createNode(SyntaxKind.SourceFile, node, node.flags)
        .asInstanceOf[SourceFile]
      (updated.statements = createNodeArray(statements))
      (updated.endOfFileToken = node.endOfFileToken)
      (updated.fileName = node.fileName)
      (updated.path = node.path)
      (updated.text = node.text)
      if ((node.amdDependencies !== undefined))
        (updated.amdDependencies = node.amdDependencies)
      if ((node.moduleName !== undefined))
        (updated.moduleName = node.moduleName)
      if ((node.referencedFiles !== undefined))
        (updated.referencedFiles = node.referencedFiles)
      if ((node.typeReferenceDirectives !== undefined))
        (updated.typeReferenceDirectives = node.typeReferenceDirectives)
      if ((node.languageVariant !== undefined))
        (updated.languageVariant = node.languageVariant)
      if ((node.isDeclarationFile !== undefined))
        (updated.isDeclarationFile = node.isDeclarationFile)
      if ((node.renamedDependencies !== undefined))
        (updated.renamedDependencies = node.renamedDependencies)
      if ((node.hasNoDefaultLib !== undefined))
        (updated.hasNoDefaultLib = node.hasNoDefaultLib)
      if ((node.languageVersion !== undefined))
        (updated.languageVersion = node.languageVersion)
      if ((node.scriptKind !== undefined))
        (updated.scriptKind = node.scriptKind)
      if ((node.externalModuleIndicator !== undefined))
        (updated.externalModuleIndicator = node.externalModuleIndicator)
      if ((node.commonJsModuleIndicator !== undefined))
        (updated.commonJsModuleIndicator = node.commonJsModuleIndicator)
      if ((node.identifiers !== undefined))
        (updated.identifiers = node.identifiers)
      if ((node.nodeCount !== undefined))
        (updated.nodeCount = node.nodeCount)
      if ((node.identifierCount !== undefined))
        (updated.identifierCount = node.identifierCount)
      if ((node.symbolCount !== undefined))
        (updated.symbolCount = node.symbolCount)
      if ((node.parseDiagnostics !== undefined))
        (updated.parseDiagnostics = node.parseDiagnostics)
      if ((node.bindDiagnostics !== undefined))
        (updated.bindDiagnostics = node.bindDiagnostics)
      if ((node.lineMap !== undefined))
        (updated.lineMap = node.lineMap)
      if ((node.classifiableNames !== undefined))
        (updated.classifiableNames = node.classifiableNames)
      if ((node.resolvedModules !== undefined))
        (updated.resolvedModules = node.resolvedModules)
      if ((node.resolvedTypeReferenceDirectiveNames !== undefined))
        (updated.resolvedTypeReferenceDirectiveNames =
          node.resolvedTypeReferenceDirectiveNames)
      if ((node.imports !== undefined))
        (updated.imports = node.imports)
      if ((node.moduleAugmentations !== undefined))
        (updated.moduleAugmentations = node.moduleAugmentations)
      if ((node.externalHelpersModuleName !== undefined))
        (updated.externalHelpersModuleName = node.externalHelpersModuleName)
      return updateNode(updated, node)

    }
    return node

  }
  def createNotEmittedStatement(original: Node) = {
    val node = createNode(SyntaxKind.NotEmittedStatement, original)
      .asInstanceOf[NotEmittedStatement]
    (node.original = original)
    return node

  }
  def createPartiallyEmittedExpression(expression: Expression,
                                       original: Node,
                                       location: TextRange) = {
    val node =
      createNode(SyntaxKind.PartiallyEmittedExpression, (location || original))
        .asInstanceOf[PartiallyEmittedExpression]
    (node.expression = expression)
    (node.original = original)
    return node

  }
  def updatePartiallyEmittedExpression(node: PartiallyEmittedExpression,
                                       expression: Expression) = {
    if ((node.expression !== expression)) {
      return updateNode(
        createPartiallyEmittedExpression(expression, node.original, node),
        node)

    }
    return node

  }
  def createComma(left: Expression, right: Expression) = {
    return createBinary(left, SyntaxKind.CommaToken, right)
      .asInstanceOf[Expression]

  }
  def createLessThan(left: Expression,
                     right: Expression,
                     location: TextRange) = {
    return createBinary(left, SyntaxKind.LessThanToken, right, location)
      .asInstanceOf[Expression]

  }
  def createAssignment(left: Expression,
                       right: Expression,
                       location: TextRange) = {
    return createBinary(left, SyntaxKind.EqualsToken, right, location)

  }
  def createStrictEquality(left: Expression, right: Expression) = {
    return createBinary(left, SyntaxKind.EqualsEqualsEqualsToken, right)

  }
  def createStrictInequality(left: Expression, right: Expression) = {
    return createBinary(left, SyntaxKind.ExclamationEqualsEqualsToken, right)

  }
  def createAdd(left: Expression, right: Expression) = {
    return createBinary(left, SyntaxKind.PlusToken, right)

  }
  def createSubtract(left: Expression, right: Expression) = {
    return createBinary(left, SyntaxKind.MinusToken, right)

  }
  def createPostfixIncrement(operand: Expression, location: TextRange) = {
    return createPostfix(operand, SyntaxKind.PlusPlusToken, location)

  }
  def createLogicalAnd(left: Expression, right: Expression) = {
    return createBinary(left, SyntaxKind.AmpersandAmpersandToken, right)

  }
  def createLogicalOr(left: Expression, right: Expression) = {
    return createBinary(left, SyntaxKind.BarBarToken, right)

  }
  def createLogicalNot(operand: Expression) = {
    return createPrefix(SyntaxKind.ExclamationToken, operand)

  }
  def createVoidZero() = {
    return createVoid(createLiteral(0))

  }
  def createMemberAccessForPropertyName(
      target: Expression,
      memberName: PropertyName,
      location: TextRange): MemberExpression = {
    if (isComputedPropertyName(memberName)) {
      return createElementAccess(target, memberName.expression, location)

    } else {
      val expression =
        (if (isIdentifier(memberName))
           createPropertyAccess(target, memberName, location)
         else createElementAccess(target, memberName, location))
      (((expression.emitNode || ((expression.emitNode = Map(
        ))))).flags |= EmitFlags.NoNestedSourceMaps)
      return expression

    }

  }
  def createRestParameter(name: (String | Identifier)) = {
    return createParameterDeclaration(
      undefined,
      undefined,
      createToken(SyntaxKind.DotDotDotToken),
      name,
      undefined,
      undefined,
      undefined)

  }
  def createFunctionCall(func: Expression,
                         thisArg: Expression,
                         argumentsList: Array[Expression],
                         location: TextRange) = {
    return createCall(
      createPropertyAccess(func, "call"),
      undefined,
      Array(thisArg, argumentsList: _*),
      location)

  }
  def createFunctionApply(func: Expression,
                          thisArg: Expression,
                          argumentsExpression: Expression,
                          location: TextRange) = {
    return createCall(
      createPropertyAccess(func, "apply"),
      undefined,
      Array(thisArg, argumentsExpression),
      location)

  }
  def createArraySlice(array: Expression, start: (Int | Expression)) = {
    val argumentsList: Array[Expression] = Array()
    if ((start !== undefined)) {
      argumentsList.push(
        (if ((typeof(start) === "number")) createLiteral(start) else start))

    }
    return createCall(
      createPropertyAccess(array, "slice"),
      undefined,
      argumentsList)

  }
  def createArrayConcat(array: Expression, values: Array[Expression]) = {
    return createCall(createPropertyAccess(array, "concat"), undefined, values)

  }
  def createMathPow(left: Expression, right: Expression, location: TextRange) = {
    return createCall(
      createPropertyAccess(createIdentifier("Math"), "pow"),
      undefined,
      Array(left, right),
      location)

  }
  def createReactNamespace(reactNamespace: String,
                           parent: JsxOpeningLikeElement) = {
    val react = createIdentifier((reactNamespace || "React"))
    (react.flags &= (~NodeFlags.Synthesized))
    (react.parent = getParseTreeNode(parent))
    return react

  }
  def createReactCreateElement(reactNamespace: String,
                               tagName: Expression,
                               props: Expression,
                               children: Array[Expression],
                               parentElement: JsxOpeningLikeElement,
                               location: TextRange): LeftHandSideExpression = {
    val argumentsList = Array(tagName)
    if (props) {
      argumentsList.push(props)

    }
    if ((children && (children.length > 0))) {
      if ((!props)) {
        argumentsList.push(createNull())

      }
      if ((children.length > 1)) {
        (children).foreach { fresh2 =>
          val child = zeroOfMyType = fresh2 {
            (child.startsOnNewLine = true)
            argumentsList.push(child)

          }
        }

      } else {
        argumentsList.push(children(0))

      }

    }
    return createCall(
      createPropertyAccess(
        createReactNamespace(reactNamespace, parentElement),
        "createElement"),
      undefined,
      argumentsList,
      location)

  }
  def createLetDeclarationList(declarations: Array[VariableDeclaration],
                               location: TextRange) = {
    return createVariableDeclarationList(declarations, location, NodeFlags.Let)

  }
  def createConstDeclarationList(declarations: Array[VariableDeclaration],
                                 location: TextRange) = {
    return createVariableDeclarationList(
      declarations,
      location,
      NodeFlags.Const)

  }
  def createHelperName(externalHelpersModuleName: (Identifier | undefined),
                       name: String) = {
    return (if (externalHelpersModuleName)
              createPropertyAccess(externalHelpersModuleName, name)
            else createIdentifier(name))

  }
  def createExtendsHelper(externalHelpersModuleName: (Identifier | undefined),
                          name: Identifier) = {
    return createCall(
      createHelperName(externalHelpersModuleName, "__extends"),
      undefined,
      Array(name, createIdentifier("_super")))

  }
  def createAssignHelper(externalHelpersModuleName: (Identifier | undefined),
                         attributesSegments: Array[Expression]) = {
    return createCall(
      createHelperName(externalHelpersModuleName, "__assign"),
      undefined,
      attributesSegments)

  }
  def createParamHelper(externalHelpersModuleName: (Identifier | undefined),
                        expression: Expression,
                        parameterOffset: Int,
                        location: TextRange) = {
    return createCall(
      createHelperName(externalHelpersModuleName, "__param"),
      undefined,
      Array(createLiteral(parameterOffset), expression),
      location)

  }
  def createMetadataHelper(externalHelpersModuleName: (Identifier | undefined),
                           metadataKey: String,
                           metadataValue: Expression) = {
    return createCall(
      createHelperName(externalHelpersModuleName, "__metadata"),
      undefined,
      Array(createLiteral(metadataKey), metadataValue))

  }
  def createDecorateHelper(externalHelpersModuleName: (Identifier | undefined),
                           decoratorExpressions: Array[Expression],
                           target: Expression,
                           memberName: Expression,
                           descriptor: Expression,
                           location: TextRange) = {
    val argumentsArray: Array[Expression] = Array()
    argumentsArray.push(
      createArrayLiteral(decoratorExpressions, undefined, true))
    argumentsArray.push(target)
    if (memberName) {
      argumentsArray.push(memberName)
      if (descriptor) {
        argumentsArray.push(descriptor)

      }

    }
    return createCall(
      createHelperName(externalHelpersModuleName, "__decorate"),
      undefined,
      argumentsArray,
      location)

  }
  def createAwaiterHelper(externalHelpersModuleName: (Identifier | undefined),
                          hasLexicalArguments: Boolean,
                          promiseConstructor: (EntityName | Expression),
                          body: Block) = {
    val generatorFunc = createFunctionExpression(
      undefined,
      createToken(SyntaxKind.AsteriskToken),
      undefined,
      undefined,
      Array(),
      undefined,
      body)
    (((generatorFunc.emitNode || ((generatorFunc.emitNode = Map(
      ))))).flags |= EmitFlags.AsyncFunctionBody)
    return createCall(
      createHelperName(externalHelpersModuleName, "__awaiter"),
      undefined,
      Array(
        createThis(),
        (if (hasLexicalArguments) createIdentifier("arguments")
         else createVoidZero()),
        (if (promiseConstructor)
           createExpressionFromEntityName(promiseConstructor)
         else createVoidZero()),
        generatorFunc))

  }
  def createHasOwnProperty(target: LeftHandSideExpression,
                           propertyName: Expression) = {
    return createCall(
      createPropertyAccess(target, "hasOwnProperty"),
      undefined,
      Array(propertyName))

  }
  def createObjectCreate(prototype: Expression) = {
    return createCall(
      createPropertyAccess(createIdentifier("Object"), "create"),
      undefined,
      Array(prototype))

  }
  def createGeti(target: LeftHandSideExpression) = {
    return createArrowFunction(
      undefined,
      undefined,
      Array(createParameter("name")),
      undefined,
      undefined,
      createElementAccess(target, createIdentifier("name")))

  }
  def createSeti(target: LeftHandSideExpression) = {
    return createArrowFunction(
      undefined,
      undefined,
      Array(createParameter("name"), createParameter("value")),
      undefined,
      undefined,
      createAssignment(
        createElementAccess(target, createIdentifier("name")),
        createIdentifier("value")))

  }
  def createAdvancedAsyncSuperHelper() = {
    val createCache = createVariableStatement(
      undefined,
      createConstDeclarationList(
        Array(
          createVariableDeclaration(
            "cache",
            undefined,
            createObjectCreate(createNull())))))
    val getter =
      createGetAccessor(
        undefined,
        undefined,
        "value",
        Array(),
        undefined,
        createBlock(
          Array(
            createReturn(
              createCall(
                createIdentifier("geti"),
                undefined,
                Array(createIdentifier("name")))))))
    val setter =
      createSetAccessor(
        undefined,
        undefined,
        "value",
        Array(createParameter("v")),
        createBlock(
          Array(
            createStatement(
              createCall(
                createIdentifier("seti"),
                undefined,
                Array(createIdentifier("name"), createIdentifier("v")))))))
    val getOrCreateAccessorsForName = createReturn(
      createArrowFunction(
        undefined,
        undefined,
        Array(createParameter("name")),
        undefined,
        undefined,
        createLogicalOr(
          createElementAccess(
            createIdentifier("cache"),
            createIdentifier("name")),
          createParen(
            createAssignment(
              createElementAccess(
                createIdentifier("cache"),
                createIdentifier("name")),
              createObjectLiteral(Array(getter, setter)))))))
    return createVariableStatement(
      undefined,
      createConstDeclarationList(
        Array(
          createVariableDeclaration(
            "_super",
            undefined,
            createCall(
              createParen(
                createFunctionExpression(
                  undefined,
                  undefined,
                  undefined,
                  undefined,
                  Array(createParameter("geti"), createParameter("seti")),
                  undefined,
                  createBlock(
                    Array(createCache, getOrCreateAccessorsForName)))),
              undefined,
              Array(createGeti(createSuper()), createSeti(createSuper())))))))

  }
  def createSimpleAsyncSuperHelper() = {
    return createVariableStatement(
      undefined,
      createConstDeclarationList(
        Array(
          createVariableDeclaration(
            "_super",
            undefined,
            createGeti(createSuper())))))

  }
  trait CallBinding {
    var target: LeftHandSideExpression
    var thisArg: Expression
  }
  def shouldBeCapturedInTempVariable(node: Expression,
                                     cacheIdentifiers: Boolean): Boolean = {
    val target = skipParentheses(node)
    target.kind match {
      case SyntaxKind.Identifier =>
        return cacheIdentifiers
      case SyntaxKind.ThisKeyword | SyntaxKind.NumericLiteral |
          SyntaxKind.StringLiteral =>
        return false
      case SyntaxKind.ArrayLiteralExpression =>
        val elements = (target.asInstanceOf[ArrayLiteralExpression]).elements
        if ((elements.length === 0)) {
          return false

        }
        return true
      case SyntaxKind.ObjectLiteralExpression =>
        return ((target
          .asInstanceOf[ObjectLiteralExpression])
          .properties
          .length > 0)
      case _ =>
        return true
    }

  }
  def createCallBinding(expression: Expression,
                        recordTempVariable: ((Identifier) => Unit),
                        languageVersion: ScriptTarget,
                        cacheIdentifiers: Boolean): CallBinding = {
    val callee = skipOuterExpressions(expression, OuterExpressionKinds.All)
    var thisArg: Expression = zeroOfMyType
    var target: LeftHandSideExpression = zeroOfMyType
    if (isSuperProperty(callee)) {
      (thisArg = createThis())
      (target = callee)

    } else if ((callee.kind === SyntaxKind.SuperKeyword)) {
      (thisArg = createThis())
      (target =
        (if ((languageVersion < ScriptTarget.ES2015))
           createIdentifier("_super", callee)
         else callee.asInstanceOf[PrimaryExpression]))

    } else {
      callee.kind match {
        case SyntaxKind.PropertyAccessExpression => {
          if (shouldBeCapturedInTempVariable(
                (callee.asInstanceOf[PropertyAccessExpression]).expression,
                cacheIdentifiers)) {
            (thisArg = createTempVariable(recordTempVariable))
            (target = createPropertyAccess(
              createAssignment(
                thisArg,
                (callee.asInstanceOf[PropertyAccessExpression]).expression,
                (callee.asInstanceOf[PropertyAccessExpression]).expression),
              (callee.asInstanceOf[PropertyAccessExpression]).name,
              callee))

          } else {
            (thisArg =
              (callee.asInstanceOf[PropertyAccessExpression]).expression)
            (target = callee.asInstanceOf[PropertyAccessExpression])

          }
          break()

        }
        case SyntaxKind.ElementAccessExpression => {
          if (shouldBeCapturedInTempVariable(
                (callee.asInstanceOf[ElementAccessExpression]).expression,
                cacheIdentifiers)) {
            (thisArg = createTempVariable(recordTempVariable))
            (target = createElementAccess(
              createAssignment(
                thisArg,
                (callee.asInstanceOf[ElementAccessExpression]).expression,
                (callee.asInstanceOf[ElementAccessExpression]).expression),
              (callee
                .asInstanceOf[ElementAccessExpression])
                .argumentExpression,
              callee))

          } else {
            (thisArg =
              (callee.asInstanceOf[ElementAccessExpression]).expression)
            (target = callee.asInstanceOf[ElementAccessExpression])

          }
          break()

        }
        case _ => {
          (thisArg = createVoidZero())
          (target = parenthesizeForAccess(expression))
          break()

        }
      }

    }
    return Map("target" -> target, "thisArg" -> thisArg)

  }
  def inlineExpressions(expressions: Array[Expression]) = {
    return reduceLeft(expressions, createComma)

  }
  def createExpressionFromEntityName(
      node: (EntityName | Expression)): Expression = {
    if (isQualifiedName(node)) {
      val left = createExpressionFromEntityName(node.left)
      val right = getMutableClone(node.right)
      return createPropertyAccess(left, right, node)

    } else {
      return getMutableClone(node)

    }

  }
  def createExpressionForPropertyName(memberName: PropertyName): Expression = {
    if (isIdentifier(memberName)) {
      return createLiteral(memberName, undefined)

    } else if (isComputedPropertyName(memberName)) {
      return getMutableClone(memberName.expression)

    } else {
      return getMutableClone(memberName)

    }

  }
  def createExpressionForObjectLiteralElementLike(
      node: ObjectLiteralExpression,
      property: ObjectLiteralElementLike,
      receiver: Expression): Expression = {
    property.kind match {
      case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
        return createExpressionForAccessorDeclaration(
          node.properties,
          property.asInstanceOf[AccessorDeclaration],
          receiver,
          node.multiLine)
      case SyntaxKind.PropertyAssignment =>
        return createExpressionForPropertyAssignment(
          property.asInstanceOf[PropertyAssignment],
          receiver)
      case SyntaxKind.ShorthandPropertyAssignment =>
        return createExpressionForShorthandPropertyAssignment(
          property.asInstanceOf[ShorthandPropertyAssignment],
          receiver)
      case SyntaxKind.MethodDeclaration =>
        return createExpressionForMethodDeclaration(
          property.asInstanceOf[MethodDeclaration],
          receiver)
      case _ =>
    }

  }
  def createExpressionForAccessorDeclaration(
      properties: NodeArray[Declaration],
      property: AccessorDeclaration,
      receiver: Expression,
      multiLine: Boolean) = {
    const fresh3 = getAllAccessorDeclarations(properties, property)
    val firstAccessor = fresh3.firstAccessor
    val getAccessor = fresh3.getAccessor
    val setAccessor = fresh3.setAccessor
    if ((property === firstAccessor)) {
      val properties: Array[ObjectLiteralElementLike] = Array()
      if (getAccessor) {
        val getterFunction = createFunctionExpression(
          getAccessor.modifiers,
          undefined,
          undefined,
          undefined,
          getAccessor.parameters,
          undefined,
          getAccessor.body,
          getAccessor)
        setOriginalNode(getterFunction, getAccessor)
        val getter = createPropertyAssignment("get", getterFunction)
        properties.push(getter)

      }
      if (setAccessor) {
        val setterFunction = createFunctionExpression(
          setAccessor.modifiers,
          undefined,
          undefined,
          undefined,
          setAccessor.parameters,
          undefined,
          setAccessor.body,
          setAccessor)
        setOriginalNode(setterFunction, setAccessor)
        val setter = createPropertyAssignment("set", setterFunction)
        properties.push(setter)

      }
      properties.push(
        createPropertyAssignment("enumerable", createLiteral(true)))
      properties.push(
        createPropertyAssignment("configurable", createLiteral(true)))
      val expression = createCall(
        createPropertyAccess(createIdentifier("Object"), "defineProperty"),
        undefined,
        Array(
          receiver,
          createExpressionForPropertyName(property.name),
          createObjectLiteral(properties, undefined, multiLine)),
        firstAccessor)
      return aggregateTransformFlags(expression)

    }
    return undefined

  }
  def createExpressionForPropertyAssignment(property: PropertyAssignment,
                                            receiver: Expression) = {
    return aggregateTransformFlags(
      setOriginalNode(
        createAssignment(
          createMemberAccessForPropertyName(
            receiver,
            property.name,
            property.name),
          property.initializer,
          property),
        property))

  }
  def createExpressionForShorthandPropertyAssignment(
      property: ShorthandPropertyAssignment,
      receiver: Expression) = {
    return aggregateTransformFlags(
      setOriginalNode(
        createAssignment(
          createMemberAccessForPropertyName(
            receiver,
            property.name,
            property.name),
          getSynthesizedClone(property.name),
          property),
        property))

  }
  def createExpressionForMethodDeclaration(method: MethodDeclaration,
                                           receiver: Expression) = {
    return aggregateTransformFlags(
      setOriginalNode(
        createAssignment(
          createMemberAccessForPropertyName(
            receiver,
            method.name,
            method.name),
          setOriginalNode(
            createFunctionExpression(
              method.modifiers,
              method.asteriskToken,
              undefined,
              undefined,
              method.parameters,
              undefined,
              method.body,
              method),
            method),
          method),
        method))

  }
  def isUseStrictPrologue(node: ExpressionStatement): Boolean = {
    return ((node.expression
      .asInstanceOf[StringLiteral])
      .text === "use strict")

  }
  def addPrologueDirectives(target: Array[Statement],
                            source: Array[Statement],
                            ensureUseStrict: Boolean,
                            visitor: ((Node) => VisitResult[Node])): Int = {
    Debug.assert(
      (target.length === 0),
      "Prologue directives should be at the first statement in the target statements array")
    var foundUseStrict = false
    var statementOffset = 0
    val numStatements = source.length
    while ((statementOffset < numStatements)) {
      {
        val statement = source(statementOffset)
        if (isPrologueDirective(statement)) {
          if (isUseStrictPrologue(statement.asInstanceOf[ExpressionStatement])) {
            (foundUseStrict = true)

          }
          target.push(statement)

        } else {
          break()

        }
        (statementOffset += 1)

      }
    }
    if ((ensureUseStrict && (!foundUseStrict))) {
      target.push(startOnNewLine(createStatement(createLiteral("use strict"))))

    }
    while ((statementOffset < numStatements)) {
      {
        val statement = source(statementOffset)
        if ((getEmitFlags(statement) & EmitFlags.CustomPrologue)) {
          target.push(
            (if (visitor) visitNode(statement, visitor, isStatement)
             else statement))

        } else {
          break()

        }
        (statementOffset += 1)

      }
    }
    return statementOffset

  }
  def ensureUseStrict(node: SourceFile): SourceFile = {
    var foundUseStrict = false
    (node.statements).foreach { fresh4 =>
      val statement = zeroOfMyType = fresh4 {
        if (isPrologueDirective(statement)) {
          if (isUseStrictPrologue(statement.asInstanceOf[ExpressionStatement])) {
            (foundUseStrict = true)
            break()

          }

        } else {
          break()

        }

      }
    }
    if ((!foundUseStrict)) {
      val statements: Array[Statement] = Array()
      statements.push(
        startOnNewLine(createStatement(createLiteral("use strict"))))
      return updateSourceFileNode(node, statements.concat(node.statements))

    }
    return node

  }
  def parenthesizeBinaryOperand(binaryOperator: SyntaxKind,
                                operand: Expression,
                                isLeftSideOfBinary: Boolean,
                                leftOperand: Expression) = {
    val skipped = skipPartiallyEmittedExpressions(operand)
    if ((skipped.kind === SyntaxKind.ParenthesizedExpression)) {
      return operand

    }
    return (if (binaryOperandNeedsParentheses(
                  binaryOperator,
                  operand,
                  isLeftSideOfBinary,
                  leftOperand)) createParen(operand)
            else operand)

  }
  def binaryOperandNeedsParentheses(binaryOperator: SyntaxKind,
                                    operand: Expression,
                                    isLeftSideOfBinary: Boolean,
                                    leftOperand: Expression) = {
    val binaryOperatorPrecedence =
      getOperatorPrecedence(SyntaxKind.BinaryExpression, binaryOperator)
    val binaryOperatorAssociativity =
      getOperatorAssociativity(SyntaxKind.BinaryExpression, binaryOperator)
    val emittedOperand = skipPartiallyEmittedExpressions(operand)
    val operandPrecedence = getExpressionPrecedence(emittedOperand)
    compareValues(operandPrecedence, binaryOperatorPrecedence) match {
      case Comparison.LessThan =>
        if ((((!isLeftSideOfBinary) && (binaryOperatorAssociativity === Associativity.Right)) && (operand.kind === SyntaxKind.YieldExpression))) {
          return false

        }
        return true
      case Comparison.GreaterThan =>
        return false
      case Comparison.EqualTo =>
        if (isLeftSideOfBinary) {
          return (binaryOperatorAssociativity === Associativity.Right)

        } else {
          if ((isBinaryExpression(emittedOperand) && (emittedOperand.operatorToken.kind === binaryOperator))) {
            if (operatorHasAssociativeProperty(binaryOperator)) {
              return false

            }
            if ((binaryOperator === SyntaxKind.PlusToken)) {
              val leftKind =
                (if (leftOperand)
                   getLiteralKindOfBinaryPlusOperand(leftOperand)
                 else SyntaxKind.Unknown)
              if ((isLiteralKind(leftKind) && (leftKind === getLiteralKindOfBinaryPlusOperand(
                    emittedOperand)))) {
                return false

              }

            }

          }
          val operandAssociativity = getExpressionAssociativity(emittedOperand)
          return (operandAssociativity === Associativity.Left)

        }
      case _ =>
    }

  }
  def operatorHasAssociativeProperty(binaryOperator: SyntaxKind) = {
    return ((((binaryOperator === SyntaxKind.AsteriskToken) || (binaryOperator === SyntaxKind.BarToken)) || (binaryOperator === SyntaxKind.AmpersandToken)) || (binaryOperator === SyntaxKind.CaretToken))

  }
  trait BinaryPlusExpression extends BinaryExpression {
    var cachedLiteralKind: SyntaxKind
  }
  def getLiteralKindOfBinaryPlusOperand(node: Expression): SyntaxKind = {
    (node = skipPartiallyEmittedExpressions(node))
    if (isLiteralKind(node.kind)) {
      return node.kind

    }
    if (((node.kind === SyntaxKind.BinaryExpression) && ((node
          .asInstanceOf[BinaryExpression])
          .operatorToken
          .kind === SyntaxKind.PlusToken))) {
      if (((node
            .asInstanceOf[BinaryPlusExpression])
            .cachedLiteralKind !== undefined)) {
        return (node.asInstanceOf[BinaryPlusExpression]).cachedLiteralKind

      }
      val leftKind = getLiteralKindOfBinaryPlusOperand(
        (node.asInstanceOf[BinaryExpression]).left)
      val literalKind =
        (if ((isLiteralKind(leftKind) && (leftKind === getLiteralKindOfBinaryPlusOperand(
               (node.asInstanceOf[BinaryExpression]).right)))) leftKind
         else SyntaxKind.Unknown)
      ((node.asInstanceOf[BinaryPlusExpression]).cachedLiteralKind =
        literalKind)
      return literalKind

    }
    return SyntaxKind.Unknown

  }
  def parenthesizeForNew(expression: Expression): LeftHandSideExpression = {
    val emittedExpression = skipPartiallyEmittedExpressions(expression)
    emittedExpression.kind match {
      case SyntaxKind.CallExpression =>
        return createParen(expression)
      case SyntaxKind.NewExpression =>
        return (if ((emittedExpression.asInstanceOf[NewExpression]).arguments)
                  expression.asInstanceOf[LeftHandSideExpression]
                else createParen(expression))
      case _ =>
    }
    return parenthesizeForAccess(expression)

  }
  def parenthesizeForAccess(expression: Expression): LeftHandSideExpression = {
    val emittedExpression = skipPartiallyEmittedExpressions(expression)
    if (((isLeftHandSideExpression(emittedExpression) && (((emittedExpression.kind !== SyntaxKind.NewExpression) || (emittedExpression
          .asInstanceOf[NewExpression])
          .arguments))) && (emittedExpression.kind !== SyntaxKind.NumericLiteral))) {
      return expression.asInstanceOf[LeftHandSideExpression]

    }
    return createParen(expression, expression)

  }
  def parenthesizePostfixOperand(operand: Expression) = {
    return (if (isLeftHandSideExpression(operand))
              operand.asInstanceOf[LeftHandSideExpression]
            else createParen(operand, operand))

  }
  def parenthesizePrefixOperand(operand: Expression) = {
    return (if (isUnaryExpression(operand))
              operand.asInstanceOf[UnaryExpression]
            else createParen(operand, operand))

  }
  def parenthesizeListElements(elements: NodeArray[Expression]) = {
    var result: Array[Expression] = zeroOfMyType {
      var i = 0
      while ((i < elements.length)) {
        {
          val element = parenthesizeExpressionForList(elements(i))
          if (((result !== undefined) || (element !== elements(i)))) {
            if ((result === undefined)) {
              (result = elements.slice(0, i))

            }
            result.push(element)

          }

        }
        (i += 1)
      }
    }
    if ((result !== undefined)) {
      return createNodeArray(result, elements, elements.hasTrailingComma)

    }
    return elements

  }
  def parenthesizeExpressionForList(expression: Expression) = {
    val emittedExpression = skipPartiallyEmittedExpressions(expression)
    val expressionPrecedence = getExpressionPrecedence(emittedExpression)
    val commaPrecedence =
      getOperatorPrecedence(SyntaxKind.BinaryExpression, SyntaxKind.CommaToken)
    return (if ((expressionPrecedence > commaPrecedence)) expression
            else createParen(expression, expression))

  }
  def parenthesizeExpressionForExpressionStatement(expression: Expression) = {
    val emittedExpression = skipPartiallyEmittedExpressions(expression)
    if (isCallExpression(emittedExpression)) {
      val callee = emittedExpression.expression
      val kind = skipPartiallyEmittedExpressions(callee).kind
      if (((kind === SyntaxKind.FunctionExpression) || (kind === SyntaxKind.ArrowFunction))) {
        val mutableCall = getMutableClone(emittedExpression)
        (mutableCall.expression = createParen(callee, callee))
        return recreatePartiallyEmittedExpressions(expression, mutableCall)

      }

    } else {
      val leftmostExpressionKind =
        getLeftmostExpression(emittedExpression).kind
      if (((leftmostExpressionKind === SyntaxKind.ObjectLiteralExpression) || (leftmostExpressionKind === SyntaxKind.FunctionExpression))) {
        return createParen(expression, expression)

      }

    }
    return expression

  }
  def recreatePartiallyEmittedExpressions(originalOuterExpression: Expression,
                                          newInnerExpression: Expression) = {
    if (isPartiallyEmittedExpression(originalOuterExpression)) {
      val clone = getMutableClone(originalOuterExpression)
      (clone.expression = recreatePartiallyEmittedExpressions(
        clone.expression,
        newInnerExpression))
      return clone

    }
    return newInnerExpression

  }
  def getLeftmostExpression(node: Expression): Expression = {
    while (true) {
      {
        node.kind match {
          case SyntaxKind.PostfixUnaryExpression =>
            (node = (node.asInstanceOf[PostfixUnaryExpression]).operand)
            continue
          case SyntaxKind.BinaryExpression =>
            (node = (node.asInstanceOf[BinaryExpression]).left)
            continue
          case SyntaxKind.ConditionalExpression =>
            (node = (node.asInstanceOf[ConditionalExpression]).condition)
            continue
          case SyntaxKind.CallExpression | SyntaxKind.ElementAccessExpression |
              SyntaxKind.PropertyAccessExpression =>
            (node = (node
              .asInstanceOf[
                (CallExpression | PropertyAccessExpression | ElementAccessExpression)])
              .expression)
            continue
          case SyntaxKind.PartiallyEmittedExpression =>
            (node = (node.asInstanceOf[PartiallyEmittedExpression]).expression)
            continue
          case _ =>
        }
        return node

      }
    }

  }
  def parenthesizeConciseBody(body: ConciseBody): ConciseBody = {
    val emittedBody = skipPartiallyEmittedExpressions(body)
    if ((emittedBody.kind === SyntaxKind.ObjectLiteralExpression)) {
      return createParen(body.asInstanceOf[Expression], body)

    }
    return body

  }
  sealed abstract class OuterExpressionKinds
  object OuterExpressionKinds {
    case object Parentheses extends OuterExpressionKinds
    case object Assertions extends OuterExpressionKinds
    case object PartiallyEmittedExpressions extends OuterExpressionKinds
    case object All extends OuterExpressionKinds
  }
  def skipOuterExpressions(node: Expression,
                           kinds: OuterExpressionKinds): Expression
  def skipOuterExpressions(node: Node, kinds: OuterExpressionKinds): Node
  def skipOuterExpressions(node: Node,
                           kinds: Nothing = OuterExpressionKinds.All) = {
    var previousNode: Node = zeroOfMyType
    do {
      {
        (previousNode = node)
        if ((kinds & OuterExpressionKinds.Parentheses)) {
          (node = skipParentheses(node))

        }
        if ((kinds & OuterExpressionKinds.Assertions)) {
          (node = skipAssertions(node))

        }
        if ((kinds & OuterExpressionKinds.PartiallyEmittedExpressions)) {
          (node = skipPartiallyEmittedExpressions(node))

        }

      }
    } while ((previousNode !== node))
    return node

  }
  def skipParentheses(node: Expression): Expression
  def skipParentheses(node: Node): Node
  def skipParentheses(node: Node): Node = {
    while ((node.kind === SyntaxKind.ParenthesizedExpression)) {
      {
        (node = (node.asInstanceOf[ParenthesizedExpression]).expression)

      }
    }
    return node

  }
  def skipAssertions(node: Expression): Expression
  def skipAssertions(node: Node): Node
  def skipAssertions(node: Node): Node = {
    while (isAssertionExpression(node)) {
      {
        (node = (node.asInstanceOf[AssertionExpression]).expression)

      }
    }
    return node

  }
  def skipPartiallyEmittedExpressions(node: Expression): Expression
  def skipPartiallyEmittedExpressions(node: Node): Node
  def skipPartiallyEmittedExpressions(node: Node) = {
    while ((node.kind === SyntaxKind.PartiallyEmittedExpression)) {
      {
        (node = (node.asInstanceOf[PartiallyEmittedExpression]).expression)

      }
    }
    return node

  }
  def startOnNewLine[T <: Node](node: T): T = {
    (node.startsOnNewLine = true)
    return node

  }
  def setOriginalNode[T <: Node](node: T, original: Node): T = {
    (node.original = original)
    if (original) {
      val emitNode = original.emitNode
      if (emitNode)
        (node.emitNode = mergeEmitNode(emitNode, node.emitNode))

    }
    return node

  }
  def mergeEmitNode(sourceEmitNode: EmitNode, destEmitNode: EmitNode) = {
    const fresh5 = sourceEmitNode
    val flags = fresh5.flags
    val commentRange = fresh5.commentRange
    val sourceMapRange = fresh5.sourceMapRange
    val tokenSourceMapRanges = fresh5.tokenSourceMapRanges
    if (((!destEmitNode) && ((((flags || commentRange) || sourceMapRange) || tokenSourceMapRanges))))
      (destEmitNode = Map(
        ))
    if (flags)
      (destEmitNode.flags = flags)
    if (commentRange)
      (destEmitNode.commentRange = commentRange)
    if (sourceMapRange)
      (destEmitNode.sourceMapRange = sourceMapRange)
    if (tokenSourceMapRanges)
      (destEmitNode.tokenSourceMapRanges = mergeTokenSourceMapRanges(
        tokenSourceMapRanges,
        destEmitNode.tokenSourceMapRanges))
    return destEmitNode

  }
  def mergeTokenSourceMapRanges(sourceRanges: Map[TextRange],
                                destRanges: Map[TextRange]) = {
    if ((!destRanges))
      (destRanges = createMap[TextRange]())
    copyProperties(sourceRanges, destRanges)
    return destRanges

  }
  def disposeEmitNodes(sourceFile: SourceFile) = {
    (sourceFile = getSourceFileOfNode(getParseTreeNode(sourceFile)))
    val emitNode = (sourceFile && sourceFile.emitNode)
    val annotatedNodes = (emitNode && emitNode.annotatedNodes)
    if (annotatedNodes) {
      (annotatedNodes).foreach { fresh6 =>
        val node = zeroOfMyType = fresh6 {
          (node.emitNode = undefined)

        }
      }

    }

  }
  def getOrCreateEmitNode(node: Node) = {
    if ((!node.emitNode)) {
      if (isParseTreeNode(node)) {
        if ((node.kind === SyntaxKind.SourceFile)) {
          return (node.emitNode = Map("annotatedNodes" -> Array(node)))

        }
        val sourceFile = getSourceFileOfNode(node)
        getOrCreateEmitNode(sourceFile).annotatedNodes.push(node)

      }
      (node.emitNode = Map(
        ))

    }
    return node.emitNode

  }
  def getEmitFlags(node: Node) = {
    val emitNode = node.emitNode
    return (emitNode && emitNode.flags)

  }
  def setEmitFlags[T <: Node](node: T, emitFlags: EmitFlags) = {
    (getOrCreateEmitNode(node).flags = emitFlags)
    return node

  }
  def setSourceMapRange[T <: Node](node: T, range: TextRange) = {
    (getOrCreateEmitNode(node).sourceMapRange = range)
    return node

  }
  def setTokenSourceMapRange[T <: Node](node: T,
                                        token: SyntaxKind,
                                        range: TextRange) = {
    val emitNode = getOrCreateEmitNode(node)
    val tokenSourceMapRanges = (emitNode.tokenSourceMapRanges || ((emitNode.tokenSourceMapRanges =
        createMap[TextRange]())))
    (tokenSourceMapRanges(token) = range)
    return node

  }
  def setCommentRange[T <: Node](node: T, range: TextRange) = {
    (getOrCreateEmitNode(node).commentRange = range)
    return node

  }
  def getCommentRange(node: Node) = {
    val emitNode = node.emitNode
    return (((emitNode && emitNode.commentRange)) || node)

  }
  def getSourceMapRange(node: Node) = {
    val emitNode = node.emitNode
    return (((emitNode && emitNode.sourceMapRange)) || node)

  }
  def getTokenSourceMapRange(node: Node, token: SyntaxKind) = {
    val emitNode = node.emitNode
    val tokenSourceMapRanges = (emitNode && emitNode.tokenSourceMapRanges)
    return (tokenSourceMapRanges && tokenSourceMapRanges(token))

  }
  def getConstantValue(
      node: (PropertyAccessExpression | ElementAccessExpression)) = {
    val emitNode = node.emitNode
    return (emitNode && emitNode.constantValue)

  }
  def setConstantValue(
      node: (PropertyAccessExpression | ElementAccessExpression),
      value: Int) = {
    val emitNode = getOrCreateEmitNode(node)
    (emitNode.constantValue = value)
    return node

  }
  def setTextRange[T <: TextRange](node: T, location: TextRange): T = {
    if (location) {
      (node.pos = location.pos)
      (node.end = location.end)

    }
    return node

  }
  def setNodeFlags[T <: Node](node: T, flags: NodeFlags): T = {
    (node.flags = flags)
    return node

  }
  def setMultiLine[
      T <: (ObjectLiteralExpression | ArrayLiteralExpression | Block)](
      node: T,
      multiLine: Boolean): T = {
    (node.multiLine = multiLine)
    return node

  }
  def setHasTrailingComma[T <: Node](
      nodes: NodeArray[T],
      hasTrailingComma: Boolean): NodeArray[T] = {
    (nodes.hasTrailingComma = hasTrailingComma)
    return nodes

  }
  def getLocalNameForExternalImport(
      node: (ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration),
      sourceFile: SourceFile): Identifier = {
    val namespaceDeclaration = getNamespaceDeclarationNode(node)
    if ((namespaceDeclaration && (!isDefaultImport(node)))) {
      val name = namespaceDeclaration.name
      return (if (isGeneratedIdentifier(name)) name
              else
                createIdentifier(
                  getSourceTextOfNodeFromSourceFile(
                    sourceFile,
                    namespaceDeclaration.name)))

    }
    if (((node.kind === SyntaxKind.ImportDeclaration) && (node
          .asInstanceOf[ImportDeclaration])
          .importClause)) {
      return getGeneratedNameForNode(node)

    }
    if (((node.kind === SyntaxKind.ExportDeclaration) && (node
          .asInstanceOf[ExportDeclaration])
          .moduleSpecifier)) {
      return getGeneratedNameForNode(node)

    }
    return undefined

  }
  def getExternalModuleNameLiteral(
      importNode: (ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration),
      sourceFile: SourceFile,
      host: EmitHost,
      resolver: EmitResolver,
      compilerOptions: CompilerOptions) = {
    val moduleName = getExternalModuleName(importNode)
    if ((moduleName.kind === SyntaxKind.StringLiteral)) {
      return ((tryGetModuleNameFromDeclaration(
        importNode,
        host,
        resolver,
        compilerOptions) || tryRenameExternalModule(
        moduleName.asInstanceOf[StringLiteral],
        sourceFile)) || getSynthesizedClone(
        moduleName.asInstanceOf[StringLiteral]))

    }
    return undefined

  }
  def tryRenameExternalModule(moduleName: LiteralExpression,
                              sourceFile: SourceFile) = {
    if ((sourceFile.renamedDependencies && hasProperty(
          sourceFile.renamedDependencies,
          moduleName.text))) {
      return createLiteral(sourceFile.renamedDependencies(moduleName.text))

    }
    return undefined

  }
  def tryGetModuleNameFromFile(file: SourceFile,
                               host: EmitHost,
                               options: CompilerOptions): StringLiteral = {
    if ((!file)) {
      return undefined

    }
    if (file.moduleName) {
      return createLiteral(file.moduleName)

    }
    if (((!isDeclarationFile(file)) && ((options.out || options.outFile)))) {
      return createLiteral(getExternalModuleNameFromPath(host, file.fileName))

    }
    return undefined

  }
  def tryGetModuleNameFromDeclaration(
      declaration: (ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration),
      host: EmitHost,
      resolver: EmitResolver,
      compilerOptions: CompilerOptions) = {
    return tryGetModuleNameFromFile(
      resolver.getExternalModuleFileFromDeclaration(declaration),
      host,
      compilerOptions)

  }
}
