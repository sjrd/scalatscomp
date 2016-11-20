package scalatscomp.transformers
object Scala {
  val keywords =
    Map("this" -> 0, "type" -> 0, "match" -> 0, "object" -> 0, "val" -> 0)
  def transformScala(context: TransformationContext) = {
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      emitScala(node)
      return node

    }
    def emitScala(node: SourceFile) = {
      val targetFileName = (node.fileName + ".scala")
      val emitterDiagnostics = createDiagnosticCollection()
      val host = context.getEmitHost()
      val writer = createTextWriter(host.getNewLine())
      emitNode(writer, node)
      writeFile(host, emitterDiagnostics, targetFileName, writer.getText(),
          false)

    }
    def emitNode(writer: EmitTextWriter, node: Node): Unit = {
      const fresh1 = writer
      val write = fresh1.write
      val writeLine = fresh1.writeLine
      var counter = 0
      emit(node)
      def fresh() = {
        (counter += 1)
        return ("fresh" + counter)

      }
      def emitTokenText(kind: SyntaxKind): Unit = {
        kind match {
          case SyntaxKind.AnyKeyword =>
            write("Any")
          case SyntaxKind.BooleanKeyword =>
            write("Boolean")
          case SyntaxKind.NumberKeyword =>
            write("Int")
          case SyntaxKind.StringKeyword =>
            write("String")
          case SyntaxKind.VoidKeyword =>
            write("Unit")
          case SyntaxKind.NeverKeyword =>
            write("Nothing")
          case _ =>
            write(tokenToString(kind))
        }

      }
      def emitSourceFile(node: SourceFile): Unit = {
        console.log(("Processing source file: " + node.fileName))
        write("package scalatscomp")
        writeLine()
        val statements = node.statements
        val statementOffset = emitPrologueDirectives(statements) {
          var i = statementOffset
          while ((i < statements.length)) {
            {
              emit(statements(i))
              writeLine()

            }
            (++ i)
          }
        }

      }
      def emitPrologueDirectives(statements: Array[Node]): Int = {
        {
          var i = 0
          while ((i < statements.length)) {
            {
              if (isPrologueDirective(statements(i))) {} else {
                return i

              }

            }
            (i += 1)
          }
        }
        return statements.length

      }
      def emitParameter(node: ParameterDeclaration): Unit = {
        emitDecorators(node.decorators)
        emitModifiers(node.modifiers)
        emit(node.name)
        emitExpressionWithPrefix(" = ", node.initializer)
        emitWithPrefix(": ", node.`type`)

      }
      def emitIdentifier(node: Identifier): Unit = {
        val text = node.text
        if ((textinkeywords)) {
          write((("`" + text) + "`"))

        } else if ((text === "_")) {
          write("_underscore_")

        } else {
          write(text)

        }

      }
      def emitQualifiedName(node: QualifiedName): Unit = {
        emit(node.left)
        write(".")
        emit(node.right)

      }
      def emitComputedPropertyName(node: ComputedPropertyName): Unit = {
        emit(node.expression)

      }
      def emitTypeParameters(
          tparams: NodeArray[TypeParameterDeclaration]): Unit = {
        if ((tparams && (tparams.length !== 0))) {
          write("[")
          var first = true
          (tparams).foreach { fresh2 =>
            val param = zeroOfMyType = fresh2 {
              if (first)
                (first = false)
              else
                write(", ")
              emitTypeParameter(param)

            }
          }
          write("]")

        }

      }
      def emitTypeParameter(node: TypeParameterDeclaration): Unit = {
        emit(node.name)
        if (node.constraint) {
          write(" <: ")
          emit(node.constraint)

        }

      }
      def emitDecorator(node: Decorator): Unit = {
        console.log("emitDecorator")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitPropertySignature(node: PropertySignature): Unit = {
        write("  var ")
        emit(node.name)
        emitTypeResult(node.`type`, (!(!node.initializer)))
        if (node.initializer) {
          write(" = ")
          emit(node.initializer)

        }
        writeLine()

      }
      def emitPropertyDeclaration(node: PropertyDeclaration): Unit = {
        write("  var ")
        emit(node.name)
        emitTypeResult(node.`type`, (!(!node.initializer)))
        write(" = ")
        if (node.initializer)
          emit(node.initializer)
        else
          write("_")
        writeLine()

      }
      def emitMethodSignature(node: MethodSignature): Unit = {
        write("  def ")
        emit(node.name)
        emitSignature(node)
        emitTypeResult(node.`type`, false)
        writeLine()

      }
      def emitMethodDeclaration(node: MethodDeclaration): Unit = {
        write("  def ")
        emit(node.name)
        emitSignature(node)
        emitTypeResult(node.`type`, (!(!node.body)))
        if (node.body) {
          write(" = ")
          emit(node.body)

        }
        writeLine()

      }
      def emitConstructor(node: ConstructorDeclaration): Unit = {
        write("  def this")
        emitSignature(node)
        if (node.body) {
          write(" = ")
          emit(node.body)

        } else {
          write(" = this()")

        }
        writeLine()

      }
      def emitAccessorDeclaration(node: AccessorDeclaration): Unit = {
        console.log("emitAccessorDeclaration")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitCallSignature(node: CallSignatureDeclaration): Unit = {
        write("  def apply")
        emitSignature(node)
        emitTypeResult(node.`type`, false)
        writeLine()

      }
      def emitConstructSignature(node: ConstructSignatureDeclaration): Unit = {
        write("  def `new`")
        emitSignature(node)
        emitTypeResult(node.`type`, false)
        writeLine()

      }
      def emitIndexSignature(node: IndexSignatureDeclaration): Unit = {
        write("  def apply")
        emitSignature(node)
        emitTypeResult(node.`type`, false)
        writeLine()
        write("  /* def update() -- if you need it */")
        writeLine()

      }
      def emitTypePredicate(node: TypePredicateNode): Unit = {
        const fresh3 = node
        write("Boolean")

      }
      def emitTypeReference(node: TypeReferenceNode): Unit = {
        emit(node.typeName)
        if ((node.typeArguments && (node.typeArguments.length !== 0))) {
          var first = true
          write("[")
          (node.typeArguments).foreach { fresh4 =>
            val typeArg = zeroOfMyType = fresh4 {
              if (first)
                (first = false)
              else
                write(", ")
              emit(typeArg)

            }
          }
          write("]")

        }

      }
      def emitFunctionOrConstructorTypeNode(
          node: FunctionOrConstructorTypeNode): Unit = {
        write("((")
        if (node.parameters) {
          var first = true
          (node.parameters).foreach { fresh5 =>
            val param = zeroOfMyType = fresh5 {
              if (first)
                (first = false)
              else
                write(", ")
              emit(param.`type`)

            }
          }

        }
        write(") => ")
        emit(node.`type`)
        write(")")

      }
      def emitFunctionType(node: FunctionTypeNode): Unit = {
        emitFunctionOrConstructorTypeNode(node)

      }
      def emitConstructorType(node: ConstructorTypeNode): Unit = {
        emitFunctionOrConstructorTypeNode(node)

      }
      def emitTypeQuery(node: TypeQueryNode): Unit = {
        emit(node.exprName)
        write(".type")

      }
      def emitTypeLiteral(node: TypeLiteralNode): Unit = {
        write("{")
        emitList(node.members, ListFormat.TypeLiteralMembers)
        write("}")

      }
      def emitArrayType(node: ArrayTypeNode): Unit = {
        write("Array[")
        emit(node.elementType)
        write("]")

      }
      def emitTupleType(node: TupleTypeNode): Unit = {
        write("(")
        var first = true
        (node.elementTypes).foreach { fresh6 =>
          val item = zeroOfMyType = fresh6 {
            if (first)
              (first = false)
            else
              write(", ")
            emit(item)

          }
        }
        write(")")

      }
      def emitUnionType(node: UnionTypeNode): Unit = {
        write("(")
        emitList(node.types, ListFormat.UnionTypeConstituents)
        write(")")

      }
      def emitIntersectionType(node: IntersectionTypeNode): Unit = {
        if ((((node.types.length === 2) && (node
              .types(0)
              .kind === SyntaxKind.StringKeyword)) && (node
              .types(1)
              .kind === SyntaxKind.TypeLiteral))) {
          write("String")

        } else {
          write("(")
          emitList(node.types, ListFormat.IntersectionTypeConstituents)
          write(")")

        }

      }
      def emitParenthesizedType(node: ParenthesizedTypeNode): Unit = {
        emit(node.`type`)

      }
      def emitExpressionWithTypeArguments(
          node: ExpressionWithTypeArguments): Unit = {
        emit(node.expression)
        emitTypeArguments(node.typeArguments)

      }
      def emitThisType(): Unit = {
        console.log("emitThisType")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitLiteralType(node: LiteralTypeNode): Unit = {
        write("`")
        emit(node.literal)
        write("`")

      }
      def emitObjectBindingPattern(node: ObjectBindingPattern): Unit = {
        console.log(
            ("unreachable: emitObjectBindingPattern in " + node.parent.kind))

      }
      def emitArrayBindingPattern(): Unit = {
        console.log("unreachable: emitArrayBindingPattern")

      }
      def emitBindingElement(): Unit = {
        console.log("unreachable: emitBindingElement")

      }
      def emitTemplateSpan(node: TemplateSpan): Unit = {
        emitExpression(node.expression)
        emit(node.literal)

      }
      def emitSemicolonClassElement(): Unit = {
        console.log("emitSemicolonClassElement")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitBlock(node: Block): Unit = {
        if (isSingleLineEmptyBlock(node)) {
          emitTokenText(SyntaxKind.OpenBraceToken)
          write(" ")
          emitTokenText(SyntaxKind.CloseBraceToken)

        } else {
          emitTokenText(SyntaxKind.OpenBraceToken)
          writeLine()
          emitBlockStatements(node)
          writeLine()
          emitTokenText(SyntaxKind.CloseBraceToken)

        }

      }
      def emitBlockStatements(node: BlockLike) = {
        if ((getEmitFlags(node) & EmitFlags.SingleLine)) {
          emitList(node.statements, ListFormat.SingleLineBlockStatements)

        } else {
          emitList(node.statements, ListFormat.MultiLineBlockStatements)

        }

      }
      def emitVariableStatement(node: VariableStatement): Unit = {
        emitModifiers(node.modifiers)
        emit(node.declarationList)

      }
      def emitModifiers(modifiers: NodeArray[Modifier]) = {
        const fresh7 = modifiers

      }
      def emitEmptyStatement(): Unit = {
        write(";")

      }
      def emitExpressionStatement(node: ExpressionStatement): Unit = {
        emitExpression(node.expression)
        writeLine()

      }
      def emitIfStatement(node: IfStatement): Unit = {
        emitTokenText(SyntaxKind.IfKeyword)
        write(" ")
        emitTokenText(SyntaxKind.OpenParenToken)
        emitExpression(node.expression)
        emitTokenText(SyntaxKind.CloseParenToken)
        emitEmbeddedStatement(node.thenStatement)
        if (node.elseStatement) {
          writeLine()
          emitTokenText(SyntaxKind.ElseKeyword)
          if ((node.elseStatement.kind === SyntaxKind.IfStatement)) {
            write(" ")
            emit(node.elseStatement)

          } else {
            emitEmbeddedStatement(node.elseStatement)

          }

        }
        writeLine()

      }
      def emitEmbeddedStatement(node: Statement) = {
        if (isBlock(node)) {
          write(" ")
          emit(node)

        } else {
          writeLine()
          emit(node)

        }

      }
      def emitDoStatement(node: DoStatement): Unit = {
        write("do {")
        writeLine()
        emit(node.statement)
        writeLine()
        write("} while (")
        emit(node.expression)
        write(")")

      }
      def emitWhileStatement(node: WhileStatement): Unit = {
        write("while (")
        emit(node.expression)
        write(") {")
        writeLine()
        emit(node.statement)
        writeLine()
        write("}")
        writeLine()

      }
      def emitForStatement(node: ForStatement): Unit = {
        write("{")
        writeLine()
        emitForBinding(node.initializer)
        writeLine()
        write("while(")
        emitExpressionWithPrefix(" ", node.condition)
        write(") {")
        writeLine()
        emitEmbeddedStatement(node.statement)
        writeLine()
        emitExpressionWithPrefix(" ", node.incrementor)
        writeLine()
        write("}")
        writeLine()
        write("}")
        writeLine()

      }
      def emitForBinding(node: (VariableDeclarationList | Expression)) = {
        if ((node !== undefined)) {
          if ((node.kind === SyntaxKind.VariableDeclarationList)) {
            emit(node)

          } else {
            emitExpression(node.asInstanceOf[Expression])

          }

        }

      }
      def emitForInStatement(node: ForInStatement): Unit = {
        write("(")
        emitExpression(node.expression)
        val x = fresh()
        write(((").keys.foreach { " + x) + " => "))
        writeLine()
        emitForBinding(node.initializer)
        write((" = " + x))
        writeLine()
        emitEmbeddedStatement(node.statement)
        writeLine()
        write("}")
        writeLine()

      }
      def emitForOfStatement(node: ForOfStatement): Unit = {
        write("(")
        emitExpression(node.expression)
        val x = fresh()
        write(((").foreach { " + x) + " => "))
        writeLine()
        emitForBinding(node.initializer)
        write((" = " + x))
        writeLine()
        emitEmbeddedStatement(node.statement)
        writeLine()
        write("}")
        writeLine()

      }
      def emitContinueStatement(node: ContinueStatement): Unit = {
        if (node.label) {
          write("continue ")
          emit(node.label)

        } else {
          write("continue")

        }
        writeLine()

      }
      def emitBreakStatement(node: BreakStatement): Unit = {
        if (node.label) {
          emit(node.label)
          write(".break()")

        } else {
          write("break()")

        }
        writeLine()

      }
      def emitReturnStatement(node: ReturnStatement): Unit = {
        emitTokenText(SyntaxKind.ReturnKeyword)
        emitExpressionWithPrefix(" ", node.expression)
        writeLine()

      }
      def emitWithStatement(node: WithStatement): Unit = {
        console.log("emitWhileStatement")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitSwitchStatement(node: SwitchStatement): Unit = {
        emit(node.expression)
        write(" match {")
        writeLine()
        val clauses = node.caseBlock.clauses
        var alternatives = createNodeArray[Expression]()
        var hasDefault = false
        (clauses).foreach { fresh8 =>
          val clause = zeroOfMyType = fresh8 {
            clause.kind match {
              case SyntaxKind.CaseClause =>
                val caseClause = clause.asInstanceOf[CaseClause]
                alternatives.push(caseClause.expression)
                val statements = caseClause.statements
                if (statements.length) {
                  write("  case ")
                  emitList(alternatives, ListFormat.UnionTypeConstituents)
                  write(" =>")
                  writeLine()
                  (alternatives = createNodeArray[Expression]())
                  val canFallThrough = emitSwitchClauseStatements(statements)
                  if (canFallThrough)
                    console.log("WARNING: non-empty case can fall through")

                }
              case SyntaxKind.DefaultClause =>
                val defaultClause = clause.asInstanceOf[DefaultClause]
                (hasDefault = true)
                if (alternatives.length)
                  console.log("INFO: discarding alternatives in default")
                write("  case _ =>")
                writeLine()
                emitSwitchClauseStatements(defaultClause.statements)
              case _ =>
            }

          }
        }
        if ((!hasDefault)) {
          write("  case _ =>")
          writeLine()

        }
        write("}")
        writeLine()

      }
      def emitSwitchClauseStatements(
          statements: NodeArray[Statement]): Boolean = {
        (statements).foreach { fresh9 =>
          val statement = zeroOfMyType = fresh9 {
            statement.kind match {
              case SyntaxKind.BreakStatement =>
                return false
              case SyntaxKind.ContinueStatement | SyntaxKind.ReturnStatement =>
                emit(statement)
                writeLine()
                return false
              case _ =>
                emit(statement)
                writeLine()
            }

          }
        }

      }
      def emitLabeledStatement(node: LabeledStatement): Unit = {
        write("val ")
        emit(node.label)
        write(" = new scala.util.control.Breaks")
        writeLine()
        emit(node.label)
        write(".breakable {")
        writeLine()
        emit(node.statement)
        writeLine()
        write("}")

      }
      def emitThrowStatement(node: ThrowStatement): Unit = {
        write("throw")
        emitExpressionWithPrefix(" ", node.expression)

      }
      def emitTryStatement(node: TryStatement): Unit = {
        write("try ")
        emit(node.tryBlock)
        if (node.catchClause) {
          emit(node.catchClause)

        }
        if (node.finallyBlock) {
          write(" finally ")
          emit(node.finallyBlock)

        }
        writeLine()

      }
      def emitDebuggerStatement(): Unit = {
        write(";")

      }
      def emitVariableDeclaration(node: VariableDeclaration): Unit = {
        emit(node.name)
        emitWithPrefix(": ", node.`type`)
        emitExpressionWithPrefix(" = ", node.initializer)

      }
      def emitVariableDeclarationList(decls: VariableDeclarationList): Unit = {
        val varity =
          (if (isLet(node)) "var "
           else (if (isConst(decls)) "val "
                 else "var "))
        var emitRhs: (() => Unit) = zeroOfMyType
        (decls.declarations).foreach { fresh10 =>
          val decl = zeroOfMyType = fresh10 {
            (emitRhs = (() => {
                  if (decl.initializer)
                    emitExpressionWithPrefix(" = ", decl.initializer)
                  else
                    write(" = zeroOfMyType")

                }))
            def ident(ident: Identifier): Unit = {
              emitModifiers(node.modifiers)
              write(varity)
              emitIdentifier(ident)
              emitWithPrefix(": ", decl.`type`)
              emitRhs()
              writeLine()

            }
            val name = decl.name
            name.kind match {
              case SyntaxKind.Identifier =>
                ident(name.asInstanceOf[Identifier])
              case SyntaxKind.ObjectBindingPattern =>
                val objpat = name.asInstanceOf[ObjectBindingPattern]
                if (objpat.elements) {
                  val x = fresh()
                  write(("const " + x))
                  emitRhs()
                  writeLine()
                  (objpat.elements).foreach { fresh11 =>
                    val elem = zeroOfMyType = fresh11 {
                      elem.name.kind match {
                        case SyntaxKind.Identifier =>
                          val nested = elem.name.asInstanceOf[Identifier]
                          (emitRhs = (() => {
                                write(" = ")
                                write((x + "."))
                                emitIdentifier(nested)
                                writeLine()

                              }))
                          ident(nested)
                        case _ =>
                          console.log(
                              "Warning: nested object patterns are not supported")
                      }

                    }
                  }

                }
              case SyntaxKind.ArrayBindingPattern =>
                console.log("Warning: array patterns are not supported")
              case _ =>
            }

          }
        }

      }
      def emitFunctionDeclaration(node: FunctionDeclaration): Unit = {
        write("def ")
        emit(node.name)
        emitSignature(node)
        emitTypeResult(node.`type`, (!(!node.body)))
        if (node.body) {
          write(" = ")
          emit(node.body)

        }
        writeLine()

      }
      def emitSignature(node: SignatureDeclaration): Unit = {
        emitTypeParameters(node.typeParameters)
        emitParameterList(node.parameters)

      }
      def emitParameterList(
          parameters: NodeArray[ParameterDeclaration]): Unit = {
        write("(")
        var first = true
        (parameters).foreach { fresh12 =>
          val param = zeroOfMyType = fresh12 {
            if (first)
              (first = false)
            else
              write(", ")
            emit(param.name)
            emitTypeResult(param.`type`, false)
            if (param.initializer) {
              write(" = ")
              emit(param.initializer)

            }

          }
        }
        write(")")

      }
      def emitTypeResult(`type`: TypeNode, canInfer: Boolean): Unit = {
        if ((`type` || (!canInfer))) {
          write(": ")
          if (`type`)
            emit(`type`)
          else
            write("Nothing")

        }

      }
      def emitClassDeclaration(node: ClassDeclaration): Unit = {
        write("class ")
        emit(node.name)
        emitTypeParameters(node.typeParameters)
        emitHeritageClauses(node.heritageClauses)
        write(" {")
        writeLine()
        (node.members).foreach { fresh13 =>
          val member = zeroOfMyType = fresh13 {
            emit(member)
            writeLine()

          }
        }
        write("}")
        writeLine()

      }
      def emitInterfaceDeclaration(node: InterfaceDeclaration): Unit = {
        write("trait ")
        emit(node.name)
        emitTypeParameters(node.typeParameters)
        emitHeritageClauses(node.heritageClauses)
        write(" {")
        writeLine()
        (node.members).foreach { fresh14 =>
          val member = zeroOfMyType = fresh14 {
            emit(member)
            writeLine()

          }
        }
        write("}")
        writeLine()

      }
      def emitTypeAliasDeclaration(node: TypeAliasDeclaration): Unit = {
        write("type ")
        emit(node.name)
        emitTypeParameters(node.typeParameters)
        write(" = ")
        emit(node.`type`)
        writeLine()

      }
      def emitEnumDeclaration(node: EnumDeclaration): Unit = {
        write("sealed abstract class ")
        emit(node.name)
        writeLine()
        write("object ")
        emit(node.name)
        write(" {")
        writeLine()
        emitList(node.members, ListFormat.EnumMembers)
        writeLine()
        write("}")
        writeLine()

      }
      def emitModuleDeclaration(node: ModuleDeclaration): Unit = {
        write("object ")
        emit(node.name)
        write(" {")
        writeLine()
        emit(node.body)
        write("}")
        writeLine()

      }
      def emitModuleBlock(node: ModuleBlock): Unit = {
        (node.statements).foreach { fresh15 =>
          val stat = zeroOfMyType = fresh15
          emit(stat)
        }

      }
      def emitImportEqualsDeclaration(node: ImportEqualsDeclaration): Unit = {
        console.log("emitImportEqualsDeclaration")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitImportDeclaration(node: ImportDeclaration): Unit = {
        console.log("emitImportDeclaration")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitImportClause(node: ImportClause): Unit = {
        console.log("emitImportClause")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitNamespaceImport(node: NamespaceImport): Unit = {
        console.log("emitNamespaceImport")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitNamedImports(node: NamedImports): Unit = {
        console.log("emitNamedImports")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitImportSpecifier(node: ImportSpecifier): Unit = {
        console.log("emitImportSpecifier")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitExportAssignment(node: ExportAssignment): Unit = {
        console.log("emitExportAssignment")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitExportDeclaration(node: ExportDeclaration): Unit = {
        console.log("emitExportDeclaration")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitNamedExports(node: NamedExports): Unit = {
        console.log("emitNamedExports")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitExportSpecifier(node: ExportSpecifier): Unit = {
        console.log("emitExportSpecifier")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitHeritageClauses(nodes: NodeArray[HeritageClause]): Unit = {
        if (nodes) {
          (nodes).foreach { fresh16 =>
            val node = zeroOfMyType = fresh16 {
              emitHeritageClause(node)

            }
          }

        }

      }
      def emitHeritageClause(node: HeritageClause): Unit = {
        if (node.types) {
          var first = true
          (node.types).foreach { fresh17 =>
            val `type` = zeroOfMyType = fresh17 {
              if (first) {
                write(" extends ")
                (first = false)

              } else {
                write(" with ")

              }
              emit(`type`)

            }
          }

        }

      }
      def emitCatchClause(node: CatchClause): Unit = {
        write(" catch { case ")
        emit(node.variableDeclaration)
        write(": Throwable => ")
        emit(node.block)
        write("}")

      }
      def emitPropertyAssignment(node: PropertyAssignment): Unit = {
        if (node.initializer) {
          if ((node.name.kind === SyntaxKind.Identifier))
            write((("\"" + (node.name.asInstanceOf[Identifier]).text) + "\""))
          else
            emit(node.name)
          write(" -> ")
          emit(node.initializer)

        }

      }
      def emitShorthandPropertyAssignment(
          node: ShorthandPropertyAssignment): Unit = {
        write((("\"" + node.name.text) + "\" -> "))
        emit(node.name)

      }
      def emitEnumMember(node: EnumMember): Unit = {
        write("  case object ")
        emit(node.name)
        write(" extends ")
        emit((node.parent.asInstanceOf[EnumDeclaration]).name)

      }
      def emitExternalModuleReference(node: ExternalModuleReference): Unit = {
        console.log("emitExternalModuleReference")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitArrayLiteralExpression(node: ArrayLiteralExpression): Unit = {
        val elements = node.elements
        if ((elements.length === 0)) {
          write("Array()")

        } else {
          write("Array")
          emitExpressionList(elements,
              ListFormat.ArrayLiteralExpressionElements)

        }

      }
      def emitObjectLiteralExpression(node: ObjectLiteralExpression): Unit = {
        val properties = node.properties
        write("Map")
        emitList(properties, ListFormat.ObjectLiteralExpressionProperties)

      }
      def emitPropertyAccessExpression(
          node: PropertyAccessExpression): Unit = {
        emitExpression(node.expression)
        write(".")
        emit(node.name)

      }
      def emitElementAccessExpression(node: ElementAccessExpression): Unit = {
        emitExpression(node.expression)
        write("(")
        emitExpression(node.argumentExpression)
        write(")")

      }
      def emitCallExpression(node: CallExpression): Unit = {
        emitExpression(node.expression)
        emitTypeArguments(node.typeArguments)
        emitExpressionList(node.arguments, ListFormat.CallExpressionArguments)

      }
      def emitDecorators(decorators: NodeArray[Decorator]) = {
        emitList(decorators, ListFormat.Decorators)

      }
      def emitTypeArguments(typeArguments: NodeArray[TypeNode]) = {
        emitList(typeArguments, ListFormat.TypeArguments)

      }
      def emitNewExpression(node: NewExpression): Unit = {
        write("new ")
        emitExpression(node.expression)
        emitTypeArguments(node.typeArguments)
        emitExpressionList(node.arguments, ListFormat.NewExpressionArguments)

      }
      def emitTemplateExpression(node: TemplateExpression): Unit = {
        write("s")
        emit(node.head)
        emitList(node.templateSpans, ListFormat.TemplateExpressionSpans)

      }
      def emitTaggedTemplateExpression(
          node: TaggedTemplateExpression): Unit = {
        console.log("emitTaggedTemplateExpression")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitTypeAssertionExpression(node: TypeAssertion): Unit = {
        emitExpression(node.expression)
        write(".asInstanceOf[")
        emit(node.`type`)
        write("]")

      }
      def emitParenthesizedExpression(node: ParenthesizedExpression): Unit = {
        write("(")
        emitExpression(node.expression)
        write(")")

      }
      def emitFunctionExpression(node: FunctionExpression): Unit = {
        emitFunctionLikeExpression(node)

      }
      def emitArrowFunction(node: ArrowFunction): Unit = {
        emitFunctionLikeExpression(node)

      }
      def emitFunctionLikeExpression(node: FunctionLikeDeclaration): Unit = {
        write("(")
        emitDecorators(node.decorators)
        emitModifiers(node.modifiers)
        emitSignatureAndBody(node, emitArrowFunctionHead)
        write(")")

      }
      def emitArrowFunctionHead(node: FunctionLikeDeclaration) = {
        emitTypeParameters(node.typeParameters)
        emitParametersForArrow(node.parameters)
        write(" => ")

      }
      def emitParametersForArrow(
          parameters: NodeArray[ParameterDeclaration]) = {
        if (((parameters && (parameters.length === 1)) && (parameters(0).`type` === undefined))) {
          emit(parameters(0))

        } else {
          emitParameters(parameters)

        }

      }
      def emitParameters(parameters: NodeArray[ParameterDeclaration]) = {
        emitList(parameters, ListFormat.Parameters)

      }
      def emitSignatureAndBody(node: FunctionLikeDeclaration,
          emitSignatureHead: ((SignatureDeclaration) => Unit)) = {
        val body = node.body
        if (body) {
          if (isBlock(body)) {
            if ((getEmitFlags(node) & EmitFlags.ReuseTempVariableScope)) {
              emitSignatureHead(node)
              emitBlockFunctionBody(body)

            } else {
              emitSignatureHead(node)
              emitBlockFunctionBody(body)

            }

          } else {
            emitSignatureHead(node)
            write(" ")
            emitExpression(body)

          }

        } else {
          emitSignatureHead(node)
          write(";")

        }

      }
      def emitBlockFunctionBody(body: Block) = {
        write(" {")
        writeLine()
        emitList(body.statements, ListFormat.SingleLineFunctionBodyStatements)
        writeLine()
        write("}")

      }
      def emitDeleteExpression(node: DeleteExpression): Unit = {
        val expr = node.expression
        expr.kind match {
          case SyntaxKind.PropertyAccessExpression =>
            val propAccessExpr = expr.asInstanceOf[PropertyAccessExpression]
            emitExpression(propAccessExpr.expression)
            write(".remove(")
            write((("\"" + propAccessExpr.name.text) + "\""))
            write(")")
          case SyntaxKind.ElementAccessExpression =>
            val elemAccessExpr = expr.asInstanceOf[ElementAccessExpression]
            emitExpression(elemAccessExpr.expression)
            write(".remove(")
            emitExpression(elemAccessExpr.argumentExpression)
            write(")")
          case _ =>
            console.log(
                ("Need to handle emitDeleteExpression() with " + expr.kind))
        }

      }
      def emitTypeOfExpression(node: TypeOfExpression): Unit = {
        write("typeof(")
        emitExpression(node.expression)
        write(")")

      }
      def emitVoidExpression(node: VoidExpression): Unit = {
        console.log("emitVoidExpression")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitAwaitExpression(node: AwaitExpression): Unit = {
        console.log("emitAwaitExpression")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitPrefixUnaryExpression(node: PrefixUnaryExpression): Unit = {
        write("(")
        emitTokenText(node.operator)
        emitExpression(node.operand)
        write(")")

      }
      def emitPostfixUnaryExpression(node: PostfixUnaryExpression): Unit = {
        write("(")
        emitExpression(node.operand)
        node.operator match {
          case SyntaxKind.PlusPlusToken =>
            write("+= 1)")
          case SyntaxKind.MinusMinusToken =>
            write("-= 1)")
          case _ =>
        }

      }
      def emitBinaryExpression(node: BinaryExpression): Unit = {
        write("(")
        emitExpression(node.left)
        emitTokenText(node.operatorToken.kind)
        emitExpression(node.right)
        write(")")

      }
      def emitConditionalExpression(node: ConditionalExpression): Unit = {
        write("(")
        write("if (")
        emitExpression(node.condition)
        write(") ")
        emitExpression(node.whenTrue)
        write(" else ")
        emitExpression(node.whenFalse)
        write(")")

      }
      def emitYieldExpression(node: YieldExpression): Unit = {
        console.log("emitYieldExpression")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitSpreadElementExpression(node: SpreadElementExpression): Unit = {
        emitExpression(node.expression)
        write(": _*")

      }
      def emitClassExpression(node: ClassExpression): Unit = {
        console.log("emitClassExpression")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitAsExpression(node: AsExpression): Unit = {
        emitExpression(node.expression)
        if (node.`type`) {
          write(".asInstanceOf[")
          emit(node.`type`)
          write("]")

        }

      }
      def emitNonNullExpression(node: NonNullExpression): Unit = {
        console.log("emitNonNullExpression")
        console.log(("Need to handle node kind " + node.kind))

      }
      def emitList(children: NodeArray[Node], format: ListFormat, start: Int,
          count: Int) = {
        emitNodeList(emit, children, format, start, count)

      }
      def emitExpressionList(children: NodeArray[Node], format: ListFormat,
          start: Int, count: Int) = {
        emitNodeList(emitExpression, children, format, start, count)

      }
      def emitExpression(node: Node): Unit = {
        emit(node)

      }
      def emitNodeList(emit: ((Node) => Unit), children: NodeArray[Node],
          format: ListFormat, start: Nothing = 0,
          count: Nothing = (if (children) (children.length - start) else 0)) = {
        val isUndefined = (children === undefined)
        if ((isUndefined && (format & ListFormat.OptionalIfUndefined))) {
          return

        }
        val isEmpty = (((isUndefined || (children.length === 0)) || (start >= children.length)) || (count === 0))
        if ((isEmpty && (format & ListFormat.OptionalIfEmpty))) {
          return

        }
        if ((format & ListFormat.BracketsMask)) {
          write(getOpeningBracket(format))

        }
        if (isEmpty) {
          if ((format & ListFormat.MultiLine)) {
            writeLine()

          } else if ((format & ListFormat.SpaceBetweenBraces)) {
            write(" ")

          }

        } else {
          write(" ")
          var previousSibling: Node = zeroOfMyType
          val delimiter = getDelimiter(format) {
            var i = 0
            while ((i < count)) {
              {
                val child = children((start + i))
                if (previousSibling) {
                  write(delimiter)
                  if ((format & ListFormat.MultiLine))
                    writeLine()
                  else
                    write(" ")

                }
                emit(child)
                (previousSibling = child)

              }
              (i += 1)
            }
          }
          val hasTrailingComma = (((format & ListFormat.AllowTrailingComma)) && children.hasTrailingComma)
          if (((format & ListFormat.CommaDelimited) && hasTrailingComma)) {
            write(",")

          }
          write(" ")

        }
        if ((format & ListFormat.BracketsMask)) {
          write(getClosingBracket(format))

        }

      }
      def emitWithPrefix(prefix: String, node: Node) = {
        emitNodeWithPrefix(prefix, node, emit)

      }
      def emitExpressionWithPrefix(prefix: String, node: Node) = {
        emitNodeWithPrefix(prefix, node, emitExpression)

      }
      def emitNodeWithPrefix(prefix: String, node: Node,
          emit: ((Node) => Unit)) = {
        if (node) {
          write(prefix)
          emit(node)

        }

      }
      def getLiteralTextOfNode(node: LiteralLikeNode): String = {
        if (((node.kind === SyntaxKind.StringLiteral) && (
                node.asInstanceOf[StringLiteral]).textSourceNode)) {
          val textSourceNode =
            (node.asInstanceOf[StringLiteral]).textSourceNode
          if (isIdentifier(textSourceNode)) {
            return (("\"" + escapeNonAsciiCharacters(
                escapeString(getTextOfNode(textSourceNode)))) + "\"")

          } else {
            return getLiteralTextOfNode(textSourceNode)

          }

        }
        def getQuotedEscapedLiteralText(leftQuote: String, text: String,
            rightQuote: String) = {
          return ((leftQuote + escapeNonAsciiCharacters(escapeString(text))) + rightQuote)

        }
        node.kind match {
          case SyntaxKind.StringLiteral =>
            return getQuotedEscapedLiteralText("\"", node.text, "\"")
          case SyntaxKind.NoSubstitutionTemplateLiteral =>
            return getQuotedEscapedLiteralText("\"\"\"", node.text, "\"\"\"")
          case SyntaxKind.TemplateHead =>
            return getQuotedEscapedLiteralText("\"\"\"", node.text, "${")
          case SyntaxKind.TemplateMiddle =>
            return getQuotedEscapedLiteralText("}", node.text, "${")
          case SyntaxKind.TemplateTail =>
            return getQuotedEscapedLiteralText("}", node.text, "\"\"\"")
          case SyntaxKind.NumericLiteral =>
            return node.text
          case SyntaxKind.RegularExpressionLiteral =>
            val text = node.text.substring(1)
            val slashPos = text.lastIndexOf("/")
            val pattern0 = text.substring(0, slashPos)
            var pattern = "" {
              var i = 0
              while ((i < pattern0.length)) {
                {
                  if ((pattern0(i) == "$"))
                    (pattern += "$$")
                  else
                    (pattern += pattern0(i))

                }
                (++ i)
              }
            }
            val flags = text.substring((slashPos + 1))
            if ((flags === ""))
              return (("java.util.regex.Pattern.compile(raw\"\"\"" + pattern) + "\"\"\")")
            else
              return (((("java.util.regex.Pattern.compile(raw\"\"\"" + pattern) + "\"\"\", \"") + flags) + "\")")
          case _ =>
        }

      }
      def emitLiteral(node: LiteralLikeNode) = {
        write(getLiteralTextOfNode(node))

      }
      def emitNumericLiteral(node: NumericLiteral): Unit = {
        emitLiteral(node)

      }
      def emit(node: Node): Unit = {
        node.kind match {
          case SyntaxKind.SourceFile =>
            return emitSourceFile(node.asInstanceOf[SourceFile])
          case SyntaxKind.NumericLiteral =>
            return emitNumericLiteral(node.asInstanceOf[NumericLiteral])
          case SyntaxKind.StringLiteral | SyntaxKind.RegularExpressionLiteral |
              SyntaxKind.NoSubstitutionTemplateLiteral =>
            return emitLiteral(node.asInstanceOf[LiteralExpression])
          case SyntaxKind.TemplateHead | SyntaxKind.TemplateMiddle |
              SyntaxKind.TemplateTail =>
            return emitLiteral(node.asInstanceOf[LiteralExpression])
          case SyntaxKind.Identifier =>
            return emitIdentifier(node.asInstanceOf[Identifier])
          case SyntaxKind.FalseKeyword | SyntaxKind.NullKeyword |
              SyntaxKind.SuperKeyword | SyntaxKind.TrueKeyword |
              SyntaxKind.ThisKeyword | SyntaxKind.ConstKeyword |
              SyntaxKind.DefaultKeyword | SyntaxKind.ExportKeyword |
              SyntaxKind.VoidKeyword | SyntaxKind.PrivateKeyword |
              SyntaxKind.ProtectedKeyword | SyntaxKind.PublicKeyword |
              SyntaxKind.StaticKeyword | SyntaxKind.AbstractKeyword |
              SyntaxKind.AsKeyword | SyntaxKind.AnyKeyword |
              SyntaxKind.AsyncKeyword | SyntaxKind.AwaitKeyword |
              SyntaxKind.BooleanKeyword | SyntaxKind.ConstructorKeyword |
              SyntaxKind.DeclareKeyword | SyntaxKind.GetKeyword |
              SyntaxKind.IsKeyword | SyntaxKind.ModuleKeyword |
              SyntaxKind.NamespaceKeyword | SyntaxKind.NeverKeyword |
              SyntaxKind.ReadonlyKeyword | SyntaxKind.RequireKeyword |
              SyntaxKind.NumberKeyword | SyntaxKind.SetKeyword |
              SyntaxKind.StringKeyword | SyntaxKind.SymbolKeyword |
              SyntaxKind.TypeKeyword | SyntaxKind.UndefinedKeyword |
              SyntaxKind.FromKeyword | SyntaxKind.GlobalKeyword |
              SyntaxKind.OfKeyword =>
            emitTokenText(node.kind)
            return
          case SyntaxKind.QualifiedName =>
            return emitQualifiedName(node.asInstanceOf[QualifiedName])
          case SyntaxKind.ComputedPropertyName =>
            return emitComputedPropertyName(
                node.asInstanceOf[ComputedPropertyName])
          case SyntaxKind.TypeParameter =>
            return emitTypeParameter(
                node.asInstanceOf[TypeParameterDeclaration])
          case SyntaxKind.Parameter =>
            return emitParameter(node.asInstanceOf[ParameterDeclaration])
          case SyntaxKind.Decorator =>
            return emitDecorator(node.asInstanceOf[Decorator])
          case SyntaxKind.PropertySignature =>
            return emitPropertySignature(node.asInstanceOf[PropertySignature])
          case SyntaxKind.PropertyDeclaration =>
            return emitPropertyDeclaration(
                node.asInstanceOf[PropertyDeclaration])
          case SyntaxKind.MethodSignature =>
            return emitMethodSignature(node.asInstanceOf[MethodSignature])
          case SyntaxKind.MethodDeclaration =>
            return emitMethodDeclaration(node.asInstanceOf[MethodDeclaration])
          case SyntaxKind.Constructor =>
            return emitConstructor(node.asInstanceOf[ConstructorDeclaration])
          case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
            return emitAccessorDeclaration(
                node.asInstanceOf[AccessorDeclaration])
          case SyntaxKind.CallSignature =>
            return emitCallSignature(
                node.asInstanceOf[CallSignatureDeclaration])
          case SyntaxKind.ConstructSignature =>
            return emitConstructSignature(
                node.asInstanceOf[ConstructSignatureDeclaration])
          case SyntaxKind.IndexSignature =>
            return emitIndexSignature(
                node.asInstanceOf[IndexSignatureDeclaration])
          case SyntaxKind.TypePredicate =>
            return emitTypePredicate(node.asInstanceOf[TypePredicateNode])
          case SyntaxKind.TypeReference =>
            return emitTypeReference(node.asInstanceOf[TypeReferenceNode])
          case SyntaxKind.FunctionType =>
            return emitFunctionType(node.asInstanceOf[FunctionTypeNode])
          case SyntaxKind.ConstructorType =>
            return emitConstructorType(node.asInstanceOf[ConstructorTypeNode])
          case SyntaxKind.TypeQuery =>
            return emitTypeQuery(node.asInstanceOf[TypeQueryNode])
          case SyntaxKind.TypeLiteral =>
            return emitTypeLiteral(node.asInstanceOf[TypeLiteralNode])
          case SyntaxKind.ArrayType =>
            return emitArrayType(node.asInstanceOf[ArrayTypeNode])
          case SyntaxKind.TupleType =>
            return emitTupleType(node.asInstanceOf[TupleTypeNode])
          case SyntaxKind.UnionType =>
            return emitUnionType(node.asInstanceOf[UnionTypeNode])
          case SyntaxKind.IntersectionType =>
            return emitIntersectionType(
                node.asInstanceOf[IntersectionTypeNode])
          case SyntaxKind.ParenthesizedType =>
            return emitParenthesizedType(
                node.asInstanceOf[ParenthesizedTypeNode])
          case SyntaxKind.ExpressionWithTypeArguments =>
            return emitExpressionWithTypeArguments(
                node.asInstanceOf[ExpressionWithTypeArguments])
          case SyntaxKind.ThisType =>
            return emitThisType()
          case SyntaxKind.LiteralType =>
            return emitLiteralType(node.asInstanceOf[LiteralTypeNode])
          case SyntaxKind.ObjectBindingPattern =>
            return emitObjectBindingPattern(
                node.asInstanceOf[ObjectBindingPattern])
          case SyntaxKind.ArrayBindingPattern =>
            return emitArrayBindingPattern()
          case SyntaxKind.BindingElement =>
            return emitBindingElement()
          case SyntaxKind.TemplateSpan =>
            return emitTemplateSpan(node.asInstanceOf[TemplateSpan])
          case SyntaxKind.SemicolonClassElement =>
            return emitSemicolonClassElement()
          case SyntaxKind.Block =>
            return emitBlock(node.asInstanceOf[Block])
          case SyntaxKind.VariableStatement =>
            return emitVariableStatement(node.asInstanceOf[VariableStatement])
          case SyntaxKind.EmptyStatement =>
            return emitEmptyStatement()
          case SyntaxKind.ExpressionStatement =>
            return emitExpressionStatement(
                node.asInstanceOf[ExpressionStatement])
          case SyntaxKind.IfStatement =>
            return emitIfStatement(node.asInstanceOf[IfStatement])
          case SyntaxKind.DoStatement =>
            return emitDoStatement(node.asInstanceOf[DoStatement])
          case SyntaxKind.WhileStatement =>
            return emitWhileStatement(node.asInstanceOf[WhileStatement])
          case SyntaxKind.ForStatement =>
            return emitForStatement(node.asInstanceOf[ForStatement])
          case SyntaxKind.ForInStatement =>
            return emitForInStatement(node.asInstanceOf[ForInStatement])
          case SyntaxKind.ForOfStatement =>
            return emitForOfStatement(node.asInstanceOf[ForOfStatement])
          case SyntaxKind.ContinueStatement =>
            return emitContinueStatement(node.asInstanceOf[ContinueStatement])
          case SyntaxKind.BreakStatement =>
            return emitBreakStatement(node.asInstanceOf[BreakStatement])
          case SyntaxKind.ReturnStatement =>
            return emitReturnStatement(node.asInstanceOf[ReturnStatement])
          case SyntaxKind.WithStatement =>
            return emitWithStatement(node.asInstanceOf[WithStatement])
          case SyntaxKind.SwitchStatement =>
            return emitSwitchStatement(node.asInstanceOf[SwitchStatement])
          case SyntaxKind.LabeledStatement =>
            return emitLabeledStatement(node.asInstanceOf[LabeledStatement])
          case SyntaxKind.ThrowStatement =>
            return emitThrowStatement(node.asInstanceOf[ThrowStatement])
          case SyntaxKind.TryStatement =>
            return emitTryStatement(node.asInstanceOf[TryStatement])
          case SyntaxKind.DebuggerStatement =>
            return emitDebuggerStatement()
          case SyntaxKind.VariableDeclaration =>
            return emitVariableDeclaration(
                node.asInstanceOf[VariableDeclaration])
          case SyntaxKind.VariableDeclarationList =>
            return emitVariableDeclarationList(
                node.asInstanceOf[VariableDeclarationList])
          case SyntaxKind.FunctionDeclaration =>
            return emitFunctionDeclaration(
                node.asInstanceOf[FunctionDeclaration])
          case SyntaxKind.ClassDeclaration =>
            return emitClassDeclaration(node.asInstanceOf[ClassDeclaration])
          case SyntaxKind.InterfaceDeclaration =>
            return emitInterfaceDeclaration(
                node.asInstanceOf[InterfaceDeclaration])
          case SyntaxKind.TypeAliasDeclaration =>
            return emitTypeAliasDeclaration(
                node.asInstanceOf[TypeAliasDeclaration])
          case SyntaxKind.EnumDeclaration =>
            return emitEnumDeclaration(node.asInstanceOf[EnumDeclaration])
          case SyntaxKind.ModuleDeclaration =>
            return emitModuleDeclaration(node.asInstanceOf[ModuleDeclaration])
          case SyntaxKind.ModuleBlock =>
            return emitModuleBlock(node.asInstanceOf[ModuleBlock])
          case SyntaxKind.ImportEqualsDeclaration =>
            return emitImportEqualsDeclaration(
                node.asInstanceOf[ImportEqualsDeclaration])
          case SyntaxKind.ImportDeclaration =>
            return emitImportDeclaration(node.asInstanceOf[ImportDeclaration])
          case SyntaxKind.ImportClause =>
            return emitImportClause(node.asInstanceOf[ImportClause])
          case SyntaxKind.NamespaceImport =>
            return emitNamespaceImport(node.asInstanceOf[NamespaceImport])
          case SyntaxKind.NamedImports =>
            return emitNamedImports(node.asInstanceOf[NamedImports])
          case SyntaxKind.ImportSpecifier =>
            return emitImportSpecifier(node.asInstanceOf[ImportSpecifier])
          case SyntaxKind.ExportAssignment =>
            return emitExportAssignment(node.asInstanceOf[ExportAssignment])
          case SyntaxKind.ExportDeclaration =>
            return emitExportDeclaration(node.asInstanceOf[ExportDeclaration])
          case SyntaxKind.NamedExports =>
            return emitNamedExports(node.asInstanceOf[NamedExports])
          case SyntaxKind.ExportSpecifier =>
            return emitExportSpecifier(node.asInstanceOf[ExportSpecifier])
          case SyntaxKind.MissingDeclaration =>
            return
          case SyntaxKind.ExternalModuleReference =>
            return emitExternalModuleReference(
                node.asInstanceOf[ExternalModuleReference])
          case SyntaxKind.ArrayLiteralExpression =>
            return emitArrayLiteralExpression(
                node.asInstanceOf[ArrayLiteralExpression])
          case SyntaxKind.ObjectLiteralExpression =>
            return emitObjectLiteralExpression(
                node.asInstanceOf[ObjectLiteralExpression])
          case SyntaxKind.PropertyAccessExpression =>
            return emitPropertyAccessExpression(
                node.asInstanceOf[PropertyAccessExpression])
          case SyntaxKind.ElementAccessExpression =>
            return emitElementAccessExpression(
                node.asInstanceOf[ElementAccessExpression])
          case SyntaxKind.CallExpression =>
            return emitCallExpression(node.asInstanceOf[CallExpression])
          case SyntaxKind.NewExpression =>
            return emitNewExpression(node.asInstanceOf[NewExpression])
          case SyntaxKind.TaggedTemplateExpression =>
            return emitTaggedTemplateExpression(
                node.asInstanceOf[TaggedTemplateExpression])
          case SyntaxKind.TypeAssertionExpression =>
            return emitTypeAssertionExpression(
                node.asInstanceOf[TypeAssertion])
          case SyntaxKind.ParenthesizedExpression =>
            return emitParenthesizedExpression(
                node.asInstanceOf[ParenthesizedExpression])
          case SyntaxKind.FunctionExpression =>
            return emitFunctionExpression(
                node.asInstanceOf[FunctionExpression])
          case SyntaxKind.ArrowFunction =>
            return emitArrowFunction(node.asInstanceOf[ArrowFunction])
          case SyntaxKind.DeleteExpression =>
            return emitDeleteExpression(node.asInstanceOf[DeleteExpression])
          case SyntaxKind.TypeOfExpression =>
            return emitTypeOfExpression(node.asInstanceOf[TypeOfExpression])
          case SyntaxKind.VoidExpression =>
            return emitVoidExpression(node.asInstanceOf[VoidExpression])
          case SyntaxKind.AwaitExpression =>
            return emitAwaitExpression(node.asInstanceOf[AwaitExpression])
          case SyntaxKind.PrefixUnaryExpression =>
            return emitPrefixUnaryExpression(
                node.asInstanceOf[PrefixUnaryExpression])
          case SyntaxKind.PostfixUnaryExpression =>
            return emitPostfixUnaryExpression(
                node.asInstanceOf[PostfixUnaryExpression])
          case SyntaxKind.BinaryExpression =>
            return emitBinaryExpression(node.asInstanceOf[BinaryExpression])
          case SyntaxKind.ConditionalExpression =>
            return emitConditionalExpression(
                node.asInstanceOf[ConditionalExpression])
          case SyntaxKind.TemplateExpression =>
            return emitTemplateExpression(
                node.asInstanceOf[TemplateExpression])
          case SyntaxKind.YieldExpression =>
            return emitYieldExpression(node.asInstanceOf[YieldExpression])
          case SyntaxKind.SpreadElementExpression =>
            return emitSpreadElementExpression(
                node.asInstanceOf[SpreadElementExpression])
          case SyntaxKind.ClassExpression =>
            return emitClassExpression(node.asInstanceOf[ClassExpression])
          case SyntaxKind.OmittedExpression =>
            return
          case SyntaxKind.AsExpression =>
            return emitAsExpression(node.asInstanceOf[AsExpression])
          case SyntaxKind.NonNullExpression =>
            return emitNonNullExpression(node.asInstanceOf[NonNullExpression])
          case SyntaxKind.JsxText =>
            return
          case SyntaxKind.JsxOpeningElement =>
            return
          case SyntaxKind.JsxClosingElement =>
            return
          case SyntaxKind.JsxAttribute =>
            return
          case SyntaxKind.JsxSpreadAttribute =>
            return
          case SyntaxKind.JsxExpression =>
            return
          case SyntaxKind.HeritageClause =>
            return emitHeritageClause(node.asInstanceOf[HeritageClause])
          case SyntaxKind.CatchClause =>
            return emitCatchClause(node.asInstanceOf[CatchClause])
          case SyntaxKind.PropertyAssignment =>
            return emitPropertyAssignment(
                node.asInstanceOf[PropertyAssignment])
          case SyntaxKind.ShorthandPropertyAssignment =>
            return emitShorthandPropertyAssignment(
                node.asInstanceOf[ShorthandPropertyAssignment])
          case SyntaxKind.EnumMember =>
            return emitEnumMember(node.asInstanceOf[EnumMember])
          case _ =>
            console.log(("uknown node kind: " + node.kind))
            return
        }

      }

    }

  }
  sealed abstract class ListFormat
  object ListFormat {
    case object None extends ListFormat
    case object SingleLine extends ListFormat
    case object MultiLine extends ListFormat
    case object PreserveLines extends ListFormat
    case object LinesMask extends ListFormat
    case object NotDelimited extends ListFormat
    case object BarDelimited extends ListFormat
    case object WithDelimited extends ListFormat
    case object CommaDelimited extends ListFormat
    case object DelimitersMask extends ListFormat
    case object AllowTrailingComma extends ListFormat
    case object Indented extends ListFormat
    case object SpaceBetweenBraces extends ListFormat
    case object SpaceBetweenSiblings extends ListFormat
    case object Braces extends ListFormat
    case object Parenthesis extends ListFormat
    case object AngleBrackets extends ListFormat
    case object SquareBrackets extends ListFormat
    case object BracketsMask extends ListFormat
    case object OptionalIfUndefined extends ListFormat
    case object OptionalIfEmpty extends ListFormat
    case object Optional extends ListFormat
    case object PreferNewLine extends ListFormat
    case object NoTrailingNewLine extends ListFormat
    case object NoInterveningComments extends ListFormat
    case object Modifiers extends ListFormat
    case object HeritageClauses extends ListFormat
    case object TypeLiteralMembers extends ListFormat
    case object TupleTypeElements extends ListFormat
    case object UnionTypeConstituents extends ListFormat
    case object IntersectionTypeConstituents extends ListFormat
    case object ObjectBindingPatternElements extends ListFormat
    case object ArrayBindingPatternElements extends ListFormat
    case object ObjectLiteralExpressionProperties extends ListFormat
    case object ArrayLiteralExpressionElements extends ListFormat
    case object CallExpressionArguments extends ListFormat
    case object NewExpressionArguments extends ListFormat
    case object TemplateExpressionSpans extends ListFormat
    case object SingleLineBlockStatements extends ListFormat
    case object MultiLineBlockStatements extends ListFormat
    case object VariableDeclarationList extends ListFormat
    case object SingleLineFunctionBodyStatements extends ListFormat
    case object MultiLineFunctionBodyStatements extends ListFormat
    case object ClassHeritageClauses extends ListFormat
    case object ClassMembers extends ListFormat
    case object InterfaceMembers extends ListFormat
    case object EnumMembers extends ListFormat
    case object CaseBlockClauses extends ListFormat
    case object NamedImportsOrExportsElements extends ListFormat
    case object JsxElementChildren extends ListFormat
    case object JsxElementAttributes extends ListFormat
    case object CaseOrDefaultClauseStatements extends ListFormat
    case object HeritageClauseTypes extends ListFormat
    case object SourceFileStatements extends ListFormat
    case object Decorators extends ListFormat
    case object TypeArguments extends ListFormat
    case object TypeParameters extends ListFormat
    case object Parameters extends ListFormat
    case object IndexSignatureParameters extends ListFormat
  }
  def isSingleLineEmptyBlock(block: Block) = {
    return ((!block.multiLine) && isEmptyBlock(block))

  }
  def isEmptyBlock(block: BlockLike) = {
    return (block.statements.length === 0)

  }
  def createDelimiterMap() = {
    val delimiters: Array[String] = Array()
    (delimiters(ListFormat.None) = "")
    (delimiters(ListFormat.CommaDelimited) = ",")
    (delimiters(ListFormat.BarDelimited) = " |")
    (delimiters(ListFormat.WithDelimited) = " with")
    return delimiters

  }
  def createBracketsMap() = {
    val brackets: Array[Array[String]] = Array()
    (brackets(ListFormat.Braces) = Array("{", "}"))
    (brackets(ListFormat.Parenthesis) = Array("(", ")"))
    (brackets(ListFormat.AngleBrackets) = Array("<", ">"))
    (brackets(ListFormat.SquareBrackets) = Array("[", "]"))
    return brackets

  }
  val brackets = createBracketsMap()
  def getOpeningBracket(format: ListFormat) = {
    return brackets((format & ListFormat.BracketsMask))(0)

  }
  def getClosingBracket(format: ListFormat) = {
    return brackets((format & ListFormat.BracketsMask))(1)

  }
  val delimeters = createDelimiterMap()
  def getDelimiter(format: ListFormat) = {
    return delimeters((format & ListFormat.DelimitersMask))

  }
}
