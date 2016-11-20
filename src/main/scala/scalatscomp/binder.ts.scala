package scalatscomp
object Binder {
  sealed abstract class ModuleInstanceState
  object ModuleInstanceState {
    case object NonInstantiated extends ModuleInstanceState
    case object Instantiated extends ModuleInstanceState
    case object ConstEnumOnly extends ModuleInstanceState
  }
  trait ActiveLabel {
    var name: String
    var breakTarget: FlowLabel
    var continueTarget: FlowLabel
    var referenced: Boolean
  }
  def getModuleInstanceState(node: Node): ModuleInstanceState = {
    if (((node.kind === SyntaxKind.InterfaceDeclaration) || (node.kind === SyntaxKind.TypeAliasDeclaration))) {
      return ModuleInstanceState.NonInstantiated

    } else if (isConstEnumDeclaration(node)) {
      return ModuleInstanceState.ConstEnumOnly

    } else if (((((node.kind === SyntaxKind.ImportDeclaration) || (node.kind === SyntaxKind.ImportEqualsDeclaration))) && (!(
            hasModifier(node, ModifierFlags.Export))))) {
      return ModuleInstanceState.NonInstantiated

    } else if ((node.kind === SyntaxKind.ModuleBlock)) {
      var state = ModuleInstanceState.NonInstantiated
      forEachChild(node,
          (n => {
            getModuleInstanceState(n) match {
              case ModuleInstanceState.NonInstantiated =>
                return false
              case ModuleInstanceState.ConstEnumOnly =>
                (state = ModuleInstanceState.ConstEnumOnly)
                return false
              case ModuleInstanceState.Instantiated =>
                (state = ModuleInstanceState.Instantiated)
                return true
              case _ =>
            }

          }))
      return state

    } else if ((node.kind === SyntaxKind.ModuleDeclaration)) {
      val body = (node.asInstanceOf[ModuleDeclaration]).body
      return (if (body) getModuleInstanceState(body)
              else ModuleInstanceState.Instantiated)

    } else if (((node.kind === SyntaxKind.Identifier) && (
            node.asInstanceOf[Identifier]).isInJSDocNamespace)) {
      return ModuleInstanceState.NonInstantiated

    } else {
      return ModuleInstanceState.Instantiated

    }

  }
  sealed abstract class ContainerFlags
  object ContainerFlags {
    case object None extends ContainerFlags
    case object IsContainer extends ContainerFlags
    case object IsBlockScopedContainer extends ContainerFlags
    case object IsControlFlowContainer extends ContainerFlags
    case object IsFunctionLike extends ContainerFlags
    case object IsFunctionExpression extends ContainerFlags
    case object HasLocals extends ContainerFlags
    case object IsInterface extends ContainerFlags
    case object IsObjectLiteralOrClassExpressionMethod extends ContainerFlags
  }
  val binder = createBinder()
  def bindSourceFile(file: SourceFile, options: CompilerOptions) = {
    performance.mark("beforeBind")
    binder(file, options)
    performance.mark("afterBind")
    performance.measure("Bind", "beforeBind", "afterBind")

  }
  def createBinder(): ((SourceFile, CompilerOptions) => Unit) = {
    var file: SourceFile = zeroOfMyType
    var options: CompilerOptions = zeroOfMyType
    var languageVersion: ScriptTarget = zeroOfMyType
    var parent: Node = zeroOfMyType
    var container: Node = zeroOfMyType
    var blockScopeContainer: Node = zeroOfMyType
    var lastContainer: Node = zeroOfMyType
    var seenThisKeyword: Boolean = zeroOfMyType
    var currentFlow: FlowNode = zeroOfMyType
    var currentBreakTarget: FlowLabel = zeroOfMyType
    var currentContinueTarget: FlowLabel = zeroOfMyType
    var currentReturnTarget: FlowLabel = zeroOfMyType
    var currentTrueTarget: FlowLabel = zeroOfMyType
    var currentFalseTarget: FlowLabel = zeroOfMyType
    var preSwitchCaseFlow: FlowNode = zeroOfMyType
    var activeLabels: Array[ActiveLabel] = zeroOfMyType
    var hasExplicitReturn: Boolean = zeroOfMyType
    var emitFlags: NodeFlags = zeroOfMyType
    var inStrictMode: Boolean = zeroOfMyType
    var symbolCount = 0
    var Symbol: { def `new`(flags: SymbolFlags, name: String): Symbol } =
      zeroOfMyType
    var classifiableNames: Map[String] = zeroOfMyType
    val unreachableFlow: FlowNode = Map("flags" -> FlowFlags.Unreachable)
    val reportedUnreachableFlow: FlowNode =
      Map("flags" -> FlowFlags.Unreachable)
    var subtreeTransformFlags: TransformFlags = TransformFlags.None
    var skipTransformFlagAggregation: Boolean = zeroOfMyType
    def bindSourceFile(f: SourceFile, opts: CompilerOptions) = {
      (file = f)
      (options = opts)
      (languageVersion = getEmitScriptTarget(options))
      (inStrictMode = bindInStrictMode(file, opts))
      (classifiableNames = createMap[String]())
      (symbolCount = 0)
      (skipTransformFlagAggregation = isDeclarationFile(file))
      (Symbol = objectAllocator.getSymbolConstructor())
      if ((!file.locals)) {
        bind(file)
        (file.symbolCount = symbolCount)
        (file.classifiableNames = classifiableNames)

      }
      (file = undefined)
      (options = undefined)
      (languageVersion = undefined)
      (parent = undefined)
      (container = undefined)
      (blockScopeContainer = undefined)
      (lastContainer = undefined)
      (seenThisKeyword = false)
      (currentFlow = undefined)
      (currentBreakTarget = undefined)
      (currentContinueTarget = undefined)
      (currentReturnTarget = undefined)
      (currentTrueTarget = undefined)
      (currentFalseTarget = undefined)
      (activeLabels = undefined)
      (hasExplicitReturn = false)
      (emitFlags = NodeFlags.None)
      (subtreeTransformFlags = TransformFlags.None)

    }
    return bindSourceFile
    def bindInStrictMode(file: SourceFile, opts: CompilerOptions): Boolean = {
      if ((opts.alwaysStrict && (!isDeclarationFile(file)))) {
        return true

      } else {
        return (!(!file.externalModuleIndicator))

      }

    }
    def createSymbol(flags: SymbolFlags, name: String): Symbol = {
      (symbolCount += 1)
      return new Symbol(flags, name)

    }
    def addDeclarationToSymbol(symbol: Symbol, node: Declaration,
        symbolFlags: SymbolFlags) = {
      (symbol.flags |= symbolFlags)
      (node.symbol = symbol)
      if ((!symbol.declarations)) {
        (symbol.declarations = Array())

      }
      symbol.declarations.push(node)
      if (((symbolFlags & SymbolFlags.HasExports) && (!symbol.exports))) {
        (symbol.exports = createMap[Symbol]())

      }
      if (((symbolFlags & SymbolFlags.HasMembers) && (!symbol.members))) {
        (symbol.members = createMap[Symbol]())

      }
      if ((symbolFlags & SymbolFlags.Value)) {
        val valueDeclaration = symbol.valueDeclaration
        if (((!valueDeclaration) || (((valueDeclaration.kind !== node.kind) && (valueDeclaration.kind === SyntaxKind.ModuleDeclaration))))) {
          (symbol.valueDeclaration = node)

        }

      }

    }
    def getDeclarationName(node: Declaration): String = {
      if (node.name) {
        if (isAmbientModule(node)) {
          return (if (isGlobalScopeAugmentation(
                          node.asInstanceOf[ModuleDeclaration])) "__global"
                  else
                    s"""\"${(node.name.asInstanceOf[LiteralExpression]).text}\"""")

        }
        if ((node.name.kind === SyntaxKind.ComputedPropertyName)) {
          val nameExpression =
            (node.name.asInstanceOf[ComputedPropertyName]).expression
          if (isStringOrNumericLiteral(nameExpression.kind)) {
            return (nameExpression.asInstanceOf[LiteralExpression]).text

          }
          Debug.assert(isWellKnownSymbolSyntactically(nameExpression))
          return getPropertyNameForKnownSymbolName((nameExpression
                    .asInstanceOf[PropertyAccessExpression])
                .name
                .text)

        }
        return (node.name.asInstanceOf[(Identifier | LiteralExpression)]).text

      }
      node.kind match {
        case SyntaxKind.Constructor =>
          return "__constructor"
        case SyntaxKind.FunctionType | SyntaxKind.CallSignature =>
          return "__call"
        case SyntaxKind.ConstructorType | SyntaxKind.ConstructSignature =>
          return "__new"
        case SyntaxKind.IndexSignature =>
          return "__index"
        case SyntaxKind.ExportDeclaration =>
          return "__export"
        case SyntaxKind.ExportAssignment =>
          return (if ((node.asInstanceOf[ExportAssignment]).isExportEquals)
                    "export="
                  else "default")
        case SyntaxKind.BinaryExpression =>
          getSpecialPropertyAssignmentKind(node) match {
            case SpecialPropertyAssignmentKind.ModuleExports =>
              return "export="
            case SpecialPropertyAssignmentKind.ExportsProperty |
                SpecialPropertyAssignmentKind.ThisProperty =>
              return (
                  (node.asInstanceOf[BinaryExpression]).left.asInstanceOf[PropertyAccessExpression]).name.text
            case SpecialPropertyAssignmentKind.PrototypeProperty =>
              return (
                  ((node.asInstanceOf[BinaryExpression]).left.asInstanceOf[PropertyAccessExpression]).expression.asInstanceOf[PropertyAccessExpression]).name.text
            case _ =>
          }
          Debug.fail("Unknown binary declaration kind")
        case SyntaxKind.FunctionDeclaration | SyntaxKind.ClassDeclaration =>
          return (if (hasModifier(node, ModifierFlags.Default)) "default"
                  else undefined)
        case SyntaxKind.JSDocFunctionType =>
          return (if (isJSDocConstructSignature(node)) "__new" else "__call")
        case SyntaxKind.Parameter =>
          Debug.assert((node.parent.kind === SyntaxKind.JSDocFunctionType))
          var functionType = node.parent.asInstanceOf[JSDocFunctionType]
          var index = indexOf(functionType.parameters, node)
          return ("arg" + index)
        case SyntaxKind.JSDocTypedefTag =>
          val parentNode = (node.parent && node.parent.parent)
          var nameFromParentNode: String = zeroOfMyType
          if ((parentNode && (parentNode.kind === SyntaxKind.VariableStatement))) {
            if (((parentNode.asInstanceOf[VariableStatement]).declarationList.declarations.length > 0)) {
              val nameIdentifier =
                (parentNode.asInstanceOf[VariableStatement]).declarationList.declarations(0).name
              if ((nameIdentifier.kind === SyntaxKind.Identifier)) {
                (nameFromParentNode =
                  (nameIdentifier.asInstanceOf[Identifier]).text)

              }

            }

          }
          return nameFromParentNode
        case _ =>
      }

    }
    def getDisplayName(node: Declaration): String = {
      return (if (node.name) declarationNameToString(node.name)
              else getDeclarationName(node))

    }
    def declareSymbol(symbolTable: SymbolTable, parent: Symbol,
        node: Declaration, includes: SymbolFlags,
        excludes: SymbolFlags): Symbol = {
      Debug.assert((!hasDynamicName(node)))
      val isDefaultExport = hasModifier(node, ModifierFlags.Default)
      val name =
        (if ((isDefaultExport && parent)) "default"
         else getDeclarationName(node))
      var symbol: Symbol = zeroOfMyType
      if ((name === undefined)) {
        (symbol = createSymbol(SymbolFlags.None, "__missing"))

      } else {
        (symbol = (symbolTable(name) || (
              (symbolTable(name) = createSymbol(SymbolFlags.None, name)))))
        if ((name && ((includes & SymbolFlags.Classifiable)))) {
          (classifiableNames(name) = name)

        }
        if ((symbol.flags & excludes)) {
          if (symbol.isReplaceableByMethod) {
            (symbol =
              (symbolTable(name) = createSymbol(SymbolFlags.None, name)))

          } else {
            if (node.name) {
              (node.name.parent = node)

            }
            var message =
              (if ((symbol.flags & SymbolFlags.BlockScopedVariable))
                 Diagnostics.Cannot_redeclare_block_scoped_variable_0
               else Diagnostics.Duplicate_identifier_0)
            if ((symbol.declarations && symbol.declarations.length)) {
              if (isDefaultExport) {
                (message =
                  Diagnostics.A_module_cannot_have_multiple_default_exports)

              } else {
                if (((symbol.declarations && symbol.declarations.length) && ((isDefaultExport || (((node.kind === SyntaxKind.ExportAssignment) && (!(
                        node
                          .asInstanceOf[ExportAssignment])
                      .isExportEquals))))))) {
                  (message =
                    Diagnostics.A_module_cannot_have_multiple_default_exports)

                }

              }

            }
            forEach(symbol.declarations,
                (declaration => {
                  file.bindDiagnostics.push(
                      createDiagnosticForNode(
                          (declaration.name || declaration), message,
                          getDisplayName(declaration)))

                }))
            file.bindDiagnostics.push(createDiagnosticForNode(
                    (node.name || node), message, getDisplayName(node)))
            (symbol = createSymbol(SymbolFlags.None, name))

          }

        }

      }
      addDeclarationToSymbol(symbol, node, includes)
      (symbol.parent = parent)
      return symbol

    }
    def declareModuleMember(node: Declaration, symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags): Symbol = {
      val hasExportModifier = (getCombinedModifierFlags(node) & ModifierFlags.Export)
      if ((symbolFlags & SymbolFlags.Alias)) {
        if (((node.kind === SyntaxKind.ExportSpecifier) || (((node.kind === SyntaxKind.ImportEqualsDeclaration) && hasExportModifier)))) {
          return declareSymbol(container.symbol.exports, container.symbol,
              node, symbolFlags, symbolExcludes)

        } else {
          return declareSymbol(container.locals, undefined, node, symbolFlags,
              symbolExcludes)

        }

      } else {
        val isJSDocTypedefInJSDocNamespace = ((((node.kind === SyntaxKind.JSDocTypedefTag) && node.name) && (node.name.kind === SyntaxKind.Identifier)) && (
              node.name.asInstanceOf[Identifier]).isInJSDocNamespace)
        if (((((!isAmbientModule(node)) && ((hasExportModifier || (container.flags & NodeFlags.ExportContext))))) || isJSDocTypedefInJSDocNamespace)) {
          val exportKind = ((((if ((symbolFlags & SymbolFlags.Value))
                  SymbolFlags.ExportValue
                else 0) ) | ((if ((symbolFlags & SymbolFlags.Type))
                  SymbolFlags.ExportType
                else 0) )) | ((if ((symbolFlags & SymbolFlags.Namespace))
                  SymbolFlags.ExportNamespace
                else 0) ))
          val local = declareSymbol(container.locals, undefined, node,
              exportKind, symbolExcludes)
          (local.exportSymbol = declareSymbol(container.symbol.exports,
              container.symbol, node, symbolFlags, symbolExcludes))
          (node.localSymbol = local)
          return local

        } else {
          return declareSymbol(container.locals, undefined, node, symbolFlags,
              symbolExcludes)

        }

      }

    }
    def bindContainer(node: Node, containerFlags: ContainerFlags) = {
      val saveContainer = container
      val savedBlockScopeContainer = blockScopeContainer
      if ((containerFlags & ContainerFlags.IsContainer)) {
        (container = (blockScopeContainer = node))
        if ((containerFlags & ContainerFlags.HasLocals)) {
          (container.locals = createMap[Symbol]())

        }
        addToContainerChain(container)

      } else if ((containerFlags & ContainerFlags.IsBlockScopedContainer)) {
        (blockScopeContainer = node)
        (blockScopeContainer.locals = undefined)

      }
      if ((containerFlags & ContainerFlags.IsControlFlowContainer)) {
        val saveCurrentFlow = currentFlow
        val saveBreakTarget = currentBreakTarget
        val saveContinueTarget = currentContinueTarget
        val saveReturnTarget = currentReturnTarget
        val saveActiveLabels = activeLabels
        val saveHasExplicitReturn = hasExplicitReturn
        val isIIFE = ((containerFlags & ContainerFlags.IsFunctionExpression) && (!(!getImmediatelyInvokedFunctionExpression(
              node))))
        if (isIIFE) {
          (currentReturnTarget = createBranchLabel())

        } else {
          (currentFlow = Map("flags" -> FlowFlags.Start))
          if ((containerFlags & ((ContainerFlags.IsFunctionExpression | ContainerFlags.IsObjectLiteralOrClassExpressionMethod)))) {
            ((currentFlow.asInstanceOf[FlowStart]).container =
              node.asInstanceOf[
                  (FunctionExpression | ArrowFunction | MethodDeclaration)])

          }
          (currentReturnTarget = undefined)

        }
        (currentBreakTarget = undefined)
        (currentContinueTarget = undefined)
        (activeLabels = undefined)
        (hasExplicitReturn = false)
        bindChildren(node)
        (node.flags &= (~NodeFlags.ReachabilityAndEmitFlags))
        if ((((!((currentFlow.flags & FlowFlags.Unreachable))) && (containerFlags & ContainerFlags.IsFunctionLike)) && nodeIsPresent(
                (node.asInstanceOf[FunctionLikeDeclaration]).body))) {
          (node.flags |= NodeFlags.HasImplicitReturn)
          if (hasExplicitReturn)
            (node.flags |= NodeFlags.HasExplicitReturn)

        }
        if ((node.kind === SyntaxKind.SourceFile)) {
          (node.flags |= emitFlags)

        }
        if (isIIFE) {
          addAntecedent(currentReturnTarget, currentFlow)
          (currentFlow = finishFlowLabel(currentReturnTarget))

        } else {
          (currentFlow = saveCurrentFlow)

        }
        (currentBreakTarget = saveBreakTarget)
        (currentContinueTarget = saveContinueTarget)
        (currentReturnTarget = saveReturnTarget)
        (activeLabels = saveActiveLabels)
        (hasExplicitReturn = saveHasExplicitReturn)

      } else if ((containerFlags & ContainerFlags.IsInterface)) {
        (seenThisKeyword = false)
        bindChildren(node)
        (node.flags =
          (if (seenThisKeyword) (node.flags | NodeFlags.ContainsThis)
           else (node.flags & (~NodeFlags.ContainsThis))))

      } else {
        bindChildren(node)

      }
      (container = saveContainer)
      (blockScopeContainer = savedBlockScopeContainer)

    }
    def bindChildren(node: Node): Unit = {
      if (skipTransformFlagAggregation) {
        bindChildrenWorker(node)

      } else if ((node.transformFlags & TransformFlags.HasComputedFlags)) {
        (skipTransformFlagAggregation = true)
        bindChildrenWorker(node)
        (skipTransformFlagAggregation = false)

      } else {
        val savedSubtreeTransformFlags = subtreeTransformFlags
        (subtreeTransformFlags = 0)
        bindChildrenWorker(node)
        (subtreeTransformFlags = (savedSubtreeTransformFlags | computeTransformFlagsForNode(
              node, subtreeTransformFlags)))

      }

    }
    def bindChildrenWorker(node: Node): Unit = {
      if ((isInJavaScriptFile(node) && node.jsDocComments)) {
        forEach(node.jsDocComments, bind)

      }
      if (checkUnreachable(node)) {
        forEachChild(node, bind)
        return

      }
      node.kind match {
        case SyntaxKind.WhileStatement =>
          bindWhileStatement(node.asInstanceOf[WhileStatement])
        case SyntaxKind.DoStatement =>
          bindDoStatement(node.asInstanceOf[DoStatement])
        case SyntaxKind.ForStatement =>
          bindForStatement(node.asInstanceOf[ForStatement])
        case SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement =>
          bindForInOrForOfStatement(
              node.asInstanceOf[(ForInStatement | ForOfStatement)])
        case SyntaxKind.IfStatement =>
          bindIfStatement(node.asInstanceOf[IfStatement])
        case SyntaxKind.ReturnStatement | SyntaxKind.ThrowStatement =>
          bindReturnOrThrow(
              node.asInstanceOf[(ReturnStatement | ThrowStatement)])
        case SyntaxKind.BreakStatement | SyntaxKind.ContinueStatement =>
          bindBreakOrContinueStatement(
              node.asInstanceOf[BreakOrContinueStatement])
        case SyntaxKind.TryStatement =>
          bindTryStatement(node.asInstanceOf[TryStatement])
        case SyntaxKind.SwitchStatement =>
          bindSwitchStatement(node.asInstanceOf[SwitchStatement])
        case SyntaxKind.CaseBlock =>
          bindCaseBlock(node.asInstanceOf[CaseBlock])
        case SyntaxKind.CaseClause =>
          bindCaseClause(node.asInstanceOf[CaseClause])
        case SyntaxKind.LabeledStatement =>
          bindLabeledStatement(node.asInstanceOf[LabeledStatement])
        case SyntaxKind.PrefixUnaryExpression =>
          bindPrefixUnaryExpressionFlow(
              node.asInstanceOf[PrefixUnaryExpression])
        case SyntaxKind.PostfixUnaryExpression =>
          bindPostfixUnaryExpressionFlow(
              node.asInstanceOf[PostfixUnaryExpression])
        case SyntaxKind.BinaryExpression =>
          bindBinaryExpressionFlow(node.asInstanceOf[BinaryExpression])
        case SyntaxKind.DeleteExpression =>
          bindDeleteExpressionFlow(node.asInstanceOf[DeleteExpression])
        case SyntaxKind.ConditionalExpression =>
          bindConditionalExpressionFlow(
              node.asInstanceOf[ConditionalExpression])
        case SyntaxKind.VariableDeclaration =>
          bindVariableDeclarationFlow(node.asInstanceOf[VariableDeclaration])
        case SyntaxKind.CallExpression =>
          bindCallExpressionFlow(node.asInstanceOf[CallExpression])
        case _ =>
          forEachChild(node, bind)
      }

    }
    def isNarrowingExpression(expr: Expression): Boolean = {
      expr.kind match {
        case SyntaxKind.Identifier | SyntaxKind.ThisKeyword |
            SyntaxKind.PropertyAccessExpression =>
          return isNarrowableReference(expr)
        case SyntaxKind.CallExpression =>
          return hasNarrowableArgument(expr.asInstanceOf[CallExpression])
        case SyntaxKind.ParenthesizedExpression =>
          return isNarrowingExpression(
              (expr.asInstanceOf[ParenthesizedExpression]).expression)
        case SyntaxKind.BinaryExpression =>
          return isNarrowingBinaryExpression(
              expr.asInstanceOf[BinaryExpression])
        case SyntaxKind.PrefixUnaryExpression =>
          return (((expr.asInstanceOf[PrefixUnaryExpression]).operator === SyntaxKind.ExclamationToken) && isNarrowingExpression(
              (expr.asInstanceOf[PrefixUnaryExpression]).operand))
        case _ =>
      }
      return false

    }
    def isNarrowableReference(expr: Expression): Boolean = {
      return (((expr.kind === SyntaxKind.Identifier) || (expr.kind === SyntaxKind.ThisKeyword)) || ((expr.kind === SyntaxKind.PropertyAccessExpression) && isNarrowableReference(
          (expr.asInstanceOf[PropertyAccessExpression]).expression)))

    }
    def hasNarrowableArgument(expr: CallExpression) = {
      if (expr.arguments) {
        (expr.arguments).foreach { fresh1 =>
          val argument = zeroOfMyType = fresh1 {
            if (isNarrowableReference(argument)) {
              return true

            }

          }
        }

      }
      if (((expr.expression.kind === SyntaxKind.PropertyAccessExpression) && isNarrowableReference((
                  expr.expression
                    .asInstanceOf[PropertyAccessExpression])
                .expression))) {
        return true

      }
      return false

    }
    def isNarrowingTypeofOperands(expr1: Expression, expr2: Expression) = {
      return (((expr1.kind === SyntaxKind.TypeOfExpression) && isNarrowableOperand((
              expr1
                .asInstanceOf[TypeOfExpression])
            .expression)) && (expr2.kind === SyntaxKind.StringLiteral))

    }
    def isNarrowingBinaryExpression(expr: BinaryExpression) = {
      expr.operatorToken.kind match {
        case SyntaxKind.EqualsToken =>
          return isNarrowableReference(expr.left)
        case SyntaxKind.EqualsEqualsToken | SyntaxKind.ExclamationEqualsToken |
            SyntaxKind.EqualsEqualsEqualsToken |
            SyntaxKind.ExclamationEqualsEqualsToken =>
          return (((isNarrowableOperand(expr.left) || isNarrowableOperand(
              expr.right)) || isNarrowingTypeofOperands(expr.right,
              expr.left)) || isNarrowingTypeofOperands(expr.left, expr.right))
        case SyntaxKind.InstanceOfKeyword =>
          return isNarrowableOperand(expr.left)
        case SyntaxKind.CommaToken =>
          return isNarrowingExpression(expr.right)
        case _ =>
      }
      return false

    }
    def isNarrowableOperand(expr: Expression): Boolean = {
      expr.kind match {
        case SyntaxKind.ParenthesizedExpression =>
          return isNarrowableOperand(
              (expr.asInstanceOf[ParenthesizedExpression]).expression)
        case SyntaxKind.BinaryExpression =>
          (expr.asInstanceOf[BinaryExpression]).operatorToken.kind match {
            case SyntaxKind.EqualsToken =>
              return isNarrowableOperand(
                  (expr.asInstanceOf[BinaryExpression]).left)
            case SyntaxKind.CommaToken =>
              return isNarrowableOperand(
                  (expr.asInstanceOf[BinaryExpression]).right)
            case _ =>
          }
        case _ =>
      }
      return isNarrowableReference(expr)

    }
    def createBranchLabel(): FlowLabel = {
      return Map("flags" -> FlowFlags.BranchLabel, "antecedents" -> undefined)

    }
    def createLoopLabel(): FlowLabel = {
      return Map("flags" -> FlowFlags.LoopLabel, "antecedents" -> undefined)

    }
    def setFlowNodeReferenced(flow: FlowNode) = {
      (flow.flags |= (if ((flow.flags & FlowFlags.Referenced)) FlowFlags.Shared
                      else FlowFlags.Referenced))

    }
    def addAntecedent(label: FlowLabel, antecedent: FlowNode): Unit = {
      if (((!((antecedent.flags & FlowFlags.Unreachable))) && (!contains(
              label.antecedents, antecedent)))) {
        ((label.antecedents || ((label.antecedents = Array())))).push(
            antecedent)
        setFlowNodeReferenced(antecedent)

      }

    }
    def createFlowCondition(flags: FlowFlags, antecedent: FlowNode,
        expression: Expression): FlowNode = {
      if ((antecedent.flags & FlowFlags.Unreachable)) {
        return antecedent

      }
      if ((!expression)) {
        return (if ((flags & FlowFlags.TrueCondition)) antecedent
                else unreachableFlow)

      }
      if ((((expression.kind === SyntaxKind.TrueKeyword) && (flags & FlowFlags.FalseCondition)) || ((expression.kind === SyntaxKind.FalseKeyword) && (flags & FlowFlags.TrueCondition)))) {
        return unreachableFlow

      }
      if ((!isNarrowingExpression(expression))) {
        return antecedent

      }
      setFlowNodeReferenced(antecedent)
      return Map("flags" -> flags, "expression" -> expression,
          "antecedent" -> antecedent).asInstanceOf[FlowCondition]

    }
    def createFlowSwitchClause(antecedent: FlowNode,
        switchStatement: SwitchStatement, clauseStart: Int,
        clauseEnd: Int): FlowNode = {
      if ((!isNarrowingExpression(switchStatement.expression))) {
        return antecedent

      }
      setFlowNodeReferenced(antecedent)
      return Map("flags" -> FlowFlags.SwitchClause,
          "switchStatement" -> switchStatement, "clauseStart" -> clauseStart,
          "clauseEnd" -> clauseEnd, "antecedent" -> antecedent).asInstanceOf[
          FlowSwitchClause]

    }
    def createFlowAssignment(antecedent: FlowNode,
        node: (Expression | VariableDeclaration | BindingElement)): FlowNode = {
      setFlowNodeReferenced(antecedent)
      return Map("flags" -> FlowFlags.Assignment, "antecedent" -> antecedent,
          "node" -> node).asInstanceOf[FlowAssignment]

    }
    def createFlowArrayMutation(antecedent: FlowNode,
        node: (CallExpression | BinaryExpression)): FlowNode = {
      setFlowNodeReferenced(antecedent)
      return Map("flags" -> FlowFlags.ArrayMutation,
          "antecedent" -> antecedent, "node" -> node).asInstanceOf[
          FlowArrayMutation]

    }
    def finishFlowLabel(flow: FlowLabel): FlowNode = {
      val antecedents = flow.antecedents
      if ((!antecedents)) {
        return unreachableFlow

      }
      if ((antecedents.length === 1)) {
        return antecedents(0)

      }
      return flow

    }
    def isStatementCondition(node: Node) = {
      val parent = node.parent
      parent.kind match {
        case SyntaxKind.IfStatement | SyntaxKind.WhileStatement |
            SyntaxKind.DoStatement =>
          return ((
              parent
                .asInstanceOf[(IfStatement | WhileStatement | DoStatement)])
            .expression === node)
        case SyntaxKind.ForStatement | SyntaxKind.ConditionalExpression =>
          return ((parent.asInstanceOf[(ForStatement | ConditionalExpression)]).condition === node)
        case _ =>
      }
      return false

    }
    def isLogicalExpression(node: Node) = {
      while (true) {
        {
          if ((node.kind === SyntaxKind.ParenthesizedExpression)) {
            (node = (node.asInstanceOf[ParenthesizedExpression]).expression)

          } else if (((node.kind === SyntaxKind.PrefixUnaryExpression) && ((
                  node
                    .asInstanceOf[PrefixUnaryExpression])
                .operator === SyntaxKind.ExclamationToken))) {
            (node = (node.asInstanceOf[PrefixUnaryExpression]).operand)

          } else {
            return ((node.kind === SyntaxKind.BinaryExpression) && ((((node
                  .asInstanceOf[BinaryExpression])
              .operatorToken
              .kind === SyntaxKind.AmpersandAmpersandToken) || ((node
                  .asInstanceOf[BinaryExpression])
              .operatorToken
              .kind === SyntaxKind.BarBarToken))))

          }

        }
      }

    }
    def isTopLevelLogicalExpression(node: Node): Boolean = {
      while (((node.parent.kind === SyntaxKind.ParenthesizedExpression) || ((node.parent.kind === SyntaxKind.PrefixUnaryExpression) && ((
              node.parent
                .asInstanceOf[PrefixUnaryExpression])
            .operator === SyntaxKind.ExclamationToken)))) {
        {
          (node = node.parent)

        }
      }
      return ((!isStatementCondition(node)) && (!isLogicalExpression(
          node.parent)))

    }
    def bindCondition(node: Expression, trueTarget: FlowLabel,
        falseTarget: FlowLabel) = {
      val saveTrueTarget = currentTrueTarget
      val saveFalseTarget = currentFalseTarget
      (currentTrueTarget = trueTarget)
      (currentFalseTarget = falseTarget)
      bind(node)
      (currentTrueTarget = saveTrueTarget)
      (currentFalseTarget = saveFalseTarget)
      if (((!node) || (!isLogicalExpression(node)))) {
        addAntecedent(trueTarget,
            createFlowCondition(FlowFlags.TrueCondition, currentFlow, node))
        addAntecedent(falseTarget,
            createFlowCondition(FlowFlags.FalseCondition, currentFlow, node))

      }

    }
    def bindIterativeStatement(node: Statement, breakTarget: FlowLabel,
        continueTarget: FlowLabel): Unit = {
      val saveBreakTarget = currentBreakTarget
      val saveContinueTarget = currentContinueTarget
      (currentBreakTarget = breakTarget)
      (currentContinueTarget = continueTarget)
      bind(node)
      (currentBreakTarget = saveBreakTarget)
      (currentContinueTarget = saveContinueTarget)

    }
    def bindWhileStatement(node: WhileStatement): Unit = {
      val preWhileLabel = createLoopLabel()
      val preBodyLabel = createBranchLabel()
      val postWhileLabel = createBranchLabel()
      addAntecedent(preWhileLabel, currentFlow)
      (currentFlow = preWhileLabel)
      bindCondition(node.expression, preBodyLabel, postWhileLabel)
      (currentFlow = finishFlowLabel(preBodyLabel))
      bindIterativeStatement(node.statement, postWhileLabel, preWhileLabel)
      addAntecedent(preWhileLabel, currentFlow)
      (currentFlow = finishFlowLabel(postWhileLabel))

    }
    def bindDoStatement(node: DoStatement): Unit = {
      val preDoLabel = createLoopLabel()
      val preConditionLabel = createBranchLabel()
      val postDoLabel = createBranchLabel()
      addAntecedent(preDoLabel, currentFlow)
      (currentFlow = preDoLabel)
      bindIterativeStatement(node.statement, postDoLabel, preConditionLabel)
      addAntecedent(preConditionLabel, currentFlow)
      (currentFlow = finishFlowLabel(preConditionLabel))
      bindCondition(node.expression, preDoLabel, postDoLabel)
      (currentFlow = finishFlowLabel(postDoLabel))

    }
    def bindForStatement(node: ForStatement): Unit = {
      val preLoopLabel = createLoopLabel()
      val preBodyLabel = createBranchLabel()
      val postLoopLabel = createBranchLabel()
      bind(node.initializer)
      addAntecedent(preLoopLabel, currentFlow)
      (currentFlow = preLoopLabel)
      bindCondition(node.condition, preBodyLabel, postLoopLabel)
      (currentFlow = finishFlowLabel(preBodyLabel))
      bindIterativeStatement(node.statement, postLoopLabel, preLoopLabel)
      bind(node.incrementor)
      addAntecedent(preLoopLabel, currentFlow)
      (currentFlow = finishFlowLabel(postLoopLabel))

    }
    def bindForInOrForOfStatement(
        node: (ForInStatement | ForOfStatement)): Unit = {
      val preLoopLabel = createLoopLabel()
      val postLoopLabel = createBranchLabel()
      addAntecedent(preLoopLabel, currentFlow)
      (currentFlow = preLoopLabel)
      bind(node.expression)
      addAntecedent(postLoopLabel, currentFlow)
      bind(node.initializer)
      if ((node.initializer.kind !== SyntaxKind.VariableDeclarationList)) {
        bindAssignmentTargetFlow(node.initializer.asInstanceOf[Expression])

      }
      bindIterativeStatement(node.statement, postLoopLabel, preLoopLabel)
      addAntecedent(preLoopLabel, currentFlow)
      (currentFlow = finishFlowLabel(postLoopLabel))

    }
    def bindIfStatement(node: IfStatement): Unit = {
      val thenLabel = createBranchLabel()
      val elseLabel = createBranchLabel()
      val postIfLabel = createBranchLabel()
      bindCondition(node.expression, thenLabel, elseLabel)
      (currentFlow = finishFlowLabel(thenLabel))
      bind(node.thenStatement)
      addAntecedent(postIfLabel, currentFlow)
      (currentFlow = finishFlowLabel(elseLabel))
      bind(node.elseStatement)
      addAntecedent(postIfLabel, currentFlow)
      (currentFlow = finishFlowLabel(postIfLabel))

    }
    def bindReturnOrThrow(node: (ReturnStatement | ThrowStatement)): Unit = {
      bind(node.expression)
      if ((node.kind === SyntaxKind.ReturnStatement)) {
        (hasExplicitReturn = true)
        if (currentReturnTarget) {
          addAntecedent(currentReturnTarget, currentFlow)

        }

      }
      (currentFlow = unreachableFlow)

    }
    def findActiveLabel(name: String) = {
      if (activeLabels) {
        (activeLabels).foreach { fresh2 =>
          val label = zeroOfMyType = fresh2 {
            if ((label.name === name)) {
              return label

            }

          }
        }

      }
      return undefined

    }
    def bindbreakOrContinueFlow(node: BreakOrContinueStatement,
        breakTarget: FlowLabel, continueTarget: FlowLabel) = {
      val flowLabel =
        (if ((node.kind === SyntaxKind.BreakStatement)) breakTarget
         else continueTarget)
      if (flowLabel) {
        addAntecedent(flowLabel, currentFlow)
        (currentFlow = unreachableFlow)

      }

    }
    def bindBreakOrContinueStatement(node: BreakOrContinueStatement): Unit = {
      bind(node.label)
      if (node.label) {
        val activeLabel = findActiveLabel(node.label.text)
        if (activeLabel) {
          (activeLabel.referenced = true)
          bindbreakOrContinueFlow(node, activeLabel.breakTarget,
              activeLabel.continueTarget)

        }

      } else {
        bindbreakOrContinueFlow(node, currentBreakTarget,
            currentContinueTarget)

      }

    }
    def bindTryStatement(node: TryStatement): Unit = {
      val preFinallyLabel = createBranchLabel()
      val preTryFlow = currentFlow
      bind(node.tryBlock)
      addAntecedent(preFinallyLabel, currentFlow)
      val flowAfterTry = currentFlow
      var flowAfterCatch = unreachableFlow
      if (node.catchClause) {
        (currentFlow = preTryFlow)
        bind(node.catchClause)
        addAntecedent(preFinallyLabel, currentFlow)
        (flowAfterCatch = currentFlow)

      }
      if (node.finallyBlock) {
        addAntecedent(preFinallyLabel, preTryFlow)
        (currentFlow = finishFlowLabel(preFinallyLabel))
        bind(node.finallyBlock)
        if ((!((currentFlow.flags & FlowFlags.Unreachable)))) {
          if ((((flowAfterTry.flags & FlowFlags.Unreachable)) && ((flowAfterCatch.flags & FlowFlags.Unreachable)))) {
            (currentFlow =
              (if (((flowAfterTry === reportedUnreachableFlow) || (flowAfterCatch === reportedUnreachableFlow)))
                 reportedUnreachableFlow
               else unreachableFlow))

          }

        }

      } else {
        (currentFlow = finishFlowLabel(preFinallyLabel))

      }

    }
    def bindSwitchStatement(node: SwitchStatement): Unit = {
      val postSwitchLabel = createBranchLabel()
      bind(node.expression)
      val saveBreakTarget = currentBreakTarget
      val savePreSwitchCaseFlow = preSwitchCaseFlow
      (currentBreakTarget = postSwitchLabel)
      (preSwitchCaseFlow = currentFlow)
      bind(node.caseBlock)
      addAntecedent(postSwitchLabel, currentFlow)
      val hasDefault = forEach(node.caseBlock.clauses,
          (c => (c.kind === SyntaxKind.DefaultClause)))
      (node.possiblyExhaustive = ((!hasDefault) && (!postSwitchLabel.antecedents)))
      if ((!hasDefault)) {
        addAntecedent(postSwitchLabel,
            createFlowSwitchClause(preSwitchCaseFlow, node, 0, 0))

      }
      (currentBreakTarget = saveBreakTarget)
      (preSwitchCaseFlow = savePreSwitchCaseFlow)
      (currentFlow = finishFlowLabel(postSwitchLabel))

    }
    def bindCaseBlock(node: CaseBlock): Unit = {
      val clauses = node.clauses
      var fallthroughFlow = unreachableFlow {
        var i = 0
        while ((i < clauses.length)) {
          {
            val clauseStart = i
            while (((!clauses(i).statements.length) && ((i + 1) < clauses.length))) {
              {
                bind(clauses(i))
                (i += 1)

              }
            }
            val preCaseLabel = createBranchLabel()
            addAntecedent(preCaseLabel,
                createFlowSwitchClause(preSwitchCaseFlow,
                    node.parent.asInstanceOf[SwitchStatement], clauseStart,
                    (i + 1)))
            addAntecedent(preCaseLabel, fallthroughFlow)
            (currentFlow = finishFlowLabel(preCaseLabel))
            val clause = clauses(i)
            bind(clause)
            (fallthroughFlow = currentFlow)
            if ((((!((currentFlow.flags & FlowFlags.Unreachable))) && (i !== (clauses.length - 1))) && options.noFallthroughCasesInSwitch)) {
              errorOnFirstToken(clause, Diagnostics.Fallthrough_case_in_switch)

            }

          }
          (i += 1)
        }
      }

    }
    def bindCaseClause(node: CaseClause): Unit = {
      val saveCurrentFlow = currentFlow
      (currentFlow = preSwitchCaseFlow)
      bind(node.expression)
      (currentFlow = saveCurrentFlow)
      forEach(node.statements, bind)

    }
    def pushActiveLabel(name: String, breakTarget: FlowLabel,
        continueTarget: FlowLabel): ActiveLabel = {
      val activeLabel = Map("name" -> name, "breakTarget" -> breakTarget,
          "continueTarget" -> continueTarget, "referenced" -> false)
      ((activeLabels || ((activeLabels = Array())))).push(activeLabel)
      return activeLabel

    }
    def popActiveLabel() = {
      activeLabels.pop()

    }
    def bindLabeledStatement(node: LabeledStatement): Unit = {
      val preStatementLabel = createLoopLabel()
      val postStatementLabel = createBranchLabel()
      bind(node.label)
      addAntecedent(preStatementLabel, currentFlow)
      val activeLabel =
        pushActiveLabel(node.label.text, postStatementLabel, preStatementLabel)
      bind(node.statement)
      popActiveLabel()
      if (((!activeLabel.referenced) && (!options.allowUnusedLabels))) {
        file.bindDiagnostics
          .push(createDiagnosticForNode(node.label, Diagnostics.Unused_label))

      }
      addAntecedent(postStatementLabel, currentFlow)
      (currentFlow = finishFlowLabel(postStatementLabel))

    }
    def bindDestructuringTargetFlow(node: Expression) = {
      if (((node.kind === SyntaxKind.BinaryExpression) && ((node
                .asInstanceOf[BinaryExpression])
            .operatorToken
            .kind === SyntaxKind.EqualsToken))) {
        bindAssignmentTargetFlow((node.asInstanceOf[BinaryExpression]).left)

      } else {
        bindAssignmentTargetFlow(node)

      }

    }
    def bindAssignmentTargetFlow(node: Expression) = {
      if (isNarrowableReference(node)) {
        (currentFlow = createFlowAssignment(currentFlow, node))

      } else if ((node.kind === SyntaxKind.ArrayLiteralExpression)) {
        ((node.asInstanceOf[ArrayLiteralExpression]).elements).foreach {
          fresh3 =>
            val e = zeroOfMyType = fresh3 {
              if ((e.kind === SyntaxKind.SpreadElementExpression)) {
                bindAssignmentTargetFlow(
                    (e.asInstanceOf[SpreadElementExpression]).expression)

              } else {
                bindDestructuringTargetFlow(e)

              }

            }
        }

      } else if ((node.kind === SyntaxKind.ObjectLiteralExpression)) {
        ((node.asInstanceOf[ObjectLiteralExpression]).properties).foreach {
          fresh4 =>
            val p = zeroOfMyType = fresh4 {
              if ((p.kind === SyntaxKind.PropertyAssignment)) {
                bindDestructuringTargetFlow(
                    (p.asInstanceOf[PropertyAssignment]).initializer)

              } else if ((p.kind === SyntaxKind.ShorthandPropertyAssignment)) {
                bindAssignmentTargetFlow(
                    (p.asInstanceOf[ShorthandPropertyAssignment]).name)

              }

            }
        }

      }

    }
    def bindLogicalExpression(node: BinaryExpression, trueTarget: FlowLabel,
        falseTarget: FlowLabel) = {
      val preRightLabel = createBranchLabel()
      if ((node.operatorToken.kind === SyntaxKind.AmpersandAmpersandToken)) {
        bindCondition(node.left, preRightLabel, falseTarget)

      } else {
        bindCondition(node.left, trueTarget, preRightLabel)

      }
      (currentFlow = finishFlowLabel(preRightLabel))
      bind(node.operatorToken)
      bindCondition(node.right, trueTarget, falseTarget)

    }
    def bindPrefixUnaryExpressionFlow(node: PrefixUnaryExpression) = {
      if ((node.operator === SyntaxKind.ExclamationToken)) {
        val saveTrueTarget = currentTrueTarget
        (currentTrueTarget = currentFalseTarget)
        (currentFalseTarget = saveTrueTarget)
        forEachChild(node, bind)
        (currentFalseTarget = currentTrueTarget)
        (currentTrueTarget = saveTrueTarget)

      } else {
        forEachChild(node, bind)
        if (((node.operator === SyntaxKind.PlusPlusToken) || (node.operator === SyntaxKind.MinusMinusToken))) {
          bindAssignmentTargetFlow(node.operand)

        }

      }

    }
    def bindPostfixUnaryExpressionFlow(node: PostfixUnaryExpression) = {
      forEachChild(node, bind)
      if (((node.operator === SyntaxKind.PlusPlusToken) || (node.operator === SyntaxKind.MinusMinusToken))) {
        bindAssignmentTargetFlow(node.operand)

      }

    }
    def bindBinaryExpressionFlow(node: BinaryExpression) = {
      val operator = node.operatorToken.kind
      if (((operator === SyntaxKind.AmpersandAmpersandToken) || (operator === SyntaxKind.BarBarToken))) {
        if (isTopLevelLogicalExpression(node)) {
          val postExpressionLabel = createBranchLabel()
          bindLogicalExpression(node, postExpressionLabel, postExpressionLabel)
          (currentFlow = finishFlowLabel(postExpressionLabel))

        } else {
          bindLogicalExpression(node, currentTrueTarget, currentFalseTarget)

        }

      } else {
        forEachChild(node, bind)
        if (((operator === SyntaxKind.EqualsToken) && (!isAssignmentTarget(
                node)))) {
          bindAssignmentTargetFlow(node.left)
          if ((node.left.kind === SyntaxKind.ElementAccessExpression)) {
            val elementAccess = node.left.asInstanceOf[ElementAccessExpression]
            if (isNarrowableOperand(elementAccess.expression)) {
              (currentFlow = createFlowArrayMutation(currentFlow, node))

            }

          }

        }

      }

    }
    def bindDeleteExpressionFlow(node: DeleteExpression) = {
      forEachChild(node, bind)
      if ((node.expression.kind === SyntaxKind.PropertyAccessExpression)) {
        bindAssignmentTargetFlow(node.expression)

      }

    }
    def bindConditionalExpressionFlow(node: ConditionalExpression) = {
      val trueLabel = createBranchLabel()
      val falseLabel = createBranchLabel()
      val postExpressionLabel = createBranchLabel()
      bindCondition(node.condition, trueLabel, falseLabel)
      (currentFlow = finishFlowLabel(trueLabel))
      bind(node.whenTrue)
      addAntecedent(postExpressionLabel, currentFlow)
      (currentFlow = finishFlowLabel(falseLabel))
      bind(node.whenFalse)
      addAntecedent(postExpressionLabel, currentFlow)
      (currentFlow = finishFlowLabel(postExpressionLabel))

    }
    def bindInitializedVariableFlow(
        node: (VariableDeclaration | ArrayBindingElement)) = {
      val name = (if ((!isOmittedExpression(node))) node.name else undefined)
      if (isBindingPattern(name)) {
        (name.elements).foreach { fresh5 =>
          val child = zeroOfMyType = fresh5 {
            bindInitializedVariableFlow(child)

          }
        }

      } else {
        (currentFlow = createFlowAssignment(currentFlow, node))

      }

    }
    def bindVariableDeclarationFlow(node: VariableDeclaration) = {
      forEachChild(node, bind)
      if (((node.initializer || (node.parent.parent.kind === SyntaxKind.ForInStatement)) || (node.parent.parent.kind === SyntaxKind.ForOfStatement))) {
        bindInitializedVariableFlow(node)

      }

    }
    def bindCallExpressionFlow(node: CallExpression) = {
      var expr: Expression = node.expression
      while ((expr.kind === SyntaxKind.ParenthesizedExpression)) {
        {
          (expr = (expr.asInstanceOf[ParenthesizedExpression]).expression)

        }
      }
      if (((expr.kind === SyntaxKind.FunctionExpression) || (expr.kind === SyntaxKind.ArrowFunction))) {
        forEach(node.typeArguments, bind)
        forEach(node.arguments, bind)
        bind(node.expression)

      } else {
        forEachChild(node, bind)

      }
      if ((node.expression.kind === SyntaxKind.PropertyAccessExpression)) {
        val propertyAccess =
          node.expression.asInstanceOf[PropertyAccessExpression]
        if ((isNarrowableOperand(propertyAccess.expression) && isPushOrUnshiftIdentifier(
                propertyAccess.name))) {
          (currentFlow = createFlowArrayMutation(currentFlow, node))

        }

      }

    }
    def getContainerFlags(node: Node): ContainerFlags = {
      node.kind match {
        case SyntaxKind.ClassExpression | SyntaxKind.ClassDeclaration |
            SyntaxKind.EnumDeclaration | SyntaxKind.ObjectLiteralExpression |
            SyntaxKind.TypeLiteral | SyntaxKind.JSDocTypeLiteral |
            SyntaxKind.JSDocRecordType =>
          return ContainerFlags.IsContainer
        case SyntaxKind.InterfaceDeclaration =>
          return (ContainerFlags.IsContainer | ContainerFlags.IsInterface)
        case SyntaxKind.JSDocFunctionType | SyntaxKind.ModuleDeclaration |
            SyntaxKind.TypeAliasDeclaration =>
          return (ContainerFlags.IsContainer | ContainerFlags.HasLocals)
        case SyntaxKind.SourceFile =>
          return ((ContainerFlags.IsContainer | ContainerFlags.IsControlFlowContainer) | ContainerFlags.HasLocals)
        case SyntaxKind.MethodDeclaration =>
          if (isObjectLiteralOrClassExpressionMethod(node)) {
            return ((((ContainerFlags.IsContainer | ContainerFlags.IsControlFlowContainer) | ContainerFlags.HasLocals) | ContainerFlags.IsFunctionLike) | ContainerFlags.IsObjectLiteralOrClassExpressionMethod)

          }
        case SyntaxKind.Constructor | SyntaxKind.FunctionDeclaration |
            SyntaxKind.MethodSignature | SyntaxKind.GetAccessor |
            SyntaxKind.SetAccessor | SyntaxKind.CallSignature |
            SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature |
            SyntaxKind.FunctionType | SyntaxKind.ConstructorType =>
          return (((ContainerFlags.IsContainer | ContainerFlags.IsControlFlowContainer) | ContainerFlags.HasLocals) | ContainerFlags.IsFunctionLike)
        case SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction =>
          return ((((ContainerFlags.IsContainer | ContainerFlags.IsControlFlowContainer) | ContainerFlags.HasLocals) | ContainerFlags.IsFunctionLike) | ContainerFlags.IsFunctionExpression)
        case SyntaxKind.ModuleBlock =>
          return ContainerFlags.IsControlFlowContainer
        case SyntaxKind.PropertyDeclaration =>
          return (if ((node.asInstanceOf[PropertyDeclaration]).initializer)
                    ContainerFlags.IsControlFlowContainer
                  else 0)
        case SyntaxKind.CatchClause | SyntaxKind.ForStatement |
            SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement |
            SyntaxKind.CaseBlock =>
          return ContainerFlags.IsBlockScopedContainer
        case SyntaxKind.Block =>
          return (if (isFunctionLike(node.parent)) ContainerFlags.None
                  else ContainerFlags.IsBlockScopedContainer)
        case _ =>
      }
      return ContainerFlags.None

    }
    def addToContainerChain(next: Node) = {
      if (lastContainer) {
        (lastContainer.nextContainer = next)

      }
      (lastContainer = next)

    }
    def declareSymbolAndAddToSymbolTable(node: Declaration,
        symbolFlags: SymbolFlags, symbolExcludes: SymbolFlags): Symbol = {
      return declareSymbolAndAddToSymbolTableWorker(node, symbolFlags,
          symbolExcludes)

    }
    def declareSymbolAndAddToSymbolTableWorker(node: Declaration,
        symbolFlags: SymbolFlags, symbolExcludes: SymbolFlags): Symbol = {
      container.kind match {
        case SyntaxKind.ModuleDeclaration =>
          return declareModuleMember(node, symbolFlags, symbolExcludes)
        case SyntaxKind.SourceFile =>
          return declareSourceFileMember(node, symbolFlags, symbolExcludes)
        case SyntaxKind.ClassExpression | SyntaxKind.ClassDeclaration =>
          return declareClassMember(node, symbolFlags, symbolExcludes)
        case SyntaxKind.EnumDeclaration =>
          return declareSymbol(container.symbol.exports, container.symbol,
              node, symbolFlags, symbolExcludes)
        case SyntaxKind.TypeLiteral | SyntaxKind.ObjectLiteralExpression |
            SyntaxKind.InterfaceDeclaration | SyntaxKind.JSDocRecordType |
            SyntaxKind.JSDocTypeLiteral =>
          return declareSymbol(container.symbol.members, container.symbol,
              node, symbolFlags, symbolExcludes)
        case SyntaxKind.FunctionType | SyntaxKind.ConstructorType |
            SyntaxKind.CallSignature | SyntaxKind.ConstructSignature |
            SyntaxKind.IndexSignature | SyntaxKind.MethodDeclaration |
            SyntaxKind.MethodSignature | SyntaxKind.Constructor |
            SyntaxKind.GetAccessor | SyntaxKind.SetAccessor |
            SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression |
            SyntaxKind.ArrowFunction | SyntaxKind.JSDocFunctionType |
            SyntaxKind.TypeAliasDeclaration =>
          return declareSymbol(container.locals, undefined, node, symbolFlags,
              symbolExcludes)
        case _ =>
      }

    }
    def declareClassMember(node: Declaration, symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags) = {
      return (if (hasModifier(node, ModifierFlags.Static))
                declareSymbol(container.symbol.exports, container.symbol, node,
                    symbolFlags, symbolExcludes)
              else
                declareSymbol(container.symbol.members, container.symbol, node,
                    symbolFlags, symbolExcludes))

    }
    def declareSourceFileMember(node: Declaration, symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags) = {
      return (if (isExternalModule(file))
                declareModuleMember(node, symbolFlags, symbolExcludes)
              else
                declareSymbol(file.locals, undefined, node, symbolFlags,
                    symbolExcludes))

    }
    def hasExportDeclarations(
        node: (ModuleDeclaration | SourceFile)): Boolean = {
      val body =
        (if ((node.kind === SyntaxKind.SourceFile)) node
         else (node.asInstanceOf[ModuleDeclaration]).body)
      if ((body && (((body.kind === SyntaxKind.SourceFile) || (body.kind === SyntaxKind.ModuleBlock))))) {
        ((body.asInstanceOf[BlockLike]).statements).foreach { fresh6 =>
          val stat = zeroOfMyType = fresh6 {
            if (((stat.kind === SyntaxKind.ExportDeclaration) || (stat.kind === SyntaxKind.ExportAssignment))) {
              return true

            }

          }
        }

      }
      return false

    }
    def setExportContextFlag(node: (ModuleDeclaration | SourceFile)) = {
      if ((isInAmbientContext(node) && (!hasExportDeclarations(node)))) {
        (node.flags |= NodeFlags.ExportContext)

      } else {
        (node.flags &= (~NodeFlags.ExportContext))

      }

    }
    def bindModuleDeclaration(node: ModuleDeclaration) = {
      setExportContextFlag(node)
      if (isAmbientModule(node)) {
        if (hasModifier(node, ModifierFlags.Export)) {
          errorOnFirstToken(node,
              Diagnostics.export_modifier_cannot_be_applied_to_ambient_modules_and_module_augmentations_since_they_are_always_visible)

        }
        if (isExternalModuleAugmentation(node)) {
          declareSymbolAndAddToSymbolTable(node, SymbolFlags.NamespaceModule,
              SymbolFlags.NamespaceModuleExcludes)

        } else {
          var pattern: (Pattern | undefined) = zeroOfMyType
          if ((node.name.kind === SyntaxKind.StringLiteral)) {
            val text = (node.name.asInstanceOf[StringLiteral]).text
            if (hasZeroOrOneAsteriskCharacter(text)) {
              (pattern = tryParsePattern(text))

            } else {
              errorOnFirstToken(node.name,
                  Diagnostics.Pattern_0_can_have_at_most_one_Asterisk_character,
                  text)

            }

          }
          val symbol = declareSymbolAndAddToSymbolTable(node,
              SymbolFlags.ValueModule, SymbolFlags.ValueModuleExcludes)
          if (pattern) {
            ((file.patternAmbientModules || ((file.patternAmbientModules =
              Array())))).push(Map("pattern" -> pattern, "symbol" -> symbol))

          }

        }

      } else {
        val state = getModuleInstanceState(node)
        if ((state === ModuleInstanceState.NonInstantiated)) {
          declareSymbolAndAddToSymbolTable(node, SymbolFlags.NamespaceModule,
              SymbolFlags.NamespaceModuleExcludes)

        } else {
          declareSymbolAndAddToSymbolTable(node, SymbolFlags.ValueModule,
              SymbolFlags.ValueModuleExcludes)
          if ((node.symbol.flags & (((SymbolFlags.Function | SymbolFlags.Class) | SymbolFlags.RegularEnum)))) {
            (node.symbol.constEnumOnlyModule = false)

          } else {
            val currentModuleIsConstEnumOnly = (state === ModuleInstanceState.ConstEnumOnly)
            if ((node.symbol.constEnumOnlyModule === undefined)) {
              (node.symbol.constEnumOnlyModule = currentModuleIsConstEnumOnly)

            } else {
              (node.symbol.constEnumOnlyModule = (node.symbol.constEnumOnlyModule && currentModuleIsConstEnumOnly))

            }

          }

        }

      }

    }
    def bindFunctionOrConstructorType(node: SignatureDeclaration): Unit = {
      val symbol =
        createSymbol(SymbolFlags.Signature, getDeclarationName(node))
      addDeclarationToSymbol(symbol, node, SymbolFlags.Signature)
      val typeLiteralSymbol = createSymbol(SymbolFlags.TypeLiteral, "__type")
      addDeclarationToSymbol(typeLiteralSymbol, node, SymbolFlags.TypeLiteral)
      (typeLiteralSymbol.members = createMap[Symbol]())
      (typeLiteralSymbol.members(symbol.name) = symbol)

    }
    def bindObjectLiteralExpression(node: ObjectLiteralExpression) = {
      sealed abstract class ElementKind
      object ElementKind {
        case object Property extends ElementKind
        case object Accessor extends ElementKind
      }
      if (inStrictMode) {
        val seen = createMap[ElementKind]()
        (node.properties).foreach { fresh7 =>
          val prop = zeroOfMyType = fresh7 {
            if ((prop.name.kind !== SyntaxKind.Identifier)) {
              continue

            }
            val identifier = prop.name.asInstanceOf[Identifier]
            val currentKind =
              (if ((((prop.kind === SyntaxKind.PropertyAssignment) || (prop.kind === SyntaxKind.ShorthandPropertyAssignment)) || (prop.kind === SyntaxKind.MethodDeclaration)))
                 ElementKind.Property
               else ElementKind.Accessor)
            val existingKind = seen(identifier.text)
            if ((!existingKind)) {
              (seen(identifier.text) = currentKind)
              continue

            }
            if (((currentKind === ElementKind.Property) && (existingKind === ElementKind.Property))) {
              val span = getErrorSpanForNode(file, identifier)
              file.bindDiagnostics.push(createFileDiagnostic(file, span.start,
                      span.length,
                      Diagnostics.An_object_literal_cannot_have_multiple_properties_with_the_same_name_in_strict_mode))

            }

          }
        }

      }
      return bindAnonymousDeclaration(node, SymbolFlags.ObjectLiteral,
          "__object")

    }
    def bindAnonymousDeclaration(node: Declaration, symbolFlags: SymbolFlags,
        name: String) = {
      val symbol = createSymbol(symbolFlags, name)
      addDeclarationToSymbol(symbol, node, symbolFlags)

    }
    def bindBlockScopedDeclaration(node: Declaration, symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags) = {
      blockScopeContainer.kind match {
        case SyntaxKind.ModuleDeclaration =>
          declareModuleMember(node, symbolFlags, symbolExcludes)
        case SyntaxKind.SourceFile =>
          if (isExternalModule(container.asInstanceOf[SourceFile])) {
            declareModuleMember(node, symbolFlags, symbolExcludes)
            break()

          }
        case _ =>
          if ((!blockScopeContainer.locals)) {
            (blockScopeContainer.locals = createMap[Symbol]())
            addToContainerChain(blockScopeContainer)

          }
          declareSymbol(blockScopeContainer.locals, undefined, node,
              symbolFlags, symbolExcludes)
      }

    }
    def bindBlockScopedVariableDeclaration(node: Declaration) = {
      bindBlockScopedDeclaration(node, SymbolFlags.BlockScopedVariable,
          SymbolFlags.BlockScopedVariableExcludes)

    }
    def checkStrictModeIdentifier(node: Identifier) = {
      if (((((inStrictMode && (node.originalKeywordKind >= SyntaxKind.FirstFutureReservedWord)) && (node.originalKeywordKind <= SyntaxKind.LastFutureReservedWord)) && (!isIdentifierName(
              node))) && (!isInAmbientContext(node)))) {
        if ((!file.parseDiagnostics.length)) {
          file.bindDiagnostics.push(createDiagnosticForNode(node,
                  getStrictModeIdentifierMessage(node),
                  declarationNameToString(node)))

        }

      }

    }
    def getStrictModeIdentifierMessage(node: Node) = {
      if (getContainingClass(node)) {
        return Diagnostics.Identifier_expected_0_is_a_reserved_word_in_strict_mode_Class_definitions_are_automatically_in_strict_mode

      }
      if (file.externalModuleIndicator) {
        return Diagnostics.Identifier_expected_0_is_a_reserved_word_in_strict_mode_Modules_are_automatically_in_strict_mode

      }
      return Diagnostics.Identifier_expected_0_is_a_reserved_word_in_strict_mode

    }
    def checkStrictModeBinaryExpression(node: BinaryExpression) = {
      if (((inStrictMode && isLeftHandSideExpression(node.left)) && isAssignmentOperator(
              node.operatorToken.kind))) {
        checkStrictModeEvalOrArguments(node,
            node.left.asInstanceOf[Identifier])

      }

    }
    def checkStrictModeCatchClause(node: CatchClause) = {
      if ((inStrictMode && node.variableDeclaration)) {
        checkStrictModeEvalOrArguments(node, node.variableDeclaration.name)

      }

    }
    def checkStrictModeDeleteExpression(node: DeleteExpression) = {
      if ((inStrictMode && (node.expression.kind === SyntaxKind.Identifier))) {
        val span = getErrorSpanForNode(file, node.expression)
        file.bindDiagnostics.push(createFileDiagnostic(file, span.start,
                span.length,
                Diagnostics.delete_cannot_be_called_on_an_identifier_in_strict_mode))

      }

    }
    def isEvalOrArgumentsIdentifier(node: Node): Boolean = {
      return ((node.kind === SyntaxKind.Identifier) && ((((
          node.asInstanceOf[Identifier]).text === "eval") || ((
          node.asInstanceOf[Identifier]).text === "arguments"))))

    }
    def checkStrictModeEvalOrArguments(contextNode: Node, name: Node) = {
      if ((name && (name.kind === SyntaxKind.Identifier))) {
        val identifier = name.asInstanceOf[Identifier]
        if (isEvalOrArgumentsIdentifier(identifier)) {
          val span = getErrorSpanForNode(file, name)
          file.bindDiagnostics
            .push(createFileDiagnostic(file, span.start, span.length,
                    getStrictModeEvalOrArgumentsMessage(contextNode),
                    identifier.text))

        }

      }

    }
    def getStrictModeEvalOrArgumentsMessage(node: Node) = {
      if (getContainingClass(node)) {
        return Diagnostics.Invalid_use_of_0_Class_definitions_are_automatically_in_strict_mode

      }
      if (file.externalModuleIndicator) {
        return Diagnostics.Invalid_use_of_0_Modules_are_automatically_in_strict_mode

      }
      return Diagnostics.Invalid_use_of_0_in_strict_mode

    }
    def checkStrictModeFunctionName(node: FunctionLikeDeclaration) = {
      if (inStrictMode) {
        checkStrictModeEvalOrArguments(node, node.name)

      }

    }
    def getStrictModeBlockScopeFunctionDeclarationMessage(node: Node) = {
      if (getContainingClass(node)) {
        return Diagnostics.Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Class_definitions_are_automatically_in_strict_mode

      }
      if (file.externalModuleIndicator) {
        return Diagnostics.Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Modules_are_automatically_in_strict_mode

      }
      return Diagnostics.Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5

    }
    def checkStrictModeFunctionDeclaration(node: FunctionDeclaration) = {
      if ((languageVersion < ScriptTarget.ES2015)) {
        if ((((blockScopeContainer.kind !== SyntaxKind.SourceFile) && (blockScopeContainer.kind !== SyntaxKind.ModuleDeclaration)) && (!isFunctionLike(
                blockScopeContainer)))) {
          val errorSpan = getErrorSpanForNode(file, node)
          file.bindDiagnostics
            .push(createFileDiagnostic(file, errorSpan.start, errorSpan.length,
                    getStrictModeBlockScopeFunctionDeclarationMessage(node)))

        }

      }

    }
    def checkStrictModeNumericLiteral(node: NumericLiteral) = {
      if ((inStrictMode && node.isOctalLiteral)) {
        file.bindDiagnostics.push(createDiagnosticForNode(node,
                Diagnostics.Octal_literals_are_not_allowed_in_strict_mode))

      }

    }
    def checkStrictModePostfixUnaryExpression(node: PostfixUnaryExpression) = {
      if (inStrictMode) {
        checkStrictModeEvalOrArguments(node,
            node.operand.asInstanceOf[Identifier])

      }

    }
    def checkStrictModePrefixUnaryExpression(node: PrefixUnaryExpression) = {
      if (inStrictMode) {
        if (((node.operator === SyntaxKind.PlusPlusToken) || (node.operator === SyntaxKind.MinusMinusToken))) {
          checkStrictModeEvalOrArguments(node,
              node.operand.asInstanceOf[Identifier])

        }

      }

    }
    def checkStrictModeWithStatement(node: WithStatement) = {
      if (inStrictMode) {
        errorOnFirstToken(node,
            Diagnostics.with_statements_are_not_allowed_in_strict_mode)

      }

    }
    def errorOnFirstToken(node: Node, message: DiagnosticMessage, arg0: Any,
        arg1: Any, arg2: Any) = {
      val span = getSpanOfTokenAtPosition(file, node.pos)
      file.bindDiagnostics.push(createFileDiagnostic(file, span.start,
              span.length, message, arg0, arg1, arg2))

    }
    def getDestructuringParameterName(node: Declaration) = {
      return ("__" + indexOf(
          (node.parent.asInstanceOf[SignatureDeclaration]).parameters, node))

    }
    def bind(node: Node): Unit = {
      if ((!node)) {
        return

      }
      (node.parent = parent)
      val saveInStrictMode = inStrictMode
      bindWorker(node)
      if ((node.kind > SyntaxKind.LastToken)) {
        val saveParent = parent
        (parent = node)
        val containerFlags = getContainerFlags(node)
        if ((containerFlags === ContainerFlags.None)) {
          bindChildren(node)

        } else {
          bindContainer(node, containerFlags)

        }
        (parent = saveParent)

      } else if (((!skipTransformFlagAggregation) && (((node.transformFlags & TransformFlags.HasComputedFlags)) === 0))) {
        (subtreeTransformFlags |= computeTransformFlagsForNode(node, 0))

      }
      (inStrictMode = saveInStrictMode)

    }
    def updateStrictModeStatementList(statements: NodeArray[Statement]) = {
      if ((!inStrictMode)) {
        (statements).foreach { fresh8 =>
          val statement = zeroOfMyType = fresh8 {
            if ((!isPrologueDirective(statement))) {
              return

            }
            if (isUseStrictPrologueDirective(
                    statement.asInstanceOf[ExpressionStatement])) {
              (inStrictMode = true)
              return

            }

          }
        }

      }

    }
    def isUseStrictPrologueDirective(node: ExpressionStatement): Boolean = {
      val nodeText = getTextOfNodeFromSourceText(file.text, node.expression)
      return ((nodeText === "\"use strict\"") || (nodeText === "'use strict'"))

    }
    def bindWorker(node: Node) = {
      node.kind match {
        case SyntaxKind.Identifier =>
          if ((node.asInstanceOf[Identifier]).isInJSDocNamespace) {
            var parentNode = node.parent
            while ((parentNode && (parentNode.kind !== SyntaxKind.JSDocTypedefTag))) {
              {
                (parentNode = parentNode.parent)

              }
            }
            bindBlockScopedDeclaration(parentNode.asInstanceOf[Declaration],
                SymbolFlags.TypeAlias, SymbolFlags.TypeAliasExcludes)
            break()

          }
        case SyntaxKind.ThisKeyword =>
          if ((currentFlow && ((isExpression(node) || (parent.kind === SyntaxKind.ShorthandPropertyAssignment))))) {
            (node.flowNode = currentFlow)

          }
          return checkStrictModeIdentifier(node.asInstanceOf[Identifier])
        case SyntaxKind.PropertyAccessExpression =>
          if ((currentFlow && isNarrowableReference(
                  node.asInstanceOf[Expression]))) {
            (node.flowNode = currentFlow)

          }
        case SyntaxKind.BinaryExpression =>
          if (isInJavaScriptFile(node)) {
            val specialKind = getSpecialPropertyAssignmentKind(node)
            specialKind match {
              case SpecialPropertyAssignmentKind.ExportsProperty =>
                bindExportsPropertyAssignment(
                    node.asInstanceOf[BinaryExpression])
              case SpecialPropertyAssignmentKind.ModuleExports =>
                bindModuleExportsAssignment(
                    node.asInstanceOf[BinaryExpression])
              case SpecialPropertyAssignmentKind.PrototypeProperty =>
                bindPrototypePropertyAssignment(
                    node.asInstanceOf[BinaryExpression])
              case SpecialPropertyAssignmentKind.ThisProperty =>
                bindThisPropertyAssignment(node.asInstanceOf[BinaryExpression])
              case SpecialPropertyAssignmentKind.None =>
              case _ =>
                Debug.fail("Unknown special property assignment kind")
            }

          }
          return checkStrictModeBinaryExpression(
              node.asInstanceOf[BinaryExpression])
        case SyntaxKind.CatchClause =>
          return checkStrictModeCatchClause(node.asInstanceOf[CatchClause])
        case SyntaxKind.DeleteExpression =>
          return checkStrictModeDeleteExpression(
              node.asInstanceOf[DeleteExpression])
        case SyntaxKind.NumericLiteral =>
          return checkStrictModeNumericLiteral(
              node.asInstanceOf[NumericLiteral])
        case SyntaxKind.PostfixUnaryExpression =>
          return checkStrictModePostfixUnaryExpression(
              node.asInstanceOf[PostfixUnaryExpression])
        case SyntaxKind.PrefixUnaryExpression =>
          return checkStrictModePrefixUnaryExpression(
              node.asInstanceOf[PrefixUnaryExpression])
        case SyntaxKind.WithStatement =>
          return checkStrictModeWithStatement(node.asInstanceOf[WithStatement])
        case SyntaxKind.ThisType =>
          (seenThisKeyword = true)
          return
        case SyntaxKind.TypePredicate =>
          return checkTypePredicate(node.asInstanceOf[TypePredicateNode])
        case SyntaxKind.TypeParameter =>
          return declareSymbolAndAddToSymbolTable(
              node.asInstanceOf[Declaration], SymbolFlags.TypeParameter,
              SymbolFlags.TypeParameterExcludes)
        case SyntaxKind.Parameter =>
          return bindParameter(node.asInstanceOf[ParameterDeclaration])
        case SyntaxKind.VariableDeclaration | SyntaxKind.BindingElement =>
          return bindVariableDeclarationOrBindingElement(
              node.asInstanceOf[(VariableDeclaration | BindingElement)])
        case SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature |
            SyntaxKind.JSDocRecordMember =>
          return bindPropertyOrMethodOrAccessor(node.asInstanceOf[Declaration],
              (SymbolFlags.Property | ((if ((node
                            .asInstanceOf[PropertyDeclaration])
                        .questionToken) SymbolFlags.Optional
                  else SymbolFlags.None) )),
              SymbolFlags.PropertyExcludes)
        case SyntaxKind.JSDocPropertyTag =>
          return bindJSDocProperty(node.asInstanceOf[JSDocPropertyTag])
        case SyntaxKind.PropertyAssignment |
            SyntaxKind.ShorthandPropertyAssignment =>
          return bindPropertyOrMethodOrAccessor(node.asInstanceOf[Declaration],
              SymbolFlags.Property, SymbolFlags.PropertyExcludes)
        case SyntaxKind.EnumMember =>
          return bindPropertyOrMethodOrAccessor(node.asInstanceOf[Declaration],
              SymbolFlags.EnumMember, SymbolFlags.EnumMemberExcludes)
        case SyntaxKind.JsxSpreadAttribute =>
          (emitFlags |= NodeFlags.HasJsxSpreadAttributes)
          return
        case SyntaxKind.CallSignature | SyntaxKind.ConstructSignature |
            SyntaxKind.IndexSignature =>
          return declareSymbolAndAddToSymbolTable(
              node.asInstanceOf[Declaration], SymbolFlags.Signature,
              SymbolFlags.None)
        case SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature =>
          return bindPropertyOrMethodOrAccessor(node.asInstanceOf[Declaration],
              (SymbolFlags.Method | ((if ((
                          node.asInstanceOf[MethodDeclaration]).questionToken)
                    SymbolFlags.Optional
                  else SymbolFlags.None) )),
              (if (isObjectLiteralMethod(node)) SymbolFlags.PropertyExcludes
               else SymbolFlags.MethodExcludes))
        case SyntaxKind.FunctionDeclaration =>
          return bindFunctionDeclaration(
              node.asInstanceOf[FunctionDeclaration])
        case SyntaxKind.Constructor =>
          return declareSymbolAndAddToSymbolTable(
              node.asInstanceOf[Declaration], SymbolFlags.Constructor,
              SymbolFlags.None)
        case SyntaxKind.GetAccessor =>
          return bindPropertyOrMethodOrAccessor(node.asInstanceOf[Declaration],
              SymbolFlags.GetAccessor, SymbolFlags.GetAccessorExcludes)
        case SyntaxKind.SetAccessor =>
          return bindPropertyOrMethodOrAccessor(node.asInstanceOf[Declaration],
              SymbolFlags.SetAccessor, SymbolFlags.SetAccessorExcludes)
        case SyntaxKind.FunctionType | SyntaxKind.ConstructorType |
            SyntaxKind.JSDocFunctionType =>
          return bindFunctionOrConstructorType(
              node.asInstanceOf[SignatureDeclaration])
        case SyntaxKind.TypeLiteral | SyntaxKind.JSDocTypeLiteral |
            SyntaxKind.JSDocRecordType =>
          return bindAnonymousDeclaration(node.asInstanceOf[TypeLiteralNode],
              SymbolFlags.TypeLiteral, "__type")
        case SyntaxKind.ObjectLiteralExpression =>
          return bindObjectLiteralExpression(
              node.asInstanceOf[ObjectLiteralExpression])
        case SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction =>
          return bindFunctionExpression(node.asInstanceOf[FunctionExpression])
        case SyntaxKind.CallExpression =>
          if (isInJavaScriptFile(node)) {
            bindCallExpression(node.asInstanceOf[CallExpression])

          }
        case SyntaxKind.ClassExpression | SyntaxKind.ClassDeclaration =>
          (inStrictMode = true)
          return bindClassLikeDeclaration(
              node.asInstanceOf[ClassLikeDeclaration])
        case SyntaxKind.InterfaceDeclaration =>
          return bindBlockScopedDeclaration(node.asInstanceOf[Declaration],
              SymbolFlags.Interface, SymbolFlags.InterfaceExcludes)
        case SyntaxKind.JSDocTypedefTag =>
          if (((!(node.asInstanceOf[JSDocTypedefTag]).fullName) || ((node
                    .asInstanceOf[JSDocTypedefTag])
                .fullName
                .kind === SyntaxKind.Identifier))) {
            return bindBlockScopedDeclaration(node.asInstanceOf[Declaration],
                SymbolFlags.TypeAlias, SymbolFlags.TypeAliasExcludes)

          }
        case SyntaxKind.TypeAliasDeclaration =>
          return bindBlockScopedDeclaration(node.asInstanceOf[Declaration],
              SymbolFlags.TypeAlias, SymbolFlags.TypeAliasExcludes)
        case SyntaxKind.EnumDeclaration =>
          return bindEnumDeclaration(node.asInstanceOf[EnumDeclaration])
        case SyntaxKind.ModuleDeclaration =>
          return bindModuleDeclaration(node.asInstanceOf[ModuleDeclaration])
        case SyntaxKind.ImportEqualsDeclaration | SyntaxKind.NamespaceImport |
            SyntaxKind.ImportSpecifier | SyntaxKind.ExportSpecifier =>
          return declareSymbolAndAddToSymbolTable(
              node.asInstanceOf[Declaration], SymbolFlags.Alias,
              SymbolFlags.AliasExcludes)
        case SyntaxKind.NamespaceExportDeclaration =>
          return bindNamespaceExportDeclaration(
              node.asInstanceOf[NamespaceExportDeclaration])
        case SyntaxKind.ImportClause =>
          return bindImportClause(node.asInstanceOf[ImportClause])
        case SyntaxKind.ExportDeclaration =>
          return bindExportDeclaration(node.asInstanceOf[ExportDeclaration])
        case SyntaxKind.ExportAssignment =>
          return bindExportAssignment(node.asInstanceOf[ExportAssignment])
        case SyntaxKind.SourceFile =>
          updateStrictModeStatementList(
              (node.asInstanceOf[SourceFile]).statements)
          return bindSourceFileIfExternalModule()
        case SyntaxKind.Block =>
          if ((!isFunctionLike(node.parent))) {
            return

          }
        case SyntaxKind.ModuleBlock =>
          return updateStrictModeStatementList(
              (node.asInstanceOf[(Block | ModuleBlock)]).statements)
        case _ =>
      }

    }
    def checkTypePredicate(node: TypePredicateNode) = {
      const fresh9 = node
      val parameterName = fresh9.parameterName
      val `type` = fresh9.`type`
      if ((parameterName && (parameterName.kind === SyntaxKind.Identifier))) {
        checkStrictModeIdentifier(parameterName.asInstanceOf[Identifier])

      }
      if ((parameterName && (parameterName.kind === SyntaxKind.ThisType))) {
        (seenThisKeyword = true)

      }
      bind(`type`)

    }
    def bindSourceFileIfExternalModule() = {
      setExportContextFlag(file)
      if (isExternalModule(file)) {
        bindSourceFileAsExternalModule()

      }

    }
    def bindSourceFileAsExternalModule() = {
      bindAnonymousDeclaration(file, SymbolFlags.ValueModule,
          s"""\"${removeFileExtension(file.fileName)}\"""")

    }
    def bindExportAssignment(node: (ExportAssignment | BinaryExpression)) = {
      if (((!container.symbol) || (!container.symbol.exports))) {
        bindAnonymousDeclaration(node, SymbolFlags.Alias,
            getDeclarationName(node))

      } else {
        val flags =
          (if (((node.kind === SyntaxKind.ExportAssignment) && exportAssignmentIsAlias(
                   node.asInstanceOf[ExportAssignment]))) SymbolFlags.Alias
           else SymbolFlags.Property)
        declareSymbol(container.symbol.exports, container.symbol, node, flags,
            (((SymbolFlags.Property | SymbolFlags.AliasExcludes) | SymbolFlags.Class) | SymbolFlags.Function))

      }

    }
    def bindNamespaceExportDeclaration(node: NamespaceExportDeclaration) = {
      if ((node.modifiers && node.modifiers.length)) {
        file.bindDiagnostics.push(createDiagnosticForNode(node,
                Diagnostics.Modifiers_cannot_appear_here))

      }
      if ((node.parent.kind !== SyntaxKind.SourceFile)) {
        file.bindDiagnostics.push(createDiagnosticForNode(node,
                Diagnostics.Global_module_exports_may_only_appear_at_top_level))
        return

      } else {
        val parent = node.parent.asInstanceOf[SourceFile]
        if ((!isExternalModule(parent))) {
          file.bindDiagnostics.push(createDiagnosticForNode(node,
                  Diagnostics.Global_module_exports_may_only_appear_in_module_files))
          return

        }
        if ((!parent.isDeclarationFile)) {
          file.bindDiagnostics.push(createDiagnosticForNode(node,
                  Diagnostics.Global_module_exports_may_only_appear_in_declaration_files))
          return

        }

      }
      (file.symbol.globalExports = (file.symbol.globalExports || createMap[
            Symbol]()))
      declareSymbol(file.symbol.globalExports, file.symbol, node,
          SymbolFlags.Alias, SymbolFlags.AliasExcludes)

    }
    def bindExportDeclaration(node: ExportDeclaration) = {
      if (((!container.symbol) || (!container.symbol.exports))) {
        bindAnonymousDeclaration(node, SymbolFlags.ExportStar,
            getDeclarationName(node))

      } else if ((!node.exportClause)) {
        declareSymbol(container.symbol.exports, container.symbol, node,
            SymbolFlags.ExportStar, SymbolFlags.None)

      }

    }
    def bindImportClause(node: ImportClause) = {
      if (node.name) {
        declareSymbolAndAddToSymbolTable(node, SymbolFlags.Alias,
            SymbolFlags.AliasExcludes)

      }

    }
    def setCommonJsModuleIndicator(node: Node) = {
      if ((!file.commonJsModuleIndicator)) {
        (file.commonJsModuleIndicator = node)
        if ((!file.externalModuleIndicator)) {
          bindSourceFileAsExternalModule()

        }

      }

    }
    def bindExportsPropertyAssignment(node: BinaryExpression) = {
      setCommonJsModuleIndicator(node)
      declareSymbol(file.symbol.exports, file.symbol,
          node.left.asInstanceOf[PropertyAccessExpression],
          (SymbolFlags.Property | SymbolFlags.Export), SymbolFlags.None)

    }
    def bindModuleExportsAssignment(node: BinaryExpression) = {
      setCommonJsModuleIndicator(node)
      declareSymbol(file.symbol.exports, file.symbol, node,
          ((SymbolFlags.Property | SymbolFlags.Export) | SymbolFlags.ValueModule),
          SymbolFlags.None)

    }
    def bindThisPropertyAssignment(node: BinaryExpression) = {
      Debug.assert(isInJavaScriptFile(node))
      if (((container.kind === SyntaxKind.FunctionDeclaration) || (container.kind === SyntaxKind.FunctionExpression))) {
        (container.symbol.members = (container.symbol.members || createMap[
              Symbol]()))
        declareSymbol(container.symbol.members, container.symbol, node,
            SymbolFlags.Property,
            (SymbolFlags.PropertyExcludes & (~SymbolFlags.Property)))

      } else if ((container.kind === SyntaxKind.Constructor)) {
        val saveContainer = container
        (container = container.parent)
        val symbol = bindPropertyOrMethodOrAccessor(node, SymbolFlags.Property,
            SymbolFlags.None)
        if (symbol) {
          ((symbol.asInstanceOf[Symbol]).isReplaceableByMethod = true)

        }
        (container = saveContainer)

      }

    }
    def bindPrototypePropertyAssignment(node: BinaryExpression) = {
      val leftSideOfAssignment =
        node.left.asInstanceOf[PropertyAccessExpression]
      val classPrototype =
        leftSideOfAssignment.expression.asInstanceOf[PropertyAccessExpression]
      val constructorFunction =
        classPrototype.expression.asInstanceOf[Identifier]
      (leftSideOfAssignment.parent = node)
      (constructorFunction.parent = classPrototype)
      (classPrototype.parent = leftSideOfAssignment)
      val funcSymbol = container.locals(constructorFunction.text)
      if (((!funcSymbol) || (!(((funcSymbol.flags & SymbolFlags.Function) || isDeclarationOfFunctionExpression(
              funcSymbol)))))) {
        return

      }
      if ((!funcSymbol.members)) {
        (funcSymbol.members = createMap[Symbol]())

      }
      declareSymbol(funcSymbol.members, funcSymbol, leftSideOfAssignment,
          SymbolFlags.Property, SymbolFlags.PropertyExcludes)

    }
    def bindCallExpression(node: CallExpression) = {
      if (((!file.commonJsModuleIndicator) && isRequireCall(node, false))) {
        setCommonJsModuleIndicator(node)

      }

    }
    def bindClassLikeDeclaration(node: ClassLikeDeclaration) = {
      if (((!isDeclarationFile(file)) && (!isInAmbientContext(node)))) {
        if ((getClassExtendsHeritageClauseElement(node) !== undefined)) {
          (emitFlags |= NodeFlags.HasClassExtends)

        }
        if (nodeIsDecorated(node)) {
          (emitFlags |= NodeFlags.HasDecorators)

        }

      }
      if ((node.kind === SyntaxKind.ClassDeclaration)) {
        bindBlockScopedDeclaration(node, SymbolFlags.Class,
            SymbolFlags.ClassExcludes)

      } else {
        val bindingName = (if (node.name) node.name.text else "__class")
        bindAnonymousDeclaration(node, SymbolFlags.Class, bindingName)
        if (node.name) {
          (classifiableNames(node.name.text) = node.name.text)

        }

      }
      val symbol = node.symbol
      val prototypeSymbol = createSymbol(
          (SymbolFlags.Property | SymbolFlags.Prototype), "prototype")
      if (symbol.exports(prototypeSymbol.name)) {
        if (node.name) {
          (node.name.parent = node)

        }
        file.bindDiagnostics.push(
            createDiagnosticForNode(
                symbol.exports(prototypeSymbol.name).declarations(0),
                Diagnostics.Duplicate_identifier_0, prototypeSymbol.name))

      }
      (symbol.exports(prototypeSymbol.name) = prototypeSymbol)
      (prototypeSymbol.parent = symbol)

    }
    def bindEnumDeclaration(node: EnumDeclaration) = {
      return (if (isConst(node))
                bindBlockScopedDeclaration(node, SymbolFlags.ConstEnum,
                    SymbolFlags.ConstEnumExcludes)
              else
                bindBlockScopedDeclaration(node, SymbolFlags.RegularEnum,
                    SymbolFlags.RegularEnumExcludes))

    }
    def bindVariableDeclarationOrBindingElement(
        node: (VariableDeclaration | BindingElement)) = {
      if (inStrictMode) {
        checkStrictModeEvalOrArguments(node, node.name)

      }
      if ((!isBindingPattern(node.name))) {
        if (isBlockOrCatchScoped(node)) {
          bindBlockScopedVariableDeclaration(node)

        } else if (isParameterDeclaration(node)) {
          declareSymbolAndAddToSymbolTable(node,
              SymbolFlags.FunctionScopedVariable,
              SymbolFlags.ParameterExcludes)

        } else {
          declareSymbolAndAddToSymbolTable(node,
              SymbolFlags.FunctionScopedVariable,
              SymbolFlags.FunctionScopedVariableExcludes)

        }

      }

    }
    def bindParameter(node: ParameterDeclaration) = {
      if ((((!isDeclarationFile(file)) && (!isInAmbientContext(node))) && nodeIsDecorated(
              node))) {
        (emitFlags |= ((NodeFlags.HasDecorators | NodeFlags.HasParamDecorators)))

      }
      if (inStrictMode) {
        checkStrictModeEvalOrArguments(node, node.name)

      }
      if (isBindingPattern(node.name)) {
        bindAnonymousDeclaration(node, SymbolFlags.FunctionScopedVariable,
            getDestructuringParameterName(node))

      } else {
        declareSymbolAndAddToSymbolTable(node,
            SymbolFlags.FunctionScopedVariable, SymbolFlags.ParameterExcludes)

      }
      if (isParameterPropertyDeclaration(node)) {
        val classDeclaration =
          node.parent.parent.asInstanceOf[ClassLikeDeclaration]
        declareSymbol(classDeclaration.symbol.members, classDeclaration.symbol,
            node,
            (SymbolFlags.Property | ((if (node.questionToken)
                  SymbolFlags.Optional
                else SymbolFlags.None) )),
            SymbolFlags.PropertyExcludes)

      }

    }
    def bindFunctionDeclaration(node: FunctionDeclaration) = {
      if (((!isDeclarationFile(file)) && (!isInAmbientContext(node)))) {
        if (isAsyncFunctionLike(node)) {
          (emitFlags |= NodeFlags.HasAsyncFunctions)

        }

      }
      checkStrictModeFunctionName(node.asInstanceOf[FunctionDeclaration])
      if (inStrictMode) {
        checkStrictModeFunctionDeclaration(node)
        bindBlockScopedDeclaration(node, SymbolFlags.Function,
            SymbolFlags.FunctionExcludes)

      } else {
        declareSymbolAndAddToSymbolTable(node.asInstanceOf[Declaration],
            SymbolFlags.Function, SymbolFlags.FunctionExcludes)

      }

    }
    def bindFunctionExpression(node: FunctionExpression) = {
      if (((!isDeclarationFile(file)) && (!isInAmbientContext(node)))) {
        if (isAsyncFunctionLike(node)) {
          (emitFlags |= NodeFlags.HasAsyncFunctions)

        }

      }
      if (currentFlow) {
        (node.flowNode = currentFlow)

      }
      checkStrictModeFunctionName(node)
      val bindingName = (if (node.name) node.name.text else "__function")
      return bindAnonymousDeclaration(node, SymbolFlags.Function, bindingName)

    }
    def bindPropertyOrMethodOrAccessor(node: Declaration,
        symbolFlags: SymbolFlags, symbolExcludes: SymbolFlags) = {
      if (((!isDeclarationFile(file)) && (!isInAmbientContext(node)))) {
        if (isAsyncFunctionLike(node)) {
          (emitFlags |= NodeFlags.HasAsyncFunctions)

        }
        if (nodeIsDecorated(node)) {
          (emitFlags |= NodeFlags.HasDecorators)

        }

      }
      if ((currentFlow && isObjectLiteralOrClassExpressionMethod(node))) {
        (node.flowNode = currentFlow)

      }
      return (if (hasDynamicName(node))
                bindAnonymousDeclaration(node, symbolFlags, "__computed")
              else
                declareSymbolAndAddToSymbolTable(node, symbolFlags,
                    symbolExcludes))

    }
    def bindJSDocProperty(node: JSDocPropertyTag) = {
      return declareSymbolAndAddToSymbolTable(node, SymbolFlags.Property,
          SymbolFlags.PropertyExcludes)

    }
    def shouldReportErrorOnModuleDeclaration(
        node: ModuleDeclaration): Boolean = {
      val instanceState = getModuleInstanceState(node)
      return ((instanceState === ModuleInstanceState.Instantiated) || (((instanceState === ModuleInstanceState.ConstEnumOnly) && options.preserveConstEnums)))

    }
    def checkUnreachable(node: Node): Boolean = {
      if ((!((currentFlow.flags & FlowFlags.Unreachable)))) {
        return false

      }
      if ((currentFlow === unreachableFlow)) {
        val reportError = (((((isStatementButNotDeclaration(node) && (node.kind !== SyntaxKind.EmptyStatement))) || (node.kind === SyntaxKind.ClassDeclaration)) || (((node.kind === SyntaxKind.ModuleDeclaration) && shouldReportErrorOnModuleDeclaration(
              node
                .asInstanceOf[ModuleDeclaration])))) || (((node.kind === SyntaxKind.EnumDeclaration) && (((!isConstEnumDeclaration(
              node)) || options.preserveConstEnums)))))
        if (reportError) {
          (currentFlow = reportedUnreachableFlow)
          val reportUnreachableCode = (((!options.allowUnreachableCode) && (!isInAmbientContext(
                node))) && ((((node.kind !== SyntaxKind.VariableStatement) || (getCombinedNodeFlags((
                    node
                      .asInstanceOf[VariableStatement])
                  .declarationList) & NodeFlags.BlockScoped)) || forEach((node
                      .asInstanceOf[VariableStatement])
                  .declarationList
                  .declarations, (d => d.initializer)))))
          if (reportUnreachableCode) {
            errorOnFirstToken(node, Diagnostics.Unreachable_code_detected)

          }

        }

      }
      return true

    }

  }
  def computeTransformFlagsForNode(node: Node,
      subtreeFlags: TransformFlags): TransformFlags = {
    val kind = node.kind
    kind match {
      case SyntaxKind.CallExpression =>
        return computeCallExpression(node.asInstanceOf[CallExpression],
            subtreeFlags)
      case SyntaxKind.NewExpression =>
        return computeNewExpression(node.asInstanceOf[NewExpression],
            subtreeFlags)
      case SyntaxKind.ModuleDeclaration =>
        return computeModuleDeclaration(node.asInstanceOf[ModuleDeclaration],
            subtreeFlags)
      case SyntaxKind.ParenthesizedExpression =>
        return computeParenthesizedExpression(
            node.asInstanceOf[ParenthesizedExpression], subtreeFlags)
      case SyntaxKind.BinaryExpression =>
        return computeBinaryExpression(node.asInstanceOf[BinaryExpression],
            subtreeFlags)
      case SyntaxKind.ExpressionStatement =>
        return computeExpressionStatement(
            node.asInstanceOf[ExpressionStatement], subtreeFlags)
      case SyntaxKind.Parameter =>
        return computeParameter(node.asInstanceOf[ParameterDeclaration],
            subtreeFlags)
      case SyntaxKind.ArrowFunction =>
        return computeArrowFunction(node.asInstanceOf[ArrowFunction],
            subtreeFlags)
      case SyntaxKind.FunctionExpression =>
        return computeFunctionExpression(node.asInstanceOf[FunctionExpression],
            subtreeFlags)
      case SyntaxKind.FunctionDeclaration =>
        return computeFunctionDeclaration(
            node.asInstanceOf[FunctionDeclaration], subtreeFlags)
      case SyntaxKind.VariableDeclaration =>
        return computeVariableDeclaration(
            node.asInstanceOf[VariableDeclaration], subtreeFlags)
      case SyntaxKind.VariableDeclarationList =>
        return computeVariableDeclarationList(
            node.asInstanceOf[VariableDeclarationList], subtreeFlags)
      case SyntaxKind.VariableStatement =>
        return computeVariableStatement(node.asInstanceOf[VariableStatement],
            subtreeFlags)
      case SyntaxKind.LabeledStatement =>
        return computeLabeledStatement(node.asInstanceOf[LabeledStatement],
            subtreeFlags)
      case SyntaxKind.ClassDeclaration =>
        return computeClassDeclaration(node.asInstanceOf[ClassDeclaration],
            subtreeFlags)
      case SyntaxKind.ClassExpression =>
        return computeClassExpression(node.asInstanceOf[ClassExpression],
            subtreeFlags)
      case SyntaxKind.HeritageClause =>
        return computeHeritageClause(node.asInstanceOf[HeritageClause],
            subtreeFlags)
      case SyntaxKind.CatchClause =>
        return computeCatchClause(node.asInstanceOf[CatchClause], subtreeFlags)
      case SyntaxKind.ExpressionWithTypeArguments =>
        return computeExpressionWithTypeArguments(
            node.asInstanceOf[ExpressionWithTypeArguments], subtreeFlags)
      case SyntaxKind.Constructor =>
        return computeConstructor(node.asInstanceOf[ConstructorDeclaration],
            subtreeFlags)
      case SyntaxKind.PropertyDeclaration =>
        return computePropertyDeclaration(
            node.asInstanceOf[PropertyDeclaration], subtreeFlags)
      case SyntaxKind.MethodDeclaration =>
        return computeMethod(node.asInstanceOf[MethodDeclaration],
            subtreeFlags)
      case SyntaxKind.GetAccessor | SyntaxKind.SetAccessor =>
        return computeAccessor(node.asInstanceOf[AccessorDeclaration],
            subtreeFlags)
      case SyntaxKind.ImportEqualsDeclaration =>
        return computeImportEquals(node.asInstanceOf[ImportEqualsDeclaration],
            subtreeFlags)
      case SyntaxKind.PropertyAccessExpression =>
        return computePropertyAccess(
            node.asInstanceOf[PropertyAccessExpression], subtreeFlags)
      case _ =>
        return computeOther(node, kind, subtreeFlags)
    }

  }
  def computeCallExpression(node: CallExpression,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    val expression = node.expression
    val expressionKind = expression.kind
    if (node.typeArguments) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if (((subtreeFlags & TransformFlags.ContainsSpreadElementExpression) || isSuperOrSuperProperty(
            expression, expressionKind))) {
      (transformFlags |= TransformFlags.AssertES2015)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ArrayLiteralOrCallOrNewExcludes))

  }
  def isSuperOrSuperProperty(node: Node, kind: SyntaxKind) = {
    kind match {
      case SyntaxKind.SuperKeyword =>
        return true
      case SyntaxKind.PropertyAccessExpression |
          SyntaxKind.ElementAccessExpression =>
        val expression = (
            node
              .asInstanceOf[
                  (PropertyAccessExpression | ElementAccessExpression)])
          .expression
        val expressionKind = expression.kind
        return (expressionKind === SyntaxKind.SuperKeyword)
      case _ =>
    }
    return false

  }
  def computeNewExpression(node: NewExpression,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if (node.typeArguments) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if ((subtreeFlags & TransformFlags.ContainsSpreadElementExpression)) {
      (transformFlags |= TransformFlags.AssertES2015)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ArrayLiteralOrCallOrNewExcludes))

  }
  def computeBinaryExpression(node: BinaryExpression,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    val operatorTokenKind = node.operatorToken.kind
    val leftKind = node.left.kind
    if (((operatorTokenKind === SyntaxKind.EqualsToken) && (((leftKind === SyntaxKind.ObjectLiteralExpression) || (leftKind === SyntaxKind.ArrayLiteralExpression))))) {
      (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.DestructuringAssignment))

    } else if (((operatorTokenKind === SyntaxKind.AsteriskAsteriskToken) || (operatorTokenKind === SyntaxKind.AsteriskAsteriskEqualsToken))) {
      (transformFlags |= TransformFlags.AssertES2016)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeParameter(node: ParameterDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    val modifierFlags = getModifierFlags(node)
    val name = node.name
    val initializer = node.initializer
    val dotDotDotToken = node.dotDotDotToken
    if ((((node.questionToken || node.`type`) || (subtreeFlags & TransformFlags.ContainsDecorators)) || isThisIdentifier(
            name))) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if ((modifierFlags & ModifierFlags.ParameterPropertyModifier)) {
      (transformFlags |= (TransformFlags.AssertTypeScript | TransformFlags.ContainsParameterPropertyAssignments))

    }
    if ((((subtreeFlags & TransformFlags.ContainsBindingPattern) || initializer) || dotDotDotToken)) {
      (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.ContainsDefaultValueAssignments))

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ParameterExcludes))

  }
  def computeParenthesizedExpression(node: ParenthesizedExpression,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    val expression = node.expression
    val expressionKind = expression.kind
    val expressionTransformFlags = expression.transformFlags
    if (((expressionKind === SyntaxKind.AsExpression) || (expressionKind === SyntaxKind.TypeAssertionExpression))) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if ((expressionTransformFlags & TransformFlags.DestructuringAssignment)) {
      (transformFlags |= TransformFlags.DestructuringAssignment)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeClassDeclaration(node: ClassDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags: TransformFlags = zeroOfMyType
    val modifierFlags = getModifierFlags(node)
    if ((modifierFlags & ModifierFlags.Ambient)) {
      (transformFlags = TransformFlags.AssertTypeScript)

    } else {
      (transformFlags = (subtreeFlags | TransformFlags.AssertES2015))
      if (((((subtreeFlags & TransformFlags.TypeScriptClassSyntaxMask)) || ((modifierFlags & ModifierFlags.Export))) || node.typeParameters)) {
        (transformFlags |= TransformFlags.AssertTypeScript)

      }
      if ((subtreeFlags & TransformFlags.ContainsLexicalThisInComputedPropertyName)) {
        (transformFlags |= TransformFlags.ContainsLexicalThis)

      }

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ClassExcludes))

  }
  def computeClassExpression(node: ClassExpression,
      subtreeFlags: TransformFlags) = {
    var transformFlags = (subtreeFlags | TransformFlags.AssertES2015)
    if (((subtreeFlags & TransformFlags.TypeScriptClassSyntaxMask) || node.typeParameters)) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if ((subtreeFlags & TransformFlags.ContainsLexicalThisInComputedPropertyName)) {
      (transformFlags |= TransformFlags.ContainsLexicalThis)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ClassExcludes))

  }
  def computeHeritageClause(node: HeritageClause,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    node.token match {
      case SyntaxKind.ExtendsKeyword =>
        (transformFlags |= TransformFlags.AssertES2015)
      case SyntaxKind.ImplementsKeyword =>
        (transformFlags |= TransformFlags.AssertTypeScript)
      case _ =>
        Debug.fail("Unexpected token for heritage clause")
    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeCatchClause(node: CatchClause, subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if ((node.variableDeclaration && isBindingPattern(
            node.variableDeclaration.name))) {
      (transformFlags |= TransformFlags.AssertES2015)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeExpressionWithTypeArguments(node: ExpressionWithTypeArguments,
      subtreeFlags: TransformFlags) = {
    var transformFlags = (subtreeFlags | TransformFlags.AssertES2015)
    if (node.typeArguments) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeConstructor(node: ConstructorDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if ((hasModifier(node,
            ModifierFlags.TypeScriptModifier) || (!node.body))) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ConstructorExcludes))

  }
  def computeMethod(node: MethodDeclaration, subtreeFlags: TransformFlags) = {
    var transformFlags = (subtreeFlags | TransformFlags.AssertES2015)
    if (((((node.decorators || hasModifier(node,
            ModifierFlags.TypeScriptModifier)) || node.typeParameters) || node.`type`) || (!node.body))) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if (hasModifier(node, ModifierFlags.Async)) {
      (transformFlags |= TransformFlags.AssertES2017)

    }
    if ((node.asteriskToken && (getEmitFlags(node) & EmitFlags.AsyncFunctionBody))) {
      (transformFlags |= TransformFlags.AssertGenerator)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.MethodOrAccessorExcludes))

  }
  def computeAccessor(node: AccessorDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if ((((node.decorators || hasModifier(node,
            ModifierFlags.TypeScriptModifier)) || node.`type`) || (!node.body))) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.MethodOrAccessorExcludes))

  }
  def computePropertyDeclaration(node: PropertyDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags = (subtreeFlags | TransformFlags.AssertTypeScript)
    if (node.initializer) {
      (transformFlags |= TransformFlags.ContainsPropertyInitializer)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeFunctionDeclaration(node: FunctionDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags: TransformFlags = zeroOfMyType
    val modifierFlags = getModifierFlags(node)
    val body = node.body
    if (((!body) || ((modifierFlags & ModifierFlags.Ambient)))) {
      (transformFlags = TransformFlags.AssertTypeScript)

    } else {
      (transformFlags = (subtreeFlags | TransformFlags.ContainsHoistedDeclarationOrCompletion))
      if ((modifierFlags & ModifierFlags.Export)) {
        (transformFlags |= (TransformFlags.AssertTypeScript | TransformFlags.AssertES2015))

      }
      if ((((modifierFlags & ModifierFlags.TypeScriptModifier) || node.typeParameters) || node.`type`)) {
        (transformFlags |= TransformFlags.AssertTypeScript)

      }
      if ((modifierFlags & ModifierFlags.Async)) {
        (transformFlags |= TransformFlags.AssertES2017)

      }
      if ((subtreeFlags & TransformFlags.ES2015FunctionSyntaxMask)) {
        (transformFlags |= TransformFlags.AssertES2015)

      }
      if ((node.asteriskToken && (getEmitFlags(node) & EmitFlags.AsyncFunctionBody))) {
        (transformFlags |= TransformFlags.AssertGenerator)

      }

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.FunctionExcludes))

  }
  def computeFunctionExpression(node: FunctionExpression,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if (((hasModifier(node,
            ModifierFlags.TypeScriptModifier) || node.typeParameters) || node.`type`)) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if (hasModifier(node, ModifierFlags.Async)) {
      (transformFlags |= TransformFlags.AssertES2017)

    }
    if ((subtreeFlags & TransformFlags.ES2015FunctionSyntaxMask)) {
      (transformFlags |= TransformFlags.AssertES2015)

    }
    if ((node.asteriskToken && (getEmitFlags(node) & EmitFlags.AsyncFunctionBody))) {
      (transformFlags |= TransformFlags.AssertGenerator)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.FunctionExcludes))

  }
  def computeArrowFunction(node: ArrowFunction,
      subtreeFlags: TransformFlags) = {
    var transformFlags = (subtreeFlags | TransformFlags.AssertES2015)
    if (((hasModifier(node,
            ModifierFlags.TypeScriptModifier) || node.typeParameters) || node.`type`)) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    if (hasModifier(node, ModifierFlags.Async)) {
      (transformFlags |= TransformFlags.AssertES2017)

    }
    if ((subtreeFlags & TransformFlags.ContainsLexicalThis)) {
      (transformFlags |= TransformFlags.ContainsCapturedLexicalThis)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ArrowFunctionExcludes))

  }
  def computePropertyAccess(node: PropertyAccessExpression,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    val expression = node.expression
    val expressionKind = expression.kind
    if ((expressionKind === SyntaxKind.SuperKeyword)) {
      (transformFlags |= TransformFlags.ContainsLexicalThis)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeVariableDeclaration(node: VariableDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    val nameKind = node.name.kind
    if (((nameKind === SyntaxKind.ObjectBindingPattern) || (nameKind === SyntaxKind.ArrayBindingPattern))) {
      (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.ContainsBindingPattern))

    }
    if (node.`type`) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeVariableStatement(node: VariableStatement,
      subtreeFlags: TransformFlags) = {
    var transformFlags: TransformFlags = zeroOfMyType
    val modifierFlags = getModifierFlags(node)
    val declarationListTransformFlags = node.declarationList.transformFlags
    if ((modifierFlags & ModifierFlags.Ambient)) {
      (transformFlags = TransformFlags.AssertTypeScript)

    } else {
      (transformFlags = subtreeFlags)
      if ((modifierFlags & ModifierFlags.Export)) {
        (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.AssertTypeScript))

      }
      if ((declarationListTransformFlags & TransformFlags.ContainsBindingPattern)) {
        (transformFlags |= TransformFlags.AssertES2015)

      }

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeLabeledStatement(node: LabeledStatement,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if (((subtreeFlags & TransformFlags.ContainsBlockScopedBinding) && isIterationStatement(
            node, true))) {
      (transformFlags |= TransformFlags.AssertES2015)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeImportEquals(node: ImportEqualsDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if ((!isExternalModuleImportEqualsDeclaration(node))) {
      (transformFlags |= TransformFlags.AssertTypeScript)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeExpressionStatement(node: ExpressionStatement,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    if ((node.expression.transformFlags & TransformFlags.DestructuringAssignment)) {
      (transformFlags |= TransformFlags.AssertES2015)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.NodeExcludes))

  }
  def computeModuleDeclaration(node: ModuleDeclaration,
      subtreeFlags: TransformFlags) = {
    var transformFlags = TransformFlags.AssertTypeScript
    val modifierFlags = getModifierFlags(node)
    if ((((modifierFlags & ModifierFlags.Ambient)) === 0)) {
      (transformFlags |= subtreeFlags)

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.ModuleExcludes))

  }
  def computeVariableDeclarationList(node: VariableDeclarationList,
      subtreeFlags: TransformFlags) = {
    var transformFlags = (subtreeFlags | TransformFlags.ContainsHoistedDeclarationOrCompletion)
    if ((subtreeFlags & TransformFlags.ContainsBindingPattern)) {
      (transformFlags |= TransformFlags.AssertES2015)

    }
    if ((node.flags & NodeFlags.BlockScoped)) {
      (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.ContainsBlockScopedBinding))

    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~TransformFlags.VariableDeclarationListExcludes))

  }
  def computeOther(node: Node, kind: SyntaxKind,
      subtreeFlags: TransformFlags) = {
    var transformFlags = subtreeFlags
    var excludeFlags = TransformFlags.NodeExcludes
    kind match {
      case SyntaxKind.AsyncKeyword | SyntaxKind.AwaitExpression =>
        (transformFlags |= TransformFlags.AssertES2017)
      case SyntaxKind.PublicKeyword | SyntaxKind.PrivateKeyword |
          SyntaxKind.ProtectedKeyword | SyntaxKind.AbstractKeyword |
          SyntaxKind.DeclareKeyword | SyntaxKind.ConstKeyword |
          SyntaxKind.EnumDeclaration | SyntaxKind.EnumMember |
          SyntaxKind.TypeAssertionExpression | SyntaxKind.AsExpression |
          SyntaxKind.NonNullExpression | SyntaxKind.ReadonlyKeyword =>
        (transformFlags |= TransformFlags.AssertTypeScript)
      case SyntaxKind.JsxElement | SyntaxKind.JsxSelfClosingElement |
          SyntaxKind.JsxOpeningElement | SyntaxKind.JsxText |
          SyntaxKind.JsxClosingElement | SyntaxKind.JsxAttribute |
          SyntaxKind.JsxSpreadAttribute | SyntaxKind.JsxExpression =>
        (transformFlags |= TransformFlags.AssertJsx)
      case SyntaxKind.ExportKeyword =>
        (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.AssertTypeScript))
      case SyntaxKind.DefaultKeyword |
          SyntaxKind.NoSubstitutionTemplateLiteral | SyntaxKind.TemplateHead |
          SyntaxKind.TemplateMiddle | SyntaxKind.TemplateTail |
          SyntaxKind.TemplateExpression | SyntaxKind.TaggedTemplateExpression |
          SyntaxKind.ShorthandPropertyAssignment | SyntaxKind.ForOfStatement =>
        (transformFlags |= TransformFlags.AssertES2015)
      case SyntaxKind.YieldExpression =>
        (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.ContainsYield))
      case SyntaxKind.AnyKeyword | SyntaxKind.NumberKeyword |
          SyntaxKind.NeverKeyword | SyntaxKind.StringKeyword |
          SyntaxKind.BooleanKeyword | SyntaxKind.SymbolKeyword |
          SyntaxKind.VoidKeyword | SyntaxKind.TypeParameter |
          SyntaxKind.PropertySignature | SyntaxKind.MethodSignature |
          SyntaxKind.CallSignature | SyntaxKind.ConstructSignature |
          SyntaxKind.IndexSignature | SyntaxKind.TypePredicate |
          SyntaxKind.TypeReference | SyntaxKind.FunctionType |
          SyntaxKind.ConstructorType | SyntaxKind.TypeQuery |
          SyntaxKind.TypeLiteral | SyntaxKind.ArrayType |
          SyntaxKind.TupleType | SyntaxKind.UnionType |
          SyntaxKind.IntersectionType | SyntaxKind.ParenthesizedType |
          SyntaxKind.InterfaceDeclaration | SyntaxKind.TypeAliasDeclaration |
          SyntaxKind.ThisType | SyntaxKind.LiteralType =>
        (transformFlags = TransformFlags.AssertTypeScript)
        (excludeFlags = TransformFlags.TypeExcludes)
      case SyntaxKind.ComputedPropertyName =>
        (transformFlags |= TransformFlags.ContainsComputedPropertyName)
        if ((subtreeFlags & TransformFlags.ContainsLexicalThis)) {
          (transformFlags |= TransformFlags.ContainsLexicalThisInComputedPropertyName)

        }
      case SyntaxKind.SpreadElementExpression =>
        (transformFlags |= TransformFlags.ContainsSpreadElementExpression)
      case SyntaxKind.SuperKeyword =>
        (transformFlags |= TransformFlags.AssertES2015)
      case SyntaxKind.ThisKeyword =>
        (transformFlags |= TransformFlags.ContainsLexicalThis)
      case SyntaxKind.ObjectBindingPattern | SyntaxKind.ArrayBindingPattern =>
        (transformFlags |= (TransformFlags.AssertES2015 | TransformFlags.ContainsBindingPattern))
      case SyntaxKind.Decorator =>
        (transformFlags |= (TransformFlags.AssertTypeScript | TransformFlags.ContainsDecorators))
      case SyntaxKind.ObjectLiteralExpression =>
        (excludeFlags = TransformFlags.ObjectLiteralExcludes)
        if ((subtreeFlags & TransformFlags.ContainsComputedPropertyName)) {
          (transformFlags |= TransformFlags.AssertES2015)

        }
        if ((subtreeFlags & TransformFlags.ContainsLexicalThisInComputedPropertyName)) {
          (transformFlags |= TransformFlags.ContainsLexicalThis)

        }
      case SyntaxKind.ArrayLiteralExpression | SyntaxKind.NewExpression =>
        (excludeFlags = TransformFlags.ArrayLiteralOrCallOrNewExcludes)
        if ((subtreeFlags & TransformFlags.ContainsSpreadElementExpression)) {
          (transformFlags |= TransformFlags.AssertES2015)

        }
      case SyntaxKind.DoStatement | SyntaxKind.WhileStatement |
          SyntaxKind.ForStatement | SyntaxKind.ForInStatement =>
        if ((subtreeFlags & TransformFlags.ContainsBlockScopedBinding)) {
          (transformFlags |= TransformFlags.AssertES2015)

        }
      case SyntaxKind.SourceFile =>
        if ((subtreeFlags & TransformFlags.ContainsCapturedLexicalThis)) {
          (transformFlags |= TransformFlags.AssertES2015)

        }
      case SyntaxKind.ReturnStatement | SyntaxKind.ContinueStatement |
          SyntaxKind.BreakStatement =>
        (transformFlags |= TransformFlags.ContainsHoistedDeclarationOrCompletion)
      case _ =>
    }
    (node.transformFlags = (transformFlags | TransformFlags.HasComputedFlags))
    return (transformFlags & (~excludeFlags))

  }
}
