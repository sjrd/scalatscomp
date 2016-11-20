package scalatscomp
object Transformer {
  val moduleTransformerMap = createMap[Transformer](
    Map(
      ModuleKind.ES2015 -> transformES2015Module,
      ModuleKind.System -> transformSystemModule,
      ModuleKind.AMD -> transformModule,
      ModuleKind.CommonJS -> transformModule,
      ModuleKind.UMD -> transformModule,
      ModuleKind.None -> transformModule))
  sealed abstract class SyntaxKindFeatureFlags
  object SyntaxKindFeatureFlags {
    case object Substitution extends SyntaxKindFeatureFlags
    case object EmitNotifications extends SyntaxKindFeatureFlags
  }
  trait TransformationResult {
    var transformed: Array[SourceFile]
    def emitNodeWithSubstitution(emitContext: EmitContext,
                                 node: Node,
                                 emitCallback: ((EmitContext,
                                                 Node) => Unit)): Unit
    def emitNodeWithNotification(emitContext: EmitContext,
                                 node: Node,
                                 emitCallback: ((EmitContext,
                                                 Node) => Unit)): Unit
  }
  trait TransformationContext extends LexicalEnvironment {
    def getCompilerOptions(): CompilerOptions
    def getEmitResolver(): EmitResolver
    def getEmitHost(): EmitHost
    def hoistFunctionDeclaration(node: FunctionDeclaration): Unit
    def hoistVariableDeclaration(node: Identifier): Unit
    def enableSubstitution(kind: SyntaxKind): Unit
    def isSubstitutionEnabled(node: Node): Boolean
    var onSubstituteNode: ((EmitContext, Node) => Node)
    def enableEmitNotification(kind: SyntaxKind): Unit
    def isEmitNotificationEnabled(node: Node): Boolean
    var onEmitNode: ((EmitContext, Node, ((EmitContext,
                                           Node) => Unit)) => Unit)
  }
  type Transformer = ((TransformationContext) => ((SourceFile) => SourceFile))
  def getTransformers(compilerOptions: CompilerOptions) = {
    val jsx = compilerOptions.jsx
    val languageVersion = getEmitScriptTarget(compilerOptions)
    val moduleKind = getEmitModuleKind(compilerOptions)
    val transformers: Array[Transformer] = Array()
    transformers.push(transformScala)
    transformers.push(transformTypeScript)
    transformers.push(
      (moduleTransformerMap(moduleKind) || moduleTransformerMap(
        ModuleKind.None)))
    if ((jsx === JsxEmit.React)) {
      transformers.push(transformJsx)

    }
    if ((languageVersion < ScriptTarget.ES2017)) {
      transformers.push(transformES2017)

    }
    if ((languageVersion < ScriptTarget.ES2016)) {
      transformers.push(transformES2016)

    }
    if ((languageVersion < ScriptTarget.ES2015)) {
      transformers.push(transformES2015)
      transformers.push(transformGenerators)

    }
    if ((languageVersion < ScriptTarget.ES5)) {
      transformers.push(transformES5)

    }
    return transformers

  }
  def transformFiles(
      resolver: EmitResolver,
      host: EmitHost,
      sourceFiles: Array[SourceFile],
      transformers: Array[Transformer]): TransformationResult = {
    val lexicalEnvironmentVariableDeclarationsStack: Array[
      Array[VariableDeclaration]] = Array()
    val lexicalEnvironmentFunctionDeclarationsStack: Array[
      Array[FunctionDeclaration]] = Array()
    val enabledSyntaxKindFeatures =
      new Array[SyntaxKindFeatureFlags](SyntaxKind.Count)
    var lexicalEnvironmentStackOffset = 0
    var hoistedVariableDeclarations: Array[VariableDeclaration] = zeroOfMyType
    var hoistedFunctionDeclarations: Array[FunctionDeclaration] = zeroOfMyType
    var lexicalEnvironmentDisabled: Boolean = zeroOfMyType
    val context: TransformationContext = Map(
      "getCompilerOptions" -> (() => host.getCompilerOptions()),
      "getEmitResolver" -> (() => resolver),
      "getEmitHost" -> (() => host),
      "hoistVariableDeclaration" -> hoistVariableDeclaration,
      "hoistFunctionDeclaration" -> hoistFunctionDeclaration,
      "startLexicalEnvironment" -> startLexicalEnvironment,
      "endLexicalEnvironment" -> endLexicalEnvironment,
      "onSubstituteNode" -> ((_emitContext, node) => node),
      "enableSubstitution" -> enableSubstitution,
      "isSubstitutionEnabled" -> isSubstitutionEnabled,
      "onEmitNode" -> ((node, emitContext,
                        emitCallback) => emitCallback(node, emitContext)),
      "enableEmitNotification" -> enableEmitNotification,
      "isEmitNotificationEnabled" -> isEmitNotificationEnabled)
    val transformation = chain(transformers: _*)(context)
    val transformed = map(sourceFiles, transformSourceFile)
    (lexicalEnvironmentDisabled = true)
    return Map(
      "transformed" -> transformed,
      "emitNodeWithSubstitution" -> emitNodeWithSubstitution,
      "emitNodeWithNotification" -> emitNodeWithNotification)
    def transformSourceFile(sourceFile: SourceFile) = {
      if (isDeclarationFile(sourceFile)) {
        return sourceFile

      }
      return transformation(sourceFile)

    }
    def enableSubstitution(kind: SyntaxKind) = {
      (enabledSyntaxKindFeatures(kind) |= SyntaxKindFeatureFlags.Substitution)

    }
    def isSubstitutionEnabled(node: Node) = {
      return ((((enabledSyntaxKindFeatures(node.kind) & SyntaxKindFeatureFlags.Substitution)) !== 0) && (((getEmitFlags(
        node) & EmitFlags.NoSubstitution)) === 0))

    }
    def emitNodeWithSubstitution(emitContext: EmitContext,
                                 node: Node,
                                 emitCallback: ((EmitContext,
                                                 Node) => Unit)) = {
      if (node) {
        if (isSubstitutionEnabled(node)) {
          val substitute = context.onSubstituteNode(emitContext, node)
          if ((substitute && (substitute !== node))) {
            emitCallback(emitContext, substitute)
            return

          }

        }
        emitCallback(emitContext, node)

      }

    }
    def enableEmitNotification(kind: SyntaxKind) = {
      (enabledSyntaxKindFeatures(kind) |= SyntaxKindFeatureFlags.EmitNotifications)

    }
    def isEmitNotificationEnabled(node: Node) = {
      return ((((enabledSyntaxKindFeatures(node.kind) & SyntaxKindFeatureFlags.EmitNotifications)) !== 0) || (((getEmitFlags(
        node) & EmitFlags.AdviseOnEmitNode)) !== 0))

    }
    def emitNodeWithNotification(emitContext: EmitContext,
                                 node: Node,
                                 emitCallback: ((EmitContext,
                                                 Node) => Unit)) = {
      if (node) {
        if (isEmitNotificationEnabled(node)) {
          context.onEmitNode(emitContext, node, emitCallback)

        } else {
          emitCallback(emitContext, node)

        }

      }

    }
    def hoistVariableDeclaration(name: Identifier): Unit = {
      Debug.assert(
        (!lexicalEnvironmentDisabled),
        "Cannot modify the lexical environment during the print phase.")
      val decl = createVariableDeclaration(name)
      if ((!hoistedVariableDeclarations)) {
        (hoistedVariableDeclarations = Array(decl))

      } else {
        hoistedVariableDeclarations.push(decl)

      }

    }
    def hoistFunctionDeclaration(func: FunctionDeclaration): Unit = {
      Debug.assert(
        (!lexicalEnvironmentDisabled),
        "Cannot modify the lexical environment during the print phase.")
      if ((!hoistedFunctionDeclarations)) {
        (hoistedFunctionDeclarations = Array(func))

      } else {
        hoistedFunctionDeclarations.push(func)

      }

    }
    def startLexicalEnvironment(): Unit = {
      Debug.assert(
        (!lexicalEnvironmentDisabled),
        "Cannot start a lexical environment during the print phase.")
      (lexicalEnvironmentVariableDeclarationsStack(
        lexicalEnvironmentStackOffset) = hoistedVariableDeclarations)
      (lexicalEnvironmentFunctionDeclarationsStack(
        lexicalEnvironmentStackOffset) = hoistedFunctionDeclarations)
      (lexicalEnvironmentStackOffset += 1)
      (hoistedVariableDeclarations = undefined)
      (hoistedFunctionDeclarations = undefined)

    }
    def endLexicalEnvironment(): Array[Statement] = {
      Debug.assert(
        (!lexicalEnvironmentDisabled),
        "Cannot end a lexical environment during the print phase.")
      var statements: Array[Statement] = zeroOfMyType
      if ((hoistedVariableDeclarations || hoistedFunctionDeclarations)) {
        if (hoistedFunctionDeclarations) {
          (statements = Array(hoistedFunctionDeclarations: _*))

        }
        if (hoistedVariableDeclarations) {
          val statement = createVariableStatement(
            undefined,
            createVariableDeclarationList(hoistedVariableDeclarations))
          if ((!statements)) {
            (statements = Array(statement))

          } else {
            statements.push(statement)

          }

        }

      }
      (lexicalEnvironmentStackOffset -= 1)
      (hoistedVariableDeclarations =
        lexicalEnvironmentVariableDeclarationsStack(
          lexicalEnvironmentStackOffset))
      (hoistedFunctionDeclarations =
        lexicalEnvironmentFunctionDeclarationsStack(
          lexicalEnvironmentStackOffset))
      return statements

    }

  }
}
