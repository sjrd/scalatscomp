package scalatscomp

object Program {
  val version = "2.1.1"
  val emptyArray: Array[Any] = Array()

  def findConfigFile(searchPath: String, fileExists: ((String) => Boolean), configName: Nothing = "tsconfig.json"): String = {
    while (true) {
      {
        val fileName = combinePaths(searchPath, configName)
        if (fileExists(fileName)) {
          return fileName

        }
        val parentPath = getDirectoryPath(searchPath)
        if ((parentPath === searchPath)) {
          break()

        }
        (searchPath = parentPath)

      }
    }
    return undefined

  }

  def resolveTripleslashReference(moduleName: String, containingFile: String): String = {
    val basePath = getDirectoryPath(containingFile)
    val referencedFileName = (if (isRootedDiskPath(moduleName)) moduleName else combinePaths(basePath, moduleName))
    return normalizePath(referencedFileName)

  }

  def computeCommonSourceDirectoryOfFilenames(fileNames: Array[String], currentDirectory: String, getCanonicalFileName: ((String) => String)): String = {
    var commonPathComponents: Array[String] = zeroOfMyType
    val failed = forEach(fileNames, (sourceFile => {
      val sourcePathComponents = getNormalizedPathComponents(sourceFile, currentDirectory)
      sourcePathComponents.pop()
      if ((!commonPathComponents)) {
        (commonPathComponents = sourcePathComponents)
        return

      }
      {
        var i = 0
        var n = Math.min(commonPathComponents.length, sourcePathComponents.length)
        while ((i < n)) {
          {
            if ((getCanonicalFileName(commonPathComponents(i)) !== getCanonicalFileName(sourcePathComponents(i)))) {
              if ((i === 0)) {
                return true

              }
              (commonPathComponents.length = i)
              break()

            }

          }
          (i += 1)
        }
      }
      if ((sourcePathComponents.length < commonPathComponents.length)) {
        (commonPathComponents.length = sourcePathComponents.length)

      }

    }))
    if (failed) {
      return ""

    }
    if ((!commonPathComponents)) {
      return currentDirectory

    }
    return getNormalizedPathFromPathComponents(commonPathComponents)

  }

  trait OutputFingerprint {
    var hash: String
    var byteOrderMark: Boolean
    var mtime: Date
  }

  def createCompilerHost(options: CompilerOptions, setParentNodes: Boolean): CompilerHost = {
    val existingDirectories = createMap[Boolean]()
    def getCanonicalFileName(fileName: String): String = {
      return (if (sys.useCaseSensitiveFileNames) fileName else fileName.toLowerCase())

    }
    val unsupportedFileEncodingErrorCode = (-2147024809)
    def getSourceFile(fileName: String, languageVersion: ScriptTarget, onError: ((String) => Unit)): SourceFile = {
      var text: String = zeroOfMyType
      try {
        performance.mark("beforeIORead")
        (text = sys.readFile(fileName, options.charset))
        performance.mark("afterIORead")
        performance.measure("I/O Read", "beforeIORead", "afterIORead")

      } catch {
        case e: Throwable => {
          if (onError) {
            onError((if ((e.number === unsupportedFileEncodingErrorCode)) createCompilerDiagnostic(Diagnostics.Unsupported_file_encoding).messageText else e.message))

          }
          (text = "")

        }
      }
      return (if ((text !== undefined)) createSourceFile(fileName, text, languageVersion, setParentNodes) else undefined)

    }
    def directoryExists(directoryPath: String): Boolean = {
      if ((directoryPathinexistingDirectories)) {
        return true

      }
      if (sys.directoryExists(directoryPath)) {
        (existingDirectories(directoryPath) = true)
        return true

      }
      return false

    }
    def ensureDirectoriesExist(directoryPath: String) = {
      if (((directoryPath.length > getRootLength(directoryPath)) && (!directoryExists(directoryPath)))) {
        val parentDirectory = getDirectoryPath(directoryPath)
        ensureDirectoriesExist(parentDirectory)
        sys.createDirectory(directoryPath)

      }

    }
    var outputFingerprints: Map[OutputFingerprint] = zeroOfMyType
    def writeFileIfUpdated(fileName: String, data: String, writeByteOrderMark: Boolean): Unit = {
      if ((!outputFingerprints)) {
        (outputFingerprints = createMap[OutputFingerprint]())

      }
      val hash = sys.createHash(data)
      val mtimeBefore = sys.getModifiedTime(fileName)
      if ((mtimeBefore && (fileNameinoutputFingerprints))) {
        val fingerprint = outputFingerprints(fileName)
        if ((((fingerprint.byteOrderMark === writeByteOrderMark) && (fingerprint.hash === hash)) && (fingerprint.mtime.getTime() === mtimeBefore.getTime()))) {
          return

        }

      }
      sys.writeFile(fileName, data, writeByteOrderMark)
      val mtimeAfter = sys.getModifiedTime(fileName)
      (outputFingerprints(fileName) = Map("hash" -> hash,
        "byteOrderMark" -> writeByteOrderMark,
        "mtime" -> mtimeAfter))

    }
    def writeFile(fileName: String, data: String, writeByteOrderMark: Boolean, onError: ((String) => Unit)) = {
      try {
        performance.mark("beforeIOWrite")
        ensureDirectoriesExist(getDirectoryPath(normalizePath(fileName)))
        if (((isWatchSet(options) && sys.createHash) && sys.getModifiedTime)) {
          writeFileIfUpdated(fileName, data, writeByteOrderMark)

        }
        else {
          sys.writeFile(fileName, data, writeByteOrderMark)

        }
        performance.mark("afterIOWrite")
        performance.measure("I/O Write", "beforeIOWrite", "afterIOWrite")

      } catch {
        case e: Throwable => {
          if (onError) {
            onError(e.message)

          }

        }
      }

    }
    def getDefaultLibLocation(): String = {
      return getDirectoryPath(normalizePath(sys.getExecutingFilePath()))

    }
    val newLine = getNewLineCharacter(options)
    val realpath = (sys.realpath && (((path: String) => sys.realpath(path))))
    return Map("getSourceFile" -> getSourceFile,
      "getDefaultLibLocation" -> getDefaultLibLocation,
      "getDefaultLibFileName" -> (options => combinePaths(getDefaultLibLocation(), getDefaultLibFileName(options))),
      "writeFile" -> writeFile,
      "getCurrentDirectory" -> memoize((() => sys.getCurrentDirectory())),
      "useCaseSensitiveFileNames" -> (() => sys.useCaseSensitiveFileNames),
      "getCanonicalFileName" -> getCanonicalFileName,
      "getNewLine" -> (() => newLine),
      "fileExists" -> (fileName => sys.fileExists(fileName)),
      "readFile" -> (fileName => sys.readFile(fileName)),
      "trace" -> ((s: String) => sys.write((s + newLine))),
      "directoryExists" -> (directoryName => sys.directoryExists(directoryName)),
      "getEnvironmentVariable" -> (name => (if (sys.getEnvironmentVariable) sys.getEnvironmentVariable(name) else "")),
      "getDirectories" -> ((path: String) => sys.getDirectories(path)),
      "realpath" -> realpath)

  }

  def getPreEmitDiagnostics(program: Program, sourceFile: SourceFile, cancellationToken: CancellationToken): Array[Diagnostic] = {
    var diagnostics = program.getOptionsDiagnostics(cancellationToken).concat(program.getSyntacticDiagnostics(sourceFile, cancellationToken), program.getGlobalDiagnostics(cancellationToken), program.getSemanticDiagnostics(sourceFile, cancellationToken))
    if (program.getCompilerOptions().declaration) {
      (diagnostics = diagnostics.concat(program.getDeclarationDiagnostics(sourceFile, cancellationToken)))

    }
    return sortAndDeduplicateDiagnostics(diagnostics)

  }

  trait FormatDiagnosticsHost {
    def getCurrentDirectory(): String

    def getCanonicalFileName(fileName: String): String

    def getNewLine(): String
  }

  def formatDiagnostics(diagnostics: Array[Diagnostic], host: FormatDiagnosticsHost): String = {
    var output = ""
    (diagnostics).foreach { fresh1 =>
      val diagnostic = zeroOfMyType
        = fresh1 {
        if (diagnostic.file) {
          const fresh2 = getLineAndCharacterOfPosition(diagnostic.file, diagnostic.start)
          val line = fresh2.line
          val character = fresh2.character
          val fileName = diagnostic.file.fileName
          val relativeFileName = convertToRelativePath(fileName, host.getCurrentDirectory(), (fileName => host.getCanonicalFileName(fileName)))
          (output +=s"""${relativeFileName}(${(line + 1)},${(character + 1)}): """)

        }
        val category = DiagnosticCategory(diagnostic.category).toLowerCase()
        (output +=s"""${category} TS${diagnostic.code}: ${flattenDiagnosticMessageText(diagnostic.messageText, host.getNewLine())}${host.getNewLine()}""")

      }
    }
    return output

  }

  def flattenDiagnosticMessageText(messageText: (String | DiagnosticMessageChain), newLine: String): String = {
    if ((typeof(messageText) === "string")) {
      return messageText

    }
    else {
      var diagnosticChain = messageText
      var result = ""
      var indent = 0
      while (diagnosticChain) {
        {
          if (indent) {
            (result += newLine) {
              var i = 0
              while ((i < indent)) {
                {
                  (result += "  ")

                }
                (i += 1)
              }
            }

          }
          (result += diagnosticChain.messageText)
          (indent += 1)
          (diagnosticChain = diagnosticChain.next)

        }
      }
      return result

    }

  }

  def loadWithLocalCache[T](names: Array[String], containingFile: String, loader: ((String, String) => T)): Array[T] = {
    if ((names.length === 0)) {
      return Array()

    }
    val resolutions: Array[T] = Array()
    val cache = createMap[T]()
    (names).foreach { fresh3 =>
      val name = zeroOfMyType
        = fresh3 {
        val result = (if ((nameincache)) cache(name) else (cache(name) = loader(name, containingFile)))
        resolutions.push(result)

      }
    }
    return resolutions

  }

  def createProgram(rootNames: Array[String], options: CompilerOptions, host: CompilerHost, oldProgram: Program): Program = {
    var program: Program = zeroOfMyType
    var files: Array[SourceFile] = Array()
    var commonSourceDirectory: String = zeroOfMyType
    var diagnosticsProducingTypeChecker: TypeChecker = zeroOfMyType
    var noDiagnosticsTypeChecker: TypeChecker = zeroOfMyType
    var classifiableNames: Map[String] = zeroOfMyType
    var resolvedTypeReferenceDirectives = createMap[ResolvedTypeReferenceDirective]()
    var fileProcessingDiagnostics = createDiagnosticCollection()
    val maxNodeModulesJsDepth = (if ((typeof(options.maxNodeModuleJsDepth) === "number")) options.maxNodeModuleJsDepth else 0)
    var currentNodeModulesDepth = 0
    val modulesWithElidedImports = createMap[Boolean]()
    val sourceFilesFoundSearchingNodeModules = createMap[Boolean]()
    performance.mark("beforeProgram")
    (host = (host || createCompilerHost(options)))
    var skipDefaultLib = options.noLib
    val programDiagnostics = createDiagnosticCollection()
    val currentDirectory = host.getCurrentDirectory()
    val supportedExtensions = getSupportedExtensions(options)
    val hasEmitBlockingDiagnostics = createFileMap[Boolean](getCanonicalFileName)
    var resolveModuleNamesWorker: ((Array[String], String) => Array[ResolvedModule]) = zeroOfMyType
    if (host.resolveModuleNames) {
      (resolveModuleNamesWorker = ((moduleNames, containingFile) => host.resolveModuleNames(moduleNames, containingFile)))

    }
    else {
      val loader = ((moduleName: String, containingFile: String) => resolveModuleName(moduleName, containingFile, options, host).resolvedModule)
      (resolveModuleNamesWorker = ((moduleNames, containingFile) => loadWithLocalCache(moduleNames, containingFile, loader)))

    }
    var resolveTypeReferenceDirectiveNamesWorker: ((Array[String], String) => Array[ResolvedTypeReferenceDirective]) = zeroOfMyType
    if (host.resolveTypeReferenceDirectives) {
      (resolveTypeReferenceDirectiveNamesWorker = ((typeDirectiveNames, containingFile) => host.resolveTypeReferenceDirectives(typeDirectiveNames, containingFile)))

    }
    else {
      val loader = ((typesRef: String, containingFile: String) => resolveTypeReferenceDirective(typesRef, containingFile, options, host).resolvedTypeReferenceDirective)
      (resolveTypeReferenceDirectiveNamesWorker = ((typeReferenceDirectiveNames, containingFile) => loadWithLocalCache(typeReferenceDirectiveNames, containingFile, loader)))

    }
    val filesByName = createFileMap[SourceFile]()
    val filesByNameIgnoreCase = (if (host.useCaseSensitiveFileNames()) createFileMap[SourceFile]((fileName => fileName.toLowerCase())) else undefined)
    if ((!tryReuseStructureFromOldProgram())) {
      forEach(rootNames, (name => processRootFile(name, false)))
      val typeReferences: Array[String] = getAutomaticTypeDirectiveNames(options, host)
      if (typeReferences.length) {
        val containingFilename = combinePaths(host.getCurrentDirectory(), "__inferred type names__.ts")
        val resolutions = resolveTypeReferenceDirectiveNamesWorker(typeReferences, containingFilename) {
          var i = 0
          while ((i < typeReferences.length)) {
            {
              processTypeReferenceDirective(typeReferences(i), resolutions(i))

            }
            (i += 1)
          }
        }

      }
      if ((!skipDefaultLib)) {
        if ((!options.lib)) {
          processRootFile(host.getDefaultLibFileName(options), true)

        }
        else {
          val libDirectory = (if (host.getDefaultLibLocation) host.getDefaultLibLocation() else getDirectoryPath(host.getDefaultLibFileName(options)))
          forEach(options.lib, (libFileName => {
            processRootFile(combinePaths(libDirectory, libFileName), true)

          }))

        }

      }

    }
    (oldProgram = undefined)
    (program = Map("getRootFileNames" -> (() => rootNames),
      "getSourceFile" -> getSourceFile,
      "getSourceFileByPath" -> getSourceFileByPath,
      "getSourceFiles" -> (() => files),
      "getCompilerOptions" -> (() => options),
      "getSyntacticDiagnostics" -> getSyntacticDiagnostics,
      "getOptionsDiagnostics" -> getOptionsDiagnostics,
      "getGlobalDiagnostics" -> getGlobalDiagnostics,
      "getSemanticDiagnostics" -> getSemanticDiagnostics,
      "getDeclarationDiagnostics" -> getDeclarationDiagnostics,
      "getTypeChecker" -> getTypeChecker,
      "getClassifiableNames" -> getClassifiableNames,
      "getDiagnosticsProducingTypeChecker" -> getDiagnosticsProducingTypeChecker,
      "getCommonSourceDirectory" -> getCommonSourceDirectory,
      "emit" -> emit,
      "getCurrentDirectory" -> (() => currentDirectory),
      "getNodeCount" -> (() => getDiagnosticsProducingTypeChecker().getNodeCount()),
      "getIdentifierCount" -> (() => getDiagnosticsProducingTypeChecker().getIdentifierCount()),
      "getSymbolCount" -> (() => getDiagnosticsProducingTypeChecker().getSymbolCount()),
      "getTypeCount" -> (() => getDiagnosticsProducingTypeChecker().getTypeCount()),
      "getFileProcessingDiagnostics" -> (() => fileProcessingDiagnostics),
      "getResolvedTypeReferenceDirectives" -> (() => resolvedTypeReferenceDirectives),
      "dropDiagnosticsProducingTypeChecker" -> dropDiagnosticsProducingTypeChecker))
    verifyCompilerOptions()
    performance.mark("afterProgram")
    performance.measure("Program", "beforeProgram", "afterProgram")
    return program
    def getCommonSourceDirectory() = {
      if ((typeof(commonSourceDirectory) === "undefined")) {
        if ((options.rootDir && checkSourceFilesBelongToPath(files, options.rootDir))) {
          (commonSourceDirectory = getNormalizedAbsolutePath(options.rootDir, currentDirectory))

        }
        else {
          (commonSourceDirectory = computeCommonSourceDirectory(files))

        }
        if ((commonSourceDirectory && (commonSourceDirectory((commonSourceDirectory.length - 1)) !== directorySeparator))) {
          (commonSourceDirectory += directorySeparator)

        }

      }
      return commonSourceDirectory

    }
    def getClassifiableNames() = {
      if ((!classifiableNames)) {
        getTypeChecker()
        (classifiableNames = createMap[String]())
        (files).foreach { fresh4 =>
          val sourceFile = zeroOfMyType
            = fresh4 {
            copyProperties(sourceFile.classifiableNames, classifiableNames)

          }
        }

      }
      return classifiableNames

    }
    def tryReuseStructureFromOldProgram(): Boolean = {
      if ((!oldProgram)) {
        return false

      }
      val oldOptions = oldProgram.getCompilerOptions()
      if (changesAffectModuleResolution(oldOptions, options)) {
        return false

      }
      Debug.assert((!oldProgram.structureIsReused))
      val oldRootNames = oldProgram.getRootFileNames()
      if ((!arrayIsEqualTo(oldRootNames, rootNames))) {
        return false

      }
      if ((!arrayIsEqualTo(options.types, oldOptions.types))) {
        return false

      }
      val newSourceFiles: Array[SourceFile] = Array()
      val filePaths: Array[Path] = Array()
      val modifiedSourceFiles: Array[SourceFile] = Array()
      (oldProgram.getSourceFiles()).foreach { fresh5 =>
        val oldSourceFile = zeroOfMyType
          = fresh5 {
          var newSourceFile = (if (host.getSourceFileByPath) host.getSourceFileByPath(oldSourceFile.fileName, oldSourceFile.path, options.target) else host.getSourceFile(oldSourceFile.fileName, options.target))
          if ((!newSourceFile)) {
            return false

          }
          (newSourceFile.path = oldSourceFile.path)
          filePaths.push(newSourceFile.path)
          if ((oldSourceFile !== newSourceFile)) {
            if ((oldSourceFile.hasNoDefaultLib !== newSourceFile.hasNoDefaultLib)) {
              return false

            }
            if ((!arrayIsEqualTo(oldSourceFile.referencedFiles, newSourceFile.referencedFiles, fileReferenceIsEqualTo))) {
              return false

            }
            collectExternalModuleReferences(newSourceFile)
            if ((!arrayIsEqualTo(oldSourceFile.imports, newSourceFile.imports, moduleNameIsEqualTo))) {
              return false

            }
            if ((!arrayIsEqualTo(oldSourceFile.moduleAugmentations, newSourceFile.moduleAugmentations, moduleNameIsEqualTo))) {
              return false

            }
            if ((!arrayIsEqualTo(oldSourceFile.typeReferenceDirectives, newSourceFile.typeReferenceDirectives, fileReferenceIsEqualTo))) {
              return false

            }
            val newSourceFilePath = getNormalizedAbsolutePath(newSourceFile.fileName, currentDirectory)
            if (resolveModuleNamesWorker) {
              val moduleNames = map(concatenate(newSourceFile.imports, newSourceFile.moduleAugmentations), getTextOfLiteral)
              val resolutions = resolveModuleNamesWorker(moduleNames, newSourceFilePath)
              val resolutionsChanged = hasChangesInResolutions(moduleNames, resolutions, oldSourceFile.resolvedModules, moduleResolutionIsEqualTo)
              if (resolutionsChanged) {
                return false

              }

            }
            if (resolveTypeReferenceDirectiveNamesWorker) {
              val typesReferenceDirectives = map(newSourceFile.typeReferenceDirectives, (x => x.fileName))
              val resolutions = resolveTypeReferenceDirectiveNamesWorker(typesReferenceDirectives, newSourceFilePath)
              val resolutionsChanged = hasChangesInResolutions(typesReferenceDirectives, resolutions, oldSourceFile.resolvedTypeReferenceDirectiveNames, typeDirectiveIsEqualTo)
              if (resolutionsChanged) {
                return false

              }

            }
            (newSourceFile.resolvedModules = oldSourceFile.resolvedModules)
            (newSourceFile.resolvedTypeReferenceDirectiveNames = oldSourceFile.resolvedTypeReferenceDirectiveNames)
            modifiedSourceFiles.push(newSourceFile)

          }
          else {
            (newSourceFile = oldSourceFile)

          }
          newSourceFiles.push(newSourceFile)

        }
      } {
        var i = 0
        var len = newSourceFiles.length
        while ((i < len)) {
          {
            filesByName.set(filePaths(i), newSourceFiles(i))

          }
          (i += 1)
        }
      }
      (files = newSourceFiles)
      (fileProcessingDiagnostics = oldProgram.getFileProcessingDiagnostics())
      (modifiedSourceFiles).foreach { fresh6 =>
        val modifiedFile = zeroOfMyType
          = fresh6 {
          fileProcessingDiagnostics.reattachFileDiagnostics(modifiedFile)

        }
      }
      (resolvedTypeReferenceDirectives = oldProgram.getResolvedTypeReferenceDirectives())
      (oldProgram.structureIsReused = true)
      return true

    }
    def getEmitHost(writeFileCallback: WriteFileCallback): EmitHost = {
      return Map("getCanonicalFileName" -> getCanonicalFileName,
        "getCommonSourceDirectory" -> program.getCommonSourceDirectory,
        "getCompilerOptions" -> program.getCompilerOptions,
        "getCurrentDirectory" -> (() => currentDirectory),
        "getNewLine" -> (() => host.getNewLine()),
        "getSourceFile" -> program.getSourceFile,
        "getSourceFileByPath" -> program.getSourceFileByPath,
        "getSourceFiles" -> program.getSourceFiles,
        "isSourceFileFromExternalLibrary" -> ((file: SourceFile) => (!(!sourceFilesFoundSearchingNodeModules(file.path)))),
        "writeFile" -> (writeFileCallback || (((fileName, data, writeByteOrderMark, onError, sourceFiles) => host.writeFile(fileName, data, writeByteOrderMark, onError, sourceFiles)))),
        "isEmitBlocked" -> isEmitBlocked)

    }
    def getDiagnosticsProducingTypeChecker() = {
      return (diagnosticsProducingTypeChecker || ((diagnosticsProducingTypeChecker = createTypeChecker(program, true))))

    }
    def dropDiagnosticsProducingTypeChecker() = {
      (diagnosticsProducingTypeChecker = undefined)

    }
    def getTypeChecker() = {
      return (noDiagnosticsTypeChecker || ((noDiagnosticsTypeChecker = createTypeChecker(program, false))))

    }
    def emit(sourceFile: SourceFile, writeFileCallback: WriteFileCallback, cancellationToken: CancellationToken, emitOnlyDtsFiles: Boolean): EmitResult = {
      return runWithCancellationToken((() => emitWorker(program, sourceFile, writeFileCallback, cancellationToken, emitOnlyDtsFiles)))

    }
    def isEmitBlocked(emitFileName: String): Boolean = {
      return hasEmitBlockingDiagnostics.contains(toPath(emitFileName, currentDirectory, getCanonicalFileName))

    }
    def emitWorker(program: Program, sourceFile: SourceFile, writeFileCallback: WriteFileCallback, cancellationToken: CancellationToken, emitOnlyDtsFiles: Boolean): EmitResult = {
      var declarationDiagnostics: Array[Diagnostic] = Array()
      if (options.noEmit) {
        return Map("diagnostics" -> declarationDiagnostics,
          "sourceMaps" -> undefined,
          "emittedFiles" -> undefined,
          "emitSkipped" -> true)

      }
      if (options.noEmitOnError) {
        val diagnostics = program.getOptionsDiagnostics(cancellationToken).concat(program.getSyntacticDiagnostics(sourceFile, cancellationToken), program.getGlobalDiagnostics(cancellationToken), program.getSemanticDiagnostics(sourceFile, cancellationToken))
        if (((diagnostics.length === 0) && program.getCompilerOptions().declaration)) {
          (declarationDiagnostics = program.getDeclarationDiagnostics(undefined, cancellationToken))

        }
        if (((diagnostics.length > 0) || (declarationDiagnostics.length > 0))) {
          return Map("diagnostics" -> concatenate(diagnostics, declarationDiagnostics),
            "sourceMaps" -> undefined,
            "emittedFiles" -> undefined,
            "emitSkipped" -> true)

        }

      }
      val emitResolver = getDiagnosticsProducingTypeChecker().getEmitResolver((if (((options.outFile || options.out))) undefined else sourceFile))
      performance.mark("beforeEmit")
      val emitResult = emitFiles(emitResolver, getEmitHost(writeFileCallback), sourceFile, emitOnlyDtsFiles)
      performance.mark("afterEmit")
      performance.measure("Emit", "beforeEmit", "afterEmit")
      return emitResult

    }
    def getSourceFile(fileName: String): SourceFile = {
      return getSourceFileByPath(toPath(fileName, currentDirectory, getCanonicalFileName))

    }
    def getSourceFileByPath(path: Path): SourceFile = {
      return filesByName.get(path)

    }
    def getDiagnosticsHelper(sourceFile: SourceFile, getDiagnostics: ((SourceFile, CancellationToken) => Array[Diagnostic]), cancellationToken: CancellationToken): Array[Diagnostic] = {
      if (sourceFile) {
        return getDiagnostics(sourceFile, cancellationToken)

      }
      val allDiagnostics: Array[Diagnostic] = Array()
      forEach(program.getSourceFiles(), (sourceFile => {
        if (cancellationToken) {
          cancellationToken.throwIfCancellationRequested()

        }
        addRange(allDiagnostics, getDiagnostics(sourceFile, cancellationToken))

      }))
      return sortAndDeduplicateDiagnostics(allDiagnostics)

    }
    def getSyntacticDiagnostics(sourceFile: SourceFile, cancellationToken: CancellationToken): Array[Diagnostic] = {
      return getDiagnosticsHelper(sourceFile, getSyntacticDiagnosticsForFile, cancellationToken)

    }
    def getSemanticDiagnostics(sourceFile: SourceFile, cancellationToken: CancellationToken): Array[Diagnostic] = {
      return getDiagnosticsHelper(sourceFile, getSemanticDiagnosticsForFile, cancellationToken)

    }
    def getDeclarationDiagnostics(sourceFile: SourceFile, cancellationToken: CancellationToken): Array[Diagnostic] = {
      val options = program.getCompilerOptions()
      if ((((!sourceFile) || options.out) || options.outFile)) {
        return getDeclarationDiagnosticsWorker(sourceFile, cancellationToken)

      }
      else {
        return getDiagnosticsHelper(sourceFile, getDeclarationDiagnosticsForFile, cancellationToken)

      }

    }
    def getSyntacticDiagnosticsForFile(sourceFile: SourceFile): Array[Diagnostic] = {
      return sourceFile.parseDiagnostics

    }
    def runWithCancellationToken[T](func: (() => T)): T = {
      try {
        return func()

      } catch {
        case e: Throwable => {
          if ((einstanceofOperationCanceledException)) {
            (noDiagnosticsTypeChecker = undefined)
            (diagnosticsProducingTypeChecker = undefined)

          }
          throw e
        }
      }

    }
    def getSemanticDiagnosticsForFile(sourceFile: SourceFile, cancellationToken: CancellationToken): Array[Diagnostic] = {
      return runWithCancellationToken((() => {
        val typeChecker = getDiagnosticsProducingTypeChecker()
        Debug.assert((!(!sourceFile.bindDiagnostics)))
        val bindDiagnostics = sourceFile.bindDiagnostics
        val checkDiagnostics = (if (isSourceFileJavaScript(sourceFile)) getJavaScriptSemanticDiagnosticsForFile(sourceFile) else typeChecker.getDiagnostics(sourceFile, cancellationToken))
        val fileProcessingDiagnosticsInFile = fileProcessingDiagnostics.getDiagnostics(sourceFile.fileName)
        val programDiagnosticsInFile = programDiagnostics.getDiagnostics(sourceFile.fileName)
        return bindDiagnostics.concat(checkDiagnostics, fileProcessingDiagnosticsInFile, programDiagnosticsInFile)

      }))

    }
    def getJavaScriptSemanticDiagnosticsForFile(sourceFile: SourceFile): Array[Diagnostic] = {
      return runWithCancellationToken((() => {
        val diagnostics: Array[Diagnostic] = Array()
        walk(sourceFile)
        return diagnostics
        def walk(node: Node): Boolean = {
          if ((!node)) {
            return false

          }
          node.kind match {
            case SyntaxKind.ImportEqualsDeclaration =>
              diagnostics.push(createDiagnosticForNode(node, Diagnostics.import_can_only_be_used_in_a_ts_file))
              return true
            case SyntaxKind.ExportAssignment =>
              if ((node.asInstanceOf[ExportAssignment]).isExportEquals) {
                diagnostics.push(createDiagnosticForNode(node, Diagnostics.export_can_only_be_used_in_a_ts_file))
                return true

              }
            case SyntaxKind.ClassDeclaration =>
              var classDeclaration = node.asInstanceOf[ClassDeclaration]
              if ((checkModifiers(classDeclaration.modifiers) || checkTypeParameters(classDeclaration.typeParameters))) {
                return true

              }
            case SyntaxKind.HeritageClause =>
              var heritageClause = node.asInstanceOf[HeritageClause]
              if ((heritageClause.token === SyntaxKind.ImplementsKeyword)) {
                diagnostics.push(createDiagnosticForNode(node, Diagnostics.implements_clauses_can_only_be_used_in_a_ts_file))
                return true

              }
            case SyntaxKind.InterfaceDeclaration =>
              diagnostics.push(createDiagnosticForNode(node, Diagnostics.interface_declarations_can_only_be_used_in_a_ts_file))
              return true
            case SyntaxKind.ModuleDeclaration =>
              diagnostics.push(createDiagnosticForNode(node, Diagnostics.module_declarations_can_only_be_used_in_a_ts_file))
              return true
            case SyntaxKind.TypeAliasDeclaration =>
              diagnostics.push(createDiagnosticForNode(node, Diagnostics.type_aliases_can_only_be_used_in_a_ts_file))
              return true
            case SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionExpression | SyntaxKind.FunctionDeclaration | SyntaxKind.ArrowFunction | SyntaxKind.FunctionDeclaration =>
              val functionDeclaration = node.asInstanceOf[FunctionLikeDeclaration]
              if (((checkModifiers(functionDeclaration.modifiers) || checkTypeParameters(functionDeclaration.typeParameters)) || checkTypeAnnotation(functionDeclaration.`type`))) {
                return true

              }
            case SyntaxKind.VariableStatement =>
              val variableStatement = node.asInstanceOf[VariableStatement]
              if (checkModifiers(variableStatement.modifiers)) {
                return true

              }
            case SyntaxKind.VariableDeclaration =>
              val variableDeclaration = node.asInstanceOf[VariableDeclaration]
              if (checkTypeAnnotation(variableDeclaration.`type`)) {
                return true

              }
            case SyntaxKind.CallExpression | SyntaxKind.NewExpression =>
              val expression = node.asInstanceOf[CallExpression]
              if ((expression.typeArguments && (expression.typeArguments.length > 0))) {
                val start = expression.typeArguments.pos
                diagnostics.push(createFileDiagnostic(sourceFile, start, (expression.typeArguments.end - start), Diagnostics.type_arguments_can_only_be_used_in_a_ts_file))
                return true

              }
            case SyntaxKind.Parameter =>
              val parameter = node.asInstanceOf[ParameterDeclaration]
              if (parameter.modifiers) {
                val start = parameter.modifiers.pos
                diagnostics.push(createFileDiagnostic(sourceFile, start, (parameter.modifiers.end - start), Diagnostics.parameter_modifiers_can_only_be_used_in_a_ts_file))
                return true

              }
              if (parameter.questionToken) {
                diagnostics.push(createDiagnosticForNode(parameter.questionToken, Diagnostics._0_can_only_be_used_in_a_ts_file, "?"))
                return true

              }
              if (parameter.`type`) {
                diagnostics.push(createDiagnosticForNode(parameter.`type`, Diagnostics.types_can_only_be_used_in_a_ts_file))
                return true

              }
            case SyntaxKind.PropertyDeclaration =>
              val propertyDeclaration = node.asInstanceOf[PropertyDeclaration]
              if (propertyDeclaration.modifiers) {
                (propertyDeclaration.modifiers).foreach { fresh7 =>
                  val modifier = zeroOfMyType
                    = fresh7 {
                    if ((modifier.kind !== SyntaxKind.StaticKeyword)) {
                      diagnostics.push(createDiagnosticForNode(modifier, Diagnostics._0_can_only_be_used_in_a_ts_file, tokenToString(modifier.kind)))
                      return true

                    }

                  }
                }

              }
              if (checkTypeAnnotation((node.asInstanceOf[PropertyDeclaration]).`type`)) {
                return true

              }
            case SyntaxKind.EnumDeclaration =>
              diagnostics.push(createDiagnosticForNode(node, Diagnostics.enum_declarations_can_only_be_used_in_a_ts_file))
              return true
            case SyntaxKind.TypeAssertionExpression =>
              var typeAssertionExpression = node.asInstanceOf[TypeAssertion]
              diagnostics.push(createDiagnosticForNode(typeAssertionExpression.`type`, Diagnostics.type_assertion_expressions_can_only_be_used_in_a_ts_file))
              return true
            case SyntaxKind.Decorator =>
              if ((!options.experimentalDecorators)) {
                diagnostics.push(createDiagnosticForNode(node, Diagnostics.Experimental_support_for_decorators_is_a_feature_that_is_subject_to_change_in_a_future_release_Set_the_experimentalDecorators_option_to_remove_this_warning))

              }
              return true
            case _ =>
          }
          return forEachChild(node, walk)

        }
        def checkTypeParameters(typeParameters: NodeArray[TypeParameterDeclaration]): Boolean = {
          if (typeParameters) {
            val start = typeParameters.pos
            diagnostics.push(createFileDiagnostic(sourceFile, start, (typeParameters.end - start), Diagnostics.type_parameter_declarations_can_only_be_used_in_a_ts_file))
            return true

          }
          return false

        }
        def checkTypeAnnotation(`type`: TypeNode): Boolean = {
          if (`type`) {
            diagnostics.push(createDiagnosticForNode(`type`, Diagnostics.types_can_only_be_used_in_a_ts_file))
            return true

          }
          return false

        }
        def checkModifiers(modifiers: NodeArray[Modifier]): Boolean = {
          if (modifiers) {
            (modifiers).foreach { fresh8 =>
              val modifier = zeroOfMyType
                = fresh8 {
                modifier.kind match {
                  case SyntaxKind.PublicKeyword | SyntaxKind.PrivateKeyword | SyntaxKind.ProtectedKeyword | SyntaxKind.ReadonlyKeyword | SyntaxKind.DeclareKeyword =>
                    diagnostics.push(createDiagnosticForNode(modifier, Diagnostics._0_can_only_be_used_in_a_ts_file, tokenToString(modifier.kind)))
                    return true
                  case _ =>
                }

              }
            }

          }
          return false

        }

      }))

    }
    def getDeclarationDiagnosticsWorker(sourceFile: SourceFile, cancellationToken: CancellationToken): Array[Diagnostic] = {
      return runWithCancellationToken((() => {
        val resolver = getDiagnosticsProducingTypeChecker().getEmitResolver(sourceFile, cancellationToken)
        return ts.getDeclarationDiagnostics(getEmitHost(noop), resolver, sourceFile)

      }))

    }
    def getDeclarationDiagnosticsForFile(sourceFile: SourceFile, cancellationToken: CancellationToken): Array[Diagnostic] = {
      return (if (isDeclarationFile(sourceFile)) Array() else getDeclarationDiagnosticsWorker(sourceFile, cancellationToken))

    }
    def getOptionsDiagnostics(): Array[Diagnostic] = {
      val allDiagnostics: Array[Diagnostic] = Array()
      addRange(allDiagnostics, fileProcessingDiagnostics.getGlobalDiagnostics())
      addRange(allDiagnostics, programDiagnostics.getGlobalDiagnostics())
      return sortAndDeduplicateDiagnostics(allDiagnostics)

    }
    def getGlobalDiagnostics(): Array[Diagnostic] = {
      val allDiagnostics: Array[Diagnostic] = Array()
      addRange(allDiagnostics, getDiagnosticsProducingTypeChecker().getGlobalDiagnostics())
      return sortAndDeduplicateDiagnostics(allDiagnostics)

    }
    def hasExtension(fileName: String): Boolean = {
      return (getBaseFileName(fileName).indexOf(".") >= 0)

    }
    def processRootFile(fileName: String, isDefaultLib: Boolean) = {
      processSourceFile(normalizePath(fileName), isDefaultLib)

    }
    def fileReferenceIsEqualTo(a: FileReference, b: FileReference): Boolean = {
      return (a.fileName === b.fileName)

    }
    def moduleNameIsEqualTo(a: LiteralExpression, b: LiteralExpression): Boolean = {
      return (a.text === b.text)

    }
    def getTextOfLiteral(literal: LiteralExpression): String = {
      return literal.text

    }
    def collectExternalModuleReferences(file: SourceFile): Unit = {
      if (file.imports) {
        return

      }
      val isJavaScriptFile = isSourceFileJavaScript(file)
      val isExternalModuleFile = isExternalModule(file)
      var imports: Array[LiteralExpression] = zeroOfMyType
      var moduleAugmentations: Array[LiteralExpression] = zeroOfMyType
      if (((options.importHelpers && ((options.isolatedModules || isExternalModuleFile))) && (!file.isDeclarationFile))) {
        val externalHelpersModuleReference = createNode(SyntaxKind.StringLiteral).asInstanceOf[StringLiteral]
        (externalHelpersModuleReference.text = externalHelpersModuleNameText)
        (externalHelpersModuleReference.parent = file)
        (imports = Array(externalHelpersModuleReference))

      }
      (file.statements).foreach { fresh9 =>
        val node = zeroOfMyType
          = fresh9 {
          collectModuleReferences(node, false)
          if (isJavaScriptFile) {
            collectRequireCalls(node)

          }

        }
      }
      (file.imports = (imports || emptyArray))
      (file.moduleAugmentations = (moduleAugmentations || emptyArray))
      return
      def collectModuleReferences(node: Node, inAmbientModule: Boolean): Unit = {
        node.kind match {
          case SyntaxKind.ImportDeclaration | SyntaxKind.ImportEqualsDeclaration | SyntaxKind.ExportDeclaration =>
            var moduleNameExpr = getExternalModuleName(node)
            if (((!moduleNameExpr) || (moduleNameExpr.kind !== SyntaxKind.StringLiteral))) {
              break()

            }
            if ((!(moduleNameExpr.asInstanceOf[LiteralExpression]).text)) {
              break()

            }
            if (((!inAmbientModule) || (!isExternalModuleNameRelative((moduleNameExpr.asInstanceOf[LiteralExpression]).text)))) {
              ((imports || ((imports = Array())))).push(moduleNameExpr.asInstanceOf[LiteralExpression])

            }
          case SyntaxKind.ModuleDeclaration =>
            if ((isAmbientModule(node.asInstanceOf[ModuleDeclaration]) && (((inAmbientModule || hasModifier(node, ModifierFlags.Ambient)) || isDeclarationFile(file))))) {
              val moduleName = (node.asInstanceOf[ModuleDeclaration]).name.asInstanceOf[LiteralExpression]
              if ((isExternalModuleFile || ((inAmbientModule && (!isExternalModuleNameRelative(moduleName.text)))))) {
                ((moduleAugmentations || ((moduleAugmentations = Array())))).push(moduleName)

              }
              else if ((!inAmbientModule)) {
                val body = (node.asInstanceOf[ModuleDeclaration]).body.asInstanceOf[ModuleBlock]
                if (body) {
                  (body.statements).foreach { fresh10 =>
                    val statement = zeroOfMyType
                      = fresh10 {
                      collectModuleReferences(statement, true)

                    }
                  }

                }

              }

            }
          case _ =>
        }

      }
      def collectRequireCalls(node: Node): Unit = {
        if (isRequireCall(node, true)) {
          ((imports || ((imports = Array())))).push((node.asInstanceOf[CallExpression]).arguments(0).asInstanceOf[StringLiteral])

        }
        else {
          forEachChild(node, collectRequireCalls)

        }

      }

    }
    def processSourceFile(fileName: String, isDefaultLib: Boolean, refFile: SourceFile, refPos: Int, refEnd: Int) = {
      var diagnosticArgument: Array[String] = zeroOfMyType
      var diagnostic: DiagnosticMessage = zeroOfMyType
      if (hasExtension(fileName)) {
        if (((!options.allowNonTsExtensions) && (!forEach(supportedExtensions, (extension => fileExtensionIs(host.getCanonicalFileName(fileName), extension)))))) {
          (diagnostic = Diagnostics.File_0_has_unsupported_extension_The_only_supported_extensions_are_1)
          (diagnosticArgument = Array(fileName, (("'" + supportedExtensions.join("', '")) + "'")))

        }
        else if ((!findSourceFile(fileName, toPath(fileName, currentDirectory, getCanonicalFileName), isDefaultLib, refFile, refPos, refEnd))) {
          (diagnostic = Diagnostics.File_0_not_found)
          (diagnosticArgument = Array(fileName))

        }
        else if ((refFile && (host.getCanonicalFileName(fileName) === host.getCanonicalFileName(refFile.fileName)))) {
          (diagnostic = Diagnostics.A_file_cannot_have_a_reference_to_itself)
          (diagnosticArgument = Array(fileName))

        }

      }
      else {
        val nonTsFile: SourceFile = (options.allowNonTsExtensions && findSourceFile(fileName, toPath(fileName, currentDirectory, getCanonicalFileName), isDefaultLib, refFile, refPos, refEnd))
        if ((!nonTsFile)) {
          if (options.allowNonTsExtensions) {
            (diagnostic = Diagnostics.File_0_not_found)
            (diagnosticArgument = Array(fileName))

          }
          else if ((!forEach(supportedExtensions, (extension => findSourceFile((fileName + extension), toPath((fileName + extension), currentDirectory, getCanonicalFileName), isDefaultLib, refFile, refPos, refEnd))))) {
            (diagnostic = Diagnostics.File_0_not_found)
            (fileName += ".ts")
            (diagnosticArgument = Array(fileName))

          }

        }

      }
      if (diagnostic) {
        if ((((refFile !== undefined) && (refEnd !== undefined)) && (refPos !== undefined))) {
          fileProcessingDiagnostics.add(createFileDiagnostic(refFile, refPos, (refEnd - refPos), diagnostic, diagnosticArgument: _*))

        }
        else {
          fileProcessingDiagnostics.add(createCompilerDiagnostic(diagnostic, diagnosticArgument: _*))

        }

      }

    }
    def reportFileNamesDifferOnlyInCasingError(fileName: String, existingFileName: String, refFile: SourceFile, refPos: Int, refEnd: Int): Unit = {
      if ((((refFile !== undefined) && (refPos !== undefined)) && (refEnd !== undefined))) {
        fileProcessingDiagnostics.add(createFileDiagnostic(refFile, refPos, (refEnd - refPos), Diagnostics.File_name_0_differs_from_already_included_file_name_1_only_in_casing, fileName, existingFileName))

      }
      else {
        fileProcessingDiagnostics.add(createCompilerDiagnostic(Diagnostics.File_name_0_differs_from_already_included_file_name_1_only_in_casing, fileName, existingFileName))

      }

    }
    def findSourceFile(fileName: String, path: Path, isDefaultLib: Boolean, refFile: SourceFile, refPos: Int, refEnd: Int): SourceFile = {
      if (filesByName.contains(path)) {
        val file = filesByName.get(path)
        if (((file && options.forceConsistentCasingInFileNames) && (getNormalizedAbsolutePath(file.fileName, currentDirectory) !== getNormalizedAbsolutePath(fileName, currentDirectory)))) {
          reportFileNamesDifferOnlyInCasingError(fileName, file.fileName, refFile, refPos, refEnd)

        }
        if (((file && sourceFilesFoundSearchingNodeModules(file.path)) && (currentNodeModulesDepth == 0))) {
          (sourceFilesFoundSearchingNodeModules(file.path) = false)
          if ((!options.noResolve)) {
            processReferencedFiles(file, isDefaultLib)
            processTypeReferenceDirectives(file)

          }
          (modulesWithElidedImports(file.path) = false)
          processImportedModules(file)

        }
        else if ((file && modulesWithElidedImports(file.path))) {
          if ((currentNodeModulesDepth < maxNodeModulesJsDepth)) {
            (modulesWithElidedImports(file.path) = false)
            processImportedModules(file)

          }

        }
        return file

      }
      val file = host.getSourceFile(fileName, options.target, (hostErrorMessage => {
        if ((((refFile !== undefined) && (refPos !== undefined)) && (refEnd !== undefined))) {
          fileProcessingDiagnostics.add(createFileDiagnostic(refFile, refPos, (refEnd - refPos), Diagnostics.Cannot_read_file_0_Colon_1, fileName, hostErrorMessage))

        }
        else {
          fileProcessingDiagnostics.add(createCompilerDiagnostic(Diagnostics.Cannot_read_file_0_Colon_1, fileName, hostErrorMessage))

        }

      }))
      filesByName.set(path, file)
      if (file) {
        (sourceFilesFoundSearchingNodeModules(path) = ((currentNodeModulesDepth > 0)))
        (file.path = path)
        if (host.useCaseSensitiveFileNames()) {
          val existingFile = filesByNameIgnoreCase.get(path)
          if (existingFile) {
            reportFileNamesDifferOnlyInCasingError(fileName, existingFile.fileName, refFile, refPos, refEnd)

          }
          else {
            filesByNameIgnoreCase.set(path, file)

          }

        }
        (skipDefaultLib = (skipDefaultLib || file.hasNoDefaultLib))
        if ((!options.noResolve)) {
          processReferencedFiles(file, isDefaultLib)
          processTypeReferenceDirectives(file)

        }
        processImportedModules(file)
        if (isDefaultLib) {
          files.unshift(file)

        }
        else {
          files.push(file)

        }

      }
      return file

    }
    def processReferencedFiles(file: SourceFile, isDefaultLib: Boolean) = {
      forEach(file.referencedFiles, (ref => {
        val referencedFileName = resolveTripleslashReference(ref.fileName, file.fileName)
        processSourceFile(referencedFileName, isDefaultLib, file, ref.pos, ref.end)

      }))

    }
    def processTypeReferenceDirectives(file: SourceFile) = {
      val typeDirectives = map(file.typeReferenceDirectives, (ref => ref.fileName.toLocaleLowerCase()))
      val resolutions = resolveTypeReferenceDirectiveNamesWorker(typeDirectives, file.fileName) {
        var i = 0
        while ((i < typeDirectives.length)) {
          {
            val ref = file.typeReferenceDirectives(i)
            val resolvedTypeReferenceDirective = resolutions(i)
            val fileName = ref.fileName.toLocaleLowerCase()
            setResolvedTypeReferenceDirective(file, fileName, resolvedTypeReferenceDirective)
            processTypeReferenceDirective(fileName, resolvedTypeReferenceDirective, file, ref.pos, ref.end)

          }
          (i += 1)
        }
      }

    }
    def processTypeReferenceDirective(typeReferenceDirective: String, resolvedTypeReferenceDirective: ResolvedTypeReferenceDirective, refFile: SourceFile, refPos: Int, refEnd: Int): Unit = {
      val previousResolution = resolvedTypeReferenceDirectives(typeReferenceDirective)
      if ((previousResolution && previousResolution.primary)) {
        return

      }
      var saveResolution = true
      if (resolvedTypeReferenceDirective) {
        if (resolvedTypeReferenceDirective.primary) {
          processSourceFile(resolvedTypeReferenceDirective.resolvedFileName, false, refFile, refPos, refEnd)

        }
        else {
          if (previousResolution) {
            val otherFileText = host.readFile(resolvedTypeReferenceDirective.resolvedFileName)
            if ((otherFileText !== getSourceFile(previousResolution.resolvedFileName).text)) {
              fileProcessingDiagnostics.add(createDiagnostic(refFile, refPos, refEnd, Diagnostics.Conflicting_definitions_for_0_found_at_1_and_2_Consider_installing_a_specific_version_of_this_library_to_resolve_the_conflict, typeReferenceDirective, resolvedTypeReferenceDirective.resolvedFileName, previousResolution.resolvedFileName))

            }
            (saveResolution = false)

          }
          else {
            processSourceFile(resolvedTypeReferenceDirective.resolvedFileName, false, refFile, refPos, refEnd)

          }

        }

      }
      else {
        fileProcessingDiagnostics.add(createDiagnostic(refFile, refPos, refEnd, Diagnostics.Cannot_find_type_definition_file_for_0, typeReferenceDirective))

      }
      if (saveResolution) {
        (resolvedTypeReferenceDirectives(typeReferenceDirective) = resolvedTypeReferenceDirective)

      }

    }
    def createDiagnostic(refFile: SourceFile, refPos: Int, refEnd: Int, message: DiagnosticMessage, args: Array[Any]): Diagnostic = {
      if ((((refFile === undefined) || (refPos === undefined)) || (refEnd === undefined))) {
        return createCompilerDiagnostic(message, args: _*)

      }
      else {
        return createFileDiagnostic(refFile, refPos, (refEnd - refPos), message, args: _*)

      }

    }
    def getCanonicalFileName(fileName: String): String = {
      return host.getCanonicalFileName(fileName)

    }
    def processImportedModules(file: SourceFile) = {
      collectExternalModuleReferences(file)
      if ((file.imports.length || file.moduleAugmentations.length)) {
        (file.resolvedModules = createMap[ResolvedModule]())
        val moduleNames = map(concatenate(file.imports, file.moduleAugmentations), getTextOfLiteral)
        val resolutions = resolveModuleNamesWorker(moduleNames, getNormalizedAbsolutePath(file.fileName, currentDirectory)) {
          var i = 0
          while ((i < moduleNames.length)) {
            {
              val resolution = resolutions(i)
              setResolvedModule(file, moduleNames(i), resolution)
              val isFromNodeModulesSearch = (resolution && resolution.isExternalLibraryImport)
              val isJsFileFromNodeModules = (isFromNodeModulesSearch && hasJavaScriptFileExtension(resolution.resolvedFileName))
              if (isFromNodeModulesSearch) {
                (currentNodeModulesDepth += 1)

              }
              val elideImport = (isJsFileFromNodeModules && (currentNodeModulesDepth > maxNodeModulesJsDepth))
              val shouldAddFile = (((resolution && (!options.noResolve)) && (i < file.imports.length)) && (!elideImport))
              if (elideImport) {
                (modulesWithElidedImports(file.path) = true)

              }
              else if (shouldAddFile) {
                findSourceFile(resolution.resolvedFileName, toPath(resolution.resolvedFileName, currentDirectory, getCanonicalFileName), false, file, skipTrivia(file.text, file.imports(i).pos), file.imports(i).end)

              }
              if (isFromNodeModulesSearch) {
                (currentNodeModulesDepth -= 1)

              }

            }
            (i += 1)
          }
        }

      }
      else {
        (file.resolvedModules = undefined)

      }
      return

    }
    def computeCommonSourceDirectory(sourceFiles: Array[SourceFile]): String = {
      val fileNames: Array[String] = Array()
      (sourceFiles).foreach { fresh11 =>
        val file = zeroOfMyType
          = fresh11 {
          if ((!file.isDeclarationFile)) {
            fileNames.push(file.fileName)

          }

        }
      }
      return computeCommonSourceDirectoryOfFilenames(fileNames, currentDirectory, getCanonicalFileName)

    }
    def checkSourceFilesBelongToPath(sourceFiles: Array[SourceFile], rootDirectory: String): Boolean = {
      var allFilesBelongToPath = true
      if (sourceFiles) {
        val absoluteRootDirectoryPath = host.getCanonicalFileName(getNormalizedAbsolutePath(rootDirectory, currentDirectory))
        (sourceFiles).foreach { fresh12 =>
          val sourceFile = zeroOfMyType
            = fresh12 {
            if ((!isDeclarationFile(sourceFile))) {
              val absoluteSourceFilePath = host.getCanonicalFileName(getNormalizedAbsolutePath(sourceFile.fileName, currentDirectory))
              if ((absoluteSourceFilePath.indexOf(absoluteRootDirectoryPath) !== 0)) {
                programDiagnostics.add(createCompilerDiagnostic(Diagnostics.File_0_is_not_under_rootDir_1_rootDir_is_expected_to_contain_all_source_files, sourceFile.fileName, options.rootDir))
                (allFilesBelongToPath = false)

              }

            }

          }
        }

      }
      return allFilesBelongToPath

    }
    def verifyCompilerOptions() = {
      if (options.isolatedModules) {
        if (options.declaration) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "declaration", "isolatedModules"))

        }
        if (options.noEmitOnError) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "noEmitOnError", "isolatedModules"))

        }
        if (options.out) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "out", "isolatedModules"))

        }
        if (options.outFile) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "outFile", "isolatedModules"))

        }

      }
      if (options.inlineSourceMap) {
        if (options.sourceMap) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "sourceMap", "inlineSourceMap"))

        }
        if (options.mapRoot) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "mapRoot", "inlineSourceMap"))

        }

      }
      if ((options.paths && (options.baseUrl === undefined))) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_paths_cannot_be_used_without_specifying_baseUrl_option))

      }
      if (options.paths) {
        (options.paths).keys.foreach { fresh13 =>
          val key = zeroOfMyType
            = fresh13 {
            if ((!hasProperty(options.paths, key))) {
              continue

            }
            if ((!hasZeroOrOneAsteriskCharacter(key))) {
              programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Pattern_0_can_have_at_most_one_Asterisk_character, key))

            }
            if (isArray(options.paths(key))) {
              if ((options.paths(key).length === 0)) {
                programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Substitutions_for_pattern_0_shouldn_t_be_an_empty_array, key))

              }
              (options.paths(key)).foreach { fresh14 =>
                val subst = zeroOfMyType
                  = fresh14 {
                  val typeOfSubst = typeof(subst)
                  if ((typeOfSubst === "string")) {
                    if ((!hasZeroOrOneAsteriskCharacter(subst))) {
                      programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Substitution_0_in_pattern_1_in_can_have_at_most_one_Asterisk_character, subst, key))

                    }

                  }
                  else {
                    programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Substitution_0_for_pattern_1_has_incorrect_type_expected_string_got_2, subst, key, typeOfSubst))

                  }

                }
              }

            }
            else {
              programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Substitutions_for_pattern_0_should_be_an_array, key))

            }

          }
        }

      }
      if (((!options.sourceMap) && (!options.inlineSourceMap))) {
        if (options.inlineSources) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_can_only_be_used_when_either_option_inlineSourceMap_or_option_sourceMap_is_provided, "inlineSources"))

        }
        if (options.sourceRoot) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_can_only_be_used_when_either_option_inlineSourceMap_or_option_sourceMap_is_provided, "sourceRoot"))

        }

      }
      if ((options.out && options.outFile)) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "out", "outFile"))

      }
      if ((options.mapRoot && (!options.sourceMap))) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_without_specifying_option_1, "mapRoot", "sourceMap"))

      }
      if (options.declarationDir) {
        if ((!options.declaration)) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_without_specifying_option_1, "declarationDir", "declaration"))

        }
        if ((options.out || options.outFile)) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "declarationDir", (if (options.out) "out" else "outFile")))

        }

      }
      if ((options.lib && options.noLib)) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "lib", "noLib"))

      }
      if ((options.noImplicitUseStrict && options.alwaysStrict)) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "noImplicitUseStrict", "alwaysStrict"))

      }
      val languageVersion = (options.target || ScriptTarget.ES3)
      val outFile = (options.outFile || options.out)
      val firstNonAmbientExternalModuleSourceFile = forEach(files, (f => (if ((isExternalModule(f) && (!isDeclarationFile(f)))) f else undefined)))
      if (options.isolatedModules) {
        if (((options.module === ModuleKind.None) && (languageVersion < ScriptTarget.ES2015))) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_isolatedModules_can_only_be_used_when_either_option_module_is_provided_or_option_target_is_ES2015_or_higher))

        }
        val firstNonExternalModuleSourceFile = forEach(files, (f => (if (((!isExternalModule(f)) && (!isDeclarationFile(f)))) f else undefined)))
        if (firstNonExternalModuleSourceFile) {
          val span = getErrorSpanForNode(firstNonExternalModuleSourceFile, firstNonExternalModuleSourceFile)
          programDiagnostics.add(createFileDiagnostic(firstNonExternalModuleSourceFile, span.start, span.length, Diagnostics.Cannot_compile_namespaces_when_the_isolatedModules_flag_is_provided))

        }

      }
      else if (((firstNonAmbientExternalModuleSourceFile && (languageVersion < ScriptTarget.ES2015)) && (options.module === ModuleKind.None))) {
        val span = getErrorSpanForNode(firstNonAmbientExternalModuleSourceFile, firstNonAmbientExternalModuleSourceFile.externalModuleIndicator)
        programDiagnostics.add(createFileDiagnostic(firstNonAmbientExternalModuleSourceFile, span.start, span.length, Diagnostics.Cannot_use_imports_exports_or_module_augmentations_when_module_is_none))

      }
      if (outFile) {
        if ((options.module && (!(((options.module === ModuleKind.AMD) || (options.module === ModuleKind.System)))))) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Only_amd_and_system_modules_are_supported_alongside_0, (if (options.out) "out" else "outFile")))

        }
        else if (((options.module === undefined) && firstNonAmbientExternalModuleSourceFile)) {
          val span = getErrorSpanForNode(firstNonAmbientExternalModuleSourceFile, firstNonAmbientExternalModuleSourceFile.externalModuleIndicator)
          programDiagnostics.add(createFileDiagnostic(firstNonAmbientExternalModuleSourceFile, span.start, span.length, Diagnostics.Cannot_compile_modules_using_option_0_unless_the_module_flag_is_amd_or_system, (if (options.out) "out" else "outFile")))

        }

      }
      if (((options.outDir || options.sourceRoot) || options.mapRoot)) {
        val dir = getCommonSourceDirectory()
        if (((options.outDir && (dir === "")) && forEach(files, (file => (getRootLength(file.fileName) > 1))))) {
          programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Cannot_find_the_common_subdirectory_path_for_the_input_files))

        }

      }
      if ((((!options.noEmit) && options.allowJs) && options.declaration)) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_with_option_1, "allowJs", "declaration"))

      }
      if ((options.emitDecoratorMetadata && (!options.experimentalDecorators))) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Option_0_cannot_be_specified_without_specifying_option_1, "emitDecoratorMetadata", "experimentalDecorators"))

      }
      if ((options.reactNamespace && (!isIdentifierText(options.reactNamespace, languageVersion)))) {
        programDiagnostics.add(createCompilerDiagnostic(Diagnostics.Invalid_value_for_reactNamespace_0_is_not_a_valid_identifier, options.reactNamespace))

      }
      if (((!options.noEmit) && (!options.suppressOutputPathCheck))) {
        val emitHost = getEmitHost()
        val emitFilesSeen = createFileMap[Boolean]((if ((!host.useCaseSensitiveFileNames())) (key => key.toLocaleLowerCase()) else undefined))
        forEachExpectedEmitFile(emitHost, (emitFileNames => {
          verifyEmitFilePath(emitFileNames.jsFilePath, emitFilesSeen)
          verifyEmitFilePath(emitFileNames.declarationFilePath, emitFilesSeen)

        }))

      }
      def verifyEmitFilePath(emitFileName: String, emitFilesSeen: FileMap[Boolean]) = {
        if (emitFileName) {
          val emitFilePath = toPath(emitFileName, currentDirectory, getCanonicalFileName)
          if (filesByName.contains(emitFilePath)) {
            if ((((options.noEmitOverwritenFiles && (!options.out)) && (!options.outDir)) && (!options.outFile))) {
              blockEmittingOfFile(emitFileName)

            }
            else {
              var chain: DiagnosticMessageChain = zeroOfMyType
              if ((!options.configFilePath)) {
                (chain = chainDiagnosticMessages(undefined, Diagnostics.Adding_a_tsconfig_json_file_will_help_organize_projects_that_contain_both_TypeScript_and_JavaScript_files_Learn_more_at_https_Colon_Slash_Slashaka_ms_Slashtsconfig))

              }
              (chain = chainDiagnosticMessages(chain, Diagnostics.Cannot_write_file_0_because_it_would_overwrite_input_file, emitFileName))
              blockEmittingOfFile(emitFileName, createCompilerDiagnosticFromMessageChain(chain))

            }

          }
          if (emitFilesSeen.contains(emitFilePath)) {
            blockEmittingOfFile(emitFileName, createCompilerDiagnostic(Diagnostics.Cannot_write_file_0_because_it_would_be_overwritten_by_multiple_input_files, emitFileName))

          }
          else {
            emitFilesSeen.set(emitFilePath, true)

          }

        }

      }

    }
    def blockEmittingOfFile(emitFileName: String, diag: Diagnostic) = {
      hasEmitBlockingDiagnostics.set(toPath(emitFileName, currentDirectory, getCanonicalFileName), true)
      if (diag) {
        programDiagnostics.add(diag)

      }

    }

  }
}
