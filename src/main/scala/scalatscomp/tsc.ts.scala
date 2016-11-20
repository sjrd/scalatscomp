package scalatscomp
object TSC {
  trait SourceFile {
    var fileWatcher: FileWatcher
  }
  trait Statistic {
    var name: String
    var value: String
  }
  val defaultFormatDiagnosticsHost: FormatDiagnosticsHost = Map(
    "getCurrentDirectory" -> (() => sys.getCurrentDirectory()),
    "getNewLine" -> (() => sys.newLine),
    "getCanonicalFileName" -> createGetCanonicalFileName(
      sys.useCaseSensitiveFileNames))
  var reportDiagnosticWorker = reportDiagnosticSimply
  def reportDiagnostic(diagnostic: Diagnostic, host: FormatDiagnosticsHost) = {
    reportDiagnosticWorker(diagnostic, (host || defaultFormatDiagnosticsHost))

  }
  def reportDiagnostics(diagnostics: Array[Diagnostic],
                        host: FormatDiagnosticsHost): Unit = {
    (diagnostics).foreach { fresh1 =>
      val diagnostic = zeroOfMyType = fresh1 {
        reportDiagnostic(diagnostic, host)

      }
    }

  }
  def reportEmittedFiles(files: Array[String]): Unit = {
    if (((!files) || (files.length == 0))) {
      return

    }
    val currentDir = sys.getCurrentDirectory()
    (files).foreach { fresh2 =>
      val file = zeroOfMyType = fresh2 {
        val filepath = getNormalizedAbsolutePath(file, currentDir)
        sys.write(s"""TSFILE: ${filepath}${sys.newLine}""")

      }
    }

  }
  def validateLocaleAndSetLanguage(locale: String,
                                   errors: Array[Diagnostic]): Boolean = {
    val matchResult = java.util.regex.Pattern
      .compile(raw"""^([a-z]+)([_\-]([a-z]+))?$$""")
      .exec(locale.toLowerCase())
    if ((!matchResult)) {
      errors.push(
        createCompilerDiagnostic(
          Diagnostics.Locale_must_be_of_the_form_language_or_language_territory_For_example_0_or_1,
          "en",
          "ja-jp"))
      return false

    }
    val language = matchResult(1)
    val territory = matchResult(3)
    if ((!trySetLanguageAndTerritory(language, territory, errors))) {
      trySetLanguageAndTerritory(language, undefined, errors)

    }
    return true

  }
  def trySetLanguageAndTerritory(language: String,
                                 territory: String,
                                 errors: Array[Diagnostic]): Boolean = {
    val compilerFilePath = normalizePath(sys.getExecutingFilePath())
    val containingDirectoryPath = getDirectoryPath(compilerFilePath)
    var filePath = combinePaths(containingDirectoryPath, language)
    if (territory) {
      (filePath = ((filePath + "-") + territory))

    }
    (filePath = sys.resolvePath(
      combinePaths(filePath, "diagnosticMessages.generated.json")))
    if ((!sys.fileExists(filePath))) {
      return false

    }
    var fileContents = ""
    try {
      (fileContents = sys.readFile(filePath))

    } catch {
      case e: Throwable => {
        errors.push(
          createCompilerDiagnostic(
            Diagnostics.Unable_to_open_file_0,
            filePath))
        return false

      }
    }
    try {
      (ts.localizedDiagnosticMessages = JSON.parse(fileContents))

    } catch {
      case e: Throwable => {
        errors.push(
          createCompilerDiagnostic(
            Diagnostics.Corrupted_locale_file_0,
            filePath))
        return false

      }
    }
    return true

  }
  def countLines(program: Program): Int = {
    var count = 0
    forEach(program.getSourceFiles(), (file => {
                                         (count += getLineStarts(file).length)

                                       }))
    return count

  }
  def getDiagnosticText(_message: DiagnosticMessage,
                        _args: Array[Any]): String = {
    val diagnostic = createCompilerDiagnostic.apply(undefined, arguments)
    return diagnostic.messageText.asInstanceOf[String]

  }
  def reportDiagnosticSimply(diagnostic: Diagnostic,
                             host: FormatDiagnosticsHost): Unit = {
    sys.write(ts.formatDiagnostics(Array(diagnostic), host))

  }
  val redForegroundEscapeSequence = "\u001B[91m"
  val yellowForegroundEscapeSequence = "\u001B[93m"
  val blueForegroundEscapeSequence = "\u001B[93m"
  val gutterStyleSequence = "\u001B[100;30m"
  val gutterSeparator = " "
  val resetEscapeSequence = "\u001B[0m"
  val ellipsis = "..."
  val categoryFormatMap = createMap[String](
    Map(
      DiagnosticCategory.Warning -> yellowForegroundEscapeSequence,
      DiagnosticCategory.Error -> redForegroundEscapeSequence,
      DiagnosticCategory.Message -> blueForegroundEscapeSequence))
  def formatAndReset(text: String, formatStyle: String) = {
    return ((formatStyle + text) + resetEscapeSequence)

  }
  def reportDiagnosticWithColorAndContext(
      diagnostic: Diagnostic,
      host: FormatDiagnosticsHost): Unit = {
    var output = ""
    if (diagnostic.file) {
      const fresh3 = diagnostic
      val start = fresh3.start
      val length = fresh3.length
      val file = fresh3.file
      const fresh4 = getLineAndCharacterOfPosition(file, start)
      val firstLine = fresh4.firstLine
      val firstLineChar = fresh4.firstLineChar
      const fresh5 = getLineAndCharacterOfPosition(file, (start + length))
      val lastLine = fresh5.lastLine
      val lastLineChar = fresh5.lastLineChar
      val lastLineInFile =
        getLineAndCharacterOfPosition(file, file.text.length).line
      val relativeFileName =
        (if (host)
           convertToRelativePath(
             file.fileName,
             host.getCurrentDirectory(),
             (fileName => host.getCanonicalFileName(fileName)))
         else file.fileName)
      val hasMoreThanFiveLines = (((lastLine - firstLine)) >= 4)
      var gutterWidth = (((lastLine + 1) + "")).length
      if (hasMoreThanFiveLines) {
        (gutterWidth = Math.max(ellipsis.length, gutterWidth))

      }
      (output += sys.newLine) {
        var i = firstLine
        while ((i <= lastLine)) {
          {
            if (((hasMoreThanFiveLines && ((firstLine + 1) < i)) && (i < (lastLine - 1)))) {
              (output += ((formatAndReset(
                padLeft(ellipsis, gutterWidth),
                gutterStyleSequence) + gutterSeparator) + sys.newLine))
              (i = (lastLine - 1))

            }
            val lineStart = getPositionOfLineAndCharacter(file, i, 0)
            val lineEnd =
              (if ((i < lastLineInFile))
                 getPositionOfLineAndCharacter(file, (i + 1), 0)
               else file.text.length)
            var lineContent = file.text.slice(lineStart, lineEnd)
            (lineContent = lineContent.replace(
              java.util.regex.Pattern.compile(raw"""\s+$$""", "g"),
              ""))
            (lineContent = lineContent.replace("\t", " "))
            (output += (formatAndReset(
              padLeft(((i + 1) + ""), gutterWidth),
              gutterStyleSequence) + gutterSeparator))
            (output += (lineContent + sys.newLine))
            (output += (formatAndReset(
              padLeft("", gutterWidth),
              gutterStyleSequence) + gutterSeparator))
            (output += redForegroundEscapeSequence)
            if ((i === firstLine)) {
              val lastCharForLine =
                (if ((i === lastLine)) lastLineChar else undefined)
              (output += lineContent
                .slice(0, firstLineChar)
                .replace(
                  java.util.regex.Pattern.compile(raw"""\S""", "g"),
                  " "))
              (output += lineContent
                .slice(firstLineChar, lastCharForLine)
                .replace(
                  java.util.regex.Pattern.compile(raw""".""", "g"),
                  "~"))

            } else if ((i === lastLine)) {
              (output += lineContent
                .slice(0, lastLineChar)
                .replace(
                  java.util.regex.Pattern.compile(raw""".""", "g"),
                  "~"))

            } else {
              (output += lineContent.replace(
                java.util.regex.Pattern.compile(raw""".""", "g"),
                "~"))

            }
            (output += resetEscapeSequence)
            (output += sys.newLine)

          }
          (i += 1)
        }
      }
      (output += sys.newLine)
      (output += s"""${relativeFileName}(${(firstLine + 1)},${(firstLineChar + 1)}): """)

    }
    val categoryColor = categoryFormatMap(diagnostic.category)
    val category = DiagnosticCategory(diagnostic.category).toLowerCase()
    (output += s"""${formatAndReset(category, categoryColor)} TS${diagnostic.code}: ${flattenDiagnosticMessageText(
      diagnostic.messageText,
      sys.newLine)}""")
    (output += (sys.newLine + sys.newLine))
    sys.write(output)

  }
  def reportWatchDiagnostic(diagnostic: Diagnostic) = {
    var output = (new Date().toLocaleTimeString() + " - ")
    if (diagnostic.file) {
      val loc =
        getLineAndCharacterOfPosition(diagnostic.file, diagnostic.start)
      (output += s"""${diagnostic.file.fileName}(${(loc.line + 1)},${(loc.character + 1)}): """)

    }
    (output += s"""${flattenDiagnosticMessageText(
      diagnostic.messageText,
      sys.newLine)}${sys.newLine}""")
    sys.write(output)

  }
  def padLeft(s: String, length: Int) = {
    while ((s.length < length)) {
      {
        (s = (" " + s))

      }
    }
    return s

  }
  def padRight(s: String, length: Int) = {
    while ((s.length < length)) {
      {
        (s = (s + " "))

      }
    }
    return s

  }
  def isJSONSupported() = {
    return ((typeof(JSON) === "object") && (typeof(JSON.parse) === "function"))

  }
  def executeCommandLine(args: Array[String]): Unit = {
    val commandLine = parseCommandLine(args)
    var configFileName: String = zeroOfMyType
    var cachedConfigFileText: String = zeroOfMyType
    var configFileWatcher: FileWatcher = zeroOfMyType
    var directoryWatcher: FileWatcher = zeroOfMyType
    var cachedProgram: Program = zeroOfMyType
    var rootFileNames: Array[String] = zeroOfMyType
    var compilerOptions: CompilerOptions = zeroOfMyType
    var compilerHost: CompilerHost = zeroOfMyType
    var hostGetSourceFile: compilerHost.getSourceFile.type = zeroOfMyType
    var timerHandleForRecompilation: Any = zeroOfMyType
    var timerHandleForDirectoryChanges: Any = zeroOfMyType
    var cachedExistingFiles: Map[Boolean] = zeroOfMyType
    var hostFileExists: compilerHost.fileExists.type = zeroOfMyType
    if (commandLine.options.locale) {
      if ((!isJSONSupported())) {
        reportDiagnostic(
          createCompilerDiagnostic(
            Diagnostics.The_current_host_does_not_support_the_0_option,
            "--locale"),
          undefined)
        return sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

      }
      validateLocaleAndSetLanguage(
        commandLine.options.locale,
        commandLine.errors)

    }
    if ((commandLine.errors.length > 0)) {
      reportDiagnostics(commandLine.errors, compilerHost)
      return sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

    }
    if (commandLine.options.init) {
      writeConfigFile(commandLine.options, commandLine.fileNames)
      return sys.exit(ExitStatus.Success)

    }
    if (commandLine.options.version) {
      printVersion()
      return sys.exit(ExitStatus.Success)

    }
    if (commandLine.options.help) {
      printVersion()
      printHelp()
      return sys.exit(ExitStatus.Success)

    }
    if (commandLine.options.project) {
      if ((!isJSONSupported())) {
        reportDiagnostic(
          createCompilerDiagnostic(
            Diagnostics.The_current_host_does_not_support_the_0_option,
            "--project"),
          undefined)
        return sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

      }
      if ((commandLine.fileNames.length !== 0)) {
        reportDiagnostic(
          createCompilerDiagnostic(
            Diagnostics.Option_project_cannot_be_mixed_with_source_files_on_a_command_line),
          undefined)
        return sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

      }
      val fileOrDirectory = normalizePath(commandLine.options.project)
      if (((!fileOrDirectory) || sys.directoryExists(fileOrDirectory))) {
        (configFileName = combinePaths(fileOrDirectory, "tsconfig.json"))
        if ((!sys.fileExists(configFileName))) {
          reportDiagnostic(
            createCompilerDiagnostic(
              Diagnostics.Cannot_find_a_tsconfig_json_file_at_the_specified_directory_Colon_0,
              commandLine.options.project),
            undefined)
          return sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

        }

      } else {
        (configFileName = fileOrDirectory)
        if ((!sys.fileExists(configFileName))) {
          reportDiagnostic(
            createCompilerDiagnostic(
              Diagnostics.The_specified_path_does_not_exist_Colon_0,
              commandLine.options.project),
            undefined)
          return sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

        }

      }

    } else if (((commandLine.fileNames.length === 0) && isJSONSupported())) {
      val searchPath = normalizePath(sys.getCurrentDirectory())
      (configFileName = findConfigFile(searchPath, sys.fileExists))

    }
    if (((commandLine.fileNames.length === 0) && (!configFileName))) {
      printVersion()
      printHelp()
      return sys.exit(ExitStatus.Success)

    }
    if (isWatchSet(commandLine.options)) {
      if ((!sys.watchFile)) {
        reportDiagnostic(
          createCompilerDiagnostic(
            Diagnostics.The_current_host_does_not_support_the_0_option,
            "--watch"),
          undefined)
        return sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

      }
      if (configFileName) {
        (configFileWatcher = sys.watchFile(configFileName, configFileChanged))

      }
      if ((sys.watchDirectory && configFileName)) {
        val directory = ts.getDirectoryPath(configFileName)
        (directoryWatcher = sys.watchDirectory(
          (if ((directory == "")) "." else directory),
          watchedDirectoryChanged,
          true))

      }

    }
    performCompilation()
    def parseConfigFile(): ParsedCommandLine = {
      if ((!cachedConfigFileText)) {
        try {
          (cachedConfigFileText = sys.readFile(configFileName))

        } catch {
          case e: Throwable => {
            val error = createCompilerDiagnostic(
              Diagnostics.Cannot_read_file_0_Colon_1,
              configFileName,
              e.message)
            reportWatchDiagnostic(error)
            sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)
            return

          }
        }

      }
      if ((!cachedConfigFileText)) {
        val error = createCompilerDiagnostic(
          Diagnostics.File_0_not_found,
          configFileName)
        reportDiagnostics(Array(error), undefined)
        sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)
        return

      }
      val result =
        parseConfigFileTextToJson(configFileName, cachedConfigFileText)
      val configObject = result.config
      if ((!configObject)) {
        reportDiagnostics(Array(result.error), undefined)
        sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)
        return

      }
      val cwd = sys.getCurrentDirectory()
      val configParseResult = parseJsonConfigFileContent(
        configObject,
        sys,
        getNormalizedAbsolutePath(getDirectoryPath(configFileName), cwd),
        commandLine.options,
        getNormalizedAbsolutePath(configFileName, cwd))
      if ((configParseResult.errors.length > 0)) {
        reportDiagnostics(configParseResult.errors, undefined)
        sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)
        return

      }
      if (isWatchSet(configParseResult.options)) {
        if ((!sys.watchFile)) {
          reportDiagnostic(
            createCompilerDiagnostic(
              Diagnostics.The_current_host_does_not_support_the_0_option,
              "--watch"),
            undefined)
          sys.exit(ExitStatus.DiagnosticsPresent_OutputsSkipped)

        }
        if ((((!directoryWatcher) && sys.watchDirectory) && configFileName)) {
          val directory = ts.getDirectoryPath(configFileName)
          (directoryWatcher = sys.watchDirectory(
            (if ((directory == "")) "." else directory),
            watchedDirectoryChanged,
            true))

        };
      }
      return configParseResult

    }
    def performCompilation() = {
      if ((!cachedProgram)) {
        if (configFileName) {
          val configParseResult = parseConfigFile()
          (rootFileNames = configParseResult.fileNames)
          (compilerOptions = configParseResult.options)

        } else {
          (rootFileNames = commandLine.fileNames)
          (compilerOptions = commandLine.options)

        }
        (compilerHost = createCompilerHost(compilerOptions))
        (hostGetSourceFile = compilerHost.getSourceFile)
        (compilerHost.getSourceFile = getSourceFile)
        (hostFileExists = compilerHost.fileExists)
        (compilerHost.fileExists = cachedFileExists)

      }
      if (compilerOptions.pretty) {
        (reportDiagnosticWorker = reportDiagnosticWithColorAndContext)

      }
      (cachedExistingFiles = createMap[Boolean]())
      val compileResult = compile(rootFileNames, compilerOptions, compilerHost)
      if ((!isWatchSet(compilerOptions))) {
        return sys.exit(compileResult.exitStatus)

      }
      setCachedProgram(compileResult.program)
      reportWatchDiagnostic(
        createCompilerDiagnostic(
          Diagnostics.Compilation_complete_Watching_for_file_changes))

    }
    def cachedFileExists(fileName: String): Boolean = {
      return (if ((fileNameincachedExistingFiles))
                cachedExistingFiles(fileName)
              else (cachedExistingFiles(fileName) = hostFileExists(fileName)))

    }
    def getSourceFile(fileName: String,
                      languageVersion: ScriptTarget,
                      onError: ((String) => Unit)) = {
      if (cachedProgram) {
        val sourceFile = cachedProgram.getSourceFile(fileName)
        if ((sourceFile && sourceFile.fileWatcher)) {
          return sourceFile

        }

      }
      val sourceFile = hostGetSourceFile(fileName, languageVersion, onError)
      if (((sourceFile && isWatchSet(compilerOptions)) && sys.watchFile)) {
        (sourceFile.fileWatcher = sys.watchFile(
          sourceFile.fileName,
          ((_fileName: String,
            removed: Boolean) => sourceFileChanged(sourceFile, removed))))

      }
      return sourceFile

    }
    def setCachedProgram(program: Program) = {
      if (cachedProgram) {
        val newSourceFiles =
          (if (program) program.getSourceFiles() else undefined)
        forEach(
          cachedProgram.getSourceFiles(),
          (sourceFile => {
             if ((!((newSourceFiles && contains(newSourceFiles, sourceFile))))) {
               if (sourceFile.fileWatcher) {
                 sourceFile.fileWatcher.close()
                 (sourceFile.fileWatcher = undefined)

               }

             }

           }))

      }
      (cachedProgram = program)

    }
    def sourceFileChanged(sourceFile: SourceFile, removed: Boolean) = {
      sourceFile.fileWatcher.close()
      (sourceFile.fileWatcher = undefined)
      if (removed) {
        unorderedRemoveItem(rootFileNames, sourceFile.fileName)

      }
      startTimerForRecompilation()

    }
    def configFileChanged() = {
      setCachedProgram(undefined)
      (cachedConfigFileText = undefined)
      startTimerForRecompilation()

    }
    def watchedDirectoryChanged(fileName: String) = {
      if ((fileName && (!ts.isSupportedSourceFileName(
            fileName,
            compilerOptions)))) {
        return

      }
      startTimerForHandlingDirectoryChanges()

    }
    def startTimerForHandlingDirectoryChanges() = {
      if (((!sys.setTimeout) || (!sys.clearTimeout))) {
        return

      }
      if (timerHandleForDirectoryChanges) {
        sys.clearTimeout(timerHandleForDirectoryChanges)

      }
      (timerHandleForDirectoryChanges =
        sys.setTimeout(directoryChangeHandler, 250))

    }
    def directoryChangeHandler() = {
      val parsedCommandLine = parseConfigFile()
      val newFileNames =
        ts.map(parsedCommandLine.fileNames, compilerHost.getCanonicalFileName)
      val canonicalRootFileNames =
        ts.map(rootFileNames, compilerHost.getCanonicalFileName)
      if ((!arrayIsEqualTo(
            (newFileNames && newFileNames.sort()),
            (canonicalRootFileNames && canonicalRootFileNames.sort())))) {
        setCachedProgram(undefined)
        startTimerForRecompilation()

      }

    }
    def startTimerForRecompilation() = {
      if (((!sys.setTimeout) || (!sys.clearTimeout))) {
        return

      }
      if (timerHandleForRecompilation) {
        sys.clearTimeout(timerHandleForRecompilation)

      }
      (timerHandleForRecompilation = sys.setTimeout(recompile, 250))

    }
    def recompile() = {
      (timerHandleForRecompilation = undefined)
      reportWatchDiagnostic(
        createCompilerDiagnostic(
          Diagnostics.File_change_detected_Starting_incremental_compilation))
      performCompilation()

    }

  }
  def compile(fileNames: Array[String],
              compilerOptions: CompilerOptions,
              compilerHost: CompilerHost) = {
    val hasDiagnostics = (compilerOptions.diagnostics || compilerOptions.extendedDiagnostics)
    var statistics: Array[Statistic] = zeroOfMyType
    if (hasDiagnostics) {
      performance.enable()
      (statistics = Array())

    }
    val program = createProgram(fileNames, compilerOptions, compilerHost)
    val exitStatus = compileProgram()
    if (compilerOptions.listFiles) {
      forEach(program.getSourceFiles(), (file => {
                                           sys.write(
                                             (file.fileName + sys.newLine))

                                         }))

    }
    if (hasDiagnostics) {
      val memoryUsed = (if (sys.getMemoryUsage) sys.getMemoryUsage() else (-1))
      reportCountStatistic("Files", program.getSourceFiles().length)
      reportCountStatistic("Lines", countLines(program))
      reportCountStatistic("Nodes", program.getNodeCount())
      reportCountStatistic("Identifiers", program.getIdentifierCount())
      reportCountStatistic("Symbols", program.getSymbolCount())
      reportCountStatistic("Types", program.getTypeCount())
      if ((memoryUsed >= 0)) {
        reportStatisticalValue(
          "Memory used",
          (Math.round((memoryUsed / 1000)) + "K"))

      }
      val programTime = performance.getDuration("Program")
      val bindTime = performance.getDuration("Bind")
      val checkTime = performance.getDuration("Check")
      val emitTime = performance.getDuration("Emit")
      if (compilerOptions.extendedDiagnostics) {
        performance.forEachMeasure(
          ((name, duration) =>
             reportTimeStatistic(s"""${name} time""", duration)))

      } else {
        reportTimeStatistic("I/O read", performance.getDuration("I/O Read"))
        reportTimeStatistic("I/O write", performance.getDuration("I/O Write"))
        reportTimeStatistic("Parse time", programTime)
        reportTimeStatistic("Bind time", bindTime)
        reportTimeStatistic("Check time", checkTime)
        reportTimeStatistic("Emit time", emitTime)

      }
      reportTimeStatistic(
        "Total time",
        (((programTime + bindTime) + checkTime) + emitTime))
      reportStatistics()
      performance.disable()

    }
    return Map("program" -> program, "exitStatus" -> exitStatus)
    def compileProgram(): ExitStatus = {
      var diagnostics: Array[Diagnostic] = zeroOfMyType
      (diagnostics = program.getSyntacticDiagnostics())
      if ((diagnostics.length === 0)) {
        (diagnostics = program
          .getOptionsDiagnostics()
          .concat(program.getGlobalDiagnostics()))
        if ((diagnostics.length === 0)) {
          (diagnostics = program.getSemanticDiagnostics())

        }

      }
      val emitOutput = program.emit()
      (diagnostics = diagnostics.concat(emitOutput.diagnostics))
      reportDiagnostics(
        sortAndDeduplicateDiagnostics(diagnostics),
        compilerHost)
      reportEmittedFiles(emitOutput.emittedFiles)
      if ((emitOutput.emitSkipped && (diagnostics.length > 0))) {
        return ExitStatus.DiagnosticsPresent_OutputsSkipped

      } else if ((diagnostics.length > 0)) {
        return ExitStatus.DiagnosticsPresent_OutputsGenerated

      }
      return ExitStatus.Success

    }
    def reportStatistics() = {
      var nameSize = 0
      var valueSize = 0
      (statistics).foreach { fresh6 =>
        const fresh7 = zeroOfMyType
        val name = fresh7.name
        val value = fresh7.value = fresh6 {
          if ((name.length > nameSize)) {
            (nameSize = name.length)

          }
          if ((value.length > valueSize)) {
            (valueSize = value.length)

          }

        }
      }
      (statistics).foreach { fresh8 =>
        const fresh9 = zeroOfMyType
        val name = fresh9.name
        val value = fresh9.value = fresh8 {
          sys.write(
            ((padRight((name + ":"), (nameSize + 2)) + padLeft(
              value.`toString`(),
              valueSize)) + sys.newLine))

        }
      }

    }
    def reportStatisticalValue(name: String, value: String) = {
      statistics.push(Map("name" -> name, "value" -> value))

    }
    def reportCountStatistic(name: String, count: Int) = {
      reportStatisticalValue(name, ("" + count))

    }
    def reportTimeStatistic(name: String, time: Int) = {
      reportStatisticalValue(name, (((time / 1000)).toFixed(2) + "s"))

    }

  }
  def printVersion() = {
    sys.write(
      (getDiagnosticText(Diagnostics.Version_0, ts.version) + sys.newLine))

  }
  def printHelp() = {
    val output: Array[String] = Array()
    val syntaxLength = getDiagnosticText(Diagnostics.Syntax_Colon_0, "").length
    val examplesLength =
      getDiagnosticText(Diagnostics.Examples_Colon_0, "").length
    var marginLength = Math.max(syntaxLength, examplesLength)
    var syntax = makePadding((marginLength - syntaxLength))
    (syntax += (((("tsc [" + getDiagnosticText(Diagnostics.options)) + "] [") + getDiagnosticText(
      Diagnostics.file)) + " ...]"))
    output.push(getDiagnosticText(Diagnostics.Syntax_Colon_0, syntax))
    output.push((sys.newLine + sys.newLine))
    val padding = makePadding(marginLength)
    output.push(
      (getDiagnosticText(
        Diagnostics.Examples_Colon_0,
        (makePadding((marginLength - examplesLength)) + "tsc hello.ts")) + sys.newLine))
    output.push(((padding + "tsc --outFile file.js file.ts") + sys.newLine))
    output.push(((padding + "tsc @args.txt") + sys.newLine))
    output.push(sys.newLine)
    output.push((getDiagnosticText(Diagnostics.Options_Colon) + sys.newLine))
    val optsList = filter(optionDeclarations.slice(), (v => (!v.experimental)))
    optsList.sort(
      ((a, b) =>
         compareValues[String](a.name.toLowerCase(), b.name.toLowerCase())))
    (marginLength = 0)
    val usageColumn: Array[String] = Array()
    val descriptionColumn: Array[String] = Array()
    val optionsDescriptionMap = createMap[Array[String]]() {
      var i = 0
      while ((i < optsList.length)) {
        {
          val option = optsList(i)
          if ((!option.description)) {
            continue

          }
          var usageText = " "
          if (option.shortName) {
            (usageText += ("-" + option.shortName))
            (usageText += getParamType(option))
            (usageText += ", ")

          }
          (usageText += ("--" + option.name))
          (usageText += getParamType(option))
          usageColumn.push(usageText)
          var description: String = zeroOfMyType
          if ((option.name === "lib")) {
            (description = getDiagnosticText(option.description))
            val options: Array[String] = Array()
            val element =
              (option.asInstanceOf[CommandLineOptionOfListType]).element
            val typeMap = element.`type`.asInstanceOf[Map[(Int | String)]]
            (typeMap).keys.foreach { fresh10 =>
              val key = zeroOfMyType = fresh10 {
                options.push(s"""'${key}'""")

              }
            }
            (optionsDescriptionMap(description) = options)

          } else {
            (description = getDiagnosticText(option.description))

          }
          descriptionColumn.push(description)
          (marginLength = Math.max(usageText.length, marginLength))

        }
        (i += 1)
      }
    }
    val usageText = ((" @<" + getDiagnosticText(Diagnostics.file)) + ">")
    usageColumn.push(usageText)
    descriptionColumn.push(
      getDiagnosticText(
        Diagnostics.Insert_command_line_options_and_files_from_a_file))
    (marginLength = Math.max(usageText.length, marginLength)) {
      var i = 0
      while ((i < usageColumn.length)) {
        {
          val usage = usageColumn(i)
          val description = descriptionColumn(i)
          val kindsList = optionsDescriptionMap(description)
          output.push((((usage + makePadding(
            ((marginLength - usage.length) + 2))) + description) + sys.newLine))
          if (kindsList) {
            output.push(makePadding((marginLength + 4)))
            (kindsList).foreach { fresh11 =>
              val kind = zeroOfMyType = fresh11 {
                output.push((kind + " "))

              }
            }
            output.push(sys.newLine)

          }

        }
        (i += 1)
      }
    }
    (output).foreach { fresh12 =>
      val line = zeroOfMyType = fresh12 {
        sys.write(line)

      }
    }
    return
    def getParamType(option: CommandLineOption) = {
      if ((option.paramType !== undefined)) {
        return (" " + getDiagnosticText(option.paramType))

      }
      return ""

    }
    def makePadding(paddingLength: Int): String = {
      return Array((paddingLength + 1)).join(" ")

    }

  }
  def writeConfigFile(options: CompilerOptions, fileNames: Array[String]) = {
    val currentDirectory = sys.getCurrentDirectory()
    val file = normalizePath(combinePaths(currentDirectory, "tsconfig.json"))
    if (sys.fileExists(file)) {
      reportDiagnostic(
        createCompilerDiagnostic(
          Diagnostics.A_tsconfig_json_file_is_already_defined_at_Colon_0,
          file),
        undefined)

    } else {
      sys.writeFile(
        file,
        JSON.stringify(generateTSConfig(options, fileNames), undefined, 4))
      reportDiagnostic(
        createCompilerDiagnostic(
          Diagnostics.Successfully_created_a_tsconfig_json_file),
        undefined)

    }
    return

  }
  if ((ts.sys.tryEnableSourceMapsForHost && java.util.regex.Pattern
        .compile(raw"""^development$$""", "i")
        .test(ts.sys.getEnvironmentVariable("NODE_ENV")))) {
    ts.sys.tryEnableSourceMapsForHost()

  }
  ts.executeCommandLine(ts.sys.args)
}
