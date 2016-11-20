package scalatscomp
object ModuleNameResolver {
  def trace(host: ModuleResolutionHost,
            message: DiagnosticMessage,
            args: Array[Any]): Unit
  def trace(host: ModuleResolutionHost): Unit = {
    host.trace(formatMessage.apply(undefined, arguments))

  }
  def isTraceEnabled(compilerOptions: CompilerOptions,
                     host: ModuleResolutionHost): Boolean = {
    return (compilerOptions.traceResolution && (host.trace !== undefined))

  }
  def createResolvedModule(resolvedFileName: String,
                           isExternalLibraryImport: Boolean,
                           failedLookupLocations: Array[String])
    : ResolvedModuleWithFailedLookupLocations = {
    return Map(
      "resolvedModule" -> (if (resolvedFileName)
                             Map(
                               "resolvedFileName" -> resolvedFileName,
                               "isExternalLibraryImport" -> isExternalLibraryImport)
                           else undefined),
      "failedLookupLocations" -> failedLookupLocations)

  }
  def moduleHasNonRelativeName(moduleName: String): Boolean = {
    return (!((isRootedDiskPath(moduleName) || isExternalModuleNameRelative(
      moduleName))))

  }
  trait ModuleResolutionState {
    var host: ModuleResolutionHost
    var compilerOptions: CompilerOptions
    var traceEnabled: Boolean
    var skipTsx: Boolean
  }
  def tryReadTypesSection(packageJsonPath: String,
                          baseDirectory: String,
                          state: ModuleResolutionState): String = {
    val jsonContent = readJson(packageJsonPath, state.host)
    def tryReadFromField(fieldName: String) = {
      if (hasProperty(jsonContent, fieldName)) {
        val typesFile = (jsonContent.asInstanceOf[Any])(fieldName)
        if ((typeof(typesFile) === "string")) {
          val typesFilePath = normalizePath(
            combinePaths(baseDirectory, typesFile))
          if (state.traceEnabled) {
            trace(
              state.host,
              Diagnostics.package_json_has_0_field_1_that_references_2,
              fieldName,
              typesFile,
              typesFilePath)

          }
          return typesFilePath

        } else {
          if (state.traceEnabled) {
            trace(
              state.host,
              Diagnostics.Expected_type_of_0_field_in_package_json_to_be_string_got_1,
              fieldName,
              typeof(typesFile))

          }

        }

      }

    }
    val typesFilePath = (tryReadFromField("typings") || tryReadFromField(
        "types"))
    if (typesFilePath) {
      return typesFilePath

    }
    if (((state.compilerOptions.allowJs && jsonContent.main) && (typeof(
          jsonContent.main) === "string"))) {
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.No_types_specified_in_package_json_but_allowJs_is_set_so_returning_main_value_of_0,
          jsonContent.main)

      }
      val mainFilePath = normalizePath(
        combinePaths(baseDirectory, jsonContent.main))
      return mainFilePath

    }
    return undefined

  }
  def readJson(path: String, host: ModuleResolutionHost): {
    var typings: String
    var types: String
    var main: String
  } = {
    try {
      val jsonText = host.readFile(path)
      return (if (jsonText) JSON.parse(jsonText)
              else
                Map(
                  ))

    } catch {
      case e: Throwable => {
        return Map(
          )

      }
    }

  }
  val typeReferenceExtensions = Array(".d.ts")
  def getEffectiveTypeRoots(options: CompilerOptions, host: {
    var directoryExists: ((String) => Boolean)
    var getCurrentDirectory: (() => String)
  }): (Array[String] | undefined) = {
    if (options.typeRoots) {
      return options.typeRoots

    }
    var currentDirectory: String = zeroOfMyType
    if (options.configFilePath) {
      (currentDirectory = getDirectoryPath(options.configFilePath))

    } else if (host.getCurrentDirectory) {
      (currentDirectory = host.getCurrentDirectory())

    }
    return ((currentDirectory !== undefined) && getDefaultTypeRoots(
      currentDirectory,
      host))

  }
  def getDefaultTypeRoots(currentDirectory: String, host: {
    var directoryExists: ((String) => Boolean)
  }): (Array[String] | undefined) = {
    if ((!host.directoryExists)) {
      return Array(combinePaths(currentDirectory, nodeModulesAtTypes))

    }
    var typeRoots: Array[String] = zeroOfMyType
    while (true) {
      {
        val atTypes = combinePaths(currentDirectory, nodeModulesAtTypes)
        if (host.directoryExists(atTypes)) {
          ((typeRoots || ((typeRoots = Array())))).push(atTypes)

        }
        val parent = getDirectoryPath(currentDirectory)
        if ((parent === currentDirectory)) {
          break()

        }
        (currentDirectory = parent)

      }
    }
    return typeRoots

  }
  val nodeModulesAtTypes = combinePaths("node_modules", "@types")
  def resolveTypeReferenceDirective(typeReferenceDirectiveName: String,
                                    containingFile: String,
                                    options: CompilerOptions,
                                    host: ModuleResolutionHost)
    : ResolvedTypeReferenceDirectiveWithFailedLookupLocations = {
    val traceEnabled = isTraceEnabled(options, host)
    val moduleResolutionState: ModuleResolutionState = Map(
      "compilerOptions" -> options,
      "host" -> host,
      "skipTsx" -> true,
      "traceEnabled" -> traceEnabled)
    val typeRoots = getEffectiveTypeRoots(options, host)
    if (traceEnabled) {
      if ((containingFile === undefined)) {
        if ((typeRoots === undefined)) {
          trace(
            host,
            Diagnostics.Resolving_type_reference_directive_0_containing_file_not_set_root_directory_not_set,
            typeReferenceDirectiveName)

        } else {
          trace(
            host,
            Diagnostics.Resolving_type_reference_directive_0_containing_file_not_set_root_directory_1,
            typeReferenceDirectiveName,
            typeRoots)

        }

      } else {
        if ((typeRoots === undefined)) {
          trace(
            host,
            Diagnostics.Resolving_type_reference_directive_0_containing_file_1_root_directory_not_set,
            typeReferenceDirectiveName,
            containingFile)

        } else {
          trace(
            host,
            Diagnostics.Resolving_type_reference_directive_0_containing_file_1_root_directory_2,
            typeReferenceDirectiveName,
            containingFile,
            typeRoots)

        }

      }

    }
    val failedLookupLocations: Array[String] = Array()
    if ((typeRoots && typeRoots.length)) {
      if (traceEnabled) {
        trace(
          host,
          Diagnostics.Resolving_with_primary_search_path_0,
          typeRoots.join(", "))

      }
      val primarySearchPaths = typeRoots
      (primarySearchPaths).foreach { fresh1 =>
        val typeRoot = zeroOfMyType = fresh1 {
          val candidate = combinePaths(typeRoot, typeReferenceDirectiveName)
          val candidateDirectory = getDirectoryPath(candidate)
          val resolvedFile = loadNodeModuleFromDirectory(
            typeReferenceExtensions,
            candidate,
            failedLookupLocations,
            (!directoryProbablyExists(candidateDirectory, host)),
            moduleResolutionState)
          if (resolvedFile) {
            if (traceEnabled) {
              trace(
                host,
                Diagnostics.Type_reference_directive_0_was_successfully_resolved_to_1_primary_Colon_2,
                typeReferenceDirectiveName,
                resolvedFile,
                true)

            }
            return Map(
              "resolvedTypeReferenceDirective" -> Map(
                "primary" -> true,
                "resolvedFileName" -> resolvedFile),
              "failedLookupLocations" -> failedLookupLocations)

          }

        }
      }

    } else {
      if (traceEnabled) {
        trace(
          host,
          Diagnostics.Root_directory_cannot_be_determined_skipping_primary_search_paths)

      }

    }
    var resolvedFile: String = zeroOfMyType
    var initialLocationForSecondaryLookup: String = zeroOfMyType
    if (containingFile) {
      (initialLocationForSecondaryLookup = getDirectoryPath(containingFile))

    }
    if ((initialLocationForSecondaryLookup !== undefined)) {
      if (traceEnabled) {
        trace(
          host,
          Diagnostics.Looking_up_in_node_modules_folder_initial_location_0,
          initialLocationForSecondaryLookup)

      }
      (resolvedFile = loadModuleFromNodeModules(
        typeReferenceDirectiveName,
        initialLocationForSecondaryLookup,
        failedLookupLocations,
        moduleResolutionState,
        false))
      if (traceEnabled) {
        if (resolvedFile) {
          trace(
            host,
            Diagnostics.Type_reference_directive_0_was_successfully_resolved_to_1_primary_Colon_2,
            typeReferenceDirectiveName,
            resolvedFile,
            false)

        } else {
          trace(
            host,
            Diagnostics.Type_reference_directive_0_was_not_resolved,
            typeReferenceDirectiveName)

        }

      }

    } else {
      if (traceEnabled) {
        trace(
          host,
          Diagnostics.Containing_file_is_not_specified_and_root_directory_cannot_be_determined_skipping_lookup_in_node_modules_folder)

      }

    }
    return Map(
      "resolvedTypeReferenceDirective" -> (if (resolvedFile)
                                             Map(
                                               "primary" -> false,
                                               "resolvedFileName" -> resolvedFile)
                                           else undefined),
      "failedLookupLocations" -> failedLookupLocations)

  }
  def getAutomaticTypeDirectiveNames(
      options: CompilerOptions,
      host: ModuleResolutionHost): Array[String] = {
    if (options.types) {
      return options.types

    }
    val result: Array[String] = Array()
    if ((host.directoryExists && host.getDirectories)) {
      val typeRoots = getEffectiveTypeRoots(options, host)
      if (typeRoots) {
        (typeRoots).foreach { fresh2 =>
          val root = zeroOfMyType = fresh2 {
            if (host.directoryExists(root)) {
              (host.getDirectories(root)).foreach { fresh3 =>
                val typeDirectivePath = zeroOfMyType = fresh3 {
                  val normalized = normalizePath(typeDirectivePath)
                  val packageJsonPath =
                    pathToPackageJson(combinePaths(root, normalized))
                  val isNotNeededPackage = (host
                      .fileExists(packageJsonPath) && (readJson(
                      packageJsonPath,
                      host).typings === null))
                  if ((!isNotNeededPackage)) {
                    result.push(getBaseFileName(normalized))

                  }

                }
              }

            }

          }
        }

      }

    }
    return result

  }
  def resolveModuleName(
      moduleName: String,
      containingFile: String,
      compilerOptions: CompilerOptions,
      host: ModuleResolutionHost): ResolvedModuleWithFailedLookupLocations = {
    val traceEnabled = isTraceEnabled(compilerOptions, host)
    if (traceEnabled) {
      trace(
        host,
        Diagnostics.Resolving_module_0_from_1,
        moduleName,
        containingFile)

    }
    var moduleResolution = compilerOptions.moduleResolution
    if ((moduleResolution === undefined)) {
      (moduleResolution =
        (if ((getEmitModuleKind(compilerOptions) === ModuleKind.CommonJS))
           ModuleResolutionKind.NodeJs
         else ModuleResolutionKind.Classic))
      if (traceEnabled) {
        trace(
          host,
          Diagnostics.Module_resolution_kind_is_not_specified_using_0,
          ModuleResolutionKind(moduleResolution))

      }

    } else {
      if (traceEnabled) {
        trace(
          host,
          Diagnostics.Explicitly_specified_module_resolution_kind_Colon_0,
          ModuleResolutionKind(moduleResolution))

      }

    }
    var result: ResolvedModuleWithFailedLookupLocations = zeroOfMyType
    moduleResolution match {
      case ModuleResolutionKind.NodeJs =>
        (result = nodeModuleNameResolver(
          moduleName,
          containingFile,
          compilerOptions,
          host))
      case ModuleResolutionKind.Classic =>
        (result = classicNameResolver(
          moduleName,
          containingFile,
          compilerOptions,
          host))
      case _ =>
    }
    if (traceEnabled) {
      if (result.resolvedModule) {
        trace(
          host,
          Diagnostics.Module_name_0_was_successfully_resolved_to_1,
          moduleName,
          result.resolvedModule.resolvedFileName)

      } else {
        trace(host, Diagnostics.Module_name_0_was_not_resolved, moduleName)

      }

    }
    return result

  }
  type ResolutionKindSpecificLoader = ((String, Array[String], Array[String],
                                        Boolean,
                                        ModuleResolutionState) => String)
  def tryLoadModuleUsingOptionalResolutionSettings(
      moduleName: String,
      containingDirectory: String,
      loader: ResolutionKindSpecificLoader,
      failedLookupLocations: Array[String],
      supportedExtensions: Array[String],
      state: ModuleResolutionState): String = {
    if (moduleHasNonRelativeName(moduleName)) {
      return tryLoadModuleUsingBaseUrl(
        moduleName,
        loader,
        failedLookupLocations,
        supportedExtensions,
        state)

    } else {
      return tryLoadModuleUsingRootDirs(
        moduleName,
        containingDirectory,
        loader,
        failedLookupLocations,
        supportedExtensions,
        state)

    }

  }
  def tryLoadModuleUsingRootDirs(moduleName: String,
                                 containingDirectory: String,
                                 loader: ResolutionKindSpecificLoader,
                                 failedLookupLocations: Array[String],
                                 supportedExtensions: Array[String],
                                 state: ModuleResolutionState): String = {
    if ((!state.compilerOptions.rootDirs)) {
      return undefined

    }
    if (state.traceEnabled) {
      trace(
        state.host,
        Diagnostics.rootDirs_option_is_set_using_it_to_resolve_relative_module_name_0,
        moduleName)

    }
    val candidate = normalizePath(
      combinePaths(containingDirectory, moduleName))
    var matchedRootDir: String = zeroOfMyType
    var matchedNormalizedPrefix: String = zeroOfMyType
    (state.compilerOptions.rootDirs).foreach { fresh4 =>
      val rootDir = zeroOfMyType = fresh4 {
        var normalizedRoot = normalizePath(rootDir)
        if ((!endsWith(normalizedRoot, directorySeparator))) {
          (normalizedRoot += directorySeparator)

        }
        val isLongestMatchingPrefix = (startsWith(candidate, normalizedRoot) && (((matchedNormalizedPrefix === undefined) || (matchedNormalizedPrefix.length < normalizedRoot.length))))
        if (state.traceEnabled) {
          trace(
            state.host,
            Diagnostics.Checking_if_0_is_the_longest_matching_prefix_for_1_2,
            normalizedRoot,
            candidate,
            isLongestMatchingPrefix)

        }
        if (isLongestMatchingPrefix) {
          (matchedNormalizedPrefix = normalizedRoot)
          (matchedRootDir = rootDir)

        }

      }
    }
    if (matchedNormalizedPrefix) {
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.Longest_matching_prefix_for_0_is_1,
          candidate,
          matchedNormalizedPrefix)

      }
      val suffix = candidate.substr(matchedNormalizedPrefix.length)
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.Loading_0_from_the_root_dir_1_candidate_location_2,
          suffix,
          matchedNormalizedPrefix,
          candidate)

      }
      val resolvedFileName = loader(
        candidate,
        supportedExtensions,
        failedLookupLocations,
        (!directoryProbablyExists(containingDirectory, state.host)),
        state)
      if (resolvedFileName) {
        return resolvedFileName

      }
      if (state.traceEnabled) {
        trace(state.host, Diagnostics.Trying_other_entries_in_rootDirs)

      }
      (state.compilerOptions.rootDirs).foreach { fresh5 =>
        val rootDir = zeroOfMyType = fresh5 {
          if ((rootDir === matchedRootDir)) {
            continue

          }
          val candidate = combinePaths(normalizePath(rootDir), suffix)
          if (state.traceEnabled) {
            trace(
              state.host,
              Diagnostics.Loading_0_from_the_root_dir_1_candidate_location_2,
              suffix,
              rootDir,
              candidate)

          }
          val baseDirectory = getDirectoryPath(candidate)
          val resolvedFileName = loader(
            candidate,
            supportedExtensions,
            failedLookupLocations,
            (!directoryProbablyExists(baseDirectory, state.host)),
            state)
          if (resolvedFileName) {
            return resolvedFileName

          }

        }
      }
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.Module_resolution_using_rootDirs_has_failed)

      }

    }
    return undefined

  }
  def tryLoadModuleUsingBaseUrl(moduleName: String,
                                loader: ResolutionKindSpecificLoader,
                                failedLookupLocations: Array[String],
                                supportedExtensions: Array[String],
                                state: ModuleResolutionState): String = {
    if ((!state.compilerOptions.baseUrl)) {
      return undefined

    }
    if (state.traceEnabled) {
      trace(
        state.host,
        Diagnostics.baseUrl_option_is_set_to_0_using_this_value_to_resolve_non_relative_module_name_1,
        state.compilerOptions.baseUrl,
        moduleName)

    }
    var matchedPattern: (Pattern | String | undefined) = undefined
    if (state.compilerOptions.paths) {
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.paths_option_is_specified_looking_for_a_pattern_to_match_module_name_0,
          moduleName)

      }
      (matchedPattern = matchPatternOrExact(
        getOwnKeys(state.compilerOptions.paths),
        moduleName))

    }
    if (matchedPattern) {
      val matchedStar =
        (if ((typeof(matchedPattern) === "string")) undefined
         else matchedText(matchedPattern, moduleName))
      val matchedPatternText =
        (if ((typeof(matchedPattern) === "string")) matchedPattern
         else patternText(matchedPattern))
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.Module_name_0_matched_pattern_1,
          moduleName,
          matchedPatternText)

      }
      (state.compilerOptions.paths(matchedPatternText)).foreach { fresh6 =>
        val subst = zeroOfMyType = fresh6 {
          val path =
            (if (matchedStar) subst.replace("*", matchedStar) else subst)
          val candidate =
            normalizePath(combinePaths(state.compilerOptions.baseUrl, path))
          if (state.traceEnabled) {
            trace(
              state.host,
              Diagnostics.Trying_substitution_0_candidate_module_location_Colon_1,
              subst,
              path)

          }
          val resolvedFileName = loader(
            candidate,
            supportedExtensions,
            failedLookupLocations,
            (!directoryProbablyExists(
              getDirectoryPath(candidate),
              state.host)),
            state)
          if (resolvedFileName) {
            return resolvedFileName

          }

        }
      }
      return undefined

    } else {
      val candidate = normalizePath(
        combinePaths(state.compilerOptions.baseUrl, moduleName))
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.Resolving_module_name_0_relative_to_base_url_1_2,
          moduleName,
          state.compilerOptions.baseUrl,
          candidate)

      }
      return loader(
        candidate,
        supportedExtensions,
        failedLookupLocations,
        (!directoryProbablyExists(getDirectoryPath(candidate), state.host)),
        state)

    }

  }
  def nodeModuleNameResolver(
      moduleName: String,
      containingFile: String,
      compilerOptions: CompilerOptions,
      host: ModuleResolutionHost): ResolvedModuleWithFailedLookupLocations = {
    val containingDirectory = getDirectoryPath(containingFile)
    val supportedExtensions = getSupportedExtensions(compilerOptions)
    val traceEnabled = isTraceEnabled(compilerOptions, host)
    val failedLookupLocations: Array[String] = Array()
    val state = Map(
      "compilerOptions" -> compilerOptions,
      "host" -> host,
      "traceEnabled" -> traceEnabled,
      "skipTsx" -> false)
    var resolvedFileName = tryLoadModuleUsingOptionalResolutionSettings(
      moduleName,
      containingDirectory,
      nodeLoadModuleByRelativeName,
      failedLookupLocations,
      supportedExtensions,
      state)
    var isExternalLibraryImport = false
    if ((!resolvedFileName)) {
      if (moduleHasNonRelativeName(moduleName)) {
        if (traceEnabled) {
          trace(
            host,
            Diagnostics.Loading_module_0_from_node_modules_folder,
            moduleName)

        }
        (resolvedFileName = loadModuleFromNodeModules(
          moduleName,
          containingDirectory,
          failedLookupLocations,
          state,
          false))
        (isExternalLibraryImport = (resolvedFileName !== undefined))

      } else {
        val candidate = normalizePath(
          combinePaths(containingDirectory, moduleName))
        (resolvedFileName = nodeLoadModuleByRelativeName(
          candidate,
          supportedExtensions,
          failedLookupLocations,
          false,
          state))

      }

    }
    if ((resolvedFileName && host.realpath)) {
      val originalFileName = resolvedFileName
      (resolvedFileName = normalizePath(host.realpath(resolvedFileName)))
      if (traceEnabled) {
        trace(
          host,
          Diagnostics.Resolving_real_path_for_0_result_1,
          originalFileName,
          resolvedFileName)

      }

    }
    return createResolvedModule(
      resolvedFileName,
      isExternalLibraryImport,
      failedLookupLocations)

  }
  def nodeLoadModuleByRelativeName(candidate: String,
                                   supportedExtensions: Array[String],
                                   failedLookupLocations: Array[String],
                                   onlyRecordFailures: Boolean,
                                   state: ModuleResolutionState): String = {
    if (state.traceEnabled) {
      trace(
        state.host,
        Diagnostics.Loading_module_as_file_Slash_folder_candidate_module_location_0,
        candidate)

    }
    val resolvedFileName = ((!pathEndsWithDirectorySeparator(candidate)) && loadModuleFromFile(
        candidate,
        supportedExtensions,
        failedLookupLocations,
        onlyRecordFailures,
        state))
    return (resolvedFileName || loadNodeModuleFromDirectory(
      supportedExtensions,
      candidate,
      failedLookupLocations,
      onlyRecordFailures,
      state))

  }
  def directoryProbablyExists(directoryName: String, host: {
    var directoryExists: ((String) => Boolean)
  }): Boolean = {
    return ((!host.directoryExists) || host.directoryExists(directoryName))

  }
  def loadModuleFromFile(
      candidate: String,
      extensions: Array[String],
      failedLookupLocation: Array[String],
      onlyRecordFailures: Boolean,
      state: ModuleResolutionState): (String | undefined) = {
    val resolvedByAddingExtension = tryAddingExtensions(
      candidate,
      extensions,
      failedLookupLocation,
      onlyRecordFailures,
      state)
    if (resolvedByAddingExtension) {
      return resolvedByAddingExtension

    }
    if (hasJavaScriptFileExtension(candidate)) {
      val extensionless = removeFileExtension(candidate)
      if (state.traceEnabled) {
        val extension = candidate.substring(extensionless.length)
        trace(
          state.host,
          Diagnostics.File_name_0_has_a_1_extension_stripping_it,
          candidate,
          extension)

      }
      return tryAddingExtensions(
        extensionless,
        extensions,
        failedLookupLocation,
        onlyRecordFailures,
        state)

    }

  }
  def tryAddingExtensions(
      candidate: String,
      extensions: Array[String],
      failedLookupLocation: Array[String],
      onlyRecordFailures: Boolean,
      state: ModuleResolutionState): (String | undefined) = {
    if ((!onlyRecordFailures)) {
      val directory = getDirectoryPath(candidate)
      if (directory) {
        (onlyRecordFailures = (!directoryProbablyExists(directory, state.host)))

      }

    }
    return forEach(
      extensions,
      (ext =>
         ((!((state.skipTsx && isJsxOrTsxExtension(ext)))) && tryFile(
           (candidate + ext),
           failedLookupLocation,
           onlyRecordFailures,
           state))))

  }
  def tryFile(fileName: String,
              failedLookupLocation: Array[String],
              onlyRecordFailures: Boolean,
              state: ModuleResolutionState): (String | undefined) = {
    if (((!onlyRecordFailures) && state.host.fileExists(fileName))) {
      if (state.traceEnabled) {
        trace(
          state.host,
          Diagnostics.File_0_exist_use_it_as_a_name_resolution_result,
          fileName)

      }
      return fileName

    } else {
      if (state.traceEnabled) {
        trace(state.host, Diagnostics.File_0_does_not_exist, fileName)

      }
      failedLookupLocation.push(fileName)
      return undefined

    }

  }
  def loadNodeModuleFromDirectory(extensions: Array[String],
                                  candidate: String,
                                  failedLookupLocation: Array[String],
                                  onlyRecordFailures: Boolean,
                                  state: ModuleResolutionState): String = {
    val packageJsonPath = pathToPackageJson(candidate)
    val directoryExists = ((!onlyRecordFailures) && directoryProbablyExists(
        candidate,
        state.host))
    if ((directoryExists && state.host.fileExists(packageJsonPath))) {
      if (state.traceEnabled) {
        trace(state.host, Diagnostics.Found_package_json_at_0, packageJsonPath)

      }
      val typesFile = tryReadTypesSection(packageJsonPath, candidate, state)
      if (typesFile) {
        val onlyRecordFailures =
          (!directoryProbablyExists(getDirectoryPath(typesFile), state.host))
        val result = (tryFile(
            typesFile,
            failedLookupLocation,
            onlyRecordFailures,
            state) || tryAddingExtensions(
            typesFile,
            extensions,
            failedLookupLocation,
            onlyRecordFailures,
            state))
        if (result) {
          return result

        }

      } else {
        if (state.traceEnabled) {
          trace(state.host, Diagnostics.package_json_does_not_have_types_field)

        }

      }

    } else {
      if (state.traceEnabled) {
        trace(state.host, Diagnostics.File_0_does_not_exist, packageJsonPath)

      }
      failedLookupLocation.push(packageJsonPath)

    }
    return loadModuleFromFile(
      combinePaths(candidate, "index"),
      extensions,
      failedLookupLocation,
      (!directoryExists),
      state)

  }
  def pathToPackageJson(directory: String): String = {
    return combinePaths(directory, "package.json")

  }
  def loadModuleFromNodeModulesFolder(moduleName: String,
                                      directory: String,
                                      failedLookupLocations: Array[String],
                                      state: ModuleResolutionState): String = {
    val nodeModulesFolder = combinePaths(directory, "node_modules")
    val nodeModulesFolderExists =
      directoryProbablyExists(nodeModulesFolder, state.host)
    val candidate = normalizePath(combinePaths(nodeModulesFolder, moduleName))
    val supportedExtensions = getSupportedExtensions(state.compilerOptions)
    var result = loadModuleFromFile(
      candidate,
      supportedExtensions,
      failedLookupLocations,
      (!nodeModulesFolderExists),
      state)
    if (result) {
      return result

    }
    (result = loadNodeModuleFromDirectory(
      supportedExtensions,
      candidate,
      failedLookupLocations,
      (!nodeModulesFolderExists),
      state))
    if (result) {
      return result

    }

  }
  def loadModuleFromNodeModules(moduleName: String,
                                directory: String,
                                failedLookupLocations: Array[String],
                                state: ModuleResolutionState,
                                checkOneLevel: Boolean): String = {
    return loadModuleFromNodeModulesWorker(
      moduleName,
      directory,
      failedLookupLocations,
      state,
      checkOneLevel,
      false)

  }
  def loadModuleFromNodeModulesAtTypes(
      moduleName: String,
      directory: String,
      failedLookupLocations: Array[String],
      state: ModuleResolutionState): String = {
    return loadModuleFromNodeModulesWorker(
      moduleName,
      directory,
      failedLookupLocations,
      state,
      false,
      true)

  }
  def loadModuleFromNodeModulesWorker(moduleName: String,
                                      directory: String,
                                      failedLookupLocations: Array[String],
                                      state: ModuleResolutionState,
                                      checkOneLevel: Boolean,
                                      typesOnly: Boolean): String = {
    (directory = normalizeSlashes(directory))
    while (true) {
      {
        val baseName = getBaseFileName(directory)
        if ((baseName !== "node_modules")) {
          var packageResult: (String | undefined) = zeroOfMyType
          if ((!typesOnly)) {
            (packageResult = loadModuleFromNodeModulesFolder(
              moduleName,
              directory,
              failedLookupLocations,
              state))
            if ((packageResult && hasTypeScriptFileExtension(packageResult))) {
              return packageResult

            }

          }
          val typesResult = loadModuleFromNodeModulesFolder(
            combinePaths("@types", moduleName),
            directory,
            failedLookupLocations,
            state)
          if ((typesResult || packageResult)) {
            return (typesResult || packageResult)

          }

        }
        val parentPath = getDirectoryPath(directory)
        if (((parentPath === directory) || checkOneLevel)) {
          break()

        }
        (directory = parentPath)

      }
    }
    return undefined

  }
  def classicNameResolver(
      moduleName: String,
      containingFile: String,
      compilerOptions: CompilerOptions,
      host: ModuleResolutionHost): ResolvedModuleWithFailedLookupLocations = {
    val traceEnabled = isTraceEnabled(compilerOptions, host)
    val state = Map(
      "compilerOptions" -> compilerOptions,
      "host" -> host,
      "traceEnabled" -> traceEnabled,
      "skipTsx" -> (!compilerOptions.jsx))
    val failedLookupLocations: Array[String] = Array()
    val supportedExtensions = getSupportedExtensions(compilerOptions)
    val containingDirectory = getDirectoryPath(containingFile)
    val resolvedFileName = tryLoadModuleUsingOptionalResolutionSettings(
      moduleName,
      containingDirectory,
      loadModuleFromFile,
      failedLookupLocations,
      supportedExtensions,
      state)
    if (resolvedFileName) {
      return createResolvedModule(
        resolvedFileName,
        false,
        failedLookupLocations)

    }
    var referencedSourceFile: String = zeroOfMyType
    if (moduleHasNonRelativeName(moduleName)) {
      (referencedSourceFile =
        (referencedSourceFile = (loadModuleFromAncestorDirectories(
            moduleName,
            containingDirectory,
            supportedExtensions,
            failedLookupLocations,
            state) || loadModuleFromNodeModulesAtTypes(
            moduleName,
            containingDirectory,
            failedLookupLocations,
            state))))

    } else {
      val candidate = normalizePath(
        combinePaths(containingDirectory, moduleName))
      (referencedSourceFile = loadModuleFromFile(
        candidate,
        supportedExtensions,
        failedLookupLocations,
        false,
        state))

    }
    return (if (referencedSourceFile)
              Map(
                "resolvedModule" -> Map(
                  "resolvedFileName" -> referencedSourceFile),
                "failedLookupLocations" -> failedLookupLocations)
            else
              Map(
                "resolvedModule" -> undefined,
                "failedLookupLocations" -> failedLookupLocations))

  }
  def loadModuleFromAncestorDirectories(
      moduleName: String,
      containingDirectory: String,
      supportedExtensions: Array[String],
      failedLookupLocations: Array[String],
      state: ModuleResolutionState): (String | undefined) = {
    while (true) {
      {
        val searchName = normalizePath(
          combinePaths(containingDirectory, moduleName))
        val referencedSourceFile = loadModuleFromFile(
          searchName,
          supportedExtensions,
          failedLookupLocations,
          false,
          state)
        if (referencedSourceFile) {
          return referencedSourceFile

        }
        val parentPath = getDirectoryPath(containingDirectory)
        if ((parentPath === containingDirectory)) {
          return undefined

        }
        (containingDirectory = parentPath)

      }
    }

  }
}
