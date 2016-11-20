package scalatscomp
object CommandLineParser {
val compileOnSaveCommandLineOption: CommandLineOption = Map( "name" -> "compileOnSave",
"type" -> "boolean" )
val optionDeclarations: Array[CommandLineOption] = Array( Map( "name" -> "charset",
"type" -> "string" ), compileOnSaveCommandLineOption, Map( "name" -> "declaration",
"shortName" -> "d",
"type" -> "boolean",
"description" -> Diagnostics.Generates_corresponding_d_ts_file ), Map( "name" -> "declarationDir",
"type" -> "string",
"isFilePath" -> true,
"paramType" -> Diagnostics.DIRECTORY ), Map( "name" -> "diagnostics",
"type" -> "boolean" ), Map( "name" -> "extendedDiagnostics",
"type" -> "boolean",
"experimental" -> true ), Map( "name" -> "emitBOM",
"type" -> "boolean" ), Map( "name" -> "help",
"shortName" -> "h",
"type" -> "boolean",
"description" -> Diagnostics.Print_this_message ), Map( "name" -> "help",
"shortName" -> "?",
"type" -> "boolean" ), Map( "name" -> "init",
"type" -> "boolean",
"description" -> Diagnostics.Initializes_a_TypeScript_project_and_creates_a_tsconfig_json_file ), Map( "name" -> "inlineSourceMap",
"type" -> "boolean" ), Map( "name" -> "inlineSources",
"type" -> "boolean" ), Map( "name" -> "jsx",
"type" -> createMap( Map( "preserve" -> JsxEmit.Preserve,
"react" -> JsxEmit.React ) ),
"paramType" -> Diagnostics.KIND,
"description" -> Diagnostics.Specify_JSX_code_generation_Colon_preserve_or_react ), Map( "name" -> "reactNamespace",
"type" -> "string",
"description" -> Diagnostics.Specify_the_object_invoked_for_createElement_and_spread_when_targeting_react_JSX_emit ), Map( "name" -> "listFiles",
"type" -> "boolean" ), Map( "name" -> "locale",
"type" -> "string" ), Map( "name" -> "mapRoot",
"type" -> "string",
"isFilePath" -> true,
"description" -> Diagnostics.Specify_the_location_where_debugger_should_locate_map_files_instead_of_generated_locations,
"paramType" -> Diagnostics.LOCATION ), Map( "name" -> "module",
"shortName" -> "m",
"type" -> createMap( Map( "none" -> ModuleKind.None,
"commonjs" -> ModuleKind.CommonJS,
"amd" -> ModuleKind.AMD,
"system" -> ModuleKind.System,
"umd" -> ModuleKind.UMD,
"es6" -> ModuleKind.ES2015,
"es2015" -> ModuleKind.ES2015 ) ),
"description" -> Diagnostics.Specify_module_code_generation_Colon_commonjs_amd_system_umd_or_es2015,
"paramType" -> Diagnostics.KIND ), Map( "name" -> "newLine",
"type" -> createMap( Map( "crlf" -> NewLineKind.CarriageReturnLineFeed,
"lf" -> NewLineKind.LineFeed ) ),
"description" -> Diagnostics.Specify_the_end_of_line_sequence_to_be_used_when_emitting_files_Colon_CRLF_dos_or_LF_unix,
"paramType" -> Diagnostics.NEWLINE ), Map( "name" -> "noEmit",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_emit_outputs ), Map( "name" -> "noEmitHelpers",
"type" -> "boolean" ), Map( "name" -> "noEmitOnError",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_emit_outputs_if_any_errors_were_reported ), Map( "name" -> "noErrorTruncation",
"type" -> "boolean" ), Map( "name" -> "noImplicitAny",
"type" -> "boolean",
"description" -> Diagnostics.Raise_error_on_expressions_and_declarations_with_an_implied_any_type ), Map( "name" -> "noImplicitThis",
"type" -> "boolean",
"description" -> Diagnostics.Raise_error_on_this_expressions_with_an_implied_any_type ), Map( "name" -> "noUnusedLocals",
"type" -> "boolean",
"description" -> Diagnostics.Report_errors_on_unused_locals ), Map( "name" -> "noUnusedParameters",
"type" -> "boolean",
"description" -> Diagnostics.Report_errors_on_unused_parameters ), Map( "name" -> "noLib",
"type" -> "boolean" ), Map( "name" -> "noResolve",
"type" -> "boolean" ), Map( "name" -> "skipDefaultLibCheck",
"type" -> "boolean" ), Map( "name" -> "skipLibCheck",
"type" -> "boolean",
"description" -> Diagnostics.Skip_type_checking_of_declaration_files ), Map( "name" -> "out",
"type" -> "string",
"isFilePath" -> false,
"paramType" -> Diagnostics.FILE ), Map( "name" -> "outFile",
"type" -> "string",
"isFilePath" -> true,
"description" -> Diagnostics.Concatenate_and_emit_output_to_single_file,
"paramType" -> Diagnostics.FILE ), Map( "name" -> "outDir",
"type" -> "string",
"isFilePath" -> true,
"description" -> Diagnostics.Redirect_output_structure_to_the_directory,
"paramType" -> Diagnostics.DIRECTORY ), Map( "name" -> "preserveConstEnums",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_erase_const_enum_declarations_in_generated_code ), Map( "name" -> "pretty",
"description" -> Diagnostics.Stylize_errors_and_messages_using_color_and_context_experimental,
"type" -> "boolean" ), Map( "name" -> "project",
"shortName" -> "p",
"type" -> "string",
"isFilePath" -> true,
"description" -> Diagnostics.Compile_the_project_in_the_given_directory,
"paramType" -> Diagnostics.DIRECTORY ), Map( "name" -> "removeComments",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_emit_comments_to_output ), Map( "name" -> "rootDir",
"type" -> "string",
"isFilePath" -> true,
"paramType" -> Diagnostics.LOCATION,
"description" -> Diagnostics.Specify_the_root_directory_of_input_files_Use_to_control_the_output_directory_structure_with_outDir ), Map( "name" -> "isolatedModules",
"type" -> "boolean" ), Map( "name" -> "sourceMap",
"type" -> "boolean",
"description" -> Diagnostics.Generates_corresponding_map_file ), Map( "name" -> "sourceRoot",
"type" -> "string",
"isFilePath" -> true,
"description" -> Diagnostics.Specify_the_location_where_debugger_should_locate_TypeScript_files_instead_of_source_locations,
"paramType" -> Diagnostics.LOCATION ), Map( "name" -> "suppressExcessPropertyErrors",
"type" -> "boolean",
"description" -> Diagnostics.Suppress_excess_property_checks_for_object_literals,
"experimental" -> true ), Map( "name" -> "suppressImplicitAnyIndexErrors",
"type" -> "boolean",
"description" -> Diagnostics.Suppress_noImplicitAny_errors_for_indexing_objects_lacking_index_signatures ), Map( "name" -> "stripInternal",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_emit_declarations_for_code_that_has_an_internal_annotation,
"experimental" -> true ), Map( "name" -> "target",
"shortName" -> "t",
"type" -> createMap( Map( "es3" -> ScriptTarget.ES3,
"es5" -> ScriptTarget.ES5,
"es6" -> ScriptTarget.ES2015,
"es2015" -> ScriptTarget.ES2015,
"es2016" -> ScriptTarget.ES2016,
"es2017" -> ScriptTarget.ES2017 ) ),
"description" -> Diagnostics.Specify_ECMAScript_target_version_Colon_ES3_default_ES5_or_ES2015,
"paramType" -> Diagnostics.VERSION ), Map( "name" -> "version",
"shortName" -> "v",
"type" -> "boolean",
"description" -> Diagnostics.Print_the_compiler_s_version ), Map( "name" -> "watch",
"shortName" -> "w",
"type" -> "boolean",
"description" -> Diagnostics.Watch_input_files ), Map( "name" -> "experimentalDecorators",
"type" -> "boolean",
"description" -> Diagnostics.Enables_experimental_support_for_ES7_decorators ), Map( "name" -> "emitDecoratorMetadata",
"type" -> "boolean",
"experimental" -> true,
"description" -> Diagnostics.Enables_experimental_support_for_emitting_type_metadata_for_decorators ), Map( "name" -> "moduleResolution",
"type" -> createMap( Map( "node" -> ModuleResolutionKind.NodeJs,
"classic" -> ModuleResolutionKind.Classic ) ),
"description" -> Diagnostics.Specify_module_resolution_strategy_Colon_node_Node_js_or_classic_TypeScript_pre_1_6,
"paramType" -> Diagnostics.STRATEGY ), Map( "name" -> "allowUnusedLabels",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_report_errors_on_unused_labels ), Map( "name" -> "noImplicitReturns",
"type" -> "boolean",
"description" -> Diagnostics.Report_error_when_not_all_code_paths_in_function_return_a_value ), Map( "name" -> "noFallthroughCasesInSwitch",
"type" -> "boolean",
"description" -> Diagnostics.Report_errors_for_fallthrough_cases_in_switch_statement ), Map( "name" -> "allowUnreachableCode",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_report_errors_on_unreachable_code ), Map( "name" -> "forceConsistentCasingInFileNames",
"type" -> "boolean",
"description" -> Diagnostics.Disallow_inconsistently_cased_references_to_the_same_file ), Map( "name" -> "baseUrl",
"type" -> "string",
"isFilePath" -> true,
"description" -> Diagnostics.Base_directory_to_resolve_non_absolute_module_names ), Map( "name" -> "paths",
"type" -> "object",
"isTSConfigOnly" -> true ), Map( "name" -> "rootDirs",
"type" -> "list",
"isTSConfigOnly" -> true,
"element" -> Map( "name" -> "rootDirs",
"type" -> "string",
"isFilePath" -> true ) ), Map( "name" -> "typeRoots",
"type" -> "list",
"element" -> Map( "name" -> "typeRoots",
"type" -> "string",
"isFilePath" -> true ) ), Map( "name" -> "types",
"type" -> "list",
"element" -> Map( "name" -> "types",
"type" -> "string" ),
"description" -> Diagnostics.Type_declaration_files_to_be_included_in_compilation ), Map( "name" -> "traceResolution",
"type" -> "boolean",
"description" -> Diagnostics.Enable_tracing_of_the_name_resolution_process ), Map( "name" -> "allowJs",
"type" -> "boolean",
"description" -> Diagnostics.Allow_javascript_files_to_be_compiled ), Map( "name" -> "allowSyntheticDefaultImports",
"type" -> "boolean",
"description" -> Diagnostics.Allow_default_imports_from_modules_with_no_default_export_This_does_not_affect_code_emit_just_typechecking ), Map( "name" -> "noImplicitUseStrict",
"type" -> "boolean",
"description" -> Diagnostics.Do_not_emit_use_strict_directives_in_module_output ), Map( "name" -> "maxNodeModuleJsDepth",
"type" -> "number",
"description" -> Diagnostics.The_maximum_dependency_depth_to_search_under_node_modules_and_load_JavaScript_files ), Map( "name" -> "listEmittedFiles",
"type" -> "boolean" ), Map( "name" -> "lib",
"type" -> "list",
"element" -> Map( "name" -> "lib",
"type" -> createMap( Map( "es5" -> "lib.es5.d.ts",
"es6" -> "lib.es2015.d.ts",
"es2015" -> "lib.es2015.d.ts",
"es7" -> "lib.es2016.d.ts",
"es2016" -> "lib.es2016.d.ts",
"es2017" -> "lib.es2017.d.ts",
"dom" -> "lib.dom.d.ts",
"dom.iterable" -> "lib.dom.iterable.d.ts",
"webworker" -> "lib.webworker.d.ts",
"scripthost" -> "lib.scripthost.d.ts",
"es2015.core" -> "lib.es2015.core.d.ts",
"es2015.collection" -> "lib.es2015.collection.d.ts",
"es2015.generator" -> "lib.es2015.generator.d.ts",
"es2015.iterable" -> "lib.es2015.iterable.d.ts",
"es2015.promise" -> "lib.es2015.promise.d.ts",
"es2015.proxy" -> "lib.es2015.proxy.d.ts",
"es2015.reflect" -> "lib.es2015.reflect.d.ts",
"es2015.symbol" -> "lib.es2015.symbol.d.ts",
"es2015.symbol.wellknown" -> "lib.es2015.symbol.wellknown.d.ts",
"es2016.array.include" -> "lib.es2016.array.include.d.ts",
"es2017.object" -> "lib.es2017.object.d.ts",
"es2017.sharedmemory" -> "lib.es2017.sharedmemory.d.ts" ) ) ),
"description" -> Diagnostics.Specify_library_files_to_be_included_in_the_compilation_Colon ), Map( "name" -> "disableSizeLimit",
"type" -> "boolean" ), Map( "name" -> "strictNullChecks",
"type" -> "boolean",
"description" -> Diagnostics.Enable_strict_null_checks ), Map( "name" -> "importHelpers",
"type" -> "boolean",
"description" -> Diagnostics.Import_emit_helpers_from_tslib ), Map( "name" -> "alwaysStrict",
"type" -> "boolean",
"description" -> Diagnostics.Parse_in_strict_mode_and_emit_use_strict_for_each_source_file ) )
var typingOptionDeclarations: Array[CommandLineOption] = Array( Map( "name" -> "enableAutoDiscovery",
"type" -> "boolean" ), Map( "name" -> "include",
"type" -> "list",
"element" -> Map( "name" -> "include",
"type" -> "string" ) ), Map( "name" -> "exclude",
"type" -> "list",
"element" -> Map( "name" -> "exclude",
"type" -> "string" ) ) )
trait OptionNameMap {
  var optionNameMap: Map[CommandLineOption]
  var shortOptionNames: Map[String]
}
val defaultInitCompilerOptions: CompilerOptions = Map( "module" -> ModuleKind.CommonJS,
"target" -> ScriptTarget.ES5,
"noImplicitAny" -> false,
"sourceMap" -> false )
var optionNameMapCache: OptionNameMap = zeroOfMyType
def getOptionNameMap(): OptionNameMap = {
 if (optionNameMapCache) {
 return optionNameMapCache

}
val optionNameMap = createMap[ CommandLineOption ]()
val shortOptionNames = createMap[ String ]()
forEach( optionDeclarations, (option =>  {
 (optionNameMap(option.name.toLowerCase())=option)
 if (option.shortName) {
 (shortOptionNames(option.shortName)=option.name)

}

}) )
(optionNameMapCache=Map( "optionNameMap" -> optionNameMap,
"shortOptionNames" -> shortOptionNames ))
return optionNameMapCache

}
def createCompilerDiagnosticForInvalidCustomType(opt: CommandLineOptionOfCustomType): Diagnostic = {
 val namesOfType = Object.keys( opt.`type` ).map( (key =>  s"""'${ key}'""" ) ).join( ", " )
return createCompilerDiagnostic( Diagnostics.Argument_for_0_option_must_be_Colon_1, s"""--${ opt.name}""" , namesOfType )

}
def parseCustomTypeOption(opt: CommandLineOptionOfCustomType, value: String, errors: Array[Diagnostic]) = {
 val key = trimString( ((value||"")) ).toLowerCase()
val map = opt.`type`
if ((keyinmap)) {
 return map(key)

}
else {
 errors.push( createCompilerDiagnosticForInvalidCustomType( opt ) )

}

}
def parseListTypeOption(opt: CommandLineOptionOfListType, value: Nothing = "", errors: Array[Diagnostic]): ( Array[( String | Int )] | undefined ) = {
 (value=trimString( value ))
if (startsWith( value, "-" )) {
 return undefined

}
if ((value==="")) {
 return Array()

}
val values = value.split( "," )
opt.element.`type` match {
  case  "number"  =>
return map( values, parseInt )
  case  "string"  =>
return map( values, (v =>  (v||"")) )
  case _ =>
return filter( map( values, (v =>  parseCustomTypeOption( opt.element.asInstanceOf[CommandLineOptionOfCustomType], v, errors )) ), (v =>  (!(!v))) )
}

}
def parseCommandLine(commandLine: Array[String], readFile: ((String) => String)): ParsedCommandLine = {
 val options: CompilerOptions = Map(
)
val fileNames: Array[String] = Array()
val errors: Array[Diagnostic] = Array()
const fresh1 = getOptionNameMap()
val optionNameMap = fresh1.optionNameMap
val shortOptionNames = fresh1.shortOptionNames
parseStrings( commandLine )
return Map( "options" -> options,
"fileNames" -> fileNames,
"errors" -> errors )
def parseStrings(args: Array[String]) = {
 var i = 0
while ((i<args.length)) {
{
 var s = args(i)
(i+= 1)
if ((s.charCodeAt( 0 )===CharacterCodes.at)) {
 parseResponseFile( s.slice( 1 ) )

}
else if ((s.charCodeAt( 0 )===CharacterCodes.minus)) {
 (s=s.slice( (if ((s.charCodeAt( 1 )===CharacterCodes.minus)) 2 else 1) ).toLowerCase())
if ((sinshortOptionNames)) {
 (s=shortOptionNames(s))

}
if ((sinoptionNameMap)) {
 val opt = optionNameMap(s)
if (opt.isTSConfigOnly) {
 errors.push( createCompilerDiagnostic( Diagnostics.Option_0_can_only_be_specified_in_tsconfig_json_file, opt.name ) )

}
else {
 if (((!args(i))&&(opt.`type`!=="boolean"))) {
 errors.push( createCompilerDiagnostic( Diagnostics.Compiler_option_0_expects_an_argument, opt.name ) )

}
opt.`type` match {
  case  "number"  =>
(options(opt.name)=parseInt( args(i) ))
(i+= 1)
  case  "boolean"  =>
var optValue = args(i)
(options(opt.name)=(optValue!=="false"))
if (((optValue==="false")||(optValue==="true"))) {
 (i+= 1)

}
  case  "string"  =>
(options(opt.name)=(args(i)||""))
(i+= 1)
  case  "list"  =>
val result = parseListTypeOption( opt.asInstanceOf[CommandLineOptionOfListType], args(i), errors )
(options(opt.name)=(result||Array()))
if (result) {
 (i+= 1)

}
  case _ =>
(options(opt.name)=parseCustomTypeOption( opt.asInstanceOf[CommandLineOptionOfCustomType], args(i), errors ))
(i+= 1)
}

}

}
else {
 errors.push( createCompilerDiagnostic( Diagnostics.Unknown_compiler_option_0, s ) )

}

}
else {
 fileNames.push( s )

}

}
}

}
def parseResponseFile(fileName: String) = {
 val text = (if (readFile) readFile( fileName ) else sys.readFile( fileName ))
if ((!text)) {
 errors.push( createCompilerDiagnostic( Diagnostics.File_0_not_found, fileName ) )
return

}
val args: Array[String] = Array()
var pos = 0
while (true) {
{
 while (((pos<text.length)&&(text.charCodeAt( pos )<=CharacterCodes.space))) {
(pos+= 1)
}
if ((pos>=text.length))
break()
val start = pos
if ((text.charCodeAt( start )===CharacterCodes.doubleQuote)) {
 (pos+= 1)
while (((pos<text.length)&&(text.charCodeAt( pos )!==CharacterCodes.doubleQuote))) {
(pos+= 1)
}
if ((pos<text.length)) {
 args.push( text.substring( (start+1), pos ) )
(pos+= 1)

}
else {
 errors.push( createCompilerDiagnostic( Diagnostics.Unterminated_quoted_string_in_response_file_0, fileName ) )

}

}
else {
 while ((text.charCodeAt( pos )>CharacterCodes.space)) {
(pos+= 1)
}
args.push( text.substring( start, pos ) )

}

}
}
parseStrings( args )

}

}
def readConfigFile(fileName: String, readFile: ((String) => String)): {   var config: Any
  var error: Diagnostic
 } = {
 var text = ""
try {
 (text=readFile( fileName ))

} catch { case e: Throwable => {
 return Map( "error" -> createCompilerDiagnostic( Diagnostics.Cannot_read_file_0_Colon_1, fileName, e.message ) )

}}
return parseConfigFileTextToJson( fileName, text )

}
def parseConfigFileTextToJson(fileName: String, jsonText: String, stripComments: Nothing = true): {   var config: Any
  var error: Diagnostic
 } = {
 try {
 val jsonTextToParse = (if (stripComments) removeComments( jsonText ) else jsonText)
return Map( "config" -> (if (java.util.regex.Pattern.compile(raw"""\S""").test( jsonTextToParse )) JSON.parse( jsonTextToParse ) else Map(
)) )

} catch { case e: Throwable => {
 return Map( "error" -> createCompilerDiagnostic( Diagnostics.Failed_to_parse_file_0_Colon_1, fileName, e.message ) )

}}

}
def generateTSConfig(options: CompilerOptions, fileNames: Array[String]): {   var compilerOptions: Map[CompilerOptionsValue]
 } = {
 val compilerOptions = extend( options, defaultInitCompilerOptions )
val configurations: Any = Map( "compilerOptions" -> serializeCompilerOptions( compilerOptions ) )
if ((fileNames&&fileNames.length)) {
 (configurations.files=fileNames)

}
return configurations
def getCustomTypeMapOfCommandLineOption(optionDefinition: CommandLineOption): ( Map[( String | Int )] | undefined ) = {
 if ((((optionDefinition.`type`==="string")||(optionDefinition.`type`==="number"))||(optionDefinition.`type`==="boolean"))) {
 return undefined

}
else if ((optionDefinition.`type`==="list")) {
 return getCustomTypeMapOfCommandLineOption( (optionDefinition.asInstanceOf[CommandLineOptionOfListType]).element )

}
else {
 return (optionDefinition.asInstanceOf[CommandLineOptionOfCustomType]).`type`

}

}
def getNameOfCompilerOptionValue(value: CompilerOptionsValue, customTypeMap: MapLike[( String | Int )]): ( String | undefined ) = {
 (customTypeMap).keys.foreach { fresh2 =>
val key = zeroOfMyType
 = fresh2
 {
 if ((customTypeMap(key)===value)) {
 return key

}

}
}
return undefined

}
def serializeCompilerOptions(options: CompilerOptions): Map[CompilerOptionsValue] = {
 val result = createMap[ CompilerOptionsValue ]()
val optionsNameMap = getOptionNameMap().optionNameMap
(options).keys.foreach { fresh3 =>
val name = zeroOfMyType
 = fresh3
 {
 if (hasProperty( options, name )) {
 name match {
  case  "init" | "watch" | "version" | "help" | "project"  =>
  case _ =>
val value = options(name)
var optionDefinition = optionsNameMap(name.toLowerCase())
if (optionDefinition) {
 val customTypeMap = getCustomTypeMapOfCommandLineOption( optionDefinition )
if ((!customTypeMap)) {
 (result(name)=value)

}
else {
 if ((optionDefinition.`type`==="list")) {
 val convertedValue: Array[String] = Array()
(value.asInstanceOf[Array[( String | Int )]]).foreach { fresh4 =>
val element = zeroOfMyType
 = fresh4
 {
 convertedValue.push( getNameOfCompilerOptionValue( element, customTypeMap ) )

}
}
(result(name)=convertedValue)

}
else {
 (result(name)=getNameOfCompilerOptionValue( value, customTypeMap ))

}

}

}
}

}

}
}
return result

}

}
def removeComments(jsonText: String): String = {
 var output = ""
val scanner = createScanner( ScriptTarget.ES5, false, LanguageVariant.Standard, jsonText )
var token: SyntaxKind = zeroOfMyType
while ((((token=scanner.scan()))!==SyntaxKind.EndOfFileToken)) {
{
 token match {
  case  SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia  =>
(output+=scanner.getTokenText().replace( java.util.regex.Pattern.compile(raw"""\S""", "g"), " " ))
  case _ =>
(output+=scanner.getTokenText())
}

}
}
return output

}
def parseJsonConfigFileContent(json: Any, host: ParseConfigHost, basePath: String, existingOptions: CompilerOptions = Map(
), configFileName: String, resolutionStack: Array[Path] = Array()): ParsedCommandLine = {
 val errors: Array[Diagnostic] = Array()
val getCanonicalFileName = createGetCanonicalFileName( host.useCaseSensitiveFileNames )
val resolvedPath = toPath( (configFileName||""), basePath, getCanonicalFileName )
if ((resolutionStack.indexOf( resolvedPath )>=0)) {
 return Map( "options" -> Map(
),
"fileNames" -> Array(),
"typingOptions" -> Map(
),
"raw" -> json,
"errors" -> Array( createCompilerDiagnostic( Diagnostics.Circularity_detected_while_resolving_configuration_Colon_0, Array( resolutionStack: _*, resolvedPath ).join( " -> " ) ) ),
"wildcardDirectories" -> Map(
) )

}
var options: CompilerOptions = convertCompilerOptionsFromJsonWorker( json("compilerOptions"), basePath, errors, configFileName )
val typingOptions: TypingOptions = convertTypingOptionsFromJsonWorker( json("typingOptions"), basePath, errors, configFileName )
if (json("extends")) {
 var include: Array[String] = undefined
var exclude: Array[String] = undefined
var files: Array[String] = undefined
var baseOptions: CompilerOptions = Map(
)
if ((typeof(json("extends"))==="string")) {
 val arr = ((tryExtendsName( json("extends") )||Array( include, exclude, files, baseOptions )))
(include=arr(0))
(exclude=arr(1))
(files=arr(2))
(baseOptions=arr(3))

}
else {
 errors.push( createCompilerDiagnostic( Diagnostics.Compiler_option_0_requires_a_value_of_type_1, "extends", "string" ) )

}
if ((include&&(!json("include")))) {
 (json("include")=include)

}
if ((exclude&&(!json("exclude")))) {
 (json("exclude")=exclude)

}
if ((files&&(!json("files")))) {
 (json("files")=files)

}
(options=assign( Map(
), baseOptions, options ))

}
(options=extend( existingOptions, options ))
(options.configFilePath=configFileName)
const fresh5 = getFileNames( errors )
val fileNames = fresh5.fileNames
val wildcardDirectories = fresh5.wildcardDirectories
val compileOnSave = convertCompileOnSaveOptionFromJson( json, basePath, errors )
return Map( "options" -> options,
"fileNames" -> fileNames,
"typingOptions" -> typingOptions,
"raw" -> json,
"errors" -> errors,
"wildcardDirectories" -> wildcardDirectories,
"compileOnSave" -> compileOnSave )
def tryExtendsName(extendedConfig: String): (Array[String], Array[String], Array[String], CompilerOptions) = {
 if ((!(((isRootedDiskPath( extendedConfig )||startsWith( normalizeSlashes( extendedConfig ), "./" ))||startsWith( normalizeSlashes( extendedConfig ), "../" ))))) {
 errors.push( createCompilerDiagnostic( Diagnostics.The_path_in_an_extends_options_must_be_relative_or_rooted ) )
return

}
var extendedConfigPath = toPath( extendedConfig, basePath, getCanonicalFileName )
if (((!host.fileExists( extendedConfigPath ))&&(!endsWith( extendedConfigPath, ".json" )))) {
 (extendedConfigPath=s"""${ extendedConfigPath}.json""" .asInstanceOf[Path])
if ((!host.fileExists( extendedConfigPath ))) {
 errors.push( createCompilerDiagnostic( Diagnostics.File_0_does_not_exist, extendedConfig ) )
return

}

}
val extendedResult = readConfigFile( extendedConfigPath, (path =>  host.readFile( path )) )
if (extendedResult.error) {
 errors.push( extendedResult.error )
return

}
val extendedDirname = getDirectoryPath( extendedConfigPath )
val relativeDifference = convertToRelativePath( extendedDirname, basePath, getCanonicalFileName )
val updatePath: ((String) => String) = (path =>  (if (isRootedDiskPath( path )) path else combinePaths( relativeDifference, path )))
val result = parseJsonConfigFileContent( extendedResult.config, host, extendedDirname, undefined, getBaseFileName( extendedConfigPath ), resolutionStack.concat( Array( resolvedPath ) ) )
errors.push( result.errors: _* )
val triple = map( Array( "include", "exclude", "files" ), (key =>  {
 if (((!json(key))&&extendedResult.config(key))) {
 return map( extendedResult.config(key), updatePath )

}

}) )
val include = triple(0)
val exclude = triple(1)
val files = triple(2)
return Array( include, exclude, files, result.options )

}
def getFileNames(errors: Array[Diagnostic]): ExpandResult = {
 var fileNames: Array[String] = zeroOfMyType
if (hasProperty( json, "files" )) {
 if (isArray( json("files") )) {
 (fileNames=json("files").asInstanceOf[Array[String]])
if ((fileNames.length===0)) {
 errors.push( createCompilerDiagnostic( Diagnostics.The_files_list_in_config_file_0_is_empty, (configFileName||"tsconfig.json") ) )

}

}
else {
 errors.push( createCompilerDiagnostic( Diagnostics.Compiler_option_0_requires_a_value_of_type_1, "files", "Array" ) )

}

}
var includeSpecs: Array[String] = zeroOfMyType
if (hasProperty( json, "include" )) {
 if (isArray( json("include") )) {
 (includeSpecs=json("include").asInstanceOf[Array[String]])

}
else {
 errors.push( createCompilerDiagnostic( Diagnostics.Compiler_option_0_requires_a_value_of_type_1, "include", "Array" ) )

}

}
var excludeSpecs: Array[String] = zeroOfMyType
if (hasProperty( json, "exclude" )) {
 if (isArray( json("exclude") )) {
 (excludeSpecs=json("exclude").asInstanceOf[Array[String]])

}
else {
 errors.push( createCompilerDiagnostic( Diagnostics.Compiler_option_0_requires_a_value_of_type_1, "exclude", "Array" ) )

}

}
else if (hasProperty( json, "excludes" )) {
 errors.push( createCompilerDiagnostic( Diagnostics.Unknown_option_excludes_Did_you_mean_exclude ) )

}
else {
 (excludeSpecs=Array( "node_modules", "bower_components", "jspm_packages" ))
val outDir = (json("compilerOptions")&&json("compilerOptions")("outDir"))
if (outDir) {
 excludeSpecs.push( outDir )

}

}
if (((fileNames===undefined)&&(includeSpecs===undefined))) {
 (includeSpecs=Array( "**/*" ))

}
val result = matchFileNames( fileNames, includeSpecs, excludeSpecs, basePath, options, host, errors )
if ((((result.fileNames.length===0)&&(!hasProperty( json, "files" )))&&(resolutionStack.length===0))) {
 errors.push( createCompilerDiagnostic( Diagnostics.No_inputs_were_found_in_config_file_0_Specified_include_paths_were_1_and_exclude_paths_were_2, (configFileName||"tsconfig.json"), JSON.stringify( (includeSpecs||Array()) ), JSON.stringify( (excludeSpecs||Array()) ) ) )

}
return result

}

}
def convertCompileOnSaveOptionFromJson(jsonOption: Any, basePath: String, errors: Array[Diagnostic]): Boolean = {
 if ((!hasProperty( jsonOption, compileOnSaveCommandLineOption.name ))) {
 return false

}
val result = convertJsonOption( compileOnSaveCommandLineOption, jsonOption("compileOnSave"), basePath, errors )
if (((typeof(result)==="boolean")&&result)) {
 return result

}
return false

}
def convertCompilerOptionsFromJson(jsonOptions: Any, basePath: String, configFileName: String): {   var options: CompilerOptions
  var errors: Array[Diagnostic]
 } = {
 val errors: Array[Diagnostic] = Array()
val options = convertCompilerOptionsFromJsonWorker( jsonOptions, basePath, errors, configFileName )
return Map( "options" -> options,
"errors" -> errors )

}
def convertTypingOptionsFromJson(jsonOptions: Any, basePath: String, configFileName: String): {   var options: TypingOptions
  var errors: Array[Diagnostic]
 } = {
 val errors: Array[Diagnostic] = Array()
val options = convertTypingOptionsFromJsonWorker( jsonOptions, basePath, errors, configFileName )
return Map( "options" -> options,
"errors" -> errors )

}
def convertCompilerOptionsFromJsonWorker(jsonOptions: Any, basePath: String, errors: Array[Diagnostic], configFileName: String): CompilerOptions = {
 val options: CompilerOptions = (if ((getBaseFileName( configFileName )==="jsconfig.json")) Map( "allowJs" -> true,
"maxNodeModuleJsDepth" -> 2,
"allowSyntheticDefaultImports" -> true,
"skipLibCheck" -> true ) else Map(
))
convertOptionsFromJson( optionDeclarations, jsonOptions, basePath, options, Diagnostics.Unknown_compiler_option_0, errors )
return options

}
def convertTypingOptionsFromJsonWorker(jsonOptions: Any, basePath: String, errors: Array[Diagnostic], configFileName: String): TypingOptions = {
 val options: TypingOptions = (if ((getBaseFileName( configFileName )==="jsconfig.json")) Map( "enableAutoDiscovery" -> true,
"include" -> Array(),
"exclude" -> Array() ) else Map( "enableAutoDiscovery" -> false,
"include" -> Array(),
"exclude" -> Array() ))
convertOptionsFromJson( typingOptionDeclarations, jsonOptions, basePath, options, Diagnostics.Unknown_typing_option_0, errors )
return options

}
def convertOptionsFromJson(optionDeclarations: Array[CommandLineOption], jsonOptions: Any, basePath: String, defaultOptions: ( CompilerOptions | TypingOptions ), diagnosticMessage: DiagnosticMessage, errors: Array[Diagnostic]) = {
 if ((!jsonOptions)) {
 return

}
val optionNameMap = arrayToMap( optionDeclarations, (opt =>  opt.name) )
(jsonOptions).keys.foreach { fresh6 =>
val id = zeroOfMyType
 = fresh6
 {
 if ((idinoptionNameMap)) {
 val opt = optionNameMap(id)
(defaultOptions(opt.name)=convertJsonOption( opt, jsonOptions(id), basePath, errors ))

}
else {
 errors.push( createCompilerDiagnostic( diagnosticMessage, id ) )

}

}
}

}
def convertJsonOption(opt: CommandLineOption, value: Any, basePath: String, errors: Array[Diagnostic]): CompilerOptionsValue = {
 val optType = opt.`type`
val expectedType = (if ((typeof(optType)==="string")) optType else "string")
if (((optType==="list")&&isArray( value ))) {
 return convertJsonOptionOfListType( opt.asInstanceOf[CommandLineOptionOfListType], value, basePath, errors )

}
else if ((typeof(value)===expectedType)) {
 if ((typeof(optType)!=="string")) {
 return convertJsonOptionOfCustomType( opt.asInstanceOf[CommandLineOptionOfCustomType], value, errors )

}
else {
 if (opt.isFilePath) {
 (value=normalizePath( combinePaths( basePath, value ) ))
if ((value==="")) {
 (value=".")

}

}

}
return value

}
else {
 errors.push( createCompilerDiagnostic( Diagnostics.Compiler_option_0_requires_a_value_of_type_1, opt.name, expectedType ) )

}

}
def convertJsonOptionOfCustomType(opt: CommandLineOptionOfCustomType, value: String, errors: Array[Diagnostic]) = {
 val key = value.toLowerCase()
if ((keyinopt.`type`)) {
 return opt.`type`(key)

}
else {
 errors.push( createCompilerDiagnosticForInvalidCustomType( opt ) )

}

}
def convertJsonOptionOfListType(option: CommandLineOptionOfListType, values: Array[Any], basePath: String, errors: Array[Diagnostic]): Array[Any] = {
 return filter( map( values, (v =>  convertJsonOption( option.element, v, basePath, errors )) ), (v =>  (!(!v))) )

}
def trimString(s: String) = {
 return (if ((typeof(s.trim)==="function")) s.trim() else s.replace( java.util.regex.Pattern.compile(raw"""^[\s]+|[\s]+$$""", "g"), "" ))

}
val invalidTrailingRecursionPattern = java.util.regex.Pattern.compile(raw"""(^|\/)\*\*\/?$$""")
val invalidMultipleRecursionPatterns = java.util.regex.Pattern.compile(raw"""(^|\/)\*\*\/(.*\/)?\*\*($$|\/)""")
val invalidDotDotAfterRecursiveWildcardPattern = java.util.regex.Pattern.compile(raw"""(^|\/)\*\*\/(.*\/)?\.\.($$|\/)""")
val watchRecursivePattern = java.util.regex.Pattern.compile(raw"""\/[^/]*?[*?][^/]*\/""")
val wildcardDirectoryPattern = java.util.regex.Pattern.compile(raw"""^[^*?]*(?=\/[^/]*[*?])""")
def matchFileNames(fileNames: Array[String], include: Array[String], exclude: Array[String], basePath: String, options: CompilerOptions, host: ParseConfigHost, errors: Array[Diagnostic]): ExpandResult = {
 (basePath=normalizePath( basePath ))
val keyMapper = (if (host.useCaseSensitiveFileNames) caseSensitiveKeyMapper else caseInsensitiveKeyMapper)
val literalFileMap = createMap[ String ]()
val wildcardFileMap = createMap[ String ]()
if (include) {
 (include=validateSpecs( include, errors, false ))

}
if (exclude) {
 (exclude=validateSpecs( exclude, errors, true ))

}
val wildcardDirectories: Map[WatchDirectoryFlags] = getWildcardDirectories( include, exclude, basePath, host.useCaseSensitiveFileNames )
val supportedExtensions = getSupportedExtensions( options )
if (fileNames) {
 (fileNames).foreach { fresh7 =>
val fileName = zeroOfMyType
 = fresh7
 {
 val file = combinePaths( basePath, fileName )
(literalFileMap(keyMapper( file ))=file)

}
}

}
if ((include&&(include.length>0))) {
 (host.readDirectory( basePath, supportedExtensions, exclude, include )).foreach { fresh8 =>
val file = zeroOfMyType
 = fresh8
 {
 if (hasFileWithHigherPriorityExtension( file, literalFileMap, wildcardFileMap, supportedExtensions, keyMapper )) {
 continue

}
removeWildcardFilesWithLowerPriorityExtension( file, wildcardFileMap, supportedExtensions, keyMapper )
val key = keyMapper( file )
if (((!((keyinliteralFileMap)))&&(!((keyinwildcardFileMap))))) {
 (wildcardFileMap(key)=file)

}

}
}

}
val literalFiles = reduceProperties( literalFileMap, addFileToOutput, Array() )
val wildcardFiles = reduceProperties( wildcardFileMap, addFileToOutput, Array() )
wildcardFiles.sort( (if (host.useCaseSensitiveFileNames) compareStrings else compareStringsCaseInsensitive) )
return Map( "fileNames" -> literalFiles.concat( wildcardFiles ),
"wildcardDirectories" -> wildcardDirectories )

}
def validateSpecs(specs: Array[String], errors: Array[Diagnostic], allowTrailingRecursion: Boolean) = {
 val validSpecs: Array[String] = Array()
(specs).foreach { fresh9 =>
val spec = zeroOfMyType
 = fresh9
 {
 if (((!allowTrailingRecursion)&&invalidTrailingRecursionPattern.test( spec ))) {
 errors.push( createCompilerDiagnostic( Diagnostics.File_specification_cannot_end_in_a_recursive_directory_wildcard_Asterisk_Asterisk_Colon_0, spec ) )

}
else if (invalidMultipleRecursionPatterns.test( spec )) {
 errors.push( createCompilerDiagnostic( Diagnostics.File_specification_cannot_contain_multiple_recursive_directory_wildcards_Asterisk_Asterisk_Colon_0, spec ) )

}
else if (invalidDotDotAfterRecursiveWildcardPattern.test( spec )) {
 errors.push( createCompilerDiagnostic( Diagnostics.File_specification_cannot_contain_a_parent_directory_that_appears_after_a_recursive_directory_wildcard_Asterisk_Asterisk_Colon_0, spec ) )

}
else {
 validSpecs.push( spec )

}

}
}
return validSpecs

}
def getWildcardDirectories(include: Array[String], exclude: Array[String], path: String, useCaseSensitiveFileNames: Boolean) = {
 val rawExcludeRegex = getRegularExpressionForWildcard( exclude, path, "exclude" )
val excludeRegex = (rawExcludeRegex&&new RegExp( rawExcludeRegex, (if (useCaseSensitiveFileNames) "" else "i") ))
val wildcardDirectories = createMap[ WatchDirectoryFlags ]()
if ((include!==undefined)) {
 val recursiveKeys: Array[String] = Array()
(include).foreach { fresh10 =>
val file = zeroOfMyType
 = fresh10
 {
 val name = normalizePath( combinePaths( path, file ) )
if ((excludeRegex&&excludeRegex.test( name ))) {
 continue

}
val `match` = wildcardDirectoryPattern.exec( name )
if (`match`) {
 val key = (if (useCaseSensitiveFileNames) `match`(0) else `match`(0).toLowerCase())
val flags = (if (watchRecursivePattern.test( name )) WatchDirectoryFlags.Recursive else WatchDirectoryFlags.None)
val existingFlags = wildcardDirectories(key)
if (((existingFlags===undefined)||(existingFlags<flags))) {
 (wildcardDirectories(key)=flags)
if ((flags===WatchDirectoryFlags.Recursive)) {
 recursiveKeys.push( key )

}

}

}

}
}
(wildcardDirectories).keys.foreach { fresh11 =>
val key = zeroOfMyType
 = fresh11
 {
 (recursiveKeys).foreach { fresh12 =>
val recursiveKey = zeroOfMyType
 = fresh12
 {
 if (((key!==recursiveKey)&&containsPath( recursiveKey, key, path, (!useCaseSensitiveFileNames) ))) {
 wildcardDirectories.remove(key)

}

}
}

}
}

}
return wildcardDirectories

}
def hasFileWithHigherPriorityExtension(file: String, literalFiles: Map[String], wildcardFiles: Map[String], extensions: Array[String], keyMapper: ((String) => String)) = {
 val extensionPriority = getExtensionPriority( file, extensions )
val adjustedExtensionPriority = adjustExtensionPriority( extensionPriority )
{
var i = ExtensionPriority.Highest
while( (i<adjustedExtensionPriority)) {
 {
 val higherPriorityExtension = extensions(i)
val higherPriorityPath = keyMapper( changeExtension( file, higherPriorityExtension ) )
if (((higherPriorityPathinliteralFiles)||(higherPriorityPathinwildcardFiles))) {
 return true

}

}
 (i+= 1)
}
}
return false

}
def removeWildcardFilesWithLowerPriorityExtension(file: String, wildcardFiles: Map[String], extensions: Array[String], keyMapper: ((String) => String)) = {
 val extensionPriority = getExtensionPriority( file, extensions )
val nextExtensionPriority = getNextLowestExtensionPriority( extensionPriority )
{
var i = nextExtensionPriority
while( (i<extensions.length)) {
 {
 val lowerPriorityExtension = extensions(i)
val lowerPriorityPath = keyMapper( changeExtension( file, lowerPriorityExtension ) )
wildcardFiles.remove(lowerPriorityPath)

}
 (i+= 1)
}
}

}
def addFileToOutput(output: Array[String], file: String) = {
 output.push( file )
return output

}
def caseSensitiveKeyMapper(key: String) = {
 return key

}
def caseInsensitiveKeyMapper(key: String) = {
 return key.toLowerCase()

}
}
