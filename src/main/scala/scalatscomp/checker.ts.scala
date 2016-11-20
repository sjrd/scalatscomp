package scalatscomp
object Checker {
val ambientModuleSymbolRegex = java.util.regex.Pattern.compile(raw"""^".+"$$""")
var nextSymbolId = 1
var nextNodeId = 1
var nextMergeId = 1
var nextFlowId = 1
def getNodeId(node: Node): Int = {
 if ((!node.id)) {
 (node.id=nextNodeId)
(nextNodeId+= 1)

}
return node.id

}
def getSymbolId(symbol: Symbol): Int = {
 if ((!symbol.id)) {
 (symbol.id=nextSymbolId)
(nextSymbolId+= 1)

}
return symbol.id

}
def createTypeChecker(host: TypeCheckerHost, produceDiagnostics: Boolean): TypeChecker = {
 var cancellationToken: CancellationToken = zeroOfMyType
val Symbol = objectAllocator.getSymbolConstructor()
val Type = objectAllocator.getTypeConstructor()
val Signature = objectAllocator.getSignatureConstructor()
var typeCount = 0
var symbolCount = 0
val emptyArray: Array[Any] = Array()
val emptySymbols = createMap[ Symbol ]()
val compilerOptions = host.getCompilerOptions()
val languageVersion = (compilerOptions.target||ScriptTarget.ES3)
val modulekind = getEmitModuleKind( compilerOptions )
val noUnusedIdentifiers = ((!(!compilerOptions.noUnusedLocals))||(!(!compilerOptions.noUnusedParameters)))
val allowSyntheticDefaultImports = (if ((typeof(compilerOptions.allowSyntheticDefaultImports)!=="undefined")) compilerOptions.allowSyntheticDefaultImports else (modulekind===ModuleKind.System))
val strictNullChecks = compilerOptions.strictNullChecks
val emitResolver = createResolver()
val undefinedSymbol = createSymbol( (SymbolFlags.Property|SymbolFlags.Transient), "undefined" )
(undefinedSymbol.declarations=Array())
val argumentsSymbol = createSymbol( (SymbolFlags.Property|SymbolFlags.Transient), "arguments" )
val checker: TypeChecker = Map( "getNodeCount" -> (() =>  sum( host.getSourceFiles(), "nodeCount" )),
"getIdentifierCount" -> (() =>  sum( host.getSourceFiles(), "identifierCount" )),
"getSymbolCount" -> (() =>  (sum( host.getSourceFiles(), "symbolCount" )+symbolCount)),
"getTypeCount" -> (() =>  typeCount),
"isUndefinedSymbol" -> (symbol =>  (symbol===undefinedSymbol)),
"isArgumentsSymbol" -> (symbol =>  (symbol===argumentsSymbol)),
"isUnknownSymbol" -> (symbol =>  (symbol===unknownSymbol)),
"getDiagnostics" -> getDiagnostics,
"getGlobalDiagnostics" -> getGlobalDiagnostics,
"getTypeOfSymbolAtLocation" -> getTypeOfSymbolAtLocation,
"getSymbolsOfParameterPropertyDeclaration" -> getSymbolsOfParameterPropertyDeclaration,
"getDeclaredTypeOfSymbol" -> getDeclaredTypeOfSymbol,
"getPropertiesOfType" -> getPropertiesOfType,
"getPropertyOfType" -> getPropertyOfType,
"getSignaturesOfType" -> getSignaturesOfType,
"getIndexTypeOfType" -> getIndexTypeOfType,
"getBaseTypes" -> getBaseTypes,
"getReturnTypeOfSignature" -> getReturnTypeOfSignature,
"getNonNullableType" -> getNonNullableType,
"getSymbolsInScope" -> getSymbolsInScope,
"getSymbolAtLocation" -> getSymbolAtLocation,
"getShorthandAssignmentValueSymbol" -> getShorthandAssignmentValueSymbol,
"getExportSpecifierLocalTargetSymbol" -> getExportSpecifierLocalTargetSymbol,
"getTypeAtLocation" -> getTypeOfNode,
"getPropertySymbolOfDestructuringAssignment" -> getPropertySymbolOfDestructuringAssignment,
"typeToString" -> typeToString,
"getSymbolDisplayBuilder" -> getSymbolDisplayBuilder,
"symbolToString" -> symbolToString,
"getAugmentedPropertiesOfType" -> getAugmentedPropertiesOfType,
"getRootSymbols" -> getRootSymbols,
"getContextualType" -> getContextualType,
"getFullyQualifiedName" -> getFullyQualifiedName,
"getResolvedSignature" -> getResolvedSignature,
"getConstantValue" -> getConstantValue,
"isValidPropertyAccess" -> isValidPropertyAccess,
"getSignatureFromDeclaration" -> getSignatureFromDeclaration,
"isImplementationOfOverload" -> isImplementationOfOverload,
"getAliasedSymbol" -> resolveAlias,
"getEmitResolver" -> getEmitResolver,
"getExportsOfModule" -> getExportsOfModuleAsArray,
"getAmbientModules" -> getAmbientModules,
"getJsxElementAttributesType" -> getJsxElementAttributesType,
"getJsxIntrinsicTagNames" -> getJsxIntrinsicTagNames,
"isOptionalParameter" -> isOptionalParameter )
val tupleTypes: Array[GenericType] = Array()
val unionTypes = createMap[ UnionType ]()
val intersectionTypes = createMap[ IntersectionType ]()
val stringLiteralTypes = createMap[ LiteralType ]()
val numericLiteralTypes = createMap[ LiteralType ]()
val evolvingArrayTypes: Array[EvolvingArrayType] = Array()
val unknownSymbol = createSymbol( (SymbolFlags.Property|SymbolFlags.Transient), "unknown" )
val resolvingSymbol = createSymbol( SymbolFlags.Transient, "__resolving__" )
val anyType = createIntrinsicType( TypeFlags.Any, "any" )
val autoType = createIntrinsicType( TypeFlags.Any, "any" )
val unknownType = createIntrinsicType( TypeFlags.Any, "unknown" )
val undefinedType = createIntrinsicType( TypeFlags.Undefined, "undefined" )
val undefinedWideningType = (if (strictNullChecks) undefinedType else createIntrinsicType( (TypeFlags.Undefined|TypeFlags.ContainsWideningType), "undefined" ))
val nullType = createIntrinsicType( TypeFlags.Null, "null" )
val nullWideningType = (if (strictNullChecks) nullType else createIntrinsicType( (TypeFlags.Null|TypeFlags.ContainsWideningType), "null" ))
val stringType = createIntrinsicType( TypeFlags.String, "string" )
val numberType = createIntrinsicType( TypeFlags.Number, "number" )
val trueType = createIntrinsicType( TypeFlags.BooleanLiteral, "true" )
val falseType = createIntrinsicType( TypeFlags.BooleanLiteral, "false" )
val booleanType = createBooleanType( Array( trueType, falseType ) )
val esSymbolType = createIntrinsicType( TypeFlags.ESSymbol, "symbol" )
val voidType = createIntrinsicType( TypeFlags.Void, "void" )
val neverType = createIntrinsicType( TypeFlags.Never, "never" )
val silentNeverType = createIntrinsicType( TypeFlags.Never, "never" )
val emptyObjectType = createAnonymousType( undefined, emptySymbols, emptyArray, emptyArray, undefined, undefined )
val emptyGenericType = createAnonymousType( undefined, emptySymbols, emptyArray, emptyArray, undefined, undefined ).asInstanceOf[ObjectType].asInstanceOf[GenericType]
(emptyGenericType.instantiations=createMap[ TypeReference ]())
val anyFunctionType = createAnonymousType( undefined, emptySymbols, emptyArray, emptyArray, undefined, undefined )
(anyFunctionType.flags|=TypeFlags.ContainsAnyFunctionType)
val noConstraintType = createAnonymousType( undefined, emptySymbols, emptyArray, emptyArray, undefined, undefined )
val anySignature = createSignature( undefined, undefined, undefined, emptyArray, anyType, undefined, 0, false, false )
val unknownSignature = createSignature( undefined, undefined, undefined, emptyArray, unknownType, undefined, 0, false, false )
val resolvingSignature = createSignature( undefined, undefined, undefined, emptyArray, anyType, undefined, 0, false, false )
val silentNeverSignature = createSignature( undefined, undefined, undefined, emptyArray, silentNeverType, undefined, 0, false, false )
val enumNumberIndexInfo = createIndexInfo( stringType, true )
val globals = createMap[ Symbol ]()
var patternAmbientModules: Array[PatternAmbientModule] = zeroOfMyType
var getGlobalESSymbolConstructorSymbol: (() => Symbol) = zeroOfMyType
var getGlobalPromiseConstructorSymbol: (() => Symbol) = zeroOfMyType
var tryGetGlobalPromiseConstructorSymbol: (() => Symbol) = zeroOfMyType
var globalObjectType: ObjectType = zeroOfMyType
var globalFunctionType: ObjectType = zeroOfMyType
var globalArrayType: GenericType = zeroOfMyType
var globalReadonlyArrayType: GenericType = zeroOfMyType
var globalStringType: ObjectType = zeroOfMyType
var globalNumberType: ObjectType = zeroOfMyType
var globalBooleanType: ObjectType = zeroOfMyType
var globalRegExpType: ObjectType = zeroOfMyType
var anyArrayType: Type = zeroOfMyType
var autoArrayType: Type = zeroOfMyType
var anyReadonlyArrayType: Type = zeroOfMyType
var getGlobalTemplateStringsArrayType: (() => ObjectType) = zeroOfMyType
var getGlobalESSymbolType: (() => ObjectType) = zeroOfMyType
var getGlobalIterableType: (() => GenericType) = zeroOfMyType
var getGlobalIteratorType: (() => GenericType) = zeroOfMyType
var getGlobalIterableIteratorType: (() => GenericType) = zeroOfMyType
var getGlobalClassDecoratorType: (() => ObjectType) = zeroOfMyType
var getGlobalParameterDecoratorType: (() => ObjectType) = zeroOfMyType
var getGlobalPropertyDecoratorType: (() => ObjectType) = zeroOfMyType
var getGlobalMethodDecoratorType: (() => ObjectType) = zeroOfMyType
var getGlobalTypedPropertyDescriptorType: (() => ObjectType) = zeroOfMyType
var getGlobalPromiseType: (() => ObjectType) = zeroOfMyType
var tryGetGlobalPromiseType: (() => ObjectType) = zeroOfMyType
var getGlobalPromiseLikeType: (() => ObjectType) = zeroOfMyType
var getInstantiatedGlobalPromiseLikeType: (() => ObjectType) = zeroOfMyType
var getGlobalPromiseConstructorLikeType: (() => ObjectType) = zeroOfMyType
var getGlobalThenableType: (() => ObjectType) = zeroOfMyType
var jsxElementClassType: Type = zeroOfMyType
var deferredNodes: Array[Node] = zeroOfMyType
var deferredUnusedIdentifierNodes: Array[Node] = zeroOfMyType
var flowLoopStart = 0
var flowLoopCount = 0
var visitedFlowCount = 0
val emptyStringType = getLiteralTypeForText( TypeFlags.StringLiteral, "" )
val zeroType = getLiteralTypeForText( TypeFlags.NumberLiteral, "0" )
val resolutionTargets: Array[TypeSystemEntity] = Array()
val resolutionResults: Array[Boolean] = Array()
val resolutionPropertyNames: Array[TypeSystemPropertyName] = Array()
val mergedSymbols: Array[Symbol] = Array()
val symbolLinks: Array[SymbolLinks] = Array()
val nodeLinks: Array[NodeLinks] = Array()
val flowLoopCaches: Array[Map[Type]] = Array()
val flowLoopNodes: Array[FlowNode] = Array()
val flowLoopKeys: Array[String] = Array()
val flowLoopTypes: Array[Array[Type]] = Array()
val visitedFlowNodes: Array[FlowNode] = Array()
val visitedFlowTypes: Array[FlowType] = Array()
val potentialThisCollisions: Array[Node] = Array()
val awaitedTypeStack: Array[Int] = Array()
val diagnostics = createDiagnosticCollection()
sealed abstract class TypeFacts
object TypeFacts {
   case object None extends TypeFacts
  case object TypeofEQString extends TypeFacts
  case object TypeofEQNumber extends TypeFacts
  case object TypeofEQBoolean extends TypeFacts
  case object TypeofEQSymbol extends TypeFacts
  case object TypeofEQObject extends TypeFacts
  case object TypeofEQFunction extends TypeFacts
  case object TypeofEQHostObject extends TypeFacts
  case object TypeofNEString extends TypeFacts
  case object TypeofNENumber extends TypeFacts
  case object TypeofNEBoolean extends TypeFacts
  case object TypeofNESymbol extends TypeFacts
  case object TypeofNEObject extends TypeFacts
  case object TypeofNEFunction extends TypeFacts
  case object TypeofNEHostObject extends TypeFacts
  case object EQUndefined extends TypeFacts
  case object EQNull extends TypeFacts
  case object EQUndefinedOrNull extends TypeFacts
  case object NEUndefined extends TypeFacts
  case object NENull extends TypeFacts
  case object NEUndefinedOrNull extends TypeFacts
  case object Truthy extends TypeFacts
  case object Falsy extends TypeFacts
  case object Discriminatable extends TypeFacts
  case object All extends TypeFacts
  case object BaseStringStrictFacts extends TypeFacts
  case object BaseStringFacts extends TypeFacts
  case object StringStrictFacts extends TypeFacts
  case object StringFacts extends TypeFacts
  case object EmptyStringStrictFacts extends TypeFacts
  case object EmptyStringFacts extends TypeFacts
  case object NonEmptyStringStrictFacts extends TypeFacts
  case object NonEmptyStringFacts extends TypeFacts
  case object BaseNumberStrictFacts extends TypeFacts
  case object BaseNumberFacts extends TypeFacts
  case object NumberStrictFacts extends TypeFacts
  case object NumberFacts extends TypeFacts
  case object ZeroStrictFacts extends TypeFacts
  case object ZeroFacts extends TypeFacts
  case object NonZeroStrictFacts extends TypeFacts
  case object NonZeroFacts extends TypeFacts
  case object BaseBooleanStrictFacts extends TypeFacts
  case object BaseBooleanFacts extends TypeFacts
  case object BooleanStrictFacts extends TypeFacts
  case object BooleanFacts extends TypeFacts
  case object FalseStrictFacts extends TypeFacts
  case object FalseFacts extends TypeFacts
  case object TrueStrictFacts extends TypeFacts
  case object TrueFacts extends TypeFacts
  case object SymbolStrictFacts extends TypeFacts
  case object SymbolFacts extends TypeFacts
  case object ObjectStrictFacts extends TypeFacts
  case object ObjectFacts extends TypeFacts
  case object FunctionStrictFacts extends TypeFacts
  case object FunctionFacts extends TypeFacts
  case object UndefinedFacts extends TypeFacts
  case object NullFacts extends TypeFacts
}
val typeofEQFacts = createMap( Map( "string" -> TypeFacts.TypeofEQString,
"number" -> TypeFacts.TypeofEQNumber,
"boolean" -> TypeFacts.TypeofEQBoolean,
"symbol" -> TypeFacts.TypeofEQSymbol,
"undefined" -> TypeFacts.EQUndefined,
"object" -> TypeFacts.TypeofEQObject,
"function" -> TypeFacts.TypeofEQFunction ) )
val typeofNEFacts = createMap( Map( "string" -> TypeFacts.TypeofNEString,
"number" -> TypeFacts.TypeofNENumber,
"boolean" -> TypeFacts.TypeofNEBoolean,
"symbol" -> TypeFacts.TypeofNESymbol,
"undefined" -> TypeFacts.NEUndefined,
"object" -> TypeFacts.TypeofNEObject,
"function" -> TypeFacts.TypeofNEFunction ) )
val typeofTypesByName = createMap[ Type ]( Map( "string" -> stringType,
"number" -> numberType,
"boolean" -> booleanType,
"symbol" -> esSymbolType,
"undefined" -> undefinedType ) )
var jsxElementType: Type = zeroOfMyType
val jsxTypes = createMap[ Type ]()
val JsxNames = Map( "JSX" -> "JSX",
"IntrinsicElements" -> "IntrinsicElements",
"ElementClass" -> "ElementClass",
"ElementAttributesPropertyNameContainer" -> "ElementAttributesProperty",
"Element" -> "Element",
"IntrinsicAttributes" -> "IntrinsicAttributes",
"IntrinsicClassAttributes" -> "IntrinsicClassAttributes" )
val subtypeRelation = createMap[ RelationComparisonResult ]()
val assignableRelation = createMap[ RelationComparisonResult ]()
val comparableRelation = createMap[ RelationComparisonResult ]()
val identityRelation = createMap[ RelationComparisonResult ]()
val enumRelation = createMap[ Boolean ]()
var _displayBuilder: SymbolDisplayBuilder = zeroOfMyType
type TypeSystemEntity = ( Symbol | Type | Signature )
sealed abstract class TypeSystemPropertyName
object TypeSystemPropertyName {
   case object Type extends TypeSystemPropertyName
  case object ResolvedBaseConstructorType extends TypeSystemPropertyName
  case object DeclaredType extends TypeSystemPropertyName
  case object ResolvedReturnType extends TypeSystemPropertyName
}
val builtinGlobals = createMap[ Symbol ]()
(builtinGlobals(undefinedSymbol.name)=undefinedSymbol)
initializeTypeChecker()
return checker
def getEmitResolver(sourceFile: SourceFile, cancellationToken: CancellationToken) = {
 getDiagnostics( sourceFile, cancellationToken )
return emitResolver

}
def error(location: Node, message: DiagnosticMessage, arg0: ( String | Int ), arg1: ( String | Int ), arg2: ( String | Int )): Unit = {
 val diagnostic = (if (location) createDiagnosticForNode( location, message, arg0, arg1, arg2 ) else createCompilerDiagnostic( message, arg0, arg1, arg2 ))
diagnostics.add( diagnostic )

}
def createSymbol(flags: SymbolFlags, name: String): Symbol = {
 (symbolCount+= 1)
return new Symbol( flags, name )

}
def getExcludedSymbolFlags(flags: SymbolFlags): SymbolFlags = {
 var result: SymbolFlags = 0
if ((flags&SymbolFlags.BlockScopedVariable))
(result|=SymbolFlags.BlockScopedVariableExcludes)
if ((flags&SymbolFlags.FunctionScopedVariable))
(result|=SymbolFlags.FunctionScopedVariableExcludes)
if ((flags&SymbolFlags.Property))
(result|=SymbolFlags.PropertyExcludes)
if ((flags&SymbolFlags.EnumMember))
(result|=SymbolFlags.EnumMemberExcludes)
if ((flags&SymbolFlags.Function))
(result|=SymbolFlags.FunctionExcludes)
if ((flags&SymbolFlags.Class))
(result|=SymbolFlags.ClassExcludes)
if ((flags&SymbolFlags.Interface))
(result|=SymbolFlags.InterfaceExcludes)
if ((flags&SymbolFlags.RegularEnum))
(result|=SymbolFlags.RegularEnumExcludes)
if ((flags&SymbolFlags.ConstEnum))
(result|=SymbolFlags.ConstEnumExcludes)
if ((flags&SymbolFlags.ValueModule))
(result|=SymbolFlags.ValueModuleExcludes)
if ((flags&SymbolFlags.Method))
(result|=SymbolFlags.MethodExcludes)
if ((flags&SymbolFlags.GetAccessor))
(result|=SymbolFlags.GetAccessorExcludes)
if ((flags&SymbolFlags.SetAccessor))
(result|=SymbolFlags.SetAccessorExcludes)
if ((flags&SymbolFlags.TypeParameter))
(result|=SymbolFlags.TypeParameterExcludes)
if ((flags&SymbolFlags.TypeAlias))
(result|=SymbolFlags.TypeAliasExcludes)
if ((flags&SymbolFlags.Alias))
(result|=SymbolFlags.AliasExcludes)
return result

}
def recordMergedSymbol(target: Symbol, source: Symbol) = {
 if ((!source.mergeId)) {
 (source.mergeId=nextMergeId)
(nextMergeId+= 1)

}
(mergedSymbols(source.mergeId)=target)

}
def cloneSymbol(symbol: Symbol): Symbol = {
 val result = createSymbol( (symbol.flags|SymbolFlags.Merged), symbol.name )
(result.declarations=symbol.declarations.slice( 0 ))
(result.parent=symbol.parent)
if (symbol.valueDeclaration)
(result.valueDeclaration=symbol.valueDeclaration)
if (symbol.constEnumOnlyModule)
(result.constEnumOnlyModule=true)
if (symbol.members)
(result.members=cloneMap( symbol.members ))
if (symbol.exports)
(result.exports=cloneMap( symbol.exports ))
recordMergedSymbol( result, symbol )
return result

}
def mergeSymbol(target: Symbol, source: Symbol) = {
 if ((!((target.flags&getExcludedSymbolFlags( source.flags ))))) {
 if (((((source.flags&SymbolFlags.ValueModule)&&(target.flags&SymbolFlags.ValueModule))&&target.constEnumOnlyModule)&&(!source.constEnumOnlyModule))) {
 (target.constEnumOnlyModule=false)

}
(target.flags|=source.flags)
if ((source.valueDeclaration&&(((!target.valueDeclaration)||(((target.valueDeclaration.kind===SyntaxKind.ModuleDeclaration)&&(source.valueDeclaration.kind!==SyntaxKind.ModuleDeclaration))))))) {
 (target.valueDeclaration=source.valueDeclaration)

}
forEach( source.declarations, (node =>  {
 target.declarations.push( node )

}) )
if (source.members) {
 if ((!target.members))
(target.members=createMap[ Symbol ]())
mergeSymbolTable( target.members, source.members )

}
if (source.exports) {
 if ((!target.exports))
(target.exports=createMap[ Symbol ]())
mergeSymbolTable( target.exports, source.exports )

}
recordMergedSymbol( target, source )

}
else {
 val message = (if (((target.flags&SymbolFlags.BlockScopedVariable)||(source.flags&SymbolFlags.BlockScopedVariable))) Diagnostics.Cannot_redeclare_block_scoped_variable_0 else Diagnostics.Duplicate_identifier_0)
forEach( source.declarations, (node =>  {
 error( (if (node.name) node.name else node), message, symbolToString( source ) )

}) )
forEach( target.declarations, (node =>  {
 error( (if (node.name) node.name else node), message, symbolToString( source ) )

}) )

}

}
def mergeSymbolTable(target: SymbolTable, source: SymbolTable) = {
 (source).keys.foreach { fresh1 =>
val id = zeroOfMyType
 = fresh1
 {
 var targetSymbol = target(id)
if ((!targetSymbol)) {
 (target(id)=source(id))

}
else {
 if ((!((targetSymbol.flags&SymbolFlags.Merged)))) {
 (target(id)=(targetSymbol=cloneSymbol( targetSymbol )))

}
mergeSymbol( targetSymbol, source(id) )

}

}
}

}
def mergeModuleAugmentation(moduleName: LiteralExpression): Unit = {
 val moduleAugmentation = moduleName.parent.asInstanceOf[ModuleDeclaration]
if ((moduleAugmentation.symbol.declarations(0)!==moduleAugmentation)) {
 Debug.assert( (moduleAugmentation.symbol.declarations.length>1) )
return

}
if (isGlobalScopeAugmentation( moduleAugmentation )) {
 mergeSymbolTable( globals, moduleAugmentation.symbol.exports )

}
else {
 val moduleNotFoundError = (if ((!isInAmbientContext( moduleName.parent.parent ))) Diagnostics.Invalid_module_name_in_augmentation_module_0_cannot_be_found else undefined)
var mainModule = resolveExternalModuleNameWorker( moduleName, moduleName, moduleNotFoundError )
if ((!mainModule)) {
 return

}
(mainModule=resolveExternalModuleSymbol( mainModule ))
if ((mainModule.flags&SymbolFlags.Namespace)) {
 (mainModule=(if ((mainModule.flags&SymbolFlags.Merged)) mainModule else cloneSymbol( mainModule )))
mergeSymbol( mainModule, moduleAugmentation.symbol )

}
else {
 error( moduleName, Diagnostics.Cannot_augment_module_0_because_it_resolves_to_a_non_module_entity, moduleName.text )

}

}

}
def addToSymbolTable(target: SymbolTable, source: SymbolTable, message: DiagnosticMessage) = {
 (source).keys.foreach { fresh2 =>
val id = zeroOfMyType
 = fresh2
 {
 if (target(id)) {
 forEach( target(id).declarations, addDeclarationDiagnostic( id, message ) )

}
else {
 (target(id)=source(id))

}

}
}
def addDeclarationDiagnostic(id: String, message: DiagnosticMessage) = {
 return (( declaration: Declaration ) =>  diagnostics.add( createDiagnosticForNode( declaration, message, id ) ))

}

}
def getSymbolLinks(symbol: Symbol): SymbolLinks = {
 if ((symbol.flags&SymbolFlags.Transient))
return symbol.asInstanceOf[TransientSymbol]
val id = getSymbolId( symbol )
return (symbolLinks(id)||((symbolLinks(id)=Map(
))))

}
def getNodeLinks(node: Node): NodeLinks = {
 val nodeId = getNodeId( node )
return (nodeLinks(nodeId)||((nodeLinks(nodeId)=Map( "flags" -> 0 ))))

}
def getObjectFlags(`type`: Type): ObjectFlags = {
 return (if ((`type`.flags&TypeFlags.Object)) (`type`.asInstanceOf[ObjectType]).objectFlags else 0)

}
def isGlobalSourceFile(node: Node) = {
 return ((node.kind===SyntaxKind.SourceFile)&&(!isExternalOrCommonJsModule( node.asInstanceOf[SourceFile] )))

}
def getSymbol(symbols: SymbolTable, name: String, meaning: SymbolFlags): Symbol = {
 if (meaning) {
 val symbol = symbols(name)
if (symbol) {
 Debug.assert( (((symbol.flags&SymbolFlags.Instantiated))===0), "Should never get an instantiated symbol here." )
if ((symbol.flags&meaning)) {
 return symbol

}
if ((symbol.flags&SymbolFlags.Alias)) {
 val target = resolveAlias( symbol )
if (((target===unknownSymbol)||(target.flags&meaning))) {
 return symbol

}

}

}

}

}
def getSymbolsOfParameterPropertyDeclaration(parameter: ParameterDeclaration, parameterName: String): (Symbol, Symbol) = {
 val constructorDeclaration = parameter.parent
val classDeclaration = parameter.parent.parent
val parameterSymbol = getSymbol( constructorDeclaration.locals, parameterName, SymbolFlags.Value )
val propertySymbol = getSymbol( classDeclaration.symbol.members, parameterName, SymbolFlags.Value )
if ((parameterSymbol&&propertySymbol)) {
 return Array( parameterSymbol, propertySymbol )

}
Debug.fail( "There should exist two symbols, one as property declaration and one as parameter declaration" )

}
def isBlockScopedNameDeclaredBeforeUse(declaration: Declaration, usage: Node): Boolean = {
 val declarationFile = getSourceFileOfNode( declaration )
val useFile = getSourceFileOfNode( usage )
if ((declarationFile!==useFile)) {
 if ((((modulekind&&((declarationFile.externalModuleIndicator||useFile.externalModuleIndicator))))||(((!compilerOptions.outFile)&&(!compilerOptions.out))))) {
 return true

}
if (isUsedInFunctionOrNonStaticProperty( usage )) {
 return true

}
val sourceFiles = host.getSourceFiles()
return (indexOf( sourceFiles, declarationFile )<=indexOf( sourceFiles, useFile ))

}
if ((declaration.pos<=usage.pos)) {
 return ((declaration.kind!==SyntaxKind.VariableDeclaration)||(!isImmediatelyUsedInInitializerOfBlockScopedVariable( declaration.asInstanceOf[VariableDeclaration], usage )))

}
val container = getEnclosingBlockScopeContainer( declaration )
return isUsedInFunctionOrNonStaticProperty( usage, container )
def isImmediatelyUsedInInitializerOfBlockScopedVariable(declaration: VariableDeclaration, usage: Node): Boolean = {
 val container = getEnclosingBlockScopeContainer( declaration )
declaration.parent.parent.kind match {
  case  SyntaxKind.VariableStatement | SyntaxKind.ForStatement | SyntaxKind.ForOfStatement  =>
if (isSameScopeDescendentOf( usage, declaration, container )) {
 return true

}
  case _ =>
}
declaration.parent.parent.kind match {
  case  SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement  =>
if (isSameScopeDescendentOf( usage, (declaration.parent.parent.asInstanceOf[( ForInStatement | ForOfStatement )]).expression, container )) {
 return true

}
  case _ =>
}
return false

}
def isUsedInFunctionOrNonStaticProperty(usage: Node, container: Node): Boolean = {
 var current = usage
while (current) {
{
 if ((current===container)) {
 return false

}
if (isFunctionLike( current )) {
 return true

}
val initializerOfNonStaticProperty = (((current.parent&&(current.parent.kind===SyntaxKind.PropertyDeclaration))&&(((getModifierFlags( current.parent )&ModifierFlags.Static))===0))&&((current.parent.asInstanceOf[PropertyDeclaration]).initializer===current))
if (initializerOfNonStaticProperty) {
 return true

}
(current=current.parent)

}
}
return false

}

}
def resolveName(location: ( Node | undefined ), name: String, meaning: SymbolFlags, nameNotFoundMessage: DiagnosticMessage, nameArg: ( String | Identifier )): Symbol = {
 var result: Symbol = zeroOfMyType
var lastLocation: Node = zeroOfMyType
var propertyWithInvalidInitializer: Node = zeroOfMyType
val errorLocation = location
var grandparent: Node = zeroOfMyType
var isInExternalModule = false
val loop = new scala.util.control.Breaks
loop.breakable {
while (location) {
{
 if ((location.locals&&(!isGlobalSourceFile( location )))) {
 if ((result=getSymbol( location.locals, name, meaning ))) {
 var useResult = true
if (((isFunctionLike( location )&&lastLocation)&&(lastLocation!==(location.asInstanceOf[FunctionLikeDeclaration]).body))) {
 if ((((meaning&result.flags)&SymbolFlags.Type)&&(lastLocation.kind!==SyntaxKind.JSDocComment))) {
 (useResult=(if ((result.flags&SymbolFlags.TypeParameter)) (((lastLocation===(location.asInstanceOf[FunctionLikeDeclaration]).`type`)||(lastLocation.kind===SyntaxKind.Parameter))||(lastLocation.kind===SyntaxKind.TypeParameter)) else false))

}
if (((meaning&SymbolFlags.Value)&&(result.flags&SymbolFlags.FunctionScopedVariable))) {
 (useResult=((lastLocation.kind===SyntaxKind.Parameter)||(((lastLocation===(location.asInstanceOf[FunctionLikeDeclaration]).`type`)&&(result.valueDeclaration.kind===SyntaxKind.Parameter)))))

}

}
if (useResult) {
 loop.break()

}
else {
 (result=undefined)

}

}

}
location.kind match {
  case  SyntaxKind.SourceFile  =>
if ((!isExternalOrCommonJsModule( location.asInstanceOf[SourceFile] )))
break()
(isInExternalModule=true)
  case  SyntaxKind.ModuleDeclaration  =>
val moduleExports = getSymbolOfNode( location ).exports
if (((location.kind===SyntaxKind.SourceFile)||isAmbientModule( location ))) {
 if ((result=moduleExports("default"))) {
 val localSymbol = getLocalSymbolForExportDefault( result )
if (((localSymbol&&((result.flags&meaning)))&&(localSymbol.name===name))) {
 loop.break()

}
(result=undefined)

}
if (((moduleExports(name)&&(moduleExports(name).flags===SymbolFlags.Alias))&&getDeclarationOfKind( moduleExports(name), SyntaxKind.ExportSpecifier ))) {
 break()

}

}
if ((result=getSymbol( moduleExports, name, (meaning&SymbolFlags.ModuleMember) ))) {
 loop.break()

}
  case  SyntaxKind.EnumDeclaration  =>
if ((result=getSymbol( getSymbolOfNode( location ).exports, name, (meaning&SymbolFlags.EnumMember) ))) {
 loop.break()

}
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature  =>
if ((isClassLike( location.parent )&&(!((getModifierFlags( location )&ModifierFlags.Static))))) {
 val ctor = findConstructorDeclaration( location.parent.asInstanceOf[ClassLikeDeclaration] )
if ((ctor&&ctor.locals)) {
 if (getSymbol( ctor.locals, name, (meaning&SymbolFlags.Value) )) {
 (propertyWithInvalidInitializer=location)

}

}

}
  case  SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression | SyntaxKind.InterfaceDeclaration  =>
if ((result=getSymbol( getSymbolOfNode( location ).members, name, (meaning&SymbolFlags.Type) ))) {
 if ((lastLocation&&(getModifierFlags( lastLocation )&ModifierFlags.Static))) {
 error( errorLocation, Diagnostics.Static_members_cannot_reference_class_type_parameters )
return undefined

}
loop.break()

}
if (((location.kind===SyntaxKind.ClassExpression)&&(meaning&SymbolFlags.Class))) {
 val className = (location.asInstanceOf[ClassExpression]).name
if ((className&&(name===className.text))) {
 (result=location.symbol)
loop.break()

}

}
  case  SyntaxKind.ComputedPropertyName  =>
(grandparent=location.parent.parent)
if ((isClassLike( grandparent )||(grandparent.kind===SyntaxKind.InterfaceDeclaration))) {
 if ((result=getSymbol( getSymbolOfNode( grandparent ).members, name, (meaning&SymbolFlags.Type) ))) {
 error( errorLocation, Diagnostics.A_computed_property_name_cannot_reference_a_type_parameter_from_its_containing_type )
return undefined

}

}
  case  SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionDeclaration | SyntaxKind.ArrowFunction  =>
if (((meaning&SymbolFlags.Variable)&&(name==="arguments"))) {
 (result=argumentsSymbol)
loop.break()

}
  case  SyntaxKind.FunctionExpression  =>
if (((meaning&SymbolFlags.Variable)&&(name==="arguments"))) {
 (result=argumentsSymbol)
loop.break()

}
if ((meaning&SymbolFlags.Function)) {
 val functionName = (location.asInstanceOf[FunctionExpression]).name
if ((functionName&&(name===functionName.text))) {
 (result=location.symbol)
loop.break()

}

}
  case  SyntaxKind.Decorator  =>
if ((location.parent&&(location.parent.kind===SyntaxKind.Parameter))) {
 (location=location.parent)

}
if ((location.parent&&isClassElement( location.parent ))) {
 (location=location.parent)

}
  case _ =>
}
(lastLocation=location)
(location=location.parent)

}
}
}
if (((result&&nameNotFoundMessage)&&noUnusedIdentifiers)) {
 (result.isReferenced=true)

}
if ((!result)) {
 (result=getSymbol( globals, name, meaning ))

}
if ((!result)) {
 if (nameNotFoundMessage) {
 if (((!errorLocation)||(((!checkAndReportErrorForMissingPrefix( errorLocation, name, nameArg ))&&(!checkAndReportErrorForExtendingInterface( errorLocation )))&&(!checkAndReportErrorForUsingTypeAsValue( errorLocation, name, meaning ))))) {
 error( errorLocation, nameNotFoundMessage, (if ((typeof(nameArg)==="string")) nameArg else declarationNameToString( nameArg )) )

}

}
return undefined

}
if (nameNotFoundMessage) {
 if (propertyWithInvalidInitializer) {
 val propertyName = (propertyWithInvalidInitializer.asInstanceOf[PropertyDeclaration]).name
error( errorLocation, Diagnostics.Initializer_of_instance_member_variable_0_cannot_reference_identifier_1_declared_in_the_constructor, declarationNameToString( propertyName ), (if ((typeof(nameArg)==="string")) nameArg else declarationNameToString( nameArg )) )
return undefined

}
if ((meaning&SymbolFlags.BlockScopedVariable)) {
 val exportOrLocalSymbol = getExportSymbolOfValueSymbolIfExported( result )
if ((exportOrLocalSymbol.flags&SymbolFlags.BlockScopedVariable)) {
 checkResolvedBlockScopedVariable( exportOrLocalSymbol, errorLocation )

}

}
if (((result&&isInExternalModule)&&(((meaning&SymbolFlags.Value))===SymbolFlags.Value))) {
 val decls = result.declarations
if (((decls&&(decls.length===1))&&(decls(0).kind===SyntaxKind.NamespaceExportDeclaration))) {
 error( errorLocation, Diagnostics._0_refers_to_a_UMD_global_but_the_current_file_is_a_module_Consider_adding_an_import_instead, name )

}

}

}
return result

}
def checkAndReportErrorForMissingPrefix(errorLocation: Node, name: String, nameArg: ( String | Identifier )): Boolean = {
 if (((((errorLocation.kind===SyntaxKind.Identifier)&&(isTypeReferenceIdentifier( errorLocation.asInstanceOf[Identifier] )))||isInTypeQuery( errorLocation )))) {
 return false

}
val container = getThisContainer( errorLocation, true )
var location = container
while (location) {
{
 if (isClassLike( location.parent )) {
 val classSymbol = getSymbolOfNode( location.parent )
if ((!classSymbol)) {
 break()

}
val constructorType = getTypeOfSymbol( classSymbol )
if (getPropertyOfType( constructorType, name )) {
 error( errorLocation, Diagnostics.Cannot_find_name_0_Did_you_mean_the_static_member_1_0, (if ((typeof(nameArg)==="string")) nameArg else declarationNameToString( nameArg )), symbolToString( classSymbol ) )
return true

}
if (((location===container)&&(!((getModifierFlags( location )&ModifierFlags.Static))))) {
 val instanceType = (getDeclaredTypeOfSymbol( classSymbol ).asInstanceOf[InterfaceType]).thisType
if (getPropertyOfType( instanceType, name )) {
 error( errorLocation, Diagnostics.Cannot_find_name_0_Did_you_mean_the_instance_member_this_0, (if ((typeof(nameArg)==="string")) nameArg else declarationNameToString( nameArg )) )
return true

}

}

}
(location=location.parent)

}
}
return false

}
def checkAndReportErrorForExtendingInterface(errorLocation: Node): Boolean = {
 val expression = getEntityNameForExtendingInterface( errorLocation )
val isError = (!(!((expression&&resolveEntityName( expression, SymbolFlags.Interface, true )))))
if (isError) {
 error( errorLocation, Diagnostics.Cannot_extend_an_interface_0_Did_you_mean_implements, getTextOfNode( expression ) )

}
return isError

}
def getEntityNameForExtendingInterface(node: Node): ( EntityNameExpression | undefined ) = {
 node.kind match {
  case  SyntaxKind.Identifier | SyntaxKind.PropertyAccessExpression  =>
return (if (node.parent) getEntityNameForExtendingInterface( node.parent ) else undefined)
  case  SyntaxKind.ExpressionWithTypeArguments  =>
Debug.assert( isEntityNameExpression( (node.asInstanceOf[ExpressionWithTypeArguments]).expression ) )
return (node.asInstanceOf[ExpressionWithTypeArguments]).expression.asInstanceOf[EntityNameExpression]
  case _ =>
return undefined
}

}
def checkAndReportErrorForUsingTypeAsValue(errorLocation: Node, name: String, meaning: SymbolFlags): Boolean = {
 if ((meaning&((SymbolFlags.Value&(~SymbolFlags.NamespaceModule))))) {
 val symbol = resolveSymbol( resolveName( errorLocation, name, (SymbolFlags.Type&(~SymbolFlags.Value)), undefined, undefined ) )
if ((symbol&&(!((symbol.flags&SymbolFlags.NamespaceModule))))) {
 error( errorLocation, Diagnostics._0_only_refers_to_a_type_but_is_being_used_as_a_value_here, name )
return true

}

}
return false

}
def checkResolvedBlockScopedVariable(result: Symbol, errorLocation: Node): Unit = {
 Debug.assert( (((result.flags&SymbolFlags.BlockScopedVariable))!==0) )
val declaration = forEach( result.declarations, (d =>  (if (isBlockOrCatchScoped( d )) d else undefined)) )
Debug.assert( (declaration!==undefined), "Block-scoped variable declaration is undefined" )
if (((!isInAmbientContext( declaration ))&&(!isBlockScopedNameDeclaredBeforeUse( getAncestor( declaration, SyntaxKind.VariableDeclaration ).asInstanceOf[Declaration], errorLocation )))) {
 error( errorLocation, Diagnostics.Block_scoped_variable_0_used_before_its_declaration, declarationNameToString( declaration.name ) )

}

}
def isSameScopeDescendentOf(initial: Node, parent: Node, stopAt: Node): Boolean = {
 if ((!parent)) {
 return false

}
{
var current = initial
while( ((current&&(current!==stopAt))&&(!isFunctionLike( current )))) {
 {
 if ((current===parent)) {
 return true

}

}
 (current=current.parent)
}
}
return false

}
def getAnyImportSyntax(node: Node): AnyImportSyntax = {
 if (isAliasSymbolDeclaration( node )) {
 if ((node.kind===SyntaxKind.ImportEqualsDeclaration)) {
 return node.asInstanceOf[ImportEqualsDeclaration]

}
while ((node&&(node.kind!==SyntaxKind.ImportDeclaration))) {
{
 (node=node.parent)

}
}
return node.asInstanceOf[ImportDeclaration]

}

}
def getDeclarationOfAliasSymbol(symbol: Symbol): ( Declaration | undefined ) = {
 return forEach( symbol.declarations, (d =>  (if (isAliasSymbolDeclaration( d )) d else undefined)) )

}
def getTargetOfImportEqualsDeclaration(node: ImportEqualsDeclaration): Symbol = {
 if ((node.moduleReference.kind===SyntaxKind.ExternalModuleReference)) {
 return resolveExternalModuleSymbol( resolveExternalModuleName( node, getExternalModuleImportEqualsDeclarationExpression( node ) ) )

}
return getSymbolOfPartOfRightHandSideOfImportEquals( node.moduleReference.asInstanceOf[EntityName] )

}
def getTargetOfImportClause(node: ImportClause): Symbol = {
 val moduleSymbol = resolveExternalModuleName( node, (node.parent.asInstanceOf[ImportDeclaration]).moduleSpecifier )
if (moduleSymbol) {
 val exportDefaultSymbol = (if (isShorthandAmbientModuleSymbol( moduleSymbol )) moduleSymbol else (if (moduleSymbol.exports("export=")) getPropertyOfType( getTypeOfSymbol( moduleSymbol.exports("export=") ), "default" ) else resolveSymbol( moduleSymbol.exports("default") )))
if (((!exportDefaultSymbol)&&(!allowSyntheticDefaultImports))) {
 error( node.name, Diagnostics.Module_0_has_no_default_export, symbolToString( moduleSymbol ) )

}
else if (((!exportDefaultSymbol)&&allowSyntheticDefaultImports)) {
 return (resolveExternalModuleSymbol( moduleSymbol )||resolveSymbol( moduleSymbol ))

}
return exportDefaultSymbol

}

}
def getTargetOfNamespaceImport(node: NamespaceImport): Symbol = {
 val moduleSpecifier = (node.parent.parent.asInstanceOf[ImportDeclaration]).moduleSpecifier
return resolveESModuleSymbol( resolveExternalModuleName( node, moduleSpecifier ), moduleSpecifier )

}
def combineValueAndTypeSymbols(valueSymbol: Symbol, typeSymbol: Symbol): Symbol = {
 if ((valueSymbol.flags&((SymbolFlags.Type|SymbolFlags.Namespace)))) {
 return valueSymbol

}
val result = createSymbol( (valueSymbol.flags|typeSymbol.flags), valueSymbol.name )
(result.declarations=concatenate( valueSymbol.declarations, typeSymbol.declarations ))
(result.parent=(valueSymbol.parent||typeSymbol.parent))
if (valueSymbol.valueDeclaration)
(result.valueDeclaration=valueSymbol.valueDeclaration)
if (typeSymbol.members)
(result.members=typeSymbol.members)
if (valueSymbol.exports)
(result.exports=valueSymbol.exports)
return result

}
def getExportOfModule(symbol: Symbol, name: String): Symbol = {
 if ((symbol.flags&SymbolFlags.Module)) {
 val exportedSymbol = getExportsOfSymbol( symbol )(name)
if (exportedSymbol) {
 return resolveSymbol( exportedSymbol )

}

}

}
def getPropertyOfVariable(symbol: Symbol, name: String): Symbol = {
 if ((symbol.flags&SymbolFlags.Variable)) {
 val typeAnnotation = (symbol.valueDeclaration.asInstanceOf[VariableDeclaration]).`type`
if (typeAnnotation) {
 return resolveSymbol( getPropertyOfType( getTypeFromTypeNode( typeAnnotation ), name ) )

}

}

}
def getExternalModuleMember(node: ( ImportDeclaration | ExportDeclaration ), specifier: ImportOrExportSpecifier): Symbol = {
 val moduleSymbol = resolveExternalModuleName( node, node.moduleSpecifier )
val targetSymbol = resolveESModuleSymbol( moduleSymbol, node.moduleSpecifier )
if (targetSymbol) {
 val name = (specifier.propertyName||specifier.name)
if (name.text) {
 if (isShorthandAmbientModuleSymbol( moduleSymbol )) {
 return moduleSymbol

}
var symbolFromVariable: Symbol = zeroOfMyType
if (((moduleSymbol&&moduleSymbol.exports)&&moduleSymbol.exports("export="))) {
 (symbolFromVariable=getPropertyOfType( getTypeOfSymbol( targetSymbol ), name.text ))

}
else {
 (symbolFromVariable=getPropertyOfVariable( targetSymbol, name.text ))

}
(symbolFromVariable=resolveSymbol( symbolFromVariable ))
var symbolFromModule = getExportOfModule( targetSymbol, name.text )
if ((((!symbolFromModule)&&allowSyntheticDefaultImports)&&(name.text==="default"))) {
 (symbolFromModule=(resolveExternalModuleSymbol( moduleSymbol )||resolveSymbol( moduleSymbol )))

}
val symbol = (if ((symbolFromModule&&symbolFromVariable)) combineValueAndTypeSymbols( symbolFromVariable, symbolFromModule ) else (symbolFromModule||symbolFromVariable))
if ((!symbol)) {
 error( name, Diagnostics.Module_0_has_no_exported_member_1, getFullyQualifiedName( moduleSymbol ), declarationNameToString( name ) )

}
return symbol

}

}

}
def getTargetOfImportSpecifier(node: ImportSpecifier): Symbol = {
 return getExternalModuleMember( node.parent.parent.parent.asInstanceOf[ImportDeclaration], node )

}
def getTargetOfNamespaceExportDeclaration(node: NamespaceExportDeclaration): Symbol = {
 return resolveExternalModuleSymbol( node.parent.symbol )

}
def getTargetOfExportSpecifier(node: ExportSpecifier): Symbol = {
 return (if ((node.parent.parent.asInstanceOf[ExportDeclaration]).moduleSpecifier) getExternalModuleMember( node.parent.parent.asInstanceOf[ExportDeclaration], node ) else resolveEntityName( (node.propertyName||node.name), ((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace) ))

}
def getTargetOfExportAssignment(node: ExportAssignment): Symbol = {
 return resolveEntityName( node.expression.asInstanceOf[EntityNameExpression], ((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace) )

}
def getTargetOfAliasDeclaration(node: Declaration): Symbol = {
 node.kind match {
  case  SyntaxKind.ImportEqualsDeclaration  =>
return getTargetOfImportEqualsDeclaration( node.asInstanceOf[ImportEqualsDeclaration] )
  case  SyntaxKind.ImportClause  =>
return getTargetOfImportClause( node.asInstanceOf[ImportClause] )
  case  SyntaxKind.NamespaceImport  =>
return getTargetOfNamespaceImport( node.asInstanceOf[NamespaceImport] )
  case  SyntaxKind.ImportSpecifier  =>
return getTargetOfImportSpecifier( node.asInstanceOf[ImportSpecifier] )
  case  SyntaxKind.ExportSpecifier  =>
return getTargetOfExportSpecifier( node.asInstanceOf[ExportSpecifier] )
  case  SyntaxKind.ExportAssignment  =>
return getTargetOfExportAssignment( node.asInstanceOf[ExportAssignment] )
  case  SyntaxKind.NamespaceExportDeclaration  =>
return getTargetOfNamespaceExportDeclaration( node.asInstanceOf[NamespaceExportDeclaration] )
  case _ =>
}

}
def resolveSymbol(symbol: Symbol): Symbol = {
 return (if (((symbol&&(symbol.flags&SymbolFlags.Alias))&&(!((symbol.flags&(((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace))))))) resolveAlias( symbol ) else symbol)

}
def resolveAlias(symbol: Symbol): Symbol = {
 Debug.assert( (((symbol.flags&SymbolFlags.Alias))!==0), "Should only get Alias here." )
val links = getSymbolLinks( symbol )
if ((!links.target)) {
 (links.target=resolvingSymbol)
val node = getDeclarationOfAliasSymbol( symbol )
Debug.assert( (!(!node)) )
val target = getTargetOfAliasDeclaration( node )
if ((links.target===resolvingSymbol)) {
 (links.target=(target||unknownSymbol))

}
else {
 error( node, Diagnostics.Circular_definition_of_import_alias_0, symbolToString( symbol ) )

}

}
else if ((links.target===resolvingSymbol)) {
 (links.target=unknownSymbol)

}
return links.target

}
def markExportAsReferenced(node: ( ImportEqualsDeclaration | ExportAssignment | ExportSpecifier )) = {
 val symbol = getSymbolOfNode( node )
val target = resolveAlias( symbol )
if (target) {
 val markAlias = ((target===unknownSymbol)||((((target.flags&SymbolFlags.Value))&&(!isConstEnumOrConstEnumOnlyModule( target )))))
if (markAlias) {
 markAliasSymbolAsReferenced( symbol )

}

}

}
def markAliasSymbolAsReferenced(symbol: Symbol) = {
 val links = getSymbolLinks( symbol )
if ((!links.referenced)) {
 (links.referenced=true)
val node = getDeclarationOfAliasSymbol( symbol )
Debug.assert( (!(!node)) )
if ((node.kind===SyntaxKind.ExportAssignment)) {
 checkExpressionCached( (node.asInstanceOf[ExportAssignment]).expression )

}
else if ((node.kind===SyntaxKind.ExportSpecifier)) {
 checkExpressionCached( ((node.asInstanceOf[ExportSpecifier]).propertyName||(node.asInstanceOf[ExportSpecifier]).name) )

}
else if (isInternalModuleImportEqualsDeclaration( node )) {
 checkExpressionCached( (node.asInstanceOf[ImportEqualsDeclaration]).moduleReference.asInstanceOf[Expression] )

}

}

}
def getSymbolOfPartOfRightHandSideOfImportEquals(entityName: EntityName, dontResolveAlias: Boolean): Symbol = {
 if (((entityName.kind===SyntaxKind.Identifier)&&isRightSideOfQualifiedNameOrPropertyAccess( entityName ))) {
 (entityName=entityName.parent.asInstanceOf[QualifiedName])

}
if (((entityName.kind===SyntaxKind.Identifier)||(entityName.parent.kind===SyntaxKind.QualifiedName))) {
 return resolveEntityName( entityName, SymbolFlags.Namespace, false, dontResolveAlias )

}
else {
 Debug.assert( (entityName.parent.kind===SyntaxKind.ImportEqualsDeclaration) )
return resolveEntityName( entityName, ((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace), false, dontResolveAlias )

}

}
def getFullyQualifiedName(symbol: Symbol): String = {
 return (if (symbol.parent) ((getFullyQualifiedName( symbol.parent )+".")+symbolToString( symbol )) else symbolToString( symbol ))

}
def resolveEntityName(name: EntityNameOrEntityNameExpression, meaning: SymbolFlags, ignoreErrors: Boolean, dontResolveAlias: Boolean, location: Node): ( Symbol | undefined ) = {
 if (nodeIsMissing( name )) {
 return undefined

}
var symbol: Symbol = zeroOfMyType
if ((name.kind===SyntaxKind.Identifier)) {
 val message = (if ((meaning===SymbolFlags.Namespace)) Diagnostics.Cannot_find_namespace_0 else Diagnostics.Cannot_find_name_0)
(symbol=resolveName( (location||name), (name.asInstanceOf[Identifier]).text, meaning, (if (ignoreErrors) undefined else message), name.asInstanceOf[Identifier] ))
if ((!symbol)) {
 return undefined

}

}
else if (((name.kind===SyntaxKind.QualifiedName)||(name.kind===SyntaxKind.PropertyAccessExpression))) {
 val left = (if ((name.kind===SyntaxKind.QualifiedName)) (name.asInstanceOf[QualifiedName]).left else (name.asInstanceOf[PropertyAccessEntityNameExpression]).expression)
val right = (if ((name.kind===SyntaxKind.QualifiedName)) (name.asInstanceOf[QualifiedName]).right else (name.asInstanceOf[PropertyAccessExpression]).name)
val namespace = resolveEntityName( left, SymbolFlags.Namespace, ignoreErrors, false, location )
if (((!namespace)||nodeIsMissing( right ))) {
 return undefined

}
else if ((namespace===unknownSymbol)) {
 return namespace

}
(symbol=getSymbol( getExportsOfSymbol( namespace ), right.text, meaning ))
if ((!symbol)) {
 if ((!ignoreErrors)) {
 error( right, Diagnostics.Namespace_0_has_no_exported_member_1, getFullyQualifiedName( namespace ), declarationNameToString( right ) )

}
return undefined

}

}
else {
 Debug.fail( "Unknown entity name kind." )

}
Debug.assert( (((symbol.flags&SymbolFlags.Instantiated))===0), "Should never get an instantiated symbol here." )
return (if ((((symbol.flags&meaning))||dontResolveAlias)) symbol else resolveAlias( symbol ))

}
def resolveExternalModuleName(location: Node, moduleReferenceExpression: Expression): Symbol = {
 return resolveExternalModuleNameWorker( location, moduleReferenceExpression, Diagnostics.Cannot_find_module_0 )

}
def resolveExternalModuleNameWorker(location: Node, moduleReferenceExpression: Expression, moduleNotFoundError: DiagnosticMessage): Symbol = {
 if ((moduleReferenceExpression.kind!==SyntaxKind.StringLiteral)) {
 return

}
val moduleReferenceLiteral = moduleReferenceExpression.asInstanceOf[LiteralExpression]
return resolveExternalModule( location, moduleReferenceLiteral.text, moduleNotFoundError, moduleReferenceLiteral )

}
def resolveExternalModule(location: Node, moduleReference: String, moduleNotFoundError: DiagnosticMessage, errorNode: Node): Symbol = {
 val moduleName = escapeIdentifier( moduleReference )
if ((moduleName===undefined)) {
 return

}
val isRelative = isExternalModuleNameRelative( moduleName )
if ((!isRelative)) {
 val symbol = getSymbol( globals, (("\""+moduleName)+"\""), SymbolFlags.ValueModule )
if (symbol) {
 return getMergedSymbol( symbol )

}

}
val resolvedModule = getResolvedModule( getSourceFileOfNode( location ), moduleReference )
val sourceFile = (resolvedModule&&host.getSourceFile( resolvedModule.resolvedFileName ))
if (sourceFile) {
 if (sourceFile.symbol) {
 return getMergedSymbol( sourceFile.symbol )

}
if (moduleNotFoundError) {
 error( errorNode, Diagnostics.File_0_is_not_a_module, sourceFile.fileName )

}
return undefined

}
if (patternAmbientModules) {
 val pattern = findBestPatternMatch( patternAmbientModules, (_underscore_ =>  _underscore_.pattern), moduleName )
if (pattern) {
 return getMergedSymbol( pattern.symbol )

}

}
if (moduleNotFoundError) {
 val tsExtension = tryExtractTypeScriptExtension( moduleName )
if (tsExtension) {
 val diag = Diagnostics.An_import_path_cannot_end_with_a_0_extension_Consider_importing_1_instead
error( errorNode, diag, tsExtension, removeExtension( moduleName, tsExtension ) )

}
else {
 error( errorNode, moduleNotFoundError, moduleName )

}

}
return undefined

}
def resolveExternalModuleSymbol(moduleSymbol: Symbol): Symbol = {
 return ((moduleSymbol&&getMergedSymbol( resolveSymbol( moduleSymbol.exports("export=") ) ))||moduleSymbol)

}
def resolveESModuleSymbol(moduleSymbol: Symbol, moduleReferenceExpression: Expression): Symbol = {
 var symbol = resolveExternalModuleSymbol( moduleSymbol )
if ((symbol&&(!((symbol.flags&((SymbolFlags.Module|SymbolFlags.Variable))))))) {
 error( moduleReferenceExpression, Diagnostics.Module_0_resolves_to_a_non_module_entity_and_cannot_be_imported_using_this_construct, symbolToString( moduleSymbol ) )
(symbol=undefined)

}
return symbol

}
def hasExportAssignmentSymbol(moduleSymbol: Symbol): Boolean = {
 return (moduleSymbol.exports("export=")!==undefined)

}
def getExportsOfModuleAsArray(moduleSymbol: Symbol): Array[Symbol] = {
 return symbolsToArray( getExportsOfModule( moduleSymbol ) )

}
def getExportsOfSymbol(symbol: Symbol): SymbolTable = {
 return (if ((symbol.flags&SymbolFlags.Module)) getExportsOfModule( symbol ) else (symbol.exports||emptySymbols))

}
def getExportsOfModule(moduleSymbol: Symbol): SymbolTable = {
 val links = getSymbolLinks( moduleSymbol )
return (links.resolvedExports||((links.resolvedExports=getExportsForModule( moduleSymbol ))))

}
trait ExportCollisionTracker {
  var specifierText: String
  var exportsWithDuplicate: Array[ExportDeclaration]
}
def extendExportSymbols(target: SymbolTable, source: SymbolTable, lookupTable: Map[ExportCollisionTracker], exportNode: ExportDeclaration) = {
 (source).keys.foreach { fresh3 =>
val id = zeroOfMyType
 = fresh3
 {
 if (((id!=="default")&&(!target(id)))) {
 (target(id)=source(id))
if ((lookupTable&&exportNode)) {
 (lookupTable(id)=Map( "specifierText" -> getTextOfNode( exportNode.moduleSpecifier ) ).asInstanceOf[ExportCollisionTracker])

}

}
else if (((((lookupTable&&exportNode)&&(id!=="default"))&&target(id))&&(resolveSymbol( target(id) )!==resolveSymbol( source(id) )))) {
 if ((!lookupTable(id).exportsWithDuplicate)) {
 (lookupTable(id).exportsWithDuplicate=Array( exportNode ))

}
else {
 lookupTable(id).exportsWithDuplicate.push( exportNode )

}

}

}
}

}
def getExportsForModule(moduleSymbol: Symbol): SymbolTable = {
 val visitedSymbols: Array[Symbol] = Array()
(moduleSymbol=resolveExternalModuleSymbol( moduleSymbol ))
return (visit( moduleSymbol )||moduleSymbol.exports)
def visit(symbol: Symbol): SymbolTable = {
 if ((!(((symbol&&(symbol.flags&SymbolFlags.HasExports))&&(!contains( visitedSymbols, symbol )))))) {
 return

}
visitedSymbols.push( symbol )
val symbols = cloneMap( symbol.exports )
val exportStars = symbol.exports("___export")
if (exportStars) {
 val nestedSymbols = createMap[ Symbol ]()
val lookupTable = createMap[ ExportCollisionTracker ]()
(exportStars.declarations).foreach { fresh4 =>
val node = zeroOfMyType
 = fresh4
 {
 val resolvedModule = resolveExternalModuleName( node, (node.asInstanceOf[ExportDeclaration]).moduleSpecifier )
val exportedSymbols = visit( resolvedModule )
extendExportSymbols( nestedSymbols, exportedSymbols, lookupTable, node.asInstanceOf[ExportDeclaration] )

}
}
(lookupTable).keys.foreach { fresh5 =>
val id = zeroOfMyType
 = fresh5
 {
 const fresh6 = lookupTable(id)
val exportsWithDuplicate = fresh6.exportsWithDuplicate
if ((((id==="export=")||(!((exportsWithDuplicate&&exportsWithDuplicate.length))))||symbols(id))) {
 continue

}
(exportsWithDuplicate).foreach { fresh7 =>
val node = zeroOfMyType
 = fresh7
 {
 diagnostics.add( createDiagnosticForNode( node, Diagnostics.Module_0_has_already_exported_a_member_named_1_Consider_explicitly_re_exporting_to_resolve_the_ambiguity, lookupTable(id).specifierText, id ) )

}
}

}
}
extendExportSymbols( symbols, nestedSymbols )

}
return symbols

}

}
def getMergedSymbol(symbol: Symbol): Symbol = {
 var merged: Symbol = zeroOfMyType
return (if (((symbol&&symbol.mergeId)&&((merged=mergedSymbols(symbol.mergeId))))) merged else symbol)

}
def getSymbolOfNode(node: Node): Symbol = {
 return getMergedSymbol( node.symbol )

}
def getParentOfSymbol(symbol: Symbol): Symbol = {
 return getMergedSymbol( symbol.parent )

}
def getExportSymbolOfValueSymbolIfExported(symbol: Symbol): Symbol = {
 return (if ((symbol&&(((symbol.flags&SymbolFlags.ExportValue))!==0))) getMergedSymbol( symbol.exportSymbol ) else symbol)

}
def symbolIsValue(symbol: Symbol): Boolean = {
 if ((symbol.flags&SymbolFlags.Instantiated)) {
 return symbolIsValue( getSymbolLinks( symbol ).target )

}
if ((symbol.flags&SymbolFlags.Value)) {
 return true

}
if ((symbol.flags&SymbolFlags.Alias)) {
 return (((resolveAlias( symbol ).flags&SymbolFlags.Value))!==0)

}
return false

}
def findConstructorDeclaration(node: ClassLikeDeclaration): ConstructorDeclaration = {
 val members = node.members
(members).foreach { fresh8 =>
val member = zeroOfMyType
 = fresh8
 {
 if (((member.kind===SyntaxKind.Constructor)&&nodeIsPresent( (member.asInstanceOf[ConstructorDeclaration]).body ))) {
 return member.asInstanceOf[ConstructorDeclaration]

}

}
}

}
def createType(flags: TypeFlags): Type = {
 val result = new Type( checker, flags )
(typeCount+= 1)
(result.id=typeCount)
return result

}
def createIntrinsicType(kind: TypeFlags, intrinsicName: String): IntrinsicType = {
 val `type` = createType( kind ).asInstanceOf[IntrinsicType]
(`type`.intrinsicName=intrinsicName)
return `type`

}
def createBooleanType(trueFalseTypes: Array[Type]): ( IntrinsicType with UnionType ) = {
 val `type` = getUnionType( trueFalseTypes ).asInstanceOf[( IntrinsicType with UnionType )]
(`type`.flags|=TypeFlags.Boolean)
(`type`.intrinsicName="boolean")
return `type`

}
def createObjectType(objectFlags: ObjectFlags, symbol: Symbol): ObjectType = {
 val `type` = createType( TypeFlags.Object ).asInstanceOf[ObjectType]
(`type`.objectFlags=objectFlags)
(`type`.symbol=symbol)
return `type`

}
def isReservedMemberName(name: String) = {
 return ((((name.charCodeAt( 0 )===CharacterCodes._underscore_)&&(name.charCodeAt( 1 )===CharacterCodes._underscore_))&&(name.charCodeAt( 2 )!==CharacterCodes._underscore_))&&(name.charCodeAt( 2 )!==CharacterCodes.at))

}
def getNamedMembers(members: SymbolTable): Array[Symbol] = {
 var result: Array[Symbol] = zeroOfMyType
(members).keys.foreach { fresh9 =>
val id = zeroOfMyType
 = fresh9
 {
 if ((!isReservedMemberName( id ))) {
 if ((!result))
(result=Array())
val symbol = members(id)
if (symbolIsValue( symbol )) {
 result.push( symbol )

}

}

}
}
return (result||emptyArray)

}
def setStructuredTypeMembers(`type`: StructuredType, members: SymbolTable, callSignatures: Array[Signature], constructSignatures: Array[Signature], stringIndexInfo: IndexInfo, numberIndexInfo: IndexInfo): ResolvedType = {
 ((`type`.asInstanceOf[ResolvedType]).members=members)
((`type`.asInstanceOf[ResolvedType]).properties=getNamedMembers( members ))
((`type`.asInstanceOf[ResolvedType]).callSignatures=callSignatures)
((`type`.asInstanceOf[ResolvedType]).constructSignatures=constructSignatures)
if (stringIndexInfo)
((`type`.asInstanceOf[ResolvedType]).stringIndexInfo=stringIndexInfo)
if (numberIndexInfo)
((`type`.asInstanceOf[ResolvedType]).numberIndexInfo=numberIndexInfo)
return `type`.asInstanceOf[ResolvedType]

}
def createAnonymousType(symbol: Symbol, members: SymbolTable, callSignatures: Array[Signature], constructSignatures: Array[Signature], stringIndexInfo: IndexInfo, numberIndexInfo: IndexInfo): ResolvedType = {
 return setStructuredTypeMembers( createObjectType( ObjectFlags.Anonymous, symbol ), members, callSignatures, constructSignatures, stringIndexInfo, numberIndexInfo )

}
def forEachSymbolTableInScope[T](enclosingDeclaration: Node, callback: ((SymbolTable) => T)): T = {
 var result: T = zeroOfMyType
{
var location = enclosingDeclaration
while( location) {
 {
 if ((location.locals&&(!isGlobalSourceFile( location )))) {
 if ((result=callback( location.locals ))) {
 return result

}

}
location.kind match {
  case  SyntaxKind.SourceFile  =>
if ((!isExternalOrCommonJsModule( location.asInstanceOf[SourceFile] ))) {
 break()

}
  case  SyntaxKind.ModuleDeclaration  =>
if ((result=callback( getSymbolOfNode( location ).exports ))) {
 return result

}
  case _ =>
}

}
 (location=location.parent)
}
}
return callback( globals )

}
def getQualifiedLeftMeaning(rightMeaning: SymbolFlags) = {
 return (if ((rightMeaning===SymbolFlags.Value)) SymbolFlags.Value else SymbolFlags.Namespace)

}
def getAccessibleSymbolChain(symbol: Symbol, enclosingDeclaration: Node, meaning: SymbolFlags, useOnlyExternalAliasing: Boolean): Array[Symbol] = {
 def getAccessibleSymbolChainFromSymbolTable(symbols: SymbolTable): Array[Symbol] = {
 def canQualifySymbol(symbolFromSymbolTable: Symbol, meaning: SymbolFlags) = {
 if ((!needsQualification( symbolFromSymbolTable, enclosingDeclaration, meaning ))) {
 return true

}
val accessibleParent = getAccessibleSymbolChain( symbolFromSymbolTable.parent, enclosingDeclaration, getQualifiedLeftMeaning( meaning ), useOnlyExternalAliasing )
return (!(!accessibleParent))

}
def isAccessible(symbolFromSymbolTable: Symbol, resolvedAliasSymbol: Symbol) = {
 if ((symbol===((resolvedAliasSymbol||symbolFromSymbolTable)))) {
 return ((!forEach( symbolFromSymbolTable.declarations, hasExternalModuleSymbol ))&&canQualifySymbol( symbolFromSymbolTable, meaning ))

}

}
if (isAccessible( symbols(symbol.name) )) {
 return Array( symbol )

}
return forEachProperty( symbols, (symbolFromSymbolTable =>  {
 if ((((symbolFromSymbolTable.flags&SymbolFlags.Alias)&&(symbolFromSymbolTable.name!=="export="))&&(!getDeclarationOfKind( symbolFromSymbolTable, SyntaxKind.ExportSpecifier )))) {
 if (((!useOnlyExternalAliasing)||ts.forEach( symbolFromSymbolTable.declarations, isExternalModuleImportEqualsDeclaration ))) {
 val resolvedImportedSymbol = resolveAlias( symbolFromSymbolTable )
if (isAccessible( symbolFromSymbolTable, resolveAlias( symbolFromSymbolTable ) )) {
 return Array( symbolFromSymbolTable )

}
val accessibleSymbolsFromExports = (if (resolvedImportedSymbol.exports) getAccessibleSymbolChainFromSymbolTable( resolvedImportedSymbol.exports ) else undefined)
if ((accessibleSymbolsFromExports&&canQualifySymbol( symbolFromSymbolTable, getQualifiedLeftMeaning( meaning ) ))) {
 return Array( symbolFromSymbolTable ).concat( accessibleSymbolsFromExports )

}

}

}

}) )

}
if (symbol) {
 if ((!(isPropertyOrMethodDeclarationSymbol( symbol )))) {
 return forEachSymbolTableInScope( enclosingDeclaration, getAccessibleSymbolChainFromSymbolTable )

}

}

}
def needsQualification(symbol: Symbol, enclosingDeclaration: Node, meaning: SymbolFlags) = {
 var qualify = false
forEachSymbolTableInScope( enclosingDeclaration, (symbolTable =>  {
 var symbolFromSymbolTable = symbolTable(symbol.name)
 if ((!symbolFromSymbolTable)) {
 return false

}
 if ((symbolFromSymbolTable===symbol)) {
 return true

}
 (symbolFromSymbolTable=(if ((((symbolFromSymbolTable.flags&SymbolFlags.Alias)&&(!getDeclarationOfKind( symbolFromSymbolTable, SyntaxKind.ExportSpecifier ))))) resolveAlias( symbolFromSymbolTable ) else symbolFromSymbolTable))
 if ((symbolFromSymbolTable.flags&meaning)) {
 (qualify=true)
return true

}
 return false

}) )
return qualify

}
def isPropertyOrMethodDeclarationSymbol(symbol: Symbol) = {
 if ((symbol.declarations&&symbol.declarations.length)) {
 (symbol.declarations).foreach { fresh10 =>
val declaration = zeroOfMyType
 = fresh10
 {
 declaration.kind match {
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
continue
  case _ =>
return false
}

}
}
return true

}
return false

}
def isSymbolAccessible(symbol: Symbol, enclosingDeclaration: Node, meaning: SymbolFlags, shouldComputeAliasesToMakeVisible: Boolean): SymbolAccessibilityResult = {
 if (((symbol&&enclosingDeclaration)&&(!((symbol.flags&SymbolFlags.TypeParameter))))) {
 val initialSymbol = symbol
var meaningToLook = meaning
while (symbol) {
{
 val accessibleSymbolChain = getAccessibleSymbolChain( symbol, enclosingDeclaration, meaningToLook, false )
if (accessibleSymbolChain) {
 val hasAccessibleDeclarations = hasVisibleDeclarations( accessibleSymbolChain(0), shouldComputeAliasesToMakeVisible )
if ((!hasAccessibleDeclarations)) {
 return Map( "accessibility" -> SymbolAccessibility.NotAccessible,
"errorSymbolName" -> symbolToString( initialSymbol, enclosingDeclaration, meaning ),
"errorModuleName" -> (if ((symbol!==initialSymbol)) symbolToString( symbol, enclosingDeclaration, SymbolFlags.Namespace ) else undefined) ).asInstanceOf[SymbolAccessibilityResult]

}
return hasAccessibleDeclarations

}
(meaningToLook=getQualifiedLeftMeaning( meaning ))
(symbol=getParentOfSymbol( symbol ))

}
}
val symbolExternalModule = forEach( initialSymbol.declarations, getExternalModuleContainer )
if (symbolExternalModule) {
 val enclosingExternalModule = getExternalModuleContainer( enclosingDeclaration )
if ((symbolExternalModule!==enclosingExternalModule)) {
 return Map( "accessibility" -> SymbolAccessibility.CannotBeNamed,
"errorSymbolName" -> symbolToString( initialSymbol, enclosingDeclaration, meaning ),
"errorModuleName" -> symbolToString( symbolExternalModule ) )

}

}
return Map( "accessibility" -> SymbolAccessibility.NotAccessible,
"errorSymbolName" -> symbolToString( initialSymbol, enclosingDeclaration, meaning ) )

}
return Map( "accessibility" -> SymbolAccessibility.Accessible )
def getExternalModuleContainer(declaration: Node) = {
 {
while( declaration) {
 {
 if (hasExternalModuleSymbol( declaration )) {
 return getSymbolOfNode( declaration )

}

}
 (declaration=declaration.parent)
}
}

}

}
def hasExternalModuleSymbol(declaration: Node) = {
 return (isAmbientModule( declaration )||(((declaration.kind===SyntaxKind.SourceFile)&&isExternalOrCommonJsModule( declaration.asInstanceOf[SourceFile] ))))

}
def hasVisibleDeclarations(symbol: Symbol, shouldComputeAliasToMakeVisible: Boolean): SymbolVisibilityResult = {
 var aliasesToMakeVisible: Array[AnyImportSyntax] = zeroOfMyType
if (forEach( symbol.declarations, (declaration =>  (!getIsDeclarationVisible( declaration ))) )) {
 return undefined

}
return Map( "accessibility" -> SymbolAccessibility.Accessible,
"aliasesToMakeVisible" -> aliasesToMakeVisible )
def getIsDeclarationVisible(declaration: Declaration) = {
 if ((!isDeclarationVisible( declaration ))) {
 val anyImportSyntax = getAnyImportSyntax( declaration )
if (((anyImportSyntax&&(!((getModifierFlags( anyImportSyntax )&ModifierFlags.Export))))&&isDeclarationVisible( anyImportSyntax.parent.asInstanceOf[Declaration] ))) {
 if (shouldComputeAliasToMakeVisible) {
 (getNodeLinks( declaration ).isVisible=true)
if (aliasesToMakeVisible) {
 if ((!contains( aliasesToMakeVisible, anyImportSyntax ))) {
 aliasesToMakeVisible.push( anyImportSyntax )

}

}
else {
 (aliasesToMakeVisible=Array( anyImportSyntax ))

}

}
return true

}
return false

}
return true

}

}
def isEntityNameVisible(entityName: EntityNameOrEntityNameExpression, enclosingDeclaration: Node): SymbolVisibilityResult = {
 var meaning: SymbolFlags = zeroOfMyType
if (((entityName.parent.kind===SyntaxKind.TypeQuery)||isExpressionWithTypeArgumentsInClassExtendsClause( entityName.parent ))) {
 (meaning=(SymbolFlags.Value|SymbolFlags.ExportValue))

}
else if ((((entityName.kind===SyntaxKind.QualifiedName)||(entityName.kind===SyntaxKind.PropertyAccessExpression))||(entityName.parent.kind===SyntaxKind.ImportEqualsDeclaration))) {
 (meaning=SymbolFlags.Namespace)

}
else {
 (meaning=SymbolFlags.Type)

}
val firstIdentifier = getFirstIdentifier( entityName )
val symbol = resolveName( enclosingDeclaration, (firstIdentifier.asInstanceOf[Identifier]).text, meaning, undefined, undefined )
return (((symbol&&hasVisibleDeclarations( symbol, true )))||Map( "accessibility" -> SymbolAccessibility.NotAccessible,
"errorSymbolName" -> getTextOfNode( firstIdentifier ),
"errorNode" -> firstIdentifier ).asInstanceOf[SymbolVisibilityResult])

}
def writeKeyword(writer: SymbolWriter, kind: SyntaxKind) = {
 writer.writeKeyword( tokenToString( kind ) )

}
def writePunctuation(writer: SymbolWriter, kind: SyntaxKind) = {
 writer.writePunctuation( tokenToString( kind ) )

}
def writeSpace(writer: SymbolWriter) = {
 writer.writeSpace( " " )

}
def symbolToString(symbol: Symbol, enclosingDeclaration: Node, meaning: SymbolFlags): String = {
 val writer = getSingleLineStringWriter()
getSymbolDisplayBuilder().buildSymbolDisplay( symbol, writer, enclosingDeclaration, meaning )
val result = writer.string()
releaseStringWriter( writer )
return result

}
def signatureToString(signature: Signature, enclosingDeclaration: Node, flags: TypeFormatFlags, kind: SignatureKind): String = {
 val writer = getSingleLineStringWriter()
getSymbolDisplayBuilder().buildSignatureDisplay( signature, writer, enclosingDeclaration, flags, kind )
val result = writer.string()
releaseStringWriter( writer )
return result

}
def typeToString(`type`: Type, enclosingDeclaration: Node, flags: TypeFormatFlags): String = {
 val writer = getSingleLineStringWriter()
getSymbolDisplayBuilder().buildTypeDisplay( `type`, writer, enclosingDeclaration, flags )
var result = writer.string()
releaseStringWriter( writer )
val maxLength = (if ((compilerOptions.noErrorTruncation||(flags&TypeFormatFlags.NoTruncation))) undefined else 100)
if ((maxLength&&(result.length>=maxLength))) {
 (result=(result.substr( 0, (maxLength-"...".length) )+"..."))

}
return result

}
def typePredicateToString(typePredicate: TypePredicate, enclosingDeclaration: Declaration, flags: TypeFormatFlags): String = {
 val writer = getSingleLineStringWriter()
getSymbolDisplayBuilder().buildTypePredicateDisplay( typePredicate, writer, enclosingDeclaration, flags )
val result = writer.string()
releaseStringWriter( writer )
return result

}
def formatUnionTypes(types: Array[Type]): Array[Type] = {
 val result: Array[Type] = Array()
var flags: TypeFlags = 0
{
var i = 0
while( (i<types.length)) {
 {
 val t = types(i)
(flags|=t.flags)
if ((!((t.flags&TypeFlags.Nullable)))) {
 if ((t.flags&((TypeFlags.BooleanLiteral|TypeFlags.EnumLiteral)))) {
 val baseType = (if ((t.flags&TypeFlags.BooleanLiteral)) booleanType else (t.asInstanceOf[EnumLiteralType]).baseType)
val count = baseType.types.length
if ((((i+count)<=types.length)&&(types(((i+count)-1))===baseType.types((count-1))))) {
 result.push( baseType )
(i+=(count-1))
continue

}

}
result.push( t )

}

}
 (i+= 1)
}
}
if ((flags&TypeFlags.Null))
result.push( nullType )
if ((flags&TypeFlags.Undefined))
result.push( undefinedType )
return (result||types)

}
def visibilityToString(flags: ModifierFlags) = {
 if ((flags===ModifierFlags.Private)) {
 return "private"

}
if ((flags===ModifierFlags.Protected)) {
 return "protected"

}
return "public"

}
def getTypeAliasForTypeLiteral(`type`: Type): Symbol = {
 if ((`type`.symbol&&(`type`.symbol.flags&SymbolFlags.TypeLiteral))) {
 var node = `type`.symbol.declarations(0).parent
while ((node.kind===SyntaxKind.ParenthesizedType)) {
{
 (node=node.parent)

}
}
if ((node.kind===SyntaxKind.TypeAliasDeclaration)) {
 return getSymbolOfNode( node )

}

}
return undefined

}
def isTopLevelInExternalModuleAugmentation(node: Node): Boolean = {
 return (((node&&node.parent)&&(node.parent.kind===SyntaxKind.ModuleBlock))&&isExternalModuleAugmentation( node.parent.parent ))

}
def literalTypeToString(`type`: LiteralType) = {
 return (if ((`type`.flags&TypeFlags.StringLiteral)) s"""\"${ escapeString( (`type`.asInstanceOf[LiteralType]).text )}\""""  else (`type`.asInstanceOf[LiteralType]).text)

}
def getSymbolDisplayBuilder(): SymbolDisplayBuilder = {
 def getNameOfSymbol(symbol: Symbol): String = {
 if ((symbol.declarations&&symbol.declarations.length)) {
 val declaration = symbol.declarations(0)
if (declaration.name) {
 return declarationNameToString( declaration.name )

}
declaration.kind match {
  case  SyntaxKind.ClassExpression  =>
return "(Anonymous class)"
  case  SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction  =>
return "(Anonymous function)"
  case _ =>
}

}
return symbol.name

}
def appendSymbolNameOnly(symbol: Symbol, writer: SymbolWriter): Unit = {
 writer.writeSymbol( getNameOfSymbol( symbol ), symbol )

}
def appendPropertyOrElementAccessForSymbol(symbol: Symbol, writer: SymbolWriter): Unit = {
 val symbolName = getNameOfSymbol( symbol )
val firstChar = symbolName.charCodeAt( 0 )
val needsElementAccess = (!isIdentifierStart( firstChar, languageVersion ))
if (needsElementAccess) {
 writePunctuation( writer, SyntaxKind.OpenBracketToken )
if (isSingleOrDoubleQuote( firstChar )) {
 writer.writeStringLiteral( symbolName )

}
else {
 writer.writeSymbol( symbolName, symbol )

}
writePunctuation( writer, SyntaxKind.CloseBracketToken )

}
else {
 writePunctuation( writer, SyntaxKind.DotToken )
writer.writeSymbol( symbolName, symbol )

}

}
def buildSymbolDisplay(symbol: Symbol, writer: SymbolWriter, enclosingDeclaration: Node, meaning: SymbolFlags, flags: SymbolFormatFlags, typeFlags: TypeFormatFlags): Unit = {
 var parentSymbol: Symbol = zeroOfMyType
def appendParentTypeArgumentsAndSymbolName(symbol: Symbol): Unit = {
 if (parentSymbol) {
 if ((flags&SymbolFormatFlags.WriteTypeParametersOrArguments)) {
 if ((symbol.flags&SymbolFlags.Instantiated)) {
 buildDisplayForTypeArgumentsAndDelimiters( getTypeParametersOfClassOrInterface( parentSymbol ), (symbol.asInstanceOf[TransientSymbol]).mapper, writer, enclosingDeclaration )

}
else {
 buildTypeParameterDisplayFromSymbol( parentSymbol, writer, enclosingDeclaration )

}

}
appendPropertyOrElementAccessForSymbol( symbol, writer )

}
else {
 appendSymbolNameOnly( symbol, writer )

}
(parentSymbol=symbol)

}
writer.trackSymbol( symbol, enclosingDeclaration, meaning )
def walkSymbol(symbol: Symbol, meaning: SymbolFlags, endOfChain: Boolean): Unit = {
 val accessibleSymbolChain = getAccessibleSymbolChain( symbol, enclosingDeclaration, meaning, (!(!((flags&SymbolFormatFlags.UseOnlyExternalAliasing)))) )
if (((!accessibleSymbolChain)||needsQualification( accessibleSymbolChain(0), enclosingDeclaration, (if ((accessibleSymbolChain.length===1)) meaning else getQualifiedLeftMeaning( meaning )) ))) {
 val parent = getParentOfSymbol( (if (accessibleSymbolChain) accessibleSymbolChain(0) else symbol) )
if (parent) {
 walkSymbol( parent, getQualifiedLeftMeaning( meaning ), false )

}

}
if (accessibleSymbolChain) {
 (accessibleSymbolChain).foreach { fresh11 =>
val accessibleSymbol = zeroOfMyType
 = fresh11
 {
 appendParentTypeArgumentsAndSymbolName( accessibleSymbol )

}
}

}
else if ((endOfChain||((!(((!parentSymbol)&&ts.forEach( symbol.declarations, hasExternalModuleSymbol ))))&&(!((symbol.flags&((SymbolFlags.TypeLiteral|SymbolFlags.ObjectLiteral)))))))) {
 appendParentTypeArgumentsAndSymbolName( symbol )

}

}
val isTypeParameter = (symbol.flags&SymbolFlags.TypeParameter)
val typeFormatFlag = (TypeFormatFlags.UseFullyQualifiedType&typeFlags)
if (((!isTypeParameter)&&((enclosingDeclaration||typeFormatFlag)))) {
 walkSymbol( symbol, meaning, true )

}
else {
 appendParentTypeArgumentsAndSymbolName( symbol )

}

}
def buildTypeDisplay(`type`: Type, writer: SymbolWriter, enclosingDeclaration: Node, globalFlags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 val globalFlagsToPass = (globalFlags&TypeFormatFlags.WriteOwnNameForAnyLike)
var inObjectTypeLiteral = false
return writeType( `type`, globalFlags )
def writeType(`type`: Type, flags: TypeFormatFlags) = {
 val nextFlags = (flags&(~TypeFormatFlags.InTypeAlias))
if ((`type`.flags&TypeFlags.Intrinsic)) {
 writer.writeKeyword( (if (((!((globalFlags&TypeFormatFlags.WriteOwnNameForAnyLike)))&&isTypeAny( `type` ))) "any" else (`type`.asInstanceOf[IntrinsicType]).intrinsicName) )

}
else if (((`type`.flags&TypeFlags.TypeParameter)&&(`type`.asInstanceOf[TypeParameter]).isThisType)) {
 if (inObjectTypeLiteral) {
 writer.reportInaccessibleThisError()

}
writer.writeKeyword( "this" )

}
else if ((getObjectFlags( `type` )&ObjectFlags.Reference)) {
 writeTypeReference( `type`.asInstanceOf[TypeReference], nextFlags )

}
else if ((`type`.flags&TypeFlags.EnumLiteral)) {
 buildSymbolDisplay( getParentOfSymbol( `type`.symbol ), writer, enclosingDeclaration, SymbolFlags.Type, SymbolFormatFlags.None, nextFlags )
writePunctuation( writer, SyntaxKind.DotToken )
appendSymbolNameOnly( `type`.symbol, writer )

}
else if (((getObjectFlags( `type` )&ObjectFlags.ClassOrInterface)||(`type`.flags&((TypeFlags.Enum|TypeFlags.TypeParameter))))) {
 buildSymbolDisplay( `type`.symbol, writer, enclosingDeclaration, SymbolFlags.Type, SymbolFormatFlags.None, nextFlags )

}
else if (((((!((flags&TypeFormatFlags.InTypeAlias)))&&((((getObjectFlags( `type` )&ObjectFlags.Anonymous)&&(!(`type`.asInstanceOf[AnonymousType]).target))||(`type`.flags&TypeFlags.UnionOrIntersection))))&&`type`.aliasSymbol)&&(isSymbolAccessible( `type`.aliasSymbol, enclosingDeclaration, SymbolFlags.Type, false ).accessibility===SymbolAccessibility.Accessible))) {
 val typeArguments = (`type`.asInstanceOf[AnonymousType]).aliasTypeArguments
writeSymbolTypeReference( `type`.aliasSymbol, typeArguments, 0, (if (typeArguments) typeArguments.length else 0), nextFlags )

}
else if ((`type`.flags&TypeFlags.UnionOrIntersection)) {
 writeUnionOrIntersectionType( `type`.asInstanceOf[UnionOrIntersectionType], nextFlags )

}
else if ((getObjectFlags( `type` )&ObjectFlags.Anonymous)) {
 writeAnonymousType( `type`.asInstanceOf[ObjectType], nextFlags )

}
else if ((`type`.flags&TypeFlags.StringOrNumberLiteral)) {
 writer.writeStringLiteral( literalTypeToString( `type`.asInstanceOf[LiteralType] ) )

}
else {
 writePunctuation( writer, SyntaxKind.OpenBraceToken )
writeSpace( writer )
writePunctuation( writer, SyntaxKind.DotDotDotToken )
writeSpace( writer )
writePunctuation( writer, SyntaxKind.CloseBraceToken )

}

}
def writeTypeList(types: Array[Type], delimiter: SyntaxKind) = {
 {
var i = 0
while( (i<types.length)) {
 {
 if ((i>0)) {
 if ((delimiter!==SyntaxKind.CommaToken)) {
 writeSpace( writer )

}
writePunctuation( writer, delimiter )
writeSpace( writer )

}
writeType( types(i), (if ((delimiter===SyntaxKind.CommaToken)) TypeFormatFlags.None else TypeFormatFlags.InElementType) )

}
 (i+= 1)
}
}

}
def writeSymbolTypeReference(symbol: Symbol, typeArguments: Array[Type], pos: Int, end: Int, flags: TypeFormatFlags) = {
 if (((symbol.flags&SymbolFlags.Class)||(!isReservedMemberName( symbol.name )))) {
 buildSymbolDisplay( symbol, writer, enclosingDeclaration, SymbolFlags.Type, SymbolFormatFlags.None, flags )

}
if ((pos<end)) {
 writePunctuation( writer, SyntaxKind.LessThanToken )
writeType( typeArguments(pos), TypeFormatFlags.InFirstTypeArgument )
(pos+= 1)
while ((pos<end)) {
{
 writePunctuation( writer, SyntaxKind.CommaToken )
writeSpace( writer )
writeType( typeArguments(pos), TypeFormatFlags.None )
(pos+= 1)

}
}
writePunctuation( writer, SyntaxKind.GreaterThanToken )

}

}
def writeTypeReference(`type`: TypeReference, flags: TypeFormatFlags) = {
 val typeArguments = (`type`.typeArguments||emptyArray)
if (((`type`.target===globalArrayType)&&(!((flags&TypeFormatFlags.WriteArrayAsGenericType))))) {
 writeType( typeArguments(0), TypeFormatFlags.InElementType )
writePunctuation( writer, SyntaxKind.OpenBracketToken )
writePunctuation( writer, SyntaxKind.CloseBracketToken )

}
else if ((`type`.target.objectFlags&ObjectFlags.Tuple)) {
 writePunctuation( writer, SyntaxKind.OpenBracketToken )
writeTypeList( `type`.typeArguments.slice( 0, getTypeReferenceArity( `type` ) ), SyntaxKind.CommaToken )
writePunctuation( writer, SyntaxKind.CloseBracketToken )

}
else {
 val outerTypeParameters = `type`.target.outerTypeParameters
var i = 0
if (outerTypeParameters) {
 val length = outerTypeParameters.length
while ((i<length)) {
{
 val start = i
val parent = getParentSymbolOfTypeParameter( outerTypeParameters(i) )
do {
{
 (i+= 1)

}
} while (((i<length)&&(getParentSymbolOfTypeParameter( outerTypeParameters(i) )===parent)))
if ((!rangeEquals( outerTypeParameters, typeArguments, start, i ))) {
 writeSymbolTypeReference( parent, typeArguments, start, i, flags )
writePunctuation( writer, SyntaxKind.DotToken )

}

}
}

}
val typeParameterCount = ((`type`.target.typeParameters||emptyArray)).length
writeSymbolTypeReference( `type`.symbol, typeArguments, i, typeParameterCount, flags )

}

}
def writeUnionOrIntersectionType(`type`: UnionOrIntersectionType, flags: TypeFormatFlags) = {
 if ((flags&TypeFormatFlags.InElementType)) {
 writePunctuation( writer, SyntaxKind.OpenParenToken )

}
if ((`type`.flags&TypeFlags.Union)) {
 writeTypeList( formatUnionTypes( `type`.types ), SyntaxKind.BarToken )

}
else {
 writeTypeList( `type`.types, SyntaxKind.AmpersandToken )

}
if ((flags&TypeFormatFlags.InElementType)) {
 writePunctuation( writer, SyntaxKind.CloseParenToken )

}

}
def writeAnonymousType(`type`: ObjectType, flags: TypeFormatFlags) = {
 val symbol = `type`.symbol
if (symbol) {
 if ((symbol.flags&(((SymbolFlags.Class|SymbolFlags.Enum)|SymbolFlags.ValueModule)))) {
 writeTypeOfSymbol( `type`, flags )

}
else if (shouldWriteTypeOfFunctionSymbol()) {
 writeTypeOfSymbol( `type`, flags )

}
else if (contains( symbolStack, symbol )) {
 val typeAlias = getTypeAliasForTypeLiteral( `type` )
if ((typeAlias&&(!(`type`.asInstanceOf[AnonymousType]).target))) {
 buildSymbolDisplay( typeAlias, writer, enclosingDeclaration, SymbolFlags.Type, SymbolFormatFlags.None, flags )

}
else {
 writeKeyword( writer, SyntaxKind.AnyKeyword )

}

}
else {
 if ((!symbolStack)) {
 (symbolStack=Array())

}
symbolStack.push( symbol )
writeLiteralType( `type`, flags )
symbolStack.pop()

}

}
else {
 writeLiteralType( `type`, flags )

}
def shouldWriteTypeOfFunctionSymbol() = {
 val isStaticMethodSymbol = (!(!(((symbol.flags&SymbolFlags.Method)&&forEach( symbol.declarations, (declaration =>  (getModifierFlags( declaration )&ModifierFlags.Static)) )))))
val isNonLocalFunctionSymbol = ((!(!((symbol.flags&SymbolFlags.Function))))&&((symbol.parent||forEach( symbol.declarations, (declaration =>  ((declaration.parent.kind===SyntaxKind.SourceFile)||(declaration.parent.kind===SyntaxKind.ModuleBlock))) ))))
if ((isStaticMethodSymbol||isNonLocalFunctionSymbol)) {
 return ((!(!((flags&TypeFormatFlags.UseTypeOfFunction))))||(contains( symbolStack, symbol )))

}

}

}
def writeTypeOfSymbol(`type`: ObjectType, typeFormatFlags: TypeFormatFlags) = {
 writeKeyword( writer, SyntaxKind.TypeOfKeyword )
writeSpace( writer )
buildSymbolDisplay( `type`.symbol, writer, enclosingDeclaration, SymbolFlags.Value, SymbolFormatFlags.None, typeFormatFlags )

}
def writeIndexSignature(info: IndexInfo, keyword: SyntaxKind) = {
 if (info) {
 if (info.isReadonly) {
 writeKeyword( writer, SyntaxKind.ReadonlyKeyword )
writeSpace( writer )

}
writePunctuation( writer, SyntaxKind.OpenBracketToken )
writer.writeParameter( (if (info.declaration) declarationNameToString( info.declaration.parameters(0).name ) else "x") )
writePunctuation( writer, SyntaxKind.ColonToken )
writeSpace( writer )
writeKeyword( writer, keyword )
writePunctuation( writer, SyntaxKind.CloseBracketToken )
writePunctuation( writer, SyntaxKind.ColonToken )
writeSpace( writer )
writeType( info.`type`, TypeFormatFlags.None )
writePunctuation( writer, SyntaxKind.SemicolonToken )
writer.writeLine()

}

}
def writePropertyWithModifiers(prop: Symbol) = {
 if (isReadonlySymbol( prop )) {
 writeKeyword( writer, SyntaxKind.ReadonlyKeyword )
writeSpace( writer )

}
buildSymbolDisplay( prop, writer )
if ((prop.flags&SymbolFlags.Optional)) {
 writePunctuation( writer, SyntaxKind.QuestionToken )

}

}
def shouldAddParenthesisAroundFunctionType(callSignature: Signature, flags: TypeFormatFlags) = {
 if ((flags&TypeFormatFlags.InElementType)) {
 return true

}
else if ((flags&TypeFormatFlags.InFirstTypeArgument)) {
 val typeParameters = (if ((callSignature.target&&((flags&TypeFormatFlags.WriteTypeArgumentsOfSignature)))) callSignature.target.typeParameters else callSignature.typeParameters)
return (typeParameters&&(typeParameters.length!==0))

}
return false

}
def writeLiteralType(`type`: ObjectType, flags: TypeFormatFlags) = {
 val resolved = resolveStructuredTypeMembers( `type` )
if ((((!resolved.properties.length)&&(!resolved.stringIndexInfo))&&(!resolved.numberIndexInfo))) {
 if (((!resolved.callSignatures.length)&&(!resolved.constructSignatures.length))) {
 writePunctuation( writer, SyntaxKind.OpenBraceToken )
writePunctuation( writer, SyntaxKind.CloseBraceToken )
return

}
if (((resolved.callSignatures.length===1)&&(!resolved.constructSignatures.length))) {
 val parenthesizeSignature = shouldAddParenthesisAroundFunctionType( resolved.callSignatures(0), flags )
if (parenthesizeSignature) {
 writePunctuation( writer, SyntaxKind.OpenParenToken )

}
buildSignatureDisplay( resolved.callSignatures(0), writer, enclosingDeclaration, (globalFlagsToPass|TypeFormatFlags.WriteArrowStyleSignature), undefined, symbolStack )
if (parenthesizeSignature) {
 writePunctuation( writer, SyntaxKind.CloseParenToken )

}
return

}
if (((resolved.constructSignatures.length===1)&&(!resolved.callSignatures.length))) {
 if ((flags&TypeFormatFlags.InElementType)) {
 writePunctuation( writer, SyntaxKind.OpenParenToken )

}
writeKeyword( writer, SyntaxKind.NewKeyword )
writeSpace( writer )
buildSignatureDisplay( resolved.constructSignatures(0), writer, enclosingDeclaration, (globalFlagsToPass|TypeFormatFlags.WriteArrowStyleSignature), undefined, symbolStack )
if ((flags&TypeFormatFlags.InElementType)) {
 writePunctuation( writer, SyntaxKind.CloseParenToken )

}
return

}

}
val saveInObjectTypeLiteral = inObjectTypeLiteral
(inObjectTypeLiteral=true)
writePunctuation( writer, SyntaxKind.OpenBraceToken )
writer.writeLine()
writer.increaseIndent()
(resolved.callSignatures).foreach { fresh12 =>
val signature = zeroOfMyType
 = fresh12
 {
 buildSignatureDisplay( signature, writer, enclosingDeclaration, globalFlagsToPass, undefined, symbolStack )
writePunctuation( writer, SyntaxKind.SemicolonToken )
writer.writeLine()

}
}
(resolved.constructSignatures).foreach { fresh13 =>
val signature = zeroOfMyType
 = fresh13
 {
 buildSignatureDisplay( signature, writer, enclosingDeclaration, globalFlagsToPass, SignatureKind.Construct, symbolStack )
writePunctuation( writer, SyntaxKind.SemicolonToken )
writer.writeLine()

}
}
writeIndexSignature( resolved.stringIndexInfo, SyntaxKind.StringKeyword )
writeIndexSignature( resolved.numberIndexInfo, SyntaxKind.NumberKeyword )
(resolved.properties).foreach { fresh14 =>
val p = zeroOfMyType
 = fresh14
 {
 val t = getTypeOfSymbol( p )
if (((p.flags&((SymbolFlags.Function|SymbolFlags.Method)))&&(!getPropertiesOfObjectType( t ).length))) {
 val signatures = getSignaturesOfType( t, SignatureKind.Call )
(signatures).foreach { fresh15 =>
val signature = zeroOfMyType
 = fresh15
 {
 writePropertyWithModifiers( p )
buildSignatureDisplay( signature, writer, enclosingDeclaration, globalFlagsToPass, undefined, symbolStack )
writePunctuation( writer, SyntaxKind.SemicolonToken )
writer.writeLine()

}
}

}
else {
 writePropertyWithModifiers( p )
writePunctuation( writer, SyntaxKind.ColonToken )
writeSpace( writer )
writeType( t, TypeFormatFlags.None )
writePunctuation( writer, SyntaxKind.SemicolonToken )
writer.writeLine()

}

}
}
writer.decreaseIndent()
writePunctuation( writer, SyntaxKind.CloseBraceToken )
(inObjectTypeLiteral=saveInObjectTypeLiteral)

}

}
def buildTypeParameterDisplayFromSymbol(symbol: Symbol, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags) = {
 val targetSymbol = getTargetSymbol( symbol )
if ((((targetSymbol.flags&SymbolFlags.Class)||(targetSymbol.flags&SymbolFlags.Interface))||(targetSymbol.flags&SymbolFlags.TypeAlias))) {
 buildDisplayForTypeParametersAndDelimiters( getLocalTypeParametersOfClassOrInterfaceOrTypeAlias( symbol ), writer, enclosingDeclaration, flags )

}

}
def buildTypeParameterDisplay(tp: TypeParameter, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 appendSymbolNameOnly( tp.symbol, writer )
val constraint = getConstraintOfTypeParameter( tp )
if (constraint) {
 writeSpace( writer )
writeKeyword( writer, SyntaxKind.ExtendsKeyword )
writeSpace( writer )
buildTypeDisplay( constraint, writer, enclosingDeclaration, flags, symbolStack )

}

}
def buildParameterDisplay(p: Symbol, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 val parameterNode = p.valueDeclaration.asInstanceOf[ParameterDeclaration]
if (isRestParameter( parameterNode )) {
 writePunctuation( writer, SyntaxKind.DotDotDotToken )

}
if (isBindingPattern( parameterNode.name )) {
 buildBindingPatternDisplay( parameterNode.name.asInstanceOf[BindingPattern], writer, enclosingDeclaration, flags, symbolStack )

}
else {
 appendSymbolNameOnly( p, writer )

}
if (isOptionalParameter( parameterNode )) {
 writePunctuation( writer, SyntaxKind.QuestionToken )

}
writePunctuation( writer, SyntaxKind.ColonToken )
writeSpace( writer )
buildTypeDisplay( getTypeOfSymbol( p ), writer, enclosingDeclaration, flags, symbolStack )

}
def buildBindingPatternDisplay(bindingPattern: BindingPattern, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 if ((bindingPattern.kind===SyntaxKind.ObjectBindingPattern)) {
 writePunctuation( writer, SyntaxKind.OpenBraceToken )
buildDisplayForCommaSeparatedList( bindingPattern.elements, writer, (e =>  buildBindingElementDisplay( e, writer, enclosingDeclaration, flags, symbolStack )) )
writePunctuation( writer, SyntaxKind.CloseBraceToken )

}
else if ((bindingPattern.kind===SyntaxKind.ArrayBindingPattern)) {
 writePunctuation( writer, SyntaxKind.OpenBracketToken )
val elements = bindingPattern.elements
buildDisplayForCommaSeparatedList( elements, writer, (e =>  buildBindingElementDisplay( e, writer, enclosingDeclaration, flags, symbolStack )) )
if ((elements&&elements.hasTrailingComma)) {
 writePunctuation( writer, SyntaxKind.CommaToken )

}
writePunctuation( writer, SyntaxKind.CloseBracketToken )

}

}
def buildBindingElementDisplay(bindingElement: ArrayBindingElement, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 if (isOmittedExpression( bindingElement )) {
 return

}
Debug.assert( (bindingElement.kind===SyntaxKind.BindingElement) )
if (bindingElement.propertyName) {
 writer.writeSymbol( getTextOfNode( bindingElement.propertyName ), bindingElement.symbol )
writePunctuation( writer, SyntaxKind.ColonToken )
writeSpace( writer )

}
if (isBindingPattern( bindingElement.name )) {
 buildBindingPatternDisplay( bindingElement.name.asInstanceOf[BindingPattern], writer, enclosingDeclaration, flags, symbolStack )

}
else {
 if (bindingElement.dotDotDotToken) {
 writePunctuation( writer, SyntaxKind.DotDotDotToken )

}
appendSymbolNameOnly( bindingElement.symbol, writer )

}

}
def buildDisplayForTypeParametersAndDelimiters(typeParameters: Array[TypeParameter], writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 if ((typeParameters&&typeParameters.length)) {
 writePunctuation( writer, SyntaxKind.LessThanToken )
buildDisplayForCommaSeparatedList( typeParameters, writer, (p =>  buildTypeParameterDisplay( p, writer, enclosingDeclaration, flags, symbolStack )) )
writePunctuation( writer, SyntaxKind.GreaterThanToken )

}

}
def buildDisplayForCommaSeparatedList[T](list: Array[T], writer: SymbolWriter, action: ((T) => Unit)) = {
 {
var i = 0
while( (i<list.length)) {
 {
 if ((i>0)) {
 writePunctuation( writer, SyntaxKind.CommaToken )
writeSpace( writer )

}
action( list(i) )

}
 (i+= 1)
}
}

}
def buildDisplayForTypeArgumentsAndDelimiters(typeParameters: Array[TypeParameter], mapper: TypeMapper, writer: SymbolWriter, enclosingDeclaration: Node) = {
 if ((typeParameters&&typeParameters.length)) {
 writePunctuation( writer, SyntaxKind.LessThanToken )
var flags = TypeFormatFlags.InFirstTypeArgument
{
var i = 0
while( (i<typeParameters.length)) {
 {
 if ((i>0)) {
 writePunctuation( writer, SyntaxKind.CommaToken )
writeSpace( writer )
(flags=TypeFormatFlags.None)

}
buildTypeDisplay( mapper( typeParameters(i) ), writer, enclosingDeclaration, flags )

}
 (i+= 1)
}
}
writePunctuation( writer, SyntaxKind.GreaterThanToken )

}

}
def buildDisplayForParametersAndDelimiters(thisParameter: ( Symbol | undefined ), parameters: Array[Symbol], writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 writePunctuation( writer, SyntaxKind.OpenParenToken )
if (thisParameter) {
 buildParameterDisplay( thisParameter, writer, enclosingDeclaration, flags, symbolStack )

}
{
var i = 0
while( (i<parameters.length)) {
 {
 if (((i>0)||thisParameter)) {
 writePunctuation( writer, SyntaxKind.CommaToken )
writeSpace( writer )

}
buildParameterDisplay( parameters(i), writer, enclosingDeclaration, flags, symbolStack )

}
 (i+= 1)
}
}
writePunctuation( writer, SyntaxKind.CloseParenToken )

}
def buildTypePredicateDisplay(predicate: TypePredicate, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]): Unit = {
 if (isIdentifierTypePredicate( predicate )) {
 writer.writeParameter( predicate.parameterName )

}
else {
 writeKeyword( writer, SyntaxKind.ThisKeyword )

}
writeSpace( writer )
writeKeyword( writer, SyntaxKind.IsKeyword )
writeSpace( writer )
buildTypeDisplay( predicate.`type`, writer, enclosingDeclaration, flags, symbolStack )

}
def buildReturnTypeDisplay(signature: Signature, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, symbolStack: Array[Symbol]) = {
 if ((flags&TypeFormatFlags.WriteArrowStyleSignature)) {
 writeSpace( writer )
writePunctuation( writer, SyntaxKind.EqualsGreaterThanToken )

}
else {
 writePunctuation( writer, SyntaxKind.ColonToken )

}
writeSpace( writer )
if (signature.typePredicate) {
 buildTypePredicateDisplay( signature.typePredicate, writer, enclosingDeclaration, flags, symbolStack )

}
else {
 val returnType = getReturnTypeOfSignature( signature )
buildTypeDisplay( returnType, writer, enclosingDeclaration, flags, symbolStack )

}

}
def buildSignatureDisplay(signature: Signature, writer: SymbolWriter, enclosingDeclaration: Node, flags: TypeFormatFlags, kind: SignatureKind, symbolStack: Array[Symbol]) = {
 if ((kind===SignatureKind.Construct)) {
 writeKeyword( writer, SyntaxKind.NewKeyword )
writeSpace( writer )

}
if ((signature.target&&((flags&TypeFormatFlags.WriteTypeArgumentsOfSignature)))) {
 buildDisplayForTypeArgumentsAndDelimiters( signature.target.typeParameters, signature.mapper, writer, enclosingDeclaration )

}
else {
 buildDisplayForTypeParametersAndDelimiters( signature.typeParameters, writer, enclosingDeclaration, flags, symbolStack )

}
buildDisplayForParametersAndDelimiters( signature.thisParameter, signature.parameters, writer, enclosingDeclaration, flags, symbolStack )
buildReturnTypeDisplay( signature, writer, enclosingDeclaration, flags, symbolStack )

}
return (_displayBuilder||((_displayBuilder=Map( "buildSymbolDisplay" -> buildSymbolDisplay,
"buildTypeDisplay" -> buildTypeDisplay,
"buildTypeParameterDisplay" -> buildTypeParameterDisplay,
"buildTypePredicateDisplay" -> buildTypePredicateDisplay,
"buildParameterDisplay" -> buildParameterDisplay,
"buildDisplayForParametersAndDelimiters" -> buildDisplayForParametersAndDelimiters,
"buildDisplayForTypeParametersAndDelimiters" -> buildDisplayForTypeParametersAndDelimiters,
"buildTypeParameterDisplayFromSymbol" -> buildTypeParameterDisplayFromSymbol,
"buildSignatureDisplay" -> buildSignatureDisplay,
"buildReturnTypeDisplay" -> buildReturnTypeDisplay ))))

}
def isDeclarationVisible(node: Declaration): Boolean = {
 if (node) {
 val links = getNodeLinks( node )
if ((links.isVisible===undefined)) {
 (links.isVisible=(!(!determineIfDeclarationIsVisible())))

}
return links.isVisible

}
return false
def determineIfDeclarationIsVisible() = {
 node.kind match {
  case  SyntaxKind.BindingElement  =>
return isDeclarationVisible( node.parent.parent.asInstanceOf[Declaration] )
  case  SyntaxKind.VariableDeclaration  =>
if ((isBindingPattern( node.name )&&(!(node.name.asInstanceOf[BindingPattern]).elements.length))) {
 return false

}
  case  SyntaxKind.ModuleDeclaration | SyntaxKind.ClassDeclaration | SyntaxKind.InterfaceDeclaration | SyntaxKind.TypeAliasDeclaration | SyntaxKind.FunctionDeclaration | SyntaxKind.EnumDeclaration | SyntaxKind.ImportEqualsDeclaration  =>
if (isExternalModuleAugmentation( node )) {
 return true

}
val parent = getDeclarationContainer( node )
if (((!((getCombinedModifierFlags( node )&ModifierFlags.Export)))&&(!((((node.kind!==SyntaxKind.ImportEqualsDeclaration)&&(parent.kind!==SyntaxKind.SourceFile))&&isInAmbientContext( parent )))))) {
 return isGlobalSourceFile( parent )

}
return isDeclarationVisible( parent.asInstanceOf[Declaration] )
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature  =>
if ((getModifierFlags( node )&((ModifierFlags.Private|ModifierFlags.Protected)))) {
 return false

}
  case  SyntaxKind.Constructor | SyntaxKind.ConstructSignature | SyntaxKind.CallSignature | SyntaxKind.IndexSignature | SyntaxKind.Parameter | SyntaxKind.ModuleBlock | SyntaxKind.FunctionType | SyntaxKind.ConstructorType | SyntaxKind.TypeLiteral | SyntaxKind.TypeReference | SyntaxKind.ArrayType | SyntaxKind.TupleType | SyntaxKind.UnionType | SyntaxKind.IntersectionType | SyntaxKind.ParenthesizedType  =>
return isDeclarationVisible( node.parent.asInstanceOf[Declaration] )
  case  SyntaxKind.ImportClause | SyntaxKind.NamespaceImport | SyntaxKind.ImportSpecifier  =>
return false
  case  SyntaxKind.TypeParameter | SyntaxKind.SourceFile | SyntaxKind.NamespaceExportDeclaration  =>
return true
  case  SyntaxKind.ExportAssignment  =>
return false
  case _ =>
return false
}

}

}
def collectLinkedAliases(node: Identifier): Array[Node] = {
 var exportSymbol: Symbol = zeroOfMyType
if ((node.parent&&(node.parent.kind===SyntaxKind.ExportAssignment))) {
 (exportSymbol=resolveName( node.parent, node.text, (((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace)|SymbolFlags.Alias), Diagnostics.Cannot_find_name_0, node ))

}
else if ((node.parent.kind===SyntaxKind.ExportSpecifier)) {
 val exportSpecifier = node.parent.asInstanceOf[ExportSpecifier]
(exportSymbol=(if ((exportSpecifier.parent.parent.asInstanceOf[ExportDeclaration]).moduleSpecifier) getExternalModuleMember( exportSpecifier.parent.parent.asInstanceOf[ExportDeclaration], exportSpecifier ) else resolveEntityName( (exportSpecifier.propertyName||exportSpecifier.name), (((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace)|SymbolFlags.Alias) )))

}
val result: Array[Node] = Array()
if (exportSymbol) {
 buildVisibleNodeList( exportSymbol.declarations )

}
return result
def buildVisibleNodeList(declarations: Array[Declaration]) = {
 forEach( declarations, (declaration =>  {
 (getNodeLinks( declaration ).isVisible=true)
 val resultNode = (getAnyImportSyntax( declaration )||declaration)
 if ((!contains( result, resultNode ))) {
 result.push( resultNode )

}
 if (isInternalModuleImportEqualsDeclaration( declaration )) {
 val internalModuleReference = (declaration.asInstanceOf[ImportEqualsDeclaration]).moduleReference.asInstanceOf[( Identifier | QualifiedName )]
val firstIdentifier = getFirstIdentifier( internalModuleReference )
val importSymbol = resolveName( declaration, firstIdentifier.text, ((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace), undefined, undefined )
if (importSymbol) {
 buildVisibleNodeList( importSymbol.declarations )

}

}

}) )

}

}
def pushTypeResolution(target: TypeSystemEntity, propertyName: TypeSystemPropertyName): Boolean = {
 val resolutionCycleStartIndex = findResolutionCycleStartIndex( target, propertyName )
if ((resolutionCycleStartIndex>=0)) {
 const fresh16 = resolutionTargets
val length = fresh16.length
{
var i = resolutionCycleStartIndex
while( (i<length)) {
 {
 (resolutionResults(i)=false)

}
 (i+= 1)
}
}
return false

}
resolutionTargets.push( target )
resolutionResults.push( true )
resolutionPropertyNames.push( propertyName )
return true

}
def findResolutionCycleStartIndex(target: TypeSystemEntity, propertyName: TypeSystemPropertyName): Int = {
 {
var i = (resolutionTargets.length-1)
while( (i>=0)) {
 {
 if (hasType( resolutionTargets(i), resolutionPropertyNames(i) )) {
 return (-1)

}
if (((resolutionTargets(i)===target)&&(resolutionPropertyNames(i)===propertyName))) {
 return i

}

}
 (i-= 1)
}
}
return (-1)

}
def hasType(target: TypeSystemEntity, propertyName: TypeSystemPropertyName): Type = {
 if ((propertyName===TypeSystemPropertyName.Type)) {
 return getSymbolLinks( target.asInstanceOf[Symbol] ).`type`

}
if ((propertyName===TypeSystemPropertyName.DeclaredType)) {
 return getSymbolLinks( target.asInstanceOf[Symbol] ).declaredType

}
if ((propertyName===TypeSystemPropertyName.ResolvedBaseConstructorType)) {
 return (target.asInstanceOf[InterfaceType]).resolvedBaseConstructorType

}
if ((propertyName===TypeSystemPropertyName.ResolvedReturnType)) {
 return (target.asInstanceOf[Signature]).resolvedReturnType

}
Debug.fail( ("Unhandled TypeSystemPropertyName "+propertyName) )

}
def popTypeResolution(): Boolean = {
 resolutionTargets.pop()
resolutionPropertyNames.pop()
return resolutionResults.pop()

}
def getDeclarationContainer(node: Node): Node = {
 (node=getRootDeclaration( node ))
while (node) {
{
 node.kind match {
  case  SyntaxKind.VariableDeclaration | SyntaxKind.VariableDeclarationList | SyntaxKind.ImportSpecifier | SyntaxKind.NamedImports | SyntaxKind.NamespaceImport | SyntaxKind.ImportClause  =>
(node=node.parent)
  case _ =>
return node.parent
}

}
}

}
def getTypeOfPrototypeProperty(prototype: Symbol): Type = {
 val classType = getDeclaredTypeOfSymbol( getParentOfSymbol( prototype ) ).asInstanceOf[InterfaceType]
return (if (classType.typeParameters) createTypeReference( classType.asInstanceOf[GenericType], map( classType.typeParameters, (_underscore_ =>  anyType) ) ) else classType)

}
def getTypeOfPropertyOfType(`type`: Type, name: String): Type = {
 val prop = getPropertyOfType( `type`, name )
return (if (prop) getTypeOfSymbol( prop ) else undefined)

}
def isTypeAny(`type`: Type) = {
 return (`type`&&(((`type`.flags&TypeFlags.Any))!==0))

}
def isTypeNever(`type`: Type) = {
 return (`type`&&(((`type`.flags&TypeFlags.Never))!==0))

}
def getTypeForBindingElementParent(node: VariableLikeDeclaration) = {
 val symbol = getSymbolOfNode( node )
return ((symbol&&getSymbolLinks( symbol ).`type`)||getTypeForVariableLikeDeclaration( node, false ))

}
def getTextOfPropertyName(name: PropertyName): String = {
 name.kind match {
  case  SyntaxKind.Identifier  =>
return (name.asInstanceOf[Identifier]).text
  case  SyntaxKind.StringLiteral | SyntaxKind.NumericLiteral  =>
return (name.asInstanceOf[LiteralExpression]).text
  case  SyntaxKind.ComputedPropertyName  =>
if (isStringOrNumericLiteral( (name.asInstanceOf[ComputedPropertyName]).expression.kind )) {
 return ((name.asInstanceOf[ComputedPropertyName]).expression.asInstanceOf[LiteralExpression]).text

}
  case _ =>
}
return undefined

}
def isComputedNonLiteralName(name: PropertyName): Boolean = {
 return ((name.kind===SyntaxKind.ComputedPropertyName)&&(!isStringOrNumericLiteral( (name.asInstanceOf[ComputedPropertyName]).expression.kind )))

}
def getTypeForBindingElement(declaration: BindingElement): Type = {
 val pattern = declaration.parent.asInstanceOf[BindingPattern]
val parentType = getTypeForBindingElementParent( pattern.parent.asInstanceOf[VariableLikeDeclaration] )
if ((parentType===unknownType)) {
 return unknownType

}
if (((!parentType)||isTypeAny( parentType ))) {
 if (declaration.initializer) {
 return checkDeclarationInitializer( declaration )

}
return parentType

}
var `type`: Type = zeroOfMyType
if ((pattern.kind===SyntaxKind.ObjectBindingPattern)) {
 val name = (declaration.propertyName||declaration.name.asInstanceOf[Identifier])
if (isComputedNonLiteralName( name )) {
 return anyType

}
if (declaration.initializer) {
 getContextualType( declaration.initializer )

}
val text = getTextOfPropertyName( name )
(`type`=((getTypeOfPropertyOfType( parentType, text )||(isNumericLiteralName( text )&&getIndexTypeOfType( parentType, IndexKind.Number )))||getIndexTypeOfType( parentType, IndexKind.String )))
if ((!`type`)) {
 error( name, Diagnostics.Type_0_has_no_property_1_and_no_string_index_signature, typeToString( parentType ), declarationNameToString( name ) )
return unknownType

}

}
else {
 val elementType = checkIteratedTypeOrElementType( parentType, pattern, false )
if ((!declaration.dotDotDotToken)) {
 val propName = (""+indexOf( pattern.elements, declaration ))
(`type`=(if (isTupleLikeType( parentType )) getTypeOfPropertyOfType( parentType, propName ) else elementType))
if ((!`type`)) {
 if (isTupleType( parentType )) {
 error( declaration, Diagnostics.Tuple_type_0_with_length_1_cannot_be_assigned_to_tuple_with_length_2, typeToString( parentType ), getTypeReferenceArity( parentType.asInstanceOf[TypeReference] ), pattern.elements.length )

}
else {
 error( declaration, Diagnostics.Type_0_has_no_property_1, typeToString( parentType ), propName )

}
return unknownType

}

}
else {
 (`type`=createArrayType( elementType ))

}

}
if (((strictNullChecks&&declaration.initializer)&&(!((getFalsyFlags( checkExpressionCached( declaration.initializer ) )&TypeFlags.Undefined))))) {
 (`type`=getTypeWithFacts( `type`, TypeFacts.NEUndefined ))

}
return (if (declaration.initializer) getUnionType( Array( `type`, checkExpressionCached( declaration.initializer ) ), true ) else `type`)

}
def getTypeForVariableLikeDeclarationFromJSDocComment(declaration: VariableLikeDeclaration) = {
 val jsDocType = getJSDocTypeForVariableLikeDeclarationFromJSDocComment( declaration )
if (jsDocType) {
 return getTypeFromTypeNode( jsDocType )

}

}
def getJSDocTypeForVariableLikeDeclarationFromJSDocComment(declaration: VariableLikeDeclaration): JSDocType = {
 val typeTag = getJSDocTypeTag( declaration )
if ((typeTag&&typeTag.typeExpression)) {
 return typeTag.typeExpression.`type`

}
if ((((declaration.kind===SyntaxKind.VariableDeclaration)&&(declaration.parent.kind===SyntaxKind.VariableDeclarationList))&&(declaration.parent.parent.kind===SyntaxKind.VariableStatement))) {
 val annotation = getJSDocTypeTag( declaration.parent.parent )
if ((annotation&&annotation.typeExpression)) {
 return annotation.typeExpression.`type`

}

}
else if ((declaration.kind===SyntaxKind.Parameter)) {
 val paramTag = getCorrespondingJSDocParameterTag( declaration.asInstanceOf[ParameterDeclaration] )
if ((paramTag&&paramTag.typeExpression)) {
 return paramTag.typeExpression.`type`

}

}
return undefined

}
def isNullOrUndefined(node: Expression) = {
 val expr = skipParentheses( node )
return ((expr.kind===SyntaxKind.NullKeyword)||((expr.kind===SyntaxKind.Identifier)&&(getResolvedSymbol( expr.asInstanceOf[Identifier] )===undefinedSymbol)))

}
def isEmptyArrayLiteral(node: Expression) = {
 val expr = skipParentheses( node )
return ((expr.kind===SyntaxKind.ArrayLiteralExpression)&&((expr.asInstanceOf[ArrayLiteralExpression]).elements.length===0))

}
def addOptionality(`type`: Type, optional: Boolean): Type = {
 return (if ((strictNullChecks&&optional)) includeFalsyTypes( `type`, TypeFlags.Undefined ) else `type`)

}
def getTypeForVariableLikeDeclaration(declaration: VariableLikeDeclaration, includeOptionality: Boolean): Type = {
 if ((declaration.flags&NodeFlags.JavaScriptFile)) {
 val `type` = getTypeForVariableLikeDeclarationFromJSDocComment( declaration )
if ((`type`&&(`type`!==unknownType))) {
 return `type`

}

}
if ((declaration.parent.parent.kind===SyntaxKind.ForInStatement)) {
 return stringType

}
if ((declaration.parent.parent.kind===SyntaxKind.ForOfStatement)) {
 return (checkRightHandSideOfForOf( (declaration.parent.parent.asInstanceOf[ForOfStatement]).expression )||anyType)

}
if (isBindingPattern( declaration.parent )) {
 return getTypeForBindingElement( declaration.asInstanceOf[BindingElement] )

}
if (declaration.`type`) {
 return addOptionality( getTypeFromTypeNode( declaration.`type` ), (declaration.questionToken&&includeOptionality) )

}
if (((((declaration.kind===SyntaxKind.VariableDeclaration)&&(!isBindingPattern( declaration.name )))&&(!((getCombinedModifierFlags( declaration )&ModifierFlags.Export))))&&(!isInAmbientContext( declaration )))) {
 if (((!((getCombinedNodeFlags( declaration )&NodeFlags.Const)))&&(((!declaration.initializer)||isNullOrUndefined( declaration.initializer ))))) {
 return autoType

}
if ((declaration.initializer&&isEmptyArrayLiteral( declaration.initializer ))) {
 return autoArrayType

}

}
if ((declaration.kind===SyntaxKind.Parameter)) {
 val func = declaration.parent.asInstanceOf[FunctionLikeDeclaration]
if (((func.kind===SyntaxKind.SetAccessor)&&(!hasDynamicName( func )))) {
 val getter = getDeclarationOfKind( declaration.parent.symbol, SyntaxKind.GetAccessor ).asInstanceOf[AccessorDeclaration]
if (getter) {
 val getterSignature = getSignatureFromDeclaration( getter )
val thisParameter = getAccessorThisParameter( func.asInstanceOf[AccessorDeclaration] )
if ((thisParameter&&(declaration===thisParameter))) {
 Debug.assert( (!thisParameter.`type`) )
return getTypeOfSymbol( getterSignature.thisParameter )

}
return getReturnTypeOfSignature( getterSignature )

}

}
var `type`: Type = zeroOfMyType
if ((declaration.symbol.name==="this")) {
 (`type`=getContextualThisParameterType( func ))

}
else {
 (`type`=getContextuallyTypedParameterType( declaration.asInstanceOf[ParameterDeclaration] ))

}
if (`type`) {
 return addOptionality( `type`, (declaration.questionToken&&includeOptionality) )

}

}
if (declaration.initializer) {
 val `type` = checkDeclarationInitializer( declaration )
return addOptionality( `type`, (declaration.questionToken&&includeOptionality) )

}
if ((declaration.kind===SyntaxKind.ShorthandPropertyAssignment)) {
 return checkIdentifier( declaration.name.asInstanceOf[Identifier] )

}
if (isBindingPattern( declaration.name )) {
 return getTypeFromBindingPattern( declaration.name.asInstanceOf[BindingPattern], false, true )

}
return undefined

}
def getTypeFromBindingElement(element: BindingElement, includePatternInType: Boolean, reportErrors: Boolean): Type = {
 if (element.initializer) {
 return checkDeclarationInitializer( element )

}
if (isBindingPattern( element.name )) {
 return getTypeFromBindingPattern( element.name.asInstanceOf[BindingPattern], includePatternInType, reportErrors )

}
if (((reportErrors&&compilerOptions.noImplicitAny)&&(!declarationBelongsToPrivateAmbientMember( element )))) {
 reportImplicitAnyError( element, anyType )

}
return anyType

}
def getTypeFromObjectBindingPattern(pattern: ObjectBindingPattern, includePatternInType: Boolean, reportErrors: Boolean): Type = {
 val members = createMap[ Symbol ]()
var hasComputedProperties = false
forEach( pattern.elements, (e =>  {
 val name = (e.propertyName||e.name.asInstanceOf[Identifier])
 if (isComputedNonLiteralName( name )) {
 (hasComputedProperties=true)
return

}
 val text = getTextOfPropertyName( name )
 val flags = ((SymbolFlags.Property|SymbolFlags.Transient)|((if (e.initializer) SymbolFlags.Optional else 0)))
 val symbol = createSymbol( flags, text ).asInstanceOf[TransientSymbol]
 (symbol.`type`=getTypeFromBindingElement( e, includePatternInType, reportErrors ))
 (symbol.bindingElement=e)
 (members(symbol.name)=symbol)

}) )
val result = createAnonymousType( undefined, members, emptyArray, emptyArray, undefined, undefined )
if (includePatternInType) {
 (result.pattern=pattern)

}
if (hasComputedProperties) {
 (result.objectFlags|=ObjectFlags.ObjectLiteralPatternWithComputedProperties)

}
return result

}
def getTypeFromArrayBindingPattern(pattern: BindingPattern, includePatternInType: Boolean, reportErrors: Boolean): Type = {
 val elements = pattern.elements
val lastElement = lastOrUndefined( elements )
if (((elements.length===0)||(((!isOmittedExpression( lastElement ))&&lastElement.dotDotDotToken)))) {
 return (if ((languageVersion>=ScriptTarget.ES2015)) createIterableType( anyType ) else anyArrayType)

}
val elementTypes = map( elements, (e =>  (if (isOmittedExpression( e )) anyType else getTypeFromBindingElement( e, includePatternInType, reportErrors ))) )
var result = createTupleType( elementTypes )
if (includePatternInType) {
 (result=cloneTypeReference( result ))
(result.pattern=pattern)

}
return result

}
def getTypeFromBindingPattern(pattern: BindingPattern, includePatternInType: Boolean, reportErrors: Boolean): Type = {
 return (if ((pattern.kind===SyntaxKind.ObjectBindingPattern)) getTypeFromObjectBindingPattern( pattern.asInstanceOf[ObjectBindingPattern], includePatternInType, reportErrors ) else getTypeFromArrayBindingPattern( pattern.asInstanceOf[ArrayBindingPattern], includePatternInType, reportErrors ))

}
def getWidenedTypeForVariableLikeDeclaration(declaration: VariableLikeDeclaration, reportErrors: Boolean): Type = {
 var `type` = getTypeForVariableLikeDeclaration( declaration, true )
if (`type`) {
 if (reportErrors) {
 reportErrorsFromWidening( declaration, `type` )

}
if ((declaration.kind===SyntaxKind.PropertyAssignment)) {
 return `type`

}
return getWidenedType( `type` )

}
(`type`=(if (declaration.dotDotDotToken) anyArrayType else anyType))
if ((reportErrors&&compilerOptions.noImplicitAny)) {
 if ((!declarationBelongsToPrivateAmbientMember( declaration ))) {
 reportImplicitAnyError( declaration, `type` )

}

}
return `type`

}
def declarationBelongsToPrivateAmbientMember(declaration: VariableLikeDeclaration) = {
 val root = getRootDeclaration( declaration )
val memberDeclaration = (if ((root.kind===SyntaxKind.Parameter)) root.parent else root)
return isPrivateWithinAmbient( memberDeclaration )

}
def getTypeOfVariableOrParameterOrProperty(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.`type`)) {
 if ((symbol.flags&SymbolFlags.Prototype)) {
 return (links.`type`=getTypeOfPrototypeProperty( symbol ))

}
val declaration = symbol.valueDeclaration
if (isCatchClauseVariableDeclarationOrBindingElement( declaration )) {
 return (links.`type`=anyType)

}
if ((declaration.kind===SyntaxKind.ExportAssignment)) {
 return (links.`type`=checkExpression( (declaration.asInstanceOf[ExportAssignment]).expression ))

}
if ((((declaration.flags&NodeFlags.JavaScriptFile)&&(declaration.kind===SyntaxKind.JSDocPropertyTag))&&(declaration.asInstanceOf[JSDocPropertyTag]).typeExpression)) {
 return (links.`type`=getTypeFromTypeNode( (declaration.asInstanceOf[JSDocPropertyTag]).typeExpression.`type` ))

}
if ((!pushTypeResolution( symbol, TypeSystemPropertyName.Type ))) {
 return unknownType

}
var `type`: Type = zeroOfMyType
if (((declaration.kind===SyntaxKind.BinaryExpression)||((declaration.kind===SyntaxKind.PropertyAccessExpression)&&(declaration.parent.kind===SyntaxKind.BinaryExpression)))) {
 if ((declaration.flags&NodeFlags.JavaScriptFile)) {
 val typeTag = getJSDocTypeTag( declaration.parent )
if ((typeTag&&typeTag.typeExpression)) {
 return (links.`type`=getTypeFromTypeNode( typeTag.typeExpression.`type` ))

}

}
val declaredTypes = map( symbol.declarations, (decl =>  (if ((decl.kind===SyntaxKind.BinaryExpression)) checkExpressionCached( (decl.asInstanceOf[BinaryExpression]).right ) else checkExpressionCached( (decl.parent.asInstanceOf[BinaryExpression]).right ))) )
(`type`=getUnionType( declaredTypes, true ))

}
else {
 (`type`=getWidenedTypeForVariableLikeDeclaration( declaration.asInstanceOf[VariableLikeDeclaration], true ))

}
if ((!popTypeResolution())) {
 if ((symbol.valueDeclaration.asInstanceOf[VariableLikeDeclaration]).`type`) {
 (`type`=unknownType)
error( symbol.valueDeclaration, Diagnostics._0_is_referenced_directly_or_indirectly_in_its_own_type_annotation, symbolToString( symbol ) )

}
else {
 (`type`=anyType)
if (compilerOptions.noImplicitAny) {
 error( symbol.valueDeclaration, Diagnostics._0_implicitly_has_type_any_because_it_does_not_have_a_type_annotation_and_is_referenced_directly_or_indirectly_in_its_own_initializer, symbolToString( symbol ) )

}

}

}
(links.`type`=`type`)

}
return links.`type`

}
def getAnnotatedAccessorType(accessor: AccessorDeclaration): Type = {
 if (accessor) {
 if ((accessor.kind===SyntaxKind.GetAccessor)) {
 return (accessor.`type`&&getTypeFromTypeNode( accessor.`type` ))

}
else {
 val setterTypeAnnotation = getSetAccessorTypeAnnotationNode( accessor )
return (setterTypeAnnotation&&getTypeFromTypeNode( setterTypeAnnotation ))

}

}
return undefined

}
def getAnnotatedAccessorThisParameter(accessor: AccessorDeclaration): ( Symbol | undefined ) = {
 val parameter = getAccessorThisParameter( accessor )
return (parameter&&parameter.symbol)

}
def getThisTypeOfDeclaration(declaration: SignatureDeclaration): ( Type | undefined ) = {
 return getThisTypeOfSignature( getSignatureFromDeclaration( declaration ) )

}
def getTypeOfAccessors(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.`type`)) {
 val getter = getDeclarationOfKind( symbol, SyntaxKind.GetAccessor ).asInstanceOf[AccessorDeclaration]
val setter = getDeclarationOfKind( symbol, SyntaxKind.SetAccessor ).asInstanceOf[AccessorDeclaration]
if ((getter&&(getter.flags&NodeFlags.JavaScriptFile))) {
 val jsDocType = getTypeForVariableLikeDeclarationFromJSDocComment( getter )
if (jsDocType) {
 return (links.`type`=jsDocType)

}

}
if ((!pushTypeResolution( symbol, TypeSystemPropertyName.Type ))) {
 return unknownType

}
var `type`: Type = zeroOfMyType
val getterReturnType = getAnnotatedAccessorType( getter )
if (getterReturnType) {
 (`type`=getterReturnType)

}
else {
 val setterParameterType = getAnnotatedAccessorType( setter )
if (setterParameterType) {
 (`type`=setterParameterType)

}
else {
 if ((getter&&getter.body)) {
 (`type`=getReturnTypeFromBody( getter ))

}
else {
 if (compilerOptions.noImplicitAny) {
 if (setter) {
 error( setter, Diagnostics.Property_0_implicitly_has_type_any_because_its_set_accessor_lacks_a_parameter_type_annotation, symbolToString( symbol ) )

}
else {
 Debug.assert( (!(!getter)), "there must existed getter as we are current checking either setter or getter in this function" )
error( getter, Diagnostics.Property_0_implicitly_has_type_any_because_its_get_accessor_lacks_a_return_type_annotation, symbolToString( symbol ) )

}

}
(`type`=anyType)

}

}

}
if ((!popTypeResolution())) {
 (`type`=anyType)
if (compilerOptions.noImplicitAny) {
 val getter = getDeclarationOfKind( symbol, SyntaxKind.GetAccessor ).asInstanceOf[AccessorDeclaration]
error( getter, Diagnostics._0_implicitly_has_return_type_any_because_it_does_not_have_a_return_type_annotation_and_is_referenced_directly_or_indirectly_in_one_of_its_return_expressions, symbolToString( symbol ) )

}

}
(links.`type`=`type`)

}
return links.`type`

}
def getTypeOfFuncClassEnumModule(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.`type`)) {
 if (((symbol.valueDeclaration.kind===SyntaxKind.ModuleDeclaration)&&isShorthandAmbientModuleSymbol( symbol ))) {
 (links.`type`=anyType)

}
else {
 val `type` = createObjectType( ObjectFlags.Anonymous, symbol )
(links.`type`=(if ((strictNullChecks&&(symbol.flags&SymbolFlags.Optional))) includeFalsyTypes( `type`, TypeFlags.Undefined ) else `type`))

}

}
return links.`type`

}
def getTypeOfEnumMember(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.`type`)) {
 (links.`type`=getDeclaredTypeOfEnumMember( symbol ))

}
return links.`type`

}
def getTypeOfAlias(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.`type`)) {
 val targetSymbol = resolveAlias( symbol )
(links.`type`=(if ((targetSymbol.flags&SymbolFlags.Value)) getTypeOfSymbol( targetSymbol ) else unknownType))

}
return links.`type`

}
def getTypeOfInstantiatedSymbol(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.`type`)) {
 (links.`type`=instantiateType( getTypeOfSymbol( links.target ), links.mapper ))

}
return links.`type`

}
def getTypeOfSymbol(symbol: Symbol): Type = {
 if ((symbol.flags&SymbolFlags.Instantiated)) {
 return getTypeOfInstantiatedSymbol( symbol )

}
if ((symbol.flags&((SymbolFlags.Variable|SymbolFlags.Property)))) {
 return getTypeOfVariableOrParameterOrProperty( symbol )

}
if ((symbol.flags&(((((SymbolFlags.Function|SymbolFlags.Method)|SymbolFlags.Class)|SymbolFlags.Enum)|SymbolFlags.ValueModule)))) {
 return getTypeOfFuncClassEnumModule( symbol )

}
if ((symbol.flags&SymbolFlags.EnumMember)) {
 return getTypeOfEnumMember( symbol )

}
if ((symbol.flags&SymbolFlags.Accessor)) {
 return getTypeOfAccessors( symbol )

}
if ((symbol.flags&SymbolFlags.Alias)) {
 return getTypeOfAlias( symbol )

}
return unknownType

}
def getTargetType(`type`: Type): Type = {
 return (if ((getObjectFlags( `type` )&ObjectFlags.Reference)) (`type`.asInstanceOf[TypeReference]).target else `type`)

}
def hasBaseType(`type`: InterfaceType, checkBase: InterfaceType) = {
 return check( `type` )
def check(`type`: InterfaceType): Boolean = {
 val target = getTargetType( `type` ).asInstanceOf[InterfaceType]
return ((target===checkBase)||forEach( getBaseTypes( target ), check ))

}

}
def appendTypeParameters(typeParameters: Array[TypeParameter], declarations: Array[TypeParameterDeclaration]): Array[TypeParameter] = {
 (declarations).foreach { fresh17 =>
val declaration = zeroOfMyType
 = fresh17
 {
 val tp = getDeclaredTypeOfTypeParameter( getSymbolOfNode( declaration ) )
if ((!typeParameters)) {
 (typeParameters=Array( tp ))

}
else if ((!contains( typeParameters, tp ))) {
 typeParameters.push( tp )

}

}
}
return typeParameters

}
def appendOuterTypeParameters(typeParameters: Array[TypeParameter], node: Node): Array[TypeParameter] = {
 while (true) {
{
 (node=node.parent)
if ((!node)) {
 return typeParameters

}
if (((((((node.kind===SyntaxKind.ClassDeclaration)||(node.kind===SyntaxKind.ClassExpression))||(node.kind===SyntaxKind.FunctionDeclaration))||(node.kind===SyntaxKind.FunctionExpression))||(node.kind===SyntaxKind.MethodDeclaration))||(node.kind===SyntaxKind.ArrowFunction))) {
 val declarations = (node.asInstanceOf[( ClassLikeDeclaration | FunctionLikeDeclaration )]).typeParameters
if (declarations) {
 return appendTypeParameters( appendOuterTypeParameters( typeParameters, node ), declarations )

}

}

}
}

}
def getOuterTypeParametersOfClassOrInterface(symbol: Symbol): Array[TypeParameter] = {
 val declaration = (if ((symbol.flags&SymbolFlags.Class)) symbol.valueDeclaration else getDeclarationOfKind( symbol, SyntaxKind.InterfaceDeclaration ))
return appendOuterTypeParameters( undefined, declaration )

}
def getLocalTypeParametersOfClassOrInterfaceOrTypeAlias(symbol: Symbol): Array[TypeParameter] = {
 var result: Array[TypeParameter] = zeroOfMyType
(symbol.declarations).foreach { fresh18 =>
val node = zeroOfMyType
 = fresh18
 {
 if (((((node.kind===SyntaxKind.InterfaceDeclaration)||(node.kind===SyntaxKind.ClassDeclaration))||(node.kind===SyntaxKind.ClassExpression))||(node.kind===SyntaxKind.TypeAliasDeclaration))) {
 val declaration = node.asInstanceOf[( InterfaceDeclaration | TypeAliasDeclaration )]
if (declaration.typeParameters) {
 (result=appendTypeParameters( result, declaration.typeParameters ))

}

}

}
}
return result

}
def getTypeParametersOfClassOrInterface(symbol: Symbol): Array[TypeParameter] = {
 return concatenate( getOuterTypeParametersOfClassOrInterface( symbol ), getLocalTypeParametersOfClassOrInterfaceOrTypeAlias( symbol ) )

}
def isConstructorType(`type`: Type): Boolean = {
 return ((`type`.flags&TypeFlags.Object)&&(getSignaturesOfType( `type`, SignatureKind.Construct ).length>0))

}
def getBaseTypeNodeOfClass(`type`: InterfaceType): ExpressionWithTypeArguments = {
 return getClassExtendsHeritageClauseElement( `type`.symbol.valueDeclaration.asInstanceOf[ClassLikeDeclaration] )

}
def getConstructorsForTypeArguments(`type`: Type, typeArgumentNodes: Array[TypeNode]): Array[Signature] = {
 val typeArgCount = (if (typeArgumentNodes) typeArgumentNodes.length else 0)
return filter( getSignaturesOfType( `type`, SignatureKind.Construct ), (sig =>  (((if (sig.typeParameters) sig.typeParameters.length else 0))===typeArgCount)) )

}
def getInstantiatedConstructorsForTypeArguments(`type`: Type, typeArgumentNodes: Array[TypeNode]): Array[Signature] = {
 var signatures = getConstructorsForTypeArguments( `type`, typeArgumentNodes )
if (typeArgumentNodes) {
 val typeArguments = map( typeArgumentNodes, getTypeFromTypeNodeNoAlias )
(signatures=map( signatures, (sig =>  getSignatureInstantiation( sig, typeArguments )) ))

}
return signatures

}
def getBaseConstructorTypeOfClass(`type`: InterfaceType): Type = {
 if ((!`type`.resolvedBaseConstructorType)) {
 val baseTypeNode = getBaseTypeNodeOfClass( `type` )
if ((!baseTypeNode)) {
 return (`type`.resolvedBaseConstructorType=undefinedType)

}
if ((!pushTypeResolution( `type`, TypeSystemPropertyName.ResolvedBaseConstructorType ))) {
 return unknownType

}
val baseConstructorType = checkExpression( baseTypeNode.expression )
if ((baseConstructorType.flags&TypeFlags.Object)) {
 resolveStructuredTypeMembers( baseConstructorType.asInstanceOf[ObjectType] )

}
if ((!popTypeResolution())) {
 error( `type`.symbol.valueDeclaration, Diagnostics._0_is_referenced_directly_or_indirectly_in_its_own_base_expression, symbolToString( `type`.symbol ) )
return (`type`.resolvedBaseConstructorType=unknownType)

}
if ((((baseConstructorType!==unknownType)&&(baseConstructorType!==nullWideningType))&&(!isConstructorType( baseConstructorType )))) {
 error( baseTypeNode.expression, Diagnostics.Type_0_is_not_a_constructor_function_type, typeToString( baseConstructorType ) )
return (`type`.resolvedBaseConstructorType=unknownType)

}
(`type`.resolvedBaseConstructorType=baseConstructorType)

}
return `type`.resolvedBaseConstructorType

}
def getBaseTypes(`type`: InterfaceType): Array[ObjectType] = {
 if ((!`type`.resolvedBaseTypes)) {
 if ((`type`.objectFlags&ObjectFlags.Tuple)) {
 (`type`.resolvedBaseTypes=Array( createArrayType( getUnionType( `type`.typeParameters ) ) ))

}
else if ((`type`.symbol.flags&((SymbolFlags.Class|SymbolFlags.Interface)))) {
 if ((`type`.symbol.flags&SymbolFlags.Class)) {
 resolveBaseTypesOfClass( `type` )

}
if ((`type`.symbol.flags&SymbolFlags.Interface)) {
 resolveBaseTypesOfInterface( `type` )

}

}
else {
 Debug.fail( "type must be class or interface" )

}

}
return `type`.resolvedBaseTypes

}
def resolveBaseTypesOfClass(`type`: InterfaceType): Unit = {
 (`type`.resolvedBaseTypes=(`type`.resolvedBaseTypes||emptyArray))
val baseConstructorType = getBaseConstructorTypeOfClass( `type` ).asInstanceOf[ObjectType]
if ((!((baseConstructorType.flags&TypeFlags.Object)))) {
 return

}
val baseTypeNode = getBaseTypeNodeOfClass( `type` )
var baseType: Type = zeroOfMyType
val originalBaseType = (if ((baseConstructorType&&baseConstructorType.symbol)) getDeclaredTypeOfSymbol( baseConstructorType.symbol ) else undefined)
if (((baseConstructorType.symbol&&(baseConstructorType.symbol.flags&SymbolFlags.Class))&&areAllOuterTypeParametersApplied( originalBaseType ))) {
 (baseType=getTypeFromClassOrInterfaceReference( baseTypeNode, baseConstructorType.symbol ))

}
else {
 val constructors = getInstantiatedConstructorsForTypeArguments( baseConstructorType, baseTypeNode.typeArguments )
if ((!constructors.length)) {
 error( baseTypeNode.expression, Diagnostics.No_base_constructor_has_the_specified_number_of_type_arguments )
return

}
(baseType=getReturnTypeOfSignature( constructors(0) ))

}
if ((baseType===unknownType)) {
 return

}
if ((!((getObjectFlags( getTargetType( baseType ) )&ObjectFlags.ClassOrInterface)))) {
 error( baseTypeNode.expression, Diagnostics.Base_constructor_return_type_0_is_not_a_class_or_interface_type, typeToString( baseType ) )
return

}
if (((`type`===baseType)||hasBaseType( baseType.asInstanceOf[InterfaceType], `type` ))) {
 error( `type`.symbol.valueDeclaration, Diagnostics.Type_0_recursively_references_itself_as_a_base_type, typeToString( `type`, undefined, TypeFormatFlags.WriteArrayAsGenericType ) )
return

}
if ((`type`.resolvedBaseTypes===emptyArray)) {
 (`type`.resolvedBaseTypes=Array( baseType.asInstanceOf[ObjectType] ))

}
else {
 `type`.resolvedBaseTypes.push( baseType.asInstanceOf[ObjectType] )

}

}
def areAllOuterTypeParametersApplied(`type`: Type): Boolean = {
 val outerTypeParameters = (`type`.asInstanceOf[InterfaceType]).outerTypeParameters
if (outerTypeParameters) {
 val last = (outerTypeParameters.length-1)
val typeArguments = (`type`.asInstanceOf[TypeReference]).typeArguments
return (outerTypeParameters(last).symbol!==typeArguments(last).symbol)

}
return true

}
def resolveBaseTypesOfInterface(`type`: InterfaceType): Unit = {
 (`type`.resolvedBaseTypes=(`type`.resolvedBaseTypes||emptyArray))
(`type`.symbol.declarations).foreach { fresh19 =>
val declaration = zeroOfMyType
 = fresh19
 {
 if (((declaration.kind===SyntaxKind.InterfaceDeclaration)&&getInterfaceBaseTypeNodes( declaration.asInstanceOf[InterfaceDeclaration] ))) {
 (getInterfaceBaseTypeNodes( declaration.asInstanceOf[InterfaceDeclaration] )).foreach { fresh20 =>
val node = zeroOfMyType
 = fresh20
 {
 val baseType = getTypeFromTypeNode( node )
if ((baseType!==unknownType)) {
 if ((getObjectFlags( getTargetType( baseType ) )&ObjectFlags.ClassOrInterface)) {
 if (((`type`!==baseType)&&(!hasBaseType( baseType.asInstanceOf[InterfaceType], `type` )))) {
 if ((`type`.resolvedBaseTypes===emptyArray)) {
 (`type`.resolvedBaseTypes=Array( baseType.asInstanceOf[ObjectType] ))

}
else {
 `type`.resolvedBaseTypes.push( baseType.asInstanceOf[ObjectType] )

}

}
else {
 error( declaration, Diagnostics.Type_0_recursively_references_itself_as_a_base_type, typeToString( `type`, undefined, TypeFormatFlags.WriteArrayAsGenericType ) )

}

}
else {
 error( node, Diagnostics.An_interface_may_only_extend_a_class_or_another_interface )

}

}

}
}

}

}
}

}
def isIndependentInterface(symbol: Symbol): Boolean = {
 (symbol.declarations).foreach { fresh21 =>
val declaration = zeroOfMyType
 = fresh21
 {
 if ((declaration.kind===SyntaxKind.InterfaceDeclaration)) {
 if ((declaration.flags&NodeFlags.ContainsThis)) {
 return false

}
val baseTypeNodes = getInterfaceBaseTypeNodes( declaration.asInstanceOf[InterfaceDeclaration] )
if (baseTypeNodes) {
 (baseTypeNodes).foreach { fresh22 =>
val node = zeroOfMyType
 = fresh22
 {
 if (isEntityNameExpression( node.expression )) {
 val baseSymbol = resolveEntityName( node.expression, SymbolFlags.Type, true )
if ((((!baseSymbol)||(!((baseSymbol.flags&SymbolFlags.Interface))))||getDeclaredTypeOfClassOrInterface( baseSymbol ).thisType)) {
 return false

}

}

}
}

}

}

}
}
return true

}
def getDeclaredTypeOfClassOrInterface(symbol: Symbol): InterfaceType = {
 val links = getSymbolLinks( symbol )
if ((!links.declaredType)) {
 val kind = (if ((symbol.flags&SymbolFlags.Class)) ObjectFlags.Class else ObjectFlags.Interface)
val `type` = (links.declaredType=createObjectType( kind, symbol ).asInstanceOf[InterfaceType])
val outerTypeParameters = getOuterTypeParametersOfClassOrInterface( symbol )
val localTypeParameters = getLocalTypeParametersOfClassOrInterfaceOrTypeAlias( symbol )
if ((((outerTypeParameters||localTypeParameters)||(kind===ObjectFlags.Class))||(!isIndependentInterface( symbol )))) {
 (`type`.objectFlags|=ObjectFlags.Reference)
(`type`.typeParameters=concatenate( outerTypeParameters, localTypeParameters ))
(`type`.outerTypeParameters=outerTypeParameters)
(`type`.localTypeParameters=localTypeParameters)
((`type`.asInstanceOf[GenericType]).instantiations=createMap[ TypeReference ]())
((`type`.asInstanceOf[GenericType]).instantiations(getTypeListId( `type`.typeParameters ))=`type`.asInstanceOf[GenericType])
((`type`.asInstanceOf[GenericType]).target=`type`.asInstanceOf[GenericType])
((`type`.asInstanceOf[GenericType]).typeArguments=`type`.typeParameters)
(`type`.thisType=createType( TypeFlags.TypeParameter ).asInstanceOf[TypeParameter])
(`type`.thisType.isThisType=true)
(`type`.thisType.symbol=symbol)
(`type`.thisType.constraint=`type`)

}

}
return links.declaredType.asInstanceOf[InterfaceType]

}
def getDeclaredTypeOfTypeAlias(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.declaredType)) {
 if ((!pushTypeResolution( symbol, TypeSystemPropertyName.DeclaredType ))) {
 return unknownType

}
val typeParameters = getLocalTypeParametersOfClassOrInterfaceOrTypeAlias( symbol )
var declaration: ( JSDocTypedefTag | TypeAliasDeclaration ) = getDeclarationOfKind( symbol, SyntaxKind.JSDocTypedefTag ).asInstanceOf[JSDocTypedefTag]
var `type`: Type = zeroOfMyType
if (declaration) {
 if (declaration.jsDocTypeLiteral) {
 (`type`=getTypeFromTypeNode( declaration.jsDocTypeLiteral ))

}
else {
 (`type`=getTypeFromTypeNode( declaration.typeExpression.`type` ))

}

}
else {
 (declaration=getDeclarationOfKind( symbol, SyntaxKind.TypeAliasDeclaration ).asInstanceOf[TypeAliasDeclaration])
(`type`=getTypeFromTypeNode( declaration.`type`, symbol, typeParameters ))

}
if (popTypeResolution()) {
 (links.typeParameters=typeParameters)
if (typeParameters) {
 (links.instantiations=createMap[ Type ]())
(links.instantiations(getTypeListId( links.typeParameters ))=`type`)

}

}
else {
 (`type`=unknownType)
error( declaration.name, Diagnostics.Type_alias_0_circularly_references_itself, symbolToString( symbol ) )

}
(links.declaredType=`type`)

}
return links.declaredType

}
def isLiteralEnumMember(symbol: Symbol, member: EnumMember) = {
 val expr = member.initializer
if ((!expr)) {
 return (!isInAmbientContext( member ))

}
return (((expr.kind===SyntaxKind.NumericLiteral)||(((expr.kind===SyntaxKind.PrefixUnaryExpression)&&((expr.asInstanceOf[PrefixUnaryExpression]).operator===SyntaxKind.MinusToken))&&((expr.asInstanceOf[PrefixUnaryExpression]).operand.kind===SyntaxKind.NumericLiteral)))||((expr.kind===SyntaxKind.Identifier)&&(!(!symbol.exports((expr.asInstanceOf[Identifier]).text)))))

}
def enumHasLiteralMembers(symbol: Symbol) = {
 (symbol.declarations).foreach { fresh23 =>
val declaration = zeroOfMyType
 = fresh23
 {
 if ((declaration.kind===SyntaxKind.EnumDeclaration)) {
 ((declaration.asInstanceOf[EnumDeclaration]).members).foreach { fresh24 =>
val member = zeroOfMyType
 = fresh24
 {
 if ((!isLiteralEnumMember( symbol, member ))) {
 return false

}

}
}

}

}
}
return true

}
def createEnumLiteralType(symbol: Symbol, baseType: EnumType, text: String) = {
 val `type` = createType( TypeFlags.EnumLiteral ).asInstanceOf[EnumLiteralType]
(`type`.symbol=symbol)
(`type`.baseType=baseType.asInstanceOf[( EnumType with UnionType )])
(`type`.text=text)
return `type`

}
def getDeclaredTypeOfEnum(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.declaredType)) {
 val enumType = (links.declaredType=createType( TypeFlags.Enum ).asInstanceOf[EnumType])
(enumType.symbol=symbol)
if (enumHasLiteralMembers( symbol )) {
 val memberTypeList: Array[Type] = Array()
val memberTypes = createMap[ EnumLiteralType ]()
(enumType.symbol.declarations).foreach { fresh25 =>
val declaration = zeroOfMyType
 = fresh25
 {
 if ((declaration.kind===SyntaxKind.EnumDeclaration)) {
 computeEnumMemberValues( declaration.asInstanceOf[EnumDeclaration] )
((declaration.asInstanceOf[EnumDeclaration]).members).foreach { fresh26 =>
val member = zeroOfMyType
 = fresh26
 {
 val memberSymbol = getSymbolOfNode( member )
val value = getEnumMemberValue( member )
if ((!memberTypes(value))) {
 val memberType = (memberTypes(value)=createEnumLiteralType( memberSymbol, enumType, (""+value) ))
memberTypeList.push( memberType )

}

}
}

}

}
}
(enumType.memberTypes=memberTypes)
if ((memberTypeList.length>1)) {
 (enumType.flags|=TypeFlags.Union)
((enumType.asInstanceOf[( EnumType with UnionType )]).types=memberTypeList)
(unionTypes(getTypeListId( memberTypeList ))=enumType.asInstanceOf[( EnumType with UnionType )])

}

}

}
return links.declaredType

}
def getDeclaredTypeOfEnumMember(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.declaredType)) {
 val enumType = getDeclaredTypeOfEnum( getParentOfSymbol( symbol ) ).asInstanceOf[EnumType]
(links.declaredType=(if ((enumType.flags&TypeFlags.Union)) enumType.memberTypes(getEnumMemberValue( symbol.valueDeclaration.asInstanceOf[EnumMember] )) else enumType))

}
return links.declaredType

}
def getDeclaredTypeOfTypeParameter(symbol: Symbol): TypeParameter = {
 val links = getSymbolLinks( symbol )
if ((!links.declaredType)) {
 val `type` = createType( TypeFlags.TypeParameter ).asInstanceOf[TypeParameter]
(`type`.symbol=symbol)
if ((!(getDeclarationOfKind( symbol, SyntaxKind.TypeParameter ).asInstanceOf[TypeParameterDeclaration]).constraint)) {
 (`type`.constraint=noConstraintType)

}
(links.declaredType=`type`)

}
return links.declaredType.asInstanceOf[TypeParameter]

}
def getDeclaredTypeOfAlias(symbol: Symbol): Type = {
 val links = getSymbolLinks( symbol )
if ((!links.declaredType)) {
 (links.declaredType=getDeclaredTypeOfSymbol( resolveAlias( symbol ) ))

}
return links.declaredType

}
def getDeclaredTypeOfSymbol(symbol: Symbol): Type = {
 Debug.assert( (((symbol.flags&SymbolFlags.Instantiated))===0) )
if ((symbol.flags&((SymbolFlags.Class|SymbolFlags.Interface)))) {
 return getDeclaredTypeOfClassOrInterface( symbol )

}
if ((symbol.flags&SymbolFlags.TypeAlias)) {
 return getDeclaredTypeOfTypeAlias( symbol )

}
if ((symbol.flags&SymbolFlags.TypeParameter)) {
 return getDeclaredTypeOfTypeParameter( symbol )

}
if ((symbol.flags&SymbolFlags.Enum)) {
 return getDeclaredTypeOfEnum( symbol )

}
if ((symbol.flags&SymbolFlags.EnumMember)) {
 return getDeclaredTypeOfEnumMember( symbol )

}
if ((symbol.flags&SymbolFlags.Alias)) {
 return getDeclaredTypeOfAlias( symbol )

}
return unknownType

}
def isIndependentTypeReference(node: TypeReferenceNode): Boolean = {
 if (node.typeArguments) {
 (node.typeArguments).foreach { fresh27 =>
val typeNode = zeroOfMyType
 = fresh27
 {
 if ((!isIndependentType( typeNode ))) {
 return false

}

}
}

}
return true

}
def isIndependentType(node: TypeNode): Boolean = {
 node.kind match {
  case  SyntaxKind.AnyKeyword | SyntaxKind.StringKeyword | SyntaxKind.NumberKeyword | SyntaxKind.BooleanKeyword | SyntaxKind.SymbolKeyword | SyntaxKind.VoidKeyword | SyntaxKind.UndefinedKeyword | SyntaxKind.NullKeyword | SyntaxKind.NeverKeyword | SyntaxKind.LiteralType  =>
return true
  case  SyntaxKind.ArrayType  =>
return isIndependentType( (node.asInstanceOf[ArrayTypeNode]).elementType )
  case  SyntaxKind.TypeReference  =>
return isIndependentTypeReference( node.asInstanceOf[TypeReferenceNode] )
  case _ =>
}
return false

}
def isIndependentVariableLikeDeclaration(node: VariableLikeDeclaration): Boolean = {
 return ((node.`type`&&isIndependentType( node.`type` ))||((!node.`type`)&&(!node.initializer)))

}
def isIndependentFunctionLikeDeclaration(node: FunctionLikeDeclaration): Boolean = {
 if (((node.kind!==SyntaxKind.Constructor)&&(((!node.`type`)||(!isIndependentType( node.`type` )))))) {
 return false

}
(node.parameters).foreach { fresh28 =>
val parameter = zeroOfMyType
 = fresh28
 {
 if ((!isIndependentVariableLikeDeclaration( parameter ))) {
 return false

}

}
}
return true

}
def isIndependentMember(symbol: Symbol): Boolean = {
 if ((symbol.declarations&&(symbol.declarations.length===1))) {
 val declaration = symbol.declarations(0)
if (declaration) {
 declaration.kind match {
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature  =>
return isIndependentVariableLikeDeclaration( declaration.asInstanceOf[VariableLikeDeclaration] )
  case  SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor  =>
return isIndependentFunctionLikeDeclaration( declaration.asInstanceOf[FunctionLikeDeclaration] )
  case _ =>
}

}

}
return false

}
def createSymbolTable(symbols: Array[Symbol]): SymbolTable = {
 val result = createMap[ Symbol ]()
(symbols).foreach { fresh29 =>
val symbol = zeroOfMyType
 = fresh29
 {
 (result(symbol.name)=symbol)

}
}
return result

}
def createInstantiatedSymbolTable(symbols: Array[Symbol], mapper: TypeMapper, mappingThisOnly: Boolean): SymbolTable = {
 val result = createMap[ Symbol ]()
(symbols).foreach { fresh30 =>
val symbol = zeroOfMyType
 = fresh30
 {
 (result(symbol.name)=(if ((mappingThisOnly&&isIndependentMember( symbol ))) symbol else instantiateSymbol( symbol, mapper )))

}
}
return result

}
def addInheritedMembers(symbols: SymbolTable, baseSymbols: Array[Symbol]) = {
 (baseSymbols).foreach { fresh31 =>
val s = zeroOfMyType
 = fresh31
 {
 if ((!symbols(s.name))) {
 (symbols(s.name)=s)

}

}
}

}
def resolveDeclaredMembers(`type`: InterfaceType): InterfaceTypeWithDeclaredMembers = {
 if ((!(`type`.asInstanceOf[InterfaceTypeWithDeclaredMembers]).declaredProperties)) {
 val symbol = `type`.symbol
((`type`.asInstanceOf[InterfaceTypeWithDeclaredMembers]).declaredProperties=getNamedMembers( symbol.members ))
((`type`.asInstanceOf[InterfaceTypeWithDeclaredMembers]).declaredCallSignatures=getSignaturesOfSymbol( symbol.members("___call") ))
((`type`.asInstanceOf[InterfaceTypeWithDeclaredMembers]).declaredConstructSignatures=getSignaturesOfSymbol( symbol.members("___new") ))
((`type`.asInstanceOf[InterfaceTypeWithDeclaredMembers]).declaredStringIndexInfo=getIndexInfoOfSymbol( symbol, IndexKind.String ))
((`type`.asInstanceOf[InterfaceTypeWithDeclaredMembers]).declaredNumberIndexInfo=getIndexInfoOfSymbol( symbol, IndexKind.Number ))

}
return `type`.asInstanceOf[InterfaceTypeWithDeclaredMembers]

}
def getTypeWithThisArgument(`type`: Type, thisArgument: Type) = {
 if ((getObjectFlags( `type` )&ObjectFlags.Reference)) {
 return createTypeReference( (`type`.asInstanceOf[TypeReference]).target, concatenate( (`type`.asInstanceOf[TypeReference]).typeArguments, Array( (thisArgument||(`type`.asInstanceOf[TypeReference]).target.thisType) ) ) )

}
return `type`

}
def resolveObjectTypeMembers(`type`: ObjectType, source: InterfaceTypeWithDeclaredMembers, typeParameters: Array[TypeParameter], typeArguments: Array[Type]) = {
 var mapper: TypeMapper = zeroOfMyType
var members: SymbolTable = zeroOfMyType
var callSignatures: Array[Signature] = zeroOfMyType
var constructSignatures: Array[Signature] = zeroOfMyType
var stringIndexInfo: IndexInfo = zeroOfMyType
var numberIndexInfo: IndexInfo = zeroOfMyType
if (rangeEquals( typeParameters, typeArguments, 0, typeParameters.length )) {
 (mapper=identityMapper)
(members=(if (source.symbol) source.symbol.members else createSymbolTable( source.declaredProperties )))
(callSignatures=source.declaredCallSignatures)
(constructSignatures=source.declaredConstructSignatures)
(stringIndexInfo=source.declaredStringIndexInfo)
(numberIndexInfo=source.declaredNumberIndexInfo)

}
else {
 (mapper=createTypeMapper( typeParameters, typeArguments ))
(members=createInstantiatedSymbolTable( source.declaredProperties, mapper, (typeParameters.length===1) ))
(callSignatures=instantiateList( source.declaredCallSignatures, mapper, instantiateSignature ))
(constructSignatures=instantiateList( source.declaredConstructSignatures, mapper, instantiateSignature ))
(stringIndexInfo=instantiateIndexInfo( source.declaredStringIndexInfo, mapper ))
(numberIndexInfo=instantiateIndexInfo( source.declaredNumberIndexInfo, mapper ))

}
val baseTypes = getBaseTypes( source )
if (baseTypes.length) {
 if ((source.symbol&&(members===source.symbol.members))) {
 (members=createSymbolTable( source.declaredProperties ))

}
val thisArgument = lastOrUndefined( typeArguments )
(baseTypes).foreach { fresh32 =>
val baseType = zeroOfMyType
 = fresh32
 {
 val instantiatedBaseType = (if (thisArgument) getTypeWithThisArgument( instantiateType( baseType, mapper ).asInstanceOf[ObjectType], thisArgument ) else baseType)
addInheritedMembers( members, getPropertiesOfObjectType( instantiatedBaseType ) )
(callSignatures=concatenate( callSignatures, getSignaturesOfType( instantiatedBaseType, SignatureKind.Call ) ))
(constructSignatures=concatenate( constructSignatures, getSignaturesOfType( instantiatedBaseType, SignatureKind.Construct ) ))
(stringIndexInfo=(stringIndexInfo||getIndexInfoOfType( instantiatedBaseType, IndexKind.String )))
(numberIndexInfo=(numberIndexInfo||getIndexInfoOfType( instantiatedBaseType, IndexKind.Number )))

}
}

}
setStructuredTypeMembers( `type`, members, callSignatures, constructSignatures, stringIndexInfo, numberIndexInfo )

}
def resolveClassOrInterfaceMembers(`type`: InterfaceType): Unit = {
 resolveObjectTypeMembers( `type`, resolveDeclaredMembers( `type` ), emptyArray, emptyArray )

}
def resolveTypeReferenceMembers(`type`: TypeReference): Unit = {
 val source = resolveDeclaredMembers( `type`.target )
val typeParameters = concatenate( source.typeParameters, Array( source.thisType ) )
val typeArguments = (if ((`type`.typeArguments&&(`type`.typeArguments.length===typeParameters.length))) `type`.typeArguments else concatenate( `type`.typeArguments, Array( `type` ) ))
resolveObjectTypeMembers( `type`, source, typeParameters, typeArguments )

}
def createSignature(declaration: SignatureDeclaration, typeParameters: Array[TypeParameter], thisParameter: ( Symbol | undefined ), parameters: Array[Symbol], resolvedReturnType: Type, typePredicate: TypePredicate, minArgumentCount: Int, hasRestParameter: Boolean, hasLiteralTypes: Boolean): Signature = {
 val sig = new Signature( checker )
(sig.declaration=declaration)
(sig.typeParameters=typeParameters)
(sig.parameters=parameters)
(sig.thisParameter=thisParameter)
(sig.resolvedReturnType=resolvedReturnType)
(sig.typePredicate=typePredicate)
(sig.minArgumentCount=minArgumentCount)
(sig.hasRestParameter=hasRestParameter)
(sig.hasLiteralTypes=hasLiteralTypes)
return sig

}
def cloneSignature(sig: Signature): Signature = {
 return createSignature( sig.declaration, sig.typeParameters, sig.thisParameter, sig.parameters, sig.resolvedReturnType, sig.typePredicate, sig.minArgumentCount, sig.hasRestParameter, sig.hasLiteralTypes )

}
def getDefaultConstructSignatures(classType: InterfaceType): Array[Signature] = {
 val baseConstructorType = getBaseConstructorTypeOfClass( classType )
val baseSignatures = getSignaturesOfType( baseConstructorType, SignatureKind.Construct )
if ((baseSignatures.length===0)) {
 return Array( createSignature( undefined, classType.localTypeParameters, undefined, emptyArray, classType, undefined, 0, false, false ) )

}
val baseTypeNode = getBaseTypeNodeOfClass( classType )
val typeArguments = map( baseTypeNode.typeArguments, getTypeFromTypeNodeNoAlias )
val typeArgCount = (if (typeArguments) typeArguments.length else 0)
val result: Array[Signature] = Array()
(baseSignatures).foreach { fresh33 =>
val baseSig = zeroOfMyType
 = fresh33
 {
 val typeParamCount = (if (baseSig.typeParameters) baseSig.typeParameters.length else 0)
if ((typeParamCount===typeArgCount)) {
 val sig = (if (typeParamCount) getSignatureInstantiation( baseSig, typeArguments ) else cloneSignature( baseSig ))
(sig.typeParameters=classType.localTypeParameters)
(sig.resolvedReturnType=classType)
result.push( sig )

}

}
}
return result

}
def findMatchingSignature(signatureList: Array[Signature], signature: Signature, partialMatch: Boolean, ignoreThisTypes: Boolean, ignoreReturnTypes: Boolean): Signature = {
 (signatureList).foreach { fresh34 =>
val s = zeroOfMyType
 = fresh34
 {
 if (compareSignaturesIdentical( s, signature, partialMatch, ignoreThisTypes, ignoreReturnTypes, compareTypesIdentical )) {
 return s

}

}
}

}
def findMatchingSignatures(signatureLists: Array[Array[Signature]], signature: Signature, listIndex: Int): Array[Signature] = {
 if (signature.typeParameters) {
 if ((listIndex>0)) {
 return undefined

}
{
var i = 1
while( (i<signatureLists.length)) {
 {
 if ((!findMatchingSignature( signatureLists(i), signature, false, false, false ))) {
 return undefined

}

}
 (i+= 1)
}
}
return Array( signature )

}
var result: Array[Signature] = undefined
{
var i = 0
while( (i<signatureLists.length)) {
 {
 val `match` = (if ((i===listIndex)) signature else findMatchingSignature( signatureLists(i), signature, true, true, true ))
if ((!`match`)) {
 return undefined

}
if ((!contains( result, `match` ))) {
 ((result||((result=Array())))).push( `match` )

}

}
 (i+= 1)
}
}
return result

}
def getUnionSignatures(types: Array[Type], kind: SignatureKind): Array[Signature] = {
 val signatureLists = map( types, (t =>  getSignaturesOfType( t, kind )) )
var result: Array[Signature] = undefined
{
var i = 0
while( (i<signatureLists.length)) {
 {
 (signatureLists(i)).foreach { fresh35 =>
val signature = zeroOfMyType
 = fresh35
 {
 if (((!result)||(!findMatchingSignature( result, signature, false, true, true )))) {
 val unionSignatures = findMatchingSignatures( signatureLists, signature, i )
if (unionSignatures) {
 var s = signature
if ((unionSignatures.length>1)) {
 (s=cloneSignature( signature ))
if (forEach( unionSignatures, (sig =>  sig.thisParameter) )) {
 val thisType = getUnionType( map( unionSignatures, (sig =>  (getTypeOfSymbol( sig.thisParameter )||anyType)) ), true )
(s.thisParameter=createTransientSymbol( signature.thisParameter, thisType ))

}
(s.resolvedReturnType=undefined)
(s.unionSignatures=unionSignatures)

}
((result||((result=Array())))).push( s )

}

}

}
}

}
 (i+= 1)
}
}
return (result||emptyArray)

}
def getUnionIndexInfo(types: Array[Type], kind: IndexKind): IndexInfo = {
 val indexTypes: Array[Type] = Array()
var isAnyReadonly = false
(types).foreach { fresh36 =>
val `type` = zeroOfMyType
 = fresh36
 {
 val indexInfo = getIndexInfoOfType( `type`, kind )
if ((!indexInfo)) {
 return undefined

}
indexTypes.push( indexInfo.`type` )
(isAnyReadonly=(isAnyReadonly||indexInfo.isReadonly))

}
}
return createIndexInfo( getUnionType( indexTypes, true ), isAnyReadonly )

}
def resolveUnionTypeMembers(`type`: UnionType) = {
 val callSignatures = getUnionSignatures( `type`.types, SignatureKind.Call )
val constructSignatures = getUnionSignatures( `type`.types, SignatureKind.Construct )
val stringIndexInfo = getUnionIndexInfo( `type`.types, IndexKind.String )
val numberIndexInfo = getUnionIndexInfo( `type`.types, IndexKind.Number )
setStructuredTypeMembers( `type`, emptySymbols, callSignatures, constructSignatures, stringIndexInfo, numberIndexInfo )

}
def intersectTypes(type1: Type, type2: Type): Type = {
 return (if ((!type1)) type2 else (if ((!type2)) type1 else getIntersectionType( Array( type1, type2 ) )))

}
def intersectIndexInfos(info1: IndexInfo, info2: IndexInfo): IndexInfo = {
 return (if ((!info1)) info2 else (if ((!info2)) info1 else createIndexInfo( getIntersectionType( Array( info1.`type`, info2.`type` ) ), (info1.isReadonly&&info2.isReadonly) )))

}
def resolveIntersectionTypeMembers(`type`: IntersectionType) = {
 var callSignatures: Array[Signature] = emptyArray
var constructSignatures: Array[Signature] = emptyArray
var stringIndexInfo: IndexInfo = undefined
var numberIndexInfo: IndexInfo = undefined
(`type`.types).foreach { fresh37 =>
val t = zeroOfMyType
 = fresh37
 {
 (callSignatures=concatenate( callSignatures, getSignaturesOfType( t, SignatureKind.Call ) ))
(constructSignatures=concatenate( constructSignatures, getSignaturesOfType( t, SignatureKind.Construct ) ))
(stringIndexInfo=intersectIndexInfos( stringIndexInfo, getIndexInfoOfType( t, IndexKind.String ) ))
(numberIndexInfo=intersectIndexInfos( numberIndexInfo, getIndexInfoOfType( t, IndexKind.Number ) ))

}
}
setStructuredTypeMembers( `type`, emptySymbols, callSignatures, constructSignatures, stringIndexInfo, numberIndexInfo )

}
def resolveAnonymousTypeMembers(`type`: AnonymousType) = {
 val symbol = `type`.symbol
if (`type`.target) {
 val members = createInstantiatedSymbolTable( getPropertiesOfObjectType( `type`.target ), `type`.mapper, false )
val callSignatures = instantiateList( getSignaturesOfType( `type`.target, SignatureKind.Call ), `type`.mapper, instantiateSignature )
val constructSignatures = instantiateList( getSignaturesOfType( `type`.target, SignatureKind.Construct ), `type`.mapper, instantiateSignature )
val stringIndexInfo = instantiateIndexInfo( getIndexInfoOfType( `type`.target, IndexKind.String ), `type`.mapper )
val numberIndexInfo = instantiateIndexInfo( getIndexInfoOfType( `type`.target, IndexKind.Number ), `type`.mapper )
setStructuredTypeMembers( `type`, members, callSignatures, constructSignatures, stringIndexInfo, numberIndexInfo )

}
else if ((symbol.flags&SymbolFlags.TypeLiteral)) {
 val members = symbol.members
val callSignatures = getSignaturesOfSymbol( members("___call") )
val constructSignatures = getSignaturesOfSymbol( members("___new") )
val stringIndexInfo = getIndexInfoOfSymbol( symbol, IndexKind.String )
val numberIndexInfo = getIndexInfoOfSymbol( symbol, IndexKind.Number )
setStructuredTypeMembers( `type`, members, callSignatures, constructSignatures, stringIndexInfo, numberIndexInfo )

}
else {
 var members = emptySymbols
var constructSignatures: Array[Signature] = emptyArray
if ((symbol.flags&SymbolFlags.HasExports)) {
 (members=getExportsOfSymbol( symbol ))

}
if ((symbol.flags&SymbolFlags.Class)) {
 val classType = getDeclaredTypeOfClassOrInterface( symbol )
(constructSignatures=getSignaturesOfSymbol( symbol.members("___constructor") ))
if ((!constructSignatures.length)) {
 (constructSignatures=getDefaultConstructSignatures( classType ))

}
val baseConstructorType = getBaseConstructorTypeOfClass( classType )
if ((baseConstructorType.flags&TypeFlags.Object)) {
 (members=createSymbolTable( getNamedMembers( members ) ))
addInheritedMembers( members, getPropertiesOfObjectType( baseConstructorType ) )

}

}
val numberIndexInfo = (if ((symbol.flags&SymbolFlags.Enum)) enumNumberIndexInfo else undefined)
setStructuredTypeMembers( `type`, members, emptyArray, constructSignatures, undefined, numberIndexInfo )
if ((symbol.flags&((SymbolFlags.Function|SymbolFlags.Method)))) {
 ((`type`.asInstanceOf[ResolvedType]).callSignatures=getSignaturesOfSymbol( symbol ))

}

}

}
def resolveStructuredTypeMembers(`type`: StructuredType): ResolvedType = {
 if ((!(`type`.asInstanceOf[ResolvedType]).members)) {
 if ((`type`.flags&TypeFlags.Object)) {
 if (((`type`.asInstanceOf[ObjectType]).objectFlags&ObjectFlags.Reference)) {
 resolveTypeReferenceMembers( `type`.asInstanceOf[TypeReference] )

}
else if (((`type`.asInstanceOf[ObjectType]).objectFlags&ObjectFlags.ClassOrInterface)) {
 resolveClassOrInterfaceMembers( `type`.asInstanceOf[InterfaceType] )

}
else if (((`type`.asInstanceOf[ObjectType]).objectFlags&ObjectFlags.Anonymous)) {
 resolveAnonymousTypeMembers( `type`.asInstanceOf[AnonymousType] )

}

}
else if ((`type`.flags&TypeFlags.Union)) {
 resolveUnionTypeMembers( `type`.asInstanceOf[UnionType] )

}
else if ((`type`.flags&TypeFlags.Intersection)) {
 resolveIntersectionTypeMembers( `type`.asInstanceOf[IntersectionType] )

}

}
return `type`.asInstanceOf[ResolvedType]

}
def getPropertiesOfObjectType(`type`: Type): Array[Symbol] = {
 if ((`type`.flags&TypeFlags.Object)) {
 return resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] ).properties

}
return emptyArray

}
def getPropertyOfObjectType(`type`: Type, name: String): Symbol = {
 if ((`type`.flags&TypeFlags.Object)) {
 val resolved = resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] )
val symbol = resolved.members(name)
if ((symbol&&symbolIsValue( symbol ))) {
 return symbol

}

}

}
def getPropertiesOfUnionOrIntersectionType(`type`: UnionOrIntersectionType): Array[Symbol] = {
 (`type`.types).foreach { fresh38 =>
val current = zeroOfMyType
 = fresh38
 {
 (getPropertiesOfType( current )).foreach { fresh39 =>
val prop = zeroOfMyType
 = fresh39
 {
 getUnionOrIntersectionProperty( `type`, prop.name )

}
}
if ((`type`.flags&TypeFlags.Union)) {
 break()

}

}
}
val props = `type`.resolvedProperties
if (props) {
 val result: Array[Symbol] = Array()
(props).keys.foreach { fresh40 =>
val key = zeroOfMyType
 = fresh40
 {
 val prop = props(key)
if ((!(((prop.flags&SymbolFlags.SyntheticProperty)&&(prop.asInstanceOf[TransientSymbol]).isPartial)))) {
 result.push( prop )

}

}
}
return result

}
return emptyArray

}
def getPropertiesOfType(`type`: Type): Array[Symbol] = {
 (`type`=getApparentType( `type` ))
return (if ((`type`.flags&TypeFlags.UnionOrIntersection)) getPropertiesOfUnionOrIntersectionType( `type`.asInstanceOf[UnionType] ) else getPropertiesOfObjectType( `type` ))

}
def getApparentTypeOfTypeParameter(`type`: TypeParameter) = {
 if ((!`type`.resolvedApparentType)) {
 var constraintType = getConstraintOfTypeParameter( `type` )
while ((constraintType&&(constraintType.flags&TypeFlags.TypeParameter))) {
{
 (constraintType=getConstraintOfTypeParameter( constraintType.asInstanceOf[TypeParameter] ))

}
}
(`type`.resolvedApparentType=getTypeWithThisArgument( (constraintType||emptyObjectType), `type` ))

}
return `type`.resolvedApparentType

}
def getApparentType(`type`: Type): Type = {
 if ((`type`.flags&TypeFlags.TypeParameter)) {
 (`type`=getApparentTypeOfTypeParameter( `type`.asInstanceOf[TypeParameter] ))

}
if ((`type`.flags&TypeFlags.StringLike)) {
 (`type`=globalStringType)

}
else if ((`type`.flags&TypeFlags.NumberLike)) {
 (`type`=globalNumberType)

}
else if ((`type`.flags&TypeFlags.BooleanLike)) {
 (`type`=globalBooleanType)

}
else if ((`type`.flags&TypeFlags.ESSymbol)) {
 (`type`=getGlobalESSymbolType())

}
return `type`

}
def createUnionOrIntersectionProperty(containingType: UnionOrIntersectionType, name: String): Symbol = {
 val types = containingType.types
var props: Array[Symbol] = zeroOfMyType
var commonFlags = (if (((containingType.flags&TypeFlags.Intersection))) SymbolFlags.Optional else SymbolFlags.None)
var isReadonly = false
var isPartial = false
(types).foreach { fresh41 =>
val current = zeroOfMyType
 = fresh41
 {
 val `type` = getApparentType( current )
if ((`type`!==unknownType)) {
 val prop = getPropertyOfType( `type`, name )
if ((prop&&(!((getDeclarationModifierFlagsFromSymbol( prop )&((ModifierFlags.Private|ModifierFlags.Protected))))))) {
 (commonFlags&=prop.flags)
if ((!props)) {
 (props=Array( prop ))

}
else if ((!contains( props, prop ))) {
 props.push( prop )

}
if (isReadonlySymbol( prop )) {
 (isReadonly=true)

}

}
else if ((containingType.flags&TypeFlags.Union)) {
 (isPartial=true)

}

}

}
}
if ((!props)) {
 return undefined

}
if (((props.length===1)&&(!isPartial))) {
 return props(0)

}
val propTypes: Array[Type] = Array()
val declarations: Array[Declaration] = Array()
var commonType: Type = undefined
var hasNonUniformType = false
(props).foreach { fresh42 =>
val prop = zeroOfMyType
 = fresh42
 {
 if (prop.declarations) {
 addRange( declarations, prop.declarations )

}
val `type` = getTypeOfSymbol( prop )
if ((!commonType)) {
 (commonType=`type`)

}
else if ((`type`!==commonType)) {
 (hasNonUniformType=true)

}
propTypes.push( `type` )

}
}
val result = createSymbol( (((SymbolFlags.Property|SymbolFlags.Transient)|SymbolFlags.SyntheticProperty)|commonFlags), name ).asInstanceOf[TransientSymbol]
(result.containingType=containingType)
(result.hasNonUniformType=hasNonUniformType)
(result.isPartial=isPartial)
(result.declarations=declarations)
(result.isReadonly=isReadonly)
(result.`type`=(if ((containingType.flags&TypeFlags.Union)) getUnionType( propTypes ) else getIntersectionType( propTypes )))
return result

}
def getUnionOrIntersectionProperty(`type`: UnionOrIntersectionType, name: String): Symbol = {
 val properties = (`type`.resolvedProperties||((`type`.resolvedProperties=createMap[ Symbol ]())))
var property = properties(name)
if ((!property)) {
 (property=createUnionOrIntersectionProperty( `type`, name ))
if (property) {
 (properties(name)=property)

}

}
return property

}
def getPropertyOfUnionOrIntersectionType(`type`: UnionOrIntersectionType, name: String): Symbol = {
 val property = getUnionOrIntersectionProperty( `type`, name )
return (if ((property&&(!(((property.flags&SymbolFlags.SyntheticProperty)&&(property.asInstanceOf[TransientSymbol]).isPartial))))) property else undefined)

}
def getPropertyOfType(`type`: Type, name: String): Symbol = {
 (`type`=getApparentType( `type` ))
if ((`type`.flags&TypeFlags.Object)) {
 val resolved = resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] )
val symbol = resolved.members(name)
if ((symbol&&symbolIsValue( symbol ))) {
 return symbol

}
if ((((resolved===anyFunctionType)||resolved.callSignatures.length)||resolved.constructSignatures.length)) {
 val symbol = getPropertyOfObjectType( globalFunctionType, name )
if (symbol) {
 return symbol

}

}
return getPropertyOfObjectType( globalObjectType, name )

}
if ((`type`.flags&TypeFlags.UnionOrIntersection)) {
 return getPropertyOfUnionOrIntersectionType( `type`.asInstanceOf[UnionOrIntersectionType], name )

}
return undefined

}
def getSignaturesOfStructuredType(`type`: Type, kind: SignatureKind): Array[Signature] = {
 if ((`type`.flags&TypeFlags.StructuredType)) {
 val resolved = resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] )
return (if ((kind===SignatureKind.Call)) resolved.callSignatures else resolved.constructSignatures)

}
return emptyArray

}
def getSignaturesOfType(`type`: Type, kind: SignatureKind): Array[Signature] = {
 return getSignaturesOfStructuredType( getApparentType( `type` ), kind )

}
def getIndexInfoOfStructuredType(`type`: Type, kind: IndexKind): IndexInfo = {
 if ((`type`.flags&TypeFlags.StructuredType)) {
 val resolved = resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] )
return (if ((kind===IndexKind.String)) resolved.stringIndexInfo else resolved.numberIndexInfo)

}

}
def getIndexTypeOfStructuredType(`type`: Type, kind: IndexKind): Type = {
 val info = getIndexInfoOfStructuredType( `type`, kind )
return (info&&info.`type`)

}
def getIndexInfoOfType(`type`: Type, kind: IndexKind): IndexInfo = {
 return getIndexInfoOfStructuredType( getApparentType( `type` ), kind )

}
def getIndexTypeOfType(`type`: Type, kind: IndexKind): Type = {
 return getIndexTypeOfStructuredType( getApparentType( `type` ), kind )

}
def getImplicitIndexTypeOfType(`type`: Type, kind: IndexKind): Type = {
 if (isObjectLiteralType( `type` )) {
 val propTypes: Array[Type] = Array()
(getPropertiesOfType( `type` )).foreach { fresh43 =>
val prop = zeroOfMyType
 = fresh43
 {
 if (((kind===IndexKind.String)||isNumericLiteralName( prop.name ))) {
 propTypes.push( getTypeOfSymbol( prop ) )

}

}
}
if (propTypes.length) {
 return getUnionType( propTypes, true )

}

}
return undefined

}
def getTypeParametersFromJSDocTemplate(declaration: SignatureDeclaration): Array[TypeParameter] = {
 if ((declaration.flags&NodeFlags.JavaScriptFile)) {
 val templateTag = getJSDocTemplateTag( declaration )
if (templateTag) {
 return getTypeParametersFromDeclaration( templateTag.typeParameters )

}

}
return undefined

}
def getTypeParametersFromDeclaration(typeParameterDeclarations: Array[TypeParameterDeclaration]): Array[TypeParameter] = {
 val result: Array[TypeParameter] = Array()
forEach( typeParameterDeclarations, (node =>  {
 val tp = getDeclaredTypeOfTypeParameter( node.symbol )
 if ((!contains( result, tp ))) {
 result.push( tp )

}

}) )
return result

}
def symbolsToArray(symbols: SymbolTable): Array[Symbol] = {
 val result: Array[Symbol] = Array()
(symbols).keys.foreach { fresh44 =>
val id = zeroOfMyType
 = fresh44
 {
 if ((!isReservedMemberName( id ))) {
 result.push( symbols(id) )

}

}
}
return result

}
def isJSDocOptionalParameter(node: ParameterDeclaration) = {
 if ((node.flags&NodeFlags.JavaScriptFile)) {
 if ((node.`type`&&(node.`type`.kind===SyntaxKind.JSDocOptionalType))) {
 return true

}
val paramTag = getCorrespondingJSDocParameterTag( node )
if (paramTag) {
 if (paramTag.isBracketed) {
 return true

}
if (paramTag.typeExpression) {
 return (paramTag.typeExpression.`type`.kind===SyntaxKind.JSDocOptionalType)

}

}

}

}
def isOptionalParameter(node: ParameterDeclaration) = {
 if ((hasQuestionToken( node )||isJSDocOptionalParameter( node ))) {
 return true

}
if (node.initializer) {
 val signatureDeclaration = node.parent.asInstanceOf[SignatureDeclaration]
val signature = getSignatureFromDeclaration( signatureDeclaration )
val parameterIndex = ts.indexOf( signatureDeclaration.parameters, node )
Debug.assert( (parameterIndex>=0) )
return (parameterIndex>=signature.minArgumentCount)

}
return false

}
def createTypePredicateFromTypePredicateNode(node: TypePredicateNode): ( IdentifierTypePredicate | ThisTypePredicate ) = {
 if ((node.parameterName.kind===SyntaxKind.Identifier)) {
 val parameterName = node.parameterName.asInstanceOf[Identifier]
return Map( "kind" -> TypePredicateKind.Identifier,
"parameterName" -> (if (parameterName) parameterName.text else undefined),
"parameterIndex" -> (if (parameterName) getTypePredicateParameterIndex( (node.parent.asInstanceOf[SignatureDeclaration]).parameters, parameterName ) else undefined),
"type" -> getTypeFromTypeNode( node.`type` ) ).asInstanceOf[IdentifierTypePredicate]

}
else {
 return Map( "kind" -> TypePredicateKind.This,
"type" -> getTypeFromTypeNode( node.`type` ) ).asInstanceOf[ThisTypePredicate]

}

}
def getSignatureFromDeclaration(declaration: SignatureDeclaration): Signature = {
 val links = getNodeLinks( declaration )
if ((!links.resolvedSignature)) {
 val parameters: Array[Symbol] = Array()
var hasLiteralTypes = false
var minArgumentCount = (-1)
var thisParameter: Symbol = undefined
var hasThisParameter: Boolean = zeroOfMyType
val isJSConstructSignature = isJSDocConstructSignature( declaration )
{
var i = (if (isJSConstructSignature) 1 else 0)
var n = declaration.parameters.length
while( (i<n)) {
 {
 val param = declaration.parameters(i)
var paramSymbol = param.symbol
if (((paramSymbol&&(!(!((paramSymbol.flags&SymbolFlags.Property)))))&&(!isBindingPattern( param.name )))) {
 val resolvedSymbol = resolveName( param, paramSymbol.name, SymbolFlags.Value, undefined, undefined )
(paramSymbol=resolvedSymbol)

}
if (((i===0)&&(paramSymbol.name==="this"))) {
 (hasThisParameter=true)
(thisParameter=param.symbol)

}
else {
 parameters.push( paramSymbol )

}
if ((param.`type`&&(param.`type`.kind===SyntaxKind.LiteralType))) {
 (hasLiteralTypes=true)

}
if ((((param.initializer||param.questionToken)||param.dotDotDotToken)||isJSDocOptionalParameter( param ))) {
 if ((minArgumentCount<0)) {
 (minArgumentCount=(i-((if (hasThisParameter) 1 else 0))))

}

}
else {
 (minArgumentCount=(-1))

}

}
 (i+= 1)
}
}
if ((((((declaration.kind===SyntaxKind.GetAccessor)||(declaration.kind===SyntaxKind.SetAccessor)))&&(!hasDynamicName( declaration )))&&(((!hasThisParameter)||(!thisParameter))))) {
 val otherKind = (if ((declaration.kind===SyntaxKind.GetAccessor)) SyntaxKind.SetAccessor else SyntaxKind.GetAccessor)
val other = getDeclarationOfKind( declaration.symbol, otherKind ).asInstanceOf[AccessorDeclaration]
if (other) {
 (thisParameter=getAnnotatedAccessorThisParameter( other ))

}

}
if ((minArgumentCount<0)) {
 (minArgumentCount=(declaration.parameters.length-((if (hasThisParameter) 1 else 0))))

}
if (isJSConstructSignature) {
 (minArgumentCount-= 1)

}
val classType = (if ((declaration.kind===SyntaxKind.Constructor)) getDeclaredTypeOfClassOrInterface( getMergedSymbol( (declaration.parent.asInstanceOf[ClassDeclaration]).symbol ) ) else undefined)
val typeParameters = (if (classType) classType.localTypeParameters else (if (declaration.typeParameters) getTypeParametersFromDeclaration( declaration.typeParameters ) else getTypeParametersFromJSDocTemplate( declaration )))
val returnType = getSignatureReturnTypeFromDeclaration( declaration, isJSConstructSignature, classType )
val typePredicate = (if ((declaration.`type`&&(declaration.`type`.kind===SyntaxKind.TypePredicate))) createTypePredicateFromTypePredicateNode( declaration.`type`.asInstanceOf[TypePredicateNode] ) else undefined)
(links.resolvedSignature=createSignature( declaration, typeParameters, thisParameter, parameters, returnType, typePredicate, minArgumentCount, hasRestParameter( declaration ), hasLiteralTypes ))

}
return links.resolvedSignature

}
def getSignatureReturnTypeFromDeclaration(declaration: SignatureDeclaration, isJSConstructSignature: Boolean, classType: Type) = {
 if (isJSConstructSignature) {
 return getTypeFromTypeNode( declaration.parameters(0).`type` )

}
else if (classType) {
 return classType

}
else if (declaration.`type`) {
 return getTypeFromTypeNode( declaration.`type` )

}
if ((declaration.flags&NodeFlags.JavaScriptFile)) {
 val `type` = getReturnTypeFromJSDocComment( declaration )
if ((`type`&&(`type`!==unknownType))) {
 return `type`

}

}
if (((declaration.kind===SyntaxKind.GetAccessor)&&(!hasDynamicName( declaration )))) {
 val setter = getDeclarationOfKind( declaration.symbol, SyntaxKind.SetAccessor ).asInstanceOf[AccessorDeclaration]
return getAnnotatedAccessorType( setter )

}
if (nodeIsMissing( (declaration.asInstanceOf[FunctionLikeDeclaration]).body )) {
 return anyType

}

}
def getSignaturesOfSymbol(symbol: Symbol): Array[Signature] = {
 if ((!symbol))
return emptyArray
val result: Array[Signature] = Array()
{
var i = 0
var len = symbol.declarations.length
while( (i<len)) {
 {
 val node = symbol.declarations(i)
node.kind match {
  case  SyntaxKind.FunctionType | SyntaxKind.ConstructorType | SyntaxKind.FunctionDeclaration | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction | SyntaxKind.JSDocFunctionType  =>
if (((i>0)&&(node.asInstanceOf[FunctionLikeDeclaration]).body)) {
 val previous = symbol.declarations((i-1))
if ((((node.parent===previous.parent)&&(node.kind===previous.kind))&&(node.pos===previous.end))) {
 break()

}

}
result.push( getSignatureFromDeclaration( node.asInstanceOf[SignatureDeclaration] ) )
  case _ =>
}

}
 (i+= 1)
}
}
return result

}
def resolveExternalModuleTypeByLiteral(name: StringLiteral) = {
 val moduleSym = resolveExternalModuleName( name, name )
if (moduleSym) {
 val resolvedModuleSymbol = resolveExternalModuleSymbol( moduleSym )
if (resolvedModuleSymbol) {
 return getTypeOfSymbol( resolvedModuleSymbol )

}

}
return anyType

}
def getThisTypeOfSignature(signature: Signature): ( Type | undefined ) = {
 if (signature.thisParameter) {
 return getTypeOfSymbol( signature.thisParameter )

}

}
def getReturnTypeOfSignature(signature: Signature): Type = {
 if ((!signature.resolvedReturnType)) {
 if ((!pushTypeResolution( signature, TypeSystemPropertyName.ResolvedReturnType ))) {
 return unknownType

}
var `type`: Type = zeroOfMyType
if (signature.target) {
 (`type`=instantiateType( getReturnTypeOfSignature( signature.target ), signature.mapper ))

}
else if (signature.unionSignatures) {
 (`type`=getUnionType( map( signature.unionSignatures, getReturnTypeOfSignature ), true ))

}
else {
 (`type`=getReturnTypeFromBody( signature.declaration.asInstanceOf[FunctionLikeDeclaration] ))

}
if ((!popTypeResolution())) {
 (`type`=anyType)
if (compilerOptions.noImplicitAny) {
 val declaration = signature.declaration.asInstanceOf[Declaration]
if (declaration.name) {
 error( declaration.name, Diagnostics._0_implicitly_has_return_type_any_because_it_does_not_have_a_return_type_annotation_and_is_referenced_directly_or_indirectly_in_one_of_its_return_expressions, declarationNameToString( declaration.name ) )

}
else {
 error( declaration, Diagnostics.Function_implicitly_has_return_type_any_because_it_does_not_have_a_return_type_annotation_and_is_referenced_directly_or_indirectly_in_one_of_its_return_expressions )

}

}

}
(signature.resolvedReturnType=`type`)

}
return signature.resolvedReturnType

}
def getRestTypeOfSignature(signature: Signature): Type = {
 if (signature.hasRestParameter) {
 val `type` = getTypeOfSymbol( lastOrUndefined( signature.parameters ) )
if (((getObjectFlags( `type` )&ObjectFlags.Reference)&&((`type`.asInstanceOf[TypeReference]).target===globalArrayType))) {
 return (`type`.asInstanceOf[TypeReference]).typeArguments(0)

}

}
return anyType

}
def getSignatureInstantiation(signature: Signature, typeArguments: Array[Type]): Signature = {
 return instantiateSignature( signature, createTypeMapper( signature.typeParameters, typeArguments ), true )

}
def getErasedSignature(signature: Signature): Signature = {
 if ((!signature.typeParameters))
return signature
if ((!signature.erasedSignatureCache)) {
 (signature.erasedSignatureCache=instantiateSignature( signature, createTypeEraser( signature.typeParameters ), true ))

}
return signature.erasedSignatureCache

}
def getOrCreateTypeFromSignature(signature: Signature): ObjectType = {
 if ((!signature.isolatedSignatureType)) {
 val isConstructor = ((signature.declaration.kind===SyntaxKind.Constructor)||(signature.declaration.kind===SyntaxKind.ConstructSignature))
val `type` = createObjectType( ObjectFlags.Anonymous ).asInstanceOf[ResolvedType]
(`type`.members=emptySymbols)
(`type`.properties=emptyArray)
(`type`.callSignatures=(if ((!isConstructor)) Array( signature ) else emptyArray))
(`type`.constructSignatures=(if (isConstructor) Array( signature ) else emptyArray))
(signature.isolatedSignatureType=`type`)

}
return signature.isolatedSignatureType

}
def getIndexSymbol(symbol: Symbol): Symbol = {
 return symbol.members("___index")

}
def getIndexDeclarationOfSymbol(symbol: Symbol, kind: IndexKind): SignatureDeclaration = {
 val syntaxKind = (if ((kind===IndexKind.Number)) SyntaxKind.NumberKeyword else SyntaxKind.StringKeyword)
val indexSymbol = getIndexSymbol( symbol )
if (indexSymbol) {
 (indexSymbol.declarations).foreach { fresh45 =>
val decl = zeroOfMyType
 = fresh45
 {
 val node = decl.asInstanceOf[SignatureDeclaration]
if ((node.parameters.length===1)) {
 val parameter = node.parameters(0)
if (((parameter&&parameter.`type`)&&(parameter.`type`.kind===syntaxKind))) {
 return node

}

}

}
}

}
return undefined

}
def createIndexInfo(`type`: Type, isReadonly: Boolean, declaration: SignatureDeclaration): IndexInfo = {
 return Map( "type" -> `type`,
"isReadonly" -> isReadonly,
"declaration" -> declaration )

}
def getIndexInfoOfSymbol(symbol: Symbol, kind: IndexKind): IndexInfo = {
 val declaration = getIndexDeclarationOfSymbol( symbol, kind )
if (declaration) {
 return createIndexInfo( (if (declaration.`type`) getTypeFromTypeNode( declaration.`type` ) else anyType), (((getModifierFlags( declaration )&ModifierFlags.Readonly))!==0), declaration )

}
return undefined

}
def getConstraintDeclaration(`type`: TypeParameter) = {
 return (getDeclarationOfKind( `type`.symbol, SyntaxKind.TypeParameter ).asInstanceOf[TypeParameterDeclaration]).constraint

}
def hasConstraintReferenceTo(`type`: Type, target: TypeParameter): Boolean = {
 var checked: Array[Type] = zeroOfMyType
while ((((`type`&&(`type`.flags&TypeFlags.TypeParameter))&&(!((`type`.asInstanceOf[TypeParameter]).isThisType)))&&(!contains( checked, `type` )))) {
{
 if ((`type`===target)) {
 return true

}
((checked||((checked=Array())))).push( `type` )
val constraintDeclaration = getConstraintDeclaration( `type`.asInstanceOf[TypeParameter] )
(`type`=(constraintDeclaration&&getTypeFromTypeNode( constraintDeclaration )))

}
}
return false

}
def getConstraintOfTypeParameter(typeParameter: TypeParameter): Type = {
 if ((!typeParameter.constraint)) {
 if (typeParameter.target) {
 val targetConstraint = getConstraintOfTypeParameter( typeParameter.target )
(typeParameter.constraint=(if (targetConstraint) instantiateType( targetConstraint, typeParameter.mapper ) else noConstraintType))

}
else {
 val constraintDeclaration = getConstraintDeclaration( typeParameter )
var constraint = getTypeFromTypeNode( constraintDeclaration )
if (hasConstraintReferenceTo( constraint, typeParameter )) {
 error( constraintDeclaration, Diagnostics.Type_parameter_0_has_a_circular_constraint, typeToString( typeParameter ) )
(constraint=unknownType)

}
(typeParameter.constraint=constraint)

}

}
return (if ((typeParameter.constraint===noConstraintType)) undefined else typeParameter.constraint)

}
def getParentSymbolOfTypeParameter(typeParameter: TypeParameter): Symbol = {
 return getSymbolOfNode( getDeclarationOfKind( typeParameter.symbol, SyntaxKind.TypeParameter ).parent )

}
def getTypeListId(types: Array[Type]) = {
 var result = ""
if (types) {
 val length = types.length
var i = 0
while ((i<length)) {
{
 val startId = types(i).id
var count = 1
while ((((i+count)<length)&&(types((i+count)).id===(startId+count)))) {
{
 (count+= 1)

}
}
if (result.length) {
 (result+=",")

}
(result+=startId)
if ((count>1)) {
 (result+=(":"+count))

}
(i+=count)

}
}

}
return result

}
def getPropagatingFlagsOfTypes(types: Array[Type], excludeKinds: TypeFlags): TypeFlags = {
 var result: TypeFlags = 0
(types).foreach { fresh46 =>
val `type` = zeroOfMyType
 = fresh46
 {
 if ((!((`type`.flags&excludeKinds)))) {
 (result|=`type`.flags)

}

}
}
return (result&TypeFlags.PropagatingFlags)

}
def createTypeReference(target: GenericType, typeArguments: Array[Type]): TypeReference = {
 val id = getTypeListId( typeArguments )
var `type` = target.instantiations(id)
if ((!`type`)) {
 (`type`=(target.instantiations(id)=createObjectType( ObjectFlags.Reference, target.symbol ).asInstanceOf[TypeReference]))
(`type`.flags|=(if (typeArguments) getPropagatingFlagsOfTypes( typeArguments, 0 ) else 0))
(`type`.target=target)
(`type`.typeArguments=typeArguments)

}
return `type`

}
def cloneTypeReference(source: TypeReference): TypeReference = {
 val `type` = createType( source.flags ).asInstanceOf[TypeReference]
(`type`.symbol=source.symbol)
(`type`.objectFlags=source.objectFlags)
(`type`.target=source.target)
(`type`.typeArguments=source.typeArguments)
return `type`

}
def getTypeReferenceArity(`type`: TypeReference): Int = {
 return (if (`type`.target.typeParameters) `type`.target.typeParameters.length else 0)

}
def getTypeFromClassOrInterfaceReference(node: ( TypeReferenceNode | ExpressionWithTypeArguments | JSDocTypeReference ), symbol: Symbol): Type = {
 val `type` = getDeclaredTypeOfSymbol( getMergedSymbol( symbol ) ).asInstanceOf[InterfaceType]
val typeParameters = `type`.localTypeParameters
if (typeParameters) {
 if (((!node.typeArguments)||(node.typeArguments.length!==typeParameters.length))) {
 error( node, Diagnostics.Generic_type_0_requires_1_type_argument_s, typeToString( `type`, undefined, TypeFormatFlags.WriteArrayAsGenericType ), typeParameters.length )
return unknownType

}
return createTypeReference( `type`.asInstanceOf[GenericType], concatenate( `type`.outerTypeParameters, map( node.typeArguments, getTypeFromTypeNodeNoAlias ) ) )

}
if (node.typeArguments) {
 error( node, Diagnostics.Type_0_is_not_generic, typeToString( `type` ) )
return unknownType

}
return `type`

}
def getTypeFromTypeAliasReference(node: ( TypeReferenceNode | ExpressionWithTypeArguments | JSDocTypeReference ), symbol: Symbol): Type = {
 val `type` = getDeclaredTypeOfSymbol( symbol )
val links = getSymbolLinks( symbol )
val typeParameters = links.typeParameters
if (typeParameters) {
 if (((!node.typeArguments)||(node.typeArguments.length!==typeParameters.length))) {
 error( node, Diagnostics.Generic_type_0_requires_1_type_argument_s, symbolToString( symbol ), typeParameters.length )
return unknownType

}
val typeArguments = map( node.typeArguments, getTypeFromTypeNodeNoAlias )
val id = getTypeListId( typeArguments )
return (links.instantiations(id)||((links.instantiations(id)=instantiateType( `type`, createTypeMapper( typeParameters, typeArguments ) ))))

}
if (node.typeArguments) {
 error( node, Diagnostics.Type_0_is_not_generic, symbolToString( symbol ) )
return unknownType

}
return `type`

}
def getTypeFromNonGenericTypeReference(node: ( TypeReferenceNode | ExpressionWithTypeArguments | JSDocTypeReference ), symbol: Symbol): Type = {
 if (node.typeArguments) {
 error( node, Diagnostics.Type_0_is_not_generic, symbolToString( symbol ) )
return unknownType

}
return getDeclaredTypeOfSymbol( symbol )

}
def getTypeReferenceName(node: ( TypeReferenceNode | ExpressionWithTypeArguments | JSDocTypeReference )): ( EntityNameOrEntityNameExpression | undefined ) = {
 node.kind match {
  case  SyntaxKind.TypeReference  =>
return (node.asInstanceOf[TypeReferenceNode]).typeName
  case  SyntaxKind.JSDocTypeReference  =>
return (node.asInstanceOf[JSDocTypeReference]).name
  case  SyntaxKind.ExpressionWithTypeArguments  =>
val expr = (node.asInstanceOf[ExpressionWithTypeArguments]).expression
if (isEntityNameExpression( expr )) {
 return expr

}
  case _ =>
}
return undefined

}
def resolveTypeReferenceName(typeReferenceName: ( EntityNameExpression | EntityName )) = {
 if ((!typeReferenceName)) {
 return unknownSymbol

}
return (resolveEntityName( typeReferenceName, SymbolFlags.Type )||unknownSymbol)

}
def getTypeReferenceType(node: ( TypeReferenceNode | ExpressionWithTypeArguments | JSDocTypeReference ), symbol: Symbol) = {
 if ((symbol===unknownSymbol)) {
 return unknownType

}
if ((symbol.flags&((SymbolFlags.Class|SymbolFlags.Interface)))) {
 return getTypeFromClassOrInterfaceReference( node, symbol )

}
if ((symbol.flags&SymbolFlags.TypeAlias)) {
 return getTypeFromTypeAliasReference( node, symbol )

}
if (((symbol.flags&SymbolFlags.Value)&&(node.kind===SyntaxKind.JSDocTypeReference))) {
 return getTypeOfSymbol( symbol )

}
return getTypeFromNonGenericTypeReference( node, symbol )

}
def getTypeFromTypeReference(node: ( TypeReferenceNode | ExpressionWithTypeArguments | JSDocTypeReference )): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 var symbol: Symbol = zeroOfMyType
var `type`: Type = zeroOfMyType
if ((node.kind===SyntaxKind.JSDocTypeReference)) {
 val typeReferenceName = getTypeReferenceName( node )
(symbol=resolveTypeReferenceName( typeReferenceName ))
(`type`=getTypeReferenceType( node, symbol ))

}
else {
 val typeNameOrExpression: EntityNameOrEntityNameExpression = (if ((node.kind===SyntaxKind.TypeReference)) (node.asInstanceOf[TypeReferenceNode]).typeName else (if (isEntityNameExpression( (node.asInstanceOf[ExpressionWithTypeArguments]).expression )) (node.asInstanceOf[ExpressionWithTypeArguments]).expression.asInstanceOf[EntityNameExpression] else undefined))
(symbol=((typeNameOrExpression&&resolveEntityName( typeNameOrExpression, SymbolFlags.Type ))||unknownSymbol))
(`type`=(if ((symbol===unknownSymbol)) unknownType else (if ((symbol.flags&((SymbolFlags.Class|SymbolFlags.Interface)))) getTypeFromClassOrInterfaceReference( node, symbol ) else (if ((symbol.flags&SymbolFlags.TypeAlias)) getTypeFromTypeAliasReference( node, symbol ) else getTypeFromNonGenericTypeReference( node, symbol )))))

}
(links.resolvedSymbol=symbol)
(links.resolvedType=`type`)

}
return links.resolvedType

}
def getTypeFromTypeQueryNode(node: TypeQueryNode): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 (links.resolvedType=getWidenedType( checkExpression( node.exprName ) ))

}
return links.resolvedType

}
def getTypeOfGlobalSymbol(symbol: Symbol, arity: Int): ObjectType = {
 def getTypeDeclaration(symbol: Symbol): Declaration = {
 val declarations = symbol.declarations
(declarations).foreach { fresh47 =>
val declaration = zeroOfMyType
 = fresh47
 {
 declaration.kind match {
  case  SyntaxKind.ClassDeclaration | SyntaxKind.InterfaceDeclaration | SyntaxKind.EnumDeclaration  =>
return declaration
  case _ =>
}

}
}

}
if ((!symbol)) {
 return (if (arity) emptyGenericType else emptyObjectType)

}
val `type` = getDeclaredTypeOfSymbol( symbol )
if ((!((`type`.flags&TypeFlags.Object)))) {
 error( getTypeDeclaration( symbol ), Diagnostics.Global_type_0_must_be_a_class_or_interface_type, symbol.name )
return (if (arity) emptyGenericType else emptyObjectType)

}
if ((((if ((`type`.asInstanceOf[InterfaceType]).typeParameters) (`type`.asInstanceOf[InterfaceType]).typeParameters.length else 0))!==arity)) {
 error( getTypeDeclaration( symbol ), Diagnostics.Global_type_0_must_have_1_type_parameter_s, symbol.name, arity )
return (if (arity) emptyGenericType else emptyObjectType)

}
return `type`.asInstanceOf[ObjectType]

}
def getGlobalValueSymbol(name: String): Symbol = {
 return getGlobalSymbol( name, SymbolFlags.Value, Diagnostics.Cannot_find_global_value_0 )

}
def getGlobalTypeSymbol(name: String): Symbol = {
 return getGlobalSymbol( name, SymbolFlags.Type, Diagnostics.Cannot_find_global_type_0 )

}
def getGlobalSymbol(name: String, meaning: SymbolFlags, diagnostic: DiagnosticMessage): Symbol = {
 return resolveName( undefined, name, meaning, diagnostic, name )

}
def getGlobalType(name: String, arity: Nothing = 0): ObjectType = {
 return getTypeOfGlobalSymbol( getGlobalTypeSymbol( name ), arity )

}
def getExportedTypeFromNamespace(namespace: String, name: String): Type = {
 val namespaceSymbol = getGlobalSymbol( namespace, SymbolFlags.Namespace, undefined )
val typeSymbol = (namespaceSymbol&&getSymbol( namespaceSymbol.exports, name, SymbolFlags.Type ))
return (typeSymbol&&getDeclaredTypeOfSymbol( typeSymbol ))

}
def createTypedPropertyDescriptorType(propertyType: Type): Type = {
 val globalTypedPropertyDescriptorType = getGlobalTypedPropertyDescriptorType()
return (if ((globalTypedPropertyDescriptorType!==emptyGenericType)) createTypeReference( globalTypedPropertyDescriptorType.asInstanceOf[GenericType], Array( propertyType ) ) else emptyObjectType)

}
def createTypeFromGenericGlobalType(genericGlobalType: GenericType, typeArguments: Array[Type]): ObjectType = {
 return (if ((genericGlobalType!==emptyGenericType)) createTypeReference( genericGlobalType, typeArguments ) else emptyObjectType)

}
def createIterableType(elementType: Type): Type = {
 return createTypeFromGenericGlobalType( getGlobalIterableType(), Array( elementType ) )

}
def createIterableIteratorType(elementType: Type): Type = {
 return createTypeFromGenericGlobalType( getGlobalIterableIteratorType(), Array( elementType ) )

}
def createArrayType(elementType: Type): ObjectType = {
 return createTypeFromGenericGlobalType( globalArrayType, Array( elementType ) )

}
def getTypeFromArrayTypeNode(node: ArrayTypeNode): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 (links.resolvedType=createArrayType( getTypeFromTypeNode( node.elementType ) ))

}
return links.resolvedType

}
def createTupleTypeOfArity(arity: Int): GenericType = {
 val typeParameters: Array[TypeParameter] = Array()
val properties: Array[Symbol] = Array()
{
var i = 0
while( (i<arity)) {
 {
 val typeParameter = createType( TypeFlags.TypeParameter ).asInstanceOf[TypeParameter]
typeParameters.push( typeParameter )
val property = createSymbol( (SymbolFlags.Property|SymbolFlags.Transient), (""+i) ).asInstanceOf[TransientSymbol]
(property.`type`=typeParameter)
properties.push( property )

}
 (i+= 1)
}
}
val `type` = createObjectType( (ObjectFlags.Tuple|ObjectFlags.Reference) ).asInstanceOf[( GenericType with InterfaceTypeWithDeclaredMembers )]
(`type`.typeParameters=typeParameters)
(`type`.outerTypeParameters=undefined)
(`type`.localTypeParameters=typeParameters)
(`type`.instantiations=createMap[ TypeReference ]())
(`type`.instantiations(getTypeListId( `type`.typeParameters ))=`type`.asInstanceOf[GenericType])
(`type`.target=`type`.asInstanceOf[GenericType])
(`type`.typeArguments=`type`.typeParameters)
(`type`.thisType=createType( TypeFlags.TypeParameter ).asInstanceOf[TypeParameter])
(`type`.thisType.isThisType=true)
(`type`.thisType.constraint=`type`)
(`type`.declaredProperties=properties)
(`type`.declaredCallSignatures=emptyArray)
(`type`.declaredConstructSignatures=emptyArray)
(`type`.declaredStringIndexInfo=undefined)
(`type`.declaredNumberIndexInfo=undefined)
return `type`

}
def getTupleTypeOfArity(arity: Int): GenericType = {
 return (tupleTypes(arity)||((tupleTypes(arity)=createTupleTypeOfArity( arity ))))

}
def createTupleType(elementTypes: Array[Type]) = {
 return createTypeReference( getTupleTypeOfArity( elementTypes.length ), elementTypes )

}
def getTypeFromTupleTypeNode(node: TupleTypeNode): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 (links.resolvedType=createTupleType( map( node.elementTypes, getTypeFromTypeNodeNoAlias ) ))

}
return links.resolvedType

}
trait TypeSet extends Array[ Type ] {
  var containsAny: Boolean
  var containsUndefined: Boolean
  var containsNull: Boolean
  var containsNonWideningType: Boolean
  var containsString: Boolean
  var containsNumber: Boolean
  var containsStringOrNumberLiteral: Boolean
}
def binarySearchTypes(types: Array[Type], `type`: Type): Int = {
 var low = 0
var high = (types.length-1)
val typeId = `type`.id
while ((low<=high)) {
{
 val middle = (low+((((high-low))>>1)))
val id = types(middle).id
if ((id===typeId)) {
 return middle

}
else if ((id>typeId)) {
 (high=(middle-1))

}
else {
 (low=(middle+1))

}

}
}
return (~low)

}
def containsType(types: Array[Type], `type`: Type): Boolean = {
 return (binarySearchTypes( types, `type` )>=0)

}
def addTypeToUnion(typeSet: TypeSet, `type`: Type) = {
 val flags = `type`.flags
if ((flags&TypeFlags.Union)) {
 addTypesToUnion( typeSet, (`type`.asInstanceOf[UnionType]).types )

}
else if ((flags&TypeFlags.Any)) {
 (typeSet.containsAny=true)

}
else if (((!strictNullChecks)&&(flags&TypeFlags.Nullable))) {
 if ((flags&TypeFlags.Undefined))
(typeSet.containsUndefined=true)
if ((flags&TypeFlags.Null))
(typeSet.containsNull=true)
if ((!((flags&TypeFlags.ContainsWideningType))))
(typeSet.containsNonWideningType=true)

}
else if ((!((flags&TypeFlags.Never)))) {
 if ((flags&TypeFlags.String))
(typeSet.containsString=true)
if ((flags&TypeFlags.Number))
(typeSet.containsNumber=true)
if ((flags&TypeFlags.StringOrNumberLiteral))
(typeSet.containsStringOrNumberLiteral=true)
val len = typeSet.length
val index = (if ((len&&(`type`.id>typeSet((len-1)).id))) (~len) else binarySearchTypes( typeSet, `type` ))
if ((index<0)) {
 if ((!((((((flags&TypeFlags.Object)&&((`type`.asInstanceOf[ObjectType]).objectFlags&ObjectFlags.Anonymous))&&`type`.symbol)&&(`type`.symbol.flags&((SymbolFlags.Function|SymbolFlags.Method))))&&containsIdenticalType( typeSet, `type` ))))) {
 typeSet.splice( (~index), 0, `type` )

}

}

}

}
def addTypesToUnion(typeSet: TypeSet, types: Array[Type]) = {
 (types).foreach { fresh48 =>
val `type` = zeroOfMyType
 = fresh48
 {
 addTypeToUnion( typeSet, `type` )

}
}

}
def containsIdenticalType(types: Array[Type], `type`: Type) = {
 (types).foreach { fresh49 =>
val t = zeroOfMyType
 = fresh49
 {
 if (isTypeIdenticalTo( t, `type` )) {
 return true

}

}
}
return false

}
def isSubtypeOfAny(candidate: Type, types: Array[Type]): Boolean = {
 {
var i = 0
var len = types.length
while( (i<len)) {
 {
 if (((candidate!==types(i))&&isTypeSubtypeOf( candidate, types(i) ))) {
 return true

}

}
 (i+= 1)
}
}
return false

}
def isSetOfLiteralsFromSameEnum(types: TypeSet): Boolean = {
 val first = types(0)
if ((first.flags&TypeFlags.EnumLiteral)) {
 val firstEnum = getParentOfSymbol( first.symbol )
{
var i = 1
while( (i<types.length)) {
 {
 val other = types(i)
if (((!((other.flags&TypeFlags.EnumLiteral)))||((firstEnum!==getParentOfSymbol( other.symbol ))))) {
 return false

}

}
 (i+= 1)
}
}
return true

}
return false

}
def removeSubtypes(types: TypeSet) = {
 if (((types.length===0)||isSetOfLiteralsFromSameEnum( types ))) {
 return

}
var i = types.length
while ((i>0)) {
{
 (i-= 1)
if (isSubtypeOfAny( types(i), types )) {
 orderedRemoveItemAt( types, i )

}

}
}

}
def removeRedundantLiteralTypes(types: TypeSet) = {
 var i = types.length
while ((i>0)) {
{
 (i-= 1)
val t = types(i)
val remove = ((((t.flags&TypeFlags.StringLiteral)&&types.containsString)||((t.flags&TypeFlags.NumberLiteral)&&types.containsNumber))||(((t.flags&TypeFlags.StringOrNumberLiteral)&&(t.flags&TypeFlags.FreshLiteral))&&containsType( types, (t.asInstanceOf[LiteralType]).regularType )))
if (remove) {
 orderedRemoveItemAt( types, i )

}

}
}

}
def getUnionType(types: Array[Type], subtypeReduction: Boolean, aliasSymbol: Symbol, aliasTypeArguments: Array[Type]): Type = {
 if ((types.length===0)) {
 return neverType

}
if ((types.length===1)) {
 return types(0)

}
val typeSet = Array().asInstanceOf[TypeSet]
addTypesToUnion( typeSet, types )
if (typeSet.containsAny) {
 return anyType

}
if (subtypeReduction) {
 removeSubtypes( typeSet )

}
else if (typeSet.containsStringOrNumberLiteral) {
 removeRedundantLiteralTypes( typeSet )

}
if ((typeSet.length===0)) {
 return (if (typeSet.containsNull) (if (typeSet.containsNonWideningType) nullType else nullWideningType) else (if (typeSet.containsUndefined) (if (typeSet.containsNonWideningType) undefinedType else undefinedWideningType) else neverType))

}
return getUnionTypeFromSortedList( typeSet, aliasSymbol, aliasTypeArguments )

}
def getUnionTypeFromSortedList(types: Array[Type], aliasSymbol: Symbol, aliasTypeArguments: Array[Type]): Type = {
 if ((types.length===0)) {
 return neverType

}
if ((types.length===1)) {
 return types(0)

}
val id = getTypeListId( types )
var `type` = unionTypes(id)
if ((!`type`)) {
 val propagatedFlags = getPropagatingFlagsOfTypes( types, TypeFlags.Nullable )
(`type`=(unionTypes(id)=createType( (TypeFlags.Union|propagatedFlags) ).asInstanceOf[UnionType]))
(`type`.types=types)
(`type`.aliasSymbol=aliasSymbol)
(`type`.aliasTypeArguments=aliasTypeArguments)

}
return `type`

}
def getTypeFromUnionTypeNode(node: UnionTypeNode, aliasSymbol: Symbol, aliasTypeArguments: Array[Type]): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 (links.resolvedType=getUnionType( map( node.types, getTypeFromTypeNodeNoAlias ), false, aliasSymbol, aliasTypeArguments ))

}
return links.resolvedType

}
def addTypeToIntersection(typeSet: TypeSet, `type`: Type) = {
 if ((`type`.flags&TypeFlags.Intersection)) {
 addTypesToIntersection( typeSet, (`type`.asInstanceOf[IntersectionType]).types )

}
else if ((`type`.flags&TypeFlags.Any)) {
 (typeSet.containsAny=true)

}
else if ((((!((`type`.flags&TypeFlags.Never)))&&((strictNullChecks||(!((`type`.flags&TypeFlags.Nullable))))))&&(!contains( typeSet, `type` )))) {
 typeSet.push( `type` )

}

}
def addTypesToIntersection(typeSet: TypeSet, types: Array[Type]) = {
 (types).foreach { fresh50 =>
val `type` = zeroOfMyType
 = fresh50
 {
 addTypeToIntersection( typeSet, `type` )

}
}

}
def getIntersectionType(types: Array[Type], aliasSymbol: Symbol, aliasTypeArguments: Array[Type]): Type = {
 if ((types.length===0)) {
 return emptyObjectType

}
{
var i = 0
while( (i<types.length)) {
 {
 val `type` = types(i)
if ((`type`.flags&TypeFlags.Union)) {
 return getUnionType( map( (`type`.asInstanceOf[UnionType]).types, (t =>  getIntersectionType( replaceElement( types, i, t ) )) ), false, aliasSymbol, aliasTypeArguments )

}

}
 (i+= 1)
}
}
val typeSet = Array().asInstanceOf[TypeSet]
addTypesToIntersection( typeSet, types )
if (typeSet.containsAny) {
 return anyType

}
if ((typeSet.length===1)) {
 return typeSet(0)

}
val id = getTypeListId( typeSet )
var `type` = intersectionTypes(id)
if ((!`type`)) {
 val propagatedFlags = getPropagatingFlagsOfTypes( typeSet, TypeFlags.Nullable )
(`type`=(intersectionTypes(id)=createType( (TypeFlags.Intersection|propagatedFlags) ).asInstanceOf[IntersectionType]))
(`type`.types=typeSet)
(`type`.aliasSymbol=aliasSymbol)
(`type`.aliasTypeArguments=aliasTypeArguments)

}
return `type`

}
def getTypeFromIntersectionTypeNode(node: IntersectionTypeNode, aliasSymbol: Symbol, aliasTypeArguments: Array[Type]): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 (links.resolvedType=getIntersectionType( map( node.types, getTypeFromTypeNodeNoAlias ), aliasSymbol, aliasTypeArguments ))

}
return links.resolvedType

}
def getTypeFromTypeLiteralOrFunctionOrConstructorTypeNode(node: Node, aliasSymbol: Symbol, aliasTypeArguments: Array[Type]): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 val `type` = createObjectType( ObjectFlags.Anonymous, node.symbol )
(`type`.aliasSymbol=aliasSymbol)
(`type`.aliasTypeArguments=aliasTypeArguments)
(links.resolvedType=`type`)

}
return links.resolvedType

}
def createLiteralType(flags: TypeFlags, text: String) = {
 val `type` = createType( flags ).asInstanceOf[LiteralType]
(`type`.text=text)
return `type`

}
def getFreshTypeOfLiteralType(`type`: Type) = {
 if (((`type`.flags&TypeFlags.StringOrNumberLiteral)&&(!((`type`.flags&TypeFlags.FreshLiteral))))) {
 if ((!(`type`.asInstanceOf[LiteralType]).freshType)) {
 val freshType = createLiteralType( (`type`.flags|TypeFlags.FreshLiteral), (`type`.asInstanceOf[LiteralType]).text ).asInstanceOf[LiteralType]
(freshType.regularType=`type`.asInstanceOf[LiteralType])
((`type`.asInstanceOf[LiteralType]).freshType=freshType)

}
return (`type`.asInstanceOf[LiteralType]).freshType

}
return `type`

}
def getRegularTypeOfLiteralType(`type`: Type) = {
 return (if (((`type`.flags&TypeFlags.StringOrNumberLiteral)&&(`type`.flags&TypeFlags.FreshLiteral))) (`type`.asInstanceOf[LiteralType]).regularType else `type`)

}
def getLiteralTypeForText(flags: TypeFlags, text: String) = {
 val map = (if ((flags&TypeFlags.StringLiteral)) stringLiteralTypes else numericLiteralTypes)
return (map(text)||((map(text)=createLiteralType( flags, text ))))

}
def getTypeFromLiteralTypeNode(node: LiteralTypeNode): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 (links.resolvedType=getRegularTypeOfLiteralType( checkExpression( node.literal ) ))

}
return links.resolvedType

}
def getTypeFromJSDocVariadicType(node: JSDocVariadicType): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 val `type` = getTypeFromTypeNode( node.`type` )
(links.resolvedType=(if (`type`) createArrayType( `type` ) else unknownType))

}
return links.resolvedType

}
def getTypeFromJSDocTupleType(node: JSDocTupleType): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 val types = map( node.types, getTypeFromTypeNodeNoAlias )
(links.resolvedType=createTupleType( types ))

}
return links.resolvedType

}
def getThisType(node: Node): Type = {
 val container = getThisContainer( node, false )
val parent = (container&&container.parent)
if ((parent&&((isClassLike( parent )||(parent.kind===SyntaxKind.InterfaceDeclaration))))) {
 if (((!((getModifierFlags( container )&ModifierFlags.Static)))&&(((container.kind!==SyntaxKind.Constructor)||isNodeDescendantOf( node, (container.asInstanceOf[ConstructorDeclaration]).body ))))) {
 return getDeclaredTypeOfClassOrInterface( getSymbolOfNode( parent ) ).thisType

}

}
error( node, Diagnostics.A_this_type_is_available_only_in_a_non_static_member_of_a_class_or_interface )
return unknownType

}
def getTypeFromThisTypeNode(node: TypeNode): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 (links.resolvedType=getThisType( node ))

}
return links.resolvedType

}
def getTypeFromTypeNodeNoAlias(`type`: TypeNode) = {
 return getTypeFromTypeNode( `type`, undefined, undefined )

}
def getTypeFromTypeNode(node: TypeNode, aliasSymbol: Symbol, aliasTypeArguments: Array[Type]): Type = {
 node.kind match {
  case  SyntaxKind.AnyKeyword | SyntaxKind.JSDocAllType | SyntaxKind.JSDocUnknownType  =>
return anyType
  case  SyntaxKind.StringKeyword  =>
return stringType
  case  SyntaxKind.NumberKeyword  =>
return numberType
  case  SyntaxKind.BooleanKeyword  =>
return booleanType
  case  SyntaxKind.SymbolKeyword  =>
return esSymbolType
  case  SyntaxKind.VoidKeyword  =>
return voidType
  case  SyntaxKind.UndefinedKeyword  =>
return undefinedType
  case  SyntaxKind.NullKeyword  =>
return nullType
  case  SyntaxKind.NeverKeyword  =>
return neverType
  case  SyntaxKind.JSDocNullKeyword  =>
return nullType
  case  SyntaxKind.JSDocUndefinedKeyword  =>
return undefinedType
  case  SyntaxKind.JSDocNeverKeyword  =>
return neverType
  case  SyntaxKind.ThisType | SyntaxKind.ThisKeyword  =>
return getTypeFromThisTypeNode( node )
  case  SyntaxKind.LiteralType  =>
return getTypeFromLiteralTypeNode( node.asInstanceOf[LiteralTypeNode] )
  case  SyntaxKind.JSDocLiteralType  =>
return getTypeFromLiteralTypeNode( (node.asInstanceOf[JSDocLiteralType]).literal )
  case  SyntaxKind.TypeReference | SyntaxKind.JSDocTypeReference  =>
return getTypeFromTypeReference( node.asInstanceOf[TypeReferenceNode] )
  case  SyntaxKind.TypePredicate  =>
return booleanType
  case  SyntaxKind.ExpressionWithTypeArguments  =>
return getTypeFromTypeReference( node.asInstanceOf[ExpressionWithTypeArguments] )
  case  SyntaxKind.TypeQuery  =>
return getTypeFromTypeQueryNode( node.asInstanceOf[TypeQueryNode] )
  case  SyntaxKind.ArrayType | SyntaxKind.JSDocArrayType  =>
return getTypeFromArrayTypeNode( node.asInstanceOf[ArrayTypeNode] )
  case  SyntaxKind.TupleType  =>
return getTypeFromTupleTypeNode( node.asInstanceOf[TupleTypeNode] )
  case  SyntaxKind.UnionType | SyntaxKind.JSDocUnionType  =>
return getTypeFromUnionTypeNode( node.asInstanceOf[UnionTypeNode], aliasSymbol, aliasTypeArguments )
  case  SyntaxKind.IntersectionType  =>
return getTypeFromIntersectionTypeNode( node.asInstanceOf[IntersectionTypeNode], aliasSymbol, aliasTypeArguments )
  case  SyntaxKind.ParenthesizedType | SyntaxKind.JSDocNullableType | SyntaxKind.JSDocNonNullableType | SyntaxKind.JSDocConstructorType | SyntaxKind.JSDocThisType | SyntaxKind.JSDocOptionalType  =>
return getTypeFromTypeNode( (node.asInstanceOf[( ParenthesizedTypeNode | JSDocTypeReferencingNode )]).`type` )
  case  SyntaxKind.JSDocRecordType  =>
return getTypeFromTypeNode( (node.asInstanceOf[JSDocRecordType]).literal )
  case  SyntaxKind.FunctionType | SyntaxKind.ConstructorType | SyntaxKind.TypeLiteral | SyntaxKind.JSDocTypeLiteral | SyntaxKind.JSDocFunctionType  =>
return getTypeFromTypeLiteralOrFunctionOrConstructorTypeNode( node, aliasSymbol, aliasTypeArguments )
  case  SyntaxKind.Identifier | SyntaxKind.QualifiedName  =>
val symbol = getSymbolAtLocation( node )
return (symbol&&getDeclaredTypeOfSymbol( symbol ))
  case  SyntaxKind.JSDocTupleType  =>
return getTypeFromJSDocTupleType( node.asInstanceOf[JSDocTupleType] )
  case  SyntaxKind.JSDocVariadicType  =>
return getTypeFromJSDocVariadicType( node.asInstanceOf[JSDocVariadicType] )
  case _ =>
return unknownType
}

}
def instantiateList[T](items: Array[T], mapper: TypeMapper, instantiator: ((T, TypeMapper) => T)): Array[T] = {
 if ((items&&items.length)) {
 val result: Array[T] = Array()
(items).foreach { fresh51 =>
val v = zeroOfMyType
 = fresh51
 {
 result.push( instantiator( v, mapper ) )

}
}
return result

}
return items

}
def createUnaryTypeMapper(source: Type, target: Type): TypeMapper = {
 return (t =>  (if ((t===source)) target else t))

}
def createBinaryTypeMapper(source1: Type, target1: Type, source2: Type, target2: Type): TypeMapper = {
 return (t =>  (if ((t===source1)) target1 else (if ((t===source2)) target2 else t)))

}
def createArrayTypeMapper(sources: Array[Type], targets: Array[Type]): TypeMapper = {
 return (t =>  {
 {
var i = 0
while( (i<sources.length)) {
 {
 if ((t===sources(i))) {
 return (if (targets) targets(i) else anyType)

}

}
 (i+= 1)
}
}
 return t

})

}
def createTypeMapper(sources: Array[Type], targets: Array[Type]): TypeMapper = {
 val count = sources.length
val mapper: TypeMapper = (if ((count==1)) createUnaryTypeMapper( sources(0), (if (targets) targets(0) else anyType) ) else (if ((count==2)) createBinaryTypeMapper( sources(0), (if (targets) targets(0) else anyType), sources(1), (if (targets) targets(1) else anyType) ) else createArrayTypeMapper( sources, targets )))
(mapper.mappedTypes=sources)
(mapper.targetTypes=targets)
return mapper

}
def createTypeEraser(sources: Array[Type]): TypeMapper = {
 return createTypeMapper( sources, undefined )

}
def getInferenceMapper(context: InferenceContext): TypeMapper = {
 if ((!context.mapper)) {
 val mapper: TypeMapper = (t =>  {
 val typeParameters = context.signature.typeParameters
 {
var i = 0
while( (i<typeParameters.length)) {
 {
 if ((t===typeParameters(i))) {
 (context.inferences(i).isFixed=true)
return getInferredType( context, i )

}

}
 (i+= 1)
}
}
 return t

})
(mapper.mappedTypes=context.signature.typeParameters)
(mapper.context=context)
(context.mapper=mapper)

}
return context.mapper

}
def identityMapper(`type`: Type): Type = {
 return `type`

}
def combineTypeMappers(mapper1: TypeMapper, mapper2: TypeMapper): TypeMapper = {
 val mapper: TypeMapper = (t =>  instantiateType( mapper1( t ), mapper2 ))
(mapper.mappedTypes=mapper1.mappedTypes)
return mapper

}
def cloneTypeParameter(typeParameter: TypeParameter): TypeParameter = {
 val result = createType( TypeFlags.TypeParameter ).asInstanceOf[TypeParameter]
(result.symbol=typeParameter.symbol)
(result.target=typeParameter)
return result

}
def cloneTypePredicate(predicate: TypePredicate, mapper: TypeMapper): ( ThisTypePredicate | IdentifierTypePredicate ) = {
 if (isIdentifierTypePredicate( predicate )) {
 return Map( "kind" -> TypePredicateKind.Identifier,
"parameterName" -> predicate.parameterName,
"parameterIndex" -> predicate.parameterIndex,
"type" -> instantiateType( predicate.`type`, mapper ) ).asInstanceOf[IdentifierTypePredicate]

}
else {
 return Map( "kind" -> TypePredicateKind.This,
"type" -> instantiateType( predicate.`type`, mapper ) ).asInstanceOf[ThisTypePredicate]

}

}
def instantiateSignature(signature: Signature, mapper: TypeMapper, eraseTypeParameters: Boolean): Signature = {
 var freshTypeParameters: Array[TypeParameter] = zeroOfMyType
var freshTypePredicate: TypePredicate = zeroOfMyType
if ((signature.typeParameters&&(!eraseTypeParameters))) {
 (freshTypeParameters=map( signature.typeParameters, cloneTypeParameter ))
(mapper=combineTypeMappers( createTypeMapper( signature.typeParameters, freshTypeParameters ), mapper ))
(freshTypeParameters).foreach { fresh52 =>
val tp = zeroOfMyType
 = fresh52
 {
 (tp.mapper=mapper)

}
}

}
if (signature.typePredicate) {
 (freshTypePredicate=cloneTypePredicate( signature.typePredicate, mapper ))

}
val result = createSignature( signature.declaration, freshTypeParameters, (signature.thisParameter&&instantiateSymbol( signature.thisParameter, mapper )), instantiateList( signature.parameters, mapper, instantiateSymbol ), instantiateType( signature.resolvedReturnType, mapper ), freshTypePredicate, signature.minArgumentCount, signature.hasRestParameter, signature.hasLiteralTypes )
(result.target=signature)
(result.mapper=mapper)
return result

}
def instantiateSymbol(symbol: Symbol, mapper: TypeMapper): Symbol = {
 if ((symbol.flags&SymbolFlags.Instantiated)) {
 val links = getSymbolLinks( symbol )
(symbol=links.target)
(mapper=combineTypeMappers( links.mapper, mapper ))

}
val result = createSymbol( ((SymbolFlags.Instantiated|SymbolFlags.Transient)|symbol.flags), symbol.name ).asInstanceOf[TransientSymbol]
(result.declarations=symbol.declarations)
(result.parent=symbol.parent)
(result.target=symbol)
(result.mapper=mapper)
if (symbol.valueDeclaration) {
 (result.valueDeclaration=symbol.valueDeclaration)

}
return result

}
def instantiateAnonymousType(`type`: AnonymousType, mapper: TypeMapper): ObjectType = {
 if (mapper.instantiations) {
 val cachedType = mapper.instantiations(`type`.id).asInstanceOf[ObjectType]
if (cachedType) {
 return cachedType

}

}
else {
 (mapper.instantiations=Array())

}
val result = createObjectType( (ObjectFlags.Anonymous|ObjectFlags.Instantiated), `type`.symbol ).asInstanceOf[AnonymousType]
(result.target=`type`)
(result.mapper=mapper)
(result.aliasSymbol=`type`.aliasSymbol)
(result.aliasTypeArguments=mapper.targetTypes)
(mapper.instantiations(`type`.id)=result)
return result

}
def isSymbolInScopeOfMappedTypeParameter(symbol: Symbol, mapper: TypeMapper) = {
 val mappedTypes = mapper.mappedTypes
var node = symbol.declarations(0).parent
while (node) {
{
 node.kind match {
  case  SyntaxKind.FunctionType | SyntaxKind.ConstructorType | SyntaxKind.FunctionDeclaration | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction | SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression | SyntaxKind.InterfaceDeclaration | SyntaxKind.TypeAliasDeclaration  =>
val declaration = node.asInstanceOf[DeclarationWithTypeParameters]
if (declaration.typeParameters) {
 (declaration.typeParameters).foreach { fresh53 =>
val d = zeroOfMyType
 = fresh53
 {
 if (contains( mappedTypes, getDeclaredTypeOfTypeParameter( getSymbolOfNode( d ) ) )) {
 return true

}

}
}

}
if ((isClassLike( node )||(node.kind===SyntaxKind.InterfaceDeclaration))) {
 val thisType = getDeclaredTypeOfClassOrInterface( getSymbolOfNode( node ) ).thisType
if ((thisType&&contains( mappedTypes, thisType ))) {
 return true

}

}
  case  SyntaxKind.ModuleDeclaration | SyntaxKind.SourceFile  =>
return false
  case _ =>
}
(node=node.parent)

}
}
return false

}
def instantiateType(`type`: Type, mapper: TypeMapper): Type = {
 if ((`type`&&(mapper!==identityMapper))) {
 if ((`type`.flags&TypeFlags.TypeParameter)) {
 return mapper( `type`.asInstanceOf[TypeParameter] )

}
if ((`type`.flags&TypeFlags.Object)) {
 if (((`type`.asInstanceOf[ObjectType]).objectFlags&ObjectFlags.Anonymous)) {
 return (if (((`type`.symbol&&(`type`.symbol.flags&(((((SymbolFlags.Function|SymbolFlags.Method)|SymbolFlags.Class)|SymbolFlags.TypeLiteral)|SymbolFlags.ObjectLiteral))))&&((((`type`.asInstanceOf[ObjectType]).objectFlags&ObjectFlags.Instantiated)||isSymbolInScopeOfMappedTypeParameter( `type`.symbol, mapper ))))) instantiateAnonymousType( `type`.asInstanceOf[AnonymousType], mapper ) else `type`)

}
if (((`type`.asInstanceOf[ObjectType]).objectFlags&ObjectFlags.Reference)) {
 return createTypeReference( (`type`.asInstanceOf[TypeReference]).target, instantiateList( (`type`.asInstanceOf[TypeReference]).typeArguments, mapper, instantiateType ) )

}

}
if (((`type`.flags&TypeFlags.Union)&&(!((`type`.flags&TypeFlags.Primitive))))) {
 return getUnionType( instantiateList( (`type`.asInstanceOf[UnionType]).types, mapper, instantiateType ), false, `type`.aliasSymbol, mapper.targetTypes )

}
if ((`type`.flags&TypeFlags.Intersection)) {
 return getIntersectionType( instantiateList( (`type`.asInstanceOf[IntersectionType]).types, mapper, instantiateType ), `type`.aliasSymbol, mapper.targetTypes )

}

}
return `type`

}
def instantiateIndexInfo(info: IndexInfo, mapper: TypeMapper): IndexInfo = {
 return (info&&createIndexInfo( instantiateType( info.`type`, mapper ), info.isReadonly, info.declaration ))

}
def isContextSensitive(node: ( Expression | MethodDeclaration | ObjectLiteralElementLike )): Boolean = {
 Debug.assert( ((node.kind!==SyntaxKind.MethodDeclaration)||isObjectLiteralMethod( node )) )
node.kind match {
  case  SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction  =>
return isContextSensitiveFunctionLikeDeclaration( node.asInstanceOf[FunctionExpression] )
  case  SyntaxKind.ObjectLiteralExpression  =>
return forEach( (node.asInstanceOf[ObjectLiteralExpression]).properties, isContextSensitive )
  case  SyntaxKind.ArrayLiteralExpression  =>
return forEach( (node.asInstanceOf[ArrayLiteralExpression]).elements, isContextSensitive )
  case  SyntaxKind.ConditionalExpression  =>
return (isContextSensitive( (node.asInstanceOf[ConditionalExpression]).whenTrue )||isContextSensitive( (node.asInstanceOf[ConditionalExpression]).whenFalse ))
  case  SyntaxKind.BinaryExpression  =>
return (((node.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.BarBarToken)&&((isContextSensitive( (node.asInstanceOf[BinaryExpression]).left )||isContextSensitive( (node.asInstanceOf[BinaryExpression]).right ))))
  case  SyntaxKind.PropertyAssignment  =>
return isContextSensitive( (node.asInstanceOf[PropertyAssignment]).initializer )
  case  SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature  =>
return isContextSensitiveFunctionLikeDeclaration( node.asInstanceOf[MethodDeclaration] )
  case  SyntaxKind.ParenthesizedExpression  =>
return isContextSensitive( (node.asInstanceOf[ParenthesizedExpression]).expression )
  case _ =>
}
return false

}
def isContextSensitiveFunctionLikeDeclaration(node: FunctionLikeDeclaration) = {
 if (node.typeParameters) {
 return false

}
if (forEach( node.parameters, (p =>  (!p.`type`)) )) {
 return true

}
if ((node.kind===SyntaxKind.ArrowFunction)) {
 return false

}
val parameter = firstOrUndefined( node.parameters )
return (!((parameter&&parameterIsThisKeyword( parameter ))))

}
def isContextSensitiveFunctionOrObjectLiteralMethod(func: Node): Boolean = {
 return (((isFunctionExpressionOrArrowFunction( func )||isObjectLiteralMethod( func )))&&isContextSensitiveFunctionLikeDeclaration( func ))

}
def getTypeWithoutSignatures(`type`: Type): Type = {
 if ((`type`.flags&TypeFlags.Object)) {
 val resolved = resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] )
if (resolved.constructSignatures.length) {
 val result = createObjectType( ObjectFlags.Anonymous, `type`.symbol ).asInstanceOf[ResolvedType]
(result.members=resolved.members)
(result.properties=resolved.properties)
(result.callSignatures=emptyArray)
(result.constructSignatures=emptyArray)
(`type`=result)

}

}
return `type`

}
def isTypeIdenticalTo(source: Type, target: Type): Boolean = {
 return isTypeRelatedTo( source, target, identityRelation )

}
def compareTypesIdentical(source: Type, target: Type): Ternary = {
 return (if (isTypeRelatedTo( source, target, identityRelation )) Ternary.True else Ternary.False)

}
def compareTypesAssignable(source: Type, target: Type): Ternary = {
 return (if (isTypeRelatedTo( source, target, assignableRelation )) Ternary.True else Ternary.False)

}
def isTypeSubtypeOf(source: Type, target: Type): Boolean = {
 return isTypeRelatedTo( source, target, subtypeRelation )

}
def isTypeAssignableTo(source: Type, target: Type): Boolean = {
 return isTypeRelatedTo( source, target, assignableRelation )

}
def isTypeInstanceOf(source: Type, target: Type): Boolean = {
 return ((source===target)||(isTypeSubtypeOf( source, target )&&(!isTypeIdenticalTo( source, target ))))

}
def isTypeComparableTo(source: Type, target: Type): Boolean = {
 return isTypeRelatedTo( source, target, comparableRelation )

}
def areTypesComparable(type1: Type, type2: Type): Boolean = {
 return (isTypeComparableTo( type1, type2 )||isTypeComparableTo( type2, type1 ))

}
def checkTypeSubtypeOf(source: Type, target: Type, errorNode: Node, headMessage: DiagnosticMessage, containingMessageChain: DiagnosticMessageChain): Boolean = {
 return checkTypeRelatedTo( source, target, subtypeRelation, errorNode, headMessage, containingMessageChain )

}
def checkTypeAssignableTo(source: Type, target: Type, errorNode: Node, headMessage: DiagnosticMessage, containingMessageChain: DiagnosticMessageChain): Boolean = {
 return checkTypeRelatedTo( source, target, assignableRelation, errorNode, headMessage, containingMessageChain )

}
def checkTypeComparableTo(source: Type, target: Type, errorNode: Node, headMessage: DiagnosticMessage, containingMessageChain: DiagnosticMessageChain): Boolean = {
 return checkTypeRelatedTo( source, target, comparableRelation, errorNode, headMessage, containingMessageChain )

}
def isSignatureAssignableTo(source: Signature, target: Signature, ignoreReturnTypes: Boolean): Boolean = {
 return (compareSignaturesRelated( source, target, ignoreReturnTypes, false, undefined, compareTypesAssignable )!==Ternary.False)

}
type ErrorReporter = ((DiagnosticMessage, String, String) => Unit)
def compareSignaturesRelated(source: Signature, target: Signature, ignoreReturnTypes: Boolean, reportErrors: Boolean, errorReporter: ErrorReporter, compareTypes: ((Type, Type, Boolean) => Ternary)): Ternary = {
 if ((source===target)) {
 return Ternary.True

}
if (((!target.hasRestParameter)&&(source.minArgumentCount>target.parameters.length))) {
 return Ternary.False

}
(source=getErasedSignature( source ))
(target=getErasedSignature( target ))
var result = Ternary.True
val sourceThisType = getThisTypeOfSignature( source )
if ((sourceThisType&&(sourceThisType!==voidType))) {
 val targetThisType = getThisTypeOfSignature( target )
if (targetThisType) {
 val related = (compareTypes( sourceThisType, targetThisType, false )||compareTypes( targetThisType, sourceThisType, reportErrors ))
if ((!related)) {
 if (reportErrors) {
 errorReporter( Diagnostics.The_this_types_of_each_signature_are_incompatible )

}
return Ternary.False

}
(result&=related)

}

}
val sourceMax = getNumNonRestParameters( source )
val targetMax = getNumNonRestParameters( target )
val checkCount = getNumParametersToCheckForSignatureRelatability( source, sourceMax, target, targetMax )
val sourceParams = source.parameters
val targetParams = target.parameters
{
var i = 0
while( (i<checkCount)) {
 {
 val s = (if ((i<sourceMax)) getTypeOfParameter( sourceParams(i) ) else getRestTypeOfSignature( source ))
val t = (if ((i<targetMax)) getTypeOfParameter( targetParams(i) ) else getRestTypeOfSignature( target ))
val related = (compareTypes( s, t, false )||compareTypes( t, s, reportErrors ))
if ((!related)) {
 if (reportErrors) {
 errorReporter( Diagnostics.Types_of_parameters_0_and_1_are_incompatible, sourceParams((if ((i<sourceMax)) i else sourceMax)).name, targetParams((if ((i<targetMax)) i else targetMax)).name )

}
return Ternary.False

}
(result&=related)

}
 (i+= 1)
}
}
if ((!ignoreReturnTypes)) {
 val targetReturnType = getReturnTypeOfSignature( target )
if ((targetReturnType===voidType)) {
 return result

}
val sourceReturnType = getReturnTypeOfSignature( source )
if (target.typePredicate) {
 if (source.typePredicate) {
 (result&=compareTypePredicateRelatedTo( source.typePredicate, target.typePredicate, reportErrors, errorReporter, compareTypes ))

}
else if (isIdentifierTypePredicate( target.typePredicate )) {
 if (reportErrors) {
 errorReporter( Diagnostics.Signature_0_must_have_a_type_predicate, signatureToString( source ) )

}
return Ternary.False

}

}
else {
 (result&=compareTypes( sourceReturnType, targetReturnType, reportErrors ))

}

}
return result

}
def compareTypePredicateRelatedTo(source: TypePredicate, target: TypePredicate, reportErrors: Boolean, errorReporter: ErrorReporter, compareTypes: ((Type, Type, Boolean) => Ternary)): Ternary = {
 if ((source.kind!==target.kind)) {
 if (reportErrors) {
 errorReporter( Diagnostics.A_this_based_type_guard_is_not_compatible_with_a_parameter_based_type_guard )
errorReporter( Diagnostics.Type_predicate_0_is_not_assignable_to_1, typePredicateToString( source ), typePredicateToString( target ) )

}
return Ternary.False

}
if ((source.kind===TypePredicateKind.Identifier)) {
 val sourceIdentifierPredicate = source.asInstanceOf[IdentifierTypePredicate]
val targetIdentifierPredicate = target.asInstanceOf[IdentifierTypePredicate]
if ((sourceIdentifierPredicate.parameterIndex!==targetIdentifierPredicate.parameterIndex)) {
 if (reportErrors) {
 errorReporter( Diagnostics.Parameter_0_is_not_in_the_same_position_as_parameter_1, sourceIdentifierPredicate.parameterName, targetIdentifierPredicate.parameterName )
errorReporter( Diagnostics.Type_predicate_0_is_not_assignable_to_1, typePredicateToString( source ), typePredicateToString( target ) )

}
return Ternary.False

}

}
val related = compareTypes( source.`type`, target.`type`, reportErrors )
if (((related===Ternary.False)&&reportErrors)) {
 errorReporter( Diagnostics.Type_predicate_0_is_not_assignable_to_1, typePredicateToString( source ), typePredicateToString( target ) )

}
return related

}
def isImplementationCompatibleWithOverload(implementation: Signature, overload: Signature): Boolean = {
 val erasedSource = getErasedSignature( implementation )
val erasedTarget = getErasedSignature( overload )
val sourceReturnType = getReturnTypeOfSignature( erasedSource )
val targetReturnType = getReturnTypeOfSignature( erasedTarget )
if ((((targetReturnType===voidType)||isTypeRelatedTo( targetReturnType, sourceReturnType, assignableRelation ))||isTypeRelatedTo( sourceReturnType, targetReturnType, assignableRelation ))) {
 return isSignatureAssignableTo( erasedSource, erasedTarget, true )

}
return false

}
def getNumNonRestParameters(signature: Signature) = {
 val numParams = signature.parameters.length
return (if (signature.hasRestParameter) (numParams-1) else numParams)

}
def getNumParametersToCheckForSignatureRelatability(source: Signature, sourceNonRestParamCount: Int, target: Signature, targetNonRestParamCount: Int) = {
 if ((source.hasRestParameter===target.hasRestParameter)) {
 if (source.hasRestParameter) {
 return (Math.max( sourceNonRestParamCount, targetNonRestParamCount )+1)

}
else {
 return Math.min( sourceNonRestParamCount, targetNonRestParamCount )

}

}
else {
 return (if (source.hasRestParameter) targetNonRestParamCount else sourceNonRestParamCount)

}

}
def isEnumTypeRelatedTo(source: EnumType, target: EnumType, errorReporter: ErrorReporter) = {
 if ((source===target)) {
 return true

}
val id = ((source.id+",")+target.id)
if ((enumRelation(id)!==undefined)) {
 return enumRelation(id)

}
if (((((source.symbol.name!==target.symbol.name)||(!((source.symbol.flags&SymbolFlags.RegularEnum))))||(!((target.symbol.flags&SymbolFlags.RegularEnum))))||(((source.flags&TypeFlags.Union))!==((target.flags&TypeFlags.Union))))) {
 return (enumRelation(id)=false)

}
val targetEnumType = getTypeOfSymbol( target.symbol )
(getPropertiesOfType( getTypeOfSymbol( source.symbol ) )).foreach { fresh54 =>
val property = zeroOfMyType
 = fresh54
 {
 if ((property.flags&SymbolFlags.EnumMember)) {
 val targetProperty = getPropertyOfType( targetEnumType, property.name )
if (((!targetProperty)||(!((targetProperty.flags&SymbolFlags.EnumMember))))) {
 if (errorReporter) {
 errorReporter( Diagnostics.Property_0_is_missing_in_type_1, property.name, typeToString( target, undefined, TypeFormatFlags.UseFullyQualifiedType ) )

}
return (enumRelation(id)=false)

}

}

}
}
return (enumRelation(id)=true)

}
def isSimpleTypeRelatedTo(source: Type, target: Type, relation: Map[RelationComparisonResult], errorReporter: ErrorReporter) = {
 if ((target.flags&TypeFlags.Never))
return false
if (((target.flags&TypeFlags.Any)||(source.flags&TypeFlags.Never)))
return true
if (((source.flags&TypeFlags.StringLike)&&(target.flags&TypeFlags.String)))
return true
if (((source.flags&TypeFlags.NumberLike)&&(target.flags&TypeFlags.Number)))
return true
if (((source.flags&TypeFlags.BooleanLike)&&(target.flags&TypeFlags.Boolean)))
return true
if ((((source.flags&TypeFlags.EnumLiteral)&&(target.flags&TypeFlags.Enum))&&((source.asInstanceOf[EnumLiteralType]).baseType===target)))
return true
if ((((source.flags&TypeFlags.Enum)&&(target.flags&TypeFlags.Enum))&&isEnumTypeRelatedTo( source.asInstanceOf[EnumType], target.asInstanceOf[EnumType], errorReporter )))
return true
if (((source.flags&TypeFlags.Undefined)&&(((!strictNullChecks)||(target.flags&((TypeFlags.Undefined|TypeFlags.Void)))))))
return true
if (((source.flags&TypeFlags.Null)&&(((!strictNullChecks)||(target.flags&TypeFlags.Null)))))
return true
if (((relation===assignableRelation)||(relation===comparableRelation))) {
 if ((source.flags&TypeFlags.Any))
return true
if (((((source.flags&TypeFlags.Number)|(source.flags&TypeFlags.NumberLiteral)))&&(target.flags&TypeFlags.EnumLike)))
return true
if (((((source.flags&TypeFlags.EnumLiteral)&&(target.flags&TypeFlags.EnumLiteral))&&((source.asInstanceOf[EnumLiteralType]).text===(target.asInstanceOf[EnumLiteralType]).text))&&isEnumTypeRelatedTo( (source.asInstanceOf[EnumLiteralType]).baseType, (target.asInstanceOf[EnumLiteralType]).baseType, errorReporter ))) {
 return true

}
if ((((source.flags&TypeFlags.EnumLiteral)&&(target.flags&TypeFlags.Enum))&&isEnumTypeRelatedTo( target.asInstanceOf[EnumType], (source.asInstanceOf[EnumLiteralType]).baseType, errorReporter ))) {
 return true

}

}
return false

}
def isTypeRelatedTo(source: Type, target: Type, relation: Map[RelationComparisonResult]) = {
 if (((source.flags&TypeFlags.StringOrNumberLiteral)&&(source.flags&TypeFlags.FreshLiteral))) {
 (source=(source.asInstanceOf[LiteralType]).regularType)

}
if (((target.flags&TypeFlags.StringOrNumberLiteral)&&(target.flags&TypeFlags.FreshLiteral))) {
 (target=(target.asInstanceOf[LiteralType]).regularType)

}
if (((source===target)||((relation!==identityRelation)&&isSimpleTypeRelatedTo( source, target, relation )))) {
 return true

}
if (((source.flags&TypeFlags.Object)&&(target.flags&TypeFlags.Object))) {
 val id = (if (((relation!==identityRelation)||(source.id<target.id))) ((source.id+",")+target.id) else ((target.id+",")+source.id))
val related = relation(id)
if ((related!==undefined)) {
 return (related===RelationComparisonResult.Succeeded)

}

}
if (((source.flags&TypeFlags.StructuredOrTypeParameter)||(target.flags&TypeFlags.StructuredOrTypeParameter))) {
 return checkTypeRelatedTo( source, target, relation, undefined, undefined, undefined )

}
return false

}
def checkTypeRelatedTo(source: Type, target: Type, relation: Map[RelationComparisonResult], errorNode: Node, headMessage: DiagnosticMessage, containingMessageChain: DiagnosticMessageChain): Boolean = {
 var errorInfo: DiagnosticMessageChain = zeroOfMyType
var sourceStack: Array[Type] = zeroOfMyType
var targetStack: Array[Type] = zeroOfMyType
var maybeStack: Array[Map[RelationComparisonResult]] = zeroOfMyType
var expandingFlags: Int = zeroOfMyType
var depth = 0
var overflow = false
Debug.assert( ((relation!==identityRelation)||(!errorNode)), "no error reporting in identity checking" )
val result = isRelatedTo( source, target, (!(!errorNode)), headMessage )
if (overflow) {
 error( errorNode, Diagnostics.Excessive_stack_depth_comparing_types_0_and_1, typeToString( source ), typeToString( target ) )

}
else if (errorInfo) {
 if (containingMessageChain) {
 (errorInfo=concatenateDiagnosticMessageChains( containingMessageChain, errorInfo ))

}
diagnostics.add( createDiagnosticForNodeFromMessageChain( errorNode, errorInfo ) )

}
return (result!==Ternary.False)
def reportError(message: DiagnosticMessage, arg0: String, arg1: String, arg2: String): Unit = {
 Debug.assert( (!(!errorNode)) )
(errorInfo=chainDiagnosticMessages( errorInfo, message, arg0, arg1, arg2 ))

}
def reportRelationError(message: DiagnosticMessage, source: Type, target: Type) = {
 var sourceType = typeToString( source )
var targetType = typeToString( target )
if ((sourceType===targetType)) {
 (sourceType=typeToString( source, undefined, TypeFormatFlags.UseFullyQualifiedType ))
(targetType=typeToString( target, undefined, TypeFormatFlags.UseFullyQualifiedType ))

}
if ((!message)) {
 (message=(if ((relation===comparableRelation)) Diagnostics.Type_0_is_not_comparable_to_type_1 else Diagnostics.Type_0_is_not_assignable_to_type_1))

}
reportError( message, sourceType, targetType )

}
def tryElaborateErrorsForPrimitivesAndObjects(source: Type, target: Type) = {
 val sourceType = typeToString( source )
val targetType = typeToString( target )
if (((((((globalStringType===source)&&(stringType===target)))||(((globalNumberType===source)&&(numberType===target))))||(((globalBooleanType===source)&&(booleanType===target))))||(((getGlobalESSymbolType()===source)&&(esSymbolType===target))))) {
 reportError( Diagnostics._0_is_a_primitive_but_1_is_a_wrapper_object_Prefer_using_0_when_possible, targetType, sourceType )

}

}
def isRelatedTo(source: Type, target: Type, reportErrors: Boolean, headMessage: DiagnosticMessage): Ternary = {
 var result: Ternary = zeroOfMyType
if (((source.flags&TypeFlags.StringOrNumberLiteral)&&(source.flags&TypeFlags.FreshLiteral))) {
 (source=(source.asInstanceOf[LiteralType]).regularType)

}
if (((target.flags&TypeFlags.StringOrNumberLiteral)&&(target.flags&TypeFlags.FreshLiteral))) {
 (target=(target.asInstanceOf[LiteralType]).regularType)

}
if ((source===target))
return Ternary.True
if ((relation===identityRelation)) {
 return isIdenticalTo( source, target )

}
if (isSimpleTypeRelatedTo( source, target, relation, (if (reportErrors) reportError else undefined) ))
return Ternary.True
if (((getObjectFlags( source )&ObjectFlags.ObjectLiteral)&&(source.flags&TypeFlags.FreshLiteral))) {
 if (hasExcessProperties( source.asInstanceOf[FreshObjectLiteralType], target, reportErrors )) {
 if (reportErrors) {
 reportRelationError( headMessage, source, target )

}
return Ternary.False

}
if ((target.flags&TypeFlags.UnionOrIntersection)) {
 (source=getRegularTypeOfObjectLiteral( source ))

}

}
val saveErrorInfo = errorInfo
if ((source.flags&TypeFlags.Union)) {
 if ((relation===comparableRelation)) {
 (result=someTypeRelatedToType( source.asInstanceOf[UnionType], target, (reportErrors&&(!((source.flags&TypeFlags.Primitive)))) ))

}
else {
 (result=eachTypeRelatedToType( source.asInstanceOf[UnionType], target, (reportErrors&&(!((source.flags&TypeFlags.Primitive)))) ))

}
if (result) {
 return result

}

}
else if ((target.flags&TypeFlags.Union)) {
 if ((result=typeRelatedToSomeType( source, target.asInstanceOf[UnionType], ((reportErrors&&(!((source.flags&TypeFlags.Primitive))))&&(!((target.flags&TypeFlags.Primitive)))) ))) {
 return result

}

}
else if ((target.flags&TypeFlags.Intersection)) {
 if ((result=typeRelatedToEachType( source, target.asInstanceOf[IntersectionType], reportErrors ))) {
 return result

}

}
else if ((source.flags&TypeFlags.Intersection)) {
 if ((result=someTypeRelatedToType( source.asInstanceOf[IntersectionType], target, false ))) {
 return result

}

}
if ((source.flags&TypeFlags.TypeParameter)) {
 var constraint = getConstraintOfTypeParameter( source.asInstanceOf[TypeParameter] )
if (((!constraint)||(constraint.flags&TypeFlags.Any))) {
 (constraint=emptyObjectType)

}
(constraint=getTypeWithThisArgument( constraint, source ))
val reportConstraintErrors = (reportErrors&&(constraint!==emptyObjectType))
if ((result=isRelatedTo( constraint, target, reportConstraintErrors ))) {
 (errorInfo=saveErrorInfo)
return result

}

}
else {
 if ((((getObjectFlags( source )&ObjectFlags.Reference)&&(getObjectFlags( target )&ObjectFlags.Reference))&&((source.asInstanceOf[TypeReference]).target===(target.asInstanceOf[TypeReference]).target))) {
 if ((result=typeArgumentsRelatedTo( source.asInstanceOf[TypeReference], target.asInstanceOf[TypeReference], reportErrors ))) {
 return result

}

}
val apparentSource = getApparentType( source )
if (((apparentSource.flags&((TypeFlags.Object|TypeFlags.Intersection)))&&(target.flags&TypeFlags.Object))) {
 val reportStructuralErrors = ((reportErrors&&(errorInfo===saveErrorInfo))&&(!((source.flags&TypeFlags.Primitive))))
if ((result=objectTypeRelatedTo( apparentSource, source, target, reportStructuralErrors ))) {
 (errorInfo=saveErrorInfo)
return result

}

}

}
if (reportErrors) {
 if (((source.flags&TypeFlags.Object)&&(target.flags&TypeFlags.Primitive))) {
 tryElaborateErrorsForPrimitivesAndObjects( source, target )

}
else if (((source.symbol&&(source.flags&TypeFlags.Object))&&(globalObjectType===source))) {
 reportError( Diagnostics.The_Object_type_is_assignable_to_very_few_other_types_Did_you_mean_to_use_the_any_type_instead )

}
reportRelationError( headMessage, source, target )

}
return Ternary.False

}
def isIdenticalTo(source: Type, target: Type): Ternary = {
 var result: Ternary = zeroOfMyType
if (((source.flags&TypeFlags.Object)&&(target.flags&TypeFlags.Object))) {
 if ((((getObjectFlags( source )&ObjectFlags.Reference)&&(getObjectFlags( target )&ObjectFlags.Reference))&&((source.asInstanceOf[TypeReference]).target===(target.asInstanceOf[TypeReference]).target))) {
 if ((result=typeArgumentsRelatedTo( source.asInstanceOf[TypeReference], target.asInstanceOf[TypeReference], false ))) {
 return result

}

}
return objectTypeRelatedTo( source, source, target, false )

}
if ((((source.flags&TypeFlags.Union)&&(target.flags&TypeFlags.Union))||((source.flags&TypeFlags.Intersection)&&(target.flags&TypeFlags.Intersection)))) {
 if ((result=eachTypeRelatedToSomeType( source.asInstanceOf[UnionOrIntersectionType], target.asInstanceOf[UnionOrIntersectionType] ))) {
 if ((result&=eachTypeRelatedToSomeType( target.asInstanceOf[UnionOrIntersectionType], source.asInstanceOf[UnionOrIntersectionType] ))) {
 return result

}

}

}
return Ternary.False

}
def isKnownProperty(`type`: Type, name: String): Boolean = {
 if ((`type`.flags&TypeFlags.Object)) {
 val resolved = resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] )
if ((((((((relation===assignableRelation)||(relation===comparableRelation)))&&(((`type`===globalObjectType)||isEmptyObjectType( resolved ))))||resolved.stringIndexInfo)||((resolved.numberIndexInfo&&isNumericLiteralName( name ))))||getPropertyOfType( `type`, name ))) {
 return true

}

}
else if ((`type`.flags&TypeFlags.UnionOrIntersection)) {
 ((`type`.asInstanceOf[UnionOrIntersectionType]).types).foreach { fresh55 =>
val t = zeroOfMyType
 = fresh55
 {
 if (isKnownProperty( t, name )) {
 return true

}

}
}

}
return false

}
def isEmptyObjectType(t: ResolvedType) = {
 return (((((t.properties.length===0)&&(t.callSignatures.length===0))&&(t.constructSignatures.length===0))&&(!t.stringIndexInfo))&&(!t.numberIndexInfo))

}
def hasExcessProperties(source: FreshObjectLiteralType, target: Type, reportErrors: Boolean): Boolean = {
 if ((maybeTypeOfKind( target, TypeFlags.Object )&&(!((getObjectFlags( target )&ObjectFlags.ObjectLiteralPatternWithComputedProperties))))) {
 (getPropertiesOfObjectType( source )).foreach { fresh56 =>
val prop = zeroOfMyType
 = fresh56
 {
 if ((!isKnownProperty( target, prop.name ))) {
 if (reportErrors) {
 Debug.assert( (!(!errorNode)) )
(errorNode=prop.valueDeclaration)
reportError( Diagnostics.Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1, symbolToString( prop ), typeToString( target ) )

}
return true

}

}
}

}
return false

}
def eachTypeRelatedToSomeType(source: UnionOrIntersectionType, target: UnionOrIntersectionType): Ternary = {
 var result = Ternary.True
val sourceTypes = source.types
(sourceTypes).foreach { fresh57 =>
val sourceType = zeroOfMyType
 = fresh57
 {
 val related = typeRelatedToSomeType( sourceType, target, false )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}
}
return result

}
def typeRelatedToSomeType(source: Type, target: UnionOrIntersectionType, reportErrors: Boolean): Ternary = {
 val targetTypes = target.types
if (((target.flags&TypeFlags.Union)&&containsType( targetTypes, source ))) {
 return Ternary.True

}
val len = targetTypes.length
{
var i = 0
while( (i<len)) {
 {
 val related = isRelatedTo( source, targetTypes(i), (reportErrors&&(i===(len-1))) )
if (related) {
 return related

}

}
 (i+= 1)
}
}
return Ternary.False

}
def typeRelatedToEachType(source: Type, target: UnionOrIntersectionType, reportErrors: Boolean): Ternary = {
 var result = Ternary.True
val targetTypes = target.types
(targetTypes).foreach { fresh58 =>
val targetType = zeroOfMyType
 = fresh58
 {
 val related = isRelatedTo( source, targetType, reportErrors )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}
}
return result

}
def someTypeRelatedToType(source: UnionOrIntersectionType, target: Type, reportErrors: Boolean): Ternary = {
 val sourceTypes = source.types
if (((source.flags&TypeFlags.Union)&&containsType( sourceTypes, target ))) {
 return Ternary.True

}
val len = sourceTypes.length
{
var i = 0
while( (i<len)) {
 {
 val related = isRelatedTo( sourceTypes(i), target, (reportErrors&&(i===(len-1))) )
if (related) {
 return related

}

}
 (i+= 1)
}
}
return Ternary.False

}
def eachTypeRelatedToType(source: UnionOrIntersectionType, target: Type, reportErrors: Boolean): Ternary = {
 var result = Ternary.True
val sourceTypes = source.types
(sourceTypes).foreach { fresh59 =>
val sourceType = zeroOfMyType
 = fresh59
 {
 val related = isRelatedTo( sourceType, target, reportErrors )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}
}
return result

}
def typeArgumentsRelatedTo(source: TypeReference, target: TypeReference, reportErrors: Boolean): Ternary = {
 val sources = (source.typeArguments||emptyArray)
val targets = (target.typeArguments||emptyArray)
if (((sources.length!==targets.length)&&(relation===identityRelation))) {
 return Ternary.False

}
val length = (if ((sources.length<=targets.length)) sources.length else targets.length)
var result = Ternary.True
{
var i = 0
while( (i<length)) {
 {
 val related = isRelatedTo( sources(i), targets(i), reportErrors )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}
 (i+= 1)
}
}
return result

}
def objectTypeRelatedTo(source: Type, originalSource: Type, target: Type, reportErrors: Boolean): Ternary = {
 if (overflow) {
 return Ternary.False

}
val id = (if (((relation!==identityRelation)||(source.id<target.id))) ((source.id+",")+target.id) else ((target.id+",")+source.id))
val related = relation(id)
if ((related!==undefined)) {
 if ((reportErrors&&(related===RelationComparisonResult.Failed))) {
 (relation(id)=RelationComparisonResult.FailedAndReported)

}
else {
 return (if ((related===RelationComparisonResult.Succeeded)) Ternary.True else Ternary.False)

}

}
if ((depth>0)) {
 {
var i = 0
while( (i<depth)) {
 {
 if (maybeStack(i)(id)) {
 return Ternary.Maybe

}

}
 (i+= 1)
}
}
if ((depth===100)) {
 (overflow=true)
return Ternary.False

}

}
else {
 (sourceStack=Array())
(targetStack=Array())
(maybeStack=Array())
(expandingFlags=0)

}
(sourceStack(depth)=source)
(targetStack(depth)=target)
(maybeStack(depth)=createMap[ RelationComparisonResult ]())
(maybeStack(depth)(id)=RelationComparisonResult.Succeeded)
(depth+= 1)
val saveExpandingFlags = expandingFlags
if (((!((expandingFlags&1)))&&isDeeplyNestedGeneric( source, sourceStack, depth )))
(expandingFlags|=1)
if (((!((expandingFlags&2)))&&isDeeplyNestedGeneric( target, targetStack, depth )))
(expandingFlags|=2)
var result: Ternary = zeroOfMyType
if ((expandingFlags===3)) {
 (result=Ternary.Maybe)

}
else {
 (result=propertiesRelatedTo( source, target, reportErrors ))
if (result) {
 (result&=signaturesRelatedTo( source, target, SignatureKind.Call, reportErrors ))
if (result) {
 (result&=signaturesRelatedTo( source, target, SignatureKind.Construct, reportErrors ))
if (result) {
 (result&=indexTypesRelatedTo( source, originalSource, target, IndexKind.String, reportErrors ))
if (result) {
 (result&=indexTypesRelatedTo( source, originalSource, target, IndexKind.Number, reportErrors ))

}

}

}

}

}
(expandingFlags=saveExpandingFlags)
(depth-= 1)
if (result) {
 val maybeCache = maybeStack(depth)
val destinationCache = (if ((((result===Ternary.True)||(depth===0)))) relation else maybeStack((depth-1)))
copyProperties( maybeCache, destinationCache )

}
else {
 (relation(id)=(if (reportErrors) RelationComparisonResult.FailedAndReported else RelationComparisonResult.Failed))

}
return result

}
def propertiesRelatedTo(source: Type, target: Type, reportErrors: Boolean): Ternary = {
 if ((relation===identityRelation)) {
 return propertiesIdenticalTo( source, target )

}
var result = Ternary.True
val properties = getPropertiesOfObjectType( target )
val requireOptionalProperties = ((relation===subtypeRelation)&&(!((getObjectFlags( source )&ObjectFlags.ObjectLiteral))))
(properties).foreach { fresh60 =>
val targetProp = zeroOfMyType
 = fresh60
 {
 val sourceProp = getPropertyOfType( source, targetProp.name )
if ((sourceProp!==targetProp)) {
 if ((!sourceProp)) {
 if (((!((targetProp.flags&SymbolFlags.Optional)))||requireOptionalProperties)) {
 if (reportErrors) {
 reportError( Diagnostics.Property_0_is_missing_in_type_1, symbolToString( targetProp ), typeToString( source ) )

}
return Ternary.False

}

}
else if ((!((targetProp.flags&SymbolFlags.Prototype)))) {
 val sourcePropFlags = getDeclarationModifierFlagsFromSymbol( sourceProp )
val targetPropFlags = getDeclarationModifierFlagsFromSymbol( targetProp )
if (((sourcePropFlags&ModifierFlags.Private)||(targetPropFlags&ModifierFlags.Private))) {
 if ((sourceProp.valueDeclaration!==targetProp.valueDeclaration)) {
 if (reportErrors) {
 if (((sourcePropFlags&ModifierFlags.Private)&&(targetPropFlags&ModifierFlags.Private))) {
 reportError( Diagnostics.Types_have_separate_declarations_of_a_private_property_0, symbolToString( targetProp ) )

}
else {
 reportError( Diagnostics.Property_0_is_private_in_type_1_but_not_in_type_2, symbolToString( targetProp ), typeToString( (if ((sourcePropFlags&ModifierFlags.Private)) source else target) ), typeToString( (if ((sourcePropFlags&ModifierFlags.Private)) target else source) ) )

}

}
return Ternary.False

}

}
else if ((targetPropFlags&ModifierFlags.Protected)) {
 val sourceDeclaredInClass = (sourceProp.parent&&(sourceProp.parent.flags&SymbolFlags.Class))
val sourceClass = (if (sourceDeclaredInClass) getDeclaredTypeOfSymbol( getParentOfSymbol( sourceProp ) ).asInstanceOf[InterfaceType] else undefined)
val targetClass = getDeclaredTypeOfSymbol( getParentOfSymbol( targetProp ) ).asInstanceOf[InterfaceType]
if (((!sourceClass)||(!hasBaseType( sourceClass, targetClass )))) {
 if (reportErrors) {
 reportError( Diagnostics.Property_0_is_protected_but_type_1_is_not_a_class_derived_from_2, symbolToString( targetProp ), typeToString( (sourceClass||source) ), typeToString( targetClass ) )

}
return Ternary.False

}

}
else if ((sourcePropFlags&ModifierFlags.Protected)) {
 if (reportErrors) {
 reportError( Diagnostics.Property_0_is_protected_in_type_1_but_public_in_type_2, symbolToString( targetProp ), typeToString( source ), typeToString( target ) )

}
return Ternary.False

}
val related = isRelatedTo( getTypeOfSymbol( sourceProp ), getTypeOfSymbol( targetProp ), reportErrors )
if ((!related)) {
 if (reportErrors) {
 reportError( Diagnostics.Types_of_property_0_are_incompatible, symbolToString( targetProp ) )

}
return Ternary.False

}
(result&=related)
if (((sourceProp.flags&SymbolFlags.Optional)&&(!((targetProp.flags&SymbolFlags.Optional))))) {
 if (reportErrors) {
 reportError( Diagnostics.Property_0_is_optional_in_type_1_but_required_in_type_2, symbolToString( targetProp ), typeToString( source ), typeToString( target ) )

}
return Ternary.False

}

}

}

}
}
return result

}
def propertiesIdenticalTo(source: Type, target: Type): Ternary = {
 if ((!(((source.flags&TypeFlags.Object)&&(target.flags&TypeFlags.Object))))) {
 return Ternary.False

}
val sourceProperties = getPropertiesOfObjectType( source )
val targetProperties = getPropertiesOfObjectType( target )
if ((sourceProperties.length!==targetProperties.length)) {
 return Ternary.False

}
var result = Ternary.True
(sourceProperties).foreach { fresh61 =>
val sourceProp = zeroOfMyType
 = fresh61
 {
 val targetProp = getPropertyOfObjectType( target, sourceProp.name )
if ((!targetProp)) {
 return Ternary.False

}
val related = compareProperties( sourceProp, targetProp, isRelatedTo )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}
}
return result

}
def signaturesRelatedTo(source: Type, target: Type, kind: SignatureKind, reportErrors: Boolean): Ternary = {
 if ((relation===identityRelation)) {
 return signaturesIdenticalTo( source, target, kind )

}
if (((target===anyFunctionType)||(source===anyFunctionType))) {
 return Ternary.True

}
val sourceSignatures = getSignaturesOfType( source, kind )
val targetSignatures = getSignaturesOfType( target, kind )
if ((((kind===SignatureKind.Construct)&&sourceSignatures.length)&&targetSignatures.length)) {
 if ((isAbstractConstructorType( source )&&(!isAbstractConstructorType( target )))) {
 if (reportErrors) {
 reportError( Diagnostics.Cannot_assign_an_abstract_constructor_type_to_a_non_abstract_constructor_type )

}
return Ternary.False

}
if ((!constructorVisibilitiesAreCompatible( sourceSignatures(0), targetSignatures(0), reportErrors ))) {
 return Ternary.False

}

}
var result = Ternary.True
val saveErrorInfo = errorInfo
val outer = new scala.util.control.Breaks
outer.breakable {
(targetSignatures).foreach { fresh62 =>
val t = zeroOfMyType
 = fresh62
 {
 var shouldElaborateErrors = reportErrors
(sourceSignatures).foreach { fresh63 =>
val s = zeroOfMyType
 = fresh63
 {
 val related = signatureRelatedTo( s, t, shouldElaborateErrors )
if (related) {
 (result&=related)
(errorInfo=saveErrorInfo)
continue outer

}
(shouldElaborateErrors=false)

}
}
if (shouldElaborateErrors) {
 reportError( Diagnostics.Type_0_provides_no_match_for_the_signature_1, typeToString( source ), signatureToString( t, undefined, undefined, kind ) )

}
return Ternary.False

}
}
}
return result

}
def signatureRelatedTo(source: Signature, target: Signature, reportErrors: Boolean): Ternary = {
 return compareSignaturesRelated( source, target, false, reportErrors, reportError, isRelatedTo )

}
def signaturesIdenticalTo(source: Type, target: Type, kind: SignatureKind): Ternary = {
 val sourceSignatures = getSignaturesOfType( source, kind )
val targetSignatures = getSignaturesOfType( target, kind )
if ((sourceSignatures.length!==targetSignatures.length)) {
 return Ternary.False

}
var result = Ternary.True
{
var i = 0
var len = sourceSignatures.length
while( (i<len)) {
 {
 val related = compareSignaturesIdentical( sourceSignatures(i), targetSignatures(i), false, false, false, isRelatedTo )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}
 (i+= 1)
}
}
return result

}
def eachPropertyRelatedTo(source: Type, target: Type, kind: IndexKind, reportErrors: Boolean): Ternary = {
 var result = Ternary.True
(getPropertiesOfObjectType( source )).foreach { fresh64 =>
val prop = zeroOfMyType
 = fresh64
 {
 if (((kind===IndexKind.String)||isNumericLiteralName( prop.name ))) {
 val related = isRelatedTo( getTypeOfSymbol( prop ), target, reportErrors )
if ((!related)) {
 if (reportErrors) {
 reportError( Diagnostics.Property_0_is_incompatible_with_index_signature, symbolToString( prop ) )

}
return Ternary.False

}
(result&=related)

}

}
}
return result

}
def indexInfoRelatedTo(sourceInfo: IndexInfo, targetInfo: IndexInfo, reportErrors: Boolean) = {
 val related = isRelatedTo( sourceInfo.`type`, targetInfo.`type`, reportErrors )
if (((!related)&&reportErrors)) {
 reportError( Diagnostics.Index_signatures_are_incompatible )

}
return related

}
def indexTypesRelatedTo(source: Type, originalSource: Type, target: Type, kind: IndexKind, reportErrors: Boolean) = {
 if ((relation===identityRelation)) {
 return indexTypesIdenticalTo( source, target, kind )

}
val targetInfo = getIndexInfoOfType( target, kind )
if (((!targetInfo)||((((targetInfo.`type`.flags&TypeFlags.Any))&&(!((originalSource.flags&TypeFlags.Primitive))))))) {
 return Ternary.True

}
val sourceInfo = (getIndexInfoOfType( source, kind )||((kind===IndexKind.Number)&&getIndexInfoOfType( source, IndexKind.String )))
if (sourceInfo) {
 return indexInfoRelatedTo( sourceInfo, targetInfo, reportErrors )

}
if (isObjectLiteralType( source )) {
 var related = Ternary.True
if ((kind===IndexKind.String)) {
 val sourceNumberInfo = getIndexInfoOfType( source, IndexKind.Number )
if (sourceNumberInfo) {
 (related=indexInfoRelatedTo( sourceNumberInfo, targetInfo, reportErrors ))

}

}
if (related) {
 (related&=eachPropertyRelatedTo( source, targetInfo.`type`, kind, reportErrors ))

}
return related

}
if (reportErrors) {
 reportError( Diagnostics.Index_signature_is_missing_in_type_0, typeToString( source ) )

}
return Ternary.False

}
def indexTypesIdenticalTo(source: Type, target: Type, indexKind: IndexKind): Ternary = {
 val targetInfo = getIndexInfoOfType( target, indexKind )
val sourceInfo = getIndexInfoOfType( source, indexKind )
if (((!sourceInfo)&&(!targetInfo))) {
 return Ternary.True

}
if (((sourceInfo&&targetInfo)&&(sourceInfo.isReadonly===targetInfo.isReadonly))) {
 return isRelatedTo( sourceInfo.`type`, targetInfo.`type` )

}
return Ternary.False

}
def constructorVisibilitiesAreCompatible(sourceSignature: Signature, targetSignature: Signature, reportErrors: Boolean) = {
 if (((!sourceSignature.declaration)||(!targetSignature.declaration))) {
 return true

}
val sourceAccessibility = (getModifierFlags( sourceSignature.declaration )&ModifierFlags.NonPublicAccessibilityModifier)
val targetAccessibility = (getModifierFlags( targetSignature.declaration )&ModifierFlags.NonPublicAccessibilityModifier)
if ((targetAccessibility===ModifierFlags.Private)) {
 return true

}
if (((targetAccessibility===ModifierFlags.Protected)&&(sourceAccessibility!==ModifierFlags.Private))) {
 return true

}
if (((targetAccessibility!==ModifierFlags.Protected)&&(!sourceAccessibility))) {
 return true

}
if (reportErrors) {
 reportError( Diagnostics.Cannot_assign_a_0_constructor_type_to_a_1_constructor_type, visibilityToString( sourceAccessibility ), visibilityToString( targetAccessibility ) )

}
return false

}

}
def isAbstractConstructorType(`type`: Type) = {
 if ((getObjectFlags( `type` )&ObjectFlags.Anonymous)) {
 val symbol = `type`.symbol
if ((symbol&&(symbol.flags&SymbolFlags.Class))) {
 val declaration = getClassLikeDeclarationOfSymbol( symbol )
if ((declaration&&(getModifierFlags( declaration )&ModifierFlags.Abstract))) {
 return true

}

}

}
return false

}
def isDeeplyNestedGeneric(`type`: Type, stack: Array[Type], depth: Int): Boolean = {
 if (((getObjectFlags( `type` )&((ObjectFlags.Reference|ObjectFlags.Instantiated)))&&(depth>=5))) {
 val symbol = `type`.symbol
var count = 0
{
var i = 0
while( (i<depth)) {
 {
 val t = stack(i)
if (((getObjectFlags( t )&((ObjectFlags.Reference|ObjectFlags.Instantiated)))&&(t.symbol===symbol))) {
 (count+= 1)
if ((count>=5))
return true

}

}
 (i+= 1)
}
}

}
return false

}
def isPropertyIdenticalTo(sourceProp: Symbol, targetProp: Symbol): Boolean = {
 return (compareProperties( sourceProp, targetProp, compareTypesIdentical )!==Ternary.False)

}
def compareProperties(sourceProp: Symbol, targetProp: Symbol, compareTypes: ((Type, Type) => Ternary)): Ternary = {
 if ((sourceProp===targetProp)) {
 return Ternary.True

}
val sourcePropAccessibility = (getDeclarationModifierFlagsFromSymbol( sourceProp )&ModifierFlags.NonPublicAccessibilityModifier)
val targetPropAccessibility = (getDeclarationModifierFlagsFromSymbol( targetProp )&ModifierFlags.NonPublicAccessibilityModifier)
if ((sourcePropAccessibility!==targetPropAccessibility)) {
 return Ternary.False

}
if (sourcePropAccessibility) {
 if ((getTargetSymbol( sourceProp )!==getTargetSymbol( targetProp ))) {
 return Ternary.False

}

}
else {
 if ((((sourceProp.flags&SymbolFlags.Optional))!==((targetProp.flags&SymbolFlags.Optional)))) {
 return Ternary.False

}

}
if ((isReadonlySymbol( sourceProp )!==isReadonlySymbol( targetProp ))) {
 return Ternary.False

}
return compareTypes( getTypeOfSymbol( sourceProp ), getTypeOfSymbol( targetProp ) )

}
def isMatchingSignature(source: Signature, target: Signature, partialMatch: Boolean) = {
 if ((((source.parameters.length===target.parameters.length)&&(source.minArgumentCount===target.minArgumentCount))&&(source.hasRestParameter===target.hasRestParameter))) {
 return true

}
val sourceRestCount = (if (source.hasRestParameter) 1 else 0)
val targetRestCount = (if (target.hasRestParameter) 1 else 0)
if (((partialMatch&&(source.minArgumentCount<=target.minArgumentCount))&&(((sourceRestCount>targetRestCount)||((sourceRestCount===targetRestCount)&&(source.parameters.length>=target.parameters.length)))))) {
 return true

}
return false

}
def compareSignaturesIdentical(source: Signature, target: Signature, partialMatch: Boolean, ignoreThisTypes: Boolean, ignoreReturnTypes: Boolean, compareTypes: ((Type, Type) => Ternary)): Ternary = {
 if ((source===target)) {
 return Ternary.True

}
if ((!(isMatchingSignature( source, target, partialMatch )))) {
 return Ternary.False

}
if ((((if (source.typeParameters) source.typeParameters.length else 0))!==((if (target.typeParameters) target.typeParameters.length else 0)))) {
 return Ternary.False

}
(source=getErasedSignature( source ))
(target=getErasedSignature( target ))
var result = Ternary.True
if ((!ignoreThisTypes)) {
 val sourceThisType = getThisTypeOfSignature( source )
if (sourceThisType) {
 val targetThisType = getThisTypeOfSignature( target )
if (targetThisType) {
 val related = compareTypes( sourceThisType, targetThisType )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}

}

}
val targetLen = target.parameters.length
{
var i = 0
while( (i<targetLen)) {
 {
 val s = (if (isRestParameterIndex( source, i )) getRestTypeOfSignature( source ) else getTypeOfParameter( source.parameters(i) ))
val t = (if (isRestParameterIndex( target, i )) getRestTypeOfSignature( target ) else getTypeOfParameter( target.parameters(i) ))
val related = compareTypes( s, t )
if ((!related)) {
 return Ternary.False

}
(result&=related)

}
 (i+= 1)
}
}
if ((!ignoreReturnTypes)) {
 (result&=compareTypes( getReturnTypeOfSignature( source ), getReturnTypeOfSignature( target ) ))

}
return result

}
def isRestParameterIndex(signature: Signature, parameterIndex: Int) = {
 return (signature.hasRestParameter&&(parameterIndex>=(signature.parameters.length-1)))

}
def isSupertypeOfEach(candidate: Type, types: Array[Type]): Boolean = {
 (types).foreach { fresh65 =>
val t = zeroOfMyType
 = fresh65
 {
 if (((candidate!==t)&&(!isTypeSubtypeOf( t, candidate ))))
return false

}
}
return true

}
def literalTypesWithSameBaseType(types: Array[Type]): Boolean = {
 var commonBaseType: Type = zeroOfMyType
(types).foreach { fresh66 =>
val t = zeroOfMyType
 = fresh66
 {
 val baseType = getBaseTypeOfLiteralType( t )
if ((!commonBaseType)) {
 (commonBaseType=baseType)

}
if (((baseType===t)||(baseType!==commonBaseType))) {
 return false

}

}
}
return true

}
def getSupertypeOrUnion(types: Array[Type]): Type = {
 return (if (literalTypesWithSameBaseType( types )) getUnionType( types ) else forEach( types, (t =>  (if (isSupertypeOfEach( t, types )) t else undefined)) ))

}
def getCommonSupertype(types: Array[Type]): Type = {
 if ((!strictNullChecks)) {
 return getSupertypeOrUnion( types )

}
val primaryTypes = filter( types, (t =>  (!((t.flags&TypeFlags.Nullable)))) )
if ((!primaryTypes.length)) {
 return getUnionType( types, true )

}
val supertype = getSupertypeOrUnion( primaryTypes )
return (supertype&&includeFalsyTypes( supertype, (getFalsyFlagsOfTypes( types )&TypeFlags.Nullable) ))

}
def reportNoCommonSupertypeError(types: Array[Type], errorLocation: Node, errorMessageChainHead: DiagnosticMessageChain): Unit = {
 var bestSupertype: Type = zeroOfMyType
var bestSupertypeDownfallType: Type = zeroOfMyType
var bestSupertypeScore = 0
{
var i = 0
while( (i<types.length)) {
 {
 var score = 0
var downfallType: Type = undefined
{
var j = 0
while( (j<types.length)) {
 {
 if (isTypeSubtypeOf( types(j), types(i) )) {
 (score+= 1)

}
else if ((!downfallType)) {
 (downfallType=types(j))

}

}
 (j+= 1)
}
}
Debug.assert( (!(!downfallType)), "If there is no common supertype, each type should have a downfallType" )
if ((score>bestSupertypeScore)) {
 (bestSupertype=types(i))
(bestSupertypeDownfallType=downfallType)
(bestSupertypeScore=score)

}
if ((bestSupertypeScore===(types.length-1))) {
 break()

}

}
 (i+= 1)
}
}
checkTypeSubtypeOf( bestSupertypeDownfallType, bestSupertype, errorLocation, Diagnostics.Type_argument_candidate_1_is_not_a_valid_type_argument_because_it_is_not_a_supertype_of_candidate_0, errorMessageChainHead )

}
def isArrayType(`type`: Type): Boolean = {
 return ((getObjectFlags( `type` )&ObjectFlags.Reference)&&((`type`.asInstanceOf[TypeReference]).target===globalArrayType))

}
def isArrayLikeType(`type`: Type): Boolean = {
 return (((getObjectFlags( `type` )&ObjectFlags.Reference)&&((((`type`.asInstanceOf[TypeReference]).target===globalArrayType)||((`type`.asInstanceOf[TypeReference]).target===globalReadonlyArrayType))))||((!((`type`.flags&TypeFlags.Nullable)))&&isTypeAssignableTo( `type`, anyReadonlyArrayType )))

}
def isTupleLikeType(`type`: Type): Boolean = {
 return (!(!getPropertyOfType( `type`, "0" )))

}
def isUnitType(`type`: Type): Boolean = {
 return (((`type`.flags&(((TypeFlags.Literal|TypeFlags.Undefined)|TypeFlags.Null))))!==0)

}
def isLiteralType(`type`: Type): Boolean = {
 return (if ((`type`.flags&TypeFlags.Boolean)) true else (if ((`type`.flags&TypeFlags.Union)) (if ((`type`.flags&TypeFlags.Enum)) true else (!forEach( (`type`.asInstanceOf[UnionType]).types, (t =>  (!isUnitType( t ))) ))) else isUnitType( `type` )))

}
def getBaseTypeOfLiteralType(`type`: Type): Type = {
 return (if ((`type`.flags&TypeFlags.StringLiteral)) stringType else (if ((`type`.flags&TypeFlags.NumberLiteral)) numberType else (if ((`type`.flags&TypeFlags.BooleanLiteral)) booleanType else (if ((`type`.flags&TypeFlags.EnumLiteral)) (`type`.asInstanceOf[EnumLiteralType]).baseType else (if (((`type`.flags&TypeFlags.Union)&&(!((`type`.flags&TypeFlags.Enum))))) getUnionType( sameMap( (`type`.asInstanceOf[UnionType]).types, getBaseTypeOfLiteralType ) ) else `type`)))))

}
def getWidenedLiteralType(`type`: Type): Type = {
 return (if (((`type`.flags&TypeFlags.StringLiteral)&&(`type`.flags&TypeFlags.FreshLiteral))) stringType else (if (((`type`.flags&TypeFlags.NumberLiteral)&&(`type`.flags&TypeFlags.FreshLiteral))) numberType else (if ((`type`.flags&TypeFlags.BooleanLiteral)) booleanType else (if ((`type`.flags&TypeFlags.EnumLiteral)) (`type`.asInstanceOf[EnumLiteralType]).baseType else (if (((`type`.flags&TypeFlags.Union)&&(!((`type`.flags&TypeFlags.Enum))))) getUnionType( sameMap( (`type`.asInstanceOf[UnionType]).types, getWidenedLiteralType ) ) else `type`)))))

}
def isTupleType(`type`: Type): Boolean = {
 return (!(!(((getObjectFlags( `type` )&ObjectFlags.Reference)&&((`type`.asInstanceOf[TypeReference]).target.objectFlags&ObjectFlags.Tuple)))))

}
def getFalsyFlagsOfTypes(types: Array[Type]): TypeFlags = {
 var result: TypeFlags = 0
(types).foreach { fresh67 =>
val t = zeroOfMyType
 = fresh67
 {
 (result|=getFalsyFlags( t ))

}
}
return result

}
def getFalsyFlags(`type`: Type): TypeFlags = {
 return (if ((`type`.flags&TypeFlags.Union)) getFalsyFlagsOfTypes( (`type`.asInstanceOf[UnionType]).types ) else (if ((`type`.flags&TypeFlags.StringLiteral)) (if (((`type`.asInstanceOf[LiteralType]).text==="")) TypeFlags.StringLiteral else 0) else (if ((`type`.flags&TypeFlags.NumberLiteral)) (if (((`type`.asInstanceOf[LiteralType]).text==="0")) TypeFlags.NumberLiteral else 0) else (if ((`type`.flags&TypeFlags.BooleanLiteral)) (if ((`type`===falseType)) TypeFlags.BooleanLiteral else 0) else (`type`.flags&TypeFlags.PossiblyFalsy)))))

}
def includeFalsyTypes(`type`: Type, flags: TypeFlags) = {
 if ((((getFalsyFlags( `type` )&flags))===flags)) {
 return `type`

}
val types = Array( `type` )
if ((flags&TypeFlags.StringLike))
types.push( emptyStringType )
if ((flags&TypeFlags.NumberLike))
types.push( zeroType )
if ((flags&TypeFlags.BooleanLike))
types.push( falseType )
if ((flags&TypeFlags.Void))
types.push( voidType )
if ((flags&TypeFlags.Undefined))
types.push( undefinedType )
if ((flags&TypeFlags.Null))
types.push( nullType )
return getUnionType( types, true )

}
def removeDefinitelyFalsyTypes(`type`: Type): Type = {
 return (if ((getFalsyFlags( `type` )&TypeFlags.DefinitelyFalsy)) filterType( `type`, (t =>  (!((getFalsyFlags( t )&TypeFlags.DefinitelyFalsy)))) ) else `type`)

}
def getNonNullableType(`type`: Type): Type = {
 return (if (strictNullChecks) getTypeWithFacts( `type`, TypeFacts.NEUndefinedOrNull ) else `type`)

}
def isObjectLiteralType(`type`: Type) = {
 return (((`type`.symbol&&(((`type`.symbol.flags&((SymbolFlags.ObjectLiteral|SymbolFlags.TypeLiteral))))!==0))&&(getSignaturesOfType( `type`, SignatureKind.Call ).length===0))&&(getSignaturesOfType( `type`, SignatureKind.Construct ).length===0))

}
def createTransientSymbol(source: Symbol, `type`: Type) = {
 val symbol = createSymbol( (source.flags|SymbolFlags.Transient), source.name ).asInstanceOf[TransientSymbol]
(symbol.declarations=source.declarations)
(symbol.parent=source.parent)
(symbol.`type`=`type`)
(symbol.target=source)
if (source.valueDeclaration) {
 (symbol.valueDeclaration=source.valueDeclaration)

}
return symbol

}
def transformTypeOfMembers(`type`: Type, f: ((Type) => Type)) = {
 val members = createMap[ Symbol ]()
(getPropertiesOfObjectType( `type` )).foreach { fresh68 =>
val property = zeroOfMyType
 = fresh68
 {
 val original = getTypeOfSymbol( property )
val updated = f( original )
(members(property.name)=(if ((updated===original)) property else createTransientSymbol( property, updated )))

}
}
;
return members

}
def getRegularTypeOfObjectLiteral(`type`: Type): Type = {
 if ((!(((getObjectFlags( `type` )&ObjectFlags.ObjectLiteral)&&(`type`.flags&TypeFlags.FreshLiteral))))) {
 return `type`

}
val regularType = (`type`.asInstanceOf[FreshObjectLiteralType]).regularType
if (regularType) {
 return regularType

}
val resolved = `type`.asInstanceOf[ResolvedType]
val members = transformTypeOfMembers( `type`, getRegularTypeOfObjectLiteral )
val regularNew = createAnonymousType( resolved.symbol, members, resolved.callSignatures, resolved.constructSignatures, resolved.stringIndexInfo, resolved.numberIndexInfo )
(regularNew.flags=(resolved.flags&(~TypeFlags.FreshLiteral)))
(regularNew.objectFlags|=ObjectFlags.ObjectLiteral)
((`type`.asInstanceOf[FreshObjectLiteralType]).regularType=regularNew)
return regularNew

}
def getWidenedTypeOfObjectLiteral(`type`: Type): Type = {
 val members = transformTypeOfMembers( `type`, (prop =>  {
 val widened = getWidenedType( prop )
 return (if ((prop===widened)) prop else widened)

}) )
val stringIndexInfo = getIndexInfoOfType( `type`, IndexKind.String )
val numberIndexInfo = getIndexInfoOfType( `type`, IndexKind.Number )
return createAnonymousType( `type`.symbol, members, emptyArray, emptyArray, (stringIndexInfo&&createIndexInfo( getWidenedType( stringIndexInfo.`type` ), stringIndexInfo.isReadonly )), (numberIndexInfo&&createIndexInfo( getWidenedType( numberIndexInfo.`type` ), numberIndexInfo.isReadonly )) )

}
def getWidenedConstituentType(`type`: Type): Type = {
 return (if ((`type`.flags&TypeFlags.Nullable)) `type` else getWidenedType( `type` ))

}
def getWidenedType(`type`: Type): Type = {
 if ((`type`.flags&TypeFlags.RequiresWidening)) {
 if ((`type`.flags&TypeFlags.Nullable)) {
 return anyType

}
if ((getObjectFlags( `type` )&ObjectFlags.ObjectLiteral)) {
 return getWidenedTypeOfObjectLiteral( `type` )

}
if ((`type`.flags&TypeFlags.Union)) {
 return getUnionType( sameMap( (`type`.asInstanceOf[UnionType]).types, getWidenedConstituentType ) )

}
if ((isArrayType( `type` )||isTupleType( `type` ))) {
 return createTypeReference( (`type`.asInstanceOf[TypeReference]).target, sameMap( (`type`.asInstanceOf[TypeReference]).typeArguments, getWidenedType ) )

}

}
return `type`

}
def reportWideningErrorsInType(`type`: Type): Boolean = {
 var errorReported = false
if ((`type`.flags&TypeFlags.Union)) {
 ((`type`.asInstanceOf[UnionType]).types).foreach { fresh69 =>
val t = zeroOfMyType
 = fresh69
 {
 if (reportWideningErrorsInType( t )) {
 (errorReported=true)

}

}
}

}
if ((isArrayType( `type` )||isTupleType( `type` ))) {
 ((`type`.asInstanceOf[TypeReference]).typeArguments).foreach { fresh70 =>
val t = zeroOfMyType
 = fresh70
 {
 if (reportWideningErrorsInType( t )) {
 (errorReported=true)

}

}
}

}
if ((getObjectFlags( `type` )&ObjectFlags.ObjectLiteral)) {
 (getPropertiesOfObjectType( `type` )).foreach { fresh71 =>
val p = zeroOfMyType
 = fresh71
 {
 val t = getTypeOfSymbol( p )
if ((t.flags&TypeFlags.ContainsWideningType)) {
 if ((!reportWideningErrorsInType( t ))) {
 error( p.valueDeclaration, Diagnostics.Object_literal_s_property_0_implicitly_has_an_1_type, p.name, typeToString( getWidenedType( t ) ) )

}
(errorReported=true)

}

}
}

}
return errorReported

}
def reportImplicitAnyError(declaration: Declaration, `type`: Type) = {
 val typeAsString = typeToString( getWidenedType( `type` ) )
var diagnostic: DiagnosticMessage = zeroOfMyType
declaration.kind match {
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature  =>
(diagnostic=Diagnostics.Member_0_implicitly_has_an_1_type)
  case  SyntaxKind.Parameter  =>
(diagnostic=(if ((declaration.asInstanceOf[ParameterDeclaration]).dotDotDotToken) Diagnostics.Rest_parameter_0_implicitly_has_an_any_type else Diagnostics.Parameter_0_implicitly_has_an_1_type))
  case  SyntaxKind.BindingElement  =>
(diagnostic=Diagnostics.Binding_element_0_implicitly_has_an_1_type)
  case  SyntaxKind.FunctionDeclaration | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction  =>
if ((!declaration.name)) {
 error( declaration, Diagnostics.Function_expression_which_lacks_return_type_annotation_implicitly_has_an_0_return_type, typeAsString )
return

}
(diagnostic=Diagnostics._0_which_lacks_return_type_annotation_implicitly_has_an_1_return_type)
  case _ =>
(diagnostic=Diagnostics.Variable_0_implicitly_has_an_1_type)
}
error( declaration, diagnostic, declarationNameToString( declaration.name ), typeAsString )

}
def reportErrorsFromWidening(declaration: Declaration, `type`: Type) = {
 if (((produceDiagnostics&&compilerOptions.noImplicitAny)&&(`type`.flags&TypeFlags.ContainsWideningType))) {
 if ((!reportWideningErrorsInType( `type` ))) {
 reportImplicitAnyError( declaration, `type` )

}

}

}
def forEachMatchingParameterType(source: Signature, target: Signature, callback: ((Type, Type) => Unit)) = {
 val sourceMax = source.parameters.length
val targetMax = target.parameters.length
var count: Int = zeroOfMyType
if ((source.hasRestParameter&&target.hasRestParameter)) {
 (count=Math.max( sourceMax, targetMax ))

}
else if (source.hasRestParameter) {
 (count=targetMax)

}
else if (target.hasRestParameter) {
 (count=sourceMax)

}
else {
 (count=Math.min( sourceMax, targetMax ))

}
{
var i = 0
while( (i<count)) {
 {
 callback( getTypeAtPosition( source, i ), getTypeAtPosition( target, i ) )

}
 (i+= 1)
}
}

}
def createInferenceContext(signature: Signature, inferUnionTypes: Boolean): InferenceContext = {
 val inferences = map( signature.typeParameters, createTypeInferencesObject )
return Map( "signature" -> signature,
"inferUnionTypes" -> inferUnionTypes,
"inferences" -> inferences,
"inferredTypes" -> new Array( signature.typeParameters.length ) )

}
def createTypeInferencesObject(): TypeInferences = {
 return Map( "primary" -> undefined,
"secondary" -> undefined,
"topLevel" -> true,
"isFixed" -> false )

}
def couldContainTypeParameters(`type`: Type): Boolean = {
 return (!(!(((((`type`.flags&TypeFlags.TypeParameter)||((getObjectFlags( `type` )&ObjectFlags.Reference)&&forEach( (`type`.asInstanceOf[TypeReference]).typeArguments, couldContainTypeParameters )))||(((getObjectFlags( `type` )&ObjectFlags.Anonymous)&&`type`.symbol)&&(`type`.symbol.flags&(((SymbolFlags.Method|SymbolFlags.TypeLiteral)|SymbolFlags.Class)))))||((`type`.flags&TypeFlags.UnionOrIntersection)&&couldUnionOrIntersectionContainTypeParameters( `type`.asInstanceOf[UnionOrIntersectionType] ))))))

}
def couldUnionOrIntersectionContainTypeParameters(`type`: UnionOrIntersectionType): Boolean = {
 if ((`type`.couldContainTypeParameters===undefined)) {
 (`type`.couldContainTypeParameters=forEach( `type`.types, couldContainTypeParameters ))

}
return `type`.couldContainTypeParameters

}
def isTypeParameterAtTopLevel(`type`: Type, typeParameter: TypeParameter): Boolean = {
 return ((`type`===typeParameter)||((`type`.flags&TypeFlags.UnionOrIntersection)&&forEach( (`type`.asInstanceOf[UnionOrIntersectionType]).types, (t =>  isTypeParameterAtTopLevel( t, typeParameter )) )))

}
def inferTypes(context: InferenceContext, originalSource: Type, originalTarget: Type) = {
 val typeParameters = context.signature.typeParameters
var sourceStack: Array[Type] = zeroOfMyType
var targetStack: Array[Type] = zeroOfMyType
var depth = 0
var inferiority = 0
val visited = createMap[ Boolean ]()
inferFromTypes( originalSource, originalTarget )
def isInProcess(source: Type, target: Type) = {
 {
var i = 0
while( (i<depth)) {
 {
 if (((source===sourceStack(i))&&(target===targetStack(i)))) {
 return true

}

}
 (i+= 1)
}
}
return false

}
def inferFromTypes(source: Type, target: Type) = {
 if ((!couldContainTypeParameters( target ))) {
 return

}
if (((((source.flags&TypeFlags.Union)&&(target.flags&TypeFlags.Union))&&(!(((source.flags&TypeFlags.Enum)&&(target.flags&TypeFlags.Enum)))))||((source.flags&TypeFlags.Intersection)&&(target.flags&TypeFlags.Intersection)))) {
 if ((source===target)) {
 ((source.asInstanceOf[UnionOrIntersectionType]).types).foreach { fresh72 =>
val t = zeroOfMyType
 = fresh72
 {
 inferFromTypes( t, t )

}
}
return

}
var matchingTypes: Array[Type] = zeroOfMyType
((source.asInstanceOf[UnionOrIntersectionType]).types).foreach { fresh73 =>
val t = zeroOfMyType
 = fresh73
 {
 if (typeIdenticalToSomeType( t, (target.asInstanceOf[UnionOrIntersectionType]).types )) {
 ((matchingTypes||((matchingTypes=Array())))).push( t )
inferFromTypes( t, t )

}
else if ((t.flags&((TypeFlags.NumberLiteral|TypeFlags.StringLiteral)))) {
 val b = getBaseTypeOfLiteralType( t )
if (typeIdenticalToSomeType( b, (target.asInstanceOf[UnionOrIntersectionType]).types )) {
 ((matchingTypes||((matchingTypes=Array())))).push( t, b )

}

}

}
}
if (matchingTypes) {
 (source=removeTypesFromUnionOrIntersection( source.asInstanceOf[UnionOrIntersectionType], matchingTypes ))
(target=removeTypesFromUnionOrIntersection( target.asInstanceOf[UnionOrIntersectionType], matchingTypes ))

}

}
if ((target.flags&TypeFlags.TypeParameter)) {
 if ((source.flags&TypeFlags.ContainsAnyFunctionType)) {
 return

}
{
var i = 0
while( (i<typeParameters.length)) {
 {
 if ((target===typeParameters(i))) {
 val inferences = context.inferences(i)
if ((!inferences.isFixed)) {
 val candidates = (if (inferiority) (inferences.secondary||((inferences.secondary=Array()))) else (inferences.primary||((inferences.primary=Array()))))
if ((!contains( candidates, source ))) {
 candidates.push( source )

}
if ((!isTypeParameterAtTopLevel( originalTarget, target.asInstanceOf[TypeParameter] ))) {
 (inferences.topLevel=false)

}

}
return

}

}
 (i+= 1)
}
}

}
else if ((((getObjectFlags( source )&ObjectFlags.Reference)&&(getObjectFlags( target )&ObjectFlags.Reference))&&((source.asInstanceOf[TypeReference]).target===(target.asInstanceOf[TypeReference]).target))) {
 val sourceTypes = ((source.asInstanceOf[TypeReference]).typeArguments||emptyArray)
val targetTypes = ((target.asInstanceOf[TypeReference]).typeArguments||emptyArray)
val count = (if ((sourceTypes.length<targetTypes.length)) sourceTypes.length else targetTypes.length)
{
var i = 0
while( (i<count)) {
 {
 inferFromTypes( sourceTypes(i), targetTypes(i) )

}
 (i+= 1)
}
}

}
else if ((target.flags&TypeFlags.UnionOrIntersection)) {
 val targetTypes = (target.asInstanceOf[UnionOrIntersectionType]).types
var typeParameterCount = 0
var typeParameter: TypeParameter = zeroOfMyType
(targetTypes).foreach { fresh74 =>
val t = zeroOfMyType
 = fresh74
 {
 if (((t.flags&TypeFlags.TypeParameter)&&contains( typeParameters, t ))) {
 (typeParameter=t.asInstanceOf[TypeParameter])
(typeParameterCount+= 1)

}
else {
 inferFromTypes( source, t )

}

}
}
if ((typeParameterCount===1)) {
 (inferiority+= 1)
inferFromTypes( source, typeParameter )
(inferiority-= 1)

}

}
else if ((source.flags&TypeFlags.UnionOrIntersection)) {
 val sourceTypes = (source.asInstanceOf[UnionOrIntersectionType]).types
(sourceTypes).foreach { fresh75 =>
val sourceType = zeroOfMyType
 = fresh75
 {
 inferFromTypes( sourceType, target )

}
}

}
else {
 (source=getApparentType( source ))
if ((source.flags&TypeFlags.Object)) {
 if (isInProcess( source, target )) {
 return

}
if ((isDeeplyNestedGeneric( source, sourceStack, depth )&&isDeeplyNestedGeneric( target, targetStack, depth ))) {
 return

}
val key = ((source.id+",")+target.id)
if (visited(key)) {
 return

}
(visited(key)=true)
if ((depth===0)) {
 (sourceStack=Array())
(targetStack=Array())

}
(sourceStack(depth)=source)
(targetStack(depth)=target)
(depth+= 1)
inferFromProperties( source, target )
inferFromSignatures( source, target, SignatureKind.Call )
inferFromSignatures( source, target, SignatureKind.Construct )
inferFromIndexTypes( source, target )
(depth-= 1)

}

}

}
def inferFromProperties(source: Type, target: Type) = {
 val properties = getPropertiesOfObjectType( target )
(properties).foreach { fresh76 =>
val targetProp = zeroOfMyType
 = fresh76
 {
 val sourceProp = getPropertyOfObjectType( source, targetProp.name )
if (sourceProp) {
 inferFromTypes( getTypeOfSymbol( sourceProp ), getTypeOfSymbol( targetProp ) )

}

}
}

}
def inferFromSignatures(source: Type, target: Type, kind: SignatureKind) = {
 val sourceSignatures = getSignaturesOfType( source, kind )
val targetSignatures = getSignaturesOfType( target, kind )
val sourceLen = sourceSignatures.length
val targetLen = targetSignatures.length
val len = (if ((sourceLen<targetLen)) sourceLen else targetLen)
{
var i = 0
while( (i<len)) {
 {
 inferFromSignature( getErasedSignature( sourceSignatures(((sourceLen-len)+i)) ), getErasedSignature( targetSignatures(((targetLen-len)+i)) ) )

}
 (i+= 1)
}
}

}
def inferFromParameterTypes(source: Type, target: Type) = {
 return inferFromTypes( source, target )

}
def inferFromSignature(source: Signature, target: Signature) = {
 forEachMatchingParameterType( source, target, inferFromParameterTypes )
if (((source.typePredicate&&target.typePredicate)&&(source.typePredicate.kind===target.typePredicate.kind))) {
 inferFromTypes( source.typePredicate.`type`, target.typePredicate.`type` )

}
else {
 inferFromTypes( getReturnTypeOfSignature( source ), getReturnTypeOfSignature( target ) )

}

}
def inferFromIndexTypes(source: Type, target: Type) = {
 val targetStringIndexType = getIndexTypeOfType( target, IndexKind.String )
if (targetStringIndexType) {
 val sourceIndexType = (getIndexTypeOfType( source, IndexKind.String )||getImplicitIndexTypeOfType( source, IndexKind.String ))
if (sourceIndexType) {
 inferFromTypes( sourceIndexType, targetStringIndexType )

}

}
val targetNumberIndexType = getIndexTypeOfType( target, IndexKind.Number )
if (targetNumberIndexType) {
 val sourceIndexType = ((getIndexTypeOfType( source, IndexKind.Number )||getIndexTypeOfType( source, IndexKind.String ))||getImplicitIndexTypeOfType( source, IndexKind.Number ))
if (sourceIndexType) {
 inferFromTypes( sourceIndexType, targetNumberIndexType )

}

}

}

}
def typeIdenticalToSomeType(`type`: Type, types: Array[Type]): Boolean = {
 (types).foreach { fresh77 =>
val t = zeroOfMyType
 = fresh77
 {
 if (isTypeIdenticalTo( t, `type` )) {
 return true

}

}
}
return false

}
def removeTypesFromUnionOrIntersection(`type`: UnionOrIntersectionType, typesToRemove: Array[Type]) = {
 val reducedTypes: Array[Type] = Array()
(`type`.types).foreach { fresh78 =>
val t = zeroOfMyType
 = fresh78
 {
 if ((!typeIdenticalToSomeType( t, typesToRemove ))) {
 reducedTypes.push( t )

}

}
}
return (if ((`type`.flags&TypeFlags.Union)) getUnionType( reducedTypes ) else getIntersectionType( reducedTypes ))

}
def getInferenceCandidates(context: InferenceContext, index: Int): Array[Type] = {
 val inferences = context.inferences(index)
return ((inferences.primary||inferences.secondary)||emptyArray)

}
def hasPrimitiveConstraint(`type`: TypeParameter): Boolean = {
 val constraint = getConstraintOfTypeParameter( `type` )
return (constraint&&maybeTypeOfKind( constraint, TypeFlags.Primitive ))

}
def getInferredType(context: InferenceContext, index: Int): Type = {
 var inferredType = context.inferredTypes(index)
var inferenceSucceeded: Boolean = zeroOfMyType
if ((!inferredType)) {
 val inferences = getInferenceCandidates( context, index )
if (inferences.length) {
 val signature = context.signature
val widenLiteralTypes = ((context.inferences(index).topLevel&&(!hasPrimitiveConstraint( signature.typeParameters(index) )))&&((context.inferences(index).isFixed||(!isTypeParameterAtTopLevel( getReturnTypeOfSignature( signature ), signature.typeParameters(index) )))))
val baseInferences = (if (widenLiteralTypes) sameMap( inferences, getWidenedLiteralType ) else inferences)
val unionOrSuperType = (if (context.inferUnionTypes) getUnionType( baseInferences, true ) else getCommonSupertype( baseInferences ))
(inferredType=(if (unionOrSuperType) getWidenedType( unionOrSuperType ) else unknownType))
(inferenceSucceeded=(!(!unionOrSuperType)))

}
else {
 (inferredType=emptyObjectType)
(inferenceSucceeded=true)

}
(context.inferredTypes(index)=inferredType)
if (inferenceSucceeded) {
 val constraint = getConstraintOfTypeParameter( context.signature.typeParameters(index) )
if (constraint) {
 val instantiatedConstraint = instantiateType( constraint, getInferenceMapper( context ) )
if ((!isTypeAssignableTo( inferredType, getTypeWithThisArgument( instantiatedConstraint, inferredType ) ))) {
 (context.inferredTypes(index)=(inferredType=instantiatedConstraint))

}

}

}
else if (((context.failedTypeParameterIndex===undefined)||(context.failedTypeParameterIndex>index))) {
 (context.failedTypeParameterIndex=index)

}

}
return inferredType

}
def getInferredTypes(context: InferenceContext): Array[Type] = {
 {
var i = 0
while( (i<context.inferredTypes.length)) {
 {
 getInferredType( context, i )

}
 (i+= 1)
}
}
return context.inferredTypes

}
def getResolvedSymbol(node: Identifier): Symbol = {
 val links = getNodeLinks( node )
if ((!links.resolvedSymbol)) {
 (links.resolvedSymbol=(((!nodeIsMissing( node ))&&resolveName( node, node.text, (SymbolFlags.Value|SymbolFlags.ExportValue), Diagnostics.Cannot_find_name_0, node ))||unknownSymbol))

}
return links.resolvedSymbol

}
def isInTypeQuery(node: Node): Boolean = {
 while (node) {
{
 node.kind match {
  case  SyntaxKind.TypeQuery  =>
return true
  case  SyntaxKind.Identifier | SyntaxKind.QualifiedName  =>
(node=node.parent)
continue
  case _ =>
return false
}

}
}
Debug.fail( "should not get here" )

}
def getFlowCacheKey(node: Node): String = {
 if ((node.kind===SyntaxKind.Identifier)) {
 val symbol = getResolvedSymbol( node.asInstanceOf[Identifier] )
return (if ((symbol!==unknownSymbol)) (""+getSymbolId( symbol )) else undefined)

}
if ((node.kind===SyntaxKind.ThisKeyword)) {
 return "0"

}
if ((node.kind===SyntaxKind.PropertyAccessExpression)) {
 val key = getFlowCacheKey( (node.asInstanceOf[PropertyAccessExpression]).expression )
return (key&&((key+".")+(node.asInstanceOf[PropertyAccessExpression]).name.text))

}
return undefined

}
def getLeftmostIdentifierOrThis(node: Node): Node = {
 node.kind match {
  case  SyntaxKind.Identifier | SyntaxKind.ThisKeyword  =>
return node
  case  SyntaxKind.PropertyAccessExpression  =>
return getLeftmostIdentifierOrThis( (node.asInstanceOf[PropertyAccessExpression]).expression )
  case _ =>
}
return undefined

}
def isMatchingReference(source: Node, target: Node): Boolean = {
 source.kind match {
  case  SyntaxKind.Identifier  =>
return (((target.kind===SyntaxKind.Identifier)&&(getResolvedSymbol( source.asInstanceOf[Identifier] )===getResolvedSymbol( target.asInstanceOf[Identifier] )))||((((target.kind===SyntaxKind.VariableDeclaration)||(target.kind===SyntaxKind.BindingElement)))&&(getExportSymbolOfValueSymbolIfExported( getResolvedSymbol( source.asInstanceOf[Identifier] ) )===getSymbolOfNode( target ))))
  case  SyntaxKind.ThisKeyword  =>
return (target.kind===SyntaxKind.ThisKeyword)
  case  SyntaxKind.PropertyAccessExpression  =>
return (((target.kind===SyntaxKind.PropertyAccessExpression)&&((source.asInstanceOf[PropertyAccessExpression]).name.text===(target.asInstanceOf[PropertyAccessExpression]).name.text))&&isMatchingReference( (source.asInstanceOf[PropertyAccessExpression]).expression, (target.asInstanceOf[PropertyAccessExpression]).expression ))
  case _ =>
}
return false

}
def containsMatchingReference(source: Node, target: Node) = {
 while ((source.kind===SyntaxKind.PropertyAccessExpression)) {
{
 (source=(source.asInstanceOf[PropertyAccessExpression]).expression)
if (isMatchingReference( source, target )) {
 return true

}

}
}
return false

}
def containsMatchingReferenceDiscriminant(source: Node, target: Node) = {
 return (((target.kind===SyntaxKind.PropertyAccessExpression)&&containsMatchingReference( source, (target.asInstanceOf[PropertyAccessExpression]).expression ))&&isDiscriminantProperty( getDeclaredTypeOfReference( (target.asInstanceOf[PropertyAccessExpression]).expression ), (target.asInstanceOf[PropertyAccessExpression]).name.text ))

}
def getDeclaredTypeOfReference(expr: Node): Type = {
 if ((expr.kind===SyntaxKind.Identifier)) {
 return getTypeOfSymbol( getResolvedSymbol( expr.asInstanceOf[Identifier] ) )

}
if ((expr.kind===SyntaxKind.PropertyAccessExpression)) {
 val `type` = getDeclaredTypeOfReference( (expr.asInstanceOf[PropertyAccessExpression]).expression )
return (`type`&&getTypeOfPropertyOfType( `type`, (expr.asInstanceOf[PropertyAccessExpression]).name.text ))

}
return undefined

}
def isDiscriminantProperty(`type`: Type, name: String) = {
 if ((`type`&&(`type`.flags&TypeFlags.Union))) {
 val prop = getUnionOrIntersectionProperty( `type`.asInstanceOf[UnionType], name )
if ((prop&&(prop.flags&SymbolFlags.SyntheticProperty))) {
 if (((prop.asInstanceOf[TransientSymbol]).isDiscriminantProperty===undefined)) {
 ((prop.asInstanceOf[TransientSymbol]).isDiscriminantProperty=((prop.asInstanceOf[TransientSymbol]).hasNonUniformType&&isLiteralType( getTypeOfSymbol( prop ) )))

}
return (prop.asInstanceOf[TransientSymbol]).isDiscriminantProperty

}

}
return false

}
def isOrContainsMatchingReference(source: Node, target: Node) = {
 return (isMatchingReference( source, target )||containsMatchingReference( source, target ))

}
def hasMatchingArgument(callExpression: CallExpression, reference: Node) = {
 if (callExpression.arguments) {
 (callExpression.arguments).foreach { fresh79 =>
val argument = zeroOfMyType
 = fresh79
 {
 if (isOrContainsMatchingReference( reference, argument )) {
 return true

}

}
}

}
if (((callExpression.expression.kind===SyntaxKind.PropertyAccessExpression)&&isOrContainsMatchingReference( reference, (callExpression.expression.asInstanceOf[PropertyAccessExpression]).expression ))) {
 return true

}
return false

}
def getFlowNodeId(flow: FlowNode): Int = {
 if ((!flow.id)) {
 (flow.id=nextFlowId)
(nextFlowId+= 1)

}
return flow.id

}
def typeMaybeAssignableTo(source: Type, target: Type) = {
 if ((!((source.flags&TypeFlags.Union)))) {
 return isTypeAssignableTo( source, target )

}
((source.asInstanceOf[UnionType]).types).foreach { fresh80 =>
val t = zeroOfMyType
 = fresh80
 {
 if (isTypeAssignableTo( t, target )) {
 return true

}

}
}
return false

}
def getAssignmentReducedType(declaredType: UnionType, assignedType: Type) = {
 if ((declaredType!==assignedType)) {
 if ((assignedType.flags&TypeFlags.Never)) {
 return assignedType

}
val reducedType = filterType( declaredType, (t =>  typeMaybeAssignableTo( assignedType, t )) )
if ((!((reducedType.flags&TypeFlags.Never)))) {
 return reducedType

}

}
return declaredType

}
def getTypeFactsOfTypes(types: Array[Type]): TypeFacts = {
 var result: TypeFacts = TypeFacts.None
(types).foreach { fresh81 =>
val t = zeroOfMyType
 = fresh81
 {
 (result|=getTypeFacts( t ))

}
}
return result

}
def isFunctionObjectType(`type`: ObjectType): Boolean = {
 val resolved = resolveStructuredTypeMembers( `type` )
return (!(!(((resolved.callSignatures.length||resolved.constructSignatures.length)||(resolved.members("bind")&&isTypeSubtypeOf( `type`, globalFunctionType ))))))

}
def getTypeFacts(`type`: Type): TypeFacts = {
 val flags = `type`.flags
if ((flags&TypeFlags.String)) {
 return (if (strictNullChecks) TypeFacts.StringStrictFacts else TypeFacts.StringFacts)

}
if ((flags&TypeFlags.StringLiteral)) {
 return (if (strictNullChecks) (if (((`type`.asInstanceOf[LiteralType]).text==="")) TypeFacts.EmptyStringStrictFacts else TypeFacts.NonEmptyStringStrictFacts) else (if (((`type`.asInstanceOf[LiteralType]).text==="")) TypeFacts.EmptyStringFacts else TypeFacts.NonEmptyStringFacts))

}
if ((flags&((TypeFlags.Number|TypeFlags.Enum)))) {
 return (if (strictNullChecks) TypeFacts.NumberStrictFacts else TypeFacts.NumberFacts)

}
if ((flags&((TypeFlags.NumberLiteral|TypeFlags.EnumLiteral)))) {
 val isZero = ((`type`.asInstanceOf[LiteralType]).text==="0")
return (if (strictNullChecks) (if (isZero) TypeFacts.ZeroStrictFacts else TypeFacts.NonZeroStrictFacts) else (if (isZero) TypeFacts.ZeroFacts else TypeFacts.NonZeroFacts))

}
if ((flags&TypeFlags.Boolean)) {
 return (if (strictNullChecks) TypeFacts.BooleanStrictFacts else TypeFacts.BooleanFacts)

}
if ((flags&TypeFlags.BooleanLike)) {
 return (if (strictNullChecks) (if ((`type`===falseType)) TypeFacts.FalseStrictFacts else TypeFacts.TrueStrictFacts) else (if ((`type`===falseType)) TypeFacts.FalseFacts else TypeFacts.TrueFacts))

}
if ((flags&TypeFlags.Object)) {
 return (if (isFunctionObjectType( `type`.asInstanceOf[ObjectType] )) (if (strictNullChecks) TypeFacts.FunctionStrictFacts else TypeFacts.FunctionFacts) else (if (strictNullChecks) TypeFacts.ObjectStrictFacts else TypeFacts.ObjectFacts))

}
if ((flags&((TypeFlags.Void|TypeFlags.Undefined)))) {
 return TypeFacts.UndefinedFacts

}
if ((flags&TypeFlags.Null)) {
 return TypeFacts.NullFacts

}
if ((flags&TypeFlags.ESSymbol)) {
 return (if (strictNullChecks) TypeFacts.SymbolStrictFacts else TypeFacts.SymbolFacts)

}
if ((flags&TypeFlags.TypeParameter)) {
 val constraint = getConstraintOfTypeParameter( `type`.asInstanceOf[TypeParameter] )
return getTypeFacts( (constraint||emptyObjectType) )

}
if ((flags&TypeFlags.UnionOrIntersection)) {
 return getTypeFactsOfTypes( (`type`.asInstanceOf[UnionOrIntersectionType]).types )

}
return TypeFacts.All

}
def getTypeWithFacts(`type`: Type, include: TypeFacts) = {
 return filterType( `type`, (t =>  (((getTypeFacts( t )&include))!==0)) )

}
def getTypeWithDefault(`type`: Type, defaultExpression: Expression) = {
 if (defaultExpression) {
 val defaultType = checkExpression( defaultExpression )
return getUnionType( Array( getTypeWithFacts( `type`, TypeFacts.NEUndefined ), defaultType ) )

}
return `type`

}
def getTypeOfDestructuredProperty(`type`: Type, name: ( Identifier | LiteralExpression | ComputedPropertyName )) = {
 val text = getTextOfPropertyName( name )
return (((getTypeOfPropertyOfType( `type`, text )||(isNumericLiteralName( text )&&getIndexTypeOfType( `type`, IndexKind.Number )))||getIndexTypeOfType( `type`, IndexKind.String ))||unknownType)

}
def getTypeOfDestructuredArrayElement(`type`: Type, index: Int) = {
 return (((isTupleLikeType( `type` )&&getTypeOfPropertyOfType( `type`, (""+index) ))||checkIteratedTypeOrElementType( `type`, undefined, false ))||unknownType)

}
def getTypeOfDestructuredSpreadElement(`type`: Type) = {
 return createArrayType( (checkIteratedTypeOrElementType( `type`, undefined, false )||unknownType) )

}
def getAssignedTypeOfBinaryExpression(node: BinaryExpression): Type = {
 return (if (((node.parent.kind===SyntaxKind.ArrayLiteralExpression)||(node.parent.kind===SyntaxKind.PropertyAssignment))) getTypeWithDefault( getAssignedType( node ), node.right ) else checkExpression( node.right ))

}
def getAssignedTypeOfArrayLiteralElement(node: ArrayLiteralExpression, element: Expression): Type = {
 return getTypeOfDestructuredArrayElement( getAssignedType( node ), indexOf( node.elements, element ) )

}
def getAssignedTypeOfSpreadElement(node: SpreadElementExpression): Type = {
 return getTypeOfDestructuredSpreadElement( getAssignedType( node.parent.asInstanceOf[ArrayLiteralExpression] ) )

}
def getAssignedTypeOfPropertyAssignment(node: ( PropertyAssignment | ShorthandPropertyAssignment )): Type = {
 return getTypeOfDestructuredProperty( getAssignedType( node.parent.asInstanceOf[ObjectLiteralExpression] ), node.name )

}
def getAssignedTypeOfShorthandPropertyAssignment(node: ShorthandPropertyAssignment): Type = {
 return getTypeWithDefault( getAssignedTypeOfPropertyAssignment( node ), node.objectAssignmentInitializer )

}
def getAssignedType(node: Expression): Type = {
 val parent = node.parent
parent.kind match {
  case  SyntaxKind.ForInStatement  =>
return stringType
  case  SyntaxKind.ForOfStatement  =>
return (checkRightHandSideOfForOf( (parent.asInstanceOf[ForOfStatement]).expression )||unknownType)
  case  SyntaxKind.BinaryExpression  =>
return getAssignedTypeOfBinaryExpression( parent.asInstanceOf[BinaryExpression] )
  case  SyntaxKind.DeleteExpression  =>
return undefinedType
  case  SyntaxKind.ArrayLiteralExpression  =>
return getAssignedTypeOfArrayLiteralElement( parent.asInstanceOf[ArrayLiteralExpression], node )
  case  SyntaxKind.SpreadElementExpression  =>
return getAssignedTypeOfSpreadElement( parent.asInstanceOf[SpreadElementExpression] )
  case  SyntaxKind.PropertyAssignment  =>
return getAssignedTypeOfPropertyAssignment( parent.asInstanceOf[PropertyAssignment] )
  case  SyntaxKind.ShorthandPropertyAssignment  =>
return getAssignedTypeOfShorthandPropertyAssignment( parent.asInstanceOf[ShorthandPropertyAssignment] )
  case _ =>
}
return unknownType

}
def getInitialTypeOfBindingElement(node: BindingElement): Type = {
 val pattern = node.parent.asInstanceOf[BindingPattern]
val parentType = getInitialType( pattern.parent.asInstanceOf[( VariableDeclaration | BindingElement )] )
val `type` = (if ((pattern.kind===SyntaxKind.ObjectBindingPattern)) getTypeOfDestructuredProperty( parentType, (node.propertyName||node.name.asInstanceOf[Identifier]) ) else (if ((!node.dotDotDotToken)) getTypeOfDestructuredArrayElement( parentType, indexOf( pattern.elements, node ) ) else getTypeOfDestructuredSpreadElement( parentType )))
return getTypeWithDefault( `type`, node.initializer )

}
def getTypeOfInitializer(node: Expression) = {
 val links = getNodeLinks( node )
return (links.resolvedType||checkExpression( node ))

}
def getInitialTypeOfVariableDeclaration(node: VariableDeclaration) = {
 if (node.initializer) {
 return getTypeOfInitializer( node.initializer )

}
if ((node.parent.parent.kind===SyntaxKind.ForInStatement)) {
 return stringType

}
if ((node.parent.parent.kind===SyntaxKind.ForOfStatement)) {
 return (checkRightHandSideOfForOf( (node.parent.parent.asInstanceOf[ForOfStatement]).expression )||unknownType)

}
return unknownType

}
def getInitialType(node: ( VariableDeclaration | BindingElement )) = {
 return (if ((node.kind===SyntaxKind.VariableDeclaration)) getInitialTypeOfVariableDeclaration( node.asInstanceOf[VariableDeclaration] ) else getInitialTypeOfBindingElement( node.asInstanceOf[BindingElement] ))

}
def getInitialOrAssignedType(node: ( VariableDeclaration | BindingElement | Expression )) = {
 return (if (((node.kind===SyntaxKind.VariableDeclaration)||(node.kind===SyntaxKind.BindingElement))) getInitialType( node.asInstanceOf[( VariableDeclaration | BindingElement )] ) else getAssignedType( node.asInstanceOf[Expression] ))

}
def isEmptyArrayAssignment(node: ( VariableDeclaration | BindingElement | Expression )) = {
 return ((((node.kind===SyntaxKind.VariableDeclaration)&&(node.asInstanceOf[VariableDeclaration]).initializer)&&isEmptyArrayLiteral( (node.asInstanceOf[VariableDeclaration]).initializer ))||(((node.kind!==SyntaxKind.BindingElement)&&(node.parent.kind===SyntaxKind.BinaryExpression))&&isEmptyArrayLiteral( (node.parent.asInstanceOf[BinaryExpression]).right )))

}
def getReferenceCandidate(node: Expression): Expression = {
 node.kind match {
  case  SyntaxKind.ParenthesizedExpression  =>
return getReferenceCandidate( (node.asInstanceOf[ParenthesizedExpression]).expression )
  case  SyntaxKind.BinaryExpression  =>
(node.asInstanceOf[BinaryExpression]).operatorToken.kind match {
  case  SyntaxKind.EqualsToken  =>
return getReferenceCandidate( (node.asInstanceOf[BinaryExpression]).left )
  case  SyntaxKind.CommaToken  =>
return getReferenceCandidate( (node.asInstanceOf[BinaryExpression]).right )
  case _ =>
}
  case _ =>
}
return node

}
def getReferenceRoot(node: Node): Node = {
 val parent = node.parent
return (if ((((parent.kind===SyntaxKind.ParenthesizedExpression)||(((parent.kind===SyntaxKind.BinaryExpression)&&((parent.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.EqualsToken))&&((parent.asInstanceOf[BinaryExpression]).left===node)))||(((parent.kind===SyntaxKind.BinaryExpression)&&((parent.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.CommaToken))&&((parent.asInstanceOf[BinaryExpression]).right===node)))) getReferenceRoot( parent ) else node)

}
def getTypeOfSwitchClause(clause: ( CaseClause | DefaultClause )) = {
 if ((clause.kind===SyntaxKind.CaseClause)) {
 val caseType = getRegularTypeOfLiteralType( checkExpression( (clause.asInstanceOf[CaseClause]).expression ) )
return (if (isUnitType( caseType )) caseType else undefined)

}
return neverType

}
def getSwitchClauseTypes(switchStatement: SwitchStatement): Array[Type] = {
 val links = getNodeLinks( switchStatement )
if ((!links.switchTypes)) {
 val types = map( switchStatement.caseBlock.clauses, getTypeOfSwitchClause )
(links.switchTypes=(if ((!contains( types, undefined ))) types else emptyArray))

}
return links.switchTypes

}
def eachTypeContainedIn(source: Type, types: Array[Type]) = {
 return (if ((source.flags&TypeFlags.Union)) (!forEach( (source.asInstanceOf[UnionType]).types, (t =>  (!contains( types, t ))) )) else contains( types, source ))

}
def isTypeSubsetOf(source: Type, target: Type) = {
 return ((source===target)||((target.flags&TypeFlags.Union)&&isTypeSubsetOfUnion( source, target.asInstanceOf[UnionType] )))

}
def isTypeSubsetOfUnion(source: Type, target: UnionType) = {
 if ((source.flags&TypeFlags.Union)) {
 ((source.asInstanceOf[UnionType]).types).foreach { fresh82 =>
val t = zeroOfMyType
 = fresh82
 {
 if ((!containsType( target.types, t ))) {
 return false

}

}
}
return true

}
if ((((source.flags&TypeFlags.EnumLiteral)&&(target.flags&TypeFlags.Enum))&&((source.asInstanceOf[EnumLiteralType]).baseType===target))) {
 return true

}
return containsType( target.types, source )

}
def filterType(`type`: Type, f: ((Type) => Boolean)): Type = {
 if ((`type`.flags&TypeFlags.Union)) {
 val types = (`type`.asInstanceOf[UnionType]).types
val filtered = filter( types, f )
return (if ((filtered===types)) `type` else getUnionTypeFromSortedList( filtered ))

}
return (if (f( `type` )) `type` else neverType)

}
def mapType(`type`: Type, f: ((Type) => Type)): Type = {
 return (if ((`type`.flags&TypeFlags.Union)) getUnionType( map( (`type`.asInstanceOf[UnionType]).types, f ) ) else f( `type` ))

}
def extractTypesOfKind(`type`: Type, kind: TypeFlags) = {
 return filterType( `type`, (t =>  (((t.flags&kind))!==0)) )

}
def replacePrimitivesWithLiterals(typeWithPrimitives: Type, typeWithLiterals: Type) = {
 if (((isTypeSubsetOf( stringType, typeWithPrimitives )&&maybeTypeOfKind( typeWithLiterals, TypeFlags.StringLiteral ))||(isTypeSubsetOf( numberType, typeWithPrimitives )&&maybeTypeOfKind( typeWithLiterals, TypeFlags.NumberLiteral )))) {
 return mapType( typeWithPrimitives, (t =>  (if ((t.flags&TypeFlags.String)) extractTypesOfKind( typeWithLiterals, (TypeFlags.String|TypeFlags.StringLiteral) ) else (if ((t.flags&TypeFlags.Number)) extractTypesOfKind( typeWithLiterals, (TypeFlags.Number|TypeFlags.NumberLiteral) ) else t))) )

}
return typeWithPrimitives

}
def isIncomplete(flowType: FlowType) = {
 return (flowType.flags===0)

}
def getTypeFromFlowType(flowType: FlowType) = {
 return (if ((flowType.flags===0)) (flowType.asInstanceOf[IncompleteType]).`type` else flowType.asInstanceOf[Type])

}
def createFlowType(`type`: Type, incomplete: Boolean): FlowType = {
 return (if (incomplete) Map( "flags" -> 0,
"type" -> `type` ) else `type`)

}
def createEvolvingArrayType(elementType: Type): EvolvingArrayType = {
 val result = createObjectType( ObjectFlags.EvolvingArray ).asInstanceOf[EvolvingArrayType]
(result.elementType=elementType)
return result

}
def getEvolvingArrayType(elementType: Type): EvolvingArrayType = {
 return (evolvingArrayTypes(elementType.id)||((evolvingArrayTypes(elementType.id)=createEvolvingArrayType( elementType ))))

}
def addEvolvingArrayElementType(evolvingArrayType: EvolvingArrayType, node: Expression): EvolvingArrayType = {
 val elementType = getBaseTypeOfLiteralType( checkExpression( node ) )
return (if (isTypeSubsetOf( elementType, evolvingArrayType.elementType )) evolvingArrayType else getEvolvingArrayType( getUnionType( Array( evolvingArrayType.elementType, elementType ) ) ))

}
def createFinalArrayType(elementType: Type) = {
 return (if ((elementType.flags&TypeFlags.Never)) autoArrayType else createArrayType( (if ((elementType.flags&TypeFlags.Union)) getUnionType( (elementType.asInstanceOf[UnionType]).types, true ) else elementType) ))

}
def getFinalArrayType(evolvingArrayType: EvolvingArrayType): Type = {
 return (evolvingArrayType.finalArrayType||((evolvingArrayType.finalArrayType=createFinalArrayType( evolvingArrayType.elementType ))))

}
def finalizeEvolvingArrayType(`type`: Type): Type = {
 return (if ((getObjectFlags( `type` )&ObjectFlags.EvolvingArray)) getFinalArrayType( `type`.asInstanceOf[EvolvingArrayType] ) else `type`)

}
def getElementTypeOfEvolvingArrayType(`type`: Type) = {
 return (if ((getObjectFlags( `type` )&ObjectFlags.EvolvingArray)) (`type`.asInstanceOf[EvolvingArrayType]).elementType else neverType)

}
def isEvolvingArrayTypeList(types: Array[Type]) = {
 var hasEvolvingArrayType = false
(types).foreach { fresh83 =>
val t = zeroOfMyType
 = fresh83
 {
 if ((!((t.flags&TypeFlags.Never)))) {
 if ((!((getObjectFlags( t )&ObjectFlags.EvolvingArray)))) {
 return false

}
(hasEvolvingArrayType=true)

}

}
}
return hasEvolvingArrayType

}
def getUnionOrEvolvingArrayType(types: Array[Type], subtypeReduction: Boolean) = {
 return (if (isEvolvingArrayTypeList( types )) getEvolvingArrayType( getUnionType( map( types, getElementTypeOfEvolvingArrayType ) ) ) else getUnionType( sameMap( types, finalizeEvolvingArrayType ), subtypeReduction ))

}
def isEvolvingArrayOperationTarget(node: Node) = {
 val root = getReferenceRoot( node )
val parent = root.parent
val isLengthPushOrUnshift = ((parent.kind===SyntaxKind.PropertyAccessExpression)&&((((parent.asInstanceOf[PropertyAccessExpression]).name.text==="length")||((parent.parent.kind===SyntaxKind.CallExpression)&&isPushOrUnshiftIdentifier( (parent.asInstanceOf[PropertyAccessExpression]).name )))))
val isElementAssignment = (((((((parent.kind===SyntaxKind.ElementAccessExpression)&&((parent.asInstanceOf[ElementAccessExpression]).expression===root))&&(parent.parent.kind===SyntaxKind.BinaryExpression))&&((parent.parent.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.EqualsToken))&&((parent.parent.asInstanceOf[BinaryExpression]).left===parent))&&(!isAssignmentTarget( parent.parent )))&&isTypeAnyOrAllConstituentTypesHaveKind( checkExpression( (parent.asInstanceOf[ElementAccessExpression]).argumentExpression ), (TypeFlags.NumberLike|TypeFlags.Undefined) ))
return (isLengthPushOrUnshift||isElementAssignment)

}
def getFlowTypeOfReference(reference: Node, declaredType: Type, assumeInitialized: Boolean, flowContainer: Node) = {
 var key: String = zeroOfMyType
if (((!reference.flowNode)||(assumeInitialized&&(!((declaredType.flags&TypeFlags.Narrowable)))))) {
 return declaredType

}
val initialType = (if (assumeInitialized) declaredType else (if (((declaredType===autoType)||(declaredType===autoArrayType))) undefinedType else includeFalsyTypes( declaredType, TypeFlags.Undefined )))
val visitedFlowStart = visitedFlowCount
val evolvedType = getTypeFromFlowType( getTypeAtFlowNode( reference.flowNode ) )
(visitedFlowCount=visitedFlowStart)
val resultType = (if (((getObjectFlags( evolvedType )&ObjectFlags.EvolvingArray)&&isEvolvingArrayOperationTarget( reference ))) anyArrayType else finalizeEvolvingArrayType( evolvedType ))
if (((reference.parent.kind===SyntaxKind.NonNullExpression)&&(getTypeWithFacts( resultType, TypeFacts.NEUndefinedOrNull ).flags&TypeFlags.Never))) {
 return declaredType

}
return resultType
def getTypeAtFlowNode(flow: FlowNode): FlowType = {
 while (true) {
{
 if ((flow.flags&FlowFlags.Shared)) {
 {
var i = visitedFlowStart
while( (i<visitedFlowCount)) {
 {
 if ((visitedFlowNodes(i)===flow)) {
 return visitedFlowTypes(i)

}

}
 (i+= 1)
}
}

}
var `type`: FlowType = zeroOfMyType
if ((flow.flags&FlowFlags.Assignment)) {
 (`type`=getTypeAtFlowAssignment( flow.asInstanceOf[FlowAssignment] ))
if ((!`type`)) {
 (flow=(flow.asInstanceOf[FlowAssignment]).antecedent)
continue

}

}
else if ((flow.flags&FlowFlags.Condition)) {
 (`type`=getTypeAtFlowCondition( flow.asInstanceOf[FlowCondition] ))

}
else if ((flow.flags&FlowFlags.SwitchClause)) {
 (`type`=getTypeAtSwitchClause( flow.asInstanceOf[FlowSwitchClause] ))

}
else if ((flow.flags&FlowFlags.Label)) {
 if (((flow.asInstanceOf[FlowLabel]).antecedents.length===1)) {
 (flow=(flow.asInstanceOf[FlowLabel]).antecedents(0))
continue

}
(`type`=(if ((flow.flags&FlowFlags.BranchLabel)) getTypeAtFlowBranchLabel( flow.asInstanceOf[FlowLabel] ) else getTypeAtFlowLoopLabel( flow.asInstanceOf[FlowLabel] )))

}
else if ((flow.flags&FlowFlags.ArrayMutation)) {
 (`type`=getTypeAtFlowArrayMutation( flow.asInstanceOf[FlowArrayMutation] ))
if ((!`type`)) {
 (flow=(flow.asInstanceOf[FlowArrayMutation]).antecedent)
continue

}

}
else if ((flow.flags&FlowFlags.Start)) {
 val container = (flow.asInstanceOf[FlowStart]).container
if (((container&&(container!==flowContainer))&&(reference.kind!==SyntaxKind.PropertyAccessExpression))) {
 (flow=container.flowNode)
continue

}
(`type`=initialType)

}
else {
 (`type`=convertAutoToAny( declaredType ))

}
if ((flow.flags&FlowFlags.Shared)) {
 (visitedFlowNodes(visitedFlowCount)=flow)
(visitedFlowTypes(visitedFlowCount)=`type`)
(visitedFlowCount+= 1)

}
return `type`

}
}

}
def getTypeAtFlowAssignment(flow: FlowAssignment) = {
 val node = flow.node
if (isMatchingReference( reference, node )) {
 if (((node.parent.kind===SyntaxKind.PrefixUnaryExpression)||(node.parent.kind===SyntaxKind.PostfixUnaryExpression))) {
 val flowType = getTypeAtFlowNode( flow.antecedent )
return createFlowType( getBaseTypeOfLiteralType( getTypeFromFlowType( flowType ) ), isIncomplete( flowType ) )

}
if (((declaredType===autoType)||(declaredType===autoArrayType))) {
 if (isEmptyArrayAssignment( node )) {
 return getEvolvingArrayType( neverType )

}
val assignedType = getBaseTypeOfLiteralType( getInitialOrAssignedType( node ) )
return (if (isTypeAssignableTo( assignedType, declaredType )) assignedType else anyArrayType)

}
if ((declaredType.flags&TypeFlags.Union)) {
 return getAssignmentReducedType( declaredType.asInstanceOf[UnionType], getInitialOrAssignedType( node ) )

}
return declaredType

}
if (containsMatchingReference( reference, node )) {
 return declaredType

}
return undefined

}
def getTypeAtFlowArrayMutation(flow: FlowArrayMutation): FlowType = {
 val node = flow.node
val expr = (if ((node.kind===SyntaxKind.CallExpression)) ((node.asInstanceOf[CallExpression]).expression.asInstanceOf[PropertyAccessExpression]).expression else ((node.asInstanceOf[BinaryExpression]).left.asInstanceOf[ElementAccessExpression]).expression)
if (isMatchingReference( reference, getReferenceCandidate( expr ) )) {
 val flowType = getTypeAtFlowNode( flow.antecedent )
val `type` = getTypeFromFlowType( flowType )
if ((getObjectFlags( `type` )&ObjectFlags.EvolvingArray)) {
 var evolvedType = `type`.asInstanceOf[EvolvingArrayType]
if ((node.kind===SyntaxKind.CallExpression)) {
 ((node.asInstanceOf[CallExpression]).arguments).foreach { fresh84 =>
val arg = zeroOfMyType
 = fresh84
 {
 (evolvedType=addEvolvingArrayElementType( evolvedType, arg ))

}
}

}
else {
 val indexType = checkExpression( ((node.asInstanceOf[BinaryExpression]).left.asInstanceOf[ElementAccessExpression]).argumentExpression )
if (isTypeAnyOrAllConstituentTypesHaveKind( indexType, (TypeFlags.NumberLike|TypeFlags.Undefined) )) {
 (evolvedType=addEvolvingArrayElementType( evolvedType, (node.asInstanceOf[BinaryExpression]).right ))

}

}
return (if ((evolvedType===`type`)) flowType else createFlowType( evolvedType, isIncomplete( flowType ) ))

}
return flowType

}
return undefined

}
def getTypeAtFlowCondition(flow: FlowCondition): FlowType = {
 val flowType = getTypeAtFlowNode( flow.antecedent )
val `type` = getTypeFromFlowType( flowType )
if ((`type`.flags&TypeFlags.Never)) {
 return flowType

}
val assumeTrue = (((flow.flags&FlowFlags.TrueCondition))!==0)
val nonEvolvingType = finalizeEvolvingArrayType( `type` )
val narrowedType = narrowType( nonEvolvingType, flow.expression, assumeTrue )
if ((narrowedType===nonEvolvingType)) {
 return flowType

}
val incomplete = isIncomplete( flowType )
val resultType = (if ((incomplete&&(narrowedType.flags&TypeFlags.Never))) silentNeverType else narrowedType)
return createFlowType( resultType, incomplete )

}
def getTypeAtSwitchClause(flow: FlowSwitchClause): FlowType = {
 val flowType = getTypeAtFlowNode( flow.antecedent )
var `type` = getTypeFromFlowType( flowType )
val expr = flow.switchStatement.expression
if (isMatchingReference( reference, expr )) {
 (`type`=narrowTypeBySwitchOnDiscriminant( `type`, flow.switchStatement, flow.clauseStart, flow.clauseEnd ))

}
else if (isMatchingReferenceDiscriminant( expr )) {
 (`type`=narrowTypeByDiscriminant( `type`, expr.asInstanceOf[PropertyAccessExpression], (t =>  narrowTypeBySwitchOnDiscriminant( t, flow.switchStatement, flow.clauseStart, flow.clauseEnd )) ))

}
return createFlowType( `type`, isIncomplete( flowType ) )

}
def getTypeAtFlowBranchLabel(flow: FlowLabel): FlowType = {
 val antecedentTypes: Array[Type] = Array()
var subtypeReduction = false
var seenIncomplete = false
(flow.antecedents).foreach { fresh85 =>
val antecedent = zeroOfMyType
 = fresh85
 {
 val flowType = getTypeAtFlowNode( antecedent )
val `type` = getTypeFromFlowType( flowType )
if (((`type`===declaredType)&&(declaredType===initialType))) {
 return `type`

}
if ((!contains( antecedentTypes, `type` ))) {
 antecedentTypes.push( `type` )

}
if ((!isTypeSubsetOf( `type`, declaredType ))) {
 (subtypeReduction=true)

}
if (isIncomplete( flowType )) {
 (seenIncomplete=true)

}

}
}
return createFlowType( getUnionOrEvolvingArrayType( antecedentTypes, subtypeReduction ), seenIncomplete )

}
def getTypeAtFlowLoopLabel(flow: FlowLabel): FlowType = {
 val id = getFlowNodeId( flow )
val cache = (flowLoopCaches(id)||((flowLoopCaches(id)=createMap[ Type ]())))
if ((!key)) {
 (key=getFlowCacheKey( reference ))

}
if (cache(key)) {
 return cache(key)

}
{
var i = flowLoopStart
while( (i<flowLoopCount)) {
 {
 if ((((flowLoopNodes(i)===flow)&&(flowLoopKeys(i)===key))&&flowLoopTypes(i).length)) {
 return createFlowType( getUnionOrEvolvingArrayType( flowLoopTypes(i), false ), true )

}

}
 (i+= 1)
}
}
val antecedentTypes: Array[Type] = Array()
var subtypeReduction = false
var firstAntecedentType: FlowType = zeroOfMyType
(flowLoopNodes(flowLoopCount)=flow)
(flowLoopKeys(flowLoopCount)=key)
(flowLoopTypes(flowLoopCount)=antecedentTypes)
(flow.antecedents).foreach { fresh86 =>
val antecedent = zeroOfMyType
 = fresh86
 {
 (flowLoopCount+= 1)
val flowType = getTypeAtFlowNode( antecedent )
(flowLoopCount-= 1)
if ((!firstAntecedentType)) {
 (firstAntecedentType=flowType)

}
val `type` = getTypeFromFlowType( flowType )
if (cache(key)) {
 return cache(key)

}
if ((!contains( antecedentTypes, `type` ))) {
 antecedentTypes.push( `type` )

}
if ((!isTypeSubsetOf( `type`, declaredType ))) {
 (subtypeReduction=true)

}
if ((`type`===declaredType)) {
 break()

}

}
}
val result = getUnionOrEvolvingArrayType( antecedentTypes, subtypeReduction )
if (isIncomplete( firstAntecedentType )) {
 return createFlowType( result, true )

}
return (cache(key)=result)

}
def isMatchingReferenceDiscriminant(expr: Expression) = {
 return ((((expr.kind===SyntaxKind.PropertyAccessExpression)&&(declaredType.flags&TypeFlags.Union))&&isMatchingReference( reference, (expr.asInstanceOf[PropertyAccessExpression]).expression ))&&isDiscriminantProperty( declaredType, (expr.asInstanceOf[PropertyAccessExpression]).name.text ))

}
def narrowTypeByDiscriminant(`type`: Type, propAccess: PropertyAccessExpression, narrowType: ((Type) => Type)): Type = {
 val propName = propAccess.name.text
val propType = getTypeOfPropertyOfType( `type`, propName )
val narrowedPropType = (propType&&narrowType( propType ))
return (if ((propType===narrowedPropType)) `type` else filterType( `type`, (t =>  isTypeComparableTo( getTypeOfPropertyOfType( t, propName ), narrowedPropType )) ))

}
def narrowTypeByTruthiness(`type`: Type, expr: Expression, assumeTrue: Boolean): Type = {
 if (isMatchingReference( reference, expr )) {
 return getTypeWithFacts( `type`, (if (assumeTrue) TypeFacts.Truthy else TypeFacts.Falsy) )

}
if (isMatchingReferenceDiscriminant( expr )) {
 return narrowTypeByDiscriminant( `type`, expr.asInstanceOf[PropertyAccessExpression], (t =>  getTypeWithFacts( t, (if (assumeTrue) TypeFacts.Truthy else TypeFacts.Falsy) )) )

}
if (containsMatchingReferenceDiscriminant( reference, expr )) {
 return declaredType

}
return `type`

}
def narrowTypeByBinaryExpression(`type`: Type, expr: BinaryExpression, assumeTrue: Boolean): Type = {
 expr.operatorToken.kind match {
  case  SyntaxKind.EqualsToken  =>
return narrowTypeByTruthiness( `type`, expr.left, assumeTrue )
  case  SyntaxKind.EqualsEqualsToken | SyntaxKind.ExclamationEqualsToken | SyntaxKind.EqualsEqualsEqualsToken | SyntaxKind.ExclamationEqualsEqualsToken  =>
val operator = expr.operatorToken.kind
val left = getReferenceCandidate( expr.left )
val right = getReferenceCandidate( expr.right )
if (((left.kind===SyntaxKind.TypeOfExpression)&&(right.kind===SyntaxKind.StringLiteral))) {
 return narrowTypeByTypeof( `type`, left.asInstanceOf[TypeOfExpression], operator, right.asInstanceOf[LiteralExpression], assumeTrue )

}
if (((right.kind===SyntaxKind.TypeOfExpression)&&(left.kind===SyntaxKind.StringLiteral))) {
 return narrowTypeByTypeof( `type`, right.asInstanceOf[TypeOfExpression], operator, left.asInstanceOf[LiteralExpression], assumeTrue )

}
if (isMatchingReference( reference, left )) {
 return narrowTypeByEquality( `type`, operator, right, assumeTrue )

}
if (isMatchingReference( reference, right )) {
 return narrowTypeByEquality( `type`, operator, left, assumeTrue )

}
if (isMatchingReferenceDiscriminant( left )) {
 return narrowTypeByDiscriminant( `type`, left.asInstanceOf[PropertyAccessExpression], (t =>  narrowTypeByEquality( t, operator, right, assumeTrue )) )

}
if (isMatchingReferenceDiscriminant( right )) {
 return narrowTypeByDiscriminant( `type`, right.asInstanceOf[PropertyAccessExpression], (t =>  narrowTypeByEquality( t, operator, left, assumeTrue )) )

}
if ((containsMatchingReferenceDiscriminant( reference, left )||containsMatchingReferenceDiscriminant( reference, right ))) {
 return declaredType

}
  case  SyntaxKind.InstanceOfKeyword  =>
return narrowTypeByInstanceof( `type`, expr, assumeTrue )
  case  SyntaxKind.CommaToken  =>
return narrowType( `type`, expr.right, assumeTrue )
  case _ =>
}
return `type`

}
def narrowTypeByEquality(`type`: Type, operator: SyntaxKind, value: Expression, assumeTrue: Boolean): Type = {
 if ((`type`.flags&TypeFlags.Any)) {
 return `type`

}
if (((operator===SyntaxKind.ExclamationEqualsToken)||(operator===SyntaxKind.ExclamationEqualsEqualsToken))) {
 (assumeTrue=(!assumeTrue))

}
val valueType = checkExpression( value )
if ((valueType.flags&TypeFlags.Nullable)) {
 if ((!strictNullChecks)) {
 return `type`

}
val doubleEquals = ((operator===SyntaxKind.EqualsEqualsToken)||(operator===SyntaxKind.ExclamationEqualsToken))
val facts = (if (doubleEquals) (if (assumeTrue) TypeFacts.EQUndefinedOrNull else TypeFacts.NEUndefinedOrNull) else (if ((value.kind===SyntaxKind.NullKeyword)) (if (assumeTrue) TypeFacts.EQNull else TypeFacts.NENull) else (if (assumeTrue) TypeFacts.EQUndefined else TypeFacts.NEUndefined)))
return getTypeWithFacts( `type`, facts )

}
if ((`type`.flags&TypeFlags.NotUnionOrUnit)) {
 return `type`

}
if (assumeTrue) {
 val narrowedType = filterType( `type`, (t =>  areTypesComparable( t, valueType )) )
return (if ((narrowedType.flags&TypeFlags.Never)) `type` else replacePrimitivesWithLiterals( narrowedType, valueType ))

}
if (isUnitType( valueType )) {
 val regularType = getRegularTypeOfLiteralType( valueType )
return filterType( `type`, (t =>  (getRegularTypeOfLiteralType( t )!==regularType)) )

}
return `type`

}
def narrowTypeByTypeof(`type`: Type, typeOfExpr: TypeOfExpression, operator: SyntaxKind, literal: LiteralExpression, assumeTrue: Boolean): Type = {
 val target = getReferenceCandidate( typeOfExpr.expression )
if ((!isMatchingReference( reference, target ))) {
 if (containsMatchingReference( reference, target )) {
 return declaredType

}
return `type`

}
if (((operator===SyntaxKind.ExclamationEqualsToken)||(operator===SyntaxKind.ExclamationEqualsEqualsToken))) {
 (assumeTrue=(!assumeTrue))

}
if ((assumeTrue&&(!((`type`.flags&TypeFlags.Union))))) {
 val targetType = typeofTypesByName(literal.text)
if ((targetType&&isTypeSubtypeOf( targetType, `type` ))) {
 return targetType

}

}
val facts = (if (assumeTrue) (typeofEQFacts(literal.text)||TypeFacts.TypeofEQHostObject) else (typeofNEFacts(literal.text)||TypeFacts.TypeofNEHostObject))
return getTypeWithFacts( `type`, facts )

}
def narrowTypeBySwitchOnDiscriminant(`type`: Type, switchStatement: SwitchStatement, clauseStart: Int, clauseEnd: Int) = {
 val switchTypes = getSwitchClauseTypes( switchStatement )
if ((!switchTypes.length)) {
 return `type`

}
val clauseTypes = switchTypes.slice( clauseStart, clauseEnd )
val hasDefaultClause = ((clauseStart===clauseEnd)||contains( clauseTypes, neverType ))
val discriminantType = getUnionType( clauseTypes )
val caseType = (if ((discriminantType.flags&TypeFlags.Never)) neverType else replacePrimitivesWithLiterals( filterType( `type`, (t =>  isTypeComparableTo( discriminantType, t )) ), discriminantType ))
if ((!hasDefaultClause)) {
 return caseType

}
val defaultType = filterType( `type`, (t =>  (!((isUnitType( t )&&contains( switchTypes, getRegularTypeOfLiteralType( t ) ))))) )
return (if ((caseType.flags&TypeFlags.Never)) defaultType else getUnionType( Array( caseType, defaultType ) ))

}
def narrowTypeByInstanceof(`type`: Type, expr: BinaryExpression, assumeTrue: Boolean): Type = {
 val left = getReferenceCandidate( expr.left )
if ((!isMatchingReference( reference, left ))) {
 if (containsMatchingReference( reference, left )) {
 return declaredType

}
return `type`

}
val rightType = checkExpression( expr.right )
if ((!isTypeSubtypeOf( rightType, globalFunctionType ))) {
 return `type`

}
var targetType: Type = zeroOfMyType
val prototypeProperty = getPropertyOfType( rightType, "prototype" )
if (prototypeProperty) {
 val prototypePropertyType = getTypeOfSymbol( prototypeProperty )
if ((!isTypeAny( prototypePropertyType ))) {
 (targetType=prototypePropertyType)

}

}
if ((isTypeAny( `type` )&&(((targetType===globalObjectType)||(targetType===globalFunctionType))))) {
 return `type`

}
if ((!targetType)) {
 var constructSignatures: Array[Signature] = zeroOfMyType
if ((getObjectFlags( rightType )&ObjectFlags.Interface)) {
 (constructSignatures=resolveDeclaredMembers( rightType.asInstanceOf[InterfaceType] ).declaredConstructSignatures)

}
else if ((getObjectFlags( rightType )&ObjectFlags.Anonymous)) {
 (constructSignatures=getSignaturesOfType( rightType, SignatureKind.Construct ))

}
if ((constructSignatures&&constructSignatures.length)) {
 (targetType=getUnionType( map( constructSignatures, (signature =>  getReturnTypeOfSignature( getErasedSignature( signature ) )) ) ))

}

}
if (targetType) {
 return getNarrowedType( `type`, targetType, assumeTrue )

}
return `type`

}
def getNarrowedType(`type`: Type, candidate: Type, assumeTrue: Boolean) = {
 if ((!assumeTrue)) {
 return filterType( `type`, (t =>  (!isTypeInstanceOf( t, candidate ))) )

}
if ((`type`.flags&TypeFlags.Union)) {
 val assignableType = filterType( `type`, (t =>  isTypeInstanceOf( t, candidate )) )
if ((!((assignableType.flags&TypeFlags.Never)))) {
 return assignableType

}

}
val targetType = (if ((`type`.flags&TypeFlags.TypeParameter)) getApparentType( `type` ) else `type`)
return (if (isTypeSubtypeOf( candidate, `type` )) candidate else (if (isTypeAssignableTo( `type`, candidate )) `type` else (if (isTypeAssignableTo( candidate, targetType )) candidate else getIntersectionType( Array( `type`, candidate ) ))))

}
def narrowTypeByTypePredicate(`type`: Type, callExpression: CallExpression, assumeTrue: Boolean): Type = {
 if ((!hasMatchingArgument( callExpression, reference ))) {
 return `type`

}
val signature = getResolvedSignature( callExpression )
val predicate = signature.typePredicate
if ((!predicate)) {
 return `type`

}
if ((isTypeAny( `type` )&&(((predicate.`type`===globalObjectType)||(predicate.`type`===globalFunctionType))))) {
 return `type`

}
if (isIdentifierTypePredicate( predicate )) {
 val predicateArgument = callExpression.arguments(predicate.parameterIndex)
if (predicateArgument) {
 if (isMatchingReference( reference, predicateArgument )) {
 return getNarrowedType( `type`, predicate.`type`, assumeTrue )

}
if (containsMatchingReference( reference, predicateArgument )) {
 return declaredType

}

}

}
else {
 val invokedExpression = skipParenthesizedNodes( callExpression.expression )
if (((invokedExpression.kind===SyntaxKind.ElementAccessExpression)||(invokedExpression.kind===SyntaxKind.PropertyAccessExpression))) {
 val accessExpression = invokedExpression.asInstanceOf[( ElementAccessExpression | PropertyAccessExpression )]
val possibleReference = skipParenthesizedNodes( accessExpression.expression )
if (isMatchingReference( reference, possibleReference )) {
 return getNarrowedType( `type`, predicate.`type`, assumeTrue )

}
if (containsMatchingReference( reference, possibleReference )) {
 return declaredType

}

}

}
return `type`

}
def narrowType(`type`: Type, expr: Expression, assumeTrue: Boolean): Type = {
 expr.kind match {
  case  SyntaxKind.Identifier | SyntaxKind.ThisKeyword | SyntaxKind.PropertyAccessExpression  =>
return narrowTypeByTruthiness( `type`, expr, assumeTrue )
  case  SyntaxKind.CallExpression  =>
return narrowTypeByTypePredicate( `type`, expr.asInstanceOf[CallExpression], assumeTrue )
  case  SyntaxKind.ParenthesizedExpression  =>
return narrowType( `type`, (expr.asInstanceOf[ParenthesizedExpression]).expression, assumeTrue )
  case  SyntaxKind.BinaryExpression  =>
return narrowTypeByBinaryExpression( `type`, expr.asInstanceOf[BinaryExpression], assumeTrue )
  case  SyntaxKind.PrefixUnaryExpression  =>
if (((expr.asInstanceOf[PrefixUnaryExpression]).operator===SyntaxKind.ExclamationToken)) {
 return narrowType( `type`, (expr.asInstanceOf[PrefixUnaryExpression]).operand, (!assumeTrue) )

}
  case _ =>
}
return `type`

}

}
def getTypeOfSymbolAtLocation(symbol: Symbol, location: Node) = {
 if ((location.kind===SyntaxKind.Identifier)) {
 if (isRightSideOfQualifiedNameOrPropertyAccess( location )) {
 (location=location.parent)

}
if ((isPartOfExpression( location )&&(!isAssignmentTarget( location )))) {
 val `type` = checkExpression( location.asInstanceOf[Expression] )
if ((getExportSymbolOfValueSymbolIfExported( getNodeLinks( location ).resolvedSymbol )===symbol)) {
 return `type`

}

}

}
return getTypeOfSymbol( symbol )

}
def skipParenthesizedNodes(expression: Expression): Expression = {
 while ((expression.kind===SyntaxKind.ParenthesizedExpression)) {
{
 (expression=(expression.asInstanceOf[ParenthesizedExpression]).expression)

}
}
return expression

}
def getControlFlowContainer(node: Node): Node = {
 while (true) {
{
 (node=node.parent)
if (((((isFunctionLike( node )&&(!getImmediatelyInvokedFunctionExpression( node )))||(node.kind===SyntaxKind.ModuleBlock))||(node.kind===SyntaxKind.SourceFile))||(node.kind===SyntaxKind.PropertyDeclaration))) {
 return node

}

}
}

}
def isParameterAssigned(symbol: Symbol) = {
 val func = getRootDeclaration( symbol.valueDeclaration ).parent.asInstanceOf[FunctionLikeDeclaration]
val links = getNodeLinks( func )
if ((!((links.flags&NodeCheckFlags.AssignmentsMarked)))) {
 (links.flags|=NodeCheckFlags.AssignmentsMarked)
if ((!hasParentWithAssignmentsMarked( func ))) {
 markParameterAssignments( func )

}

}
return (symbol.isAssigned||false)

}
def hasParentWithAssignmentsMarked(node: Node) = {
 while (true) {
{
 (node=node.parent)
if ((!node)) {
 return false

}
if ((isFunctionLike( node )&&(getNodeLinks( node ).flags&NodeCheckFlags.AssignmentsMarked))) {
 return true

}

}
}

}
def markParameterAssignments(node: Node) = {
 if ((node.kind===SyntaxKind.Identifier)) {
 if (isAssignmentTarget( node )) {
 val symbol = getResolvedSymbol( node.asInstanceOf[Identifier] )
if ((symbol.valueDeclaration&&(getRootDeclaration( symbol.valueDeclaration ).kind===SyntaxKind.Parameter))) {
 (symbol.isAssigned=true)

}

}

}
else {
 forEachChild( node, markParameterAssignments )

}

}
def isConstVariable(symbol: Symbol) = {
 return (((symbol.flags&SymbolFlags.Variable)&&(((getDeclarationNodeFlagsFromSymbol( symbol )&NodeFlags.Const))!==0))&&(getTypeOfSymbol( symbol )!==autoArrayType))

}
def checkIdentifier(node: Identifier): Type = {
 val symbol = getResolvedSymbol( node )
if ((symbol===argumentsSymbol)) {
 val container = getContainingFunction( node )
if ((languageVersion<ScriptTarget.ES2015)) {
 if ((container.kind===SyntaxKind.ArrowFunction)) {
 error( node, Diagnostics.The_arguments_object_cannot_be_referenced_in_an_arrow_function_in_ES3_and_ES5_Consider_using_a_standard_function_expression )

}
else if (hasModifier( container, ModifierFlags.Async )) {
 error( node, Diagnostics.The_arguments_object_cannot_be_referenced_in_an_async_function_or_method_in_ES3_and_ES5_Consider_using_a_standard_function_or_method )

}

}
if ((node.flags&NodeFlags.AwaitContext)) {
 (getNodeLinks( container ).flags|=NodeCheckFlags.CaptureArguments)

}

}
if ((((symbol.flags&SymbolFlags.Alias)&&(!isInTypeQuery( node )))&&(!isConstEnumOrConstEnumOnlyModule( resolveAlias( symbol ) )))) {
 markAliasSymbolAsReferenced( symbol )

}
val localOrExportSymbol = getExportSymbolOfValueSymbolIfExported( symbol )
if ((localOrExportSymbol.flags&SymbolFlags.Class)) {
 val declaration = localOrExportSymbol.valueDeclaration
if ((((languageVersion===ScriptTarget.ES2015)&&(declaration.kind===SyntaxKind.ClassDeclaration))&&nodeIsDecorated( declaration ))) {
 var container = getContainingClass( node )
while ((container!==undefined)) {
{
 if (((container===declaration)&&(container.name!==node))) {
 (getNodeLinks( declaration ).flags|=NodeCheckFlags.ClassWithConstructorReference)
(getNodeLinks( node ).flags|=NodeCheckFlags.ConstructorReferenceInClass)
break()

}
(container=getContainingClass( container ))

}
}

}
else if ((declaration.kind===SyntaxKind.ClassExpression)) {
 var container = getThisContainer( node, false )
while ((container!==undefined)) {
{
 if ((container.parent===declaration)) {
 if (((container.kind===SyntaxKind.PropertyDeclaration)&&hasModifier( container, ModifierFlags.Static ))) {
 (getNodeLinks( declaration ).flags|=NodeCheckFlags.ClassWithConstructorReference)
(getNodeLinks( node ).flags|=NodeCheckFlags.ConstructorReferenceInClass)

}
break()

}
(container=getThisContainer( container, false ))

}
}

}

}
checkCollisionWithCapturedSuperVariable( node, node )
checkCollisionWithCapturedThisVariable( node, node )
checkNestedBlockScopedBinding( node, symbol )
val `type` = getTypeOfSymbol( localOrExportSymbol )
val declaration = localOrExportSymbol.valueDeclaration
if ((((!((localOrExportSymbol.flags&SymbolFlags.Variable)))||isAssignmentTarget( node ))||(!declaration))) {
 return `type`

}
val isParameter = (getRootDeclaration( declaration ).kind===SyntaxKind.Parameter)
val declarationContainer = getControlFlowContainer( declaration )
var flowContainer = getControlFlowContainer( node )
val isOuterVariable = (flowContainer!==declarationContainer)
while ((((flowContainer!==declarationContainer)&&((((flowContainer.kind===SyntaxKind.FunctionExpression)||(flowContainer.kind===SyntaxKind.ArrowFunction))||isObjectLiteralOrClassExpressionMethod( flowContainer ))))&&((isConstVariable( localOrExportSymbol )||(isParameter&&(!isParameterAssigned( localOrExportSymbol ))))))) {
{
 (flowContainer=getControlFlowContainer( flowContainer ))

}
}
val assumeInitialized = (((isParameter||isOuterVariable)||(((`type`!==autoType)&&(`type`!==autoArrayType))&&(((!strictNullChecks)||(((`type`.flags&TypeFlags.Any))!==0)))))||isInAmbientContext( declaration ))
val flowType = getFlowTypeOfReference( node, `type`, assumeInitialized, flowContainer )
if (((`type`===autoType)||(`type`===autoArrayType))) {
 if (((flowType===autoType)||(flowType===autoArrayType))) {
 if (compilerOptions.noImplicitAny) {
 error( declaration.name, Diagnostics.Variable_0_implicitly_has_type_1_in_some_locations_where_its_type_cannot_be_determined, symbolToString( symbol ), typeToString( flowType ) )
error( node, Diagnostics.Variable_0_implicitly_has_an_1_type, symbolToString( symbol ), typeToString( flowType ) )

}
return convertAutoToAny( flowType )

}

}
else if ((((!assumeInitialized)&&(!((getFalsyFlags( `type` )&TypeFlags.Undefined))))&&(getFalsyFlags( flowType )&TypeFlags.Undefined))) {
 error( node, Diagnostics.Variable_0_is_used_before_being_assigned, symbolToString( symbol ) )
return `type`

}
return flowType

}
def isInsideFunction(node: Node, threshold: Node): Boolean = {
 var current = node
while ((current&&(current!==threshold))) {
{
 if (isFunctionLike( current )) {
 return true

}
(current=current.parent)

}
}
return false

}
def checkNestedBlockScopedBinding(node: Identifier, symbol: Symbol): Unit = {
 if ((((languageVersion>=ScriptTarget.ES2015)||(((symbol.flags&((SymbolFlags.BlockScopedVariable|SymbolFlags.Class))))===0))||(symbol.valueDeclaration.parent.kind===SyntaxKind.CatchClause))) {
 return

}
val container = getEnclosingBlockScopeContainer( symbol.valueDeclaration )
val usedInFunction = isInsideFunction( node.parent, container )
var current = container
var containedInIterationStatement = false
while ((current&&(!nodeStartsNewLexicalEnvironment( current )))) {
{
 if (isIterationStatement( current, false )) {
 (containedInIterationStatement=true)
break()

}
(current=current.parent)

}
}
if (containedInIterationStatement) {
 if (usedInFunction) {
 (getNodeLinks( current ).flags|=NodeCheckFlags.LoopWithCapturedBlockScopedBinding)

}
if ((((container.kind===SyntaxKind.ForStatement)&&(getAncestor( symbol.valueDeclaration, SyntaxKind.VariableDeclarationList ).parent===container))&&isAssignedInBodyOfForStatement( node, container.asInstanceOf[ForStatement] ))) {
 (getNodeLinks( symbol.valueDeclaration ).flags|=NodeCheckFlags.NeedsLoopOutParameter)

}
(getNodeLinks( symbol.valueDeclaration ).flags|=NodeCheckFlags.BlockScopedBindingInLoop)

}
if (usedInFunction) {
 (getNodeLinks( symbol.valueDeclaration ).flags|=NodeCheckFlags.CapturedBlockScopedBinding)

}

}
def isAssignedInBodyOfForStatement(node: Identifier, container: ForStatement): Boolean = {
 var current: Node = node
while ((current.parent.kind===SyntaxKind.ParenthesizedExpression)) {
{
 (current=current.parent)

}
}
var isAssigned = false
if (isAssignmentTarget( current )) {
 (isAssigned=true)

}
else if ((((current.parent.kind===SyntaxKind.PrefixUnaryExpression)||(current.parent.kind===SyntaxKind.PostfixUnaryExpression)))) {
 val expr = current.parent.asInstanceOf[( PrefixUnaryExpression | PostfixUnaryExpression )]
(isAssigned=((expr.operator===SyntaxKind.PlusPlusToken)||(expr.operator===SyntaxKind.MinusMinusToken)))

}
if ((!isAssigned)) {
 return false

}
while ((current!==container)) {
{
 if ((current===container.statement)) {
 return true

}
else {
 (current=current.parent)

}

}
}
return false

}
def captureLexicalThis(node: Node, container: Node): Unit = {
 (getNodeLinks( node ).flags|=NodeCheckFlags.LexicalThis)
if (((container.kind===SyntaxKind.PropertyDeclaration)||(container.kind===SyntaxKind.Constructor))) {
 val classNode = container.parent
(getNodeLinks( classNode ).flags|=NodeCheckFlags.CaptureThis)

}
else {
 (getNodeLinks( container ).flags|=NodeCheckFlags.CaptureThis)

}

}
def findFirstSuperCall(n: Node): Node = {
 if (isSuperCall( n )) {
 return n

}
else if (isFunctionLike( n )) {
 return undefined

}
return forEachChild( n, findFirstSuperCall )

}
def getSuperCallInConstructor(`constructor`: ConstructorDeclaration): ExpressionStatement = {
 val links = getNodeLinks( `constructor` )
if ((links.hasSuperCall===undefined)) {
 (links.superCall=findFirstSuperCall( `constructor`.body ).asInstanceOf[ExpressionStatement])
(links.hasSuperCall=(if (links.superCall) true else false))

}
return links.superCall

}
def classDeclarationExtendsNull(classDecl: ClassDeclaration): Boolean = {
 val classSymbol = getSymbolOfNode( classDecl )
val classInstanceType = getDeclaredTypeOfSymbol( classSymbol ).asInstanceOf[InterfaceType]
val baseConstructorType = getBaseConstructorTypeOfClass( classInstanceType )
return (baseConstructorType===nullWideningType)

}
def checkThisExpression(node: Node): Type = {
 var container = getThisContainer( node, true )
var needToCaptureLexicalThis = false
if ((container.kind===SyntaxKind.Constructor)) {
 val containingClassDecl = container.parent.asInstanceOf[ClassDeclaration]
val baseTypeNode = getClassExtendsHeritageClauseElement( containingClassDecl )
if ((baseTypeNode&&(!classDeclarationExtendsNull( containingClassDecl )))) {
 val superCall = getSuperCallInConstructor( container.asInstanceOf[ConstructorDeclaration] )
if (((!superCall)||(superCall.end>node.pos))) {
 error( node, Diagnostics.super_must_be_called_before_accessing_this_in_the_constructor_of_a_derived_class )

}

}

}
if ((container.kind===SyntaxKind.ArrowFunction)) {
 (container=getThisContainer( container, false ))
(needToCaptureLexicalThis=((languageVersion<ScriptTarget.ES2015)))

}
container.kind match {
  case  SyntaxKind.ModuleDeclaration  =>
error( node, Diagnostics.this_cannot_be_referenced_in_a_module_or_namespace_body )
  case  SyntaxKind.EnumDeclaration  =>
error( node, Diagnostics.this_cannot_be_referenced_in_current_location )
  case  SyntaxKind.Constructor  =>
if (isInConstructorArgumentInitializer( node, container )) {
 error( node, Diagnostics.this_cannot_be_referenced_in_constructor_arguments )

}
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature  =>
if ((getModifierFlags( container )&ModifierFlags.Static)) {
 error( node, Diagnostics.this_cannot_be_referenced_in_a_static_property_initializer )

}
  case  SyntaxKind.ComputedPropertyName  =>
error( node, Diagnostics.this_cannot_be_referenced_in_a_computed_property_name )
  case _ =>
}
if (needToCaptureLexicalThis) {
 captureLexicalThis( node, container )

}
if ((isFunctionLike( container )&&(((!isInParameterInitializerBeforeContainingFunction( node ))||getThisParameter( container ))))) {
 if ((((container.kind===SyntaxKind.FunctionExpression)&&isInJavaScriptFile( container.parent ))&&(getSpecialPropertyAssignmentKind( container.parent )===SpecialPropertyAssignmentKind.PrototypeProperty))) {
 val className = (((container.parent.asInstanceOf[BinaryExpression]).left.asInstanceOf[PropertyAccessExpression]).expression.asInstanceOf[PropertyAccessExpression]).expression
val classSymbol = checkExpression( className ).symbol
if (((classSymbol&&classSymbol.members)&&((classSymbol.flags&SymbolFlags.Function)))) {
 return getInferredClassType( classSymbol )

}

}
val thisType = (getThisTypeOfDeclaration( container )||getContextualThisParameterType( container ))
if (thisType) {
 return thisType

}

}
if (isClassLike( container.parent )) {
 val symbol = getSymbolOfNode( container.parent )
val `type` = (if (hasModifier( container, ModifierFlags.Static )) getTypeOfSymbol( symbol ) else (getDeclaredTypeOfSymbol( symbol ).asInstanceOf[InterfaceType]).thisType)
return getFlowTypeOfReference( node, `type`, true, undefined )

}
if (isInJavaScriptFile( node )) {
 val `type` = getTypeForThisExpressionFromJSDoc( container )
if ((`type`&&(`type`!==unknownType))) {
 return `type`

}

}
if (compilerOptions.noImplicitThis) {
 error( node, Diagnostics.this_implicitly_has_type_any_because_it_does_not_have_a_type_annotation )

}
return anyType

}
def getTypeForThisExpressionFromJSDoc(node: Node) = {
 val typeTag = getJSDocTypeTag( node )
if ((((typeTag&&typeTag.typeExpression)&&typeTag.typeExpression.`type`)&&(typeTag.typeExpression.`type`.kind===SyntaxKind.JSDocFunctionType))) {
 val jsDocFunctionType = typeTag.typeExpression.`type`.asInstanceOf[JSDocFunctionType]
if (((jsDocFunctionType.parameters.length>0)&&(jsDocFunctionType.parameters(0).`type`.kind===SyntaxKind.JSDocThisType))) {
 return getTypeFromTypeNode( jsDocFunctionType.parameters(0).`type` )

}

}

}
def isInConstructorArgumentInitializer(node: Node, constructorDecl: Node): Boolean = {
 {
var n = node
while( (n&&(n!==constructorDecl))) {
 {
 if ((n.kind===SyntaxKind.Parameter)) {
 return true

}

}
 (n=n.parent)
}
}
return false

}
def checkSuperExpression(node: Node): Type = {
 val isCallExpression = ((node.parent.kind===SyntaxKind.CallExpression)&&((node.parent.asInstanceOf[CallExpression]).expression===node))
var container = getSuperContainer( node, true )
var needToCaptureLexicalThis = false
if ((!isCallExpression)) {
 while ((container&&(container.kind===SyntaxKind.ArrowFunction))) {
{
 (container=getSuperContainer( container, true ))
(needToCaptureLexicalThis=(languageVersion<ScriptTarget.ES2015))

}
}

}
val canUseSuperExpression = isLegalUsageOfSuperExpression( container )
var nodeCheckFlag: NodeCheckFlags = 0
if ((!canUseSuperExpression)) {
 var current = node
while (((current&&(current!==container))&&(current.kind!==SyntaxKind.ComputedPropertyName))) {
{
 (current=current.parent)

}
}
if ((current&&(current.kind===SyntaxKind.ComputedPropertyName))) {
 error( node, Diagnostics.super_cannot_be_referenced_in_a_computed_property_name )

}
else if (isCallExpression) {
 error( node, Diagnostics.Super_calls_are_not_permitted_outside_constructors_or_in_nested_functions_inside_constructors )

}
else if ((((!container)||(!container.parent))||(!((isClassLike( container.parent )||(container.parent.kind===SyntaxKind.ObjectLiteralExpression)))))) {
 error( node, Diagnostics.super_can_only_be_referenced_in_members_of_derived_classes_or_object_literal_expressions )

}
else {
 error( node, Diagnostics.super_property_access_is_permitted_only_in_a_constructor_member_function_or_member_accessor_of_a_derived_class )

}
return unknownType

}
if ((((getModifierFlags( container )&ModifierFlags.Static))||isCallExpression)) {
 (nodeCheckFlag=NodeCheckFlags.SuperStatic)

}
else {
 (nodeCheckFlag=NodeCheckFlags.SuperInstance)

}
(getNodeLinks( node ).flags|=nodeCheckFlag)
if (((container.kind===SyntaxKind.MethodDeclaration)&&(getModifierFlags( container )&ModifierFlags.Async))) {
 if ((isSuperProperty( node.parent )&&isAssignmentTarget( node.parent ))) {
 (getNodeLinks( container ).flags|=NodeCheckFlags.AsyncMethodWithSuperBinding)

}
else {
 (getNodeLinks( container ).flags|=NodeCheckFlags.AsyncMethodWithSuper)

}

}
if (needToCaptureLexicalThis) {
 captureLexicalThis( node.parent, container )

}
if ((container.parent.kind===SyntaxKind.ObjectLiteralExpression)) {
 if ((languageVersion<ScriptTarget.ES2015)) {
 error( node, Diagnostics.super_is_only_allowed_in_members_of_object_literal_expressions_when_option_target_is_ES2015_or_higher )
return unknownType

}
else {
 return anyType

}

}
val classLikeDeclaration = container.parent.asInstanceOf[ClassLikeDeclaration]
val classType = getDeclaredTypeOfSymbol( getSymbolOfNode( classLikeDeclaration ) ).asInstanceOf[InterfaceType]
val baseClassType = (classType&&getBaseTypes( classType )(0))
if ((!baseClassType)) {
 if ((!getClassExtendsHeritageClauseElement( classLikeDeclaration ))) {
 error( node, Diagnostics.super_can_only_be_referenced_in_a_derived_class )

}
return unknownType

}
if (((container.kind===SyntaxKind.Constructor)&&isInConstructorArgumentInitializer( node, container ))) {
 error( node, Diagnostics.super_cannot_be_referenced_in_constructor_arguments )
return unknownType

}
return (if ((nodeCheckFlag===NodeCheckFlags.SuperStatic)) getBaseConstructorTypeOfClass( classType ) else getTypeWithThisArgument( baseClassType, classType.thisType ))
def isLegalUsageOfSuperExpression(container: Node): Boolean = {
 if ((!container)) {
 return false

}
if (isCallExpression) {
 return (container.kind===SyntaxKind.Constructor)

}
else {
 if ((isClassLike( container.parent )||(container.parent.kind===SyntaxKind.ObjectLiteralExpression))) {
 if ((getModifierFlags( container )&ModifierFlags.Static)) {
 return ((((container.kind===SyntaxKind.MethodDeclaration)||(container.kind===SyntaxKind.MethodSignature))||(container.kind===SyntaxKind.GetAccessor))||(container.kind===SyntaxKind.SetAccessor))

}
else {
 return (((((((container.kind===SyntaxKind.MethodDeclaration)||(container.kind===SyntaxKind.MethodSignature))||(container.kind===SyntaxKind.GetAccessor))||(container.kind===SyntaxKind.SetAccessor))||(container.kind===SyntaxKind.PropertyDeclaration))||(container.kind===SyntaxKind.PropertySignature))||(container.kind===SyntaxKind.Constructor))

}

}

}
return false

}

}
def getContextualThisParameterType(func: FunctionLikeDeclaration): Type = {
 if ((isContextSensitiveFunctionOrObjectLiteralMethod( func )&&(func.kind!==SyntaxKind.ArrowFunction))) {
 val contextualSignature = getContextualSignature( func )
if (contextualSignature) {
 val thisParameter = contextualSignature.thisParameter
if (thisParameter) {
 return getTypeOfSymbol( thisParameter )

}

}

}
return undefined

}
def getContextuallyTypedParameterType(parameter: ParameterDeclaration): Type = {
 val func = parameter.parent
if (isContextSensitiveFunctionOrObjectLiteralMethod( func )) {
 val iife = getImmediatelyInvokedFunctionExpression( func )
if (iife) {
 val indexOfParameter = indexOf( func.parameters, parameter )
if ((iife.arguments&&(indexOfParameter<iife.arguments.length))) {
 if (parameter.dotDotDotToken) {
 val restTypes: Array[Type] = Array()
{
var i = indexOfParameter
while( (i<iife.arguments.length)) {
 {
 restTypes.push( getWidenedLiteralType( checkExpression( iife.arguments(i) ) ) )

}
 (i+= 1)
}
}
return createArrayType( getUnionType( restTypes ) )

}
val links = getNodeLinks( iife )
val cached = links.resolvedSignature
(links.resolvedSignature=anySignature)
val `type` = getWidenedLiteralType( checkExpression( iife.arguments(indexOfParameter) ) )
(links.resolvedSignature=cached)
return `type`

}

}
val contextualSignature = getContextualSignature( func )
if (contextualSignature) {
 val funcHasRestParameters = hasRestParameter( func )
val len = (func.parameters.length-((if (funcHasRestParameters) 1 else 0)))
val indexOfParameter = indexOf( func.parameters, parameter )
if ((indexOfParameter<len)) {
 return getTypeAtPosition( contextualSignature, indexOfParameter )

}
if (((funcHasRestParameters&&(indexOfParameter===((func.parameters.length-1))))&&isRestParameterIndex( contextualSignature, (func.parameters.length-1) ))) {
 return getTypeOfSymbol( lastOrUndefined( contextualSignature.parameters ) )

}

}

}
return undefined

}
def getContextualTypeForInitializerExpression(node: Expression): Type = {
 val declaration = node.parent.asInstanceOf[VariableLikeDeclaration]
if ((node===declaration.initializer)) {
 if (declaration.`type`) {
 return getTypeFromTypeNode( declaration.`type` )

}
if ((declaration.kind===SyntaxKind.Parameter)) {
 val `type` = getContextuallyTypedParameterType( declaration.asInstanceOf[ParameterDeclaration] )
if (`type`) {
 return `type`

}

}
if (isBindingPattern( declaration.name )) {
 return getTypeFromBindingPattern( declaration.name.asInstanceOf[BindingPattern], true, false )

}
if (isBindingPattern( declaration.parent )) {
 val parentDeclaration = declaration.parent.parent
val name = (declaration.propertyName||declaration.name)
if (((isVariableLike( parentDeclaration )&&parentDeclaration.`type`)&&(!isBindingPattern( name )))) {
 val text = getTextOfPropertyName( name )
if (text) {
 return getTypeOfPropertyOfType( getTypeFromTypeNode( parentDeclaration.`type` ), text )

}

}

}

}
return undefined

}
def getContextualTypeForReturnExpression(node: Expression): Type = {
 val func = getContainingFunction( node )
if (isAsyncFunctionLike( func )) {
 val contextualReturnType = getContextualReturnType( func )
if (contextualReturnType) {
 return getPromisedType( contextualReturnType )

}
return undefined

}
if ((func&&(!func.asteriskToken))) {
 return getContextualReturnType( func )

}
return undefined

}
def getContextualTypeForYieldOperand(node: YieldExpression): Type = {
 val func = getContainingFunction( node )
if (func) {
 val contextualReturnType = getContextualReturnType( func )
if (contextualReturnType) {
 return (if (node.asteriskToken) contextualReturnType else getElementTypeOfIterableIterator( contextualReturnType ))

}

}
return undefined

}
def isInParameterInitializerBeforeContainingFunction(node: Node) = {
 while ((node.parent&&(!isFunctionLike( node.parent )))) {
{
 if (((node.parent.kind===SyntaxKind.Parameter)&&((node.parent.asInstanceOf[ParameterDeclaration]).initializer===node))) {
 return true

}
(node=node.parent)

}
}
return false

}
def getContextualReturnType(functionDecl: FunctionLikeDeclaration): Type = {
 if (((functionDecl.`type`||(functionDecl.kind===SyntaxKind.Constructor))||((functionDecl.kind===SyntaxKind.GetAccessor)&&getSetAccessorTypeAnnotationNode( getDeclarationOfKind( functionDecl.symbol, SyntaxKind.SetAccessor ).asInstanceOf[SetAccessorDeclaration] )))) {
 return getReturnTypeOfSignature( getSignatureFromDeclaration( functionDecl ) )

}
val signature = getContextualSignatureForFunctionLikeDeclaration( functionDecl.asInstanceOf[FunctionExpression] )
if (signature) {
 return getReturnTypeOfSignature( signature )

}
return undefined

}
def getContextualTypeForArgument(callTarget: CallLikeExpression, arg: Expression): Type = {
 val args = getEffectiveCallArguments( callTarget )
val argIndex = indexOf( args, arg )
if ((argIndex>=0)) {
 val signature = getResolvedOrAnySignature( callTarget )
return getTypeAtPosition( signature, argIndex )

}
return undefined

}
def getContextualTypeForSubstitutionExpression(template: TemplateExpression, substitutionExpression: Expression) = {
 if ((template.parent.kind===SyntaxKind.TaggedTemplateExpression)) {
 return getContextualTypeForArgument( template.parent.asInstanceOf[TaggedTemplateExpression], substitutionExpression )

}
return undefined

}
def getContextualTypeForBinaryOperand(node: Expression): Type = {
 val binaryExpression = node.parent.asInstanceOf[BinaryExpression]
val operator = binaryExpression.operatorToken.kind
if (((operator>=SyntaxKind.FirstAssignment)&&(operator<=SyntaxKind.LastAssignment))) {
 if ((getSpecialPropertyAssignmentKind( binaryExpression )!==SpecialPropertyAssignmentKind.None)) {
 return undefined

}
if ((node===binaryExpression.right)) {
 return checkExpression( binaryExpression.left )

}

}
else if ((operator===SyntaxKind.BarBarToken)) {
 var `type` = getContextualType( binaryExpression )
if (((!`type`)&&(node===binaryExpression.right))) {
 (`type`=checkExpression( binaryExpression.left ))

}
return `type`

}
else if (((operator===SyntaxKind.AmpersandAmpersandToken)||(operator===SyntaxKind.CommaToken))) {
 if ((node===binaryExpression.right)) {
 return getContextualType( binaryExpression )

}

}
return undefined

}
def applyToContextualType(`type`: Type, mapper: ((Type) => Type)): Type = {
 if ((!((`type`.flags&TypeFlags.Union)))) {
 return mapper( `type` )

}
val types = (`type`.asInstanceOf[UnionType]).types
var mappedType: Type = zeroOfMyType
var mappedTypes: Array[Type] = zeroOfMyType
(types).foreach { fresh87 =>
val current = zeroOfMyType
 = fresh87
 {
 val t = mapper( current )
if (t) {
 if ((!mappedType)) {
 (mappedType=t)

}
else if ((!mappedTypes)) {
 (mappedTypes=Array( mappedType, t ))

}
else {
 mappedTypes.push( t )

}

}

}
}
return (if (mappedTypes) getUnionType( mappedTypes ) else mappedType)

}
def getTypeOfPropertyOfContextualType(`type`: Type, name: String) = {
 return applyToContextualType( `type`, (t =>  {
 val prop = (if ((t.flags&TypeFlags.StructuredType)) getPropertyOfType( t, name ) else undefined)
 return (if (prop) getTypeOfSymbol( prop ) else undefined)

}) )

}
def getIndexTypeOfContextualType(`type`: Type, kind: IndexKind) = {
 return applyToContextualType( `type`, (t =>  getIndexTypeOfStructuredType( t, kind )) )

}
def contextualTypeIsTupleLikeType(`type`: Type): Boolean = {
 return (!(!((if ((`type`.flags&TypeFlags.Union)) forEach( (`type`.asInstanceOf[UnionType]).types, isTupleLikeType ) else isTupleLikeType( `type` )))))

}
def getContextualTypeForObjectLiteralMethod(node: MethodDeclaration): Type = {
 Debug.assert( isObjectLiteralMethod( node ) )
if (isInsideWithStatementBody( node )) {
 return undefined

}
return getContextualTypeForObjectLiteralElement( node )

}
def getContextualTypeForObjectLiteralElement(element: ObjectLiteralElementLike) = {
 val objectLiteral = element.parent.asInstanceOf[ObjectLiteralExpression]
val `type` = getApparentTypeOfContextualType( objectLiteral )
if (`type`) {
 if ((!hasDynamicName( element ))) {
 val symbolName = getSymbolOfNode( element ).name
val propertyType = getTypeOfPropertyOfContextualType( `type`, symbolName )
if (propertyType) {
 return propertyType

}

}
return ((isNumericName( element.name )&&getIndexTypeOfContextualType( `type`, IndexKind.Number ))||getIndexTypeOfContextualType( `type`, IndexKind.String ))

}
return undefined

}
def getContextualTypeForElementExpression(node: Expression): Type = {
 val arrayLiteral = node.parent.asInstanceOf[ArrayLiteralExpression]
val `type` = getApparentTypeOfContextualType( arrayLiteral )
if (`type`) {
 val index = indexOf( arrayLiteral.elements, node )
return ((getTypeOfPropertyOfContextualType( `type`, (""+index) )||getIndexTypeOfContextualType( `type`, IndexKind.Number ))||((if ((languageVersion>=ScriptTarget.ES2015)) getElementTypeOfIterable( `type`, undefined ) else undefined)))

}
return undefined

}
def getContextualTypeForConditionalOperand(node: Expression): Type = {
 val conditional = node.parent.asInstanceOf[ConditionalExpression]
return (if (((node===conditional.whenTrue)||(node===conditional.whenFalse))) getContextualType( conditional ) else undefined)

}
def getContextualTypeForJsxAttribute(attribute: ( JsxAttribute | JsxSpreadAttribute )) = {
 val kind = attribute.kind
val jsxElement = attribute.parent.asInstanceOf[JsxOpeningLikeElement]
val attrsType = getJsxElementAttributesType( jsxElement )
if ((attribute.kind===SyntaxKind.JsxAttribute)) {
 if (((!attrsType)||isTypeAny( attrsType ))) {
 return undefined

}
return getTypeOfPropertyOfType( attrsType, (attribute.asInstanceOf[JsxAttribute]).name.text )

}
else if ((attribute.kind===SyntaxKind.JsxSpreadAttribute)) {
 return attrsType

}
Debug.fail( s"""Expected JsxAttribute or JsxSpreadAttribute, got ts.SyntaxKind[${ kind}]"""  )

}
def getApparentTypeOfContextualType(node: Expression): Type = {
 val `type` = getContextualType( node )
return (`type`&&getApparentType( `type` ))

}
def getContextualType(node: Expression): Type = {
 if (isInsideWithStatementBody( node )) {
 return undefined

}
if (node.contextualType) {
 return node.contextualType

}
val parent = node.parent
parent.kind match {
  case  SyntaxKind.VariableDeclaration | SyntaxKind.Parameter | SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.BindingElement  =>
return getContextualTypeForInitializerExpression( node )
  case  SyntaxKind.ArrowFunction | SyntaxKind.ReturnStatement  =>
return getContextualTypeForReturnExpression( node )
  case  SyntaxKind.YieldExpression  =>
return getContextualTypeForYieldOperand( parent.asInstanceOf[YieldExpression] )
  case  SyntaxKind.CallExpression | SyntaxKind.NewExpression  =>
return getContextualTypeForArgument( parent.asInstanceOf[CallExpression], node )
  case  SyntaxKind.TypeAssertionExpression | SyntaxKind.AsExpression  =>
return getTypeFromTypeNode( (parent.asInstanceOf[AssertionExpression]).`type` )
  case  SyntaxKind.BinaryExpression  =>
return getContextualTypeForBinaryOperand( node )
  case  SyntaxKind.PropertyAssignment | SyntaxKind.ShorthandPropertyAssignment  =>
return getContextualTypeForObjectLiteralElement( parent.asInstanceOf[ObjectLiteralElementLike] )
  case  SyntaxKind.ArrayLiteralExpression  =>
return getContextualTypeForElementExpression( node )
  case  SyntaxKind.ConditionalExpression  =>
return getContextualTypeForConditionalOperand( node )
  case  SyntaxKind.TemplateSpan  =>
Debug.assert( (parent.parent.kind===SyntaxKind.TemplateExpression) )
return getContextualTypeForSubstitutionExpression( parent.parent.asInstanceOf[TemplateExpression], node )
  case  SyntaxKind.ParenthesizedExpression  =>
return getContextualType( parent.asInstanceOf[ParenthesizedExpression] )
  case  SyntaxKind.JsxExpression  =>
return getContextualType( parent.asInstanceOf[JsxExpression] )
  case  SyntaxKind.JsxAttribute | SyntaxKind.JsxSpreadAttribute  =>
return getContextualTypeForJsxAttribute( parent.asInstanceOf[( JsxAttribute | JsxSpreadAttribute )] )
  case _ =>
}
return undefined

}
def getNonGenericSignature(`type`: Type, node: ( FunctionExpression | ArrowFunction | MethodDeclaration )): Signature = {
 val signatures = getSignaturesOfStructuredType( `type`, SignatureKind.Call )
if ((signatures.length===1)) {
 val signature = signatures(0)
if (((!signature.typeParameters)&&(!isAritySmaller( signature, node )))) {
 return signature

}

}

}
def isAritySmaller(signature: Signature, target: ( FunctionExpression | ArrowFunction | MethodDeclaration )) = {
 var targetParameterCount = 0
{
while( (targetParameterCount<target.parameters.length)) {
 {
 val param = target.parameters(targetParameterCount)
if ((((param.initializer||param.questionToken)||param.dotDotDotToken)||isJSDocOptionalParameter( param ))) {
 break()

}

}
 (targetParameterCount+= 1)
}
}
if ((target.parameters.length&&parameterIsThisKeyword( target.parameters(0) ))) {
 (targetParameterCount-= 1)

}
val sourceLength = (if (signature.hasRestParameter) Number.MAX_VALUE else signature.parameters.length)
return (sourceLength<targetParameterCount)

}
def isFunctionExpressionOrArrowFunction(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.FunctionExpression)||(node.kind===SyntaxKind.ArrowFunction))

}
def getContextualSignatureForFunctionLikeDeclaration(node: FunctionLikeDeclaration): Signature = {
 return (if ((isFunctionExpressionOrArrowFunction( node )||isObjectLiteralMethod( node ))) getContextualSignature( node.asInstanceOf[FunctionExpression] ) else undefined)

}
def getContextualTypeForFunctionLikeDeclaration(node: ( FunctionExpression | ArrowFunction | MethodDeclaration )) = {
 return (if (isObjectLiteralMethod( node )) getContextualTypeForObjectLiteralMethod( node ) else getApparentTypeOfContextualType( node ))

}
def getContextualSignature(node: ( FunctionExpression | ArrowFunction | MethodDeclaration )): Signature = {
 Debug.assert( ((node.kind!==SyntaxKind.MethodDeclaration)||isObjectLiteralMethod( node )) )
val `type` = getContextualTypeForFunctionLikeDeclaration( node )
if ((!`type`)) {
 return undefined

}
if ((!((`type`.flags&TypeFlags.Union)))) {
 return getNonGenericSignature( `type`, node )

}
var signatureList: Array[Signature] = zeroOfMyType
val types = (`type`.asInstanceOf[UnionType]).types
(types).foreach { fresh88 =>
val current = zeroOfMyType
 = fresh88
 {
 val signature = getNonGenericSignature( current, node )
if (signature) {
 if ((!signatureList)) {
 (signatureList=Array( signature ))

}
else if ((!compareSignaturesIdentical( signatureList(0), signature, false, true, true, compareTypesIdentical ))) {
 return undefined

}
else {
 signatureList.push( signature )

}

}

}
}
var result: Signature = zeroOfMyType
if (signatureList) {
 (result=cloneSignature( signatureList(0) ))
(result.resolvedReturnType=undefined)
(result.unionSignatures=signatureList)

}
return result

}
def isInferentialContext(mapper: TypeMapper) = {
 return (mapper&&mapper.context)

}
def checkSpreadElementExpression(node: SpreadElementExpression, contextualMapper: TypeMapper): Type = {
 val arrayOrIterableType = checkExpressionCached( node.expression, contextualMapper )
return checkIteratedTypeOrElementType( arrayOrIterableType, node.expression, false )

}
def hasDefaultValue(node: ( BindingElement | Expression )): Boolean = {
 return ((((node.kind===SyntaxKind.BindingElement)&&(!(!(node.asInstanceOf[BindingElement]).initializer))))||(((node.kind===SyntaxKind.BinaryExpression)&&((node.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.EqualsToken))))

}
def checkArrayLiteral(node: ArrayLiteralExpression, contextualMapper: TypeMapper): Type = {
 val elements = node.elements
var hasSpreadElement = false
val elementTypes: Array[Type] = Array()
val inDestructuringPattern = isAssignmentTarget( node )
(elements).foreach { fresh89 =>
val e = zeroOfMyType
 = fresh89
 {
 if ((inDestructuringPattern&&(e.kind===SyntaxKind.SpreadElementExpression))) {
 val restArrayType = checkExpression( (e.asInstanceOf[SpreadElementExpression]).expression, contextualMapper )
val restElementType = (getIndexTypeOfType( restArrayType, IndexKind.Number )||((if ((languageVersion>=ScriptTarget.ES2015)) getElementTypeOfIterable( restArrayType, undefined ) else undefined)))
if (restElementType) {
 elementTypes.push( restElementType )

}

}
else {
 val `type` = checkExpressionForMutableLocation( e, contextualMapper )
elementTypes.push( `type` )

}
(hasSpreadElement=(hasSpreadElement||(e.kind===SyntaxKind.SpreadElementExpression)))

}
}
if ((!hasSpreadElement)) {
 if ((inDestructuringPattern&&elementTypes.length)) {
 val `type` = cloneTypeReference( createTupleType( elementTypes ) )
(`type`.pattern=node)
return `type`

}
val contextualType = getApparentTypeOfContextualType( node )
if ((contextualType&&contextualTypeIsTupleLikeType( contextualType ))) {
 val pattern = contextualType.pattern
if ((pattern&&(((pattern.kind===SyntaxKind.ArrayBindingPattern)||(pattern.kind===SyntaxKind.ArrayLiteralExpression))))) {
 val patternElements = (pattern.asInstanceOf[( BindingPattern | ArrayLiteralExpression )]).elements
{
var i = elementTypes.length
while( (i<patternElements.length)) {
 {
 val patternElement = patternElements(i)
if (hasDefaultValue( patternElement )) {
 elementTypes.push( (contextualType.asInstanceOf[TypeReference]).typeArguments(i) )

}
else {
 if ((patternElement.kind!==SyntaxKind.OmittedExpression)) {
 error( patternElement, Diagnostics.Initializer_provides_no_value_for_this_binding_element_and_the_binding_element_has_no_default_value )

}
elementTypes.push( unknownType )

}

}
 (i+= 1)
}
}

}
if (elementTypes.length) {
 return createTupleType( elementTypes )

}

}

}
return createArrayType( (if (elementTypes.length) getUnionType( elementTypes, true ) else (if (strictNullChecks) neverType else undefinedWideningType)) )

}
def isNumericName(name: DeclarationName): Boolean = {
 return (if ((name.kind===SyntaxKind.ComputedPropertyName)) isNumericComputedName( name.asInstanceOf[ComputedPropertyName] ) else isNumericLiteralName( (name.asInstanceOf[Identifier]).text ))

}
def isNumericComputedName(name: ComputedPropertyName): Boolean = {
 return isTypeAnyOrAllConstituentTypesHaveKind( checkComputedPropertyName( name ), TypeFlags.NumberLike )

}
def isTypeAnyOrAllConstituentTypesHaveKind(`type`: Type, kind: TypeFlags): Boolean = {
 return (isTypeAny( `type` )||isTypeOfKind( `type`, kind ))

}
def isInfinityOrNaNString(name: String): Boolean = {
 return (((name==="Infinity")||(name==="-Infinity"))||(name==="NaN"))

}
def isNumericLiteralName(name: String) = {
 return (((+name)).`toString`()===name)

}
def checkComputedPropertyName(node: ComputedPropertyName): Type = {
 val links = getNodeLinks( node.expression )
if ((!links.resolvedType)) {
 (links.resolvedType=checkExpression( node.expression ))
if ((!isTypeAnyOrAllConstituentTypesHaveKind( links.resolvedType, ((TypeFlags.NumberLike|TypeFlags.StringLike)|TypeFlags.ESSymbol) ))) {
 error( node, Diagnostics.A_computed_property_name_must_be_of_type_string_number_symbol_or_any )

}
else {
 checkThatExpressionIsProperSymbolReference( node.expression, links.resolvedType, true )

}

}
return links.resolvedType

}
def getObjectLiteralIndexInfo(node: ObjectLiteralExpression, properties: Array[Symbol], kind: IndexKind): IndexInfo = {
 val propTypes: Array[Type] = Array()
{
var i = 0
while( (i<properties.length)) {
 {
 if (((kind===IndexKind.String)||isNumericName( node.properties(i).name ))) {
 propTypes.push( getTypeOfSymbol( properties(i) ) )

}

}
 (i+= 1)
}
}
val unionType = (if (propTypes.length) getUnionType( propTypes, true ) else undefinedType)
return createIndexInfo( unionType, false )

}
def checkObjectLiteral(node: ObjectLiteralExpression, contextualMapper: TypeMapper): Type = {
 val inDestructuringPattern = isAssignmentTarget( node )
checkGrammarObjectLiteralExpression( node, inDestructuringPattern )
val propertiesTable = createMap[ Symbol ]()
val propertiesArray: Array[Symbol] = Array()
val contextualType = getApparentTypeOfContextualType( node )
val contextualTypeHasPattern = ((contextualType&&contextualType.pattern)&&(((contextualType.pattern.kind===SyntaxKind.ObjectBindingPattern)||(contextualType.pattern.kind===SyntaxKind.ObjectLiteralExpression))))
var typeFlags: TypeFlags = 0
var patternWithComputedProperties = false
var hasComputedStringProperty = false
var hasComputedNumberProperty = false
(node.properties).foreach { fresh90 =>
val memberDecl = zeroOfMyType
 = fresh90
 {
 var member = memberDecl.symbol
if ((((memberDecl.kind===SyntaxKind.PropertyAssignment)||(memberDecl.kind===SyntaxKind.ShorthandPropertyAssignment))||isObjectLiteralMethod( memberDecl ))) {
 var `type`: Type = zeroOfMyType
if ((memberDecl.kind===SyntaxKind.PropertyAssignment)) {
 (`type`=checkPropertyAssignment( memberDecl.asInstanceOf[PropertyAssignment], contextualMapper ))

}
else if ((memberDecl.kind===SyntaxKind.MethodDeclaration)) {
 (`type`=checkObjectLiteralMethod( memberDecl.asInstanceOf[MethodDeclaration], contextualMapper ))

}
else {
 Debug.assert( (memberDecl.kind===SyntaxKind.ShorthandPropertyAssignment) )
(`type`=checkExpressionForMutableLocation( (memberDecl.asInstanceOf[ShorthandPropertyAssignment]).name, contextualMapper ))

}
(typeFlags|=`type`.flags)
val prop = createSymbol( ((SymbolFlags.Property|SymbolFlags.Transient)|member.flags), member.name ).asInstanceOf[TransientSymbol]
if (inDestructuringPattern) {
 val isOptional = ((((memberDecl.kind===SyntaxKind.PropertyAssignment)&&hasDefaultValue( (memberDecl.asInstanceOf[PropertyAssignment]).initializer )))||(((memberDecl.kind===SyntaxKind.ShorthandPropertyAssignment)&&(memberDecl.asInstanceOf[ShorthandPropertyAssignment]).objectAssignmentInitializer)))
if (isOptional) {
 (prop.flags|=SymbolFlags.Optional)

}
if (hasDynamicName( memberDecl )) {
 (patternWithComputedProperties=true)

}

}
else if ((contextualTypeHasPattern&&(!((getObjectFlags( contextualType )&ObjectFlags.ObjectLiteralPatternWithComputedProperties))))) {
 val impliedProp = getPropertyOfType( contextualType, member.name )
if (impliedProp) {
 (prop.flags|=(impliedProp.flags&SymbolFlags.Optional))

}
else if ((!compilerOptions.suppressExcessPropertyErrors)) {
 error( memberDecl.name, Diagnostics.Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1, symbolToString( member ), typeToString( contextualType ) )

}

}
(prop.declarations=member.declarations)
(prop.parent=member.parent)
if (member.valueDeclaration) {
 (prop.valueDeclaration=member.valueDeclaration)

}
(prop.`type`=`type`)
(prop.target=member)
(member=prop)

}
else {
 Debug.assert( ((memberDecl.kind===SyntaxKind.GetAccessor)||(memberDecl.kind===SyntaxKind.SetAccessor)) )
checkAccessorDeclaration( memberDecl.asInstanceOf[AccessorDeclaration] )

}
if (hasDynamicName( memberDecl )) {
 if (isNumericName( memberDecl.name )) {
 (hasComputedNumberProperty=true)

}
else {
 (hasComputedStringProperty=true)

}

}
else {
 (propertiesTable(member.name)=member)

}
propertiesArray.push( member )

}
}
if (contextualTypeHasPattern) {
 (getPropertiesOfType( contextualType )).foreach { fresh91 =>
val prop = zeroOfMyType
 = fresh91
 {
 if ((!propertiesTable(prop.name))) {
 if ((!((prop.flags&SymbolFlags.Optional)))) {
 error( (prop.valueDeclaration||(prop.asInstanceOf[TransientSymbol]).bindingElement), Diagnostics.Initializer_provides_no_value_for_this_binding_element_and_the_binding_element_has_no_default_value )

}
(propertiesTable(prop.name)=prop)
propertiesArray.push( prop )

}

}
}

}
val stringIndexInfo = (if (hasComputedStringProperty) getObjectLiteralIndexInfo( node, propertiesArray, IndexKind.String ) else undefined)
val numberIndexInfo = (if (hasComputedNumberProperty) getObjectLiteralIndexInfo( node, propertiesArray, IndexKind.Number ) else undefined)
val result = createAnonymousType( node.symbol, propertiesTable, emptyArray, emptyArray, stringIndexInfo, numberIndexInfo )
val freshObjectLiteralFlag = (if (compilerOptions.suppressExcessPropertyErrors) 0 else TypeFlags.FreshLiteral)
(result.flags|=((TypeFlags.ContainsObjectLiteral|freshObjectLiteralFlag)|((typeFlags&TypeFlags.PropagatingFlags))))
(result.objectFlags|=ObjectFlags.ObjectLiteral)
if (patternWithComputedProperties) {
 (result.objectFlags|=ObjectFlags.ObjectLiteralPatternWithComputedProperties)

}
if (inDestructuringPattern) {
 (result.pattern=node)

}
return result

}
def checkJsxSelfClosingElement(node: JsxSelfClosingElement) = {
 checkJsxOpeningLikeElement( node )
return (jsxElementType||anyType)

}
def checkJsxElement(node: JsxElement) = {
 checkJsxOpeningLikeElement( node.openingElement )
if (isJsxIntrinsicIdentifier( node.closingElement.tagName )) {
 getIntrinsicTagSymbol( node.closingElement )

}
else {
 checkExpression( node.closingElement.tagName )

}
(node.children).foreach { fresh92 =>
val child = zeroOfMyType
 = fresh92
 {
 child.kind match {
  case  SyntaxKind.JsxExpression  =>
checkJsxExpression( child.asInstanceOf[JsxExpression] )
  case  SyntaxKind.JsxElement  =>
checkJsxElement( child.asInstanceOf[JsxElement] )
  case  SyntaxKind.JsxSelfClosingElement  =>
checkJsxSelfClosingElement( child.asInstanceOf[JsxSelfClosingElement] )
  case _ =>
}

}
}
return (jsxElementType||anyType)

}
def isUnhyphenatedJsxName(name: String) = {
 return (name.indexOf( "-" )<0)

}
def isJsxIntrinsicIdentifier(tagName: JsxTagNameExpression) = {
 if (((tagName.kind===SyntaxKind.PropertyAccessExpression)||(tagName.kind===SyntaxKind.ThisKeyword))) {
 return false

}
else {
 return isIntrinsicJsxName( (tagName.asInstanceOf[Identifier]).text )

}

}
def checkJsxAttribute(node: JsxAttribute, elementAttributesType: Type, nameTable: Map[Boolean]) = {
 var correspondingPropType: Type = undefined
if (((elementAttributesType===emptyObjectType)&&isUnhyphenatedJsxName( node.name.text ))) {
 error( node.parent, Diagnostics.JSX_element_class_does_not_support_attributes_because_it_does_not_have_a_0_property, getJsxElementPropertiesName() )

}
else if ((elementAttributesType&&(!isTypeAny( elementAttributesType )))) {
 val correspondingPropSymbol = getPropertyOfType( elementAttributesType, node.name.text )
(correspondingPropType=(correspondingPropSymbol&&getTypeOfSymbol( correspondingPropSymbol )))
if (isUnhyphenatedJsxName( node.name.text )) {
 val attributeType = (getTypeOfPropertyOfType( elementAttributesType, getTextOfPropertyName( node.name ) )||getIndexTypeOfType( elementAttributesType, IndexKind.String ))
if (attributeType) {
 (correspondingPropType=attributeType)

}
else {
 if ((!correspondingPropType)) {
 error( node.name, Diagnostics.Property_0_does_not_exist_on_type_1, node.name.text, typeToString( elementAttributesType ) )
return unknownType

}

}

}

}
var exprType: Type = zeroOfMyType
if (node.initializer) {
 (exprType=checkExpression( node.initializer ))

}
else {
 (exprType=booleanType)

}
if (correspondingPropType) {
 checkTypeAssignableTo( exprType, correspondingPropType, node )

}
(nameTable(node.name.text)=true)
return exprType

}
def checkJsxSpreadAttribute(node: JsxSpreadAttribute, elementAttributesType: Type, nameTable: Map[Boolean]) = {
 val `type` = checkExpression( node.expression )
val props = getPropertiesOfType( `type` )
(props).foreach { fresh93 =>
val prop = zeroOfMyType
 = fresh93
 {
 if ((!nameTable(prop.name))) {
 val targetPropSym = getPropertyOfType( elementAttributesType, prop.name )
if (targetPropSym) {
 val msg = chainDiagnosticMessages( undefined, Diagnostics.Property_0_of_JSX_spread_attribute_is_not_assignable_to_target_property, prop.name )
checkTypeAssignableTo( getTypeOfSymbol( prop ), getTypeOfSymbol( targetPropSym ), node, undefined, msg )

}
(nameTable(prop.name)=true)

}

}
}
return `type`

}
def getJsxType(name: String) = {
 if ((jsxTypes(name)===undefined)) {
 return (jsxTypes(name)=(getExportedTypeFromNamespace( JsxNames.JSX, name )||unknownType))

}
return jsxTypes(name)

}
def getIntrinsicTagSymbol(node: ( JsxOpeningLikeElement | JsxClosingElement )): Symbol = {
 val links = getNodeLinks( node )
if ((!links.resolvedSymbol)) {
 val intrinsicElementsType = getJsxType( JsxNames.IntrinsicElements )
if ((intrinsicElementsType!==unknownType)) {
 val intrinsicProp = getPropertyOfType( intrinsicElementsType, (node.tagName.asInstanceOf[Identifier]).text )
if (intrinsicProp) {
 (links.jsxFlags|=JsxFlags.IntrinsicNamedElement)
return (links.resolvedSymbol=intrinsicProp)

}
val indexSignatureType = getIndexTypeOfType( intrinsicElementsType, IndexKind.String )
if (indexSignatureType) {
 (links.jsxFlags|=JsxFlags.IntrinsicIndexedElement)
return (links.resolvedSymbol=intrinsicElementsType.symbol)

}
error( node, Diagnostics.Property_0_does_not_exist_on_type_1, (node.tagName.asInstanceOf[Identifier]).text, ("JSX."+JsxNames.IntrinsicElements) )
return (links.resolvedSymbol=unknownSymbol)

}
else {
 if (compilerOptions.noImplicitAny) {
 error( node, Diagnostics.JSX_element_implicitly_has_type_any_because_no_interface_JSX_0_exists, JsxNames.IntrinsicElements )

}
return (links.resolvedSymbol=unknownSymbol)

}

}
return links.resolvedSymbol

}
def getJsxElementInstanceType(node: JsxOpeningLikeElement, valueType: Type) = {
 Debug.assert( (!((valueType.flags&TypeFlags.Union))) )
if (isTypeAny( valueType )) {
 return anyType

}
var signatures = getSignaturesOfType( valueType, SignatureKind.Construct )
if ((signatures.length===0)) {
 (signatures=getSignaturesOfType( valueType, SignatureKind.Call ))
if ((signatures.length===0)) {
 error( node.tagName, Diagnostics.JSX_element_type_0_does_not_have_any_construct_or_call_signatures, getTextOfNode( node.tagName ) )
return unknownType

}

}
return getUnionType( map( signatures, getReturnTypeOfSignature ), true )

}
def getJsxElementPropertiesName() = {
 val jsxNamespace = getGlobalSymbol( JsxNames.JSX, SymbolFlags.Namespace, undefined )
val attribsPropTypeSym = (jsxNamespace&&getSymbol( jsxNamespace.exports, JsxNames.ElementAttributesPropertyNameContainer, SymbolFlags.Type ))
val attribPropType = (attribsPropTypeSym&&getDeclaredTypeOfSymbol( attribsPropTypeSym ))
val attribProperties = (attribPropType&&getPropertiesOfType( attribPropType ))
if (attribProperties) {
 if ((attribProperties.length===0)) {
 return ""

}
else if ((attribProperties.length===1)) {
 return attribProperties(0).name

}
else {
 error( attribsPropTypeSym.declarations(0), Diagnostics.The_global_type_JSX_0_may_not_have_more_than_one_property, JsxNames.ElementAttributesPropertyNameContainer )
return undefined

}

}
else {
 return undefined

}

}
def getResolvedJsxType(node: JsxOpeningLikeElement, elemType: Type, elemClassType: Type): Type = {
 if ((!elemType)) {
 (elemType=checkExpression( node.tagName ))

}
if ((elemType.flags&TypeFlags.Union)) {
 val types = (elemType.asInstanceOf[UnionOrIntersectionType]).types
return getUnionType( map( types, (`type` =>  {
 return getResolvedJsxType( node, `type`, elemClassType )

}) ), true )

}
if ((elemType.flags&TypeFlags.String)) {
 return anyType

}
else if ((elemType.flags&TypeFlags.StringLiteral)) {
 val intrinsicElementsType = getJsxType( JsxNames.IntrinsicElements )
if ((intrinsicElementsType!==unknownType)) {
 val stringLiteralTypeName = (elemType.asInstanceOf[LiteralType]).text
val intrinsicProp = getPropertyOfType( intrinsicElementsType, stringLiteralTypeName )
if (intrinsicProp) {
 return getTypeOfSymbol( intrinsicProp )

}
val indexSignatureType = getIndexTypeOfType( intrinsicElementsType, IndexKind.String )
if (indexSignatureType) {
 return indexSignatureType

}
error( node, Diagnostics.Property_0_does_not_exist_on_type_1, stringLiteralTypeName, ("JSX."+JsxNames.IntrinsicElements) )

}
return anyType

}
val elemInstanceType = getJsxElementInstanceType( node, elemType )
if (((!elemClassType)||(!isTypeAssignableTo( elemInstanceType, elemClassType )))) {
 if (jsxElementType) {
 val callSignatures = (elemType&&getSignaturesOfType( elemType, SignatureKind.Call ))
val callSignature = ((callSignatures&&(callSignatures.length>0))&&callSignatures(0))
val callReturnType = (callSignature&&getReturnTypeOfSignature( callSignature ))
var paramType = (callReturnType&&((if ((callSignature.parameters.length===0)) emptyObjectType else getTypeOfSymbol( callSignature.parameters(0) ))))
if ((callReturnType&&isTypeAssignableTo( callReturnType, jsxElementType ))) {
 val intrinsicAttributes = getJsxType( JsxNames.IntrinsicAttributes )
if ((intrinsicAttributes!==unknownType)) {
 (paramType=intersectTypes( intrinsicAttributes, paramType ))

}
return paramType

}

}

}
if (elemClassType) {
 checkTypeRelatedTo( elemInstanceType, elemClassType, assignableRelation, node, Diagnostics.JSX_element_type_0_is_not_a_constructor_function_for_JSX_elements )

}
if (isTypeAny( elemInstanceType )) {
 return elemInstanceType

}
val propsName = getJsxElementPropertiesName()
if ((propsName===undefined)) {
 return anyType

}
else if ((propsName==="")) {
 return elemInstanceType

}
else {
 val attributesType = getTypeOfPropertyOfType( elemInstanceType, propsName )
if ((!attributesType)) {
 return emptyObjectType

}
else if ((isTypeAny( attributesType )||((attributesType===unknownType)))) {
 return attributesType

}
else if ((attributesType.flags&TypeFlags.Union)) {
 error( node.tagName, Diagnostics.JSX_element_attributes_type_0_may_not_be_a_union_type, typeToString( attributesType ) )
return anyType

}
else {
 var apparentAttributesType = attributesType
val intrinsicClassAttribs = getJsxType( JsxNames.IntrinsicClassAttributes )
if ((intrinsicClassAttribs!==unknownType)) {
 val typeParams = getLocalTypeParametersOfClassOrInterfaceOrTypeAlias( intrinsicClassAttribs.symbol )
if (typeParams) {
 if ((typeParams.length===1)) {
 (apparentAttributesType=intersectTypes( createTypeReference( intrinsicClassAttribs.asInstanceOf[GenericType], Array( elemInstanceType ) ), apparentAttributesType ))

}

}
else {
 (apparentAttributesType=intersectTypes( attributesType, intrinsicClassAttribs ))

}

}
val intrinsicAttribs = getJsxType( JsxNames.IntrinsicAttributes )
if ((intrinsicAttribs!==unknownType)) {
 (apparentAttributesType=intersectTypes( intrinsicAttribs, apparentAttributesType ))

}
return apparentAttributesType

}

}

}
def getJsxElementAttributesType(node: JsxOpeningLikeElement): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedJsxType)) {
 if (isJsxIntrinsicIdentifier( node.tagName )) {
 val symbol = getIntrinsicTagSymbol( node )
if ((links.jsxFlags&JsxFlags.IntrinsicNamedElement)) {
 return (links.resolvedJsxType=getTypeOfSymbol( symbol ))

}
else if ((links.jsxFlags&JsxFlags.IntrinsicIndexedElement)) {
 return (links.resolvedJsxType=getIndexInfoOfSymbol( symbol, IndexKind.String ).`type`)

}
else {
 return (links.resolvedJsxType=unknownType)

}

}
else {
 val elemClassType = getJsxGlobalElementClassType()
return (links.resolvedJsxType=getResolvedJsxType( node, undefined, elemClassType ))

}

}
return links.resolvedJsxType

}
def getJsxAttributePropertySymbol(attrib: JsxAttribute): Symbol = {
 val attributesType = getJsxElementAttributesType( attrib.parent.asInstanceOf[JsxOpeningElement] )
val prop = getPropertyOfType( attributesType, attrib.name.text )
return (prop||unknownSymbol)

}
def getJsxGlobalElementClassType(): Type = {
 if ((!jsxElementClassType)) {
 (jsxElementClassType=getExportedTypeFromNamespace( JsxNames.JSX, JsxNames.ElementClass ))

}
return jsxElementClassType

}
def getJsxIntrinsicTagNames(): Array[Symbol] = {
 val intrinsics = getJsxType( JsxNames.IntrinsicElements )
return (if (intrinsics) getPropertiesOfType( intrinsics ) else emptyArray)

}
def checkJsxPreconditions(errorNode: Node) = {
 if ((((compilerOptions.jsx||JsxEmit.None))===JsxEmit.None)) {
 error( errorNode, Diagnostics.Cannot_use_JSX_unless_the_jsx_flag_is_provided )

}
if ((jsxElementType===undefined)) {
 if (compilerOptions.noImplicitAny) {
 error( errorNode, Diagnostics.JSX_element_implicitly_has_type_any_because_the_global_type_JSX_Element_does_not_exist )

}

}

}
def checkJsxOpeningLikeElement(node: JsxOpeningLikeElement) = {
 checkGrammarJsxElement( node )
checkJsxPreconditions( node )
val reactRefErr = (if ((compilerOptions.jsx===JsxEmit.React)) Diagnostics.Cannot_find_name_0 else undefined)
val reactNamespace = (if (compilerOptions.reactNamespace) compilerOptions.reactNamespace else "React")
val reactSym = resolveName( node.tagName, reactNamespace, SymbolFlags.Value, reactRefErr, reactNamespace )
if (reactSym) {
 (reactSym.isReferenced=true)
if (((reactSym.flags&SymbolFlags.Alias)&&(!isConstEnumOrConstEnumOnlyModule( resolveAlias( reactSym ) )))) {
 markAliasSymbolAsReferenced( reactSym )

}

}
val targetAttributesType = getJsxElementAttributesType( node )
val nameTable = createMap[ Boolean ]()
var sawSpreadedAny = false
{
var i = (node.attributes.length-1)
while( (i>=0)) {
 {
 if ((node.attributes(i).kind===SyntaxKind.JsxAttribute)) {
 checkJsxAttribute( (node.attributes(i)).asInstanceOf[JsxAttribute], targetAttributesType, nameTable )

}
else {
 Debug.assert( (node.attributes(i).kind===SyntaxKind.JsxSpreadAttribute) )
val spreadType = checkJsxSpreadAttribute( (node.attributes(i)).asInstanceOf[JsxSpreadAttribute], targetAttributesType, nameTable )
if (isTypeAny( spreadType )) {
 (sawSpreadedAny=true)

}

}

}
 (i-= 1)
}
}
if ((targetAttributesType&&(!sawSpreadedAny))) {
 val targetProperties = getPropertiesOfType( targetAttributesType )
{
var i = 0
while( (i<targetProperties.length)) {
 {
 if (((!((targetProperties(i).flags&SymbolFlags.Optional)))&&(!nameTable(targetProperties(i).name)))) {
 error( node, Diagnostics.Property_0_is_missing_in_type_1, targetProperties(i).name, typeToString( targetAttributesType ) )

}

}
 (i+= 1)
}
}

}

}
def checkJsxExpression(node: JsxExpression) = {
 if (node.expression) {
 return checkExpression( node.expression )

}
else {
 return unknownType

}

}
def getDeclarationKindFromSymbol(s: Symbol) = {
 return (if (s.valueDeclaration) s.valueDeclaration.kind else SyntaxKind.PropertyDeclaration)

}
def getDeclarationModifierFlagsFromSymbol(s: Symbol): ModifierFlags = {
 return (if (s.valueDeclaration) getCombinedModifierFlags( s.valueDeclaration ) else (if ((s.flags&SymbolFlags.Prototype)) (ModifierFlags.Public|ModifierFlags.Static) else 0))

}
def getDeclarationNodeFlagsFromSymbol(s: Symbol): NodeFlags = {
 return (if (s.valueDeclaration) getCombinedNodeFlags( s.valueDeclaration ) else 0)

}
def checkClassPropertyAccess(node: ( PropertyAccessExpression | QualifiedName | VariableLikeDeclaration ), left: ( Expression | QualifiedName ), `type`: Type, prop: Symbol): Boolean = {
 val flags = getDeclarationModifierFlagsFromSymbol( prop )
val declaringClass = getDeclaredTypeOfSymbol( getParentOfSymbol( prop ) ).asInstanceOf[InterfaceType]
val errorNode = (if (((node.kind===SyntaxKind.PropertyAccessExpression)||(node.kind===SyntaxKind.VariableDeclaration))) (node.asInstanceOf[( PropertyAccessExpression | VariableDeclaration )]).name else (node.asInstanceOf[QualifiedName]).right)
if ((left.kind===SyntaxKind.SuperKeyword)) {
 if (((languageVersion<ScriptTarget.ES2015)&&(getDeclarationKindFromSymbol( prop )!==SyntaxKind.MethodDeclaration))) {
 error( errorNode, Diagnostics.Only_public_and_protected_methods_of_the_base_class_are_accessible_via_the_super_keyword )
return false

}
if ((flags&ModifierFlags.Abstract)) {
 error( errorNode, Diagnostics.Abstract_method_0_in_class_1_cannot_be_accessed_via_super_expression, symbolToString( prop ), typeToString( declaringClass ) )
return false

}

}
if ((!((flags&ModifierFlags.NonPublicAccessibilityModifier)))) {
 return true

}
if ((flags&ModifierFlags.Private)) {
 val declaringClassDeclaration = getClassLikeDeclarationOfSymbol( getParentOfSymbol( prop ) ).asInstanceOf[ClassLikeDeclaration]
if ((!isNodeWithinClass( node, declaringClassDeclaration ))) {
 error( errorNode, Diagnostics.Property_0_is_private_and_only_accessible_within_class_1, symbolToString( prop ), typeToString( declaringClass ) )
return false

}
return true

}
if ((left.kind===SyntaxKind.SuperKeyword)) {
 return true

}
val enclosingClass = forEachEnclosingClass( node, (enclosingDeclaration =>  {
 val enclosingClass = getDeclaredTypeOfSymbol( getSymbolOfNode( enclosingDeclaration ) ).asInstanceOf[InterfaceType]
 return (if (hasBaseType( enclosingClass, declaringClass )) enclosingClass else undefined)

}) )
if ((!enclosingClass)) {
 error( errorNode, Diagnostics.Property_0_is_protected_and_only_accessible_within_class_1_and_its_subclasses, symbolToString( prop ), typeToString( declaringClass ) )
return false

}
if ((flags&ModifierFlags.Static)) {
 return true

}
if (((`type`.flags&TypeFlags.TypeParameter)&&(`type`.asInstanceOf[TypeParameter]).isThisType)) {
 (`type`=getConstraintOfTypeParameter( `type`.asInstanceOf[TypeParameter] ))

}
if ((!(((getObjectFlags( getTargetType( `type` ) )&ObjectFlags.ClassOrInterface)&&hasBaseType( `type`.asInstanceOf[InterfaceType], enclosingClass ))))) {
 error( errorNode, Diagnostics.Property_0_is_protected_and_only_accessible_through_an_instance_of_class_1, symbolToString( prop ), typeToString( enclosingClass ) )
return false

}
return true

}
def checkNonNullExpression(node: ( Expression | QualifiedName )) = {
 val `type` = checkExpression( node )
if (strictNullChecks) {
 val kind = (getFalsyFlags( `type` )&TypeFlags.Nullable)
if (kind) {
 error( node, (if ((kind&TypeFlags.Undefined)) (if ((kind&TypeFlags.Null)) Diagnostics.Object_is_possibly_null_or_undefined else Diagnostics.Object_is_possibly_undefined) else Diagnostics.Object_is_possibly_null) )

}
return getNonNullableType( `type` )

}
return `type`

}
def checkPropertyAccessExpression(node: PropertyAccessExpression) = {
 return checkPropertyAccessExpressionOrQualifiedName( node, node.expression, node.name )

}
def checkQualifiedName(node: QualifiedName) = {
 return checkPropertyAccessExpressionOrQualifiedName( node, node.left, node.right )

}
def checkPropertyAccessExpressionOrQualifiedName(node: ( PropertyAccessExpression | QualifiedName ), left: ( Expression | QualifiedName ), right: Identifier) = {
 val `type` = checkNonNullExpression( left )
if ((isTypeAny( `type` )||(`type`===silentNeverType))) {
 return `type`

}
val apparentType = getApparentType( getWidenedType( `type` ) )
if (((apparentType===unknownType)||(((`type`.flags&TypeFlags.TypeParameter)&&isTypeAny( apparentType ))))) {
 return apparentType

}
val prop = getPropertyOfType( apparentType, right.text )
if ((!prop)) {
 if ((right.text&&(!checkAndReportErrorForExtendingInterface( node )))) {
 reportNonexistentProperty( right, (if (((`type`.flags&TypeFlags.TypeParameter)&&(`type`.asInstanceOf[TypeParameter]).isThisType)) apparentType else `type`) )

}
return unknownType

}
if ((((noUnusedIdentifiers&&((prop.flags&SymbolFlags.ClassMember)))&&prop.valueDeclaration)&&((getModifierFlags( prop.valueDeclaration )&ModifierFlags.Private)))) {
 if ((prop.flags&SymbolFlags.Instantiated)) {
 (getSymbolLinks( prop ).target.isReferenced=true)

}
else {
 (prop.isReferenced=true)

}

}
(getNodeLinks( node ).resolvedSymbol=prop)
if ((prop.parent&&(prop.parent.flags&SymbolFlags.Class))) {
 checkClassPropertyAccess( node, left, apparentType, prop )

}
val propType = getTypeOfSymbol( prop )
if ((((node.kind!==SyntaxKind.PropertyAccessExpression)||isAssignmentTarget( node ))||((!((prop.flags&(((SymbolFlags.Variable|SymbolFlags.Property)|SymbolFlags.Accessor)))))&&(!(((prop.flags&SymbolFlags.Method)&&(propType.flags&TypeFlags.Union))))))) {
 return propType

}
return getFlowTypeOfReference( node, propType, true, undefined )
def reportNonexistentProperty(propNode: Identifier, containingType: Type) = {
 var errorInfo: DiagnosticMessageChain = zeroOfMyType
if (((containingType.flags&TypeFlags.Union)&&(!((containingType.flags&TypeFlags.Primitive))))) {
 ((containingType.asInstanceOf[UnionType]).types).foreach { fresh94 =>
val subtype = zeroOfMyType
 = fresh94
 {
 if ((!getPropertyOfType( subtype, propNode.text ))) {
 (errorInfo=chainDiagnosticMessages( errorInfo, Diagnostics.Property_0_does_not_exist_on_type_1, declarationNameToString( propNode ), typeToString( subtype ) ))
break()

}

}
}

}
(errorInfo=chainDiagnosticMessages( errorInfo, Diagnostics.Property_0_does_not_exist_on_type_1, declarationNameToString( propNode ), typeToString( containingType ) ))
diagnostics.add( createDiagnosticForNodeFromMessageChain( propNode, errorInfo ) )

}

}
def isValidPropertyAccess(node: ( PropertyAccessExpression | QualifiedName ), propertyName: String): Boolean = {
 val left = (if ((node.kind===SyntaxKind.PropertyAccessExpression)) (node.asInstanceOf[PropertyAccessExpression]).expression else (node.asInstanceOf[QualifiedName]).left)
val `type` = checkExpression( left )
if (((`type`!==unknownType)&&(!isTypeAny( `type` )))) {
 val prop = getPropertyOfType( getWidenedType( `type` ), propertyName )
if (((prop&&prop.parent)&&(prop.parent.flags&SymbolFlags.Class))) {
 return checkClassPropertyAccess( node, left, `type`, prop )

}

}
return true

}
def getForInVariableSymbol(node: ForInStatement): Symbol = {
 val initializer = node.initializer
if ((initializer.kind===SyntaxKind.VariableDeclarationList)) {
 val variable = (initializer.asInstanceOf[VariableDeclarationList]).declarations(0)
if ((variable&&(!isBindingPattern( variable.name )))) {
 return getSymbolOfNode( variable )

}

}
else if ((initializer.kind===SyntaxKind.Identifier)) {
 return getResolvedSymbol( initializer.asInstanceOf[Identifier] )

}
return undefined

}
def hasNumericPropertyNames(`type`: Type) = {
 return (getIndexTypeOfType( `type`, IndexKind.Number )&&(!getIndexTypeOfType( `type`, IndexKind.String )))

}
def isForInVariableForNumericPropertyNames(expr: Expression) = {
 val e = skipParenthesizedNodes( expr )
if ((e.kind===SyntaxKind.Identifier)) {
 val symbol = getResolvedSymbol( e.asInstanceOf[Identifier] )
if ((symbol.flags&SymbolFlags.Variable)) {
 var child: Node = expr
var node = expr.parent
while (node) {
{
 if (((((node.kind===SyntaxKind.ForInStatement)&&(child===(node.asInstanceOf[ForInStatement]).statement))&&(getForInVariableSymbol( node.asInstanceOf[ForInStatement] )===symbol))&&hasNumericPropertyNames( checkExpression( (node.asInstanceOf[ForInStatement]).expression ) ))) {
 return true

}
(child=node)
(node=node.parent)

}
}

}

}
return false

}
def checkIndexedAccess(node: ElementAccessExpression): Type = {
 if ((!node.argumentExpression)) {
 val sourceFile = getSourceFileOfNode( node )
if (((node.parent.kind===SyntaxKind.NewExpression)&&((node.parent.asInstanceOf[NewExpression]).expression===node))) {
 val start = skipTrivia( sourceFile.text, node.expression.end )
val end = node.end
grammarErrorAtPos( sourceFile, start, (end-start), Diagnostics.new_T_cannot_be_used_to_create_an_array_Use_new_Array_T_instead )

}
else {
 val start = (node.end-"]".length)
val end = node.end
grammarErrorAtPos( sourceFile, start, (end-start), Diagnostics.Expression_expected )

}

}
val objectType = getApparentType( checkNonNullExpression( node.expression ) )
val indexType = (if (node.argumentExpression) checkExpression( node.argumentExpression ) else unknownType)
if (((objectType===unknownType)||(objectType===silentNeverType))) {
 return objectType

}
val isConstEnum = isConstEnumObjectType( objectType )
if ((isConstEnum&&(((!node.argumentExpression)||(node.argumentExpression.kind!==SyntaxKind.StringLiteral))))) {
 error( node.argumentExpression, Diagnostics.A_const_enum_member_can_only_be_accessed_using_a_string_literal )
return unknownType

}
if (node.argumentExpression) {
 val name = getPropertyNameForIndexedAccess( node.argumentExpression, indexType )
if ((name!==undefined)) {
 val prop = getPropertyOfType( objectType, name )
if (prop) {
 (getNodeLinks( node ).resolvedSymbol=prop)
return getTypeOfSymbol( prop )

}
else if (isConstEnum) {
 error( node.argumentExpression, Diagnostics.Property_0_does_not_exist_on_const_enum_1, name, symbolToString( objectType.symbol ) )
return unknownType

}

}

}
val allowedNullableFlags = (if (strictNullChecks) 0 else TypeFlags.Nullable)
if (isTypeAnyOrAllConstituentTypesHaveKind( indexType, (((TypeFlags.StringLike|TypeFlags.NumberLike)|TypeFlags.ESSymbol)|allowedNullableFlags) )) {
 if ((isTypeAnyOrAllConstituentTypesHaveKind( indexType, (TypeFlags.NumberLike|allowedNullableFlags) )||isForInVariableForNumericPropertyNames( node.argumentExpression ))) {
 val numberIndexInfo = getIndexInfoOfType( objectType, IndexKind.Number )
if (numberIndexInfo) {
 (getNodeLinks( node ).resolvedIndexInfo=numberIndexInfo)
return numberIndexInfo.`type`

}

}
val stringIndexInfo = getIndexInfoOfType( objectType, IndexKind.String )
if (stringIndexInfo) {
 (getNodeLinks( node ).resolvedIndexInfo=stringIndexInfo)
return stringIndexInfo.`type`

}
if (((compilerOptions.noImplicitAny&&(!compilerOptions.suppressImplicitAnyIndexErrors))&&(!isTypeAny( objectType )))) {
 error( node, (if (getIndexTypeOfType( objectType, IndexKind.Number )) Diagnostics.Element_implicitly_has_an_any_type_because_index_expression_is_not_of_type_number else Diagnostics.Index_signature_of_object_type_implicitly_has_an_any_type) )

}
return anyType

}
error( node, Diagnostics.An_index_expression_argument_must_be_of_type_string_number_symbol_or_any )
return unknownType

}
def getPropertyNameForIndexedAccess(indexArgumentExpression: Expression, indexArgumentType: Type): String = {
 if (((indexArgumentExpression.kind===SyntaxKind.StringLiteral)||(indexArgumentExpression.kind===SyntaxKind.NumericLiteral))) {
 return (indexArgumentExpression.asInstanceOf[LiteralExpression]).text

}
if (((indexArgumentExpression.kind===SyntaxKind.ElementAccessExpression)||(indexArgumentExpression.kind===SyntaxKind.PropertyAccessExpression))) {
 val value = getConstantValue( indexArgumentExpression.asInstanceOf[( ElementAccessExpression | PropertyAccessExpression )] )
if ((value!==undefined)) {
 return value.`toString`()

}

}
if (checkThatExpressionIsProperSymbolReference( indexArgumentExpression, indexArgumentType, false )) {
 val rightHandSideName = ((indexArgumentExpression.asInstanceOf[PropertyAccessExpression]).name.asInstanceOf[Identifier]).text
return getPropertyNameForKnownSymbolName( rightHandSideName )

}
return undefined

}
def checkThatExpressionIsProperSymbolReference(expression: Expression, expressionType: Type, reportError: Boolean): Boolean = {
 if ((expressionType===unknownType)) {
 return false

}
if ((!isWellKnownSymbolSyntactically( expression ))) {
 return false

}
if ((((expressionType.flags&TypeFlags.ESSymbol))===0)) {
 if (reportError) {
 error( expression, Diagnostics.A_computed_property_name_of_the_form_0_must_be_of_type_symbol, getTextOfNode( expression ) )

}
return false

}
val leftHandSide = (expression.asInstanceOf[PropertyAccessExpression]).expression.asInstanceOf[Identifier]
val leftHandSideSymbol = getResolvedSymbol( leftHandSide )
if ((!leftHandSideSymbol)) {
 return false

}
val globalESSymbol = getGlobalESSymbolConstructorSymbol()
if ((!globalESSymbol)) {
 return false

}
if ((leftHandSideSymbol!==globalESSymbol)) {
 if (reportError) {
 error( leftHandSide, Diagnostics.Symbol_reference_does_not_refer_to_the_global_Symbol_constructor_object )

}
return false

}
return true

}
def resolveUntypedCall(node: CallLikeExpression): Signature = {
 if ((node.kind===SyntaxKind.TaggedTemplateExpression)) {
 checkExpression( (node.asInstanceOf[TaggedTemplateExpression]).template )

}
else if ((node.kind!==SyntaxKind.Decorator)) {
 forEach( (node.asInstanceOf[CallExpression]).arguments, (argument =>  {
 checkExpression( argument )

}) )

}
return anySignature

}
def resolveErrorCall(node: CallLikeExpression): Signature = {
 resolveUntypedCall( node )
return unknownSignature

}
def reorderCandidates(signatures: Array[Signature], result: Array[Signature]): Unit = {
 var lastParent: Node = zeroOfMyType
var lastSymbol: Symbol = zeroOfMyType
var cutoffIndex = 0
var index: Int = zeroOfMyType
var specializedIndex = (-1)
var spliceIndex: Int = zeroOfMyType
Debug.assert( (!result.length) )
(signatures).foreach { fresh95 =>
val signature = zeroOfMyType
 = fresh95
 {
 val symbol = (signature.declaration&&getSymbolOfNode( signature.declaration ))
val parent = (signature.declaration&&signature.declaration.parent)
if (((!lastSymbol)||(symbol===lastSymbol))) {
 if ((lastParent&&(parent===lastParent))) {
 (index+= 1)

}
else {
 (lastParent=parent)
(index=cutoffIndex)

}

}
else {
 (index=(cutoffIndex=result.length))
(lastParent=parent)

}
(lastSymbol=symbol)
if (signature.hasLiteralTypes) {
 (specializedIndex+= 1)
(spliceIndex=specializedIndex)
(cutoffIndex+= 1)

}
else {
 (spliceIndex=index)

}
result.splice( spliceIndex, 0, signature )

}
}

}
def getSpreadArgumentIndex(args: Array[Expression]): Int = {
 {
var i = 0
while( (i<args.length)) {
 {
 val arg = args(i)
if ((arg&&(arg.kind===SyntaxKind.SpreadElementExpression))) {
 return i

}

}
 (i+= 1)
}
}
return (-1)

}
def hasCorrectArity(node: CallLikeExpression, args: Array[Expression], signature: Signature, signatureHelpTrailingComma: Nothing = false) = {
 var argCount: Int = zeroOfMyType
var typeArguments: NodeArray[TypeNode] = zeroOfMyType
var callIsIncomplete: Boolean = zeroOfMyType
var isDecorator: Boolean = zeroOfMyType
var spreadArgIndex = (-1)
if ((node.kind===SyntaxKind.TaggedTemplateExpression)) {
 val tagExpression = node.asInstanceOf[TaggedTemplateExpression]
(argCount=args.length)
(typeArguments=undefined)
if ((tagExpression.template.kind===SyntaxKind.TemplateExpression)) {
 val templateExpression = tagExpression.template.asInstanceOf[TemplateExpression]
val lastSpan = lastOrUndefined( templateExpression.templateSpans )
Debug.assert( (lastSpan!==undefined) )
(callIsIncomplete=(nodeIsMissing( lastSpan.literal )||(!(!lastSpan.literal.isUnterminated))))

}
else {
 val templateLiteral = tagExpression.template.asInstanceOf[LiteralExpression]
Debug.assert( (templateLiteral.kind===SyntaxKind.NoSubstitutionTemplateLiteral) )
(callIsIncomplete=(!(!templateLiteral.isUnterminated)))

}

}
else if ((node.kind===SyntaxKind.Decorator)) {
 (isDecorator=true)
(typeArguments=undefined)
(argCount=getEffectiveArgumentCount( node, undefined, signature ))

}
else {
 val callExpression = node.asInstanceOf[( CallExpression | NewExpression )]
if ((!callExpression.arguments)) {
 Debug.assert( (callExpression.kind===SyntaxKind.NewExpression) )
return (signature.minArgumentCount===0)

}
(argCount=(if (signatureHelpTrailingComma) (args.length+1) else args.length))
(callIsIncomplete=(callExpression.arguments.end===callExpression.end))
(typeArguments=callExpression.typeArguments)
(spreadArgIndex=getSpreadArgumentIndex( args ))

}
val hasRightNumberOfTypeArgs = ((!typeArguments)||((signature.typeParameters&&(typeArguments.length===signature.typeParameters.length))))
if ((!hasRightNumberOfTypeArgs)) {
 return false

}
if ((spreadArgIndex>=0)) {
 return isRestParameterIndex( signature, spreadArgIndex )

}
if (((!signature.hasRestParameter)&&(argCount>signature.parameters.length))) {
 return false

}
val hasEnoughArguments = (argCount>=signature.minArgumentCount)
return (callIsIncomplete||hasEnoughArguments)

}
def getSingleCallSignature(`type`: Type): Signature = {
 if ((`type`.flags&TypeFlags.Object)) {
 val resolved = resolveStructuredTypeMembers( `type`.asInstanceOf[ObjectType] )
if ((((((resolved.callSignatures.length===1)&&(resolved.constructSignatures.length===0))&&(resolved.properties.length===0))&&(!resolved.stringIndexInfo))&&(!resolved.numberIndexInfo))) {
 return resolved.callSignatures(0)

}

}
return undefined

}
def instantiateSignatureInContextOf(signature: Signature, contextualSignature: Signature, contextualMapper: TypeMapper): Signature = {
 val context = createInferenceContext( signature, true )
forEachMatchingParameterType( contextualSignature, signature, (( source, target ) =>  {
 inferTypes( context, instantiateType( source, contextualMapper ), target )

}) )
return getSignatureInstantiation( signature, getInferredTypes( context ) )

}
def inferTypeArguments(node: CallLikeExpression, signature: Signature, args: Array[Expression], excludeArgument: Array[Boolean], context: InferenceContext): Unit = {
 val typeParameters = signature.typeParameters
val inferenceMapper = getInferenceMapper( context )
{
var i = 0
while( (i<typeParameters.length)) {
 {
 if ((!context.inferences(i).isFixed)) {
 (context.inferredTypes(i)=undefined)

}

}
 (i+= 1)
}
}
if (((context.failedTypeParameterIndex!==undefined)&&(!context.inferences(context.failedTypeParameterIndex).isFixed))) {
 (context.failedTypeParameterIndex=undefined)

}
val thisType = getThisTypeOfSignature( signature )
if (thisType) {
 val thisArgumentNode = getThisArgumentOfCall( node )
val thisArgumentType = (if (thisArgumentNode) checkExpression( thisArgumentNode ) else voidType)
inferTypes( context, thisArgumentType, thisType )

}
val argCount = getEffectiveArgumentCount( node, args, signature )
{
var i = 0
while( (i<argCount)) {
 {
 val arg = getEffectiveArgument( node, args, i )
if (((arg===undefined)||(arg.kind!==SyntaxKind.OmittedExpression))) {
 val paramType = getTypeAtPosition( signature, i )
var argType = getEffectiveArgumentType( node, i )
if ((argType===undefined)) {
 val mapper = (if ((excludeArgument&&(excludeArgument(i)!==undefined))) identityMapper else inferenceMapper)
(argType=checkExpressionWithContextualType( arg, paramType, mapper ))

}
inferTypes( context, argType, paramType )

}

}
 (i+= 1)
}
}
if (excludeArgument) {
 {
var i = 0
while( (i<argCount)) {
 {
 if ((excludeArgument(i)===false)) {
 val arg = args(i)
val paramType = getTypeAtPosition( signature, i )
inferTypes( context, checkExpressionWithContextualType( arg, paramType, inferenceMapper ), paramType )

}

}
 (i+= 1)
}
}

}
getInferredTypes( context )

}
def checkTypeArguments(signature: Signature, typeArgumentNodes: Array[TypeNode], typeArgumentTypes: Array[Type], reportErrors: Boolean, headMessage: DiagnosticMessage): Boolean = {
 val typeParameters = signature.typeParameters
var typeArgumentsAreAssignable = true
var mapper: TypeMapper = zeroOfMyType
{
var i = 0
while( (i<typeParameters.length)) {
 {
 if (typeArgumentsAreAssignable) {
 val constraint = getConstraintOfTypeParameter( typeParameters(i) )
if (constraint) {
 var errorInfo: DiagnosticMessageChain = zeroOfMyType
var typeArgumentHeadMessage = Diagnostics.Type_0_does_not_satisfy_the_constraint_1
if ((reportErrors&&headMessage)) {
 (errorInfo=chainDiagnosticMessages( errorInfo, typeArgumentHeadMessage ))
(typeArgumentHeadMessage=headMessage)

}
if ((!mapper)) {
 (mapper=createTypeMapper( typeParameters, typeArgumentTypes ))

}
val typeArgument = typeArgumentTypes(i)
(typeArgumentsAreAssignable=checkTypeAssignableTo( typeArgument, getTypeWithThisArgument( instantiateType( constraint, mapper ), typeArgument ), (if (reportErrors) typeArgumentNodes(i) else undefined), typeArgumentHeadMessage, errorInfo ))

}

}

}
 (i+= 1)
}
}
return typeArgumentsAreAssignable

}
def checkApplicableSignature(node: CallLikeExpression, args: Array[Expression], signature: Signature, relation: Map[RelationComparisonResult], excludeArgument: Array[Boolean], reportErrors: Boolean) = {
 val thisType = getThisTypeOfSignature( signature )
if (((thisType&&(thisType!==voidType))&&(node.kind!==SyntaxKind.NewExpression))) {
 val thisArgumentNode = getThisArgumentOfCall( node )
val thisArgumentType = (if (thisArgumentNode) checkExpression( thisArgumentNode ) else voidType)
val errorNode = (if (reportErrors) ((thisArgumentNode||node)) else undefined)
val headMessage = Diagnostics.The_this_context_of_type_0_is_not_assignable_to_method_s_this_of_type_1
if ((!checkTypeRelatedTo( thisArgumentType, getThisTypeOfSignature( signature ), relation, errorNode, headMessage ))) {
 return false

}

}
val headMessage = Diagnostics.Argument_of_type_0_is_not_assignable_to_parameter_of_type_1
val argCount = getEffectiveArgumentCount( node, args, signature )
{
var i = 0
while( (i<argCount)) {
 {
 val arg = getEffectiveArgument( node, args, i )
if (((arg===undefined)||(arg.kind!==SyntaxKind.OmittedExpression))) {
 val paramType = getTypeAtPosition( signature, i )
var argType = getEffectiveArgumentType( node, i )
if ((argType===undefined)) {
 (argType=checkExpressionWithContextualType( arg, paramType, (if ((excludeArgument&&excludeArgument(i))) identityMapper else undefined) ))

}
val errorNode = (if (reportErrors) getEffectiveArgumentErrorNode( node, i, arg ) else undefined)
if ((!checkTypeRelatedTo( argType, paramType, relation, errorNode, headMessage ))) {
 return false

}

}

}
 (i+= 1)
}
}
return true

}
def getThisArgumentOfCall(node: CallLikeExpression): LeftHandSideExpression = {
 if ((node.kind===SyntaxKind.CallExpression)) {
 val callee = (node.asInstanceOf[CallExpression]).expression
if ((callee.kind===SyntaxKind.PropertyAccessExpression)) {
 return (callee.asInstanceOf[PropertyAccessExpression]).expression

}
else if ((callee.kind===SyntaxKind.ElementAccessExpression)) {
 return (callee.asInstanceOf[ElementAccessExpression]).expression

}

}

}
def getEffectiveCallArguments(node: CallLikeExpression): Array[Expression] = {
 var args: Array[Expression] = zeroOfMyType
if ((node.kind===SyntaxKind.TaggedTemplateExpression)) {
 val template = (node.asInstanceOf[TaggedTemplateExpression]).template
(args=Array( undefined ))
if ((template.kind===SyntaxKind.TemplateExpression)) {
 forEach( (template.asInstanceOf[TemplateExpression]).templateSpans, (span =>  {
 args.push( span.expression )

}) )

}

}
else if ((node.kind===SyntaxKind.Decorator)) {
 return undefined

}
else {
 (args=((node.asInstanceOf[CallExpression]).arguments||emptyArray))

}
return args

}
def getEffectiveArgumentCount(node: CallLikeExpression, args: Array[Expression], signature: Signature) = {
 if ((node.kind===SyntaxKind.Decorator)) {
 node.parent.kind match {
  case  SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression  =>
return 1
  case  SyntaxKind.PropertyDeclaration  =>
return 2
  case  SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
if ((languageVersion===ScriptTarget.ES3)) {
 return 2

}
return (if ((signature.parameters.length>=3)) 3 else 2)
  case  SyntaxKind.Parameter  =>
return 3
  case _ =>
}

}
else {
 return args.length

}

}
def getEffectiveDecoratorFirstArgumentType(node: Node): Type = {
 if ((node.kind===SyntaxKind.ClassDeclaration)) {
 val classSymbol = getSymbolOfNode( node )
return getTypeOfSymbol( classSymbol )

}
if ((node.kind===SyntaxKind.Parameter)) {
 (node=node.parent)
if ((node.kind===SyntaxKind.Constructor)) {
 val classSymbol = getSymbolOfNode( node )
return getTypeOfSymbol( classSymbol )

}

}
if (((((node.kind===SyntaxKind.PropertyDeclaration)||(node.kind===SyntaxKind.MethodDeclaration))||(node.kind===SyntaxKind.GetAccessor))||(node.kind===SyntaxKind.SetAccessor))) {
 return getParentTypeOfClassElement( node.asInstanceOf[ClassElement] )

}
Debug.fail( "Unsupported decorator target." )
return unknownType

}
def getEffectiveDecoratorSecondArgumentType(node: Node) = {
 if ((node.kind===SyntaxKind.ClassDeclaration)) {
 Debug.fail( "Class decorators should not have a second synthetic argument." )
return unknownType

}
if ((node.kind===SyntaxKind.Parameter)) {
 (node=node.parent)
if ((node.kind===SyntaxKind.Constructor)) {
 return anyType

}

}
if (((((node.kind===SyntaxKind.PropertyDeclaration)||(node.kind===SyntaxKind.MethodDeclaration))||(node.kind===SyntaxKind.GetAccessor))||(node.kind===SyntaxKind.SetAccessor))) {
 val element = node.asInstanceOf[ClassElement]
element.name.kind match {
  case  SyntaxKind.Identifier | SyntaxKind.NumericLiteral | SyntaxKind.StringLiteral  =>
return getLiteralTypeForText( TypeFlags.StringLiteral, (element.name.asInstanceOf[( Identifier | LiteralExpression )]).text )
  case  SyntaxKind.ComputedPropertyName  =>
val nameType = checkComputedPropertyName( element.name.asInstanceOf[ComputedPropertyName] )
if (isTypeOfKind( nameType, TypeFlags.ESSymbol )) {
 return nameType

}
else {
 return stringType

}
  case _ =>
Debug.fail( "Unsupported property name." )
return unknownType
}

}
Debug.fail( "Unsupported decorator target." )
return unknownType

}
def getEffectiveDecoratorThirdArgumentType(node: Node) = {
 if ((node.kind===SyntaxKind.ClassDeclaration)) {
 Debug.fail( "Class decorators should not have a third synthetic argument." )
return unknownType

}
if ((node.kind===SyntaxKind.Parameter)) {
 return numberType

}
if ((node.kind===SyntaxKind.PropertyDeclaration)) {
 Debug.fail( "Property decorators should not have a third synthetic argument." )
return unknownType

}
if ((((node.kind===SyntaxKind.MethodDeclaration)||(node.kind===SyntaxKind.GetAccessor))||(node.kind===SyntaxKind.SetAccessor))) {
 val propertyType = getTypeOfNode( node )
return createTypedPropertyDescriptorType( propertyType )

}
Debug.fail( "Unsupported decorator target." )
return unknownType

}
def getEffectiveDecoratorArgumentType(node: Decorator, argIndex: Int): Type = {
 if ((argIndex===0)) {
 return getEffectiveDecoratorFirstArgumentType( node.parent )

}
else if ((argIndex===1)) {
 return getEffectiveDecoratorSecondArgumentType( node.parent )

}
else if ((argIndex===2)) {
 return getEffectiveDecoratorThirdArgumentType( node.parent )

}
Debug.fail( "Decorators should not have a fourth synthetic argument." )
return unknownType

}
def getEffectiveArgumentType(node: CallLikeExpression, argIndex: Int): Type = {
 if ((node.kind===SyntaxKind.Decorator)) {
 return getEffectiveDecoratorArgumentType( node.asInstanceOf[Decorator], argIndex )

}
else if (((argIndex===0)&&(node.kind===SyntaxKind.TaggedTemplateExpression))) {
 return getGlobalTemplateStringsArrayType()

}
return undefined

}
def getEffectiveArgument(node: CallLikeExpression, args: Array[Expression], argIndex: Int) = {
 if (((node.kind===SyntaxKind.Decorator)||(((argIndex===0)&&(node.kind===SyntaxKind.TaggedTemplateExpression))))) {
 return undefined

}
return args(argIndex)

}
def getEffectiveArgumentErrorNode(node: CallLikeExpression, argIndex: Int, arg: Expression) = {
 if ((node.kind===SyntaxKind.Decorator)) {
 return (node.asInstanceOf[Decorator]).expression

}
else if (((argIndex===0)&&(node.kind===SyntaxKind.TaggedTemplateExpression))) {
 return (node.asInstanceOf[TaggedTemplateExpression]).template

}
else {
 return arg

}

}
def resolveCall(node: CallLikeExpression, signatures: Array[Signature], candidatesOutArray: Array[Signature], headMessage: DiagnosticMessage): Signature = {
 val isTaggedTemplate = (node.kind===SyntaxKind.TaggedTemplateExpression)
val isDecorator = (node.kind===SyntaxKind.Decorator)
var typeArguments: Array[TypeNode] = zeroOfMyType
if (((!isTaggedTemplate)&&(!isDecorator))) {
 (typeArguments=(node.asInstanceOf[CallExpression]).typeArguments)
if (((node.asInstanceOf[CallExpression]).expression.kind!==SyntaxKind.SuperKeyword)) {
 forEach( typeArguments, checkSourceElement )

}

}
val candidates = (candidatesOutArray||Array())
reorderCandidates( signatures, candidates )
if ((!candidates.length)) {
 reportError( Diagnostics.Supplied_parameters_do_not_match_any_signature_of_call_target )
return resolveErrorCall( node )

}
val args = getEffectiveCallArguments( node )
var excludeArgument: Array[Boolean] = zeroOfMyType
if ((!isDecorator)) {
 {
var i = (if (isTaggedTemplate) 1 else 0)
while( (i<args.length)) {
 {
 if (isContextSensitive( args(i) )) {
 if ((!excludeArgument)) {
 (excludeArgument=new Array( args.length ))

}
(excludeArgument(i)=true)

}

}
 (i+= 1)
}
}

}
var candidateForArgumentError: Signature = zeroOfMyType
var candidateForTypeArgumentError: Signature = zeroOfMyType
var resultOfFailedInference: InferenceContext = zeroOfMyType
var result: Signature = zeroOfMyType
val signatureHelpTrailingComma = ((candidatesOutArray&&(node.kind===SyntaxKind.CallExpression))&&(node.asInstanceOf[CallExpression]).arguments.hasTrailingComma)
if ((candidates.length>1)) {
 (result=chooseOverload( candidates, subtypeRelation, signatureHelpTrailingComma ))

}
if ((!result)) {
 (candidateForArgumentError=undefined)
(candidateForTypeArgumentError=undefined)
(resultOfFailedInference=undefined)
(result=chooseOverload( candidates, assignableRelation, signatureHelpTrailingComma ))

}
if (result) {
 return result

}
if (candidateForArgumentError) {
 checkApplicableSignature( node, args, candidateForArgumentError, assignableRelation, undefined, true )

}
else if (candidateForTypeArgumentError) {
 if ((((!isTaggedTemplate)&&(!isDecorator))&&typeArguments)) {
 val typeArguments = (node.asInstanceOf[CallExpression]).typeArguments
checkTypeArguments( candidateForTypeArgumentError, typeArguments, map( typeArguments, getTypeFromTypeNodeNoAlias ), true, headMessage )

}
else {
 Debug.assert( (resultOfFailedInference.failedTypeParameterIndex>=0) )
val failedTypeParameter = candidateForTypeArgumentError.typeParameters(resultOfFailedInference.failedTypeParameterIndex)
val inferenceCandidates = getInferenceCandidates( resultOfFailedInference, resultOfFailedInference.failedTypeParameterIndex )
var diagnosticChainHead = chainDiagnosticMessages( undefined, Diagnostics.The_type_argument_for_type_parameter_0_cannot_be_inferred_from_the_usage_Consider_specifying_the_type_arguments_explicitly, typeToString( failedTypeParameter ) )
if (headMessage) {
 (diagnosticChainHead=chainDiagnosticMessages( diagnosticChainHead, headMessage ))

}
reportNoCommonSupertypeError( inferenceCandidates, ((node.asInstanceOf[CallExpression]).expression||(node.asInstanceOf[TaggedTemplateExpression]).tag), diagnosticChainHead )

}

}
else {
 reportError( Diagnostics.Supplied_parameters_do_not_match_any_signature_of_call_target )

}
if ((!produceDiagnostics)) {
 (candidates).foreach { fresh96 =>
var candidate = zeroOfMyType
 = fresh96
 {
 if (hasCorrectArity( node, args, candidate )) {
 if ((candidate.typeParameters&&typeArguments)) {
 (candidate=getSignatureInstantiation( candidate, map( typeArguments, getTypeFromTypeNodeNoAlias ) ))

}
return candidate

}

}
}

}
return resolveErrorCall( node )
def reportError(message: DiagnosticMessage, arg0: String, arg1: String, arg2: String): Unit = {
 var errorInfo: DiagnosticMessageChain = zeroOfMyType
(errorInfo=chainDiagnosticMessages( errorInfo, message, arg0, arg1, arg2 ))
if (headMessage) {
 (errorInfo=chainDiagnosticMessages( errorInfo, headMessage ))

}
diagnostics.add( createDiagnosticForNodeFromMessageChain( node, errorInfo ) )

}
def chooseOverload(candidates: Array[Signature], relation: Map[RelationComparisonResult], signatureHelpTrailingComma: Nothing = false) = {
 (candidates).foreach { fresh97 =>
val originalCandidate = zeroOfMyType
 = fresh97
 {
 if ((!hasCorrectArity( node, args, originalCandidate, signatureHelpTrailingComma ))) {
 continue

}
var candidate: Signature = zeroOfMyType
var typeArgumentsAreValid: Boolean = zeroOfMyType
val inferenceContext = (if (originalCandidate.typeParameters) createInferenceContext( originalCandidate, false ) else undefined)
while (true) {
{
 (candidate=originalCandidate)
if (candidate.typeParameters) {
 var typeArgumentTypes: Array[Type] = zeroOfMyType
if (typeArguments) {
 (typeArgumentTypes=map( typeArguments, getTypeFromTypeNodeNoAlias ))
(typeArgumentsAreValid=checkTypeArguments( candidate, typeArguments, typeArgumentTypes, false ))

}
else {
 inferTypeArguments( node, candidate, args, excludeArgument, inferenceContext )
(typeArgumentsAreValid=(inferenceContext.failedTypeParameterIndex===undefined))
(typeArgumentTypes=inferenceContext.inferredTypes)

}
if ((!typeArgumentsAreValid)) {
 break()

}
(candidate=getSignatureInstantiation( candidate, typeArgumentTypes ))

}
if ((!checkApplicableSignature( node, args, candidate, relation, excludeArgument, false ))) {
 break()

}
val index = (if (excludeArgument) indexOf( excludeArgument, true ) else (-1))
if ((index<0)) {
 return candidate

}
(excludeArgument(index)=false)

}
}
if (originalCandidate.typeParameters) {
 val instantiatedCandidate = candidate
if (typeArgumentsAreValid) {
 (candidateForArgumentError=instantiatedCandidate)

}
else {
 (candidateForTypeArgumentError=originalCandidate)
if ((!typeArguments)) {
 (resultOfFailedInference=inferenceContext)

}

}

}
else {
 Debug.assert( (originalCandidate===candidate) )
(candidateForArgumentError=originalCandidate)

}

}
}
return undefined

}

}
def resolveCallExpression(node: CallExpression, candidatesOutArray: Array[Signature]): Signature = {
 if ((node.expression.kind===SyntaxKind.SuperKeyword)) {
 val superType = checkSuperExpression( node.expression )
if ((superType!==unknownType)) {
 val baseTypeNode = getClassExtendsHeritageClauseElement( getContainingClass( node ) )
if (baseTypeNode) {
 val baseConstructors = getInstantiatedConstructorsForTypeArguments( superType, baseTypeNode.typeArguments )
return resolveCall( node, baseConstructors, candidatesOutArray )

}

}
return resolveUntypedCall( node )

}
val funcType = checkNonNullExpression( node.expression )
if ((funcType===silentNeverType)) {
 return silentNeverSignature

}
val apparentType = getApparentType( funcType )
if ((apparentType===unknownType)) {
 return resolveErrorCall( node )

}
val callSignatures = getSignaturesOfType( apparentType, SignatureKind.Call )
val constructSignatures = getSignaturesOfType( apparentType, SignatureKind.Construct )
if (isUntypedFunctionCall( funcType, apparentType, callSignatures.length, constructSignatures.length )) {
 if (((funcType!==unknownType)&&node.typeArguments)) {
 error( node, Diagnostics.Untyped_function_calls_may_not_accept_type_arguments )

}
return resolveUntypedCall( node )

}
if ((!callSignatures.length)) {
 if (constructSignatures.length) {
 error( node, Diagnostics.Value_of_type_0_is_not_callable_Did_you_mean_to_include_new, typeToString( funcType ) )

}
else {
 error( node, Diagnostics.Cannot_invoke_an_expression_whose_type_lacks_a_call_signature_Type_0_has_no_compatible_call_signatures, typeToString( apparentType ) )

}
return resolveErrorCall( node )

}
return resolveCall( node, callSignatures, candidatesOutArray )

}
def isUntypedFunctionCall(funcType: Type, apparentFuncType: Type, numCallSignatures: Int, numConstructSignatures: Int) = {
 if (isTypeAny( funcType )) {
 return true

}
if ((isTypeAny( apparentFuncType )&&(funcType.flags&TypeFlags.TypeParameter))) {
 return true

}
if (((!numCallSignatures)&&(!numConstructSignatures))) {
 if ((funcType.flags&TypeFlags.Union)) {
 return false

}
return isTypeAssignableTo( funcType, globalFunctionType )

}
return false

}
def resolveNewExpression(node: NewExpression, candidatesOutArray: Array[Signature]): Signature = {
 if ((node.arguments&&(languageVersion<ScriptTarget.ES5))) {
 val spreadIndex = getSpreadArgumentIndex( node.arguments )
if ((spreadIndex>=0)) {
 error( node.arguments(spreadIndex), Diagnostics.Spread_operator_in_new_expressions_is_only_available_when_targeting_ECMAScript_5_and_higher )

}

}
var expressionType = checkNonNullExpression( node.expression )
if ((expressionType===silentNeverType)) {
 return silentNeverSignature

}
(expressionType=getApparentType( expressionType ))
if ((expressionType===unknownType)) {
 return resolveErrorCall( node )

}
val valueDecl = (expressionType.symbol&&getClassLikeDeclarationOfSymbol( expressionType.symbol ))
if ((valueDecl&&(getModifierFlags( valueDecl )&ModifierFlags.Abstract))) {
 error( node, Diagnostics.Cannot_create_an_instance_of_the_abstract_class_0, declarationNameToString( valueDecl.name ) )
return resolveErrorCall( node )

}
if (isTypeAny( expressionType )) {
 if (node.typeArguments) {
 error( node, Diagnostics.Untyped_function_calls_may_not_accept_type_arguments )

}
return resolveUntypedCall( node )

}
val constructSignatures = getSignaturesOfType( expressionType, SignatureKind.Construct )
if (constructSignatures.length) {
 if ((!isConstructorAccessible( node, constructSignatures(0) ))) {
 return resolveErrorCall( node )

}
return resolveCall( node, constructSignatures, candidatesOutArray )

}
val callSignatures = getSignaturesOfType( expressionType, SignatureKind.Call )
if (callSignatures.length) {
 val signature = resolveCall( node, callSignatures, candidatesOutArray )
if ((getReturnTypeOfSignature( signature )!==voidType)) {
 error( node, Diagnostics.Only_a_void_function_can_be_called_with_the_new_keyword )

}
if ((getThisTypeOfSignature( signature )===voidType)) {
 error( node, Diagnostics.A_function_that_is_called_with_the_new_keyword_cannot_have_a_this_type_that_is_void )

}
return signature

}
error( node, Diagnostics.Cannot_use_new_with_an_expression_whose_type_lacks_a_call_or_construct_signature )
return resolveErrorCall( node )

}
def isConstructorAccessible(node: NewExpression, signature: Signature) = {
 if (((!signature)||(!signature.declaration))) {
 return true

}
val declaration = signature.declaration
val modifiers = getModifierFlags( declaration )
if ((!((modifiers&ModifierFlags.NonPublicAccessibilityModifier)))) {
 return true

}
val declaringClassDeclaration = getClassLikeDeclarationOfSymbol( declaration.parent.symbol ).asInstanceOf[ClassLikeDeclaration]
val declaringClass = getDeclaredTypeOfSymbol( declaration.parent.symbol ).asInstanceOf[InterfaceType]
if ((!isNodeWithinClass( node, declaringClassDeclaration ))) {
 val containingClass = getContainingClass( node )
if (containingClass) {
 val containingType = getTypeOfNode( containingClass )
val baseTypes = getBaseTypes( containingType.asInstanceOf[InterfaceType] )
if (baseTypes.length) {
 val baseType = baseTypes(0)
if (((modifiers&ModifierFlags.Protected)&&(baseType.symbol===declaration.parent.symbol))) {
 return true

}

}

}
if ((modifiers&ModifierFlags.Private)) {
 error( node, Diagnostics.Constructor_of_class_0_is_private_and_only_accessible_within_the_class_declaration, typeToString( declaringClass ) )

}
if ((modifiers&ModifierFlags.Protected)) {
 error( node, Diagnostics.Constructor_of_class_0_is_protected_and_only_accessible_within_the_class_declaration, typeToString( declaringClass ) )

}
return false

}
return true

}
def resolveTaggedTemplateExpression(node: TaggedTemplateExpression, candidatesOutArray: Array[Signature]): Signature = {
 val tagType = checkExpression( node.tag )
val apparentType = getApparentType( tagType )
if ((apparentType===unknownType)) {
 return resolveErrorCall( node )

}
val callSignatures = getSignaturesOfType( apparentType, SignatureKind.Call )
val constructSignatures = getSignaturesOfType( apparentType, SignatureKind.Construct )
if (isUntypedFunctionCall( tagType, apparentType, callSignatures.length, constructSignatures.length )) {
 return resolveUntypedCall( node )

}
if ((!callSignatures.length)) {
 error( node, Diagnostics.Cannot_invoke_an_expression_whose_type_lacks_a_call_signature_Type_0_has_no_compatible_call_signatures, typeToString( apparentType ) )
return resolveErrorCall( node )

}
return resolveCall( node, callSignatures, candidatesOutArray )

}
def getDiagnosticHeadMessageForDecoratorResolution(node: Decorator) = {
 node.parent.kind match {
  case  SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression  =>
return Diagnostics.Unable_to_resolve_signature_of_class_decorator_when_called_as_an_expression
  case  SyntaxKind.Parameter  =>
return Diagnostics.Unable_to_resolve_signature_of_parameter_decorator_when_called_as_an_expression
  case  SyntaxKind.PropertyDeclaration  =>
return Diagnostics.Unable_to_resolve_signature_of_property_decorator_when_called_as_an_expression
  case  SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
return Diagnostics.Unable_to_resolve_signature_of_method_decorator_when_called_as_an_expression
  case _ =>
}

}
def resolveDecorator(node: Decorator, candidatesOutArray: Array[Signature]): Signature = {
 val funcType = checkExpression( node.expression )
val apparentType = getApparentType( funcType )
if ((apparentType===unknownType)) {
 return resolveErrorCall( node )

}
val callSignatures = getSignaturesOfType( apparentType, SignatureKind.Call )
val constructSignatures = getSignaturesOfType( apparentType, SignatureKind.Construct )
if (isUntypedFunctionCall( funcType, apparentType, callSignatures.length, constructSignatures.length )) {
 return resolveUntypedCall( node )

}
val headMessage = getDiagnosticHeadMessageForDecoratorResolution( node )
if ((!callSignatures.length)) {
 var errorInfo: DiagnosticMessageChain = zeroOfMyType
(errorInfo=chainDiagnosticMessages( errorInfo, Diagnostics.Cannot_invoke_an_expression_whose_type_lacks_a_call_signature_Type_0_has_no_compatible_call_signatures, typeToString( apparentType ) ))
(errorInfo=chainDiagnosticMessages( errorInfo, headMessage ))
diagnostics.add( createDiagnosticForNodeFromMessageChain( node, errorInfo ) )
return resolveErrorCall( node )

}
return resolveCall( node, callSignatures, candidatesOutArray, headMessage )

}
def resolveSignature(node: CallLikeExpression, candidatesOutArray: Array[Signature]): Signature = {
 node.kind match {
  case  SyntaxKind.CallExpression  =>
return resolveCallExpression( node.asInstanceOf[CallExpression], candidatesOutArray )
  case  SyntaxKind.NewExpression  =>
return resolveNewExpression( node.asInstanceOf[NewExpression], candidatesOutArray )
  case  SyntaxKind.TaggedTemplateExpression  =>
return resolveTaggedTemplateExpression( node.asInstanceOf[TaggedTemplateExpression], candidatesOutArray )
  case  SyntaxKind.Decorator  =>
return resolveDecorator( node.asInstanceOf[Decorator], candidatesOutArray )
  case _ =>
}
Debug.fail( "Branch in 'resolveSignature' should be unreachable." )

}
def getResolvedSignature(node: CallLikeExpression, candidatesOutArray: Array[Signature]): Signature = {
 val links = getNodeLinks( node )
val cached = links.resolvedSignature
if (((cached&&(cached!==resolvingSignature))&&(!candidatesOutArray))) {
 return cached

}
(links.resolvedSignature=resolvingSignature)
val result = resolveSignature( node, candidatesOutArray )
(links.resolvedSignature=(if ((flowLoopStart===flowLoopCount)) result else cached))
return result

}
def getResolvedOrAnySignature(node: CallLikeExpression) = {
 return (if ((getNodeLinks( node ).resolvedSignature===resolvingSignature)) resolvingSignature else getResolvedSignature( node ))

}
def getInferredClassType(symbol: Symbol) = {
 val links = getSymbolLinks( symbol )
if ((!links.inferredClassType)) {
 (links.inferredClassType=createAnonymousType( symbol, symbol.members, emptyArray, emptyArray, undefined, undefined ))

}
return links.inferredClassType

}
def checkCallExpression(node: ( CallExpression | NewExpression )): Type = {
 (checkGrammarTypeArguments( node, node.typeArguments )||checkGrammarArguments( node, node.arguments ))
val signature = getResolvedSignature( node )
if ((node.expression.kind===SyntaxKind.SuperKeyword)) {
 return voidType

}
if ((node.kind===SyntaxKind.NewExpression)) {
 val declaration = signature.declaration
if (((((declaration&&(declaration.kind!==SyntaxKind.Constructor))&&(declaration.kind!==SyntaxKind.ConstructSignature))&&(declaration.kind!==SyntaxKind.ConstructorType))&&(!isJSDocConstructSignature( declaration )))) {
 val funcSymbol = (if ((node.expression.kind===SyntaxKind.Identifier)) getResolvedSymbol( node.expression.asInstanceOf[Identifier] ) else checkExpression( node.expression ).symbol)
if (((funcSymbol&&funcSymbol.members)&&(((funcSymbol.flags&SymbolFlags.Function)||isDeclarationOfFunctionExpression( funcSymbol ))))) {
 return getInferredClassType( funcSymbol )

}
else if (compilerOptions.noImplicitAny) {
 error( node, Diagnostics.new_expression_whose_target_lacks_a_construct_signature_implicitly_has_an_any_type )

}
return anyType

}

}
if ((isInJavaScriptFile( node )&&isCommonJsRequire( node ))) {
 return resolveExternalModuleTypeByLiteral( node.arguments(0).asInstanceOf[StringLiteral] )

}
return getReturnTypeOfSignature( signature )

}
def isCommonJsRequire(node: Node) = {
 if ((!isRequireCall( node, true ))) {
 return false

}
val resolvedRequire = resolveName( node.expression, (node.expression.asInstanceOf[Identifier]).text, SymbolFlags.Value, undefined, undefined )
if ((!resolvedRequire)) {
 return true

}
if ((resolvedRequire.flags&SymbolFlags.Alias)) {
 return false

}
val targetDeclarationKind = (if ((resolvedRequire.flags&SymbolFlags.Function)) SyntaxKind.FunctionDeclaration else (if ((resolvedRequire.flags&SymbolFlags.Variable)) SyntaxKind.VariableDeclaration else SyntaxKind.Unknown))
if ((targetDeclarationKind!==SyntaxKind.Unknown)) {
 val decl = getDeclarationOfKind( resolvedRequire, targetDeclarationKind )
return isInAmbientContext( decl )

}
return false

}
def checkTaggedTemplateExpression(node: TaggedTemplateExpression): Type = {
 return getReturnTypeOfSignature( getResolvedSignature( node ) )

}
def checkAssertion(node: AssertionExpression) = {
 val exprType = getRegularTypeOfObjectLiteral( getBaseTypeOfLiteralType( checkExpression( node.expression ) ) )
checkSourceElement( node.`type` )
val targetType = getTypeFromTypeNode( node.`type` )
if ((produceDiagnostics&&(targetType!==unknownType))) {
 val widenedType = getWidenedType( exprType )
if ((!isTypeComparableTo( targetType, widenedType ))) {
 checkTypeComparableTo( exprType, targetType, node, Diagnostics.Type_0_cannot_be_converted_to_type_1 )

}

}
return targetType

}
def checkNonNullAssertion(node: NonNullExpression) = {
 return getNonNullableType( checkExpression( node.expression ) )

}
def getTypeOfParameter(symbol: Symbol) = {
 val `type` = getTypeOfSymbol( symbol )
if (strictNullChecks) {
 val declaration = symbol.valueDeclaration
if ((declaration&&(declaration.asInstanceOf[VariableLikeDeclaration]).initializer)) {
 return includeFalsyTypes( `type`, TypeFlags.Undefined )

}

}
return `type`

}
def getTypeAtPosition(signature: Signature, pos: Int): Type = {
 return (if (signature.hasRestParameter) (if ((pos<(signature.parameters.length-1))) getTypeOfParameter( signature.parameters(pos) ) else getRestTypeOfSignature( signature )) else (if ((pos<signature.parameters.length)) getTypeOfParameter( signature.parameters(pos) ) else anyType))

}
def assignContextualParameterTypes(signature: Signature, context: Signature, mapper: TypeMapper) = {
 val len = (signature.parameters.length-((if (signature.hasRestParameter) 1 else 0)))
if (isInferentialContext( mapper )) {
 {
var i = 0
while( (i<len)) {
 {
 val declaration = signature.parameters(i).valueDeclaration.asInstanceOf[ParameterDeclaration]
if (declaration.`type`) {
 inferTypes( mapper.context, getTypeFromTypeNode( declaration.`type` ), getTypeAtPosition( context, i ) )

}

}
 (i+= 1)
}
}

}
if (context.thisParameter) {
 val parameter = signature.thisParameter
if (((!parameter)||(parameter.valueDeclaration&&(!(parameter.valueDeclaration.asInstanceOf[ParameterDeclaration]).`type`)))) {
 if ((!parameter)) {
 (signature.thisParameter=createTransientSymbol( context.thisParameter, undefined ))

}
assignTypeToParameterAndFixTypeParameters( signature.thisParameter, getTypeOfSymbol( context.thisParameter ), mapper )

}

}
{
var i = 0
while( (i<len)) {
 {
 val parameter = signature.parameters(i)
if ((!(parameter.valueDeclaration.asInstanceOf[ParameterDeclaration]).`type`)) {
 val contextualParameterType = getTypeAtPosition( context, i )
assignTypeToParameterAndFixTypeParameters( parameter, contextualParameterType, mapper )

}

}
 (i+= 1)
}
}
if ((signature.hasRestParameter&&isRestParameterIndex( context, (signature.parameters.length-1) ))) {
 val parameter = lastOrUndefined( signature.parameters )
if ((!(parameter.valueDeclaration.asInstanceOf[ParameterDeclaration]).`type`)) {
 val contextualParameterType = getTypeOfSymbol( lastOrUndefined( context.parameters ) )
assignTypeToParameterAndFixTypeParameters( parameter, contextualParameterType, mapper )

}

}

}
def assignBindingElementTypes(node: VariableLikeDeclaration) = {
 if (isBindingPattern( node.name )) {
 ((node.name.asInstanceOf[BindingPattern]).elements).foreach { fresh98 =>
val element = zeroOfMyType
 = fresh98
 {
 if ((!isOmittedExpression( element ))) {
 if ((element.name.kind===SyntaxKind.Identifier)) {
 (getSymbolLinks( getSymbolOfNode( element ) ).`type`=getTypeForBindingElement( element ))

}
assignBindingElementTypes( element )

}

}
}

}

}
def assignTypeToParameterAndFixTypeParameters(parameter: Symbol, contextualType: Type, mapper: TypeMapper) = {
 val links = getSymbolLinks( parameter )
if ((!links.`type`)) {
 (links.`type`=instantiateType( contextualType, mapper ))
if (((links.`type`===emptyObjectType)&&(((parameter.valueDeclaration.name.kind===SyntaxKind.ObjectBindingPattern)||(parameter.valueDeclaration.name.kind===SyntaxKind.ArrayBindingPattern))))) {
 (links.`type`=getTypeFromBindingPattern( parameter.valueDeclaration.name.asInstanceOf[BindingPattern] ))

}
assignBindingElementTypes( parameter.valueDeclaration.asInstanceOf[ParameterDeclaration] )

}
else if (isInferentialContext( mapper )) {
 inferTypes( mapper.context, links.`type`, instantiateType( contextualType, mapper ) )

}

}
def getReturnTypeFromJSDocComment(func: ( SignatureDeclaration | FunctionDeclaration )): Type = {
 val returnTag = getJSDocReturnTag( func )
if ((returnTag&&returnTag.typeExpression)) {
 return getTypeFromTypeNode( returnTag.typeExpression.`type` )

}
return undefined

}
def createPromiseType(promisedType: Type): Type = {
 val globalPromiseType = getGlobalPromiseType()
if ((globalPromiseType!==emptyGenericType)) {
 (promisedType=getAwaitedType( promisedType ))
return createTypeReference( globalPromiseType.asInstanceOf[GenericType], Array( promisedType ) )

}
return emptyObjectType

}
def createPromiseReturnType(func: FunctionLikeDeclaration, promisedType: Type) = {
 val promiseType = createPromiseType( promisedType )
if ((promiseType===emptyObjectType)) {
 error( func, Diagnostics.An_async_function_or_method_must_have_a_valid_awaitable_return_type )
return unknownType

}
return promiseType

}
def getReturnTypeFromBody(func: FunctionLikeDeclaration, contextualMapper: TypeMapper): Type = {
 val contextualSignature = getContextualSignatureForFunctionLikeDeclaration( func )
if ((!func.body)) {
 return unknownType

}
val isAsync = isAsyncFunctionLike( func )
var `type`: Type = zeroOfMyType
if ((func.body.kind!==SyntaxKind.Block)) {
 (`type`=checkExpressionCached( func.body.asInstanceOf[Expression], contextualMapper ))
if (isAsync) {
 (`type`=checkAwaitedType( `type`, func, Diagnostics.Return_expression_in_async_function_does_not_have_a_valid_callable_then_member ))

}

}
else {
 var types: Array[Type] = zeroOfMyType
val funcIsGenerator = (!(!func.asteriskToken))
if (funcIsGenerator) {
 (types=checkAndAggregateYieldOperandTypes( func, contextualMapper ))
if ((types.length===0)) {
 val iterableIteratorAny = createIterableIteratorType( anyType )
if (compilerOptions.noImplicitAny) {
 error( func.asteriskToken, Diagnostics.Generator_implicitly_has_type_0_because_it_does_not_yield_any_values_Consider_supplying_a_return_type, typeToString( iterableIteratorAny ) )

}
return iterableIteratorAny

}

}
else {
 (types=checkAndAggregateReturnExpressionTypes( func, contextualMapper ))
if ((!types)) {
 return (if (isAsync) createPromiseReturnType( func, neverType ) else neverType)

}
if ((types.length===0)) {
 return (if (isAsync) createPromiseReturnType( func, voidType ) else voidType)

}

}
(`type`=getUnionType( types, true ))
if (funcIsGenerator) {
 (`type`=createIterableIteratorType( `type` ))

}

}
if ((!contextualSignature)) {
 reportErrorsFromWidening( func, `type` )

}
if ((isUnitType( `type` )&&(!((contextualSignature&&isLiteralContextualType( (if ((contextualSignature===getSignatureFromDeclaration( func ))) `type` else getReturnTypeOfSignature( contextualSignature )) )))))) {
 (`type`=getWidenedLiteralType( `type` ))

}
val widenedType = getWidenedType( `type` )
return (if (isAsync) createPromiseReturnType( func, widenedType ) else widenedType)

}
def checkAndAggregateYieldOperandTypes(func: FunctionLikeDeclaration, contextualMapper: TypeMapper): Array[Type] = {
 val aggregatedTypes: Array[Type] = Array()
forEachYieldExpression( func.body.asInstanceOf[Block], (yieldExpression =>  {
 val expr = yieldExpression.expression
 if (expr) {
 var `type` = checkExpressionCached( expr, contextualMapper )
if (yieldExpression.asteriskToken) {
 (`type`=checkElementTypeOfIterable( `type`, yieldExpression.expression ))

}
if ((!contains( aggregatedTypes, `type` ))) {
 aggregatedTypes.push( `type` )

}

}

}) )
return aggregatedTypes

}
def isExhaustiveSwitchStatement(node: SwitchStatement): Boolean = {
 if ((!node.possiblyExhaustive)) {
 return false

}
val `type` = checkExpression( node.expression )
if ((!isLiteralType( `type` ))) {
 return false

}
val switchTypes = getSwitchClauseTypes( node )
if ((!switchTypes.length)) {
 return false

}
return eachTypeContainedIn( `type`, switchTypes )

}
def functionHasImplicitReturn(func: FunctionLikeDeclaration) = {
 if ((!((func.flags&NodeFlags.HasImplicitReturn)))) {
 return false

}
val lastStatement = lastOrUndefined( (func.body.asInstanceOf[Block]).statements )
if (((lastStatement&&(lastStatement.kind===SyntaxKind.SwitchStatement))&&isExhaustiveSwitchStatement( lastStatement.asInstanceOf[SwitchStatement] ))) {
 return false

}
return true

}
def checkAndAggregateReturnExpressionTypes(func: FunctionLikeDeclaration, contextualMapper: TypeMapper): Array[Type] = {
 val isAsync = isAsyncFunctionLike( func )
val aggregatedTypes: Array[Type] = Array()
var hasReturnWithNoExpression = functionHasImplicitReturn( func )
var hasReturnOfTypeNever = false
forEachReturnStatement( func.body.asInstanceOf[Block], (returnStatement =>  {
 val expr = returnStatement.expression
 if (expr) {
 var `type` = checkExpressionCached( expr, contextualMapper )
if (isAsync) {
 (`type`=checkAwaitedType( `type`, func, Diagnostics.Return_expression_in_async_function_does_not_have_a_valid_callable_then_member ))

}
if ((`type`.flags&TypeFlags.Never)) {
 (hasReturnOfTypeNever=true)

}
else if ((!contains( aggregatedTypes, `type` ))) {
 aggregatedTypes.push( `type` )

}

}
else {
 (hasReturnWithNoExpression=true)

}

}) )
if ((((aggregatedTypes.length===0)&&(!hasReturnWithNoExpression))&&(((hasReturnOfTypeNever||(func.kind===SyntaxKind.FunctionExpression))||(func.kind===SyntaxKind.ArrowFunction))))) {
 return undefined

}
if (((strictNullChecks&&aggregatedTypes.length)&&hasReturnWithNoExpression)) {
 if ((!contains( aggregatedTypes, undefinedType ))) {
 aggregatedTypes.push( undefinedType )

}

}
return aggregatedTypes

}
def checkAllCodePathsInNonVoidFunctionReturnOrThrow(func: FunctionLikeDeclaration, returnType: Type): Unit = {
 if ((!produceDiagnostics)) {
 return

}
if ((returnType&&maybeTypeOfKind( returnType, (TypeFlags.Any|TypeFlags.Void) ))) {
 return

}
if (((nodeIsMissing( func.body )||(func.body.kind!==SyntaxKind.Block))||(!functionHasImplicitReturn( func )))) {
 return

}
val hasExplicitReturn = (func.flags&NodeFlags.HasExplicitReturn)
if ((returnType&&(returnType.flags&TypeFlags.Never))) {
 error( func.`type`, Diagnostics.A_function_returning_never_cannot_have_a_reachable_end_point )

}
else if ((returnType&&(!hasExplicitReturn))) {
 error( func.`type`, Diagnostics.A_function_whose_declared_type_is_neither_void_nor_any_must_return_a_value )

}
else if (((returnType&&strictNullChecks)&&(!isTypeAssignableTo( undefinedType, returnType )))) {
 error( func.`type`, Diagnostics.Function_lacks_ending_return_statement_and_return_type_does_not_include_undefined )

}
else if (compilerOptions.noImplicitReturns) {
 if ((!returnType)) {
 if ((!hasExplicitReturn)) {
 return

}
val inferredReturnType = getReturnTypeOfSignature( getSignatureFromDeclaration( func ) )
if (isUnwrappedReturnTypeVoidOrAny( func, inferredReturnType )) {
 return

}

}
error( (func.`type`||func), Diagnostics.Not_all_code_paths_return_a_value )

}

}
def checkFunctionExpressionOrObjectLiteralMethod(node: ( FunctionExpression | MethodDeclaration ), contextualMapper: TypeMapper): Type = {
 Debug.assert( ((node.kind!==SyntaxKind.MethodDeclaration)||isObjectLiteralMethod( node )) )
val hasGrammarError = checkGrammarFunctionLikeDeclaration( node )
if (((!hasGrammarError)&&(node.kind===SyntaxKind.FunctionExpression))) {
 checkGrammarForGenerator( node )

}
if (((contextualMapper===identityMapper)&&isContextSensitive( node ))) {
 checkNodeDeferred( node )
return anyFunctionType

}
val links = getNodeLinks( node )
val `type` = getTypeOfSymbol( node.symbol )
val contextSensitive = isContextSensitive( node )
val mightFixTypeParameters = (contextSensitive&&isInferentialContext( contextualMapper ))
if ((mightFixTypeParameters||(!((links.flags&NodeCheckFlags.ContextChecked))))) {
 val contextualSignature = getContextualSignature( node )
val contextChecked = (!(!((links.flags&NodeCheckFlags.ContextChecked))))
if ((mightFixTypeParameters||(!contextChecked))) {
 (links.flags|=NodeCheckFlags.ContextChecked)
if (contextualSignature) {
 val signature = getSignaturesOfType( `type`, SignatureKind.Call )(0)
if (contextSensitive) {
 assignContextualParameterTypes( signature, contextualSignature, (contextualMapper||identityMapper) )

}
if ((mightFixTypeParameters||((!node.`type`)&&(!signature.resolvedReturnType)))) {
 val returnType = getReturnTypeFromBody( node, contextualMapper )
if ((!signature.resolvedReturnType)) {
 (signature.resolvedReturnType=returnType)

}

}

}
if ((!contextChecked)) {
 checkSignatureDeclaration( node )
checkNodeDeferred( node )

}

}

}
if ((produceDiagnostics&&(node.kind!==SyntaxKind.MethodDeclaration))) {
 checkCollisionWithCapturedSuperVariable( node, (node.asInstanceOf[FunctionExpression]).name )
checkCollisionWithCapturedThisVariable( node, (node.asInstanceOf[FunctionExpression]).name )

}
return `type`

}
def checkFunctionExpressionOrObjectLiteralMethodDeferred(node: ( ArrowFunction | FunctionExpression | MethodDeclaration )) = {
 Debug.assert( ((node.kind!==SyntaxKind.MethodDeclaration)||isObjectLiteralMethod( node )) )
val isAsync = isAsyncFunctionLike( node )
val returnOrPromisedType = (node.`type`&&((if (isAsync) checkAsyncFunctionReturnType( node ) else getTypeFromTypeNode( node.`type` ))))
if ((!node.asteriskToken)) {
 checkAllCodePathsInNonVoidFunctionReturnOrThrow( node, returnOrPromisedType )

}
if (node.body) {
 if ((!node.`type`)) {
 getReturnTypeOfSignature( getSignatureFromDeclaration( node ) )

}
if ((node.body.kind===SyntaxKind.Block)) {
 checkSourceElement( node.body )

}
else {
 val exprType = checkExpression( node.body.asInstanceOf[Expression] )
if (returnOrPromisedType) {
 if (isAsync) {
 val awaitedType = checkAwaitedType( exprType, node.body, Diagnostics.Expression_body_for_async_arrow_function_does_not_have_a_valid_callable_then_member )
checkTypeAssignableTo( awaitedType, returnOrPromisedType, node.body )

}
else {
 checkTypeAssignableTo( exprType, returnOrPromisedType, node.body )

}

}

}
registerForUnusedIdentifiersCheck( node )

}

}
def checkArithmeticOperandType(operand: Node, `type`: Type, diagnostic: DiagnosticMessage): Boolean = {
 if ((!isTypeAnyOrAllConstituentTypesHaveKind( `type`, TypeFlags.NumberLike ))) {
 error( operand, diagnostic )
return false

}
return true

}
def isReadonlySymbol(symbol: Symbol): Boolean = {
 return ((((symbol.isReadonly||((symbol.flags&SymbolFlags.Property)&&(((getDeclarationModifierFlagsFromSymbol( symbol )&ModifierFlags.Readonly))!==0)))||((symbol.flags&SymbolFlags.Variable)&&(((getDeclarationNodeFlagsFromSymbol( symbol )&NodeFlags.Const))!==0)))||((symbol.flags&SymbolFlags.Accessor)&&(!((symbol.flags&SymbolFlags.SetAccessor)))))||(((symbol.flags&SymbolFlags.EnumMember))!==0))

}
def isReferenceToReadonlyEntity(expr: Expression, symbol: Symbol): Boolean = {
 if (isReadonlySymbol( symbol )) {
 if ((((symbol.flags&SymbolFlags.Property)&&(((expr.kind===SyntaxKind.PropertyAccessExpression)||(expr.kind===SyntaxKind.ElementAccessExpression))))&&((expr.asInstanceOf[( PropertyAccessExpression | ElementAccessExpression )]).expression.kind===SyntaxKind.ThisKeyword))) {
 val func = getContainingFunction( expr )
if ((!((func&&(func.kind===SyntaxKind.Constructor)))))
return true
return (!(((func.parent===symbol.valueDeclaration.parent)||(func===symbol.valueDeclaration.parent))))

}
return true

}
return false

}
def isReferenceThroughNamespaceImport(expr: Expression): Boolean = {
 if (((expr.kind===SyntaxKind.PropertyAccessExpression)||(expr.kind===SyntaxKind.ElementAccessExpression))) {
 val node = skipParenthesizedNodes( (expr.asInstanceOf[( PropertyAccessExpression | ElementAccessExpression )]).expression )
if ((node.kind===SyntaxKind.Identifier)) {
 val symbol = getNodeLinks( node ).resolvedSymbol
if ((symbol.flags&SymbolFlags.Alias)) {
 val declaration = getDeclarationOfAliasSymbol( symbol )
return (declaration&&(declaration.kind===SyntaxKind.NamespaceImport))

}

}

}
return false

}
def checkReferenceExpression(expr: Expression, invalidReferenceMessage: DiagnosticMessage, constantVariableMessage: DiagnosticMessage): Boolean = {
 val node = skipParenthesizedNodes( expr )
if ((((node.kind!==SyntaxKind.Identifier)&&(node.kind!==SyntaxKind.PropertyAccessExpression))&&(node.kind!==SyntaxKind.ElementAccessExpression))) {
 error( expr, invalidReferenceMessage )
return false

}
val links = getNodeLinks( node )
val symbol = getExportSymbolOfValueSymbolIfExported( links.resolvedSymbol )
if (symbol) {
 if (((symbol!==unknownSymbol)&&(symbol!==argumentsSymbol))) {
 if (((node.kind===SyntaxKind.Identifier)&&(!((symbol.flags&SymbolFlags.Variable))))) {
 error( expr, invalidReferenceMessage )
return false

}
if ((isReferenceToReadonlyEntity( node, symbol )||isReferenceThroughNamespaceImport( node ))) {
 error( expr, constantVariableMessage )
return false

}

}

}
else if ((node.kind===SyntaxKind.ElementAccessExpression)) {
 if ((links.resolvedIndexInfo&&links.resolvedIndexInfo.isReadonly)) {
 error( expr, constantVariableMessage )
return false

}

}
return true

}
def checkDeleteExpression(node: DeleteExpression): Type = {
 checkExpression( node.expression )
return booleanType

}
def checkTypeOfExpression(node: TypeOfExpression): Type = {
 checkExpression( node.expression )
return stringType

}
def checkVoidExpression(node: VoidExpression): Type = {
 checkExpression( node.expression )
return undefinedWideningType

}
def checkAwaitExpression(node: AwaitExpression): Type = {
 if (produceDiagnostics) {
 if ((!((node.flags&NodeFlags.AwaitContext)))) {
 grammarErrorOnFirstToken( node, Diagnostics.await_expression_is_only_allowed_within_an_async_function )

}
if (isInParameterInitializerBeforeContainingFunction( node )) {
 error( node, Diagnostics.await_expressions_cannot_be_used_in_a_parameter_initializer )

}

}
val operandType = checkExpression( node.expression )
return checkAwaitedType( operandType, node )

}
def checkPrefixUnaryExpression(node: PrefixUnaryExpression): Type = {
 val operandType = checkExpression( node.operand )
if ((operandType===silentNeverType)) {
 return silentNeverType

}
if (((node.operator===SyntaxKind.MinusToken)&&(node.operand.kind===SyntaxKind.NumericLiteral))) {
 return getFreshTypeOfLiteralType( getLiteralTypeForText( TypeFlags.NumberLiteral, (""+(-(node.operand.asInstanceOf[LiteralExpression]).text)) ) )

}
node.operator match {
  case  SyntaxKind.PlusToken | SyntaxKind.MinusToken | SyntaxKind.TildeToken  =>
if (maybeTypeOfKind( operandType, TypeFlags.ESSymbol )) {
 error( node.operand, Diagnostics.The_0_operator_cannot_be_applied_to_type_symbol, tokenToString( node.operator ) )

}
return numberType
  case  SyntaxKind.ExclamationToken  =>
val facts = (getTypeFacts( operandType )&((TypeFacts.Truthy|TypeFacts.Falsy)))
return (if ((facts===TypeFacts.Truthy)) falseType else (if ((facts===TypeFacts.Falsy)) trueType else booleanType))
  case  SyntaxKind.PlusPlusToken | SyntaxKind.MinusMinusToken  =>
val ok = checkArithmeticOperandType( node.operand, getNonNullableType( operandType ), Diagnostics.An_arithmetic_operand_must_be_of_type_any_number_or_an_enum_type )
if (ok) {
 checkReferenceExpression( node.operand, Diagnostics.The_operand_of_an_increment_or_decrement_operator_must_be_a_variable_property_or_indexer, Diagnostics.The_operand_of_an_increment_or_decrement_operator_cannot_be_a_constant_or_a_read_only_property )

}
return numberType
  case _ =>
}
return unknownType

}
def checkPostfixUnaryExpression(node: PostfixUnaryExpression): Type = {
 val operandType = checkExpression( node.operand )
if ((operandType===silentNeverType)) {
 return silentNeverType

}
val ok = checkArithmeticOperandType( node.operand, getNonNullableType( operandType ), Diagnostics.An_arithmetic_operand_must_be_of_type_any_number_or_an_enum_type )
if (ok) {
 checkReferenceExpression( node.operand, Diagnostics.The_operand_of_an_increment_or_decrement_operator_must_be_a_variable_property_or_indexer, Diagnostics.The_operand_of_an_increment_or_decrement_operator_cannot_be_a_constant_or_a_read_only_property )

}
return numberType

}
def maybeTypeOfKind(`type`: Type, kind: TypeFlags): Boolean = {
 if ((`type`.flags&kind)) {
 return true

}
if ((`type`.flags&TypeFlags.UnionOrIntersection)) {
 val types = (`type`.asInstanceOf[UnionOrIntersectionType]).types
(types).foreach { fresh99 =>
val t = zeroOfMyType
 = fresh99
 {
 if (maybeTypeOfKind( t, kind )) {
 return true

}

}
}

}
return false

}
def isTypeOfKind(`type`: Type, kind: TypeFlags): Boolean = {
 if ((`type`.flags&kind)) {
 return true

}
if ((`type`.flags&TypeFlags.Union)) {
 val types = (`type`.asInstanceOf[UnionOrIntersectionType]).types
(types).foreach { fresh100 =>
val t = zeroOfMyType
 = fresh100
 {
 if ((!isTypeOfKind( t, kind ))) {
 return false

}

}
}
return true

}
if ((`type`.flags&TypeFlags.Intersection)) {
 val types = (`type`.asInstanceOf[UnionOrIntersectionType]).types
(types).foreach { fresh101 =>
val t = zeroOfMyType
 = fresh101
 {
 if (isTypeOfKind( t, kind )) {
 return true

}

}
}

}
return false

}
def isConstEnumObjectType(`type`: Type): Boolean = {
 return (((getObjectFlags( `type` )&ObjectFlags.Anonymous)&&`type`.symbol)&&isConstEnumSymbol( `type`.symbol ))

}
def isConstEnumSymbol(symbol: Symbol): Boolean = {
 return (((symbol.flags&SymbolFlags.ConstEnum))!==0)

}
def checkInstanceOfExpression(left: Expression, right: Expression, leftType: Type, rightType: Type): Type = {
 if (((leftType===silentNeverType)||(rightType===silentNeverType))) {
 return silentNeverType

}
if (isTypeOfKind( leftType, TypeFlags.Primitive )) {
 error( left, Diagnostics.The_left_hand_side_of_an_instanceof_expression_must_be_of_type_any_an_object_type_or_a_type_parameter )

}
if ((!((isTypeAny( rightType )||isTypeSubtypeOf( rightType, globalFunctionType ))))) {
 error( right, Diagnostics.The_right_hand_side_of_an_instanceof_expression_must_be_of_type_any_or_of_a_type_assignable_to_the_Function_interface_type )

}
return booleanType

}
def checkInExpression(left: Expression, right: Expression, leftType: Type, rightType: Type): Type = {
 if (((leftType===silentNeverType)||(rightType===silentNeverType))) {
 return silentNeverType

}
if ((!isTypeAnyOrAllConstituentTypesHaveKind( leftType, ((TypeFlags.StringLike|TypeFlags.NumberLike)|TypeFlags.ESSymbol) ))) {
 error( left, Diagnostics.The_left_hand_side_of_an_in_expression_must_be_of_type_any_string_number_or_symbol )

}
if ((!isTypeAnyOrAllConstituentTypesHaveKind( rightType, (TypeFlags.Object|TypeFlags.TypeParameter) ))) {
 error( right, Diagnostics.The_right_hand_side_of_an_in_expression_must_be_of_type_any_an_object_type_or_a_type_parameter )

}
return booleanType

}
def checkObjectLiteralAssignment(node: ObjectLiteralExpression, sourceType: Type): Type = {
 val properties = node.properties
(properties).foreach { fresh102 =>
val p = zeroOfMyType
 = fresh102
 {
 checkObjectLiteralDestructuringPropertyAssignment( sourceType, p )

}
}
return sourceType

}
def checkObjectLiteralDestructuringPropertyAssignment(objectLiteralType: Type, property: ObjectLiteralElementLike) = {
 if (((property.kind===SyntaxKind.PropertyAssignment)||(property.kind===SyntaxKind.ShorthandPropertyAssignment))) {
 val name = (property.asInstanceOf[PropertyAssignment]).name.asInstanceOf[PropertyName]
if ((name.kind===SyntaxKind.ComputedPropertyName)) {
 checkComputedPropertyName( name.asInstanceOf[ComputedPropertyName] )

}
if (isComputedNonLiteralName( name )) {
 return undefined

}
val text = getTextOfPropertyName( name )
val `type` = (if (isTypeAny( objectLiteralType )) objectLiteralType else ((getTypeOfPropertyOfType( objectLiteralType, text )||(isNumericLiteralName( text )&&getIndexTypeOfType( objectLiteralType, IndexKind.Number )))||getIndexTypeOfType( objectLiteralType, IndexKind.String )))
if (`type`) {
 if ((property.kind===SyntaxKind.ShorthandPropertyAssignment)) {
 return checkDestructuringAssignment( property.asInstanceOf[ShorthandPropertyAssignment], `type` )

}
else {
 return checkDestructuringAssignment( (property.asInstanceOf[PropertyAssignment]).initializer, `type` )

}

}
else {
 error( name, Diagnostics.Type_0_has_no_property_1_and_no_string_index_signature, typeToString( objectLiteralType ), declarationNameToString( name ) )

}

}
else {
 error( property, Diagnostics.Property_assignment_expected )

}

}
def checkArrayLiteralAssignment(node: ArrayLiteralExpression, sourceType: Type, contextualMapper: TypeMapper): Type = {
 val elementType = (checkIteratedTypeOrElementType( sourceType, node, false )||unknownType)
val elements = node.elements
{
var i = 0
while( (i<elements.length)) {
 {
 checkArrayLiteralDestructuringElementAssignment( node, sourceType, i, elementType, contextualMapper )

}
 (i+= 1)
}
}
return sourceType

}
def checkArrayLiteralDestructuringElementAssignment(node: ArrayLiteralExpression, sourceType: Type, elementIndex: Int, elementType: Type, contextualMapper: TypeMapper) = {
 val elements = node.elements
val element = elements(elementIndex)
if ((element.kind!==SyntaxKind.OmittedExpression)) {
 if ((element.kind!==SyntaxKind.SpreadElementExpression)) {
 val propName = (""+elementIndex)
val `type` = (if (isTypeAny( sourceType )) sourceType else (if (isTupleLikeType( sourceType )) getTypeOfPropertyOfType( sourceType, propName ) else elementType))
if (`type`) {
 return checkDestructuringAssignment( element, `type`, contextualMapper )

}
else {
 checkExpression( element )
if (isTupleType( sourceType )) {
 error( element, Diagnostics.Tuple_type_0_with_length_1_cannot_be_assigned_to_tuple_with_length_2, typeToString( sourceType ), getTypeReferenceArity( sourceType.asInstanceOf[TypeReference] ), elements.length )

}
else {
 error( element, Diagnostics.Type_0_has_no_property_1, typeToString( sourceType ), propName )

}

}

}
else {
 if ((elementIndex<(elements.length-1))) {
 error( element, Diagnostics.A_rest_element_must_be_last_in_an_array_destructuring_pattern )

}
else {
 val restExpression = (element.asInstanceOf[SpreadElementExpression]).expression
if (((restExpression.kind===SyntaxKind.BinaryExpression)&&((restExpression.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.EqualsToken))) {
 error( (restExpression.asInstanceOf[BinaryExpression]).operatorToken, Diagnostics.A_rest_element_cannot_have_an_initializer )

}
else {
 return checkDestructuringAssignment( restExpression, createArrayType( elementType ), contextualMapper )

}

}

}

}
return undefined

}
def checkDestructuringAssignment(exprOrAssignment: ( Expression | ShorthandPropertyAssignment ), sourceType: Type, contextualMapper: TypeMapper): Type = {
 var target: Expression = zeroOfMyType
if ((exprOrAssignment.kind===SyntaxKind.ShorthandPropertyAssignment)) {
 val prop = exprOrAssignment.asInstanceOf[ShorthandPropertyAssignment]
if (prop.objectAssignmentInitializer) {
 if ((strictNullChecks&&(!((getFalsyFlags( checkExpression( prop.objectAssignmentInitializer ) )&TypeFlags.Undefined))))) {
 (sourceType=getTypeWithFacts( sourceType, TypeFacts.NEUndefined ))

}
checkBinaryLikeExpression( prop.name, prop.equalsToken, prop.objectAssignmentInitializer, contextualMapper )

}
(target=(exprOrAssignment.asInstanceOf[ShorthandPropertyAssignment]).name)

}
else {
 (target=exprOrAssignment.asInstanceOf[Expression])

}
if (((target.kind===SyntaxKind.BinaryExpression)&&((target.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.EqualsToken))) {
 checkBinaryExpression( target.asInstanceOf[BinaryExpression], contextualMapper )
(target=(target.asInstanceOf[BinaryExpression]).left)

}
if ((target.kind===SyntaxKind.ObjectLiteralExpression)) {
 return checkObjectLiteralAssignment( target.asInstanceOf[ObjectLiteralExpression], sourceType )

}
if ((target.kind===SyntaxKind.ArrayLiteralExpression)) {
 return checkArrayLiteralAssignment( target.asInstanceOf[ArrayLiteralExpression], sourceType, contextualMapper )

}
return checkReferenceAssignment( target, sourceType, contextualMapper )

}
def checkReferenceAssignment(target: Expression, sourceType: Type, contextualMapper: TypeMapper): Type = {
 val targetType = checkExpression( target, contextualMapper )
if (checkReferenceExpression( target, Diagnostics.Invalid_left_hand_side_of_assignment_expression, Diagnostics.Left_hand_side_of_assignment_expression_cannot_be_a_constant_or_a_read_only_property )) {
 checkTypeAssignableTo( sourceType, targetType, target, undefined )

}
return sourceType

}
def isSideEffectFree(node: Node): Boolean = {
 (node=skipParentheses( node ))
node.kind match {
  case  SyntaxKind.Identifier | SyntaxKind.StringLiteral | SyntaxKind.RegularExpressionLiteral | SyntaxKind.TaggedTemplateExpression | SyntaxKind.TemplateExpression | SyntaxKind.NoSubstitutionTemplateLiteral | SyntaxKind.NumericLiteral | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword | SyntaxKind.NullKeyword | SyntaxKind.UndefinedKeyword | SyntaxKind.FunctionExpression | SyntaxKind.ClassExpression | SyntaxKind.ArrowFunction | SyntaxKind.ArrayLiteralExpression | SyntaxKind.ObjectLiteralExpression | SyntaxKind.TypeOfExpression | SyntaxKind.NonNullExpression | SyntaxKind.JsxSelfClosingElement | SyntaxKind.JsxElement  =>
return true
  case  SyntaxKind.ConditionalExpression  =>
return (isSideEffectFree( (node.asInstanceOf[ConditionalExpression]).whenTrue )&&isSideEffectFree( (node.asInstanceOf[ConditionalExpression]).whenFalse ))
  case  SyntaxKind.BinaryExpression  =>
if (isAssignmentOperator( (node.asInstanceOf[BinaryExpression]).operatorToken.kind )) {
 return false

}
return (isSideEffectFree( (node.asInstanceOf[BinaryExpression]).left )&&isSideEffectFree( (node.asInstanceOf[BinaryExpression]).right ))
  case  SyntaxKind.PrefixUnaryExpression | SyntaxKind.PostfixUnaryExpression  =>
(node.asInstanceOf[PrefixUnaryExpression]).operator match {
  case  SyntaxKind.ExclamationToken | SyntaxKind.PlusToken | SyntaxKind.MinusToken | SyntaxKind.TildeToken  =>
return true
  case _ =>
}
return false
  case _ =>
return false
}

}
def isTypeEqualityComparableTo(source: Type, target: Type) = {
 return ((((target.flags&TypeFlags.Nullable))!==0)||isTypeComparableTo( source, target ))

}
def getBestChoiceType(type1: Type, type2: Type): Type = {
 val firstAssignableToSecond = isTypeAssignableTo( type1, type2 )
val secondAssignableToFirst = isTypeAssignableTo( type2, type1 )
return (if ((secondAssignableToFirst&&(!firstAssignableToSecond))) type1 else (if ((firstAssignableToSecond&&(!secondAssignableToFirst))) type2 else getUnionType( Array( type1, type2 ), true )))

}
def checkBinaryExpression(node: BinaryExpression, contextualMapper: TypeMapper) = {
 return checkBinaryLikeExpression( node.left, node.operatorToken, node.right, contextualMapper, node )

}
def checkBinaryLikeExpression(left: Expression, operatorToken: Node, right: Expression, contextualMapper: TypeMapper, errorNode: Node) = {
 val operator = operatorToken.kind
if (((operator===SyntaxKind.EqualsToken)&&(((left.kind===SyntaxKind.ObjectLiteralExpression)||(left.kind===SyntaxKind.ArrayLiteralExpression))))) {
 return checkDestructuringAssignment( left, checkExpression( right, contextualMapper ), contextualMapper )

}
var leftType = checkExpression( left, contextualMapper )
var rightType = checkExpression( right, contextualMapper )
operator match {
  case  SyntaxKind.AsteriskToken | SyntaxKind.AsteriskAsteriskToken | SyntaxKind.AsteriskEqualsToken | SyntaxKind.AsteriskAsteriskEqualsToken | SyntaxKind.SlashToken | SyntaxKind.SlashEqualsToken | SyntaxKind.PercentToken | SyntaxKind.PercentEqualsToken | SyntaxKind.MinusToken | SyntaxKind.MinusEqualsToken | SyntaxKind.LessThanLessThanToken | SyntaxKind.LessThanLessThanEqualsToken | SyntaxKind.GreaterThanGreaterThanToken | SyntaxKind.GreaterThanGreaterThanEqualsToken | SyntaxKind.GreaterThanGreaterThanGreaterThanToken | SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken | SyntaxKind.BarToken | SyntaxKind.BarEqualsToken | SyntaxKind.CaretToken | SyntaxKind.CaretEqualsToken | SyntaxKind.AmpersandToken | SyntaxKind.AmpersandEqualsToken  =>
if (((leftType===silentNeverType)||(rightType===silentNeverType))) {
 return silentNeverType

}
if ((leftType.flags&TypeFlags.Nullable))
(leftType=rightType)
if ((rightType.flags&TypeFlags.Nullable))
(rightType=leftType)
(leftType=getNonNullableType( leftType ))
(rightType=getNonNullableType( rightType ))
var suggestedOperator: SyntaxKind = zeroOfMyType
if (((((leftType.flags&TypeFlags.BooleanLike))&&((rightType.flags&TypeFlags.BooleanLike)))&&(((suggestedOperator=getSuggestedBooleanOperator( operatorToken.kind )))!==undefined))) {
 error( (errorNode||operatorToken), Diagnostics.The_0_operator_is_not_allowed_for_boolean_types_Consider_using_1_instead, tokenToString( operatorToken.kind ), tokenToString( suggestedOperator ) )

}
else {
 val leftOk = checkArithmeticOperandType( left, leftType, Diagnostics.The_left_hand_side_of_an_arithmetic_operation_must_be_of_type_any_number_or_an_enum_type )
val rightOk = checkArithmeticOperandType( right, rightType, Diagnostics.The_right_hand_side_of_an_arithmetic_operation_must_be_of_type_any_number_or_an_enum_type )
if ((leftOk&&rightOk)) {
 checkAssignmentOperator( numberType )

}

}
return numberType
  case  SyntaxKind.PlusToken | SyntaxKind.PlusEqualsToken  =>
if (((leftType===silentNeverType)||(rightType===silentNeverType))) {
 return silentNeverType

}
if ((leftType.flags&TypeFlags.Nullable))
(leftType=rightType)
if ((rightType.flags&TypeFlags.Nullable))
(rightType=leftType)
(leftType=getNonNullableType( leftType ))
(rightType=getNonNullableType( rightType ))
var resultType: Type = zeroOfMyType
if ((isTypeOfKind( leftType, TypeFlags.NumberLike )&&isTypeOfKind( rightType, TypeFlags.NumberLike ))) {
 (resultType=numberType)

}
else {
 if ((isTypeOfKind( leftType, TypeFlags.StringLike )||isTypeOfKind( rightType, TypeFlags.StringLike ))) {
 (resultType=stringType)

}
else if ((isTypeAny( leftType )||isTypeAny( rightType ))) {
 (resultType=(if (((leftType===unknownType)||(rightType===unknownType))) unknownType else anyType))

}
if ((resultType&&(!checkForDisallowedESSymbolOperand( operator )))) {
 return resultType

}

}
if ((!resultType)) {
 reportOperatorError()
return anyType

}
if ((operator===SyntaxKind.PlusEqualsToken)) {
 checkAssignmentOperator( resultType )

}
return resultType
  case  SyntaxKind.LessThanToken | SyntaxKind.GreaterThanToken | SyntaxKind.LessThanEqualsToken | SyntaxKind.GreaterThanEqualsToken  =>
if (checkForDisallowedESSymbolOperand( operator )) {
 if (((!isTypeComparableTo( leftType, rightType ))&&(!isTypeComparableTo( rightType, leftType )))) {
 reportOperatorError()

}

}
return booleanType
  case  SyntaxKind.EqualsEqualsToken | SyntaxKind.ExclamationEqualsToken | SyntaxKind.EqualsEqualsEqualsToken | SyntaxKind.ExclamationEqualsEqualsToken  =>
val leftIsLiteral = isLiteralType( leftType )
val rightIsLiteral = isLiteralType( rightType )
if (((!leftIsLiteral)||(!rightIsLiteral))) {
 (leftType=(if (leftIsLiteral) getBaseTypeOfLiteralType( leftType ) else leftType))
(rightType=(if (rightIsLiteral) getBaseTypeOfLiteralType( rightType ) else rightType))

}
if (((!isTypeEqualityComparableTo( leftType, rightType ))&&(!isTypeEqualityComparableTo( rightType, leftType )))) {
 reportOperatorError()

}
return booleanType
  case  SyntaxKind.InstanceOfKeyword  =>
return checkInstanceOfExpression( left, right, leftType, rightType )
  case  SyntaxKind.InKeyword  =>
return checkInExpression( left, right, leftType, rightType )
  case  SyntaxKind.AmpersandAmpersandToken  =>
return (if ((getTypeFacts( leftType )&TypeFacts.Truthy)) includeFalsyTypes( rightType, getFalsyFlags( (if (strictNullChecks) leftType else getBaseTypeOfLiteralType( rightType )) ) ) else leftType)
  case  SyntaxKind.BarBarToken  =>
return (if ((getTypeFacts( leftType )&TypeFacts.Falsy)) getBestChoiceType( removeDefinitelyFalsyTypes( leftType ), rightType ) else leftType)
  case  SyntaxKind.EqualsToken  =>
checkAssignmentOperator( rightType )
return getRegularTypeOfObjectLiteral( rightType )
  case  SyntaxKind.CommaToken  =>
if (((!compilerOptions.allowUnreachableCode)&&isSideEffectFree( left ))) {
 error( left, Diagnostics.Left_side_of_comma_operator_is_unused_and_has_no_side_effects )

}
return rightType
  case _ =>
}
def checkForDisallowedESSymbolOperand(operator: SyntaxKind): Boolean = {
 val offendingSymbolOperand = (if (maybeTypeOfKind( leftType, TypeFlags.ESSymbol )) left else (if (maybeTypeOfKind( rightType, TypeFlags.ESSymbol )) right else undefined))
if (offendingSymbolOperand) {
 error( offendingSymbolOperand, Diagnostics.The_0_operator_cannot_be_applied_to_type_symbol, tokenToString( operator ) )
return false

}
return true

}
def getSuggestedBooleanOperator(operator: SyntaxKind): SyntaxKind = {
 operator match {
  case  SyntaxKind.BarToken | SyntaxKind.BarEqualsToken  =>
return SyntaxKind.BarBarToken
  case  SyntaxKind.CaretToken | SyntaxKind.CaretEqualsToken  =>
return SyntaxKind.ExclamationEqualsEqualsToken
  case  SyntaxKind.AmpersandToken | SyntaxKind.AmpersandEqualsToken  =>
return SyntaxKind.AmpersandAmpersandToken
  case _ =>
return undefined
}

}
def checkAssignmentOperator(valueType: Type): Unit = {
 if (((produceDiagnostics&&(operator>=SyntaxKind.FirstAssignment))&&(operator<=SyntaxKind.LastAssignment))) {
 val ok = checkReferenceExpression( left, Diagnostics.Invalid_left_hand_side_of_assignment_expression, Diagnostics.Left_hand_side_of_assignment_expression_cannot_be_a_constant_or_a_read_only_property )
if (ok) {
 checkTypeAssignableTo( valueType, leftType, left, undefined )

}

}

}
def reportOperatorError() = {
 error( (errorNode||operatorToken), Diagnostics.Operator_0_cannot_be_applied_to_types_1_and_2, tokenToString( operatorToken.kind ), typeToString( leftType ), typeToString( rightType ) )

}

}
def isYieldExpressionInClass(node: YieldExpression): Boolean = {
 var current: Node = node
var parent = node.parent
while (parent) {
{
 if ((isFunctionLike( parent )&&(current===(parent.asInstanceOf[FunctionLikeDeclaration]).body))) {
 return false

}
else if (isClassLike( current )) {
 return true

}
(current=parent)
(parent=parent.parent)

}
}
return false

}
def checkYieldExpression(node: YieldExpression): Type = {
 if (produceDiagnostics) {
 if (((!((node.flags&NodeFlags.YieldContext)))||isYieldExpressionInClass( node ))) {
 grammarErrorOnFirstToken( node, Diagnostics.A_yield_expression_is_only_allowed_in_a_generator_body )

}
if (isInParameterInitializerBeforeContainingFunction( node )) {
 error( node, Diagnostics.yield_expressions_cannot_be_used_in_a_parameter_initializer )

}

}
if (node.expression) {
 val func = getContainingFunction( node )
if ((func&&func.asteriskToken)) {
 val expressionType = checkExpressionCached( node.expression, undefined )
var expressionElementType: Type = zeroOfMyType
val nodeIsYieldStar = (!(!node.asteriskToken))
if (nodeIsYieldStar) {
 (expressionElementType=checkElementTypeOfIterable( expressionType, node.expression ))

}
if (func.`type`) {
 val signatureElementType = (getElementTypeOfIterableIterator( getTypeFromTypeNode( func.`type` ) )||anyType)
if (nodeIsYieldStar) {
 checkTypeAssignableTo( expressionElementType, signatureElementType, node.expression, undefined )

}
else {
 checkTypeAssignableTo( expressionType, signatureElementType, node.expression, undefined )

}

}

}

}
return anyType

}
def checkConditionalExpression(node: ConditionalExpression, contextualMapper: TypeMapper): Type = {
 checkExpression( node.condition )
val type1 = checkExpression( node.whenTrue, contextualMapper )
val type2 = checkExpression( node.whenFalse, contextualMapper )
return getBestChoiceType( type1, type2 )

}
def checkLiteralExpression(node: Expression): Type = {
 if ((node.kind===SyntaxKind.NumericLiteral)) {
 checkGrammarNumericLiteral( node.asInstanceOf[NumericLiteral] )

}
node.kind match {
  case  SyntaxKind.StringLiteral  =>
return getFreshTypeOfLiteralType( getLiteralTypeForText( TypeFlags.StringLiteral, (node.asInstanceOf[LiteralExpression]).text ) )
  case  SyntaxKind.NumericLiteral  =>
return getFreshTypeOfLiteralType( getLiteralTypeForText( TypeFlags.NumberLiteral, (node.asInstanceOf[LiteralExpression]).text ) )
  case  SyntaxKind.TrueKeyword  =>
return trueType
  case  SyntaxKind.FalseKeyword  =>
return falseType
  case _ =>
}

}
def checkTemplateExpression(node: TemplateExpression): Type = {
 forEach( (node.asInstanceOf[TemplateExpression]).templateSpans, (templateSpan =>  {
 checkExpression( templateSpan.expression )

}) )
return stringType

}
def checkExpressionWithContextualType(node: Expression, contextualType: Type, contextualMapper: TypeMapper): Type = {
 val saveContextualType = node.contextualType
(node.contextualType=contextualType)
val result = checkExpression( node, contextualMapper )
(node.contextualType=saveContextualType)
return result

}
def checkExpressionCached(node: Expression, contextualMapper: TypeMapper): Type = {
 val links = getNodeLinks( node )
if ((!links.resolvedType)) {
 val saveFlowLoopStart = flowLoopStart
(flowLoopStart=flowLoopCount)
(links.resolvedType=checkExpression( node, contextualMapper ))
(flowLoopStart=saveFlowLoopStart)

}
return links.resolvedType

}
def isTypeAssertion(node: Expression) = {
 (node=skipParenthesizedNodes( node ))
return ((node.kind===SyntaxKind.TypeAssertionExpression)||(node.kind===SyntaxKind.AsExpression))

}
def checkDeclarationInitializer(declaration: VariableLikeDeclaration) = {
 val `type` = checkExpressionCached( declaration.initializer )
return (if ((((getCombinedNodeFlags( declaration )&NodeFlags.Const)||(getCombinedModifierFlags( declaration )&ModifierFlags.Readonly))||isTypeAssertion( declaration.initializer ))) `type` else getWidenedLiteralType( `type` ))

}
def isLiteralContextualType(contextualType: Type) = {
 if (contextualType) {
 if ((contextualType.flags&TypeFlags.TypeParameter)) {
 val apparentType = getApparentTypeOfTypeParameter( contextualType.asInstanceOf[TypeParameter] )
if ((apparentType.flags&((((TypeFlags.String|TypeFlags.Number)|TypeFlags.Boolean)|TypeFlags.Enum)))) {
 return true

}
(contextualType=apparentType)

}
return maybeTypeOfKind( contextualType, TypeFlags.Literal )

}
return false

}
def checkExpressionForMutableLocation(node: Expression, contextualMapper: TypeMapper): Type = {
 val `type` = checkExpression( node, contextualMapper )
return (if ((isTypeAssertion( node )||isLiteralContextualType( getContextualType( node ) ))) `type` else getWidenedLiteralType( `type` ))

}
def checkPropertyAssignment(node: PropertyAssignment, contextualMapper: TypeMapper): Type = {
 if ((node.name.kind===SyntaxKind.ComputedPropertyName)) {
 checkComputedPropertyName( node.name.asInstanceOf[ComputedPropertyName] )

}
return checkExpressionForMutableLocation( (node.asInstanceOf[PropertyAssignment]).initializer, contextualMapper )

}
def checkObjectLiteralMethod(node: MethodDeclaration, contextualMapper: TypeMapper): Type = {
 checkGrammarMethod( node )
if ((node.name.kind===SyntaxKind.ComputedPropertyName)) {
 checkComputedPropertyName( node.name.asInstanceOf[ComputedPropertyName] )

}
val uninstantiatedType = checkFunctionExpressionOrObjectLiteralMethod( node, contextualMapper )
return instantiateTypeWithSingleGenericCallSignature( node, uninstantiatedType, contextualMapper )

}
def instantiateTypeWithSingleGenericCallSignature(node: ( Expression | MethodDeclaration ), `type`: Type, contextualMapper: TypeMapper) = {
 if (isInferentialContext( contextualMapper )) {
 val signature = getSingleCallSignature( `type` )
if ((signature&&signature.typeParameters)) {
 val contextualType = getApparentTypeOfContextualType( node.asInstanceOf[Expression] )
if (contextualType) {
 val contextualSignature = getSingleCallSignature( contextualType )
if ((contextualSignature&&(!contextualSignature.typeParameters))) {
 return getOrCreateTypeFromSignature( instantiateSignatureInContextOf( signature, contextualSignature, contextualMapper ) )

}

}

}

}
return `type`

}
def checkExpression(node: ( Expression | QualifiedName ), contextualMapper: TypeMapper): Type = {
 var `type`: Type = zeroOfMyType
if ((node.kind===SyntaxKind.QualifiedName)) {
 (`type`=checkQualifiedName( node.asInstanceOf[QualifiedName] ))

}
else {
 val uninstantiatedType = checkExpressionWorker( node.asInstanceOf[Expression], contextualMapper )
(`type`=instantiateTypeWithSingleGenericCallSignature( node.asInstanceOf[Expression], uninstantiatedType, contextualMapper ))

}
if (isConstEnumObjectType( `type` )) {
 val ok = (((((node.parent.kind===SyntaxKind.PropertyAccessExpression)&&((node.parent.asInstanceOf[PropertyAccessExpression]).expression===node)))||(((node.parent.kind===SyntaxKind.ElementAccessExpression)&&((node.parent.asInstanceOf[ElementAccessExpression]).expression===node))))||(((((node.kind===SyntaxKind.Identifier)||(node.kind===SyntaxKind.QualifiedName)))&&isInRightSideOfImportOrExportAssignment( node.asInstanceOf[Identifier] ))))
if ((!ok)) {
 error( node, Diagnostics.const_enums_can_only_be_used_in_property_or_index_access_expressions_or_the_right_hand_side_of_an_import_declaration_or_export_assignment )

}

}
return `type`

}
def checkExpressionWorker(node: Expression, contextualMapper: TypeMapper): Type = {
 node.kind match {
  case  SyntaxKind.Identifier  =>
return checkIdentifier( node.asInstanceOf[Identifier] )
  case  SyntaxKind.ThisKeyword  =>
return checkThisExpression( node )
  case  SyntaxKind.SuperKeyword  =>
return checkSuperExpression( node )
  case  SyntaxKind.NullKeyword  =>
return nullWideningType
  case  SyntaxKind.StringLiteral | SyntaxKind.NumericLiteral | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword  =>
return checkLiteralExpression( node )
  case  SyntaxKind.TemplateExpression  =>
return checkTemplateExpression( node.asInstanceOf[TemplateExpression] )
  case  SyntaxKind.NoSubstitutionTemplateLiteral  =>
return stringType
  case  SyntaxKind.RegularExpressionLiteral  =>
return globalRegExpType
  case  SyntaxKind.ArrayLiteralExpression  =>
return checkArrayLiteral( node.asInstanceOf[ArrayLiteralExpression], contextualMapper )
  case  SyntaxKind.ObjectLiteralExpression  =>
return checkObjectLiteral( node.asInstanceOf[ObjectLiteralExpression], contextualMapper )
  case  SyntaxKind.PropertyAccessExpression  =>
return checkPropertyAccessExpression( node.asInstanceOf[PropertyAccessExpression] )
  case  SyntaxKind.ElementAccessExpression  =>
return checkIndexedAccess( node.asInstanceOf[ElementAccessExpression] )
  case  SyntaxKind.CallExpression | SyntaxKind.NewExpression  =>
return checkCallExpression( node.asInstanceOf[CallExpression] )
  case  SyntaxKind.TaggedTemplateExpression  =>
return checkTaggedTemplateExpression( node.asInstanceOf[TaggedTemplateExpression] )
  case  SyntaxKind.ParenthesizedExpression  =>
return checkExpression( (node.asInstanceOf[ParenthesizedExpression]).expression, contextualMapper )
  case  SyntaxKind.ClassExpression  =>
return checkClassExpression( node.asInstanceOf[ClassExpression] )
  case  SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction  =>
return checkFunctionExpressionOrObjectLiteralMethod( node.asInstanceOf[FunctionExpression], contextualMapper )
  case  SyntaxKind.TypeOfExpression  =>
return checkTypeOfExpression( node.asInstanceOf[TypeOfExpression] )
  case  SyntaxKind.TypeAssertionExpression | SyntaxKind.AsExpression  =>
return checkAssertion( node.asInstanceOf[AssertionExpression] )
  case  SyntaxKind.NonNullExpression  =>
return checkNonNullAssertion( node.asInstanceOf[NonNullExpression] )
  case  SyntaxKind.DeleteExpression  =>
return checkDeleteExpression( node.asInstanceOf[DeleteExpression] )
  case  SyntaxKind.VoidExpression  =>
return checkVoidExpression( node.asInstanceOf[VoidExpression] )
  case  SyntaxKind.AwaitExpression  =>
return checkAwaitExpression( node.asInstanceOf[AwaitExpression] )
  case  SyntaxKind.PrefixUnaryExpression  =>
return checkPrefixUnaryExpression( node.asInstanceOf[PrefixUnaryExpression] )
  case  SyntaxKind.PostfixUnaryExpression  =>
return checkPostfixUnaryExpression( node.asInstanceOf[PostfixUnaryExpression] )
  case  SyntaxKind.BinaryExpression  =>
return checkBinaryExpression( node.asInstanceOf[BinaryExpression], contextualMapper )
  case  SyntaxKind.ConditionalExpression  =>
return checkConditionalExpression( node.asInstanceOf[ConditionalExpression], contextualMapper )
  case  SyntaxKind.SpreadElementExpression  =>
return checkSpreadElementExpression( node.asInstanceOf[SpreadElementExpression], contextualMapper )
  case  SyntaxKind.OmittedExpression  =>
return undefinedWideningType
  case  SyntaxKind.YieldExpression  =>
return checkYieldExpression( node.asInstanceOf[YieldExpression] )
  case  SyntaxKind.JsxExpression  =>
return checkJsxExpression( node.asInstanceOf[JsxExpression] )
  case  SyntaxKind.JsxElement  =>
return checkJsxElement( node.asInstanceOf[JsxElement] )
  case  SyntaxKind.JsxSelfClosingElement  =>
return checkJsxSelfClosingElement( node.asInstanceOf[JsxSelfClosingElement] )
  case  SyntaxKind.JsxOpeningElement  =>
Debug.fail( "Shouldn't ever directly check a JsxOpeningElement" )
  case _ =>
}
return unknownType

}
def checkTypeParameter(node: TypeParameterDeclaration) = {
 if (node.expression) {
 grammarErrorOnFirstToken( node.expression, Diagnostics.Type_expected )

}
checkSourceElement( node.constraint )
getConstraintOfTypeParameter( getDeclaredTypeOfTypeParameter( getSymbolOfNode( node ) ) )
if (produceDiagnostics) {
 checkTypeNameIsReserved( node.name, Diagnostics.Type_parameter_name_cannot_be_0 )

}

}
def checkParameter(node: ParameterDeclaration) = {
 (checkGrammarDecorators( node )||checkGrammarModifiers( node ))
checkVariableLikeDeclaration( node )
var func = getContainingFunction( node )
if ((getModifierFlags( node )&ModifierFlags.ParameterPropertyModifier)) {
 (func=getContainingFunction( node ))
if ((!(((func.kind===SyntaxKind.Constructor)&&nodeIsPresent( func.body ))))) {
 error( node, Diagnostics.A_parameter_property_is_only_allowed_in_a_constructor_implementation )

}

}
if (((node.questionToken&&isBindingPattern( node.name ))&&func.body)) {
 error( node, Diagnostics.A_binding_pattern_parameter_cannot_be_optional_in_an_implementation_signature )

}
if (((node.name.asInstanceOf[Identifier]).text==="this")) {
 if ((indexOf( func.parameters, node )!==0)) {
 error( node, Diagnostics.A_this_parameter_must_be_the_first_parameter )

}
if ((((func.kind===SyntaxKind.Constructor)||(func.kind===SyntaxKind.ConstructSignature))||(func.kind===SyntaxKind.ConstructorType))) {
 error( node, Diagnostics.A_constructor_cannot_have_a_this_parameter )

}

}
if (((node.dotDotDotToken&&(!isBindingPattern( node.name )))&&(!isArrayType( getTypeOfSymbol( node.symbol ) )))) {
 error( node, Diagnostics.A_rest_parameter_must_be_of_an_array_type )

}

}
def isSyntacticallyValidGenerator(node: SignatureDeclaration): Boolean = {
 if (((!(node.asInstanceOf[FunctionLikeDeclaration]).asteriskToken)||(!(node.asInstanceOf[FunctionLikeDeclaration]).body))) {
 return false

}
return (((node.kind===SyntaxKind.MethodDeclaration)||(node.kind===SyntaxKind.FunctionDeclaration))||(node.kind===SyntaxKind.FunctionExpression))

}
def getTypePredicateParameterIndex(parameterList: NodeArray[ParameterDeclaration], parameter: Identifier): Int = {
 if (parameterList) {
 {
var i = 0
while( (i<parameterList.length)) {
 {
 val param = parameterList(i)
if (((param.name.kind===SyntaxKind.Identifier)&&((param.name.asInstanceOf[Identifier]).text===parameter.text))) {
 return i

}

}
 (i+= 1)
}
}

}
return (-1)

}
def checkTypePredicate(node: TypePredicateNode): Unit = {
 val parent = getTypePredicateParent( node )
if ((!parent)) {
 error( node, Diagnostics.A_type_predicate_is_only_allowed_in_return_type_position_for_functions_and_methods )
return

}
val typePredicate = getSignatureFromDeclaration( parent ).typePredicate
if ((!typePredicate)) {
 return

}
const fresh103 = node
val parameterName = fresh103.parameterName
if (isThisTypePredicate( typePredicate )) {
 getTypeFromThisTypeNode( parameterName.asInstanceOf[ThisTypeNode] )

}
else {
 if ((typePredicate.parameterIndex>=0)) {
 if (parent.parameters(typePredicate.parameterIndex).dotDotDotToken) {
 error( parameterName, Diagnostics.A_type_predicate_cannot_reference_a_rest_parameter )

}
else {
 val leadingError = chainDiagnosticMessages( undefined, Diagnostics.A_type_predicate_s_type_must_be_assignable_to_its_parameter_s_type )
checkTypeAssignableTo( typePredicate.`type`, getTypeOfNode( parent.parameters(typePredicate.parameterIndex) ), node.`type`, undefined, leadingError )

}

}
else if (parameterName) {
 var hasReportedError = false
(parent.parameters).foreach { fresh104 =>
const fresh105 = zeroOfMyType
val name = fresh105.name
 = fresh104
 {
 if ((isBindingPattern( name )&&checkIfTypePredicateVariableIsDeclaredInBindingPattern( name, parameterName, typePredicate.parameterName ))) {
 (hasReportedError=true)
break()

}

}
}
if ((!hasReportedError)) {
 error( node.parameterName, Diagnostics.Cannot_find_parameter_0, typePredicate.parameterName )

}

}

}

}
def getTypePredicateParent(node: Node): SignatureDeclaration = {
 node.parent.kind match {
  case  SyntaxKind.ArrowFunction | SyntaxKind.CallSignature | SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression | SyntaxKind.FunctionType | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature  =>
val parent = node.parent.asInstanceOf[SignatureDeclaration]
if ((node===parent.`type`)) {
 return parent

}
  case _ =>
}

}
def checkIfTypePredicateVariableIsDeclaredInBindingPattern(pattern: BindingPattern, predicateVariableNode: Node, predicateVariableName: String) = {
 (pattern.elements).foreach { fresh106 =>
val element = zeroOfMyType
 = fresh106
 {
 if (isOmittedExpression( element )) {
 continue

}
val name = element.name
if (((name.kind===SyntaxKind.Identifier)&&((name.asInstanceOf[Identifier]).text===predicateVariableName))) {
 error( predicateVariableNode, Diagnostics.A_type_predicate_cannot_reference_element_0_in_a_binding_pattern, predicateVariableName )
return true

}
else if (((name.kind===SyntaxKind.ArrayBindingPattern)||(name.kind===SyntaxKind.ObjectBindingPattern))) {
 if (checkIfTypePredicateVariableIsDeclaredInBindingPattern( name.asInstanceOf[BindingPattern], predicateVariableNode, predicateVariableName )) {
 return true

}

}

}
}

}
def checkSignatureDeclaration(node: SignatureDeclaration) = {
 if ((node.kind===SyntaxKind.IndexSignature)) {
 checkGrammarIndexSignature( node.asInstanceOf[SignatureDeclaration] )

}
else if (((((((node.kind===SyntaxKind.FunctionType)||(node.kind===SyntaxKind.FunctionDeclaration))||(node.kind===SyntaxKind.ConstructorType))||(node.kind===SyntaxKind.CallSignature))||(node.kind===SyntaxKind.Constructor))||(node.kind===SyntaxKind.ConstructSignature))) {
 checkGrammarFunctionLikeDeclaration( node.asInstanceOf[FunctionLikeDeclaration] )

}
checkTypeParameters( node.typeParameters )
forEach( node.parameters, checkParameter )
if (node.`type`) {
 checkSourceElement( node.`type` )

}
if (produceDiagnostics) {
 checkCollisionWithArgumentsInGeneratedCode( node )
if ((compilerOptions.noImplicitAny&&(!node.`type`))) {
 node.kind match {
  case  SyntaxKind.ConstructSignature  =>
error( node, Diagnostics.Construct_signature_which_lacks_return_type_annotation_implicitly_has_an_any_return_type )
  case  SyntaxKind.CallSignature  =>
error( node, Diagnostics.Call_signature_which_lacks_return_type_annotation_implicitly_has_an_any_return_type )
  case _ =>
}

}
if (node.`type`) {
 if (((languageVersion>=ScriptTarget.ES2015)&&isSyntacticallyValidGenerator( node ))) {
 val returnType = getTypeFromTypeNode( node.`type` )
if ((returnType===voidType)) {
 error( node.`type`, Diagnostics.A_generator_cannot_have_a_void_type_annotation )

}
else {
 val generatorElementType = (getElementTypeOfIterableIterator( returnType )||anyType)
val iterableIteratorInstantiation = createIterableIteratorType( generatorElementType )
checkTypeAssignableTo( iterableIteratorInstantiation, returnType, node.`type` )

}

}
else if (isAsyncFunctionLike( node )) {
 checkAsyncFunctionReturnType( node.asInstanceOf[FunctionLikeDeclaration] )

}

}
if ((noUnusedIdentifiers&&(!(node.asInstanceOf[FunctionDeclaration]).body))) {
 checkUnusedTypeParameters( node )

}

}

}
def checkClassForDuplicateDeclarations(node: ClassLikeDeclaration) = {
 sealed abstract class Accessor
object Accessor {
   case object Getter extends Accessor
  case object Setter extends Accessor
  case object Property extends Accessor
}
val instanceNames = createMap[ Accessor ]()
val staticNames = createMap[ Accessor ]()
(node.members).foreach { fresh107 =>
val member = zeroOfMyType
 = fresh107
 {
 if ((member.kind===SyntaxKind.Constructor)) {
 ((member.asInstanceOf[ConstructorDeclaration]).parameters).foreach { fresh108 =>
val param = zeroOfMyType
 = fresh108
 {
 if (isParameterPropertyDeclaration( param )) {
 addName( instanceNames, param.name, (param.name.asInstanceOf[Identifier]).text, Accessor.Property )

}

}
}

}
else {
 val isStatic = forEach( member.modifiers, (m =>  (m.kind===SyntaxKind.StaticKeyword)) )
val names = (if (isStatic) staticNames else instanceNames)
val memberName = (member.name&&getPropertyNameForPropertyNameNode( member.name ))
if (memberName) {
 member.kind match {
  case  SyntaxKind.GetAccessor  =>
addName( names, member.name, memberName, Accessor.Getter )
  case  SyntaxKind.SetAccessor  =>
addName( names, member.name, memberName, Accessor.Setter )
  case  SyntaxKind.PropertyDeclaration  =>
addName( names, member.name, memberName, Accessor.Property )
  case _ =>
}

}

}

}
}
def addName(names: Map[Accessor], location: Node, name: String, meaning: Accessor) = {
 val prev = names(name)
if (prev) {
 if ((prev&meaning)) {
 error( location, Diagnostics.Duplicate_identifier_0, getTextOfNode( location ) )

}
else {
 (names(name)=(prev|meaning))

}

}
else {
 (names(name)=meaning)

}

}

}
def checkObjectTypeForDuplicateDeclarations(node: ( TypeLiteralNode | InterfaceDeclaration )) = {
 val names = createMap[ Boolean ]()
(node.members).foreach { fresh109 =>
val member = zeroOfMyType
 = fresh109
 {
 if ((member.kind==SyntaxKind.PropertySignature)) {
 var memberName: String = zeroOfMyType
member.name.kind match {
  case  SyntaxKind.StringLiteral | SyntaxKind.NumericLiteral | SyntaxKind.Identifier  =>
(memberName=(member.name.asInstanceOf[( LiteralExpression | Identifier )]).text)
  case _ =>
continue
}
if (names(memberName)) {
 error( member.symbol.valueDeclaration.name, Diagnostics.Duplicate_identifier_0, memberName )
error( member.name, Diagnostics.Duplicate_identifier_0, memberName )

}
else {
 (names(memberName)=true)

}

}

}
}

}
def checkTypeForDuplicateIndexSignatures(node: Node) = {
 if ((node.kind===SyntaxKind.InterfaceDeclaration)) {
 val nodeSymbol = getSymbolOfNode( node )
if (((nodeSymbol.declarations.length>0)&&(nodeSymbol.declarations(0)!==node))) {
 return

}

}
val indexSymbol = getIndexSymbol( getSymbolOfNode( node ) )
if (indexSymbol) {
 var seenNumericIndexer = false
var seenStringIndexer = false
(indexSymbol.declarations).foreach { fresh110 =>
val decl = zeroOfMyType
 = fresh110
 {
 val declaration = decl.asInstanceOf[SignatureDeclaration]
if (((declaration.parameters.length===1)&&declaration.parameters(0).`type`)) {
 declaration.parameters(0).`type`.kind match {
  case  SyntaxKind.StringKeyword  =>
if ((!seenStringIndexer)) {
 (seenStringIndexer=true)

}
else {
 error( declaration, Diagnostics.Duplicate_string_index_signature )

}
  case  SyntaxKind.NumberKeyword  =>
if ((!seenNumericIndexer)) {
 (seenNumericIndexer=true)

}
else {
 error( declaration, Diagnostics.Duplicate_number_index_signature )

}
  case _ =>
}

}

}
}

}

}
def checkPropertyDeclaration(node: PropertyDeclaration) = {
 (((checkGrammarDecorators( node )||checkGrammarModifiers( node ))||checkGrammarProperty( node ))||checkGrammarComputedPropertyName( node.name ))
checkVariableLikeDeclaration( node )

}
def checkMethodDeclaration(node: MethodDeclaration) = {
 (checkGrammarMethod( node )||checkGrammarComputedPropertyName( node.name ))
checkFunctionOrMethodDeclaration( node )
if (((getModifierFlags( node )&ModifierFlags.Abstract)&&node.body)) {
 error( node, Diagnostics.Method_0_cannot_have_an_implementation_because_it_is_marked_abstract, declarationNameToString( node.name ) )

}

}
def checkConstructorDeclaration(node: ConstructorDeclaration) = {
 checkSignatureDeclaration( node )
(checkGrammarConstructorTypeParameters( node )||checkGrammarConstructorTypeAnnotation( node ))
checkSourceElement( node.body )
registerForUnusedIdentifiersCheck( node )
val symbol = getSymbolOfNode( node )
val firstDeclaration = getDeclarationOfKind( symbol, node.kind )
if ((node===firstDeclaration)) {
 checkFunctionOrConstructorSymbol( symbol )

}
if (nodeIsMissing( node.body )) {
 return

}
if ((!produceDiagnostics)) {
 return

}
def containsSuperCallAsComputedPropertyName(n: Declaration): Boolean = {
 return (n.name&&containsSuperCall( n.name ))

}
def containsSuperCall(n: Node): Boolean = {
 if (isSuperCall( n )) {
 return true

}
else if (isFunctionLike( n )) {
 return false

}
else if (isClassLike( n )) {
 return forEach( (n.asInstanceOf[ClassLikeDeclaration]).members, containsSuperCallAsComputedPropertyName )

}
return forEachChild( n, containsSuperCall )

}
def markThisReferencesAsErrors(n: Node): Unit = {
 if ((n.kind===SyntaxKind.ThisKeyword)) {
 error( n, Diagnostics.this_cannot_be_referenced_in_current_location )

}
else if (((n.kind!==SyntaxKind.FunctionExpression)&&(n.kind!==SyntaxKind.FunctionDeclaration))) {
 forEachChild( n, markThisReferencesAsErrors )

}

}
def isInstancePropertyWithInitializer(n: Node): Boolean = {
 return (((n.kind===SyntaxKind.PropertyDeclaration)&&(!((getModifierFlags( n )&ModifierFlags.Static))))&&(!(!(n.asInstanceOf[PropertyDeclaration]).initializer)))

}
val containingClassDecl = node.parent.asInstanceOf[ClassDeclaration]
if (getClassExtendsHeritageClauseElement( containingClassDecl )) {
 captureLexicalThis( node.parent, containingClassDecl )
val classExtendsNull = classDeclarationExtendsNull( containingClassDecl )
val superCall = getSuperCallInConstructor( node )
if (superCall) {
 if (classExtendsNull) {
 error( superCall, Diagnostics.A_constructor_cannot_contain_a_super_call_when_its_class_extends_null )

}
val superCallShouldBeFirst = (forEach( (node.parent.asInstanceOf[ClassDeclaration]).members, isInstancePropertyWithInitializer )||forEach( node.parameters, (p =>  (getModifierFlags( p )&ModifierFlags.ParameterPropertyModifier)) ))
if (superCallShouldBeFirst) {
 val statements = (node.body.asInstanceOf[Block]).statements
var superCallStatement: ExpressionStatement = zeroOfMyType
(statements).foreach { fresh111 =>
val statement = zeroOfMyType
 = fresh111
 {
 if (((statement.kind===SyntaxKind.ExpressionStatement)&&isSuperCall( (statement.asInstanceOf[ExpressionStatement]).expression ))) {
 (superCallStatement=statement.asInstanceOf[ExpressionStatement])
break()

}
if ((!isPrologueDirective( statement ))) {
 break()

}

}
}
if ((!superCallStatement)) {
 error( node, Diagnostics.A_super_call_must_be_the_first_statement_in_the_constructor_when_a_class_contains_initialized_properties_or_has_parameter_properties )

}

}

}
else if ((!classExtendsNull)) {
 error( node, Diagnostics.Constructors_for_derived_classes_must_contain_a_super_call )

}

}

}
def checkAccessorDeclaration(node: AccessorDeclaration) = {
 if (produceDiagnostics) {
 ((checkGrammarFunctionLikeDeclaration( node )||checkGrammarAccessor( node ))||checkGrammarComputedPropertyName( node.name ))
checkDecorators( node )
checkSignatureDeclaration( node )
if ((node.kind===SyntaxKind.GetAccessor)) {
 if ((((!isInAmbientContext( node ))&&nodeIsPresent( node.body ))&&((node.flags&NodeFlags.HasImplicitReturn)))) {
 if ((!((node.flags&NodeFlags.HasExplicitReturn)))) {
 error( node.name, Diagnostics.A_get_accessor_must_return_a_value )

}

}

}
if ((node.name.kind===SyntaxKind.ComputedPropertyName)) {
 checkComputedPropertyName( node.name.asInstanceOf[ComputedPropertyName] )

}
if ((!hasDynamicName( node ))) {
 val otherKind = (if ((node.kind===SyntaxKind.GetAccessor)) SyntaxKind.SetAccessor else SyntaxKind.GetAccessor)
val otherAccessor = getDeclarationOfKind( node.symbol, otherKind ).asInstanceOf[AccessorDeclaration]
if (otherAccessor) {
 if ((((getModifierFlags( node )&ModifierFlags.AccessibilityModifier))!==((getModifierFlags( otherAccessor )&ModifierFlags.AccessibilityModifier)))) {
 error( node.name, Diagnostics.Getter_and_setter_accessors_do_not_agree_in_visibility )

}
if ((hasModifier( node, ModifierFlags.Abstract )!==hasModifier( otherAccessor, ModifierFlags.Abstract ))) {
 error( node.name, Diagnostics.Accessors_must_both_be_abstract_or_non_abstract )

}
checkAccessorDeclarationTypesIdentical( node, otherAccessor, getAnnotatedAccessorType, Diagnostics.get_and_set_accessor_must_have_the_same_type )
checkAccessorDeclarationTypesIdentical( node, otherAccessor, getThisTypeOfDeclaration, Diagnostics.get_and_set_accessor_must_have_the_same_this_type )

}

}
val returnType = getTypeOfAccessors( getSymbolOfNode( node ) )
if ((node.kind===SyntaxKind.GetAccessor)) {
 checkAllCodePathsInNonVoidFunctionReturnOrThrow( node, returnType )

}

}
if ((node.parent.kind!==SyntaxKind.ObjectLiteralExpression)) {
 checkSourceElement( node.body )
registerForUnusedIdentifiersCheck( node )

}
else {
 checkNodeDeferred( node )

}

}
def checkAccessorDeclarationTypesIdentical(first: AccessorDeclaration, second: AccessorDeclaration, getAnnotatedType: ((AccessorDeclaration) => Type), message: DiagnosticMessage) = {
 val firstType = getAnnotatedType( first )
val secondType = getAnnotatedType( second )
if (((firstType&&secondType)&&(!isTypeIdenticalTo( firstType, secondType )))) {
 error( first, message )

}

}
def checkAccessorDeferred(node: AccessorDeclaration) = {
 checkSourceElement( node.body )
registerForUnusedIdentifiersCheck( node )

}
def checkMissingDeclaration(node: Node) = {
 checkDecorators( node )

}
def checkTypeArgumentConstraints(typeParameters: Array[TypeParameter], typeArgumentNodes: Array[TypeNode]): Boolean = {
 var typeArguments: Array[Type] = zeroOfMyType
var mapper: TypeMapper = zeroOfMyType
var result = true
{
var i = 0
while( (i<typeParameters.length)) {
 {
 val constraint = getConstraintOfTypeParameter( typeParameters(i) )
if (constraint) {
 if ((!typeArguments)) {
 (typeArguments=map( typeArgumentNodes, getTypeFromTypeNodeNoAlias ))
(mapper=createTypeMapper( typeParameters, typeArguments ))

}
val typeArgument = typeArguments(i)
(result=(result&&checkTypeAssignableTo( typeArgument, getTypeWithThisArgument( instantiateType( constraint, mapper ), typeArgument ), typeArgumentNodes(i), Diagnostics.Type_0_does_not_satisfy_the_constraint_1 )))

}

}
 (i+= 1)
}
}
return result

}
def checkTypeReferenceNode(node: ( TypeReferenceNode | ExpressionWithTypeArguments )) = {
 checkGrammarTypeArguments( node, node.typeArguments )
val `type` = getTypeFromTypeReference( node )
if ((`type`!==unknownType)) {
 if (node.typeArguments) {
 forEach( node.typeArguments, checkSourceElement )
if (produceDiagnostics) {
 val symbol = getNodeLinks( node ).resolvedSymbol
val typeParameters = (if ((symbol.flags&SymbolFlags.TypeAlias)) getSymbolLinks( symbol ).typeParameters else (`type`.asInstanceOf[TypeReference]).target.localTypeParameters)
checkTypeArgumentConstraints( typeParameters, node.typeArguments )

}

}
if ((((`type`.flags&TypeFlags.Enum)&&(!(`type`.asInstanceOf[EnumType]).memberTypes))&&(getNodeLinks( node ).resolvedSymbol.flags&SymbolFlags.EnumMember))) {
 error( node, Diagnostics.Enum_type_0_has_members_with_initializers_that_are_not_literals, typeToString( `type` ) )

}

}

}
def checkTypeQuery(node: TypeQueryNode) = {
 getTypeFromTypeQueryNode( node )

}
def checkTypeLiteral(node: TypeLiteralNode) = {
 forEach( node.members, checkSourceElement )
if (produceDiagnostics) {
 val `type` = getTypeFromTypeLiteralOrFunctionOrConstructorTypeNode( node )
checkIndexConstraints( `type` )
checkTypeForDuplicateIndexSignatures( node )
checkObjectTypeForDuplicateDeclarations( node )

}

}
def checkArrayType(node: ArrayTypeNode) = {
 checkSourceElement( node.elementType )

}
def checkTupleType(node: TupleTypeNode) = {
 val hasErrorFromDisallowedTrailingComma = checkGrammarForDisallowedTrailingComma( node.elementTypes )
if (((!hasErrorFromDisallowedTrailingComma)&&(node.elementTypes.length===0))) {
 grammarErrorOnNode( node, Diagnostics.A_tuple_type_element_list_cannot_be_empty )

}
forEach( node.elementTypes, checkSourceElement )

}
def checkUnionOrIntersectionType(node: UnionOrIntersectionTypeNode) = {
 forEach( node.types, checkSourceElement )

}
def isPrivateWithinAmbient(node: Node): Boolean = {
 return (((getModifierFlags( node )&ModifierFlags.Private))&&isInAmbientContext( node ))

}
def getEffectiveDeclarationFlags(n: Node, flagsToCheck: ModifierFlags): ModifierFlags = {
 var flags = getCombinedModifierFlags( n )
if (((((n.parent.kind!==SyntaxKind.InterfaceDeclaration)&&(n.parent.kind!==SyntaxKind.ClassDeclaration))&&(n.parent.kind!==SyntaxKind.ClassExpression))&&isInAmbientContext( n ))) {
 if ((!((flags&ModifierFlags.Ambient)))) {
 (flags|=ModifierFlags.Export)

}
(flags|=ModifierFlags.Ambient)

}
return (flags&flagsToCheck)

}
def checkFunctionOrConstructorSymbol(symbol: Symbol): Unit = {
 if ((!produceDiagnostics)) {
 return

}
def getCanonicalOverload(overloads: Array[Declaration], implementation: FunctionLikeDeclaration) = {
 val implementationSharesContainerWithFirstOverload = ((implementation!==undefined)&&(implementation.parent===overloads(0).parent))
return (if (implementationSharesContainerWithFirstOverload) implementation else overloads(0))

}
def checkFlagAgreementBetweenOverloads(overloads: Array[Declaration], implementation: FunctionLikeDeclaration, flagsToCheck: ModifierFlags, someOverloadFlags: ModifierFlags, allOverloadFlags: ModifierFlags): Unit = {
 val someButNotAllOverloadFlags = (someOverloadFlags^allOverloadFlags)
if ((someButNotAllOverloadFlags!==0)) {
 val canonicalFlags = getEffectiveDeclarationFlags( getCanonicalOverload( overloads, implementation ), flagsToCheck )
forEach( overloads, (o =>  {
 val deviation = (getEffectiveDeclarationFlags( o, flagsToCheck )^canonicalFlags)
 if ((deviation&ModifierFlags.Export)) {
 error( o.name, Diagnostics.Overload_signatures_must_all_be_exported_or_non_exported )

}
else if ((deviation&ModifierFlags.Ambient)) {
 error( o.name, Diagnostics.Overload_signatures_must_all_be_ambient_or_non_ambient )

}
else if ((deviation&((ModifierFlags.Private|ModifierFlags.Protected)))) {
 error( (o.name||o), Diagnostics.Overload_signatures_must_all_be_public_private_or_protected )

}
else if ((deviation&ModifierFlags.Abstract)) {
 error( o.name, Diagnostics.Overload_signatures_must_all_be_abstract_or_non_abstract )

}

}) )

}

}
def checkQuestionTokenAgreementBetweenOverloads(overloads: Array[Declaration], implementation: FunctionLikeDeclaration, someHaveQuestionToken: Boolean, allHaveQuestionToken: Boolean): Unit = {
 if ((someHaveQuestionToken!==allHaveQuestionToken)) {
 val canonicalHasQuestionToken = hasQuestionToken( getCanonicalOverload( overloads, implementation ) )
forEach( overloads, (o =>  {
 val deviation = (hasQuestionToken( o )!==canonicalHasQuestionToken)
 if (deviation) {
 error( o.name, Diagnostics.Overload_signatures_must_all_be_optional_or_required )

}

}) )

}

}
val flagsToCheck: ModifierFlags = ((((ModifierFlags.Export|ModifierFlags.Ambient)|ModifierFlags.Private)|ModifierFlags.Protected)|ModifierFlags.Abstract)
var someNodeFlags: ModifierFlags = ModifierFlags.None
var allNodeFlags = flagsToCheck
var someHaveQuestionToken = false
var allHaveQuestionToken = true
var hasOverloads = false
var bodyDeclaration: FunctionLikeDeclaration = zeroOfMyType
var lastSeenNonAmbientDeclaration: FunctionLikeDeclaration = zeroOfMyType
var previousDeclaration: FunctionLikeDeclaration = zeroOfMyType
val declarations = symbol.declarations
val isConstructor = (((symbol.flags&SymbolFlags.Constructor))!==0)
def reportImplementationExpectedError(node: FunctionLikeDeclaration): Unit = {
 if ((node.name&&nodeIsMissing( node.name ))) {
 return

}
var seen = false
val subsequentNode = forEachChild( node.parent, (c =>  {
 if (seen) {
 return c

}
else {
 (seen=(c===node))

}

}) )
if ((subsequentNode&&(subsequentNode.pos===node.end))) {
 if ((subsequentNode.kind===node.kind)) {
 val errorNode: Node = ((subsequentNode.asInstanceOf[FunctionLikeDeclaration]).name||subsequentNode)
if (((node.name&&(subsequentNode.asInstanceOf[FunctionLikeDeclaration]).name)&&((node.name.asInstanceOf[Identifier]).text===((subsequentNode.asInstanceOf[FunctionLikeDeclaration]).name.asInstanceOf[Identifier]).text))) {
 val reportError = ((((node.kind===SyntaxKind.MethodDeclaration)||(node.kind===SyntaxKind.MethodSignature)))&&(((getModifierFlags( node )&ModifierFlags.Static))!==((getModifierFlags( subsequentNode )&ModifierFlags.Static))))
if (reportError) {
 val diagnostic = (if ((getModifierFlags( node )&ModifierFlags.Static)) Diagnostics.Function_overload_must_be_static else Diagnostics.Function_overload_must_not_be_static)
error( errorNode, diagnostic )

}
return

}
else if (nodeIsPresent( (subsequentNode.asInstanceOf[FunctionLikeDeclaration]).body )) {
 error( errorNode, Diagnostics.Function_implementation_name_must_be_0, declarationNameToString( node.name ) )
return

}

}

}
val errorNode: Node = (node.name||node)
if (isConstructor) {
 error( errorNode, Diagnostics.Constructor_implementation_is_missing )

}
else {
 if ((getModifierFlags( node )&ModifierFlags.Abstract)) {
 error( errorNode, Diagnostics.All_declarations_of_an_abstract_method_must_be_consecutive )

}
else {
 error( errorNode, Diagnostics.Function_implementation_is_missing_or_not_immediately_following_the_declaration )

}

}

}
var duplicateFunctionDeclaration = false
var multipleConstructorImplementation = false
(declarations).foreach { fresh112 =>
val current = zeroOfMyType
 = fresh112
 {
 val node = current.asInstanceOf[FunctionLikeDeclaration]
val inAmbientContext = isInAmbientContext( node )
val inAmbientContextOrInterface = (((node.parent.kind===SyntaxKind.InterfaceDeclaration)||(node.parent.kind===SyntaxKind.TypeLiteral))||inAmbientContext)
if (inAmbientContextOrInterface) {
 (previousDeclaration=undefined)

}
if (((((node.kind===SyntaxKind.FunctionDeclaration)||(node.kind===SyntaxKind.MethodDeclaration))||(node.kind===SyntaxKind.MethodSignature))||(node.kind===SyntaxKind.Constructor))) {
 val currentNodeFlags = getEffectiveDeclarationFlags( node, flagsToCheck )
(someNodeFlags|=currentNodeFlags)
(allNodeFlags&=currentNodeFlags)
(someHaveQuestionToken=(someHaveQuestionToken||hasQuestionToken( node )))
(allHaveQuestionToken=(allHaveQuestionToken&&hasQuestionToken( node )))
if ((nodeIsPresent( node.body )&&bodyDeclaration)) {
 if (isConstructor) {
 (multipleConstructorImplementation=true)

}
else {
 (duplicateFunctionDeclaration=true)

}

}
else if (((previousDeclaration&&(previousDeclaration.parent===node.parent))&&(previousDeclaration.end!==node.pos))) {
 reportImplementationExpectedError( previousDeclaration )

}
if (nodeIsPresent( node.body )) {
 if ((!bodyDeclaration)) {
 (bodyDeclaration=node)

}

}
else {
 (hasOverloads=true)

}
(previousDeclaration=node)
if ((!inAmbientContextOrInterface)) {
 (lastSeenNonAmbientDeclaration=node)

}

}

}
}
if (multipleConstructorImplementation) {
 forEach( declarations, (declaration =>  {
 error( declaration, Diagnostics.Multiple_constructor_implementations_are_not_allowed )

}) )

}
if (duplicateFunctionDeclaration) {
 forEach( declarations, (declaration =>  {
 error( declaration.name, Diagnostics.Duplicate_function_implementation )

}) )

}
if ((((lastSeenNonAmbientDeclaration&&(!lastSeenNonAmbientDeclaration.body))&&(!((getModifierFlags( lastSeenNonAmbientDeclaration )&ModifierFlags.Abstract))))&&(!lastSeenNonAmbientDeclaration.questionToken))) {
 reportImplementationExpectedError( lastSeenNonAmbientDeclaration )

}
if (hasOverloads) {
 checkFlagAgreementBetweenOverloads( declarations, bodyDeclaration, flagsToCheck, someNodeFlags, allNodeFlags )
checkQuestionTokenAgreementBetweenOverloads( declarations, bodyDeclaration, someHaveQuestionToken, allHaveQuestionToken )
if (bodyDeclaration) {
 val signatures = getSignaturesOfSymbol( symbol )
val bodySignature = getSignatureFromDeclaration( bodyDeclaration )
(signatures).foreach { fresh113 =>
val signature = zeroOfMyType
 = fresh113
 {
 if ((!isImplementationCompatibleWithOverload( bodySignature, signature ))) {
 error( signature.declaration, Diagnostics.Overload_signature_is_not_compatible_with_function_implementation )
break()

}

}
}

}

}

}
def checkExportsOnMergedDeclarations(node: Node): Unit = {
 if ((!produceDiagnostics)) {
 return

}
var symbol = node.localSymbol
if ((!symbol)) {
 (symbol=getSymbolOfNode( node ))
if ((!((symbol.flags&SymbolFlags.Export)))) {
 return

}

}
if ((getDeclarationOfKind( symbol, node.kind )!==node)) {
 return

}
var exportedDeclarationSpaces = SymbolFlags.None
var nonExportedDeclarationSpaces = SymbolFlags.None
var defaultExportedDeclarationSpaces = SymbolFlags.None
(symbol.declarations).foreach { fresh114 =>
val d = zeroOfMyType
 = fresh114
 {
 val declarationSpaces = getDeclarationSpaces( d )
val effectiveDeclarationFlags = getEffectiveDeclarationFlags( d, (ModifierFlags.Export|ModifierFlags.Default) )
if ((effectiveDeclarationFlags&ModifierFlags.Export)) {
 if ((effectiveDeclarationFlags&ModifierFlags.Default)) {
 (defaultExportedDeclarationSpaces|=declarationSpaces)

}
else {
 (exportedDeclarationSpaces|=declarationSpaces)

}

}
else {
 (nonExportedDeclarationSpaces|=declarationSpaces)

}

}
}
val nonDefaultExportedDeclarationSpaces = (exportedDeclarationSpaces|nonExportedDeclarationSpaces)
val commonDeclarationSpacesForExportsAndLocals = (exportedDeclarationSpaces&nonExportedDeclarationSpaces)
val commonDeclarationSpacesForDefaultAndNonDefault = (defaultExportedDeclarationSpaces&nonDefaultExportedDeclarationSpaces)
if ((commonDeclarationSpacesForExportsAndLocals||commonDeclarationSpacesForDefaultAndNonDefault)) {
 (symbol.declarations).foreach { fresh115 =>
val d = zeroOfMyType
 = fresh115
 {
 val declarationSpaces = getDeclarationSpaces( d )
if ((declarationSpaces&commonDeclarationSpacesForDefaultAndNonDefault)) {
 error( d.name, Diagnostics.Merged_declaration_0_cannot_include_a_default_export_declaration_Consider_adding_a_separate_export_default_0_declaration_instead, declarationNameToString( d.name ) )

}
else if ((declarationSpaces&commonDeclarationSpacesForExportsAndLocals)) {
 error( d.name, Diagnostics.Individual_declarations_in_merged_declaration_0_must_be_all_exported_or_all_local, declarationNameToString( d.name ) )

}

}
}

}
def getDeclarationSpaces(d: Declaration): SymbolFlags = {
 d.kind match {
  case  SyntaxKind.InterfaceDeclaration  =>
return SymbolFlags.ExportType
  case  SyntaxKind.ModuleDeclaration  =>
return (if ((isAmbientModule( d )||(getModuleInstanceState( d )!==ModuleInstanceState.NonInstantiated))) (SymbolFlags.ExportNamespace|SymbolFlags.ExportValue) else SymbolFlags.ExportNamespace)
  case  SyntaxKind.ClassDeclaration | SyntaxKind.EnumDeclaration  =>
return (SymbolFlags.ExportType|SymbolFlags.ExportValue)
  case  SyntaxKind.ImportEqualsDeclaration  =>
var result: SymbolFlags = 0
val target = resolveAlias( getSymbolOfNode( d ) )
forEach( target.declarations, (d =>  {
 (result|=getDeclarationSpaces( d ))

}) )
return result
  case _ =>
return SymbolFlags.ExportValue
}

}

}
def checkNonThenableType(`type`: Type, location: Node, message: DiagnosticMessage) = {
 (`type`=getWidenedType( `type` ))
if ((((!isTypeAny( `type` ))&&(!isTypeNever( `type` )))&&isTypeAssignableTo( `type`, getGlobalThenableType() ))) {
 if (location) {
 if ((!message)) {
 (message=Diagnostics.Operand_for_await_does_not_have_a_valid_callable_then_member)

}
error( location, message )

}
return unknownType

}
return `type`

}
def getPromisedType(promise: Type): Type = {
 if (isTypeAny( promise )) {
 return undefined

}
if ((getObjectFlags( promise )&ObjectFlags.Reference)) {
 if ((((promise.asInstanceOf[GenericType]).target===tryGetGlobalPromiseType())||((promise.asInstanceOf[GenericType]).target===getGlobalPromiseLikeType()))) {
 return (promise.asInstanceOf[GenericType]).typeArguments(0)

}

}
val globalPromiseLikeType = getInstantiatedGlobalPromiseLikeType()
if (((globalPromiseLikeType===emptyObjectType)||(!isTypeAssignableTo( promise, globalPromiseLikeType )))) {
 return undefined

}
val thenFunction = getTypeOfPropertyOfType( promise, "then" )
if (((!thenFunction)||isTypeAny( thenFunction ))) {
 return undefined

}
val thenSignatures = getSignaturesOfType( thenFunction, SignatureKind.Call )
if ((thenSignatures.length===0)) {
 return undefined

}
val onfulfilledParameterType = getTypeWithFacts( getUnionType( map( thenSignatures, getTypeOfFirstParameterOfSignature ) ), TypeFacts.NEUndefined )
if (isTypeAny( onfulfilledParameterType )) {
 return undefined

}
val onfulfilledParameterSignatures = getSignaturesOfType( onfulfilledParameterType, SignatureKind.Call )
if ((onfulfilledParameterSignatures.length===0)) {
 return undefined

}
return getUnionType( map( onfulfilledParameterSignatures, getTypeOfFirstParameterOfSignature ), true )

}
def getTypeOfFirstParameterOfSignature(signature: Signature) = {
 return (if ((signature.parameters.length>0)) getTypeAtPosition( signature, 0 ) else neverType)

}
def getAwaitedType(`type`: Type) = {
 return checkAwaitedType( `type`, undefined, undefined )

}
def checkAwaitedType(`type`: Type, location: Node, message: DiagnosticMessage) = {
 return checkAwaitedTypeWorker( `type` )
def checkAwaitedTypeWorker(`type`: Type): Type = {
 if ((`type`.flags&TypeFlags.Union)) {
 val types: Array[Type] = Array()
((`type`.asInstanceOf[UnionType]).types).foreach { fresh116 =>
val constituentType = zeroOfMyType
 = fresh116
 {
 types.push( checkAwaitedTypeWorker( constituentType ) )

}
}
return getUnionType( types, true )

}
else {
 val promisedType = getPromisedType( `type` )
if ((promisedType===undefined)) {
 return checkNonThenableType( `type`, location, message )

}
else {
 if (((`type`.id===promisedType.id)||(indexOf( awaitedTypeStack, promisedType.id )>=0))) {
 if (location) {
 error( location, Diagnostics._0_is_referenced_directly_or_indirectly_in_the_fulfillment_callback_of_its_own_then_method, symbolToString( `type`.symbol ) )

}
return unknownType

}
awaitedTypeStack.push( `type`.id )
val awaitedType = checkAwaitedTypeWorker( promisedType )
awaitedTypeStack.pop()
return awaitedType

}

}

}

}
def checkCorrectPromiseType(returnType: Type, location: Node, diagnostic: DiagnosticMessage, typeName: String) = {
 if ((returnType===unknownType)) {
 return unknownType

}
val globalPromiseType = getGlobalPromiseType()
if (((globalPromiseType===emptyGenericType)||(globalPromiseType===getTargetType( returnType )))) {
 return checkAwaitedType( returnType, location, Diagnostics.An_async_function_or_method_must_have_a_valid_awaitable_return_type )

}
error( location, diagnostic, typeName )
return unknownType

}
def checkAsyncFunctionReturnType(node: FunctionLikeDeclaration): Type = {
 if ((languageVersion>=ScriptTarget.ES2015)) {
 val returnType = getTypeFromTypeNode( node.`type` )
return checkCorrectPromiseType( returnType, node.`type`, Diagnostics.The_return_type_of_an_async_function_or_method_must_be_the_global_Promise_T_type )

}
val globalPromiseConstructorLikeType = getGlobalPromiseConstructorLikeType()
if ((globalPromiseConstructorLikeType===emptyObjectType)) {
 return unknownType

}
val promiseType = getTypeFromTypeNode( node.`type` )
if (((promiseType===unknownType)&&compilerOptions.isolatedModules)) {
 return unknownType

}
val promiseConstructor = getNodeLinks( node.`type` ).resolvedSymbol
if (((!promiseConstructor)||(!symbolIsValue( promiseConstructor )))) {
 val typeName = (if (promiseConstructor) symbolToString( promiseConstructor ) else typeToString( promiseType ))
return checkCorrectPromiseType( promiseType, node.`type`, Diagnostics.Type_0_is_not_a_valid_async_function_return_type, typeName )

}
checkReturnTypeAnnotationAsExpression( node )
val promiseConstructorType = getTypeOfSymbol( promiseConstructor )
if ((!checkTypeAssignableTo( promiseConstructorType, globalPromiseConstructorLikeType, node.`type`, Diagnostics.Type_0_is_not_a_valid_async_function_return_type ))) {
 return unknownType

}
val promiseName = getEntityNameFromTypeNode( node.`type` )
val promiseNameOrNamespaceRoot = getFirstIdentifier( promiseName )
val rootSymbol = getSymbol( node.locals, promiseNameOrNamespaceRoot.text, SymbolFlags.Value )
if (rootSymbol) {
 error( rootSymbol.valueDeclaration, Diagnostics.Duplicate_identifier_0_Compiler_uses_declaration_1_to_support_async_functions, promiseNameOrNamespaceRoot.text, getFullyQualifiedName( promiseConstructor ) )
return unknownType

}
return checkAwaitedType( promiseType, node, Diagnostics.An_async_function_or_method_must_have_a_valid_awaitable_return_type )

}
def checkDecorator(node: Decorator): Unit = {
 val signature = getResolvedSignature( node )
val returnType = getReturnTypeOfSignature( signature )
if ((returnType.flags&TypeFlags.Any)) {
 return

}
var expectedReturnType: Type = zeroOfMyType
val headMessage = getDiagnosticHeadMessageForDecoratorResolution( node )
var errorInfo: DiagnosticMessageChain = zeroOfMyType
node.parent.kind match {
  case  SyntaxKind.ClassDeclaration  =>
val classSymbol = getSymbolOfNode( node.parent )
val classConstructorType = getTypeOfSymbol( classSymbol )
(expectedReturnType=getUnionType( Array( classConstructorType, voidType ) ))
  case  SyntaxKind.Parameter  =>
(expectedReturnType=voidType)
(errorInfo=chainDiagnosticMessages( errorInfo, Diagnostics.The_return_type_of_a_parameter_decorator_function_must_be_either_void_or_any ))
  case  SyntaxKind.PropertyDeclaration  =>
(expectedReturnType=voidType)
(errorInfo=chainDiagnosticMessages( errorInfo, Diagnostics.The_return_type_of_a_property_decorator_function_must_be_either_void_or_any ))
  case  SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
val methodType = getTypeOfNode( node.parent )
val descriptorType = createTypedPropertyDescriptorType( methodType )
(expectedReturnType=getUnionType( Array( descriptorType, voidType ) ))
  case _ =>
}
checkTypeAssignableTo( returnType, expectedReturnType, node, headMessage, errorInfo )

}
def checkTypeNodeAsExpression(node: TypeNode) = {
 if ((node&&(node.kind===SyntaxKind.TypeReference))) {
 val root = getFirstIdentifier( (node.asInstanceOf[TypeReferenceNode]).typeName )
val meaning = (if ((root.parent.kind===SyntaxKind.TypeReference)) SymbolFlags.Type else SymbolFlags.Namespace)
val rootSymbol = resolveName( root, root.text, (meaning|SymbolFlags.Alias), undefined, undefined )
if ((rootSymbol&&(rootSymbol.flags&SymbolFlags.Alias))) {
 val aliasTarget = resolveAlias( rootSymbol )
if (((aliasTarget.flags&SymbolFlags.Value)&&(!isConstEnumOrConstEnumOnlyModule( resolveAlias( rootSymbol ) )))) {
 markAliasSymbolAsReferenced( rootSymbol )

}

}

}

}
def checkTypeAnnotationAsExpression(node: VariableLikeDeclaration) = {
 checkTypeNodeAsExpression( (node.asInstanceOf[PropertyDeclaration]).`type` )

}
def checkReturnTypeAnnotationAsExpression(node: FunctionLikeDeclaration) = {
 checkTypeNodeAsExpression( node.`type` )

}
def checkParameterTypeAnnotationsAsExpressions(node: FunctionLikeDeclaration) = {
 (node.parameters).foreach { fresh117 =>
val parameter = zeroOfMyType
 = fresh117
 {
 checkTypeAnnotationAsExpression( parameter )

}
}

}
def checkDecorators(node: Node): Unit = {
 if ((!node.decorators)) {
 return

}
if ((!nodeCanBeDecorated( node ))) {
 return

}
if ((!compilerOptions.experimentalDecorators)) {
 error( node, Diagnostics.Experimental_support_for_decorators_is_a_feature_that_is_subject_to_change_in_a_future_release_Set_the_experimentalDecorators_option_to_remove_this_warning )

}
if (compilerOptions.emitDecoratorMetadata) {
 node.kind match {
  case  SyntaxKind.ClassDeclaration  =>
val `constructor` = getFirstConstructorWithBody( node.asInstanceOf[ClassDeclaration] )
if (`constructor`) {
 checkParameterTypeAnnotationsAsExpressions( `constructor` )

}
  case  SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
checkParameterTypeAnnotationsAsExpressions( node.asInstanceOf[FunctionLikeDeclaration] )
checkReturnTypeAnnotationAsExpression( node.asInstanceOf[FunctionLikeDeclaration] )
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.Parameter  =>
checkTypeAnnotationAsExpression( node.asInstanceOf[( PropertyDeclaration | ParameterDeclaration )] )
  case _ =>
}

}
forEach( node.decorators, checkDecorator )

}
def checkFunctionDeclaration(node: FunctionDeclaration): Unit = {
 if (produceDiagnostics) {
 (checkFunctionOrMethodDeclaration( node )||checkGrammarForGenerator( node ))
checkCollisionWithCapturedSuperVariable( node, node.name )
checkCollisionWithCapturedThisVariable( node, node.name )
checkCollisionWithRequireExportsInGeneratedCode( node, node.name )
checkCollisionWithGlobalPromiseInGeneratedCode( node, node.name )

}

}
def checkFunctionOrMethodDeclaration(node: ( FunctionDeclaration | MethodDeclaration )): Unit = {
 checkDecorators( node )
checkSignatureDeclaration( node )
val isAsync = isAsyncFunctionLike( node )
if ((node.name&&(node.name.kind===SyntaxKind.ComputedPropertyName))) {
 checkComputedPropertyName( node.name.asInstanceOf[ComputedPropertyName] )

}
if ((!hasDynamicName( node ))) {
 val symbol = getSymbolOfNode( node )
val localSymbol = (node.localSymbol||symbol)
val firstDeclaration = forEach( localSymbol.declarations, (declaration =>  (if (((declaration.kind===node.kind)&&(!isSourceFileJavaScript( getSourceFileOfNode( declaration ) )))) declaration else undefined)) )
if ((node===firstDeclaration)) {
 checkFunctionOrConstructorSymbol( localSymbol )

}
if (symbol.parent) {
 if ((getDeclarationOfKind( symbol, node.kind )===node)) {
 checkFunctionOrConstructorSymbol( symbol )

}

}

}
checkSourceElement( node.body )
if ((!node.asteriskToken)) {
 val returnOrPromisedType = (node.`type`&&((if (isAsync) checkAsyncFunctionReturnType( node ) else getTypeFromTypeNode( node.`type` ))))
checkAllCodePathsInNonVoidFunctionReturnOrThrow( node, returnOrPromisedType )

}
if ((produceDiagnostics&&(!node.`type`))) {
 if (((compilerOptions.noImplicitAny&&nodeIsMissing( node.body ))&&(!isPrivateWithinAmbient( node )))) {
 reportImplicitAnyError( node, anyType )

}
if ((node.asteriskToken&&nodeIsPresent( node.body ))) {
 getReturnTypeOfSignature( getSignatureFromDeclaration( node ) )

}

}
registerForUnusedIdentifiersCheck( node )

}
def registerForUnusedIdentifiersCheck(node: Node) = {
 if (deferredUnusedIdentifierNodes) {
 deferredUnusedIdentifierNodes.push( node )

}

}
def checkUnusedIdentifiers() = {
 if (deferredUnusedIdentifierNodes) {
 (deferredUnusedIdentifierNodes).foreach { fresh118 =>
val node = zeroOfMyType
 = fresh118
 {
 node.kind match {
  case  SyntaxKind.SourceFile | SyntaxKind.ModuleDeclaration  =>
checkUnusedModuleMembers( node.asInstanceOf[( ModuleDeclaration | SourceFile )] )
  case  SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression  =>
checkUnusedClassMembers( node.asInstanceOf[( ClassDeclaration | ClassExpression )] )
checkUnusedTypeParameters( node.asInstanceOf[( ClassDeclaration | ClassExpression )] )
  case  SyntaxKind.InterfaceDeclaration  =>
checkUnusedTypeParameters( node.asInstanceOf[InterfaceDeclaration] )
  case  SyntaxKind.Block | SyntaxKind.CaseBlock | SyntaxKind.ForStatement | SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement  =>
checkUnusedLocalsAndParameters( node )
  case  SyntaxKind.Constructor | SyntaxKind.FunctionExpression | SyntaxKind.FunctionDeclaration | SyntaxKind.ArrowFunction | SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
if ((node.asInstanceOf[FunctionLikeDeclaration]).body) {
 checkUnusedLocalsAndParameters( node.asInstanceOf[FunctionLikeDeclaration] )

}
checkUnusedTypeParameters( node.asInstanceOf[FunctionLikeDeclaration] )
  case  SyntaxKind.MethodSignature | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature | SyntaxKind.FunctionType | SyntaxKind.ConstructorType  =>
checkUnusedTypeParameters( node.asInstanceOf[FunctionLikeDeclaration] )
  case _ =>
}
;
}
}

}

}
def checkUnusedLocalsAndParameters(node: Node): Unit = {
 if ((((node.parent.kind!==SyntaxKind.InterfaceDeclaration)&&noUnusedIdentifiers)&&(!isInAmbientContext( node )))) {
 (node.locals).keys.foreach { fresh119 =>
val key = zeroOfMyType
 = fresh119
 {
 val local = node.locals(key)
if ((!local.isReferenced)) {
 if ((local.valueDeclaration&&(getRootDeclaration( local.valueDeclaration ).kind===SyntaxKind.Parameter))) {
 val parameter = getRootDeclaration( local.valueDeclaration ).asInstanceOf[ParameterDeclaration]
if ((((compilerOptions.noUnusedParameters&&(!isParameterPropertyDeclaration( parameter )))&&(!parameterIsThisKeyword( parameter )))&&(!parameterNameStartsWithUnderscore( local.valueDeclaration.name )))) {
 error( local.valueDeclaration.name, Diagnostics._0_is_declared_but_never_used, local.name )

}

}
else if (compilerOptions.noUnusedLocals) {
 forEach( local.declarations, (d =>  errorUnusedLocal( (d.name||d), local.name )) )

}

}

}
}

}

}
def errorUnusedLocal(node: Node, name: String) = {
 if (isIdentifierThatStartsWithUnderScore( node )) {
 val declaration = getRootDeclaration( node.parent )
if (((declaration.kind===SyntaxKind.VariableDeclaration)&&(((declaration.parent.parent.kind===SyntaxKind.ForInStatement)||(declaration.parent.parent.kind===SyntaxKind.ForOfStatement))))) {
 return

}

}
error( node, Diagnostics._0_is_declared_but_never_used, name )

}
def parameterNameStartsWithUnderscore(parameterName: DeclarationName) = {
 return (parameterName&&isIdentifierThatStartsWithUnderScore( parameterName ))

}
def isIdentifierThatStartsWithUnderScore(node: Node) = {
 return ((node.kind===SyntaxKind.Identifier)&&((node.asInstanceOf[Identifier]).text.charCodeAt( 0 )===CharacterCodes._underscore_))

}
def checkUnusedClassMembers(node: ( ClassDeclaration | ClassExpression )): Unit = {
 if ((compilerOptions.noUnusedLocals&&(!isInAmbientContext( node )))) {
 if (node.members) {
 (node.members).foreach { fresh120 =>
val member = zeroOfMyType
 = fresh120
 {
 if (((member.kind===SyntaxKind.MethodDeclaration)||(member.kind===SyntaxKind.PropertyDeclaration))) {
 if (((!member.symbol.isReferenced)&&(getModifierFlags( member )&ModifierFlags.Private))) {
 error( member.name, Diagnostics._0_is_declared_but_never_used, member.symbol.name )

}

}
else if ((member.kind===SyntaxKind.Constructor)) {
 ((member.asInstanceOf[ConstructorDeclaration]).parameters).foreach { fresh121 =>
val parameter = zeroOfMyType
 = fresh121
 {
 if (((!parameter.symbol.isReferenced)&&(getModifierFlags( parameter )&ModifierFlags.Private))) {
 error( parameter.name, Diagnostics.Property_0_is_declared_but_never_used, parameter.symbol.name )

}

}
}

}

}
}

}

}

}
def checkUnusedTypeParameters(node: ( ClassDeclaration | ClassExpression | FunctionDeclaration | MethodDeclaration | FunctionExpression | ArrowFunction | ConstructorDeclaration | SignatureDeclaration | InterfaceDeclaration )) = {
 if ((compilerOptions.noUnusedLocals&&(!isInAmbientContext( node )))) {
 if (node.typeParameters) {
 val symbol = getSymbolOfNode( node )
val lastDeclaration = ((symbol&&symbol.declarations)&&lastOrUndefined( symbol.declarations ))
if ((lastDeclaration!==node)) {
 return

}
(node.typeParameters).foreach { fresh122 =>
val typeParameter = zeroOfMyType
 = fresh122
 {
 if ((!getMergedSymbol( typeParameter.symbol ).isReferenced)) {
 error( typeParameter.name, Diagnostics._0_is_declared_but_never_used, typeParameter.symbol.name )

}

}
}

}

}

}
def checkUnusedModuleMembers(node: ( ModuleDeclaration | SourceFile )): Unit = {
 if ((compilerOptions.noUnusedLocals&&(!isInAmbientContext( node )))) {
 (node.locals).keys.foreach { fresh123 =>
val key = zeroOfMyType
 = fresh123
 {
 val local = node.locals(key)
if (((!local.isReferenced)&&(!local.exportSymbol))) {
 (local.declarations).foreach { fresh124 =>
val declaration = zeroOfMyType
 = fresh124
 {
 if ((!isAmbientModule( declaration ))) {
 error( declaration.name, Diagnostics._0_is_declared_but_never_used, local.name )

}

}
}

}

}
}

}

}
def checkBlock(node: Block) = {
 if ((node.kind===SyntaxKind.Block)) {
 checkGrammarStatementInAmbientContext( node )

}
forEach( node.statements, checkSourceElement )
if (node.locals) {
 registerForUnusedIdentifiersCheck( node )

}

}
def checkCollisionWithArgumentsInGeneratedCode(node: SignatureDeclaration) = {
 if ((((!hasDeclaredRestParameter( node ))||isInAmbientContext( node ))||nodeIsMissing( (node.asInstanceOf[FunctionLikeDeclaration]).body ))) {
 return

}
forEach( node.parameters, (p =>  {
 if (((p.name&&(!isBindingPattern( p.name )))&&((p.name.asInstanceOf[Identifier]).text===argumentsSymbol.name))) {
 error( p, Diagnostics.Duplicate_identifier_arguments_Compiler_uses_arguments_to_initialize_rest_parameters )

}

}) )

}
def needCollisionCheckForIdentifier(node: Node, identifier: Identifier, name: String): Boolean = {
 if ((!((identifier&&(identifier.text===name))))) {
 return false

}
if (((((((node.kind===SyntaxKind.PropertyDeclaration)||(node.kind===SyntaxKind.PropertySignature))||(node.kind===SyntaxKind.MethodDeclaration))||(node.kind===SyntaxKind.MethodSignature))||(node.kind===SyntaxKind.GetAccessor))||(node.kind===SyntaxKind.SetAccessor))) {
 return false

}
if (isInAmbientContext( node )) {
 return false

}
val root = getRootDeclaration( node )
if (((root.kind===SyntaxKind.Parameter)&&nodeIsMissing( (root.parent.asInstanceOf[FunctionLikeDeclaration]).body ))) {
 return false

}
return true

}
def checkCollisionWithCapturedThisVariable(node: Node, name: Identifier): Unit = {
 if (needCollisionCheckForIdentifier( node, name, "_this" )) {
 potentialThisCollisions.push( node )

}

}
def checkIfThisIsCapturedInEnclosingScope(node: Node): Unit = {
 var current = node
while (current) {
{
 if ((getNodeCheckFlags( current )&NodeCheckFlags.CaptureThis)) {
 val isDeclaration = (node.kind!==SyntaxKind.Identifier)
if (isDeclaration) {
 error( (node.asInstanceOf[Declaration]).name, Diagnostics.Duplicate_identifier_this_Compiler_uses_variable_declaration_this_to_capture_this_reference )

}
else {
 error( node, Diagnostics.Expression_resolves_to_variable_declaration_this_that_compiler_uses_to_capture_this_reference )

}
return

}
(current=current.parent)

}
}

}
def checkCollisionWithCapturedSuperVariable(node: Node, name: Identifier) = {
 if ((!needCollisionCheckForIdentifier( node, name, "_super" ))) {
 return

}
val enclosingClass = getContainingClass( node )
if (((!enclosingClass)||isInAmbientContext( enclosingClass ))) {
 return

}
if (getClassExtendsHeritageClauseElement( enclosingClass )) {
 val isDeclaration = (node.kind!==SyntaxKind.Identifier)
if (isDeclaration) {
 error( node, Diagnostics.Duplicate_identifier_super_Compiler_uses_super_to_capture_base_class_reference )

}
else {
 error( node, Diagnostics.Expression_resolves_to_super_that_compiler_uses_to_capture_base_class_reference )

}

}

}
def checkCollisionWithRequireExportsInGeneratedCode(node: Node, name: Identifier) = {
 if ((modulekind>=ModuleKind.ES2015)) {
 return

}
if (((!needCollisionCheckForIdentifier( node, name, "require" ))&&(!needCollisionCheckForIdentifier( node, name, "exports" )))) {
 return

}
if (((node.kind===SyntaxKind.ModuleDeclaration)&&(getModuleInstanceState( node )!==ModuleInstanceState.Instantiated))) {
 return

}
val parent = getDeclarationContainer( node )
if (((parent.kind===SyntaxKind.SourceFile)&&isExternalOrCommonJsModule( parent.asInstanceOf[SourceFile] ))) {
 error( name, Diagnostics.Duplicate_identifier_0_Compiler_reserves_name_1_in_top_level_scope_of_a_module, declarationNameToString( name ), declarationNameToString( name ) )

}

}
def checkCollisionWithGlobalPromiseInGeneratedCode(node: Node, name: Identifier): Unit = {
 if ((!needCollisionCheckForIdentifier( node, name, "Promise" ))) {
 return

}
if (((node.kind===SyntaxKind.ModuleDeclaration)&&(getModuleInstanceState( node )!==ModuleInstanceState.Instantiated))) {
 return

}
val parent = getDeclarationContainer( node )
if ((((parent.kind===SyntaxKind.SourceFile)&&isExternalOrCommonJsModule( parent.asInstanceOf[SourceFile] ))&&(parent.flags&NodeFlags.HasAsyncFunctions))) {
 error( name, Diagnostics.Duplicate_identifier_0_Compiler_reserves_name_1_in_top_level_scope_of_a_module_containing_async_functions, declarationNameToString( name ), declarationNameToString( name ) )

}

}
def checkVarDeclaredNamesNotShadowed(node: ( VariableDeclaration | BindingElement )) = {
 if (((((getCombinedNodeFlags( node )&NodeFlags.BlockScoped))!==0)||isParameterDeclaration( node ))) {
 return

}
if (((node.kind===SyntaxKind.VariableDeclaration)&&(!node.initializer))) {
 return

}
val symbol = getSymbolOfNode( node )
if ((symbol.flags&SymbolFlags.FunctionScopedVariable)) {
 val localDeclarationSymbol = resolveName( node, (node.name.asInstanceOf[Identifier]).text, SymbolFlags.Variable, undefined, undefined )
if (((localDeclarationSymbol&&(localDeclarationSymbol!==symbol))&&(localDeclarationSymbol.flags&SymbolFlags.BlockScopedVariable))) {
 if ((getDeclarationNodeFlagsFromSymbol( localDeclarationSymbol )&NodeFlags.BlockScoped)) {
 val varDeclList = getAncestor( localDeclarationSymbol.valueDeclaration, SyntaxKind.VariableDeclarationList )
val container = (if (((varDeclList.parent.kind===SyntaxKind.VariableStatement)&&varDeclList.parent.parent)) varDeclList.parent.parent else undefined)
val namesShareScope = (container&&((((((container.kind===SyntaxKind.Block)&&isFunctionLike( container.parent ))||(container.kind===SyntaxKind.ModuleBlock))||(container.kind===SyntaxKind.ModuleDeclaration))||(container.kind===SyntaxKind.SourceFile))))
if ((!namesShareScope)) {
 val name = symbolToString( localDeclarationSymbol )
error( node, Diagnostics.Cannot_initialize_outer_scoped_variable_0_in_the_same_scope_as_block_scoped_declaration_1, name, name )

}

}

}

}

}
def checkParameterInitializer(node: VariableLikeDeclaration): Unit = {
 if ((getRootDeclaration( node ).kind!==SyntaxKind.Parameter)) {
 return

}
val func = getContainingFunction( node )
visit( node.initializer )
def visit(n: Node): Unit = {
 if ((isTypeNode( n )||isDeclarationName( n ))) {
 return

}
if ((n.kind===SyntaxKind.PropertyAccessExpression)) {
 return visit( (n.asInstanceOf[PropertyAccessExpression]).expression )

}
else if ((n.kind===SyntaxKind.Identifier)) {
 val symbol = resolveName( n, (n.asInstanceOf[Identifier]).text, (SymbolFlags.Value|SymbolFlags.Alias), undefined, undefined )
if ((((!symbol)||(symbol===unknownSymbol))||(!symbol.valueDeclaration))) {
 return

}
if ((symbol.valueDeclaration===node)) {
 error( n, Diagnostics.Parameter_0_cannot_be_referenced_in_its_initializer, declarationNameToString( node.name ) )
return

}
val enclosingContainer = getEnclosingBlockScopeContainer( symbol.valueDeclaration )
if ((enclosingContainer===func)) {
 if ((symbol.valueDeclaration.kind===SyntaxKind.Parameter)) {
 if ((symbol.valueDeclaration.pos<node.pos)) {
 return

}
var current = n
while ((current!==node.initializer)) {
{
 if (isFunctionLike( current.parent )) {
 return

}
if ((((current.parent.kind===SyntaxKind.PropertyDeclaration)&&(!(hasModifier( current.parent, ModifierFlags.Static ))))&&isClassLike( current.parent.parent ))) {
 return

}
(current=current.parent)

}
}

}
error( n, Diagnostics.Initializer_of_parameter_0_cannot_reference_identifier_1_declared_after_it, declarationNameToString( node.name ), declarationNameToString( n.asInstanceOf[Identifier] ) )

}

}
else {
 return forEachChild( n, visit )

}

}

}
def convertAutoToAny(`type`: Type) = {
 return (if ((`type`===autoType)) anyType else (if ((`type`===autoArrayType)) anyArrayType else `type`))

}
def checkVariableLikeDeclaration(node: VariableLikeDeclaration) = {
 checkDecorators( node )
checkSourceElement( node.`type` )
if ((node.name.kind===SyntaxKind.ComputedPropertyName)) {
 checkComputedPropertyName( node.name.asInstanceOf[ComputedPropertyName] )
if (node.initializer) {
 checkExpressionCached( node.initializer )

}

}
if ((node.kind===SyntaxKind.BindingElement)) {
 if ((node.propertyName&&(node.propertyName.kind===SyntaxKind.ComputedPropertyName))) {
 checkComputedPropertyName( node.propertyName.asInstanceOf[ComputedPropertyName] )

}
val parent = (node.parent.asInstanceOf[BindingPattern]).parent.asInstanceOf[VariableLikeDeclaration]
val parentType = getTypeForBindingElementParent( parent )
val name = (node.propertyName||node.name.asInstanceOf[Identifier])
val property = getPropertyOfType( parentType, getTextOfPropertyName( name ) )
if (((parent.initializer&&property)&&getParentOfSymbol( property ))) {
 checkClassPropertyAccess( parent, parent.initializer, parentType, property )

}

}
if (isBindingPattern( node.name )) {
 forEach( (node.name.asInstanceOf[BindingPattern]).elements, checkSourceElement )

}
if (((node.initializer&&(getRootDeclaration( node ).kind===SyntaxKind.Parameter))&&nodeIsMissing( getContainingFunction( node ).body ))) {
 error( node, Diagnostics.A_parameter_initializer_is_only_allowed_in_a_function_or_constructor_implementation )
return

}
if (isBindingPattern( node.name )) {
 if ((node.initializer&&(node.parent.parent.kind!==SyntaxKind.ForInStatement))) {
 checkTypeAssignableTo( checkExpressionCached( node.initializer ), getWidenedTypeForVariableLikeDeclaration( node ), node, undefined )
checkParameterInitializer( node )

}
return

}
val symbol = getSymbolOfNode( node )
val `type` = convertAutoToAny( getTypeOfVariableOrParameterOrProperty( symbol ) )
if ((node===symbol.valueDeclaration)) {
 if ((node.initializer&&(node.parent.parent.kind!==SyntaxKind.ForInStatement))) {
 checkTypeAssignableTo( checkExpressionCached( node.initializer ), `type`, node, undefined )
checkParameterInitializer( node )

}

}
else {
 val declarationType = convertAutoToAny( getWidenedTypeForVariableLikeDeclaration( node ) )
if ((((`type`!==unknownType)&&(declarationType!==unknownType))&&(!isTypeIdenticalTo( `type`, declarationType )))) {
 error( node.name, Diagnostics.Subsequent_variable_declarations_must_have_the_same_type_Variable_0_must_be_of_type_1_but_here_has_type_2, declarationNameToString( node.name ), typeToString( `type` ), typeToString( declarationType ) )

}
if (node.initializer) {
 checkTypeAssignableTo( checkExpressionCached( node.initializer ), declarationType, node, undefined )

}
if ((!areDeclarationFlagsIdentical( node, symbol.valueDeclaration ))) {
 error( symbol.valueDeclaration.name, Diagnostics.All_declarations_of_0_must_have_identical_modifiers, declarationNameToString( node.name ) )
error( node.name, Diagnostics.All_declarations_of_0_must_have_identical_modifiers, declarationNameToString( node.name ) )

}

}
if (((node.kind!==SyntaxKind.PropertyDeclaration)&&(node.kind!==SyntaxKind.PropertySignature))) {
 checkExportsOnMergedDeclarations( node )
if (((node.kind===SyntaxKind.VariableDeclaration)||(node.kind===SyntaxKind.BindingElement))) {
 checkVarDeclaredNamesNotShadowed( node.asInstanceOf[( VariableDeclaration | BindingElement )] )

}
checkCollisionWithCapturedSuperVariable( node, node.name.asInstanceOf[Identifier] )
checkCollisionWithCapturedThisVariable( node, node.name.asInstanceOf[Identifier] )
checkCollisionWithRequireExportsInGeneratedCode( node, node.name.asInstanceOf[Identifier] )
checkCollisionWithGlobalPromiseInGeneratedCode( node, node.name.asInstanceOf[Identifier] )

}

}
def areDeclarationFlagsIdentical(left: Declaration, right: Declaration) = {
 if (((((left.kind===SyntaxKind.Parameter)&&(right.kind===SyntaxKind.VariableDeclaration)))||(((left.kind===SyntaxKind.VariableDeclaration)&&(right.kind===SyntaxKind.Parameter))))) {
 return true

}
if ((hasQuestionToken( left )!==hasQuestionToken( right ))) {
 return false

}
val interestingFlags = (((((ModifierFlags.Private|ModifierFlags.Protected)|ModifierFlags.Async)|ModifierFlags.Abstract)|ModifierFlags.Readonly)|ModifierFlags.Static)
return (((getModifierFlags( left )&interestingFlags))===((getModifierFlags( right )&interestingFlags)))

}
def checkVariableDeclaration(node: VariableDeclaration) = {
 checkGrammarVariableDeclaration( node )
return checkVariableLikeDeclaration( node )

}
def checkBindingElement(node: BindingElement) = {
 checkGrammarBindingElement( node.asInstanceOf[BindingElement] )
return checkVariableLikeDeclaration( node )

}
def checkVariableStatement(node: VariableStatement) = {
 (((checkGrammarDecorators( node )||checkGrammarModifiers( node ))||checkGrammarVariableDeclarationList( node.declarationList ))||checkGrammarForDisallowedLetOrConstStatement( node ))
forEach( node.declarationList.declarations, checkSourceElement )

}
def checkGrammarDisallowedModifiersOnObjectLiteralExpressionMethod(node: Node) = {
 if ((node.modifiers&&(node.parent.kind===SyntaxKind.ObjectLiteralExpression))) {
 if (isAsyncFunctionLike( node )) {
 if ((node.modifiers.length>1)) {
 return grammarErrorOnFirstToken( node, Diagnostics.Modifiers_cannot_appear_here )

}

}
else {
 return grammarErrorOnFirstToken( node, Diagnostics.Modifiers_cannot_appear_here )

}

}

}
def checkExpressionStatement(node: ExpressionStatement) = {
 checkGrammarStatementInAmbientContext( node )
checkExpression( node.expression )

}
def checkIfStatement(node: IfStatement) = {
 checkGrammarStatementInAmbientContext( node )
checkExpression( node.expression )
checkSourceElement( node.thenStatement )
if ((node.thenStatement.kind===SyntaxKind.EmptyStatement)) {
 error( node.thenStatement, Diagnostics.The_body_of_an_if_statement_cannot_be_the_empty_statement )

}
checkSourceElement( node.elseStatement )

}
def checkDoStatement(node: DoStatement) = {
 checkGrammarStatementInAmbientContext( node )
checkSourceElement( node.statement )
checkExpression( node.expression )

}
def checkWhileStatement(node: WhileStatement) = {
 checkGrammarStatementInAmbientContext( node )
checkExpression( node.expression )
checkSourceElement( node.statement )

}
def checkForStatement(node: ForStatement) = {
 if ((!checkGrammarStatementInAmbientContext( node ))) {
 if ((node.initializer&&(node.initializer.kind===SyntaxKind.VariableDeclarationList))) {
 checkGrammarVariableDeclarationList( node.initializer.asInstanceOf[VariableDeclarationList] )

}

}
if (node.initializer) {
 if ((node.initializer.kind===SyntaxKind.VariableDeclarationList)) {
 forEach( (node.initializer.asInstanceOf[VariableDeclarationList]).declarations, checkVariableDeclaration )

}
else {
 checkExpression( node.initializer.asInstanceOf[Expression] )

}

}
if (node.condition)
checkExpression( node.condition )
if (node.incrementor)
checkExpression( node.incrementor )
checkSourceElement( node.statement )
if (node.locals) {
 registerForUnusedIdentifiersCheck( node )

}

}
def checkForOfStatement(node: ForOfStatement): Unit = {
 checkGrammarForInOrForOfStatement( node )
if ((node.initializer.kind===SyntaxKind.VariableDeclarationList)) {
 checkForInOrForOfVariableDeclaration( node )

}
else {
 val varExpr = node.initializer.asInstanceOf[Expression]
val iteratedType = checkRightHandSideOfForOf( node.expression )
if (((varExpr.kind===SyntaxKind.ArrayLiteralExpression)||(varExpr.kind===SyntaxKind.ObjectLiteralExpression))) {
 checkDestructuringAssignment( varExpr, (iteratedType||unknownType) )

}
else {
 val leftType = checkExpression( varExpr )
checkReferenceExpression( varExpr, Diagnostics.Invalid_left_hand_side_in_for_of_statement, Diagnostics.The_left_hand_side_of_a_for_of_statement_cannot_be_a_constant_or_a_read_only_property )
if (iteratedType) {
 checkTypeAssignableTo( iteratedType, leftType, varExpr, undefined )

}

}

}
checkSourceElement( node.statement )
if (node.locals) {
 registerForUnusedIdentifiersCheck( node )

}

}
def checkForInStatement(node: ForInStatement) = {
 checkGrammarForInOrForOfStatement( node )
if ((node.initializer.kind===SyntaxKind.VariableDeclarationList)) {
 val variable = (node.initializer.asInstanceOf[VariableDeclarationList]).declarations(0)
if ((variable&&isBindingPattern( variable.name ))) {
 error( variable.name, Diagnostics.The_left_hand_side_of_a_for_in_statement_cannot_be_a_destructuring_pattern )

}
checkForInOrForOfVariableDeclaration( node )

}
else {
 val varExpr = node.initializer.asInstanceOf[Expression]
val leftType = checkExpression( varExpr )
if (((varExpr.kind===SyntaxKind.ArrayLiteralExpression)||(varExpr.kind===SyntaxKind.ObjectLiteralExpression))) {
 error( varExpr, Diagnostics.The_left_hand_side_of_a_for_in_statement_cannot_be_a_destructuring_pattern )

}
else if ((!isTypeAnyOrAllConstituentTypesHaveKind( leftType, TypeFlags.StringLike ))) {
 error( varExpr, Diagnostics.The_left_hand_side_of_a_for_in_statement_must_be_of_type_string_or_any )

}
else {
 checkReferenceExpression( varExpr, Diagnostics.Invalid_left_hand_side_in_for_in_statement, Diagnostics.The_left_hand_side_of_a_for_in_statement_cannot_be_a_constant_or_a_read_only_property )

}

}
val rightType = checkNonNullExpression( node.expression )
if ((!isTypeAnyOrAllConstituentTypesHaveKind( rightType, (TypeFlags.Object|TypeFlags.TypeParameter) ))) {
 error( node.expression, Diagnostics.The_right_hand_side_of_a_for_in_statement_must_be_of_type_any_an_object_type_or_a_type_parameter )

}
checkSourceElement( node.statement )
if (node.locals) {
 registerForUnusedIdentifiersCheck( node )

}

}
def checkForInOrForOfVariableDeclaration(iterationStatement: ( ForInStatement | ForOfStatement )): Unit = {
 val variableDeclarationList = iterationStatement.initializer.asInstanceOf[VariableDeclarationList]
if ((variableDeclarationList.declarations.length>=1)) {
 val decl = variableDeclarationList.declarations(0)
checkVariableDeclaration( decl )

}

}
def checkRightHandSideOfForOf(rhsExpression: Expression): Type = {
 val expressionType = checkNonNullExpression( rhsExpression )
return checkIteratedTypeOrElementType( expressionType, rhsExpression, true )

}
def checkIteratedTypeOrElementType(inputType: Type, errorNode: Node, allowStringInput: Boolean): Type = {
 if (isTypeAny( inputType )) {
 return inputType

}
if ((languageVersion>=ScriptTarget.ES2015)) {
 return checkElementTypeOfIterable( inputType, errorNode )

}
if (allowStringInput) {
 return checkElementTypeOfArrayOrString( inputType, errorNode )

}
if (isArrayLikeType( inputType )) {
 val indexType = getIndexTypeOfType( inputType, IndexKind.Number )
if (indexType) {
 return indexType

}

}
if (errorNode) {
 error( errorNode, Diagnostics.Type_0_is_not_an_array_type, typeToString( inputType ) )

}
return unknownType

}
def checkElementTypeOfIterable(iterable: Type, errorNode: Node): Type = {
 val elementType = getElementTypeOfIterable( iterable, errorNode )
if ((errorNode&&elementType)) {
 checkTypeAssignableTo( iterable, createIterableType( elementType ), errorNode )

}
return (elementType||anyType)

}
def getElementTypeOfIterable(`type`: Type, errorNode: Node): Type = {
 if (isTypeAny( `type` )) {
 return undefined

}
val typeAsIterable = `type`.asInstanceOf[IterableOrIteratorType]
if ((!typeAsIterable.iterableElementType)) {
 if ((((getObjectFlags( `type` )&ObjectFlags.Reference))&&((`type`.asInstanceOf[GenericType]).target===getGlobalIterableType()))) {
 (typeAsIterable.iterableElementType=(`type`.asInstanceOf[GenericType]).typeArguments(0))

}
else {
 val iteratorFunction = getTypeOfPropertyOfType( `type`, getPropertyNameForKnownSymbolName( "iterator" ) )
if (isTypeAny( iteratorFunction )) {
 return undefined

}
val iteratorFunctionSignatures = (if (iteratorFunction) getSignaturesOfType( iteratorFunction, SignatureKind.Call ) else emptyArray)
if ((iteratorFunctionSignatures.length===0)) {
 if (errorNode) {
 error( errorNode, Diagnostics.Type_must_have_a_Symbol_iterator_method_that_returns_an_iterator )

}
return undefined

}
(typeAsIterable.iterableElementType=getElementTypeOfIterator( getUnionType( map( iteratorFunctionSignatures, getReturnTypeOfSignature ), true ), errorNode ))

}

}
return typeAsIterable.iterableElementType

}
def getElementTypeOfIterator(`type`: Type, errorNode: Node): Type = {
 if (isTypeAny( `type` )) {
 return undefined

}
val typeAsIterator = `type`.asInstanceOf[IterableOrIteratorType]
if ((!typeAsIterator.iteratorElementType)) {
 if ((((getObjectFlags( `type` )&ObjectFlags.Reference))&&((`type`.asInstanceOf[GenericType]).target===getGlobalIteratorType()))) {
 (typeAsIterator.iteratorElementType=(`type`.asInstanceOf[GenericType]).typeArguments(0))

}
else {
 val iteratorNextFunction = getTypeOfPropertyOfType( `type`, "next" )
if (isTypeAny( iteratorNextFunction )) {
 return undefined

}
val iteratorNextFunctionSignatures = (if (iteratorNextFunction) getSignaturesOfType( iteratorNextFunction, SignatureKind.Call ) else emptyArray)
if ((iteratorNextFunctionSignatures.length===0)) {
 if (errorNode) {
 error( errorNode, Diagnostics.An_iterator_must_have_a_next_method )

}
return undefined

}
val iteratorNextResult = getUnionType( map( iteratorNextFunctionSignatures, getReturnTypeOfSignature ), true )
if (isTypeAny( iteratorNextResult )) {
 return undefined

}
val iteratorNextValue = getTypeOfPropertyOfType( iteratorNextResult, "value" )
if ((!iteratorNextValue)) {
 if (errorNode) {
 error( errorNode, Diagnostics.The_type_returned_by_the_next_method_of_an_iterator_must_have_a_value_property )

}
return undefined

}
(typeAsIterator.iteratorElementType=iteratorNextValue)

}

}
return typeAsIterator.iteratorElementType

}
def getElementTypeOfIterableIterator(`type`: Type): Type = {
 if (isTypeAny( `type` )) {
 return undefined

}
if ((((getObjectFlags( `type` )&ObjectFlags.Reference))&&((`type`.asInstanceOf[GenericType]).target===getGlobalIterableIteratorType()))) {
 return (`type`.asInstanceOf[GenericType]).typeArguments(0)

}
return (getElementTypeOfIterable( `type`, undefined )||getElementTypeOfIterator( `type`, undefined ))

}
def checkElementTypeOfArrayOrString(arrayOrStringType: Type, errorNode: Node): Type = {
 Debug.assert( (languageVersion<ScriptTarget.ES2015) )
var arrayType = arrayOrStringType
if ((arrayOrStringType.flags&TypeFlags.Union)) {
 (arrayType=getUnionType( filter( (arrayOrStringType.asInstanceOf[UnionType]).types, (t =>  (!((t.flags&TypeFlags.StringLike)))) ), true ))

}
else if ((arrayOrStringType.flags&TypeFlags.StringLike)) {
 (arrayType=neverType)

}
val hasStringConstituent = (arrayOrStringType!==arrayType)
var reportedError = false
if (hasStringConstituent) {
 if ((languageVersion<ScriptTarget.ES5)) {
 error( errorNode, Diagnostics.Using_a_string_in_a_for_of_statement_is_only_supported_in_ECMAScript_5_and_higher )
(reportedError=true)

}
if ((arrayType.flags&TypeFlags.Never)) {
 return stringType

}

}
if ((!isArrayLikeType( arrayType ))) {
 if ((!reportedError)) {
 val diagnostic = (if (hasStringConstituent) Diagnostics.Type_0_is_not_an_array_type else Diagnostics.Type_0_is_not_an_array_type_or_a_string_type)
error( errorNode, diagnostic, typeToString( arrayType ) )

}
return (if (hasStringConstituent) stringType else unknownType)

}
val arrayElementType = (getIndexTypeOfType( arrayType, IndexKind.Number )||unknownType)
if (hasStringConstituent) {
 if ((arrayElementType.flags&TypeFlags.StringLike)) {
 return stringType

}
return getUnionType( Array( arrayElementType, stringType ), true )

}
return arrayElementType

}
def checkBreakOrContinueStatement(node: BreakOrContinueStatement) = {
 (checkGrammarStatementInAmbientContext( node )||checkGrammarBreakOrContinueStatement( node ))

}
def isGetAccessorWithAnnotatedSetAccessor(node: FunctionLikeDeclaration) = {
 return (!(!(((node.kind===SyntaxKind.GetAccessor)&&getSetAccessorTypeAnnotationNode( getDeclarationOfKind( node.symbol, SyntaxKind.SetAccessor ).asInstanceOf[SetAccessorDeclaration] )))))

}
def isUnwrappedReturnTypeVoidOrAny(func: FunctionLikeDeclaration, returnType: Type): Boolean = {
 val unwrappedReturnType = (if (isAsyncFunctionLike( func )) getPromisedType( returnType ) else returnType)
return (unwrappedReturnType&&maybeTypeOfKind( unwrappedReturnType, (TypeFlags.Void|TypeFlags.Any) ))

}
def checkReturnStatement(node: ReturnStatement) = {
 if ((!checkGrammarStatementInAmbientContext( node ))) {
 val functionBlock = getContainingFunction( node )
if ((!functionBlock)) {
 grammarErrorOnFirstToken( node, Diagnostics.A_return_statement_can_only_be_used_within_a_function_body )

}

}
val func = getContainingFunction( node )
if (func) {
 val signature = getSignatureFromDeclaration( func )
val returnType = getReturnTypeOfSignature( signature )
if (((strictNullChecks||node.expression)||(returnType.flags&TypeFlags.Never))) {
 val exprType = (if (node.expression) checkExpressionCached( node.expression ) else undefinedType)
if (func.asteriskToken) {
 return

}
if ((func.kind===SyntaxKind.SetAccessor)) {
 if (node.expression) {
 error( node.expression, Diagnostics.Setters_cannot_return_a_value )

}

}
else if ((func.kind===SyntaxKind.Constructor)) {
 if ((node.expression&&(!checkTypeAssignableTo( exprType, returnType, node.expression )))) {
 error( node.expression, Diagnostics.Return_type_of_constructor_signature_must_be_assignable_to_the_instance_type_of_the_class )

}

}
else if ((func.`type`||isGetAccessorWithAnnotatedSetAccessor( func ))) {
 if (isAsyncFunctionLike( func )) {
 val promisedType = getPromisedType( returnType )
val awaitedType = checkAwaitedType( exprType, (node.expression||node), Diagnostics.Return_expression_in_async_function_does_not_have_a_valid_callable_then_member )
if (promisedType) {
 checkTypeAssignableTo( awaitedType, promisedType, (node.expression||node) )

}

}
else {
 checkTypeAssignableTo( exprType, returnType, (node.expression||node) )

}

}

}
else if ((((func.kind!==SyntaxKind.Constructor)&&compilerOptions.noImplicitReturns)&&(!isUnwrappedReturnTypeVoidOrAny( func, returnType )))) {
 error( node, Diagnostics.Not_all_code_paths_return_a_value )

}

}

}
def checkWithStatement(node: WithStatement) = {
 if ((!checkGrammarStatementInAmbientContext( node ))) {
 if ((node.flags&NodeFlags.AwaitContext)) {
 grammarErrorOnFirstToken( node, Diagnostics.with_statements_are_not_allowed_in_an_async_function_block )

}

}
checkExpression( node.expression )
val sourceFile = getSourceFileOfNode( node )
if ((!hasParseDiagnostics( sourceFile ))) {
 val start = getSpanOfTokenAtPosition( sourceFile, node.pos ).start
val end = node.statement.pos
grammarErrorAtPos( sourceFile, start, (end-start), Diagnostics.The_with_statement_is_not_supported_All_symbols_in_a_with_block_will_have_type_any )

}

}
def checkSwitchStatement(node: SwitchStatement) = {
 checkGrammarStatementInAmbientContext( node )
var firstDefaultClause: CaseOrDefaultClause = zeroOfMyType
var hasDuplicateDefaultClause = false
val expressionType = checkExpression( node.expression )
forEach( node.caseBlock.clauses, (clause =>  {
 if (((clause.kind===SyntaxKind.DefaultClause)&&(!hasDuplicateDefaultClause))) {
 if ((firstDefaultClause===undefined)) {
 (firstDefaultClause=clause)

}
else {
 val sourceFile = getSourceFileOfNode( node )
val start = skipTrivia( sourceFile.text, clause.pos )
val end = (if ((clause.statements.length>0)) clause.statements(0).pos else clause.end)
grammarErrorAtPos( sourceFile, start, (end-start), Diagnostics.A_default_clause_cannot_appear_more_than_once_in_a_switch_statement )
(hasDuplicateDefaultClause=true)

}

}
 if ((produceDiagnostics&&(clause.kind===SyntaxKind.CaseClause))) {
 val caseClause = clause.asInstanceOf[CaseClause]
val caseType = checkExpression( caseClause.expression )
if ((!isTypeEqualityComparableTo( expressionType, caseType ))) {
 checkTypeComparableTo( caseType, expressionType, caseClause.expression, undefined )

}

}
 forEach( clause.statements, checkSourceElement )

}) )
if (node.caseBlock.locals) {
 registerForUnusedIdentifiersCheck( node.caseBlock )

}

}
def checkLabeledStatement(node: LabeledStatement) = {
 if ((!checkGrammarStatementInAmbientContext( node ))) {
 var current = node.parent
while (current) {
{
 if (isFunctionLike( current )) {
 break()

}
if (((current.kind===SyntaxKind.LabeledStatement)&&((current.asInstanceOf[LabeledStatement]).label.text===node.label.text))) {
 val sourceFile = getSourceFileOfNode( node )
grammarErrorOnNode( node.label, Diagnostics.Duplicate_label_0, getTextOfNodeFromSourceText( sourceFile.text, node.label ) )
break()

}
(current=current.parent)

}
}

}
checkSourceElement( node.statement )

}
def checkThrowStatement(node: ThrowStatement) = {
 if ((!checkGrammarStatementInAmbientContext( node ))) {
 if ((node.expression===undefined)) {
 grammarErrorAfterFirstToken( node, Diagnostics.Line_break_not_permitted_here )

}

}
if (node.expression) {
 checkExpression( node.expression )

}

}
def checkTryStatement(node: TryStatement) = {
 checkGrammarStatementInAmbientContext( node )
checkBlock( node.tryBlock )
val catchClause = node.catchClause
if (catchClause) {
 if (catchClause.variableDeclaration) {
 if (catchClause.variableDeclaration.`type`) {
 grammarErrorOnFirstToken( catchClause.variableDeclaration.`type`, Diagnostics.Catch_clause_variable_cannot_have_a_type_annotation )

}
else if (catchClause.variableDeclaration.initializer) {
 grammarErrorOnFirstToken( catchClause.variableDeclaration.initializer, Diagnostics.Catch_clause_variable_cannot_have_an_initializer )

}
else {
 val blockLocals = catchClause.block.locals
if (blockLocals) {
 (catchClause.locals).keys.foreach { fresh125 =>
val caughtName = zeroOfMyType
 = fresh125
 {
 val blockLocal = blockLocals(caughtName)
if ((blockLocal&&(((blockLocal.flags&SymbolFlags.BlockScopedVariable))!==0))) {
 grammarErrorOnNode( blockLocal.valueDeclaration, Diagnostics.Cannot_redeclare_identifier_0_in_catch_clause, caughtName )

}

}
}

}

}

}
checkBlock( catchClause.block )

}
if (node.finallyBlock) {
 checkBlock( node.finallyBlock )

}

}
def checkIndexConstraints(`type`: Type) = {
 val declaredNumberIndexer = getIndexDeclarationOfSymbol( `type`.symbol, IndexKind.Number )
val declaredStringIndexer = getIndexDeclarationOfSymbol( `type`.symbol, IndexKind.String )
val stringIndexType = getIndexTypeOfType( `type`, IndexKind.String )
val numberIndexType = getIndexTypeOfType( `type`, IndexKind.Number )
if ((stringIndexType||numberIndexType)) {
 forEach( getPropertiesOfObjectType( `type` ), (prop =>  {
 val propType = getTypeOfSymbol( prop )
 checkIndexConstraintForProperty( prop, propType, `type`, declaredStringIndexer, stringIndexType, IndexKind.String )
 checkIndexConstraintForProperty( prop, propType, `type`, declaredNumberIndexer, numberIndexType, IndexKind.Number )

}) )
if (((getObjectFlags( `type` )&ObjectFlags.Class)&&isClassLike( `type`.symbol.valueDeclaration ))) {
 val classDeclaration = `type`.symbol.valueDeclaration.asInstanceOf[ClassLikeDeclaration]
(classDeclaration.members).foreach { fresh126 =>
val member = zeroOfMyType
 = fresh126
 {
 if (((!((getModifierFlags( member )&ModifierFlags.Static)))&&hasDynamicName( member ))) {
 val propType = getTypeOfSymbol( member.symbol )
checkIndexConstraintForProperty( member.symbol, propType, `type`, declaredStringIndexer, stringIndexType, IndexKind.String )
checkIndexConstraintForProperty( member.symbol, propType, `type`, declaredNumberIndexer, numberIndexType, IndexKind.Number )

}

}
}

}

}
var errorNode: Node = zeroOfMyType
if ((stringIndexType&&numberIndexType)) {
 (errorNode=(declaredNumberIndexer||declaredStringIndexer))
if (((!errorNode)&&((getObjectFlags( `type` )&ObjectFlags.Interface)))) {
 val someBaseTypeHasBothIndexers = forEach( getBaseTypes( `type`.asInstanceOf[InterfaceType] ), (base =>  (getIndexTypeOfType( base, IndexKind.String )&&getIndexTypeOfType( base, IndexKind.Number ))) )
(errorNode=(if (someBaseTypeHasBothIndexers) undefined else `type`.symbol.declarations(0)))

}

}
if ((errorNode&&(!isTypeAssignableTo( numberIndexType, stringIndexType )))) {
 error( errorNode, Diagnostics.Numeric_index_type_0_is_not_assignable_to_string_index_type_1, typeToString( numberIndexType ), typeToString( stringIndexType ) )

}
def checkIndexConstraintForProperty(prop: Symbol, propertyType: Type, containingType: Type, indexDeclaration: Declaration, indexType: Type, indexKind: IndexKind): Unit = {
 if ((!indexType)) {
 return

}
if (((indexKind===IndexKind.Number)&&(!isNumericName( prop.valueDeclaration.name )))) {
 return

}
var errorNode: Node = zeroOfMyType
if (((prop.valueDeclaration.name.kind===SyntaxKind.ComputedPropertyName)||(prop.parent===containingType.symbol))) {
 (errorNode=prop.valueDeclaration)

}
else if (indexDeclaration) {
 (errorNode=indexDeclaration)

}
else if ((getObjectFlags( containingType )&ObjectFlags.Interface)) {
 val someBaseClassHasBothPropertyAndIndexer = forEach( getBaseTypes( containingType.asInstanceOf[InterfaceType] ), (base =>  (getPropertyOfObjectType( base, prop.name )&&getIndexTypeOfType( base, indexKind ))) )
(errorNode=(if (someBaseClassHasBothPropertyAndIndexer) undefined else containingType.symbol.declarations(0)))

}
if ((errorNode&&(!isTypeAssignableTo( propertyType, indexType )))) {
 val errorMessage = (if ((indexKind===IndexKind.String)) Diagnostics.Property_0_of_type_1_is_not_assignable_to_string_index_type_2 else Diagnostics.Property_0_of_type_1_is_not_assignable_to_numeric_index_type_2)
error( errorNode, errorMessage, symbolToString( prop ), typeToString( propertyType ), typeToString( indexType ) )

}

}

}
def checkTypeNameIsReserved(name: DeclarationName, message: DiagnosticMessage): Unit = {
 (name.asInstanceOf[Identifier]).text match {
  case  "any" | "number" | "boolean" | "string" | "symbol" | "void"  =>
error( name, message, (name.asInstanceOf[Identifier]).text )
  case _ =>
}

}
def checkTypeParameters(typeParameterDeclarations: Array[TypeParameterDeclaration]) = {
 if (typeParameterDeclarations) {
 {
var i = 0
var n = typeParameterDeclarations.length
while( (i<n)) {
 {
 val node = typeParameterDeclarations(i)
checkTypeParameter( node )
if (produceDiagnostics) {
 {
var j = 0
while( (j<i)) {
 {
 if ((typeParameterDeclarations(j).symbol===node.symbol)) {
 error( node.name, Diagnostics.Duplicate_identifier_0, declarationNameToString( node.name ) )

}

}
 (j+= 1)
}
}

}

}
 (i+= 1)
}
}

}

}
def checkTypeParameterListsIdentical(node: ( ClassLikeDeclaration | InterfaceDeclaration ), symbol: Symbol) = {
 if ((symbol.declarations.length===1)) {
 return

}
var firstDecl: ( ClassLikeDeclaration | InterfaceDeclaration ) = zeroOfMyType
(symbol.declarations).foreach { fresh127 =>
val declaration = zeroOfMyType
 = fresh127
 {
 if (((declaration.kind===SyntaxKind.ClassDeclaration)||(declaration.kind===SyntaxKind.InterfaceDeclaration))) {
 if ((!firstDecl)) {
 (firstDecl=declaration.asInstanceOf[( ClassLikeDeclaration | InterfaceDeclaration )])

}
else if ((!areTypeParametersIdentical( firstDecl.typeParameters, node.typeParameters ))) {
 error( node.name, Diagnostics.All_declarations_of_0_must_have_identical_type_parameters, node.name.text )

}

}

}
}

}
def checkClassExpression(node: ClassExpression): Type = {
 checkClassLikeDeclaration( node )
checkNodeDeferred( node )
return getTypeOfSymbol( getSymbolOfNode( node ) )

}
def checkClassExpressionDeferred(node: ClassExpression) = {
 forEach( node.members, checkSourceElement )
registerForUnusedIdentifiersCheck( node )

}
def checkClassDeclaration(node: ClassDeclaration) = {
 if (((!node.name)&&(!((getModifierFlags( node )&ModifierFlags.Default))))) {
 grammarErrorOnFirstToken( node, Diagnostics.A_class_declaration_without_the_default_modifier_must_have_a_name )

}
checkClassLikeDeclaration( node )
forEach( node.members, checkSourceElement )
registerForUnusedIdentifiersCheck( node )

}
def checkClassLikeDeclaration(node: ClassLikeDeclaration) = {
 checkGrammarClassDeclarationHeritageClauses( node )
checkDecorators( node )
if (node.name) {
 checkTypeNameIsReserved( node.name, Diagnostics.Class_name_cannot_be_0 )
checkCollisionWithCapturedThisVariable( node, node.name )
checkCollisionWithRequireExportsInGeneratedCode( node, node.name )
checkCollisionWithGlobalPromiseInGeneratedCode( node, node.name )

}
checkTypeParameters( node.typeParameters )
checkExportsOnMergedDeclarations( node )
val symbol = getSymbolOfNode( node )
val `type` = getDeclaredTypeOfSymbol( symbol ).asInstanceOf[InterfaceType]
val typeWithThis = getTypeWithThisArgument( `type` )
val staticType = getTypeOfSymbol( symbol ).asInstanceOf[ObjectType]
checkTypeParameterListsIdentical( node, symbol )
checkClassForDuplicateDeclarations( node )
val baseTypeNode = getClassExtendsHeritageClauseElement( node )
if (baseTypeNode) {
 val baseTypes = getBaseTypes( `type` )
if ((baseTypes.length&&produceDiagnostics)) {
 val baseType = baseTypes(0)
val staticBaseType = getBaseConstructorTypeOfClass( `type` )
checkBaseTypeAccessibility( staticBaseType, baseTypeNode )
checkSourceElement( baseTypeNode.expression )
if (baseTypeNode.typeArguments) {
 forEach( baseTypeNode.typeArguments, checkSourceElement )
(getConstructorsForTypeArguments( staticBaseType, baseTypeNode.typeArguments )).foreach { fresh128 =>
val `constructor` = zeroOfMyType
 = fresh128
 {
 if ((!checkTypeArgumentConstraints( `constructor`.typeParameters, baseTypeNode.typeArguments ))) {
 break()

}

}
}

}
checkTypeAssignableTo( typeWithThis, getTypeWithThisArgument( baseType, `type`.thisType ), (node.name||node), Diagnostics.Class_0_incorrectly_extends_base_class_1 )
checkTypeAssignableTo( staticType, getTypeWithoutSignatures( staticBaseType ), (node.name||node), Diagnostics.Class_static_side_0_incorrectly_extends_base_class_static_side_1 )
if (((baseType.symbol.valueDeclaration&&(!isInAmbientContext( baseType.symbol.valueDeclaration )))&&(baseType.symbol.valueDeclaration.kind===SyntaxKind.ClassDeclaration))) {
 if ((!isBlockScopedNameDeclaredBeforeUse( baseType.symbol.valueDeclaration, node ))) {
 error( baseTypeNode, Diagnostics.A_class_must_be_declared_after_its_base_class )

}

}
if ((!((staticBaseType.symbol&&(staticBaseType.symbol.flags&SymbolFlags.Class))))) {
 val constructors = getInstantiatedConstructorsForTypeArguments( staticBaseType, baseTypeNode.typeArguments )
if (forEach( constructors, (sig =>  (getReturnTypeOfSignature( sig )!==baseType)) )) {
 error( baseTypeNode.expression, Diagnostics.Base_constructors_must_all_have_the_same_return_type )

}

}
checkKindsOfPropertyMemberOverrides( `type`, baseType )

}

}
val implementedTypeNodes = getClassImplementsHeritageClauseElements( node )
if (implementedTypeNodes) {
 (implementedTypeNodes).foreach { fresh129 =>
val typeRefNode = zeroOfMyType
 = fresh129
 {
 if ((!isEntityNameExpression( typeRefNode.expression ))) {
 error( typeRefNode.expression, Diagnostics.A_class_can_only_implement_an_identifier_Slashqualified_name_with_optional_type_arguments )

}
checkTypeReferenceNode( typeRefNode )
if (produceDiagnostics) {
 val t = getTypeFromTypeNode( typeRefNode )
if ((t!==unknownType)) {
 val declaredType = (if ((getObjectFlags( t )&ObjectFlags.Reference)) (t.asInstanceOf[TypeReference]).target else t)
if ((getObjectFlags( declaredType )&ObjectFlags.ClassOrInterface)) {
 checkTypeAssignableTo( typeWithThis, getTypeWithThisArgument( t, `type`.thisType ), (node.name||node), Diagnostics.Class_0_incorrectly_implements_interface_1 )

}
else {
 error( typeRefNode, Diagnostics.A_class_may_only_implement_another_class_or_interface )

}

}

}

}
}

}
if (produceDiagnostics) {
 checkIndexConstraints( `type` )
checkTypeForDuplicateIndexSignatures( node )

}

}
def checkBaseTypeAccessibility(`type`: Type, node: ExpressionWithTypeArguments) = {
 val signatures = getSignaturesOfType( `type`, SignatureKind.Construct )
if (signatures.length) {
 val declaration = signatures(0).declaration
if ((declaration&&(getModifierFlags( declaration )&ModifierFlags.Private))) {
 val typeClassDeclaration = getClassLikeDeclarationOfSymbol( `type`.symbol ).asInstanceOf[ClassLikeDeclaration]
if ((!isNodeWithinClass( node, typeClassDeclaration ))) {
 error( node, Diagnostics.Cannot_extend_a_class_0_Class_constructor_is_marked_as_private, getFullyQualifiedName( `type`.symbol ) )

}

}

}

}
def getTargetSymbol(s: Symbol) = {
 return (if ((s.flags&SymbolFlags.Instantiated)) getSymbolLinks( s ).target else s)

}
def getClassLikeDeclarationOfSymbol(symbol: Symbol): Declaration = {
 return forEach( symbol.declarations, (d =>  (if (isClassLike( d )) d else undefined)) )

}
def checkKindsOfPropertyMemberOverrides(`type`: InterfaceType, baseType: ObjectType): Unit = {
 val baseProperties = getPropertiesOfObjectType( baseType )
(baseProperties).foreach { fresh130 =>
val baseProperty = zeroOfMyType
 = fresh130
 {
 val base = getTargetSymbol( baseProperty )
if ((base.flags&SymbolFlags.Prototype)) {
 continue

}
val derived = getTargetSymbol( getPropertyOfObjectType( `type`, base.name ) )
val baseDeclarationFlags = getDeclarationModifierFlagsFromSymbol( base )
Debug.assert( (!(!derived)), "derived should point to something, even if it is the base class' declaration." )
if (derived) {
 if ((derived===base)) {
 val derivedClassDecl = getClassLikeDeclarationOfSymbol( `type`.symbol )
if (((baseDeclarationFlags&ModifierFlags.Abstract)&&(((!derivedClassDecl)||(!((getModifierFlags( derivedClassDecl )&ModifierFlags.Abstract))))))) {
 if ((derivedClassDecl.kind===SyntaxKind.ClassExpression)) {
 error( derivedClassDecl, Diagnostics.Non_abstract_class_expression_does_not_implement_inherited_abstract_member_0_from_class_1, symbolToString( baseProperty ), typeToString( baseType ) )

}
else {
 error( derivedClassDecl, Diagnostics.Non_abstract_class_0_does_not_implement_inherited_abstract_member_1_from_class_2, typeToString( `type` ), symbolToString( baseProperty ), typeToString( baseType ) )

}

}

}
else {
 val derivedDeclarationFlags = getDeclarationModifierFlagsFromSymbol( derived )
if ((((baseDeclarationFlags&ModifierFlags.Private))||((derivedDeclarationFlags&ModifierFlags.Private)))) {
 continue

}
if ((((baseDeclarationFlags&ModifierFlags.Static))!==((derivedDeclarationFlags&ModifierFlags.Static)))) {
 continue

}
if (((((base.flags&derived.flags)&SymbolFlags.Method))||((((base.flags&SymbolFlags.PropertyOrAccessor))&&((derived.flags&SymbolFlags.PropertyOrAccessor)))))) {
 continue

}
var errorMessage: DiagnosticMessage = zeroOfMyType
if ((base.flags&SymbolFlags.Method)) {
 if ((derived.flags&SymbolFlags.Accessor)) {
 (errorMessage=Diagnostics.Class_0_defines_instance_member_function_1_but_extended_class_2_defines_it_as_instance_member_accessor)

}
else {
 Debug.assert( (((derived.flags&SymbolFlags.Property))!==0) )
(errorMessage=Diagnostics.Class_0_defines_instance_member_function_1_but_extended_class_2_defines_it_as_instance_member_property)

}

}
else if ((base.flags&SymbolFlags.Property)) {
 Debug.assert( (((derived.flags&SymbolFlags.Method))!==0) )
(errorMessage=Diagnostics.Class_0_defines_instance_member_property_1_but_extended_class_2_defines_it_as_instance_member_function)

}
else {
 Debug.assert( (((base.flags&SymbolFlags.Accessor))!==0) )
Debug.assert( (((derived.flags&SymbolFlags.Method))!==0) )
(errorMessage=Diagnostics.Class_0_defines_instance_member_accessor_1_but_extended_class_2_defines_it_as_instance_member_function)

}
error( derived.valueDeclaration.name, errorMessage, typeToString( baseType ), symbolToString( base ), typeToString( `type` ) )

}

}

}
}

}
def isAccessor(kind: SyntaxKind): Boolean = {
 return ((kind===SyntaxKind.GetAccessor)||(kind===SyntaxKind.SetAccessor))

}
def areTypeParametersIdentical(list1: Array[TypeParameterDeclaration], list2: Array[TypeParameterDeclaration]) = {
 if (((!list1)&&(!list2))) {
 return true

}
if ((((!list1)||(!list2))||(list1.length!==list2.length))) {
 return false

}
{
var i = 0
var len = list1.length
while( (i<len)) {
 {
 val tp1 = list1(i)
val tp2 = list2(i)
if ((tp1.name.text!==tp2.name.text)) {
 return false

}
if (((!tp1.constraint)&&(!tp2.constraint))) {
 continue

}
if (((!tp1.constraint)||(!tp2.constraint))) {
 return false

}
if ((!isTypeIdenticalTo( getTypeFromTypeNode( tp1.constraint ), getTypeFromTypeNode( tp2.constraint ) ))) {
 return false

}

}
 (i+= 1)
}
}
return true

}
def checkInheritedPropertiesAreIdentical(`type`: InterfaceType, typeNode: Node): Boolean = {
 val baseTypes = getBaseTypes( `type` )
if ((baseTypes.length<2)) {
 return true

}
val seen = createMap[ {   var prop: Symbol
  var containingType: Type
 } ]()
forEach( resolveDeclaredMembers( `type` ).declaredProperties, (p =>  {
 (seen(p.name)=Map( "prop" -> p,
"containingType" -> `type` ))

}) )
var ok = true
(baseTypes).foreach { fresh131 =>
val base = zeroOfMyType
 = fresh131
 {
 val properties = getPropertiesOfObjectType( getTypeWithThisArgument( base, `type`.thisType ) )
(properties).foreach { fresh132 =>
val prop = zeroOfMyType
 = fresh132
 {
 val existing = seen(prop.name)
if ((!existing)) {
 (seen(prop.name)=Map( "prop" -> prop,
"containingType" -> base ))

}
else {
 val isInheritedProperty = (existing.containingType!==`type`)
if ((isInheritedProperty&&(!isPropertyIdenticalTo( existing.prop, prop )))) {
 (ok=false)
val typeName1 = typeToString( existing.containingType )
val typeName2 = typeToString( base )
var errorInfo = chainDiagnosticMessages( undefined, Diagnostics.Named_property_0_of_types_1_and_2_are_not_identical, symbolToString( prop ), typeName1, typeName2 )
(errorInfo=chainDiagnosticMessages( errorInfo, Diagnostics.Interface_0_cannot_simultaneously_extend_types_1_and_2, typeToString( `type` ), typeName1, typeName2 ))
diagnostics.add( createDiagnosticForNodeFromMessageChain( typeNode, errorInfo ) )

}

}

}
}

}
}
return ok

}
def checkInterfaceDeclaration(node: InterfaceDeclaration) = {
 ((checkGrammarDecorators( node )||checkGrammarModifiers( node ))||checkGrammarInterfaceDeclaration( node ))
checkTypeParameters( node.typeParameters )
if (produceDiagnostics) {
 checkTypeNameIsReserved( node.name, Diagnostics.Interface_name_cannot_be_0 )
checkExportsOnMergedDeclarations( node )
val symbol = getSymbolOfNode( node )
checkTypeParameterListsIdentical( node, symbol )
val firstInterfaceDecl = getDeclarationOfKind( symbol, SyntaxKind.InterfaceDeclaration ).asInstanceOf[InterfaceDeclaration]
if ((node===firstInterfaceDecl)) {
 val `type` = getDeclaredTypeOfSymbol( symbol ).asInstanceOf[InterfaceType]
val typeWithThis = getTypeWithThisArgument( `type` )
if (checkInheritedPropertiesAreIdentical( `type`, node.name )) {
 (getBaseTypes( `type` )).foreach { fresh133 =>
val baseType = zeroOfMyType
 = fresh133
 {
 checkTypeAssignableTo( typeWithThis, getTypeWithThisArgument( baseType, `type`.thisType ), node.name, Diagnostics.Interface_0_incorrectly_extends_interface_1 )

}
}
checkIndexConstraints( `type` )

}

}
checkObjectTypeForDuplicateDeclarations( node )

}
forEach( getInterfaceBaseTypeNodes( node ), (heritageElement =>  {
 if ((!isEntityNameExpression( heritageElement.expression ))) {
 error( heritageElement.expression, Diagnostics.An_interface_can_only_extend_an_identifier_Slashqualified_name_with_optional_type_arguments )

}
 checkTypeReferenceNode( heritageElement )

}) )
forEach( node.members, checkSourceElement )
if (produceDiagnostics) {
 checkTypeForDuplicateIndexSignatures( node )
registerForUnusedIdentifiersCheck( node )

}

}
def checkTypeAliasDeclaration(node: TypeAliasDeclaration) = {
 (checkGrammarDecorators( node )||checkGrammarModifiers( node ))
checkTypeNameIsReserved( node.name, Diagnostics.Type_alias_name_cannot_be_0 )
checkTypeParameters( node.typeParameters )
checkSourceElement( node.`type` )

}
def computeEnumMemberValues(node: EnumDeclaration) = {
 val nodeLinks = getNodeLinks( node )
if ((!((nodeLinks.flags&NodeCheckFlags.EnumValuesComputed)))) {
 val enumSymbol = getSymbolOfNode( node )
val enumType = getDeclaredTypeOfSymbol( enumSymbol )
var autoValue = 0
val ambient = isInAmbientContext( node )
val enumIsConst = isConst( node )
(node.members).foreach { fresh134 =>
val member = zeroOfMyType
 = fresh134
 {
 if (isComputedNonLiteralName( member.name.asInstanceOf[PropertyName] )) {
 error( member.name, Diagnostics.Computed_property_names_are_not_allowed_in_enums )

}
else {
 val text = getTextOfPropertyName( member.name.asInstanceOf[PropertyName] )
if ((isNumericLiteralName( text )&&(!isInfinityOrNaNString( text )))) {
 error( member.name, Diagnostics.An_enum_member_cannot_have_a_numeric_name )

}

}
val previousEnumMemberIsNonConstant = (autoValue===undefined)
val initializer = member.initializer
if (initializer) {
 (autoValue=computeConstantValueForEnumMemberInitializer( initializer, enumType, enumIsConst, ambient ))

}
else if ((ambient&&(!enumIsConst))) {
 (autoValue=undefined)

}
else if (previousEnumMemberIsNonConstant) {
 error( member.name, Diagnostics.Enum_member_must_have_initializer )

}
if ((autoValue!==undefined)) {
 (getNodeLinks( member ).enumMemberValue=autoValue)
(autoValue+= 1)

}

}
}
(nodeLinks.flags|=NodeCheckFlags.EnumValuesComputed)

}
def computeConstantValueForEnumMemberInitializer(initializer: Expression, enumType: Type, enumIsConst: Boolean, ambient: Boolean): Int = {
 var reportError = true
val value = evalConstant( initializer )
if (reportError) {
 if ((value===undefined)) {
 if (enumIsConst) {
 error( initializer, Diagnostics.In_const_enum_declarations_member_initializer_must_be_constant_expression )

}
else if (ambient) {
 error( initializer, Diagnostics.In_ambient_enum_declarations_member_initializer_must_be_constant_expression )

}
else {
 checkTypeAssignableTo( checkExpression( initializer ), enumType, initializer, undefined )

}

}
else if (enumIsConst) {
 if (isNaN( value )) {
 error( initializer, Diagnostics.const_enum_member_initializer_was_evaluated_to_disallowed_value_NaN )

}
else if ((!isFinite( value ))) {
 error( initializer, Diagnostics.const_enum_member_initializer_was_evaluated_to_a_non_finite_value )

}

}

}
return value
def evalConstant(e: Node): Int = {
 e.kind match {
  case  SyntaxKind.PrefixUnaryExpression  =>
val value = evalConstant( (e.asInstanceOf[PrefixUnaryExpression]).operand )
if ((value===undefined)) {
 return undefined

}
(e.asInstanceOf[PrefixUnaryExpression]).operator match {
  case  SyntaxKind.PlusToken  =>
return value
  case  SyntaxKind.MinusToken  =>
return (-value)
  case  SyntaxKind.TildeToken  =>
return (~value)
  case _ =>
}
return undefined
  case  SyntaxKind.BinaryExpression  =>
val left = evalConstant( (e.asInstanceOf[BinaryExpression]).left )
if ((left===undefined)) {
 return undefined

}
val right = evalConstant( (e.asInstanceOf[BinaryExpression]).right )
if ((right===undefined)) {
 return undefined

}
(e.asInstanceOf[BinaryExpression]).operatorToken.kind match {
  case  SyntaxKind.BarToken  =>
return (left|right)
  case  SyntaxKind.AmpersandToken  =>
return (left&right)
  case  SyntaxKind.GreaterThanGreaterThanToken  =>
return (left>>right)
  case  SyntaxKind.GreaterThanGreaterThanGreaterThanToken  =>
return (left>>>right)
  case  SyntaxKind.LessThanLessThanToken  =>
return (left<<right)
  case  SyntaxKind.CaretToken  =>
return (left^right)
  case  SyntaxKind.AsteriskToken  =>
return (left*right)
  case  SyntaxKind.SlashToken  =>
return (left/right)
  case  SyntaxKind.PlusToken  =>
return (left+right)
  case  SyntaxKind.MinusToken  =>
return (left-right)
  case  SyntaxKind.PercentToken  =>
return (left%right)
  case _ =>
}
return undefined
  case  SyntaxKind.NumericLiteral  =>
return (+(e.asInstanceOf[NumericLiteral]).text)
  case  SyntaxKind.ParenthesizedExpression  =>
return evalConstant( (e.asInstanceOf[ParenthesizedExpression]).expression )
  case  SyntaxKind.Identifier | SyntaxKind.ElementAccessExpression | SyntaxKind.PropertyAccessExpression  =>
val member = initializer.parent
val currentType = getTypeOfSymbol( getSymbolOfNode( member.parent ) )
var enumType: Type = zeroOfMyType
var propertyName: String = zeroOfMyType
if ((e.kind===SyntaxKind.Identifier)) {
 (enumType=currentType)
(propertyName=(e.asInstanceOf[Identifier]).text)

}
else {
 var expression: Expression = zeroOfMyType
if ((e.kind===SyntaxKind.ElementAccessExpression)) {
 if ((((e.asInstanceOf[ElementAccessExpression]).argumentExpression===undefined)||((e.asInstanceOf[ElementAccessExpression]).argumentExpression.kind!==SyntaxKind.StringLiteral))) {
 return undefined

}
(expression=(e.asInstanceOf[ElementAccessExpression]).expression)
(propertyName=((e.asInstanceOf[ElementAccessExpression]).argumentExpression.asInstanceOf[LiteralExpression]).text)

}
else {
 (expression=(e.asInstanceOf[PropertyAccessExpression]).expression)
(propertyName=(e.asInstanceOf[PropertyAccessExpression]).name.text)

}
var current = expression
while (current) {
{
 if ((current.kind===SyntaxKind.Identifier)) {
 break()

}
else if ((current.kind===SyntaxKind.PropertyAccessExpression)) {
 (current=(current.asInstanceOf[ElementAccessExpression]).expression)

}
else {
 return undefined

}

}
}
(enumType=checkExpression( expression ))
if ((!((enumType.symbol&&((enumType.symbol.flags&SymbolFlags.Enum)))))) {
 return undefined

}

}
if ((propertyName===undefined)) {
 return undefined

}
val property = getPropertyOfObjectType( enumType, propertyName )
if (((!property)||(!((property.flags&SymbolFlags.EnumMember))))) {
 return undefined

}
val propertyDecl = property.valueDeclaration
if ((member===propertyDecl)) {
 return undefined

}
if ((!isBlockScopedNameDeclaredBeforeUse( propertyDecl, member ))) {
 (reportError=false)
error( e, Diagnostics.A_member_initializer_in_a_enum_declaration_cannot_reference_members_declared_after_it_including_members_defined_in_other_enums )
return undefined

}
return getNodeLinks( propertyDecl ).enumMemberValue.asInstanceOf[Int]
  case _ =>
}

}

}

}
def checkEnumDeclaration(node: EnumDeclaration) = {
 if ((!produceDiagnostics)) {
 return

}
(checkGrammarDecorators( node )||checkGrammarModifiers( node ))
checkTypeNameIsReserved( node.name, Diagnostics.Enum_name_cannot_be_0 )
checkCollisionWithCapturedThisVariable( node, node.name )
checkCollisionWithRequireExportsInGeneratedCode( node, node.name )
checkCollisionWithGlobalPromiseInGeneratedCode( node, node.name )
checkExportsOnMergedDeclarations( node )
computeEnumMemberValues( node )
val enumIsConst = isConst( node )
if (((compilerOptions.isolatedModules&&enumIsConst)&&isInAmbientContext( node ))) {
 error( node.name, Diagnostics.Ambient_const_enums_are_not_allowed_when_the_isolatedModules_flag_is_provided )

}
val enumSymbol = getSymbolOfNode( node )
val firstDeclaration = getDeclarationOfKind( enumSymbol, node.kind )
if ((node===firstDeclaration)) {
 if ((enumSymbol.declarations.length>1)) {
 forEach( enumSymbol.declarations, (decl =>  {
 if ((isConstEnumDeclaration( decl )!==enumIsConst)) {
 error( decl.name, Diagnostics.Enum_declarations_must_all_be_const_or_non_const )

}

}) )

}
var seenEnumMissingInitialInitializer = false
forEach( enumSymbol.declarations, (declaration =>  {
 if ((declaration.kind!==SyntaxKind.EnumDeclaration)) {
 return false

}
 val enumDeclaration = declaration.asInstanceOf[EnumDeclaration]
 if ((!enumDeclaration.members.length)) {
 return false

}
 val firstEnumMember = enumDeclaration.members(0)
 if ((!firstEnumMember.initializer)) {
 if (seenEnumMissingInitialInitializer) {
 error( firstEnumMember.name, Diagnostics.In_an_enum_with_multiple_declarations_only_one_declaration_can_omit_an_initializer_for_its_first_enum_element )

}
else {
 (seenEnumMissingInitialInitializer=true)

}

}

}) )

}

}
def getFirstNonAmbientClassOrFunctionDeclaration(symbol: Symbol): Declaration = {
 val declarations = symbol.declarations
(declarations).foreach { fresh135 =>
val declaration = zeroOfMyType
 = fresh135
 {
 if (((((declaration.kind===SyntaxKind.ClassDeclaration)||(((declaration.kind===SyntaxKind.FunctionDeclaration)&&nodeIsPresent( (declaration.asInstanceOf[FunctionLikeDeclaration]).body )))))&&(!isInAmbientContext( declaration )))) {
 return declaration

}

}
}
return undefined

}
def inSameLexicalScope(node1: Node, node2: Node) = {
 val container1 = getEnclosingBlockScopeContainer( node1 )
val container2 = getEnclosingBlockScopeContainer( node2 )
if (isGlobalSourceFile( container1 )) {
 return isGlobalSourceFile( container2 )

}
else if (isGlobalSourceFile( container2 )) {
 return false

}
else {
 return (container1===container2)

}

}
def checkModuleDeclaration(node: ModuleDeclaration) = {
 if (produceDiagnostics) {
 val isGlobalAugmentation = isGlobalScopeAugmentation( node )
val inAmbientContext = isInAmbientContext( node )
if ((isGlobalAugmentation&&(!inAmbientContext))) {
 error( node.name, Diagnostics.Augmentations_for_the_global_scope_should_have_declare_modifier_unless_they_appear_in_already_ambient_context )

}
val isAmbientExternalModule = isAmbientModule( node )
val contextErrorMessage = (if (isAmbientExternalModule) Diagnostics.An_ambient_module_declaration_is_only_allowed_at_the_top_level_in_a_file else Diagnostics.A_namespace_declaration_is_only_allowed_in_a_namespace_or_module)
if (checkGrammarModuleElementContext( node, contextErrorMessage )) {
 return

}
if (((!checkGrammarDecorators( node ))&&(!checkGrammarModifiers( node )))) {
 if (((!inAmbientContext)&&(node.name.kind===SyntaxKind.StringLiteral))) {
 grammarErrorOnNode( node.name, Diagnostics.Only_ambient_modules_can_use_quoted_names )

}

}
if (isIdentifier( node.name )) {
 checkCollisionWithCapturedThisVariable( node, node.name )
checkCollisionWithRequireExportsInGeneratedCode( node, node.name )
checkCollisionWithGlobalPromiseInGeneratedCode( node, node.name )

}
checkExportsOnMergedDeclarations( node )
val symbol = getSymbolOfNode( node )
if (((((symbol.flags&SymbolFlags.ValueModule)&&(symbol.declarations.length>1))&&(!inAmbientContext))&&isInstantiatedModule( node, (compilerOptions.preserveConstEnums||compilerOptions.isolatedModules) ))) {
 val firstNonAmbientClassOrFunc = getFirstNonAmbientClassOrFunctionDeclaration( symbol )
if (firstNonAmbientClassOrFunc) {
 if ((getSourceFileOfNode( node )!==getSourceFileOfNode( firstNonAmbientClassOrFunc ))) {
 error( node.name, Diagnostics.A_namespace_declaration_cannot_be_in_a_different_file_from_a_class_or_function_with_which_it_is_merged )

}
else if ((node.pos<firstNonAmbientClassOrFunc.pos)) {
 error( node.name, Diagnostics.A_namespace_declaration_cannot_be_located_prior_to_a_class_or_function_with_which_it_is_merged )

}

}
val mergedClass = getDeclarationOfKind( symbol, SyntaxKind.ClassDeclaration )
if ((mergedClass&&inSameLexicalScope( node, mergedClass ))) {
 (getNodeLinks( node ).flags|=NodeCheckFlags.LexicalModuleMergesWithClass)

}

}
if (isAmbientExternalModule) {
 if (isExternalModuleAugmentation( node )) {
 val checkBody = (isGlobalAugmentation||((getSymbolOfNode( node ).flags&SymbolFlags.Merged)))
if ((checkBody&&node.body)) {
 ((node.body.asInstanceOf[ModuleBlock]).statements).foreach { fresh136 =>
val statement = zeroOfMyType
 = fresh136
 {
 checkModuleAugmentationElement( statement, isGlobalAugmentation )

}
}

}

}
else if (isGlobalSourceFile( node.parent )) {
 if (isGlobalAugmentation) {
 error( node.name, Diagnostics.Augmentations_for_the_global_scope_can_only_be_directly_nested_in_external_modules_or_ambient_module_declarations )

}
else if (isExternalModuleNameRelative( node.name.text )) {
 error( node.name, Diagnostics.Ambient_module_declaration_cannot_specify_relative_module_name )

}

}
else {
 if (isGlobalAugmentation) {
 error( node.name, Diagnostics.Augmentations_for_the_global_scope_can_only_be_directly_nested_in_external_modules_or_ambient_module_declarations )

}
else {
 error( node.name, Diagnostics.Ambient_modules_cannot_be_nested_in_other_modules_or_namespaces )

}

}

}

}
if (node.body) {
 checkSourceElement( node.body )
if ((!isGlobalScopeAugmentation( node ))) {
 registerForUnusedIdentifiersCheck( node )

}

}

}
def checkModuleAugmentationElement(node: Node, isGlobalAugmentation: Boolean): Unit = {
 node.kind match {
  case  SyntaxKind.VariableStatement  =>
((node.asInstanceOf[VariableStatement]).declarationList.declarations).foreach { fresh137 =>
val decl = zeroOfMyType
 = fresh137
 {
 checkModuleAugmentationElement( decl, isGlobalAugmentation )

}
}
  case  SyntaxKind.ExportAssignment | SyntaxKind.ExportDeclaration  =>
grammarErrorOnFirstToken( node, Diagnostics.Exports_and_export_assignments_are_not_permitted_in_module_augmentations )
  case  SyntaxKind.ImportEqualsDeclaration | SyntaxKind.ImportDeclaration  =>
grammarErrorOnFirstToken( node, Diagnostics.Imports_are_not_permitted_in_module_augmentations_Consider_moving_them_to_the_enclosing_external_module )
  case  SyntaxKind.BindingElement | SyntaxKind.VariableDeclaration  =>
val name = (node.asInstanceOf[( VariableDeclaration | BindingElement )]).name
if (isBindingPattern( name )) {
 (name.elements).foreach { fresh138 =>
val el = zeroOfMyType
 = fresh138
 {
 checkModuleAugmentationElement( el, isGlobalAugmentation )

}
}
break()

}
  case  SyntaxKind.ClassDeclaration | SyntaxKind.EnumDeclaration | SyntaxKind.FunctionDeclaration | SyntaxKind.InterfaceDeclaration | SyntaxKind.ModuleDeclaration | SyntaxKind.TypeAliasDeclaration  =>
if (isGlobalAugmentation) {
 return

}
val symbol = getSymbolOfNode( node )
if (symbol) {
 var reportError = (!((symbol.flags&SymbolFlags.Merged)))
if ((!reportError)) {
 (reportError=isExternalModuleAugmentation( symbol.parent.declarations(0) ))

}

}
  case _ =>
}

}
def getFirstIdentifier(node: EntityNameOrEntityNameExpression): Identifier = {
 node.kind match {
  case  SyntaxKind.Identifier  =>
return node.asInstanceOf[Identifier]
  case  SyntaxKind.QualifiedName  =>
do {
{
 (node=(node.asInstanceOf[QualifiedName]).left)

}
} while ((node.kind!==SyntaxKind.Identifier))
return node.asInstanceOf[Identifier]
  case  SyntaxKind.PropertyAccessExpression  =>
do {
{
 (node=(node.asInstanceOf[PropertyAccessEntityNameExpression]).expression)

}
} while ((node.kind!==SyntaxKind.Identifier))
return node.asInstanceOf[Identifier]
  case _ =>
}

}
def checkExternalImportOrExportDeclaration(node: ( ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration )): Boolean = {
 val moduleName = getExternalModuleName( node )
if (((!nodeIsMissing( moduleName ))&&(moduleName.kind!==SyntaxKind.StringLiteral))) {
 error( moduleName, Diagnostics.String_literal_expected )
return false

}
val inAmbientExternalModule = ((node.parent.kind===SyntaxKind.ModuleBlock)&&isAmbientModule( node.parent.parent.asInstanceOf[ModuleDeclaration] ))
if (((node.parent.kind!==SyntaxKind.SourceFile)&&(!inAmbientExternalModule))) {
 error( moduleName, (if ((node.kind===SyntaxKind.ExportDeclaration)) Diagnostics.Export_declarations_are_not_permitted_in_a_namespace else Diagnostics.Import_declarations_in_a_namespace_cannot_reference_a_module) )
return false

}
if ((inAmbientExternalModule&&isExternalModuleNameRelative( (moduleName.asInstanceOf[LiteralExpression]).text ))) {
 if ((!isTopLevelInExternalModuleAugmentation( node ))) {
 error( node, Diagnostics.Import_or_export_declaration_in_an_ambient_module_declaration_cannot_reference_module_through_relative_module_name )
return false

}

}
return true

}
def checkAliasSymbol(node: ( ImportEqualsDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier )) = {
 val symbol = getSymbolOfNode( node )
val target = resolveAlias( symbol )
if ((target!==unknownSymbol)) {
 val excludedMeanings = ((((if ((symbol.flags&((SymbolFlags.Value|SymbolFlags.ExportValue)))) SymbolFlags.Value else 0))|((if ((symbol.flags&SymbolFlags.Type)) SymbolFlags.Type else 0)))|((if ((symbol.flags&SymbolFlags.Namespace)) SymbolFlags.Namespace else 0)))
if ((target.flags&excludedMeanings)) {
 val message = (if ((node.kind===SyntaxKind.ExportSpecifier)) Diagnostics.Export_declaration_conflicts_with_exported_declaration_of_0 else Diagnostics.Import_declaration_conflicts_with_local_declaration_of_0)
error( node, message, symbolToString( symbol ) )

}

}

}
def checkImportBinding(node: ( ImportEqualsDeclaration | ImportClause | NamespaceImport | ImportSpecifier )) = {
 checkCollisionWithCapturedThisVariable( node, node.name )
checkCollisionWithRequireExportsInGeneratedCode( node, node.name )
checkCollisionWithGlobalPromiseInGeneratedCode( node, node.name )
checkAliasSymbol( node )

}
def checkImportDeclaration(node: ImportDeclaration) = {
 if (checkGrammarModuleElementContext( node, Diagnostics.An_import_declaration_can_only_be_used_in_a_namespace_or_module )) {
 return

}
if ((((!checkGrammarDecorators( node ))&&(!checkGrammarModifiers( node )))&&(getModifierFlags( node )!==0))) {
 grammarErrorOnFirstToken( node, Diagnostics.An_import_declaration_cannot_have_modifiers )

}
if (checkExternalImportOrExportDeclaration( node )) {
 val importClause = node.importClause
if (importClause) {
 if (importClause.name) {
 checkImportBinding( importClause )

}
if (importClause.namedBindings) {
 if ((importClause.namedBindings.kind===SyntaxKind.NamespaceImport)) {
 checkImportBinding( importClause.namedBindings.asInstanceOf[NamespaceImport] )

}
else {
 forEach( (importClause.namedBindings.asInstanceOf[NamedImports]).elements, checkImportBinding )

}

}

}

}

}
def checkImportEqualsDeclaration(node: ImportEqualsDeclaration) = {
 if (checkGrammarModuleElementContext( node, Diagnostics.An_import_declaration_can_only_be_used_in_a_namespace_or_module )) {
 return

}
(checkGrammarDecorators( node )||checkGrammarModifiers( node ))
if ((isInternalModuleImportEqualsDeclaration( node )||checkExternalImportOrExportDeclaration( node ))) {
 checkImportBinding( node )
if ((getModifierFlags( node )&ModifierFlags.Export)) {
 markExportAsReferenced( node )

}
if (isInternalModuleImportEqualsDeclaration( node )) {
 val target = resolveAlias( getSymbolOfNode( node ) )
if ((target!==unknownSymbol)) {
 if ((target.flags&SymbolFlags.Value)) {
 val moduleName = getFirstIdentifier( node.moduleReference.asInstanceOf[EntityName] )
if ((!((resolveEntityName( moduleName, (SymbolFlags.Value|SymbolFlags.Namespace) ).flags&SymbolFlags.Namespace)))) {
 error( moduleName, Diagnostics.Module_0_is_hidden_by_a_local_declaration_with_the_same_name, declarationNameToString( moduleName ) )

}

}
if ((target.flags&SymbolFlags.Type)) {
 checkTypeNameIsReserved( node.name, Diagnostics.Import_name_cannot_be_0 )

}

}

}
else {
 if (((modulekind===ModuleKind.ES2015)&&(!isInAmbientContext( node )))) {
 grammarErrorOnNode( node, Diagnostics.Import_assignment_cannot_be_used_when_targeting_ECMAScript_2015_modules_Consider_using_import_Asterisk_as_ns_from_mod_import_a_from_mod_import_d_from_mod_or_another_module_format_instead )

}

}

}

}
def checkExportDeclaration(node: ExportDeclaration) = {
 if (checkGrammarModuleElementContext( node, Diagnostics.An_export_declaration_can_only_be_used_in_a_module )) {
 return

}
if ((((!checkGrammarDecorators( node ))&&(!checkGrammarModifiers( node )))&&(getModifierFlags( node )!==0))) {
 grammarErrorOnFirstToken( node, Diagnostics.An_export_declaration_cannot_have_modifiers )

}
if (((!node.moduleSpecifier)||checkExternalImportOrExportDeclaration( node ))) {
 if (node.exportClause) {
 forEach( node.exportClause.elements, checkExportSpecifier )
val inAmbientExternalModule = ((node.parent.kind===SyntaxKind.ModuleBlock)&&isAmbientModule( node.parent.parent ))
if (((node.parent.kind!==SyntaxKind.SourceFile)&&(!inAmbientExternalModule))) {
 error( node, Diagnostics.Export_declarations_are_not_permitted_in_a_namespace )

}

}
else {
 val moduleSymbol = resolveExternalModuleName( node, node.moduleSpecifier )
if ((moduleSymbol&&hasExportAssignmentSymbol( moduleSymbol ))) {
 error( node.moduleSpecifier, Diagnostics.Module_0_uses_export_and_cannot_be_used_with_export_Asterisk, symbolToString( moduleSymbol ) )

}

}

}

}
def checkGrammarModuleElementContext(node: Statement, errorMessage: DiagnosticMessage): Boolean = {
 val isInAppropriateContext = (((node.parent.kind===SyntaxKind.SourceFile)||(node.parent.kind===SyntaxKind.ModuleBlock))||(node.parent.kind===SyntaxKind.ModuleDeclaration))
if ((!isInAppropriateContext)) {
 grammarErrorOnFirstToken( node, errorMessage )

}
return (!isInAppropriateContext)

}
def checkExportSpecifier(node: ExportSpecifier) = {
 checkAliasSymbol( node )
if ((!(node.parent.parent.asInstanceOf[ExportDeclaration]).moduleSpecifier)) {
 val exportedName = (node.propertyName||node.name)
val symbol = resolveName( exportedName, exportedName.text, (((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace)|SymbolFlags.Alias), undefined, undefined )
if ((symbol&&(((symbol===undefinedSymbol)||isGlobalSourceFile( getDeclarationContainer( symbol.declarations(0) ) ))))) {
 error( exportedName, Diagnostics.Cannot_export_0_Only_local_declarations_can_be_exported_from_a_module, exportedName.text )

}
else {
 markExportAsReferenced( node )

}

}

}
def checkExportAssignment(node: ExportAssignment) = {
 if (checkGrammarModuleElementContext( node, Diagnostics.An_export_assignment_can_only_be_used_in_a_module )) {
 return

}
val container = (if ((node.parent.kind===SyntaxKind.SourceFile)) node.parent.asInstanceOf[SourceFile] else node.parent.parent.asInstanceOf[ModuleDeclaration])
if (((container.kind===SyntaxKind.ModuleDeclaration)&&(!isAmbientModule( container )))) {
 error( node, Diagnostics.An_export_assignment_cannot_be_used_in_a_namespace )
return

}
if ((((!checkGrammarDecorators( node ))&&(!checkGrammarModifiers( node )))&&(getModifierFlags( node )!==0))) {
 grammarErrorOnFirstToken( node, Diagnostics.An_export_assignment_cannot_have_modifiers )

}
if ((node.expression.kind===SyntaxKind.Identifier)) {
 markExportAsReferenced( node )

}
else {
 checkExpressionCached( node.expression )

}
checkExternalModuleExports( container )
if ((node.isExportEquals&&(!isInAmbientContext( node )))) {
 if ((modulekind===ModuleKind.ES2015)) {
 grammarErrorOnNode( node, Diagnostics.Export_assignment_cannot_be_used_when_targeting_ECMAScript_2015_modules_Consider_using_export_default_or_another_module_format_instead )

}
else if ((modulekind===ModuleKind.System)) {
 grammarErrorOnNode( node, Diagnostics.Export_assignment_is_not_supported_when_module_flag_is_system )

}

}

}
def hasExportedMembers(moduleSymbol: Symbol) = {
 (moduleSymbol.exports).keys.foreach { fresh139 =>
val id = zeroOfMyType
 = fresh139
 {
 if ((id!=="export=")) {
 return true

}

}
}
return false

}
def checkExternalModuleExports(node: ( SourceFile | ModuleDeclaration )) = {
 val moduleSymbol = getSymbolOfNode( node )
val links = getSymbolLinks( moduleSymbol )
if ((!links.exportsChecked)) {
 val exportEqualsSymbol = moduleSymbol.exports("export=")
if ((exportEqualsSymbol&&hasExportedMembers( moduleSymbol ))) {
 val declaration = (getDeclarationOfAliasSymbol( exportEqualsSymbol )||exportEqualsSymbol.valueDeclaration)
if ((!isTopLevelInExternalModuleAugmentation( declaration ))) {
 error( declaration, Diagnostics.An_export_assignment_cannot_be_used_in_a_module_with_other_exported_elements )

}

}
val exports = getExportsOfModule( moduleSymbol )
(exports).keys.foreach { fresh140 =>
val id = zeroOfMyType
 = fresh140
 {
 if ((id==="__export")) {
 continue

}
const fresh141 = exports(id)
val declarations = fresh141.declarations
val flags = fresh141.flags
if ((flags&(((SymbolFlags.Namespace|SymbolFlags.Interface)|SymbolFlags.Enum)))) {
 continue

}
val exportedDeclarationsCount = countWhere( declarations, isNotOverload )
if (((flags&SymbolFlags.TypeAlias)&&(exportedDeclarationsCount<=2))) {
 continue

}
if ((exportedDeclarationsCount>1)) {
 (declarations).foreach { fresh142 =>
val declaration = zeroOfMyType
 = fresh142
 {
 if (isNotOverload( declaration )) {
 diagnostics.add( createDiagnosticForNode( declaration, Diagnostics.Cannot_redeclare_exported_variable_0, id ) )

}

}
}

}

}
}
(links.exportsChecked=true)

}
def isNotOverload(declaration: Declaration): Boolean = {
 return ((((declaration.kind!==SyntaxKind.FunctionDeclaration)&&(declaration.kind!==SyntaxKind.MethodDeclaration)))||(!(!(declaration.asInstanceOf[FunctionDeclaration]).body)))

}

}
def checkSourceElement(node: Node): Unit = {
 if ((!node)) {
 return

}
val kind = node.kind
if (cancellationToken) {
 kind match {
  case  SyntaxKind.ModuleDeclaration | SyntaxKind.ClassDeclaration | SyntaxKind.InterfaceDeclaration | SyntaxKind.FunctionDeclaration  =>
cancellationToken.throwIfCancellationRequested()
  case _ =>
}

}
kind match {
  case  SyntaxKind.TypeParameter  =>
return checkTypeParameter( node.asInstanceOf[TypeParameterDeclaration] )
  case  SyntaxKind.Parameter  =>
return checkParameter( node.asInstanceOf[ParameterDeclaration] )
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature  =>
return checkPropertyDeclaration( node.asInstanceOf[PropertyDeclaration] )
  case  SyntaxKind.FunctionType | SyntaxKind.ConstructorType | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature  =>
return checkSignatureDeclaration( node.asInstanceOf[SignatureDeclaration] )
  case  SyntaxKind.IndexSignature  =>
return checkSignatureDeclaration( node.asInstanceOf[SignatureDeclaration] )
  case  SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature  =>
return checkMethodDeclaration( node.asInstanceOf[MethodDeclaration] )
  case  SyntaxKind.Constructor  =>
return checkConstructorDeclaration( node.asInstanceOf[ConstructorDeclaration] )
  case  SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
return checkAccessorDeclaration( node.asInstanceOf[AccessorDeclaration] )
  case  SyntaxKind.TypeReference  =>
return checkTypeReferenceNode( node.asInstanceOf[TypeReferenceNode] )
  case  SyntaxKind.TypePredicate  =>
return checkTypePredicate( node.asInstanceOf[TypePredicateNode] )
  case  SyntaxKind.TypeQuery  =>
return checkTypeQuery( node.asInstanceOf[TypeQueryNode] )
  case  SyntaxKind.TypeLiteral  =>
return checkTypeLiteral( node.asInstanceOf[TypeLiteralNode] )
  case  SyntaxKind.ArrayType  =>
return checkArrayType( node.asInstanceOf[ArrayTypeNode] )
  case  SyntaxKind.TupleType  =>
return checkTupleType( node.asInstanceOf[TupleTypeNode] )
  case  SyntaxKind.UnionType | SyntaxKind.IntersectionType  =>
return checkUnionOrIntersectionType( node.asInstanceOf[UnionOrIntersectionTypeNode] )
  case  SyntaxKind.ParenthesizedType  =>
return checkSourceElement( (node.asInstanceOf[ParenthesizedTypeNode]).`type` )
  case  SyntaxKind.FunctionDeclaration  =>
return checkFunctionDeclaration( node.asInstanceOf[FunctionDeclaration] )
  case  SyntaxKind.Block | SyntaxKind.ModuleBlock  =>
return checkBlock( node.asInstanceOf[Block] )
  case  SyntaxKind.VariableStatement  =>
return checkVariableStatement( node.asInstanceOf[VariableStatement] )
  case  SyntaxKind.ExpressionStatement  =>
return checkExpressionStatement( node.asInstanceOf[ExpressionStatement] )
  case  SyntaxKind.IfStatement  =>
return checkIfStatement( node.asInstanceOf[IfStatement] )
  case  SyntaxKind.DoStatement  =>
return checkDoStatement( node.asInstanceOf[DoStatement] )
  case  SyntaxKind.WhileStatement  =>
return checkWhileStatement( node.asInstanceOf[WhileStatement] )
  case  SyntaxKind.ForStatement  =>
return checkForStatement( node.asInstanceOf[ForStatement] )
  case  SyntaxKind.ForInStatement  =>
return checkForInStatement( node.asInstanceOf[ForInStatement] )
  case  SyntaxKind.ForOfStatement  =>
return checkForOfStatement( node.asInstanceOf[ForOfStatement] )
  case  SyntaxKind.ContinueStatement | SyntaxKind.BreakStatement  =>
return checkBreakOrContinueStatement( node.asInstanceOf[BreakOrContinueStatement] )
  case  SyntaxKind.ReturnStatement  =>
return checkReturnStatement( node.asInstanceOf[ReturnStatement] )
  case  SyntaxKind.WithStatement  =>
return checkWithStatement( node.asInstanceOf[WithStatement] )
  case  SyntaxKind.SwitchStatement  =>
return checkSwitchStatement( node.asInstanceOf[SwitchStatement] )
  case  SyntaxKind.LabeledStatement  =>
return checkLabeledStatement( node.asInstanceOf[LabeledStatement] )
  case  SyntaxKind.ThrowStatement  =>
return checkThrowStatement( node.asInstanceOf[ThrowStatement] )
  case  SyntaxKind.TryStatement  =>
return checkTryStatement( node.asInstanceOf[TryStatement] )
  case  SyntaxKind.VariableDeclaration  =>
return checkVariableDeclaration( node.asInstanceOf[VariableDeclaration] )
  case  SyntaxKind.BindingElement  =>
return checkBindingElement( node.asInstanceOf[BindingElement] )
  case  SyntaxKind.ClassDeclaration  =>
return checkClassDeclaration( node.asInstanceOf[ClassDeclaration] )
  case  SyntaxKind.InterfaceDeclaration  =>
return checkInterfaceDeclaration( node.asInstanceOf[InterfaceDeclaration] )
  case  SyntaxKind.TypeAliasDeclaration  =>
return checkTypeAliasDeclaration( node.asInstanceOf[TypeAliasDeclaration] )
  case  SyntaxKind.EnumDeclaration  =>
return checkEnumDeclaration( node.asInstanceOf[EnumDeclaration] )
  case  SyntaxKind.ModuleDeclaration  =>
return checkModuleDeclaration( node.asInstanceOf[ModuleDeclaration] )
  case  SyntaxKind.ImportDeclaration  =>
return checkImportDeclaration( node.asInstanceOf[ImportDeclaration] )
  case  SyntaxKind.ImportEqualsDeclaration  =>
return checkImportEqualsDeclaration( node.asInstanceOf[ImportEqualsDeclaration] )
  case  SyntaxKind.ExportDeclaration  =>
return checkExportDeclaration( node.asInstanceOf[ExportDeclaration] )
  case  SyntaxKind.ExportAssignment  =>
return checkExportAssignment( node.asInstanceOf[ExportAssignment] )
  case  SyntaxKind.EmptyStatement  =>
checkGrammarStatementInAmbientContext( node )
return
  case  SyntaxKind.DebuggerStatement  =>
checkGrammarStatementInAmbientContext( node )
return
  case  SyntaxKind.MissingDeclaration  =>
return checkMissingDeclaration( node )
  case _ =>
}

}
def checkNodeDeferred(node: Node) = {
 if (deferredNodes) {
 deferredNodes.push( node )

}

}
def checkDeferredNodes() = {
 (deferredNodes).foreach { fresh143 =>
val node = zeroOfMyType
 = fresh143
 {
 node.kind match {
  case  SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature  =>
checkFunctionExpressionOrObjectLiteralMethodDeferred( node.asInstanceOf[FunctionExpression] )
  case  SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
checkAccessorDeferred( node.asInstanceOf[AccessorDeclaration] )
  case  SyntaxKind.ClassExpression  =>
checkClassExpressionDeferred( node.asInstanceOf[ClassExpression] )
  case _ =>
}

}
}

}
def checkSourceFile(node: SourceFile) = {
 performance.mark( "beforeCheck" )
checkSourceFileWorker( node )
performance.mark( "afterCheck" )
performance.measure( "Check", "beforeCheck", "afterCheck" )

}
def checkSourceFileWorker(node: SourceFile) = {
 val links = getNodeLinks( node )
if ((!((links.flags&NodeCheckFlags.TypeChecked)))) {
 if (((compilerOptions.skipLibCheck&&node.isDeclarationFile)||(compilerOptions.skipDefaultLibCheck&&node.hasNoDefaultLib))) {
 return

}
checkGrammarSourceFile( node )
(potentialThisCollisions.length=0)
(deferredNodes=Array())
(deferredUnusedIdentifierNodes=(if ((produceDiagnostics&&noUnusedIdentifiers)) Array() else undefined))
forEach( node.statements, checkSourceElement )
checkDeferredNodes()
if (isExternalModule( node )) {
 registerForUnusedIdentifiersCheck( node )

}
if ((!node.isDeclarationFile)) {
 checkUnusedIdentifiers()

}
(deferredNodes=undefined)
(deferredUnusedIdentifierNodes=undefined)
if (isExternalOrCommonJsModule( node )) {
 checkExternalModuleExports( node )

}
if (potentialThisCollisions.length) {
 forEach( potentialThisCollisions, checkIfThisIsCapturedInEnclosingScope )
(potentialThisCollisions.length=0)

}
(links.flags|=NodeCheckFlags.TypeChecked)

}

}
def getDiagnostics(sourceFile: SourceFile, ct: CancellationToken): Array[Diagnostic] = {
 try {
 (cancellationToken=ct)
return getDiagnosticsWorker( sourceFile )

} finally {
 (cancellationToken=undefined)

}

}
def getDiagnosticsWorker(sourceFile: SourceFile): Array[Diagnostic] = {
 throwIfNonDiagnosticsProducing()
if (sourceFile) {
 checkSourceFile( sourceFile )
return diagnostics.getDiagnostics( sourceFile.fileName )

}
forEach( host.getSourceFiles(), checkSourceFile )
return diagnostics.getDiagnostics()

}
def getGlobalDiagnostics(): Array[Diagnostic] = {
 throwIfNonDiagnosticsProducing()
return diagnostics.getGlobalDiagnostics()

}
def throwIfNonDiagnosticsProducing() = {
 if ((!produceDiagnostics)) {
 throw new Error( "Trying to get diagnostics from a type checker that does not produce them." )
}

}
def isInsideWithStatementBody(node: Node): Boolean = {
 if (node) {
 while (node.parent) {
{
 if (((node.parent.kind===SyntaxKind.WithStatement)&&((node.parent.asInstanceOf[WithStatement]).statement===node))) {
 return true

}
(node=node.parent)

}
}

}
return false

}
def getSymbolsInScope(location: Node, meaning: SymbolFlags): Array[Symbol] = {
 val symbols = createMap[ Symbol ]()
var memberFlags: ModifierFlags = ModifierFlags.None
if (isInsideWithStatementBody( location )) {
 return Array()

}
populateSymbols()
return symbolsToArray( symbols )
def populateSymbols() = {
 while (location) {
{
 if ((location.locals&&(!isGlobalSourceFile( location )))) {
 copySymbols( location.locals, meaning )

}
location.kind match {
  case  SyntaxKind.SourceFile  =>
if ((!isExternalOrCommonJsModule( location.asInstanceOf[SourceFile] ))) {
 break()

}
  case  SyntaxKind.ModuleDeclaration  =>
copySymbols( getSymbolOfNode( location ).exports, (meaning&SymbolFlags.ModuleMember) )
  case  SyntaxKind.EnumDeclaration  =>
copySymbols( getSymbolOfNode( location ).exports, (meaning&SymbolFlags.EnumMember) )
  case  SyntaxKind.ClassExpression  =>
val className = (location.asInstanceOf[ClassExpression]).name
if (className) {
 copySymbol( location.symbol, meaning )

}
  case  SyntaxKind.ClassDeclaration | SyntaxKind.InterfaceDeclaration  =>
if ((!((memberFlags&ModifierFlags.Static)))) {
 copySymbols( getSymbolOfNode( location ).members, (meaning&SymbolFlags.Type) )

}
  case  SyntaxKind.FunctionExpression  =>
val funcName = (location.asInstanceOf[FunctionExpression]).name
if (funcName) {
 copySymbol( location.symbol, meaning )

}
  case _ =>
}
if (introducesArgumentsExoticObject( location )) {
 copySymbol( argumentsSymbol, meaning )

}
(memberFlags=getModifierFlags( location ))
(location=location.parent)

}
}
copySymbols( globals, meaning )

}
def copySymbol(symbol: Symbol, meaning: SymbolFlags): Unit = {
 if ((symbol.flags&meaning)) {
 val id = symbol.name
if ((!symbols(id))) {
 (symbols(id)=symbol)

}

}

}
def copySymbols(source: SymbolTable, meaning: SymbolFlags): Unit = {
 if (meaning) {
 (source).keys.foreach { fresh144 =>
val id = zeroOfMyType
 = fresh144
 {
 val symbol = source(id)
copySymbol( symbol, meaning )

}
}

}

}

}
def isTypeDeclarationName(name: Node): Boolean = {
 return (((name.kind===SyntaxKind.Identifier)&&isTypeDeclaration( name.parent ))&&((name.parent.asInstanceOf[Declaration]).name===name))

}
def isTypeDeclaration(node: Node): Boolean = {
 node.kind match {
  case  SyntaxKind.TypeParameter | SyntaxKind.ClassDeclaration | SyntaxKind.InterfaceDeclaration | SyntaxKind.TypeAliasDeclaration | SyntaxKind.EnumDeclaration  =>
return true
  case _ =>
}

}
def isTypeReferenceIdentifier(entityName: EntityName): Boolean = {
 var node: Node = entityName
while ((node.parent&&(node.parent.kind===SyntaxKind.QualifiedName))) {
{
 (node=node.parent)

}
}
return (node.parent&&(((node.parent.kind===SyntaxKind.TypeReference)||(node.parent.kind===SyntaxKind.JSDocTypeReference))))

}
def isHeritageClauseElementIdentifier(entityName: Node): Boolean = {
 var node = entityName
while ((node.parent&&(node.parent.kind===SyntaxKind.PropertyAccessExpression))) {
{
 (node=node.parent)

}
}
return (node.parent&&(node.parent.kind===SyntaxKind.ExpressionWithTypeArguments))

}
def forEachEnclosingClass[T](node: Node, callback: ((Node) => T)): T = {
 var result: T = zeroOfMyType
while (true) {
{
 (node=getContainingClass( node ))
if ((!node))
break()
if ((result=callback( node )))
break()

}
}
return result

}
def isNodeWithinClass(node: Node, classDeclaration: ClassLikeDeclaration) = {
 return (!(!forEachEnclosingClass( node, (n =>  (n===classDeclaration)) )))

}
def getLeftSideOfImportEqualsOrExportAssignment(nodeOnRightSide: EntityName): ( ImportEqualsDeclaration | ExportAssignment ) = {
 while ((nodeOnRightSide.parent.kind===SyntaxKind.QualifiedName)) {
{
 (nodeOnRightSide=nodeOnRightSide.parent.asInstanceOf[QualifiedName])

}
}
if ((nodeOnRightSide.parent.kind===SyntaxKind.ImportEqualsDeclaration)) {
 return (((nodeOnRightSide.parent.asInstanceOf[ImportEqualsDeclaration]).moduleReference===nodeOnRightSide)&&nodeOnRightSide.parent.asInstanceOf[ImportEqualsDeclaration])

}
if ((nodeOnRightSide.parent.kind===SyntaxKind.ExportAssignment)) {
 return (((nodeOnRightSide.parent.asInstanceOf[ExportAssignment]).expression===nodeOnRightSide.asInstanceOf[Node])&&nodeOnRightSide.parent.asInstanceOf[ExportAssignment])

}
return undefined

}
def isInRightSideOfImportOrExportAssignment(node: EntityName) = {
 return (getLeftSideOfImportEqualsOrExportAssignment( node )!==undefined)

}
def getSymbolOfEntityNameOrPropertyAccessExpression(entityName: ( EntityName | PropertyAccessExpression )): ( Symbol | undefined ) = {
 if (isDeclarationName( entityName )) {
 return getSymbolOfNode( entityName.parent )

}
if ((isInJavaScriptFile( entityName )&&(entityName.parent.kind===SyntaxKind.PropertyAccessExpression))) {
 val specialPropertyAssignmentKind = getSpecialPropertyAssignmentKind( entityName.parent.parent )
specialPropertyAssignmentKind match {
  case  SpecialPropertyAssignmentKind.ExportsProperty | SpecialPropertyAssignmentKind.PrototypeProperty  =>
return getSymbolOfNode( entityName.parent )
  case  SpecialPropertyAssignmentKind.ThisProperty | SpecialPropertyAssignmentKind.ModuleExports  =>
return getSymbolOfNode( entityName.parent.parent )
  case _ =>
}

}
if (((entityName.parent.kind===SyntaxKind.ExportAssignment)&&isEntityNameExpression( entityName.asInstanceOf[( Identifier | PropertyAccessExpression )] ))) {
 return resolveEntityName( entityName.asInstanceOf[EntityNameExpression], (((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace)|SymbolFlags.Alias) )

}
if (((entityName.kind!==SyntaxKind.PropertyAccessExpression)&&isInRightSideOfImportOrExportAssignment( entityName.asInstanceOf[EntityName] ))) {
 val importEqualsDeclaration = getAncestor( entityName, SyntaxKind.ImportEqualsDeclaration ).asInstanceOf[ImportEqualsDeclaration]
Debug.assert( (importEqualsDeclaration!==undefined) )
return getSymbolOfPartOfRightHandSideOfImportEquals( entityName.asInstanceOf[EntityName], true )

}
if (isRightSideOfQualifiedNameOrPropertyAccess( entityName )) {
 (entityName=entityName.parent.asInstanceOf[( QualifiedName | PropertyAccessEntityNameExpression )])

}
if (isHeritageClauseElementIdentifier( entityName.asInstanceOf[EntityName] )) {
 var meaning = SymbolFlags.None
if ((entityName.parent.kind===SyntaxKind.ExpressionWithTypeArguments)) {
 (meaning=SymbolFlags.Type)
if (isExpressionWithTypeArgumentsInClassExtendsClause( entityName.parent )) {
 (meaning|=SymbolFlags.Value)

}

}
else {
 (meaning=SymbolFlags.Namespace)

}
(meaning|=SymbolFlags.Alias)
return resolveEntityName( entityName.asInstanceOf[EntityName], meaning )

}
else if (isPartOfExpression( entityName )) {
 if (nodeIsMissing( entityName )) {
 return undefined

}
if ((entityName.kind===SyntaxKind.Identifier)) {
 if ((isJSXTagName( entityName )&&isJsxIntrinsicIdentifier( entityName.asInstanceOf[Identifier] ))) {
 return getIntrinsicTagSymbol( entityName.parent.asInstanceOf[JsxOpeningLikeElement] )

}
return resolveEntityName( entityName.asInstanceOf[Identifier], SymbolFlags.Value, false, true )

}
else if ((entityName.kind===SyntaxKind.PropertyAccessExpression)) {
 val symbol = getNodeLinks( entityName ).resolvedSymbol
if ((!symbol)) {
 checkPropertyAccessExpression( entityName.asInstanceOf[PropertyAccessExpression] )

}
return getNodeLinks( entityName ).resolvedSymbol

}
else if ((entityName.kind===SyntaxKind.QualifiedName)) {
 val symbol = getNodeLinks( entityName ).resolvedSymbol
if ((!symbol)) {
 checkQualifiedName( entityName.asInstanceOf[QualifiedName] )

}
return getNodeLinks( entityName ).resolvedSymbol

}

}
else if (isTypeReferenceIdentifier( entityName.asInstanceOf[EntityName] )) {
 val meaning = (if ((((entityName.parent.kind===SyntaxKind.TypeReference)||(entityName.parent.kind===SyntaxKind.JSDocTypeReference)))) SymbolFlags.Type else SymbolFlags.Namespace)
return resolveEntityName( entityName.asInstanceOf[EntityName], meaning, false, true )

}
else if ((entityName.parent.kind===SyntaxKind.JsxAttribute)) {
 return getJsxAttributePropertySymbol( entityName.parent.asInstanceOf[JsxAttribute] )

}
if ((entityName.parent.kind===SyntaxKind.TypePredicate)) {
 return resolveEntityName( entityName.asInstanceOf[Identifier], SymbolFlags.FunctionScopedVariable )

}
return undefined

}
def getSymbolAtLocation(node: Node) = {
 if ((node.kind===SyntaxKind.SourceFile)) {
 return (if (isExternalModule( node.asInstanceOf[SourceFile] )) getMergedSymbol( node.symbol ) else undefined)

}
if (isInsideWithStatementBody( node )) {
 return undefined

}
if (isDeclarationName( node )) {
 return getSymbolOfNode( node.parent )

}
else if (isLiteralComputedPropertyDeclarationName( node )) {
 return getSymbolOfNode( node.parent.parent )

}
if ((node.kind===SyntaxKind.Identifier)) {
 if (isInRightSideOfImportOrExportAssignment( node.asInstanceOf[Identifier] )) {
 return getSymbolOfEntityNameOrPropertyAccessExpression( node.asInstanceOf[Identifier] )

}
else if ((((node.parent.kind===SyntaxKind.BindingElement)&&(node.parent.parent.kind===SyntaxKind.ObjectBindingPattern))&&(node===(node.parent.asInstanceOf[BindingElement]).propertyName))) {
 val typeOfPattern = getTypeOfNode( node.parent.parent )
val propertyDeclaration = (typeOfPattern&&getPropertyOfType( typeOfPattern, (node.asInstanceOf[Identifier]).text ))
if (propertyDeclaration) {
 return propertyDeclaration

}

}

}
node.kind match {
  case  SyntaxKind.Identifier | SyntaxKind.PropertyAccessExpression | SyntaxKind.QualifiedName  =>
return getSymbolOfEntityNameOrPropertyAccessExpression( node.asInstanceOf[( EntityName | PropertyAccessExpression )] )
  case  SyntaxKind.ThisKeyword  =>
val container = getThisContainer( node, false )
if (isFunctionLike( container )) {
 val sig = getSignatureFromDeclaration( container )
if (sig.thisParameter) {
 return sig.thisParameter

}

}
  case  SyntaxKind.SuperKeyword  =>
val `type` = (if (isPartOfExpression( node )) checkExpression( node.asInstanceOf[Expression] ) else getTypeFromTypeNode( node.asInstanceOf[TypeNode] ))
return `type`.symbol
  case  SyntaxKind.ThisType  =>
return getTypeFromTypeNode( node.asInstanceOf[TypeNode] ).symbol
  case  SyntaxKind.ConstructorKeyword  =>
val constructorDeclaration = node.parent
if ((constructorDeclaration&&(constructorDeclaration.kind===SyntaxKind.Constructor))) {
 return (constructorDeclaration.parent.asInstanceOf[ClassDeclaration]).symbol

}
return undefined
  case  SyntaxKind.StringLiteral  =>
if ((((isExternalModuleImportEqualsDeclaration( node.parent.parent )&&(getExternalModuleImportEqualsDeclarationExpression( node.parent.parent )===node)))||(((((node.parent.kind===SyntaxKind.ImportDeclaration)||(node.parent.kind===SyntaxKind.ExportDeclaration)))&&((node.parent.asInstanceOf[ImportDeclaration]).moduleSpecifier===node))))) {
 return resolveExternalModuleName( node, node.asInstanceOf[LiteralExpression] )

}
if ((isInJavaScriptFile( node )&&isRequireCall( node.parent, false ))) {
 return resolveExternalModuleName( node, node.asInstanceOf[LiteralExpression] )

}
  case  SyntaxKind.NumericLiteral  =>
if (((node.parent.kind===SyntaxKind.ElementAccessExpression)&&((node.parent.asInstanceOf[ElementAccessExpression]).argumentExpression===node))) {
 val objectType = checkExpression( (node.parent.asInstanceOf[ElementAccessExpression]).expression )
if ((objectType===unknownType))
return undefined
val apparentType = getApparentType( objectType )
if ((apparentType===unknownType))
return undefined
return getPropertyOfType( apparentType, (node.asInstanceOf[NumericLiteral]).text )

}
  case _ =>
}
return undefined

}
def getShorthandAssignmentValueSymbol(location: Node): Symbol = {
 if ((location&&(location.kind===SyntaxKind.ShorthandPropertyAssignment))) {
 return resolveEntityName( (location.asInstanceOf[ShorthandPropertyAssignment]).name, (SymbolFlags.Value|SymbolFlags.Alias) )

}
return undefined

}
def getExportSpecifierLocalTargetSymbol(node: ExportSpecifier): Symbol = {
 return (if ((node.parent.parent.asInstanceOf[ExportDeclaration]).moduleSpecifier) getExternalModuleMember( node.parent.parent.asInstanceOf[ExportDeclaration], node ) else resolveEntityName( (node.propertyName||node.name), (((SymbolFlags.Value|SymbolFlags.Type)|SymbolFlags.Namespace)|SymbolFlags.Alias) ))

}
def getTypeOfNode(node: Node): Type = {
 if (isInsideWithStatementBody( node )) {
 return unknownType

}
if (isPartOfTypeNode( node )) {
 return getTypeFromTypeNode( node.asInstanceOf[TypeNode] )

}
if (isPartOfExpression( node )) {
 return getTypeOfExpression( node.asInstanceOf[Expression] )

}
if (isExpressionWithTypeArgumentsInClassExtendsClause( node )) {
 return getBaseTypes( getDeclaredTypeOfSymbol( getSymbolOfNode( node.parent.parent ) ).asInstanceOf[InterfaceType] )(0)

}
if (isTypeDeclaration( node )) {
 val symbol = getSymbolOfNode( node )
return getDeclaredTypeOfSymbol( symbol )

}
if (isTypeDeclarationName( node )) {
 val symbol = getSymbolAtLocation( node )
return (symbol&&getDeclaredTypeOfSymbol( symbol ))

}
if (isDeclaration( node )) {
 val symbol = getSymbolOfNode( node )
return getTypeOfSymbol( symbol )

}
if (isDeclarationName( node )) {
 val symbol = getSymbolAtLocation( node )
return (symbol&&getTypeOfSymbol( symbol ))

}
if (isBindingPattern( node )) {
 return getTypeForVariableLikeDeclaration( node.parent.asInstanceOf[VariableLikeDeclaration], true )

}
if (isInRightSideOfImportOrExportAssignment( node.asInstanceOf[Identifier] )) {
 val symbol = getSymbolAtLocation( node )
val declaredType = (symbol&&getDeclaredTypeOfSymbol( symbol ))
return (if ((declaredType!==unknownType)) declaredType else getTypeOfSymbol( symbol ))

}
return unknownType

}
def getTypeOfArrayLiteralOrObjectLiteralDestructuringAssignment(expr: Expression): Type = {
 Debug.assert( ((expr.kind===SyntaxKind.ObjectLiteralExpression)||(expr.kind===SyntaxKind.ArrayLiteralExpression)) )
if ((expr.parent.kind===SyntaxKind.ForOfStatement)) {
 val iteratedType = checkRightHandSideOfForOf( (expr.parent.asInstanceOf[ForOfStatement]).expression )
return checkDestructuringAssignment( expr, (iteratedType||unknownType) )

}
if ((expr.parent.kind===SyntaxKind.BinaryExpression)) {
 val iteratedType = checkExpression( (expr.parent.asInstanceOf[BinaryExpression]).right )
return checkDestructuringAssignment( expr, (iteratedType||unknownType) )

}
if ((expr.parent.kind===SyntaxKind.PropertyAssignment)) {
 val typeOfParentObjectLiteral = getTypeOfArrayLiteralOrObjectLiteralDestructuringAssignment( expr.parent.parent.asInstanceOf[Expression] )
return checkObjectLiteralDestructuringPropertyAssignment( (typeOfParentObjectLiteral||unknownType), expr.parent.asInstanceOf[ObjectLiteralElementLike] )

}
Debug.assert( (expr.parent.kind===SyntaxKind.ArrayLiteralExpression) )
val typeOfArrayLiteral = getTypeOfArrayLiteralOrObjectLiteralDestructuringAssignment( expr.parent.asInstanceOf[Expression] )
val elementType = (checkIteratedTypeOrElementType( (typeOfArrayLiteral||unknownType), expr.parent, false )||unknownType)
return checkArrayLiteralDestructuringElementAssignment( expr.parent.asInstanceOf[ArrayLiteralExpression], typeOfArrayLiteral, indexOf( (expr.parent.asInstanceOf[ArrayLiteralExpression]).elements, expr ), (elementType||unknownType) )

}
def getPropertySymbolOfDestructuringAssignment(location: Identifier) = {
 val typeOfObjectLiteral = getTypeOfArrayLiteralOrObjectLiteralDestructuringAssignment( location.parent.parent.asInstanceOf[Expression] )
return (typeOfObjectLiteral&&getPropertyOfType( typeOfObjectLiteral, location.text ))

}
def getTypeOfExpression(expr: Expression): Type = {
 if (isRightSideOfQualifiedNameOrPropertyAccess( expr )) {
 (expr=expr.parent.asInstanceOf[Expression])

}
return getRegularTypeOfLiteralType( checkExpression( expr ) )

}
def getParentTypeOfClassElement(node: ClassElement) = {
 val classSymbol = getSymbolOfNode( node.parent )
return (if ((getModifierFlags( node )&ModifierFlags.Static)) getTypeOfSymbol( classSymbol ) else getDeclaredTypeOfSymbol( classSymbol ))

}
def getAugmentedPropertiesOfType(`type`: Type): Array[Symbol] = {
 (`type`=getApparentType( `type` ))
val propsByName = createSymbolTable( getPropertiesOfType( `type` ) )
if ((getSignaturesOfType( `type`, SignatureKind.Call ).length||getSignaturesOfType( `type`, SignatureKind.Construct ).length)) {
 forEach( getPropertiesOfType( globalFunctionType ), (p =>  {
 if ((!propsByName(p.name))) {
 (propsByName(p.name)=p)

}

}) )

}
return getNamedMembers( propsByName )

}
def getRootSymbols(symbol: Symbol): Array[Symbol] = {
 if ((symbol.flags&SymbolFlags.SyntheticProperty)) {
 val symbols: Array[Symbol] = Array()
val name = symbol.name
forEach( getSymbolLinks( symbol ).containingType.types, (t =>  {
 val symbol = getPropertyOfType( t, name )
 if (symbol) {
 symbols.push( symbol )

}

}) )
return symbols

}
else if ((symbol.flags&SymbolFlags.Transient)) {
 var target: Symbol = zeroOfMyType
var next = symbol
while ((next=getSymbolLinks( next ).target)) {
{
 (target=next)

}
}
if (target) {
 return Array( target )

}

}
return Array( symbol )

}
def isArgumentsLocalBinding(node: Identifier): Boolean = {
 if ((!isGeneratedIdentifier( node ))) {
 (node=getParseTreeNode( node, isIdentifier ))
if (node) {
 return (getReferencedValueSymbol( node )===argumentsSymbol)

}

}
return false

}
def moduleExportsSomeValue(moduleReferenceExpression: Expression): Boolean = {
 var moduleSymbol = resolveExternalModuleName( moduleReferenceExpression.parent, moduleReferenceExpression )
if (((!moduleSymbol)||isShorthandAmbientModuleSymbol( moduleSymbol ))) {
 return true

}
val hasExportAssignment = hasExportAssignmentSymbol( moduleSymbol )
(moduleSymbol=resolveExternalModuleSymbol( moduleSymbol ))
val symbolLinks = getSymbolLinks( moduleSymbol )
if ((symbolLinks.exportsSomeValue===undefined)) {
 (symbolLinks.exportsSomeValue=(if (hasExportAssignment) (!(!((moduleSymbol.flags&SymbolFlags.Value)))) else forEachProperty( getExportsOfModule( moduleSymbol ), isValue )))

}
return symbolLinks.exportsSomeValue
def isValue(s: Symbol): Boolean = {
 (s=resolveSymbol( s ))
return (s&&(!(!((s.flags&SymbolFlags.Value)))))

}

}
def isNameOfModuleOrEnumDeclaration(node: Identifier) = {
 val parent = node.parent
return (isModuleOrEnumDeclaration( parent )&&(node===parent.name))

}
def getReferencedExportContainer(node: Identifier, prefixLocals: Boolean): ( SourceFile | ModuleDeclaration | EnumDeclaration | undefined ) = {
 (node=getParseTreeNode( node, isIdentifier ))
if (node) {
 var symbol = getReferencedValueSymbol( node, isNameOfModuleOrEnumDeclaration( node ) )
if (symbol) {
 if ((symbol.flags&SymbolFlags.ExportValue)) {
 val exportSymbol = getMergedSymbol( symbol.exportSymbol )
if (((!prefixLocals)&&(exportSymbol.flags&SymbolFlags.ExportHasLocal))) {
 return undefined

}
(symbol=exportSymbol)

}
val parentSymbol = getParentOfSymbol( symbol )
if (parentSymbol) {
 if (((parentSymbol.flags&SymbolFlags.ValueModule)&&(parentSymbol.valueDeclaration.kind===SyntaxKind.SourceFile))) {
 val symbolFile = parentSymbol.valueDeclaration.asInstanceOf[SourceFile]
val referenceFile = getSourceFileOfNode( node )
val symbolIsUmdExport = (symbolFile!==referenceFile)
return (if (symbolIsUmdExport) undefined else symbolFile)

}
{
var n = node.parent
while( n) {
 {
 if ((isModuleOrEnumDeclaration( n )&&(getSymbolOfNode( n )===parentSymbol))) {
 return n.asInstanceOf[( ModuleDeclaration | EnumDeclaration )]

}

}
 (n=n.parent)
}
}

}

}

}

}
def getReferencedImportDeclaration(node: Identifier): Declaration = {
 (node=getParseTreeNode( node, isIdentifier ))
if (node) {
 val symbol = getReferencedValueSymbol( node )
if ((symbol&&(symbol.flags&SymbolFlags.Alias))) {
 return getDeclarationOfAliasSymbol( symbol )

}

}
return undefined

}
def isSymbolOfDeclarationWithCollidingName(symbol: Symbol): Boolean = {
 if ((symbol.flags&SymbolFlags.BlockScoped)) {
 val links = getSymbolLinks( symbol )
if ((links.isDeclarationWithCollidingName===undefined)) {
 val container = getEnclosingBlockScopeContainer( symbol.valueDeclaration )
if (isStatementWithLocals( container )) {
 val nodeLinks = getNodeLinks( symbol.valueDeclaration )
if ((!(!resolveName( container.parent, symbol.name, SymbolFlags.Value, undefined, undefined )))) {
 (links.isDeclarationWithCollidingName=true)

}
else if ((nodeLinks.flags&NodeCheckFlags.CapturedBlockScopedBinding)) {
 val isDeclaredInLoop = (nodeLinks.flags&NodeCheckFlags.BlockScopedBindingInLoop)
val inLoopInitializer = isIterationStatement( container, false )
val inLoopBodyBlock = ((container.kind===SyntaxKind.Block)&&isIterationStatement( container.parent, false ))
(links.isDeclarationWithCollidingName=((!isBlockScopedContainerTopLevel( container ))&&(((!isDeclaredInLoop)||(((!inLoopInitializer)&&(!inLoopBodyBlock)))))))

}
else {
 (links.isDeclarationWithCollidingName=false)

}

}

}
return links.isDeclarationWithCollidingName

}
return false

}
def getReferencedDeclarationWithCollidingName(node: Identifier): Declaration = {
 if ((!isGeneratedIdentifier( node ))) {
 (node=getParseTreeNode( node, isIdentifier ))
if (node) {
 val symbol = getReferencedValueSymbol( node )
if ((symbol&&isSymbolOfDeclarationWithCollidingName( symbol ))) {
 return symbol.valueDeclaration

}

}

}
return undefined

}
def isDeclarationWithCollidingName(node: Declaration): Boolean = {
 (node=getParseTreeNode( node, isDeclaration ))
if (node) {
 val symbol = getSymbolOfNode( node )
if (symbol) {
 return isSymbolOfDeclarationWithCollidingName( symbol )

}

}
return false

}
def isValueAliasDeclaration(node: Node): Boolean = {
 (node=getParseTreeNode( node ))
if ((node===undefined)) {
 return true

}
node.kind match {
  case  SyntaxKind.ImportEqualsDeclaration | SyntaxKind.ImportClause | SyntaxKind.NamespaceImport | SyntaxKind.ImportSpecifier | SyntaxKind.ExportSpecifier  =>
return isAliasResolvedToValue( (getSymbolOfNode( node )||unknownSymbol) )
  case  SyntaxKind.ExportDeclaration  =>
val exportClause = (node.asInstanceOf[ExportDeclaration]).exportClause
return (exportClause&&forEach( exportClause.elements, isValueAliasDeclaration ))
  case  SyntaxKind.ExportAssignment  =>
return (if (((node.asInstanceOf[ExportAssignment]).expression&&((node.asInstanceOf[ExportAssignment]).expression.kind===SyntaxKind.Identifier))) isAliasResolvedToValue( (getSymbolOfNode( node )||unknownSymbol) ) else true)
  case _ =>
}
return false

}
def isTopLevelValueImportEqualsWithEntityName(node: ImportEqualsDeclaration): Boolean = {
 (node=getParseTreeNode( node, isImportEqualsDeclaration ))
if ((((node===undefined)||(node.parent.kind!==SyntaxKind.SourceFile))||(!isInternalModuleImportEqualsDeclaration( node )))) {
 return false

}
val isValue = isAliasResolvedToValue( getSymbolOfNode( node ) )
return ((isValue&&node.moduleReference)&&(!nodeIsMissing( node.moduleReference )))

}
def isAliasResolvedToValue(symbol: Symbol): Boolean = {
 val target = resolveAlias( symbol )
if ((target===unknownSymbol)) {
 return true

}
return ((target.flags&SymbolFlags.Value)&&((compilerOptions.preserveConstEnums||(!isConstEnumOrConstEnumOnlyModule( target )))))

}
def isConstEnumOrConstEnumOnlyModule(s: Symbol): Boolean = {
 return (isConstEnumSymbol( s )||s.constEnumOnlyModule)

}
def isReferencedAliasDeclaration(node: Node, checkChildren: Boolean): Boolean = {
 (node=getParseTreeNode( node ))
if ((node===undefined)) {
 return true

}
if (isAliasSymbolDeclaration( node )) {
 val symbol = getSymbolOfNode( node )
if ((symbol&&getSymbolLinks( symbol ).referenced)) {
 return true

}

}
if (checkChildren) {
 return forEachChild( node, (node =>  isReferencedAliasDeclaration( node, checkChildren )) )

}
return false

}
def isImplementationOfOverload(node: FunctionLikeDeclaration) = {
 if (nodeIsPresent( node.body )) {
 val symbol = getSymbolOfNode( node )
val signaturesOfSymbol = getSignaturesOfSymbol( symbol )
return ((signaturesOfSymbol.length>1)||(((signaturesOfSymbol.length===1)&&(signaturesOfSymbol(0).declaration!==node))))

}
return false

}
def getNodeCheckFlags(node: Node): NodeCheckFlags = {
 (node=getParseTreeNode( node ))
return (if (node) getNodeLinks( node ).flags else undefined)

}
def getEnumMemberValue(node: EnumMember): Int = {
 computeEnumMemberValues( node.parent.asInstanceOf[EnumDeclaration] )
return getNodeLinks( node ).enumMemberValue

}
def getConstantValue(node: ( EnumMember | PropertyAccessExpression | ElementAccessExpression )): Int = {
 if ((node.kind===SyntaxKind.EnumMember)) {
 return getEnumMemberValue( node.asInstanceOf[EnumMember] )

}
val symbol = getNodeLinks( node ).resolvedSymbol
if ((symbol&&((symbol.flags&SymbolFlags.EnumMember)))) {
 if (isConstEnumDeclaration( symbol.valueDeclaration.parent )) {
 return getEnumMemberValue( symbol.valueDeclaration.asInstanceOf[EnumMember] )

}

}
return undefined

}
def isFunctionType(`type`: Type): Boolean = {
 return ((`type`.flags&TypeFlags.Object)&&(getSignaturesOfType( `type`, SignatureKind.Call ).length>0))

}
def getTypeReferenceSerializationKind(typeName: EntityName, location: Node): TypeReferenceSerializationKind = {
 val valueSymbol = resolveEntityName( typeName, SymbolFlags.Value, true, false, location )
val globalPromiseSymbol = tryGetGlobalPromiseConstructorSymbol()
if ((globalPromiseSymbol&&(valueSymbol===globalPromiseSymbol))) {
 return TypeReferenceSerializationKind.Promise

}
val constructorType = (if (valueSymbol) getTypeOfSymbol( valueSymbol ) else undefined)
if ((constructorType&&isConstructorType( constructorType ))) {
 return TypeReferenceSerializationKind.TypeWithConstructSignatureAndValue

}
val typeSymbol = resolveEntityName( typeName, SymbolFlags.Type, true, false, location )
if ((!typeSymbol)) {
 return TypeReferenceSerializationKind.ObjectType

}
val `type` = getDeclaredTypeOfSymbol( typeSymbol )
if ((`type`===unknownType)) {
 return TypeReferenceSerializationKind.Unknown

}
else if ((`type`.flags&TypeFlags.Any)) {
 return TypeReferenceSerializationKind.ObjectType

}
else if (isTypeOfKind( `type`, ((TypeFlags.Void|TypeFlags.Nullable)|TypeFlags.Never) )) {
 return TypeReferenceSerializationKind.VoidNullableOrNeverType

}
else if (isTypeOfKind( `type`, TypeFlags.BooleanLike )) {
 return TypeReferenceSerializationKind.BooleanType

}
else if (isTypeOfKind( `type`, TypeFlags.NumberLike )) {
 return TypeReferenceSerializationKind.NumberLikeType

}
else if (isTypeOfKind( `type`, TypeFlags.StringLike )) {
 return TypeReferenceSerializationKind.StringLikeType

}
else if (isTupleType( `type` )) {
 return TypeReferenceSerializationKind.ArrayLikeType

}
else if (isTypeOfKind( `type`, TypeFlags.ESSymbol )) {
 return TypeReferenceSerializationKind.ESSymbolType

}
else if (isFunctionType( `type` )) {
 return TypeReferenceSerializationKind.TypeWithCallSignature

}
else if (isArrayType( `type` )) {
 return TypeReferenceSerializationKind.ArrayLikeType

}
else {
 return TypeReferenceSerializationKind.ObjectType

}

}
def writeTypeOfDeclaration(declaration: ( AccessorDeclaration | VariableLikeDeclaration ), enclosingDeclaration: Node, flags: TypeFormatFlags, writer: SymbolWriter) = {
 val symbol = getSymbolOfNode( declaration )
val `type` = (if ((symbol&&(!((symbol.flags&((SymbolFlags.TypeLiteral|SymbolFlags.Signature))))))) getWidenedLiteralType( getTypeOfSymbol( symbol ) ) else unknownType)
getSymbolDisplayBuilder().buildTypeDisplay( `type`, writer, enclosingDeclaration, flags )

}
def writeReturnTypeOfSignatureDeclaration(signatureDeclaration: SignatureDeclaration, enclosingDeclaration: Node, flags: TypeFormatFlags, writer: SymbolWriter) = {
 val signature = getSignatureFromDeclaration( signatureDeclaration )
getSymbolDisplayBuilder().buildTypeDisplay( getReturnTypeOfSignature( signature ), writer, enclosingDeclaration, flags )

}
def writeTypeOfExpression(expr: Expression, enclosingDeclaration: Node, flags: TypeFormatFlags, writer: SymbolWriter) = {
 val `type` = getWidenedType( getTypeOfExpression( expr ) )
getSymbolDisplayBuilder().buildTypeDisplay( `type`, writer, enclosingDeclaration, flags )

}
def writeBaseConstructorTypeOfClass(node: ClassLikeDeclaration, enclosingDeclaration: Node, flags: TypeFormatFlags, writer: SymbolWriter) = {
 val classType = getDeclaredTypeOfSymbol( getSymbolOfNode( node ) ).asInstanceOf[InterfaceType]
resolveBaseTypesOfClass( classType )
val baseType = (if (classType.resolvedBaseTypes.length) classType.resolvedBaseTypes(0) else unknownType)
getSymbolDisplayBuilder().buildTypeDisplay( baseType, writer, enclosingDeclaration, flags )

}
def hasGlobalName(name: String): Boolean = {
 return (!(!globals(name)))

}
def getReferencedValueSymbol(reference: Identifier, startInDeclarationContainer: Boolean): Symbol = {
 val resolvedSymbol = getNodeLinks( reference ).resolvedSymbol
if (resolvedSymbol) {
 return resolvedSymbol

}
var location: Node = reference
if (startInDeclarationContainer) {
 val parent = reference.parent
if ((isDeclaration( parent )&&(reference===parent.name))) {
 (location=getDeclarationContainer( parent ))

}

}
return resolveName( location, reference.text, ((SymbolFlags.Value|SymbolFlags.ExportValue)|SymbolFlags.Alias), undefined, undefined )

}
def getReferencedValueDeclaration(reference: Identifier): Declaration = {
 if ((!isGeneratedIdentifier( reference ))) {
 (reference=getParseTreeNode( reference, isIdentifier ))
if (reference) {
 val symbol = getReferencedValueSymbol( reference )
if (symbol) {
 return getExportSymbolOfValueSymbolIfExported( symbol ).valueDeclaration

}

}

}
return undefined

}
def isLiteralConstDeclaration(node: ( VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration )): Boolean = {
 if (isConst( node )) {
 val `type` = getTypeOfSymbol( getSymbolOfNode( node ) )
return (!(!(((`type`.flags&TypeFlags.StringOrNumberLiteral)&&(`type`.flags&TypeFlags.FreshLiteral)))))

}
return false

}
def writeLiteralConstValue(node: ( VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration ), writer: SymbolWriter) = {
 val `type` = getTypeOfSymbol( getSymbolOfNode( node ) )
writer.writeStringLiteral( literalTypeToString( `type`.asInstanceOf[LiteralType] ) )

}
def createResolver(): EmitResolver = {
 val resolvedTypeReferenceDirectives = host.getResolvedTypeReferenceDirectives()
var fileToDirective: FileMap[String] = zeroOfMyType
if (resolvedTypeReferenceDirectives) {
 (fileToDirective=createFileMap[ String ]())
(resolvedTypeReferenceDirectives).keys.foreach { fresh145 =>
val key = zeroOfMyType
 = fresh145
 {
 val resolvedDirective = resolvedTypeReferenceDirectives(key)
if ((!resolvedDirective)) {
 continue

}
val file = host.getSourceFile( resolvedDirective.resolvedFileName )
fileToDirective.set( file.path, key )

}
}

}
return Map( "getReferencedExportContainer" -> getReferencedExportContainer,
"getReferencedImportDeclaration" -> getReferencedImportDeclaration,
"getReferencedDeclarationWithCollidingName" -> getReferencedDeclarationWithCollidingName,
"isDeclarationWithCollidingName" -> isDeclarationWithCollidingName,
"isValueAliasDeclaration" -> isValueAliasDeclaration,
"hasGlobalName" -> hasGlobalName,
"isReferencedAliasDeclaration" -> isReferencedAliasDeclaration,
"getNodeCheckFlags" -> getNodeCheckFlags,
"isTopLevelValueImportEqualsWithEntityName" -> isTopLevelValueImportEqualsWithEntityName,
"isDeclarationVisible" -> isDeclarationVisible,
"isImplementationOfOverload" -> isImplementationOfOverload,
"writeTypeOfDeclaration" -> writeTypeOfDeclaration,
"writeReturnTypeOfSignatureDeclaration" -> writeReturnTypeOfSignatureDeclaration,
"writeTypeOfExpression" -> writeTypeOfExpression,
"writeBaseConstructorTypeOfClass" -> writeBaseConstructorTypeOfClass,
"isSymbolAccessible" -> isSymbolAccessible,
"isEntityNameVisible" -> isEntityNameVisible,
"getConstantValue" -> getConstantValue,
"collectLinkedAliases" -> collectLinkedAliases,
"getReferencedValueDeclaration" -> getReferencedValueDeclaration,
"getTypeReferenceSerializationKind" -> getTypeReferenceSerializationKind,
"isOptionalParameter" -> isOptionalParameter,
"moduleExportsSomeValue" -> moduleExportsSomeValue,
"isArgumentsLocalBinding" -> isArgumentsLocalBinding,
"getExternalModuleFileFromDeclaration" -> getExternalModuleFileFromDeclaration,
"getTypeReferenceDirectivesForEntityName" -> getTypeReferenceDirectivesForEntityName,
"getTypeReferenceDirectivesForSymbol" -> getTypeReferenceDirectivesForSymbol,
"isLiteralConstDeclaration" -> isLiteralConstDeclaration,
"writeLiteralConstValue" -> writeLiteralConstValue )
def getTypeReferenceDirectivesForEntityName(node: EntityNameOrEntityNameExpression): Array[String] = {
 if ((!fileToDirective)) {
 return undefined

}
val meaning = (if ((((node.kind===SyntaxKind.PropertyAccessExpression))||(((node.kind===SyntaxKind.Identifier)&&isInTypeQuery( node ))))) (SymbolFlags.Value|SymbolFlags.ExportValue) else (SymbolFlags.Type|SymbolFlags.Namespace))
val symbol = resolveEntityName( node, meaning, true )
return (if ((symbol&&(symbol!==unknownSymbol))) getTypeReferenceDirectivesForSymbol( symbol, meaning ) else undefined)

}
def getTypeReferenceDirectivesForSymbol(symbol: Symbol, meaning: SymbolFlags): Array[String] = {
 if ((!fileToDirective)) {
 return undefined

}
if ((!isSymbolFromTypeDeclarationFile( symbol ))) {
 return undefined

}
var typeReferenceDirectives: Array[String] = zeroOfMyType
(symbol.declarations).foreach { fresh146 =>
val decl = zeroOfMyType
 = fresh146
 {
 if ((decl.symbol&&(decl.symbol.flags&meaning))) {
 val file = getSourceFileOfNode( decl )
val typeReferenceDirective = fileToDirective.get( file.path )
if (typeReferenceDirective) {
 ((typeReferenceDirectives||((typeReferenceDirectives=Array())))).push( typeReferenceDirective )

}

}

}
}
return typeReferenceDirectives

}
def isSymbolFromTypeDeclarationFile(symbol: Symbol): Boolean = {
 if ((!symbol.declarations)) {
 return false

}
var current = symbol
while (true) {
{
 val parent = getParentOfSymbol( current )
if (parent) {
 (current=parent)

}
else {
 break()

}

}
}
if (((current.valueDeclaration&&(current.valueDeclaration.kind===SyntaxKind.SourceFile))&&(current.flags&SymbolFlags.ValueModule))) {
 return false

}
(symbol.declarations).foreach { fresh147 =>
val decl = zeroOfMyType
 = fresh147
 {
 val file = getSourceFileOfNode( decl )
if (fileToDirective.contains( file.path )) {
 return true

}

}
}
return false

}

}
def getExternalModuleFileFromDeclaration(declaration: ( ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration )): SourceFile = {
 val specifier = getExternalModuleName( declaration )
val moduleSymbol = resolveExternalModuleNameWorker( specifier, specifier, undefined )
if ((!moduleSymbol)) {
 return undefined

}
return getDeclarationOfKind( moduleSymbol, SyntaxKind.SourceFile ).asInstanceOf[SourceFile]

}
def initializeTypeChecker() = {
 (host.getSourceFiles()).foreach { fresh148 =>
val file = zeroOfMyType
 = fresh148
 {
 bindSourceFile( file, compilerOptions )

}
}
var augmentations: Array[Array[LiteralExpression]] = zeroOfMyType
var requestedExternalEmitHelpers: NodeFlags = 0
var firstFileRequestingExternalHelpers: SourceFile = zeroOfMyType
(host.getSourceFiles()).foreach { fresh149 =>
val file = zeroOfMyType
 = fresh149
 {
 if ((!isExternalOrCommonJsModule( file ))) {
 mergeSymbolTable( globals, file.locals )

}
if ((file.patternAmbientModules&&file.patternAmbientModules.length)) {
 (patternAmbientModules=concatenate( patternAmbientModules, file.patternAmbientModules ))

}
if (file.moduleAugmentations.length) {
 ((augmentations||((augmentations=Array())))).push( file.moduleAugmentations )

}
if ((file.symbol&&file.symbol.globalExports)) {
 val source = file.symbol.globalExports
(source).keys.foreach { fresh150 =>
val id = zeroOfMyType
 = fresh150
 {
 if ((!((idinglobals)))) {
 (globals(id)=source(id))

}

}
}

}
if ((((compilerOptions.isolatedModules||isExternalModule( file )))&&(!file.isDeclarationFile))) {
 val fileRequestedExternalEmitHelpers = (file.flags&NodeFlags.EmitHelperFlags)
if (fileRequestedExternalEmitHelpers) {
 (requestedExternalEmitHelpers|=fileRequestedExternalEmitHelpers)
if ((firstFileRequestingExternalHelpers===undefined)) {
 (firstFileRequestingExternalHelpers=file)

}

}

}

}
}
if (augmentations) {
 (augmentations).foreach { fresh151 =>
val list = zeroOfMyType
 = fresh151
 {
 (list).foreach { fresh152 =>
val augmentation = zeroOfMyType
 = fresh152
 {
 mergeModuleAugmentation( augmentation )

}
}

}
}

}
addToSymbolTable( globals, builtinGlobals, Diagnostics.Declaration_name_conflicts_with_built_in_global_identifier_0 )
(getSymbolLinks( undefinedSymbol ).`type`=undefinedWideningType)
(getSymbolLinks( argumentsSymbol ).`type`=getGlobalType( "IArguments" ))
(getSymbolLinks( unknownSymbol ).`type`=unknownType)
(globalArrayType=getGlobalType( "Array", 1 ).asInstanceOf[GenericType])
(globalObjectType=getGlobalType( "Object" ))
(globalFunctionType=getGlobalType( "Function" ))
(globalStringType=getGlobalType( "String" ))
(globalNumberType=getGlobalType( "Number" ))
(globalBooleanType=getGlobalType( "Boolean" ))
(globalRegExpType=getGlobalType( "RegExp" ))
(jsxElementType=getExportedTypeFromNamespace( "JSX", JsxNames.Element ))
(getGlobalClassDecoratorType=memoize( (() =>  getGlobalType( "ClassDecorator" )) ))
(getGlobalPropertyDecoratorType=memoize( (() =>  getGlobalType( "PropertyDecorator" )) ))
(getGlobalMethodDecoratorType=memoize( (() =>  getGlobalType( "MethodDecorator" )) ))
(getGlobalParameterDecoratorType=memoize( (() =>  getGlobalType( "ParameterDecorator" )) ))
(getGlobalTypedPropertyDescriptorType=memoize( (() =>  getGlobalType( "TypedPropertyDescriptor", 1 )) ))
(getGlobalESSymbolConstructorSymbol=memoize( (() =>  getGlobalValueSymbol( "Symbol" )) ))
(getGlobalPromiseType=memoize( (() =>  getGlobalType( "Promise", 1 )) ))
(tryGetGlobalPromiseType=memoize( (() =>  (getGlobalSymbol( "Promise", SymbolFlags.Type, undefined )&&getGlobalPromiseType())) ))
(getGlobalPromiseLikeType=memoize( (() =>  getGlobalType( "PromiseLike", 1 )) ))
(getInstantiatedGlobalPromiseLikeType=memoize( createInstantiatedPromiseLikeType ))
(getGlobalPromiseConstructorSymbol=memoize( (() =>  getGlobalValueSymbol( "Promise" )) ))
(tryGetGlobalPromiseConstructorSymbol=memoize( (() =>  (getGlobalSymbol( "Promise", SymbolFlags.Value, undefined )&&getGlobalPromiseConstructorSymbol())) ))
(getGlobalPromiseConstructorLikeType=memoize( (() =>  getGlobalType( "PromiseConstructorLike" )) ))
(getGlobalThenableType=memoize( createThenableType ))
(getGlobalTemplateStringsArrayType=memoize( (() =>  getGlobalType( "TemplateStringsArray" )) ))
if ((languageVersion>=ScriptTarget.ES2015)) {
 (getGlobalESSymbolType=memoize( (() =>  getGlobalType( "Symbol" )) ))
(getGlobalIterableType=memoize( (() =>  getGlobalType( "Iterable", 1 ).asInstanceOf[GenericType]) ))
(getGlobalIteratorType=memoize( (() =>  getGlobalType( "Iterator", 1 ).asInstanceOf[GenericType]) ))
(getGlobalIterableIteratorType=memoize( (() =>  getGlobalType( "IterableIterator", 1 ).asInstanceOf[GenericType]) ))

}
else {
 (getGlobalESSymbolType=memoize( (() =>  emptyObjectType) ))
(getGlobalIterableType=memoize( (() =>  emptyGenericType) ))
(getGlobalIteratorType=memoize( (() =>  emptyGenericType) ))
(getGlobalIterableIteratorType=memoize( (() =>  emptyGenericType) ))

}
(anyArrayType=createArrayType( anyType ))
(autoArrayType=createArrayType( autoType ))
val symbol = getGlobalSymbol( "ReadonlyArray", SymbolFlags.Type, undefined )
(globalReadonlyArrayType=(symbol&&getTypeOfGlobalSymbol( symbol, 1 ).asInstanceOf[GenericType]))
(anyReadonlyArrayType=(if (globalReadonlyArrayType) createTypeFromGenericGlobalType( globalReadonlyArrayType, Array( anyType ) ) else anyArrayType))
if ((compilerOptions.importHelpers&&firstFileRequestingExternalHelpers)) {
 val helpersModule = resolveExternalModule( firstFileRequestingExternalHelpers, externalHelpersModuleNameText, Diagnostics.Cannot_find_module_0, undefined )
if (helpersModule) {
 val exports = helpersModule.exports
if (((requestedExternalEmitHelpers&NodeFlags.HasClassExtends)&&(languageVersion<ScriptTarget.ES2015))) {
 verifyHelperSymbol( exports, "__extends", SymbolFlags.Value )

}
if (((requestedExternalEmitHelpers&NodeFlags.HasJsxSpreadAttributes)&&(compilerOptions.jsx!==JsxEmit.Preserve))) {
 verifyHelperSymbol( exports, "__assign", SymbolFlags.Value )

}
if ((requestedExternalEmitHelpers&NodeFlags.HasDecorators)) {
 verifyHelperSymbol( exports, "__decorate", SymbolFlags.Value )
if (compilerOptions.emitDecoratorMetadata) {
 verifyHelperSymbol( exports, "__metadata", SymbolFlags.Value )

}

}
if ((requestedExternalEmitHelpers&NodeFlags.HasParamDecorators)) {
 verifyHelperSymbol( exports, "__param", SymbolFlags.Value )

}
if ((requestedExternalEmitHelpers&NodeFlags.HasAsyncFunctions)) {
 verifyHelperSymbol( exports, "__awaiter", SymbolFlags.Value )
if ((languageVersion<ScriptTarget.ES2015)) {
 verifyHelperSymbol( exports, "__generator", SymbolFlags.Value )

}

}

}

}

}
def verifyHelperSymbol(symbols: SymbolTable, name: String, meaning: SymbolFlags) = {
 val symbol = getSymbol( symbols, escapeIdentifier( name ), meaning )
if ((!symbol)) {
 error( undefined, Diagnostics.Module_0_has_no_exported_member_1, externalHelpersModuleNameText, name )

}

}
def createInstantiatedPromiseLikeType(): ObjectType = {
 val promiseLikeType = getGlobalPromiseLikeType()
if ((promiseLikeType!==emptyGenericType)) {
 return createTypeReference( promiseLikeType.asInstanceOf[GenericType], Array( anyType ) )

}
return emptyObjectType

}
def createThenableType() = {
 val thenPropertySymbol = createSymbol( (SymbolFlags.Transient|SymbolFlags.Property), "then" )
(getSymbolLinks( thenPropertySymbol ).`type`=globalFunctionType)
val thenableType = createObjectType( ObjectFlags.Anonymous ).asInstanceOf[ResolvedType]
(thenableType.properties=Array( thenPropertySymbol ))
(thenableType.members=createSymbolTable( thenableType.properties ))
(thenableType.callSignatures=Array())
(thenableType.constructSignatures=Array())
return thenableType

}
def checkGrammarDecorators(node: Node): Boolean = {
 if ((!node.decorators)) {
 return false

}
if ((!nodeCanBeDecorated( node ))) {
 if (((node.kind===SyntaxKind.MethodDeclaration)&&(!ts.nodeIsPresent( (node.asInstanceOf[MethodDeclaration]).body )))) {
 return grammarErrorOnFirstToken( node, Diagnostics.A_decorator_can_only_decorate_a_method_implementation_not_an_overload )

}
else {
 return grammarErrorOnFirstToken( node, Diagnostics.Decorators_are_not_valid_here )

}

}
else if (((node.kind===SyntaxKind.GetAccessor)||(node.kind===SyntaxKind.SetAccessor))) {
 val accessors = getAllAccessorDeclarations( (node.parent.asInstanceOf[ClassDeclaration]).members, node.asInstanceOf[AccessorDeclaration] )
if ((accessors.firstAccessor.decorators&&(node===accessors.secondAccessor))) {
 return grammarErrorOnFirstToken( node, Diagnostics.Decorators_cannot_be_applied_to_multiple_get_Slashset_accessors_of_the_same_name )

}

}
return false

}
def checkGrammarModifiers(node: Node): Boolean = {
 val quickResult = reportObviousModifierErrors( node )
if ((quickResult!==undefined)) {
 return quickResult

}
var lastStatic: Node = zeroOfMyType
var lastPrivate: Node = zeroOfMyType
var lastProtected: Node = zeroOfMyType
var lastDeclare: Node = zeroOfMyType
var lastAsync: Node = zeroOfMyType
var lastReadonly: Node = zeroOfMyType
var flags = ModifierFlags.None
(node.modifiers).foreach { fresh153 =>
val modifier = zeroOfMyType
 = fresh153
 {
 if ((modifier.kind!==SyntaxKind.ReadonlyKeyword)) {
 if (((node.kind===SyntaxKind.PropertySignature)||(node.kind===SyntaxKind.MethodSignature))) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_type_member, tokenToString( modifier.kind ) )

}
if ((node.kind===SyntaxKind.IndexSignature)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_an_index_signature, tokenToString( modifier.kind ) )

}

}
modifier.kind match {
  case  SyntaxKind.ConstKeyword  =>
if (((node.kind!==SyntaxKind.EnumDeclaration)&&(node.parent.kind===SyntaxKind.ClassDeclaration))) {
 return grammarErrorOnNode( node, Diagnostics.A_class_member_cannot_have_the_0_keyword, tokenToString( SyntaxKind.ConstKeyword ) )

}
  case  SyntaxKind.PublicKeyword | SyntaxKind.ProtectedKeyword | SyntaxKind.PrivateKeyword  =>
var text = visibilityToString( modifierToFlag( modifier.kind ) )
if ((modifier.kind===SyntaxKind.ProtectedKeyword)) {
 (lastProtected=modifier)

}
else if ((modifier.kind===SyntaxKind.PrivateKeyword)) {
 (lastPrivate=modifier)

}
if ((flags&ModifierFlags.AccessibilityModifier)) {
 return grammarErrorOnNode( modifier, Diagnostics.Accessibility_modifier_already_seen )

}
else if ((flags&ModifierFlags.Static)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, text, "static" )

}
else if ((flags&ModifierFlags.Readonly)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, text, "readonly" )

}
else if ((flags&ModifierFlags.Async)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, text, "async" )

}
else if (((node.parent.kind===SyntaxKind.ModuleBlock)||(node.parent.kind===SyntaxKind.SourceFile))) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_module_or_namespace_element, text )

}
else if ((flags&ModifierFlags.Abstract)) {
 if ((modifier.kind===SyntaxKind.PrivateKeyword)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_be_used_with_1_modifier, text, "abstract" )

}
else {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, text, "abstract" )

}

}
(flags|=modifierToFlag( modifier.kind ))
  case  SyntaxKind.StaticKeyword  =>
if ((flags&ModifierFlags.Static)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_already_seen, "static" )

}
else if ((flags&ModifierFlags.Readonly)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, "static", "readonly" )

}
else if ((flags&ModifierFlags.Async)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, "static", "async" )

}
else if (((node.parent.kind===SyntaxKind.ModuleBlock)||(node.parent.kind===SyntaxKind.SourceFile))) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_module_or_namespace_element, "static" )

}
else if ((node.kind===SyntaxKind.Parameter)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_parameter, "static" )

}
else if ((flags&ModifierFlags.Abstract)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_be_used_with_1_modifier, "static", "abstract" )

}
(flags|=ModifierFlags.Static)
(lastStatic=modifier)
  case  SyntaxKind.ReadonlyKeyword  =>
if ((flags&ModifierFlags.Readonly)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_already_seen, "readonly" )

}
else if (((((node.kind!==SyntaxKind.PropertyDeclaration)&&(node.kind!==SyntaxKind.PropertySignature))&&(node.kind!==SyntaxKind.IndexSignature))&&(node.kind!==SyntaxKind.Parameter))) {
 return grammarErrorOnNode( modifier, Diagnostics.readonly_modifier_can_only_appear_on_a_property_declaration_or_index_signature )

}
(flags|=ModifierFlags.Readonly)
(lastReadonly=modifier)
  case  SyntaxKind.ExportKeyword  =>
if ((flags&ModifierFlags.Export)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_already_seen, "export" )

}
else if ((flags&ModifierFlags.Ambient)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, "export", "declare" )

}
else if ((flags&ModifierFlags.Abstract)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, "export", "abstract" )

}
else if ((flags&ModifierFlags.Async)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_must_precede_1_modifier, "export", "async" )

}
else if ((node.parent.kind===SyntaxKind.ClassDeclaration)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_class_element, "export" )

}
else if ((node.kind===SyntaxKind.Parameter)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_parameter, "export" )

}
(flags|=ModifierFlags.Export)
  case  SyntaxKind.DeclareKeyword  =>
if ((flags&ModifierFlags.Ambient)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_already_seen, "declare" )

}
else if ((flags&ModifierFlags.Async)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_be_used_in_an_ambient_context, "async" )

}
else if ((node.parent.kind===SyntaxKind.ClassDeclaration)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_class_element, "declare" )

}
else if ((node.kind===SyntaxKind.Parameter)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_parameter, "declare" )

}
else if ((isInAmbientContext( node.parent )&&(node.parent.kind===SyntaxKind.ModuleBlock))) {
 return grammarErrorOnNode( modifier, Diagnostics.A_declare_modifier_cannot_be_used_in_an_already_ambient_context )

}
(flags|=ModifierFlags.Ambient)
(lastDeclare=modifier)
  case  SyntaxKind.AbstractKeyword  =>
if ((flags&ModifierFlags.Abstract)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_already_seen, "abstract" )

}
if ((node.kind!==SyntaxKind.ClassDeclaration)) {
 if (((((node.kind!==SyntaxKind.MethodDeclaration)&&(node.kind!==SyntaxKind.PropertyDeclaration))&&(node.kind!==SyntaxKind.GetAccessor))&&(node.kind!==SyntaxKind.SetAccessor))) {
 return grammarErrorOnNode( modifier, Diagnostics.abstract_modifier_can_only_appear_on_a_class_method_or_property_declaration )

}
if ((!(((node.parent.kind===SyntaxKind.ClassDeclaration)&&(getModifierFlags( node.parent )&ModifierFlags.Abstract))))) {
 return grammarErrorOnNode( modifier, Diagnostics.Abstract_methods_can_only_appear_within_an_abstract_class )

}
if ((flags&ModifierFlags.Static)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_be_used_with_1_modifier, "static", "abstract" )

}
if ((flags&ModifierFlags.Private)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_be_used_with_1_modifier, "private", "abstract" )

}

}
(flags|=ModifierFlags.Abstract)
  case  SyntaxKind.AsyncKeyword  =>
if ((flags&ModifierFlags.Async)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_already_seen, "async" )

}
else if (((flags&ModifierFlags.Ambient)||isInAmbientContext( node.parent ))) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_be_used_in_an_ambient_context, "async" )

}
else if ((node.kind===SyntaxKind.Parameter)) {
 return grammarErrorOnNode( modifier, Diagnostics._0_modifier_cannot_appear_on_a_parameter, "async" )

}
(flags|=ModifierFlags.Async)
(lastAsync=modifier)
  case _ =>
}

}
}
if ((node.kind===SyntaxKind.Constructor)) {
 if ((flags&ModifierFlags.Static)) {
 return grammarErrorOnNode( lastStatic, Diagnostics._0_modifier_cannot_appear_on_a_constructor_declaration, "static" )

}
if ((flags&ModifierFlags.Abstract)) {
 return grammarErrorOnNode( lastStatic, Diagnostics._0_modifier_cannot_appear_on_a_constructor_declaration, "abstract" )

}
else if ((flags&ModifierFlags.Async)) {
 return grammarErrorOnNode( lastAsync, Diagnostics._0_modifier_cannot_appear_on_a_constructor_declaration, "async" )

}
else if ((flags&ModifierFlags.Readonly)) {
 return grammarErrorOnNode( lastReadonly, Diagnostics._0_modifier_cannot_appear_on_a_constructor_declaration, "readonly" )

}
return

}
else if (((((node.kind===SyntaxKind.ImportDeclaration)||(node.kind===SyntaxKind.ImportEqualsDeclaration)))&&(flags&ModifierFlags.Ambient))) {
 return grammarErrorOnNode( lastDeclare, Diagnostics.A_0_modifier_cannot_be_used_with_an_import_declaration, "declare" )

}
else if ((((node.kind===SyntaxKind.Parameter)&&((flags&ModifierFlags.ParameterPropertyModifier)))&&isBindingPattern( (node.asInstanceOf[ParameterDeclaration]).name ))) {
 return grammarErrorOnNode( node, Diagnostics.A_parameter_property_may_not_be_declared_using_a_binding_pattern )

}
else if ((((node.kind===SyntaxKind.Parameter)&&((flags&ModifierFlags.ParameterPropertyModifier)))&&(node.asInstanceOf[ParameterDeclaration]).dotDotDotToken)) {
 return grammarErrorOnNode( node, Diagnostics.A_parameter_property_cannot_be_declared_using_a_rest_parameter )

}
if ((flags&ModifierFlags.Async)) {
 return checkGrammarAsyncModifier( node, lastAsync )

}

}
def reportObviousModifierErrors(node: Node): ( Boolean | undefined ) = {
 return (if ((!node.modifiers)) false else (if (shouldReportBadModifier( node )) grammarErrorOnFirstToken( node, Diagnostics.Modifiers_cannot_appear_here ) else undefined))

}
def shouldReportBadModifier(node: Node): Boolean = {
 node.kind match {
  case  SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.Constructor | SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.IndexSignature | SyntaxKind.ModuleDeclaration | SyntaxKind.ImportDeclaration | SyntaxKind.ImportEqualsDeclaration | SyntaxKind.ExportDeclaration | SyntaxKind.ExportAssignment | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction | SyntaxKind.Parameter  =>
return false
  case _ =>
if (((node.parent.kind===SyntaxKind.ModuleBlock)||(node.parent.kind===SyntaxKind.SourceFile))) {
 return false

}
node.kind match {
  case  SyntaxKind.FunctionDeclaration  =>
return nodeHasAnyModifiersExcept( node, SyntaxKind.AsyncKeyword )
  case  SyntaxKind.ClassDeclaration  =>
return nodeHasAnyModifiersExcept( node, SyntaxKind.AbstractKeyword )
  case  SyntaxKind.InterfaceDeclaration | SyntaxKind.VariableStatement | SyntaxKind.TypeAliasDeclaration  =>
return true
  case  SyntaxKind.EnumDeclaration  =>
return nodeHasAnyModifiersExcept( node, SyntaxKind.ConstKeyword )
  case _ =>
Debug.fail()
return false
}
}

}
def nodeHasAnyModifiersExcept(node: Node, allowedModifier: SyntaxKind): Boolean = {
 return ((node.modifiers.length>1)||(node.modifiers(0).kind!==allowedModifier))

}
def checkGrammarAsyncModifier(node: Node, asyncModifier: Node): Boolean = {
 node.kind match {
  case  SyntaxKind.MethodDeclaration | SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction  =>
if ((!(node.asInstanceOf[FunctionLikeDeclaration]).asteriskToken)) {
 return false

}
  case _ =>
}
return grammarErrorOnNode( asyncModifier, Diagnostics._0_modifier_cannot_be_used_here, "async" )

}
def checkGrammarForDisallowedTrailingComma(list: NodeArray[Node]): Boolean = {
 if ((list&&list.hasTrailingComma)) {
 val start = (list.end-",".length)
val end = list.end
val sourceFile = getSourceFileOfNode( list(0) )
return grammarErrorAtPos( sourceFile, start, (end-start), Diagnostics.Trailing_comma_not_allowed )

}

}
def checkGrammarTypeParameterList(typeParameters: NodeArray[TypeParameterDeclaration], file: SourceFile): Boolean = {
 if (checkGrammarForDisallowedTrailingComma( typeParameters )) {
 return true

}
if ((typeParameters&&(typeParameters.length===0))) {
 val start = (typeParameters.pos-"<".length)
val end = (skipTrivia( file.text, typeParameters.end )+">".length)
return grammarErrorAtPos( file, start, (end-start), Diagnostics.Type_parameter_list_cannot_be_empty )

}

}
def checkGrammarParameterList(parameters: NodeArray[ParameterDeclaration]) = {
 var seenOptionalParameter = false
val parameterCount = parameters.length
{
var i = 0
while( (i<parameterCount)) {
 {
 val parameter = parameters(i)
if (parameter.dotDotDotToken) {
 if ((i!==((parameterCount-1)))) {
 return grammarErrorOnNode( parameter.dotDotDotToken, Diagnostics.A_rest_parameter_must_be_last_in_a_parameter_list )

}
if (isBindingPattern( parameter.name )) {
 return grammarErrorOnNode( parameter.name, Diagnostics.A_rest_element_cannot_contain_a_binding_pattern )

}
if (parameter.questionToken) {
 return grammarErrorOnNode( parameter.questionToken, Diagnostics.A_rest_parameter_cannot_be_optional )

}
if (parameter.initializer) {
 return grammarErrorOnNode( parameter.name, Diagnostics.A_rest_parameter_cannot_have_an_initializer )

}

}
else if (parameter.questionToken) {
 (seenOptionalParameter=true)
if (parameter.initializer) {
 return grammarErrorOnNode( parameter.name, Diagnostics.Parameter_cannot_have_question_mark_and_initializer )

}

}
else if ((seenOptionalParameter&&(!parameter.initializer))) {
 return grammarErrorOnNode( parameter.name, Diagnostics.A_required_parameter_cannot_follow_an_optional_parameter )

}

}
 (i+= 1)
}
}

}
def checkGrammarFunctionLikeDeclaration(node: FunctionLikeDeclaration): Boolean = {
 val file = getSourceFileOfNode( node )
return ((((checkGrammarDecorators( node )||checkGrammarModifiers( node ))||checkGrammarTypeParameterList( node.typeParameters, file ))||checkGrammarParameterList( node.parameters ))||checkGrammarArrowFunction( node, file ))

}
def checkGrammarArrowFunction(node: FunctionLikeDeclaration, file: SourceFile): Boolean = {
 if ((node.kind===SyntaxKind.ArrowFunction)) {
 val arrowFunction = node.asInstanceOf[ArrowFunction]
val startLine = getLineAndCharacterOfPosition( file, arrowFunction.equalsGreaterThanToken.pos ).line
val endLine = getLineAndCharacterOfPosition( file, arrowFunction.equalsGreaterThanToken.end ).line
if ((startLine!==endLine)) {
 return grammarErrorOnNode( arrowFunction.equalsGreaterThanToken, Diagnostics.Line_terminator_not_permitted_before_arrow )

}

}
return false

}
def checkGrammarIndexSignatureParameters(node: SignatureDeclaration): Boolean = {
 val parameter = node.parameters(0)
if ((node.parameters.length!==1)) {
 if (parameter) {
 return grammarErrorOnNode( parameter.name, Diagnostics.An_index_signature_must_have_exactly_one_parameter )

}
else {
 return grammarErrorOnNode( node, Diagnostics.An_index_signature_must_have_exactly_one_parameter )

}

}
if (parameter.dotDotDotToken) {
 return grammarErrorOnNode( parameter.dotDotDotToken, Diagnostics.An_index_signature_cannot_have_a_rest_parameter )

}
if ((getModifierFlags( parameter )!==0)) {
 return grammarErrorOnNode( parameter.name, Diagnostics.An_index_signature_parameter_cannot_have_an_accessibility_modifier )

}
if (parameter.questionToken) {
 return grammarErrorOnNode( parameter.questionToken, Diagnostics.An_index_signature_parameter_cannot_have_a_question_mark )

}
if (parameter.initializer) {
 return grammarErrorOnNode( parameter.name, Diagnostics.An_index_signature_parameter_cannot_have_an_initializer )

}
if ((!parameter.`type`)) {
 return grammarErrorOnNode( parameter.name, Diagnostics.An_index_signature_parameter_must_have_a_type_annotation )

}
if (((parameter.`type`.kind!==SyntaxKind.StringKeyword)&&(parameter.`type`.kind!==SyntaxKind.NumberKeyword))) {
 return grammarErrorOnNode( parameter.name, Diagnostics.An_index_signature_parameter_type_must_be_string_or_number )

}
if ((!node.`type`)) {
 return grammarErrorOnNode( node, Diagnostics.An_index_signature_must_have_a_type_annotation )

}

}
def checkGrammarIndexSignature(node: SignatureDeclaration) = {
 return ((checkGrammarDecorators( node )||checkGrammarModifiers( node ))||checkGrammarIndexSignatureParameters( node ))

}
def checkGrammarForAtLeastOneTypeArgument(node: Node, typeArguments: NodeArray[TypeNode]): Boolean = {
 if ((typeArguments&&(typeArguments.length===0))) {
 val sourceFile = getSourceFileOfNode( node )
val start = (typeArguments.pos-"<".length)
val end = (skipTrivia( sourceFile.text, typeArguments.end )+">".length)
return grammarErrorAtPos( sourceFile, start, (end-start), Diagnostics.Type_argument_list_cannot_be_empty )

}

}
def checkGrammarTypeArguments(node: Node, typeArguments: NodeArray[TypeNode]): Boolean = {
 return (checkGrammarForDisallowedTrailingComma( typeArguments )||checkGrammarForAtLeastOneTypeArgument( node, typeArguments ))

}
def checkGrammarForOmittedArgument(node: ( CallExpression | NewExpression ), args: NodeArray[Expression]): Boolean = {
 if (args) {
 val sourceFile = getSourceFileOfNode( node )
(args).foreach { fresh154 =>
val arg = zeroOfMyType
 = fresh154
 {
 if ((arg.kind===SyntaxKind.OmittedExpression)) {
 return grammarErrorAtPos( sourceFile, arg.pos, 0, Diagnostics.Argument_expression_expected )

}

}
}

}

}
def checkGrammarArguments(node: ( CallExpression | NewExpression ), args: NodeArray[Expression]): Boolean = {
 return checkGrammarForOmittedArgument( node, args )

}
def checkGrammarHeritageClause(node: HeritageClause): Boolean = {
 val types = node.types
if (checkGrammarForDisallowedTrailingComma( types )) {
 return true

}
if ((types&&(types.length===0))) {
 val listType = tokenToString( node.token )
val sourceFile = getSourceFileOfNode( node )
return grammarErrorAtPos( sourceFile, types.pos, 0, Diagnostics._0_list_cannot_be_empty, listType )

}

}
def checkGrammarClassDeclarationHeritageClauses(node: ClassLikeDeclaration) = {
 var seenExtendsClause = false
var seenImplementsClause = false
if ((((!checkGrammarDecorators( node ))&&(!checkGrammarModifiers( node )))&&node.heritageClauses)) {
 (node.heritageClauses).foreach { fresh155 =>
val heritageClause = zeroOfMyType
 = fresh155
 {
 if ((heritageClause.token===SyntaxKind.ExtendsKeyword)) {
 if (seenExtendsClause) {
 return grammarErrorOnFirstToken( heritageClause, Diagnostics.extends_clause_already_seen )

}
if (seenImplementsClause) {
 return grammarErrorOnFirstToken( heritageClause, Diagnostics.extends_clause_must_precede_implements_clause )

}
if ((heritageClause.types.length>1)) {
 return grammarErrorOnFirstToken( heritageClause.types(1), Diagnostics.Classes_can_only_extend_a_single_class )

}
(seenExtendsClause=true)

}
else {
 Debug.assert( (heritageClause.token===SyntaxKind.ImplementsKeyword) )
if (seenImplementsClause) {
 return grammarErrorOnFirstToken( heritageClause, Diagnostics.implements_clause_already_seen )

}
(seenImplementsClause=true)

}
checkGrammarHeritageClause( heritageClause )

}
}

}

}
def checkGrammarInterfaceDeclaration(node: InterfaceDeclaration) = {
 var seenExtendsClause = false
if (node.heritageClauses) {
 (node.heritageClauses).foreach { fresh156 =>
val heritageClause = zeroOfMyType
 = fresh156
 {
 if ((heritageClause.token===SyntaxKind.ExtendsKeyword)) {
 if (seenExtendsClause) {
 return grammarErrorOnFirstToken( heritageClause, Diagnostics.extends_clause_already_seen )

}
(seenExtendsClause=true)

}
else {
 Debug.assert( (heritageClause.token===SyntaxKind.ImplementsKeyword) )
return grammarErrorOnFirstToken( heritageClause, Diagnostics.Interface_declaration_cannot_have_implements_clause )

}
checkGrammarHeritageClause( heritageClause )

}
}

}
return false

}
def checkGrammarComputedPropertyName(node: Node): Boolean = {
 if ((node.kind!==SyntaxKind.ComputedPropertyName)) {
 return false

}
val computedPropertyName = node.asInstanceOf[ComputedPropertyName]
if (((computedPropertyName.expression.kind===SyntaxKind.BinaryExpression)&&((computedPropertyName.expression.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.CommaToken))) {
 return grammarErrorOnNode( computedPropertyName.expression, Diagnostics.A_comma_expression_is_not_allowed_in_a_computed_property_name )

}

}
def checkGrammarForGenerator(node: FunctionLikeDeclaration) = {
 if (node.asteriskToken) {
 Debug.assert( (((node.kind===SyntaxKind.FunctionDeclaration)||(node.kind===SyntaxKind.FunctionExpression))||(node.kind===SyntaxKind.MethodDeclaration)) )
if (isInAmbientContext( node )) {
 return grammarErrorOnNode( node.asteriskToken, Diagnostics.Generators_are_not_allowed_in_an_ambient_context )

}
if ((!node.body)) {
 return grammarErrorOnNode( node.asteriskToken, Diagnostics.An_overload_signature_cannot_be_declared_as_a_generator )

}
if ((languageVersion<ScriptTarget.ES2015)) {
 return grammarErrorOnNode( node.asteriskToken, Diagnostics.Generators_are_only_available_when_targeting_ECMAScript_2015_or_higher )

}

}

}
def checkGrammarForInvalidQuestionMark(questionToken: Node, message: DiagnosticMessage): Boolean = {
 if (questionToken) {
 return grammarErrorOnNode( questionToken, message )

}

}
def checkGrammarObjectLiteralExpression(node: ObjectLiteralExpression, inDestructuring: Boolean) = {
 val seen = createMap[ SymbolFlags ]()
val Property = 1
val GetAccessor = 2
val SetAccessor = 4
val GetOrSetAccessor = (GetAccessor|SetAccessor)
(node.properties).foreach { fresh157 =>
val prop = zeroOfMyType
 = fresh157
 {
 val name = prop.name
if ((name.kind===SyntaxKind.ComputedPropertyName)) {
 checkGrammarComputedPropertyName( name.asInstanceOf[ComputedPropertyName] )

}
if ((((prop.kind===SyntaxKind.ShorthandPropertyAssignment)&&(!inDestructuring))&&(prop.asInstanceOf[ShorthandPropertyAssignment]).objectAssignmentInitializer)) {
 return grammarErrorOnNode( (prop.asInstanceOf[ShorthandPropertyAssignment]).equalsToken, Diagnostics.can_only_be_used_in_an_object_literal_property_inside_a_destructuring_assignment )

}
if (prop.modifiers) {
 (prop.modifiers).foreach { fresh158 =>
val mod = zeroOfMyType
 = fresh158
 {
 if (((mod.kind!==SyntaxKind.AsyncKeyword)||(prop.kind!==SyntaxKind.MethodDeclaration))) {
 grammarErrorOnNode( mod, Diagnostics._0_modifier_cannot_be_used_here, getTextOfNode( mod ) )

}

}
}

}
var currentKind: Int = zeroOfMyType
if (((prop.kind===SyntaxKind.PropertyAssignment)||(prop.kind===SyntaxKind.ShorthandPropertyAssignment))) {
 checkGrammarForInvalidQuestionMark( (prop.asInstanceOf[PropertyAssignment]).questionToken, Diagnostics.An_object_member_cannot_be_declared_optional )
if ((name.kind===SyntaxKind.NumericLiteral)) {
 checkGrammarNumericLiteral( name.asInstanceOf[NumericLiteral] )

}
(currentKind=Property)

}
else if ((prop.kind===SyntaxKind.MethodDeclaration)) {
 (currentKind=Property)

}
else if ((prop.kind===SyntaxKind.GetAccessor)) {
 (currentKind=GetAccessor)

}
else if ((prop.kind===SyntaxKind.SetAccessor)) {
 (currentKind=SetAccessor)

}
else {
 Debug.fail( ("Unexpected syntax kind:"+(prop.asInstanceOf[Node]).kind) )

}
val effectiveName = getPropertyNameForPropertyNameNode( name )
if ((effectiveName===undefined)) {
 continue

}
if ((!seen(effectiveName))) {
 (seen(effectiveName)=currentKind)

}
else {
 val existingKind = seen(effectiveName)
if (((currentKind===Property)&&(existingKind===Property))) {
 grammarErrorOnNode( name, Diagnostics.Duplicate_identifier_0, getTextOfNode( name ) )

}
else if ((((currentKind&GetOrSetAccessor))&&((existingKind&GetOrSetAccessor)))) {
 if (((existingKind!==GetOrSetAccessor)&&(currentKind!==existingKind))) {
 (seen(effectiveName)=(currentKind|existingKind))

}
else {
 return grammarErrorOnNode( name, Diagnostics.An_object_literal_cannot_have_multiple_get_Slashset_accessors_with_the_same_name )

}

}
else {
 return grammarErrorOnNode( name, Diagnostics.An_object_literal_cannot_have_property_and_accessor_with_the_same_name )

}

}

}
}

}
def checkGrammarJsxElement(node: JsxOpeningLikeElement) = {
 val seen = createMap[ Boolean ]()
(node.attributes).foreach { fresh159 =>
val attr = zeroOfMyType
 = fresh159
 {
 if ((attr.kind===SyntaxKind.JsxSpreadAttribute)) {
 continue

}
val jsxAttr = (attr.asInstanceOf[JsxAttribute])
val name = jsxAttr.name
if ((!seen(name.text))) {
 (seen(name.text)=true)

}
else {
 return grammarErrorOnNode( name, Diagnostics.JSX_elements_cannot_have_multiple_attributes_with_the_same_name )

}
val initializer = jsxAttr.initializer
if (((initializer&&(initializer.kind===SyntaxKind.JsxExpression))&&(!(initializer.asInstanceOf[JsxExpression]).expression))) {
 return grammarErrorOnNode( jsxAttr.initializer, Diagnostics.JSX_attributes_must_only_be_assigned_a_non_empty_expression )

}

}
}

}
def checkGrammarForInOrForOfStatement(forInOrOfStatement: ( ForInStatement | ForOfStatement )): Boolean = {
 if (checkGrammarStatementInAmbientContext( forInOrOfStatement )) {
 return true

}
if ((forInOrOfStatement.initializer.kind===SyntaxKind.VariableDeclarationList)) {
 val variableList = forInOrOfStatement.initializer.asInstanceOf[VariableDeclarationList]
if ((!checkGrammarVariableDeclarationList( variableList ))) {
 val declarations = variableList.declarations
if ((!declarations.length)) {
 return false

}
if ((declarations.length>1)) {
 val diagnostic = (if ((forInOrOfStatement.kind===SyntaxKind.ForInStatement)) Diagnostics.Only_a_single_variable_declaration_is_allowed_in_a_for_in_statement else Diagnostics.Only_a_single_variable_declaration_is_allowed_in_a_for_of_statement)
return grammarErrorOnFirstToken( variableList.declarations(1), diagnostic )

}
val firstDeclaration = declarations(0)
if (firstDeclaration.initializer) {
 val diagnostic = (if ((forInOrOfStatement.kind===SyntaxKind.ForInStatement)) Diagnostics.The_variable_declaration_of_a_for_in_statement_cannot_have_an_initializer else Diagnostics.The_variable_declaration_of_a_for_of_statement_cannot_have_an_initializer)
return grammarErrorOnNode( firstDeclaration.name, diagnostic )

}
if (firstDeclaration.`type`) {
 val diagnostic = (if ((forInOrOfStatement.kind===SyntaxKind.ForInStatement)) Diagnostics.The_left_hand_side_of_a_for_in_statement_cannot_use_a_type_annotation else Diagnostics.The_left_hand_side_of_a_for_of_statement_cannot_use_a_type_annotation)
return grammarErrorOnNode( firstDeclaration, diagnostic )

}

}

}
return false

}
def checkGrammarAccessor(accessor: AccessorDeclaration): Boolean = {
 val kind = accessor.kind
if ((languageVersion<ScriptTarget.ES5)) {
 return grammarErrorOnNode( accessor.name, Diagnostics.Accessors_are_only_available_when_targeting_ECMAScript_5_and_higher )

}
else if (isInAmbientContext( accessor )) {
 return grammarErrorOnNode( accessor.name, Diagnostics.An_accessor_cannot_be_declared_in_an_ambient_context )

}
else if (((accessor.body===undefined)&&(!((getModifierFlags( accessor )&ModifierFlags.Abstract))))) {
 return grammarErrorAtPos( getSourceFileOfNode( accessor ), (accessor.end-1), ";".length, Diagnostics._0_expected, "{" )

}
else if (accessor.typeParameters) {
 return grammarErrorOnNode( accessor.name, Diagnostics.An_accessor_cannot_have_type_parameters )

}
else if ((!doesAccessorHaveCorrectParameterCount( accessor ))) {
 return grammarErrorOnNode( accessor.name, (if ((kind===SyntaxKind.GetAccessor)) Diagnostics.A_get_accessor_cannot_have_parameters else Diagnostics.A_set_accessor_must_have_exactly_one_parameter) )

}
else if ((kind===SyntaxKind.SetAccessor)) {
 if (accessor.`type`) {
 return grammarErrorOnNode( accessor.name, Diagnostics.A_set_accessor_cannot_have_a_return_type_annotation )

}
else {
 val parameter = accessor.parameters(0)
if (parameter.dotDotDotToken) {
 return grammarErrorOnNode( parameter.dotDotDotToken, Diagnostics.A_set_accessor_cannot_have_rest_parameter )

}
else if (parameter.questionToken) {
 return grammarErrorOnNode( parameter.questionToken, Diagnostics.A_set_accessor_cannot_have_an_optional_parameter )

}
else if (parameter.initializer) {
 return grammarErrorOnNode( accessor.name, Diagnostics.A_set_accessor_parameter_cannot_have_an_initializer )

}

}

}

}
def doesAccessorHaveCorrectParameterCount(accessor: AccessorDeclaration) = {
 return (getAccessorThisParameter( accessor )||(accessor.parameters.length===((if ((accessor.kind===SyntaxKind.GetAccessor)) 0 else 1))))

}
def getAccessorThisParameter(accessor: AccessorDeclaration): ParameterDeclaration = {
 if ((accessor.parameters.length===((if ((accessor.kind===SyntaxKind.GetAccessor)) 1 else 2)))) {
 return getThisParameter( accessor )

}

}
def checkGrammarForNonSymbolComputedProperty(node: DeclarationName, message: DiagnosticMessage) = {
 if (isDynamicName( node )) {
 return grammarErrorOnNode( node, message )

}

}
def checkGrammarMethod(node: MethodDeclaration) = {
 if (((checkGrammarDisallowedModifiersOnObjectLiteralExpressionMethod( node )||checkGrammarFunctionLikeDeclaration( node ))||checkGrammarForGenerator( node ))) {
 return true

}
if ((node.parent.kind===SyntaxKind.ObjectLiteralExpression)) {
 if (checkGrammarForInvalidQuestionMark( node.questionToken, Diagnostics.An_object_member_cannot_be_declared_optional )) {
 return true

}
else if ((node.body===undefined)) {
 return grammarErrorAtPos( getSourceFileOfNode( node ), (node.end-1), ";".length, Diagnostics._0_expected, "{" )

}

}
if (isClassLike( node.parent )) {
 if (isInAmbientContext( node )) {
 return checkGrammarForNonSymbolComputedProperty( node.name, Diagnostics.A_computed_property_name_in_an_ambient_context_must_directly_refer_to_a_built_in_symbol )

}
else if ((!node.body)) {
 return checkGrammarForNonSymbolComputedProperty( node.name, Diagnostics.A_computed_property_name_in_a_method_overload_must_directly_refer_to_a_built_in_symbol )

}

}
else if ((node.parent.kind===SyntaxKind.InterfaceDeclaration)) {
 return checkGrammarForNonSymbolComputedProperty( node.name, Diagnostics.A_computed_property_name_in_an_interface_must_directly_refer_to_a_built_in_symbol )

}
else if ((node.parent.kind===SyntaxKind.TypeLiteral)) {
 return checkGrammarForNonSymbolComputedProperty( node.name, Diagnostics.A_computed_property_name_in_a_type_literal_must_directly_refer_to_a_built_in_symbol )

}

}
def checkGrammarBreakOrContinueStatement(node: BreakOrContinueStatement): Boolean = {
 var current: Node = node
while (current) {
{
 if (isFunctionLike( current )) {
 return grammarErrorOnNode( node, Diagnostics.Jump_target_cannot_cross_function_boundary )

}
current.kind match {
  case  SyntaxKind.LabeledStatement  =>
if ((node.label&&((current.asInstanceOf[LabeledStatement]).label.text===node.label.text))) {
 val isMisplacedContinueLabel = ((node.kind===SyntaxKind.ContinueStatement)&&(!isIterationStatement( (current.asInstanceOf[LabeledStatement]).statement, true )))
if (isMisplacedContinueLabel) {
 return grammarErrorOnNode( node, Diagnostics.A_continue_statement_can_only_jump_to_a_label_of_an_enclosing_iteration_statement )

}
return false

}
  case  SyntaxKind.SwitchStatement  =>
if (((node.kind===SyntaxKind.BreakStatement)&&(!node.label))) {
 return false

}
  case _ =>
if ((isIterationStatement( current, false )&&(!node.label))) {
 return false

}
}
(current=current.parent)

}
}
if (node.label) {
 val message = (if ((node.kind===SyntaxKind.BreakStatement)) Diagnostics.A_break_statement_can_only_jump_to_a_label_of_an_enclosing_statement else Diagnostics.A_continue_statement_can_only_jump_to_a_label_of_an_enclosing_iteration_statement)
return grammarErrorOnNode( node, message )

}
else {
 val message = (if ((node.kind===SyntaxKind.BreakStatement)) Diagnostics.A_break_statement_can_only_be_used_within_an_enclosing_iteration_or_switch_statement else Diagnostics.A_continue_statement_can_only_be_used_within_an_enclosing_iteration_statement)
return grammarErrorOnNode( node, message )

}

}
def checkGrammarBindingElement(node: BindingElement) = {
 if (node.dotDotDotToken) {
 val elements = (node.parent.asInstanceOf[BindingPattern]).elements
if ((node!==lastOrUndefined( elements ))) {
 return grammarErrorOnNode( node, Diagnostics.A_rest_element_must_be_last_in_an_array_destructuring_pattern )

}
if (((node.name.kind===SyntaxKind.ArrayBindingPattern)||(node.name.kind===SyntaxKind.ObjectBindingPattern))) {
 return grammarErrorOnNode( node.name, Diagnostics.A_rest_element_cannot_contain_a_binding_pattern )

}
if (node.initializer) {
 return grammarErrorAtPos( getSourceFileOfNode( node ), (node.initializer.pos-1), 1, Diagnostics.A_rest_element_cannot_have_an_initializer )

}

}

}
def isStringOrNumberLiteralExpression(expr: Expression) = {
 return (((expr.kind===SyntaxKind.StringLiteral)||(expr.kind===SyntaxKind.NumericLiteral))||(((expr.kind===SyntaxKind.PrefixUnaryExpression)&&((expr.asInstanceOf[PrefixUnaryExpression]).operator===SyntaxKind.MinusToken))&&((expr.asInstanceOf[PrefixUnaryExpression]).operand.kind===SyntaxKind.NumericLiteral)))

}
def checkGrammarVariableDeclaration(node: VariableDeclaration) = {
 if (((node.parent.parent.kind!==SyntaxKind.ForInStatement)&&(node.parent.parent.kind!==SyntaxKind.ForOfStatement))) {
 if (isInAmbientContext( node )) {
 if (node.initializer) {
 if ((isConst( node )&&(!node.`type`))) {
 if ((!isStringOrNumberLiteralExpression( node.initializer ))) {
 return grammarErrorOnNode( node.initializer, Diagnostics.A_const_initializer_in_an_ambient_context_must_be_a_string_or_numeric_literal )

}

}
else {
 val equalsTokenLength = "=".length
return grammarErrorAtPos( getSourceFileOfNode( node ), (node.initializer.pos-equalsTokenLength), equalsTokenLength, Diagnostics.Initializers_are_not_allowed_in_ambient_contexts )

}

}
if ((node.initializer&&(!((isConst( node )&&isStringOrNumberLiteralExpression( node.initializer )))))) {
 val equalsTokenLength = "=".length
return grammarErrorAtPos( getSourceFileOfNode( node ), (node.initializer.pos-equalsTokenLength), equalsTokenLength, Diagnostics.Initializers_are_not_allowed_in_ambient_contexts )

}

}
else if ((!node.initializer)) {
 if ((isBindingPattern( node.name )&&(!isBindingPattern( node.parent )))) {
 return grammarErrorOnNode( node, Diagnostics.A_destructuring_declaration_must_have_an_initializer )

}
if (isConst( node )) {
 return grammarErrorOnNode( node, Diagnostics.const_declarations_must_be_initialized )

}

}

}
val checkLetConstNames = ((isLet( node )||isConst( node )))
return (checkLetConstNames&&checkGrammarNameInLetOrConstDeclarations( node.name ))

}
def checkGrammarNameInLetOrConstDeclarations(name: ( Identifier | BindingPattern )): Boolean = {
 if ((name.kind===SyntaxKind.Identifier)) {
 if (((name.asInstanceOf[Identifier]).originalKeywordKind===SyntaxKind.LetKeyword)) {
 return grammarErrorOnNode( name, Diagnostics.let_is_not_allowed_to_be_used_as_a_name_in_let_or_const_declarations )

}

}
else {
 val elements = (name.asInstanceOf[BindingPattern]).elements
(elements).foreach { fresh160 =>
val element = zeroOfMyType
 = fresh160
 {
 if ((!isOmittedExpression( element ))) {
 checkGrammarNameInLetOrConstDeclarations( element.name )

}

}
}

}

}
def checkGrammarVariableDeclarationList(declarationList: VariableDeclarationList): Boolean = {
 val declarations = declarationList.declarations
if (checkGrammarForDisallowedTrailingComma( declarationList.declarations )) {
 return true

}
if ((!declarationList.declarations.length)) {
 return grammarErrorAtPos( getSourceFileOfNode( declarationList ), declarations.pos, (declarations.end-declarations.pos), Diagnostics.Variable_declaration_list_cannot_be_empty )

}

}
def allowLetAndConstDeclarations(parent: Node): Boolean = {
 parent.kind match {
  case  SyntaxKind.IfStatement | SyntaxKind.DoStatement | SyntaxKind.WhileStatement | SyntaxKind.WithStatement | SyntaxKind.ForStatement | SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement  =>
return false
  case  SyntaxKind.LabeledStatement  =>
return allowLetAndConstDeclarations( parent.parent )
  case _ =>
}
return true

}
def checkGrammarForDisallowedLetOrConstStatement(node: VariableStatement) = {
 if ((!allowLetAndConstDeclarations( node.parent ))) {
 if (isLet( node.declarationList )) {
 return grammarErrorOnNode( node, Diagnostics.let_declarations_can_only_be_declared_inside_a_block )

}
else if (isConst( node.declarationList )) {
 return grammarErrorOnNode( node, Diagnostics.const_declarations_can_only_be_declared_inside_a_block )

}

}

}
def hasParseDiagnostics(sourceFile: SourceFile): Boolean = {
 return (sourceFile.parseDiagnostics.length>0)

}
def grammarErrorOnFirstToken(node: Node, message: DiagnosticMessage, arg0: Any, arg1: Any, arg2: Any): Boolean = {
 val sourceFile = getSourceFileOfNode( node )
if ((!hasParseDiagnostics( sourceFile ))) {
 val span = getSpanOfTokenAtPosition( sourceFile, node.pos )
diagnostics.add( createFileDiagnostic( sourceFile, span.start, span.length, message, arg0, arg1, arg2 ) )
return true

}

}
def grammarErrorAtPos(sourceFile: SourceFile, start: Int, length: Int, message: DiagnosticMessage, arg0: Any, arg1: Any, arg2: Any): Boolean = {
 if ((!hasParseDiagnostics( sourceFile ))) {
 diagnostics.add( createFileDiagnostic( sourceFile, start, length, message, arg0, arg1, arg2 ) )
return true

}

}
def grammarErrorOnNode(node: Node, message: DiagnosticMessage, arg0: Any, arg1: Any, arg2: Any): Boolean = {
 val sourceFile = getSourceFileOfNode( node )
if ((!hasParseDiagnostics( sourceFile ))) {
 diagnostics.add( createDiagnosticForNode( node, message, arg0, arg1, arg2 ) )
return true

}

}
def checkGrammarConstructorTypeParameters(node: ConstructorDeclaration) = {
 if (node.typeParameters) {
 return grammarErrorAtPos( getSourceFileOfNode( node ), node.typeParameters.pos, (node.typeParameters.end-node.typeParameters.pos), Diagnostics.Type_parameters_cannot_appear_on_a_constructor_declaration )

}

}
def checkGrammarConstructorTypeAnnotation(node: ConstructorDeclaration) = {
 if (node.`type`) {
 return grammarErrorOnNode( node.`type`, Diagnostics.Type_annotation_cannot_appear_on_a_constructor_declaration )

}

}
def checkGrammarProperty(node: PropertyDeclaration) = {
 if (isClassLike( node.parent )) {
 if (checkGrammarForNonSymbolComputedProperty( node.name, Diagnostics.A_computed_property_name_in_a_class_property_declaration_must_directly_refer_to_a_built_in_symbol )) {
 return true

}

}
else if ((node.parent.kind===SyntaxKind.InterfaceDeclaration)) {
 if (checkGrammarForNonSymbolComputedProperty( node.name, Diagnostics.A_computed_property_name_in_an_interface_must_directly_refer_to_a_built_in_symbol )) {
 return true

}
if (node.initializer) {
 return grammarErrorOnNode( node.initializer, Diagnostics.An_interface_property_cannot_have_an_initializer )

}

}
else if ((node.parent.kind===SyntaxKind.TypeLiteral)) {
 if (checkGrammarForNonSymbolComputedProperty( node.name, Diagnostics.A_computed_property_name_in_a_type_literal_must_directly_refer_to_a_built_in_symbol )) {
 return true

}
if (node.initializer) {
 return grammarErrorOnNode( node.initializer, Diagnostics.A_type_literal_property_cannot_have_an_initializer )

}

}
if ((isInAmbientContext( node )&&node.initializer)) {
 return grammarErrorOnFirstToken( node.initializer, Diagnostics.Initializers_are_not_allowed_in_ambient_contexts )

}

}
def checkGrammarTopLevelElementForRequiredDeclareModifier(node: Node): Boolean = {
 if (((((((((node.kind===SyntaxKind.InterfaceDeclaration)||(node.kind===SyntaxKind.TypeAliasDeclaration))||(node.kind===SyntaxKind.ImportDeclaration))||(node.kind===SyntaxKind.ImportEqualsDeclaration))||(node.kind===SyntaxKind.ExportDeclaration))||(node.kind===SyntaxKind.ExportAssignment))||(node.kind===SyntaxKind.NamespaceExportDeclaration))||(getModifierFlags( node )&(((ModifierFlags.Ambient|ModifierFlags.Export)|ModifierFlags.Default))))) {
 return false

}
return grammarErrorOnFirstToken( node, Diagnostics.A_declare_modifier_is_required_for_a_top_level_declaration_in_a_d_ts_file )

}
def checkGrammarTopLevelElementsForRequiredDeclareModifier(file: SourceFile): Boolean = {
 (file.statements).foreach { fresh161 =>
val decl = zeroOfMyType
 = fresh161
 {
 if ((isDeclaration( decl )||(decl.kind===SyntaxKind.VariableStatement))) {
 if (checkGrammarTopLevelElementForRequiredDeclareModifier( decl )) {
 return true

}

}

}
}

}
def checkGrammarSourceFile(node: SourceFile): Boolean = {
 return (isInAmbientContext( node )&&checkGrammarTopLevelElementsForRequiredDeclareModifier( node ))

}
def checkGrammarStatementInAmbientContext(node: Node): Boolean = {
 if (isInAmbientContext( node )) {
 if (isAccessor( node.parent.kind )) {
 return (getNodeLinks( node ).hasReportedStatementInAmbientContext=true)

}
val links = getNodeLinks( node )
if (((!links.hasReportedStatementInAmbientContext)&&isFunctionLike( node.parent ))) {
 return (getNodeLinks( node ).hasReportedStatementInAmbientContext=grammarErrorOnFirstToken( node, Diagnostics.An_implementation_cannot_be_declared_in_ambient_contexts ))

}
if ((((node.parent.kind===SyntaxKind.Block)||(node.parent.kind===SyntaxKind.ModuleBlock))||(node.parent.kind===SyntaxKind.SourceFile))) {
 val links = getNodeLinks( node.parent )
if ((!links.hasReportedStatementInAmbientContext)) {
 return (links.hasReportedStatementInAmbientContext=grammarErrorOnFirstToken( node, Diagnostics.Statements_are_not_allowed_in_ambient_contexts ))

}

}
else {
}

}

}
def checkGrammarNumericLiteral(node: NumericLiteral): Boolean = {
 if ((node.isOctalLiteral&&(languageVersion>=ScriptTarget.ES5))) {
 return grammarErrorOnNode( node, Diagnostics.Octal_literals_are_not_available_when_targeting_ECMAScript_5_and_higher )

}

}
def grammarErrorAfterFirstToken(node: Node, message: DiagnosticMessage, arg0: Any, arg1: Any, arg2: Any): Boolean = {
 val sourceFile = getSourceFileOfNode( node )
if ((!hasParseDiagnostics( sourceFile ))) {
 val span = getSpanOfTokenAtPosition( sourceFile, node.pos )
diagnostics.add( createFileDiagnostic( sourceFile, textSpanEnd( span ), 0, message, arg0, arg1, arg2 ) )
return true

}

}
def getAmbientModules(): Array[Symbol] = {
 val result: Array[Symbol] = Array()
(globals).keys.foreach { fresh162 =>
val sym = zeroOfMyType
 = fresh162
 {
 if (ambientModuleSymbolRegex.test( sym )) {
 result.push( globals(sym) )

}

}
}
return result

}

}
}
