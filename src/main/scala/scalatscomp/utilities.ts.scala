package scalatscomp
object Utilities {
val externalHelpersModuleNameText = "tslib"
trait ReferencePathMatchResult {
  var fileReference: FileReference
  var diagnosticMessage: DiagnosticMessage
  var isNoDefaultLib: Boolean
  var isTypeReferenceDirective: Boolean
}
def getDeclarationOfKind(symbol: Symbol, kind: SyntaxKind): Declaration = {
 val declarations = symbol.declarations
if (declarations) {
 (declarations).foreach { fresh1 =>
val declaration = zeroOfMyType
 = fresh1
 {
 if ((declaration.kind===kind)) {
 return declaration

}

}
}

}
return undefined

}
trait StringSymbolWriter extends SymbolWriter {
  def string(): String
}
trait EmitHost extends ScriptReferenceHost {
  def getSourceFiles(): Array[SourceFile]
  def isSourceFileFromExternalLibrary(file: SourceFile): Boolean
  def getCommonSourceDirectory(): String
  def getCanonicalFileName(fileName: String): String
  def getNewLine(): String
  def isEmitBlocked(emitFileName: String): Boolean
  var writeFile: WriteFileCallback
}
val stringWriters: Array[StringSymbolWriter] = Array()
def getSingleLineStringWriter(): StringSymbolWriter = {
 if ((stringWriters.length===0)) {
 var str = ""
val writeText: ((String) => Unit) = (text =>  (str+=text))
return Map( "string" -> (() =>  str),
"writeKeyword" -> writeText,
"writeOperator" -> writeText,
"writePunctuation" -> writeText,
"writeSpace" -> writeText,
"writeStringLiteral" -> writeText,
"writeParameter" -> writeText,
"writeSymbol" -> writeText,
"writeLine" -> (() =>  (str+=" ")),
"increaseIndent" -> noop,
"decreaseIndent" -> noop,
"clear" -> (() =>  (str="")),
"trackSymbol" -> noop,
"reportInaccessibleThisError" -> noop )

}
return stringWriters.pop()

}
def releaseStringWriter(writer: StringSymbolWriter) = {
 writer.clear()
stringWriters.push( writer )

}
def getFullWidth(node: Node) = {
 return (node.end-node.pos)

}
def hasResolvedModule(sourceFile: SourceFile, moduleNameText: String): Boolean = {
 return (!(!(((sourceFile&&sourceFile.resolvedModules)&&sourceFile.resolvedModules(moduleNameText)))))

}
def getResolvedModule(sourceFile: SourceFile, moduleNameText: String): ResolvedModule = {
 return (if (hasResolvedModule( sourceFile, moduleNameText )) sourceFile.resolvedModules(moduleNameText) else undefined)

}
def setResolvedModule(sourceFile: SourceFile, moduleNameText: String, resolvedModule: ResolvedModule): Unit = {
 if ((!sourceFile.resolvedModules)) {
 (sourceFile.resolvedModules=createMap[ ResolvedModule ]())

}
(sourceFile.resolvedModules(moduleNameText)=resolvedModule)

}
def setResolvedTypeReferenceDirective(sourceFile: SourceFile, typeReferenceDirectiveName: String, resolvedTypeReferenceDirective: ResolvedTypeReferenceDirective): Unit = {
 if ((!sourceFile.resolvedTypeReferenceDirectiveNames)) {
 (sourceFile.resolvedTypeReferenceDirectiveNames=createMap[ ResolvedTypeReferenceDirective ]())

}
(sourceFile.resolvedTypeReferenceDirectiveNames(typeReferenceDirectiveName)=resolvedTypeReferenceDirective)

}
def moduleResolutionIsEqualTo(oldResolution: ResolvedModule, newResolution: ResolvedModule): Boolean = {
 return ((oldResolution.resolvedFileName===newResolution.resolvedFileName)&&(oldResolution.isExternalLibraryImport===newResolution.isExternalLibraryImport))

}
def typeDirectiveIsEqualTo(oldResolution: ResolvedTypeReferenceDirective, newResolution: ResolvedTypeReferenceDirective): Boolean = {
 return ((oldResolution.resolvedFileName===newResolution.resolvedFileName)&&(oldResolution.primary===newResolution.primary))

}
def hasChangesInResolutions[T](names: Array[String], newResolutions: Array[T], oldResolutions: Map[T], comparer: ((T, T) => Boolean)): Boolean = {
 if ((names.length!==newResolutions.length)) {
 return false

}
{
var i = 0
while( (i<names.length)) {
 {
 val newResolution = newResolutions(i)
val oldResolution = (oldResolutions&&oldResolutions(names(i)))
val changed = (if (oldResolution) ((!newResolution)||(!comparer( oldResolution, newResolution ))) else newResolution)
if (changed) {
 return true

}

}
 (i+= 1)
}
}
return false

}
def containsParseError(node: Node): Boolean = {
 aggregateChildData( node )
return (((node.flags&NodeFlags.ThisNodeOrAnySubNodesHasError))!==0)

}
def aggregateChildData(node: Node): Unit = {
 if ((!((node.flags&NodeFlags.HasAggregatedChildData)))) {
 val thisNodeOrAnySubNodesHasError = (((((node.flags&NodeFlags.ThisNodeHasError))!==0))||forEachChild( node, containsParseError ))
if (thisNodeOrAnySubNodesHasError) {
 (node.flags|=NodeFlags.ThisNodeOrAnySubNodesHasError)

}
(node.flags|=NodeFlags.HasAggregatedChildData)

}

}
def getSourceFileOfNode(node: Node): SourceFile = {
 while ((node&&(node.kind!==SyntaxKind.SourceFile))) {
{
 (node=node.parent)

}
}
return node.asInstanceOf[SourceFile]

}
def isStatementWithLocals(node: Node) = {
 node.kind match {
  case  SyntaxKind.Block | SyntaxKind.CaseBlock | SyntaxKind.ForStatement | SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement  =>
return true
  case _ =>
}
return false

}
def getStartPositionOfLine(line: Int, sourceFile: SourceFile): Int = {
 Debug.assert( (line>=0) )
return getLineStarts( sourceFile )(line)

}
def nodePosToString(node: Node): String = {
 val file = getSourceFileOfNode( node )
val loc = getLineAndCharacterOfPosition( file, node.pos )
return s"""${ file.fileName}(${ (loc.line+1)},${ (loc.character+1)})"""

}
def getStartPosOfNode(node: Node): Int = {
 return node.pos

}
def isDefined(value: Any): Boolean = {
 return (value!==undefined)

}
def getEndLinePosition(line: Int, sourceFile: SourceFile): Int = {
 Debug.assert( (line>=0) )
val lineStarts = getLineStarts( sourceFile )
val lineIndex = line
val sourceText = sourceFile.text
if (((lineIndex+1)===lineStarts.length)) {
 return (sourceText.length-1)

}
else {
 val start = lineStarts(lineIndex)
var pos = (lineStarts((lineIndex+1))-1)
Debug.assert( isLineBreak( sourceText.charCodeAt( pos ) ) )
while (((start<=pos)&&isLineBreak( sourceText.charCodeAt( pos ) ))) {
{
 (pos-= 1)

}
}
return pos

}

}
def nodeIsMissing(node: Node) = {
 if ((node===undefined)) {
 return true

}
return (((node.pos===node.end)&&(node.pos>=0))&&(node.kind!==SyntaxKind.EndOfFileToken))

}
def nodeIsPresent(node: Node) = {
 return (!nodeIsMissing( node ))

}
def getTokenPosOfNode(node: Node, sourceFile: SourceFile, includeJsDocComment: Boolean): Int = {
 if (nodeIsMissing( node )) {
 return node.pos

}
if (isJSDocNode( node )) {
 return skipTrivia( ((sourceFile||getSourceFileOfNode( node ))).text, node.pos, false, true )

}
if (((includeJsDocComment&&node.jsDocComments)&&(node.jsDocComments.length>0))) {
 return getTokenPosOfNode( node.jsDocComments(0) )

}
if (((node.kind===SyntaxKind.SyntaxList)&&((node.asInstanceOf[SyntaxList])._children.length>0))) {
 return getTokenPosOfNode( (node.asInstanceOf[SyntaxList])._children(0), sourceFile, includeJsDocComment )

}
return skipTrivia( ((sourceFile||getSourceFileOfNode( node ))).text, node.pos )

}
def isJSDocNode(node: Node) = {
 return ((node.kind>=SyntaxKind.FirstJSDocNode)&&(node.kind<=SyntaxKind.LastJSDocNode))

}
def isJSDocTag(node: Node) = {
 return ((node.kind>=SyntaxKind.FirstJSDocTagNode)&&(node.kind<=SyntaxKind.LastJSDocTagNode))

}
def getNonDecoratorTokenPosOfNode(node: Node, sourceFile: SourceFile): Int = {
 if ((nodeIsMissing( node )||(!node.decorators))) {
 return getTokenPosOfNode( node, sourceFile )

}
return skipTrivia( ((sourceFile||getSourceFileOfNode( node ))).text, node.decorators.end )

}
def getSourceTextOfNodeFromSourceFile(sourceFile: SourceFile, node: Node, includeTrivia: Nothing = false): String = {
 if (nodeIsMissing( node )) {
 return ""

}
val text = sourceFile.text
return text.substring( (if (includeTrivia) node.pos else skipTrivia( text, node.pos )), node.end )

}
def getTextOfNodeFromSourceText(sourceText: String, node: Node): String = {
 if (nodeIsMissing( node )) {
 return ""

}
return sourceText.substring( skipTrivia( sourceText, node.pos ), node.end )

}
def getTextOfNode(node: Node, includeTrivia: Nothing = false): String = {
 return getSourceTextOfNodeFromSourceFile( getSourceFileOfNode( node ), node, includeTrivia )

}
def getLiteralText(node: LiteralLikeNode, sourceFile: SourceFile, languageVersion: ScriptTarget) = {
 if (((languageVersion<ScriptTarget.ES2015)&&((isTemplateLiteralKind( node.kind )||node.hasExtendedUnicodeEscape)))) {
 return getQuotedEscapedLiteralText( "\"", node.text, "\"" )

}
if (((!nodeIsSynthesized( node ))&&node.parent)) {
 val text = getSourceTextOfNodeFromSourceFile( sourceFile, node )
if (((languageVersion<ScriptTarget.ES2015)&&isBinaryOrOctalIntegerLiteral( node, text ))) {
 return node.text

}
return text

}
node.kind match {
  case  SyntaxKind.StringLiteral  =>
return getQuotedEscapedLiteralText( "\"", node.text, "\"" )
  case  SyntaxKind.NoSubstitutionTemplateLiteral  =>
return getQuotedEscapedLiteralText( "`", node.text, "`" )
  case  SyntaxKind.TemplateHead  =>
return getQuotedEscapedLiteralText( "`", node.text, "${" )
  case  SyntaxKind.TemplateMiddle  =>
return getQuotedEscapedLiteralText( "}", node.text, "${" )
  case  SyntaxKind.TemplateTail  =>
return getQuotedEscapedLiteralText( "}", node.text, "`" )
  case  SyntaxKind.NumericLiteral  =>
return node.text
  case _ =>
}
Debug.fail( s"""Literal kind '${ node.kind}' not accounted for."""  )

}
def isBinaryOrOctalIntegerLiteral(node: LiteralLikeNode, text: String) = {
 if (((node.kind===SyntaxKind.NumericLiteral)&&(text.length>1))) {
 text.charCodeAt( 1 ) match {
  case  CharacterCodes.b | CharacterCodes.B | CharacterCodes.o | CharacterCodes.O  =>
return true
  case _ =>
}

}
return false

}
def getQuotedEscapedLiteralText(leftQuote: String, text: String, rightQuote: String) = {
 return ((leftQuote+escapeNonAsciiCharacters( escapeString( text ) ))+rightQuote)

}
def escapeIdentifier(identifier: String): String = {
 return (if ((((identifier.length>=2)&&(identifier.charCodeAt( 0 )===CharacterCodes._underscore_))&&(identifier.charCodeAt( 1 )===CharacterCodes._underscore_))) ("_"+identifier) else identifier)

}
def unescapeIdentifier(identifier: String): String = {
 return (if (((((identifier.length>=3)&&(identifier.charCodeAt( 0 )===CharacterCodes._underscore_))&&(identifier.charCodeAt( 1 )===CharacterCodes._underscore_))&&(identifier.charCodeAt( 2 )===CharacterCodes._underscore_))) identifier.substr( 1 ) else identifier)

}
def makeIdentifierFromModuleName(moduleName: String): String = {
 return getBaseFileName( moduleName ).replace( java.util.regex.Pattern.compile(raw"""^(\d)"""), "_$1" ).replace( java.util.regex.Pattern.compile(raw"""\W""", "g"), "_" )

}
def isBlockOrCatchScoped(declaration: Declaration) = {
 return ((((getCombinedNodeFlags( declaration )&NodeFlags.BlockScoped))!==0)||isCatchClauseVariableDeclarationOrBindingElement( declaration ))

}
def isCatchClauseVariableDeclarationOrBindingElement(declaration: Declaration) = {
 val node = getRootDeclaration( declaration )
return ((node.kind===SyntaxKind.VariableDeclaration)&&(node.parent.kind===SyntaxKind.CatchClause))

}
def isAmbientModule(node: Node): Boolean = {
 return ((node&&(node.kind===SyntaxKind.ModuleDeclaration))&&((((node.asInstanceOf[ModuleDeclaration]).name.kind===SyntaxKind.StringLiteral)||isGlobalScopeAugmentation( node.asInstanceOf[ModuleDeclaration] ))))

}
def isShorthandAmbientModuleSymbol(moduleSymbol: Symbol): Boolean = {
 return isShorthandAmbientModule( moduleSymbol.valueDeclaration )

}
def isShorthandAmbientModule(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.ModuleDeclaration)&&((!(node.asInstanceOf[ModuleDeclaration]).body)))

}
def isBlockScopedContainerTopLevel(node: Node): Boolean = {
 return (((node.kind===SyntaxKind.SourceFile)||(node.kind===SyntaxKind.ModuleDeclaration))||isFunctionLike( node ))

}
def isGlobalScopeAugmentation(module: ModuleDeclaration): Boolean = {
 return (!(!((module.flags&NodeFlags.GlobalAugmentation))))

}
def isExternalModuleAugmentation(node: Node): Boolean = {
 if (((!node)||(!isAmbientModule( node )))) {
 return false

}
node.parent.kind match {
  case  SyntaxKind.SourceFile  =>
return isExternalModule( node.parent.asInstanceOf[SourceFile] )
  case  SyntaxKind.ModuleBlock  =>
return (isAmbientModule( node.parent.parent )&&(!isExternalModule( node.parent.parent.parent.asInstanceOf[SourceFile] )))
  case _ =>
}
return false

}
def isBlockScope(node: Node, parentNode: Node) = {
 node.kind match {
  case  SyntaxKind.SourceFile | SyntaxKind.CaseBlock | SyntaxKind.CatchClause | SyntaxKind.ModuleDeclaration | SyntaxKind.ForStatement | SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement | SyntaxKind.Constructor | SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction  =>
return true
  case  SyntaxKind.Block  =>
return (parentNode&&(!isFunctionLike( parentNode )))
  case _ =>
}
return false

}
def getEnclosingBlockScopeContainer(node: Node): Node = {
 var current = node.parent
while (current) {
{
 if (isBlockScope( current, current.parent )) {
 return current

}
(current=current.parent)

}
}

}
def declarationNameToString(name: DeclarationName) = {
 return (if ((getFullWidth( name )===0)) "(Missing)" else getTextOfNode( name ))

}
def createDiagnosticForNode(node: Node, message: DiagnosticMessage, arg0: ( String | Int ), arg1: ( String | Int ), arg2: ( String | Int )): Diagnostic = {
 val sourceFile = getSourceFileOfNode( node )
val span = getErrorSpanForNode( sourceFile, node )
return createFileDiagnostic( sourceFile, span.start, span.length, message, arg0, arg1, arg2 )

}
def createDiagnosticForNodeFromMessageChain(node: Node, messageChain: DiagnosticMessageChain): Diagnostic = {
 val sourceFile = getSourceFileOfNode( node )
val span = getErrorSpanForNode( sourceFile, node )
return Map( "file" -> sourceFile,
"start" -> span.start,
"length" -> span.length,
"code" -> messageChain.code,
"category" -> messageChain.category,
"messageText" -> (if (messageChain.next) messageChain else messageChain.messageText) )

}
def getSpanOfTokenAtPosition(sourceFile: SourceFile, pos: Int): TextSpan = {
 val scanner = createScanner( sourceFile.languageVersion, true, sourceFile.languageVariant, sourceFile.text, undefined, pos )
scanner.scan()
val start = scanner.getTokenPos()
return createTextSpanFromBounds( start, scanner.getTextPos() )

}
def getErrorSpanForArrowFunction(sourceFile: SourceFile, node: ArrowFunction): TextSpan = {
 val pos = skipTrivia( sourceFile.text, node.pos )
if ((node.body&&(node.body.kind===SyntaxKind.Block))) {
 const fresh2 = getLineAndCharacterOfPosition( sourceFile, node.body.pos )
val startLine = fresh2.startLine
const fresh3 = getLineAndCharacterOfPosition( sourceFile, node.body.end )
val endLine = fresh3.endLine
if ((startLine<endLine)) {
 return createTextSpan( pos, ((getEndLinePosition( startLine, sourceFile )-pos)+1) )

}

}
return createTextSpanFromBounds( pos, node.end )

}
def getErrorSpanForNode(sourceFile: SourceFile, node: Node): TextSpan = {
 var errorNode = node
node.kind match {
  case  SyntaxKind.SourceFile  =>
var pos = skipTrivia( sourceFile.text, 0, false )
if ((pos===sourceFile.text.length)) {
 return createTextSpan( 0, 0 )

}
return getSpanOfTokenAtPosition( sourceFile, pos )
  case  SyntaxKind.VariableDeclaration | SyntaxKind.BindingElement | SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression | SyntaxKind.InterfaceDeclaration | SyntaxKind.ModuleDeclaration | SyntaxKind.EnumDeclaration | SyntaxKind.EnumMember | SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression | SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.TypeAliasDeclaration  =>
(errorNode=(node.asInstanceOf[Declaration]).name)
  case  SyntaxKind.ArrowFunction  =>
return getErrorSpanForArrowFunction( sourceFile, node.asInstanceOf[ArrowFunction] )
  case _ =>
}
if ((errorNode===undefined)) {
 return getSpanOfTokenAtPosition( sourceFile, node.pos )

}
val pos = (if (nodeIsMissing( errorNode )) errorNode.pos else skipTrivia( sourceFile.text, errorNode.pos ))
return createTextSpanFromBounds( pos, errorNode.end )

}
def isExternalOrCommonJsModule(file: SourceFile): Boolean = {
 return (((file.externalModuleIndicator||file.commonJsModuleIndicator))!==undefined)

}
def isDeclarationFile(file: SourceFile): Boolean = {
 return file.isDeclarationFile

}
def isConstEnumDeclaration(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.EnumDeclaration)&&isConst( node ))

}
def isConst(node: Node): Boolean = {
 return ((!(!((getCombinedNodeFlags( node )&NodeFlags.Const))))||(!(!((getCombinedModifierFlags( node )&ModifierFlags.Const)))))

}
def isLet(node: Node): Boolean = {
 return (!(!((getCombinedNodeFlags( node )&NodeFlags.Let))))

}
def isSuperCall(n: Node): Boolean = {
 return ((n.kind===SyntaxKind.CallExpression)&&((n.asInstanceOf[CallExpression]).expression.kind===SyntaxKind.SuperKeyword))

}
def isPrologueDirective(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.ExpressionStatement)&&((node.asInstanceOf[ExpressionStatement]).expression.kind===SyntaxKind.StringLiteral))

}
def getLeadingCommentRangesOfNode(node: Node, sourceFileOfNode: SourceFile) = {
 return getLeadingCommentRanges( sourceFileOfNode.text, node.pos )

}
def getLeadingCommentRangesOfNodeFromText(node: Node, text: String) = {
 return getLeadingCommentRanges( text, node.pos )

}
def getJsDocComments(node: Node, sourceFileOfNode: SourceFile) = {
 return getJsDocCommentsFromText( node, sourceFileOfNode.text )

}
def getJsDocCommentsFromText(node: Node, text: String) = {
 val commentRanges = (if ((((((node.kind===SyntaxKind.Parameter)||(node.kind===SyntaxKind.TypeParameter))||(node.kind===SyntaxKind.FunctionExpression))||(node.kind===SyntaxKind.ArrowFunction)))) concatenate( getTrailingCommentRanges( text, node.pos ), getLeadingCommentRanges( text, node.pos ) ) else getLeadingCommentRangesOfNodeFromText( node, text ))
return filter( commentRanges, isJsDocComment )
def isJsDocComment(comment: CommentRange) = {
 return (((text.charCodeAt( (comment.pos+1) )===CharacterCodes.asterisk)&&(text.charCodeAt( (comment.pos+2) )===CharacterCodes.asterisk))&&(text.charCodeAt( (comment.pos+3) )!==CharacterCodes.slash))

}

}
var fullTripleSlashReferencePathRegEx = java.util.regex.Pattern.compile(raw"""^(\/\/\/\s*<reference\s+path\s*=\s*)('|")(.+?)\2.*?\/>""")
var fullTripleSlashReferenceTypeReferenceDirectiveRegEx = java.util.regex.Pattern.compile(raw"""^(\/\/\/\s*<reference\s+types\s*=\s*)('|")(.+?)\2.*?\/>""")
var fullTripleSlashAMDReferencePathRegEx = java.util.regex.Pattern.compile(raw"""^(\/\/\/\s*<amd-dependency\s+path\s*=\s*)('|")(.+?)\2.*?\/>""")
def isPartOfTypeNode(node: Node): Boolean = {
 if (((SyntaxKind.FirstTypeNode<=node.kind)&&(node.kind<=SyntaxKind.LastTypeNode))) {
 return true

}
node.kind match {
  case  SyntaxKind.AnyKeyword | SyntaxKind.NumberKeyword | SyntaxKind.StringKeyword | SyntaxKind.BooleanKeyword | SyntaxKind.SymbolKeyword | SyntaxKind.UndefinedKeyword | SyntaxKind.NeverKeyword  =>
return true
  case  SyntaxKind.VoidKeyword  =>
return (node.parent.kind!==SyntaxKind.VoidExpression)
  case  SyntaxKind.ExpressionWithTypeArguments  =>
return (!isExpressionWithTypeArgumentsInClassExtendsClause( node ))
  case  SyntaxKind.Identifier  =>
if (((node.parent.kind===SyntaxKind.QualifiedName)&&((node.parent.asInstanceOf[QualifiedName]).right===node))) {
 (node=node.parent)

}
else if (((node.parent.kind===SyntaxKind.PropertyAccessExpression)&&((node.parent.asInstanceOf[PropertyAccessExpression]).name===node))) {
 (node=node.parent)

}
Debug.assert( (((node.kind===SyntaxKind.Identifier)||(node.kind===SyntaxKind.QualifiedName))||(node.kind===SyntaxKind.PropertyAccessExpression)), "'node' was expected to be a qualified name, identifier or property access in 'isPartOfTypeNode'." )
  case  SyntaxKind.QualifiedName | SyntaxKind.PropertyAccessExpression | SyntaxKind.ThisKeyword  =>
var parent = node.parent
if ((parent.kind===SyntaxKind.TypeQuery)) {
 return false

}
if (((SyntaxKind.FirstTypeNode<=parent.kind)&&(parent.kind<=SyntaxKind.LastTypeNode))) {
 return true

}
parent.kind match {
  case  SyntaxKind.ExpressionWithTypeArguments  =>
return (!isExpressionWithTypeArgumentsInClassExtendsClause( parent ))
  case  SyntaxKind.TypeParameter  =>
return (node===(parent.asInstanceOf[TypeParameterDeclaration]).constraint)
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.Parameter | SyntaxKind.VariableDeclaration  =>
return (node===(parent.asInstanceOf[VariableLikeDeclaration]).`type`)
  case  SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction | SyntaxKind.Constructor | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
return (node===(parent.asInstanceOf[FunctionLikeDeclaration]).`type`)
  case  SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature  =>
return (node===(parent.asInstanceOf[SignatureDeclaration]).`type`)
  case  SyntaxKind.TypeAssertionExpression  =>
return (node===(parent.asInstanceOf[TypeAssertion]).`type`)
  case  SyntaxKind.CallExpression | SyntaxKind.NewExpression  =>
return ((parent.asInstanceOf[CallExpression]).typeArguments&&(indexOf( (parent.asInstanceOf[CallExpression]).typeArguments, node )>=0))
  case  SyntaxKind.TaggedTemplateExpression  =>
return false
  case _ =>
}
  case _ =>
}
return false

}
def forEachReturnStatement[T](body: Block, visitor: ((ReturnStatement) => T)): T = {
 return traverse( body )
def traverse(node: Node): T = {
 node.kind match {
  case  SyntaxKind.ReturnStatement  =>
return visitor( node.asInstanceOf[ReturnStatement] )
  case  SyntaxKind.CaseBlock | SyntaxKind.Block | SyntaxKind.IfStatement | SyntaxKind.DoStatement | SyntaxKind.WhileStatement | SyntaxKind.ForStatement | SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement | SyntaxKind.WithStatement | SyntaxKind.SwitchStatement | SyntaxKind.CaseClause | SyntaxKind.DefaultClause | SyntaxKind.LabeledStatement | SyntaxKind.TryStatement | SyntaxKind.CatchClause  =>
return forEachChild( node, traverse )
  case _ =>
}

}

}
def forEachYieldExpression(body: Block, visitor: ((YieldExpression) => Unit)): Unit = {
 return traverse( body )
def traverse(node: Node): Unit = {
 node.kind match {
  case  SyntaxKind.YieldExpression  =>
visitor( node.asInstanceOf[YieldExpression] )
var operand = (node.asInstanceOf[YieldExpression]).expression
if (operand) {
 traverse( operand )

}
  case  SyntaxKind.EnumDeclaration | SyntaxKind.InterfaceDeclaration | SyntaxKind.ModuleDeclaration | SyntaxKind.TypeAliasDeclaration | SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression  =>
return
  case _ =>
if (isFunctionLike( node )) {
 val name = (node.asInstanceOf[FunctionLikeDeclaration]).name
if ((name&&(name.kind===SyntaxKind.ComputedPropertyName))) {
 traverse( (name.asInstanceOf[ComputedPropertyName]).expression )
return

}

}
else if ((!isPartOfTypeNode( node ))) {
 forEachChild( node, traverse )

}
}

}

}
def isVariableLike(node: Node): Boolean = {
 if (node) {
 node.kind match {
  case  SyntaxKind.BindingElement | SyntaxKind.EnumMember | SyntaxKind.Parameter | SyntaxKind.PropertyAssignment | SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.ShorthandPropertyAssignment | SyntaxKind.VariableDeclaration  =>
return true
  case _ =>
}

}
return false

}
def isAccessor(node: Node): Boolean = {
 return (node&&(((node.kind===SyntaxKind.GetAccessor)||(node.kind===SyntaxKind.SetAccessor))))

}
def isClassLike(node: Node): Boolean = {
 return (node&&(((node.kind===SyntaxKind.ClassDeclaration)||(node.kind===SyntaxKind.ClassExpression))))

}
def isFunctionLike(node: Node): Boolean = {
 return (node&&isFunctionLikeKind( node.kind ))

}
def isFunctionLikeKind(kind: SyntaxKind): Boolean = {
 kind match {
  case  SyntaxKind.Constructor | SyntaxKind.FunctionExpression | SyntaxKind.FunctionDeclaration | SyntaxKind.ArrowFunction | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature | SyntaxKind.FunctionType | SyntaxKind.ConstructorType  =>
return true
  case _ =>
}
return false

}
def introducesArgumentsExoticObject(node: Node) = {
 node.kind match {
  case  SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression  =>
return true
  case _ =>
}
return false

}
def isIterationStatement(node: Node, lookInLabeledStatements: Boolean): Boolean = {
 node.kind match {
  case  SyntaxKind.ForStatement | SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement | SyntaxKind.DoStatement | SyntaxKind.WhileStatement  =>
return true
  case  SyntaxKind.LabeledStatement  =>
return (lookInLabeledStatements&&isIterationStatement( (node.asInstanceOf[LabeledStatement]).statement, lookInLabeledStatements ))
  case _ =>
}
return false

}
def isFunctionBlock(node: Node) = {
 return ((node&&(node.kind===SyntaxKind.Block))&&isFunctionLike( node.parent ))

}
def isObjectLiteralMethod(node: Node): Boolean = {
 return ((node&&(node.kind===SyntaxKind.MethodDeclaration))&&(node.parent.kind===SyntaxKind.ObjectLiteralExpression))

}
def isObjectLiteralOrClassExpressionMethod(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.MethodDeclaration)&&(((node.parent.kind===SyntaxKind.ObjectLiteralExpression)||(node.parent.kind===SyntaxKind.ClassExpression))))

}
def isIdentifierTypePredicate(predicate: TypePredicate): Boolean = {
 return (predicate&&(predicate.kind===TypePredicateKind.Identifier))

}
def isThisTypePredicate(predicate: TypePredicate): Boolean = {
 return (predicate&&(predicate.kind===TypePredicateKind.This))

}
def getContainingFunction(node: Node): FunctionLikeDeclaration = {
 while (true) {
{
 (node=node.parent)
if (((!node)||isFunctionLike( node ))) {
 return node.asInstanceOf[FunctionLikeDeclaration]

}

}
}

}
def getContainingClass(node: Node): ClassLikeDeclaration = {
 while (true) {
{
 (node=node.parent)
if (((!node)||isClassLike( node ))) {
 return node.asInstanceOf[ClassLikeDeclaration]

}

}
}

}
def getThisContainer(node: Node, includeArrowFunctions: Boolean): Node = {
 while (true) {
{
 (node=node.parent)
if ((!node)) {
 return undefined

}
node.kind match {
  case  SyntaxKind.ComputedPropertyName  =>
if (isClassLike( node.parent.parent )) {
 return node

}
(node=node.parent)
  case  SyntaxKind.Decorator  =>
if (((node.parent.kind===SyntaxKind.Parameter)&&isClassElement( node.parent.parent ))) {
 (node=node.parent.parent)

}
else if (isClassElement( node.parent )) {
 (node=node.parent)

}
  case  SyntaxKind.ArrowFunction  =>
if ((!includeArrowFunctions)) {
 continue

}
  case  SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression | SyntaxKind.ModuleDeclaration | SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature | SyntaxKind.EnumDeclaration | SyntaxKind.SourceFile  =>
return node
  case _ =>
}

}
}

}
def getSuperContainer(node: Node, stopOnFunctions: Boolean): Node = {
 while (true) {
{
 (node=node.parent)
if ((!node)) {
 return node

}
node.kind match {
  case  SyntaxKind.ComputedPropertyName  =>
(node=node.parent)
  case  SyntaxKind.FunctionDeclaration | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction  =>
if ((!stopOnFunctions)) {
 continue

}
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
return node
  case  SyntaxKind.Decorator  =>
if (((node.parent.kind===SyntaxKind.Parameter)&&isClassElement( node.parent.parent ))) {
 (node=node.parent.parent)

}
else if (isClassElement( node.parent )) {
 (node=node.parent)

}
  case _ =>
}

}
}

}
def getImmediatelyInvokedFunctionExpression(func: Node): CallExpression = {
 if (((func.kind===SyntaxKind.FunctionExpression)||(func.kind===SyntaxKind.ArrowFunction))) {
 var prev = func
var parent = func.parent
while ((parent.kind===SyntaxKind.ParenthesizedExpression)) {
{
 (prev=parent)
(parent=parent.parent)

}
}
if (((parent.kind===SyntaxKind.CallExpression)&&((parent.asInstanceOf[CallExpression]).expression===prev))) {
 return parent.asInstanceOf[CallExpression]

}

}

}
def isSuperProperty(node: Node): Boolean = {
 val kind = node.kind
return ((((kind===SyntaxKind.PropertyAccessExpression)||(kind===SyntaxKind.ElementAccessExpression)))&&((node.asInstanceOf[( PropertyAccessExpression | ElementAccessExpression )]).expression.kind===SyntaxKind.SuperKeyword))

}
def getEntityNameFromTypeNode(node: TypeNode): EntityNameOrEntityNameExpression = {
 if (node) {
 node.kind match {
  case  SyntaxKind.TypeReference  =>
return (node.asInstanceOf[TypeReferenceNode]).typeName
  case  SyntaxKind.ExpressionWithTypeArguments  =>
Debug.assert( isEntityNameExpression( (node.asInstanceOf[ExpressionWithTypeArguments]).expression ) )
return (node.asInstanceOf[ExpressionWithTypeArguments]).expression.asInstanceOf[EntityNameExpression]
  case  SyntaxKind.Identifier | SyntaxKind.QualifiedName  =>
return (node.asInstanceOf[Node].asInstanceOf[EntityName])
  case _ =>
}

}
return undefined

}
def isCallLikeExpression(node: Node): Boolean = {
 node.kind match {
  case  SyntaxKind.CallExpression | SyntaxKind.NewExpression | SyntaxKind.TaggedTemplateExpression | SyntaxKind.Decorator  =>
return true
  case _ =>
return false
}

}
def getInvokedExpression(node: CallLikeExpression): Expression = {
 if ((node.kind===SyntaxKind.TaggedTemplateExpression)) {
 return (node.asInstanceOf[TaggedTemplateExpression]).tag

}
return (node.asInstanceOf[( CallExpression | Decorator )]).expression

}
def nodeCanBeDecorated(node: Node): Boolean = {
 node.kind match {
  case  SyntaxKind.ClassDeclaration  =>
return true
  case  SyntaxKind.PropertyDeclaration  =>
return (node.parent.kind===SyntaxKind.ClassDeclaration)
  case  SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.MethodDeclaration  =>
return (((node.asInstanceOf[FunctionLikeDeclaration]).body!==undefined)&&(node.parent.kind===SyntaxKind.ClassDeclaration))
  case  SyntaxKind.Parameter  =>
return ((((node.parent.asInstanceOf[FunctionLikeDeclaration]).body!==undefined)&&((((node.parent.kind===SyntaxKind.Constructor)||(node.parent.kind===SyntaxKind.MethodDeclaration))||(node.parent.kind===SyntaxKind.SetAccessor))))&&(node.parent.parent.kind===SyntaxKind.ClassDeclaration))
  case _ =>
}
return false

}
def nodeIsDecorated(node: Node): Boolean = {
 return ((node.decorators!==undefined)&&nodeCanBeDecorated( node ))

}
def nodeOrChildIsDecorated(node: Node): Boolean = {
 return (nodeIsDecorated( node )||childIsDecorated( node ))

}
def childIsDecorated(node: Node): Boolean = {
 node.kind match {
  case  SyntaxKind.ClassDeclaration  =>
return forEach( (node.asInstanceOf[ClassDeclaration]).members, nodeOrChildIsDecorated )
  case  SyntaxKind.MethodDeclaration | SyntaxKind.SetAccessor  =>
return forEach( (node.asInstanceOf[FunctionLikeDeclaration]).parameters, nodeIsDecorated )
  case _ =>
}

}
def isJSXTagName(node: Node) = {
 val parent = node.parent
if ((((parent.kind===SyntaxKind.JsxOpeningElement)||(parent.kind===SyntaxKind.JsxSelfClosingElement))||(parent.kind===SyntaxKind.JsxClosingElement))) {
 return ((parent.asInstanceOf[JsxOpeningLikeElement]).tagName===node)

}
return false

}
def isPartOfExpression(node: Node): Boolean = {
 node.kind match {
  case  SyntaxKind.ThisKeyword | SyntaxKind.SuperKeyword | SyntaxKind.NullKeyword | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword | SyntaxKind.RegularExpressionLiteral | SyntaxKind.ArrayLiteralExpression | SyntaxKind.ObjectLiteralExpression | SyntaxKind.PropertyAccessExpression | SyntaxKind.ElementAccessExpression | SyntaxKind.CallExpression | SyntaxKind.NewExpression | SyntaxKind.TaggedTemplateExpression | SyntaxKind.AsExpression | SyntaxKind.TypeAssertionExpression | SyntaxKind.NonNullExpression | SyntaxKind.ParenthesizedExpression | SyntaxKind.FunctionExpression | SyntaxKind.ClassExpression | SyntaxKind.ArrowFunction | SyntaxKind.VoidExpression | SyntaxKind.DeleteExpression | SyntaxKind.TypeOfExpression | SyntaxKind.PrefixUnaryExpression | SyntaxKind.PostfixUnaryExpression | SyntaxKind.BinaryExpression | SyntaxKind.ConditionalExpression | SyntaxKind.SpreadElementExpression | SyntaxKind.TemplateExpression | SyntaxKind.NoSubstitutionTemplateLiteral | SyntaxKind.OmittedExpression | SyntaxKind.JsxElement | SyntaxKind.JsxSelfClosingElement | SyntaxKind.YieldExpression | SyntaxKind.AwaitExpression  =>
return true
  case  SyntaxKind.QualifiedName  =>
while ((node.parent.kind===SyntaxKind.QualifiedName)) {
{
 (node=node.parent)

}
}
return ((node.parent.kind===SyntaxKind.TypeQuery)||isJSXTagName( node ))
  case  SyntaxKind.Identifier  =>
if (((node.parent.kind===SyntaxKind.TypeQuery)||isJSXTagName( node ))) {
 return true

}
  case  SyntaxKind.NumericLiteral | SyntaxKind.StringLiteral | SyntaxKind.ThisKeyword  =>
var parent = node.parent
parent.kind match {
  case  SyntaxKind.VariableDeclaration | SyntaxKind.Parameter | SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.EnumMember | SyntaxKind.PropertyAssignment | SyntaxKind.BindingElement  =>
return ((parent.asInstanceOf[VariableLikeDeclaration]).initializer===node)
  case  SyntaxKind.ExpressionStatement | SyntaxKind.IfStatement | SyntaxKind.DoStatement | SyntaxKind.WhileStatement | SyntaxKind.ReturnStatement | SyntaxKind.WithStatement | SyntaxKind.SwitchStatement | SyntaxKind.CaseClause | SyntaxKind.ThrowStatement | SyntaxKind.SwitchStatement  =>
return ((parent.asInstanceOf[ExpressionStatement]).expression===node)
  case  SyntaxKind.ForStatement  =>
var forStatement = parent.asInstanceOf[ForStatement]
return (((((forStatement.initializer===node)&&(forStatement.initializer.kind!==SyntaxKind.VariableDeclarationList)))||(forStatement.condition===node))||(forStatement.incrementor===node))
  case  SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement  =>
var forInStatement = parent.asInstanceOf[( ForInStatement | ForOfStatement )]
return ((((forInStatement.initializer===node)&&(forInStatement.initializer.kind!==SyntaxKind.VariableDeclarationList)))||(forInStatement.expression===node))
  case  SyntaxKind.TypeAssertionExpression | SyntaxKind.AsExpression  =>
return (node===(parent.asInstanceOf[AssertionExpression]).expression)
  case  SyntaxKind.TemplateSpan  =>
return (node===(parent.asInstanceOf[TemplateSpan]).expression)
  case  SyntaxKind.ComputedPropertyName  =>
return (node===(parent.asInstanceOf[ComputedPropertyName]).expression)
  case  SyntaxKind.Decorator | SyntaxKind.JsxExpression | SyntaxKind.JsxSpreadAttribute  =>
return true
  case  SyntaxKind.ExpressionWithTypeArguments  =>
return (((parent.asInstanceOf[ExpressionWithTypeArguments]).expression===node)&&isExpressionWithTypeArgumentsInClassExtendsClause( parent ))
  case _ =>
if (isPartOfExpression( parent )) {
 return true

}
}
  case _ =>
}
return false

}
def isInstantiatedModule(node: ModuleDeclaration, preserveConstEnums: Boolean) = {
 val moduleState = getModuleInstanceState( node )
return ((moduleState===ModuleInstanceState.Instantiated)||((preserveConstEnums&&(moduleState===ModuleInstanceState.ConstEnumOnly))))

}
def isExternalModuleImportEqualsDeclaration(node: Node) = {
 return ((node.kind===SyntaxKind.ImportEqualsDeclaration)&&((node.asInstanceOf[ImportEqualsDeclaration]).moduleReference.kind===SyntaxKind.ExternalModuleReference))

}
def getExternalModuleImportEqualsDeclarationExpression(node: Node) = {
 Debug.assert( isExternalModuleImportEqualsDeclaration( node ) )
return ((node.asInstanceOf[ImportEqualsDeclaration]).moduleReference.asInstanceOf[ExternalModuleReference]).expression

}
def isInternalModuleImportEqualsDeclaration(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.ImportEqualsDeclaration)&&((node.asInstanceOf[ImportEqualsDeclaration]).moduleReference.kind!==SyntaxKind.ExternalModuleReference))

}
def isSourceFileJavaScript(file: SourceFile): Boolean = {
 return isInJavaScriptFile( file )

}
def isInJavaScriptFile(node: Node): Boolean = {
 return (node&&(!(!((node.flags&NodeFlags.JavaScriptFile)))))

}
def isRequireCall(expression: Node, checkArgumentIsStringLiteral: Boolean): Boolean = {
 val isRequire = ((((expression.kind===SyntaxKind.CallExpression)&&((expression.asInstanceOf[CallExpression]).expression.kind===SyntaxKind.Identifier))&&(((expression.asInstanceOf[CallExpression]).expression.asInstanceOf[Identifier]).text==="require"))&&((expression.asInstanceOf[CallExpression]).arguments.length===1))
return (isRequire&&(((!checkArgumentIsStringLiteral)||((expression.asInstanceOf[CallExpression]).arguments(0).kind===SyntaxKind.StringLiteral))))

}
def isSingleOrDoubleQuote(charCode: Int) = {
 return ((charCode===CharacterCodes.singleQuote)||(charCode===CharacterCodes.doubleQuote))

}
def isDeclarationOfFunctionExpression(s: Symbol) = {
 if ((s.valueDeclaration&&(s.valueDeclaration.kind===SyntaxKind.VariableDeclaration))) {
 val declaration = s.valueDeclaration.asInstanceOf[VariableDeclaration]
return (declaration.initializer&&(declaration.initializer.kind===SyntaxKind.FunctionExpression))

}
return false

}
def getSpecialPropertyAssignmentKind(expression: Node): SpecialPropertyAssignmentKind = {
 if ((!isInJavaScriptFile( expression ))) {
 return SpecialPropertyAssignmentKind.None

}
if ((expression.kind!==SyntaxKind.BinaryExpression)) {
 return SpecialPropertyAssignmentKind.None

}
val expr = expression.asInstanceOf[BinaryExpression]
if (((expr.operatorToken.kind!==SyntaxKind.EqualsToken)||(expr.left.kind!==SyntaxKind.PropertyAccessExpression))) {
 return SpecialPropertyAssignmentKind.None

}
val lhs = expr.left.asInstanceOf[PropertyAccessExpression]
if ((lhs.expression.kind===SyntaxKind.Identifier)) {
 val lhsId = lhs.expression.asInstanceOf[Identifier]
if ((lhsId.text==="exports")) {
 return SpecialPropertyAssignmentKind.ExportsProperty

}
else if (((lhsId.text==="module")&&(lhs.name.text==="exports"))) {
 return SpecialPropertyAssignmentKind.ModuleExports

}

}
else if ((lhs.expression.kind===SyntaxKind.ThisKeyword)) {
 return SpecialPropertyAssignmentKind.ThisProperty

}
else if ((lhs.expression.kind===SyntaxKind.PropertyAccessExpression)) {
 val innerPropertyAccess = lhs.expression.asInstanceOf[PropertyAccessExpression]
if ((innerPropertyAccess.expression.kind===SyntaxKind.Identifier)) {
 val innerPropertyAccessIdentifier = innerPropertyAccess.expression.asInstanceOf[Identifier]
if (((innerPropertyAccessIdentifier.text==="module")&&(innerPropertyAccess.name.text==="exports"))) {
 return SpecialPropertyAssignmentKind.ExportsProperty

}
if ((innerPropertyAccess.name.text==="prototype")) {
 return SpecialPropertyAssignmentKind.PrototypeProperty

}

}

}
return SpecialPropertyAssignmentKind.None

}
def getExternalModuleName(node: Node): Expression = {
 if ((node.kind===SyntaxKind.ImportDeclaration)) {
 return (node.asInstanceOf[ImportDeclaration]).moduleSpecifier

}
if ((node.kind===SyntaxKind.ImportEqualsDeclaration)) {
 val reference = (node.asInstanceOf[ImportEqualsDeclaration]).moduleReference
if ((reference.kind===SyntaxKind.ExternalModuleReference)) {
 return (reference.asInstanceOf[ExternalModuleReference]).expression

}

}
if ((node.kind===SyntaxKind.ExportDeclaration)) {
 return (node.asInstanceOf[ExportDeclaration]).moduleSpecifier

}
if (((node.kind===SyntaxKind.ModuleDeclaration)&&((node.asInstanceOf[ModuleDeclaration]).name.kind===SyntaxKind.StringLiteral))) {
 return (node.asInstanceOf[ModuleDeclaration]).name

}

}
def getNamespaceDeclarationNode(node: ( ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration )): ( ImportEqualsDeclaration | NamespaceImport ) = {
 if ((node.kind===SyntaxKind.ImportEqualsDeclaration)) {
 return node.asInstanceOf[ImportEqualsDeclaration]

}
val importClause = (node.asInstanceOf[ImportDeclaration]).importClause
if (((importClause&&importClause.namedBindings)&&(importClause.namedBindings.kind===SyntaxKind.NamespaceImport))) {
 return importClause.namedBindings.asInstanceOf[NamespaceImport]

}

}
def isDefaultImport(node: ( ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration )) = {
 return (((node.kind===SyntaxKind.ImportDeclaration)&&(node.asInstanceOf[ImportDeclaration]).importClause)&&(!(!(node.asInstanceOf[ImportDeclaration]).importClause.name)))

}
def hasQuestionToken(node: Node) = {
 if (node) {
 node.kind match {
  case  SyntaxKind.Parameter | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.ShorthandPropertyAssignment | SyntaxKind.PropertyAssignment | SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature  =>
return ((node.asInstanceOf[( ParameterDeclaration | MethodDeclaration | PropertyDeclaration )]).questionToken!==undefined)
  case _ =>
}

}
return false

}
def isJSDocConstructSignature(node: Node) = {
 return (((node.kind===SyntaxKind.JSDocFunctionType)&&((node.asInstanceOf[JSDocFunctionType]).parameters.length>0))&&((node.asInstanceOf[JSDocFunctionType]).parameters(0).`type`.kind===SyntaxKind.JSDocConstructorType))

}
def getJSDocTag(node: Node, kind: SyntaxKind, checkParentVariableStatement: Boolean): JSDocTag = {
 if ((!node)) {
 return undefined

}
val jsDocTags = getJSDocTags( node, checkParentVariableStatement )
if ((!jsDocTags)) {
 return undefined

}
(jsDocTags).foreach { fresh4 =>
val tag = zeroOfMyType
 = fresh4
 {
 if ((tag.kind===kind)) {
 return tag

}

}
}

}
def append[T](previous: ( Array[T] | undefined ), additional: ( Array[T] | undefined )): ( Array[T] | undefined ) = {
 if (additional) {
 if ((!previous)) {
 (previous=Array())

}
(additional).foreach { fresh5 =>
val x = zeroOfMyType
 = fresh5
 {
 previous.push( x )

}
}

}
return previous

}
def getJSDocComments(node: Node, checkParentVariableStatement: Boolean): Array[String] = {
 return getJSDocs( node, checkParentVariableStatement, (docs =>  map( docs, (doc =>  doc.comment) )), (tags =>  map( tags, (tag =>  tag.comment) )) )

}
def getJSDocTags(node: Node, checkParentVariableStatement: Boolean): Array[JSDocTag] = {
 return getJSDocs( node, checkParentVariableStatement, (docs =>  {
 val result: Array[JSDocTag] = Array()
 (docs).foreach { fresh6 =>
val doc = zeroOfMyType
 = fresh6
 {
 if (doc.tags) {
 result.push( doc.tags: _* )

}

}
}
 return result

}), (tags =>  tags) )

}
def getJSDocs[T](node: Node, checkParentVariableStatement: Boolean, getDocs: ((Array[JSDoc]) => Array[T]), getTags: ((Array[JSDocTag]) => Array[T])): Array[T] = {
 var result: Array[T] = undefined
if (checkParentVariableStatement) {
 val isInitializerOfVariableDeclarationInStatement = ((isVariableLike( node.parent )&&((node.parent).initializer===node))&&(node.parent.parent.parent.kind===SyntaxKind.VariableStatement))
val isVariableOfVariableDeclarationStatement = (isVariableLike( node )&&(node.parent.parent.kind===SyntaxKind.VariableStatement))
val variableStatementNode = (if (isInitializerOfVariableDeclarationInStatement) node.parent.parent.parent else (if (isVariableOfVariableDeclarationStatement) node.parent.parent else undefined))
if (variableStatementNode) {
 (result=append( result, getJSDocs( variableStatementNode, checkParentVariableStatement, getDocs, getTags ) ))

}
if ((((node.kind===SyntaxKind.ModuleDeclaration)&&node.parent)&&(node.parent.kind===SyntaxKind.ModuleDeclaration))) {
 (result=append( result, getJSDocs( node.parent, checkParentVariableStatement, getDocs, getTags ) ))

}
val parent = node.parent
val isSourceOfAssignmentExpressionStatement = ((((parent&&parent.parent)&&(parent.kind===SyntaxKind.BinaryExpression))&&((parent.asInstanceOf[BinaryExpression]).operatorToken.kind===SyntaxKind.EqualsToken))&&(parent.parent.kind===SyntaxKind.ExpressionStatement))
if (isSourceOfAssignmentExpressionStatement) {
 (result=append( result, getJSDocs( parent.parent, checkParentVariableStatement, getDocs, getTags ) ))

}
val isPropertyAssignmentExpression = (parent&&(parent.kind===SyntaxKind.PropertyAssignment))
if (isPropertyAssignmentExpression) {
 (result=append( result, getJSDocs( parent, checkParentVariableStatement, getDocs, getTags ) ))

}
if ((node.kind===SyntaxKind.Parameter)) {
 val paramTags = getJSDocParameterTag( node.asInstanceOf[ParameterDeclaration], checkParentVariableStatement )
if (paramTags) {
 (result=append( result, getTags( paramTags ) ))

}

}

}
if ((isVariableLike( node )&&node.initializer)) {
 (result=append( result, getJSDocs( node.initializer, false, getDocs, getTags ) ))

}
if (node.jsDocComments) {
 if (result) {
 (result=append( result, getDocs( node.jsDocComments ) ))

}
else {
 return getDocs( node.jsDocComments )

}

}
return result

}
def getJSDocParameterTag(param: ParameterDeclaration, checkParentVariableStatement: Boolean): Array[JSDocTag] = {
 val func = param.parent.asInstanceOf[FunctionLikeDeclaration]
val tags = getJSDocTags( func, checkParentVariableStatement )
if ((!param.name)) {
 val i = func.parameters.indexOf( param )
val paramTags = filter( tags, (tag =>  (tag.kind===SyntaxKind.JSDocParameterTag)) )
if (((paramTags&&(0<=i))&&(i<paramTags.length))) {
 return Array( paramTags(i) )

}

}
else if ((param.name.kind===SyntaxKind.Identifier)) {
 val name = (param.name.asInstanceOf[Identifier]).text
val paramTags = filter( tags, (tag =>  ((tag.kind===SyntaxKind.JSDocParameterTag)&&((tag.asInstanceOf[JSDocParameterTag]).parameterName.text===name))) )
if (paramTags) {
 return paramTags

}

}
else {
 return undefined

}

}
def getJSDocTypeTag(node: Node): JSDocTypeTag = {
 return getJSDocTag( node, SyntaxKind.JSDocTypeTag, false ).asInstanceOf[JSDocTypeTag]

}
def getJSDocReturnTag(node: Node): JSDocReturnTag = {
 return getJSDocTag( node, SyntaxKind.JSDocReturnTag, true ).asInstanceOf[JSDocReturnTag]

}
def getJSDocTemplateTag(node: Node): JSDocTemplateTag = {
 return getJSDocTag( node, SyntaxKind.JSDocTemplateTag, false ).asInstanceOf[JSDocTemplateTag]

}
def getCorrespondingJSDocParameterTag(parameter: ParameterDeclaration): JSDocParameterTag = {
 if ((parameter.name&&(parameter.name.kind===SyntaxKind.Identifier))) {
 val parameterName = (parameter.name.asInstanceOf[Identifier]).text
val jsDocTags = getJSDocTags( parameter.parent, true )
if ((!jsDocTags)) {
 return undefined

}
(jsDocTags).foreach { fresh7 =>
val tag = zeroOfMyType
 = fresh7
 {
 if ((tag.kind===SyntaxKind.JSDocParameterTag)) {
 val parameterTag = tag.asInstanceOf[JSDocParameterTag]
if ((parameterTag.parameterName.text===parameterName)) {
 return parameterTag

}

}

}
}

}
return undefined

}
def hasRestParameter(s: SignatureDeclaration): Boolean = {
 return isRestParameter( lastOrUndefined( s.parameters ) )

}
def hasDeclaredRestParameter(s: SignatureDeclaration): Boolean = {
 return isDeclaredRestParam( lastOrUndefined( s.parameters ) )

}
def isRestParameter(node: ParameterDeclaration) = {
 if ((node&&((node.flags&NodeFlags.JavaScriptFile)))) {
 if ((node.`type`&&(node.`type`.kind===SyntaxKind.JSDocVariadicType))) {
 return true

}
val paramTag = getCorrespondingJSDocParameterTag( node )
if ((paramTag&&paramTag.typeExpression)) {
 return (paramTag.typeExpression.`type`.kind===SyntaxKind.JSDocVariadicType)

}

}
return isDeclaredRestParam( node )

}
def isDeclaredRestParam(node: ParameterDeclaration) = {
 return (node&&(node.dotDotDotToken!==undefined))

}
def isAssignmentTarget(node: Node): Boolean = {
 while ((node.parent.kind===SyntaxKind.ParenthesizedExpression)) {
{
 (node=node.parent)

}
}
while (true) {
{
 val parent = node.parent
if (((parent.kind===SyntaxKind.ArrayLiteralExpression)||(parent.kind===SyntaxKind.SpreadElementExpression))) {
 (node=parent)
continue

}
if (((parent.kind===SyntaxKind.PropertyAssignment)||(parent.kind===SyntaxKind.ShorthandPropertyAssignment))) {
 (node=parent.parent)
continue

}
return ((((parent.kind===SyntaxKind.BinaryExpression)&&isAssignmentOperator( (parent.asInstanceOf[BinaryExpression]).operatorToken.kind ))&&((parent.asInstanceOf[BinaryExpression]).left===node))||((((parent.kind===SyntaxKind.ForInStatement)||(parent.kind===SyntaxKind.ForOfStatement)))&&((parent.asInstanceOf[( ForInStatement | ForOfStatement )]).initializer===node)))

}
}

}
def isNodeDescendantOf(node: Node, ancestor: Node): Boolean = {
 while (node) {
{
 if ((node===ancestor))
return true
(node=node.parent)

}
}
return false

}
def isInAmbientContext(node: Node): Boolean = {
 while (node) {
{
 if ((hasModifier( node, ModifierFlags.Ambient )||(((node.kind===SyntaxKind.SourceFile)&&(node.asInstanceOf[SourceFile]).isDeclarationFile)))) {
 return true

}
(node=node.parent)

}
}
return false

}
def isDeclarationName(name: Node): Boolean = {
 if ((((name.kind!==SyntaxKind.Identifier)&&(name.kind!==SyntaxKind.StringLiteral))&&(name.kind!==SyntaxKind.NumericLiteral))) {
 return false

}
val parent = name.parent
if (((parent.kind===SyntaxKind.ImportSpecifier)||(parent.kind===SyntaxKind.ExportSpecifier))) {
 if ((parent.asInstanceOf[ImportOrExportSpecifier]).propertyName) {
 return true

}

}
if (isDeclaration( parent )) {
 return ((parent.asInstanceOf[Declaration]).name===name)

}
return false

}
def isLiteralComputedPropertyDeclarationName(node: Node) = {
 return (((((node.kind===SyntaxKind.StringLiteral)||(node.kind===SyntaxKind.NumericLiteral)))&&(node.parent.kind===SyntaxKind.ComputedPropertyName))&&isDeclaration( node.parent.parent ))

}
def isIdentifierName(node: Identifier): Boolean = {
 var parent = node.parent
parent.kind match {
  case  SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.EnumMember | SyntaxKind.PropertyAssignment | SyntaxKind.PropertyAccessExpression  =>
return ((parent.asInstanceOf[( Declaration | PropertyAccessExpression )]).name===node)
  case  SyntaxKind.QualifiedName  =>
if (((parent.asInstanceOf[QualifiedName]).right===node)) {
 while ((parent.kind===SyntaxKind.QualifiedName)) {
{
 (parent=parent.parent)

}
}
return (parent.kind===SyntaxKind.TypeQuery)

}
return false
  case  SyntaxKind.BindingElement | SyntaxKind.ImportSpecifier  =>
return ((parent.asInstanceOf[( BindingElement | ImportSpecifier )]).propertyName===node)
  case  SyntaxKind.ExportSpecifier  =>
return true
  case _ =>
}
return false

}
def isAliasSymbolDeclaration(node: Node): Boolean = {
 return (((((((node.kind===SyntaxKind.ImportEqualsDeclaration)||(node.kind===SyntaxKind.NamespaceExportDeclaration))||((node.kind===SyntaxKind.ImportClause)&&(!(!(node.asInstanceOf[ImportClause]).name))))||(node.kind===SyntaxKind.NamespaceImport))||(node.kind===SyntaxKind.ImportSpecifier))||(node.kind===SyntaxKind.ExportSpecifier))||((node.kind===SyntaxKind.ExportAssignment)&&exportAssignmentIsAlias( node.asInstanceOf[ExportAssignment] )))

}
def exportAssignmentIsAlias(node: ExportAssignment): Boolean = {
 return isEntityNameExpression( node.expression )

}
def getClassExtendsHeritageClauseElement(node: ( ClassLikeDeclaration | InterfaceDeclaration )) = {
 val heritageClause = getHeritageClause( node.heritageClauses, SyntaxKind.ExtendsKeyword )
return (if ((heritageClause&&(heritageClause.types.length>0))) heritageClause.types(0) else undefined)

}
def getClassImplementsHeritageClauseElements(node: ClassLikeDeclaration) = {
 val heritageClause = getHeritageClause( node.heritageClauses, SyntaxKind.ImplementsKeyword )
return (if (heritageClause) heritageClause.types else undefined)

}
def getInterfaceBaseTypeNodes(node: InterfaceDeclaration) = {
 val heritageClause = getHeritageClause( node.heritageClauses, SyntaxKind.ExtendsKeyword )
return (if (heritageClause) heritageClause.types else undefined)

}
def getHeritageClause(clauses: NodeArray[HeritageClause], kind: SyntaxKind) = {
 if (clauses) {
 (clauses).foreach { fresh8 =>
val clause = zeroOfMyType
 = fresh8
 {
 if ((clause.token===kind)) {
 return clause

}

}
}

}
return undefined

}
def tryResolveScriptReference(host: ScriptReferenceHost, sourceFile: SourceFile, reference: FileReference) = {
 if ((!host.getCompilerOptions().noResolve)) {
 val referenceFileName = (if (isRootedDiskPath( reference.fileName )) reference.fileName else combinePaths( getDirectoryPath( sourceFile.fileName ), reference.fileName ))
return host.getSourceFile( referenceFileName )

}

}
def getAncestor(node: Node, kind: SyntaxKind): Node = {
 while (node) {
{
 if ((node.kind===kind)) {
 return node

}
(node=node.parent)

}
}
return undefined

}
def getFileReferenceFromReferencePath(comment: String, commentRange: CommentRange): ReferencePathMatchResult = {
 val simpleReferenceRegEx = java.util.regex.Pattern.compile(raw"""^\/\/\/\s*<reference\s+""", "gim")
val isNoDefaultLibRegEx = java.util.regex.Pattern.compile(raw"""^(\/\/\/\s*<reference\s+no-default-lib\s*=\s*)('|")(.+?)\2\s*\/>""", "gim")
if (simpleReferenceRegEx.test( comment )) {
 if (isNoDefaultLibRegEx.test( comment )) {
 return Map( "isNoDefaultLib" -> true )

}
else {
 val refMatchResult = fullTripleSlashReferencePathRegEx.exec( comment )
val refLibResult = ((!refMatchResult)&&fullTripleSlashReferenceTypeReferenceDirectiveRegEx.exec( comment ))
if ((refMatchResult||refLibResult)) {
 val start = commentRange.pos
val end = commentRange.end
return Map( "fileReference" -> Map( "pos" -> start,
"end" -> end,
"fileName" -> ((refMatchResult||refLibResult))(3) ),
"isNoDefaultLib" -> false,
"isTypeReferenceDirective" -> (!(!refLibResult)) )

}
return Map( "diagnosticMessage" -> Diagnostics.Invalid_reference_directive_syntax,
"isNoDefaultLib" -> false )

}

}
return undefined

}
def isKeyword(token: SyntaxKind): Boolean = {
 return ((SyntaxKind.FirstKeyword<=token)&&(token<=SyntaxKind.LastKeyword))

}
def isTrivia(token: SyntaxKind) = {
 return ((SyntaxKind.FirstTriviaToken<=token)&&(token<=SyntaxKind.LastTriviaToken))

}
def isAsyncFunctionLike(node: Node): Boolean = {
 return ((isFunctionLike( node )&&hasModifier( node, ModifierFlags.Async ))&&(!isAccessor( node )))

}
def isStringOrNumericLiteral(kind: SyntaxKind): Boolean = {
 return ((kind===SyntaxKind.StringLiteral)||(kind===SyntaxKind.NumericLiteral))

}
def hasDynamicName(declaration: Declaration): Boolean = {
 return (declaration.name&&isDynamicName( declaration.name ))

}
def isDynamicName(name: DeclarationName): Boolean = {
 return (((name.kind===SyntaxKind.ComputedPropertyName)&&(!isStringOrNumericLiteral( (name.asInstanceOf[ComputedPropertyName]).expression.kind )))&&(!isWellKnownSymbolSyntactically( (name.asInstanceOf[ComputedPropertyName]).expression )))

}
def isWellKnownSymbolSyntactically(node: Expression): Boolean = {
 return (isPropertyAccessExpression( node )&&isESSymbolIdentifier( node.expression ))

}
def getPropertyNameForPropertyNameNode(name: DeclarationName): String = {
 if (((((name.kind===SyntaxKind.Identifier)||(name.kind===SyntaxKind.StringLiteral))||(name.kind===SyntaxKind.NumericLiteral))||(name.kind===SyntaxKind.Parameter))) {
 return (name.asInstanceOf[( Identifier | LiteralExpression )]).text

}
if ((name.kind===SyntaxKind.ComputedPropertyName)) {
 val nameExpression = (name.asInstanceOf[ComputedPropertyName]).expression
if (isWellKnownSymbolSyntactically( nameExpression )) {
 val rightHandSideName = (nameExpression.asInstanceOf[PropertyAccessExpression]).name.text
return getPropertyNameForKnownSymbolName( rightHandSideName )

}
else if (((nameExpression.kind===SyntaxKind.StringLiteral)||(nameExpression.kind===SyntaxKind.NumericLiteral))) {
 return (nameExpression.asInstanceOf[LiteralExpression]).text

}

}
return undefined

}
def getPropertyNameForKnownSymbolName(symbolName: String): String = {
 return ("__@"+symbolName)

}
def isESSymbolIdentifier(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.Identifier)&&((node.asInstanceOf[Identifier]).text==="Symbol"))

}
def isPushOrUnshiftIdentifier(node: Identifier) = {
 return ((node.text==="push")||(node.text==="unshift"))

}
def isModifierKind(token: SyntaxKind): Boolean = {
 token match {
  case  SyntaxKind.AbstractKeyword | SyntaxKind.AsyncKeyword | SyntaxKind.ConstKeyword | SyntaxKind.DeclareKeyword | SyntaxKind.DefaultKeyword | SyntaxKind.ExportKeyword | SyntaxKind.PublicKeyword | SyntaxKind.PrivateKeyword | SyntaxKind.ProtectedKeyword | SyntaxKind.ReadonlyKeyword | SyntaxKind.StaticKeyword  =>
return true
  case _ =>
}
return false

}
def isParameterDeclaration(node: VariableLikeDeclaration) = {
 val root = getRootDeclaration( node )
return (root.kind===SyntaxKind.Parameter)

}
def getRootDeclaration(node: Node): Node = {
 while ((node.kind===SyntaxKind.BindingElement)) {
{
 (node=node.parent.parent)

}
}
return node

}
def nodeStartsNewLexicalEnvironment(node: Node): Boolean = {
 val kind = node.kind
return (((((((((kind===SyntaxKind.Constructor)||(kind===SyntaxKind.FunctionExpression))||(kind===SyntaxKind.FunctionDeclaration))||(kind===SyntaxKind.ArrowFunction))||(kind===SyntaxKind.MethodDeclaration))||(kind===SyntaxKind.GetAccessor))||(kind===SyntaxKind.SetAccessor))||(kind===SyntaxKind.ModuleDeclaration))||(kind===SyntaxKind.SourceFile))

}
def nodeIsSynthesized(node: TextRange): Boolean = {
 return (positionIsSynthesized( node.pos )||positionIsSynthesized( node.end ))

}
def getOriginalNode(node: Node): Node = {
 if (node) {
 while ((node.original!==undefined)) {
{
 (node=node.original)

}
}

}
return node

}
def isParseTreeNode(node: Node): Boolean = {
 return (((node.flags&NodeFlags.Synthesized))===0)

}
def getParseTreeNode(node: Node): Node
def getParseTreeNode[T <: Node](node: Node, nodeTest: ((Node) => Boolean)): T
def getParseTreeNode(node: Node, nodeTest: ((Node) => Boolean)): Node = {
 if (isParseTreeNode( node )) {
 return node

}
(node=getOriginalNode( node ))
if ((isParseTreeNode( node )&&(((!nodeTest)||nodeTest( node ))))) {
 return node

}
return undefined

}
def getOriginalSourceFiles(sourceFiles: Array[SourceFile]) = {
 val originalSourceFiles: Array[SourceFile] = Array()
(sourceFiles).foreach { fresh9 =>
val sourceFile = zeroOfMyType
 = fresh9
 {
 val originalSourceFile = getParseTreeNode( sourceFile, isSourceFile )
if (originalSourceFile) {
 originalSourceFiles.push( originalSourceFile )

}

}
}
return originalSourceFiles

}
def getOriginalNodeId(node: Node) = {
 (node=getOriginalNode( node ))
return (if (node) getNodeId( node ) else 0)

}
sealed abstract class Associativity
object Associativity {
   case object Left extends Associativity
  case object Right extends Associativity
}
def getExpressionAssociativity(expression: Expression) = {
 val operator = getOperator( expression )
val hasArguments = ((expression.kind===SyntaxKind.NewExpression)&&((expression.asInstanceOf[NewExpression]).arguments!==undefined))
return getOperatorAssociativity( expression.kind, operator, hasArguments )

}
def getOperatorAssociativity(kind: SyntaxKind, operator: SyntaxKind, hasArguments: Boolean) = {
 kind match {
  case  SyntaxKind.NewExpression  =>
return (if (hasArguments) Associativity.Left else Associativity.Right)
  case  SyntaxKind.PrefixUnaryExpression | SyntaxKind.TypeOfExpression | SyntaxKind.VoidExpression | SyntaxKind.DeleteExpression | SyntaxKind.AwaitExpression | SyntaxKind.ConditionalExpression | SyntaxKind.YieldExpression  =>
return Associativity.Right
  case  SyntaxKind.BinaryExpression  =>
operator match {
  case  SyntaxKind.AsteriskAsteriskToken | SyntaxKind.EqualsToken | SyntaxKind.PlusEqualsToken | SyntaxKind.MinusEqualsToken | SyntaxKind.AsteriskAsteriskEqualsToken | SyntaxKind.AsteriskEqualsToken | SyntaxKind.SlashEqualsToken | SyntaxKind.PercentEqualsToken | SyntaxKind.LessThanLessThanEqualsToken | SyntaxKind.GreaterThanGreaterThanEqualsToken | SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken | SyntaxKind.AmpersandEqualsToken | SyntaxKind.CaretEqualsToken | SyntaxKind.BarEqualsToken  =>
return Associativity.Right
  case _ =>
}
  case _ =>
}
return Associativity.Left

}
def getExpressionPrecedence(expression: Expression) = {
 val operator = getOperator( expression )
val hasArguments = ((expression.kind===SyntaxKind.NewExpression)&&((expression.asInstanceOf[NewExpression]).arguments!==undefined))
return getOperatorPrecedence( expression.kind, operator, hasArguments )

}
def getOperator(expression: Expression) = {
 if ((expression.kind===SyntaxKind.BinaryExpression)) {
 return (expression.asInstanceOf[BinaryExpression]).operatorToken.kind

}
else if (((expression.kind===SyntaxKind.PrefixUnaryExpression)||(expression.kind===SyntaxKind.PostfixUnaryExpression))) {
 return (expression.asInstanceOf[( PrefixUnaryExpression | PostfixUnaryExpression )]).operator

}
else {
 return expression.kind

}

}
def getOperatorPrecedence(nodeKind: SyntaxKind, operatorKind: SyntaxKind, hasArguments: Boolean) = {
 nodeKind match {
  case  SyntaxKind.ThisKeyword | SyntaxKind.SuperKeyword | SyntaxKind.Identifier | SyntaxKind.NullKeyword | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword | SyntaxKind.NumericLiteral | SyntaxKind.StringLiteral | SyntaxKind.ArrayLiteralExpression | SyntaxKind.ObjectLiteralExpression | SyntaxKind.FunctionExpression | SyntaxKind.ArrowFunction | SyntaxKind.ClassExpression | SyntaxKind.JsxElement | SyntaxKind.JsxSelfClosingElement | SyntaxKind.RegularExpressionLiteral | SyntaxKind.NoSubstitutionTemplateLiteral | SyntaxKind.TemplateExpression | SyntaxKind.ParenthesizedExpression | SyntaxKind.OmittedExpression  =>
return 19
  case  SyntaxKind.TaggedTemplateExpression | SyntaxKind.PropertyAccessExpression | SyntaxKind.ElementAccessExpression  =>
return 18
  case  SyntaxKind.NewExpression  =>
return (if (hasArguments) 18 else 17)
  case  SyntaxKind.CallExpression  =>
return 17
  case  SyntaxKind.PostfixUnaryExpression  =>
return 16
  case  SyntaxKind.PrefixUnaryExpression | SyntaxKind.TypeOfExpression | SyntaxKind.VoidExpression | SyntaxKind.DeleteExpression | SyntaxKind.AwaitExpression  =>
return 15
  case  SyntaxKind.BinaryExpression  =>
operatorKind match {
  case  SyntaxKind.ExclamationToken | SyntaxKind.TildeToken  =>
return 15
  case  SyntaxKind.AsteriskAsteriskToken | SyntaxKind.AsteriskToken | SyntaxKind.SlashToken | SyntaxKind.PercentToken  =>
return 14
  case  SyntaxKind.PlusToken | SyntaxKind.MinusToken  =>
return 13
  case  SyntaxKind.LessThanLessThanToken | SyntaxKind.GreaterThanGreaterThanToken | SyntaxKind.GreaterThanGreaterThanGreaterThanToken  =>
return 12
  case  SyntaxKind.LessThanToken | SyntaxKind.LessThanEqualsToken | SyntaxKind.GreaterThanToken | SyntaxKind.GreaterThanEqualsToken | SyntaxKind.InKeyword | SyntaxKind.InstanceOfKeyword  =>
return 11
  case  SyntaxKind.EqualsEqualsToken | SyntaxKind.EqualsEqualsEqualsToken | SyntaxKind.ExclamationEqualsToken | SyntaxKind.ExclamationEqualsEqualsToken  =>
return 10
  case  SyntaxKind.AmpersandToken  =>
return 9
  case  SyntaxKind.CaretToken  =>
return 8
  case  SyntaxKind.BarToken  =>
return 7
  case  SyntaxKind.AmpersandAmpersandToken  =>
return 6
  case  SyntaxKind.BarBarToken  =>
return 5
  case  SyntaxKind.EqualsToken | SyntaxKind.PlusEqualsToken | SyntaxKind.MinusEqualsToken | SyntaxKind.AsteriskAsteriskEqualsToken | SyntaxKind.AsteriskEqualsToken | SyntaxKind.SlashEqualsToken | SyntaxKind.PercentEqualsToken | SyntaxKind.LessThanLessThanEqualsToken | SyntaxKind.GreaterThanGreaterThanEqualsToken | SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken | SyntaxKind.AmpersandEqualsToken | SyntaxKind.CaretEqualsToken | SyntaxKind.BarEqualsToken  =>
return 3
  case  SyntaxKind.CommaToken  =>
return 0
  case _ =>
return (-1)
}
  case  SyntaxKind.ConditionalExpression  =>
return 4
  case  SyntaxKind.YieldExpression  =>
return 2
  case  SyntaxKind.SpreadElementExpression  =>
return 1
  case _ =>
return (-1)
}

}
def createDiagnosticCollection(): DiagnosticCollection = {
 var nonFileDiagnostics: Array[Diagnostic] = Array()
val fileDiagnostics = createMap[ Array[Diagnostic] ]()
var diagnosticsModified = false
var modificationCount = 0
return Map( "add" -> add,
"getGlobalDiagnostics" -> getGlobalDiagnostics,
"getDiagnostics" -> getDiagnostics,
"getModificationCount" -> getModificationCount,
"reattachFileDiagnostics" -> reattachFileDiagnostics )
def getModificationCount() = {
 return modificationCount

}
def reattachFileDiagnostics(newFile: SourceFile): Unit = {
 if ((!hasProperty( fileDiagnostics, newFile.fileName ))) {
 return

}
(fileDiagnostics(newFile.fileName)).foreach { fresh10 =>
val diagnostic = zeroOfMyType
 = fresh10
 {
 (diagnostic.file=newFile)

}
}

}
def add(diagnostic: Diagnostic): Unit = {
 var diagnostics: Array[Diagnostic] = zeroOfMyType
if (diagnostic.file) {
 (diagnostics=fileDiagnostics(diagnostic.file.fileName))
if ((!diagnostics)) {
 (diagnostics=Array())
(fileDiagnostics(diagnostic.file.fileName)=diagnostics)

}

}
else {
 (diagnostics=nonFileDiagnostics)

}
diagnostics.push( diagnostic )
(diagnosticsModified=true)
(modificationCount+= 1)

}
def getGlobalDiagnostics(): Array[Diagnostic] = {
 sortAndDeduplicate()
return nonFileDiagnostics

}
def getDiagnostics(fileName: String): Array[Diagnostic] = {
 sortAndDeduplicate()
if (fileName) {
 return (fileDiagnostics(fileName)||Array())

}
val allDiagnostics: Array[Diagnostic] = Array()
def pushDiagnostic(d: Diagnostic) = {
 allDiagnostics.push( d )

}
forEach( nonFileDiagnostics, pushDiagnostic )
(fileDiagnostics).keys.foreach { fresh11 =>
val key = zeroOfMyType
 = fresh11
 {
 forEach( fileDiagnostics(key), pushDiagnostic )

}
}
return sortAndDeduplicateDiagnostics( allDiagnostics )

}
def sortAndDeduplicate() = {
 if ((!diagnosticsModified)) {
 return

}
(diagnosticsModified=false)
(nonFileDiagnostics=sortAndDeduplicateDiagnostics( nonFileDiagnostics ))
(fileDiagnostics).keys.foreach { fresh12 =>
val key = zeroOfMyType
 = fresh12
 {
 (fileDiagnostics(key)=sortAndDeduplicateDiagnostics( fileDiagnostics(key) ))

}
}

}

}
val escapedCharsRegExp = java.util.regex.Pattern.compile(raw"""[\\\"\u0000-\u001f\t\v\f\b\r\n\u2028\u2029\u0085]""", "g")
val escapedCharsMap = createMap( Map( "\0" -> "\\0",
"\t" -> "\\t",
"\u000b" -> "\\u000b",
"\f" -> "\\f",
"\b" -> "\\b",
"\r" -> "\\r",
"\n" -> "\\n",
"\\" -> "\\\\",
"\"" -> "\\\"",
"\u2028" -> "\\u2028",
"\u2029" -> "\\u2029",
"\u0085" -> "\\u0085" ) )
def escapeString(s: String): String = {
 (s=(if (escapedCharsRegExp.test( s )) s.replace( escapedCharsRegExp, getReplacement ) else s))
return s
def getReplacement(c: String) = {
 return (escapedCharsMap(c)||get16BitUnicodeEscapeSequence( c.charCodeAt( 0 ) ))

}

}
def isIntrinsicJsxName(name: String) = {
 val ch = name.substr( 0, 1 )
return (ch.toLowerCase()===ch)

}
def get16BitUnicodeEscapeSequence(charCode: Int): String = {
 val hexCharCode = charCode.`toString`( 16 ).toUpperCase()
val paddedHexCode = (("0000"+hexCharCode)).slice( (-4) )
return ("\\u"+paddedHexCode)

}
val nonAsciiCharacters = java.util.regex.Pattern.compile(raw"""[^\u0000-\u007F]""", "g")
def escapeNonAsciiCharacters(s: String): String = {
 return (if (nonAsciiCharacters.test( s )) s.replace( nonAsciiCharacters, (c =>  get16BitUnicodeEscapeSequence( c.charCodeAt( 0 ) )) ) else s)

}
trait EmitTextWriter {
  def write(s: String): Unit
  def writeTextOfNode(text: String, node: Node): Unit
  def writeLine(): Unit
  def increaseIndent(): Unit
  def decreaseIndent(): Unit
  def getText(): String
  def rawWrite(s: String): Unit
  def writeLiteral(s: String): Unit
  def getTextPos(): Int
  def getLine(): Int
  def getColumn(): Int
  def getIndent(): Int
  def isAtStartOfLine(): Boolean
  def reset(): Unit
}
val indentStrings: Array[String] = Array( "", "    " )
def getIndentString(level: Int) = {
 if ((indentStrings(level)===undefined)) {
 (indentStrings(level)=(getIndentString( (level-1) )+indentStrings(1)))

}
return indentStrings(level)

}
def getIndentSize() = {
 return indentStrings(1).length

}
def createTextWriter(newLine: String): EmitTextWriter = {
 var output: String = zeroOfMyType
var indent: Int = zeroOfMyType
var lineStart: Boolean = zeroOfMyType
var lineCount: Int = zeroOfMyType
var linePos: Int = zeroOfMyType
def write(s: String) = {
 if ((s&&s.length)) {
 if (lineStart) {
 (output+=getIndentString( indent ))
(lineStart=false)

}
(output+=s)

}

}
def reset(): Unit = {
 (output="")
(indent=0)
(lineStart=true)
(lineCount=0)
(linePos=0)

}
def rawWrite(s: String) = {
 if ((s!==undefined)) {
 if (lineStart) {
 (lineStart=false)

}
(output+=s)

}

}
def writeLiteral(s: String) = {
 if ((s&&s.length)) {
 write( s )
val lineStartsOfS = computeLineStarts( s )
if ((lineStartsOfS.length>1)) {
 (lineCount=((lineCount+lineStartsOfS.length)-1))
(linePos=((output.length-s.length)+lastOrUndefined( lineStartsOfS )))

}

}

}
def writeLine() = {
 if ((!lineStart)) {
 (output+=newLine)
(lineCount+= 1)
(linePos=output.length)
(lineStart=true)

}

}
def writeTextOfNode(text: String, node: Node) = {
 write( getTextOfNodeFromSourceText( text, node ) )

}
reset()
return Map( "write" -> write,
"rawWrite" -> rawWrite,
"writeTextOfNode" -> writeTextOfNode,
"writeLiteral" -> writeLiteral,
"writeLine" -> writeLine,
"increaseIndent" -> (() =>  {
 (indent+= 1)

}),
"decreaseIndent" -> (() =>  {
 (indent-= 1)

}),
"getIndent" -> (() =>  indent),
"getTextPos" -> (() =>  output.length),
"getLine" -> (() =>  (lineCount+1)),
"getColumn" -> (() =>  (if (lineStart) ((indent*getIndentSize())+1) else ((output.length-linePos)+1))),
"getText" -> (() =>  output),
"isAtStartOfLine" -> (() =>  lineStart),
"reset" -> reset )

}
def getResolvedExternalModuleName(host: EmitHost, file: SourceFile): String = {
 return (file.moduleName||getExternalModuleNameFromPath( host, file.fileName ))

}
def getExternalModuleNameFromDeclaration(host: EmitHost, resolver: EmitResolver, declaration: ( ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration )): String = {
 val file = resolver.getExternalModuleFileFromDeclaration( declaration )
if (((!file)||isDeclarationFile( file ))) {
 return undefined

}
return getResolvedExternalModuleName( host, file )

}
def getExternalModuleNameFromPath(host: EmitHost, fileName: String): String = {
 val getCanonicalFileName = (( f: String ) =>  host.getCanonicalFileName( f ))
val dir = toPath( host.getCommonSourceDirectory(), host.getCurrentDirectory(), getCanonicalFileName )
val filePath = getNormalizedAbsolutePath( fileName, host.getCurrentDirectory() )
val relativePath = getRelativePathToDirectoryOrUrl( dir, filePath, dir, getCanonicalFileName, false )
return removeFileExtension( relativePath )

}
def getOwnEmitOutputFilePath(sourceFile: SourceFile, host: EmitHost, extension: String) = {
 val compilerOptions = host.getCompilerOptions()
var emitOutputFilePathWithoutExtension: String = zeroOfMyType
if (compilerOptions.outDir) {
 (emitOutputFilePathWithoutExtension=removeFileExtension( getSourceFilePathInNewDir( sourceFile, host, compilerOptions.outDir ) ))

}
else {
 (emitOutputFilePathWithoutExtension=removeFileExtension( sourceFile.fileName ))

}
return (emitOutputFilePathWithoutExtension+extension)

}
def getDeclarationEmitOutputFilePath(sourceFile: SourceFile, host: EmitHost) = {
 val options = host.getCompilerOptions()
val outputDir = (options.declarationDir||options.outDir)
val path = (if (outputDir) getSourceFilePathInNewDir( sourceFile, host, outputDir ) else sourceFile.fileName)
return (removeFileExtension( path )+".d.ts")

}
trait EmitFileNames {
  var jsFilePath: String
  var sourceMapFilePath: String
  var declarationFilePath: String
}
def getSourceFilesToEmit(host: EmitHost, targetSourceFile: SourceFile) = {
 val options = host.getCompilerOptions()
if ((options.outFile||options.out)) {
 val moduleKind = getEmitModuleKind( options )
val moduleEmitEnabled = ((moduleKind===ModuleKind.AMD)||(moduleKind===ModuleKind.System))
val sourceFiles = host.getSourceFiles()
return filter( sourceFiles, (if (moduleEmitEnabled) isNonDeclarationFile else isBundleEmitNonExternalModule) )

}
else {
 val sourceFiles = (if ((targetSourceFile===undefined)) host.getSourceFiles() else Array( targetSourceFile ))
return filter( sourceFiles, isNonDeclarationFile )

}

}
def isNonDeclarationFile(sourceFile: SourceFile) = {
 return (!isDeclarationFile( sourceFile ))

}
def isBundleEmitNonExternalModule(sourceFile: SourceFile) = {
 return ((!isDeclarationFile( sourceFile ))&&(!isExternalModule( sourceFile )))

}
def forEachTransformedEmitFile(host: EmitHost, sourceFiles: Array[SourceFile], action: ((String, String, String, Array[SourceFile], Boolean) => Unit), emitOnlyDtsFiles: Boolean) = {
 val options = host.getCompilerOptions()
if ((options.outFile||options.out)) {
 onBundledEmit( sourceFiles )

}
else {
 (sourceFiles).foreach { fresh13 =>
val sourceFile = zeroOfMyType
 = fresh13
 {
 if (((!isDeclarationFile( sourceFile ))&&(!host.isSourceFileFromExternalLibrary( sourceFile )))) {
 onSingleFileEmit( host, sourceFile )

}

}
}

}
def onSingleFileEmit(host: EmitHost, sourceFile: SourceFile) = {
 var extension = ".js"
if ((options.jsx===JsxEmit.Preserve)) {
 if (isSourceFileJavaScript( sourceFile )) {
 if (fileExtensionIs( sourceFile.fileName, ".jsx" )) {
 (extension=".jsx")

}

}
else if ((sourceFile.languageVariant===LanguageVariant.JSX)) {
 (extension=".jsx")

}

}
val jsFilePath = getOwnEmitOutputFilePath( sourceFile, host, extension )
val sourceMapFilePath = getSourceMapFilePath( jsFilePath, options )
val declarationFilePath = (if (((!isSourceFileJavaScript( sourceFile ))&&((options.declaration||emitOnlyDtsFiles)))) getDeclarationEmitOutputFilePath( sourceFile, host ) else undefined)
action( jsFilePath, sourceMapFilePath, declarationFilePath, Array( sourceFile ), false )

}
def onBundledEmit(sourceFiles: Array[SourceFile]) = {
 if (sourceFiles.length) {
 val jsFilePath = (options.outFile||options.out)
val sourceMapFilePath = getSourceMapFilePath( jsFilePath, options )
val declarationFilePath = (if (options.declaration) (removeFileExtension( jsFilePath )+".d.ts") else undefined)
action( jsFilePath, sourceMapFilePath, declarationFilePath, sourceFiles, true )

}

}

}
def getSourceMapFilePath(jsFilePath: String, options: CompilerOptions) = {
 return (if (options.sourceMap) (jsFilePath+".map") else undefined)

}
def forEachExpectedEmitFile(host: EmitHost, action: ((EmitFileNames, Array[SourceFile], Boolean, Boolean) => Unit), targetSourceFile: SourceFile, emitOnlyDtsFiles: Boolean) = {
 val options = host.getCompilerOptions()
if ((options.outFile||options.out)) {
 onBundledEmit( host )

}
else {
 val sourceFiles = (if ((targetSourceFile===undefined)) host.getSourceFiles() else Array( targetSourceFile ))
(sourceFiles).foreach { fresh14 =>
val sourceFile = zeroOfMyType
 = fresh14
 {
 if (((!isDeclarationFile( sourceFile ))&&(!host.isSourceFileFromExternalLibrary( sourceFile )))) {
 onSingleFileEmit( host, sourceFile )

}

}
}

}
def onSingleFileEmit(host: EmitHost, sourceFile: SourceFile) = {
 var extension = ".js"
if ((options.jsx===JsxEmit.Preserve)) {
 if (isSourceFileJavaScript( sourceFile )) {
 if (fileExtensionIs( sourceFile.fileName, ".jsx" )) {
 (extension=".jsx")

}

}
else if ((sourceFile.languageVariant===LanguageVariant.JSX)) {
 (extension=".jsx")

}

}
val jsFilePath = getOwnEmitOutputFilePath( sourceFile, host, extension )
val declarationFilePath = (if (((!isSourceFileJavaScript( sourceFile ))&&((emitOnlyDtsFiles||options.declaration)))) getDeclarationEmitOutputFilePath( sourceFile, host ) else undefined)
val emitFileNames: EmitFileNames = Map( "jsFilePath" -> jsFilePath,
"sourceMapFilePath" -> getSourceMapFilePath( jsFilePath, options ),
"declarationFilePath" -> declarationFilePath )
action( emitFileNames, Array( sourceFile ), false, emitOnlyDtsFiles )

}
def onBundledEmit(host: EmitHost) = {
 val bundledSources = filter( host.getSourceFiles(), (sourceFile =>  (((!isDeclarationFile( sourceFile ))&&(!host.isSourceFileFromExternalLibrary( sourceFile )))&&(((!isExternalModule( sourceFile ))||(!(!getEmitModuleKind( options ))))))) )
if (bundledSources.length) {
 val jsFilePath = (options.outFile||options.out)
val emitFileNames: EmitFileNames = Map( "jsFilePath" -> jsFilePath,
"sourceMapFilePath" -> getSourceMapFilePath( jsFilePath, options ),
"declarationFilePath" -> (if (options.declaration) (removeFileExtension( jsFilePath )+".d.ts") else undefined) )
action( emitFileNames, bundledSources, true, emitOnlyDtsFiles )

}

}

}
def getSourceFilePathInNewDir(sourceFile: SourceFile, host: EmitHost, newDirPath: String) = {
 var sourceFilePath = getNormalizedAbsolutePath( sourceFile.fileName, host.getCurrentDirectory() )
val commonSourceDirectory = host.getCommonSourceDirectory()
val isSourceFileInCommonSourceDirectory = (host.getCanonicalFileName( sourceFilePath ).indexOf( host.getCanonicalFileName( commonSourceDirectory ) )===0)
(sourceFilePath=(if (isSourceFileInCommonSourceDirectory) sourceFilePath.substring( commonSourceDirectory.length ) else sourceFilePath))
return combinePaths( newDirPath, sourceFilePath )

}
def writeFile(host: EmitHost, diagnostics: DiagnosticCollection, fileName: String, data: String, writeByteOrderMark: Boolean, sourceFiles: Array[SourceFile]) = {
 host.writeFile( fileName, data, writeByteOrderMark, (hostErrorMessage =>  {
 diagnostics.add( createCompilerDiagnostic( Diagnostics.Could_not_write_file_0_Colon_1, fileName, hostErrorMessage ) )

}), sourceFiles )

}
def getLineOfLocalPosition(currentSourceFile: SourceFile, pos: Int) = {
 return getLineAndCharacterOfPosition( currentSourceFile, pos ).line

}
def getLineOfLocalPositionFromLineMap(lineMap: Array[Int], pos: Int) = {
 return computeLineAndCharacterOfPosition( lineMap, pos ).line

}
def getFirstConstructorWithBody(node: ClassLikeDeclaration): ConstructorDeclaration = {
 return forEach( node.members, (member =>  {
 if (((member.kind===SyntaxKind.Constructor)&&nodeIsPresent( (member.asInstanceOf[ConstructorDeclaration]).body ))) {
 return member.asInstanceOf[ConstructorDeclaration]

}

}) )

}
def getSetAccessorTypeAnnotationNode(accessor: SetAccessorDeclaration): TypeNode = {
 if ((accessor&&(accessor.parameters.length>0))) {
 val hasThis = ((accessor.parameters.length===2)&&parameterIsThisKeyword( accessor.parameters(0) ))
return accessor.parameters((if (hasThis) 1 else 0)).`type`

}

}
def getThisParameter(signature: SignatureDeclaration): ( ParameterDeclaration | undefined ) = {
 if (signature.parameters.length) {
 val thisParameter = signature.parameters(0)
if (parameterIsThisKeyword( thisParameter )) {
 return thisParameter

}

}

}
def parameterIsThisKeyword(parameter: ParameterDeclaration): Boolean = {
 return isThisIdentifier( parameter.name )

}
def isThisIdentifier(node: ( Node | undefined )): Boolean = {
 return ((node&&(node.kind===SyntaxKind.Identifier))&&identifierIsThisKeyword( node.asInstanceOf[Identifier] ))

}
def identifierIsThisKeyword(id: Identifier): Boolean = {
 return (id.originalKeywordKind===SyntaxKind.ThisKeyword)

}
trait AllAccessorDeclarations {
  var firstAccessor: AccessorDeclaration
  var secondAccessor: AccessorDeclaration
  var getAccessor: AccessorDeclaration
  var setAccessor: AccessorDeclaration
}
def getAllAccessorDeclarations(declarations: NodeArray[Declaration], accessor: AccessorDeclaration): AllAccessorDeclarations = {
 var firstAccessor: AccessorDeclaration = zeroOfMyType
var secondAccessor: AccessorDeclaration = zeroOfMyType
var getAccessor: AccessorDeclaration = zeroOfMyType
var setAccessor: AccessorDeclaration = zeroOfMyType
if (hasDynamicName( accessor )) {
 (firstAccessor=accessor)
if ((accessor.kind===SyntaxKind.GetAccessor)) {
 (getAccessor=accessor)

}
else if ((accessor.kind===SyntaxKind.SetAccessor)) {
 (setAccessor=accessor)

}
else {
 Debug.fail( "Accessor has wrong kind" )

}

}
else {
 forEach( declarations, (( member: Declaration ) =>  {
 if (((((member.kind===SyntaxKind.GetAccessor)||(member.kind===SyntaxKind.SetAccessor)))&&(hasModifier( member, ModifierFlags.Static )===hasModifier( accessor, ModifierFlags.Static )))) {
 val memberName = getPropertyNameForPropertyNameNode( member.name )
val accessorName = getPropertyNameForPropertyNameNode( accessor.name )
if ((memberName===accessorName)) {
 if ((!firstAccessor)) {
 (firstAccessor=member.asInstanceOf[AccessorDeclaration])

}
else if ((!secondAccessor)) {
 (secondAccessor=member.asInstanceOf[AccessorDeclaration])

}
if (((member.kind===SyntaxKind.GetAccessor)&&(!getAccessor))) {
 (getAccessor=member.asInstanceOf[AccessorDeclaration])

}
if (((member.kind===SyntaxKind.SetAccessor)&&(!setAccessor))) {
 (setAccessor=member.asInstanceOf[AccessorDeclaration])

}

}

}

}) )

}
return Map( "firstAccessor" -> firstAccessor,
"secondAccessor" -> secondAccessor,
"getAccessor" -> getAccessor,
"setAccessor" -> setAccessor )

}
def emitNewLineBeforeLeadingComments(lineMap: Array[Int], writer: EmitTextWriter, node: TextRange, leadingComments: Array[CommentRange]) = {
 emitNewLineBeforeLeadingCommentsOfPosition( lineMap, writer, node.pos, leadingComments )

}
def emitNewLineBeforeLeadingCommentsOfPosition(lineMap: Array[Int], writer: EmitTextWriter, pos: Int, leadingComments: Array[CommentRange]) = {
 if ((((leadingComments&&leadingComments.length)&&(pos!==leadingComments(0).pos))&&(getLineOfLocalPositionFromLineMap( lineMap, pos )!==getLineOfLocalPositionFromLineMap( lineMap, leadingComments(0).pos )))) {
 writer.writeLine()

}

}
def emitNewLineBeforeLeadingCommentOfPosition(lineMap: Array[Int], writer: EmitTextWriter, pos: Int, commentPos: Int) = {
 if (((pos!==commentPos)&&(getLineOfLocalPositionFromLineMap( lineMap, pos )!==getLineOfLocalPositionFromLineMap( lineMap, commentPos )))) {
 writer.writeLine()

}

}
def emitComments(text: String, lineMap: Array[Int], writer: EmitTextWriter, comments: Array[CommentRange], leadingSeparator: Boolean, trailingSeparator: Boolean, newLine: String, writeComment: ((String, Array[Int], EmitTextWriter, Int, Int, String) => Unit)) = {
 if ((comments&&(comments.length>0))) {
 if (leadingSeparator) {
 writer.write( " " )

}
var emitInterveningSeparator = false
(comments).foreach { fresh15 =>
val comment = zeroOfMyType
 = fresh15
 {
 if (emitInterveningSeparator) {
 writer.write( " " )
(emitInterveningSeparator=false)

}
writeComment( text, lineMap, writer, comment.pos, comment.end, newLine )
if (comment.hasTrailingNewLine) {
 writer.writeLine()

}
else {
 (emitInterveningSeparator=true)

}

}
}
if ((emitInterveningSeparator&&trailingSeparator)) {
 writer.write( " " )

}

}

}
def emitDetachedComments(text: String, lineMap: Array[Int], writer: EmitTextWriter, writeComment: ((String, Array[Int], EmitTextWriter, Int, Int, String) => Unit), node: TextRange, newLine: String, removeComments: Boolean) = {
 var leadingComments: Array[CommentRange] = zeroOfMyType
var currentDetachedCommentInfo: {   var nodePos: Int
  var detachedCommentEndPos: Int
 } = zeroOfMyType
if (removeComments) {
 if ((node.pos===0)) {
 (leadingComments=filter( getLeadingCommentRanges( text, node.pos ), isPinnedComment ))

}

}
else {
 (leadingComments=getLeadingCommentRanges( text, node.pos ))

}
if (leadingComments) {
 val detachedComments: Array[CommentRange] = Array()
var lastComment: CommentRange = zeroOfMyType
(leadingComments).foreach { fresh16 =>
val comment = zeroOfMyType
 = fresh16
 {
 if (lastComment) {
 val lastCommentLine = getLineOfLocalPositionFromLineMap( lineMap, lastComment.end )
val commentLine = getLineOfLocalPositionFromLineMap( lineMap, comment.pos )
if ((commentLine>=(lastCommentLine+2))) {
 break()

}

}
detachedComments.push( comment )
(lastComment=comment)

}
}
if (detachedComments.length) {
 val lastCommentLine = getLineOfLocalPositionFromLineMap( lineMap, lastOrUndefined( detachedComments ).end )
val nodeLine = getLineOfLocalPositionFromLineMap( lineMap, skipTrivia( text, node.pos ) )
if ((nodeLine>=(lastCommentLine+2))) {
 emitNewLineBeforeLeadingComments( lineMap, writer, node, leadingComments )
emitComments( text, lineMap, writer, detachedComments, false, true, newLine, writeComment )
(currentDetachedCommentInfo=Map( "nodePos" -> node.pos,
"detachedCommentEndPos" -> lastOrUndefined( detachedComments ).end ))

}

}

}
return currentDetachedCommentInfo
def isPinnedComment(comment: CommentRange) = {
 return ((text.charCodeAt( (comment.pos+1) )===CharacterCodes.asterisk)&&(text.charCodeAt( (comment.pos+2) )===CharacterCodes.exclamation))

}

}
def writeCommentRange(text: String, lineMap: Array[Int], writer: EmitTextWriter, commentPos: Int, commentEnd: Int, newLine: String) = {
 if ((text.charCodeAt( (commentPos+1) )===CharacterCodes.asterisk)) {
 val firstCommentLineAndCharacter = computeLineAndCharacterOfPosition( lineMap, commentPos )
val lineCount = lineMap.length
var firstCommentLineIndent: Int = zeroOfMyType
{
var pos = commentPos
var currentLine = firstCommentLineAndCharacter.line
while( (pos<commentEnd)) {
 {
 val nextLineStart = (if ((((currentLine+1))===lineCount)) (text.length+1) else lineMap((currentLine+1)))
if ((pos!==commentPos)) {
 if ((firstCommentLineIndent===undefined)) {
 (firstCommentLineIndent=calculateIndent( text, lineMap(firstCommentLineAndCharacter.line), commentPos ))

}
val currentWriterIndentSpacing = (writer.getIndent()*getIndentSize())
val spacesToEmit = ((currentWriterIndentSpacing-firstCommentLineIndent)+calculateIndent( text, pos, nextLineStart ))
if ((spacesToEmit>0)) {
 var numberOfSingleSpacesToEmit = (spacesToEmit%getIndentSize())
val indentSizeSpaceString = getIndentString( (((spacesToEmit-numberOfSingleSpacesToEmit))/getIndentSize()) )
writer.rawWrite( indentSizeSpaceString )
while (numberOfSingleSpacesToEmit) {
{
 writer.rawWrite( " " )
(numberOfSingleSpacesToEmit-= 1)

}
}

}
else {
 writer.rawWrite( "" )

}

}
writeTrimmedCurrentLine( text, commentEnd, writer, newLine, pos, nextLineStart )
(pos=nextLineStart)

}
 (currentLine+= 1)
}
}

}
else {
 writer.write( text.substring( commentPos, commentEnd ) )

}

}
def writeTrimmedCurrentLine(text: String, commentEnd: Int, writer: EmitTextWriter, newLine: String, pos: Int, nextLineStart: Int) = {
 val end = Math.min( commentEnd, (nextLineStart-1) )
val currentLineText = text.substring( pos, end ).replace( java.util.regex.Pattern.compile(raw"""^\s+|\s+$$""", "g"), "" )
if (currentLineText) {
 writer.write( currentLineText )
if ((end!==commentEnd)) {
 writer.writeLine()

}

}
else {
 writer.writeLiteral( newLine )

}

}
def calculateIndent(text: String, pos: Int, end: Int) = {
 var currentLineIndent = 0
{
while( ((pos<end)&&isWhiteSpaceSingleLine( text.charCodeAt( pos ) ))) {
 {
 if ((text.charCodeAt( pos )===CharacterCodes.tab)) {
 (currentLineIndent+=(getIndentSize()-((currentLineIndent%getIndentSize()))))

}
else {
 (currentLineIndent+= 1)

}

}
 (pos+= 1)
}
}
return currentLineIndent

}
def hasModifiers(node: Node) = {
 return (getModifierFlags( node )!==ModifierFlags.None)

}
def hasModifier(node: Node, flags: ModifierFlags) = {
 return (((getModifierFlags( node )&flags))!==0)

}
def getModifierFlags(node: Node): ModifierFlags = {
 if ((node.modifierFlagsCache&ModifierFlags.HasComputedFlags)) {
 return (node.modifierFlagsCache&(~ModifierFlags.HasComputedFlags))

}
var flags = ModifierFlags.None
if (node.modifiers) {
 (node.modifiers).foreach { fresh17 =>
val modifier = zeroOfMyType
 = fresh17
 {
 (flags|=modifierToFlag( modifier.kind ))

}
}

}
if (((node.flags&NodeFlags.NestedNamespace)||(((node.kind===SyntaxKind.Identifier)&&(node.asInstanceOf[Identifier]).isInJSDocNamespace)))) {
 (flags|=ModifierFlags.Export)

}
(node.modifierFlagsCache=(flags|ModifierFlags.HasComputedFlags))
return flags

}
def modifierToFlag(token: SyntaxKind): ModifierFlags = {
 token match {
  case  SyntaxKind.StaticKeyword  =>
return ModifierFlags.Static
  case  SyntaxKind.PublicKeyword  =>
return ModifierFlags.Public
  case  SyntaxKind.ProtectedKeyword  =>
return ModifierFlags.Protected
  case  SyntaxKind.PrivateKeyword  =>
return ModifierFlags.Private
  case  SyntaxKind.AbstractKeyword  =>
return ModifierFlags.Abstract
  case  SyntaxKind.ExportKeyword  =>
return ModifierFlags.Export
  case  SyntaxKind.DeclareKeyword  =>
return ModifierFlags.Ambient
  case  SyntaxKind.ConstKeyword  =>
return ModifierFlags.Const
  case  SyntaxKind.DefaultKeyword  =>
return ModifierFlags.Default
  case  SyntaxKind.AsyncKeyword  =>
return ModifierFlags.Async
  case  SyntaxKind.ReadonlyKeyword  =>
return ModifierFlags.Readonly
  case _ =>
}
return ModifierFlags.None

}
def isLogicalOperator(token: SyntaxKind): Boolean = {
 return (((token===SyntaxKind.BarBarToken)||(token===SyntaxKind.AmpersandAmpersandToken))||(token===SyntaxKind.ExclamationToken))

}
def isAssignmentOperator(token: SyntaxKind): Boolean = {
 return ((token>=SyntaxKind.FirstAssignment)&&(token<=SyntaxKind.LastAssignment))

}
def tryGetClassExtendingExpressionWithTypeArguments(node: Node): ( ClassLikeDeclaration | undefined ) = {
 if ((((node.kind===SyntaxKind.ExpressionWithTypeArguments)&&((node.parent.asInstanceOf[HeritageClause]).token===SyntaxKind.ExtendsKeyword))&&isClassLike( node.parent.parent ))) {
 return node.parent.parent

}

}
def isDestructuringAssignment(node: Node): Boolean = {
 if (isBinaryExpression( node )) {
 if ((node.operatorToken.kind===SyntaxKind.EqualsToken)) {
 val kind = node.left.kind
return ((kind===SyntaxKind.ObjectLiteralExpression)||(kind===SyntaxKind.ArrayLiteralExpression))

}

}
return false

}
def isSupportedExpressionWithTypeArguments(node: ExpressionWithTypeArguments): Boolean = {
 return isSupportedExpressionWithTypeArgumentsRest( node.expression )

}
def isSupportedExpressionWithTypeArgumentsRest(node: Expression): Boolean = {
 if ((node.kind===SyntaxKind.Identifier)) {
 return true

}
else if (isPropertyAccessExpression( node )) {
 return isSupportedExpressionWithTypeArgumentsRest( node.expression )

}
else {
 return false

}

}
def isExpressionWithTypeArgumentsInClassExtendsClause(node: Node): Boolean = {
 return (tryGetClassExtendingExpressionWithTypeArguments( node )!==undefined)

}
def isEntityNameExpression(node: Expression): Boolean = {
 return ((node.kind===SyntaxKind.Identifier)||((node.kind===SyntaxKind.PropertyAccessExpression)&&isEntityNameExpression( (node.asInstanceOf[PropertyAccessExpression]).expression )))

}
def isRightSideOfQualifiedNameOrPropertyAccess(node: Node) = {
 return ((((node.parent.kind===SyntaxKind.QualifiedName)&&((node.parent.asInstanceOf[QualifiedName]).right===node)))||(((node.parent.kind===SyntaxKind.PropertyAccessExpression)&&((node.parent.asInstanceOf[PropertyAccessExpression]).name===node))))

}
def isEmptyObjectLiteralOrArrayLiteral(expression: Node): Boolean = {
 val kind = expression.kind
if ((kind===SyntaxKind.ObjectLiteralExpression)) {
 return ((expression.asInstanceOf[ObjectLiteralExpression]).properties.length===0)

}
if ((kind===SyntaxKind.ArrayLiteralExpression)) {
 return ((expression.asInstanceOf[ArrayLiteralExpression]).elements.length===0)

}
return false

}
def getLocalSymbolForExportDefault(symbol: Symbol) = {
 return (if (((symbol&&symbol.valueDeclaration)&&hasModifier( symbol.valueDeclaration, ModifierFlags.Default ))) symbol.valueDeclaration.localSymbol else undefined)

}
def tryExtractTypeScriptExtension(fileName: String): ( String | undefined ) = {
 return find( supportedTypescriptExtensionsForExtractExtension, (extension =>  fileExtensionIs( fileName, extension )) )

}
def getExpandedCharCodes(input: String): Array[Int] = {
 val output: Array[Int] = Array()
val length = input.length
{
var i = 0
while( (i<length)) {
 {
 val charCode = input.charCodeAt( i )
if ((charCode<128)) {
 output.push( charCode )

}
else if ((charCode<2048)) {
 output.push( (((charCode>>6))|192) )
output.push( (((charCode&63))|128) )

}
else if ((charCode<65536)) {
 output.push( (((charCode>>12))|224) )
output.push( (((((charCode>>6))&63))|128) )
output.push( (((charCode&63))|128) )

}
else if ((charCode<131072)) {
 output.push( (((charCode>>18))|240) )
output.push( (((((charCode>>12))&63))|128) )
output.push( (((((charCode>>6))&63))|128) )
output.push( (((charCode&63))|128) )

}
else {
 Debug.assert( false, "Unexpected code point" )

}

}
 (i+= 1)
}
}
return output

}
val stringify: ((Any) => String) = (if (((typeof(JSON)!=="undefined")&&JSON.stringify)) JSON.stringify else stringifyFallback)
def stringifyFallback(value: Any): String = {
 return (if ((value===undefined)) undefined else stringifyValue( value ))

}
def stringifyValue(value: Any): String = {
 return (if ((typeof(value)==="string")) s"""\"${ escapeString( value )}\""""  else (if ((typeof(value)==="number")) (if (isFinite( value )) String( value ) else "null") else (if ((typeof(value)==="boolean")) (if (value) "true" else "false") else (if (((typeof(value)==="object")&&value)) (if (isArray( value )) cycleCheck( stringifyArray, value ) else cycleCheck( stringifyObject, value )) else "null"))))

}
def cycleCheck(cb: ((Any) => String), value: Any) = {
 Debug.assert( (!value.`hasOwnProperty`( "__cycle" )), "Converting circular structure to JSON" )
(value.___cycle=true)
val result = cb( value )
value.remove("___cycle")
return result

}
def stringifyArray(value: Any) = {
 return s"""[${ reduceLeft( value, stringifyElement, "" )}]"""

}
def stringifyElement(memo: String, value: Any) = {
 return (((if (memo) (memo+",") else memo))+stringifyValue( value ))

}
def stringifyObject(value: Any) = {
 return s"""{${ reduceOwnProperties( value, stringifyProperty, "" )}}"""

}
def stringifyProperty(memo: String, value: Any, key: String) = {
 return (if ((((value===undefined)||(typeof(value)==="function"))||(key==="__cycle"))) memo else (((if (memo) (memo+",") else memo))+s"""\"${ escapeString( key )}\":${ stringifyValue( value )}""" ))

}
val base64Digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
def convertToBase64(input: String): String = {
 var result = ""
val charCodes = getExpandedCharCodes( input )
var i = 0
val length = charCodes.length
var byte1: Int = zeroOfMyType
var byte2: Int = zeroOfMyType
var byte3: Int = zeroOfMyType
var byte4: Int = zeroOfMyType
while ((i<length)) {
{
 (byte1=(charCodes(i)>>2))
(byte2=((((charCodes(i)&3))<<4)|(charCodes((i+1))>>4)))
(byte3=((((charCodes((i+1))&15))<<2)|(charCodes((i+2))>>6)))
(byte4=(charCodes((i+2))&63))
if (((i+1)>=length)) {
 (byte3=(byte4=64))

}
else if (((i+2)>=length)) {
 (byte4=64)

}
(result+=(((base64Digits.charAt( byte1 )+base64Digits.charAt( byte2 ))+base64Digits.charAt( byte3 ))+base64Digits.charAt( byte4 )))
(i+=3)

}
}
return result

}
val carriageReturnLineFeed = "\r\n"
val lineFeed = "\n"
def getNewLineCharacter(options: CompilerOptions): String = {
 if ((options.newLine===NewLineKind.CarriageReturnLineFeed)) {
 return carriageReturnLineFeed

}
else if ((options.newLine===NewLineKind.LineFeed)) {
 return lineFeed

}
else if (sys) {
 return sys.newLine

}
return carriageReturnLineFeed

}
def isSimpleExpression(node: Expression): Boolean = {
 return isSimpleExpressionWorker( node, 0 )

}
def isSimpleExpressionWorker(node: Expression, depth: Int): Boolean = {
 if ((depth<=5)) {
 val kind = node.kind
if (((((((((((kind===SyntaxKind.StringLiteral)||(kind===SyntaxKind.NumericLiteral))||(kind===SyntaxKind.RegularExpressionLiteral))||(kind===SyntaxKind.NoSubstitutionTemplateLiteral))||(kind===SyntaxKind.Identifier))||(kind===SyntaxKind.ThisKeyword))||(kind===SyntaxKind.SuperKeyword))||(kind===SyntaxKind.TrueKeyword))||(kind===SyntaxKind.FalseKeyword))||(kind===SyntaxKind.NullKeyword))) {
 return true

}
else if ((kind===SyntaxKind.PropertyAccessExpression)) {
 return isSimpleExpressionWorker( (node.asInstanceOf[PropertyAccessExpression]).expression, (depth+1) )

}
else if ((kind===SyntaxKind.ElementAccessExpression)) {
 return (isSimpleExpressionWorker( (node.asInstanceOf[ElementAccessExpression]).expression, (depth+1) )&&isSimpleExpressionWorker( (node.asInstanceOf[ElementAccessExpression]).argumentExpression, (depth+1) ))

}
else if (((kind===SyntaxKind.PrefixUnaryExpression)||(kind===SyntaxKind.PostfixUnaryExpression))) {
 return isSimpleExpressionWorker( (node.asInstanceOf[( PrefixUnaryExpression | PostfixUnaryExpression )]).operand, (depth+1) )

}
else if ((kind===SyntaxKind.BinaryExpression)) {
 return ((((node.asInstanceOf[BinaryExpression]).operatorToken.kind!==SyntaxKind.AsteriskAsteriskToken)&&isSimpleExpressionWorker( (node.asInstanceOf[BinaryExpression]).left, (depth+1) ))&&isSimpleExpressionWorker( (node.asInstanceOf[BinaryExpression]).right, (depth+1) ))

}
else if ((kind===SyntaxKind.ConditionalExpression)) {
 return ((isSimpleExpressionWorker( (node.asInstanceOf[ConditionalExpression]).condition, (depth+1) )&&isSimpleExpressionWorker( (node.asInstanceOf[ConditionalExpression]).whenTrue, (depth+1) ))&&isSimpleExpressionWorker( (node.asInstanceOf[ConditionalExpression]).whenFalse, (depth+1) ))

}
else if ((((kind===SyntaxKind.VoidExpression)||(kind===SyntaxKind.TypeOfExpression))||(kind===SyntaxKind.DeleteExpression))) {
 return isSimpleExpressionWorker( (node.asInstanceOf[( VoidExpression | TypeOfExpression | DeleteExpression )]).expression, (depth+1) )

}
else if ((kind===SyntaxKind.ArrayLiteralExpression)) {
 return ((node.asInstanceOf[ArrayLiteralExpression]).elements.length===0)

}
else if ((kind===SyntaxKind.ObjectLiteralExpression)) {
 return ((node.asInstanceOf[ObjectLiteralExpression]).properties.length===0)

}
else if ((kind===SyntaxKind.CallExpression)) {
 if ((!isSimpleExpressionWorker( (node.asInstanceOf[CallExpression]).expression, (depth+1) ))) {
 return false

}
((node.asInstanceOf[CallExpression]).arguments).foreach { fresh18 =>
val argument = zeroOfMyType
 = fresh18
 {
 if ((!isSimpleExpressionWorker( argument, (depth+1) ))) {
 return false

}

}
}
return true

}

}
return false

}
val syntaxKindCache = createMap[ String ]()
def formatSyntaxKind(kind: SyntaxKind): String = {
 val syntaxKindEnum = (ts.asInstanceOf[Any]).SyntaxKind
if (syntaxKindEnum) {
 if (syntaxKindCache(kind)) {
 return syntaxKindCache(kind)

}
(syntaxKindEnum).keys.foreach { fresh19 =>
val name = zeroOfMyType
 = fresh19
 {
 if ((syntaxKindEnum(name)===kind)) {
 return (syntaxKindCache(kind)=(((kind.`toString`()+" (")+name)+")"))

}

}
}

}
else {
 return kind.`toString`()

}

}
def movePos(pos: Int, value: Int) = {
 return (if (positionIsSynthesized( pos )) (-1) else (pos+value))

}
def createRange(pos: Int, end: Int): TextRange = {
 return Map( "pos" -> pos,
"end" -> end )

}
def moveRangeEnd(range: TextRange, end: Int): TextRange = {
 return createRange( range.pos, end )

}
def moveRangePos(range: TextRange, pos: Int): TextRange = {
 return createRange( pos, range.end )

}
def moveRangePastDecorators(node: Node): TextRange = {
 return (if ((node.decorators&&(node.decorators.length>0))) moveRangePos( node, node.decorators.end ) else node)

}
def moveRangePastModifiers(node: Node): TextRange = {
 return (if ((node.modifiers&&(node.modifiers.length>0))) moveRangePos( node, node.modifiers.end ) else moveRangePastDecorators( node ))

}
def isCollapsedRange(range: TextRange) = {
 return (range.pos===range.end)

}
def collapseRangeToStart(range: TextRange): TextRange = {
 return (if (isCollapsedRange( range )) range else moveRangeEnd( range, range.pos ))

}
def collapseRangeToEnd(range: TextRange): TextRange = {
 return (if (isCollapsedRange( range )) range else moveRangePos( range, range.end ))

}
def createTokenRange(pos: Int, token: SyntaxKind): TextRange = {
 return createRange( pos, (pos+tokenToString( token ).length) )

}
def rangeIsOnSingleLine(range: TextRange, sourceFile: SourceFile) = {
 return rangeStartIsOnSameLineAsRangeEnd( range, range, sourceFile )

}
def rangeStartPositionsAreOnSameLine(range1: TextRange, range2: TextRange, sourceFile: SourceFile) = {
 return positionsAreOnSameLine( getStartPositionOfRange( range1, sourceFile ), getStartPositionOfRange( range2, sourceFile ), sourceFile )

}
def rangeEndPositionsAreOnSameLine(range1: TextRange, range2: TextRange, sourceFile: SourceFile) = {
 return positionsAreOnSameLine( range1.end, range2.end, sourceFile )

}
def rangeStartIsOnSameLineAsRangeEnd(range1: TextRange, range2: TextRange, sourceFile: SourceFile) = {
 return positionsAreOnSameLine( getStartPositionOfRange( range1, sourceFile ), range2.end, sourceFile )

}
def rangeEndIsOnSameLineAsRangeStart(range1: TextRange, range2: TextRange, sourceFile: SourceFile) = {
 return positionsAreOnSameLine( range1.end, getStartPositionOfRange( range2, sourceFile ), sourceFile )

}
def positionsAreOnSameLine(pos1: Int, pos2: Int, sourceFile: SourceFile) = {
 return ((pos1===pos2)||(getLineOfLocalPosition( sourceFile, pos1 )===getLineOfLocalPosition( sourceFile, pos2 )))

}
def getStartPositionOfRange(range: TextRange, sourceFile: SourceFile) = {
 return (if (positionIsSynthesized( range.pos )) (-1) else skipTrivia( sourceFile.text, range.pos ))

}
def collectExternalModuleInfo(sourceFile: SourceFile) = {
 val externalImports: Array[( ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration )] = Array()
val exportSpecifiers = createMap[ Array[ExportSpecifier] ]()
var exportEquals: ExportAssignment = undefined
var hasExportStarsToExportValues = false
(sourceFile.statements).foreach { fresh20 =>
val node = zeroOfMyType
 = fresh20
 {
 node.kind match {
  case  SyntaxKind.ImportDeclaration  =>
externalImports.push( node.asInstanceOf[ImportDeclaration] )
  case  SyntaxKind.ImportEqualsDeclaration  =>
if (((node.asInstanceOf[ImportEqualsDeclaration]).moduleReference.kind===SyntaxKind.ExternalModuleReference)) {
 externalImports.push( node.asInstanceOf[ImportEqualsDeclaration] )

}
  case  SyntaxKind.ExportDeclaration  =>
if ((node.asInstanceOf[ExportDeclaration]).moduleSpecifier) {
 if ((!(node.asInstanceOf[ExportDeclaration]).exportClause)) {
 externalImports.push( node.asInstanceOf[ExportDeclaration] )
(hasExportStarsToExportValues=true)

}
else {
 externalImports.push( node.asInstanceOf[ExportDeclaration] )

}

}
else {
 ((node.asInstanceOf[ExportDeclaration]).exportClause.elements).foreach { fresh21 =>
val specifier = zeroOfMyType
 = fresh21
 {
 val name = ((specifier.propertyName||specifier.name)).text
((exportSpecifiers(name)||((exportSpecifiers(name)=Array())))).push( specifier )

}
}

}
  case  SyntaxKind.ExportAssignment  =>
if (((node.asInstanceOf[ExportAssignment]).isExportEquals&&(!exportEquals))) {
 (exportEquals=node.asInstanceOf[ExportAssignment])

}
  case _ =>
}

}
}
return Map( "externalImports" -> externalImports,
"exportSpecifiers" -> exportSpecifiers,
"exportEquals" -> exportEquals,
"hasExportStarsToExportValues" -> hasExportStarsToExportValues )

}
def getInitializedVariables(node: VariableDeclarationList) = {
 return filter( node.declarations, isInitializedVariable )

}
def isInitializedVariable(node: VariableDeclaration) = {
 return (node.initializer!==undefined)

}
def isMergedWithClass(node: Node) = {
 if (node.symbol) {
 (node.symbol.declarations).foreach { fresh22 =>
val declaration = zeroOfMyType
 = fresh22
 {
 if (((declaration.kind===SyntaxKind.ClassDeclaration)&&(declaration!==node))) {
 return true

}

}
}

}
return false

}
def isFirstDeclarationOfKind(node: Node, kind: SyntaxKind) = {
 return (node.symbol&&(getDeclarationOfKind( node.symbol, kind )===node))

}
def isNodeArray[T <: Node](array: Array[T]): Boolean = {
 return (array.`hasOwnProperty`( "pos" )&&array.`hasOwnProperty`( "end" ))

}
def isNoSubstitutionTemplateLiteral(node: Node): Boolean = {
 return (node.kind===SyntaxKind.NoSubstitutionTemplateLiteral)

}
def isLiteralKind(kind: SyntaxKind): Boolean = {
 return ((SyntaxKind.FirstLiteralToken<=kind)&&(kind<=SyntaxKind.LastLiteralToken))

}
def isTextualLiteralKind(kind: SyntaxKind): Boolean = {
 return ((kind===SyntaxKind.StringLiteral)||(kind===SyntaxKind.NoSubstitutionTemplateLiteral))

}
def isLiteralExpression(node: Node): Boolean = {
 return isLiteralKind( node.kind )

}
def isTemplateLiteralKind(kind: SyntaxKind): Boolean = {
 return ((SyntaxKind.FirstTemplateToken<=kind)&&(kind<=SyntaxKind.LastTemplateToken))

}
def isTemplateHead(node: Node): Boolean = {
 return (node.kind===SyntaxKind.TemplateHead)

}
def isTemplateMiddleOrTemplateTail(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.TemplateMiddle)||(kind===SyntaxKind.TemplateTail))

}
def isIdentifier(node: Node): Boolean = {
 return (node.kind===SyntaxKind.Identifier)

}
def isGeneratedIdentifier(node: Node): Boolean = {
 return (isIdentifier( node )&&(node.autoGenerateKind>GeneratedIdentifierKind.None))

}
def isModifier(node: Node): Boolean = {
 return isModifierKind( node.kind )

}
def isQualifiedName(node: Node): Boolean = {
 return (node.kind===SyntaxKind.QualifiedName)

}
def isComputedPropertyName(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ComputedPropertyName)

}
def isEntityName(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.QualifiedName)||(kind===SyntaxKind.Identifier))

}
def isPropertyName(node: Node): Boolean = {
 val kind = node.kind
return ((((kind===SyntaxKind.Identifier)||(kind===SyntaxKind.StringLiteral))||(kind===SyntaxKind.NumericLiteral))||(kind===SyntaxKind.ComputedPropertyName))

}
def isModuleName(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.Identifier)||(kind===SyntaxKind.StringLiteral))

}
def isBindingName(node: Node): Boolean = {
 val kind = node.kind
return (((kind===SyntaxKind.Identifier)||(kind===SyntaxKind.ObjectBindingPattern))||(kind===SyntaxKind.ArrayBindingPattern))

}
def isTypeParameter(node: Node): Boolean = {
 return (node.kind===SyntaxKind.TypeParameter)

}
def isParameter(node: Node): Boolean = {
 return (node.kind===SyntaxKind.Parameter)

}
def isDecorator(node: Node): Boolean = {
 return (node.kind===SyntaxKind.Decorator)

}
def isMethodDeclaration(node: Node): Boolean = {
 return (node.kind===SyntaxKind.MethodDeclaration)

}
def isClassElement(node: Node): Boolean = {
 val kind = node.kind
return (((((((kind===SyntaxKind.Constructor)||(kind===SyntaxKind.PropertyDeclaration))||(kind===SyntaxKind.MethodDeclaration))||(kind===SyntaxKind.GetAccessor))||(kind===SyntaxKind.SetAccessor))||(kind===SyntaxKind.IndexSignature))||(kind===SyntaxKind.SemicolonClassElement))

}
def isObjectLiteralElementLike(node: Node): Boolean = {
 val kind = node.kind
return ((((((kind===SyntaxKind.PropertyAssignment)||(kind===SyntaxKind.ShorthandPropertyAssignment))||(kind===SyntaxKind.MethodDeclaration))||(kind===SyntaxKind.GetAccessor))||(kind===SyntaxKind.SetAccessor))||(kind===SyntaxKind.MissingDeclaration))

}
def isTypeNodeKind(kind: SyntaxKind) = {
 return (((((((((((kind>=SyntaxKind.FirstTypeNode)&&(kind<=SyntaxKind.LastTypeNode)))||(kind===SyntaxKind.AnyKeyword))||(kind===SyntaxKind.NumberKeyword))||(kind===SyntaxKind.BooleanKeyword))||(kind===SyntaxKind.StringKeyword))||(kind===SyntaxKind.SymbolKeyword))||(kind===SyntaxKind.VoidKeyword))||(kind===SyntaxKind.NeverKeyword))||(kind===SyntaxKind.ExpressionWithTypeArguments))

}
def isTypeNode(node: Node): Boolean = {
 return isTypeNodeKind( node.kind )

}
def isBindingPattern(node: Node): Boolean = {
 if (node) {
 val kind = node.kind
return ((kind===SyntaxKind.ArrayBindingPattern)||(kind===SyntaxKind.ObjectBindingPattern))

}
return false

}
def isBindingElement(node: Node): Boolean = {
 return (node.kind===SyntaxKind.BindingElement)

}
def isArrayBindingElement(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.BindingElement)||(kind===SyntaxKind.OmittedExpression))

}
def isPropertyAccessExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.PropertyAccessExpression)

}
def isElementAccessExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ElementAccessExpression)

}
def isBinaryExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.BinaryExpression)

}
def isConditionalExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ConditionalExpression)

}
def isCallExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.CallExpression)

}
def isTemplateLiteral(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.TemplateExpression)||(kind===SyntaxKind.NoSubstitutionTemplateLiteral))

}
def isSpreadElementExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.SpreadElementExpression)

}
def isExpressionWithTypeArguments(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ExpressionWithTypeArguments)

}
def isLeftHandSideExpressionKind(kind: SyntaxKind): Boolean = {
 return ((((((((((((((((((((((((kind===SyntaxKind.PropertyAccessExpression)||(kind===SyntaxKind.ElementAccessExpression))||(kind===SyntaxKind.NewExpression))||(kind===SyntaxKind.CallExpression))||(kind===SyntaxKind.JsxElement))||(kind===SyntaxKind.JsxSelfClosingElement))||(kind===SyntaxKind.TaggedTemplateExpression))||(kind===SyntaxKind.ArrayLiteralExpression))||(kind===SyntaxKind.ParenthesizedExpression))||(kind===SyntaxKind.ObjectLiteralExpression))||(kind===SyntaxKind.ClassExpression))||(kind===SyntaxKind.FunctionExpression))||(kind===SyntaxKind.Identifier))||(kind===SyntaxKind.RegularExpressionLiteral))||(kind===SyntaxKind.NumericLiteral))||(kind===SyntaxKind.StringLiteral))||(kind===SyntaxKind.NoSubstitutionTemplateLiteral))||(kind===SyntaxKind.TemplateExpression))||(kind===SyntaxKind.FalseKeyword))||(kind===SyntaxKind.NullKeyword))||(kind===SyntaxKind.ThisKeyword))||(kind===SyntaxKind.TrueKeyword))||(kind===SyntaxKind.SuperKeyword))||(kind===SyntaxKind.NonNullExpression))

}
def isLeftHandSideExpression(node: Node): Boolean = {
 return isLeftHandSideExpressionKind( skipPartiallyEmittedExpressions( node ).kind )

}
def isUnaryExpressionKind(kind: SyntaxKind): Boolean = {
 return ((((((((kind===SyntaxKind.PrefixUnaryExpression)||(kind===SyntaxKind.PostfixUnaryExpression))||(kind===SyntaxKind.DeleteExpression))||(kind===SyntaxKind.TypeOfExpression))||(kind===SyntaxKind.VoidExpression))||(kind===SyntaxKind.AwaitExpression))||(kind===SyntaxKind.TypeAssertionExpression))||isLeftHandSideExpressionKind( kind ))

}
def isUnaryExpression(node: Node): Boolean = {
 return isUnaryExpressionKind( skipPartiallyEmittedExpressions( node ).kind )

}
def isExpressionKind(kind: SyntaxKind) = {
 return ((((((((kind===SyntaxKind.ConditionalExpression)||(kind===SyntaxKind.YieldExpression))||(kind===SyntaxKind.ArrowFunction))||(kind===SyntaxKind.BinaryExpression))||(kind===SyntaxKind.SpreadElementExpression))||(kind===SyntaxKind.AsExpression))||(kind===SyntaxKind.OmittedExpression))||isUnaryExpressionKind( kind ))

}
def isExpression(node: Node): Boolean = {
 return isExpressionKind( skipPartiallyEmittedExpressions( node ).kind )

}
def isAssertionExpression(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.TypeAssertionExpression)||(kind===SyntaxKind.AsExpression))

}
def isPartiallyEmittedExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.PartiallyEmittedExpression)

}
def isNotEmittedStatement(node: Node): Boolean = {
 return (node.kind===SyntaxKind.NotEmittedStatement)

}
def isNotEmittedOrPartiallyEmittedNode(node: Node): Boolean = {
 return (isNotEmittedStatement( node )||isPartiallyEmittedExpression( node ))

}
def isOmittedExpression(node: Node): Boolean = {
 return (node.kind===SyntaxKind.OmittedExpression)

}
def isTemplateSpan(node: Node): Boolean = {
 return (node.kind===SyntaxKind.TemplateSpan)

}
def isBlock(node: Node): Boolean = {
 return (node.kind===SyntaxKind.Block)

}
def isConciseBody(node: Node): Boolean = {
 return (isBlock( node )||isExpression( node ))

}
def isFunctionBody(node: Node): Boolean = {
 return isBlock( node )

}
def isForInitializer(node: Node): Boolean = {
 return (isVariableDeclarationList( node )||isExpression( node ))

}
def isVariableDeclaration(node: Node): Boolean = {
 return (node.kind===SyntaxKind.VariableDeclaration)

}
def isVariableDeclarationList(node: Node): Boolean = {
 return (node.kind===SyntaxKind.VariableDeclarationList)

}
def isCaseBlock(node: Node): Boolean = {
 return (node.kind===SyntaxKind.CaseBlock)

}
def isModuleBody(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.ModuleBlock)||(kind===SyntaxKind.ModuleDeclaration))

}
def isImportEqualsDeclaration(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ImportEqualsDeclaration)

}
def isImportClause(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ImportClause)

}
def isNamedImportBindings(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.NamedImports)||(kind===SyntaxKind.NamespaceImport))

}
def isImportSpecifier(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ImportSpecifier)

}
def isNamedExports(node: Node): Boolean = {
 return (node.kind===SyntaxKind.NamedExports)

}
def isExportSpecifier(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ExportSpecifier)

}
def isModuleOrEnumDeclaration(node: Node): Boolean = {
 return ((node.kind===SyntaxKind.ModuleDeclaration)||(node.kind===SyntaxKind.EnumDeclaration))

}
def isDeclarationKind(kind: SyntaxKind) = {
 return ((((((((((((((((((((((((((((((kind===SyntaxKind.ArrowFunction)||(kind===SyntaxKind.BindingElement))||(kind===SyntaxKind.ClassDeclaration))||(kind===SyntaxKind.ClassExpression))||(kind===SyntaxKind.Constructor))||(kind===SyntaxKind.EnumDeclaration))||(kind===SyntaxKind.EnumMember))||(kind===SyntaxKind.ExportSpecifier))||(kind===SyntaxKind.FunctionDeclaration))||(kind===SyntaxKind.FunctionExpression))||(kind===SyntaxKind.GetAccessor))||(kind===SyntaxKind.ImportClause))||(kind===SyntaxKind.ImportEqualsDeclaration))||(kind===SyntaxKind.ImportSpecifier))||(kind===SyntaxKind.InterfaceDeclaration))||(kind===SyntaxKind.MethodDeclaration))||(kind===SyntaxKind.MethodSignature))||(kind===SyntaxKind.ModuleDeclaration))||(kind===SyntaxKind.NamespaceExportDeclaration))||(kind===SyntaxKind.NamespaceImport))||(kind===SyntaxKind.Parameter))||(kind===SyntaxKind.PropertyAssignment))||(kind===SyntaxKind.PropertyDeclaration))||(kind===SyntaxKind.PropertySignature))||(kind===SyntaxKind.SetAccessor))||(kind===SyntaxKind.ShorthandPropertyAssignment))||(kind===SyntaxKind.TypeAliasDeclaration))||(kind===SyntaxKind.TypeParameter))||(kind===SyntaxKind.VariableDeclaration))||(kind===SyntaxKind.JSDocTypedefTag))

}
def isDeclarationStatementKind(kind: SyntaxKind) = {
 return ((((((((((((kind===SyntaxKind.FunctionDeclaration)||(kind===SyntaxKind.MissingDeclaration))||(kind===SyntaxKind.ClassDeclaration))||(kind===SyntaxKind.InterfaceDeclaration))||(kind===SyntaxKind.TypeAliasDeclaration))||(kind===SyntaxKind.EnumDeclaration))||(kind===SyntaxKind.ModuleDeclaration))||(kind===SyntaxKind.ImportDeclaration))||(kind===SyntaxKind.ImportEqualsDeclaration))||(kind===SyntaxKind.ExportDeclaration))||(kind===SyntaxKind.ExportAssignment))||(kind===SyntaxKind.NamespaceExportDeclaration))

}
def isStatementKindButNotDeclarationKind(kind: SyntaxKind) = {
 return (((((((((((((((((((kind===SyntaxKind.BreakStatement)||(kind===SyntaxKind.ContinueStatement))||(kind===SyntaxKind.DebuggerStatement))||(kind===SyntaxKind.DoStatement))||(kind===SyntaxKind.ExpressionStatement))||(kind===SyntaxKind.EmptyStatement))||(kind===SyntaxKind.ForInStatement))||(kind===SyntaxKind.ForOfStatement))||(kind===SyntaxKind.ForStatement))||(kind===SyntaxKind.IfStatement))||(kind===SyntaxKind.LabeledStatement))||(kind===SyntaxKind.ReturnStatement))||(kind===SyntaxKind.SwitchStatement))||(kind===SyntaxKind.ThrowStatement))||(kind===SyntaxKind.TryStatement))||(kind===SyntaxKind.VariableStatement))||(kind===SyntaxKind.WhileStatement))||(kind===SyntaxKind.WithStatement))||(kind===SyntaxKind.NotEmittedStatement))

}
def isDeclaration(node: Node): Boolean = {
 return isDeclarationKind( node.kind )

}
def isDeclarationStatement(node: Node): Boolean = {
 return isDeclarationStatementKind( node.kind )

}
def isStatementButNotDeclaration(node: Node): Boolean = {
 return isStatementKindButNotDeclarationKind( node.kind )

}
def isStatement(node: Node): Boolean = {
 val kind = node.kind
return ((isStatementKindButNotDeclarationKind( kind )||isDeclarationStatementKind( kind ))||(kind===SyntaxKind.Block))

}
def isModuleReference(node: Node): Boolean = {
 val kind = node.kind
return (((kind===SyntaxKind.ExternalModuleReference)||(kind===SyntaxKind.QualifiedName))||(kind===SyntaxKind.Identifier))

}
def isJsxOpeningElement(node: Node): Boolean = {
 return (node.kind===SyntaxKind.JsxOpeningElement)

}
def isJsxClosingElement(node: Node): Boolean = {
 return (node.kind===SyntaxKind.JsxClosingElement)

}
def isJsxTagNameExpression(node: Node): Boolean = {
 val kind = node.kind
return (((kind===SyntaxKind.ThisKeyword)||(kind===SyntaxKind.Identifier))||(kind===SyntaxKind.PropertyAccessExpression))

}
def isJsxChild(node: Node): Boolean = {
 val kind = node.kind
return ((((kind===SyntaxKind.JsxElement)||(kind===SyntaxKind.JsxExpression))||(kind===SyntaxKind.JsxSelfClosingElement))||(kind===SyntaxKind.JsxText))

}
def isJsxAttributeLike(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.JsxAttribute)||(kind===SyntaxKind.JsxSpreadAttribute))

}
def isJsxSpreadAttribute(node: Node): Boolean = {
 return (node.kind===SyntaxKind.JsxSpreadAttribute)

}
def isJsxAttribute(node: Node): Boolean = {
 return (node.kind===SyntaxKind.JsxAttribute)

}
def isStringLiteralOrJsxExpression(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.StringLiteral)||(kind===SyntaxKind.JsxExpression))

}
def isCaseOrDefaultClause(node: Node): Boolean = {
 val kind = node.kind
return ((kind===SyntaxKind.CaseClause)||(kind===SyntaxKind.DefaultClause))

}
def isHeritageClause(node: Node): Boolean = {
 return (node.kind===SyntaxKind.HeritageClause)

}
def isCatchClause(node: Node): Boolean = {
 return (node.kind===SyntaxKind.CatchClause)

}
def isPropertyAssignment(node: Node): Boolean = {
 return (node.kind===SyntaxKind.PropertyAssignment)

}
def isShorthandPropertyAssignment(node: Node): Boolean = {
 return (node.kind===SyntaxKind.ShorthandPropertyAssignment)

}
def isEnumMember(node: Node): Boolean = {
 return (node.kind===SyntaxKind.EnumMember)

}
def isSourceFile(node: Node): Boolean = {
 return (node.kind===SyntaxKind.SourceFile)

}
def isWatchSet(options: CompilerOptions) = {
 return (options.watch&&options.`hasOwnProperty`( "watch" ))

}
}
object ts {
def getDefaultLibFileName(options: CompilerOptions): String = {
 options.target match {
  case  ScriptTarget.ES2017  =>
return "lib.es2017.d.ts"
  case  ScriptTarget.ES2016  =>
return "lib.es2016.d.ts"
  case  ScriptTarget.ES2015  =>
return "lib.es6.d.ts"
  case _ =>
return "lib.d.ts"
}

}
def textSpanEnd(span: TextSpan) = {
 return (span.start+span.length)

}
def textSpanIsEmpty(span: TextSpan) = {
 return (span.length===0)

}
def textSpanContainsPosition(span: TextSpan, position: Int) = {
 return ((position>=span.start)&&(position<textSpanEnd( span )))

}
def textSpanContainsTextSpan(span: TextSpan, other: TextSpan) = {
 return ((other.start>=span.start)&&(textSpanEnd( other )<=textSpanEnd( span )))

}
def textSpanOverlapsWith(span: TextSpan, other: TextSpan) = {
 val overlapStart = Math.max( span.start, other.start )
val overlapEnd = Math.min( textSpanEnd( span ), textSpanEnd( other ) )
return (overlapStart<overlapEnd)

}
def textSpanOverlap(span1: TextSpan, span2: TextSpan) = {
 val overlapStart = Math.max( span1.start, span2.start )
val overlapEnd = Math.min( textSpanEnd( span1 ), textSpanEnd( span2 ) )
if ((overlapStart<overlapEnd)) {
 return createTextSpanFromBounds( overlapStart, overlapEnd )

}
return undefined

}
def textSpanIntersectsWithTextSpan(span: TextSpan, other: TextSpan) = {
 return ((other.start<=textSpanEnd( span ))&&(textSpanEnd( other )>=span.start))

}
def textSpanIntersectsWith(span: TextSpan, start: Int, length: Int) = {
 val end = (start+length)
return ((start<=textSpanEnd( span ))&&(end>=span.start))

}
def decodedTextSpanIntersectsWith(start1: Int, length1: Int, start2: Int, length2: Int) = {
 val end1 = (start1+length1)
val end2 = (start2+length2)
return ((start2<=end1)&&(end2>=start1))

}
def textSpanIntersectsWithPosition(span: TextSpan, position: Int) = {
 return ((position<=textSpanEnd( span ))&&(position>=span.start))

}
def textSpanIntersection(span1: TextSpan, span2: TextSpan) = {
 val intersectStart = Math.max( span1.start, span2.start )
val intersectEnd = Math.min( textSpanEnd( span1 ), textSpanEnd( span2 ) )
if ((intersectStart<=intersectEnd)) {
 return createTextSpanFromBounds( intersectStart, intersectEnd )

}
return undefined

}
def createTextSpan(start: Int, length: Int): TextSpan = {
 if ((start<0)) {
 throw new Error( "start < 0" )
}
if ((length<0)) {
 throw new Error( "length < 0" )
}
return Map( "start" -> start,
"length" -> length )

}
def createTextSpanFromBounds(start: Int, end: Int) = {
 return createTextSpan( start, (end-start) )

}
def textChangeRangeNewSpan(range: TextChangeRange) = {
 return createTextSpan( range.span.start, range.newLength )

}
def textChangeRangeIsUnchanged(range: TextChangeRange) = {
 return (textSpanIsEmpty( range.span )&&(range.newLength===0))

}
def createTextChangeRange(span: TextSpan, newLength: Int): TextChangeRange = {
 if ((newLength<0)) {
 throw new Error( "newLength < 0" )
}
return Map( "span" -> span,
"newLength" -> newLength )

}
var unchangedTextChangeRange = createTextChangeRange( createTextSpan( 0, 0 ), 0 )
def collapseTextChangeRangesAcrossMultipleVersions(changes: Array[TextChangeRange]): TextChangeRange = {
 if ((changes.length===0)) {
 return unchangedTextChangeRange

}
if ((changes.length===1)) {
 return changes(0)

}
val change0 = changes(0)
var oldStartN = change0.span.start
var oldEndN = textSpanEnd( change0.span )
var newEndN = (oldStartN+change0.newLength)
{
var i = 1
while( (i<changes.length)) {
 {
 val nextChange = changes(i)
val oldStart1 = oldStartN
val oldEnd1 = oldEndN
val newEnd1 = newEndN
val oldStart2 = nextChange.span.start
val oldEnd2 = textSpanEnd( nextChange.span )
val newEnd2 = (oldStart2+nextChange.newLength)
(oldStartN=Math.min( oldStart1, oldStart2 ))
(oldEndN=Math.max( oldEnd1, (oldEnd1+((oldEnd2-newEnd1))) ))
(newEndN=Math.max( newEnd2, (newEnd2+((newEnd1-oldEnd2))) ))

}
 (i+= 1)
}
}
return createTextChangeRange( createTextSpanFromBounds( oldStartN, oldEndN ), (newEndN-oldStartN) )

}
def getTypeParameterOwner(d: Declaration): Declaration = {
 if ((d&&(d.kind===SyntaxKind.TypeParameter))) {
 {
var current: Node = d
while( current) {
 {
 if (((isFunctionLike( current )||isClassLike( current ))||(current.kind===SyntaxKind.InterfaceDeclaration))) {
 return current.asInstanceOf[Declaration]

}

}
 (current=current.parent)
}
}

}

}
def isParameterPropertyDeclaration(node: ParameterDeclaration): Boolean = {
 return ((hasModifier( node, ModifierFlags.ParameterPropertyModifier )&&(node.parent.kind===SyntaxKind.Constructor))&&isClassLike( node.parent.parent ))

}
def walkUpBindingElementsAndPatterns(node: Node): Node = {
 while ((node&&(((node.kind===SyntaxKind.BindingElement)||isBindingPattern( node ))))) {
{
 (node=node.parent)

}
}
return node

}
def getCombinedModifierFlags(node: Node): ModifierFlags = {
 (node=walkUpBindingElementsAndPatterns( node ))
var flags = getModifierFlags( node )
if ((node.kind===SyntaxKind.VariableDeclaration)) {
 (node=node.parent)

}
if ((node&&(node.kind===SyntaxKind.VariableDeclarationList))) {
 (flags|=getModifierFlags( node ))
(node=node.parent)

}
if ((node&&(node.kind===SyntaxKind.VariableStatement))) {
 (flags|=getModifierFlags( node ))

}
return flags

}
def getCombinedNodeFlags(node: Node): NodeFlags = {
 (node=walkUpBindingElementsAndPatterns( node ))
var flags = node.flags
if ((node.kind===SyntaxKind.VariableDeclaration)) {
 (node=node.parent)

}
if ((node&&(node.kind===SyntaxKind.VariableDeclarationList))) {
 (flags|=node.flags)
(node=node.parent)

}
if ((node&&(node.kind===SyntaxKind.VariableStatement))) {
 (flags|=node.flags)

}
return flags

}
}
