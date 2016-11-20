package scalatscomp
object Parser {
var NodeConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
var TokenConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
var IdentifierConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
var SourceFileConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
def createNode(kind: SyntaxKind, pos: Int, end: Int): Node = {
 if ((kind===SyntaxKind.SourceFile)) {
 if ((!SourceFileConstructor)) {
 (SourceFileConstructor=objectAllocator.getSourceFileConstructor())

}
return new SourceFileConstructor( kind, pos, end )

}
else if ((kind===SyntaxKind.Identifier)) {
 if ((!IdentifierConstructor)) {
 (IdentifierConstructor=objectAllocator.getIdentifierConstructor())

}
return new IdentifierConstructor( kind, pos, end )

}
else if ((kind<SyntaxKind.FirstNode)) {
 if ((!TokenConstructor)) {
 (TokenConstructor=objectAllocator.getTokenConstructor())

}
return new TokenConstructor( kind, pos, end )

}
else {
 if ((!NodeConstructor)) {
 (NodeConstructor=objectAllocator.getNodeConstructor())

}
return new NodeConstructor( kind, pos, end )

}

}
def visitNode[T](cbNode: ((Node) => T), node: Node): T = {
 if (node) {
 return cbNode( node )

}

}
def visitNodeArray[T](cbNodes: ((Array[Node]) => T), nodes: Array[Node]) = {
 if (nodes) {
 return cbNodes( nodes )

}

}
def visitEachNode[T](cbNode: ((Node) => T), nodes: Array[Node]) = {
 if (nodes) {
 (nodes).foreach { fresh1 =>
val node = zeroOfMyType
 = fresh1
 {
 val result = cbNode( node )
if (result) {
 return result

}

}
}

}

}
def forEachChild[T](node: Node, cbNode: ((Node) => T), cbNodeArray: ((Array[Node]) => T)): T = {
 if ((!node)) {
 return

}
val visitNodes: ((((( Node | Array[Node] )) => T), Array[Node]) => T) = (if (cbNodeArray) visitNodeArray else visitEachNode)
val cbNodes = (cbNodeArray||cbNode)
node.kind match {
  case  SyntaxKind.QualifiedName  =>
return (visitNode( cbNode, (node.asInstanceOf[QualifiedName]).left )||visitNode( cbNode, (node.asInstanceOf[QualifiedName]).right ))
  case  SyntaxKind.TypeParameter  =>
return ((visitNode( cbNode, (node.asInstanceOf[TypeParameterDeclaration]).name )||visitNode( cbNode, (node.asInstanceOf[TypeParameterDeclaration]).constraint ))||visitNode( cbNode, (node.asInstanceOf[TypeParameterDeclaration]).expression ))
  case  SyntaxKind.ShorthandPropertyAssignment  =>
return (((((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[ShorthandPropertyAssignment]).name ))||visitNode( cbNode, (node.asInstanceOf[ShorthandPropertyAssignment]).questionToken ))||visitNode( cbNode, (node.asInstanceOf[ShorthandPropertyAssignment]).equalsToken ))||visitNode( cbNode, (node.asInstanceOf[ShorthandPropertyAssignment]).objectAssignmentInitializer ))
  case  SyntaxKind.Parameter | SyntaxKind.PropertyDeclaration | SyntaxKind.PropertySignature | SyntaxKind.PropertyAssignment | SyntaxKind.VariableDeclaration | SyntaxKind.BindingElement  =>
return (((((((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[VariableLikeDeclaration]).propertyName ))||visitNode( cbNode, (node.asInstanceOf[VariableLikeDeclaration]).dotDotDotToken ))||visitNode( cbNode, (node.asInstanceOf[VariableLikeDeclaration]).name ))||visitNode( cbNode, (node.asInstanceOf[VariableLikeDeclaration]).questionToken ))||visitNode( cbNode, (node.asInstanceOf[VariableLikeDeclaration]).`type` ))||visitNode( cbNode, (node.asInstanceOf[VariableLikeDeclaration]).initializer ))
  case  SyntaxKind.FunctionType | SyntaxKind.ConstructorType | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature  =>
return ((((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNodes( cbNodes, (node.asInstanceOf[SignatureDeclaration]).typeParameters ))||visitNodes( cbNodes, (node.asInstanceOf[SignatureDeclaration]).parameters ))||visitNode( cbNode, (node.asInstanceOf[SignatureDeclaration]).`type` ))
  case  SyntaxKind.MethodDeclaration | SyntaxKind.MethodSignature | SyntaxKind.Constructor | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.FunctionExpression | SyntaxKind.FunctionDeclaration | SyntaxKind.ArrowFunction  =>
return (((((((((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[FunctionLikeDeclaration]).asteriskToken ))||visitNode( cbNode, (node.asInstanceOf[FunctionLikeDeclaration]).name ))||visitNode( cbNode, (node.asInstanceOf[FunctionLikeDeclaration]).questionToken ))||visitNodes( cbNodes, (node.asInstanceOf[FunctionLikeDeclaration]).typeParameters ))||visitNodes( cbNodes, (node.asInstanceOf[FunctionLikeDeclaration]).parameters ))||visitNode( cbNode, (node.asInstanceOf[FunctionLikeDeclaration]).`type` ))||visitNode( cbNode, (node.asInstanceOf[ArrowFunction]).equalsGreaterThanToken ))||visitNode( cbNode, (node.asInstanceOf[FunctionLikeDeclaration]).body ))
  case  SyntaxKind.TypeReference  =>
return (visitNode( cbNode, (node.asInstanceOf[TypeReferenceNode]).typeName )||visitNodes( cbNodes, (node.asInstanceOf[TypeReferenceNode]).typeArguments ))
  case  SyntaxKind.TypePredicate  =>
return (visitNode( cbNode, (node.asInstanceOf[TypePredicateNode]).parameterName )||visitNode( cbNode, (node.asInstanceOf[TypePredicateNode]).`type` ))
  case  SyntaxKind.TypeQuery  =>
return visitNode( cbNode, (node.asInstanceOf[TypeQueryNode]).exprName )
  case  SyntaxKind.TypeLiteral  =>
return visitNodes( cbNodes, (node.asInstanceOf[TypeLiteralNode]).members )
  case  SyntaxKind.ArrayType  =>
return visitNode( cbNode, (node.asInstanceOf[ArrayTypeNode]).elementType )
  case  SyntaxKind.TupleType  =>
return visitNodes( cbNodes, (node.asInstanceOf[TupleTypeNode]).elementTypes )
  case  SyntaxKind.UnionType | SyntaxKind.IntersectionType  =>
return visitNodes( cbNodes, (node.asInstanceOf[UnionOrIntersectionTypeNode]).types )
  case  SyntaxKind.ParenthesizedType  =>
return visitNode( cbNode, (node.asInstanceOf[ParenthesizedTypeNode]).`type` )
  case  SyntaxKind.LiteralType  =>
return visitNode( cbNode, (node.asInstanceOf[LiteralTypeNode]).literal )
  case  SyntaxKind.ObjectBindingPattern | SyntaxKind.ArrayBindingPattern  =>
return visitNodes( cbNodes, (node.asInstanceOf[BindingPattern]).elements )
  case  SyntaxKind.ArrayLiteralExpression  =>
return visitNodes( cbNodes, (node.asInstanceOf[ArrayLiteralExpression]).elements )
  case  SyntaxKind.ObjectLiteralExpression  =>
return visitNodes( cbNodes, (node.asInstanceOf[ObjectLiteralExpression]).properties )
  case  SyntaxKind.PropertyAccessExpression  =>
return (visitNode( cbNode, (node.asInstanceOf[PropertyAccessExpression]).expression )||visitNode( cbNode, (node.asInstanceOf[PropertyAccessExpression]).name ))
  case  SyntaxKind.ElementAccessExpression  =>
return (visitNode( cbNode, (node.asInstanceOf[ElementAccessExpression]).expression )||visitNode( cbNode, (node.asInstanceOf[ElementAccessExpression]).argumentExpression ))
  case  SyntaxKind.CallExpression | SyntaxKind.NewExpression  =>
return ((visitNode( cbNode, (node.asInstanceOf[CallExpression]).expression )||visitNodes( cbNodes, (node.asInstanceOf[CallExpression]).typeArguments ))||visitNodes( cbNodes, (node.asInstanceOf[CallExpression]).arguments ))
  case  SyntaxKind.TaggedTemplateExpression  =>
return (visitNode( cbNode, (node.asInstanceOf[TaggedTemplateExpression]).tag )||visitNode( cbNode, (node.asInstanceOf[TaggedTemplateExpression]).template ))
  case  SyntaxKind.TypeAssertionExpression  =>
return (visitNode( cbNode, (node.asInstanceOf[TypeAssertion]).`type` )||visitNode( cbNode, (node.asInstanceOf[TypeAssertion]).expression ))
  case  SyntaxKind.ParenthesizedExpression  =>
return visitNode( cbNode, (node.asInstanceOf[ParenthesizedExpression]).expression )
  case  SyntaxKind.DeleteExpression  =>
return visitNode( cbNode, (node.asInstanceOf[DeleteExpression]).expression )
  case  SyntaxKind.TypeOfExpression  =>
return visitNode( cbNode, (node.asInstanceOf[TypeOfExpression]).expression )
  case  SyntaxKind.VoidExpression  =>
return visitNode( cbNode, (node.asInstanceOf[VoidExpression]).expression )
  case  SyntaxKind.PrefixUnaryExpression  =>
return visitNode( cbNode, (node.asInstanceOf[PrefixUnaryExpression]).operand )
  case  SyntaxKind.YieldExpression  =>
return (visitNode( cbNode, (node.asInstanceOf[YieldExpression]).asteriskToken )||visitNode( cbNode, (node.asInstanceOf[YieldExpression]).expression ))
  case  SyntaxKind.AwaitExpression  =>
return visitNode( cbNode, (node.asInstanceOf[AwaitExpression]).expression )
  case  SyntaxKind.PostfixUnaryExpression  =>
return visitNode( cbNode, (node.asInstanceOf[PostfixUnaryExpression]).operand )
  case  SyntaxKind.BinaryExpression  =>
return ((visitNode( cbNode, (node.asInstanceOf[BinaryExpression]).left )||visitNode( cbNode, (node.asInstanceOf[BinaryExpression]).operatorToken ))||visitNode( cbNode, (node.asInstanceOf[BinaryExpression]).right ))
  case  SyntaxKind.AsExpression  =>
return (visitNode( cbNode, (node.asInstanceOf[AsExpression]).expression )||visitNode( cbNode, (node.asInstanceOf[AsExpression]).`type` ))
  case  SyntaxKind.NonNullExpression  =>
return visitNode( cbNode, (node.asInstanceOf[NonNullExpression]).expression )
  case  SyntaxKind.ConditionalExpression  =>
return ((((visitNode( cbNode, (node.asInstanceOf[ConditionalExpression]).condition )||visitNode( cbNode, (node.asInstanceOf[ConditionalExpression]).questionToken ))||visitNode( cbNode, (node.asInstanceOf[ConditionalExpression]).whenTrue ))||visitNode( cbNode, (node.asInstanceOf[ConditionalExpression]).colonToken ))||visitNode( cbNode, (node.asInstanceOf[ConditionalExpression]).whenFalse ))
  case  SyntaxKind.SpreadElementExpression  =>
return visitNode( cbNode, (node.asInstanceOf[SpreadElementExpression]).expression )
  case  SyntaxKind.Block | SyntaxKind.ModuleBlock  =>
return visitNodes( cbNodes, (node.asInstanceOf[Block]).statements )
  case  SyntaxKind.SourceFile  =>
return (visitNodes( cbNodes, (node.asInstanceOf[SourceFile]).statements )||visitNode( cbNode, (node.asInstanceOf[SourceFile]).endOfFileToken ))
  case  SyntaxKind.VariableStatement  =>
return ((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[VariableStatement]).declarationList ))
  case  SyntaxKind.VariableDeclarationList  =>
return visitNodes( cbNodes, (node.asInstanceOf[VariableDeclarationList]).declarations )
  case  SyntaxKind.ExpressionStatement  =>
return visitNode( cbNode, (node.asInstanceOf[ExpressionStatement]).expression )
  case  SyntaxKind.IfStatement  =>
return ((visitNode( cbNode, (node.asInstanceOf[IfStatement]).expression )||visitNode( cbNode, (node.asInstanceOf[IfStatement]).thenStatement ))||visitNode( cbNode, (node.asInstanceOf[IfStatement]).elseStatement ))
  case  SyntaxKind.DoStatement  =>
return (visitNode( cbNode, (node.asInstanceOf[DoStatement]).statement )||visitNode( cbNode, (node.asInstanceOf[DoStatement]).expression ))
  case  SyntaxKind.WhileStatement  =>
return (visitNode( cbNode, (node.asInstanceOf[WhileStatement]).expression )||visitNode( cbNode, (node.asInstanceOf[WhileStatement]).statement ))
  case  SyntaxKind.ForStatement  =>
return (((visitNode( cbNode, (node.asInstanceOf[ForStatement]).initializer )||visitNode( cbNode, (node.asInstanceOf[ForStatement]).condition ))||visitNode( cbNode, (node.asInstanceOf[ForStatement]).incrementor ))||visitNode( cbNode, (node.asInstanceOf[ForStatement]).statement ))
  case  SyntaxKind.ForInStatement  =>
return ((visitNode( cbNode, (node.asInstanceOf[ForInStatement]).initializer )||visitNode( cbNode, (node.asInstanceOf[ForInStatement]).expression ))||visitNode( cbNode, (node.asInstanceOf[ForInStatement]).statement ))
  case  SyntaxKind.ForOfStatement  =>
return ((visitNode( cbNode, (node.asInstanceOf[ForOfStatement]).initializer )||visitNode( cbNode, (node.asInstanceOf[ForOfStatement]).expression ))||visitNode( cbNode, (node.asInstanceOf[ForOfStatement]).statement ))
  case  SyntaxKind.ContinueStatement | SyntaxKind.BreakStatement  =>
return visitNode( cbNode, (node.asInstanceOf[BreakOrContinueStatement]).label )
  case  SyntaxKind.ReturnStatement  =>
return visitNode( cbNode, (node.asInstanceOf[ReturnStatement]).expression )
  case  SyntaxKind.WithStatement  =>
return (visitNode( cbNode, (node.asInstanceOf[WithStatement]).expression )||visitNode( cbNode, (node.asInstanceOf[WithStatement]).statement ))
  case  SyntaxKind.SwitchStatement  =>
return (visitNode( cbNode, (node.asInstanceOf[SwitchStatement]).expression )||visitNode( cbNode, (node.asInstanceOf[SwitchStatement]).caseBlock ))
  case  SyntaxKind.CaseBlock  =>
return visitNodes( cbNodes, (node.asInstanceOf[CaseBlock]).clauses )
  case  SyntaxKind.CaseClause  =>
return (visitNode( cbNode, (node.asInstanceOf[CaseClause]).expression )||visitNodes( cbNodes, (node.asInstanceOf[CaseClause]).statements ))
  case  SyntaxKind.DefaultClause  =>
return visitNodes( cbNodes, (node.asInstanceOf[DefaultClause]).statements )
  case  SyntaxKind.LabeledStatement  =>
return (visitNode( cbNode, (node.asInstanceOf[LabeledStatement]).label )||visitNode( cbNode, (node.asInstanceOf[LabeledStatement]).statement ))
  case  SyntaxKind.ThrowStatement  =>
return visitNode( cbNode, (node.asInstanceOf[ThrowStatement]).expression )
  case  SyntaxKind.TryStatement  =>
return ((visitNode( cbNode, (node.asInstanceOf[TryStatement]).tryBlock )||visitNode( cbNode, (node.asInstanceOf[TryStatement]).catchClause ))||visitNode( cbNode, (node.asInstanceOf[TryStatement]).finallyBlock ))
  case  SyntaxKind.CatchClause  =>
return (visitNode( cbNode, (node.asInstanceOf[CatchClause]).variableDeclaration )||visitNode( cbNode, (node.asInstanceOf[CatchClause]).block ))
  case  SyntaxKind.Decorator  =>
return visitNode( cbNode, (node.asInstanceOf[Decorator]).expression )
  case  SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression  =>
return (((((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[ClassLikeDeclaration]).name ))||visitNodes( cbNodes, (node.asInstanceOf[ClassLikeDeclaration]).typeParameters ))||visitNodes( cbNodes, (node.asInstanceOf[ClassLikeDeclaration]).heritageClauses ))||visitNodes( cbNodes, (node.asInstanceOf[ClassLikeDeclaration]).members ))
  case  SyntaxKind.InterfaceDeclaration  =>
return (((((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[InterfaceDeclaration]).name ))||visitNodes( cbNodes, (node.asInstanceOf[InterfaceDeclaration]).typeParameters ))||visitNodes( cbNodes, (node.asInstanceOf[ClassDeclaration]).heritageClauses ))||visitNodes( cbNodes, (node.asInstanceOf[InterfaceDeclaration]).members ))
  case  SyntaxKind.TypeAliasDeclaration  =>
return ((((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[TypeAliasDeclaration]).name ))||visitNodes( cbNodes, (node.asInstanceOf[TypeAliasDeclaration]).typeParameters ))||visitNode( cbNode, (node.asInstanceOf[TypeAliasDeclaration]).`type` ))
  case  SyntaxKind.EnumDeclaration  =>
return (((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[EnumDeclaration]).name ))||visitNodes( cbNodes, (node.asInstanceOf[EnumDeclaration]).members ))
  case  SyntaxKind.EnumMember  =>
return (visitNode( cbNode, (node.asInstanceOf[EnumMember]).name )||visitNode( cbNode, (node.asInstanceOf[EnumMember]).initializer ))
  case  SyntaxKind.ModuleDeclaration  =>
return (((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[ModuleDeclaration]).name ))||visitNode( cbNode, (node.asInstanceOf[ModuleDeclaration]).body ))
  case  SyntaxKind.ImportEqualsDeclaration  =>
return (((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[ImportEqualsDeclaration]).name ))||visitNode( cbNode, (node.asInstanceOf[ImportEqualsDeclaration]).moduleReference ))
  case  SyntaxKind.ImportDeclaration  =>
return (((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[ImportDeclaration]).importClause ))||visitNode( cbNode, (node.asInstanceOf[ImportDeclaration]).moduleSpecifier ))
  case  SyntaxKind.ImportClause  =>
return (visitNode( cbNode, (node.asInstanceOf[ImportClause]).name )||visitNode( cbNode, (node.asInstanceOf[ImportClause]).namedBindings ))
  case  SyntaxKind.NamespaceExportDeclaration  =>
return visitNode( cbNode, (node.asInstanceOf[NamespaceExportDeclaration]).name )
  case  SyntaxKind.NamespaceImport  =>
return visitNode( cbNode, (node.asInstanceOf[NamespaceImport]).name )
  case  SyntaxKind.NamedImports | SyntaxKind.NamedExports  =>
return visitNodes( cbNodes, (node.asInstanceOf[NamedImportsOrExports]).elements )
  case  SyntaxKind.ExportDeclaration  =>
return (((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[ExportDeclaration]).exportClause ))||visitNode( cbNode, (node.asInstanceOf[ExportDeclaration]).moduleSpecifier ))
  case  SyntaxKind.ImportSpecifier | SyntaxKind.ExportSpecifier  =>
return (visitNode( cbNode, (node.asInstanceOf[ImportOrExportSpecifier]).propertyName )||visitNode( cbNode, (node.asInstanceOf[ImportOrExportSpecifier]).name ))
  case  SyntaxKind.ExportAssignment  =>
return ((visitNodes( cbNodes, node.decorators )||visitNodes( cbNodes, node.modifiers ))||visitNode( cbNode, (node.asInstanceOf[ExportAssignment]).expression ))
  case  SyntaxKind.TemplateExpression  =>
return (visitNode( cbNode, (node.asInstanceOf[TemplateExpression]).head )||visitNodes( cbNodes, (node.asInstanceOf[TemplateExpression]).templateSpans ))
  case  SyntaxKind.TemplateSpan  =>
return (visitNode( cbNode, (node.asInstanceOf[TemplateSpan]).expression )||visitNode( cbNode, (node.asInstanceOf[TemplateSpan]).literal ))
  case  SyntaxKind.ComputedPropertyName  =>
return visitNode( cbNode, (node.asInstanceOf[ComputedPropertyName]).expression )
  case  SyntaxKind.HeritageClause  =>
return visitNodes( cbNodes, (node.asInstanceOf[HeritageClause]).types )
  case  SyntaxKind.ExpressionWithTypeArguments  =>
return (visitNode( cbNode, (node.asInstanceOf[ExpressionWithTypeArguments]).expression )||visitNodes( cbNodes, (node.asInstanceOf[ExpressionWithTypeArguments]).typeArguments ))
  case  SyntaxKind.ExternalModuleReference  =>
return visitNode( cbNode, (node.asInstanceOf[ExternalModuleReference]).expression )
  case  SyntaxKind.MissingDeclaration  =>
return visitNodes( cbNodes, node.decorators )
  case  SyntaxKind.JsxElement  =>
return ((visitNode( cbNode, (node.asInstanceOf[JsxElement]).openingElement )||visitNodes( cbNodes, (node.asInstanceOf[JsxElement]).children ))||visitNode( cbNode, (node.asInstanceOf[JsxElement]).closingElement ))
  case  SyntaxKind.JsxSelfClosingElement | SyntaxKind.JsxOpeningElement  =>
return (visitNode( cbNode, (node.asInstanceOf[JsxOpeningLikeElement]).tagName )||visitNodes( cbNodes, (node.asInstanceOf[JsxOpeningLikeElement]).attributes ))
  case  SyntaxKind.JsxAttribute  =>
return (visitNode( cbNode, (node.asInstanceOf[JsxAttribute]).name )||visitNode( cbNode, (node.asInstanceOf[JsxAttribute]).initializer ))
  case  SyntaxKind.JsxSpreadAttribute  =>
return visitNode( cbNode, (node.asInstanceOf[JsxSpreadAttribute]).expression )
  case  SyntaxKind.JsxExpression  =>
return visitNode( cbNode, (node.asInstanceOf[JsxExpression]).expression )
  case  SyntaxKind.JsxClosingElement  =>
return visitNode( cbNode, (node.asInstanceOf[JsxClosingElement]).tagName )
  case  SyntaxKind.JSDocTypeExpression  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocTypeExpression]).`type` )
  case  SyntaxKind.JSDocUnionType  =>
return visitNodes( cbNodes, (node.asInstanceOf[JSDocUnionType]).types )
  case  SyntaxKind.JSDocTupleType  =>
return visitNodes( cbNodes, (node.asInstanceOf[JSDocTupleType]).types )
  case  SyntaxKind.JSDocArrayType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocArrayType]).elementType )
  case  SyntaxKind.JSDocNonNullableType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocNonNullableType]).`type` )
  case  SyntaxKind.JSDocNullableType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocNullableType]).`type` )
  case  SyntaxKind.JSDocRecordType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocRecordType]).literal )
  case  SyntaxKind.JSDocTypeReference  =>
return (visitNode( cbNode, (node.asInstanceOf[JSDocTypeReference]).name )||visitNodes( cbNodes, (node.asInstanceOf[JSDocTypeReference]).typeArguments ))
  case  SyntaxKind.JSDocOptionalType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocOptionalType]).`type` )
  case  SyntaxKind.JSDocFunctionType  =>
return (visitNodes( cbNodes, (node.asInstanceOf[JSDocFunctionType]).parameters )||visitNode( cbNode, (node.asInstanceOf[JSDocFunctionType]).`type` ))
  case  SyntaxKind.JSDocVariadicType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocVariadicType]).`type` )
  case  SyntaxKind.JSDocConstructorType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocConstructorType]).`type` )
  case  SyntaxKind.JSDocThisType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocThisType]).`type` )
  case  SyntaxKind.JSDocRecordMember  =>
return (visitNode( cbNode, (node.asInstanceOf[JSDocRecordMember]).name )||visitNode( cbNode, (node.asInstanceOf[JSDocRecordMember]).`type` ))
  case  SyntaxKind.JSDocComment  =>
return visitNodes( cbNodes, (node.asInstanceOf[JSDoc]).tags )
  case  SyntaxKind.JSDocParameterTag  =>
return ((visitNode( cbNode, (node.asInstanceOf[JSDocParameterTag]).preParameterName )||visitNode( cbNode, (node.asInstanceOf[JSDocParameterTag]).typeExpression ))||visitNode( cbNode, (node.asInstanceOf[JSDocParameterTag]).postParameterName ))
  case  SyntaxKind.JSDocReturnTag  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocReturnTag]).typeExpression )
  case  SyntaxKind.JSDocTypeTag  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocTypeTag]).typeExpression )
  case  SyntaxKind.JSDocTemplateTag  =>
return visitNodes( cbNodes, (node.asInstanceOf[JSDocTemplateTag]).typeParameters )
  case  SyntaxKind.JSDocTypedefTag  =>
return (((visitNode( cbNode, (node.asInstanceOf[JSDocTypedefTag]).typeExpression )||visitNode( cbNode, (node.asInstanceOf[JSDocTypedefTag]).fullName ))||visitNode( cbNode, (node.asInstanceOf[JSDocTypedefTag]).name ))||visitNode( cbNode, (node.asInstanceOf[JSDocTypedefTag]).jsDocTypeLiteral ))
  case  SyntaxKind.JSDocTypeLiteral  =>
return visitNodes( cbNodes, (node.asInstanceOf[JSDocTypeLiteral]).jsDocPropertyTags )
  case  SyntaxKind.JSDocPropertyTag  =>
return (visitNode( cbNode, (node.asInstanceOf[JSDocPropertyTag]).typeExpression )||visitNode( cbNode, (node.asInstanceOf[JSDocPropertyTag]).name ))
  case  SyntaxKind.PartiallyEmittedExpression  =>
return visitNode( cbNode, (node.asInstanceOf[PartiallyEmittedExpression]).expression )
  case  SyntaxKind.JSDocLiteralType  =>
return visitNode( cbNode, (node.asInstanceOf[JSDocLiteralType]).literal )
  case _ =>
}

}
def createSourceFile(fileName: String, sourceText: String, languageVersion: ScriptTarget, setParentNodes: Nothing = false, scriptKind: ScriptKind): SourceFile = {
 performance.mark( "beforeParse" )
val result = Parser.parseSourceFile( fileName, sourceText, languageVersion, undefined, setParentNodes, scriptKind )
performance.mark( "afterParse" )
performance.measure( "Parse", "beforeParse", "afterParse" )
return result

}
def isExternalModule(file: SourceFile): Boolean = {
 return (file.externalModuleIndicator!==undefined)

}
def updateSourceFile(sourceFile: SourceFile, newText: String, textChangeRange: TextChangeRange, aggressiveChecks: Boolean): SourceFile = {
 return IncrementalParser.updateSourceFile( sourceFile, newText, textChangeRange, aggressiveChecks )

}
def parseIsolatedJSDocComment(content: String, start: Int, length: Int) = {
 val result = Parser.JSDocParser.parseIsolatedJSDocComment( content, start, length )
if ((result&&result.jsDoc)) {
 Parser.fixupParentReferences( result.jsDoc )

}
return result

}
def parseJSDocTypeExpressionForTests(content: String, start: Int, length: Int) = {
 return Parser.JSDocParser.parseJSDocTypeExpressionForTests( content, start, length )

}
object Parser {
val scanner = createScanner( ScriptTarget.Latest, true )
val disallowInAndDecoratorContext = (NodeFlags.DisallowInContext|NodeFlags.DecoratorContext)
var NodeConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
var TokenConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
var IdentifierConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
var SourceFileConstructor: ((SyntaxKind, Int, Int) => Node) = zeroOfMyType
var sourceFile: SourceFile = zeroOfMyType
var parseDiagnostics: Array[Diagnostic] = zeroOfMyType
var syntaxCursor: IncrementalParser.SyntaxCursor = zeroOfMyType
var currentToken: SyntaxKind = zeroOfMyType
var sourceText: String = zeroOfMyType
var nodeCount: Int = zeroOfMyType
var identifiers: Map[String] = zeroOfMyType
var identifierCount: Int = zeroOfMyType
var parsingContext: ParsingContext = zeroOfMyType
var contextFlags: NodeFlags = zeroOfMyType
var parseErrorBeforeNextFinishedNode = false
def parseSourceFile(fileName: String, sourceText: String, languageVersion: ScriptTarget, syntaxCursor: IncrementalParser.SyntaxCursor, setParentNodes: Boolean, scriptKind: ScriptKind): SourceFile = {
 (scriptKind=ensureScriptKind( fileName, scriptKind ))
initializeState( sourceText, languageVersion, syntaxCursor, scriptKind )
val result = parseSourceFileWorker( fileName, languageVersion, setParentNodes, scriptKind )
clearState()
return result

}
def getLanguageVariant(scriptKind: ScriptKind) = {
 return (if ((((scriptKind===ScriptKind.TSX)||(scriptKind===ScriptKind.JSX))||(scriptKind===ScriptKind.JS))) LanguageVariant.JSX else LanguageVariant.Standard)

}
def initializeState(_sourceText: String, languageVersion: ScriptTarget, _syntaxCursor: IncrementalParser.SyntaxCursor, scriptKind: ScriptKind) = {
 (NodeConstructor=objectAllocator.getNodeConstructor())
(TokenConstructor=objectAllocator.getTokenConstructor())
(IdentifierConstructor=objectAllocator.getIdentifierConstructor())
(SourceFileConstructor=objectAllocator.getSourceFileConstructor())
(sourceText=_sourceText)
(syntaxCursor=_syntaxCursor)
(parseDiagnostics=Array())
(parsingContext=0)
(identifiers=createMap[ String ]())
(identifierCount=0)
(nodeCount=0)
(contextFlags=(if (((scriptKind===ScriptKind.JS)||(scriptKind===ScriptKind.JSX))) NodeFlags.JavaScriptFile else NodeFlags.None))
(parseErrorBeforeNextFinishedNode=false)
scanner.setText( sourceText )
scanner.setOnError( scanError )
scanner.setScriptTarget( languageVersion )
scanner.setLanguageVariant( getLanguageVariant( scriptKind ) )

}
def clearState() = {
 scanner.setText( "" )
scanner.setOnError( undefined )
(parseDiagnostics=undefined)
(sourceFile=undefined)
(identifiers=undefined)
(syntaxCursor=undefined)
(sourceText=undefined)

}
def parseSourceFileWorker(fileName: String, languageVersion: ScriptTarget, setParentNodes: Boolean, scriptKind: ScriptKind): SourceFile = {
 (sourceFile=createSourceFile( fileName, languageVersion, scriptKind ))
(sourceFile.flags=contextFlags)
nextToken()
processReferenceComments( sourceFile )
(sourceFile.statements=parseList( ParsingContext.SourceElements, parseStatement ))
Debug.assert( (token()===SyntaxKind.EndOfFileToken) )
(sourceFile.endOfFileToken=parseTokenNode().asInstanceOf[EndOfFileToken])
setExternalModuleIndicator( sourceFile )
(sourceFile.nodeCount=nodeCount)
(sourceFile.identifierCount=identifierCount)
(sourceFile.identifiers=identifiers)
(sourceFile.parseDiagnostics=parseDiagnostics)
if (setParentNodes) {
 fixupParentReferences( sourceFile )

}
return sourceFile

}
def addJSDocComment[T <: Node](node: T): T = {
 val comments = getJsDocCommentsFromText( node, sourceFile.text )
if (comments) {
 (comments).foreach { fresh2 =>
val comment = zeroOfMyType
 = fresh2
 {
 val jsDoc = JSDocParser.parseJSDocComment( node, comment.pos, (comment.end-comment.pos) )
if ((!jsDoc)) {
 continue

}
if ((!node.jsDocComments)) {
 (node.jsDocComments=Array())

}
node.jsDocComments.push( jsDoc )

}
}

}
return node

}
def fixupParentReferences(rootNode: Node) = {
 var parent: Node = rootNode
forEachChild( rootNode, visitNode )
return
def visitNode(n: Node): Unit = {
 if ((n.parent!==parent)) {
 (n.parent=parent)
val saveParent = parent
(parent=n)
forEachChild( n, visitNode )
if (n.jsDocComments) {
 (n.jsDocComments).foreach { fresh3 =>
val jsDocComment = zeroOfMyType
 = fresh3
 {
 (jsDocComment.parent=n)
(parent=jsDocComment)
forEachChild( jsDocComment, visitNode )

}
}

}
(parent=saveParent)

}

}

}
def createSourceFile(fileName: String, languageVersion: ScriptTarget, scriptKind: ScriptKind): SourceFile = {
 val sourceFile = new SourceFileConstructor( SyntaxKind.SourceFile, 0, sourceText.length ).asInstanceOf[SourceFile]
(nodeCount+= 1)
(sourceFile.text=sourceText)
(sourceFile.bindDiagnostics=Array())
(sourceFile.languageVersion=languageVersion)
(sourceFile.fileName=normalizePath( fileName ))
(sourceFile.languageVariant=getLanguageVariant( scriptKind ))
(sourceFile.isDeclarationFile=fileExtensionIs( sourceFile.fileName, ".d.ts" ))
(sourceFile.scriptKind=scriptKind)
return sourceFile

}
def setContextFlag(`val`: Boolean, flag: NodeFlags) = {
 if (`val`) {
 (contextFlags|=flag)

}
else {
 (contextFlags&=(~flag))

}

}
def setDisallowInContext(`val`: Boolean) = {
 setContextFlag( `val`, NodeFlags.DisallowInContext )

}
def setYieldContext(`val`: Boolean) = {
 setContextFlag( `val`, NodeFlags.YieldContext )

}
def setDecoratorContext(`val`: Boolean) = {
 setContextFlag( `val`, NodeFlags.DecoratorContext )

}
def setAwaitContext(`val`: Boolean) = {
 setContextFlag( `val`, NodeFlags.AwaitContext )

}
def doOutsideOfContext[T](context: NodeFlags, func: (() => T)): T = {
 val contextFlagsToClear = (context&contextFlags)
if (contextFlagsToClear) {
 setContextFlag( false, contextFlagsToClear )
val result = func()
setContextFlag( true, contextFlagsToClear )
return result

}
return func()

}
def doInsideOfContext[T](context: NodeFlags, func: (() => T)): T = {
 val contextFlagsToSet = (context&(~contextFlags))
if (contextFlagsToSet) {
 setContextFlag( true, contextFlagsToSet )
val result = func()
setContextFlag( false, contextFlagsToSet )
return result

}
return func()

}
def allowInAnd[T](func: (() => T)): T = {
 return doOutsideOfContext( NodeFlags.DisallowInContext, func )

}
def disallowInAnd[T](func: (() => T)): T = {
 return doInsideOfContext( NodeFlags.DisallowInContext, func )

}
def doInYieldContext[T](func: (() => T)): T = {
 return doInsideOfContext( NodeFlags.YieldContext, func )

}
def doInDecoratorContext[T](func: (() => T)): T = {
 return doInsideOfContext( NodeFlags.DecoratorContext, func )

}
def doInAwaitContext[T](func: (() => T)): T = {
 return doInsideOfContext( NodeFlags.AwaitContext, func )

}
def doOutsideOfAwaitContext[T](func: (() => T)): T = {
 return doOutsideOfContext( NodeFlags.AwaitContext, func )

}
def doInYieldAndAwaitContext[T](func: (() => T)): T = {
 return doInsideOfContext( (NodeFlags.YieldContext|NodeFlags.AwaitContext), func )

}
def inContext(flags: NodeFlags) = {
 return (((contextFlags&flags))!==0)

}
def inYieldContext() = {
 return inContext( NodeFlags.YieldContext )

}
def inDisallowInContext() = {
 return inContext( NodeFlags.DisallowInContext )

}
def inDecoratorContext() = {
 return inContext( NodeFlags.DecoratorContext )

}
def inAwaitContext() = {
 return inContext( NodeFlags.AwaitContext )

}
def parseErrorAtCurrentToken(message: DiagnosticMessage, arg0: Any): Unit = {
 val start = scanner.getTokenPos()
val length = (scanner.getTextPos()-start)
parseErrorAtPosition( start, length, message, arg0 )

}
def parseErrorAtPosition(start: Int, length: Int, message: DiagnosticMessage, arg0: Any): Unit = {
 val lastError = lastOrUndefined( parseDiagnostics )
if (((!lastError)||(start!==lastError.start))) {
 parseDiagnostics.push( createFileDiagnostic( sourceFile, start, length, message, arg0 ) )

}
(parseErrorBeforeNextFinishedNode=true)

}
def scanError(message: DiagnosticMessage, length: Int) = {
 val pos = scanner.getTextPos()
parseErrorAtPosition( pos, (length||0), message )

}
def getNodePos(): Int = {
 return scanner.getStartPos()

}
def getNodeEnd(): Int = {
 return scanner.getStartPos()

}
def token(): SyntaxKind = {
 return currentToken

}
def nextToken(): SyntaxKind = {
 return (currentToken=scanner.scan())

}
def reScanGreaterToken(): SyntaxKind = {
 return (currentToken=scanner.reScanGreaterToken())

}
def reScanSlashToken(): SyntaxKind = {
 return (currentToken=scanner.reScanSlashToken())

}
def reScanTemplateToken(): SyntaxKind = {
 return (currentToken=scanner.reScanTemplateToken())

}
def scanJsxIdentifier(): SyntaxKind = {
 return (currentToken=scanner.scanJsxIdentifier())

}
def scanJsxText(): SyntaxKind = {
 return (currentToken=scanner.scanJsxToken())

}
def scanJsxAttributeValue(): SyntaxKind = {
 return (currentToken=scanner.scanJsxAttributeValue())

}
def speculationHelper[T](callback: (() => T), isLookAhead: Boolean): T = {
 val saveToken = currentToken
val saveParseDiagnosticsLength = parseDiagnostics.length
val saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode
val saveContextFlags = contextFlags
val result = (if (isLookAhead) scanner.lookAhead( callback ) else scanner.tryScan( callback ))
Debug.assert( (saveContextFlags===contextFlags) )
if (((!result)||isLookAhead)) {
 (currentToken=saveToken)
(parseDiagnostics.length=saveParseDiagnosticsLength)
(parseErrorBeforeNextFinishedNode=saveParseErrorBeforeNextFinishedNode)

}
return result

}
def lookAhead[T](callback: (() => T)): T = {
 return speculationHelper( callback, true )

}
def tryParse[T](callback: (() => T)): T = {
 return speculationHelper( callback, false )

}
def isIdentifier(): Boolean = {
 if ((token()===SyntaxKind.Identifier)) {
 return true

}
if (((token()===SyntaxKind.YieldKeyword)&&inYieldContext())) {
 return false

}
if (((token()===SyntaxKind.AwaitKeyword)&&inAwaitContext())) {
 return false

}
return (token()>SyntaxKind.LastReservedWord)

}
def parseExpected(kind: SyntaxKind, diagnosticMessage: DiagnosticMessage, shouldAdvance: Nothing = true): Boolean = {
 if ((token()===kind)) {
 if (shouldAdvance) {
 nextToken()

}
return true

}
if (diagnosticMessage) {
 parseErrorAtCurrentToken( diagnosticMessage )

}
else {
 parseErrorAtCurrentToken( Diagnostics._0_expected, tokenToString( kind ) )

}
return false

}
def parseOptional(t: SyntaxKind): Boolean = {
 if ((token()===t)) {
 nextToken()
return true

}
return false

}
def parseOptionalToken[TKind <: SyntaxKind](t: TKind): Token[TKind]
def parseOptionalToken(t: SyntaxKind): Node = {
 if ((token()===t)) {
 return parseTokenNode()

}
return undefined

}
def parseExpectedToken[TKind <: SyntaxKind](t: TKind, reportAtCurrentPosition: Boolean, diagnosticMessage: DiagnosticMessage, arg0: Any): Token[TKind]
def parseExpectedToken(t: SyntaxKind, reportAtCurrentPosition: Boolean, diagnosticMessage: DiagnosticMessage, arg0: Any): Node = {
 return (parseOptionalToken( t )||createMissingNode( t, reportAtCurrentPosition, diagnosticMessage, arg0 ))

}
def parseTokenNode[T <: Node](): T = {
 val node = createNode( token() ).asInstanceOf[T]
nextToken()
return finishNode( node )

}
def canParseSemicolon() = {
 if ((token()===SyntaxKind.SemicolonToken)) {
 return true

}
return (((token()===SyntaxKind.CloseBraceToken)||(token()===SyntaxKind.EndOfFileToken))||scanner.hasPrecedingLineBreak())

}
def parseSemicolon(): Boolean = {
 if (canParseSemicolon()) {
 if ((token()===SyntaxKind.SemicolonToken)) {
 nextToken()

}
return true

}
else {
 return parseExpected( SyntaxKind.SemicolonToken )

}

}
def createNode[TKind <: SyntaxKind](kind: TKind, pos: Int): ( Node | Token[TKind] | Identifier ) = {
 (nodeCount+= 1)
if ((!((pos>=0)))) {
 (pos=scanner.getStartPos())

}
return (if ((kind>=SyntaxKind.FirstNode)) new NodeConstructor( kind, pos, pos ) else (if ((kind===SyntaxKind.Identifier)) new IdentifierConstructor( kind, pos, pos ) else new TokenConstructor( kind, pos, pos )))

}
def createNodeArray[T <: Node](elements: Array[T], pos: Int): NodeArray[T] = {
 val array = ((elements||Array())).asInstanceOf[NodeArray[T]]
if ((!((pos>=0)))) {
 (pos=getNodePos())

}
(array.pos=pos)
(array.end=pos)
return array

}
def finishNode[T <: Node](node: T, end: Int): T = {
 (node.end=(if ((end===undefined)) scanner.getStartPos() else end))
if (contextFlags) {
 (node.flags|=contextFlags)

}
if (parseErrorBeforeNextFinishedNode) {
 (parseErrorBeforeNextFinishedNode=false)
(node.flags|=NodeFlags.ThisNodeHasError)

}
return node

}
def createMissingNode(kind: SyntaxKind, reportAtCurrentPosition: Boolean, diagnosticMessage: DiagnosticMessage, arg0: Any): Node = {
 if (reportAtCurrentPosition) {
 parseErrorAtPosition( scanner.getStartPos(), 0, diagnosticMessage, arg0 )

}
else {
 parseErrorAtCurrentToken( diagnosticMessage, arg0 )

}
val result = createNode( kind, scanner.getStartPos() )
((result.asInstanceOf[Identifier]).text="")
return finishNode( result )

}
def internIdentifier(text: String): String = {
 (text=escapeIdentifier( text ))
return (identifiers(text)||((identifiers(text)=text)))

}
def createIdentifier(isIdentifier: Boolean, diagnosticMessage: DiagnosticMessage): Identifier = {
 (identifierCount+= 1)
if (isIdentifier) {
 val node = createNode( SyntaxKind.Identifier ).asInstanceOf[Identifier]
if ((token()!==SyntaxKind.Identifier)) {
 (node.originalKeywordKind=token())

}
(node.text=internIdentifier( scanner.getTokenValue() ))
nextToken()
return finishNode( node )

}
return createMissingNode( SyntaxKind.Identifier, false, (diagnosticMessage||Diagnostics.Identifier_expected) ).asInstanceOf[Identifier]

}
def parseIdentifier(diagnosticMessage: DiagnosticMessage): Identifier = {
 return createIdentifier( isIdentifier(), diagnosticMessage )

}
def parseIdentifierName(): Identifier = {
 return createIdentifier( tokenIsIdentifierOrKeyword( token() ) )

}
def isLiteralPropertyName(): Boolean = {
 return ((tokenIsIdentifierOrKeyword( token() )||(token()===SyntaxKind.StringLiteral))||(token()===SyntaxKind.NumericLiteral))

}
def parsePropertyNameWorker(allowComputedPropertyNames: Boolean): PropertyName = {
 if (((token()===SyntaxKind.StringLiteral)||(token()===SyntaxKind.NumericLiteral))) {
 return parseLiteralNode( true )

}
if ((allowComputedPropertyNames&&(token()===SyntaxKind.OpenBracketToken))) {
 return parseComputedPropertyName()

}
return parseIdentifierName()

}
def parsePropertyName(): PropertyName = {
 return parsePropertyNameWorker( true )

}
def parseSimplePropertyName(): ( Identifier | LiteralExpression ) = {
 return parsePropertyNameWorker( false ).asInstanceOf[( Identifier | LiteralExpression )]

}
def isSimplePropertyName() = {
 return (((token()===SyntaxKind.StringLiteral)||(token()===SyntaxKind.NumericLiteral))||tokenIsIdentifierOrKeyword( token() ))

}
def parseComputedPropertyName(): ComputedPropertyName = {
 val node = createNode( SyntaxKind.ComputedPropertyName ).asInstanceOf[ComputedPropertyName]
parseExpected( SyntaxKind.OpenBracketToken )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseBracketToken )
return finishNode( node )

}
def parseContextualModifier(t: SyntaxKind): Boolean = {
 return ((token()===t)&&tryParse( nextTokenCanFollowModifier ))

}
def nextTokenIsOnSameLineAndCanFollowModifier() = {
 nextToken()
if (scanner.hasPrecedingLineBreak()) {
 return false

}
return canFollowModifier()

}
def nextTokenCanFollowModifier() = {
 if ((token()===SyntaxKind.ConstKeyword)) {
 return (nextToken()===SyntaxKind.EnumKeyword)

}
if ((token()===SyntaxKind.ExportKeyword)) {
 nextToken()
if ((token()===SyntaxKind.DefaultKeyword)) {
 return lookAhead( nextTokenIsClassOrFunctionOrAsync )

}
return ((((token()!==SyntaxKind.AsteriskToken)&&(token()!==SyntaxKind.AsKeyword))&&(token()!==SyntaxKind.OpenBraceToken))&&canFollowModifier())

}
if ((token()===SyntaxKind.DefaultKeyword)) {
 return nextTokenIsClassOrFunctionOrAsync()

}
if ((token()===SyntaxKind.StaticKeyword)) {
 nextToken()
return canFollowModifier()

}
return nextTokenIsOnSameLineAndCanFollowModifier()

}
def parseAnyContextualModifier(): Boolean = {
 return (isModifierKind( token() )&&tryParse( nextTokenCanFollowModifier ))

}
def canFollowModifier(): Boolean = {
 return (((((token()===SyntaxKind.OpenBracketToken)||(token()===SyntaxKind.OpenBraceToken))||(token()===SyntaxKind.AsteriskToken))||(token()===SyntaxKind.DotDotDotToken))||isLiteralPropertyName())

}
def nextTokenIsClassOrFunctionOrAsync(): Boolean = {
 nextToken()
return (((token()===SyntaxKind.ClassKeyword)||(token()===SyntaxKind.FunctionKeyword))||(((token()===SyntaxKind.AsyncKeyword)&&lookAhead( nextTokenIsFunctionKeywordOnSameLine ))))

}
def isListElement(parsingContext: ParsingContext, inErrorRecovery: Boolean): Boolean = {
 val node = currentNode( parsingContext )
if (node) {
 return true

}
parsingContext match {
  case  ParsingContext.SourceElements | ParsingContext.BlockStatements | ParsingContext.SwitchClauseStatements  =>
return ((!(((token()===SyntaxKind.SemicolonToken)&&inErrorRecovery)))&&isStartOfStatement())
  case  ParsingContext.SwitchClauses  =>
return ((token()===SyntaxKind.CaseKeyword)||(token()===SyntaxKind.DefaultKeyword))
  case  ParsingContext.TypeMembers  =>
return lookAhead( isTypeMemberStart )
  case  ParsingContext.ClassMembers  =>
return (lookAhead( isClassMemberStart )||(((token()===SyntaxKind.SemicolonToken)&&(!inErrorRecovery))))
  case  ParsingContext.EnumMembers  =>
return ((token()===SyntaxKind.OpenBracketToken)||isLiteralPropertyName())
  case  ParsingContext.ObjectLiteralMembers  =>
return (((token()===SyntaxKind.OpenBracketToken)||(token()===SyntaxKind.AsteriskToken))||isLiteralPropertyName())
  case  ParsingContext.ObjectBindingElements  =>
return ((token()===SyntaxKind.OpenBracketToken)||isLiteralPropertyName())
  case  ParsingContext.HeritageClauseElement  =>
if ((token()===SyntaxKind.OpenBraceToken)) {
 return lookAhead( isValidHeritageClauseObjectLiteral )

}
if ((!inErrorRecovery)) {
 return (isStartOfLeftHandSideExpression()&&(!isHeritageClauseExtendsOrImplementsKeyword()))

}
else {
 return (isIdentifier()&&(!isHeritageClauseExtendsOrImplementsKeyword()))

}
  case  ParsingContext.VariableDeclarations  =>
return isIdentifierOrPattern()
  case  ParsingContext.ArrayBindingElements  =>
return (((token()===SyntaxKind.CommaToken)||(token()===SyntaxKind.DotDotDotToken))||isIdentifierOrPattern())
  case  ParsingContext.TypeParameters  =>
return isIdentifier()
  case  ParsingContext.ArgumentExpressions | ParsingContext.ArrayLiteralMembers  =>
return (((token()===SyntaxKind.CommaToken)||(token()===SyntaxKind.DotDotDotToken))||isStartOfExpression())
  case  ParsingContext.Parameters  =>
return isStartOfParameter()
  case  ParsingContext.TypeArguments | ParsingContext.TupleElementTypes  =>
return ((token()===SyntaxKind.CommaToken)||isStartOfType())
  case  ParsingContext.HeritageClauses  =>
return isHeritageClause()
  case  ParsingContext.ImportOrExportSpecifiers  =>
return tokenIsIdentifierOrKeyword( token() )
  case  ParsingContext.JsxAttributes  =>
return (tokenIsIdentifierOrKeyword( token() )||(token()===SyntaxKind.OpenBraceToken))
  case  ParsingContext.JsxChildren  =>
return true
  case  ParsingContext.JSDocFunctionParameters | ParsingContext.JSDocTypeArguments | ParsingContext.JSDocTupleTypes  =>
return JSDocParser.isJSDocType()
  case  ParsingContext.JSDocRecordMembers  =>
return isSimplePropertyName()
  case _ =>
}
Debug.fail( "Non-exhaustive case in 'isListElement'." )

}
def isValidHeritageClauseObjectLiteral() = {
 Debug.assert( (token()===SyntaxKind.OpenBraceToken) )
if ((nextToken()===SyntaxKind.CloseBraceToken)) {
 val next = nextToken()
return ((((next===SyntaxKind.CommaToken)||(next===SyntaxKind.OpenBraceToken))||(next===SyntaxKind.ExtendsKeyword))||(next===SyntaxKind.ImplementsKeyword))

}
return true

}
def nextTokenIsIdentifier() = {
 nextToken()
return isIdentifier()

}
def nextTokenIsIdentifierOrKeyword() = {
 nextToken()
return tokenIsIdentifierOrKeyword( token() )

}
def isHeritageClauseExtendsOrImplementsKeyword(): Boolean = {
 if (((token()===SyntaxKind.ImplementsKeyword)||(token()===SyntaxKind.ExtendsKeyword))) {
 return lookAhead( nextTokenIsStartOfExpression )

}
return false

}
def nextTokenIsStartOfExpression() = {
 nextToken()
return isStartOfExpression()

}
def isListTerminator(kind: ParsingContext): Boolean = {
 if ((token()===SyntaxKind.EndOfFileToken)) {
 return true

}
kind match {
  case  ParsingContext.BlockStatements | ParsingContext.SwitchClauses | ParsingContext.TypeMembers | ParsingContext.ClassMembers | ParsingContext.EnumMembers | ParsingContext.ObjectLiteralMembers | ParsingContext.ObjectBindingElements | ParsingContext.ImportOrExportSpecifiers  =>
return (token()===SyntaxKind.CloseBraceToken)
  case  ParsingContext.SwitchClauseStatements  =>
return (((token()===SyntaxKind.CloseBraceToken)||(token()===SyntaxKind.CaseKeyword))||(token()===SyntaxKind.DefaultKeyword))
  case  ParsingContext.HeritageClauseElement  =>
return (((token()===SyntaxKind.OpenBraceToken)||(token()===SyntaxKind.ExtendsKeyword))||(token()===SyntaxKind.ImplementsKeyword))
  case  ParsingContext.VariableDeclarations  =>
return isVariableDeclaratorListTerminator()
  case  ParsingContext.TypeParameters  =>
return (((((token()===SyntaxKind.GreaterThanToken)||(token()===SyntaxKind.OpenParenToken))||(token()===SyntaxKind.OpenBraceToken))||(token()===SyntaxKind.ExtendsKeyword))||(token()===SyntaxKind.ImplementsKeyword))
  case  ParsingContext.ArgumentExpressions  =>
return ((token()===SyntaxKind.CloseParenToken)||(token()===SyntaxKind.SemicolonToken))
  case  ParsingContext.ArrayLiteralMembers | ParsingContext.TupleElementTypes | ParsingContext.ArrayBindingElements  =>
return (token()===SyntaxKind.CloseBracketToken)
  case  ParsingContext.Parameters  =>
return ((token()===SyntaxKind.CloseParenToken)||(token()===SyntaxKind.CloseBracketToken))
  case  ParsingContext.TypeArguments  =>
return (token()!==SyntaxKind.CommaToken)
  case  ParsingContext.HeritageClauses  =>
return ((token()===SyntaxKind.OpenBraceToken)||(token()===SyntaxKind.CloseBraceToken))
  case  ParsingContext.JsxAttributes  =>
return ((token()===SyntaxKind.GreaterThanToken)||(token()===SyntaxKind.SlashToken))
  case  ParsingContext.JsxChildren  =>
return ((token()===SyntaxKind.LessThanToken)&&lookAhead( nextTokenIsSlash ))
  case  ParsingContext.JSDocFunctionParameters  =>
return (((token()===SyntaxKind.CloseParenToken)||(token()===SyntaxKind.ColonToken))||(token()===SyntaxKind.CloseBraceToken))
  case  ParsingContext.JSDocTypeArguments  =>
return ((token()===SyntaxKind.GreaterThanToken)||(token()===SyntaxKind.CloseBraceToken))
  case  ParsingContext.JSDocTupleTypes  =>
return ((token()===SyntaxKind.CloseBracketToken)||(token()===SyntaxKind.CloseBraceToken))
  case  ParsingContext.JSDocRecordMembers  =>
return (token()===SyntaxKind.CloseBraceToken)
  case _ =>
}

}
def isVariableDeclaratorListTerminator(): Boolean = {
 if (canParseSemicolon()) {
 return true

}
if (isInOrOfKeyword( token() )) {
 return true

}
if ((token()===SyntaxKind.EqualsGreaterThanToken)) {
 return true

}
return false

}
def isInSomeParsingContext(): Boolean = {
 {
var kind = 0
while( (kind<ParsingContext.Count)) {
 {
 if ((parsingContext&((1<<kind)))) {
 if ((isListElement( kind, true )||isListTerminator( kind ))) {
 return true

}

}

}
 (kind+= 1)
}
}
return false

}
def parseList[T <: Node](kind: ParsingContext, parseElement: (() => T)): NodeArray[T] = {
 val saveParsingContext = parsingContext
(parsingContext|=(1<<kind))
val result = createNodeArray[ T ]()
while ((!isListTerminator( kind ))) {
{
 if (isListElement( kind, false )) {
 val element = parseListElement( kind, parseElement )
result.push( element )
continue

}
if (abortParsingListOrMoveToNextToken( kind )) {
 break()

}

}
}
(result.end=getNodeEnd())
(parsingContext=saveParsingContext)
return result

}
def parseListElement[T <: Node](parsingContext: ParsingContext, parseElement: (() => T)): T = {
 val node = currentNode( parsingContext )
if (node) {
 return consumeNode( node ).asInstanceOf[T]

}
return parseElement()

}
def currentNode(parsingContext: ParsingContext): Node = {
 if (parseErrorBeforeNextFinishedNode) {
 return undefined

}
if ((!syntaxCursor)) {
 return undefined

}
val node = syntaxCursor.currentNode( scanner.getStartPos() )
if (nodeIsMissing( node )) {
 return undefined

}
if (node.intersectsChange) {
 return undefined

}
if (containsParseError( node )) {
 return undefined

}
val nodeContextFlags = (node.flags&NodeFlags.ContextFlags)
if ((nodeContextFlags!==contextFlags)) {
 return undefined

}
if ((!canReuseNode( node, parsingContext ))) {
 return undefined

}
return node

}
def consumeNode(node: Node) = {
 scanner.setTextPos( node.end )
nextToken()
return node

}
def canReuseNode(node: Node, parsingContext: ParsingContext): Boolean = {
 parsingContext match {
  case  ParsingContext.ClassMembers  =>
return isReusableClassMember( node )
  case  ParsingContext.SwitchClauses  =>
return isReusableSwitchClause( node )
  case  ParsingContext.SourceElements | ParsingContext.BlockStatements | ParsingContext.SwitchClauseStatements  =>
return isReusableStatement( node )
  case  ParsingContext.EnumMembers  =>
return isReusableEnumMember( node )
  case  ParsingContext.TypeMembers  =>
return isReusableTypeMember( node )
  case  ParsingContext.VariableDeclarations  =>
return isReusableVariableDeclaration( node )
  case  ParsingContext.Parameters  =>
return isReusableParameter( node )
  case _ =>
}
return false

}
def isReusableClassMember(node: Node) = {
 if (node) {
 node.kind match {
  case  SyntaxKind.Constructor | SyntaxKind.IndexSignature | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor | SyntaxKind.PropertyDeclaration | SyntaxKind.SemicolonClassElement  =>
return true
  case  SyntaxKind.MethodDeclaration  =>
var methodDeclaration = node.asInstanceOf[MethodDeclaration]
var nameIsConstructor = ((methodDeclaration.name.kind===SyntaxKind.Identifier)&&((methodDeclaration.name.asInstanceOf[Identifier]).originalKeywordKind===SyntaxKind.ConstructorKeyword))
return (!nameIsConstructor)
  case _ =>
}

}
return false

}
def isReusableSwitchClause(node: Node) = {
 if (node) {
 node.kind match {
  case  SyntaxKind.CaseClause | SyntaxKind.DefaultClause  =>
return true
  case _ =>
}

}
return false

}
def isReusableStatement(node: Node) = {
 if (node) {
 node.kind match {
  case  SyntaxKind.FunctionDeclaration | SyntaxKind.VariableStatement | SyntaxKind.Block | SyntaxKind.IfStatement | SyntaxKind.ExpressionStatement | SyntaxKind.ThrowStatement | SyntaxKind.ReturnStatement | SyntaxKind.SwitchStatement | SyntaxKind.BreakStatement | SyntaxKind.ContinueStatement | SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement | SyntaxKind.ForStatement | SyntaxKind.WhileStatement | SyntaxKind.WithStatement | SyntaxKind.EmptyStatement | SyntaxKind.TryStatement | SyntaxKind.LabeledStatement | SyntaxKind.DoStatement | SyntaxKind.DebuggerStatement | SyntaxKind.ImportDeclaration | SyntaxKind.ImportEqualsDeclaration | SyntaxKind.ExportDeclaration | SyntaxKind.ExportAssignment | SyntaxKind.ModuleDeclaration | SyntaxKind.ClassDeclaration | SyntaxKind.InterfaceDeclaration | SyntaxKind.EnumDeclaration | SyntaxKind.TypeAliasDeclaration  =>
return true
  case _ =>
}

}
return false

}
def isReusableEnumMember(node: Node) = {
 return (node.kind===SyntaxKind.EnumMember)

}
def isReusableTypeMember(node: Node) = {
 if (node) {
 node.kind match {
  case  SyntaxKind.ConstructSignature | SyntaxKind.MethodSignature | SyntaxKind.IndexSignature | SyntaxKind.PropertySignature | SyntaxKind.CallSignature  =>
return true
  case _ =>
}

}
return false

}
def isReusableVariableDeclaration(node: Node) = {
 if ((node.kind!==SyntaxKind.VariableDeclaration)) {
 return false

}
val variableDeclarator = node.asInstanceOf[VariableDeclaration]
return (variableDeclarator.initializer===undefined)

}
def isReusableParameter(node: Node) = {
 if ((node.kind!==SyntaxKind.Parameter)) {
 return false

}
val parameter = node.asInstanceOf[ParameterDeclaration]
return (parameter.initializer===undefined)

}
def abortParsingListOrMoveToNextToken(kind: ParsingContext) = {
 parseErrorAtCurrentToken( parsingContextErrors( kind ) )
if (isInSomeParsingContext()) {
 return true

}
nextToken()
return false

}
def parsingContextErrors(context: ParsingContext): DiagnosticMessage = {
 context match {
  case  ParsingContext.SourceElements  =>
return Diagnostics.Declaration_or_statement_expected
  case  ParsingContext.BlockStatements  =>
return Diagnostics.Declaration_or_statement_expected
  case  ParsingContext.SwitchClauses  =>
return Diagnostics.case_or_default_expected
  case  ParsingContext.SwitchClauseStatements  =>
return Diagnostics.Statement_expected
  case  ParsingContext.TypeMembers  =>
return Diagnostics.Property_or_signature_expected
  case  ParsingContext.ClassMembers  =>
return Diagnostics.Unexpected_token_A_constructor_method_accessor_or_property_was_expected
  case  ParsingContext.EnumMembers  =>
return Diagnostics.Enum_member_expected
  case  ParsingContext.HeritageClauseElement  =>
return Diagnostics.Expression_expected
  case  ParsingContext.VariableDeclarations  =>
return Diagnostics.Variable_declaration_expected
  case  ParsingContext.ObjectBindingElements  =>
return Diagnostics.Property_destructuring_pattern_expected
  case  ParsingContext.ArrayBindingElements  =>
return Diagnostics.Array_element_destructuring_pattern_expected
  case  ParsingContext.ArgumentExpressions  =>
return Diagnostics.Argument_expression_expected
  case  ParsingContext.ObjectLiteralMembers  =>
return Diagnostics.Property_assignment_expected
  case  ParsingContext.ArrayLiteralMembers  =>
return Diagnostics.Expression_or_comma_expected
  case  ParsingContext.Parameters  =>
return Diagnostics.Parameter_declaration_expected
  case  ParsingContext.TypeParameters  =>
return Diagnostics.Type_parameter_declaration_expected
  case  ParsingContext.TypeArguments  =>
return Diagnostics.Type_argument_expected
  case  ParsingContext.TupleElementTypes  =>
return Diagnostics.Type_expected
  case  ParsingContext.HeritageClauses  =>
return Diagnostics.Unexpected_token_expected
  case  ParsingContext.ImportOrExportSpecifiers  =>
return Diagnostics.Identifier_expected
  case  ParsingContext.JsxAttributes  =>
return Diagnostics.Identifier_expected
  case  ParsingContext.JsxChildren  =>
return Diagnostics.Identifier_expected
  case  ParsingContext.JSDocFunctionParameters  =>
return Diagnostics.Parameter_declaration_expected
  case  ParsingContext.JSDocTypeArguments  =>
return Diagnostics.Type_argument_expected
  case  ParsingContext.JSDocTupleTypes  =>
return Diagnostics.Type_expected
  case  ParsingContext.JSDocRecordMembers  =>
return Diagnostics.Property_assignment_expected
  case _ =>
}

}
;def parseDelimitedList[T <: Node](kind: ParsingContext, parseElement: (() => T), considerSemicolonAsDelimiter: Boolean): NodeArray[T] = {
 val saveParsingContext = parsingContext
(parsingContext|=(1<<kind))
val result = createNodeArray[ T ]()
var commaStart = (-1)
while (true) {
{
 if (isListElement( kind, false )) {
 result.push( parseListElement( kind, parseElement ) )
(commaStart=scanner.getTokenPos())
if (parseOptional( SyntaxKind.CommaToken )) {
 continue

}
(commaStart=(-1))
if (isListTerminator( kind )) {
 break()

}
parseExpected( SyntaxKind.CommaToken )
if (((considerSemicolonAsDelimiter&&(token()===SyntaxKind.SemicolonToken))&&(!scanner.hasPrecedingLineBreak()))) {
 nextToken()

}
continue

}
if (isListTerminator( kind )) {
 break()

}
if (abortParsingListOrMoveToNextToken( kind )) {
 break()

}

}
}
if ((commaStart>=0)) {
 (result.hasTrailingComma=true)

}
(result.end=getNodeEnd())
(parsingContext=saveParsingContext)
return result

}
def createMissingList[T <: Node](): NodeArray[T] = {
 return createNodeArray[ T ]()

}
def parseBracketedList[T <: Node](kind: ParsingContext, parseElement: (() => T), open: SyntaxKind, close: SyntaxKind): NodeArray[T] = {
 if (parseExpected( open )) {
 val result = parseDelimitedList( kind, parseElement )
parseExpected( close )
return result

}
return createMissingList[ T ]()

}
def parseEntityName(allowReservedWords: Boolean, diagnosticMessage: DiagnosticMessage): EntityName = {
 var entity: EntityName = parseIdentifier( diagnosticMessage )
while (parseOptional( SyntaxKind.DotToken )) {
{
 val node: QualifiedName = createNode( SyntaxKind.QualifiedName, entity.pos ).asInstanceOf[QualifiedName]
(node.left=entity)
(node.right=parseRightSideOfDot( allowReservedWords ))
(entity=finishNode( node ))

}
}
return entity

}
def parseRightSideOfDot(allowIdentifierNames: Boolean): Identifier = {
 if ((scanner.hasPrecedingLineBreak()&&tokenIsIdentifierOrKeyword( token() ))) {
 val matchesPattern = lookAhead( nextTokenIsIdentifierOrKeywordOnSameLine )
if (matchesPattern) {
 return createMissingNode( SyntaxKind.Identifier, true, Diagnostics.Identifier_expected ).asInstanceOf[Identifier]

}

}
return (if (allowIdentifierNames) parseIdentifierName() else parseIdentifier())

}
def parseTemplateExpression(): TemplateExpression = {
 val template = createNode( SyntaxKind.TemplateExpression ).asInstanceOf[TemplateExpression]
(template.head=parseTemplateHead())
Debug.assert( (template.head.kind===SyntaxKind.TemplateHead), "Template head has wrong token kind" )
val templateSpans = createNodeArray[ TemplateSpan ]()
do {
{
 templateSpans.push( parseTemplateSpan() )

}
} while ((lastOrUndefined( templateSpans ).literal.kind===SyntaxKind.TemplateMiddle))
(templateSpans.end=getNodeEnd())
(template.templateSpans=templateSpans)
return finishNode( template )

}
def parseTemplateSpan(): TemplateSpan = {
 val span = createNode( SyntaxKind.TemplateSpan ).asInstanceOf[TemplateSpan]
(span.expression=allowInAnd( parseExpression ))
var literal: ( TemplateMiddle | TemplateTail ) = zeroOfMyType
if ((token()===SyntaxKind.CloseBraceToken)) {
 reScanTemplateToken()
(literal=parseTemplateMiddleOrTemplateTail())

}
else {
 (literal=parseExpectedToken( SyntaxKind.TemplateTail, false, Diagnostics._0_expected, tokenToString( SyntaxKind.CloseBraceToken ) ).asInstanceOf[TemplateTail])

}
(span.literal=literal)
return finishNode( span )

}
def parseLiteralNode(internName: Boolean): LiteralExpression = {
 return parseLiteralLikeNode( token(), internName ).asInstanceOf[LiteralExpression]

}
def parseTemplateHead(): TemplateHead = {
 val fragment = parseLiteralLikeNode( token(), false )
Debug.assert( (fragment.kind===SyntaxKind.TemplateHead), "Template head has wrong token kind" )
return fragment.asInstanceOf[TemplateHead]

}
def parseTemplateMiddleOrTemplateTail(): ( TemplateMiddle | TemplateTail ) = {
 val fragment = parseLiteralLikeNode( token(), false )
Debug.assert( ((fragment.kind===SyntaxKind.TemplateMiddle)||(fragment.kind===SyntaxKind.TemplateTail)), "Template fragment has wrong token kind" )
return fragment.asInstanceOf[( TemplateMiddle | TemplateTail )]

}
def parseLiteralLikeNode(kind: SyntaxKind, internName: Boolean): LiteralLikeNode = {
 val node = createNode( kind ).asInstanceOf[LiteralExpression]
val text = scanner.getTokenValue()
(node.text=(if (internName) internIdentifier( text ) else text))
if (scanner.hasExtendedUnicodeEscape()) {
 (node.hasExtendedUnicodeEscape=true)

}
if (scanner.isUnterminated()) {
 (node.isUnterminated=true)

}
val tokenPos = scanner.getTokenPos()
nextToken()
finishNode( node )
if ((((node.kind===SyntaxKind.NumericLiteral)&&(sourceText.charCodeAt( tokenPos )===CharacterCodes._0))&&isOctalDigit( sourceText.charCodeAt( (tokenPos+1) ) ))) {
 (node.isOctalLiteral=true)

}
return node

}
def parseTypeReference(): TypeReferenceNode = {
 val typeName = parseEntityName( false, Diagnostics.Type_expected )
val node = createNode( SyntaxKind.TypeReference, typeName.pos ).asInstanceOf[TypeReferenceNode]
(node.typeName=typeName)
if (((!scanner.hasPrecedingLineBreak())&&(token()===SyntaxKind.LessThanToken))) {
 (node.typeArguments=parseBracketedList( ParsingContext.TypeArguments, parseType, SyntaxKind.LessThanToken, SyntaxKind.GreaterThanToken ))

}
return finishNode( node )

}
def parseThisTypePredicate(lhs: ThisTypeNode): TypePredicateNode = {
 nextToken()
val node = createNode( SyntaxKind.TypePredicate, lhs.pos ).asInstanceOf[TypePredicateNode]
(node.parameterName=lhs)
(node.`type`=parseType())
return finishNode( node )

}
def parseThisTypeNode(): ThisTypeNode = {
 val node = createNode( SyntaxKind.ThisType ).asInstanceOf[ThisTypeNode]
nextToken()
return finishNode( node )

}
def parseTypeQuery(): TypeQueryNode = {
 val node = createNode( SyntaxKind.TypeQuery ).asInstanceOf[TypeQueryNode]
parseExpected( SyntaxKind.TypeOfKeyword )
(node.exprName=parseEntityName( true ))
return finishNode( node )

}
def parseTypeParameter(): TypeParameterDeclaration = {
 val node = createNode( SyntaxKind.TypeParameter ).asInstanceOf[TypeParameterDeclaration]
(node.name=parseIdentifier())
if (parseOptional( SyntaxKind.ExtendsKeyword )) {
 if ((isStartOfType()||(!isStartOfExpression()))) {
 (node.constraint=parseType())

}
else {
 (node.expression=parseUnaryExpressionOrHigher())

}

}
return finishNode( node )

}
def parseTypeParameters(): NodeArray[TypeParameterDeclaration] = {
 if ((token()===SyntaxKind.LessThanToken)) {
 return parseBracketedList( ParsingContext.TypeParameters, parseTypeParameter, SyntaxKind.LessThanToken, SyntaxKind.GreaterThanToken )

}

}
def parseParameterType(): TypeNode = {
 if (parseOptional( SyntaxKind.ColonToken )) {
 return parseType()

}
return undefined

}
def isStartOfParameter(): Boolean = {
 return (((((token()===SyntaxKind.DotDotDotToken)||isIdentifierOrPattern())||isModifierKind( token() ))||(token()===SyntaxKind.AtToken))||(token()===SyntaxKind.ThisKeyword))

}
def parseParameter(): ParameterDeclaration = {
 val node = createNode( SyntaxKind.Parameter ).asInstanceOf[ParameterDeclaration]
if ((token()===SyntaxKind.ThisKeyword)) {
 (node.name=createIdentifier( true, undefined ))
(node.`type`=parseParameterType())
return finishNode( node )

}
(node.decorators=parseDecorators())
(node.modifiers=parseModifiers())
(node.dotDotDotToken=parseOptionalToken( SyntaxKind.DotDotDotToken ))
(node.name=parseIdentifierOrPattern())
if ((((getFullWidth( node.name )===0)&&(!hasModifiers( node )))&&isModifierKind( token() ))) {
 nextToken()

}
(node.questionToken=parseOptionalToken( SyntaxKind.QuestionToken ))
(node.`type`=parseParameterType())
(node.initializer=parseBindingElementInitializer( true ))
return addJSDocComment( finishNode( node ) )

}
def parseBindingElementInitializer(inParameter: Boolean) = {
 return (if (inParameter) parseParameterInitializer() else parseNonParameterInitializer())

}
def parseParameterInitializer() = {
 return parseInitializer( true )

}
def fillSignature(returnToken: SyntaxKind, yieldContext: Boolean, awaitContext: Boolean, requireCompleteParameterList: Boolean, signature: SignatureDeclaration): Unit = {
 val returnTokenRequired = (returnToken===SyntaxKind.EqualsGreaterThanToken)
(signature.typeParameters=parseTypeParameters())
(signature.parameters=parseParameterList( yieldContext, awaitContext, requireCompleteParameterList ))
if (returnTokenRequired) {
 parseExpected( returnToken )
(signature.`type`=parseTypeOrTypePredicate())

}
else if (parseOptional( returnToken )) {
 (signature.`type`=parseTypeOrTypePredicate())

}

}
def parseParameterList(yieldContext: Boolean, awaitContext: Boolean, requireCompleteParameterList: Boolean) = {
 if (parseExpected( SyntaxKind.OpenParenToken )) {
 val savedYieldContext = inYieldContext()
val savedAwaitContext = inAwaitContext()
setYieldContext( yieldContext )
setAwaitContext( awaitContext )
val result = parseDelimitedList( ParsingContext.Parameters, parseParameter )
setYieldContext( savedYieldContext )
setAwaitContext( savedAwaitContext )
if (((!parseExpected( SyntaxKind.CloseParenToken ))&&requireCompleteParameterList)) {
 return undefined

}
return result

}
return (if (requireCompleteParameterList) undefined else createMissingList[ ParameterDeclaration ]())

}
def parseTypeMemberSemicolon() = {
 if (parseOptional( SyntaxKind.CommaToken )) {
 return

}
parseSemicolon()

}
def parseSignatureMember(kind: SyntaxKind): ( CallSignatureDeclaration | ConstructSignatureDeclaration ) = {
 val node = createNode( kind ).asInstanceOf[( CallSignatureDeclaration | ConstructSignatureDeclaration )]
if ((kind===SyntaxKind.ConstructSignature)) {
 parseExpected( SyntaxKind.NewKeyword )

}
fillSignature( SyntaxKind.ColonToken, false, false, false, node )
parseTypeMemberSemicolon()
return addJSDocComment( finishNode( node ) )

}
def isIndexSignature(): Boolean = {
 if ((token()!==SyntaxKind.OpenBracketToken)) {
 return false

}
return lookAhead( isUnambiguouslyIndexSignature )

}
def isUnambiguouslyIndexSignature() = {
 nextToken()
if (((token()===SyntaxKind.DotDotDotToken)||(token()===SyntaxKind.CloseBracketToken))) {
 return true

}
if (isModifierKind( token() )) {
 nextToken()
if (isIdentifier()) {
 return true

}

}
else if ((!isIdentifier())) {
 return false

}
else {
 nextToken()

}
if (((token()===SyntaxKind.ColonToken)||(token()===SyntaxKind.CommaToken))) {
 return true

}
if ((token()!==SyntaxKind.QuestionToken)) {
 return false

}
nextToken()
return (((token()===SyntaxKind.ColonToken)||(token()===SyntaxKind.CommaToken))||(token()===SyntaxKind.CloseBracketToken))

}
def parseIndexSignatureDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): IndexSignatureDeclaration = {
 val node = createNode( SyntaxKind.IndexSignature, fullStart ).asInstanceOf[IndexSignatureDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
(node.parameters=parseBracketedList( ParsingContext.Parameters, parseParameter, SyntaxKind.OpenBracketToken, SyntaxKind.CloseBracketToken ))
(node.`type`=parseTypeAnnotation())
parseTypeMemberSemicolon()
return finishNode( node )

}
def parsePropertyOrMethodSignature(fullStart: Int, modifiers: NodeArray[Modifier]): ( PropertySignature | MethodSignature ) = {
 val name = parsePropertyName()
val questionToken = parseOptionalToken( SyntaxKind.QuestionToken )
if (((token()===SyntaxKind.OpenParenToken)||(token()===SyntaxKind.LessThanToken))) {
 val method = createNode( SyntaxKind.MethodSignature, fullStart ).asInstanceOf[MethodSignature]
(method.modifiers=modifiers)
(method.name=name)
(method.questionToken=questionToken)
fillSignature( SyntaxKind.ColonToken, false, false, false, method )
parseTypeMemberSemicolon()
return addJSDocComment( finishNode( method ) )

}
else {
 val property = createNode( SyntaxKind.PropertySignature, fullStart ).asInstanceOf[PropertySignature]
(property.modifiers=modifiers)
(property.name=name)
(property.questionToken=questionToken)
(property.`type`=parseTypeAnnotation())
if ((token()===SyntaxKind.EqualsToken)) {
 (property.initializer=parseNonParameterInitializer())

}
parseTypeMemberSemicolon()
return addJSDocComment( finishNode( property ) )

}

}
def isTypeMemberStart(): Boolean = {
 var idToken: SyntaxKind = zeroOfMyType
if (((token()===SyntaxKind.OpenParenToken)||(token()===SyntaxKind.LessThanToken))) {
 return true

}
while (isModifierKind( token() )) {
{
 (idToken=token())
nextToken()

}
}
if ((token()===SyntaxKind.OpenBracketToken)) {
 return true

}
if (isLiteralPropertyName()) {
 (idToken=token())
nextToken()

}
if (idToken) {
 return ((((((token()===SyntaxKind.OpenParenToken)||(token()===SyntaxKind.LessThanToken))||(token()===SyntaxKind.QuestionToken))||(token()===SyntaxKind.ColonToken))||(token()===SyntaxKind.CommaToken))||canParseSemicolon())

}
return false

}
def parseTypeMember(): TypeElement = {
 if (((token()===SyntaxKind.OpenParenToken)||(token()===SyntaxKind.LessThanToken))) {
 return parseSignatureMember( SyntaxKind.CallSignature )

}
if (((token()===SyntaxKind.NewKeyword)&&lookAhead( isStartOfConstructSignature ))) {
 return parseSignatureMember( SyntaxKind.ConstructSignature )

}
val fullStart = getNodePos()
val modifiers = parseModifiers()
if (isIndexSignature()) {
 return parseIndexSignatureDeclaration( fullStart, undefined, modifiers )

}
return parsePropertyOrMethodSignature( fullStart, modifiers )

}
def isStartOfConstructSignature() = {
 nextToken()
return ((token()===SyntaxKind.OpenParenToken)||(token()===SyntaxKind.LessThanToken))

}
def parseTypeLiteral(): TypeLiteralNode = {
 val node = createNode( SyntaxKind.TypeLiteral ).asInstanceOf[TypeLiteralNode]
(node.members=parseObjectTypeMembers())
return finishNode( node )

}
def parseObjectTypeMembers(): NodeArray[TypeElement] = {
 var members: NodeArray[TypeElement] = zeroOfMyType
if (parseExpected( SyntaxKind.OpenBraceToken )) {
 (members=parseList( ParsingContext.TypeMembers, parseTypeMember ))
parseExpected( SyntaxKind.CloseBraceToken )

}
else {
 (members=createMissingList[ TypeElement ]())

}
return members

}
def parseTupleType(): TupleTypeNode = {
 val node = createNode( SyntaxKind.TupleType ).asInstanceOf[TupleTypeNode]
(node.elementTypes=parseBracketedList( ParsingContext.TupleElementTypes, parseType, SyntaxKind.OpenBracketToken, SyntaxKind.CloseBracketToken ))
return finishNode( node )

}
def parseParenthesizedType(): ParenthesizedTypeNode = {
 val node = createNode( SyntaxKind.ParenthesizedType ).asInstanceOf[ParenthesizedTypeNode]
parseExpected( SyntaxKind.OpenParenToken )
(node.`type`=parseType())
parseExpected( SyntaxKind.CloseParenToken )
return finishNode( node )

}
def parseFunctionOrConstructorType(kind: SyntaxKind): FunctionOrConstructorTypeNode = {
 val node = createNode( kind ).asInstanceOf[FunctionOrConstructorTypeNode]
if ((kind===SyntaxKind.ConstructorType)) {
 parseExpected( SyntaxKind.NewKeyword )

}
fillSignature( SyntaxKind.EqualsGreaterThanToken, false, false, false, node )
return finishNode( node )

}
def parseKeywordAndNoDot(): TypeNode = {
 val node = parseTokenNode[ TypeNode ]()
return (if ((token()===SyntaxKind.DotToken)) undefined else node)

}
def parseLiteralTypeNode(): LiteralTypeNode = {
 val node = createNode( SyntaxKind.LiteralType ).asInstanceOf[LiteralTypeNode]
(node.literal=parseSimpleUnaryExpression())
finishNode( node )
return node

}
def nextTokenIsNumericLiteral() = {
 return (nextToken()===SyntaxKind.NumericLiteral)

}
def parseNonArrayType(): TypeNode = {
 token() match {
  case  SyntaxKind.AnyKeyword | SyntaxKind.StringKeyword | SyntaxKind.NumberKeyword | SyntaxKind.BooleanKeyword | SyntaxKind.SymbolKeyword | SyntaxKind.UndefinedKeyword | SyntaxKind.NeverKeyword  =>
val node = tryParse( parseKeywordAndNoDot )
return (node||parseTypeReference())
  case  SyntaxKind.StringLiteral | SyntaxKind.NumericLiteral | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword  =>
return parseLiteralTypeNode()
  case  SyntaxKind.MinusToken  =>
return (if (lookAhead( nextTokenIsNumericLiteral )) parseLiteralTypeNode() else parseTypeReference())
  case  SyntaxKind.VoidKeyword | SyntaxKind.NullKeyword  =>
return parseTokenNode[ TypeNode ]()
  case  SyntaxKind.ThisKeyword  =>
{
 val thisKeyword = parseThisTypeNode()
if (((token()===SyntaxKind.IsKeyword)&&(!scanner.hasPrecedingLineBreak()))) {
 return parseThisTypePredicate( thisKeyword )

}
else {
 return thisKeyword

}

}
  case  SyntaxKind.TypeOfKeyword  =>
return parseTypeQuery()
  case  SyntaxKind.OpenBraceToken  =>
return parseTypeLiteral()
  case  SyntaxKind.OpenBracketToken  =>
return parseTupleType()
  case  SyntaxKind.OpenParenToken  =>
return parseParenthesizedType()
  case _ =>
return parseTypeReference()
}

}
def isStartOfType(): Boolean = {
 token() match {
  case  SyntaxKind.AnyKeyword | SyntaxKind.StringKeyword | SyntaxKind.NumberKeyword | SyntaxKind.BooleanKeyword | SyntaxKind.SymbolKeyword | SyntaxKind.VoidKeyword | SyntaxKind.UndefinedKeyword | SyntaxKind.NullKeyword | SyntaxKind.ThisKeyword | SyntaxKind.TypeOfKeyword | SyntaxKind.NeverKeyword | SyntaxKind.OpenBraceToken | SyntaxKind.OpenBracketToken | SyntaxKind.LessThanToken | SyntaxKind.NewKeyword | SyntaxKind.StringLiteral | SyntaxKind.NumericLiteral | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword  =>
return true
  case  SyntaxKind.MinusToken  =>
return lookAhead( nextTokenIsNumericLiteral )
  case  SyntaxKind.OpenParenToken  =>
return lookAhead( isStartOfParenthesizedOrFunctionType )
  case _ =>
return isIdentifier()
}

}
def isStartOfParenthesizedOrFunctionType() = {
 nextToken()
return (((token()===SyntaxKind.CloseParenToken)||isStartOfParameter())||isStartOfType())

}
def parseArrayTypeOrHigher(): TypeNode = {
 var `type` = parseNonArrayType()
while (((!scanner.hasPrecedingLineBreak())&&parseOptional( SyntaxKind.OpenBracketToken ))) {
{
 parseExpected( SyntaxKind.CloseBracketToken )
val node = createNode( SyntaxKind.ArrayType, `type`.pos ).asInstanceOf[ArrayTypeNode]
(node.elementType=`type`)
(`type`=finishNode( node ))

}
}
return `type`

}
def parseUnionOrIntersectionType(kind: SyntaxKind, parseConstituentType: (() => TypeNode), operator: SyntaxKind): TypeNode = {
 var `type` = parseConstituentType()
if ((token()===operator)) {
 val types = createNodeArray[ TypeNode ]( Array( `type` ), `type`.pos )
while (parseOptional( operator )) {
{
 types.push( parseConstituentType() )

}
}
(types.end=getNodeEnd())
val node = createNode( kind, `type`.pos ).asInstanceOf[UnionOrIntersectionTypeNode]
(node.types=types)
(`type`=finishNode( node ))

}
return `type`

}
def parseIntersectionTypeOrHigher(): TypeNode = {
 return parseUnionOrIntersectionType( SyntaxKind.IntersectionType, parseArrayTypeOrHigher, SyntaxKind.AmpersandToken )

}
def parseUnionTypeOrHigher(): TypeNode = {
 return parseUnionOrIntersectionType( SyntaxKind.UnionType, parseIntersectionTypeOrHigher, SyntaxKind.BarToken )

}
def isStartOfFunctionType(): Boolean = {
 if ((token()===SyntaxKind.LessThanToken)) {
 return true

}
return ((token()===SyntaxKind.OpenParenToken)&&lookAhead( isUnambiguouslyStartOfFunctionType ))

}
def skipParameterStart(): Boolean = {
 if (isModifierKind( token() )) {
 parseModifiers()

}
if ((isIdentifier()||(token()===SyntaxKind.ThisKeyword))) {
 nextToken()
return true

}
if (((token()===SyntaxKind.OpenBracketToken)||(token()===SyntaxKind.OpenBraceToken))) {
 val previousErrorCount = parseDiagnostics.length
parseIdentifierOrPattern()
return (previousErrorCount===parseDiagnostics.length)

}
return false

}
def isUnambiguouslyStartOfFunctionType() = {
 nextToken()
if (((token()===SyntaxKind.CloseParenToken)||(token()===SyntaxKind.DotDotDotToken))) {
 return true

}
if (skipParameterStart()) {
 if (((((token()===SyntaxKind.ColonToken)||(token()===SyntaxKind.CommaToken))||(token()===SyntaxKind.QuestionToken))||(token()===SyntaxKind.EqualsToken))) {
 return true

}
if ((token()===SyntaxKind.CloseParenToken)) {
 nextToken()
if ((token()===SyntaxKind.EqualsGreaterThanToken)) {
 return true

}

}

}
return false

}
def parseTypeOrTypePredicate(): TypeNode = {
 val typePredicateVariable = (isIdentifier()&&tryParse( parseTypePredicatePrefix ))
val `type` = parseType()
if (typePredicateVariable) {
 val node = createNode( SyntaxKind.TypePredicate, typePredicateVariable.pos ).asInstanceOf[TypePredicateNode]
(node.parameterName=typePredicateVariable)
(node.`type`=`type`)
return finishNode( node )

}
else {
 return `type`

}

}
def parseTypePredicatePrefix() = {
 val id = parseIdentifier()
if (((token()===SyntaxKind.IsKeyword)&&(!scanner.hasPrecedingLineBreak()))) {
 nextToken()
return id

}

}
def parseType(): TypeNode = {
 return doOutsideOfContext( NodeFlags.TypeExcludesFlags, parseTypeWorker )

}
def parseTypeWorker(): TypeNode = {
 if (isStartOfFunctionType()) {
 return parseFunctionOrConstructorType( SyntaxKind.FunctionType )

}
if ((token()===SyntaxKind.NewKeyword)) {
 return parseFunctionOrConstructorType( SyntaxKind.ConstructorType )

}
return parseUnionTypeOrHigher()

}
def parseTypeAnnotation(): TypeNode = {
 return (if (parseOptional( SyntaxKind.ColonToken )) parseType() else undefined)

}
def isStartOfLeftHandSideExpression(): Boolean = {
 token() match {
  case  SyntaxKind.ThisKeyword | SyntaxKind.SuperKeyword | SyntaxKind.NullKeyword | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword | SyntaxKind.NumericLiteral | SyntaxKind.StringLiteral | SyntaxKind.NoSubstitutionTemplateLiteral | SyntaxKind.TemplateHead | SyntaxKind.OpenParenToken | SyntaxKind.OpenBracketToken | SyntaxKind.OpenBraceToken | SyntaxKind.FunctionKeyword | SyntaxKind.ClassKeyword | SyntaxKind.NewKeyword | SyntaxKind.SlashToken | SyntaxKind.SlashEqualsToken | SyntaxKind.Identifier  =>
return true
  case _ =>
return isIdentifier()
}

}
def isStartOfExpression(): Boolean = {
 if (isStartOfLeftHandSideExpression()) {
 return true

}
token() match {
  case  SyntaxKind.PlusToken | SyntaxKind.MinusToken | SyntaxKind.TildeToken | SyntaxKind.ExclamationToken | SyntaxKind.DeleteKeyword | SyntaxKind.TypeOfKeyword | SyntaxKind.VoidKeyword | SyntaxKind.PlusPlusToken | SyntaxKind.MinusMinusToken | SyntaxKind.LessThanToken | SyntaxKind.AwaitKeyword | SyntaxKind.YieldKeyword  =>
return true
  case _ =>
if (isBinaryOperator()) {
 return true

}
return isIdentifier()
}

}
def isStartOfExpressionStatement(): Boolean = {
 return (((((token()!==SyntaxKind.OpenBraceToken)&&(token()!==SyntaxKind.FunctionKeyword))&&(token()!==SyntaxKind.ClassKeyword))&&(token()!==SyntaxKind.AtToken))&&isStartOfExpression())

}
def parseExpression(): Expression = {
 val saveDecoratorContext = inDecoratorContext()
if (saveDecoratorContext) {
 setDecoratorContext( false )

}
var expr = parseAssignmentExpressionOrHigher()
var operatorToken: BinaryOperatorToken = zeroOfMyType
while (((operatorToken=parseOptionalToken( SyntaxKind.CommaToken )))) {
{
 (expr=makeBinaryExpression( expr, operatorToken, parseAssignmentExpressionOrHigher() ))

}
}
if (saveDecoratorContext) {
 setDecoratorContext( true )

}
return expr

}
def parseInitializer(inParameter: Boolean): Expression = {
 if ((token()!==SyntaxKind.EqualsToken)) {
 if (((scanner.hasPrecedingLineBreak()||((inParameter&&(token()===SyntaxKind.OpenBraceToken))))||(!isStartOfExpression()))) {
 return undefined

}

}
parseExpected( SyntaxKind.EqualsToken )
return parseAssignmentExpressionOrHigher()

}
def parseAssignmentExpressionOrHigher(): Expression = {
 if (isYieldExpression()) {
 return parseYieldExpression()

}
val arrowExpression = (tryParseParenthesizedArrowFunctionExpression()||tryParseAsyncSimpleArrowFunctionExpression())
if (arrowExpression) {
 return arrowExpression

}
val expr = parseBinaryExpressionOrHigher( 0 )
if (((expr.kind===SyntaxKind.Identifier)&&(token()===SyntaxKind.EqualsGreaterThanToken))) {
 return parseSimpleArrowFunctionExpression( expr.asInstanceOf[Identifier] )

}
if ((isLeftHandSideExpression( expr )&&isAssignmentOperator( reScanGreaterToken() ))) {
 return makeBinaryExpression( expr, parseTokenNode().asInstanceOf[BinaryOperatorToken], parseAssignmentExpressionOrHigher() )

}
return parseConditionalExpressionRest( expr )

}
def isYieldExpression(): Boolean = {
 if ((token()===SyntaxKind.YieldKeyword)) {
 if (inYieldContext()) {
 return true

}
return lookAhead( nextTokenIsIdentifierOrKeywordOrNumberOnSameLine )

}
return false

}
def nextTokenIsIdentifierOnSameLine() = {
 nextToken()
return ((!scanner.hasPrecedingLineBreak())&&isIdentifier())

}
def parseYieldExpression(): YieldExpression = {
 val node = createNode( SyntaxKind.YieldExpression ).asInstanceOf[YieldExpression]
nextToken()
if (((!scanner.hasPrecedingLineBreak())&&(((token()===SyntaxKind.AsteriskToken)||isStartOfExpression())))) {
 (node.asteriskToken=parseOptionalToken( SyntaxKind.AsteriskToken ))
(node.expression=parseAssignmentExpressionOrHigher())
return finishNode( node )

}
else {
 return finishNode( node )

}

}
def parseSimpleArrowFunctionExpression(identifier: Identifier, asyncModifier: NodeArray[Modifier]): ArrowFunction = {
 Debug.assert( (token()===SyntaxKind.EqualsGreaterThanToken), "parseSimpleArrowFunctionExpression should only have been called if we had a =>" )
var node: ArrowFunction = zeroOfMyType
if (asyncModifier) {
 (node=createNode( SyntaxKind.ArrowFunction, asyncModifier.pos ).asInstanceOf[ArrowFunction])
(node.modifiers=asyncModifier)

}
else {
 (node=createNode( SyntaxKind.ArrowFunction, identifier.pos ).asInstanceOf[ArrowFunction])

}
val parameter = createNode( SyntaxKind.Parameter, identifier.pos ).asInstanceOf[ParameterDeclaration]
(parameter.name=identifier)
finishNode( parameter )
(node.parameters=createNodeArray[ ParameterDeclaration ]( Array( parameter ), parameter.pos ))
(node.parameters.end=parameter.end)
(node.equalsGreaterThanToken=parseExpectedToken( SyntaxKind.EqualsGreaterThanToken, false, Diagnostics._0_expected, "=>" ))
(node.body=parseArrowFunctionExpressionBody( (!(!asyncModifier)) ))
return addJSDocComment( finishNode( node ) )

}
def tryParseParenthesizedArrowFunctionExpression(): Expression = {
 val triState = isParenthesizedArrowFunctionExpression()
if ((triState===Tristate.False)) {
 return undefined

}
val arrowFunction = (if ((triState===Tristate.True)) parseParenthesizedArrowFunctionExpressionHead( true ) else tryParse( parsePossibleParenthesizedArrowFunctionExpressionHead ))
if ((!arrowFunction)) {
 return undefined

}
val isAsync = (!(!((getModifierFlags( arrowFunction )&ModifierFlags.Async))))
val lastToken = token()
(arrowFunction.equalsGreaterThanToken=parseExpectedToken( SyntaxKind.EqualsGreaterThanToken, false, Diagnostics._0_expected, "=>" ))
(arrowFunction.body=(if ((((lastToken===SyntaxKind.EqualsGreaterThanToken)||(lastToken===SyntaxKind.OpenBraceToken)))) parseArrowFunctionExpressionBody( isAsync ) else parseIdentifier()))
return addJSDocComment( finishNode( arrowFunction ) )

}
def isParenthesizedArrowFunctionExpression(): Tristate = {
 if ((((token()===SyntaxKind.OpenParenToken)||(token()===SyntaxKind.LessThanToken))||(token()===SyntaxKind.AsyncKeyword))) {
 return lookAhead( isParenthesizedArrowFunctionExpressionWorker )

}
if ((token()===SyntaxKind.EqualsGreaterThanToken)) {
 return Tristate.True

}
return Tristate.False

}
def isParenthesizedArrowFunctionExpressionWorker() = {
 if ((token()===SyntaxKind.AsyncKeyword)) {
 nextToken()
if (scanner.hasPrecedingLineBreak()) {
 return Tristate.False

}
if (((token()!==SyntaxKind.OpenParenToken)&&(token()!==SyntaxKind.LessThanToken))) {
 return Tristate.False

}

}
val first = token()
val second = nextToken()
if ((first===SyntaxKind.OpenParenToken)) {
 if ((second===SyntaxKind.CloseParenToken)) {
 val third = nextToken()
third match {
  case  SyntaxKind.EqualsGreaterThanToken | SyntaxKind.ColonToken | SyntaxKind.OpenBraceToken  =>
return Tristate.True
  case _ =>
return Tristate.False
}

}
if (((second===SyntaxKind.OpenBracketToken)||(second===SyntaxKind.OpenBraceToken))) {
 return Tristate.Unknown

}
if ((second===SyntaxKind.DotDotDotToken)) {
 return Tristate.True

}
if ((!isIdentifier())) {
 return Tristate.False

}
if ((nextToken()===SyntaxKind.ColonToken)) {
 return Tristate.True

}
return Tristate.Unknown

}
else {
 Debug.assert( (first===SyntaxKind.LessThanToken) )
if ((!isIdentifier())) {
 return Tristate.False

}
if ((sourceFile.languageVariant===LanguageVariant.JSX)) {
 val isArrowFunctionInJsx = lookAhead( (() =>  {
 val third = nextToken()
 if ((third===SyntaxKind.ExtendsKeyword)) {
 val fourth = nextToken()
fourth match {
  case  SyntaxKind.EqualsToken | SyntaxKind.GreaterThanToken  =>
return false
  case _ =>
return true
}

}
else if ((third===SyntaxKind.CommaToken)) {
 return true

}
 return false

}) )
if (isArrowFunctionInJsx) {
 return Tristate.True

}
return Tristate.False

}
return Tristate.Unknown

}

}
def parsePossibleParenthesizedArrowFunctionExpressionHead(): ArrowFunction = {
 return parseParenthesizedArrowFunctionExpressionHead( false )

}
def tryParseAsyncSimpleArrowFunctionExpression(): ArrowFunction = {
 if ((token()===SyntaxKind.AsyncKeyword)) {
 val isUnParenthesizedAsyncArrowFunction = lookAhead( isUnParenthesizedAsyncArrowFunctionWorker )
if ((isUnParenthesizedAsyncArrowFunction===Tristate.True)) {
 val asyncModifier = parseModifiersForArrowFunction()
val expr = parseBinaryExpressionOrHigher( 0 )
return parseSimpleArrowFunctionExpression( expr.asInstanceOf[Identifier], asyncModifier )

}

}
return undefined

}
def isUnParenthesizedAsyncArrowFunctionWorker(): Tristate = {
 if ((token()===SyntaxKind.AsyncKeyword)) {
 nextToken()
if ((scanner.hasPrecedingLineBreak()||(token()===SyntaxKind.EqualsGreaterThanToken))) {
 return Tristate.False

}
val expr = parseBinaryExpressionOrHigher( 0 )
if ((((!scanner.hasPrecedingLineBreak())&&(expr.kind===SyntaxKind.Identifier))&&(token()===SyntaxKind.EqualsGreaterThanToken))) {
 return Tristate.True

}

}
return Tristate.False

}
def parseParenthesizedArrowFunctionExpressionHead(allowAmbiguity: Boolean): ArrowFunction = {
 val node = createNode( SyntaxKind.ArrowFunction ).asInstanceOf[ArrowFunction]
(node.modifiers=parseModifiersForArrowFunction())
val isAsync = (!(!((getModifierFlags( node )&ModifierFlags.Async))))
fillSignature( SyntaxKind.ColonToken, false, isAsync, (!allowAmbiguity), node )
if ((!node.parameters)) {
 return undefined

}
if ((((!allowAmbiguity)&&(token()!==SyntaxKind.EqualsGreaterThanToken))&&(token()!==SyntaxKind.OpenBraceToken))) {
 return undefined

}
return node

}
def parseArrowFunctionExpressionBody(isAsync: Boolean): ( Block | Expression ) = {
 if ((token()===SyntaxKind.OpenBraceToken)) {
 return parseFunctionBlock( false, isAsync, false )

}
if ((((((token()!==SyntaxKind.SemicolonToken)&&(token()!==SyntaxKind.FunctionKeyword))&&(token()!==SyntaxKind.ClassKeyword))&&isStartOfStatement())&&(!isStartOfExpressionStatement()))) {
 return parseFunctionBlock( false, isAsync, true )

}
return (if (isAsync) doInAwaitContext( parseAssignmentExpressionOrHigher ) else doOutsideOfAwaitContext( parseAssignmentExpressionOrHigher ))

}
def parseConditionalExpressionRest(leftOperand: Expression): Expression = {
 val questionToken = parseOptionalToken( SyntaxKind.QuestionToken )
if ((!questionToken)) {
 return leftOperand

}
val node = createNode( SyntaxKind.ConditionalExpression, leftOperand.pos ).asInstanceOf[ConditionalExpression]
(node.condition=leftOperand)
(node.questionToken=questionToken)
(node.whenTrue=doOutsideOfContext( disallowInAndDecoratorContext, parseAssignmentExpressionOrHigher ))
(node.colonToken=parseExpectedToken( SyntaxKind.ColonToken, false, Diagnostics._0_expected, tokenToString( SyntaxKind.ColonToken ) ))
(node.whenFalse=parseAssignmentExpressionOrHigher())
return finishNode( node )

}
def parseBinaryExpressionOrHigher(precedence: Int): Expression = {
 val leftOperand = parseUnaryExpressionOrHigher()
return parseBinaryExpressionRest( precedence, leftOperand )

}
def isInOrOfKeyword(t: SyntaxKind) = {
 return ((t===SyntaxKind.InKeyword)||(t===SyntaxKind.OfKeyword))

}
def parseBinaryExpressionRest(precedence: Int, leftOperand: Expression): Expression = {
 while (true) {
{
 reScanGreaterToken()
val newPrecedence = getBinaryOperatorPrecedence()
val consumeCurrentOperator = (if ((token()===SyntaxKind.AsteriskAsteriskToken)) (newPrecedence>=precedence) else (newPrecedence>precedence))
if ((!consumeCurrentOperator)) {
 break()

}
if (((token()===SyntaxKind.InKeyword)&&inDisallowInContext())) {
 break()

}
if ((token()===SyntaxKind.AsKeyword)) {
 if (scanner.hasPrecedingLineBreak()) {
 break()

}
else {
 nextToken()
(leftOperand=makeAsExpression( leftOperand, parseType() ))

}

}
else {
 (leftOperand=makeBinaryExpression( leftOperand, parseTokenNode().asInstanceOf[BinaryOperatorToken], parseBinaryExpressionOrHigher( newPrecedence ) ))

}

}
}
return leftOperand

}
def isBinaryOperator() = {
 if ((inDisallowInContext()&&(token()===SyntaxKind.InKeyword))) {
 return false

}
return (getBinaryOperatorPrecedence()>0)

}
def getBinaryOperatorPrecedence(): Int = {
 token() match {
  case  SyntaxKind.BarBarToken  =>
return 1
  case  SyntaxKind.AmpersandAmpersandToken  =>
return 2
  case  SyntaxKind.BarToken  =>
return 3
  case  SyntaxKind.CaretToken  =>
return 4
  case  SyntaxKind.AmpersandToken  =>
return 5
  case  SyntaxKind.EqualsEqualsToken | SyntaxKind.ExclamationEqualsToken | SyntaxKind.EqualsEqualsEqualsToken | SyntaxKind.ExclamationEqualsEqualsToken  =>
return 6
  case  SyntaxKind.LessThanToken | SyntaxKind.GreaterThanToken | SyntaxKind.LessThanEqualsToken | SyntaxKind.GreaterThanEqualsToken | SyntaxKind.InstanceOfKeyword | SyntaxKind.InKeyword | SyntaxKind.AsKeyword  =>
return 7
  case  SyntaxKind.LessThanLessThanToken | SyntaxKind.GreaterThanGreaterThanToken | SyntaxKind.GreaterThanGreaterThanGreaterThanToken  =>
return 8
  case  SyntaxKind.PlusToken | SyntaxKind.MinusToken  =>
return 9
  case  SyntaxKind.AsteriskToken | SyntaxKind.SlashToken | SyntaxKind.PercentToken  =>
return 10
  case  SyntaxKind.AsteriskAsteriskToken  =>
return 11
  case _ =>
}
return (-1)

}
def makeBinaryExpression(left: Expression, operatorToken: BinaryOperatorToken, right: Expression): BinaryExpression = {
 val node = createNode( SyntaxKind.BinaryExpression, left.pos ).asInstanceOf[BinaryExpression]
(node.left=left)
(node.operatorToken=operatorToken)
(node.right=right)
return finishNode( node )

}
def makeAsExpression(left: Expression, right: TypeNode): AsExpression = {
 val node = createNode( SyntaxKind.AsExpression, left.pos ).asInstanceOf[AsExpression]
(node.expression=left)
(node.`type`=right)
return finishNode( node )

}
def parsePrefixUnaryExpression() = {
 val node = createNode( SyntaxKind.PrefixUnaryExpression ).asInstanceOf[PrefixUnaryExpression]
(node.operator=token().asInstanceOf[PrefixUnaryOperator])
nextToken()
(node.operand=parseSimpleUnaryExpression())
return finishNode( node )

}
def parseDeleteExpression() = {
 val node = createNode( SyntaxKind.DeleteExpression ).asInstanceOf[DeleteExpression]
nextToken()
(node.expression=parseSimpleUnaryExpression())
return finishNode( node )

}
def parseTypeOfExpression() = {
 val node = createNode( SyntaxKind.TypeOfExpression ).asInstanceOf[TypeOfExpression]
nextToken()
(node.expression=parseSimpleUnaryExpression())
return finishNode( node )

}
def parseVoidExpression() = {
 val node = createNode( SyntaxKind.VoidExpression ).asInstanceOf[VoidExpression]
nextToken()
(node.expression=parseSimpleUnaryExpression())
return finishNode( node )

}
def isAwaitExpression(): Boolean = {
 if ((token()===SyntaxKind.AwaitKeyword)) {
 if (inAwaitContext()) {
 return true

}
return lookAhead( nextTokenIsIdentifierOnSameLine )

}
return false

}
def parseAwaitExpression() = {
 val node = createNode( SyntaxKind.AwaitExpression ).asInstanceOf[AwaitExpression]
nextToken()
(node.expression=parseSimpleUnaryExpression())
return finishNode( node )

}
def parseUnaryExpressionOrHigher(): ( UnaryExpression | BinaryExpression ) = {
 if (isUpdateExpression()) {
 val incrementExpression = parseIncrementExpression()
return (if ((token()===SyntaxKind.AsteriskAsteriskToken)) parseBinaryExpressionRest( getBinaryOperatorPrecedence(), incrementExpression ).asInstanceOf[BinaryExpression] else incrementExpression)

}
val unaryOperator = token()
val simpleUnaryExpression = parseSimpleUnaryExpression()
if ((token()===SyntaxKind.AsteriskAsteriskToken)) {
 val start = skipTrivia( sourceText, simpleUnaryExpression.pos )
if ((simpleUnaryExpression.kind===SyntaxKind.TypeAssertionExpression)) {
 parseErrorAtPosition( start, (simpleUnaryExpression.end-start), Diagnostics.A_type_assertion_expression_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses )

}
else {
 parseErrorAtPosition( start, (simpleUnaryExpression.end-start), Diagnostics.An_unary_expression_with_the_0_operator_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses, tokenToString( unaryOperator ) )

}

}
return simpleUnaryExpression

}
def parseSimpleUnaryExpression(): UnaryExpression = {
 token() match {
  case  SyntaxKind.PlusToken | SyntaxKind.MinusToken | SyntaxKind.TildeToken | SyntaxKind.ExclamationToken  =>
return parsePrefixUnaryExpression()
  case  SyntaxKind.DeleteKeyword  =>
return parseDeleteExpression()
  case  SyntaxKind.TypeOfKeyword  =>
return parseTypeOfExpression()
  case  SyntaxKind.VoidKeyword  =>
return parseVoidExpression()
  case  SyntaxKind.LessThanToken  =>
return parseTypeAssertion()
  case  SyntaxKind.AwaitKeyword  =>
if (isAwaitExpression()) {
 return parseAwaitExpression()

}
return parseIncrementExpression()
  case _ =>
return parseIncrementExpression()
}

}
def isUpdateExpression(): Boolean = {
 token() match {
  case  SyntaxKind.PlusToken | SyntaxKind.MinusToken | SyntaxKind.TildeToken | SyntaxKind.ExclamationToken | SyntaxKind.DeleteKeyword | SyntaxKind.TypeOfKeyword | SyntaxKind.VoidKeyword | SyntaxKind.AwaitKeyword  =>
return false
  case  SyntaxKind.LessThanToken  =>
if ((sourceFile.languageVariant!==LanguageVariant.JSX)) {
 return false

}
return true
  case _ =>
return true
}

}
def parseIncrementExpression(): IncrementExpression = {
 if (((token()===SyntaxKind.PlusPlusToken)||(token()===SyntaxKind.MinusMinusToken))) {
 val node = createNode( SyntaxKind.PrefixUnaryExpression ).asInstanceOf[PrefixUnaryExpression]
(node.operator=token().asInstanceOf[PrefixUnaryOperator])
nextToken()
(node.operand=parseLeftHandSideExpressionOrHigher())
return finishNode( node )

}
else if ((((sourceFile.languageVariant===LanguageVariant.JSX)&&(token()===SyntaxKind.LessThanToken))&&lookAhead( nextTokenIsIdentifierOrKeyword ))) {
 return parseJsxElementOrSelfClosingElement( true )

}
val expression = parseLeftHandSideExpressionOrHigher()
Debug.assert( isLeftHandSideExpression( expression ) )
if (((((token()===SyntaxKind.PlusPlusToken)||(token()===SyntaxKind.MinusMinusToken)))&&(!scanner.hasPrecedingLineBreak()))) {
 val node = createNode( SyntaxKind.PostfixUnaryExpression, expression.pos ).asInstanceOf[PostfixUnaryExpression]
(node.operand=expression)
(node.operator=token().asInstanceOf[PostfixUnaryOperator])
nextToken()
return finishNode( node )

}
return expression

}
def parseLeftHandSideExpressionOrHigher(): LeftHandSideExpression = {
 val expression = (if ((token()===SyntaxKind.SuperKeyword)) parseSuperExpression() else parseMemberExpressionOrHigher())
return parseCallExpressionRest( expression )

}
def parseMemberExpressionOrHigher(): MemberExpression = {
 val expression = parsePrimaryExpression()
return parseMemberExpressionRest( expression )

}
def parseSuperExpression(): MemberExpression = {
 val expression = parseTokenNode[ PrimaryExpression ]()
if ((((token()===SyntaxKind.OpenParenToken)||(token()===SyntaxKind.DotToken))||(token()===SyntaxKind.OpenBracketToken))) {
 return expression

}
val node = createNode( SyntaxKind.PropertyAccessExpression, expression.pos ).asInstanceOf[PropertyAccessExpression]
(node.expression=expression)
parseExpectedToken( SyntaxKind.DotToken, false, Diagnostics.super_must_be_followed_by_an_argument_list_or_member_access )
(node.name=parseRightSideOfDot( true ))
return finishNode( node )

}
def tagNamesAreEquivalent(lhs: JsxTagNameExpression, rhs: JsxTagNameExpression): Boolean = {
 if ((lhs.kind!==rhs.kind)) {
 return false

}
if ((lhs.kind===SyntaxKind.Identifier)) {
 return ((lhs.asInstanceOf[Identifier]).text===(rhs.asInstanceOf[Identifier]).text)

}
if ((lhs.kind===SyntaxKind.ThisKeyword)) {
 return true

}
return (((lhs.asInstanceOf[PropertyAccessExpression]).name.text===(rhs.asInstanceOf[PropertyAccessExpression]).name.text)&&tagNamesAreEquivalent( (lhs.asInstanceOf[PropertyAccessExpression]).expression.asInstanceOf[JsxTagNameExpression], (rhs.asInstanceOf[PropertyAccessExpression]).expression.asInstanceOf[JsxTagNameExpression] ))

}
def parseJsxElementOrSelfClosingElement(inExpressionContext: Boolean): ( JsxElement | JsxSelfClosingElement ) = {
 val opening = parseJsxOpeningOrSelfClosingElement( inExpressionContext )
var result: ( JsxElement | JsxSelfClosingElement ) = zeroOfMyType
if ((opening.kind===SyntaxKind.JsxOpeningElement)) {
 val node = createNode( SyntaxKind.JsxElement, opening.pos ).asInstanceOf[JsxElement]
(node.openingElement=opening)
(node.children=parseJsxChildren( node.openingElement.tagName ))
(node.closingElement=parseJsxClosingElement( inExpressionContext ))
if ((!tagNamesAreEquivalent( node.openingElement.tagName, node.closingElement.tagName ))) {
 parseErrorAtPosition( node.closingElement.pos, (node.closingElement.end-node.closingElement.pos), Diagnostics.Expected_corresponding_JSX_closing_tag_for_0, getTextOfNodeFromSourceText( sourceText, node.openingElement.tagName ) )

}
(result=finishNode( node ))

}
else {
 Debug.assert( (opening.kind===SyntaxKind.JsxSelfClosingElement) )
(result=opening.asInstanceOf[JsxSelfClosingElement])

}
if ((inExpressionContext&&(token()===SyntaxKind.LessThanToken))) {
 val invalidElement = tryParse( (() =>  parseJsxElementOrSelfClosingElement( true )) )
if (invalidElement) {
 parseErrorAtCurrentToken( Diagnostics.JSX_expressions_must_have_one_parent_element )
val badNode = createNode( SyntaxKind.BinaryExpression, result.pos ).asInstanceOf[BinaryExpression]
(badNode.end=invalidElement.end)
(badNode.left=result)
(badNode.right=invalidElement)
(badNode.operatorToken=createMissingNode( SyntaxKind.CommaToken, false, undefined ).asInstanceOf[BinaryOperatorToken])
(badNode.operatorToken.pos=(badNode.operatorToken.end=badNode.right.pos))
return badNode.asInstanceOf[Node].asInstanceOf[JsxElement]

}

}
return result

}
def parseJsxText(): JsxText = {
 val node = createNode( SyntaxKind.JsxText, scanner.getStartPos() ).asInstanceOf[JsxText]
(currentToken=scanner.scanJsxToken())
return finishNode( node )

}
def parseJsxChild(): JsxChild = {
 token() match {
  case  SyntaxKind.JsxText  =>
return parseJsxText()
  case  SyntaxKind.OpenBraceToken  =>
return parseJsxExpression( false )
  case  SyntaxKind.LessThanToken  =>
return parseJsxElementOrSelfClosingElement( false )
  case _ =>
}
Debug.fail( ("Unknown JSX child kind "+token()) )

}
def parseJsxChildren(openingTagName: LeftHandSideExpression): NodeArray[JsxChild] = {
 val result = createNodeArray[ JsxChild ]()
val saveParsingContext = parsingContext
(parsingContext|=(1<<ParsingContext.JsxChildren))
while (true) {
{
 (currentToken=scanner.reScanJsxToken())
if ((token()===SyntaxKind.LessThanSlashToken)) {
 break()

}
else if ((token()===SyntaxKind.EndOfFileToken)) {
 parseErrorAtPosition( openingTagName.pos, (openingTagName.end-openingTagName.pos), Diagnostics.JSX_element_0_has_no_corresponding_closing_tag, getTextOfNodeFromSourceText( sourceText, openingTagName ) )
break()

}
result.push( parseJsxChild() )

}
}
(result.end=scanner.getTokenPos())
(parsingContext=saveParsingContext)
return result

}
def parseJsxOpeningOrSelfClosingElement(inExpressionContext: Boolean): ( JsxOpeningElement | JsxSelfClosingElement ) = {
 val fullStart = scanner.getStartPos()
parseExpected( SyntaxKind.LessThanToken )
val tagName = parseJsxElementName()
val attributes = parseList( ParsingContext.JsxAttributes, parseJsxAttribute )
var node: JsxOpeningLikeElement = zeroOfMyType
if ((token()===SyntaxKind.GreaterThanToken)) {
 (node=createNode( SyntaxKind.JsxOpeningElement, fullStart ).asInstanceOf[JsxOpeningElement])
scanJsxText()

}
else {
 parseExpected( SyntaxKind.SlashToken )
if (inExpressionContext) {
 parseExpected( SyntaxKind.GreaterThanToken )

}
else {
 parseExpected( SyntaxKind.GreaterThanToken, undefined, false )
scanJsxText()

}
(node=createNode( SyntaxKind.JsxSelfClosingElement, fullStart ).asInstanceOf[JsxSelfClosingElement])

}
(node.tagName=tagName)
(node.attributes=attributes)
return finishNode( node )

}
def parseJsxElementName(): JsxTagNameExpression = {
 scanJsxIdentifier()
var expression: JsxTagNameExpression = (if ((token()===SyntaxKind.ThisKeyword)) parseTokenNode[ PrimaryExpression ]() else parseIdentifierName())
while (parseOptional( SyntaxKind.DotToken )) {
{
 val propertyAccess: PropertyAccessExpression = createNode( SyntaxKind.PropertyAccessExpression, expression.pos ).asInstanceOf[PropertyAccessExpression]
(propertyAccess.expression=expression)
(propertyAccess.name=parseRightSideOfDot( true ))
(expression=finishNode( propertyAccess ))

}
}
return expression

}
def parseJsxExpression(inExpressionContext: Boolean): JsxExpression = {
 val node = createNode( SyntaxKind.JsxExpression ).asInstanceOf[JsxExpression]
parseExpected( SyntaxKind.OpenBraceToken )
if ((token()!==SyntaxKind.CloseBraceToken)) {
 (node.expression=parseAssignmentExpressionOrHigher())

}
if (inExpressionContext) {
 parseExpected( SyntaxKind.CloseBraceToken )

}
else {
 parseExpected( SyntaxKind.CloseBraceToken, undefined, false )
scanJsxText()

}
return finishNode( node )

}
def parseJsxAttribute(): ( JsxAttribute | JsxSpreadAttribute ) = {
 if ((token()===SyntaxKind.OpenBraceToken)) {
 return parseJsxSpreadAttribute()

}
scanJsxIdentifier()
val node = createNode( SyntaxKind.JsxAttribute ).asInstanceOf[JsxAttribute]
(node.name=parseIdentifierName())
if ((token()===SyntaxKind.EqualsToken)) {
 scanJsxAttributeValue() match {
  case  SyntaxKind.StringLiteral  =>
(node.initializer=parseLiteralNode().asInstanceOf[StringLiteral])
  case _ =>
(node.initializer=parseJsxExpression( true ))
}

}
return finishNode( node )

}
def parseJsxSpreadAttribute(): JsxSpreadAttribute = {
 val node = createNode( SyntaxKind.JsxSpreadAttribute ).asInstanceOf[JsxSpreadAttribute]
parseExpected( SyntaxKind.OpenBraceToken )
parseExpected( SyntaxKind.DotDotDotToken )
(node.expression=parseExpression())
parseExpected( SyntaxKind.CloseBraceToken )
return finishNode( node )

}
def parseJsxClosingElement(inExpressionContext: Boolean): JsxClosingElement = {
 val node = createNode( SyntaxKind.JsxClosingElement ).asInstanceOf[JsxClosingElement]
parseExpected( SyntaxKind.LessThanSlashToken )
(node.tagName=parseJsxElementName())
if (inExpressionContext) {
 parseExpected( SyntaxKind.GreaterThanToken )

}
else {
 parseExpected( SyntaxKind.GreaterThanToken, undefined, false )
scanJsxText()

}
return finishNode( node )

}
def parseTypeAssertion(): TypeAssertion = {
 val node = createNode( SyntaxKind.TypeAssertionExpression ).asInstanceOf[TypeAssertion]
parseExpected( SyntaxKind.LessThanToken )
(node.`type`=parseType())
parseExpected( SyntaxKind.GreaterThanToken )
(node.expression=parseSimpleUnaryExpression())
return finishNode( node )

}
def parseMemberExpressionRest(expression: LeftHandSideExpression): MemberExpression = {
 while (true) {
{
 val dotToken = parseOptionalToken( SyntaxKind.DotToken )
if (dotToken) {
 val propertyAccess = createNode( SyntaxKind.PropertyAccessExpression, expression.pos ).asInstanceOf[PropertyAccessExpression]
(propertyAccess.expression=expression)
(propertyAccess.name=parseRightSideOfDot( true ))
(expression=finishNode( propertyAccess ))
continue

}
if (((token()===SyntaxKind.ExclamationToken)&&(!scanner.hasPrecedingLineBreak()))) {
 nextToken()
val nonNullExpression = createNode( SyntaxKind.NonNullExpression, expression.pos ).asInstanceOf[NonNullExpression]
(nonNullExpression.expression=expression)
(expression=finishNode( nonNullExpression ))
continue

}
if (((!inDecoratorContext())&&parseOptional( SyntaxKind.OpenBracketToken ))) {
 val indexedAccess = createNode( SyntaxKind.ElementAccessExpression, expression.pos ).asInstanceOf[ElementAccessExpression]
(indexedAccess.expression=expression)
if ((token()!==SyntaxKind.CloseBracketToken)) {
 (indexedAccess.argumentExpression=allowInAnd( parseExpression ))
if (((indexedAccess.argumentExpression.kind===SyntaxKind.StringLiteral)||(indexedAccess.argumentExpression.kind===SyntaxKind.NumericLiteral))) {
 val literal = indexedAccess.argumentExpression.asInstanceOf[LiteralExpression]
(literal.text=internIdentifier( literal.text ))

}

}
parseExpected( SyntaxKind.CloseBracketToken )
(expression=finishNode( indexedAccess ))
continue

}
if (((token()===SyntaxKind.NoSubstitutionTemplateLiteral)||(token()===SyntaxKind.TemplateHead))) {
 val tagExpression = createNode( SyntaxKind.TaggedTemplateExpression, expression.pos ).asInstanceOf[TaggedTemplateExpression]
(tagExpression.tag=expression)
(tagExpression.template=(if ((token()===SyntaxKind.NoSubstitutionTemplateLiteral)) parseLiteralNode().asInstanceOf[NoSubstitutionTemplateLiteral] else parseTemplateExpression()))
(expression=finishNode( tagExpression ))
continue

}
return expression.asInstanceOf[MemberExpression]

}
}

}
def parseCallExpressionRest(expression: LeftHandSideExpression): LeftHandSideExpression = {
 while (true) {
{
 (expression=parseMemberExpressionRest( expression ))
if ((token()===SyntaxKind.LessThanToken)) {
 val typeArguments = tryParse( parseTypeArgumentsInExpression )
if ((!typeArguments)) {
 return expression

}
val callExpr = createNode( SyntaxKind.CallExpression, expression.pos ).asInstanceOf[CallExpression]
(callExpr.expression=expression)
(callExpr.typeArguments=typeArguments)
(callExpr.arguments=parseArgumentList())
(expression=finishNode( callExpr ))
continue

}
else if ((token()===SyntaxKind.OpenParenToken)) {
 val callExpr = createNode( SyntaxKind.CallExpression, expression.pos ).asInstanceOf[CallExpression]
(callExpr.expression=expression)
(callExpr.arguments=parseArgumentList())
(expression=finishNode( callExpr ))
continue

}
return expression

}
}

}
def parseArgumentList() = {
 parseExpected( SyntaxKind.OpenParenToken )
val result = parseDelimitedList( ParsingContext.ArgumentExpressions, parseArgumentExpression )
parseExpected( SyntaxKind.CloseParenToken )
return result

}
def parseTypeArgumentsInExpression() = {
 if ((!parseOptional( SyntaxKind.LessThanToken ))) {
 return undefined

}
val typeArguments = parseDelimitedList( ParsingContext.TypeArguments, parseType )
if ((!parseExpected( SyntaxKind.GreaterThanToken ))) {
 return undefined

}
return (if ((typeArguments&&canFollowTypeArgumentsInExpression())) typeArguments else undefined)

}
def canFollowTypeArgumentsInExpression(): Boolean = {
 token() match {
  case  SyntaxKind.OpenParenToken | SyntaxKind.DotToken | SyntaxKind.CloseParenToken | SyntaxKind.CloseBracketToken | SyntaxKind.ColonToken | SyntaxKind.SemicolonToken | SyntaxKind.QuestionToken | SyntaxKind.EqualsEqualsToken | SyntaxKind.EqualsEqualsEqualsToken | SyntaxKind.ExclamationEqualsToken | SyntaxKind.ExclamationEqualsEqualsToken | SyntaxKind.AmpersandAmpersandToken | SyntaxKind.BarBarToken | SyntaxKind.CaretToken | SyntaxKind.AmpersandToken | SyntaxKind.BarToken | SyntaxKind.CloseBraceToken | SyntaxKind.EndOfFileToken  =>
return true
  case _ =>
return false
}

}
def parsePrimaryExpression(): PrimaryExpression = {
 token() match {
  case  SyntaxKind.NumericLiteral | SyntaxKind.StringLiteral | SyntaxKind.NoSubstitutionTemplateLiteral  =>
return parseLiteralNode()
  case  SyntaxKind.ThisKeyword | SyntaxKind.SuperKeyword | SyntaxKind.NullKeyword | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword  =>
return parseTokenNode[ PrimaryExpression ]()
  case  SyntaxKind.OpenParenToken  =>
return parseParenthesizedExpression()
  case  SyntaxKind.OpenBracketToken  =>
return parseArrayLiteralExpression()
  case  SyntaxKind.OpenBraceToken  =>
return parseObjectLiteralExpression()
  case  SyntaxKind.AsyncKeyword  =>
if ((!lookAhead( nextTokenIsFunctionKeywordOnSameLine ))) {
 break()

}
return parseFunctionExpression()
  case  SyntaxKind.ClassKeyword  =>
return parseClassExpression()
  case  SyntaxKind.FunctionKeyword  =>
return parseFunctionExpression()
  case  SyntaxKind.NewKeyword  =>
return parseNewExpression()
  case  SyntaxKind.SlashToken | SyntaxKind.SlashEqualsToken  =>
if ((reScanSlashToken()===SyntaxKind.RegularExpressionLiteral)) {
 return parseLiteralNode()

}
  case  SyntaxKind.TemplateHead  =>
return parseTemplateExpression()
  case _ =>
}
return parseIdentifier( Diagnostics.Expression_expected )

}
def parseParenthesizedExpression(): ParenthesizedExpression = {
 val node = createNode( SyntaxKind.ParenthesizedExpression ).asInstanceOf[ParenthesizedExpression]
parseExpected( SyntaxKind.OpenParenToken )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseParenToken )
return finishNode( node )

}
def parseSpreadElement(): Expression = {
 val node = createNode( SyntaxKind.SpreadElementExpression ).asInstanceOf[SpreadElementExpression]
parseExpected( SyntaxKind.DotDotDotToken )
(node.expression=parseAssignmentExpressionOrHigher())
return finishNode( node )

}
def parseArgumentOrArrayLiteralElement(): Expression = {
 return (if ((token()===SyntaxKind.DotDotDotToken)) parseSpreadElement() else (if ((token()===SyntaxKind.CommaToken)) createNode( SyntaxKind.OmittedExpression ).asInstanceOf[Expression] else parseAssignmentExpressionOrHigher()))

}
def parseArgumentExpression(): Expression = {
 return doOutsideOfContext( disallowInAndDecoratorContext, parseArgumentOrArrayLiteralElement )

}
def parseArrayLiteralExpression(): ArrayLiteralExpression = {
 val node = createNode( SyntaxKind.ArrayLiteralExpression ).asInstanceOf[ArrayLiteralExpression]
parseExpected( SyntaxKind.OpenBracketToken )
if (scanner.hasPrecedingLineBreak()) {
 (node.multiLine=true)

}
(node.elements=parseDelimitedList( ParsingContext.ArrayLiteralMembers, parseArgumentOrArrayLiteralElement ))
parseExpected( SyntaxKind.CloseBracketToken )
return finishNode( node )

}
def tryParseAccessorDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): AccessorDeclaration = {
 if (parseContextualModifier( SyntaxKind.GetKeyword )) {
 return parseAccessorDeclaration( SyntaxKind.GetAccessor, fullStart, decorators, modifiers )

}
else if (parseContextualModifier( SyntaxKind.SetKeyword )) {
 return parseAccessorDeclaration( SyntaxKind.SetAccessor, fullStart, decorators, modifiers )

}
return undefined

}
def parseObjectLiteralElement(): ObjectLiteralElementLike = {
 val fullStart = scanner.getStartPos()
val decorators = parseDecorators()
val modifiers = parseModifiers()
val accessor = tryParseAccessorDeclaration( fullStart, decorators, modifiers )
if (accessor) {
 return accessor

}
val asteriskToken = parseOptionalToken( SyntaxKind.AsteriskToken )
val tokenIsIdentifier = isIdentifier()
val propertyName = parsePropertyName()
val questionToken = parseOptionalToken( SyntaxKind.QuestionToken )
if (((asteriskToken||(token()===SyntaxKind.OpenParenToken))||(token()===SyntaxKind.LessThanToken))) {
 return parseMethodDeclaration( fullStart, decorators, modifiers, asteriskToken, propertyName, questionToken )

}
val isShorthandPropertyAssignment = (tokenIsIdentifier&&((((token()===SyntaxKind.CommaToken)||(token()===SyntaxKind.CloseBraceToken))||(token()===SyntaxKind.EqualsToken))))
if (isShorthandPropertyAssignment) {
 val shorthandDeclaration = createNode( SyntaxKind.ShorthandPropertyAssignment, fullStart ).asInstanceOf[ShorthandPropertyAssignment]
(shorthandDeclaration.name=propertyName.asInstanceOf[Identifier])
(shorthandDeclaration.questionToken=questionToken)
val equalsToken = parseOptionalToken( SyntaxKind.EqualsToken )
if (equalsToken) {
 (shorthandDeclaration.equalsToken=equalsToken)
(shorthandDeclaration.objectAssignmentInitializer=allowInAnd( parseAssignmentExpressionOrHigher ))

}
return addJSDocComment( finishNode( shorthandDeclaration ) )

}
else {
 val propertyAssignment = createNode( SyntaxKind.PropertyAssignment, fullStart ).asInstanceOf[PropertyAssignment]
(propertyAssignment.modifiers=modifiers)
(propertyAssignment.name=propertyName)
(propertyAssignment.questionToken=questionToken)
parseExpected( SyntaxKind.ColonToken )
(propertyAssignment.initializer=allowInAnd( parseAssignmentExpressionOrHigher ))
return addJSDocComment( finishNode( propertyAssignment ) )

}

}
def parseObjectLiteralExpression(): ObjectLiteralExpression = {
 val node = createNode( SyntaxKind.ObjectLiteralExpression ).asInstanceOf[ObjectLiteralExpression]
parseExpected( SyntaxKind.OpenBraceToken )
if (scanner.hasPrecedingLineBreak()) {
 (node.multiLine=true)

}
(node.properties=parseDelimitedList( ParsingContext.ObjectLiteralMembers, parseObjectLiteralElement, true ))
parseExpected( SyntaxKind.CloseBraceToken )
return finishNode( node )

}
def parseFunctionExpression(): FunctionExpression = {
 val saveDecoratorContext = inDecoratorContext()
if (saveDecoratorContext) {
 setDecoratorContext( false )

}
val node = createNode( SyntaxKind.FunctionExpression ).asInstanceOf[FunctionExpression]
(node.modifiers=parseModifiers())
parseExpected( SyntaxKind.FunctionKeyword )
(node.asteriskToken=parseOptionalToken( SyntaxKind.AsteriskToken ))
val isGenerator = (!(!node.asteriskToken))
val isAsync = (!(!((getModifierFlags( node )&ModifierFlags.Async))))
(node.name=(if ((isGenerator&&isAsync)) doInYieldAndAwaitContext( parseOptionalIdentifier ) else (if (isGenerator) doInYieldContext( parseOptionalIdentifier ) else (if (isAsync) doInAwaitContext( parseOptionalIdentifier ) else parseOptionalIdentifier()))))
fillSignature( SyntaxKind.ColonToken, isGenerator, isAsync, false, node )
(node.body=parseFunctionBlock( isGenerator, isAsync, false ))
if (saveDecoratorContext) {
 setDecoratorContext( true )

}
return addJSDocComment( finishNode( node ) )

}
def parseOptionalIdentifier() = {
 return (if (isIdentifier()) parseIdentifier() else undefined)

}
def parseNewExpression(): NewExpression = {
 val node = createNode( SyntaxKind.NewExpression ).asInstanceOf[NewExpression]
parseExpected( SyntaxKind.NewKeyword )
(node.expression=parseMemberExpressionOrHigher())
(node.typeArguments=tryParse( parseTypeArgumentsInExpression ))
if ((node.typeArguments||(token()===SyntaxKind.OpenParenToken))) {
 (node.arguments=parseArgumentList())

}
return finishNode( node )

}
def parseBlock(ignoreMissingOpenBrace: Boolean, diagnosticMessage: DiagnosticMessage): Block = {
 val node = createNode( SyntaxKind.Block ).asInstanceOf[Block]
if ((parseExpected( SyntaxKind.OpenBraceToken, diagnosticMessage )||ignoreMissingOpenBrace)) {
 if (scanner.hasPrecedingLineBreak()) {
 (node.multiLine=true)

}
(node.statements=parseList( ParsingContext.BlockStatements, parseStatement ))
parseExpected( SyntaxKind.CloseBraceToken )

}
else {
 (node.statements=createMissingList[ Statement ]())

}
return finishNode( node )

}
def parseFunctionBlock(allowYield: Boolean, allowAwait: Boolean, ignoreMissingOpenBrace: Boolean, diagnosticMessage: DiagnosticMessage): Block = {
 val savedYieldContext = inYieldContext()
setYieldContext( allowYield )
val savedAwaitContext = inAwaitContext()
setAwaitContext( allowAwait )
val saveDecoratorContext = inDecoratorContext()
if (saveDecoratorContext) {
 setDecoratorContext( false )

}
val block = parseBlock( ignoreMissingOpenBrace, diagnosticMessage )
if (saveDecoratorContext) {
 setDecoratorContext( true )

}
setYieldContext( savedYieldContext )
setAwaitContext( savedAwaitContext )
return block

}
def parseEmptyStatement(): Statement = {
 val node = createNode( SyntaxKind.EmptyStatement ).asInstanceOf[Statement]
parseExpected( SyntaxKind.SemicolonToken )
return finishNode( node )

}
def parseIfStatement(): IfStatement = {
 val node = createNode( SyntaxKind.IfStatement ).asInstanceOf[IfStatement]
parseExpected( SyntaxKind.IfKeyword )
parseExpected( SyntaxKind.OpenParenToken )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseParenToken )
(node.thenStatement=parseStatement())
(node.elseStatement=(if (parseOptional( SyntaxKind.ElseKeyword )) parseStatement() else undefined))
return finishNode( node )

}
def parseDoStatement(): DoStatement = {
 val node = createNode( SyntaxKind.DoStatement ).asInstanceOf[DoStatement]
parseExpected( SyntaxKind.DoKeyword )
(node.statement=parseStatement())
parseExpected( SyntaxKind.WhileKeyword )
parseExpected( SyntaxKind.OpenParenToken )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseParenToken )
parseOptional( SyntaxKind.SemicolonToken )
return finishNode( node )

}
def parseWhileStatement(): WhileStatement = {
 val node = createNode( SyntaxKind.WhileStatement ).asInstanceOf[WhileStatement]
parseExpected( SyntaxKind.WhileKeyword )
parseExpected( SyntaxKind.OpenParenToken )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseParenToken )
(node.statement=parseStatement())
return finishNode( node )

}
def parseForOrForInOrForOfStatement(): Statement = {
 val pos = getNodePos()
parseExpected( SyntaxKind.ForKeyword )
parseExpected( SyntaxKind.OpenParenToken )
var initializer: ( VariableDeclarationList | Expression ) = undefined
if ((token()!==SyntaxKind.SemicolonToken)) {
 if ((((token()===SyntaxKind.VarKeyword)||(token()===SyntaxKind.LetKeyword))||(token()===SyntaxKind.ConstKeyword))) {
 (initializer=parseVariableDeclarationList( true ))

}
else {
 (initializer=disallowInAnd( parseExpression ))

}

}
var forOrForInOrForOfStatement: IterationStatement = zeroOfMyType
if (parseOptional( SyntaxKind.InKeyword )) {
 val forInStatement = createNode( SyntaxKind.ForInStatement, pos ).asInstanceOf[ForInStatement]
(forInStatement.initializer=initializer)
(forInStatement.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseParenToken )
(forOrForInOrForOfStatement=forInStatement)

}
else if (parseOptional( SyntaxKind.OfKeyword )) {
 val forOfStatement = createNode( SyntaxKind.ForOfStatement, pos ).asInstanceOf[ForOfStatement]
(forOfStatement.initializer=initializer)
(forOfStatement.expression=allowInAnd( parseAssignmentExpressionOrHigher ))
parseExpected( SyntaxKind.CloseParenToken )
(forOrForInOrForOfStatement=forOfStatement)

}
else {
 val forStatement = createNode( SyntaxKind.ForStatement, pos ).asInstanceOf[ForStatement]
(forStatement.initializer=initializer)
parseExpected( SyntaxKind.SemicolonToken )
if (((token()!==SyntaxKind.SemicolonToken)&&(token()!==SyntaxKind.CloseParenToken))) {
 (forStatement.condition=allowInAnd( parseExpression ))

}
parseExpected( SyntaxKind.SemicolonToken )
if ((token()!==SyntaxKind.CloseParenToken)) {
 (forStatement.incrementor=allowInAnd( parseExpression ))

}
parseExpected( SyntaxKind.CloseParenToken )
(forOrForInOrForOfStatement=forStatement)

}
(forOrForInOrForOfStatement.statement=parseStatement())
return finishNode( forOrForInOrForOfStatement )

}
def parseBreakOrContinueStatement(kind: SyntaxKind): BreakOrContinueStatement = {
 val node = createNode( kind ).asInstanceOf[BreakOrContinueStatement]
parseExpected( (if ((kind===SyntaxKind.BreakStatement)) SyntaxKind.BreakKeyword else SyntaxKind.ContinueKeyword) )
if ((!canParseSemicolon())) {
 (node.label=parseIdentifier())

}
parseSemicolon()
return finishNode( node )

}
def parseReturnStatement(): ReturnStatement = {
 val node = createNode( SyntaxKind.ReturnStatement ).asInstanceOf[ReturnStatement]
parseExpected( SyntaxKind.ReturnKeyword )
if ((!canParseSemicolon())) {
 (node.expression=allowInAnd( parseExpression ))

}
parseSemicolon()
return finishNode( node )

}
def parseWithStatement(): WithStatement = {
 val node = createNode( SyntaxKind.WithStatement ).asInstanceOf[WithStatement]
parseExpected( SyntaxKind.WithKeyword )
parseExpected( SyntaxKind.OpenParenToken )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseParenToken )
(node.statement=parseStatement())
return finishNode( node )

}
def parseCaseClause(): CaseClause = {
 val node = createNode( SyntaxKind.CaseClause ).asInstanceOf[CaseClause]
parseExpected( SyntaxKind.CaseKeyword )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.ColonToken )
(node.statements=parseList( ParsingContext.SwitchClauseStatements, parseStatement ))
return finishNode( node )

}
def parseDefaultClause(): DefaultClause = {
 val node = createNode( SyntaxKind.DefaultClause ).asInstanceOf[DefaultClause]
parseExpected( SyntaxKind.DefaultKeyword )
parseExpected( SyntaxKind.ColonToken )
(node.statements=parseList( ParsingContext.SwitchClauseStatements, parseStatement ))
return finishNode( node )

}
def parseCaseOrDefaultClause(): CaseOrDefaultClause = {
 return (if ((token()===SyntaxKind.CaseKeyword)) parseCaseClause() else parseDefaultClause())

}
def parseSwitchStatement(): SwitchStatement = {
 val node = createNode( SyntaxKind.SwitchStatement ).asInstanceOf[SwitchStatement]
parseExpected( SyntaxKind.SwitchKeyword )
parseExpected( SyntaxKind.OpenParenToken )
(node.expression=allowInAnd( parseExpression ))
parseExpected( SyntaxKind.CloseParenToken )
val caseBlock = createNode( SyntaxKind.CaseBlock, scanner.getStartPos() ).asInstanceOf[CaseBlock]
parseExpected( SyntaxKind.OpenBraceToken )
(caseBlock.clauses=parseList( ParsingContext.SwitchClauses, parseCaseOrDefaultClause ))
parseExpected( SyntaxKind.CloseBraceToken )
(node.caseBlock=finishNode( caseBlock ))
return finishNode( node )

}
def parseThrowStatement(): ThrowStatement = {
 val node = createNode( SyntaxKind.ThrowStatement ).asInstanceOf[ThrowStatement]
parseExpected( SyntaxKind.ThrowKeyword )
(node.expression=(if (scanner.hasPrecedingLineBreak()) undefined else allowInAnd( parseExpression )))
parseSemicolon()
return finishNode( node )

}
def parseTryStatement(): TryStatement = {
 val node = createNode( SyntaxKind.TryStatement ).asInstanceOf[TryStatement]
parseExpected( SyntaxKind.TryKeyword )
(node.tryBlock=parseBlock( false ))
(node.catchClause=(if ((token()===SyntaxKind.CatchKeyword)) parseCatchClause() else undefined))
if (((!node.catchClause)||(token()===SyntaxKind.FinallyKeyword))) {
 parseExpected( SyntaxKind.FinallyKeyword )
(node.finallyBlock=parseBlock( false ))

}
return finishNode( node )

}
def parseCatchClause(): CatchClause = {
 val result = createNode( SyntaxKind.CatchClause ).asInstanceOf[CatchClause]
parseExpected( SyntaxKind.CatchKeyword )
if (parseExpected( SyntaxKind.OpenParenToken )) {
 (result.variableDeclaration=parseVariableDeclaration())

}
parseExpected( SyntaxKind.CloseParenToken )
(result.block=parseBlock( false ))
return finishNode( result )

}
def parseDebuggerStatement(): Statement = {
 val node = createNode( SyntaxKind.DebuggerStatement ).asInstanceOf[Statement]
parseExpected( SyntaxKind.DebuggerKeyword )
parseSemicolon()
return finishNode( node )

}
def parseExpressionOrLabeledStatement(): ( ExpressionStatement | LabeledStatement ) = {
 val fullStart = scanner.getStartPos()
val expression = allowInAnd( parseExpression )
if (((expression.kind===SyntaxKind.Identifier)&&parseOptional( SyntaxKind.ColonToken ))) {
 val labeledStatement = createNode( SyntaxKind.LabeledStatement, fullStart ).asInstanceOf[LabeledStatement]
(labeledStatement.label=expression.asInstanceOf[Identifier])
(labeledStatement.statement=parseStatement())
return addJSDocComment( finishNode( labeledStatement ) )

}
else {
 val expressionStatement = createNode( SyntaxKind.ExpressionStatement, fullStart ).asInstanceOf[ExpressionStatement]
(expressionStatement.expression=expression)
parseSemicolon()
return addJSDocComment( finishNode( expressionStatement ) )

}

}
def nextTokenIsIdentifierOrKeywordOnSameLine() = {
 nextToken()
return (tokenIsIdentifierOrKeyword( token() )&&(!scanner.hasPrecedingLineBreak()))

}
def nextTokenIsFunctionKeywordOnSameLine() = {
 nextToken()
return ((token()===SyntaxKind.FunctionKeyword)&&(!scanner.hasPrecedingLineBreak()))

}
def nextTokenIsIdentifierOrKeywordOrNumberOnSameLine() = {
 nextToken()
return (((tokenIsIdentifierOrKeyword( token() )||(token()===SyntaxKind.NumericLiteral)))&&(!scanner.hasPrecedingLineBreak()))

}
def isDeclaration(): Boolean = {
 while (true) {
{
 token() match {
  case  SyntaxKind.VarKeyword | SyntaxKind.LetKeyword | SyntaxKind.ConstKeyword | SyntaxKind.FunctionKeyword | SyntaxKind.ClassKeyword | SyntaxKind.EnumKeyword  =>
return true
  case  SyntaxKind.InterfaceKeyword | SyntaxKind.TypeKeyword  =>
return nextTokenIsIdentifierOnSameLine()
  case  SyntaxKind.ModuleKeyword | SyntaxKind.NamespaceKeyword  =>
return nextTokenIsIdentifierOrStringLiteralOnSameLine()
  case  SyntaxKind.AbstractKeyword | SyntaxKind.AsyncKeyword | SyntaxKind.DeclareKeyword | SyntaxKind.PrivateKeyword | SyntaxKind.ProtectedKeyword | SyntaxKind.PublicKeyword | SyntaxKind.ReadonlyKeyword  =>
nextToken()
if (scanner.hasPrecedingLineBreak()) {
 return false

}
continue
  case  SyntaxKind.GlobalKeyword  =>
nextToken()
return (((token()===SyntaxKind.OpenBraceToken)||(token()===SyntaxKind.Identifier))||(token()===SyntaxKind.ExportKeyword))
  case  SyntaxKind.ImportKeyword  =>
nextToken()
return ((((token()===SyntaxKind.StringLiteral)||(token()===SyntaxKind.AsteriskToken))||(token()===SyntaxKind.OpenBraceToken))||tokenIsIdentifierOrKeyword( token() ))
  case  SyntaxKind.ExportKeyword  =>
nextToken()
if ((((((token()===SyntaxKind.EqualsToken)||(token()===SyntaxKind.AsteriskToken))||(token()===SyntaxKind.OpenBraceToken))||(token()===SyntaxKind.DefaultKeyword))||(token()===SyntaxKind.AsKeyword))) {
 return true

}
continue
  case  SyntaxKind.StaticKeyword  =>
nextToken()
continue
  case _ =>
return false
}

}
}

}
def isStartOfDeclaration(): Boolean = {
 return lookAhead( isDeclaration )

}
def isStartOfStatement(): Boolean = {
 token() match {
  case  SyntaxKind.AtToken | SyntaxKind.SemicolonToken | SyntaxKind.OpenBraceToken | SyntaxKind.VarKeyword | SyntaxKind.LetKeyword | SyntaxKind.FunctionKeyword | SyntaxKind.ClassKeyword | SyntaxKind.EnumKeyword | SyntaxKind.IfKeyword | SyntaxKind.DoKeyword | SyntaxKind.WhileKeyword | SyntaxKind.ForKeyword | SyntaxKind.ContinueKeyword | SyntaxKind.BreakKeyword | SyntaxKind.ReturnKeyword | SyntaxKind.WithKeyword | SyntaxKind.SwitchKeyword | SyntaxKind.ThrowKeyword | SyntaxKind.TryKeyword | SyntaxKind.DebuggerKeyword | SyntaxKind.CatchKeyword | SyntaxKind.FinallyKeyword  =>
return true
  case  SyntaxKind.ConstKeyword | SyntaxKind.ExportKeyword | SyntaxKind.ImportKeyword  =>
return isStartOfDeclaration()
  case  SyntaxKind.AsyncKeyword | SyntaxKind.DeclareKeyword | SyntaxKind.InterfaceKeyword | SyntaxKind.ModuleKeyword | SyntaxKind.NamespaceKeyword | SyntaxKind.TypeKeyword | SyntaxKind.GlobalKeyword  =>
return true
  case  SyntaxKind.PublicKeyword | SyntaxKind.PrivateKeyword | SyntaxKind.ProtectedKeyword | SyntaxKind.StaticKeyword | SyntaxKind.ReadonlyKeyword  =>
return (isStartOfDeclaration()||(!lookAhead( nextTokenIsIdentifierOrKeywordOnSameLine )))
  case _ =>
return isStartOfExpression()
}

}
def nextTokenIsIdentifierOrStartOfDestructuring() = {
 nextToken()
return ((isIdentifier()||(token()===SyntaxKind.OpenBraceToken))||(token()===SyntaxKind.OpenBracketToken))

}
def isLetDeclaration() = {
 return lookAhead( nextTokenIsIdentifierOrStartOfDestructuring )

}
def parseStatement(): Statement = {
 token() match {
  case  SyntaxKind.SemicolonToken  =>
return parseEmptyStatement()
  case  SyntaxKind.OpenBraceToken  =>
return parseBlock( false )
  case  SyntaxKind.VarKeyword  =>
return parseVariableStatement( scanner.getStartPos(), undefined, undefined )
  case  SyntaxKind.LetKeyword  =>
if (isLetDeclaration()) {
 return parseVariableStatement( scanner.getStartPos(), undefined, undefined )

}
  case  SyntaxKind.FunctionKeyword  =>
return parseFunctionDeclaration( scanner.getStartPos(), undefined, undefined )
  case  SyntaxKind.ClassKeyword  =>
return parseClassDeclaration( scanner.getStartPos(), undefined, undefined )
  case  SyntaxKind.IfKeyword  =>
return parseIfStatement()
  case  SyntaxKind.DoKeyword  =>
return parseDoStatement()
  case  SyntaxKind.WhileKeyword  =>
return parseWhileStatement()
  case  SyntaxKind.ForKeyword  =>
return parseForOrForInOrForOfStatement()
  case  SyntaxKind.ContinueKeyword  =>
return parseBreakOrContinueStatement( SyntaxKind.ContinueStatement )
  case  SyntaxKind.BreakKeyword  =>
return parseBreakOrContinueStatement( SyntaxKind.BreakStatement )
  case  SyntaxKind.ReturnKeyword  =>
return parseReturnStatement()
  case  SyntaxKind.WithKeyword  =>
return parseWithStatement()
  case  SyntaxKind.SwitchKeyword  =>
return parseSwitchStatement()
  case  SyntaxKind.ThrowKeyword  =>
return parseThrowStatement()
  case  SyntaxKind.TryKeyword | SyntaxKind.CatchKeyword | SyntaxKind.FinallyKeyword  =>
return parseTryStatement()
  case  SyntaxKind.DebuggerKeyword  =>
return parseDebuggerStatement()
  case  SyntaxKind.AtToken  =>
return parseDeclaration()
  case  SyntaxKind.AsyncKeyword | SyntaxKind.InterfaceKeyword | SyntaxKind.TypeKeyword | SyntaxKind.ModuleKeyword | SyntaxKind.NamespaceKeyword | SyntaxKind.DeclareKeyword | SyntaxKind.ConstKeyword | SyntaxKind.EnumKeyword | SyntaxKind.ExportKeyword | SyntaxKind.ImportKeyword | SyntaxKind.PrivateKeyword | SyntaxKind.ProtectedKeyword | SyntaxKind.PublicKeyword | SyntaxKind.AbstractKeyword | SyntaxKind.StaticKeyword | SyntaxKind.ReadonlyKeyword | SyntaxKind.GlobalKeyword  =>
if (isStartOfDeclaration()) {
 return parseDeclaration()

}
  case _ =>
}
return parseExpressionOrLabeledStatement()

}
def parseDeclaration(): Statement = {
 val fullStart = getNodePos()
val decorators = parseDecorators()
val modifiers = parseModifiers()
token() match {
  case  SyntaxKind.VarKeyword | SyntaxKind.LetKeyword | SyntaxKind.ConstKeyword  =>
return parseVariableStatement( fullStart, decorators, modifiers )
  case  SyntaxKind.FunctionKeyword  =>
return parseFunctionDeclaration( fullStart, decorators, modifiers )
  case  SyntaxKind.ClassKeyword  =>
return parseClassDeclaration( fullStart, decorators, modifiers )
  case  SyntaxKind.InterfaceKeyword  =>
return parseInterfaceDeclaration( fullStart, decorators, modifiers )
  case  SyntaxKind.TypeKeyword  =>
return parseTypeAliasDeclaration( fullStart, decorators, modifiers )
  case  SyntaxKind.EnumKeyword  =>
return parseEnumDeclaration( fullStart, decorators, modifiers )
  case  SyntaxKind.GlobalKeyword | SyntaxKind.ModuleKeyword | SyntaxKind.NamespaceKeyword  =>
return parseModuleDeclaration( fullStart, decorators, modifiers )
  case  SyntaxKind.ImportKeyword  =>
return parseImportDeclarationOrImportEqualsDeclaration( fullStart, decorators, modifiers )
  case  SyntaxKind.ExportKeyword  =>
nextToken()
token() match {
  case  SyntaxKind.DefaultKeyword | SyntaxKind.EqualsToken  =>
return parseExportAssignment( fullStart, decorators, modifiers )
  case  SyntaxKind.AsKeyword  =>
return parseNamespaceExportDeclaration( fullStart, decorators, modifiers )
  case _ =>
return parseExportDeclaration( fullStart, decorators, modifiers )
}
  case _ =>
if ((decorators||modifiers)) {
 val node = createMissingNode( SyntaxKind.MissingDeclaration, true, Diagnostics.Declaration_expected ).asInstanceOf[Statement]
(node.pos=fullStart)
(node.decorators=decorators)
(node.modifiers=modifiers)
return finishNode( node )

}
}

}
def nextTokenIsIdentifierOrStringLiteralOnSameLine() = {
 nextToken()
return ((!scanner.hasPrecedingLineBreak())&&((isIdentifier()||(token()===SyntaxKind.StringLiteral))))

}
def parseFunctionBlockOrSemicolon(isGenerator: Boolean, isAsync: Boolean, diagnosticMessage: DiagnosticMessage): Block = {
 if (((token()!==SyntaxKind.OpenBraceToken)&&canParseSemicolon())) {
 parseSemicolon()
return

}
return parseFunctionBlock( isGenerator, isAsync, false, diagnosticMessage )

}
def parseArrayBindingElement(): ArrayBindingElement = {
 if ((token()===SyntaxKind.CommaToken)) {
 return createNode( SyntaxKind.OmittedExpression ).asInstanceOf[OmittedExpression]

}
val node = createNode( SyntaxKind.BindingElement ).asInstanceOf[BindingElement]
(node.dotDotDotToken=parseOptionalToken( SyntaxKind.DotDotDotToken ))
(node.name=parseIdentifierOrPattern())
(node.initializer=parseBindingElementInitializer( false ))
return finishNode( node )

}
def parseObjectBindingElement(): BindingElement = {
 val node = createNode( SyntaxKind.BindingElement ).asInstanceOf[BindingElement]
val tokenIsIdentifier = isIdentifier()
val propertyName = parsePropertyName()
if ((tokenIsIdentifier&&(token()!==SyntaxKind.ColonToken))) {
 (node.name=propertyName.asInstanceOf[Identifier])

}
else {
 parseExpected( SyntaxKind.ColonToken )
(node.propertyName=propertyName)
(node.name=parseIdentifierOrPattern())

}
(node.initializer=parseBindingElementInitializer( false ))
return finishNode( node )

}
def parseObjectBindingPattern(): ObjectBindingPattern = {
 val node = createNode( SyntaxKind.ObjectBindingPattern ).asInstanceOf[ObjectBindingPattern]
parseExpected( SyntaxKind.OpenBraceToken )
(node.elements=parseDelimitedList( ParsingContext.ObjectBindingElements, parseObjectBindingElement ))
parseExpected( SyntaxKind.CloseBraceToken )
return finishNode( node )

}
def parseArrayBindingPattern(): ArrayBindingPattern = {
 val node = createNode( SyntaxKind.ArrayBindingPattern ).asInstanceOf[ArrayBindingPattern]
parseExpected( SyntaxKind.OpenBracketToken )
(node.elements=parseDelimitedList( ParsingContext.ArrayBindingElements, parseArrayBindingElement ))
parseExpected( SyntaxKind.CloseBracketToken )
return finishNode( node )

}
def isIdentifierOrPattern() = {
 return (((token()===SyntaxKind.OpenBraceToken)||(token()===SyntaxKind.OpenBracketToken))||isIdentifier())

}
def parseIdentifierOrPattern(): ( Identifier | BindingPattern ) = {
 if ((token()===SyntaxKind.OpenBracketToken)) {
 return parseArrayBindingPattern()

}
if ((token()===SyntaxKind.OpenBraceToken)) {
 return parseObjectBindingPattern()

}
return parseIdentifier()

}
def parseVariableDeclaration(): VariableDeclaration = {
 val node = createNode( SyntaxKind.VariableDeclaration ).asInstanceOf[VariableDeclaration]
(node.name=parseIdentifierOrPattern())
(node.`type`=parseTypeAnnotation())
if ((!isInOrOfKeyword( token() ))) {
 (node.initializer=parseInitializer( false ))

}
return finishNode( node )

}
def parseVariableDeclarationList(inForStatementInitializer: Boolean): VariableDeclarationList = {
 val node = createNode( SyntaxKind.VariableDeclarationList ).asInstanceOf[VariableDeclarationList]
token() match {
  case  SyntaxKind.VarKeyword  =>
  case  SyntaxKind.LetKeyword  =>
(node.flags|=NodeFlags.Let)
  case  SyntaxKind.ConstKeyword  =>
(node.flags|=NodeFlags.Const)
  case _ =>
Debug.fail()
}
nextToken()
if (((token()===SyntaxKind.OfKeyword)&&lookAhead( canFollowContextualOfKeyword ))) {
 (node.declarations=createMissingList[ VariableDeclaration ]())

}
else {
 val savedDisallowIn = inDisallowInContext()
setDisallowInContext( inForStatementInitializer )
(node.declarations=parseDelimitedList( ParsingContext.VariableDeclarations, parseVariableDeclaration ))
setDisallowInContext( savedDisallowIn )

}
return finishNode( node )

}
def canFollowContextualOfKeyword(): Boolean = {
 return (nextTokenIsIdentifier()&&(nextToken()===SyntaxKind.CloseParenToken))

}
def parseVariableStatement(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): VariableStatement = {
 val node = createNode( SyntaxKind.VariableStatement, fullStart ).asInstanceOf[VariableStatement]
(node.decorators=decorators)
(node.modifiers=modifiers)
(node.declarationList=parseVariableDeclarationList( false ))
parseSemicolon()
return addJSDocComment( finishNode( node ) )

}
def parseFunctionDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): FunctionDeclaration = {
 val node = createNode( SyntaxKind.FunctionDeclaration, fullStart ).asInstanceOf[FunctionDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
parseExpected( SyntaxKind.FunctionKeyword )
(node.asteriskToken=parseOptionalToken( SyntaxKind.AsteriskToken ))
(node.name=(if (hasModifier( node, ModifierFlags.Default )) parseOptionalIdentifier() else parseIdentifier()))
val isGenerator = (!(!node.asteriskToken))
val isAsync = hasModifier( node, ModifierFlags.Async )
fillSignature( SyntaxKind.ColonToken, isGenerator, isAsync, false, node )
(node.body=parseFunctionBlockOrSemicolon( isGenerator, isAsync, Diagnostics.or_expected ))
return addJSDocComment( finishNode( node ) )

}
def parseConstructorDeclaration(pos: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ConstructorDeclaration = {
 val node = createNode( SyntaxKind.Constructor, pos ).asInstanceOf[ConstructorDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
parseExpected( SyntaxKind.ConstructorKeyword )
fillSignature( SyntaxKind.ColonToken, false, false, false, node )
(node.body=parseFunctionBlockOrSemicolon( false, false, Diagnostics.or_expected ))
return addJSDocComment( finishNode( node ) )

}
def parseMethodDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier], asteriskToken: AsteriskToken, name: PropertyName, questionToken: QuestionToken, diagnosticMessage: DiagnosticMessage): MethodDeclaration = {
 val method = createNode( SyntaxKind.MethodDeclaration, fullStart ).asInstanceOf[MethodDeclaration]
(method.decorators=decorators)
(method.modifiers=modifiers)
(method.asteriskToken=asteriskToken)
(method.name=name)
(method.questionToken=questionToken)
val isGenerator = (!(!asteriskToken))
val isAsync = hasModifier( method, ModifierFlags.Async )
fillSignature( SyntaxKind.ColonToken, isGenerator, isAsync, false, method )
(method.body=parseFunctionBlockOrSemicolon( isGenerator, isAsync, diagnosticMessage ))
return addJSDocComment( finishNode( method ) )

}
def parsePropertyDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier], name: PropertyName, questionToken: QuestionToken): ClassElement = {
 val property = createNode( SyntaxKind.PropertyDeclaration, fullStart ).asInstanceOf[PropertyDeclaration]
(property.decorators=decorators)
(property.modifiers=modifiers)
(property.name=name)
(property.questionToken=questionToken)
(property.`type`=parseTypeAnnotation())
(property.initializer=(if (hasModifier( property, ModifierFlags.Static )) allowInAnd( parseNonParameterInitializer ) else doOutsideOfContext( (NodeFlags.YieldContext|NodeFlags.DisallowInContext), parseNonParameterInitializer )))
parseSemicolon()
return addJSDocComment( finishNode( property ) )

}
def parsePropertyOrMethodDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ClassElement = {
 val asteriskToken = parseOptionalToken( SyntaxKind.AsteriskToken )
val name = parsePropertyName()
val questionToken = parseOptionalToken( SyntaxKind.QuestionToken )
if (((asteriskToken||(token()===SyntaxKind.OpenParenToken))||(token()===SyntaxKind.LessThanToken))) {
 return parseMethodDeclaration( fullStart, decorators, modifiers, asteriskToken, name, questionToken, Diagnostics.or_expected )

}
else {
 return parsePropertyDeclaration( fullStart, decorators, modifiers, name, questionToken )

}

}
def parseNonParameterInitializer() = {
 return parseInitializer( false )

}
def parseAccessorDeclaration(kind: SyntaxKind, fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): AccessorDeclaration = {
 val node = createNode( kind, fullStart ).asInstanceOf[AccessorDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
(node.name=parsePropertyName())
fillSignature( SyntaxKind.ColonToken, false, false, false, node )
(node.body=parseFunctionBlockOrSemicolon( false, false ))
return addJSDocComment( finishNode( node ) )

}
def isClassMemberModifier(idToken: SyntaxKind) = {
 idToken match {
  case  SyntaxKind.PublicKeyword | SyntaxKind.PrivateKeyword | SyntaxKind.ProtectedKeyword | SyntaxKind.StaticKeyword | SyntaxKind.ReadonlyKeyword  =>
return true
  case _ =>
return false
}

}
def isClassMemberStart(): Boolean = {
 var idToken: SyntaxKind = zeroOfMyType
if ((token()===SyntaxKind.AtToken)) {
 return true

}
while (isModifierKind( token() )) {
{
 (idToken=token())
if (isClassMemberModifier( idToken )) {
 return true

}
nextToken()

}
}
if ((token()===SyntaxKind.AsteriskToken)) {
 return true

}
if (isLiteralPropertyName()) {
 (idToken=token())
nextToken()

}
if ((token()===SyntaxKind.OpenBracketToken)) {
 return true

}
if ((idToken!==undefined)) {
 if ((((!isKeyword( idToken ))||(idToken===SyntaxKind.SetKeyword))||(idToken===SyntaxKind.GetKeyword))) {
 return true

}
token() match {
  case  SyntaxKind.OpenParenToken | SyntaxKind.LessThanToken | SyntaxKind.ColonToken | SyntaxKind.EqualsToken | SyntaxKind.QuestionToken  =>
return true
  case _ =>
return canParseSemicolon()
}

}
return false

}
def parseDecorators(): NodeArray[Decorator] = {
 var decorators: NodeArray[Decorator] = zeroOfMyType
while (true) {
{
 val decoratorStart = getNodePos()
if ((!parseOptional( SyntaxKind.AtToken ))) {
 break()

}
val decorator = createNode( SyntaxKind.Decorator, decoratorStart ).asInstanceOf[Decorator]
(decorator.expression=doInDecoratorContext( parseLeftHandSideExpressionOrHigher ))
finishNode( decorator )
if ((!decorators)) {
 (decorators=createNodeArray[ Decorator ]( Array( decorator ), decoratorStart ))

}
else {
 decorators.push( decorator )

}

}
}
if (decorators) {
 (decorators.end=getNodeEnd())

}
return decorators

}
def parseModifiers(permitInvalidConstAsModifier: Boolean): NodeArray[Modifier] = {
 var modifiers: NodeArray[Modifier] = zeroOfMyType
while (true) {
{
 val modifierStart = scanner.getStartPos()
val modifierKind = token()
if (((token()===SyntaxKind.ConstKeyword)&&permitInvalidConstAsModifier)) {
 if ((!tryParse( nextTokenIsOnSameLineAndCanFollowModifier ))) {
 break()

}

}
else {
 if ((!parseAnyContextualModifier())) {
 break()

}

}
val modifier = finishNode( createNode( modifierKind, modifierStart ).asInstanceOf[Modifier] )
if ((!modifiers)) {
 (modifiers=createNodeArray[ Modifier ]( Array( modifier ), modifierStart ))

}
else {
 modifiers.push( modifier )

}

}
}
if (modifiers) {
 (modifiers.end=scanner.getStartPos())

}
return modifiers

}
def parseModifiersForArrowFunction(): NodeArray[Modifier] = {
 var modifiers: NodeArray[Modifier] = zeroOfMyType
if ((token()===SyntaxKind.AsyncKeyword)) {
 val modifierStart = scanner.getStartPos()
val modifierKind = token()
nextToken()
val modifier = finishNode( createNode( modifierKind, modifierStart ).asInstanceOf[Modifier] )
(modifiers=createNodeArray[ Modifier ]( Array( modifier ), modifierStart ))
(modifiers.end=scanner.getStartPos())

}
return modifiers

}
def parseClassElement(): ClassElement = {
 if ((token()===SyntaxKind.SemicolonToken)) {
 val result = createNode( SyntaxKind.SemicolonClassElement ).asInstanceOf[SemicolonClassElement]
nextToken()
return finishNode( result )

}
val fullStart = getNodePos()
val decorators = parseDecorators()
val modifiers = parseModifiers( true )
val accessor = tryParseAccessorDeclaration( fullStart, decorators, modifiers )
if (accessor) {
 return accessor

}
if ((token()===SyntaxKind.ConstructorKeyword)) {
 return parseConstructorDeclaration( fullStart, decorators, modifiers )

}
if (isIndexSignature()) {
 return parseIndexSignatureDeclaration( fullStart, decorators, modifiers )

}
if (((((tokenIsIdentifierOrKeyword( token() )||(token()===SyntaxKind.StringLiteral))||(token()===SyntaxKind.NumericLiteral))||(token()===SyntaxKind.AsteriskToken))||(token()===SyntaxKind.OpenBracketToken))) {
 return parsePropertyOrMethodDeclaration( fullStart, decorators, modifiers )

}
if ((decorators||modifiers)) {
 val name = createMissingNode( SyntaxKind.Identifier, true, Diagnostics.Declaration_expected ).asInstanceOf[Identifier]
return parsePropertyDeclaration( fullStart, decorators, modifiers, name, undefined )

}
Debug.fail( "Should not have attempted to parse class member declaration." )

}
def parseClassExpression(): ClassExpression = {
 return parseClassDeclarationOrExpression( scanner.getStartPos(), undefined, undefined, SyntaxKind.ClassExpression ).asInstanceOf[ClassExpression]

}
def parseClassDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ClassDeclaration = {
 return parseClassDeclarationOrExpression( fullStart, decorators, modifiers, SyntaxKind.ClassDeclaration ).asInstanceOf[ClassDeclaration]

}
def parseClassDeclarationOrExpression(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier], kind: SyntaxKind): ClassLikeDeclaration = {
 val node = createNode( kind, fullStart ).asInstanceOf[ClassLikeDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
parseExpected( SyntaxKind.ClassKeyword )
(node.name=parseNameOfClassDeclarationOrExpression())
(node.typeParameters=parseTypeParameters())
(node.heritageClauses=parseHeritageClauses())
if (parseExpected( SyntaxKind.OpenBraceToken )) {
 (node.members=parseClassMembers())
parseExpected( SyntaxKind.CloseBraceToken )

}
else {
 (node.members=createMissingList[ ClassElement ]())

}
return addJSDocComment( finishNode( node ) )

}
def parseNameOfClassDeclarationOrExpression(): Identifier = {
 return (if ((isIdentifier()&&(!isImplementsClause()))) parseIdentifier() else undefined)

}
def isImplementsClause() = {
 return ((token()===SyntaxKind.ImplementsKeyword)&&lookAhead( nextTokenIsIdentifierOrKeyword ))

}
def parseHeritageClauses(): NodeArray[HeritageClause] = {
 if (isHeritageClause()) {
 return parseList( ParsingContext.HeritageClauses, parseHeritageClause )

}
return undefined

}
def parseHeritageClause() = {
 if (((token()===SyntaxKind.ExtendsKeyword)||(token()===SyntaxKind.ImplementsKeyword))) {
 val node = createNode( SyntaxKind.HeritageClause ).asInstanceOf[HeritageClause]
(node.token=token())
nextToken()
(node.types=parseDelimitedList( ParsingContext.HeritageClauseElement, parseExpressionWithTypeArguments ))
return finishNode( node )

}
return undefined

}
def parseExpressionWithTypeArguments(): ExpressionWithTypeArguments = {
 val node = createNode( SyntaxKind.ExpressionWithTypeArguments ).asInstanceOf[ExpressionWithTypeArguments]
(node.expression=parseLeftHandSideExpressionOrHigher())
if ((token()===SyntaxKind.LessThanToken)) {
 (node.typeArguments=parseBracketedList( ParsingContext.TypeArguments, parseType, SyntaxKind.LessThanToken, SyntaxKind.GreaterThanToken ))

}
return finishNode( node )

}
def isHeritageClause(): Boolean = {
 return ((token()===SyntaxKind.ExtendsKeyword)||(token()===SyntaxKind.ImplementsKeyword))

}
def parseClassMembers() = {
 return parseList( ParsingContext.ClassMembers, parseClassElement )

}
def parseInterfaceDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): InterfaceDeclaration = {
 val node = createNode( SyntaxKind.InterfaceDeclaration, fullStart ).asInstanceOf[InterfaceDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
parseExpected( SyntaxKind.InterfaceKeyword )
(node.name=parseIdentifier())
(node.typeParameters=parseTypeParameters())
(node.heritageClauses=parseHeritageClauses())
(node.members=parseObjectTypeMembers())
return addJSDocComment( finishNode( node ) )

}
def parseTypeAliasDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): TypeAliasDeclaration = {
 val node = createNode( SyntaxKind.TypeAliasDeclaration, fullStart ).asInstanceOf[TypeAliasDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
parseExpected( SyntaxKind.TypeKeyword )
(node.name=parseIdentifier())
(node.typeParameters=parseTypeParameters())
parseExpected( SyntaxKind.EqualsToken )
(node.`type`=parseType())
parseSemicolon()
return addJSDocComment( finishNode( node ) )

}
def parseEnumMember(): EnumMember = {
 val node = createNode( SyntaxKind.EnumMember, scanner.getStartPos() ).asInstanceOf[EnumMember]
(node.name=parsePropertyName())
(node.initializer=allowInAnd( parseNonParameterInitializer ))
return addJSDocComment( finishNode( node ) )

}
def parseEnumDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): EnumDeclaration = {
 val node = createNode( SyntaxKind.EnumDeclaration, fullStart ).asInstanceOf[EnumDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
parseExpected( SyntaxKind.EnumKeyword )
(node.name=parseIdentifier())
if (parseExpected( SyntaxKind.OpenBraceToken )) {
 (node.members=parseDelimitedList( ParsingContext.EnumMembers, parseEnumMember ))
parseExpected( SyntaxKind.CloseBraceToken )

}
else {
 (node.members=createMissingList[ EnumMember ]())

}
return addJSDocComment( finishNode( node ) )

}
def parseModuleBlock(): ModuleBlock = {
 val node = createNode( SyntaxKind.ModuleBlock, scanner.getStartPos() ).asInstanceOf[ModuleBlock]
if (parseExpected( SyntaxKind.OpenBraceToken )) {
 (node.statements=parseList( ParsingContext.BlockStatements, parseStatement ))
parseExpected( SyntaxKind.CloseBraceToken )

}
else {
 (node.statements=createMissingList[ Statement ]())

}
return finishNode( node )

}
def parseModuleOrNamespaceDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier], flags: NodeFlags): ModuleDeclaration = {
 val node = createNode( SyntaxKind.ModuleDeclaration, fullStart ).asInstanceOf[ModuleDeclaration]
val namespaceFlag = (flags&NodeFlags.Namespace)
(node.decorators=decorators)
(node.modifiers=modifiers)
(node.flags|=flags)
(node.name=parseIdentifier())
(node.body=(if (parseOptional( SyntaxKind.DotToken )) parseModuleOrNamespaceDeclaration( getNodePos(), undefined, undefined, (NodeFlags.NestedNamespace|namespaceFlag) ).asInstanceOf[NamespaceDeclaration] else parseModuleBlock()))
return addJSDocComment( finishNode( node ) )

}
def parseAmbientExternalModuleDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ModuleDeclaration = {
 val node = createNode( SyntaxKind.ModuleDeclaration, fullStart ).asInstanceOf[ModuleDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
if ((token()===SyntaxKind.GlobalKeyword)) {
 (node.name=parseIdentifier())
(node.flags|=NodeFlags.GlobalAugmentation)

}
else {
 (node.name=parseLiteralNode( true ))

}
if ((token()===SyntaxKind.OpenBraceToken)) {
 (node.body=parseModuleBlock())

}
else {
 parseSemicolon()

}
return finishNode( node )

}
def parseModuleDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ModuleDeclaration = {
 var flags: NodeFlags = 0
if ((token()===SyntaxKind.GlobalKeyword)) {
 return parseAmbientExternalModuleDeclaration( fullStart, decorators, modifiers )

}
else if (parseOptional( SyntaxKind.NamespaceKeyword )) {
 (flags|=NodeFlags.Namespace)

}
else {
 parseExpected( SyntaxKind.ModuleKeyword )
if ((token()===SyntaxKind.StringLiteral)) {
 return parseAmbientExternalModuleDeclaration( fullStart, decorators, modifiers )

}

}
return parseModuleOrNamespaceDeclaration( fullStart, decorators, modifiers, flags )

}
def isExternalModuleReference() = {
 return ((token()===SyntaxKind.RequireKeyword)&&lookAhead( nextTokenIsOpenParen ))

}
def nextTokenIsOpenParen() = {
 return (nextToken()===SyntaxKind.OpenParenToken)

}
def nextTokenIsSlash() = {
 return (nextToken()===SyntaxKind.SlashToken)

}
def parseNamespaceExportDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): NamespaceExportDeclaration = {
 val exportDeclaration = createNode( SyntaxKind.NamespaceExportDeclaration, fullStart ).asInstanceOf[NamespaceExportDeclaration]
(exportDeclaration.decorators=decorators)
(exportDeclaration.modifiers=modifiers)
parseExpected( SyntaxKind.AsKeyword )
parseExpected( SyntaxKind.NamespaceKeyword )
(exportDeclaration.name=parseIdentifier())
parseSemicolon()
return finishNode( exportDeclaration )

}
def parseImportDeclarationOrImportEqualsDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ( ImportEqualsDeclaration | ImportDeclaration ) = {
 parseExpected( SyntaxKind.ImportKeyword )
val afterImportPos = scanner.getStartPos()
var identifier: Identifier = zeroOfMyType
if (isIdentifier()) {
 (identifier=parseIdentifier())
if (((token()!==SyntaxKind.CommaToken)&&(token()!==SyntaxKind.FromKeyword))) {
 val importEqualsDeclaration = createNode( SyntaxKind.ImportEqualsDeclaration, fullStart ).asInstanceOf[ImportEqualsDeclaration]
(importEqualsDeclaration.decorators=decorators)
(importEqualsDeclaration.modifiers=modifiers)
(importEqualsDeclaration.name=identifier)
parseExpected( SyntaxKind.EqualsToken )
(importEqualsDeclaration.moduleReference=parseModuleReference())
parseSemicolon()
return addJSDocComment( finishNode( importEqualsDeclaration ) )

}

}
val importDeclaration = createNode( SyntaxKind.ImportDeclaration, fullStart ).asInstanceOf[ImportDeclaration]
(importDeclaration.decorators=decorators)
(importDeclaration.modifiers=modifiers)
if (((identifier||(token()===SyntaxKind.AsteriskToken))||(token()===SyntaxKind.OpenBraceToken))) {
 (importDeclaration.importClause=parseImportClause( identifier, afterImportPos ))
parseExpected( SyntaxKind.FromKeyword )

}
(importDeclaration.moduleSpecifier=parseModuleSpecifier())
parseSemicolon()
return finishNode( importDeclaration )

}
def parseImportClause(identifier: Identifier, fullStart: Int) = {
 val importClause = createNode( SyntaxKind.ImportClause, fullStart ).asInstanceOf[ImportClause]
if (identifier) {
 (importClause.name=identifier)

}
if (((!importClause.name)||parseOptional( SyntaxKind.CommaToken ))) {
 (importClause.namedBindings=(if ((token()===SyntaxKind.AsteriskToken)) parseNamespaceImport() else parseNamedImportsOrExports( SyntaxKind.NamedImports )))

}
return finishNode( importClause )

}
def parseModuleReference() = {
 return (if (isExternalModuleReference()) parseExternalModuleReference() else parseEntityName( false ))

}
def parseExternalModuleReference() = {
 val node = createNode( SyntaxKind.ExternalModuleReference ).asInstanceOf[ExternalModuleReference]
parseExpected( SyntaxKind.RequireKeyword )
parseExpected( SyntaxKind.OpenParenToken )
(node.expression=parseModuleSpecifier())
parseExpected( SyntaxKind.CloseParenToken )
return finishNode( node )

}
def parseModuleSpecifier(): Expression = {
 if ((token()===SyntaxKind.StringLiteral)) {
 val result = parseLiteralNode()
internIdentifier( (result.asInstanceOf[LiteralExpression]).text )
return result

}
else {
 return parseExpression()

}

}
def parseNamespaceImport(): NamespaceImport = {
 val namespaceImport = createNode( SyntaxKind.NamespaceImport ).asInstanceOf[NamespaceImport]
parseExpected( SyntaxKind.AsteriskToken )
parseExpected( SyntaxKind.AsKeyword )
(namespaceImport.name=parseIdentifier())
return finishNode( namespaceImport )

}
def parseNamedImportsOrExports(kind: SyntaxKind.NamedImports): NamedImports
def parseNamedImportsOrExports(kind: SyntaxKind.NamedExports): NamedExports
def parseNamedImportsOrExports(kind: SyntaxKind): NamedImportsOrExports = {
 val node = createNode( kind ).asInstanceOf[( NamedImports | NamedExports )]
(node.elements=parseBracketedList( ParsingContext.ImportOrExportSpecifiers, (if ((kind===SyntaxKind.NamedImports)) parseImportSpecifier else parseExportSpecifier), SyntaxKind.OpenBraceToken, SyntaxKind.CloseBraceToken ).asInstanceOf[( NodeArray[ImportSpecifier] | NodeArray[ExportSpecifier] )])
return finishNode( node )

}
def parseExportSpecifier() = {
 return parseImportOrExportSpecifier( SyntaxKind.ExportSpecifier )

}
def parseImportSpecifier() = {
 return parseImportOrExportSpecifier( SyntaxKind.ImportSpecifier )

}
def parseImportOrExportSpecifier(kind: SyntaxKind): ImportOrExportSpecifier = {
 val node = createNode( kind ).asInstanceOf[ImportSpecifier]
var checkIdentifierIsKeyword = (isKeyword( token() )&&(!isIdentifier()))
var checkIdentifierStart = scanner.getTokenPos()
var checkIdentifierEnd = scanner.getTextPos()
val identifierName = parseIdentifierName()
if ((token()===SyntaxKind.AsKeyword)) {
 (node.propertyName=identifierName)
parseExpected( SyntaxKind.AsKeyword )
(checkIdentifierIsKeyword=(isKeyword( token() )&&(!isIdentifier())))
(checkIdentifierStart=scanner.getTokenPos())
(checkIdentifierEnd=scanner.getTextPos())
(node.name=parseIdentifierName())

}
else {
 (node.name=identifierName)

}
if (((kind===SyntaxKind.ImportSpecifier)&&checkIdentifierIsKeyword)) {
 parseErrorAtPosition( checkIdentifierStart, (checkIdentifierEnd-checkIdentifierStart), Diagnostics.Identifier_expected )

}
return finishNode( node )

}
def parseExportDeclaration(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ExportDeclaration = {
 val node = createNode( SyntaxKind.ExportDeclaration, fullStart ).asInstanceOf[ExportDeclaration]
(node.decorators=decorators)
(node.modifiers=modifiers)
if (parseOptional( SyntaxKind.AsteriskToken )) {
 parseExpected( SyntaxKind.FromKeyword )
(node.moduleSpecifier=parseModuleSpecifier())

}
else {
 (node.exportClause=parseNamedImportsOrExports( SyntaxKind.NamedExports ))
if (((token()===SyntaxKind.FromKeyword)||(((token()===SyntaxKind.StringLiteral)&&(!scanner.hasPrecedingLineBreak()))))) {
 parseExpected( SyntaxKind.FromKeyword )
(node.moduleSpecifier=parseModuleSpecifier())

}

}
parseSemicolon()
return finishNode( node )

}
def parseExportAssignment(fullStart: Int, decorators: NodeArray[Decorator], modifiers: NodeArray[Modifier]): ExportAssignment = {
 val node = createNode( SyntaxKind.ExportAssignment, fullStart ).asInstanceOf[ExportAssignment]
(node.decorators=decorators)
(node.modifiers=modifiers)
if (parseOptional( SyntaxKind.EqualsToken )) {
 (node.isExportEquals=true)

}
else {
 parseExpected( SyntaxKind.DefaultKeyword )

}
(node.expression=parseAssignmentExpressionOrHigher())
parseSemicolon()
return finishNode( node )

}
def processReferenceComments(sourceFile: SourceFile): Unit = {
 val triviaScanner = createScanner( sourceFile.languageVersion, false, LanguageVariant.Standard, sourceText )
val referencedFiles: Array[FileReference] = Array()
val typeReferenceDirectives: Array[FileReference] = Array()
val amdDependencies: Array[{   var path: String
  var name: String
 }] = Array()
var amdModuleName: String = zeroOfMyType
while (true) {
{
 val kind = triviaScanner.scan()
if ((kind!==SyntaxKind.SingleLineCommentTrivia)) {
 if (isTrivia( kind )) {
 continue

}
else {
 break()

}

}
val range = Map( "pos" -> triviaScanner.getTokenPos(),
"end" -> triviaScanner.getTextPos(),
"kind" -> triviaScanner.getToken() )
val comment = sourceText.substring( range.pos, range.end )
val referencePathMatchResult = getFileReferenceFromReferencePath( comment, range )
if (referencePathMatchResult) {
 val fileReference = referencePathMatchResult.fileReference
(sourceFile.hasNoDefaultLib=referencePathMatchResult.isNoDefaultLib)
val diagnosticMessage = referencePathMatchResult.diagnosticMessage
if (fileReference) {
 if (referencePathMatchResult.isTypeReferenceDirective) {
 typeReferenceDirectives.push( fileReference )

}
else {
 referencedFiles.push( fileReference )

}

}
if (diagnosticMessage) {
 parseDiagnostics.push( createFileDiagnostic( sourceFile, range.pos, (range.end-range.pos), diagnosticMessage ) )

}

}
else {
 val amdModuleNameRegEx = java.util.regex.Pattern.compile(raw"""^\/\/\/\s*<amd-module\s+name\s*=\s*('|")(.+?)\1""", "gim")
val amdModuleNameMatchResult = amdModuleNameRegEx.exec( comment )
if (amdModuleNameMatchResult) {
 if (amdModuleName) {
 parseDiagnostics.push( createFileDiagnostic( sourceFile, range.pos, (range.end-range.pos), Diagnostics.An_AMD_module_cannot_have_multiple_name_assignments ) )

}
(amdModuleName=amdModuleNameMatchResult(2))

}
val amdDependencyRegEx = java.util.regex.Pattern.compile(raw"""^\/\/\/\s*<amd-dependency\s""", "gim")
val pathRegex = java.util.regex.Pattern.compile(raw"""\spath\s*=\s*('|")(.+?)\1""", "gim")
val nameRegex = java.util.regex.Pattern.compile(raw"""\sname\s*=\s*('|")(.+?)\1""", "gim")
val amdDependencyMatchResult = amdDependencyRegEx.exec( comment )
if (amdDependencyMatchResult) {
 val pathMatchResult = pathRegex.exec( comment )
val nameMatchResult = nameRegex.exec( comment )
if (pathMatchResult) {
 val amdDependency = Map( "path" -> pathMatchResult(2),
"name" -> (if (nameMatchResult) nameMatchResult(2) else undefined) )
amdDependencies.push( amdDependency )

}

}

}

}
}
(sourceFile.referencedFiles=referencedFiles)
(sourceFile.typeReferenceDirectives=typeReferenceDirectives)
(sourceFile.amdDependencies=amdDependencies)
(sourceFile.moduleName=amdModuleName)

}
def setExternalModuleIndicator(sourceFile: SourceFile) = {
 (sourceFile.externalModuleIndicator=forEach( sourceFile.statements, (node =>  (if (((((hasModifier( node, ModifierFlags.Export )||((node.kind===SyntaxKind.ImportEqualsDeclaration)&&((node.asInstanceOf[ImportEqualsDeclaration]).moduleReference.kind===SyntaxKind.ExternalModuleReference)))||(node.kind===SyntaxKind.ImportDeclaration))||(node.kind===SyntaxKind.ExportAssignment))||(node.kind===SyntaxKind.ExportDeclaration))) node else undefined)) ))

}
sealed abstract class ParsingContext
object ParsingContext {
   case object SourceElements extends ParsingContext
  case object BlockStatements extends ParsingContext
  case object SwitchClauses extends ParsingContext
  case object SwitchClauseStatements extends ParsingContext
  case object TypeMembers extends ParsingContext
  case object ClassMembers extends ParsingContext
  case object EnumMembers extends ParsingContext
  case object HeritageClauseElement extends ParsingContext
  case object VariableDeclarations extends ParsingContext
  case object ObjectBindingElements extends ParsingContext
  case object ArrayBindingElements extends ParsingContext
  case object ArgumentExpressions extends ParsingContext
  case object ObjectLiteralMembers extends ParsingContext
  case object JsxAttributes extends ParsingContext
  case object JsxChildren extends ParsingContext
  case object ArrayLiteralMembers extends ParsingContext
  case object Parameters extends ParsingContext
  case object TypeParameters extends ParsingContext
  case object TypeArguments extends ParsingContext
  case object TupleElementTypes extends ParsingContext
  case object HeritageClauses extends ParsingContext
  case object ImportOrExportSpecifiers extends ParsingContext
  case object JSDocFunctionParameters extends ParsingContext
  case object JSDocTypeArguments extends ParsingContext
  case object JSDocRecordMembers extends ParsingContext
  case object JSDocTupleTypes extends ParsingContext
  case object Count extends ParsingContext
}
sealed abstract class Tristate
object Tristate {
   case object False extends Tristate
  case object True extends Tristate
  case object Unknown extends Tristate
}
object JSDocParser {
def isJSDocType() = {
 token() match {
  case  SyntaxKind.AsteriskToken | SyntaxKind.QuestionToken | SyntaxKind.OpenParenToken | SyntaxKind.OpenBracketToken | SyntaxKind.ExclamationToken | SyntaxKind.OpenBraceToken | SyntaxKind.FunctionKeyword | SyntaxKind.DotDotDotToken | SyntaxKind.NewKeyword | SyntaxKind.ThisKeyword  =>
return true
  case _ =>
}
return tokenIsIdentifierOrKeyword( token() )

}
def parseJSDocTypeExpressionForTests(content: String, start: Int, length: Int) = {
 initializeState( content, ScriptTarget.Latest, undefined, ScriptKind.JS )
(sourceFile=createSourceFile( "file.js", ScriptTarget.Latest, ScriptKind.JS ))
scanner.setText( content, start, length )
(currentToken=scanner.scan())
val jsDocTypeExpression = parseJSDocTypeExpression()
val diagnostics = parseDiagnostics
clearState()
return (if (jsDocTypeExpression) Map( "jsDocTypeExpression" -> jsDocTypeExpression,
"diagnostics" -> diagnostics ) else undefined)

}
def parseJSDocTypeExpression(): JSDocTypeExpression = {
 val result = createNode( SyntaxKind.JSDocTypeExpression, scanner.getTokenPos() ).asInstanceOf[JSDocTypeExpression]
parseExpected( SyntaxKind.OpenBraceToken )
(result.`type`=parseJSDocTopLevelType())
parseExpected( SyntaxKind.CloseBraceToken )
fixupParentReferences( result )
return finishNode( result )

}
def parseJSDocTopLevelType(): JSDocType = {
 var `type` = parseJSDocType()
if ((token()===SyntaxKind.BarToken)) {
 val unionType = createNode( SyntaxKind.JSDocUnionType, `type`.pos ).asInstanceOf[JSDocUnionType]
(unionType.types=parseJSDocTypeList( `type` ))
(`type`=finishNode( unionType ))

}
if ((token()===SyntaxKind.EqualsToken)) {
 val optionalType = createNode( SyntaxKind.JSDocOptionalType, `type`.pos ).asInstanceOf[JSDocOptionalType]
nextToken()
(optionalType.`type`=`type`)
(`type`=finishNode( optionalType ))

}
return `type`

}
def parseJSDocType(): JSDocType = {
 var `type` = parseBasicTypeExpression()
while (true) {
{
 if ((token()===SyntaxKind.OpenBracketToken)) {
 val arrayType = createNode( SyntaxKind.JSDocArrayType, `type`.pos ).asInstanceOf[JSDocArrayType]
(arrayType.elementType=`type`)
nextToken()
parseExpected( SyntaxKind.CloseBracketToken )
(`type`=finishNode( arrayType ))

}
else if ((token()===SyntaxKind.QuestionToken)) {
 val nullableType = createNode( SyntaxKind.JSDocNullableType, `type`.pos ).asInstanceOf[JSDocNullableType]
(nullableType.`type`=`type`)
nextToken()
(`type`=finishNode( nullableType ))

}
else if ((token()===SyntaxKind.ExclamationToken)) {
 val nonNullableType = createNode( SyntaxKind.JSDocNonNullableType, `type`.pos ).asInstanceOf[JSDocNonNullableType]
(nonNullableType.`type`=`type`)
nextToken()
(`type`=finishNode( nonNullableType ))

}
else {
 break()

}

}
}
return `type`

}
def parseBasicTypeExpression(): JSDocType = {
 token() match {
  case  SyntaxKind.AsteriskToken  =>
return parseJSDocAllType()
  case  SyntaxKind.QuestionToken  =>
return parseJSDocUnknownOrNullableType()
  case  SyntaxKind.OpenParenToken  =>
return parseJSDocUnionType()
  case  SyntaxKind.OpenBracketToken  =>
return parseJSDocTupleType()
  case  SyntaxKind.ExclamationToken  =>
return parseJSDocNonNullableType()
  case  SyntaxKind.OpenBraceToken  =>
return parseJSDocRecordType()
  case  SyntaxKind.FunctionKeyword  =>
return parseJSDocFunctionType()
  case  SyntaxKind.DotDotDotToken  =>
return parseJSDocVariadicType()
  case  SyntaxKind.NewKeyword  =>
return parseJSDocConstructorType()
  case  SyntaxKind.ThisKeyword  =>
return parseJSDocThisType()
  case  SyntaxKind.AnyKeyword | SyntaxKind.StringKeyword | SyntaxKind.NumberKeyword | SyntaxKind.BooleanKeyword | SyntaxKind.SymbolKeyword | SyntaxKind.VoidKeyword | SyntaxKind.NullKeyword | SyntaxKind.UndefinedKeyword | SyntaxKind.NeverKeyword  =>
return parseTokenNode[ JSDocType ]()
  case  SyntaxKind.StringLiteral | SyntaxKind.NumericLiteral | SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword  =>
return parseJSDocLiteralType()
  case _ =>
}
return parseJSDocTypeReference()

}
def parseJSDocThisType(): JSDocThisType = {
 val result = createNode( SyntaxKind.JSDocThisType ).asInstanceOf[JSDocThisType]
nextToken()
parseExpected( SyntaxKind.ColonToken )
(result.`type`=parseJSDocType())
return finishNode( result )

}
def parseJSDocConstructorType(): JSDocConstructorType = {
 val result = createNode( SyntaxKind.JSDocConstructorType ).asInstanceOf[JSDocConstructorType]
nextToken()
parseExpected( SyntaxKind.ColonToken )
(result.`type`=parseJSDocType())
return finishNode( result )

}
def parseJSDocVariadicType(): JSDocVariadicType = {
 val result = createNode( SyntaxKind.JSDocVariadicType ).asInstanceOf[JSDocVariadicType]
nextToken()
(result.`type`=parseJSDocType())
return finishNode( result )

}
def parseJSDocFunctionType(): JSDocFunctionType = {
 val result = createNode( SyntaxKind.JSDocFunctionType ).asInstanceOf[JSDocFunctionType]
nextToken()
parseExpected( SyntaxKind.OpenParenToken )
(result.parameters=parseDelimitedList( ParsingContext.JSDocFunctionParameters, parseJSDocParameter ))
checkForTrailingComma( result.parameters )
parseExpected( SyntaxKind.CloseParenToken )
if ((token()===SyntaxKind.ColonToken)) {
 nextToken()
(result.`type`=parseJSDocType())

}
return finishNode( result )

}
def parseJSDocParameter(): ParameterDeclaration = {
 val parameter = createNode( SyntaxKind.Parameter ).asInstanceOf[ParameterDeclaration]
(parameter.`type`=parseJSDocType())
if (parseOptional( SyntaxKind.EqualsToken )) {
 (parameter.questionToken=createNode( SyntaxKind.EqualsToken ).asInstanceOf[QuestionToken])

}
return finishNode( parameter )

}
def parseJSDocTypeReference(): JSDocTypeReference = {
 val result = createNode( SyntaxKind.JSDocTypeReference ).asInstanceOf[JSDocTypeReference]
(result.name=parseSimplePropertyName().asInstanceOf[Identifier])
if ((token()===SyntaxKind.LessThanToken)) {
 (result.typeArguments=parseTypeArguments())

}
else {
 while (parseOptional( SyntaxKind.DotToken )) {
{
 if ((token()===SyntaxKind.LessThanToken)) {
 (result.typeArguments=parseTypeArguments())
break()

}
else {
 (result.name=parseQualifiedName( result.name ))

}

}
}

}
return finishNode( result )

}
def parseTypeArguments() = {
 nextToken()
val typeArguments = parseDelimitedList( ParsingContext.JSDocTypeArguments, parseJSDocType )
checkForTrailingComma( typeArguments )
checkForEmptyTypeArgumentList( typeArguments )
parseExpected( SyntaxKind.GreaterThanToken )
return typeArguments

}
def checkForEmptyTypeArgumentList(typeArguments: NodeArray[Node]) = {
 if ((((parseDiagnostics.length===0)&&typeArguments)&&(typeArguments.length===0))) {
 val start = (typeArguments.pos-"<".length)
val end = (skipTrivia( sourceText, typeArguments.end )+">".length)
return parseErrorAtPosition( start, (end-start), Diagnostics.Type_argument_list_cannot_be_empty )

}

}
def parseQualifiedName(left: EntityName): QualifiedName = {
 val result = createNode( SyntaxKind.QualifiedName, left.pos ).asInstanceOf[QualifiedName]
(result.left=left)
(result.right=parseIdentifierName())
return finishNode( result )

}
def parseJSDocRecordType(): JSDocRecordType = {
 val result = createNode( SyntaxKind.JSDocRecordType ).asInstanceOf[JSDocRecordType]
(result.literal=parseTypeLiteral())
return finishNode( result )

}
def parseJSDocNonNullableType(): JSDocNonNullableType = {
 val result = createNode( SyntaxKind.JSDocNonNullableType ).asInstanceOf[JSDocNonNullableType]
nextToken()
(result.`type`=parseJSDocType())
return finishNode( result )

}
def parseJSDocTupleType(): JSDocTupleType = {
 val result = createNode( SyntaxKind.JSDocTupleType ).asInstanceOf[JSDocTupleType]
nextToken()
(result.types=parseDelimitedList( ParsingContext.JSDocTupleTypes, parseJSDocType ))
checkForTrailingComma( result.types )
parseExpected( SyntaxKind.CloseBracketToken )
return finishNode( result )

}
def checkForTrailingComma(list: NodeArray[Node]) = {
 if (((parseDiagnostics.length===0)&&list.hasTrailingComma)) {
 val start = (list.end-",".length)
parseErrorAtPosition( start, ",".length, Diagnostics.Trailing_comma_not_allowed )

}

}
def parseJSDocUnionType(): JSDocUnionType = {
 val result = createNode( SyntaxKind.JSDocUnionType ).asInstanceOf[JSDocUnionType]
nextToken()
(result.types=parseJSDocTypeList( parseJSDocType() ))
parseExpected( SyntaxKind.CloseParenToken )
return finishNode( result )

}
def parseJSDocTypeList(firstType: JSDocType) = {
 Debug.assert( (!(!firstType)) )
val types = createNodeArray( Array( firstType ), firstType.pos )
while (parseOptional( SyntaxKind.BarToken )) {
{
 types.push( parseJSDocType() )

}
}
(types.end=scanner.getStartPos())
return types

}
def parseJSDocAllType(): JSDocAllType = {
 val result = createNode( SyntaxKind.JSDocAllType ).asInstanceOf[JSDocAllType]
nextToken()
return finishNode( result )

}
def parseJSDocLiteralType(): JSDocLiteralType = {
 val result = createNode( SyntaxKind.JSDocLiteralType ).asInstanceOf[JSDocLiteralType]
(result.literal=parseLiteralTypeNode())
return finishNode( result )

}
def parseJSDocUnknownOrNullableType(): ( JSDocUnknownType | JSDocNullableType ) = {
 val pos = scanner.getStartPos()
nextToken()
if (((((((token()===SyntaxKind.CommaToken)||(token()===SyntaxKind.CloseBraceToken))||(token()===SyntaxKind.CloseParenToken))||(token()===SyntaxKind.GreaterThanToken))||(token()===SyntaxKind.EqualsToken))||(token()===SyntaxKind.BarToken))) {
 val result = createNode( SyntaxKind.JSDocUnknownType, pos ).asInstanceOf[JSDocUnknownType]
return finishNode( result )

}
else {
 val result = createNode( SyntaxKind.JSDocNullableType, pos ).asInstanceOf[JSDocNullableType]
(result.`type`=parseJSDocType())
return finishNode( result )

}

}
def parseIsolatedJSDocComment(content: String, start: Int, length: Int) = {
 initializeState( content, ScriptTarget.Latest, undefined, ScriptKind.JS )
(sourceFile=Map( "languageVariant" -> LanguageVariant.Standard,
"text" -> content ).asInstanceOf[SourceFile])
val jsDoc = parseJSDocCommentWorker( start, length )
val diagnostics = parseDiagnostics
clearState()
return (if (jsDoc) Map( "jsDoc" -> jsDoc,
"diagnostics" -> diagnostics ) else undefined)

}
def parseJSDocComment(parent: Node, start: Int, length: Int): JSDoc = {
 val saveToken = currentToken
val saveParseDiagnosticsLength = parseDiagnostics.length
val saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode
val comment = parseJSDocCommentWorker( start, length )
if (comment) {
 (comment.parent=parent)

}
(currentToken=saveToken)
(parseDiagnostics.length=saveParseDiagnosticsLength)
(parseErrorBeforeNextFinishedNode=saveParseErrorBeforeNextFinishedNode)
return comment

}
sealed abstract class JSDocState
object JSDocState {
   case object BeginningOfLine extends JSDocState
  case object SawAsterisk extends JSDocState
  case object SavingComments extends JSDocState
}
def parseJSDocCommentWorker(start: Int, length: Int): JSDoc = {
 val content = sourceText
(start=(start||0))
val end = (if ((length===undefined)) content.length else (start+length))
(length=(end-start))
Debug.assert( (start>=0) )
Debug.assert( (start<=end) )
Debug.assert( (end<=content.length) )
var tags: NodeArray[JSDocTag] = zeroOfMyType
val comments: Array[String] = Array()
var result: JSDoc = zeroOfMyType
if ((!isJsDocStart( content, start ))) {
 return result

}
scanner.scanRange( (start+3), (length-5), (() =>  {
 var advanceToken = true
 var state = JSDocState.SawAsterisk
 var margin: ( Int | undefined ) = undefined
 var indent = ((start-Math.max( content.lastIndexOf( "\n", start ), 0 ))+4)
 def pushComment(text: String) = {
 if ((!margin)) {
 (margin=indent)

}
comments.push( text )
(indent+=text.length)

}
 nextJSDocToken()
 while ((token()===SyntaxKind.WhitespaceTrivia)) {
{
 nextJSDocToken()

}
}
 if ((token()===SyntaxKind.NewLineTrivia)) {
 (state=JSDocState.BeginningOfLine)
nextJSDocToken()

}
 while ((token()!==SyntaxKind.EndOfFileToken)) {
{
 token() match {
  case  SyntaxKind.AtToken  =>
if (((state===JSDocState.BeginningOfLine)||(state===JSDocState.SawAsterisk))) {
 removeTrailingNewlines( comments )
parseTag( indent )
(state=JSDocState.BeginningOfLine)
(advanceToken=false)
(margin=undefined)
(indent+= 1)

}
else {
 pushComment( scanner.getTokenText() )

}
  case  SyntaxKind.NewLineTrivia  =>
comments.push( scanner.getTokenText() )
(state=JSDocState.BeginningOfLine)
(indent=0)
  case  SyntaxKind.AsteriskToken  =>
val asterisk = scanner.getTokenText()
if ((state===JSDocState.SawAsterisk)) {
 (state=JSDocState.SavingComments)
pushComment( asterisk )

}
else {
 (state=JSDocState.SawAsterisk)
(indent+=asterisk.length)

}
  case  SyntaxKind.Identifier  =>
pushComment( scanner.getTokenText() )
(state=JSDocState.SavingComments)
  case  SyntaxKind.WhitespaceTrivia  =>
val whitespace = scanner.getTokenText()
if (((state===JSDocState.SavingComments)||((margin!==undefined)&&((indent+whitespace.length)>margin)))) {
 comments.push( whitespace.slice( ((margin-indent)-1) ) )

}
(indent+=whitespace.length)
  case  SyntaxKind.EndOfFileToken  =>
  case _ =>
pushComment( scanner.getTokenText() )
}
if (advanceToken) {
 nextJSDocToken()

}
else {
 (advanceToken=true)

}

}
}
 removeLeadingNewlines( comments )
 removeTrailingNewlines( comments )
 (result=createJSDocComment())

}) )
return result
def removeLeadingNewlines(comments: Array[String]) = {
 while ((comments.length&&(((comments(0)==="\n")||(comments(0)==="\r"))))) {
{
 comments.shift()

}
}

}
def removeTrailingNewlines(comments: Array[String]) = {
 while ((comments.length&&(((comments((comments.length-1))==="\n")||(comments((comments.length-1))==="\r"))))) {
{
 comments.pop()

}
}

}
def isJsDocStart(content: String, start: Int) = {
 return ((((content.charCodeAt( start )===CharacterCodes.slash)&&(content.charCodeAt( (start+1) )===CharacterCodes.asterisk))&&(content.charCodeAt( (start+2) )===CharacterCodes.asterisk))&&(content.charCodeAt( (start+3) )!==CharacterCodes.asterisk))

}
def createJSDocComment(): JSDoc = {
 val result = createNode( SyntaxKind.JSDocComment, start ).asInstanceOf[JSDoc]
(result.tags=tags)
(result.comment=(if (comments.length) comments.join( "" ) else undefined))
return finishNode( result, end )

}
def skipWhitespace(): Unit = {
 while (((token()===SyntaxKind.WhitespaceTrivia)||(token()===SyntaxKind.NewLineTrivia))) {
{
 nextJSDocToken()

}
}

}
def parseTag(indent: Int) = {
 Debug.assert( (token()===SyntaxKind.AtToken) )
val atToken = createNode( SyntaxKind.AtToken, scanner.getTokenPos() ).asInstanceOf[AtToken]
(atToken.end=scanner.getTextPos())
nextJSDocToken()
val tagName = parseJSDocIdentifierName()
skipWhitespace()
if ((!tagName)) {
 return

}
var tag: JSDocTag = zeroOfMyType
if (tagName) {
 tagName.text match {
  case  "param"  =>
(tag=parseParamTag( atToken, tagName ))
  case  "return" | "returns"  =>
(tag=parseReturnTag( atToken, tagName ))
  case  "template"  =>
(tag=parseTemplateTag( atToken, tagName ))
  case  "type"  =>
(tag=parseTypeTag( atToken, tagName ))
  case  "typedef"  =>
(tag=parseTypedefTag( atToken, tagName ))
  case _ =>
(tag=parseUnknownTag( atToken, tagName ))
}

}
else {
 (tag=parseUnknownTag( atToken, tagName ))

}
if ((!tag)) {
 return

}
addTag( tag, parseTagComments( ((indent+tag.end)-tag.pos) ) )

}
def parseTagComments(indent: Int) = {
 val comments: Array[String] = Array()
var state = JSDocState.SawAsterisk
var margin: ( Int | undefined ) = zeroOfMyType
def pushComment(text: String) = {
 if ((!margin)) {
 (margin=indent)

}
comments.push( text )
(indent+=text.length)

}
while (((token()!==SyntaxKind.AtToken)&&(token()!==SyntaxKind.EndOfFileToken))) {
{
 token() match {
  case  SyntaxKind.NewLineTrivia  =>
if ((state>=JSDocState.SawAsterisk)) {
 (state=JSDocState.BeginningOfLine)
comments.push( scanner.getTokenText() )

}
(indent=0)
  case  SyntaxKind.AtToken  =>
  case  SyntaxKind.WhitespaceTrivia  =>
if ((state===JSDocState.SavingComments)) {
 pushComment( scanner.getTokenText() )

}
else {
 val whitespace = scanner.getTokenText()
if (((margin!==undefined)&&((indent+whitespace.length)>margin))) {
 comments.push( whitespace.slice( ((margin-indent)-1) ) )

}
(indent+=whitespace.length)

}
  case  SyntaxKind.AsteriskToken  =>
if ((state===JSDocState.BeginningOfLine)) {
 (state=JSDocState.SawAsterisk)
(indent+=scanner.getTokenText().length)
break()

}
(state=JSDocState.SavingComments)
pushComment( scanner.getTokenText() )
  case _ =>
(state=JSDocState.SavingComments)
pushComment( scanner.getTokenText() )
}
if ((token()===SyntaxKind.AtToken)) {
 break()

}
nextJSDocToken()

}
}
removeLeadingNewlines( comments )
removeTrailingNewlines( comments )
return comments

}
def parseUnknownTag(atToken: AtToken, tagName: Identifier) = {
 val result = createNode( SyntaxKind.JSDocTag, atToken.pos ).asInstanceOf[JSDocTag]
(result.atToken=atToken)
(result.tagName=tagName)
return finishNode( result )

}
def addTag(tag: JSDocTag, comments: Array[String]): Unit = {
 (tag.comment=comments.join( "" ))
if ((!tags)) {
 (tags=createNodeArray( Array( tag ), tag.pos ))

}
else {
 tags.push( tag )

}
(tags.end=tag.end)

}
def tryParseTypeExpression(): JSDocTypeExpression = {
 return tryParse( (() =>  {
 skipWhitespace()
 if ((token()!==SyntaxKind.OpenBraceToken)) {
 return undefined

}
 return parseJSDocTypeExpression()

}) )

}
def parseParamTag(atToken: AtToken, tagName: Identifier) = {
 var typeExpression = tryParseTypeExpression()
skipWhitespace()
var name: Identifier = zeroOfMyType
var isBracketed: Boolean = zeroOfMyType
if (parseOptionalToken( SyntaxKind.OpenBracketToken )) {
 (name=parseJSDocIdentifierName())
skipWhitespace()
(isBracketed=true)
if (parseOptionalToken( SyntaxKind.EqualsToken )) {
 parseExpression()

}
parseExpected( SyntaxKind.CloseBracketToken )

}
else if (tokenIsIdentifierOrKeyword( token() )) {
 (name=parseJSDocIdentifierName())

}
if ((!name)) {
 parseErrorAtPosition( scanner.getStartPos(), 0, Diagnostics.Identifier_expected )
return undefined

}
var preName: Identifier = zeroOfMyType
var postName: Identifier = zeroOfMyType
if (typeExpression) {
 (postName=name)

}
else {
 (preName=name)

}
if ((!typeExpression)) {
 (typeExpression=tryParseTypeExpression())

}
val result = createNode( SyntaxKind.JSDocParameterTag, atToken.pos ).asInstanceOf[JSDocParameterTag]
(result.atToken=atToken)
(result.tagName=tagName)
(result.preParameterName=preName)
(result.typeExpression=typeExpression)
(result.postParameterName=postName)
(result.parameterName=(postName||preName))
(result.isBracketed=isBracketed)
return finishNode( result )

}
def parseReturnTag(atToken: AtToken, tagName: Identifier): JSDocReturnTag = {
 if (forEach( tags, (t =>  (t.kind===SyntaxKind.JSDocReturnTag)) )) {
 parseErrorAtPosition( tagName.pos, (scanner.getTokenPos()-tagName.pos), Diagnostics._0_tag_already_specified, tagName.text )

}
val result = createNode( SyntaxKind.JSDocReturnTag, atToken.pos ).asInstanceOf[JSDocReturnTag]
(result.atToken=atToken)
(result.tagName=tagName)
(result.typeExpression=tryParseTypeExpression())
return finishNode( result )

}
def parseTypeTag(atToken: AtToken, tagName: Identifier): JSDocTypeTag = {
 if (forEach( tags, (t =>  (t.kind===SyntaxKind.JSDocTypeTag)) )) {
 parseErrorAtPosition( tagName.pos, (scanner.getTokenPos()-tagName.pos), Diagnostics._0_tag_already_specified, tagName.text )

}
val result = createNode( SyntaxKind.JSDocTypeTag, atToken.pos ).asInstanceOf[JSDocTypeTag]
(result.atToken=atToken)
(result.tagName=tagName)
(result.typeExpression=tryParseTypeExpression())
return finishNode( result )

}
def parsePropertyTag(atToken: AtToken, tagName: Identifier): JSDocPropertyTag = {
 val typeExpression = tryParseTypeExpression()
skipWhitespace()
val name = parseJSDocIdentifierName()
skipWhitespace()
if ((!name)) {
 parseErrorAtPosition( scanner.getStartPos(), 0, Diagnostics.Identifier_expected )
return undefined

}
val result = createNode( SyntaxKind.JSDocPropertyTag, atToken.pos ).asInstanceOf[JSDocPropertyTag]
(result.atToken=atToken)
(result.tagName=tagName)
(result.name=name)
(result.typeExpression=typeExpression)
return finishNode( result )

}
def parseTypedefTag(atToken: AtToken, tagName: Identifier): JSDocTypedefTag = {
 val typeExpression = tryParseTypeExpression()
skipWhitespace()
val typedefTag = createNode( SyntaxKind.JSDocTypedefTag, atToken.pos ).asInstanceOf[JSDocTypedefTag]
(typedefTag.atToken=atToken)
(typedefTag.tagName=tagName)
(typedefTag.fullName=parseJSDocTypeNameWithNamespace( 0 ))
if (typedefTag.fullName) {
 var rightNode = typedefTag.fullName
while ((rightNode.kind!==SyntaxKind.Identifier)) {
{
 (rightNode=rightNode.body)

}
}
(typedefTag.name=rightNode)

}
(typedefTag.typeExpression=typeExpression)
skipWhitespace()
if (typeExpression) {
 if ((typeExpression.`type`.kind===SyntaxKind.JSDocTypeReference)) {
 val jsDocTypeReference = typeExpression.`type`.asInstanceOf[JSDocTypeReference]
if ((jsDocTypeReference.name.kind===SyntaxKind.Identifier)) {
 val name = jsDocTypeReference.name.asInstanceOf[Identifier]
if ((name.text==="Object")) {
 (typedefTag.jsDocTypeLiteral=scanChildTags())

}

}

}
if ((!typedefTag.jsDocTypeLiteral)) {
 (typedefTag.jsDocTypeLiteral=typeExpression.`type`.asInstanceOf[JSDocTypeLiteral])

}

}
else {
 (typedefTag.jsDocTypeLiteral=scanChildTags())

}
return finishNode( typedefTag )
def scanChildTags(): JSDocTypeLiteral = {
 val jsDocTypeLiteral = createNode( SyntaxKind.JSDocTypeLiteral, scanner.getStartPos() ).asInstanceOf[JSDocTypeLiteral]
var resumePos = scanner.getStartPos()
var canParseTag = true
var seenAsterisk = false
var parentTagTerminated = false
while (((token()!==SyntaxKind.EndOfFileToken)&&(!parentTagTerminated))) {
{
 nextJSDocToken()
token() match {
  case  SyntaxKind.AtToken  =>
if (canParseTag) {
 (parentTagTerminated=(!tryParseChildTag( jsDocTypeLiteral )))
if ((!parentTagTerminated)) {
 (resumePos=scanner.getStartPos())

}

}
(seenAsterisk=false)
  case  SyntaxKind.NewLineTrivia  =>
(resumePos=(scanner.getStartPos()-1))
(canParseTag=true)
(seenAsterisk=false)
  case  SyntaxKind.AsteriskToken  =>
if (seenAsterisk) {
 (canParseTag=false)

}
(seenAsterisk=true)
  case  SyntaxKind.Identifier  =>
(canParseTag=false)
  case  SyntaxKind.EndOfFileToken  =>
  case _ =>
}

}
}
scanner.setTextPos( resumePos )
return finishNode( jsDocTypeLiteral )

}
def parseJSDocTypeNameWithNamespace(flags: NodeFlags) = {
 val pos = scanner.getTokenPos()
val typeNameOrNamespaceName = parseJSDocIdentifierName()
if ((typeNameOrNamespaceName&&parseOptional( SyntaxKind.DotToken ))) {
 val jsDocNamespaceNode = createNode( SyntaxKind.ModuleDeclaration, pos ).asInstanceOf[JSDocNamespaceDeclaration]
(jsDocNamespaceNode.flags|=flags)
(jsDocNamespaceNode.name=typeNameOrNamespaceName)
(jsDocNamespaceNode.body=parseJSDocTypeNameWithNamespace( NodeFlags.NestedNamespace ))
return jsDocNamespaceNode

}
if ((typeNameOrNamespaceName&&(flags&NodeFlags.NestedNamespace))) {
 (typeNameOrNamespaceName.isInJSDocNamespace=true)

}
return typeNameOrNamespaceName

}

}
def tryParseChildTag(parentTag: JSDocTypeLiteral): Boolean = {
 Debug.assert( (token()===SyntaxKind.AtToken) )
val atToken = createNode( SyntaxKind.AtToken, scanner.getStartPos() ).asInstanceOf[AtToken]
(atToken.end=scanner.getTextPos())
nextJSDocToken()
val tagName = parseJSDocIdentifierName()
skipWhitespace()
if ((!tagName)) {
 return false

}
tagName.text match {
  case  "type"  =>
if (parentTag.jsDocTypeTag) {
 return false

}
(parentTag.jsDocTypeTag=parseTypeTag( atToken, tagName ))
return true
  case  "prop" | "property"  =>
val propertyTag = parsePropertyTag( atToken, tagName )
if (propertyTag) {
 if ((!parentTag.jsDocPropertyTags)) {
 (parentTag.jsDocPropertyTags=Array().asInstanceOf[NodeArray[JSDocPropertyTag]])

}
parentTag.jsDocPropertyTags.push( propertyTag )
return true

}
return false
  case _ =>
}
return false

}
def parseTemplateTag(atToken: AtToken, tagName: Identifier): JSDocTemplateTag = {
 if (forEach( tags, (t =>  (t.kind===SyntaxKind.JSDocTemplateTag)) )) {
 parseErrorAtPosition( tagName.pos, (scanner.getTokenPos()-tagName.pos), Diagnostics._0_tag_already_specified, tagName.text )

}
val typeParameters = createNodeArray[ TypeParameterDeclaration ]()
while (true) {
{
 val name = parseJSDocIdentifierName()
skipWhitespace()
if ((!name)) {
 parseErrorAtPosition( scanner.getStartPos(), 0, Diagnostics.Identifier_expected )
return undefined

}
val typeParameter = createNode( SyntaxKind.TypeParameter, name.pos ).asInstanceOf[TypeParameterDeclaration]
(typeParameter.name=name)
finishNode( typeParameter )
typeParameters.push( typeParameter )
if ((token()===SyntaxKind.CommaToken)) {
 nextJSDocToken()
skipWhitespace()

}
else {
 break()

}

}
}
val result = createNode( SyntaxKind.JSDocTemplateTag, atToken.pos ).asInstanceOf[JSDocTemplateTag]
(result.atToken=atToken)
(result.tagName=tagName)
(result.typeParameters=typeParameters)
finishNode( result )
(typeParameters.end=result.end)
return result

}
def nextJSDocToken(): SyntaxKind = {
 return (currentToken=scanner.scanJSDocToken())

}
def parseJSDocIdentifierName(): Identifier = {
 return createJSDocIdentifier( tokenIsIdentifierOrKeyword( token() ) )

}
def createJSDocIdentifier(isIdentifier: Boolean): Identifier = {
 if ((!isIdentifier)) {
 parseErrorAtCurrentToken( Diagnostics.Identifier_expected )
return undefined

}
val pos = scanner.getTokenPos()
val end = scanner.getTextPos()
val result = createNode( SyntaxKind.Identifier, pos ).asInstanceOf[Identifier]
(result.text=content.substring( pos, end ))
finishNode( result, end )
nextJSDocToken()
return result

}

}
}
}
object IncrementalParser {
def updateSourceFile(sourceFile: SourceFile, newText: String, textChangeRange: TextChangeRange, aggressiveChecks: Boolean): SourceFile = {
 (aggressiveChecks=(aggressiveChecks||Debug.shouldAssert( AssertionLevel.Aggressive )))
checkChangeRange( sourceFile, newText, textChangeRange, aggressiveChecks )
if (textChangeRangeIsUnchanged( textChangeRange )) {
 return sourceFile

}
if ((sourceFile.statements.length===0)) {
 return Parser.parseSourceFile( sourceFile.fileName, newText, sourceFile.languageVersion, undefined, true, sourceFile.scriptKind )

}
val incrementalSourceFile = sourceFile.asInstanceOf[Node].asInstanceOf[IncrementalNode]
Debug.assert( (!incrementalSourceFile.hasBeenIncrementallyParsed) )
(incrementalSourceFile.hasBeenIncrementallyParsed=true)
val oldText = sourceFile.text
val syntaxCursor = createSyntaxCursor( sourceFile )
val changeRange = extendToAffectedRange( sourceFile, textChangeRange )
checkChangeRange( sourceFile, newText, changeRange, aggressiveChecks )
Debug.assert( (changeRange.span.start<=textChangeRange.span.start) )
Debug.assert( (textSpanEnd( changeRange.span )===textSpanEnd( textChangeRange.span )) )
Debug.assert( (textSpanEnd( textChangeRangeNewSpan( changeRange ) )===textSpanEnd( textChangeRangeNewSpan( textChangeRange ) )) )
val delta = (textChangeRangeNewSpan( changeRange ).length-changeRange.span.length)
updateTokenPositionsAndMarkElements( incrementalSourceFile, changeRange.span.start, textSpanEnd( changeRange.span ), textSpanEnd( textChangeRangeNewSpan( changeRange ) ), delta, oldText, newText, aggressiveChecks )
val result = Parser.parseSourceFile( sourceFile.fileName, newText, sourceFile.languageVersion, syntaxCursor, true, sourceFile.scriptKind )
return result

}
def moveElementEntirelyPastChangeRange(element: IncrementalElement, isArray: Boolean, delta: Int, oldText: String, newText: String, aggressiveChecks: Boolean) = {
 if (isArray) {
 visitArray( element.asInstanceOf[IncrementalNodeArray] )

}
else {
 visitNode( element.asInstanceOf[IncrementalNode] )

}
return
def visitNode(node: IncrementalNode) = {
 var text = ""
if ((aggressiveChecks&&shouldCheckNode( node ))) {
 (text=oldText.substring( node.pos, node.end ))

}
if (node._children) {
 (node._children=undefined)

}
(node.pos+=delta)
(node.end+=delta)
if ((aggressiveChecks&&shouldCheckNode( node ))) {
 Debug.assert( (text===newText.substring( node.pos, node.end )) )

}
forEachChild( node, visitNode, visitArray )
if (node.jsDocComments) {
 (node.jsDocComments).foreach { fresh4 =>
val jsDocComment = zeroOfMyType
 = fresh4
 {
 forEachChild( jsDocComment, visitNode, visitArray )

}
}

}
checkNodePositions( node, aggressiveChecks )

}
def visitArray(array: IncrementalNodeArray) = {
 (array._children=undefined)
(array.pos+=delta)
(array.end+=delta)
(array).foreach { fresh5 =>
val node = zeroOfMyType
 = fresh5
 {
 visitNode( node )

}
}

}

}
def shouldCheckNode(node: Node) = {
 node.kind match {
  case  SyntaxKind.StringLiteral | SyntaxKind.NumericLiteral | SyntaxKind.Identifier  =>
return true
  case _ =>
}
return false

}
def adjustIntersectingElement(element: IncrementalElement, changeStart: Int, changeRangeOldEnd: Int, changeRangeNewEnd: Int, delta: Int) = {
 Debug.assert( (element.end>=changeStart), "Adjusting an element that was entirely before the change range" )
Debug.assert( (element.pos<=changeRangeOldEnd), "Adjusting an element that was entirely after the change range" )
Debug.assert( (element.pos<=element.end) )
(element.pos=Math.min( element.pos, changeRangeNewEnd ))
if ((element.end>=changeRangeOldEnd)) {
 (element.end+=delta)

}
else {
 (element.end=Math.min( element.end, changeRangeNewEnd ))

}
Debug.assert( (element.pos<=element.end) )
if (element.parent) {
 Debug.assert( (element.pos>=element.parent.pos) )
Debug.assert( (element.end<=element.parent.end) )

}

}
def checkNodePositions(node: Node, aggressiveChecks: Boolean) = {
 if (aggressiveChecks) {
 var pos = node.pos
forEachChild( node, (child =>  {
 Debug.assert( (child.pos>=pos) )
 (pos=child.end)

}) )
Debug.assert( (pos<=node.end) )

}

}
def updateTokenPositionsAndMarkElements(sourceFile: IncrementalNode, changeStart: Int, changeRangeOldEnd: Int, changeRangeNewEnd: Int, delta: Int, oldText: String, newText: String, aggressiveChecks: Boolean): Unit = {
 visitNode( sourceFile )
return
def visitNode(child: IncrementalNode) = {
 Debug.assert( (child.pos<=child.end) )
if ((child.pos>changeRangeOldEnd)) {
 moveElementEntirelyPastChangeRange( child, false, delta, oldText, newText, aggressiveChecks )
return

}
val fullEnd = child.end
if ((fullEnd>=changeStart)) {
 (child.intersectsChange=true)
(child._children=undefined)
adjustIntersectingElement( child, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta )
forEachChild( child, visitNode, visitArray )
checkNodePositions( child, aggressiveChecks )
return

}
Debug.assert( (fullEnd<changeStart) )

}
def visitArray(array: IncrementalNodeArray) = {
 Debug.assert( (array.pos<=array.end) )
if ((array.pos>changeRangeOldEnd)) {
 moveElementEntirelyPastChangeRange( array, true, delta, oldText, newText, aggressiveChecks )
return

}
val fullEnd = array.end
if ((fullEnd>=changeStart)) {
 (array.intersectsChange=true)
(array._children=undefined)
adjustIntersectingElement( array, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta )
(array).foreach { fresh6 =>
val node = zeroOfMyType
 = fresh6
 {
 visitNode( node )

}
}
return

}
Debug.assert( (fullEnd<changeStart) )

}

}
def extendToAffectedRange(sourceFile: SourceFile, changeRange: TextChangeRange): TextChangeRange = {
 val maxLookahead = 1
var start = changeRange.span.start
{
var i = 0
while( ((start>0)&&(i<=maxLookahead))) {
 {
 val nearestNode = findNearestNodeStartingBeforeOrAtPosition( sourceFile, start )
Debug.assert( (nearestNode.pos<=start) )
val position = nearestNode.pos
(start=Math.max( 0, (position-1) ))

}
 (i+= 1)
}
}
val finalSpan = createTextSpanFromBounds( start, textSpanEnd( changeRange.span ) )
val finalLength = (changeRange.newLength+((changeRange.span.start-start)))
return createTextChangeRange( finalSpan, finalLength )

}
def findNearestNodeStartingBeforeOrAtPosition(sourceFile: SourceFile, position: Int): Node = {
 var bestResult: Node = sourceFile
var lastNodeEntirelyBeforePosition: Node = zeroOfMyType
forEachChild( sourceFile, visit )
if (lastNodeEntirelyBeforePosition) {
 val lastChildOfLastEntireNodeBeforePosition = getLastChild( lastNodeEntirelyBeforePosition )
if ((lastChildOfLastEntireNodeBeforePosition.pos>bestResult.pos)) {
 (bestResult=lastChildOfLastEntireNodeBeforePosition)

}

}
return bestResult
def getLastChild(node: Node): Node = {
 while (true) {
{
 val lastChild = getLastChildWorker( node )
if (lastChild) {
 (node=lastChild)

}
else {
 return node

}

}
}

}
def getLastChildWorker(node: Node): Node = {
 var last: Node = undefined
forEachChild( node, (child =>  {
 if (nodeIsPresent( child )) {
 (last=child)

}

}) )
return last

}
def visit(child: Node) = {
 if (nodeIsMissing( child )) {
 return

}
if ((child.pos<=position)) {
 if ((child.pos>=bestResult.pos)) {
 (bestResult=child)

}
if ((position<child.end)) {
 forEachChild( child, visit )
return true

}
else {
 Debug.assert( (child.end<=position) )
(lastNodeEntirelyBeforePosition=child)

}

}
else {
 Debug.assert( (child.pos>position) )
return true

}

}

}
def checkChangeRange(sourceFile: SourceFile, newText: String, textChangeRange: TextChangeRange, aggressiveChecks: Boolean) = {
 val oldText = sourceFile.text
if (textChangeRange) {
 Debug.assert( ((((oldText.length-textChangeRange.span.length)+textChangeRange.newLength))===newText.length) )
if ((aggressiveChecks||Debug.shouldAssert( AssertionLevel.VeryAggressive ))) {
 val oldTextPrefix = oldText.substr( 0, textChangeRange.span.start )
val newTextPrefix = newText.substr( 0, textChangeRange.span.start )
Debug.assert( (oldTextPrefix===newTextPrefix) )
val oldTextSuffix = oldText.substring( textSpanEnd( textChangeRange.span ), oldText.length )
val newTextSuffix = newText.substring( textSpanEnd( textChangeRangeNewSpan( textChangeRange ) ), newText.length )
Debug.assert( (oldTextSuffix===newTextSuffix) )

}

}

}
trait IncrementalElement extends TextRange {
  var parent: Node
  var intersectsChange: Boolean
  var length: Int
  var _children: Array[Node]
}
trait IncrementalNode extends Node with IncrementalElement {
  var hasBeenIncrementallyParsed: Boolean
}
trait IncrementalNodeArray extends NodeArray[ IncrementalNode ] with IncrementalElement {
  var length: Int
}
trait SyntaxCursor {
  def currentNode(position: Int): IncrementalNode
}
def createSyntaxCursor(sourceFile: SourceFile): SyntaxCursor = {
 var currentArray: NodeArray[Node] = sourceFile.statements
var currentArrayIndex = 0
Debug.assert( (currentArrayIndex<currentArray.length) )
var current = currentArray(currentArrayIndex)
var lastQueriedPosition = InvalidPosition.Value
return Map( "currentNode" -> (( position: Int ) =>  {
 if ((position!==lastQueriedPosition)) {
 if (((current&&(current.end===position))&&(currentArrayIndex<((currentArray.length-1))))) {
 (currentArrayIndex+= 1)
(current=currentArray(currentArrayIndex))

}
if (((!current)||(current.pos!==position))) {
 findHighestListElementThatStartsAtPosition( position )

}

}
 (lastQueriedPosition=position)
 Debug.assert( ((!current)||(current.pos===position)) )
 return current.asInstanceOf[IncrementalNode]

}) )
def findHighestListElementThatStartsAtPosition(position: Int) = {
 (currentArray=undefined)
(currentArrayIndex=InvalidPosition.Value)
(current=undefined)
forEachChild( sourceFile, visitNode, visitArray )
return
def visitNode(node: Node) = {
 if (((position>=node.pos)&&(position<node.end))) {
 forEachChild( node, visitNode, visitArray )
return true

}
return false

}
def visitArray(array: NodeArray[Node]) = {
 if (((position>=array.pos)&&(position<array.end))) {
 {
var i = 0
var n = array.length
while( (i<n)) {
 {
 val child = array(i)
if (child) {
 if ((child.pos===position)) {
 (currentArray=array)
(currentArrayIndex=i)
(current=child)
return true

}
else {
 if (((child.pos<position)&&(position<child.end))) {
 forEachChild( child, visitNode, visitArray )
return true

}

}

}

}
 (i+= 1)
}
}

}
return false

}

}

}
sealed abstract class InvalidPosition
object InvalidPosition {
   case object Value extends InvalidPosition
}
}
}
