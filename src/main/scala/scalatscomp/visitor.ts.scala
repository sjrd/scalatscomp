package scalatscomp
object Visitor {
type VisitResult[T <: Node] = ( T | Array[T] )
trait NodeEdge {
  var name: String
  var optional: Boolean
  var test: ((Node) => Boolean)
  var lift: ((NodeArray[Node]) => Node)
  var parenthesize: ((Node, Node) => Node)
}
;type NodeTraversalPath = Array[NodeEdge]
val nodeEdgeTraversalMap = createMap[ NodeTraversalPath ]( Map( SyntaxKind.QualifiedName -> Array( Map( "name" -> "left",
"test" -> isEntityName ), Map( "name" -> "right",
"test" -> isIdentifier ) ),
SyntaxKind.Decorator -> Array( Map( "name" -> "expression",
"test" -> isLeftHandSideExpression ) ),
SyntaxKind.TypeAssertionExpression -> Array( Map( "name" -> "type",
"test" -> isTypeNode ), Map( "name" -> "expression",
"test" -> isUnaryExpression ) ),
SyntaxKind.AsExpression -> Array( Map( "name" -> "expression",
"test" -> isExpression ), Map( "name" -> "type",
"test" -> isTypeNode ) ),
SyntaxKind.NonNullExpression -> Array( Map( "name" -> "expression",
"test" -> isLeftHandSideExpression ) ),
SyntaxKind.EnumDeclaration -> Array( Map( "name" -> "decorators",
"test" -> isDecorator ), Map( "name" -> "modifiers",
"test" -> isModifier ), Map( "name" -> "name",
"test" -> isIdentifier ), Map( "name" -> "members",
"test" -> isEnumMember ) ),
SyntaxKind.ModuleDeclaration -> Array( Map( "name" -> "decorators",
"test" -> isDecorator ), Map( "name" -> "modifiers",
"test" -> isModifier ), Map( "name" -> "name",
"test" -> isModuleName ), Map( "name" -> "body",
"test" -> isModuleBody ) ),
SyntaxKind.ModuleBlock -> Array( Map( "name" -> "statements",
"test" -> isStatement ) ),
SyntaxKind.ImportEqualsDeclaration -> Array( Map( "name" -> "decorators",
"test" -> isDecorator ), Map( "name" -> "modifiers",
"test" -> isModifier ), Map( "name" -> "name",
"test" -> isIdentifier ), Map( "name" -> "moduleReference",
"test" -> isModuleReference ) ),
SyntaxKind.ExternalModuleReference -> Array( Map( "name" -> "expression",
"test" -> isExpression,
"optional" -> true ) ),
SyntaxKind.EnumMember -> Array( Map( "name" -> "name",
"test" -> isPropertyName ), Map( "name" -> "initializer",
"test" -> isExpression,
"optional" -> true,
"parenthesize" -> parenthesizeExpressionForList ) ) ) )
def reduceNode[T](node: Node, f: ((T, Node) => T), initial: T) = {
 return (if (node) f( initial, node ) else initial)

}
def reduceEachChild[T](node: Node, f: ((T, Node) => T), initial: T): T = {
 if ((node===undefined)) {
 return initial

}
val kind = node.kind
if ((((kind>SyntaxKind.FirstToken)&&(kind<=SyntaxKind.LastToken)))) {
 return initial

}
if ((((kind>=SyntaxKind.TypePredicate)&&(kind<=SyntaxKind.LiteralType)))) {
 return initial

}
var result = initial
node.kind match {
  case  SyntaxKind.SemicolonClassElement | SyntaxKind.EmptyStatement | SyntaxKind.OmittedExpression | SyntaxKind.DebuggerStatement | SyntaxKind.NotEmittedStatement  =>
  case  SyntaxKind.ComputedPropertyName  =>
(result=reduceNode( (node.asInstanceOf[ComputedPropertyName]).expression, f, result ))
  case  SyntaxKind.Parameter  =>
(result=reduceLeft( (node.asInstanceOf[ParameterDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[ParameterDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[ParameterDeclaration]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[ParameterDeclaration]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[ParameterDeclaration]).initializer, f, result ))
  case  SyntaxKind.Decorator  =>
(result=reduceNode( (node.asInstanceOf[Decorator]).expression, f, result ))
  case  SyntaxKind.PropertyDeclaration  =>
(result=reduceLeft( (node.asInstanceOf[PropertyDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[PropertyDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[PropertyDeclaration]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[PropertyDeclaration]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[PropertyDeclaration]).initializer, f, result ))
  case  SyntaxKind.MethodDeclaration  =>
(result=reduceLeft( (node.asInstanceOf[MethodDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[MethodDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[MethodDeclaration]).name, f, result ))
(result=reduceLeft( (node.asInstanceOf[MethodDeclaration]).typeParameters, f, result ))
(result=reduceLeft( (node.asInstanceOf[MethodDeclaration]).parameters, f, result ))
(result=reduceNode( (node.asInstanceOf[MethodDeclaration]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[MethodDeclaration]).body, f, result ))
  case  SyntaxKind.Constructor  =>
(result=reduceLeft( (node.asInstanceOf[ConstructorDeclaration]).modifiers, f, result ))
(result=reduceLeft( (node.asInstanceOf[ConstructorDeclaration]).parameters, f, result ))
(result=reduceNode( (node.asInstanceOf[ConstructorDeclaration]).body, f, result ))
  case  SyntaxKind.GetAccessor  =>
(result=reduceLeft( (node.asInstanceOf[GetAccessorDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[GetAccessorDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[GetAccessorDeclaration]).name, f, result ))
(result=reduceLeft( (node.asInstanceOf[GetAccessorDeclaration]).parameters, f, result ))
(result=reduceNode( (node.asInstanceOf[GetAccessorDeclaration]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[GetAccessorDeclaration]).body, f, result ))
  case  SyntaxKind.SetAccessor  =>
(result=reduceLeft( (node.asInstanceOf[GetAccessorDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[GetAccessorDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[GetAccessorDeclaration]).name, f, result ))
(result=reduceLeft( (node.asInstanceOf[GetAccessorDeclaration]).parameters, f, result ))
(result=reduceNode( (node.asInstanceOf[GetAccessorDeclaration]).body, f, result ))
  case  SyntaxKind.ObjectBindingPattern | SyntaxKind.ArrayBindingPattern  =>
(result=reduceLeft( (node.asInstanceOf[BindingPattern]).elements, f, result ))
  case  SyntaxKind.BindingElement  =>
(result=reduceNode( (node.asInstanceOf[BindingElement]).propertyName, f, result ))
(result=reduceNode( (node.asInstanceOf[BindingElement]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[BindingElement]).initializer, f, result ))
  case  SyntaxKind.ArrayLiteralExpression  =>
(result=reduceLeft( (node.asInstanceOf[ArrayLiteralExpression]).elements, f, result ))
  case  SyntaxKind.ObjectLiteralExpression  =>
(result=reduceLeft( (node.asInstanceOf[ObjectLiteralExpression]).properties, f, result ))
  case  SyntaxKind.PropertyAccessExpression  =>
(result=reduceNode( (node.asInstanceOf[PropertyAccessExpression]).expression, f, result ))
(result=reduceNode( (node.asInstanceOf[PropertyAccessExpression]).name, f, result ))
  case  SyntaxKind.ElementAccessExpression  =>
(result=reduceNode( (node.asInstanceOf[ElementAccessExpression]).expression, f, result ))
(result=reduceNode( (node.asInstanceOf[ElementAccessExpression]).argumentExpression, f, result ))
  case  SyntaxKind.CallExpression  =>
(result=reduceNode( (node.asInstanceOf[CallExpression]).expression, f, result ))
(result=reduceLeft( (node.asInstanceOf[CallExpression]).typeArguments, f, result ))
(result=reduceLeft( (node.asInstanceOf[CallExpression]).arguments, f, result ))
  case  SyntaxKind.NewExpression  =>
(result=reduceNode( (node.asInstanceOf[NewExpression]).expression, f, result ))
(result=reduceLeft( (node.asInstanceOf[NewExpression]).typeArguments, f, result ))
(result=reduceLeft( (node.asInstanceOf[NewExpression]).arguments, f, result ))
  case  SyntaxKind.TaggedTemplateExpression  =>
(result=reduceNode( (node.asInstanceOf[TaggedTemplateExpression]).tag, f, result ))
(result=reduceNode( (node.asInstanceOf[TaggedTemplateExpression]).template, f, result ))
  case  SyntaxKind.FunctionExpression  =>
(result=reduceLeft( (node.asInstanceOf[FunctionExpression]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[FunctionExpression]).name, f, result ))
(result=reduceLeft( (node.asInstanceOf[FunctionExpression]).typeParameters, f, result ))
(result=reduceLeft( (node.asInstanceOf[FunctionExpression]).parameters, f, result ))
(result=reduceNode( (node.asInstanceOf[FunctionExpression]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[FunctionExpression]).body, f, result ))
  case  SyntaxKind.ArrowFunction  =>
(result=reduceLeft( (node.asInstanceOf[ArrowFunction]).modifiers, f, result ))
(result=reduceLeft( (node.asInstanceOf[ArrowFunction]).typeParameters, f, result ))
(result=reduceLeft( (node.asInstanceOf[ArrowFunction]).parameters, f, result ))
(result=reduceNode( (node.asInstanceOf[ArrowFunction]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[ArrowFunction]).body, f, result ))
  case  SyntaxKind.ParenthesizedExpression | SyntaxKind.DeleteExpression | SyntaxKind.TypeOfExpression | SyntaxKind.VoidExpression | SyntaxKind.AwaitExpression | SyntaxKind.YieldExpression | SyntaxKind.SpreadElementExpression | SyntaxKind.NonNullExpression  =>
(result=reduceNode( (node.asInstanceOf[( ParenthesizedExpression | DeleteExpression | TypeOfExpression | VoidExpression | AwaitExpression | YieldExpression | SpreadElementExpression | NonNullExpression )]).expression, f, result ))
  case  SyntaxKind.PrefixUnaryExpression | SyntaxKind.PostfixUnaryExpression  =>
(result=reduceNode( (node.asInstanceOf[( PrefixUnaryExpression | PostfixUnaryExpression )]).operand, f, result ))
  case  SyntaxKind.BinaryExpression  =>
(result=reduceNode( (node.asInstanceOf[BinaryExpression]).left, f, result ))
(result=reduceNode( (node.asInstanceOf[BinaryExpression]).right, f, result ))
  case  SyntaxKind.ConditionalExpression  =>
(result=reduceNode( (node.asInstanceOf[ConditionalExpression]).condition, f, result ))
(result=reduceNode( (node.asInstanceOf[ConditionalExpression]).whenTrue, f, result ))
(result=reduceNode( (node.asInstanceOf[ConditionalExpression]).whenFalse, f, result ))
  case  SyntaxKind.TemplateExpression  =>
(result=reduceNode( (node.asInstanceOf[TemplateExpression]).head, f, result ))
(result=reduceLeft( (node.asInstanceOf[TemplateExpression]).templateSpans, f, result ))
  case  SyntaxKind.ClassExpression  =>
(result=reduceLeft( (node.asInstanceOf[ClassExpression]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[ClassExpression]).name, f, result ))
(result=reduceLeft( (node.asInstanceOf[ClassExpression]).typeParameters, f, result ))
(result=reduceLeft( (node.asInstanceOf[ClassExpression]).heritageClauses, f, result ))
(result=reduceLeft( (node.asInstanceOf[ClassExpression]).members, f, result ))
  case  SyntaxKind.ExpressionWithTypeArguments  =>
(result=reduceNode( (node.asInstanceOf[ExpressionWithTypeArguments]).expression, f, result ))
(result=reduceLeft( (node.asInstanceOf[ExpressionWithTypeArguments]).typeArguments, f, result ))
  case  SyntaxKind.TemplateSpan  =>
(result=reduceNode( (node.asInstanceOf[TemplateSpan]).expression, f, result ))
(result=reduceNode( (node.asInstanceOf[TemplateSpan]).literal, f, result ))
  case  SyntaxKind.Block  =>
(result=reduceLeft( (node.asInstanceOf[Block]).statements, f, result ))
  case  SyntaxKind.VariableStatement  =>
(result=reduceLeft( (node.asInstanceOf[VariableStatement]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[VariableStatement]).declarationList, f, result ))
  case  SyntaxKind.ExpressionStatement  =>
(result=reduceNode( (node.asInstanceOf[ExpressionStatement]).expression, f, result ))
  case  SyntaxKind.IfStatement  =>
(result=reduceNode( (node.asInstanceOf[IfStatement]).expression, f, result ))
(result=reduceNode( (node.asInstanceOf[IfStatement]).thenStatement, f, result ))
(result=reduceNode( (node.asInstanceOf[IfStatement]).elseStatement, f, result ))
  case  SyntaxKind.DoStatement  =>
(result=reduceNode( (node.asInstanceOf[DoStatement]).statement, f, result ))
(result=reduceNode( (node.asInstanceOf[DoStatement]).expression, f, result ))
  case  SyntaxKind.WhileStatement | SyntaxKind.WithStatement  =>
(result=reduceNode( (node.asInstanceOf[( WhileStatement | WithStatement )]).expression, f, result ))
(result=reduceNode( (node.asInstanceOf[( WhileStatement | WithStatement )]).statement, f, result ))
  case  SyntaxKind.ForStatement  =>
(result=reduceNode( (node.asInstanceOf[ForStatement]).initializer, f, result ))
(result=reduceNode( (node.asInstanceOf[ForStatement]).condition, f, result ))
(result=reduceNode( (node.asInstanceOf[ForStatement]).incrementor, f, result ))
(result=reduceNode( (node.asInstanceOf[ForStatement]).statement, f, result ))
  case  SyntaxKind.ForInStatement | SyntaxKind.ForOfStatement  =>
(result=reduceNode( (node.asInstanceOf[( ForInStatement | ForOfStatement )]).initializer, f, result ))
(result=reduceNode( (node.asInstanceOf[( ForInStatement | ForOfStatement )]).expression, f, result ))
(result=reduceNode( (node.asInstanceOf[( ForInStatement | ForOfStatement )]).statement, f, result ))
  case  SyntaxKind.ReturnStatement | SyntaxKind.ThrowStatement  =>
(result=reduceNode( (node.asInstanceOf[ReturnStatement]).expression, f, result ))
  case  SyntaxKind.SwitchStatement  =>
(result=reduceNode( (node.asInstanceOf[SwitchStatement]).expression, f, result ))
(result=reduceNode( (node.asInstanceOf[SwitchStatement]).caseBlock, f, result ))
  case  SyntaxKind.LabeledStatement  =>
(result=reduceNode( (node.asInstanceOf[LabeledStatement]).label, f, result ))
(result=reduceNode( (node.asInstanceOf[LabeledStatement]).statement, f, result ))
  case  SyntaxKind.TryStatement  =>
(result=reduceNode( (node.asInstanceOf[TryStatement]).tryBlock, f, result ))
(result=reduceNode( (node.asInstanceOf[TryStatement]).catchClause, f, result ))
(result=reduceNode( (node.asInstanceOf[TryStatement]).finallyBlock, f, result ))
  case  SyntaxKind.VariableDeclaration  =>
(result=reduceNode( (node.asInstanceOf[VariableDeclaration]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[VariableDeclaration]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[VariableDeclaration]).initializer, f, result ))
  case  SyntaxKind.VariableDeclarationList  =>
(result=reduceLeft( (node.asInstanceOf[VariableDeclarationList]).declarations, f, result ))
  case  SyntaxKind.FunctionDeclaration  =>
(result=reduceLeft( (node.asInstanceOf[FunctionDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[FunctionDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[FunctionDeclaration]).name, f, result ))
(result=reduceLeft( (node.asInstanceOf[FunctionDeclaration]).typeParameters, f, result ))
(result=reduceLeft( (node.asInstanceOf[FunctionDeclaration]).parameters, f, result ))
(result=reduceNode( (node.asInstanceOf[FunctionDeclaration]).`type`, f, result ))
(result=reduceNode( (node.asInstanceOf[FunctionDeclaration]).body, f, result ))
  case  SyntaxKind.ClassDeclaration  =>
(result=reduceLeft( (node.asInstanceOf[ClassDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[ClassDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[ClassDeclaration]).name, f, result ))
(result=reduceLeft( (node.asInstanceOf[ClassDeclaration]).typeParameters, f, result ))
(result=reduceLeft( (node.asInstanceOf[ClassDeclaration]).heritageClauses, f, result ))
(result=reduceLeft( (node.asInstanceOf[ClassDeclaration]).members, f, result ))
  case  SyntaxKind.CaseBlock  =>
(result=reduceLeft( (node.asInstanceOf[CaseBlock]).clauses, f, result ))
  case  SyntaxKind.ImportDeclaration  =>
(result=reduceLeft( (node.asInstanceOf[ImportDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[ImportDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[ImportDeclaration]).importClause, f, result ))
(result=reduceNode( (node.asInstanceOf[ImportDeclaration]).moduleSpecifier, f, result ))
  case  SyntaxKind.ImportClause  =>
(result=reduceNode( (node.asInstanceOf[ImportClause]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[ImportClause]).namedBindings, f, result ))
  case  SyntaxKind.NamespaceImport  =>
(result=reduceNode( (node.asInstanceOf[NamespaceImport]).name, f, result ))
  case  SyntaxKind.NamedImports | SyntaxKind.NamedExports  =>
(result=reduceLeft( (node.asInstanceOf[( NamedImports | NamedExports )]).elements, f, result ))
  case  SyntaxKind.ImportSpecifier | SyntaxKind.ExportSpecifier  =>
(result=reduceNode( (node.asInstanceOf[( ImportSpecifier | ExportSpecifier )]).propertyName, f, result ))
(result=reduceNode( (node.asInstanceOf[( ImportSpecifier | ExportSpecifier )]).name, f, result ))
  case  SyntaxKind.ExportAssignment  =>
(result=reduceLeft( (node.asInstanceOf[ExportAssignment]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[ExportAssignment]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[ExportAssignment]).expression, f, result ))
  case  SyntaxKind.ExportDeclaration  =>
(result=reduceLeft( (node.asInstanceOf[ExportDeclaration]).decorators, f, result ))
(result=reduceLeft( (node.asInstanceOf[ExportDeclaration]).modifiers, f, result ))
(result=reduceNode( (node.asInstanceOf[ExportDeclaration]).exportClause, f, result ))
(result=reduceNode( (node.asInstanceOf[ExportDeclaration]).moduleSpecifier, f, result ))
  case  SyntaxKind.JsxElement  =>
(result=reduceNode( (node.asInstanceOf[JsxElement]).openingElement, f, result ))
(result=reduceLeft( (node.asInstanceOf[JsxElement]).children, f, result ))
(result=reduceNode( (node.asInstanceOf[JsxElement]).closingElement, f, result ))
  case  SyntaxKind.JsxSelfClosingElement | SyntaxKind.JsxOpeningElement  =>
(result=reduceNode( (node.asInstanceOf[( JsxSelfClosingElement | JsxOpeningElement )]).tagName, f, result ))
(result=reduceLeft( (node.asInstanceOf[( JsxSelfClosingElement | JsxOpeningElement )]).attributes, f, result ))
  case  SyntaxKind.JsxClosingElement  =>
(result=reduceNode( (node.asInstanceOf[JsxClosingElement]).tagName, f, result ))
  case  SyntaxKind.JsxAttribute  =>
(result=reduceNode( (node.asInstanceOf[JsxAttribute]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[JsxAttribute]).initializer, f, result ))
  case  SyntaxKind.JsxSpreadAttribute  =>
(result=reduceNode( (node.asInstanceOf[JsxSpreadAttribute]).expression, f, result ))
  case  SyntaxKind.JsxExpression  =>
(result=reduceNode( (node.asInstanceOf[JsxExpression]).expression, f, result ))
  case  SyntaxKind.CaseClause  =>
(result=reduceNode( (node.asInstanceOf[CaseClause]).expression, f, result ))
  case  SyntaxKind.DefaultClause  =>
(result=reduceLeft( (node.asInstanceOf[( CaseClause | DefaultClause )]).statements, f, result ))
  case  SyntaxKind.HeritageClause  =>
(result=reduceLeft( (node.asInstanceOf[HeritageClause]).types, f, result ))
  case  SyntaxKind.CatchClause  =>
(result=reduceNode( (node.asInstanceOf[CatchClause]).variableDeclaration, f, result ))
(result=reduceNode( (node.asInstanceOf[CatchClause]).block, f, result ))
  case  SyntaxKind.PropertyAssignment  =>
(result=reduceNode( (node.asInstanceOf[PropertyAssignment]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[PropertyAssignment]).initializer, f, result ))
  case  SyntaxKind.ShorthandPropertyAssignment  =>
(result=reduceNode( (node.asInstanceOf[ShorthandPropertyAssignment]).name, f, result ))
(result=reduceNode( (node.asInstanceOf[ShorthandPropertyAssignment]).objectAssignmentInitializer, f, result ))
  case  SyntaxKind.SourceFile  =>
(result=reduceLeft( (node.asInstanceOf[SourceFile]).statements, f, result ))
  case  SyntaxKind.PartiallyEmittedExpression  =>
(result=reduceNode( (node.asInstanceOf[PartiallyEmittedExpression]).expression, f, result ))
  case _ =>
val edgeTraversalPath = nodeEdgeTraversalMap(kind)
if (edgeTraversalPath) {
 (edgeTraversalPath).foreach { fresh1 =>
val edge = zeroOfMyType
 = fresh1
 {
 val value = (node.asInstanceOf[MapLike[Any]])(edge.name)
if ((value!==undefined)) {
 (result=(if (isArray( value )) reduceLeft( value.asInstanceOf[NodeArray[Node]], f, result ) else f( result, value.asInstanceOf[Node] )))

}

}
}

}
}
return result

}
def visitNode[T <: Node](node: T, visitor: ((Node) => VisitResult[Node]), test: ((Node) => Boolean), optional: Boolean, lift: ((NodeArray[Node]) => T)): T
def visitNode[T <: Node](node: T, visitor: ((Node) => VisitResult[Node]), test: ((Node) => Boolean), optional: Boolean, lift: ((NodeArray[Node]) => T), parenthesize: ((Node, Node) => Node), parentNode: Node): T
def visitNode(node: Node, visitor: ((Node) => VisitResult[Node]), test: ((Node) => Boolean), optional: Boolean, lift: ((Array[Node]) => Node), parenthesize: ((Node, Node) => Node), parentNode: Node): Node = {
 if ((node===undefined)) {
 return undefined

}
val visited = visitor( node )
if ((visited===node)) {
 return node

}
var visitedNode: Node = zeroOfMyType
if ((visited===undefined)) {
 if ((!optional)) {
 Debug.failNotOptional()

}
return undefined

}
else if (isArray( visited )) {
 (visitedNode=((lift||extractSingleNode))( visited ))

}
else {
 (visitedNode=visited)

}
if ((parenthesize!==undefined)) {
 (visitedNode=parenthesize( visitedNode, parentNode ))

}
Debug.assertNode( visitedNode, test )
aggregateTransformFlags( visitedNode )
return visitedNode

}
def visitNodes[T <: Node](nodes: NodeArray[T], visitor: ((Node) => VisitResult[Node]), test: ((Node) => Boolean), start: Int, count: Int): NodeArray[T]
def visitNodes[T <: Node](nodes: NodeArray[T], visitor: ((Node) => VisitResult[Node]), test: ((Node) => Boolean), start: Int, count: Int, parenthesize: ((Node, Node) => Node), parentNode: Node): NodeArray[T]
def visitNodes(nodes: NodeArray[Node], visitor: ((Node) => VisitResult[Node]), test: ((Node) => Boolean), start: Int, count: Int, parenthesize: ((Node, Node) => Node), parentNode: Node): NodeArray[Node] = {
 if ((nodes===undefined)) {
 return undefined

}
var updated: NodeArray[Node] = zeroOfMyType
val length = nodes.length
if (((start===undefined)||(start<0))) {
 (start=0)

}
if (((count===undefined)||(count>(length-start)))) {
 (count=(length-start))

}
if (((start>0)||(count<length))) {
 (updated=createNodeArray[ Node ]( Array(), undefined, (nodes.hasTrailingComma&&((start+count)===length)) ))

}
{
var i = 0
while( (i<count)) {
 {
 val node = nodes((i+start))
val visited = (if ((node!==undefined)) visitor( node ) else undefined)
if ((((updated!==undefined)||(visited===undefined))||(visited!==node))) {
 if ((updated===undefined)) {
 (updated=createNodeArray( nodes.slice( 0, i ), nodes, nodes.hasTrailingComma ))

}
if (visited) {
 if (isArray( visited )) {
 (visited).foreach { fresh2 =>
var visitedNode = zeroOfMyType
 = fresh2
 {
 (visitedNode=(if (parenthesize) parenthesize( visitedNode, parentNode ) else visitedNode))
Debug.assertNode( visitedNode, test )
aggregateTransformFlags( visitedNode )
updated.push( visitedNode )

}
}

}
else {
 val visitedNode = (if (parenthesize) parenthesize( visited, parentNode ) else visited)
Debug.assertNode( visitedNode, test )
aggregateTransformFlags( visitedNode )
updated.push( visitedNode )

}

}

}

}
 (i+= 1)
}
}
return (updated||nodes)

}
def visitEachChild[T <: Node](node: T, visitor: ((Node) => VisitResult[Node]), context: LexicalEnvironment): T
def visitEachChild(node: Node, visitor: ((Node) => VisitResult[Node]), context: LexicalEnvironment): Node = {
 if ((node===undefined)) {
 return undefined

}
val kind = node.kind
if ((((kind>SyntaxKind.FirstToken)&&(kind<=SyntaxKind.LastToken)))) {
 return node

}
if ((((kind>=SyntaxKind.TypePredicate)&&(kind<=SyntaxKind.LiteralType)))) {
 return node

}
node.kind match {
  case  SyntaxKind.SemicolonClassElement | SyntaxKind.EmptyStatement | SyntaxKind.OmittedExpression | SyntaxKind.DebuggerStatement  =>
return node
  case  SyntaxKind.ComputedPropertyName  =>
return updateComputedPropertyName( node.asInstanceOf[ComputedPropertyName], visitNode( (node.asInstanceOf[ComputedPropertyName]).expression, visitor, isExpression ) )
  case  SyntaxKind.Parameter  =>
return updateParameterDeclaration( node.asInstanceOf[ParameterDeclaration], visitNodes( (node.asInstanceOf[ParameterDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[ParameterDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[ParameterDeclaration]).name, visitor, isBindingName ), visitNode( (node.asInstanceOf[ParameterDeclaration]).`type`, visitor, isTypeNode, true ), visitNode( (node.asInstanceOf[ParameterDeclaration]).initializer, visitor, isExpression, true ) )
  case  SyntaxKind.PropertyDeclaration  =>
return updateProperty( node.asInstanceOf[PropertyDeclaration], visitNodes( (node.asInstanceOf[PropertyDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[PropertyDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[PropertyDeclaration]).name, visitor, isPropertyName ), visitNode( (node.asInstanceOf[PropertyDeclaration]).`type`, visitor, isTypeNode, true ), visitNode( (node.asInstanceOf[PropertyDeclaration]).initializer, visitor, isExpression, true ) )
  case  SyntaxKind.MethodDeclaration  =>
return updateMethod( node.asInstanceOf[MethodDeclaration], visitNodes( (node.asInstanceOf[MethodDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[MethodDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[MethodDeclaration]).name, visitor, isPropertyName ), visitNodes( (node.asInstanceOf[MethodDeclaration]).typeParameters, visitor, isTypeParameter ), ((context.startLexicalEnvironment(),visitNodes( (node.asInstanceOf[MethodDeclaration]).parameters, visitor, isParameter ))), visitNode( (node.asInstanceOf[MethodDeclaration]).`type`, visitor, isTypeNode, true ), mergeFunctionBodyLexicalEnvironment( visitNode( (node.asInstanceOf[MethodDeclaration]).body, visitor, isFunctionBody, true ), context.endLexicalEnvironment() ) )
  case  SyntaxKind.Constructor  =>
return updateConstructor( node.asInstanceOf[ConstructorDeclaration], visitNodes( (node.asInstanceOf[ConstructorDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[ConstructorDeclaration]).modifiers, visitor, isModifier ), ((context.startLexicalEnvironment(),visitNodes( (node.asInstanceOf[ConstructorDeclaration]).parameters, visitor, isParameter ))), mergeFunctionBodyLexicalEnvironment( visitNode( (node.asInstanceOf[ConstructorDeclaration]).body, visitor, isFunctionBody, true ), context.endLexicalEnvironment() ) )
  case  SyntaxKind.GetAccessor  =>
return updateGetAccessor( node.asInstanceOf[GetAccessorDeclaration], visitNodes( (node.asInstanceOf[GetAccessorDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[GetAccessorDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[GetAccessorDeclaration]).name, visitor, isPropertyName ), ((context.startLexicalEnvironment(),visitNodes( (node.asInstanceOf[GetAccessorDeclaration]).parameters, visitor, isParameter ))), visitNode( (node.asInstanceOf[GetAccessorDeclaration]).`type`, visitor, isTypeNode, true ), mergeFunctionBodyLexicalEnvironment( visitNode( (node.asInstanceOf[GetAccessorDeclaration]).body, visitor, isFunctionBody, true ), context.endLexicalEnvironment() ) )
  case  SyntaxKind.SetAccessor  =>
return updateSetAccessor( node.asInstanceOf[SetAccessorDeclaration], visitNodes( (node.asInstanceOf[SetAccessorDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[SetAccessorDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[SetAccessorDeclaration]).name, visitor, isPropertyName ), ((context.startLexicalEnvironment(),visitNodes( (node.asInstanceOf[SetAccessorDeclaration]).parameters, visitor, isParameter ))), mergeFunctionBodyLexicalEnvironment( visitNode( (node.asInstanceOf[SetAccessorDeclaration]).body, visitor, isFunctionBody, true ), context.endLexicalEnvironment() ) )
  case  SyntaxKind.ObjectBindingPattern  =>
return updateObjectBindingPattern( node.asInstanceOf[ObjectBindingPattern], visitNodes( (node.asInstanceOf[ObjectBindingPattern]).elements, visitor, isBindingElement ) )
  case  SyntaxKind.ArrayBindingPattern  =>
return updateArrayBindingPattern( node.asInstanceOf[ArrayBindingPattern], visitNodes( (node.asInstanceOf[ArrayBindingPattern]).elements, visitor, isArrayBindingElement ) )
  case  SyntaxKind.BindingElement  =>
return updateBindingElement( node.asInstanceOf[BindingElement], visitNode( (node.asInstanceOf[BindingElement]).propertyName, visitor, isPropertyName, true ), visitNode( (node.asInstanceOf[BindingElement]).name, visitor, isBindingName ), visitNode( (node.asInstanceOf[BindingElement]).initializer, visitor, isExpression, true ) )
  case  SyntaxKind.ArrayLiteralExpression  =>
return updateArrayLiteral( node.asInstanceOf[ArrayLiteralExpression], visitNodes( (node.asInstanceOf[ArrayLiteralExpression]).elements, visitor, isExpression ) )
  case  SyntaxKind.ObjectLiteralExpression  =>
return updateObjectLiteral( node.asInstanceOf[ObjectLiteralExpression], visitNodes( (node.asInstanceOf[ObjectLiteralExpression]).properties, visitor, isObjectLiteralElementLike ) )
  case  SyntaxKind.PropertyAccessExpression  =>
return updatePropertyAccess( node.asInstanceOf[PropertyAccessExpression], visitNode( (node.asInstanceOf[PropertyAccessExpression]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[PropertyAccessExpression]).name, visitor, isIdentifier ) )
  case  SyntaxKind.ElementAccessExpression  =>
return updateElementAccess( node.asInstanceOf[ElementAccessExpression], visitNode( (node.asInstanceOf[ElementAccessExpression]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[ElementAccessExpression]).argumentExpression, visitor, isExpression ) )
  case  SyntaxKind.CallExpression  =>
return updateCall( node.asInstanceOf[CallExpression], visitNode( (node.asInstanceOf[CallExpression]).expression, visitor, isExpression ), visitNodes( (node.asInstanceOf[CallExpression]).typeArguments, visitor, isTypeNode ), visitNodes( (node.asInstanceOf[CallExpression]).arguments, visitor, isExpression ) )
  case  SyntaxKind.NewExpression  =>
return updateNew( node.asInstanceOf[NewExpression], visitNode( (node.asInstanceOf[NewExpression]).expression, visitor, isExpression ), visitNodes( (node.asInstanceOf[NewExpression]).typeArguments, visitor, isTypeNode ), visitNodes( (node.asInstanceOf[NewExpression]).arguments, visitor, isExpression ) )
  case  SyntaxKind.TaggedTemplateExpression  =>
return updateTaggedTemplate( node.asInstanceOf[TaggedTemplateExpression], visitNode( (node.asInstanceOf[TaggedTemplateExpression]).tag, visitor, isExpression ), visitNode( (node.asInstanceOf[TaggedTemplateExpression]).template, visitor, isTemplateLiteral ) )
  case  SyntaxKind.ParenthesizedExpression  =>
return updateParen( node.asInstanceOf[ParenthesizedExpression], visitNode( (node.asInstanceOf[ParenthesizedExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.FunctionExpression  =>
return updateFunctionExpression( node.asInstanceOf[FunctionExpression], visitNodes( (node.asInstanceOf[FunctionExpression]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[FunctionExpression]).name, visitor, isPropertyName ), visitNodes( (node.asInstanceOf[FunctionExpression]).typeParameters, visitor, isTypeParameter ), ((context.startLexicalEnvironment(),visitNodes( (node.asInstanceOf[FunctionExpression]).parameters, visitor, isParameter ))), visitNode( (node.asInstanceOf[FunctionExpression]).`type`, visitor, isTypeNode, true ), mergeFunctionBodyLexicalEnvironment( visitNode( (node.asInstanceOf[FunctionExpression]).body, visitor, isFunctionBody, true ), context.endLexicalEnvironment() ) )
  case  SyntaxKind.ArrowFunction  =>
return updateArrowFunction( node.asInstanceOf[ArrowFunction], visitNodes( (node.asInstanceOf[ArrowFunction]).modifiers, visitor, isModifier ), visitNodes( (node.asInstanceOf[ArrowFunction]).typeParameters, visitor, isTypeParameter ), ((context.startLexicalEnvironment(),visitNodes( (node.asInstanceOf[ArrowFunction]).parameters, visitor, isParameter ))), visitNode( (node.asInstanceOf[ArrowFunction]).`type`, visitor, isTypeNode, true ), mergeFunctionBodyLexicalEnvironment( visitNode( (node.asInstanceOf[ArrowFunction]).body, visitor, isConciseBody, true ), context.endLexicalEnvironment() ) )
  case  SyntaxKind.DeleteExpression  =>
return updateDelete( node.asInstanceOf[DeleteExpression], visitNode( (node.asInstanceOf[DeleteExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.TypeOfExpression  =>
return updateTypeOf( node.asInstanceOf[TypeOfExpression], visitNode( (node.asInstanceOf[TypeOfExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.VoidExpression  =>
return updateVoid( node.asInstanceOf[VoidExpression], visitNode( (node.asInstanceOf[VoidExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.AwaitExpression  =>
return updateAwait( node.asInstanceOf[AwaitExpression], visitNode( (node.asInstanceOf[AwaitExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.BinaryExpression  =>
return updateBinary( node.asInstanceOf[BinaryExpression], visitNode( (node.asInstanceOf[BinaryExpression]).left, visitor, isExpression ), visitNode( (node.asInstanceOf[BinaryExpression]).right, visitor, isExpression ) )
  case  SyntaxKind.PrefixUnaryExpression  =>
return updatePrefix( node.asInstanceOf[PrefixUnaryExpression], visitNode( (node.asInstanceOf[PrefixUnaryExpression]).operand, visitor, isExpression ) )
  case  SyntaxKind.PostfixUnaryExpression  =>
return updatePostfix( node.asInstanceOf[PostfixUnaryExpression], visitNode( (node.asInstanceOf[PostfixUnaryExpression]).operand, visitor, isExpression ) )
  case  SyntaxKind.ConditionalExpression  =>
return updateConditional( node.asInstanceOf[ConditionalExpression], visitNode( (node.asInstanceOf[ConditionalExpression]).condition, visitor, isExpression ), visitNode( (node.asInstanceOf[ConditionalExpression]).whenTrue, visitor, isExpression ), visitNode( (node.asInstanceOf[ConditionalExpression]).whenFalse, visitor, isExpression ) )
  case  SyntaxKind.TemplateExpression  =>
return updateTemplateExpression( node.asInstanceOf[TemplateExpression], visitNode( (node.asInstanceOf[TemplateExpression]).head, visitor, isTemplateHead ), visitNodes( (node.asInstanceOf[TemplateExpression]).templateSpans, visitor, isTemplateSpan ) )
  case  SyntaxKind.YieldExpression  =>
return updateYield( node.asInstanceOf[YieldExpression], visitNode( (node.asInstanceOf[YieldExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.SpreadElementExpression  =>
return updateSpread( node.asInstanceOf[SpreadElementExpression], visitNode( (node.asInstanceOf[SpreadElementExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.ClassExpression  =>
return updateClassExpression( node.asInstanceOf[ClassExpression], visitNodes( (node.asInstanceOf[ClassExpression]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[ClassExpression]).name, visitor, isIdentifier, true ), visitNodes( (node.asInstanceOf[ClassExpression]).typeParameters, visitor, isTypeParameter ), visitNodes( (node.asInstanceOf[ClassExpression]).heritageClauses, visitor, isHeritageClause ), visitNodes( (node.asInstanceOf[ClassExpression]).members, visitor, isClassElement ) )
  case  SyntaxKind.ExpressionWithTypeArguments  =>
return updateExpressionWithTypeArguments( node.asInstanceOf[ExpressionWithTypeArguments], visitNodes( (node.asInstanceOf[ExpressionWithTypeArguments]).typeArguments, visitor, isTypeNode ), visitNode( (node.asInstanceOf[ExpressionWithTypeArguments]).expression, visitor, isExpression ) )
  case  SyntaxKind.TemplateSpan  =>
return updateTemplateSpan( node.asInstanceOf[TemplateSpan], visitNode( (node.asInstanceOf[TemplateSpan]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[TemplateSpan]).literal, visitor, isTemplateMiddleOrTemplateTail ) )
  case  SyntaxKind.Block  =>
return updateBlock( node.asInstanceOf[Block], visitNodes( (node.asInstanceOf[Block]).statements, visitor, isStatement ) )
  case  SyntaxKind.VariableStatement  =>
return updateVariableStatement( node.asInstanceOf[VariableStatement], visitNodes( (node.asInstanceOf[VariableStatement]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[VariableStatement]).declarationList, visitor, isVariableDeclarationList ) )
  case  SyntaxKind.ExpressionStatement  =>
return updateStatement( node.asInstanceOf[ExpressionStatement], visitNode( (node.asInstanceOf[ExpressionStatement]).expression, visitor, isExpression ) )
  case  SyntaxKind.IfStatement  =>
return updateIf( node.asInstanceOf[IfStatement], visitNode( (node.asInstanceOf[IfStatement]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[IfStatement]).thenStatement, visitor, isStatement, false, liftToBlock ), visitNode( (node.asInstanceOf[IfStatement]).elseStatement, visitor, isStatement, true, liftToBlock ) )
  case  SyntaxKind.DoStatement  =>
return updateDo( node.asInstanceOf[DoStatement], visitNode( (node.asInstanceOf[DoStatement]).statement, visitor, isStatement, false, liftToBlock ), visitNode( (node.asInstanceOf[DoStatement]).expression, visitor, isExpression ) )
  case  SyntaxKind.WhileStatement  =>
return updateWhile( node.asInstanceOf[WhileStatement], visitNode( (node.asInstanceOf[WhileStatement]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[WhileStatement]).statement, visitor, isStatement, false, liftToBlock ) )
  case  SyntaxKind.ForStatement  =>
return updateFor( node.asInstanceOf[ForStatement], visitNode( (node.asInstanceOf[ForStatement]).initializer, visitor, isForInitializer ), visitNode( (node.asInstanceOf[ForStatement]).condition, visitor, isExpression ), visitNode( (node.asInstanceOf[ForStatement]).incrementor, visitor, isExpression ), visitNode( (node.asInstanceOf[ForStatement]).statement, visitor, isStatement, false, liftToBlock ) )
  case  SyntaxKind.ForInStatement  =>
return updateForIn( node.asInstanceOf[ForInStatement], visitNode( (node.asInstanceOf[ForInStatement]).initializer, visitor, isForInitializer ), visitNode( (node.asInstanceOf[ForInStatement]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[ForInStatement]).statement, visitor, isStatement, false, liftToBlock ) )
  case  SyntaxKind.ForOfStatement  =>
return updateForOf( node.asInstanceOf[ForOfStatement], visitNode( (node.asInstanceOf[ForOfStatement]).initializer, visitor, isForInitializer ), visitNode( (node.asInstanceOf[ForOfStatement]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[ForOfStatement]).statement, visitor, isStatement, false, liftToBlock ) )
  case  SyntaxKind.ContinueStatement  =>
return updateContinue( node.asInstanceOf[ContinueStatement], visitNode( (node.asInstanceOf[ContinueStatement]).label, visitor, isIdentifier, true ) )
  case  SyntaxKind.BreakStatement  =>
return updateBreak( node.asInstanceOf[BreakStatement], visitNode( (node.asInstanceOf[BreakStatement]).label, visitor, isIdentifier, true ) )
  case  SyntaxKind.ReturnStatement  =>
return updateReturn( node.asInstanceOf[ReturnStatement], visitNode( (node.asInstanceOf[ReturnStatement]).expression, visitor, isExpression, true ) )
  case  SyntaxKind.WithStatement  =>
return updateWith( node.asInstanceOf[WithStatement], visitNode( (node.asInstanceOf[WithStatement]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[WithStatement]).statement, visitor, isStatement, false, liftToBlock ) )
  case  SyntaxKind.SwitchStatement  =>
return updateSwitch( node.asInstanceOf[SwitchStatement], visitNode( (node.asInstanceOf[SwitchStatement]).expression, visitor, isExpression ), visitNode( (node.asInstanceOf[SwitchStatement]).caseBlock, visitor, isCaseBlock ) )
  case  SyntaxKind.LabeledStatement  =>
return updateLabel( node.asInstanceOf[LabeledStatement], visitNode( (node.asInstanceOf[LabeledStatement]).label, visitor, isIdentifier ), visitNode( (node.asInstanceOf[LabeledStatement]).statement, visitor, isStatement, false, liftToBlock ) )
  case  SyntaxKind.ThrowStatement  =>
return updateThrow( node.asInstanceOf[ThrowStatement], visitNode( (node.asInstanceOf[ThrowStatement]).expression, visitor, isExpression ) )
  case  SyntaxKind.TryStatement  =>
return updateTry( node.asInstanceOf[TryStatement], visitNode( (node.asInstanceOf[TryStatement]).tryBlock, visitor, isBlock ), visitNode( (node.asInstanceOf[TryStatement]).catchClause, visitor, isCatchClause, true ), visitNode( (node.asInstanceOf[TryStatement]).finallyBlock, visitor, isBlock, true ) )
  case  SyntaxKind.VariableDeclaration  =>
return updateVariableDeclaration( node.asInstanceOf[VariableDeclaration], visitNode( (node.asInstanceOf[VariableDeclaration]).name, visitor, isBindingName ), visitNode( (node.asInstanceOf[VariableDeclaration]).`type`, visitor, isTypeNode, true ), visitNode( (node.asInstanceOf[VariableDeclaration]).initializer, visitor, isExpression, true ) )
  case  SyntaxKind.VariableDeclarationList  =>
return updateVariableDeclarationList( node.asInstanceOf[VariableDeclarationList], visitNodes( (node.asInstanceOf[VariableDeclarationList]).declarations, visitor, isVariableDeclaration ) )
  case  SyntaxKind.FunctionDeclaration  =>
return updateFunctionDeclaration( node.asInstanceOf[FunctionDeclaration], visitNodes( (node.asInstanceOf[FunctionDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[FunctionDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[FunctionDeclaration]).name, visitor, isPropertyName ), visitNodes( (node.asInstanceOf[FunctionDeclaration]).typeParameters, visitor, isTypeParameter ), ((context.startLexicalEnvironment(),visitNodes( (node.asInstanceOf[FunctionDeclaration]).parameters, visitor, isParameter ))), visitNode( (node.asInstanceOf[FunctionDeclaration]).`type`, visitor, isTypeNode, true ), mergeFunctionBodyLexicalEnvironment( visitNode( (node.asInstanceOf[FunctionDeclaration]).body, visitor, isFunctionBody, true ), context.endLexicalEnvironment() ) )
  case  SyntaxKind.ClassDeclaration  =>
return updateClassDeclaration( node.asInstanceOf[ClassDeclaration], visitNodes( (node.asInstanceOf[ClassDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[ClassDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[ClassDeclaration]).name, visitor, isIdentifier, true ), visitNodes( (node.asInstanceOf[ClassDeclaration]).typeParameters, visitor, isTypeParameter ), visitNodes( (node.asInstanceOf[ClassDeclaration]).heritageClauses, visitor, isHeritageClause ), visitNodes( (node.asInstanceOf[ClassDeclaration]).members, visitor, isClassElement ) )
  case  SyntaxKind.CaseBlock  =>
return updateCaseBlock( node.asInstanceOf[CaseBlock], visitNodes( (node.asInstanceOf[CaseBlock]).clauses, visitor, isCaseOrDefaultClause ) )
  case  SyntaxKind.ImportDeclaration  =>
return updateImportDeclaration( node.asInstanceOf[ImportDeclaration], visitNodes( (node.asInstanceOf[ImportDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[ImportDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[ImportDeclaration]).importClause, visitor, isImportClause, true ), visitNode( (node.asInstanceOf[ImportDeclaration]).moduleSpecifier, visitor, isExpression ) )
  case  SyntaxKind.ImportClause  =>
return updateImportClause( node.asInstanceOf[ImportClause], visitNode( (node.asInstanceOf[ImportClause]).name, visitor, isIdentifier, true ), visitNode( (node.asInstanceOf[ImportClause]).namedBindings, visitor, isNamedImportBindings, true ) )
  case  SyntaxKind.NamespaceImport  =>
return updateNamespaceImport( node.asInstanceOf[NamespaceImport], visitNode( (node.asInstanceOf[NamespaceImport]).name, visitor, isIdentifier ) )
  case  SyntaxKind.NamedImports  =>
return updateNamedImports( node.asInstanceOf[NamedImports], visitNodes( (node.asInstanceOf[NamedImports]).elements, visitor, isImportSpecifier ) )
  case  SyntaxKind.ImportSpecifier  =>
return updateImportSpecifier( node.asInstanceOf[ImportSpecifier], visitNode( (node.asInstanceOf[ImportSpecifier]).propertyName, visitor, isIdentifier, true ), visitNode( (node.asInstanceOf[ImportSpecifier]).name, visitor, isIdentifier ) )
  case  SyntaxKind.ExportAssignment  =>
return updateExportAssignment( node.asInstanceOf[ExportAssignment], visitNodes( (node.asInstanceOf[ExportAssignment]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[ExportAssignment]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[ExportAssignment]).expression, visitor, isExpression ) )
  case  SyntaxKind.ExportDeclaration  =>
return updateExportDeclaration( node.asInstanceOf[ExportDeclaration], visitNodes( (node.asInstanceOf[ExportDeclaration]).decorators, visitor, isDecorator ), visitNodes( (node.asInstanceOf[ExportDeclaration]).modifiers, visitor, isModifier ), visitNode( (node.asInstanceOf[ExportDeclaration]).exportClause, visitor, isNamedExports, true ), visitNode( (node.asInstanceOf[ExportDeclaration]).moduleSpecifier, visitor, isExpression, true ) )
  case  SyntaxKind.NamedExports  =>
return updateNamedExports( node.asInstanceOf[NamedExports], visitNodes( (node.asInstanceOf[NamedExports]).elements, visitor, isExportSpecifier ) )
  case  SyntaxKind.ExportSpecifier  =>
return updateExportSpecifier( node.asInstanceOf[ExportSpecifier], visitNode( (node.asInstanceOf[ExportSpecifier]).propertyName, visitor, isIdentifier, true ), visitNode( (node.asInstanceOf[ExportSpecifier]).name, visitor, isIdentifier ) )
  case  SyntaxKind.JsxElement  =>
return updateJsxElement( node.asInstanceOf[JsxElement], visitNode( (node.asInstanceOf[JsxElement]).openingElement, visitor, isJsxOpeningElement ), visitNodes( (node.asInstanceOf[JsxElement]).children, visitor, isJsxChild ), visitNode( (node.asInstanceOf[JsxElement]).closingElement, visitor, isJsxClosingElement ) )
  case  SyntaxKind.JsxSelfClosingElement  =>
return updateJsxSelfClosingElement( node.asInstanceOf[JsxSelfClosingElement], visitNode( (node.asInstanceOf[JsxSelfClosingElement]).tagName, visitor, isJsxTagNameExpression ), visitNodes( (node.asInstanceOf[JsxSelfClosingElement]).attributes, visitor, isJsxAttributeLike ) )
  case  SyntaxKind.JsxOpeningElement  =>
return updateJsxOpeningElement( node.asInstanceOf[JsxOpeningElement], visitNode( (node.asInstanceOf[JsxOpeningElement]).tagName, visitor, isJsxTagNameExpression ), visitNodes( (node.asInstanceOf[JsxOpeningElement]).attributes, visitor, isJsxAttributeLike ) )
  case  SyntaxKind.JsxClosingElement  =>
return updateJsxClosingElement( node.asInstanceOf[JsxClosingElement], visitNode( (node.asInstanceOf[JsxClosingElement]).tagName, visitor, isJsxTagNameExpression ) )
  case  SyntaxKind.JsxAttribute  =>
return updateJsxAttribute( node.asInstanceOf[JsxAttribute], visitNode( (node.asInstanceOf[JsxAttribute]).name, visitor, isIdentifier ), visitNode( (node.asInstanceOf[JsxAttribute]).initializer, visitor, isStringLiteralOrJsxExpression ) )
  case  SyntaxKind.JsxSpreadAttribute  =>
return updateJsxSpreadAttribute( node.asInstanceOf[JsxSpreadAttribute], visitNode( (node.asInstanceOf[JsxSpreadAttribute]).expression, visitor, isExpression ) )
  case  SyntaxKind.JsxExpression  =>
return updateJsxExpression( node.asInstanceOf[JsxExpression], visitNode( (node.asInstanceOf[JsxExpression]).expression, visitor, isExpression ) )
  case  SyntaxKind.CaseClause  =>
return updateCaseClause( node.asInstanceOf[CaseClause], visitNode( (node.asInstanceOf[CaseClause]).expression, visitor, isExpression ), visitNodes( (node.asInstanceOf[CaseClause]).statements, visitor, isStatement ) )
  case  SyntaxKind.DefaultClause  =>
return updateDefaultClause( node.asInstanceOf[DefaultClause], visitNodes( (node.asInstanceOf[DefaultClause]).statements, visitor, isStatement ) )
  case  SyntaxKind.HeritageClause  =>
return updateHeritageClause( node.asInstanceOf[HeritageClause], visitNodes( (node.asInstanceOf[HeritageClause]).types, visitor, isExpressionWithTypeArguments ) )
  case  SyntaxKind.CatchClause  =>
return updateCatchClause( node.asInstanceOf[CatchClause], visitNode( (node.asInstanceOf[CatchClause]).variableDeclaration, visitor, isVariableDeclaration ), visitNode( (node.asInstanceOf[CatchClause]).block, visitor, isBlock ) )
  case  SyntaxKind.PropertyAssignment  =>
return updatePropertyAssignment( node.asInstanceOf[PropertyAssignment], visitNode( (node.asInstanceOf[PropertyAssignment]).name, visitor, isPropertyName ), visitNode( (node.asInstanceOf[PropertyAssignment]).initializer, visitor, isExpression ) )
  case  SyntaxKind.ShorthandPropertyAssignment  =>
return updateShorthandPropertyAssignment( node.asInstanceOf[ShorthandPropertyAssignment], visitNode( (node.asInstanceOf[ShorthandPropertyAssignment]).name, visitor, isIdentifier ), visitNode( (node.asInstanceOf[ShorthandPropertyAssignment]).objectAssignmentInitializer, visitor, isExpression ) )
  case  SyntaxKind.SourceFile  =>
context.startLexicalEnvironment()
return updateSourceFileNode( node.asInstanceOf[SourceFile], createNodeArray( concatenate( visitNodes( (node.asInstanceOf[SourceFile]).statements, visitor, isStatement ), context.endLexicalEnvironment() ), (node.asInstanceOf[SourceFile]).statements ) )
  case  SyntaxKind.PartiallyEmittedExpression  =>
return updatePartiallyEmittedExpression( node.asInstanceOf[PartiallyEmittedExpression], visitNode( (node.asInstanceOf[PartiallyEmittedExpression]).expression, visitor, isExpression ) )
  case _ =>
var updated: ( Node with MapLike[Any] ) = zeroOfMyType
val edgeTraversalPath = nodeEdgeTraversalMap(kind)
if (edgeTraversalPath) {
 (edgeTraversalPath).foreach { fresh3 =>
val edge = zeroOfMyType
 = fresh3
 {
 val value = (node.asInstanceOf[( Node with Map[Any] )])(edge.name).asInstanceOf[( Node | NodeArray[Node] )]
if ((value!==undefined)) {
 val visited = (if (isArray( value )) visitNodes( value, visitor, edge.test, 0, value.length, edge.parenthesize, node ) else visitNode( value, visitor, edge.test, edge.optional, edge.lift, edge.parenthesize, node ))
if (((updated!==undefined)||(visited!==value))) {
 if ((updated===undefined)) {
 (updated=getMutableClone( node ))

}
if ((visited!==value)) {
 (updated(edge.name)=visited)

}

}

}

}
}

}
return (if (updated) updateNode( updated, node ) else node)
}

}
def mergeFunctionBodyLexicalEnvironment(body: FunctionBody, declarations: Array[Statement]): FunctionBody
def mergeFunctionBodyLexicalEnvironment(body: ConciseBody, declarations: Array[Statement]): ConciseBody
def mergeFunctionBodyLexicalEnvironment(body: ConciseBody, declarations: Array[Statement]): ConciseBody = {
 if (((body&&(declarations!==undefined))&&(declarations.length>0))) {
 if (isBlock( body )) {
 return updateBlock( body, createNodeArray( concatenate( body.statements, declarations ), body.statements ) )

}
else {
 return createBlock( createNodeArray( Array( createReturn( body, body ), declarations: _* ), body ), body, true )

}

}
return body

}
def liftToBlock(nodes: Array[Node]): Statement = {
 Debug.assert( every( nodes, isStatement ), "Cannot lift nodes to a Block." )
return (singleOrUndefined( nodes ).asInstanceOf[Statement]||createBlock( nodes.asInstanceOf[NodeArray[Statement]] ))

}
def extractSingleNode(nodes: Array[Node]): Node = {
 Debug.assert( (nodes.length<=1), "Too many nodes written to output." )
return singleOrUndefined( nodes )

}
def aggregateTransformFlags[T <: Node](node: T): T = {
 aggregateTransformFlagsForNode( node )
return node

}
def aggregateTransformFlagsForNode(node: Node): TransformFlags = {
 if ((node===undefined)) {
 return TransformFlags.None

}
else if ((node.transformFlags&TransformFlags.HasComputedFlags)) {
 return (node.transformFlags&(~getTransformFlagsSubtreeExclusions( node.kind )))

}
else {
 val subtreeFlags = aggregateTransformFlagsForSubtree( node )
return computeTransformFlagsForNode( node, subtreeFlags )

}

}
def aggregateTransformFlagsForSubtree(node: Node): TransformFlags = {
 if ((hasModifier( node, ModifierFlags.Ambient )||isTypeNode( node ))) {
 return TransformFlags.None

}
return reduceEachChild( node, aggregateTransformFlagsForChildNode, TransformFlags.None )

}
def aggregateTransformFlagsForChildNode(transformFlags: TransformFlags, child: Node): TransformFlags = {
 return (transformFlags|aggregateTransformFlagsForNode( child ))

}
def getTransformFlagsSubtreeExclusions(kind: SyntaxKind) = {
 if (((kind>=SyntaxKind.FirstTypeNode)&&(kind<=SyntaxKind.LastTypeNode))) {
 return TransformFlags.TypeExcludes

}
kind match {
  case  SyntaxKind.CallExpression | SyntaxKind.NewExpression | SyntaxKind.ArrayLiteralExpression  =>
return TransformFlags.ArrayLiteralOrCallOrNewExcludes
  case  SyntaxKind.ModuleDeclaration  =>
return TransformFlags.ModuleExcludes
  case  SyntaxKind.Parameter  =>
return TransformFlags.ParameterExcludes
  case  SyntaxKind.ArrowFunction  =>
return TransformFlags.ArrowFunctionExcludes
  case  SyntaxKind.FunctionExpression | SyntaxKind.FunctionDeclaration  =>
return TransformFlags.FunctionExcludes
  case  SyntaxKind.VariableDeclarationList  =>
return TransformFlags.VariableDeclarationListExcludes
  case  SyntaxKind.ClassDeclaration | SyntaxKind.ClassExpression  =>
return TransformFlags.ClassExcludes
  case  SyntaxKind.Constructor  =>
return TransformFlags.ConstructorExcludes
  case  SyntaxKind.MethodDeclaration | SyntaxKind.GetAccessor | SyntaxKind.SetAccessor  =>
return TransformFlags.MethodOrAccessorExcludes
  case  SyntaxKind.AnyKeyword | SyntaxKind.NumberKeyword | SyntaxKind.NeverKeyword | SyntaxKind.StringKeyword | SyntaxKind.BooleanKeyword | SyntaxKind.SymbolKeyword | SyntaxKind.VoidKeyword | SyntaxKind.TypeParameter | SyntaxKind.PropertySignature | SyntaxKind.MethodSignature | SyntaxKind.CallSignature | SyntaxKind.ConstructSignature | SyntaxKind.IndexSignature | SyntaxKind.InterfaceDeclaration | SyntaxKind.TypeAliasDeclaration  =>
return TransformFlags.TypeExcludes
  case  SyntaxKind.ObjectLiteralExpression  =>
return TransformFlags.ObjectLiteralExcludes
  case _ =>
return TransformFlags.NodeExcludes
}

}
object Debug {
val failNotOptional = (if (shouldAssert( AssertionLevel.Normal )) (( message: String ) =>  assert( false, (message||"Node not optional.") )) else noop)
val failBadSyntaxKind = (if (shouldAssert( AssertionLevel.Normal )) (( node: Node, message: String ) =>  assert( false, (message||"Unexpected node."), (() =>  s"""Node ${ formatSyntaxKind( node.kind )} was unexpected.""" ) )) else noop)
val assertNode = (if (shouldAssert( AssertionLevel.Normal )) (( node: Node, test: ((Node) => Boolean), message: String ) =>  assert( ((test===undefined)||test( node )), (message||"Unexpected node."), (() =>  s"""Node ${ formatSyntaxKind( node.kind )} did not pass test '${ getFunctionName( test )}'.""" ) )) else noop)
def getFunctionName(func: Function) = {
 if ((typeof(func)!=="function")) {
 return ""

}
else if (func.`hasOwnProperty`( "name" )) {
 return (func.asInstanceOf[Any]).name

}
else {
 val text = Function.prototype.`toString`.call( func )
val `match` = java.util.regex.Pattern.compile(raw"""^function\s+([\w\$$]+)\s*\(""").exec( text )
return (if (`match`) `match`(1) else "")

}

}
}
}
