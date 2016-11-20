package scalatscomp.transformers.module
object System {
def transformSystemModule(context: TransformationContext) = {
 trait DependencyGroup {
  var name: StringLiteral
  var externalImports: Array[( ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration )]
}
const fresh1 = context
val startLexicalEnvironment = fresh1.startLexicalEnvironment
val endLexicalEnvironment = fresh1.endLexicalEnvironment
val hoistVariableDeclaration = fresh1.hoistVariableDeclaration
val hoistFunctionDeclaration = fresh1.hoistFunctionDeclaration
val compilerOptions = context.getCompilerOptions()
val resolver = context.getEmitResolver()
val host = context.getEmitHost()
val previousOnSubstituteNode = context.onSubstituteNode
val previousOnEmitNode = context.onEmitNode
(context.onSubstituteNode=onSubstituteNode)
(context.onEmitNode=onEmitNode)
context.enableSubstitution( SyntaxKind.Identifier )
context.enableSubstitution( SyntaxKind.BinaryExpression )
context.enableSubstitution( SyntaxKind.PrefixUnaryExpression )
context.enableSubstitution( SyntaxKind.PostfixUnaryExpression )
context.enableEmitNotification( SyntaxKind.SourceFile )
val exportFunctionForFileMap: Array[Identifier] = Array()
var currentSourceFile: SourceFile = zeroOfMyType
var externalImports: Array[( ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration )] = zeroOfMyType
var exportSpecifiers: Map[Array[ExportSpecifier]] = zeroOfMyType
var exportEquals: ExportAssignment = zeroOfMyType
var hasExportStarsToExportValues: Boolean = zeroOfMyType
var exportFunctionForFile: Identifier = zeroOfMyType
var contextObjectForFile: Identifier = zeroOfMyType
var exportedLocalNames: Array[Identifier] = zeroOfMyType
var exportedFunctionDeclarations: Array[ExpressionStatement] = zeroOfMyType
var enclosingBlockScopedContainer: Node = zeroOfMyType
var currentParent: Node = zeroOfMyType
var currentNode: Node = zeroOfMyType
return transformSourceFile
def transformSourceFile(node: SourceFile) = {
 if (isDeclarationFile( node )) {
 return node

}
if ((isExternalModule( node )||compilerOptions.isolatedModules)) {
 (currentSourceFile=node)
(currentNode=node)
val updated = transformSystemModuleWorker( node )
aggregateTransformFlags( updated )
(currentSourceFile=undefined)
(externalImports=undefined)
(exportSpecifiers=undefined)
(exportEquals=undefined)
(hasExportStarsToExportValues=false)
(exportFunctionForFile=undefined)
(contextObjectForFile=undefined)
(exportedLocalNames=undefined)
(exportedFunctionDeclarations=undefined)
return updated

}
return node

}
def transformSystemModuleWorker(node: SourceFile) = {
 Debug.assert( (!exportFunctionForFile) )
((Map( "externalImports" -> externalImports,
"exportSpecifiers" -> exportSpecifiers,
"exportEquals" -> exportEquals,
"hasExportStarsToExportValues" -> hasExportStarsToExportValues )=collectExternalModuleInfo( node )))
(exportFunctionForFile=createUniqueName( "exports" ))
(contextObjectForFile=createUniqueName( "context" ))
(exportFunctionForFileMap(getOriginalNodeId( node ))=exportFunctionForFile)
val dependencyGroups = collectDependencyGroups( externalImports )
val statements: Array[Statement] = Array()
addSystemModuleBody( statements, node, dependencyGroups )
val moduleName = tryGetModuleNameFromFile( node, host, compilerOptions )
val dependencies = createArrayLiteral( map( dependencyGroups, getNameOfDependencyGroup ) )
val body = createFunctionExpression( undefined, undefined, undefined, undefined, Array( createParameter( exportFunctionForFile ), createParameter( contextObjectForFile ) ), undefined, setEmitFlags( createBlock( statements, undefined, true ), EmitFlags.EmitEmitHelpers ) )
return updateSourceFile( node, Array( createStatement( createCall( createPropertyAccess( createIdentifier( "System" ), "register" ), undefined, (if (moduleName) Array( moduleName, dependencies, body ) else Array( dependencies, body )) ) ) ), ((~EmitFlags.EmitEmitHelpers)&getEmitFlags( node )) )

}
def addSystemModuleBody(statements: Array[Statement], node: SourceFile, dependencyGroups: Array[DependencyGroup]) = {
 startLexicalEnvironment()
val statementOffset = addPrologueDirectives( statements, node.statements, (!compilerOptions.noImplicitUseStrict), visitSourceElement )
statements.push( createVariableStatement( undefined, createVariableDeclarationList( Array( createVariableDeclaration( "__moduleName", undefined, createLogicalAnd( contextObjectForFile, createPropertyAccess( contextObjectForFile, "id" ) ) ) ) ) ) )
val executeStatements = visitNodes( node.statements, visitSourceElement, isStatement, statementOffset )
addRange( statements, endLexicalEnvironment() )
addRange( statements, exportedFunctionDeclarations )
val exportStarFunction = addExportStarIfNeeded( statements )
statements.push( createReturn( setMultiLine( createObjectLiteral( Array( createPropertyAssignment( "setters", generateSetters( exportStarFunction, dependencyGroups ) ), createPropertyAssignment( "execute", createFunctionExpression( undefined, undefined, undefined, undefined, Array(), undefined, createBlock( executeStatements, undefined, true ) ) ) ) ), true ) ) )

}
def addExportStarIfNeeded(statements: Array[Statement]) = {
 if ((!hasExportStarsToExportValues)) {
 return

}
if (((!exportedLocalNames)&&isEmpty( exportSpecifiers ))) {
 var hasExportDeclarationWithExportClause = false
(externalImports).foreach { fresh2 =>
val externalImport = zeroOfMyType
 = fresh2
 {
 if (((externalImport.kind===SyntaxKind.ExportDeclaration)&&(externalImport.asInstanceOf[ExportDeclaration]).exportClause)) {
 (hasExportDeclarationWithExportClause=true)
break()

}

}
}
if ((!hasExportDeclarationWithExportClause)) {
 return addExportStarFunction( statements, undefined )

}

}
val exportedNames: Array[ObjectLiteralElementLike] = Array()
if (exportedLocalNames) {
 (exportedLocalNames).foreach { fresh3 =>
val exportedLocalName = zeroOfMyType
 = fresh3
 {
 exportedNames.push( createPropertyAssignment( createLiteral( exportedLocalName.text ), createLiteral( true ) ) )

}
}

}
(externalImports).foreach { fresh4 =>
val externalImport = zeroOfMyType
 = fresh4
 {
 if ((externalImport.kind!==SyntaxKind.ExportDeclaration)) {
 continue

}
val exportDecl = externalImport.asInstanceOf[ExportDeclaration]
if ((!exportDecl.exportClause)) {
 continue

}
(exportDecl.exportClause.elements).foreach { fresh5 =>
val element = zeroOfMyType
 = fresh5
 {
 exportedNames.push( createPropertyAssignment( createLiteral( ((element.name||element.propertyName)).text ), createLiteral( true ) ) )

}
}

}
}
val exportedNamesStorageRef = createUniqueName( "exportedNames" )
statements.push( createVariableStatement( undefined, createVariableDeclarationList( Array( createVariableDeclaration( exportedNamesStorageRef, undefined, createObjectLiteral( exportedNames, undefined, true ) ) ) ) ) )
return addExportStarFunction( statements, exportedNamesStorageRef )

}
def generateSetters(exportStarFunction: Identifier, dependencyGroups: Array[DependencyGroup]) = {
 val setters: Array[Expression] = Array()
(dependencyGroups).foreach { fresh6 =>
val group = zeroOfMyType
 = fresh6
 {
 val localName = forEach( group.externalImports, (i =>  getLocalNameForExternalImport( i, currentSourceFile )) )
val parameterName = (if (localName) getGeneratedNameForNode( localName ) else createUniqueName( "" ))
val statements: Array[Statement] = Array()
(group.externalImports).foreach { fresh7 =>
val entry = zeroOfMyType
 = fresh7
 {
 val importVariableName = getLocalNameForExternalImport( entry, currentSourceFile )
entry.kind match {
  case  SyntaxKind.ImportDeclaration  =>
if ((!(entry.asInstanceOf[ImportDeclaration]).importClause)) {
 break()

}
  case  SyntaxKind.ImportEqualsDeclaration  =>
Debug.assert( (importVariableName!==undefined) )
statements.push( createStatement( createAssignment( importVariableName, parameterName ) ) )
  case  SyntaxKind.ExportDeclaration  =>
Debug.assert( (importVariableName!==undefined) )
if ((entry.asInstanceOf[ExportDeclaration]).exportClause) {
 val properties: Array[PropertyAssignment] = Array()
((entry.asInstanceOf[ExportDeclaration]).exportClause.elements).foreach { fresh8 =>
val e = zeroOfMyType
 = fresh8
 {
 properties.push( createPropertyAssignment( createLiteral( e.name.text ), createElementAccess( parameterName, createLiteral( ((e.propertyName||e.name)).text ) ) ) )

}
}
statements.push( createStatement( createCall( exportFunctionForFile, undefined, Array( createObjectLiteral( properties, undefined, true ) ) ) ) )

}
else {
 statements.push( createStatement( createCall( exportStarFunction, undefined, Array( parameterName ) ) ) )

}
  case _ =>
}

}
}
setters.push( createFunctionExpression( undefined, undefined, undefined, undefined, Array( createParameter( parameterName ) ), undefined, createBlock( statements, undefined, true ) ) )

}
}
return createArrayLiteral( setters, undefined, true )

}
def visitSourceElement(node: Node): VisitResult[Node] = {
 node.kind match {
  case  SyntaxKind.ImportDeclaration  =>
return visitImportDeclaration( node.asInstanceOf[ImportDeclaration] )
  case  SyntaxKind.ImportEqualsDeclaration  =>
return visitImportEqualsDeclaration( node.asInstanceOf[ImportEqualsDeclaration] )
  case  SyntaxKind.ExportDeclaration  =>
return visitExportDeclaration( node.asInstanceOf[ExportDeclaration] )
  case  SyntaxKind.ExportAssignment  =>
return visitExportAssignment( node.asInstanceOf[ExportAssignment] )
  case _ =>
return visitNestedNode( node )
}

}
def visitNestedNode(node: Node): VisitResult[Node] = {
 val savedEnclosingBlockScopedContainer = enclosingBlockScopedContainer
val savedCurrentParent = currentParent
val savedCurrentNode = currentNode
val currentGrandparent = currentParent
(currentParent=currentNode)
(currentNode=node)
if ((currentParent&&isBlockScope( currentParent, currentGrandparent ))) {
 (enclosingBlockScopedContainer=currentParent)

}
val result = visitNestedNodeWorker( node )
(enclosingBlockScopedContainer=savedEnclosingBlockScopedContainer)
(currentParent=savedCurrentParent)
(currentNode=savedCurrentNode)
return result

}
def visitNestedNodeWorker(node: Node): VisitResult[Node] = {
 node.kind match {
  case  SyntaxKind.VariableStatement  =>
return visitVariableStatement( node.asInstanceOf[VariableStatement] )
  case  SyntaxKind.FunctionDeclaration  =>
return visitFunctionDeclaration( node.asInstanceOf[FunctionDeclaration] )
  case  SyntaxKind.ClassDeclaration  =>
return visitClassDeclaration( node.asInstanceOf[ClassDeclaration] )
  case  SyntaxKind.ForStatement  =>
return visitForStatement( node.asInstanceOf[ForStatement] )
  case  SyntaxKind.ForInStatement  =>
return visitForInStatement( node.asInstanceOf[ForInStatement] )
  case  SyntaxKind.ForOfStatement  =>
return visitForOfStatement( node.asInstanceOf[ForOfStatement] )
  case  SyntaxKind.DoStatement  =>
return visitDoStatement( node.asInstanceOf[DoStatement] )
  case  SyntaxKind.WhileStatement  =>
return visitWhileStatement( node.asInstanceOf[WhileStatement] )
  case  SyntaxKind.LabeledStatement  =>
return visitLabeledStatement( node.asInstanceOf[LabeledStatement] )
  case  SyntaxKind.WithStatement  =>
return visitWithStatement( node.asInstanceOf[WithStatement] )
  case  SyntaxKind.SwitchStatement  =>
return visitSwitchStatement( node.asInstanceOf[SwitchStatement] )
  case  SyntaxKind.CaseBlock  =>
return visitCaseBlock( node.asInstanceOf[CaseBlock] )
  case  SyntaxKind.CaseClause  =>
return visitCaseClause( node.asInstanceOf[CaseClause] )
  case  SyntaxKind.DefaultClause  =>
return visitDefaultClause( node.asInstanceOf[DefaultClause] )
  case  SyntaxKind.TryStatement  =>
return visitTryStatement( node.asInstanceOf[TryStatement] )
  case  SyntaxKind.CatchClause  =>
return visitCatchClause( node.asInstanceOf[CatchClause] )
  case  SyntaxKind.Block  =>
return visitBlock( node.asInstanceOf[Block] )
  case  SyntaxKind.ExpressionStatement  =>
return visitExpressionStatement( node.asInstanceOf[ExpressionStatement] )
  case _ =>
return node
}

}
def visitImportDeclaration(node: ImportDeclaration): Node = {
 if ((node.importClause&&contains( externalImports, node ))) {
 hoistVariableDeclaration( getLocalNameForExternalImport( node, currentSourceFile ) )

}
return undefined

}
def visitImportEqualsDeclaration(node: ImportEqualsDeclaration): Node = {
 if (contains( externalImports, node )) {
 hoistVariableDeclaration( getLocalNameForExternalImport( node, currentSourceFile ) )

}
return undefined

}
def visitExportDeclaration(node: ExportDeclaration): VisitResult[Statement] = {
 if ((!node.moduleSpecifier)) {
 val statements: Array[Statement] = Array()
addRange( statements, map( node.exportClause.elements, visitExportSpecifier ) )
return statements

}
return undefined

}
def visitExportSpecifier(specifier: ExportSpecifier): Statement = {
 recordExportName( specifier.name )
return createExportStatement( specifier.name, (specifier.propertyName||specifier.name) )

}
def visitExportAssignment(node: ExportAssignment): Statement = {
 if (node.isExportEquals) {
 return undefined

}
return createExportStatement( createLiteral( "default" ), node.expression )

}
def visitVariableStatement(node: VariableStatement): VisitResult[Statement] = {
 val shouldHoist = (((((getCombinedNodeFlags( getOriginalNode( node.declarationList ) )&NodeFlags.BlockScoped))==0))||(enclosingBlockScopedContainer.kind===SyntaxKind.SourceFile))
if ((!shouldHoist)) {
 return node

}
val isExported = hasModifier( node, ModifierFlags.Export )
val expressions: Array[Expression] = Array()
(node.declarationList.declarations).foreach { fresh9 =>
val variable = zeroOfMyType
 = fresh9
 {
 val visited = transformVariable( variable, isExported ).asInstanceOf[Expression]
if (visited) {
 expressions.push( visited )

}

}
}
if (expressions.length) {
 return createStatement( inlineExpressions( expressions ), node )

}
return undefined

}
def transformVariable(node: VariableDeclaration, isExported: Boolean): ( VariableDeclaration | Expression ) = {
 hoistBindingElement( node, isExported )
if ((!node.initializer)) {
 return

}
val name = node.name
if (isIdentifier( name )) {
 return createAssignment( name, node.initializer )

}
else {
 return flattenVariableDestructuringToExpression( node, hoistVariableDeclaration )

}

}
def visitFunctionDeclaration(node: FunctionDeclaration): Node = {
 if (hasModifier( node, ModifierFlags.Export )) {
 val name = (node.name||getGeneratedNameForNode( node ))
val isAsync = hasModifier( node, ModifierFlags.Async )
val newNode = createFunctionDeclaration( undefined, (if (isAsync) Array( createNode( SyntaxKind.AsyncKeyword ).asInstanceOf[Modifier] ) else undefined), node.asteriskToken, name, undefined, node.parameters, undefined, node.body, node )
recordExportedFunctionDeclaration( node )
if ((!hasModifier( node, ModifierFlags.Default ))) {
 recordExportName( name )

}
setOriginalNode( newNode, node )
(node=newNode)

}
hoistFunctionDeclaration( node )
return undefined

}
def visitExpressionStatement(node: ExpressionStatement): VisitResult[Statement] = {
 val originalNode = getOriginalNode( node )
if (((((originalNode.kind===SyntaxKind.ModuleDeclaration)||(originalNode.kind===SyntaxKind.EnumDeclaration)))&&hasModifier( originalNode, ModifierFlags.Export ))) {
 val name = getDeclarationName( originalNode.asInstanceOf[( ModuleDeclaration | EnumDeclaration )] )
if ((originalNode.kind===SyntaxKind.EnumDeclaration)) {
 hoistVariableDeclaration( name )

}
return Array( node, createExportStatement( name, name ) )

}
return node

}
def visitClassDeclaration(node: ClassDeclaration): VisitResult[Statement] = {
 val name = getDeclarationName( node )
hoistVariableDeclaration( name )
val statements: Array[Statement] = Array()
statements.push( createStatement( createAssignment( name, createClassExpression( undefined, node.name, undefined, node.heritageClauses, node.members, node ) ), node ) )
if (hasModifier( node, ModifierFlags.Export )) {
 if ((!hasModifier( node, ModifierFlags.Default ))) {
 recordExportName( name )

}
statements.push( createDeclarationExport( node ) )

}
return statements

}
def shouldHoistLoopInitializer(node: ( VariableDeclarationList | Expression )) = {
 return (isVariableDeclarationList( node )&&(((getCombinedNodeFlags( node )&NodeFlags.BlockScoped))===0))

}
def visitForStatement(node: ForStatement): ForStatement = {
 val initializer = node.initializer
if (shouldHoistLoopInitializer( initializer )) {
 val expressions: Array[Expression] = Array()
((initializer.asInstanceOf[VariableDeclarationList]).declarations).foreach { fresh10 =>
val variable = zeroOfMyType
 = fresh10
 {
 val visited = transformVariable( variable, false ).asInstanceOf[Expression]
if (visited) {
 expressions.push( visited )

}

}
}
;
return createFor( (if (expressions.length) inlineExpressions( expressions ) else createSynthesizedNode( SyntaxKind.OmittedExpression ).asInstanceOf[OmittedExpression]), node.condition, node.incrementor, visitNode( node.statement, visitNestedNode, isStatement ), node )

}
else {
 return visitEachChild( node, visitNestedNode, context )

}

}
def transformForBinding(node: VariableDeclarationList): Expression = {
 val firstDeclaration = firstOrUndefined( node.declarations )
hoistBindingElement( firstDeclaration, false )
val name = firstDeclaration.name
return (if (isIdentifier( name )) name else flattenVariableDestructuringToExpression( firstDeclaration, hoistVariableDeclaration ))

}
def visitForInStatement(node: ForInStatement): ForInStatement = {
 val initializer = node.initializer
if (shouldHoistLoopInitializer( initializer )) {
 val updated = getMutableClone( node )
(updated.initializer=transformForBinding( initializer.asInstanceOf[VariableDeclarationList] ))
(updated.statement=visitNode( node.statement, visitNestedNode, isStatement, false, liftToBlock ))
return updated

}
else {
 return visitEachChild( node, visitNestedNode, context )

}

}
def visitForOfStatement(node: ForOfStatement): ForOfStatement = {
 val initializer = node.initializer
if (shouldHoistLoopInitializer( initializer )) {
 val updated = getMutableClone( node )
(updated.initializer=transformForBinding( initializer.asInstanceOf[VariableDeclarationList] ))
(updated.statement=visitNode( node.statement, visitNestedNode, isStatement, false, liftToBlock ))
return updated

}
else {
 return visitEachChild( node, visitNestedNode, context )

}

}
def visitDoStatement(node: DoStatement) = {
 val statement = visitNode( node.statement, visitNestedNode, isStatement, false, liftToBlock )
if ((statement!==node.statement)) {
 val updated = getMutableClone( node )
(updated.statement=statement)
return updated

}
return node

}
def visitWhileStatement(node: WhileStatement) = {
 val statement = visitNode( node.statement, visitNestedNode, isStatement, false, liftToBlock )
if ((statement!==node.statement)) {
 val updated = getMutableClone( node )
(updated.statement=statement)
return updated

}
return node

}
def visitLabeledStatement(node: LabeledStatement) = {
 val statement = visitNode( node.statement, visitNestedNode, isStatement, false, liftToBlock )
if ((statement!==node.statement)) {
 val updated = getMutableClone( node )
(updated.statement=statement)
return updated

}
return node

}
def visitWithStatement(node: WithStatement) = {
 val statement = visitNode( node.statement, visitNestedNode, isStatement, false, liftToBlock )
if ((statement!==node.statement)) {
 val updated = getMutableClone( node )
(updated.statement=statement)
return updated

}
return node

}
def visitSwitchStatement(node: SwitchStatement) = {
 val caseBlock = visitNode( node.caseBlock, visitNestedNode, isCaseBlock )
if ((caseBlock!==node.caseBlock)) {
 val updated = getMutableClone( node )
(updated.caseBlock=caseBlock)
return updated

}
return node

}
def visitCaseBlock(node: CaseBlock) = {
 val clauses = visitNodes( node.clauses, visitNestedNode, isCaseOrDefaultClause )
if ((clauses!==node.clauses)) {
 val updated = getMutableClone( node )
(updated.clauses=clauses)
return updated

}
return node

}
def visitCaseClause(node: CaseClause) = {
 val statements = visitNodes( node.statements, visitNestedNode, isStatement )
if ((statements!==node.statements)) {
 val updated = getMutableClone( node )
(updated.statements=statements)
return updated

}
return node

}
def visitDefaultClause(node: DefaultClause) = {
 return visitEachChild( node, visitNestedNode, context )

}
def visitTryStatement(node: TryStatement) = {
 return visitEachChild( node, visitNestedNode, context )

}
def visitCatchClause(node: CatchClause) = {
 val block = visitNode( node.block, visitNestedNode, isBlock )
if ((block!==node.block)) {
 val updated = getMutableClone( node )
(updated.block=block)
return updated

}
return node

}
def visitBlock(node: Block) = {
 return visitEachChild( node, visitNestedNode, context )

}
def onEmitNode(emitContext: EmitContext, node: Node, emitCallback: ((EmitContext, Node) => Unit)): Unit = {
 if ((node.kind===SyntaxKind.SourceFile)) {
 (exportFunctionForFile=exportFunctionForFileMap(getOriginalNodeId( node )))
previousOnEmitNode( emitContext, node, emitCallback )
(exportFunctionForFile=undefined)

}
else {
 previousOnEmitNode( emitContext, node, emitCallback )

}

}
def onSubstituteNode(emitContext: EmitContext, node: Node) = {
 (node=previousOnSubstituteNode( emitContext, node ))
if ((emitContext===EmitContext.Expression)) {
 return substituteExpression( node.asInstanceOf[Expression] )

}
return node

}
def substituteExpression(node: Expression) = {
 node.kind match {
  case  SyntaxKind.Identifier  =>
return substituteExpressionIdentifier( node.asInstanceOf[Identifier] )
  case  SyntaxKind.BinaryExpression  =>
return substituteBinaryExpression( node.asInstanceOf[BinaryExpression] )
  case  SyntaxKind.PrefixUnaryExpression | SyntaxKind.PostfixUnaryExpression  =>
return substituteUnaryExpression( node.asInstanceOf[( PrefixUnaryExpression | PostfixUnaryExpression )] )
  case _ =>
}
return node

}
def substituteExpressionIdentifier(node: Identifier): Expression = {
 val importDeclaration = resolver.getReferencedImportDeclaration( node )
if (importDeclaration) {
 val importBinding = createImportBinding( importDeclaration )
if (importBinding) {
 return importBinding

}

}
return node

}
def substituteBinaryExpression(node: BinaryExpression): Expression = {
 if (isAssignmentOperator( node.operatorToken.kind )) {
 return substituteAssignmentExpression( node )

}
return node

}
def substituteAssignmentExpression(node: BinaryExpression): Expression = {
 setEmitFlags( node, EmitFlags.NoSubstitution )
val left = node.left
left.kind match {
  case  SyntaxKind.Identifier  =>
val exportDeclaration = resolver.getReferencedExportContainer( left.asInstanceOf[Identifier] )
if (exportDeclaration) {
 return createExportExpression( left.asInstanceOf[Identifier], node )

}
  case  SyntaxKind.ObjectLiteralExpression | SyntaxKind.ArrayLiteralExpression  =>
if (hasExportedReferenceInDestructuringPattern( left.asInstanceOf[( ObjectLiteralExpression | ArrayLiteralExpression )] )) {
 return substituteDestructuring( node )

}
  case _ =>
}
return node

}
def isExportedBinding(name: Identifier) = {
 val container = resolver.getReferencedExportContainer( name )
return (container&&(container.kind===SyntaxKind.SourceFile))

}
def hasExportedReferenceInDestructuringPattern(node: ( ObjectLiteralExpression | ArrayLiteralExpression | Identifier )): Boolean = {
 node.kind match {
  case  SyntaxKind.Identifier  =>
return isExportedBinding( node.asInstanceOf[Identifier] )
  case  SyntaxKind.ObjectLiteralExpression  =>
((node.asInstanceOf[ObjectLiteralExpression]).properties).foreach { fresh11 =>
val property = zeroOfMyType
 = fresh11
 {
 if (hasExportedReferenceInObjectDestructuringElement( property )) {
 return true

}

}
}
  case  SyntaxKind.ArrayLiteralExpression  =>
((node.asInstanceOf[ArrayLiteralExpression]).elements).foreach { fresh12 =>
val element = zeroOfMyType
 = fresh12
 {
 if (hasExportedReferenceInArrayDestructuringElement( element )) {
 return true

}

}
}
  case _ =>
}
return false

}
def hasExportedReferenceInObjectDestructuringElement(node: ObjectLiteralElementLike): Boolean = {
 if (isShorthandPropertyAssignment( node )) {
 return isExportedBinding( node.name )

}
else if (isPropertyAssignment( node )) {
 return hasExportedReferenceInDestructuringElement( node.initializer )

}
else {
 return false

}

}
def hasExportedReferenceInArrayDestructuringElement(node: Expression): Boolean = {
 if (isSpreadElementExpression( node )) {
 val expression = node.expression
return (isIdentifier( expression )&&isExportedBinding( expression ))

}
else {
 return hasExportedReferenceInDestructuringElement( node )

}

}
def hasExportedReferenceInDestructuringElement(node: Expression): Boolean = {
 if (isBinaryExpression( node )) {
 val left = node.left
return (((node.operatorToken.kind===SyntaxKind.EqualsToken)&&isDestructuringPattern( left ))&&hasExportedReferenceInDestructuringPattern( left ))

}
else if (isIdentifier( node )) {
 return isExportedBinding( node )

}
else if (isSpreadElementExpression( node )) {
 val expression = node.expression
return (isIdentifier( expression )&&isExportedBinding( expression ))

}
else if (isDestructuringPattern( node )) {
 return hasExportedReferenceInDestructuringPattern( node )

}
else {
 return false

}

}
def isDestructuringPattern(node: Expression): Boolean = {
 val kind = node.kind
return (((kind===SyntaxKind.Identifier)||(kind===SyntaxKind.ObjectLiteralExpression))||(kind===SyntaxKind.ArrayLiteralExpression))

}
def substituteDestructuring(node: BinaryExpression) = {
 return flattenDestructuringAssignment( context, node, true, hoistVariableDeclaration )

}
def substituteUnaryExpression(node: ( PrefixUnaryExpression | PostfixUnaryExpression )): Expression = {
 val operand = node.operand
val operator = node.operator
val substitute = (isIdentifier( operand )&&(((node.kind===SyntaxKind.PostfixUnaryExpression)||(((node.kind===SyntaxKind.PrefixUnaryExpression)&&(((operator===SyntaxKind.PlusPlusToken)||(operator===SyntaxKind.MinusMinusToken))))))))
if (substitute) {
 val exportDeclaration = resolver.getReferencedExportContainer( operand.asInstanceOf[Identifier] )
if (exportDeclaration) {
 val expr = createPrefix( node.operator, operand, node )
setEmitFlags( expr, EmitFlags.NoSubstitution )
val call = createExportExpression( operand.asInstanceOf[Identifier], expr )
if ((node.kind===SyntaxKind.PrefixUnaryExpression)) {
 return call

}
else {
 return (if ((operator===SyntaxKind.PlusPlusToken)) createSubtract( call, createLiteral( 1 ) ) else createAdd( call, createLiteral( 1 ) ))

}

}

}
return node

}
def getDeclarationName(node: DeclarationStatement) = {
 return (if (node.name) getSynthesizedClone( node.name.asInstanceOf[Identifier] ) else getGeneratedNameForNode( node ))

}
def addExportStarFunction(statements: Array[Statement], localNames: Identifier) = {
 val exportStarFunction = createUniqueName( "exportStar" )
val m = createIdentifier( "m" )
val n = createIdentifier( "n" )
val exports = createIdentifier( "exports" )
var condition: Expression = createStrictInequality( n, createLiteral( "default" ) )
if (localNames) {
 (condition=createLogicalAnd( condition, createLogicalNot( createHasOwnProperty( localNames, n ) ) ))

}
statements.push( createFunctionDeclaration( undefined, undefined, undefined, exportStarFunction, undefined, Array( createParameter( m ) ), undefined, createBlock( Array( createVariableStatement( undefined, createVariableDeclarationList( Array( createVariableDeclaration( exports, undefined, createObjectLiteral( Array() ) ) ) ) ), createForIn( createVariableDeclarationList( Array( createVariableDeclaration( n, undefined ) ) ), m, createBlock( Array( setEmitFlags( createIf( condition, createStatement( createAssignment( createElementAccess( exports, n ), createElementAccess( m, n ) ) ) ), EmitFlags.SingleLine ) ) ) ), createStatement( createCall( exportFunctionForFile, undefined, Array( exports ) ) ) ), undefined, true ) ) )
return exportStarFunction

}
def createExportExpression(name: ( Identifier | StringLiteral ), value: Expression) = {
 val exportName = (if (isIdentifier( name )) createLiteral( name.text ) else name)
return createCall( exportFunctionForFile, undefined, Array( exportName, value ) )

}
def createExportStatement(name: ( Identifier | StringLiteral ), value: Expression) = {
 return createStatement( createExportExpression( name, value ) )

}
def createDeclarationExport(node: DeclarationStatement) = {
 val declarationName = getDeclarationName( node )
val exportName = (if (hasModifier( node, ModifierFlags.Default )) createLiteral( "default" ) else declarationName)
return createExportStatement( exportName, declarationName )

}
def createImportBinding(importDeclaration: Declaration): LeftHandSideExpression = {
 var importAlias: Identifier = zeroOfMyType
var name: Identifier = zeroOfMyType
if (isImportClause( importDeclaration )) {
 (importAlias=getGeneratedNameForNode( importDeclaration.parent ))
(name=createIdentifier( "default" ))

}
else if (isImportSpecifier( importDeclaration )) {
 (importAlias=getGeneratedNameForNode( importDeclaration.parent.parent.parent ))
(name=(importDeclaration.propertyName||importDeclaration.name))

}
else {
 return undefined

}
return createPropertyAccess( importAlias, getSynthesizedClone( name ) )

}
def collectDependencyGroups(externalImports: Array[( ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration )]) = {
 val groupIndices = createMap[ Int ]()
val dependencyGroups: Array[DependencyGroup] = Array()
{
var i = 0
while( (i<externalImports.length)) {
 {
 val externalImport = externalImports(i)
val externalModuleName = getExternalModuleNameLiteral( externalImport, currentSourceFile, host, resolver, compilerOptions )
val text = externalModuleName.text
if (hasProperty( groupIndices, text )) {
 val groupIndex = groupIndices(text)
dependencyGroups(groupIndex).externalImports.push( externalImport )
continue

}
else {
 (groupIndices(text)=dependencyGroups.length)
dependencyGroups.push( Map( "name" -> externalModuleName,
"externalImports" -> Array( externalImport ) ) )

}

}
 (i+= 1)
}
}
return dependencyGroups

}
def getNameOfDependencyGroup(dependencyGroup: DependencyGroup) = {
 return dependencyGroup.name

}
def recordExportName(name: Identifier) = {
 if ((!exportedLocalNames)) {
 (exportedLocalNames=Array())

}
exportedLocalNames.push( name )

}
def recordExportedFunctionDeclaration(node: FunctionDeclaration) = {
 if ((!exportedFunctionDeclarations)) {
 (exportedFunctionDeclarations=Array())

}
exportedFunctionDeclarations.push( createDeclarationExport( node ) )

}
def hoistBindingElement(node: ( VariableDeclaration | ArrayBindingElement ), isExported: Boolean): Unit = {
 if (isOmittedExpression( node )) {
 return

}
val name = node.name
if (isIdentifier( name )) {
 hoistVariableDeclaration( getSynthesizedClone( name ) )
if (isExported) {
 recordExportName( name )

}

}
else if (isBindingPattern( name )) {
 forEach( name.elements, (if (isExported) hoistExportedBindingElement else hoistNonExportedBindingElement) )

}

}
def hoistExportedBindingElement(node: ( VariableDeclaration | ArrayBindingElement )) = {
 hoistBindingElement( node, true )

}
def hoistNonExportedBindingElement(node: ( VariableDeclaration | ArrayBindingElement )) = {
 hoistBindingElement( node, false )

}
def updateSourceFile(node: SourceFile, statements: Array[Statement], nodeEmitFlags: EmitFlags) = {
 val updated = getMutableClone( node )
(updated.statements=createNodeArray( statements, node.statements ))
setEmitFlags( updated, nodeEmitFlags )
return updated

}

}
}
