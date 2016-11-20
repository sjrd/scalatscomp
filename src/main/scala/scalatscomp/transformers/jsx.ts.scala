package scalatscomp.transformers
object JSX {
val entities: Map[Int] = createEntitiesMap()
def transformJsx(context: TransformationContext) = {
 val compilerOptions = context.getCompilerOptions()
var currentSourceFile: SourceFile = zeroOfMyType
return transformSourceFile
def transformSourceFile(node: SourceFile) = {
 if (isDeclarationFile( node )) {
 return node

}
(currentSourceFile=node)
(node=visitEachChild( node, visitor, context ))
(currentSourceFile=undefined)
return node

}
def visitor(node: Node): VisitResult[Node] = {
 if ((node.transformFlags&TransformFlags.Jsx)) {
 return visitorWorker( node )

}
else if ((node.transformFlags&TransformFlags.ContainsJsx)) {
 return visitEachChild( node, visitor, context )

}
else {
 return node

}

}
def visitorWorker(node: Node): VisitResult[Node] = {
 node.kind match {
  case  SyntaxKind.JsxElement  =>
return visitJsxElement( node.asInstanceOf[JsxElement], false )
  case  SyntaxKind.JsxSelfClosingElement  =>
return visitJsxSelfClosingElement( node.asInstanceOf[JsxSelfClosingElement], false )
  case  SyntaxKind.JsxExpression  =>
return visitJsxExpression( node.asInstanceOf[JsxExpression] )
  case _ =>
Debug.failBadSyntaxKind( node )
return undefined
}

}
def transformJsxChildToExpression(node: JsxChild): Expression = {
 node.kind match {
  case  SyntaxKind.JsxText  =>
return visitJsxText( node.asInstanceOf[JsxText] )
  case  SyntaxKind.JsxExpression  =>
return visitJsxExpression( node.asInstanceOf[JsxExpression] )
  case  SyntaxKind.JsxElement  =>
return visitJsxElement( node.asInstanceOf[JsxElement], true )
  case  SyntaxKind.JsxSelfClosingElement  =>
return visitJsxSelfClosingElement( node.asInstanceOf[JsxSelfClosingElement], true )
  case _ =>
Debug.failBadSyntaxKind( node )
return undefined
}

}
def visitJsxElement(node: JsxElement, isChild: Boolean) = {
 return visitJsxOpeningLikeElement( node.openingElement, node.children, isChild, node )

}
def visitJsxSelfClosingElement(node: JsxSelfClosingElement, isChild: Boolean) = {
 return visitJsxOpeningLikeElement( node, undefined, isChild, node )

}
def visitJsxOpeningLikeElement(node: JsxOpeningLikeElement, children: Array[JsxChild], isChild: Boolean, location: TextRange) = {
 val tagName = getTagName( node )
var objectProperties: Expression = zeroOfMyType
val attrs = node.attributes
if ((attrs.length===0)) {
 (objectProperties=createNull())

}
else {
 val segments = flatten( spanMap( attrs, isJsxSpreadAttribute, (( attrs, isSpread ) =>  (if (isSpread) map( attrs, transformJsxSpreadAttributeToExpression ) else createObjectLiteral( map( attrs, transformJsxAttributeToObjectLiteralElement ) ))) ) )
if (isJsxSpreadAttribute( attrs(0) )) {
 segments.unshift( createObjectLiteral() )

}
(objectProperties=(singleOrUndefined( segments )||createAssignHelper( currentSourceFile.externalHelpersModuleName, segments )))

}
val element = createReactCreateElement( compilerOptions.reactNamespace, tagName, objectProperties, filter( map( children, transformJsxChildToExpression ), isDefined ), node, location )
if (isChild) {
 startOnNewLine( element )

}
return element

}
def transformJsxSpreadAttributeToExpression(node: JsxSpreadAttribute) = {
 return visitNode( node.expression, visitor, isExpression )

}
def transformJsxAttributeToObjectLiteralElement(node: JsxAttribute) = {
 val name = getAttributeName( node )
val expression = transformJsxAttributeInitializer( node.initializer )
return createPropertyAssignment( name, expression )

}
def transformJsxAttributeInitializer(node: ( StringLiteral | JsxExpression )) = {
 if ((node===undefined)) {
 return createLiteral( true )

}
else if ((node.kind===SyntaxKind.StringLiteral)) {
 val decoded = tryDecodeEntities( (node.asInstanceOf[StringLiteral]).text )
return (if (decoded) createLiteral( decoded, node ) else node)

}
else if ((node.kind===SyntaxKind.JsxExpression)) {
 return visitJsxExpression( node.asInstanceOf[JsxExpression] )

}
else {
 Debug.failBadSyntaxKind( node )

}

}
def visitJsxText(node: JsxText) = {
 val text = getTextOfNode( node, true )
var parts: Array[Expression] = zeroOfMyType
var firstNonWhitespace = 0
var lastNonWhitespace = (-1)
{
var i = 0
while( (i<text.length)) {
 {
 val c = text.charCodeAt( i )
if (isLineBreak( c )) {
 if (((firstNonWhitespace!==(-1))&&((((lastNonWhitespace-firstNonWhitespace)+1)>0)))) {
 val part = text.substr( firstNonWhitespace, ((lastNonWhitespace-firstNonWhitespace)+1) )
if ((!parts)) {
 (parts=Array())

}
parts.push( createLiteral( decodeEntities( part ) ) )

}
(firstNonWhitespace=(-1))

}
else if ((!isWhiteSpace( c ))) {
 (lastNonWhitespace=i)
if ((firstNonWhitespace===(-1))) {
 (firstNonWhitespace=i)

}

}

}
 (i+= 1)
}
}
if ((firstNonWhitespace!==(-1))) {
 val part = text.substr( firstNonWhitespace )
if ((!parts)) {
 (parts=Array())

}
parts.push( createLiteral( decodeEntities( part ) ) )

}
if (parts) {
 return reduceLeft( parts, aggregateJsxTextParts )

}
return undefined

}
def aggregateJsxTextParts(left: Expression, right: Expression) = {
 return createAdd( createAdd( left, createLiteral( " " ) ), right )

}
def decodeEntities(text: String): String = {
 return text.replace( java.util.regex.Pattern.compile(raw"""&((#((\d+)|x([\da-fA-F]+)))|(\w+));""", "g"), (( `match`, _all, _number, _digits, decimal, hex, word ) =>  {
 if (decimal) {
 return String.fromCharCode( parseInt( decimal, 10 ) )

}
else if (hex) {
 return String.fromCharCode( parseInt( hex, 16 ) )

}
else {
 val ch = entities(word)
return (if (ch) String.fromCharCode( ch ) else `match`)

}

}) )

}
def tryDecodeEntities(text: String): ( String | undefined ) = {
 val decoded = decodeEntities( text )
return (if ((decoded===text)) undefined else decoded)

}
def getTagName(node: ( JsxElement | JsxOpeningLikeElement )): Expression = {
 if ((node.kind===SyntaxKind.JsxElement)) {
 return getTagName( (node.asInstanceOf[JsxElement]).openingElement )

}
else {
 val name = (node.asInstanceOf[JsxOpeningLikeElement]).tagName
if ((isIdentifier( name )&&isIntrinsicJsxName( name.text ))) {
 return createLiteral( name.text )

}
else {
 return createExpressionFromEntityName( name )

}

}

}
def getAttributeName(node: JsxAttribute): ( StringLiteral | Identifier ) = {
 val name = node.name
if (java.util.regex.Pattern.compile(raw"""^[A-Za-z_]\w*$$""").test( name.text )) {
 return name

}
else {
 return createLiteral( name.text )

}

}
def visitJsxExpression(node: JsxExpression) = {
 return visitNode( node.expression, visitor, isExpression )

}

}
def createEntitiesMap(): Map[Int] = {
 return createMap[ Int ]( Map( "quot" -> 34,
"amp" -> 38,
"apos" -> 39,
"lt" -> 60,
"gt" -> 62,
"nbsp" -> 160,
"iexcl" -> 161,
"cent" -> 162,
"pound" -> 163,
"curren" -> 164,
"yen" -> 165,
"brvbar" -> 166,
"sect" -> 167,
"uml" -> 168,
"copy" -> 169,
"ordf" -> 170,
"laquo" -> 171,
"not" -> 172,
"shy" -> 173,
"reg" -> 174,
"macr" -> 175,
"deg" -> 176,
"plusmn" -> 177,
"sup2" -> 178,
"sup3" -> 179,
"acute" -> 180,
"micro" -> 181,
"para" -> 182,
"middot" -> 183,
"cedil" -> 184,
"sup1" -> 185,
"ordm" -> 186,
"raquo" -> 187,
"frac14" -> 188,
"frac12" -> 189,
"frac34" -> 190,
"iquest" -> 191,
"Agrave" -> 192,
"Aacute" -> 193,
"Acirc" -> 194,
"Atilde" -> 195,
"Auml" -> 196,
"Aring" -> 197,
"AElig" -> 198,
"Ccedil" -> 199,
"Egrave" -> 200,
"Eacute" -> 201,
"Ecirc" -> 202,
"Euml" -> 203,
"Igrave" -> 204,
"Iacute" -> 205,
"Icirc" -> 206,
"Iuml" -> 207,
"ETH" -> 208,
"Ntilde" -> 209,
"Ograve" -> 210,
"Oacute" -> 211,
"Ocirc" -> 212,
"Otilde" -> 213,
"Ouml" -> 214,
"times" -> 215,
"Oslash" -> 216,
"Ugrave" -> 217,
"Uacute" -> 218,
"Ucirc" -> 219,
"Uuml" -> 220,
"Yacute" -> 221,
"THORN" -> 222,
"szlig" -> 223,
"agrave" -> 224,
"aacute" -> 225,
"acirc" -> 226,
"atilde" -> 227,
"auml" -> 228,
"aring" -> 229,
"aelig" -> 230,
"ccedil" -> 231,
"egrave" -> 232,
"eacute" -> 233,
"ecirc" -> 234,
"euml" -> 235,
"igrave" -> 236,
"iacute" -> 237,
"icirc" -> 238,
"iuml" -> 239,
"eth" -> 240,
"ntilde" -> 241,
"ograve" -> 242,
"oacute" -> 243,
"ocirc" -> 244,
"otilde" -> 245,
"ouml" -> 246,
"divide" -> 247,
"oslash" -> 248,
"ugrave" -> 249,
"uacute" -> 250,
"ucirc" -> 251,
"uuml" -> 252,
"yacute" -> 253,
"thorn" -> 254,
"yuml" -> 255,
"OElig" -> 338,
"oelig" -> 339,
"Scaron" -> 352,
"scaron" -> 353,
"Yuml" -> 376,
"fnof" -> 402,
"circ" -> 710,
"tilde" -> 732,
"Alpha" -> 913,
"Beta" -> 914,
"Gamma" -> 915,
"Delta" -> 916,
"Epsilon" -> 917,
"Zeta" -> 918,
"Eta" -> 919,
"Theta" -> 920,
"Iota" -> 921,
"Kappa" -> 922,
"Lambda" -> 923,
"Mu" -> 924,
"Nu" -> 925,
"Xi" -> 926,
"Omicron" -> 927,
"Pi" -> 928,
"Rho" -> 929,
"Sigma" -> 931,
"Tau" -> 932,
"Upsilon" -> 933,
"Phi" -> 934,
"Chi" -> 935,
"Psi" -> 936,
"Omega" -> 937,
"alpha" -> 945,
"beta" -> 946,
"gamma" -> 947,
"delta" -> 948,
"epsilon" -> 949,
"zeta" -> 950,
"eta" -> 951,
"theta" -> 952,
"iota" -> 953,
"kappa" -> 954,
"lambda" -> 955,
"mu" -> 956,
"nu" -> 957,
"xi" -> 958,
"omicron" -> 959,
"pi" -> 960,
"rho" -> 961,
"sigmaf" -> 962,
"sigma" -> 963,
"tau" -> 964,
"upsilon" -> 965,
"phi" -> 966,
"chi" -> 967,
"psi" -> 968,
"omega" -> 969,
"thetasym" -> 977,
"upsih" -> 978,
"piv" -> 982,
"ensp" -> 8194,
"emsp" -> 8195,
"thinsp" -> 8201,
"zwnj" -> 8204,
"zwj" -> 8205,
"lrm" -> 8206,
"rlm" -> 8207,
"ndash" -> 8211,
"mdash" -> 8212,
"lsquo" -> 8216,
"rsquo" -> 8217,
"sbquo" -> 8218,
"ldquo" -> 8220,
"rdquo" -> 8221,
"bdquo" -> 8222,
"dagger" -> 8224,
"Dagger" -> 8225,
"bull" -> 8226,
"hellip" -> 8230,
"permil" -> 8240,
"prime" -> 8242,
"Prime" -> 8243,
"lsaquo" -> 8249,
"rsaquo" -> 8250,
"oline" -> 8254,
"frasl" -> 8260,
"euro" -> 8364,
"image" -> 8465,
"weierp" -> 8472,
"real" -> 8476,
"trade" -> 8482,
"alefsym" -> 8501,
"larr" -> 8592,
"uarr" -> 8593,
"rarr" -> 8594,
"darr" -> 8595,
"harr" -> 8596,
"crarr" -> 8629,
"lArr" -> 8656,
"uArr" -> 8657,
"rArr" -> 8658,
"dArr" -> 8659,
"hArr" -> 8660,
"forall" -> 8704,
"part" -> 8706,
"exist" -> 8707,
"empty" -> 8709,
"nabla" -> 8711,
"isin" -> 8712,
"notin" -> 8713,
"ni" -> 8715,
"prod" -> 8719,
"sum" -> 8721,
"minus" -> 8722,
"lowast" -> 8727,
"radic" -> 8730,
"prop" -> 8733,
"infin" -> 8734,
"ang" -> 8736,
"and" -> 8743,
"or" -> 8744,
"cap" -> 8745,
"cup" -> 8746,
"int" -> 8747,
"there4" -> 8756,
"sim" -> 8764,
"cong" -> 8773,
"asymp" -> 8776,
"ne" -> 8800,
"equiv" -> 8801,
"le" -> 8804,
"ge" -> 8805,
"sub" -> 8834,
"sup" -> 8835,
"nsub" -> 8836,
"sube" -> 8838,
"supe" -> 8839,
"oplus" -> 8853,
"otimes" -> 8855,
"perp" -> 8869,
"sdot" -> 8901,
"lceil" -> 8968,
"rceil" -> 8969,
"lfloor" -> 8970,
"rfloor" -> 8971,
"lang" -> 9001,
"rang" -> 9002,
"loz" -> 9674,
"spades" -> 9824,
"clubs" -> 9827,
"hearts" -> 9829,
"diams" -> 9830 ) )

}
}
