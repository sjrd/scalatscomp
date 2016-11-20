package scalatscomp
object SourceMap {
trait SourceMapWriter {
  def initialize(filePath: String, sourceMapFilePath: String, sourceFiles: Array[SourceFile], isBundledEmit: Boolean): Unit
  def reset(): Unit
  def setSourceFile(sourceFile: SourceFile): Unit
  def emitPos(pos: Int): Unit
  def emitNodeWithSourceMap(emitContext: EmitContext, node: Node, emitCallback: ((EmitContext, Node) => Unit)): Unit
  def emitTokenWithSourceMap(node: Node, token: SyntaxKind, tokenStartPos: Int, emitCallback: ((SyntaxKind, Int) => Int)): Int
  def getText(): String
  def getSourceMappingURL(): String
  def getSourceMapData(): SourceMapData
}
val defaultLastEncodedSourceMapSpan: SourceMapSpan = Map( "emittedLine" -> 1,
"emittedColumn" -> 1,
"sourceLine" -> 1,
"sourceColumn" -> 1,
"sourceIndex" -> 0 )
def createSourceMapWriter(host: EmitHost, writer: EmitTextWriter): SourceMapWriter = {
 val compilerOptions = host.getCompilerOptions()
val extendedDiagnostics = compilerOptions.extendedDiagnostics
var currentSourceFile: SourceFile = zeroOfMyType
var currentSourceText: String = zeroOfMyType
var sourceMapDir: String = zeroOfMyType
var sourceMapSourceIndex: Int = zeroOfMyType
var lastRecordedSourceMapSpan: SourceMapSpan = zeroOfMyType
var lastEncodedSourceMapSpan: SourceMapSpan = zeroOfMyType
var lastEncodedNameIndex: Int = zeroOfMyType
var sourceMapData: SourceMapData = zeroOfMyType
var disabled: Boolean = (!((compilerOptions.sourceMap||compilerOptions.inlineSourceMap)))
return Map( "initialize" -> initialize,
"reset" -> reset,
"getSourceMapData" -> (() =>  sourceMapData),
"setSourceFile" -> setSourceFile,
"emitPos" -> emitPos,
"emitNodeWithSourceMap" -> emitNodeWithSourceMap,
"emitTokenWithSourceMap" -> emitTokenWithSourceMap,
"getText" -> getText,
"getSourceMappingURL" -> getSourceMappingURL )
def initialize(filePath: String, sourceMapFilePath: String, sourceFiles: Array[SourceFile], isBundledEmit: Boolean) = {
 if (disabled) {
 return

}
if (sourceMapData) {
 reset()

}
(currentSourceFile=undefined)
(currentSourceText=undefined)
(sourceMapSourceIndex=(-1))
(lastRecordedSourceMapSpan=undefined)
(lastEncodedSourceMapSpan=defaultLastEncodedSourceMapSpan)
(lastEncodedNameIndex=0)
(sourceMapData=Map( "sourceMapFilePath" -> sourceMapFilePath,
"jsSourceMappingURL" -> (if ((!compilerOptions.inlineSourceMap)) getBaseFileName( normalizeSlashes( sourceMapFilePath ) ) else undefined),
"sourceMapFile" -> getBaseFileName( normalizeSlashes( filePath ) ),
"sourceMapSourceRoot" -> (compilerOptions.sourceRoot||""),
"sourceMapSources" -> Array(),
"inputSourceFileNames" -> Array(),
"sourceMapNames" -> Array(),
"sourceMapMappings" -> "",
"sourceMapSourcesContent" -> (if (compilerOptions.inlineSources) Array() else undefined),
"sourceMapDecodedMappings" -> Array() ))
(sourceMapData.sourceMapSourceRoot=ts.normalizeSlashes( sourceMapData.sourceMapSourceRoot ))
if ((sourceMapData.sourceMapSourceRoot.length&&(sourceMapData.sourceMapSourceRoot.charCodeAt( (sourceMapData.sourceMapSourceRoot.length-1) )!==CharacterCodes.slash))) {
 (sourceMapData.sourceMapSourceRoot+=directorySeparator)

}
if (compilerOptions.mapRoot) {
 (sourceMapDir=normalizeSlashes( compilerOptions.mapRoot ))
if ((!isBundledEmit)) {
 Debug.assert( (sourceFiles.length===1) )
(sourceMapDir=getDirectoryPath( getSourceFilePathInNewDir( sourceFiles(0), host, sourceMapDir ) ))

}
if (((!isRootedDiskPath( sourceMapDir ))&&(!isUrl( sourceMapDir )))) {
 (sourceMapDir=combinePaths( host.getCommonSourceDirectory(), sourceMapDir ))
(sourceMapData.jsSourceMappingURL=getRelativePathToDirectoryOrUrl( getDirectoryPath( normalizePath( filePath ) ), combinePaths( sourceMapDir, sourceMapData.jsSourceMappingURL ), host.getCurrentDirectory(), host.getCanonicalFileName, true ))

}
else {
 (sourceMapData.jsSourceMappingURL=combinePaths( sourceMapDir, sourceMapData.jsSourceMappingURL ))

}

}
else {
 (sourceMapDir=getDirectoryPath( normalizePath( filePath ) ))

}

}
def reset() = {
 if (disabled) {
 return

}
(currentSourceFile=undefined)
(sourceMapDir=undefined)
(sourceMapSourceIndex=undefined)
(lastRecordedSourceMapSpan=undefined)
(lastEncodedSourceMapSpan=undefined)
(lastEncodedNameIndex=undefined)
(sourceMapData=undefined)

}
def encodeLastRecordedSourceMapSpan() = {
 if (((!lastRecordedSourceMapSpan)||(lastRecordedSourceMapSpan===lastEncodedSourceMapSpan))) {
 return

}
var prevEncodedEmittedColumn = lastEncodedSourceMapSpan.emittedColumn
if ((lastEncodedSourceMapSpan.emittedLine===lastRecordedSourceMapSpan.emittedLine)) {
 if (sourceMapData.sourceMapMappings) {
 (sourceMapData.sourceMapMappings+=",")

}

}
else {
 {
var encodedLine = lastEncodedSourceMapSpan.emittedLine
while( (encodedLine<lastRecordedSourceMapSpan.emittedLine)) {
 {
 (sourceMapData.sourceMapMappings+=";")

}
 (encodedLine+= 1)
}
}
(prevEncodedEmittedColumn=1)

}
(sourceMapData.sourceMapMappings+=base64VLQFormatEncode( (lastRecordedSourceMapSpan.emittedColumn-prevEncodedEmittedColumn) ))
(sourceMapData.sourceMapMappings+=base64VLQFormatEncode( (lastRecordedSourceMapSpan.sourceIndex-lastEncodedSourceMapSpan.sourceIndex) ))
(sourceMapData.sourceMapMappings+=base64VLQFormatEncode( (lastRecordedSourceMapSpan.sourceLine-lastEncodedSourceMapSpan.sourceLine) ))
(sourceMapData.sourceMapMappings+=base64VLQFormatEncode( (lastRecordedSourceMapSpan.sourceColumn-lastEncodedSourceMapSpan.sourceColumn) ))
if ((lastRecordedSourceMapSpan.nameIndex>=0)) {
 Debug.assert( false, "We do not support name index right now, Make sure to update updateLastEncodedAndRecordedSpans when we start using this" )
(sourceMapData.sourceMapMappings+=base64VLQFormatEncode( (lastRecordedSourceMapSpan.nameIndex-lastEncodedNameIndex) ))
(lastEncodedNameIndex=lastRecordedSourceMapSpan.nameIndex)

}
(lastEncodedSourceMapSpan=lastRecordedSourceMapSpan)
sourceMapData.sourceMapDecodedMappings.push( lastEncodedSourceMapSpan )

}
def emitPos(pos: Int) = {
 if ((disabled||positionIsSynthesized( pos ))) {
 return

}
if (extendedDiagnostics) {
 performance.mark( "beforeSourcemap" )

}
val sourceLinePos = getLineAndCharacterOfPosition( currentSourceFile, pos )
(sourceLinePos.line+= 1)
(sourceLinePos.character+= 1)
val emittedLine = writer.getLine()
val emittedColumn = writer.getColumn()
if (((((!lastRecordedSourceMapSpan)||(lastRecordedSourceMapSpan.emittedLine!==emittedLine))||(lastRecordedSourceMapSpan.emittedColumn!==emittedColumn))||(((lastRecordedSourceMapSpan.sourceIndex===sourceMapSourceIndex)&&(((lastRecordedSourceMapSpan.sourceLine>sourceLinePos.line)||(((lastRecordedSourceMapSpan.sourceLine===sourceLinePos.line)&&(lastRecordedSourceMapSpan.sourceColumn>sourceLinePos.character))))))))) {
 encodeLastRecordedSourceMapSpan()
(lastRecordedSourceMapSpan=Map( "emittedLine" -> emittedLine,
"emittedColumn" -> emittedColumn,
"sourceLine" -> sourceLinePos.line,
"sourceColumn" -> sourceLinePos.character,
"sourceIndex" -> sourceMapSourceIndex ))

}
else {
 (lastRecordedSourceMapSpan.sourceLine=sourceLinePos.line)
(lastRecordedSourceMapSpan.sourceColumn=sourceLinePos.character)
(lastRecordedSourceMapSpan.sourceIndex=sourceMapSourceIndex)

}
if (extendedDiagnostics) {
 performance.mark( "afterSourcemap" )
performance.measure( "Source Map", "beforeSourcemap", "afterSourcemap" )

}

}
def emitNodeWithSourceMap(emitContext: EmitContext, node: Node, emitCallback: ((EmitContext, Node) => Unit)) = {
 if (disabled) {
 return emitCallback( emitContext, node )

}
if (node) {
 val emitNode = node.emitNode
val emitFlags = (emitNode&&emitNode.flags)
const fresh1 = ((emitNode&&emitNode.sourceMapRange)||node)
val pos = fresh1.pos
val end = fresh1.end
if ((((node.kind!==SyntaxKind.NotEmittedStatement)&&(((emitFlags&EmitFlags.NoLeadingSourceMap))===0))&&(pos>=0))) {
 emitPos( skipTrivia( currentSourceText, pos ) )

}
if ((emitFlags&EmitFlags.NoNestedSourceMaps)) {
 (disabled=true)
emitCallback( emitContext, node )
(disabled=false)

}
else {
 emitCallback( emitContext, node )

}
if ((((node.kind!==SyntaxKind.NotEmittedStatement)&&(((emitFlags&EmitFlags.NoTrailingSourceMap))===0))&&(end>=0))) {
 emitPos( end )

}

}

}
def emitTokenWithSourceMap(node: Node, token: SyntaxKind, tokenPos: Int, emitCallback: ((SyntaxKind, Int) => Int)) = {
 if (disabled) {
 return emitCallback( token, tokenPos )

}
val emitNode = (node&&node.emitNode)
val emitFlags = (emitNode&&emitNode.flags)
val range = ((emitNode&&emitNode.tokenSourceMapRanges)&&emitNode.tokenSourceMapRanges(token))
(tokenPos=skipTrivia( currentSourceText, (if (range) range.pos else tokenPos) ))
if (((((emitFlags&EmitFlags.NoTokenLeadingSourceMaps))===0)&&(tokenPos>=0))) {
 emitPos( tokenPos )

}
(tokenPos=emitCallback( token, tokenPos ))
if (range)
(tokenPos=range.end)
if (((((emitFlags&EmitFlags.NoTokenTrailingSourceMaps))===0)&&(tokenPos>=0))) {
 emitPos( tokenPos )

}
return tokenPos

}
def setSourceFile(sourceFile: SourceFile) = {
 if (disabled) {
 return

}
(currentSourceFile=sourceFile)
(currentSourceText=currentSourceFile.text)
val sourcesDirectoryPath = (if (compilerOptions.sourceRoot) host.getCommonSourceDirectory() else sourceMapDir)
val source = getRelativePathToDirectoryOrUrl( sourcesDirectoryPath, currentSourceFile.fileName, host.getCurrentDirectory(), host.getCanonicalFileName, true )
(sourceMapSourceIndex=indexOf( sourceMapData.sourceMapSources, source ))
if ((sourceMapSourceIndex===(-1))) {
 (sourceMapSourceIndex=sourceMapData.sourceMapSources.length)
sourceMapData.sourceMapSources.push( source )
sourceMapData.inputSourceFileNames.push( currentSourceFile.fileName )
if (compilerOptions.inlineSources) {
 sourceMapData.sourceMapSourcesContent.push( currentSourceFile.text )

}

}

}
def getText() = {
 if (disabled) {
 return

}
encodeLastRecordedSourceMapSpan()
return stringify( Map( "version" -> 3,
"file" -> sourceMapData.sourceMapFile,
"sourceRoot" -> sourceMapData.sourceMapSourceRoot,
"sources" -> sourceMapData.sourceMapSources,
"names" -> sourceMapData.sourceMapNames,
"mappings" -> sourceMapData.sourceMapMappings,
"sourcesContent" -> sourceMapData.sourceMapSourcesContent ) )

}
def getSourceMappingURL() = {
 if (disabled) {
 return

}
if (compilerOptions.inlineSourceMap) {
 val base64SourceMapText = convertToBase64( getText() )
return (sourceMapData.jsSourceMappingURL=s"""data:application/json;base64,${ base64SourceMapText}""" )

}
else {
 return sourceMapData.jsSourceMappingURL

}

}

}
val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
def base64FormatEncode(inValue: Int) = {
 if ((inValue<64)) {
 return base64Chars.charAt( inValue )

}
throw TypeError( (inValue+": not a 64 based value") )
}
def base64VLQFormatEncode(inValue: Int) = {
 if ((inValue<0)) {
 (inValue=(((((-inValue))<<1))+1))

}
else {
 (inValue=(inValue<<1))

}
var encodedStr = ""
do {
{
 var currentDigit = (inValue&31)
(inValue=(inValue>>5))
if ((inValue>0)) {
 (currentDigit=(currentDigit|32))

}
(encodedStr=(encodedStr+base64FormatEncode( currentDigit )))

}
} while ((inValue>0))
return encodedStr

}
}
