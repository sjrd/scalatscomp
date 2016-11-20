package scalatscomp
object Comment {
  trait CommentWriter {
    def reset(): Unit
    def setSourceFile(sourceFile: SourceFile): Unit
    def emitNodeWithComments(emitContext: EmitContext,
                             node: Node,
                             emitCallback: ((EmitContext, Node) => Unit)): Unit
    def emitBodyWithDetachedComments(node: Node,
                                     detachedRange: TextRange,
                                     emitCallback: ((Node) => Unit)): Unit
    def emitTrailingCommentsOfPosition(pos: Int): Unit
  }
  def createCommentWriter(host: EmitHost,
                          writer: EmitTextWriter,
                          sourceMap: SourceMapWriter): CommentWriter = {
    val compilerOptions = host.getCompilerOptions()
    val extendedDiagnostics = compilerOptions.extendedDiagnostics
    val newLine = host.getNewLine()
    const fresh1 = sourceMap
    val emitPos = fresh1.emitPos
    var containerPos = (-1)
    var containerEnd = (-1)
    var declarationListContainerEnd = (-1)
    var currentSourceFile: SourceFile = zeroOfMyType
    var currentText: String = zeroOfMyType
    var currentLineMap: Array[Int] = zeroOfMyType
    var detachedCommentsInfo: Array[{
      var nodePos: Int
      var detachedCommentEndPos: Int
    }] = zeroOfMyType
    var hasWrittenComment = false
    var disabled: Boolean = compilerOptions.removeComments
    return Map(
      "reset" -> reset,
      "setSourceFile" -> setSourceFile,
      "emitNodeWithComments" -> emitNodeWithComments,
      "emitBodyWithDetachedComments" -> emitBodyWithDetachedComments,
      "emitTrailingCommentsOfPosition" -> emitTrailingCommentsOfPosition)
    def emitNodeWithComments(emitContext: EmitContext,
                             node: Node,
                             emitCallback: ((EmitContext, Node) => Unit)) = {
      if (disabled) {
        emitCallback(emitContext, node)
        return

      }
      if (node) {
        const fresh2 = getCommentRange(node)
        val pos = fresh2.pos
        val end = fresh2.end
        val emitFlags = getEmitFlags(node)
        if (((((pos < 0) && (end < 0))) || ((pos === end)))) {
          if ((emitFlags & EmitFlags.NoNestedComments)) {
            (disabled = true)
            emitCallback(emitContext, node)
            (disabled = false)

          } else {
            emitCallback(emitContext, node)

          }

        } else {
          if (extendedDiagnostics) {
            performance.mark("preEmitNodeWithComment")

          }
          val isEmittedNode = (node.kind !== SyntaxKind.NotEmittedStatement)
          val skipLeadingComments = ((pos < 0) || (((emitFlags & EmitFlags.NoLeadingComments)) !== 0))
          val skipTrailingComments = ((end < 0) || (((emitFlags & EmitFlags.NoTrailingComments)) !== 0))
          if ((!skipLeadingComments)) {
            emitLeadingComments(pos, isEmittedNode)

          }
          val savedContainerPos = containerPos
          val savedContainerEnd = containerEnd
          val savedDeclarationListContainerEnd = declarationListContainerEnd
          if ((!skipLeadingComments)) {
            (containerPos = pos)

          }
          if ((!skipTrailingComments)) {
            (containerEnd = end)
            if ((node.kind === SyntaxKind.VariableDeclarationList)) {
              (declarationListContainerEnd = end)

            }

          }
          if (extendedDiagnostics) {
            performance.measure("commentTime", "preEmitNodeWithComment")

          }
          if ((emitFlags & EmitFlags.NoNestedComments)) {
            (disabled = true)
            emitCallback(emitContext, node)
            (disabled = false)

          } else {
            emitCallback(emitContext, node)

          }
          if (extendedDiagnostics) {
            performance.mark("beginEmitNodeWithComment")

          }
          (containerPos = savedContainerPos)
          (containerEnd = savedContainerEnd)
          (declarationListContainerEnd = savedDeclarationListContainerEnd)
          if (((!skipTrailingComments) && isEmittedNode)) {
            emitTrailingComments(end)

          }
          if (extendedDiagnostics) {
            performance.measure("commentTime", "beginEmitNodeWithComment")

          }

        }

      }

    }
    def emitBodyWithDetachedComments(node: Node,
                                     detachedRange: TextRange,
                                     emitCallback: ((Node) => Unit)) = {
      if (extendedDiagnostics) {
        performance.mark("preEmitBodyWithDetachedComments")

      }
      const fresh3 = detachedRange
      val pos = fresh3.pos
      val end = fresh3.end
      val emitFlags = getEmitFlags(node)
      val skipLeadingComments = ((pos < 0) || (((emitFlags & EmitFlags.NoLeadingComments)) !== 0))
      val skipTrailingComments = ((disabled || (end < 0)) || (((emitFlags & EmitFlags.NoTrailingComments)) !== 0))
      if ((!skipLeadingComments)) {
        emitDetachedCommentsAndUpdateCommentsInfo(detachedRange)

      }
      if (extendedDiagnostics) {
        performance.measure("commentTime", "preEmitBodyWithDetachedComments")

      }
      if (((emitFlags & EmitFlags.NoNestedComments) && (!disabled))) {
        (disabled = true)
        emitCallback(node)
        (disabled = false)

      } else {
        emitCallback(node)

      }
      if (extendedDiagnostics) {
        performance.mark("beginEmitBodyWithDetachedCommetns")

      }
      if ((!skipTrailingComments)) {
        emitLeadingComments(detachedRange.end, true)

      }
      if (extendedDiagnostics) {
        performance.measure("commentTime", "beginEmitBodyWithDetachedCommetns")

      }

    }
    def emitLeadingComments(pos: Int, isEmittedNode: Boolean) = {
      (hasWrittenComment = false)
      if (isEmittedNode) {
        forEachLeadingCommentToEmit(pos, emitLeadingComment)

      } else if ((pos === 0)) {
        forEachLeadingCommentToEmit(pos, emitTripleSlashLeadingComment)

      }

    }
    def emitTripleSlashLeadingComment(commentPos: Int,
                                      commentEnd: Int,
                                      kind: SyntaxKind,
                                      hasTrailingNewLine: Boolean,
                                      rangePos: Int) = {
      if (isTripleSlashComment(commentPos, commentEnd)) {
        emitLeadingComment(
          commentPos,
          commentEnd,
          kind,
          hasTrailingNewLine,
          rangePos)

      }

    }
    def emitLeadingComment(commentPos: Int,
                           commentEnd: Int,
                           _kind: SyntaxKind,
                           hasTrailingNewLine: Boolean,
                           rangePos: Int) = {
      if ((!hasWrittenComment)) {
        emitNewLineBeforeLeadingCommentOfPosition(
          currentLineMap,
          writer,
          rangePos,
          commentPos)
        (hasWrittenComment = true)

      }
      emitPos(commentPos)
      writeCommentRange(
        currentText,
        currentLineMap,
        writer,
        commentPos,
        commentEnd,
        newLine)
      emitPos(commentEnd)
      if (hasTrailingNewLine) {
        writer.writeLine()

      } else {
        writer.write(" ")

      }

    }
    def emitTrailingComments(pos: Int) = {
      forEachTrailingCommentToEmit(pos, emitTrailingComment)

    }
    def emitTrailingComment(commentPos: Int,
                            commentEnd: Int,
                            _kind: SyntaxKind,
                            hasTrailingNewLine: Boolean) = {
      if ((!writer.isAtStartOfLine())) {
        writer.write(" ")

      }
      emitPos(commentPos)
      writeCommentRange(
        currentText,
        currentLineMap,
        writer,
        commentPos,
        commentEnd,
        newLine)
      emitPos(commentEnd)
      if (hasTrailingNewLine) {
        writer.writeLine()

      }

    }
    def emitTrailingCommentsOfPosition(pos: Int) = {
      if (disabled) {
        return

      }
      if (extendedDiagnostics) {
        performance.mark("beforeEmitTrailingCommentsOfPosition")

      }
      forEachTrailingCommentToEmit(pos, emitTrailingCommentOfPosition)
      if (extendedDiagnostics) {
        performance
          .measure("commentTime", "beforeEmitTrailingCommentsOfPosition")

      }

    }
    def emitTrailingCommentOfPosition(commentPos: Int,
                                      commentEnd: Int,
                                      _kind: SyntaxKind,
                                      hasTrailingNewLine: Boolean) = {
      emitPos(commentPos)
      writeCommentRange(
        currentText,
        currentLineMap,
        writer,
        commentPos,
        commentEnd,
        newLine)
      emitPos(commentEnd)
      if (hasTrailingNewLine) {
        writer.writeLine()

      } else {
        writer.write(" ")

      }

    }
    def forEachLeadingCommentToEmit(pos: Int,
                                    cb: ((Int, Int, SyntaxKind, Boolean,
                                          Int) => Unit)) = {
      if (((containerPos === (-1)) || (pos !== containerPos))) {
        if (hasDetachedComments(pos)) {
          forEachLeadingCommentWithoutDetachedComments(cb)

        } else {
          forEachLeadingCommentRange(currentText, pos, cb, pos)

        }

      }

    }
    def forEachTrailingCommentToEmit(end: Int,
                                     cb: ((Int, Int, SyntaxKind,
                                           Boolean) => Unit)) = {
      if (((containerEnd === (-1)) || (((end !== containerEnd) && (end !== declarationListContainerEnd))))) {
        forEachTrailingCommentRange(currentText, end, cb)

      }

    }
    def reset() = {
      (currentSourceFile = undefined)
      (currentText = undefined)
      (currentLineMap = undefined)
      (detachedCommentsInfo = undefined)

    }
    def setSourceFile(sourceFile: SourceFile) = {
      (currentSourceFile = sourceFile)
      (currentText = currentSourceFile.text)
      (currentLineMap = getLineStarts(currentSourceFile))
      (detachedCommentsInfo = undefined)

    }
    def hasDetachedComments(pos: Int) = {
      return ((detachedCommentsInfo !== undefined) && (lastOrUndefined(
        detachedCommentsInfo).nodePos === pos))

    }
    def forEachLeadingCommentWithoutDetachedComments(
        cb: ((Int, Int, SyntaxKind, Boolean, Int) => Unit)) = {
      val pos = lastOrUndefined(detachedCommentsInfo).detachedCommentEndPos
      if ((detachedCommentsInfo.length - 1)) {
        detachedCommentsInfo.pop()

      } else {
        (detachedCommentsInfo = undefined)

      }
      forEachLeadingCommentRange(currentText, pos, cb, pos)

    }
    def emitDetachedCommentsAndUpdateCommentsInfo(range: TextRange) = {
      val currentDetachedCommentInfo = emitDetachedComments(
        currentText,
        currentLineMap,
        writer,
        writeComment,
        range,
        newLine,
        disabled)
      if (currentDetachedCommentInfo) {
        if (detachedCommentsInfo) {
          detachedCommentsInfo.push(currentDetachedCommentInfo)

        } else {
          (detachedCommentsInfo = Array(currentDetachedCommentInfo))

        }

      }

    }
    def writeComment(text: String,
                     lineMap: Array[Int],
                     writer: EmitTextWriter,
                     commentPos: Int,
                     commentEnd: Int,
                     newLine: String) = {
      emitPos(commentPos)
      writeCommentRange(text, lineMap, writer, commentPos, commentEnd, newLine)
      emitPos(commentEnd)

    }
    def isTripleSlashComment(commentPos: Int, commentEnd: Int) = {
      if ((((currentText.charCodeAt((commentPos + 1)) === CharacterCodes.slash) && ((commentPos + 2) < commentEnd)) && (currentText
            .charCodeAt((commentPos + 2)) === CharacterCodes.slash))) {
        val textSubStr = currentText.substring(commentPos, commentEnd)
        return (if ((textSubStr.`match`(fullTripleSlashReferencePathRegEx) || textSubStr
                      .`match`(fullTripleSlashAMDReferencePathRegEx))) true
                else false)

      }
      return false

    }

  }
}
