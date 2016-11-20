package scalatscomp.transformers.module
object ES2015 {
  def transformES2015Module(context: TransformationContext) = {
    val compilerOptions = context.getCompilerOptions()
    return transformSourceFile
    def transformSourceFile(node: SourceFile) = {
      if (isDeclarationFile(node)) {
        return node

      }
      if ((isExternalModule(node) || compilerOptions.isolatedModules)) {
        return visitEachChild(node, visitor, context)

      }
      return node

    }
    def visitor(node: Node): VisitResult[Node] = {
      node.kind match {
        case SyntaxKind.ImportEqualsDeclaration =>
          return undefined
        case SyntaxKind.ExportAssignment =>
          return visitExportAssignment(node.asInstanceOf[ExportAssignment])
        case _ =>
      }
      return node

    }
    def visitExportAssignment(
        node: ExportAssignment): VisitResult[ExportAssignment] = {
      return (if (node.isExportEquals) undefined else node)

    }

  }
}
