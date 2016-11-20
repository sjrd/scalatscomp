package scalatscomp

import scala.collection.mutable

object Types {
  type Path = String

  class FileMap[T](keyMapper: Option[String => String]) {
    private var files = mutable.Map.empty[String, T]

    def forEachValueInMap(f: ((Path, T) => Unit)) =
      files.keys.foreach { key =>
          f(key, files(key))
      }
    def getKeys() =
      files.keys
    def get(path: Path): T =
      files(toKey(path))
    def set(path: Path, value: T) =
      files(toKey(path)) = value
    def contains(path: Path) =
      files.contains(toKey(path))
    def remove(path: Path) = {
      val key = toKey(path)
      files.remove(key)
    }
    def clear() =
      files = mutable.Map.empty[String, T]
    def toKey(path: Path): String =
      keyMapper.fold(path)(_(path))
  }

  trait TextRange {
    var pos: Int = -1
    var end: Int = -1
  }

  class Symbol(val flags: SymbolFlags, val name: String) {
    var declarations: Option[Any] = None
  }

  class Type(_checker: TypeChecker, val flags: TypeFlags)

  class Signature

  sealed abstract class SyntaxKind
  object SyntaxKind {
    case object Unknown extends SyntaxKind
    case object EndOfFileToken extends SyntaxKind
    case object SingleLineCommentTrivia extends SyntaxKind
    case object MultiLineCommentTrivia extends SyntaxKind
    case object NewLineTrivia extends SyntaxKind
    case object WhitespaceTrivia extends SyntaxKind
    case object ShebangTrivia extends SyntaxKind
    case object ConflictMarkerTrivia extends SyntaxKind
    case object NumericLiteral extends SyntaxKind
    case object StringLiteral extends SyntaxKind
    case object JsxText extends SyntaxKind
    case object RegularExpressionLiteral extends SyntaxKind
    case object NoSubstitutionTemplateLiteral extends SyntaxKind
    case object TemplateHead extends SyntaxKind
    case object TemplateMiddle extends SyntaxKind
    case object TemplateTail extends SyntaxKind
    case object OpenBraceToken extends SyntaxKind
    case object CloseBraceToken extends SyntaxKind
    case object OpenParenToken extends SyntaxKind
    case object CloseParenToken extends SyntaxKind
    case object OpenBracketToken extends SyntaxKind
    case object CloseBracketToken extends SyntaxKind
    case object DotToken extends SyntaxKind
    case object DotDotDotToken extends SyntaxKind
    case object SemicolonToken extends SyntaxKind
    case object CommaToken extends SyntaxKind
    case object LessThanToken extends SyntaxKind
    case object LessThanSlashToken extends SyntaxKind
    case object GreaterThanToken extends SyntaxKind
    case object LessThanEqualsToken extends SyntaxKind
    case object GreaterThanEqualsToken extends SyntaxKind
    case object EqualsEqualsToken extends SyntaxKind
    case object ExclamationEqualsToken extends SyntaxKind
    case object EqualsEqualsEqualsToken extends SyntaxKind
    case object ExclamationEqualsEqualsToken extends SyntaxKind
    case object EqualsGreaterThanToken extends SyntaxKind
    case object PlusToken extends SyntaxKind
    case object MinusToken extends SyntaxKind
    case object AsteriskToken extends SyntaxKind
    case object AsteriskAsteriskToken extends SyntaxKind
    case object SlashToken extends SyntaxKind
    case object PercentToken extends SyntaxKind
    case object PlusPlusToken extends SyntaxKind
    case object MinusMinusToken extends SyntaxKind
    case object LessThanLessThanToken extends SyntaxKind
    case object GreaterThanGreaterThanToken extends SyntaxKind
    case object GreaterThanGreaterThanGreaterThanToken extends SyntaxKind
    case object AmpersandToken extends SyntaxKind
    case object BarToken extends SyntaxKind
    case object CaretToken extends SyntaxKind
    case object ExclamationToken extends SyntaxKind
    case object TildeToken extends SyntaxKind
    case object AmpersandAmpersandToken extends SyntaxKind
    case object BarBarToken extends SyntaxKind
    case object QuestionToken extends SyntaxKind
    case object ColonToken extends SyntaxKind
    case object AtToken extends SyntaxKind
    case object EqualsToken extends SyntaxKind
    case object PlusEqualsToken extends SyntaxKind
    case object MinusEqualsToken extends SyntaxKind
    case object AsteriskEqualsToken extends SyntaxKind
    case object AsteriskAsteriskEqualsToken extends SyntaxKind
    case object SlashEqualsToken extends SyntaxKind
    case object PercentEqualsToken extends SyntaxKind
    case object LessThanLessThanEqualsToken extends SyntaxKind
    case object GreaterThanGreaterThanEqualsToken extends SyntaxKind
    case object GreaterThanGreaterThanGreaterThanEqualsToken extends SyntaxKind
    case object AmpersandEqualsToken extends SyntaxKind
    case object BarEqualsToken extends SyntaxKind
    case object CaretEqualsToken extends SyntaxKind
    case object Identifier extends SyntaxKind
    case object BreakKeyword extends SyntaxKind
    case object CaseKeyword extends SyntaxKind
    case object CatchKeyword extends SyntaxKind
    case object ClassKeyword extends SyntaxKind
    case object ConstKeyword extends SyntaxKind
    case object ContinueKeyword extends SyntaxKind
    case object DebuggerKeyword extends SyntaxKind
    case object DefaultKeyword extends SyntaxKind
    case object DeleteKeyword extends SyntaxKind
    case object DoKeyword extends SyntaxKind
    case object ElseKeyword extends SyntaxKind
    case object EnumKeyword extends SyntaxKind
    case object ExportKeyword extends SyntaxKind
    case object ExtendsKeyword extends SyntaxKind
    case object FalseKeyword extends SyntaxKind
    case object FinallyKeyword extends SyntaxKind
    case object ForKeyword extends SyntaxKind
    case object FunctionKeyword extends SyntaxKind
    case object IfKeyword extends SyntaxKind
    case object ImportKeyword extends SyntaxKind
    case object InKeyword extends SyntaxKind
    case object InstanceOfKeyword extends SyntaxKind
    case object NewKeyword extends SyntaxKind
    case object NullKeyword extends SyntaxKind
    case object ReturnKeyword extends SyntaxKind
    case object SuperKeyword extends SyntaxKind
    case object SwitchKeyword extends SyntaxKind
    case object ThisKeyword extends SyntaxKind
    case object ThrowKeyword extends SyntaxKind
    case object TrueKeyword extends SyntaxKind
    case object TryKeyword extends SyntaxKind
    case object TypeOfKeyword extends SyntaxKind
    case object VarKeyword extends SyntaxKind
    case object VoidKeyword extends SyntaxKind
    case object WhileKeyword extends SyntaxKind
    case object WithKeyword extends SyntaxKind
    case object ImplementsKeyword extends SyntaxKind
    case object InterfaceKeyword extends SyntaxKind
    case object LetKeyword extends SyntaxKind
    case object PackageKeyword extends SyntaxKind
    case object PrivateKeyword extends SyntaxKind
    case object ProtectedKeyword extends SyntaxKind
    case object PublicKeyword extends SyntaxKind
    case object StaticKeyword extends SyntaxKind
    case object YieldKeyword extends SyntaxKind
    case object AbstractKeyword extends SyntaxKind
    case object AsKeyword extends SyntaxKind
    case object AnyKeyword extends SyntaxKind
    case object AsyncKeyword extends SyntaxKind
    case object AwaitKeyword extends SyntaxKind
    case object BooleanKeyword extends SyntaxKind
    case object ConstructorKeyword extends SyntaxKind
    case object DeclareKeyword extends SyntaxKind
    case object GetKeyword extends SyntaxKind
    case object IsKeyword extends SyntaxKind
    case object ModuleKeyword extends SyntaxKind
    case object NamespaceKeyword extends SyntaxKind
    case object NeverKeyword extends SyntaxKind
    case object ReadonlyKeyword extends SyntaxKind
    case object RequireKeyword extends SyntaxKind
    case object NumberKeyword extends SyntaxKind
    case object SetKeyword extends SyntaxKind
    case object StringKeyword extends SyntaxKind
    case object SymbolKeyword extends SyntaxKind
    case object TypeKeyword extends SyntaxKind
    case object UndefinedKeyword extends SyntaxKind
    case object FromKeyword extends SyntaxKind
    case object GlobalKeyword extends SyntaxKind
    case object OfKeyword extends SyntaxKind
    case object QualifiedName extends SyntaxKind
    case object ComputedPropertyName extends SyntaxKind
    case object TypeParameter extends SyntaxKind
    case object Parameter extends SyntaxKind
    case object Decorator extends SyntaxKind
    case object PropertySignature extends SyntaxKind
    case object PropertyDeclaration extends SyntaxKind
    case object MethodSignature extends SyntaxKind
    case object MethodDeclaration extends SyntaxKind
    case object Constructor extends SyntaxKind
    case object GetAccessor extends SyntaxKind
    case object SetAccessor extends SyntaxKind
    case object CallSignature extends SyntaxKind
    case object ConstructSignature extends SyntaxKind
    case object IndexSignature extends SyntaxKind
    case object TypePredicate extends SyntaxKind
    case object TypeReference extends SyntaxKind
    case object FunctionType extends SyntaxKind
    case object ConstructorType extends SyntaxKind
    case object TypeQuery extends SyntaxKind
    case object TypeLiteral extends SyntaxKind
    case object ArrayType extends SyntaxKind
    case object TupleType extends SyntaxKind
    case object UnionType extends SyntaxKind
    case object IntersectionType extends SyntaxKind
    case object ParenthesizedType extends SyntaxKind
    case object ThisType extends SyntaxKind
    case object LiteralType extends SyntaxKind
    case object ObjectBindingPattern extends SyntaxKind
    case object ArrayBindingPattern extends SyntaxKind
    case object BindingElement extends SyntaxKind
    case object ArrayLiteralExpression extends SyntaxKind
    case object ObjectLiteralExpression extends SyntaxKind
    case object PropertyAccessExpression extends SyntaxKind
    case object ElementAccessExpression extends SyntaxKind
    case object CallExpression extends SyntaxKind
    case object NewExpression extends SyntaxKind
    case object TaggedTemplateExpression extends SyntaxKind
    case object TypeAssertionExpression extends SyntaxKind
    case object ParenthesizedExpression extends SyntaxKind
    case object FunctionExpression extends SyntaxKind
    case object ArrowFunction extends SyntaxKind
    case object DeleteExpression extends SyntaxKind
    case object TypeOfExpression extends SyntaxKind
    case object VoidExpression extends SyntaxKind
    case object AwaitExpression extends SyntaxKind
    case object PrefixUnaryExpression extends SyntaxKind
    case object PostfixUnaryExpression extends SyntaxKind
    case object BinaryExpression extends SyntaxKind
    case object ConditionalExpression extends SyntaxKind
    case object TemplateExpression extends SyntaxKind
    case object YieldExpression extends SyntaxKind
    case object SpreadElementExpression extends SyntaxKind
    case object ClassExpression extends SyntaxKind
    case object OmittedExpression extends SyntaxKind
    case object ExpressionWithTypeArguments extends SyntaxKind
    case object AsExpression extends SyntaxKind
    case object NonNullExpression extends SyntaxKind
    case object TemplateSpan extends SyntaxKind
    case object SemicolonClassElement extends SyntaxKind
    case object Block extends SyntaxKind
    case object VariableStatement extends SyntaxKind
    case object EmptyStatement extends SyntaxKind
    case object ExpressionStatement extends SyntaxKind
    case object IfStatement extends SyntaxKind
    case object DoStatement extends SyntaxKind
    case object WhileStatement extends SyntaxKind
    case object ForStatement extends SyntaxKind
    case object ForInStatement extends SyntaxKind
    case object ForOfStatement extends SyntaxKind
    case object ContinueStatement extends SyntaxKind
    case object BreakStatement extends SyntaxKind
    case object ReturnStatement extends SyntaxKind
    case object WithStatement extends SyntaxKind
    case object SwitchStatement extends SyntaxKind
    case object LabeledStatement extends SyntaxKind
    case object ThrowStatement extends SyntaxKind
    case object TryStatement extends SyntaxKind
    case object DebuggerStatement extends SyntaxKind
    case object VariableDeclaration extends SyntaxKind
    case object VariableDeclarationList extends SyntaxKind
    case object FunctionDeclaration extends SyntaxKind
    case object ClassDeclaration extends SyntaxKind
    case object InterfaceDeclaration extends SyntaxKind
    case object TypeAliasDeclaration extends SyntaxKind
    case object EnumDeclaration extends SyntaxKind
    case object ModuleDeclaration extends SyntaxKind
    case object ModuleBlock extends SyntaxKind
    case object CaseBlock extends SyntaxKind
    case object NamespaceExportDeclaration extends SyntaxKind
    case object ImportEqualsDeclaration extends SyntaxKind
    case object ImportDeclaration extends SyntaxKind
    case object ImportClause extends SyntaxKind
    case object NamespaceImport extends SyntaxKind
    case object NamedImports extends SyntaxKind
    case object ImportSpecifier extends SyntaxKind
    case object ExportAssignment extends SyntaxKind
    case object ExportDeclaration extends SyntaxKind
    case object NamedExports extends SyntaxKind
    case object ExportSpecifier extends SyntaxKind
    case object MissingDeclaration extends SyntaxKind
    case object ExternalModuleReference extends SyntaxKind
    case object JsxElement extends SyntaxKind
    case object JsxSelfClosingElement extends SyntaxKind
    case object JsxOpeningElement extends SyntaxKind
    case object JsxClosingElement extends SyntaxKind
    case object JsxAttribute extends SyntaxKind
    case object JsxSpreadAttribute extends SyntaxKind
    case object JsxExpression extends SyntaxKind
    case object CaseClause extends SyntaxKind
    case object DefaultClause extends SyntaxKind
    case object HeritageClause extends SyntaxKind
    case object CatchClause extends SyntaxKind
    case object PropertyAssignment extends SyntaxKind
    case object ShorthandPropertyAssignment extends SyntaxKind
    case object EnumMember extends SyntaxKind
    case object SourceFile extends SyntaxKind
    case object JSDocTypeExpression extends SyntaxKind
    case object JSDocAllType extends SyntaxKind
    case object JSDocUnknownType extends SyntaxKind
    case object JSDocArrayType extends SyntaxKind
    case object JSDocUnionType extends SyntaxKind
    case object JSDocTupleType extends SyntaxKind
    case object JSDocNullableType extends SyntaxKind
    case object JSDocNonNullableType extends SyntaxKind
    case object JSDocRecordType extends SyntaxKind
    case object JSDocRecordMember extends SyntaxKind
    case object JSDocTypeReference extends SyntaxKind
    case object JSDocOptionalType extends SyntaxKind
    case object JSDocFunctionType extends SyntaxKind
    case object JSDocVariadicType extends SyntaxKind
    case object JSDocConstructorType extends SyntaxKind
    case object JSDocThisType extends SyntaxKind
    case object JSDocComment extends SyntaxKind
    case object JSDocTag extends SyntaxKind
    case object JSDocParameterTag extends SyntaxKind
    case object JSDocReturnTag extends SyntaxKind
    case object JSDocTypeTag extends SyntaxKind
    case object JSDocTemplateTag extends SyntaxKind
    case object JSDocTypedefTag extends SyntaxKind
    case object JSDocPropertyTag extends SyntaxKind
    case object JSDocTypeLiteral extends SyntaxKind
    case object JSDocLiteralType extends SyntaxKind
    case object JSDocNullKeyword extends SyntaxKind
    case object JSDocUndefinedKeyword extends SyntaxKind
    case object JSDocNeverKeyword extends SyntaxKind
    case object SyntaxList extends SyntaxKind
    case object NotEmittedStatement extends SyntaxKind
    case object PartiallyEmittedExpression extends SyntaxKind
    case object Count extends SyntaxKind
    case object FirstAssignment extends SyntaxKind
    case object LastAssignment extends SyntaxKind
    case object FirstCompoundAssignment extends SyntaxKind
    case object LastCompoundAssignment extends SyntaxKind
    case object FirstReservedWord extends SyntaxKind
    case object LastReservedWord extends SyntaxKind
    case object FirstKeyword extends SyntaxKind
    case object LastKeyword extends SyntaxKind
    case object FirstFutureReservedWord extends SyntaxKind
    case object LastFutureReservedWord extends SyntaxKind
    case object FirstTypeNode extends SyntaxKind
    case object LastTypeNode extends SyntaxKind
    case object FirstPunctuation extends SyntaxKind
    case object LastPunctuation extends SyntaxKind
    case object FirstToken extends SyntaxKind
    case object LastToken extends SyntaxKind
    case object FirstTriviaToken extends SyntaxKind
    case object LastTriviaToken extends SyntaxKind
    case object FirstLiteralToken extends SyntaxKind
    case object LastLiteralToken extends SyntaxKind
    case object FirstTemplateToken extends SyntaxKind
    case object LastTemplateToken extends SyntaxKind
    case object FirstBinaryOperator extends SyntaxKind
    case object LastBinaryOperator extends SyntaxKind
    case object FirstNode extends SyntaxKind
    case object FirstJSDocNode extends SyntaxKind
    case object LastJSDocNode extends SyntaxKind
    case object FirstJSDocTagNode extends SyntaxKind
    case object LastJSDocTagNode extends SyntaxKind
  }

  final class NodeFlags(val bits: Int) extends AnyVal {
    def |(that: NodeFlags): NodeFlags = new NodeFlags(this.bits | that.bits)
    def &(that: NodeFlags): NodeFlags = new NodeFlags(this.bits & that.bits)

    def test(that: NodeFlags): Boolean = (bits & that.bits) != 0
    def hasAll(that: NodeFlags): Boolean = (bits & that.bits) == that.bits
  }

  object NodeFlags {
    val None = new NodeFlags(0)
    val Let = new NodeFlags(1 << 0)
    val Const = new NodeFlags(1 << 1)
    val NestedNamespace = new NodeFlags(1 << 2)
    val Synthesized = new NodeFlags(1 << 3)
    val Namespace = new NodeFlags(1 << 4)
    val ExportContext = new NodeFlags(1 << 5)
    val ContainsThis = new NodeFlags(1 << 6)
    val HasImplicitReturn = new NodeFlags(1 << 7)
    val HasExplicitReturn = new NodeFlags(1 << 8)
    val GlobalAugmentation = new NodeFlags(1 << 9)
    val HasClassExtends = new NodeFlags(1 << 10)
    val HasDecorators = new NodeFlags(1 << 11)
    val HasParamDecorators = new NodeFlags(1 << 12)
    val HasAsyncFunctions = new NodeFlags(1 << 13)
    val HasJsxSpreadAttributes = new NodeFlags(1 << 14)
    val DisallowInContext = new NodeFlags(1 << 15)
    val YieldContext = new NodeFlags(1 << 16)
    val DecoratorContext = new NodeFlags(1 << 17)
    val AwaitContext = new NodeFlags(1 << 18)
    val ThisNodeHasError = new NodeFlags(1 << 19)
    val JavaScriptFile = new NodeFlags(1 << 20)
    val ThisNodeOrAnySubNodesHasError = new NodeFlags(1 << 21)
    val HasAggregatedChildData = new NodeFlags(1 << 22)

    val BlockScoped = Let | Const
    val ReachabilityCheckFlags = HasImplicitReturn | HasExplicitReturn
    val EmitHelperFlags =
      HasClassExtends | HasDecorators | HasParamDecorators | HasAsyncFunctions | HasJsxSpreadAttributes
    val ReachabilityAndEmitFlags = ReachabilityCheckFlags | EmitHelperFlags
    val ContextFlags =
      DisallowInContext | YieldContext | DecoratorContext | AwaitContext | JavaScriptFile
    val TypeExcludesFlags = YieldContext | AwaitContext
  }

  final class ModifierFlags(val bits: Int) extends AnyVal {
    def |(that: ModifierFlags): ModifierFlags =
      new ModifierFlags(this.bits | that.bits)

    def &(that: ModifierFlags): ModifierFlags =
      new ModifierFlags(this.bits & that.bits)

    def test(that: ModifierFlags): Boolean = (bits & that.bits) != 0
    def hasAll(that: ModifierFlags): Boolean = (bits & that.bits) == that.bits
  }

  object ModifierFlags {
    val None = new ModifierFlags(0)
    val Export = new ModifierFlags(1 << 0)
    val Ambient = new ModifierFlags(1 << 1)
    val Public = new ModifierFlags(1 << 2)
    val Private = new ModifierFlags(1 << 3)
    val Protected = new ModifierFlags(1 << 4)
    val Static = new ModifierFlags(1 << 5)
    val Readonly = new ModifierFlags(1 << 6)
    val Abstract = new ModifierFlags(1 << 7)
    val Async = new ModifierFlags(1 << 8)
    val Default = new ModifierFlags(1 << 9)
    val Const = new ModifierFlags(1 << 11)

    val HasComputedFlags = new ModifierFlags(1 << 29)

    val AccessibilityModifier = Public | Private | Protected
    val ParameterPropertyModifier = AccessibilityModifier | Readonly
    val NonPublicAccessibilityModifier = Private | Protected

    val TypeScriptModifier =
      Ambient | Public | Private | Protected | Readonly | Abstract | Const
  }

  final class JsxFlags(val bits: Int) extends AnyVal {
    def |(that: JsxFlags): JsxFlags =
      new JsxFlags(this.bits | that.bits)

    def &(that: JsxFlags): JsxFlags =
      new JsxFlags(this.bits & that.bits)

    def test(that: JsxFlags): Boolean = (bits & that.bits) != 0
    def hasAll(that: JsxFlags): Boolean = (bits & that.bits) == that.bits
  }

  object JsxFlags {
    val None = new JsxFlags(0)
    val IntrinsicNamedElement = new JsxFlags(1 << 0)
    val IntrinsicIndexedElement = new JsxFlags(1 << 1)

    val IntrinsicElement = IntrinsicNamedElement | IntrinsicIndexedElement
  }

  sealed abstract class RelationComparisonResult
  object RelationComparisonResult {
    case object Succeeded extends RelationComparisonResult
    case object Failed extends RelationComparisonResult
    case object FailedAndReported extends RelationComparisonResult
  }

  class Node(val kind: SyntaxKind) extends TextRange {
    var id = Node.nextID()
    var pos: Int = -1
    var end: Int = -1
    var flags: NodeFlags = NodeFlags.None
    var modifierFlagsCache: ModifierFlags = ModifierFlags.None
    var transformFlags: TransformFlags = TransformFlags.None
    var decorators: Option[NodeArray[Decorator]] = None
    var modifiers: Option[ModifiersArray] = None
    var parent: Node = null
    var original: Option[Node] = null
    var startsOnNewLine: Boolean = false
    var jsDocComments: Vector[JSDoc] = JSDoc.EmptyArray
    var symbol: Option[Symbol] = None
    var locals: Option[SymbolTable] = None
    var nextContainer: Option[Node] = None
    var localSymbol: Option[Symbol] = None
    var flowNode: Option[FlowNode] = None
    var emitNode: Option[EmitNode] = None
  }

  object Node {
    private var _lastID: Int = 0

    private def nextID(): Int = {
      _lastID += 1
      _lastID
    }
  }

  class NodeArray[T <: Node] extends mutable.ArrayBuffer[T] with TextRange {
    var hasTrailingComma: Boolean = false
  }

  class Token[+TKind <: SyntaxKind](override val kind: TKind)
      extends Node(kind)

  final case class DotDotDotToken() extends Token(SyntaxKind.DotDotDotToken)
  final case class QuestionToken() extends Token(SyntaxKind.QuestionToken)
  final case class ColonToken() extends Token(SyntaxKind.ColonToken)
  final case class EqualsToken() extends Token(SyntaxKind.EqualsToken)
  final case class AsteriskToken() extends Token(SyntaxKind.AsteriskToken)
  final case class EqualsGreaterThanToken()
      extends Token(SyntaxKind.EqualsGreaterThanToken)
  final case class EndOfFileToken() extends Token(SyntaxKind.EndOfFileToken)
  final case class AtToken() extends Token(SyntaxKind.AtToken)

  sealed abstract class Modifier[+TKind <: SyntaxKind](kd: TKind)
      extends Token[TKind](kd)

  final case class AbstractKeyword()
      extends Modifier(SyntaxKind.AbstractKeyword)
  final case class AsyncKeyword() extends Modifier(SyntaxKind.AsyncKeyword)
  final case class ConstKeyword() extends Modifier(SyntaxKind.ConstKeyword)
  final case class DeclareKeyword() extends Modifier(SyntaxKind.DeclareKeyword)
  final case class DefaultKeyword() extends Modifier(SyntaxKind.DefaultKeyword)
  final case class ExportKeyword() extends Modifier(SyntaxKind.ExportKeyword)
  final case class PublicKeyword() extends Modifier(SyntaxKind.PublicKeyword)
  final case class PrivateKeyword() extends Modifier(SyntaxKind.PrivateKeyword)
  final case class ProtectedKeyword()
      extends Modifier(SyntaxKind.ProtectedKeyword)
  final case class ReadonlyKeyword()
      extends Modifier(SyntaxKind.ReadonlyKeyword)
  final case class StaticKeyword() extends Modifier(SyntaxKind.StaticKeyword)

  type ModifiersArray = Vector[Modifier[_]]

  sealed abstract class GeneratedIdentifierKind
  object GeneratedIdentifierKind {
    case object None extends GeneratedIdentifierKind
    case object Auto extends GeneratedIdentifierKind
    case object Loop extends GeneratedIdentifierKind
    case object Unique extends GeneratedIdentifierKind
    case object Node extends GeneratedIdentifierKind
  }

  case class Identifier(text: String)
      extends Node(SyntaxKind.Identifier)
      with EntityName
      with PrimaryExpression
      with PropertyName
      with DeclarationName
      with DeclarationStatementName
      with BindingName {
    var originalKeywordKind: Option[SyntaxKind] = None
    var autoGenerateKind: GeneratedIdentifierKind =
      GeneratedIdentifierKind.None
    var autoGenerateId: Option[Int] = None
    var isInJSDocNamespace: Boolean = false
  }

  trait TransientIdentifier extends Identifier {
    var resolvedSymbol: Symbol
  }

  final case class QualifiedName(left: EntityName, right: Identifier)
      extends Node(SyntaxKind.QualifiedName)
      with EntityName

  sealed trait EntityName extends Node
  sealed trait PropertyName extends Node
  sealed trait DeclarationName extends Node

  trait Declaration extends Node {
    type NameType <: DeclarationName
    val name: NameType
  }

  sealed trait DeclarationStatementName extends DeclarationName

  trait DeclarationStatement extends Declaration with Statement {
    type NameType <: DeclarationStatementName
  }

  final case class ComputedPropertyName(expression: Expression)
      extends Node(SyntaxKind.ComputedPropertyName)
      with PropertyName
      with DeclarationName

  final case class Decorator(expression: LeftHandSideExpression)
      extends Node(SyntaxKind.Decorator)

  final case class TypeParameterDeclaration(name: Identifier,
                                            constraint: Option[TypeNode],
                                            expression: Option[Expression])
      extends Node(SyntaxKind.TypeParameter)
      with Declaration

  trait SignatureDeclaration extends Declaration {
    type NameType <: PropertyName
    val typeParameters: NodeArray[TypeParameterDeclaration]
    val parameters: NodeArray[ParameterDeclaration]
    val `type`: TypeNode
  }

  object DummyPropertyName extends Identifier("<none>")

  final case class CallSignatureDeclaration(
      typeParameters: NodeArray[TypeParameterDeclaration],
      parameters: NodeArray[ParameterDeclaration],
      `type`: TypeNode)
      extends Node(SyntaxKind.CallSignature) with SignatureDeclaration
      with TypeElement {
    type NameType = DummyPropertyName.type
    val name = DummyPropertyName
  }

  final case class ConstructSignatureDeclaration(
      typeParameters: NodeArray[TypeParameterDeclaration],
      parameters: NodeArray[ParameterDeclaration],
      `type`: TypeNode)
      extends Node(SyntaxKind.ConstructSignature) with SignatureDeclaration
      with TypeElement {
    type NameType = DummyPropertyName.type
    val name = DummyPropertyName
  }

  sealed trait BindingName extends DeclarationName

  final case class VariableDeclaration(
      override val parent: VariableDeclarationList,
      name: BindingName,
      `type`: TypeNode,
      initializer: Option[Expression])
      extends Node(SyntaxKind.VariableDeclaration) with Declaration {
    type NameType = BindingName
  }

  final case class VariableDeclarationList(
      declarations: NodeArray[VariableDeclaration])
      extends Node(SyntaxKind.VariableDeclarationList)

  final case class ParameterDeclaration(
      dotDotDotToken: Option[DotDotDotToken],
      name: BindingName,
      questionToken: Option[QuestionToken],
      `type`: Option[TypeNode],
      initializer: Option[Expression])
      extends Node(SyntaxKind.Parameter) with Declaration {
    type NameType = BindingName
  }

  final case class BindingElement(
      propertyName: PropertyName,
      dotDotDotToken: Option[DotDotDotToken],
      name: BindingName,
      initializer: Option[Expression])
      extends Node(SyntaxKind.BindingElement) with Declaration {
    type NameType = BindingName
  }

  // kind: SyntaxKind.PropertySignature | SyntaxKind.JSDocRecordMember
  final case class PropertySignature(
      kind: SyntaxKind,
      name: PropertyName,
      questionToken: Option[QuestionToken],
      `type`: Option[TypeNode],
      initializer: Option[Expression])
      extends Node(kind) with TypeElement {
    type NameType = PropertyName
  }

  final case class PropertyDeclaration(
      questionToken: Option[QuestionToken],
      name: PropertyName,
      `type`: Option[PropertyName],
      initializer: Option[Expression])
      extends Node(SyntaxKind.PropertyDeclaration) with ClassElement {
    type NameType = PropertyName
  }

  trait ObjectLiteralElement extends Declaration {
    type NameType <: PropertyName
  }

  sealed trait ObjectLiteralElementLike extends Node

  final case class PropertyAssignment(
      name: PropertyName,
      questionToken: Option[QuestionToken],
      initializer: Option[Expression])
      extends Node(SyntaxKind.PropertyAssignment)
      with ObjectLiteralElement with ObjectLiteralElementLike {
    type NameType = PropertyName
  }

  final case class ShorthandPropertyAssignment(
      name: Identifier,
      questionToken: Option[QuestionToken],
      equalsToken: Option[EqualsToken],
      objectAssignmentInitializer: Option[Expression])
      extends Node(SyntaxKind.ShorthandPropertyAssignment)
      with ObjectLiteralElement with ObjectLiteralElementLike {
    type NameType = Identifier
  }

  trait VariableLikeDeclaration extends Declaration {
    type NameType <: DeclarationName
    val propertyName: PropertyName
    val dotDotDotToken: Option[DotDotDotToken]
    val questionToken: Option[QuestionToken]
    val `type`: Option[TypeNode]
    val initializer: Option[Expression]
  }

  trait PropertyLikeDeclaration extends Declaration {
    type NameType <: PropertyName
  }

  trait BindingPattern extends Node with DeclarationName with BindingName {
    var elements: NodeArray[(BindingElement | ArrayBindingElement)]
  }
  trait ObjectBindingPattern extends BindingPattern {
    var kind: SyntaxKind.ObjectBindingPattern
    var elements: NodeArray[BindingElement]
  }
  type ArrayBindingElement = (BindingElement | OmittedExpression)
  trait ArrayBindingPattern extends BindingPattern {
    var kind: SyntaxKind.ArrayBindingPattern
    var elements: NodeArray[ArrayBindingElement]
  }
  trait FunctionLikeDeclaration extends SignatureDeclaration {
    var _functionLikeDeclarationBrand: Any
    var asteriskToken: AsteriskToken
    var questionToken: QuestionToken
    var body: (Block | Expression)
  }
  trait FunctionDeclaration
      extends FunctionLikeDeclaration
      with DeclarationStatement {
    var kind: SyntaxKind.FunctionDeclaration
    var name: Identifier
    var body: FunctionBody
  }
  trait MethodSignature extends SignatureDeclaration with TypeElement {
    var kind: SyntaxKind.MethodSignature
    var name: PropertyName
  }
  trait MethodDeclaration
      extends FunctionLikeDeclaration
      with ClassElement
      with ObjectLiteralElement
      with ObjectLiteralElementLike {
    var kind: SyntaxKind.MethodDeclaration
    var name: PropertyName
    var body: FunctionBody
  }
  trait ConstructorDeclaration
      extends FunctionLikeDeclaration
      with ClassElement {
    var kind: SyntaxKind.Constructor
    var body: FunctionBody
  }
  trait SemicolonClassElement extends ClassElement {
    var kind: SyntaxKind.SemicolonClassElement
  }

  sealed trait AccessorDeclaration extends ObjectLiteralElementLike

  trait GetAccessorDeclaration
      extends FunctionLikeDeclaration
      with ClassElement
      with ObjectLiteralElement
      with AccessorDeclaration {
    var kind: SyntaxKind.GetAccessor
    var name: PropertyName
    var body: FunctionBody
  }
  trait SetAccessorDeclaration
      extends FunctionLikeDeclaration
      with ClassElement
      with ObjectLiteralElement
      with AccessorDeclaration {
    var kind: SyntaxKind.SetAccessor
    var name: PropertyName
    var body: FunctionBody
  }
  type AccessorDeclaration = (GetAccessorDeclaration | SetAccessorDeclaration)
  trait IndexSignatureDeclaration
      extends SignatureDeclaration
      with ClassElement
      with TypeElement {
    var kind: SyntaxKind.IndexSignature
  }
  trait TypeNode extends Node {
    var _typeNodeBrand: Any
  }
  trait KeywordTypeNode extends TypeNode {
    var kind: (SyntaxKind.AnyKeyword | SyntaxKind.NumberKeyword | SyntaxKind.BooleanKeyword | SyntaxKind.StringKeyword | SyntaxKind.SymbolKeyword | SyntaxKind.VoidKeyword)
  }
  trait ThisTypeNode extends TypeNode {
    var kind: SyntaxKind.ThisType
  }
  trait FunctionOrConstructorTypeNode
      extends TypeNode
      with SignatureDeclaration {
    var kind: (SyntaxKind.FunctionType | SyntaxKind.ConstructorType)
  }
  trait FunctionTypeNode extends FunctionOrConstructorTypeNode {
    var kind: SyntaxKind.FunctionType
  }
  trait ConstructorTypeNode extends FunctionOrConstructorTypeNode {
    var kind: SyntaxKind.ConstructorType
  }
  trait TypeReferenceNode extends TypeNode {
    var kind: SyntaxKind.TypeReference
    var typeName: EntityName
    var typeArguments: NodeArray[TypeNode]
  }
  trait TypePredicateNode extends TypeNode {
    var kind: SyntaxKind.TypePredicate
    var parameterName: (Identifier | ThisTypeNode)
    var `type`: TypeNode
  }
  trait TypeQueryNode extends TypeNode {
    var kind: SyntaxKind.TypeQuery
    var exprName: EntityName
  }
  trait TypeLiteralNode extends TypeNode with Declaration {
    var kind: SyntaxKind.TypeLiteral
    var members: NodeArray[TypeElement]
  }
  trait ArrayTypeNode extends TypeNode {
    var kind: SyntaxKind.ArrayType
    var elementType: TypeNode
  }
  trait TupleTypeNode extends TypeNode {
    var kind: SyntaxKind.TupleType
    var elementTypes: NodeArray[TypeNode]
  }
  trait UnionOrIntersectionTypeNode extends TypeNode {
    var kind: (SyntaxKind.UnionType | SyntaxKind.IntersectionType)
    var types: NodeArray[TypeNode]
  }
  trait UnionTypeNode extends UnionOrIntersectionTypeNode {
    var kind: SyntaxKind.UnionType
  }
  trait IntersectionTypeNode extends UnionOrIntersectionTypeNode {
    var kind: SyntaxKind.IntersectionType
  }
  trait ParenthesizedTypeNode extends TypeNode {
    var kind: SyntaxKind.ParenthesizedType
    var `type`: TypeNode
  }
  trait LiteralTypeNode extends TypeNode {
    var kind: SyntaxKind.LiteralType
    var literal: Expression
  }
  trait StringLiteral extends LiteralExpression {
    var kind: SyntaxKind.StringLiteral
    var textSourceNode: (Identifier | StringLiteral)
  }
  trait Expression extends Node {
    var _expressionBrand: Any
    var contextualType: Type
  }
  trait OmittedExpression extends Expression {
    var kind: SyntaxKind.OmittedExpression
  }
  trait PartiallyEmittedExpression extends LeftHandSideExpression {
    var kind: SyntaxKind.PartiallyEmittedExpression
    var expression: Expression
  }
  trait UnaryExpression extends Expression {
    var _unaryExpressionBrand: Any
  }
  trait IncrementExpression extends UnaryExpression {
    var _incrementExpressionBrand: Any
  }
  type PrefixUnaryOperator =
    (SyntaxKind.PlusPlusToken | SyntaxKind.MinusMinusToken | SyntaxKind.PlusToken | SyntaxKind.MinusToken | SyntaxKind.TildeToken | SyntaxKind.ExclamationToken)
  trait PrefixUnaryExpression extends IncrementExpression {
    var kind: SyntaxKind.PrefixUnaryExpression
    var operator: PrefixUnaryOperator
    var operand: UnaryExpression
  }
  type PostfixUnaryOperator =
    (SyntaxKind.PlusPlusToken | SyntaxKind.MinusMinusToken)
  trait PostfixUnaryExpression extends IncrementExpression {
    var kind: SyntaxKind.PostfixUnaryExpression
    var operand: LeftHandSideExpression
    var operator: PostfixUnaryOperator
  }
  trait PostfixExpression extends UnaryExpression {
    var _postfixExpressionBrand: Any
  }
  trait LeftHandSideExpression extends IncrementExpression {
    var _leftHandSideExpressionBrand: Any
  }
  trait MemberExpression extends LeftHandSideExpression {
    var _memberExpressionBrand: Any
  }
  trait PrimaryExpression extends MemberExpression {
    var _primaryExpressionBrand: Any
  }
  trait NullLiteral extends PrimaryExpression {
    var kind: SyntaxKind.NullKeyword
  }
  trait BooleanLiteral extends PrimaryExpression {
    var kind: (SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword)
  }
  trait ThisExpression extends PrimaryExpression {
    var kind: SyntaxKind.ThisKeyword
  }
  trait SuperExpression extends PrimaryExpression {
    var kind: SyntaxKind.SuperKeyword
  }
  trait DeleteExpression extends UnaryExpression {
    var kind: SyntaxKind.DeleteExpression
    var expression: UnaryExpression
  }
  trait TypeOfExpression extends UnaryExpression {
    var kind: SyntaxKind.TypeOfExpression
    var expression: UnaryExpression
  }
  trait VoidExpression extends UnaryExpression {
    var kind: SyntaxKind.VoidExpression
    var expression: UnaryExpression
  }
  trait AwaitExpression extends UnaryExpression {
    var kind: SyntaxKind.AwaitExpression
    var expression: UnaryExpression
  }
  trait YieldExpression extends Expression {
    var kind: SyntaxKind.YieldExpression
    var asteriskToken: AsteriskToken
    var expression: Expression
  }
  type ExponentiationOperator = SyntaxKind.AsteriskAsteriskToken
  type MultiplicativeOperator =
    (SyntaxKind.AsteriskToken | SyntaxKind.SlashToken | SyntaxKind.PercentToken)
  type MultiplicativeOperatorOrHigher =
    (ExponentiationOperator | MultiplicativeOperator)
  type AdditiveOperator = (SyntaxKind.PlusToken | SyntaxKind.MinusToken)
  type AdditiveOperatorOrHigher =
    (MultiplicativeOperatorOrHigher | AdditiveOperator)
  type ShiftOperator =
    (SyntaxKind.LessThanLessThanToken | SyntaxKind.GreaterThanGreaterThanToken | SyntaxKind.GreaterThanGreaterThanGreaterThanToken)
  type ShiftOperatorOrHigher = (AdditiveOperatorOrHigher | ShiftOperator)
  type RelationalOperator =
    (SyntaxKind.LessThanToken | SyntaxKind.LessThanEqualsToken | SyntaxKind.GreaterThanToken | SyntaxKind.GreaterThanEqualsToken | SyntaxKind.InstanceOfKeyword | SyntaxKind.InKeyword)
  type RelationalOperatorOrHigher =
    (ShiftOperatorOrHigher | RelationalOperator)
  type EqualityOperator =
    (SyntaxKind.EqualsEqualsToken | SyntaxKind.EqualsEqualsEqualsToken | SyntaxKind.ExclamationEqualsEqualsToken | SyntaxKind.ExclamationEqualsToken)
  type EqualityOperatorOrHigher =
    (RelationalOperatorOrHigher | EqualityOperator)
  type BitwiseOperator =
    (SyntaxKind.AmpersandToken | SyntaxKind.BarToken | SyntaxKind.CaretToken)
  type BitwiseOperatorOrHigher = (EqualityOperatorOrHigher | BitwiseOperator)
  type LogicalOperator =
    (SyntaxKind.AmpersandAmpersandToken | SyntaxKind.BarBarToken)
  type LogicalOperatorOrHigher = (BitwiseOperatorOrHigher | LogicalOperator)
  type CompoundAssignmentOperator =
    (SyntaxKind.PlusEqualsToken | SyntaxKind.MinusEqualsToken | SyntaxKind.AsteriskAsteriskEqualsToken | SyntaxKind.AsteriskEqualsToken | SyntaxKind.SlashEqualsToken | SyntaxKind.PercentEqualsToken | SyntaxKind.AmpersandEqualsToken | SyntaxKind.BarEqualsToken | SyntaxKind.CaretEqualsToken | SyntaxKind.LessThanLessThanEqualsToken | SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken | SyntaxKind.GreaterThanGreaterThanEqualsToken)
  type AssignmentOperator =
    (SyntaxKind.EqualsToken | CompoundAssignmentOperator)
  type AssignmentOperatorOrHigher =
    (LogicalOperatorOrHigher | AssignmentOperator)
  type BinaryOperator = (AssignmentOperatorOrHigher | SyntaxKind.CommaToken)
  type BinaryOperatorToken = Token[BinaryOperator]
  trait BinaryExpression extends Expression with Declaration {
    var kind: SyntaxKind.BinaryExpression
    var left: Expression
    var operatorToken: BinaryOperatorToken
    var right: Expression
  }
  trait ConditionalExpression extends Expression {
    var kind: SyntaxKind.ConditionalExpression
    var condition: Expression
    var questionToken: QuestionToken
    var whenTrue: Expression
    var colonToken: ColonToken
    var whenFalse: Expression
  }
  type FunctionBody = Block
  type ConciseBody = (FunctionBody | Expression)
  trait FunctionExpression
      extends PrimaryExpression
      with FunctionLikeDeclaration {
    var kind: SyntaxKind.FunctionExpression
    var name: Identifier
    var body: FunctionBody
  }
  trait ArrowFunction extends Expression with FunctionLikeDeclaration {
    var kind: SyntaxKind.ArrowFunction
    var equalsGreaterThanToken: EqualsGreaterThanToken
    var body: ConciseBody
  }
  trait LiteralLikeNode extends Node {
    var text: String
    var isUnterminated: Boolean
    var hasExtendedUnicodeEscape: Boolean
    var isOctalLiteral: Boolean
  }
  trait LiteralExpression
      extends LiteralLikeNode
      with PrimaryExpression
      with PropertyName
      with DeclarationName
      with DeclarationStatementName {
    var _literalExpressionBrand: Any
  }
  trait RegularExpressionLiteral extends LiteralExpression {
    var kind: SyntaxKind.RegularExpressionLiteral
  }
  trait NoSubstitutionTemplateLiteral extends LiteralExpression {
    var kind: SyntaxKind.NoSubstitutionTemplateLiteral
  }
  trait NumericLiteral extends LiteralExpression {
    var kind: SyntaxKind.NumericLiteral
    var trailingComment: String
  }
  trait TemplateHead extends LiteralLikeNode {
    var kind: SyntaxKind.TemplateHead
  }
  trait TemplateMiddle extends LiteralLikeNode {
    var kind: SyntaxKind.TemplateMiddle
  }
  trait TemplateTail extends LiteralLikeNode {
    var kind: SyntaxKind.TemplateTail
  }
  type TemplateLiteral = (TemplateExpression | NoSubstitutionTemplateLiteral)
  trait TemplateExpression extends PrimaryExpression {
    var kind: SyntaxKind.TemplateExpression
    var head: TemplateHead
    var templateSpans: NodeArray[TemplateSpan]
  }
  trait TemplateSpan extends Node {
    var kind: SyntaxKind.TemplateSpan
    var expression: Expression
    var literal: (TemplateMiddle | TemplateTail)
  }
  trait ParenthesizedExpression extends PrimaryExpression {
    var kind: SyntaxKind.ParenthesizedExpression
    var expression: Expression
  }
  trait ArrayLiteralExpression extends PrimaryExpression {
    var kind: SyntaxKind.ArrayLiteralExpression
    var elements: NodeArray[Expression]
    var multiLine: Boolean
  }
  trait SpreadElementExpression extends Expression {
    var kind: SyntaxKind.SpreadElementExpression
    var expression: Expression
  }
  trait ObjectLiteralExpressionBase[T <: ObjectLiteralElement]
      extends PrimaryExpression
      with Declaration {
    var properties: NodeArray[T]
  }
  trait ObjectLiteralExpression
      extends ObjectLiteralExpressionBase[ObjectLiteralElementLike] {
    var kind: SyntaxKind.ObjectLiteralExpression
    var multiLine: Boolean
  }
  type EntityNameExpression = (Identifier | PropertyAccessEntityNameExpression)
  type EntityNameOrEntityNameExpression = (EntityName | EntityNameExpression)
  trait PropertyAccessExpression extends MemberExpression with Declaration {
    var kind: SyntaxKind.PropertyAccessExpression
    var expression: LeftHandSideExpression
    var name: Identifier
  }
  trait SuperPropertyAccessExpression extends PropertyAccessExpression {
    var expression: SuperExpression
  }
  trait PropertyAccessEntityNameExpression extends PropertyAccessExpression {
    var _propertyAccessExpressionLikeQualifiedNameBrand: Any
    var expression: EntityNameExpression
  }
  trait ElementAccessExpression extends MemberExpression {
    var kind: SyntaxKind.ElementAccessExpression
    var expression: LeftHandSideExpression
    var argumentExpression: Expression
  }
  trait SuperElementAccessExpression extends ElementAccessExpression {
    var expression: SuperExpression
  }
  type SuperProperty =
    (SuperPropertyAccessExpression | SuperElementAccessExpression)
  trait CallExpression extends LeftHandSideExpression with Declaration {
    var kind: SyntaxKind.CallExpression
    var expression: LeftHandSideExpression
    var typeArguments: NodeArray[TypeNode]
    var arguments: NodeArray[Expression]
  }
  trait SuperCall extends CallExpression {
    var expression: SuperExpression
  }
  trait ExpressionWithTypeArguments extends TypeNode {
    var kind: SyntaxKind.ExpressionWithTypeArguments
    var expression: LeftHandSideExpression
    var typeArguments: NodeArray[TypeNode]
  }
  trait NewExpression extends PrimaryExpression with Declaration {
    var kind: SyntaxKind.NewExpression
    var expression: LeftHandSideExpression
    var typeArguments: NodeArray[TypeNode]
    var arguments: NodeArray[Expression]
  }
  trait TaggedTemplateExpression extends MemberExpression {
    var kind: SyntaxKind.TaggedTemplateExpression
    var tag: LeftHandSideExpression
    var template: TemplateLiteral
  }
  type CallLikeExpression =
    (CallExpression | NewExpression | TaggedTemplateExpression | Decorator)
  trait AsExpression extends Expression {
    var kind: SyntaxKind.AsExpression
    var expression: Expression
    var `type`: TypeNode
  }
  trait TypeAssertion extends UnaryExpression {
    var kind: SyntaxKind.TypeAssertionExpression
    var `type`: TypeNode
    var expression: UnaryExpression
  }
  type AssertionExpression = (TypeAssertion | AsExpression)
  trait NonNullExpression extends LeftHandSideExpression {
    var kind: SyntaxKind.NonNullExpression
    var expression: Expression
  }
  trait JsxElement extends PrimaryExpression {
    var kind: SyntaxKind.JsxElement
    var openingElement: JsxOpeningElement
    var children: NodeArray[JsxChild]
    var closingElement: JsxClosingElement
  }
  type JsxTagNameExpression = (PrimaryExpression | PropertyAccessExpression)
  trait JsxOpeningElement extends Expression {
    var kind: SyntaxKind.JsxOpeningElement
    var tagName: JsxTagNameExpression
    var attributes: NodeArray[(JsxAttribute | JsxSpreadAttribute)]
  }
  trait JsxSelfClosingElement extends PrimaryExpression {
    var kind: SyntaxKind.JsxSelfClosingElement
    var tagName: JsxTagNameExpression
    var attributes: NodeArray[(JsxAttribute | JsxSpreadAttribute)]
  }
  type JsxOpeningLikeElement = (JsxSelfClosingElement | JsxOpeningElement)
  type JsxAttributeLike = (JsxAttribute | JsxSpreadAttribute)
  trait JsxAttribute extends Node {
    var kind: SyntaxKind.JsxAttribute
    var name: Identifier
    var initializer: (StringLiteral | JsxExpression)
  }
  trait JsxSpreadAttribute extends Node {
    var kind: SyntaxKind.JsxSpreadAttribute
    var expression: Expression
  }
  trait JsxClosingElement extends Node {
    var kind: SyntaxKind.JsxClosingElement
    var tagName: JsxTagNameExpression
  }
  trait JsxExpression extends Expression {
    var kind: SyntaxKind.JsxExpression
    var expression: Expression
  }
  trait JsxText extends Node {
    var kind: SyntaxKind.JsxText
  }
  type JsxChild =
    (JsxText | JsxExpression | JsxElement | JsxSelfClosingElement)
  trait Statement extends Node {
    var _statementBrand: Any
  }
  trait NotEmittedStatement extends Statement {
    var kind: SyntaxKind.NotEmittedStatement
  }
  trait EmptyStatement extends Statement {
    var kind: SyntaxKind.EmptyStatement
  }
  trait DebuggerStatement extends Statement {
    var kind: SyntaxKind.DebuggerStatement
  }
  trait MissingDeclaration
      extends DeclarationStatement
      with ClassElement
      with ObjectLiteralElement
      with TypeElement {
    var kind: SyntaxKind.MissingDeclaration
    var name: Identifier
  }
  type BlockLike = (SourceFile | Block | ModuleBlock | CaseClause)
  trait Block extends Statement {
    var kind: SyntaxKind.Block
    var statements: NodeArray[Statement]
    var multiLine: Boolean
  }
  trait VariableStatement extends Statement {
    var kind: SyntaxKind.VariableStatement
    var declarationList: VariableDeclarationList
  }
  trait ExpressionStatement extends Statement {
    var kind: SyntaxKind.ExpressionStatement
    var expression: Expression
  }
  trait IfStatement extends Statement {
    var kind: SyntaxKind.IfStatement
    var expression: Expression
    var thenStatement: Statement
    var elseStatement: Statement
  }
  trait IterationStatement extends Statement {
    var statement: Statement
  }
  trait DoStatement extends IterationStatement {
    var kind: SyntaxKind.DoStatement
    var expression: Expression
  }
  trait WhileStatement extends IterationStatement {
    var kind: SyntaxKind.WhileStatement
    var expression: Expression
  }
  type ForInitializer = (VariableDeclarationList | Expression)
  trait ForStatement extends IterationStatement {
    var kind: SyntaxKind.ForStatement
    var initializer: ForInitializer
    var condition: Expression
    var incrementor: Expression
  }
  trait ForInStatement extends IterationStatement {
    var kind: SyntaxKind.ForInStatement
    var initializer: ForInitializer
    var expression: Expression
  }
  trait ForOfStatement extends IterationStatement {
    var kind: SyntaxKind.ForOfStatement
    var initializer: ForInitializer
    var expression: Expression
  }
  trait BreakStatement extends Statement {
    var kind: SyntaxKind.BreakStatement
    var label: Identifier
  }
  trait ContinueStatement extends Statement {
    var kind: SyntaxKind.ContinueStatement
    var label: Identifier
  }
  type BreakOrContinueStatement = (BreakStatement | ContinueStatement)
  trait ReturnStatement extends Statement {
    var kind: SyntaxKind.ReturnStatement
    var expression: Expression
  }
  trait WithStatement extends Statement {
    var kind: SyntaxKind.WithStatement
    var expression: Expression
    var statement: Statement
  }
  trait SwitchStatement extends Statement {
    var kind: SyntaxKind.SwitchStatement
    var expression: Expression
    var caseBlock: CaseBlock
    var possiblyExhaustive: Boolean
  }
  trait CaseBlock extends Node {
    var kind: SyntaxKind.CaseBlock
    var clauses: NodeArray[CaseOrDefaultClause]
  }
  trait CaseClause extends Node {
    var kind: SyntaxKind.CaseClause
    var expression: Expression
    var statements: NodeArray[Statement]
  }
  trait DefaultClause extends Node {
    var kind: SyntaxKind.DefaultClause
    var statements: NodeArray[Statement]
  }
  type CaseOrDefaultClause = (CaseClause | DefaultClause)
  trait LabeledStatement extends Statement {
    var kind: SyntaxKind.LabeledStatement
    var label: Identifier
    var statement: Statement
  }
  trait ThrowStatement extends Statement {
    var kind: SyntaxKind.ThrowStatement
    var expression: Expression
  }
  trait TryStatement extends Statement {
    var kind: SyntaxKind.TryStatement
    var tryBlock: Block
    var catchClause: CatchClause
    var finallyBlock: Block
  }
  trait CatchClause extends Node {
    var kind: SyntaxKind.CatchClause
    var variableDeclaration: VariableDeclaration
    var block: Block
  }
  type DeclarationWithTypeParameters =
    (SignatureDeclaration | ClassLikeDeclaration | InterfaceDeclaration | TypeAliasDeclaration)
  trait ClassLikeDeclaration extends Declaration {
    var name: Identifier
    var typeParameters: NodeArray[TypeParameterDeclaration]
    var heritageClauses: NodeArray[HeritageClause]
    var members: NodeArray[ClassElement]
  }
  trait ClassDeclaration
      extends ClassLikeDeclaration
      with DeclarationStatement {
    var kind: SyntaxKind.ClassDeclaration
    var name: Identifier
  }
  trait ClassExpression extends ClassLikeDeclaration with PrimaryExpression {
    var kind: SyntaxKind.ClassExpression
  }
  trait ClassElement extends Declaration {
    var _classElementBrand: Any
    var name: PropertyName
  }
  trait TypeElement extends Declaration {
    var _typeElementBrand: Any
    var name: PropertyName
    var questionToken: QuestionToken
  }
  trait InterfaceDeclaration extends DeclarationStatement {
    var kind: SyntaxKind.InterfaceDeclaration
    var name: Identifier
    var typeParameters: NodeArray[TypeParameterDeclaration]
    var heritageClauses: NodeArray[HeritageClause]
    var members: NodeArray[TypeElement]
  }
  trait HeritageClause extends Node {
    var kind: SyntaxKind.HeritageClause
    var token: SyntaxKind
    var types: NodeArray[ExpressionWithTypeArguments]
  }
  trait TypeAliasDeclaration extends DeclarationStatement {
    var kind: SyntaxKind.TypeAliasDeclaration
    var name: Identifier
    var typeParameters: NodeArray[TypeParameterDeclaration]
    var `type`: TypeNode
  }
  trait EnumMember extends Declaration {
    var kind: SyntaxKind.EnumMember
    var name: PropertyName
    var initializer: Expression
  }
  trait EnumDeclaration extends DeclarationStatement {
    var kind: SyntaxKind.EnumDeclaration
    var name: Identifier
    var members: NodeArray[EnumMember]
  }
  type ModuleBody = (ModuleBlock | ModuleDeclaration)
  type ModuleName = (Identifier | StringLiteral)
  trait ModuleDeclaration extends DeclarationStatement {
    var kind: SyntaxKind.ModuleDeclaration
    var name: (Identifier | LiteralExpression)
    var body: (ModuleBlock | NamespaceDeclaration | JSDocNamespaceDeclaration | Identifier)
  }
  trait NamespaceDeclaration extends ModuleDeclaration {
    var name: Identifier
    var body: (ModuleBlock | NamespaceDeclaration)
  }
  trait JSDocNamespaceDeclaration extends ModuleDeclaration {
    var name: Identifier
    var body: (JSDocNamespaceDeclaration | Identifier)
  }
  trait ModuleBlock extends Node with Statement {
    var kind: SyntaxKind.ModuleBlock
    var statements: NodeArray[Statement]
  }
  type ModuleReference = (EntityName | ExternalModuleReference)
  trait ImportEqualsDeclaration extends DeclarationStatement {
    var kind: SyntaxKind.ImportEqualsDeclaration
    var name: Identifier
    var moduleReference: ModuleReference
  }
  trait ExternalModuleReference extends Node {
    var kind: SyntaxKind.ExternalModuleReference
    var expression: Expression
  }
  trait ImportDeclaration extends Statement {
    var kind: SyntaxKind.ImportDeclaration
    var importClause: ImportClause
    var moduleSpecifier: Expression
  }
  type NamedImportBindings = (NamespaceImport | NamedImports)
  trait ImportClause extends Declaration {
    var kind: SyntaxKind.ImportClause
    var name: Identifier
    var namedBindings: NamedImportBindings
  }
  trait NamespaceImport extends Declaration {
    var kind: SyntaxKind.NamespaceImport
    var name: Identifier
  }
  trait NamespaceExportDeclaration extends DeclarationStatement {
    var kind: SyntaxKind.NamespaceExportDeclaration
    var name: Identifier
    var moduleReference: LiteralLikeNode
  }
  trait ExportDeclaration extends DeclarationStatement {
    var kind: SyntaxKind.ExportDeclaration
    var exportClause: NamedExports
    var moduleSpecifier: Expression
  }
  trait NamedImports extends Node {
    var kind: SyntaxKind.NamedImports
    var elements: NodeArray[ImportSpecifier]
  }
  trait NamedExports extends Node {
    var kind: SyntaxKind.NamedExports
    var elements: NodeArray[ExportSpecifier]
  }
  type NamedImportsOrExports = (NamedImports | NamedExports)
  trait ImportSpecifier extends Declaration {
    var kind: SyntaxKind.ImportSpecifier
    var propertyName: Identifier
    var name: Identifier
  }
  trait ExportSpecifier extends Declaration {
    var kind: SyntaxKind.ExportSpecifier
    var propertyName: Identifier
    var name: Identifier
  }
  type ImportOrExportSpecifier = (ImportSpecifier | ExportSpecifier)
  trait ExportAssignment extends DeclarationStatement {
    var kind: SyntaxKind.ExportAssignment
    var isExportEquals: Boolean
    var expression: Expression
  }
  trait FileReference extends TextRange {
    var fileName: String
  }
  trait CommentRange extends TextRange {
    var hasTrailingNewLine: Boolean
    var kind: SyntaxKind
  }
  trait JSDocTypeExpression extends Node {
    var kind: SyntaxKind.JSDocTypeExpression
    var `type`: JSDocType
  }
  trait JSDocType extends TypeNode {
    var _jsDocTypeBrand: Any
  }
  trait JSDocAllType extends JSDocType {
    var kind: SyntaxKind.JSDocAllType
  }
  trait JSDocUnknownType extends JSDocType {
    var kind: SyntaxKind.JSDocUnknownType
  }
  trait JSDocArrayType extends JSDocType {
    var kind: SyntaxKind.JSDocArrayType
    var elementType: JSDocType
  }
  trait JSDocUnionType extends JSDocType {
    var kind: SyntaxKind.JSDocUnionType
    var types: NodeArray[JSDocType]
  }
  trait JSDocTupleType extends JSDocType {
    var kind: SyntaxKind.JSDocTupleType
    var types: NodeArray[JSDocType]
  }
  trait JSDocNonNullableType extends JSDocType {
    var kind: SyntaxKind.JSDocNonNullableType
    var `type`: JSDocType
  }
  trait JSDocNullableType extends JSDocType {
    var kind: SyntaxKind.JSDocNullableType
    var `type`: JSDocType
  }
  trait JSDocRecordType extends JSDocType {
    var kind: SyntaxKind.JSDocRecordType
    var literal: TypeLiteralNode
  }
  trait JSDocTypeReference extends JSDocType {
    var kind: SyntaxKind.JSDocTypeReference
    var name: EntityName
    var typeArguments: NodeArray[JSDocType]
  }
  trait JSDocOptionalType extends JSDocType {
    var kind: SyntaxKind.JSDocOptionalType
    var `type`: JSDocType
  }
  trait JSDocFunctionType extends JSDocType with SignatureDeclaration {
    var kind: SyntaxKind.JSDocFunctionType
    var parameters: NodeArray[ParameterDeclaration]
    var `type`: JSDocType
  }
  trait JSDocVariadicType extends JSDocType {
    var kind: SyntaxKind.JSDocVariadicType
    var `type`: JSDocType
  }
  trait JSDocConstructorType extends JSDocType {
    var kind: SyntaxKind.JSDocConstructorType
    var `type`: JSDocType
  }
  trait JSDocThisType extends JSDocType {
    var kind: SyntaxKind.JSDocThisType
    var `type`: JSDocType
  }
  trait JSDocLiteralType extends JSDocType {
    var kind: SyntaxKind.JSDocLiteralType
    var literal: LiteralTypeNode
  }
  type JSDocTypeReferencingNode =
    (JSDocThisType | JSDocConstructorType | JSDocVariadicType | JSDocOptionalType | JSDocNullableType | JSDocNonNullableType)
  trait JSDocRecordMember extends PropertySignature {
    var kind: SyntaxKind.JSDocRecordMember
    var name: (Identifier | LiteralExpression)
    var `type`: JSDocType
  }

  trait JSDoc extends Node {
    var kind: SyntaxKind.JSDocComment
    var tags: (NodeArray[JSDocTag] | undefined)
    var comment: (String | undefined)
  }

  object JSDoc {
    val EmptyArray: Vector[JSDoc] = Vector.empty
  }

  trait JSDocTag extends Node {
    var atToken: AtToken
    var tagName: Identifier
    var comment: (String | undefined)
  }
  trait JSDocUnknownTag extends JSDocTag {
    var kind: SyntaxKind.JSDocTag
  }
  trait JSDocTemplateTag extends JSDocTag {
    var kind: SyntaxKind.JSDocTemplateTag
    var typeParameters: NodeArray[TypeParameterDeclaration]
  }
  trait JSDocReturnTag extends JSDocTag {
    var kind: SyntaxKind.JSDocReturnTag
    var typeExpression: JSDocTypeExpression
  }
  trait JSDocTypeTag extends JSDocTag {
    var kind: SyntaxKind.JSDocTypeTag
    var typeExpression: JSDocTypeExpression
  }
  trait JSDocTypedefTag extends JSDocTag with Declaration {
    var kind: SyntaxKind.JSDocTypedefTag
    var fullName: (JSDocNamespaceDeclaration | Identifier)
    var name: Identifier
    var typeExpression: JSDocTypeExpression
    var jsDocTypeLiteral: JSDocTypeLiteral
  }
  trait JSDocPropertyTag extends JSDocTag with TypeElement {
    var kind: SyntaxKind.JSDocPropertyTag
    var name: Identifier
    var typeExpression: JSDocTypeExpression
  }
  trait JSDocTypeLiteral extends JSDocType {
    var kind: SyntaxKind.JSDocTypeLiteral
    var jsDocPropertyTags: NodeArray[JSDocPropertyTag]
    var jsDocTypeTag: JSDocTypeTag
  }
  trait JSDocParameterTag extends JSDocTag {
    var kind: SyntaxKind.JSDocParameterTag
    var preParameterName: Identifier
    var typeExpression: JSDocTypeExpression
    var postParameterName: Identifier
    var parameterName: Identifier
    var isBracketed: Boolean
  }
  sealed abstract class FlowFlags
  object FlowFlags {
    case object Unreachable extends FlowFlags
    case object Start extends FlowFlags
    case object BranchLabel extends FlowFlags
    case object LoopLabel extends FlowFlags
    case object Assignment extends FlowFlags
    case object TrueCondition extends FlowFlags
    case object FalseCondition extends FlowFlags
    case object SwitchClause extends FlowFlags
    case object ArrayMutation extends FlowFlags
    case object Referenced extends FlowFlags
    case object Shared extends FlowFlags
    case object Label extends FlowFlags
    case object Condition extends FlowFlags
  }
  trait FlowNode {
    var flags: FlowFlags
    var id: Int
  }
  trait FlowStart extends FlowNode {
    var container: (FunctionExpression | ArrowFunction | MethodDeclaration)
  }
  trait FlowLabel extends FlowNode {
    var antecedents: Array[FlowNode]
  }
  trait FlowAssignment extends FlowNode {
    var node: (Expression | VariableDeclaration | BindingElement)
    var antecedent: FlowNode
  }
  trait FlowCondition extends FlowNode {
    var expression: Expression
    var antecedent: FlowNode
  }
  trait FlowSwitchClause extends FlowNode {
    var switchStatement: SwitchStatement
    var clauseStart: Int
    var clauseEnd: Int
    var antecedent: FlowNode
  }
  trait FlowArrayMutation extends FlowNode {
    var node: (CallExpression | BinaryExpression)
    var antecedent: FlowNode
  }
  type FlowType = (Type | IncompleteType)
  trait IncompleteType {
    var flags: TypeFlags
    var `type`: Type
  }
  trait AmdDependency {
    var path: String
    var name: String
  }
  trait SourceFile extends Declaration {
    var kind: SyntaxKind.SourceFile
    var statements: NodeArray[Statement]
    var endOfFileToken: Token[SyntaxKind.EndOfFileToken]
    var fileName: String
    var path: Path
    var text: String
    var amdDependencies: Array[AmdDependency]
    var moduleName: String
    var referencedFiles: Array[FileReference]
    var typeReferenceDirectives: Array[FileReference]
    var languageVariant: LanguageVariant
    var isDeclarationFile: Boolean
    var renamedDependencies: Map[String]
    var hasNoDefaultLib: Boolean
    var languageVersion: ScriptTarget
    var scriptKind: ScriptKind
    var externalModuleIndicator: Node
    var commonJsModuleIndicator: Node
    var identifiers: Map[String]
    var nodeCount: Int
    var identifierCount: Int
    var symbolCount: Int
    var parseDiagnostics: Array[Diagnostic]
    var bindDiagnostics: Array[Diagnostic]
    var lineMap: Array[Int]
    var classifiableNames: Map[String]
    var resolvedModules: Map[ResolvedModule]
    var resolvedTypeReferenceDirectiveNames: Map[
      ResolvedTypeReferenceDirective]
    var imports: Array[LiteralExpression]
    var moduleAugmentations: Array[LiteralExpression]
    var patternAmbientModules: Array[PatternAmbientModule]
    var externalHelpersModuleName: Identifier
  }
  trait ScriptReferenceHost {
    def getCompilerOptions(): CompilerOptions
    def getSourceFile(fileName: String): SourceFile
    def getSourceFileByPath(path: Path): SourceFile
    def getCurrentDirectory(): String
  }
  trait ParseConfigHost {
    var useCaseSensitiveFileNames: Boolean
    def readDirectory(rootDir: String,
                      extensions: Array[String],
                      excludes: Array[String],
                      includes: Array[String]): Array[String]
    def fileExists(path: String): Boolean
    def readFile(path: String): String
  }
  trait WriteFileCallback {
    def apply(fileName: String,
              data: String,
              writeByteOrderMark: Boolean,
              onError: ((String) => Unit),
              sourceFiles: Array[SourceFile]): Unit
  }
  class OperationCanceledException {}
  trait CancellationToken {
    def isCancellationRequested(): Boolean
    def throwIfCancellationRequested(): Unit
  }
  trait Program extends ScriptReferenceHost {
    def getRootFileNames(): Array[String]
    def getSourceFiles(): Array[SourceFile]
    def emit(targetSourceFile: SourceFile,
             writeFile: WriteFileCallback,
             cancellationToken: CancellationToken,
             emitOnlyDtsFiles: Boolean): EmitResult
    def getOptionsDiagnostics(
        cancellationToken: CancellationToken): Array[Diagnostic]
    def getGlobalDiagnostics(
        cancellationToken: CancellationToken): Array[Diagnostic]
    def getSyntacticDiagnostics(
        sourceFile: SourceFile,
        cancellationToken: CancellationToken): Array[Diagnostic]
    def getSemanticDiagnostics(
        sourceFile: SourceFile,
        cancellationToken: CancellationToken): Array[Diagnostic]
    def getDeclarationDiagnostics(
        sourceFile: SourceFile,
        cancellationToken: CancellationToken): Array[Diagnostic]
    def getTypeChecker(): TypeChecker
    def getCommonSourceDirectory(): String
    def getDiagnosticsProducingTypeChecker(): TypeChecker
    def dropDiagnosticsProducingTypeChecker(): Unit
    def getClassifiableNames(): Map[String]
    def getNodeCount(): Int
    def getIdentifierCount(): Int
    def getSymbolCount(): Int
    def getTypeCount(): Int
    def getFileProcessingDiagnostics(): DiagnosticCollection
    def getResolvedTypeReferenceDirectives()
      : Map[ResolvedTypeReferenceDirective]
    var structureIsReused: Boolean
  }
  trait SourceMapSpan {
    var emittedLine: Int
    var emittedColumn: Int
    var sourceLine: Int
    var sourceColumn: Int
    var nameIndex: Int
    var sourceIndex: Int
  }
  trait SourceMapData {
    var sourceMapFilePath: String
    var jsSourceMappingURL: String
    var sourceMapFile: String
    var sourceMapSourceRoot: String
    var sourceMapSources: Array[String]
    var sourceMapSourcesContent: Array[String]
    var inputSourceFileNames: Array[String]
    var sourceMapNames: Array[String]
    var sourceMapMappings: String
    var sourceMapDecodedMappings: Array[SourceMapSpan]
  }
  sealed abstract class ExitStatus
  object ExitStatus {
    case object Success extends ExitStatus
    case object DiagnosticsPresent_OutputsSkipped extends ExitStatus
    case object DiagnosticsPresent_OutputsGenerated extends ExitStatus
  }
  trait EmitResult {
    var emitSkipped: Boolean
    var diagnostics: Array[Diagnostic]
    var emittedFiles: Array[String]
    var sourceMaps: Array[SourceMapData]
  }
  trait TypeCheckerHost {
    def getCompilerOptions(): CompilerOptions
    def getSourceFiles(): Array[SourceFile]
    def getSourceFile(fileName: String): SourceFile
    def getResolvedTypeReferenceDirectives()
      : Map[ResolvedTypeReferenceDirective]
  }
  trait TypeChecker {
    def getTypeOfSymbolAtLocation(symbol: Symbol, node: Node): Type
    def getDeclaredTypeOfSymbol(symbol: Symbol): Type
    def getPropertiesOfType(`type`: Type): Array[Symbol]
    def getPropertyOfType(`type`: Type, propertyName: String): Symbol
    def getSignaturesOfType(`type`: Type,
                            kind: SignatureKind): Array[Signature]
    def getIndexTypeOfType(`type`: Type, kind: IndexKind): Type
    def getBaseTypes(`type`: InterfaceType): Array[ObjectType]
    def getReturnTypeOfSignature(signature: Signature): Type
    def getNonNullableType(`type`: Type): Type
    def getSymbolsInScope(location: Node, meaning: SymbolFlags): Array[Symbol]
    def getSymbolAtLocation(node: Node): Symbol
    def getSymbolsOfParameterPropertyDeclaration(
        parameter: ParameterDeclaration,
        parameterName: String): Array[Symbol]
    def getShorthandAssignmentValueSymbol(location: Node): Symbol
    def getExportSpecifierLocalTargetSymbol(location: ExportSpecifier): Symbol
    def getPropertySymbolOfDestructuringAssignment(
        location: Identifier): Symbol
    def getTypeAtLocation(node: Node): Type
    def typeToString(`type`: Type,
                     enclosingDeclaration: Node,
                     flags: TypeFormatFlags): String
    def symbolToString(symbol: Symbol,
                       enclosingDeclaration: Node,
                       meaning: SymbolFlags): String
    def getSymbolDisplayBuilder(): SymbolDisplayBuilder
    def getFullyQualifiedName(symbol: Symbol): String
    def getAugmentedPropertiesOfType(`type`: Type): Array[Symbol]
    def getRootSymbols(symbol: Symbol): Array[Symbol]
    def getContextualType(node: Expression): Type
    def getResolvedSignature(node: CallLikeExpression,
                             candidatesOutArray: Array[Signature]): Signature
    def getSignatureFromDeclaration(
        declaration: SignatureDeclaration): Signature
    def isImplementationOfOverload(node: FunctionLikeDeclaration): Boolean
    def isUndefinedSymbol(symbol: Symbol): Boolean
    def isArgumentsSymbol(symbol: Symbol): Boolean
    def isUnknownSymbol(symbol: Symbol): Boolean
    def getConstantValue(
        node: (EnumMember | PropertyAccessExpression | ElementAccessExpression))
      : Int
    def isValidPropertyAccess(node: (PropertyAccessExpression | QualifiedName),
                              propertyName: String): Boolean
    def getAliasedSymbol(symbol: Symbol): Symbol
    def getExportsOfModule(moduleSymbol: Symbol): Array[Symbol]
    def getJsxElementAttributesType(elementNode: JsxOpeningLikeElement): Type
    def getJsxIntrinsicTagNames(): Array[Symbol]
    def isOptionalParameter(node: ParameterDeclaration): Boolean
    def getAmbientModules(): Array[Symbol]
    def getDiagnostics(sourceFile: SourceFile,
                       cancellationToken: CancellationToken): Array[Diagnostic]
    def getGlobalDiagnostics(): Array[Diagnostic]
    def getEmitResolver(sourceFile: SourceFile,
                        cancellationToken: CancellationToken): EmitResolver
    def getNodeCount(): Int
    def getIdentifierCount(): Int
    def getSymbolCount(): Int
    def getTypeCount(): Int
  }
  trait SymbolDisplayBuilder {
    def buildTypeDisplay(`type`: Type,
                         writer: SymbolWriter,
                         enclosingDeclaration: Node,
                         flags: TypeFormatFlags): Unit
    def buildSymbolDisplay(symbol: Symbol,
                           writer: SymbolWriter,
                           enclosingDeclaration: Node,
                           meaning: SymbolFlags,
                           flags: SymbolFormatFlags): Unit
    def buildSignatureDisplay(signatures: Signature,
                              writer: SymbolWriter,
                              enclosingDeclaration: Node,
                              flags: TypeFormatFlags,
                              kind: SignatureKind): Unit
    def buildParameterDisplay(parameter: Symbol,
                              writer: SymbolWriter,
                              enclosingDeclaration: Node,
                              flags: TypeFormatFlags): Unit
    def buildTypeParameterDisplay(tp: TypeParameter,
                                  writer: SymbolWriter,
                                  enclosingDeclaration: Node,
                                  flags: TypeFormatFlags): Unit
    def buildTypePredicateDisplay(predicate: TypePredicate,
                                  writer: SymbolWriter,
                                  enclosingDeclaration: Node,
                                  flags: TypeFormatFlags): Unit
    def buildTypeParameterDisplayFromSymbol(symbol: Symbol,
                                            writer: SymbolWriter,
                                            enclosingDeclaration: Node,
                                            flags: TypeFormatFlags): Unit
    def buildDisplayForParametersAndDelimiters(thisParameter: Symbol,
                                               parameters: Array[Symbol],
                                               writer: SymbolWriter,
                                               enclosingDeclaration: Node,
                                               flags: TypeFormatFlags): Unit
    def buildDisplayForTypeParametersAndDelimiters(
        typeParameters: Array[TypeParameter],
        writer: SymbolWriter,
        enclosingDeclaration: Node,
        flags: TypeFormatFlags): Unit
    def buildReturnTypeDisplay(signature: Signature,
                               writer: SymbolWriter,
                               enclosingDeclaration: Node,
                               flags: TypeFormatFlags): Unit
  }
  trait SymbolWriter {
    def writeKeyword(text: String): Unit
    def writeOperator(text: String): Unit
    def writePunctuation(text: String): Unit
    def writeSpace(text: String): Unit
    def writeStringLiteral(text: String): Unit
    def writeParameter(text: String): Unit
    def writeSymbol(text: String, symbol: Symbol): Unit
    def writeLine(): Unit
    def increaseIndent(): Unit
    def decreaseIndent(): Unit
    def clear(): Unit
    def trackSymbol(symbol: Symbol,
                    enclosingDeclaration: Node,
                    meaning: SymbolFlags): Unit
    def reportInaccessibleThisError(): Unit
  }
  sealed abstract class TypeFormatFlags
  object TypeFormatFlags {
    case object None extends TypeFormatFlags
    case object WriteArrayAsGenericType extends TypeFormatFlags
    case object UseTypeOfFunction extends TypeFormatFlags
    case object NoTruncation extends TypeFormatFlags
    case object WriteArrowStyleSignature extends TypeFormatFlags
    case object WriteOwnNameForAnyLike extends TypeFormatFlags
    case object WriteTypeArgumentsOfSignature extends TypeFormatFlags
    case object InElementType extends TypeFormatFlags
    case object UseFullyQualifiedType extends TypeFormatFlags
    case object InFirstTypeArgument extends TypeFormatFlags
    case object InTypeAlias extends TypeFormatFlags
    case object UseTypeAliasValue extends TypeFormatFlags
  }
  sealed abstract class SymbolFormatFlags
  object SymbolFormatFlags {
    case object None extends SymbolFormatFlags
    case object WriteTypeParametersOrArguments extends SymbolFormatFlags
    case object UseOnlyExternalAliasing extends SymbolFormatFlags
  }
  sealed abstract class SymbolAccessibility
  object SymbolAccessibility {
    case object Accessible extends SymbolAccessibility
    case object NotAccessible extends SymbolAccessibility
    case object CannotBeNamed extends SymbolAccessibility
  }
  sealed abstract class TypePredicateKind
  object TypePredicateKind {
    case object This extends TypePredicateKind
    case object Identifier extends TypePredicateKind
  }
  trait TypePredicateBase {
    var kind: TypePredicateKind
    var `type`: Type
  }
  trait ThisTypePredicate extends TypePredicateBase {
    var kind: TypePredicateKind.This
  }
  trait IdentifierTypePredicate extends TypePredicateBase {
    var kind: TypePredicateKind.Identifier
    var parameterName: String
    var parameterIndex: Int
  }
  type TypePredicate = (IdentifierTypePredicate | ThisTypePredicate)
  type AnyImportSyntax = (ImportDeclaration | ImportEqualsDeclaration)
  trait SymbolVisibilityResult {
    var accessibility: SymbolAccessibility
    var aliasesToMakeVisible: Array[AnyImportSyntax]
    var errorSymbolName: String
    var errorNode: Node
  }
  trait SymbolAccessibilityResult extends SymbolVisibilityResult {
    var errorModuleName: String
  }
  sealed abstract class TypeReferenceSerializationKind
  object TypeReferenceSerializationKind {
    case object Unknown extends TypeReferenceSerializationKind
    case object TypeWithConstructSignatureAndValue
        extends TypeReferenceSerializationKind
    case object VoidNullableOrNeverType extends TypeReferenceSerializationKind
    case object NumberLikeType extends TypeReferenceSerializationKind
    case object StringLikeType extends TypeReferenceSerializationKind
    case object BooleanType extends TypeReferenceSerializationKind
    case object ArrayLikeType extends TypeReferenceSerializationKind
    case object ESSymbolType extends TypeReferenceSerializationKind
    case object Promise extends TypeReferenceSerializationKind
    case object TypeWithCallSignature extends TypeReferenceSerializationKind
    case object ObjectType extends TypeReferenceSerializationKind
  }
  trait EmitResolver {
    def hasGlobalName(name: String): Boolean
    def getReferencedExportContainer(node: Identifier, prefixLocals: Boolean)
      : (SourceFile | ModuleDeclaration | EnumDeclaration)
    def getReferencedImportDeclaration(node: Identifier): Declaration
    def getReferencedDeclarationWithCollidingName(
        node: Identifier): Declaration
    def isDeclarationWithCollidingName(node: Declaration): Boolean
    def isValueAliasDeclaration(node: Node): Boolean
    def isReferencedAliasDeclaration(node: Node,
                                     checkChildren: Boolean): Boolean
    def isTopLevelValueImportEqualsWithEntityName(
        node: ImportEqualsDeclaration): Boolean
    def getNodeCheckFlags(node: Node): NodeCheckFlags
    def isDeclarationVisible(node: Declaration): Boolean
    def collectLinkedAliases(node: Identifier): Array[Node]
    def isImplementationOfOverload(node: FunctionLikeDeclaration): Boolean
    def writeTypeOfDeclaration(
        declaration: (AccessorDeclaration | VariableLikeDeclaration),
        enclosingDeclaration: Node,
        flags: TypeFormatFlags,
        writer: SymbolWriter): Unit
    def writeReturnTypeOfSignatureDeclaration(
        signatureDeclaration: SignatureDeclaration,
        enclosingDeclaration: Node,
        flags: TypeFormatFlags,
        writer: SymbolWriter): Unit
    def writeTypeOfExpression(expr: Expression,
                              enclosingDeclaration: Node,
                              flags: TypeFormatFlags,
                              writer: SymbolWriter): Unit
    def writeBaseConstructorTypeOfClass(node: ClassLikeDeclaration,
                                        enclosingDeclaration: Node,
                                        flags: TypeFormatFlags,
                                        writer: SymbolWriter): Unit
    def isSymbolAccessible(
        symbol: Symbol,
        enclosingDeclaration: Node,
        meaning: SymbolFlags,
        shouldComputeAliasToMarkVisible: Boolean): SymbolAccessibilityResult
    def isEntityNameVisible(entityName: EntityNameOrEntityNameExpression,
                            enclosingDeclaration: Node): SymbolVisibilityResult
    def getConstantValue(
        node: (EnumMember | PropertyAccessExpression | ElementAccessExpression))
      : Int
    def getReferencedValueDeclaration(reference: Identifier): Declaration
    def getTypeReferenceSerializationKind(
        typeName: EntityName,
        location: Node): TypeReferenceSerializationKind
    def isOptionalParameter(node: ParameterDeclaration): Boolean
    def moduleExportsSomeValue(moduleReferenceExpression: Expression): Boolean
    def isArgumentsLocalBinding(node: Identifier): Boolean
    def getExternalModuleFileFromDeclaration(
        declaration: (ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration))
      : SourceFile
    def getTypeReferenceDirectivesForEntityName(
        name: EntityNameOrEntityNameExpression): Array[String]
    def getTypeReferenceDirectivesForSymbol(
        symbol: Symbol,
        meaning: SymbolFlags): Array[String]
    def isLiteralConstDeclaration(
        node: (VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration))
      : Boolean
    def writeLiteralConstValue(
        node: (VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration),
        writer: SymbolWriter): Unit
  }
  sealed abstract class SymbolFlags
  object SymbolFlags {
    case object None extends SymbolFlags
    case object FunctionScopedVariable extends SymbolFlags
    case object BlockScopedVariable extends SymbolFlags
    case object Property extends SymbolFlags
    case object EnumMember extends SymbolFlags
    case object Function extends SymbolFlags
    case object Class extends SymbolFlags
    case object Interface extends SymbolFlags
    case object ConstEnum extends SymbolFlags
    case object RegularEnum extends SymbolFlags
    case object ValueModule extends SymbolFlags
    case object NamespaceModule extends SymbolFlags
    case object TypeLiteral extends SymbolFlags
    case object ObjectLiteral extends SymbolFlags
    case object Method extends SymbolFlags
    case object Constructor extends SymbolFlags
    case object GetAccessor extends SymbolFlags
    case object SetAccessor extends SymbolFlags
    case object Signature extends SymbolFlags
    case object TypeParameter extends SymbolFlags
    case object TypeAlias extends SymbolFlags
    case object ExportValue extends SymbolFlags
    case object ExportType extends SymbolFlags
    case object ExportNamespace extends SymbolFlags
    case object Alias extends SymbolFlags
    case object Instantiated extends SymbolFlags
    case object Merged extends SymbolFlags
    case object Transient extends SymbolFlags
    case object Prototype extends SymbolFlags
    case object SyntheticProperty extends SymbolFlags
    case object Optional extends SymbolFlags
    case object ExportStar extends SymbolFlags
    case object Enum extends SymbolFlags
    case object Variable extends SymbolFlags
    case object Value extends SymbolFlags
    case object Type extends SymbolFlags
    case object Namespace extends SymbolFlags
    case object Module extends SymbolFlags
    case object Accessor extends SymbolFlags
    case object FunctionScopedVariableExcludes extends SymbolFlags
    case object BlockScopedVariableExcludes extends SymbolFlags
    case object ParameterExcludes extends SymbolFlags
    case object PropertyExcludes extends SymbolFlags
    case object EnumMemberExcludes extends SymbolFlags
    case object FunctionExcludes extends SymbolFlags
    case object ClassExcludes extends SymbolFlags
    case object InterfaceExcludes extends SymbolFlags
    case object RegularEnumExcludes extends SymbolFlags
    case object ConstEnumExcludes extends SymbolFlags
    case object ValueModuleExcludes extends SymbolFlags
    case object NamespaceModuleExcludes extends SymbolFlags
    case object MethodExcludes extends SymbolFlags
    case object GetAccessorExcludes extends SymbolFlags
    case object SetAccessorExcludes extends SymbolFlags
    case object TypeParameterExcludes extends SymbolFlags
    case object TypeAliasExcludes extends SymbolFlags
    case object AliasExcludes extends SymbolFlags
    case object ModuleMember extends SymbolFlags
    case object ExportHasLocal extends SymbolFlags
    case object HasExports extends SymbolFlags
    case object HasMembers extends SymbolFlags
    case object BlockScoped extends SymbolFlags
    case object PropertyOrAccessor extends SymbolFlags
    case object Export extends SymbolFlags
    case object ClassMember extends SymbolFlags
    case object Classifiable extends SymbolFlags
  }
  trait Symbol {
    var flags: SymbolFlags
    var name: String
    var declarations: Array[Declaration]
    var valueDeclaration: Declaration
    var members: SymbolTable
    var exports: SymbolTable
    var globalExports: SymbolTable
    var isReadonly: Boolean
    var id: Int
    var mergeId: Int
    var parent: Symbol
    var exportSymbol: Symbol
    var constEnumOnlyModule: Boolean
    var isReferenced: Boolean
    var isReplaceableByMethod: Boolean
    var isAssigned: Boolean
  }
  trait SymbolLinks {
    var target: Symbol
    var `type`: Type
    var declaredType: Type
    var typeParameters: Array[TypeParameter]
    var inferredClassType: Type
    var instantiations: Map[Type]
    var mapper: TypeMapper
    var referenced: Boolean
    var containingType: UnionOrIntersectionType
    var hasNonUniformType: Boolean
    var isPartial: Boolean
    var isDiscriminantProperty: Boolean
    var resolvedExports: SymbolTable
    var exportsChecked: Boolean
    var isDeclarationWithCollidingName: Boolean
    var bindingElement: BindingElement
    var exportsSomeValue: Boolean
  }
  trait TransientSymbol extends Symbol with SymbolLinks {}
  type SymbolTable = Map[Symbol]
  trait Pattern {
    var prefix: String
    var suffix: String
  }
  trait PatternAmbientModule {
    var pattern: Pattern
    var symbol: Symbol
  }
  sealed abstract class NodeCheckFlags
  object NodeCheckFlags {
    case object TypeChecked extends NodeCheckFlags
    case object LexicalThis extends NodeCheckFlags
    case object CaptureThis extends NodeCheckFlags
    case object SuperInstance extends NodeCheckFlags
    case object SuperStatic extends NodeCheckFlags
    case object ContextChecked extends NodeCheckFlags
    case object AsyncMethodWithSuper extends NodeCheckFlags
    case object AsyncMethodWithSuperBinding extends NodeCheckFlags
    case object CaptureArguments extends NodeCheckFlags
    case object EnumValuesComputed extends NodeCheckFlags
    case object LexicalModuleMergesWithClass extends NodeCheckFlags
    case object LoopWithCapturedBlockScopedBinding extends NodeCheckFlags
    case object CapturedBlockScopedBinding extends NodeCheckFlags
    case object BlockScopedBindingInLoop extends NodeCheckFlags
    case object ClassWithBodyScopedClassBinding extends NodeCheckFlags
    case object BodyScopedClassBinding extends NodeCheckFlags
    case object NeedsLoopOutParameter extends NodeCheckFlags
    case object AssignmentsMarked extends NodeCheckFlags
    case object ClassWithConstructorReference extends NodeCheckFlags
    case object ConstructorReferenceInClass extends NodeCheckFlags
  }
  trait NodeLinks {
    var flags: NodeCheckFlags
    var resolvedType: Type
    var resolvedSignature: Signature
    var resolvedSymbol: Symbol
    var resolvedIndexInfo: IndexInfo
    var enumMemberValue: Int
    var isVisible: Boolean
    var hasReportedStatementInAmbientContext: Boolean
    var jsxFlags: JsxFlags
    var resolvedJsxType: Type
    var hasSuperCall: Boolean
    var superCall: ExpressionStatement
    var switchTypes: Array[Type]
  }
  sealed abstract class TypeFlags
  object TypeFlags {
    case object Any extends TypeFlags
    case object String extends TypeFlags
    case object Number extends TypeFlags
    case object Boolean extends TypeFlags
    case object Enum extends TypeFlags
    case object StringLiteral extends TypeFlags
    case object NumberLiteral extends TypeFlags
    case object BooleanLiteral extends TypeFlags
    case object EnumLiteral extends TypeFlags
    case object ESSymbol extends TypeFlags
    case object Void extends TypeFlags
    case object Undefined extends TypeFlags
    case object Null extends TypeFlags
    case object Never extends TypeFlags
    case object TypeParameter extends TypeFlags
    case object Object extends TypeFlags
    case object Union extends TypeFlags
    case object Intersection extends TypeFlags
    case object FreshLiteral extends TypeFlags
    case object ContainsWideningType extends TypeFlags
    case object ContainsObjectLiteral extends TypeFlags
    case object ContainsAnyFunctionType extends TypeFlags
    case object Nullable extends TypeFlags
    case object Literal extends TypeFlags
    case object StringOrNumberLiteral extends TypeFlags
    case object DefinitelyFalsy extends TypeFlags
    case object PossiblyFalsy extends TypeFlags
    case object Intrinsic extends TypeFlags
    case object Primitive extends TypeFlags
    case object StringLike extends TypeFlags
    case object NumberLike extends TypeFlags
    case object BooleanLike extends TypeFlags
    case object EnumLike extends TypeFlags
    case object UnionOrIntersection extends TypeFlags
    case object StructuredType extends TypeFlags
    case object StructuredOrTypeParameter extends TypeFlags
    case object Narrowable extends TypeFlags
    case object NotUnionOrUnit extends TypeFlags
    case object RequiresWidening extends TypeFlags
    case object PropagatingFlags extends TypeFlags
  }
  type DestructuringPattern =
    (BindingPattern | ObjectLiteralExpression | ArrayLiteralExpression)
  trait Type {
    var flags: TypeFlags
    var id: Int
    var symbol: Symbol
    var pattern: DestructuringPattern
    var aliasSymbol: Symbol
    var aliasTypeArguments: Array[Type]
  }
  trait IntrinsicType extends Type {
    var intrinsicName: String
  }
  trait LiteralType extends Type {
    var text: String
    var freshType: LiteralType
    var regularType: LiteralType
  }
  trait EnumType extends Type {
    var memberTypes: Map[EnumLiteralType]
  }
  trait EnumLiteralType extends LiteralType {
    var baseType: (EnumType with UnionType)
  }
  sealed abstract class ObjectFlags
  object ObjectFlags {
    case object Class extends ObjectFlags
    case object Interface extends ObjectFlags
    case object Reference extends ObjectFlags
    case object Tuple extends ObjectFlags
    case object Anonymous extends ObjectFlags
    case object Instantiated extends ObjectFlags
    case object ObjectLiteral extends ObjectFlags
    case object EvolvingArray extends ObjectFlags
    case object ObjectLiteralPatternWithComputedProperties extends ObjectFlags
    case object ClassOrInterface extends ObjectFlags
  }
  trait ObjectType extends Type {
    var objectFlags: ObjectFlags
  }
  trait InterfaceType extends ObjectType {
    var typeParameters: Array[TypeParameter]
    var outerTypeParameters: Array[TypeParameter]
    var localTypeParameters: Array[TypeParameter]
    var thisType: TypeParameter
    var resolvedBaseConstructorType: Type
    var resolvedBaseTypes: Array[ObjectType]
  }
  trait InterfaceTypeWithDeclaredMembers extends InterfaceType {
    var declaredProperties: Array[Symbol]
    var declaredCallSignatures: Array[Signature]
    var declaredConstructSignatures: Array[Signature]
    var declaredStringIndexInfo: IndexInfo
    var declaredNumberIndexInfo: IndexInfo
  }
  trait TypeReference extends ObjectType {
    var target: GenericType
    var typeArguments: Array[Type]
  }
  trait GenericType extends InterfaceType with TypeReference {
    var instantiations: Map[TypeReference]
  }
  trait UnionOrIntersectionType extends Type {
    var types: Array[Type]
    var resolvedProperties: SymbolTable
    var couldContainTypeParameters: Boolean
  }
  trait UnionType extends UnionOrIntersectionType {}
  trait IntersectionType extends UnionOrIntersectionType {}
  type StructuredType = (ObjectType | UnionType | IntersectionType)
  trait AnonymousType extends ObjectType {
    var target: AnonymousType
    var mapper: TypeMapper
  }
  trait EvolvingArrayType extends ObjectType {
    var elementType: Type
    var finalArrayType: Type
  }
  trait ResolvedType extends ObjectType with UnionOrIntersectionType {
    var members: SymbolTable
    var properties: Array[Symbol]
    var callSignatures: Array[Signature]
    var constructSignatures: Array[Signature]
    var stringIndexInfo: IndexInfo
    var numberIndexInfo: IndexInfo
  }
  trait FreshObjectLiteralType extends ResolvedType {
    var regularType: ResolvedType
  }
  trait IterableOrIteratorType extends ObjectType with UnionType {
    var iterableElementType: Type
    var iteratorElementType: Type
  }
  trait TypeParameter extends Type {
    var constraint: Type
    var target: TypeParameter
    var mapper: TypeMapper
    var resolvedApparentType: Type
    var isThisType: Boolean
  }
  sealed abstract class SignatureKind
  object SignatureKind {
    case object Call extends SignatureKind
    case object Construct extends SignatureKind
  }
  trait Signature {
    var declaration: SignatureDeclaration
    var typeParameters: Array[TypeParameter]
    var parameters: Array[Symbol]
    var thisParameter: Symbol
    var resolvedReturnType: Type
    var minArgumentCount: Int
    var hasRestParameter: Boolean
    var hasLiteralTypes: Boolean
    var target: Signature
    var mapper: TypeMapper
    var unionSignatures: Array[Signature]
    var erasedSignatureCache: Signature
    var isolatedSignatureType: ObjectType
    var typePredicate: TypePredicate
  }
  sealed abstract class IndexKind
  object IndexKind {
    case object String extends IndexKind
    case object Number extends IndexKind
  }
  trait IndexInfo {
    var `type`: Type
    var isReadonly: Boolean
    var declaration: SignatureDeclaration
  }
  trait TypeMapper {
    def apply(t: TypeParameter): Type
    var mappedTypes: Array[Type]
    var targetTypes: Array[Type]
    var instantiations: Array[Type]
    var context: InferenceContext
  }
  trait TypeInferences {
    var primary: Array[Type]
    var secondary: Array[Type]
    var topLevel: Boolean
    var isFixed: Boolean
  }
  trait InferenceContext {
    var signature: Signature
    var inferUnionTypes: Boolean
    var inferences: Array[TypeInferences]
    var inferredTypes: Array[Type]
    var mapper: TypeMapper
    var failedTypeParameterIndex: Int
  }
  sealed abstract class SpecialPropertyAssignmentKind
  object SpecialPropertyAssignmentKind {
    case object None extends SpecialPropertyAssignmentKind
    case object ExportsProperty extends SpecialPropertyAssignmentKind
    case object ModuleExports extends SpecialPropertyAssignmentKind
    case object PrototypeProperty extends SpecialPropertyAssignmentKind
    case object ThisProperty extends SpecialPropertyAssignmentKind
  }
  trait DiagnosticMessage {
    var key: String
    var category: DiagnosticCategory
    var code: Int
    var message: String
  }
  trait DiagnosticMessageChain {
    var messageText: String
    var category: DiagnosticCategory
    var code: Int
    var next: DiagnosticMessageChain
  }
  trait Diagnostic {
    var file: SourceFile
    var start: Int
    var length: Int
    var messageText: (String | DiagnosticMessageChain)
    var category: DiagnosticCategory
    var code: Int
  }
  sealed abstract class DiagnosticCategory
  object DiagnosticCategory {
    case object Warning extends DiagnosticCategory
    case object Error extends DiagnosticCategory
    case object Message extends DiagnosticCategory
  }
  sealed abstract class ModuleResolutionKind
  object ModuleResolutionKind {
    case object Classic extends ModuleResolutionKind
    case object NodeJs extends ModuleResolutionKind
  }
  type CompilerOptionsValue =
    (String | Int | Boolean | Array[(String | Int)] | Array[String] | MapLike[
      Array[String]])
  trait CompilerOptions {
    var allowJs: Boolean
    var allowNonTsExtensions: Boolean
    var allowSyntheticDefaultImports: Boolean
    var allowUnreachableCode: Boolean
    var allowUnusedLabels: Boolean
    var alwaysStrict: Boolean
    var baseUrl: String
    var charset: String
    var configFilePath: String
    var declaration: Boolean
    var declarationDir: String
    var diagnostics: Boolean
    var extendedDiagnostics: Boolean
    var disableSizeLimit: Boolean
    var emitBOM: Boolean
    var emitDecoratorMetadata: Boolean
    var experimentalDecorators: Boolean
    var forceConsistentCasingInFileNames: Boolean
    var help: Boolean
    var importHelpers: Boolean
    var init: Boolean
    var inlineSourceMap: Boolean
    var inlineSources: Boolean
    var isolatedModules: Boolean
    var jsx: JsxEmit
    var lib: Array[String]
    var listEmittedFiles: Boolean
    var listFiles: Boolean
    var locale: String
    var mapRoot: String
    var maxNodeModuleJsDepth: Int
    var module: ModuleKind
    var moduleResolution: ModuleResolutionKind
    var newLine: NewLineKind
    var noEmit: Boolean
    var noEmitOverwritenFiles: Boolean
    var noEmitHelpers: Boolean
    var noEmitOnError: Boolean
    var noErrorTruncation: Boolean
    var noFallthroughCasesInSwitch: Boolean
    var noImplicitAny: Boolean
    var noImplicitReturns: Boolean
    var noImplicitThis: Boolean
    var noUnusedLocals: Boolean
    var noUnusedParameters: Boolean
    var noImplicitUseStrict: Boolean
    var noLib: Boolean
    var noResolve: Boolean
    var out: String
    var outDir: String
    var outFile: String
    var paths: MapLike[Array[String]]
    var preserveConstEnums: Boolean
    var project: String
    var pretty: DiagnosticStyle
    var reactNamespace: String
    var removeComments: Boolean
    var rootDir: String
    var rootDirs: Array[String]
    var skipLibCheck: Boolean
    var skipDefaultLibCheck: Boolean
    var sourceMap: Boolean
    var sourceRoot: String
    var strictNullChecks: Boolean
    var stripInternal: Boolean
    var suppressExcessPropertyErrors: Boolean
    var suppressImplicitAnyIndexErrors: Boolean
    var suppressOutputPathCheck: Boolean
    var target: ScriptTarget
    var traceResolution: Boolean
    var types: Array[String]
    var typeRoots: Array[String]
    var version: Boolean
    var watch: Boolean
    def apply(option: String): (CompilerOptionsValue | undefined)
    /* def update() -- if you need it */
  }
  trait TypingOptions {
    var enableAutoDiscovery: Boolean
    var include: Array[String]
    var exclude: Array[String]
    def apply(option: String): (Array[String] | Boolean | undefined)
    /* def update() -- if you need it */
  }
  trait DiscoverTypingsInfo {
    var fileNames: Array[String]
    var projectRootPath: String
    var safeListPath: String
    var packageNameToTypingLocation: Map[String]
    var typingOptions: TypingOptions
    var compilerOptions: CompilerOptions
    var unresolvedImports: ReadonlyArray[String]
  }
  sealed abstract class ModuleKind
  object ModuleKind {
    case object None extends ModuleKind
    case object CommonJS extends ModuleKind
    case object AMD extends ModuleKind
    case object UMD extends ModuleKind
    case object System extends ModuleKind
    case object ES2015 extends ModuleKind
  }
  sealed abstract class JsxEmit
  object JsxEmit {
    case object None extends JsxEmit
    case object Preserve extends JsxEmit
    case object React extends JsxEmit
  }
  sealed abstract class NewLineKind
  object NewLineKind {
    case object CarriageReturnLineFeed extends NewLineKind
    case object LineFeed extends NewLineKind
  }
  trait LineAndCharacter {
    var line: Int
    var character: Int
  }
  sealed abstract class ScriptKind
  object ScriptKind {
    case object Unknown extends ScriptKind
    case object JS extends ScriptKind
    case object JSX extends ScriptKind
    case object TS extends ScriptKind
    case object TSX extends ScriptKind
  }
  sealed abstract class ScriptTarget
  object ScriptTarget {
    case object ES3 extends ScriptTarget
    case object ES5 extends ScriptTarget
    case object ES2015 extends ScriptTarget
    case object ES2016 extends ScriptTarget
    case object ES2017 extends ScriptTarget
    case object Latest extends ScriptTarget
  }
  sealed abstract class LanguageVariant
  object LanguageVariant {
    case object Standard extends LanguageVariant
    case object JSX extends LanguageVariant
  }
  sealed abstract class DiagnosticStyle
  object DiagnosticStyle {
    case object Simple extends DiagnosticStyle
    case object Pretty extends DiagnosticStyle
  }
  trait ParsedCommandLine {
    var options: CompilerOptions
    var typingOptions: TypingOptions
    var fileNames: Array[String]
    var raw: Any
    var errors: Array[Diagnostic]
    var wildcardDirectories: MapLike[WatchDirectoryFlags]
    var compileOnSave: Boolean
  }
  sealed abstract class WatchDirectoryFlags
  object WatchDirectoryFlags {
    case object None extends WatchDirectoryFlags
    case object Recursive extends WatchDirectoryFlags
  }
  trait ExpandResult {
    var fileNames: Array[String]
    var wildcardDirectories: MapLike[WatchDirectoryFlags]
  }
  trait CommandLineOptionBase {
    var name: String
    var `type`: (`"string"` | `"number"` | `"boolean"` | `"object"` | `"list"` | Map[
      (Int | String)])
    var isFilePath: Boolean
    var shortName: String
    var description: DiagnosticMessage
    var paramType: DiagnosticMessage
    var experimental: Boolean
    var isTSConfigOnly: Boolean
  }
  trait CommandLineOptionOfPrimitiveType extends CommandLineOptionBase {
    var `type`: (`"string"` | `"number"` | `"boolean"`)
  }
  trait CommandLineOptionOfCustomType extends CommandLineOptionBase {
    var `type`: Map[(Int | String)]
  }
  trait TsConfigOnlyOption extends CommandLineOptionBase {
    var `type`: `"object"`
  }
  trait CommandLineOptionOfListType extends CommandLineOptionBase {
    var `type`: `"list"`
    var element: (CommandLineOptionOfCustomType | CommandLineOptionOfPrimitiveType)
  }
  type CommandLineOption =
    (CommandLineOptionOfCustomType | CommandLineOptionOfPrimitiveType | TsConfigOnlyOption | CommandLineOptionOfListType)
  sealed abstract class CharacterCodes
  object CharacterCodes {
    case object nullCharacter extends CharacterCodes
    case object maxAsciiCharacter extends CharacterCodes
    case object lineFeed extends CharacterCodes
    case object carriageReturn extends CharacterCodes
    case object lineSeparator extends CharacterCodes
    case object paragraphSeparator extends CharacterCodes
    case object nextLine extends CharacterCodes
    case object space extends CharacterCodes
    case object nonBreakingSpace extends CharacterCodes
    case object enQuad extends CharacterCodes
    case object emQuad extends CharacterCodes
    case object enSpace extends CharacterCodes
    case object emSpace extends CharacterCodes
    case object threePerEmSpace extends CharacterCodes
    case object fourPerEmSpace extends CharacterCodes
    case object sixPerEmSpace extends CharacterCodes
    case object figureSpace extends CharacterCodes
    case object punctuationSpace extends CharacterCodes
    case object thinSpace extends CharacterCodes
    case object hairSpace extends CharacterCodes
    case object zeroWidthSpace extends CharacterCodes
    case object narrowNoBreakSpace extends CharacterCodes
    case object ideographicSpace extends CharacterCodes
    case object mathematicalSpace extends CharacterCodes
    case object ogham extends CharacterCodes
    case object _underscore_ extends CharacterCodes
    case object $ extends CharacterCodes
    case object _0 extends CharacterCodes
    case object _1 extends CharacterCodes
    case object _2 extends CharacterCodes
    case object _3 extends CharacterCodes
    case object _4 extends CharacterCodes
    case object _5 extends CharacterCodes
    case object _6 extends CharacterCodes
    case object _7 extends CharacterCodes
    case object _8 extends CharacterCodes
    case object _9 extends CharacterCodes
    case object a extends CharacterCodes
    case object b extends CharacterCodes
    case object c extends CharacterCodes
    case object d extends CharacterCodes
    case object e extends CharacterCodes
    case object f extends CharacterCodes
    case object g extends CharacterCodes
    case object h extends CharacterCodes
    case object i extends CharacterCodes
    case object j extends CharacterCodes
    case object k extends CharacterCodes
    case object l extends CharacterCodes
    case object m extends CharacterCodes
    case object n extends CharacterCodes
    case object o extends CharacterCodes
    case object p extends CharacterCodes
    case object q extends CharacterCodes
    case object r extends CharacterCodes
    case object s extends CharacterCodes
    case object t extends CharacterCodes
    case object u extends CharacterCodes
    case object v extends CharacterCodes
    case object w extends CharacterCodes
    case object x extends CharacterCodes
    case object y extends CharacterCodes
    case object z extends CharacterCodes
    case object A extends CharacterCodes
    case object B extends CharacterCodes
    case object C extends CharacterCodes
    case object D extends CharacterCodes
    case object E extends CharacterCodes
    case object F extends CharacterCodes
    case object G extends CharacterCodes
    case object H extends CharacterCodes
    case object I extends CharacterCodes
    case object J extends CharacterCodes
    case object K extends CharacterCodes
    case object L extends CharacterCodes
    case object M extends CharacterCodes
    case object N extends CharacterCodes
    case object O extends CharacterCodes
    case object P extends CharacterCodes
    case object Q extends CharacterCodes
    case object R extends CharacterCodes
    case object S extends CharacterCodes
    case object T extends CharacterCodes
    case object U extends CharacterCodes
    case object V extends CharacterCodes
    case object W extends CharacterCodes
    case object X extends CharacterCodes
    case object Y extends CharacterCodes
    case object Z extends CharacterCodes
    case object ampersand extends CharacterCodes
    case object asterisk extends CharacterCodes
    case object at extends CharacterCodes
    case object backslash extends CharacterCodes
    case object backtick extends CharacterCodes
    case object bar extends CharacterCodes
    case object caret extends CharacterCodes
    case object closeBrace extends CharacterCodes
    case object closeBracket extends CharacterCodes
    case object closeParen extends CharacterCodes
    case object colon extends CharacterCodes
    case object comma extends CharacterCodes
    case object dot extends CharacterCodes
    case object doubleQuote extends CharacterCodes
    case object equals extends CharacterCodes
    case object exclamation extends CharacterCodes
    case object greaterThan extends CharacterCodes
    case object hash extends CharacterCodes
    case object lessThan extends CharacterCodes
    case object minus extends CharacterCodes
    case object openBrace extends CharacterCodes
    case object openBracket extends CharacterCodes
    case object openParen extends CharacterCodes
    case object percent extends CharacterCodes
    case object plus extends CharacterCodes
    case object question extends CharacterCodes
    case object semicolon extends CharacterCodes
    case object singleQuote extends CharacterCodes
    case object slash extends CharacterCodes
    case object tilde extends CharacterCodes
    case object backspace extends CharacterCodes
    case object formFeed extends CharacterCodes
    case object byteOrderMark extends CharacterCodes
    case object tab extends CharacterCodes
    case object verticalTab extends CharacterCodes
  }
  trait ModuleResolutionHost {
    def fileExists(fileName: String): Boolean
    def readFile(fileName: String): String
    def trace(s: String): Unit
    def directoryExists(directoryName: String): Boolean
    def realpath(path: String): String
    def getCurrentDirectory(): String
    def getDirectories(path: String): Array[String]
  }
  trait ResolvedModule {
    var resolvedFileName: String
    var isExternalLibraryImport: Boolean
  }
  trait ResolvedModuleWithFailedLookupLocations {
    var resolvedModule: ResolvedModule
    var failedLookupLocations: Array[String]
  }
  trait ResolvedTypeReferenceDirective {
    var primary: Boolean
    var resolvedFileName: String
  }
  trait ResolvedTypeReferenceDirectiveWithFailedLookupLocations {
    var resolvedTypeReferenceDirective: ResolvedTypeReferenceDirective
    var failedLookupLocations: Array[String]
  }
  trait CompilerHost extends ModuleResolutionHost {
    def getSourceFile(fileName: String,
                      languageVersion: ScriptTarget,
                      onError: ((String) => Unit)): SourceFile
    def getSourceFileByPath(fileName: String,
                            path: Path,
                            languageVersion: ScriptTarget,
                            onError: ((String) => Unit)): SourceFile
    def getCancellationToken(): CancellationToken
    def getDefaultLibFileName(options: CompilerOptions): String
    def getDefaultLibLocation(): String
    var writeFile: WriteFileCallback
    def getCurrentDirectory(): String
    def getDirectories(path: String): Array[String]
    def getCanonicalFileName(fileName: String): String
    def useCaseSensitiveFileNames(): Boolean
    def getNewLine(): String
    def resolveModuleNames(moduleNames: Array[String],
                           containingFile: String): Array[ResolvedModule]
    def resolveTypeReferenceDirectives(
        typeReferenceDirectiveNames: Array[String],
        containingFile: String): Array[ResolvedTypeReferenceDirective]
    def getEnvironmentVariable(name: String): String
  }
  sealed abstract class TransformFlags
  object TransformFlags {
    case object None extends TransformFlags
    case object TypeScript extends TransformFlags
    case object ContainsTypeScript extends TransformFlags
    case object Jsx extends TransformFlags
    case object ContainsJsx extends TransformFlags
    case object ES2017 extends TransformFlags
    case object ContainsES2017 extends TransformFlags
    case object ES2016 extends TransformFlags
    case object ContainsES2016 extends TransformFlags
    case object ES2015 extends TransformFlags
    case object ContainsES2015 extends TransformFlags
    case object DestructuringAssignment extends TransformFlags
    case object Generator extends TransformFlags
    case object ContainsGenerator extends TransformFlags
    case object ContainsDecorators extends TransformFlags
    case object ContainsPropertyInitializer extends TransformFlags
    case object ContainsLexicalThis extends TransformFlags
    case object ContainsCapturedLexicalThis extends TransformFlags
    case object ContainsLexicalThisInComputedPropertyName
        extends TransformFlags
    case object ContainsDefaultValueAssignments extends TransformFlags
    case object ContainsParameterPropertyAssignments extends TransformFlags
    case object ContainsSpreadElementExpression extends TransformFlags
    case object ContainsComputedPropertyName extends TransformFlags
    case object ContainsBlockScopedBinding extends TransformFlags
    case object ContainsBindingPattern extends TransformFlags
    case object ContainsYield extends TransformFlags
    case object ContainsHoistedDeclarationOrCompletion extends TransformFlags
    case object HasComputedFlags extends TransformFlags
    case object AssertTypeScript extends TransformFlags
    case object AssertJsx extends TransformFlags
    case object AssertES2017 extends TransformFlags
    case object AssertES2016 extends TransformFlags
    case object AssertES2015 extends TransformFlags
    case object AssertGenerator extends TransformFlags
    case object NodeExcludes extends TransformFlags
    case object ArrowFunctionExcludes extends TransformFlags
    case object FunctionExcludes extends TransformFlags
    case object ConstructorExcludes extends TransformFlags
    case object MethodOrAccessorExcludes extends TransformFlags
    case object ClassExcludes extends TransformFlags
    case object ModuleExcludes extends TransformFlags
    case object TypeExcludes extends TransformFlags
    case object ObjectLiteralExcludes extends TransformFlags
    case object ArrayLiteralOrCallOrNewExcludes extends TransformFlags
    case object VariableDeclarationListExcludes extends TransformFlags
    case object ParameterExcludes extends TransformFlags
    case object TypeScriptClassSyntaxMask extends TransformFlags
    case object ES2015FunctionSyntaxMask extends TransformFlags
  }
  trait EmitNode {
    var flags: EmitFlags
    var commentRange: TextRange
    var sourceMapRange: TextRange
    var tokenSourceMapRanges: Map[TextRange]
    var annotatedNodes: Array[Node]
    var constantValue: Int
  }
  sealed abstract class EmitFlags
  object EmitFlags {
    case object EmitEmitHelpers extends EmitFlags
    case object EmitExportStar extends EmitFlags
    case object EmitSuperHelper extends EmitFlags
    case object EmitAdvancedSuperHelper extends EmitFlags
    case object UMDDefine extends EmitFlags
    case object SingleLine extends EmitFlags
    case object AdviseOnEmitNode extends EmitFlags
    case object NoSubstitution extends EmitFlags
    case object CapturesThis extends EmitFlags
    case object NoLeadingSourceMap extends EmitFlags
    case object NoTrailingSourceMap extends EmitFlags
    case object NoSourceMap extends EmitFlags
    case object NoNestedSourceMaps extends EmitFlags
    case object NoTokenLeadingSourceMaps extends EmitFlags
    case object NoTokenTrailingSourceMaps extends EmitFlags
    case object NoTokenSourceMaps extends EmitFlags
    case object NoLeadingComments extends EmitFlags
    case object NoTrailingComments extends EmitFlags
    case object NoComments extends EmitFlags
    case object NoNestedComments extends EmitFlags
    case object ExportName extends EmitFlags
    case object LocalName extends EmitFlags
    case object Indented extends EmitFlags
    case object NoIndentation extends EmitFlags
    case object AsyncFunctionBody extends EmitFlags
    case object ReuseTempVariableScope extends EmitFlags
    case object CustomPrologue extends EmitFlags
  }
  sealed abstract class EmitContext
  object EmitContext {
    case object SourceFile extends EmitContext
    case object Expression extends EmitContext
    case object IdentifierName extends EmitContext
    case object Unspecified extends EmitContext
  }
  trait LexicalEnvironment {
    def startLexicalEnvironment(): Unit
    def endLexicalEnvironment(): Array[Statement]
  }
  trait TextSpan {
    var start: Int
    var length: Int
  }
  trait TextChangeRange {
    var span: TextSpan
    var newLength: Int
  }
  trait DiagnosticCollection {
    def add(diagnostic: Diagnostic): Unit
    def getGlobalDiagnostics(): Array[Diagnostic]
    def getDiagnostics(fileName: String): Array[Diagnostic]
    def getModificationCount(): Int
    def reattachFileDiagnostics(newFile: SourceFile): Unit
  }
  trait SyntaxList extends Node {
    var _children: Array[Node]
  }
}
