package scalatscomp

object Scanner {

  trait ErrorCallback {
    def apply(message: DiagnosticMessage, length: Int): Unit
  }

  def tokenIsIdentifierOrKeyword(token: SyntaxKind): Boolean = {
    return (token >= SyntaxKind.Identifier)

  }

  trait Scanner {
    def getStartPos(): Int

    def getToken(): SyntaxKind

    def getTextPos(): Int

    def getTokenPos(): Int

    def getTokenText(): String

    def getTokenValue(): String

    def hasExtendedUnicodeEscape(): Boolean

    def hasPrecedingLineBreak(): Boolean

    def isIdentifier(): Boolean

    def isReservedWord(): Boolean

    def isUnterminated(): Boolean

    def reScanGreaterToken(): SyntaxKind

    def reScanSlashToken(): SyntaxKind

    def reScanTemplateToken(): SyntaxKind

    def scanJsxIdentifier(): SyntaxKind

    def scanJsxAttributeValue(): SyntaxKind

    def reScanJsxToken(): SyntaxKind

    def scanJsxToken(): SyntaxKind

    def scanJSDocToken(): SyntaxKind

    def scan(): SyntaxKind

    def getText(): String

    def setText(text: String, start: Int, length: Int): Unit

    def setOnError(onError: ErrorCallback): Unit

    def setScriptTarget(scriptTarget: ScriptTarget): Unit

    def setLanguageVariant(variant: LanguageVariant): Unit

    def setTextPos(textPos: Int): Unit

    def lookAhead[T](callback: (() => T)): T

    def scanRange[T](start: Int, length: Int, callback: (() => T)): T

    def tryScan[T](callback: (() => T)): T
  }

  val textToToken = createMap(Map("abstract" -> SyntaxKind.AbstractKeyword,
    "any" -> SyntaxKind.AnyKeyword,
    "as" -> SyntaxKind.AsKeyword,
    "boolean" -> SyntaxKind.BooleanKeyword,
    "break" -> SyntaxKind.BreakKeyword,
    "case" -> SyntaxKind.CaseKeyword,
    "catch" -> SyntaxKind.CatchKeyword,
    "class" -> SyntaxKind.ClassKeyword,
    "continue" -> SyntaxKind.ContinueKeyword,
    "const" -> SyntaxKind.ConstKeyword,
    "constructor" -> SyntaxKind.ConstructorKeyword,
    "debugger" -> SyntaxKind.DebuggerKeyword,
    "declare" -> SyntaxKind.DeclareKeyword,
    "default" -> SyntaxKind.DefaultKeyword,
    "delete" -> SyntaxKind.DeleteKeyword,
    "do" -> SyntaxKind.DoKeyword,
    "else" -> SyntaxKind.ElseKeyword,
    "enum" -> SyntaxKind.EnumKeyword,
    "export" -> SyntaxKind.ExportKeyword,
    "extends" -> SyntaxKind.ExtendsKeyword,
    "false" -> SyntaxKind.FalseKeyword,
    "finally" -> SyntaxKind.FinallyKeyword,
    "for" -> SyntaxKind.ForKeyword,
    "from" -> SyntaxKind.FromKeyword,
    "function" -> SyntaxKind.FunctionKeyword,
    "get" -> SyntaxKind.GetKeyword,
    "if" -> SyntaxKind.IfKeyword,
    "implements" -> SyntaxKind.ImplementsKeyword,
    "import" -> SyntaxKind.ImportKeyword,
    "in" -> SyntaxKind.InKeyword,
    "instanceof" -> SyntaxKind.InstanceOfKeyword,
    "interface" -> SyntaxKind.InterfaceKeyword,
    "is" -> SyntaxKind.IsKeyword,
    "let" -> SyntaxKind.LetKeyword,
    "module" -> SyntaxKind.ModuleKeyword,
    "namespace" -> SyntaxKind.NamespaceKeyword,
    "never" -> SyntaxKind.NeverKeyword,
    "new" -> SyntaxKind.NewKeyword,
    "null" -> SyntaxKind.NullKeyword,
    "number" -> SyntaxKind.NumberKeyword,
    "package" -> SyntaxKind.PackageKeyword,
    "private" -> SyntaxKind.PrivateKeyword,
    "protected" -> SyntaxKind.ProtectedKeyword,
    "public" -> SyntaxKind.PublicKeyword,
    "readonly" -> SyntaxKind.ReadonlyKeyword,
    "require" -> SyntaxKind.RequireKeyword,
    "global" -> SyntaxKind.GlobalKeyword,
    "return" -> SyntaxKind.ReturnKeyword,
    "set" -> SyntaxKind.SetKeyword,
    "static" -> SyntaxKind.StaticKeyword,
    "string" -> SyntaxKind.StringKeyword,
    "super" -> SyntaxKind.SuperKeyword,
    "switch" -> SyntaxKind.SwitchKeyword,
    "symbol" -> SyntaxKind.SymbolKeyword,
    "this" -> SyntaxKind.ThisKeyword,
    "throw" -> SyntaxKind.ThrowKeyword,
    "true" -> SyntaxKind.TrueKeyword,
    "try" -> SyntaxKind.TryKeyword,
    "type" -> SyntaxKind.TypeKeyword,
    "typeof" -> SyntaxKind.TypeOfKeyword,
    "undefined" -> SyntaxKind.UndefinedKeyword,
    "var" -> SyntaxKind.VarKeyword,
    "void" -> SyntaxKind.VoidKeyword,
    "while" -> SyntaxKind.WhileKeyword,
    "with" -> SyntaxKind.WithKeyword,
    "yield" -> SyntaxKind.YieldKeyword,
    "async" -> SyntaxKind.AsyncKeyword,
    "await" -> SyntaxKind.AwaitKeyword,
    "of" -> SyntaxKind.OfKeyword,
    "{" -> SyntaxKind.OpenBraceToken,
    "}" -> SyntaxKind.CloseBraceToken,
    "(" -> SyntaxKind.OpenParenToken,
    ")" -> SyntaxKind.CloseParenToken,
    "[" -> SyntaxKind.OpenBracketToken,
    "]" -> SyntaxKind.CloseBracketToken,
    "." -> SyntaxKind.DotToken,
    "..." -> SyntaxKind.DotDotDotToken,
    ";" -> SyntaxKind.SemicolonToken,
    "," -> SyntaxKind.CommaToken,
    "<" -> SyntaxKind.LessThanToken,
    ">" -> SyntaxKind.GreaterThanToken,
    "<=" -> SyntaxKind.LessThanEqualsToken,
    ">=" -> SyntaxKind.GreaterThanEqualsToken,
    "==" -> SyntaxKind.EqualsEqualsToken,
    "!=" -> SyntaxKind.ExclamationEqualsToken,
    "===" -> SyntaxKind.EqualsEqualsEqualsToken,
    "!==" -> SyntaxKind.ExclamationEqualsEqualsToken,
    "=>" -> SyntaxKind.EqualsGreaterThanToken,
    "+" -> SyntaxKind.PlusToken,
    "-" -> SyntaxKind.MinusToken,
    "**" -> SyntaxKind.AsteriskAsteriskToken,
    "*" -> SyntaxKind.AsteriskToken,
    "/" -> SyntaxKind.SlashToken,
    "%" -> SyntaxKind.PercentToken,
    "++" -> SyntaxKind.PlusPlusToken,
    "--" -> SyntaxKind.MinusMinusToken,
    "<<" -> SyntaxKind.LessThanLessThanToken,
    "</" -> SyntaxKind.LessThanSlashToken,
    ">>" -> SyntaxKind.GreaterThanGreaterThanToken,
    ">>>" -> SyntaxKind.GreaterThanGreaterThanGreaterThanToken,
    "&" -> SyntaxKind.AmpersandToken,
    "|" -> SyntaxKind.BarToken,
    "^" -> SyntaxKind.CaretToken,
    "!" -> SyntaxKind.ExclamationToken,
    "~" -> SyntaxKind.TildeToken,
    "&&" -> SyntaxKind.AmpersandAmpersandToken,
    "||" -> SyntaxKind.BarBarToken,
    "?" -> SyntaxKind.QuestionToken,
    ":" -> SyntaxKind.ColonToken,
    "=" -> SyntaxKind.EqualsToken,
    "+=" -> SyntaxKind.PlusEqualsToken,
    "-=" -> SyntaxKind.MinusEqualsToken,
    "*=" -> SyntaxKind.AsteriskEqualsToken,
    "**=" -> SyntaxKind.AsteriskAsteriskEqualsToken,
    "/=" -> SyntaxKind.SlashEqualsToken,
    "%=" -> SyntaxKind.PercentEqualsToken,
    "<<=" -> SyntaxKind.LessThanLessThanEqualsToken,
    ">>=" -> SyntaxKind.GreaterThanGreaterThanEqualsToken,
    ">>>=" -> SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken,
    "&=" -> SyntaxKind.AmpersandEqualsToken,
    "|=" -> SyntaxKind.BarEqualsToken,
    "^=" -> SyntaxKind.CaretEqualsToken,
    "@" -> SyntaxKind.AtToken))
  val unicodeES3IdentifierStart = Array(170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 543, 546, 563, 592, 685, 688, 696, 699, 705, 720, 721, 736, 740, 750, 750, 890, 890, 902, 902, 904, 906, 908, 908, 910, 929, 931, 974, 976, 983, 986, 1011, 1024, 1153, 1164, 1220, 1223, 1224, 1227, 1228, 1232, 1269, 1272, 1273, 1329, 1366, 1369, 1369, 1377, 1415, 1488, 1514, 1520, 1522, 1569, 1594, 1600, 1610, 1649, 1747, 1749, 1749, 1765, 1766, 1786, 1788, 1808, 1808, 1810, 1836, 1920, 1957, 2309, 2361, 2365, 2365, 2384, 2384, 2392, 2401, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2524, 2525, 2527, 2529, 2544, 2545, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2649, 2652, 2654, 2654, 2674, 2676, 2693, 2699, 2701, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2749, 2749, 2768, 2768, 2784, 2784, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2870, 2873, 2877, 2877, 2908, 2909, 2911, 2913, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 2997, 2999, 3001, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3168, 3169, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3294, 3294, 3296, 3297, 3333, 3340, 3342, 3344, 3346, 3368, 3370, 3385, 3424, 3425, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3585, 3632, 3634, 3635, 3648, 3654, 3713, 3714, 3716, 3716, 3719, 3720, 3722, 3722, 3725, 3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749, 3751, 3751, 3754, 3755, 3757, 3760, 3762, 3763, 3773, 3773, 3776, 3780, 3782, 3782, 3804, 3805, 3840, 3840, 3904, 3911, 3913, 3946, 3976, 3979, 4096, 4129, 4131, 4135, 4137, 4138, 4176, 4181, 4256, 4293, 4304, 4342, 4352, 4441, 4447, 4514, 4520, 4601, 4608, 4614, 4616, 4678, 4680, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4742, 4744, 4744, 4746, 4749, 4752, 4782, 4784, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4814, 4816, 4822, 4824, 4846, 4848, 4878, 4880, 4880, 4882, 4885, 4888, 4894, 4896, 4934, 4936, 4954, 5024, 5108, 5121, 5740, 5743, 5750, 5761, 5786, 5792, 5866, 6016, 6067, 6176, 6263, 6272, 6312, 7680, 7835, 7840, 7929, 7936, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8319, 8319, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495, 8497, 8499, 8505, 8544, 8579, 12293, 12295, 12321, 12329, 12337, 12341, 12344, 12346, 12353, 12436, 12445, 12446, 12449, 12538, 12540, 12542, 12549, 12588, 12593, 12686, 12704, 12727, 13312, 19893, 19968, 40869, 40960, 42124, 44032, 55203, 63744, 64045, 64256, 64262, 64275, 64279, 64285, 64285, 64287, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65136, 65138, 65140, 65140, 65142, 65276, 65313, 65338, 65345, 65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500)
  val unicodeES3IdentifierPart = Array(170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 543, 546, 563, 592, 685, 688, 696, 699, 705, 720, 721, 736, 740, 750, 750, 768, 846, 864, 866, 890, 890, 902, 902, 904, 906, 908, 908, 910, 929, 931, 974, 976, 983, 986, 1011, 1024, 1153, 1155, 1158, 1164, 1220, 1223, 1224, 1227, 1228, 1232, 1269, 1272, 1273, 1329, 1366, 1369, 1369, 1377, 1415, 1425, 1441, 1443, 1465, 1467, 1469, 1471, 1471, 1473, 1474, 1476, 1476, 1488, 1514, 1520, 1522, 1569, 1594, 1600, 1621, 1632, 1641, 1648, 1747, 1749, 1756, 1759, 1768, 1770, 1773, 1776, 1788, 1808, 1836, 1840, 1866, 1920, 1968, 2305, 2307, 2309, 2361, 2364, 2381, 2384, 2388, 2392, 2403, 2406, 2415, 2433, 2435, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2492, 2492, 2494, 2500, 2503, 2504, 2507, 2509, 2519, 2519, 2524, 2525, 2527, 2531, 2534, 2545, 2562, 2562, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2620, 2620, 2622, 2626, 2631, 2632, 2635, 2637, 2649, 2652, 2654, 2654, 2662, 2676, 2689, 2691, 2693, 2699, 2701, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2748, 2757, 2759, 2761, 2763, 2765, 2768, 2768, 2784, 2784, 2790, 2799, 2817, 2819, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2870, 2873, 2876, 2883, 2887, 2888, 2891, 2893, 2902, 2903, 2908, 2909, 2911, 2913, 2918, 2927, 2946, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 2997, 2999, 3001, 3006, 3010, 3014, 3016, 3018, 3021, 3031, 3031, 3047, 3055, 3073, 3075, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3134, 3140, 3142, 3144, 3146, 3149, 3157, 3158, 3168, 3169, 3174, 3183, 3202, 3203, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3262, 3268, 3270, 3272, 3274, 3277, 3285, 3286, 3294, 3294, 3296, 3297, 3302, 3311, 3330, 3331, 3333, 3340, 3342, 3344, 3346, 3368, 3370, 3385, 3390, 3395, 3398, 3400, 3402, 3405, 3415, 3415, 3424, 3425, 3430, 3439, 3458, 3459, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3530, 3530, 3535, 3540, 3542, 3542, 3544, 3551, 3570, 3571, 3585, 3642, 3648, 3662, 3664, 3673, 3713, 3714, 3716, 3716, 3719, 3720, 3722, 3722, 3725, 3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749, 3751, 3751, 3754, 3755, 3757, 3769, 3771, 3773, 3776, 3780, 3782, 3782, 3784, 3789, 3792, 3801, 3804, 3805, 3840, 3840, 3864, 3865, 3872, 3881, 3893, 3893, 3895, 3895, 3897, 3897, 3902, 3911, 3913, 3946, 3953, 3972, 3974, 3979, 3984, 3991, 3993, 4028, 4038, 4038, 4096, 4129, 4131, 4135, 4137, 4138, 4140, 4146, 4150, 4153, 4160, 4169, 4176, 4185, 4256, 4293, 4304, 4342, 4352, 4441, 4447, 4514, 4520, 4601, 4608, 4614, 4616, 4678, 4680, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4742, 4744, 4744, 4746, 4749, 4752, 4782, 4784, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4814, 4816, 4822, 4824, 4846, 4848, 4878, 4880, 4880, 4882, 4885, 4888, 4894, 4896, 4934, 4936, 4954, 4969, 4977, 5024, 5108, 5121, 5740, 5743, 5750, 5761, 5786, 5792, 5866, 6016, 6099, 6112, 6121, 6160, 6169, 6176, 6263, 6272, 6313, 7680, 7835, 7840, 7929, 7936, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8255, 8256, 8319, 8319, 8400, 8412, 8417, 8417, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495, 8497, 8499, 8505, 8544, 8579, 12293, 12295, 12321, 12335, 12337, 12341, 12344, 12346, 12353, 12436, 12441, 12442, 12445, 12446, 12449, 12542, 12549, 12588, 12593, 12686, 12704, 12727, 13312, 19893, 19968, 40869, 40960, 42124, 44032, 55203, 63744, 64045, 64256, 64262, 64275, 64279, 64285, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65056, 65059, 65075, 65076, 65101, 65103, 65136, 65138, 65140, 65140, 65142, 65276, 65296, 65305, 65313, 65338, 65343, 65343, 65345, 65370, 65381, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500)
  val unicodeES5IdentifierStart = Array(170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 705, 710, 721, 736, 740, 748, 748, 750, 750, 880, 884, 886, 887, 890, 893, 902, 902, 904, 906, 908, 908, 910, 929, 931, 1013, 1015, 1153, 1162, 1319, 1329, 1366, 1369, 1369, 1377, 1415, 1488, 1514, 1520, 1522, 1568, 1610, 1646, 1647, 1649, 1747, 1749, 1749, 1765, 1766, 1774, 1775, 1786, 1788, 1791, 1791, 1808, 1808, 1810, 1839, 1869, 1957, 1969, 1969, 1994, 2026, 2036, 2037, 2042, 2042, 2048, 2069, 2074, 2074, 2084, 2084, 2088, 2088, 2112, 2136, 2208, 2208, 2210, 2220, 2308, 2361, 2365, 2365, 2384, 2384, 2392, 2401, 2417, 2423, 2425, 2431, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2493, 2493, 2510, 2510, 2524, 2525, 2527, 2529, 2544, 2545, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2649, 2652, 2654, 2654, 2674, 2676, 2693, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2749, 2749, 2768, 2768, 2784, 2785, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2869, 2873, 2877, 2877, 2908, 2909, 2911, 2913, 2929, 2929, 2947, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 3001, 3024, 3024, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3133, 3133, 3160, 3161, 3168, 3169, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3261, 3261, 3294, 3294, 3296, 3297, 3313, 3314, 3333, 3340, 3342, 3344, 3346, 3386, 3389, 3389, 3406, 3406, 3424, 3425, 3450, 3455, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3585, 3632, 3634, 3635, 3648, 3654, 3713, 3714, 3716, 3716, 3719, 3720, 3722, 3722, 3725, 3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749, 3751, 3751, 3754, 3755, 3757, 3760, 3762, 3763, 3773, 3773, 3776, 3780, 3782, 3782, 3804, 3807, 3840, 3840, 3904, 3911, 3913, 3948, 3976, 3980, 4096, 4138, 4159, 4159, 4176, 4181, 4186, 4189, 4193, 4193, 4197, 4198, 4206, 4208, 4213, 4225, 4238, 4238, 4256, 4293, 4295, 4295, 4301, 4301, 4304, 4346, 4348, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4744, 4746, 4749, 4752, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4822, 4824, 4880, 4882, 4885, 4888, 4954, 4992, 5007, 5024, 5108, 5121, 5740, 5743, 5759, 5761, 5786, 5792, 5866, 5870, 5872, 5888, 5900, 5902, 5905, 5920, 5937, 5952, 5969, 5984, 5996, 5998, 6000, 6016, 6067, 6103, 6103, 6108, 6108, 6176, 6263, 6272, 6312, 6314, 6314, 6320, 6389, 6400, 6428, 6480, 6509, 6512, 6516, 6528, 6571, 6593, 6599, 6656, 6678, 6688, 6740, 6823, 6823, 6917, 6963, 6981, 6987, 7043, 7072, 7086, 7087, 7098, 7141, 7168, 7203, 7245, 7247, 7258, 7293, 7401, 7404, 7406, 7409, 7413, 7414, 7424, 7615, 7680, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8305, 8305, 8319, 8319, 8336, 8348, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495, 8505, 8508, 8511, 8517, 8521, 8526, 8526, 8544, 8584, 11264, 11310, 11312, 11358, 11360, 11492, 11499, 11502, 11506, 11507, 11520, 11557, 11559, 11559, 11565, 11565, 11568, 11623, 11631, 11631, 11648, 11670, 11680, 11686, 11688, 11694, 11696, 11702, 11704, 11710, 11712, 11718, 11720, 11726, 11728, 11734, 11736, 11742, 11823, 11823, 12293, 12295, 12321, 12329, 12337, 12341, 12344, 12348, 12353, 12438, 12445, 12447, 12449, 12538, 12540, 12543, 12549, 12589, 12593, 12686, 12704, 12730, 12784, 12799, 13312, 19893, 19968, 40908, 40960, 42124, 42192, 42237, 42240, 42508, 42512, 42527, 42538, 42539, 42560, 42606, 42623, 42647, 42656, 42735, 42775, 42783, 42786, 42888, 42891, 42894, 42896, 42899, 42912, 42922, 43000, 43009, 43011, 43013, 43015, 43018, 43020, 43042, 43072, 43123, 43138, 43187, 43250, 43255, 43259, 43259, 43274, 43301, 43312, 43334, 43360, 43388, 43396, 43442, 43471, 43471, 43520, 43560, 43584, 43586, 43588, 43595, 43616, 43638, 43642, 43642, 43648, 43695, 43697, 43697, 43701, 43702, 43705, 43709, 43712, 43712, 43714, 43714, 43739, 43741, 43744, 43754, 43762, 43764, 43777, 43782, 43785, 43790, 43793, 43798, 43808, 43814, 43816, 43822, 43968, 44002, 44032, 55203, 55216, 55238, 55243, 55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64285, 64285, 64287, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65136, 65140, 65142, 65276, 65313, 65338, 65345, 65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500)
  val unicodeES5IdentifierPart = Array(170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 705, 710, 721, 736, 740, 748, 748, 750, 750, 768, 884, 886, 887, 890, 893, 902, 902, 904, 906, 908, 908, 910, 929, 931, 1013, 1015, 1153, 1155, 1159, 1162, 1319, 1329, 1366, 1369, 1369, 1377, 1415, 1425, 1469, 1471, 1471, 1473, 1474, 1476, 1477, 1479, 1479, 1488, 1514, 1520, 1522, 1552, 1562, 1568, 1641, 1646, 1747, 1749, 1756, 1759, 1768, 1770, 1788, 1791, 1791, 1808, 1866, 1869, 1969, 1984, 2037, 2042, 2042, 2048, 2093, 2112, 2139, 2208, 2208, 2210, 2220, 2276, 2302, 2304, 2403, 2406, 2415, 2417, 2423, 2425, 2431, 2433, 2435, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2492, 2500, 2503, 2504, 2507, 2510, 2519, 2519, 2524, 2525, 2527, 2531, 2534, 2545, 2561, 2563, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2620, 2620, 2622, 2626, 2631, 2632, 2635, 2637, 2641, 2641, 2649, 2652, 2654, 2654, 2662, 2677, 2689, 2691, 2693, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2748, 2757, 2759, 2761, 2763, 2765, 2768, 2768, 2784, 2787, 2790, 2799, 2817, 2819, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2869, 2873, 2876, 2884, 2887, 2888, 2891, 2893, 2902, 2903, 2908, 2909, 2911, 2915, 2918, 2927, 2929, 2929, 2946, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 3001, 3006, 3010, 3014, 3016, 3018, 3021, 3024, 3024, 3031, 3031, 3046, 3055, 3073, 3075, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3133, 3140, 3142, 3144, 3146, 3149, 3157, 3158, 3160, 3161, 3168, 3171, 3174, 3183, 3202, 3203, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3260, 3268, 3270, 3272, 3274, 3277, 3285, 3286, 3294, 3294, 3296, 3299, 3302, 3311, 3313, 3314, 3330, 3331, 3333, 3340, 3342, 3344, 3346, 3386, 3389, 3396, 3398, 3400, 3402, 3406, 3415, 3415, 3424, 3427, 3430, 3439, 3450, 3455, 3458, 3459, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3530, 3530, 3535, 3540, 3542, 3542, 3544, 3551, 3570, 3571, 3585, 3642, 3648, 3662, 3664, 3673, 3713, 3714, 3716, 3716, 3719, 3720, 3722, 3722, 3725, 3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749, 3751, 3751, 3754, 3755, 3757, 3769, 3771, 3773, 3776, 3780, 3782, 3782, 3784, 3789, 3792, 3801, 3804, 3807, 3840, 3840, 3864, 3865, 3872, 3881, 3893, 3893, 3895, 3895, 3897, 3897, 3902, 3911, 3913, 3948, 3953, 3972, 3974, 3991, 3993, 4028, 4038, 4038, 4096, 4169, 4176, 4253, 4256, 4293, 4295, 4295, 4301, 4301, 4304, 4346, 4348, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4744, 4746, 4749, 4752, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4822, 4824, 4880, 4882, 4885, 4888, 4954, 4957, 4959, 4992, 5007, 5024, 5108, 5121, 5740, 5743, 5759, 5761, 5786, 5792, 5866, 5870, 5872, 5888, 5900, 5902, 5908, 5920, 5940, 5952, 5971, 5984, 5996, 5998, 6000, 6002, 6003, 6016, 6099, 6103, 6103, 6108, 6109, 6112, 6121, 6155, 6157, 6160, 6169, 6176, 6263, 6272, 6314, 6320, 6389, 6400, 6428, 6432, 6443, 6448, 6459, 6470, 6509, 6512, 6516, 6528, 6571, 6576, 6601, 6608, 6617, 6656, 6683, 6688, 6750, 6752, 6780, 6783, 6793, 6800, 6809, 6823, 6823, 6912, 6987, 6992, 7001, 7019, 7027, 7040, 7155, 7168, 7223, 7232, 7241, 7245, 7293, 7376, 7378, 7380, 7414, 7424, 7654, 7676, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8204, 8205, 8255, 8256, 8276, 8276, 8305, 8305, 8319, 8319, 8336, 8348, 8400, 8412, 8417, 8417, 8421, 8432, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495, 8505, 8508, 8511, 8517, 8521, 8526, 8526, 8544, 8584, 11264, 11310, 11312, 11358, 11360, 11492, 11499, 11507, 11520, 11557, 11559, 11559, 11565, 11565, 11568, 11623, 11631, 11631, 11647, 11670, 11680, 11686, 11688, 11694, 11696, 11702, 11704, 11710, 11712, 11718, 11720, 11726, 11728, 11734, 11736, 11742, 11744, 11775, 11823, 11823, 12293, 12295, 12321, 12335, 12337, 12341, 12344, 12348, 12353, 12438, 12441, 12442, 12445, 12447, 12449, 12538, 12540, 12543, 12549, 12589, 12593, 12686, 12704, 12730, 12784, 12799, 13312, 19893, 19968, 40908, 40960, 42124, 42192, 42237, 42240, 42508, 42512, 42539, 42560, 42607, 42612, 42621, 42623, 42647, 42655, 42737, 42775, 42783, 42786, 42888, 42891, 42894, 42896, 42899, 42912, 42922, 43000, 43047, 43072, 43123, 43136, 43204, 43216, 43225, 43232, 43255, 43259, 43259, 43264, 43309, 43312, 43347, 43360, 43388, 43392, 43456, 43471, 43481, 43520, 43574, 43584, 43597, 43600, 43609, 43616, 43638, 43642, 43643, 43648, 43714, 43739, 43741, 43744, 43759, 43762, 43766, 43777, 43782, 43785, 43790, 43793, 43798, 43808, 43814, 43816, 43822, 43968, 44010, 44012, 44013, 44016, 44025, 44032, 55203, 55216, 55238, 55243, 55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64285, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65024, 65039, 65056, 65062, 65075, 65076, 65101, 65103, 65136, 65140, 65142, 65276, 65296, 65305, 65313, 65338, 65343, 65343, 65345, 65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500)

  def lookupInUnicodeMap(code: Int, map: Array[Int]): Boolean = {
    if ((code < map(0))) {
      return false

    }
    var lo = 0
    var hi: Int = map.length
    var mid: Int = zeroOfMyType
    while (((lo + 1) < hi)) {
      {
        (mid = (lo + (((hi - lo)) / 2)))
        (mid -= (mid % 2))
        if (((map(mid) <= code) && (code <= map((mid + 1))))) {
          return true

        }
        if ((code < map(mid))) {
          (hi = mid)

        }
        else {
          (lo = (mid + 2))

        }

      }
    }
    return false

  }

  def isUnicodeIdentifierStart(code: Int, languageVersion: ScriptTarget) = {
    return (if ((languageVersion >= ScriptTarget.ES5)) lookupInUnicodeMap(code, unicodeES5IdentifierStart) else lookupInUnicodeMap(code, unicodeES3IdentifierStart))

  }

  def isUnicodeIdentifierPart(code: Int, languageVersion: ScriptTarget) = {
    return (if ((languageVersion >= ScriptTarget.ES5)) lookupInUnicodeMap(code, unicodeES5IdentifierPart) else lookupInUnicodeMap(code, unicodeES3IdentifierPart))

  }

  def makeReverseMap(source: Map[Int]): Array[String] = {
    val result: Array[String] = Array()
    (source).keys.foreach { fresh1 =>
      val name = zeroOfMyType
        = fresh1 {
        (result(source(name)) = name)

      }
    }
    return result

  }

  val tokenStrings = makeReverseMap(textToToken)

  def tokenToString(t: SyntaxKind): String = {
    return tokenStrings(t)

  }

  def stringToToken(s: String): SyntaxKind = {
    return textToToken(s)

  }

  def computeLineStarts(text: String): Array[Int] = {
    val result: Array[Int] = new Array()
    var pos = 0
    var lineStart = 0
    while ((pos < text.length)) {
      {
        val ch = text.charCodeAt(pos)
        (pos += 1)
        ch match {
          case CharacterCodes.carriageReturn =>
            if ((text.charCodeAt(pos) === CharacterCodes.lineFeed)) {
              (pos += 1)

            }
          case CharacterCodes.lineFeed =>
            result.push(lineStart)
            (lineStart = pos)
          case _ =>
            if (((ch > CharacterCodes.maxAsciiCharacter) && isLineBreak(ch))) {
              result.push(lineStart)
              (lineStart = pos)

            }
        }

      }
    }
    result.push(lineStart)
    return result

  }

  def getPositionOfLineAndCharacter(sourceFile: SourceFile, line: Int, character: Int): Int = {
    return computePositionOfLineAndCharacter(getLineStarts(sourceFile), line, character)

  }

  def computePositionOfLineAndCharacter(lineStarts: Array[Int], line: Int, character: Int): Int = {
    Debug.assert(((line >= 0) && (line < lineStarts.length)))
    return (lineStarts(line) + character)

  }

  def getLineStarts(sourceFile: SourceFile): Array[Int] = {
    return (sourceFile.lineMap || ((sourceFile.lineMap = computeLineStarts(sourceFile.text))))

  }

  def computeLineAndCharacterOfPosition(lineStarts: Array[Int], position: Int) = {
    var lineNumber = binarySearch(lineStarts, position)
    if ((lineNumber < 0)) {
      (lineNumber = ((~lineNumber) - 1))
      Debug.assert((lineNumber !== (-1)), "position cannot precede the beginning of the file")

    }
    return Map("line" -> lineNumber,
      "character" -> (position - lineStarts(lineNumber)))

  }

  def getLineAndCharacterOfPosition(sourceFile: SourceFile, position: Int): LineAndCharacter = {
    return computeLineAndCharacterOfPosition(getLineStarts(sourceFile), position)

  }

  val `hasOwnProperty` = Object.prototype.`hasOwnProperty`

  def isWhiteSpace(ch: Int): Boolean = {
    return (isWhiteSpaceSingleLine(ch) || isLineBreak(ch))

  }

  def isWhiteSpaceSingleLine(ch: Int): Boolean = {
    return ((((((((((((ch === CharacterCodes.space) || (ch === CharacterCodes.tab)) || (ch === CharacterCodes.verticalTab)) || (ch === CharacterCodes.formFeed)) || (ch === CharacterCodes.nonBreakingSpace)) || (ch === CharacterCodes.nextLine)) || (ch === CharacterCodes.ogham)) || ((ch >= CharacterCodes.enQuad) && (ch <= CharacterCodes.zeroWidthSpace))) || (ch === CharacterCodes.narrowNoBreakSpace)) || (ch === CharacterCodes.mathematicalSpace)) || (ch === CharacterCodes.ideographicSpace)) || (ch === CharacterCodes.byteOrderMark))

  }

  def isLineBreak(ch: Int): Boolean = {
    return ((((ch === CharacterCodes.lineFeed) || (ch === CharacterCodes.carriageReturn)) || (ch === CharacterCodes.lineSeparator)) || (ch === CharacterCodes.paragraphSeparator))

  }

  def isDigit(ch: Int): Boolean = {
    return ((ch >= CharacterCodes._0) && (ch <= CharacterCodes._9))

  }

  def isOctalDigit(ch: Int): Boolean = {
    return ((ch >= CharacterCodes._0) && (ch <= CharacterCodes._7))

  }

  def couldStartTrivia(text: String, pos: Int): Boolean = {
    val ch = text.charCodeAt(pos)
    ch match {
      case CharacterCodes.carriageReturn | CharacterCodes.lineFeed | CharacterCodes.tab | CharacterCodes.verticalTab | CharacterCodes.formFeed | CharacterCodes.space | CharacterCodes.slash | CharacterCodes.lessThan | CharacterCodes.equals | CharacterCodes.greaterThan =>
        return true
      case CharacterCodes.hash =>
        return (pos === 0)
      case _ =>
        return (ch > CharacterCodes.maxAsciiCharacter)
    }

  }

  def skipTrivia(text: String, pos: Int, stopAfterLineBreak: Boolean, stopAtComments: Nothing = false): Int = {
    if (positionIsSynthesized(pos)) {
      return pos

    }
    while (true) {
      {
        val ch = text.charCodeAt(pos)
        ch match {
          case CharacterCodes.carriageReturn =>
            if ((text.charCodeAt((pos + 1)) === CharacterCodes.lineFeed)) {
              (pos += 1)

            }
          case CharacterCodes.lineFeed =>
            (pos += 1)
            if (stopAfterLineBreak) {
              return pos

            }
            continue
          case CharacterCodes.tab | CharacterCodes.verticalTab | CharacterCodes.formFeed | CharacterCodes.space =>
            (pos += 1)
            continue
          case CharacterCodes.slash =>
            if (stopAtComments) {
              break()

            }
            if ((text.charCodeAt((pos + 1)) === CharacterCodes.slash)) {
              (pos += 2)
              while ((pos < text.length)) {
                {
                  if (isLineBreak(text.charCodeAt(pos))) {
                    break()

                  }
                  (pos += 1)

                }
              }
              continue

            }
            if ((text.charCodeAt((pos + 1)) === CharacterCodes.asterisk)) {
              (pos += 2)
              while ((pos < text.length)) {
                {
                  if (((text.charCodeAt(pos) === CharacterCodes.asterisk) && (text.charCodeAt((pos + 1)) === CharacterCodes.slash))) {
                    (pos += 2)
                    break()

                  }
                  (pos += 1)

                }
              }
              continue

            }
          case CharacterCodes.lessThan | CharacterCodes.equals | CharacterCodes.greaterThan =>
            if (isConflictMarkerTrivia(text, pos)) {
              (pos = scanConflictMarkerTrivia(text, pos))
              continue

            }
          case CharacterCodes.hash =>
            if (((pos === 0) && isShebangTrivia(text, pos))) {
              (pos = scanShebangTrivia(text, pos))
              continue

            }
          case _ =>
            if (((ch > CharacterCodes.maxAsciiCharacter) && (isWhiteSpace(ch)))) {
              (pos += 1)
              continue

            }
        }
        return pos

      }
    }

  }

  val mergeConflictMarkerLength = "<<<<<<<".length

  def isConflictMarkerTrivia(text: String, pos: Int) = {
    Debug.assert((pos >= 0))
    if (((pos === 0) || isLineBreak(text.charCodeAt((pos - 1))))) {
      val ch = text.charCodeAt(pos)
      if ((((pos + mergeConflictMarkerLength)) < text.length)) {
        {
          var i = 0
          var n = mergeConflictMarkerLength
          while ((i < n)) {
            {
              if ((text.charCodeAt((pos + i)) !== ch)) {
                return false

              }

            }
            (i += 1)
          }
        }
        return ((ch === CharacterCodes.equals) || (text.charCodeAt((pos + mergeConflictMarkerLength)) === CharacterCodes.space))

      }

    }
    return false

  }

  def scanConflictMarkerTrivia(text: String, pos: Int, error: ErrorCallback) = {
    if (error) {
      error(Diagnostics.Merge_conflict_marker_encountered, mergeConflictMarkerLength)

    }
    val ch = text.charCodeAt(pos)
    val len = text.length
    if (((ch === CharacterCodes.lessThan) || (ch === CharacterCodes.greaterThan))) {
      while (((pos < len) && (!isLineBreak(text.charCodeAt(pos))))) {
        {
          (pos += 1)

        }
      }

    }
    else {
      Debug.assert((ch === CharacterCodes.equals))
      while ((pos < len)) {
        {
          val ch = text.charCodeAt(pos)
          if (((ch === CharacterCodes.greaterThan) && isConflictMarkerTrivia(text, pos))) {
            break()

          }
          (pos += 1)

        }
      }

    }
    return pos

  }

  val shebangTriviaRegex = java.util.regex.Pattern.compile(raw"""^#!.*""")

  def isShebangTrivia(text: String, pos: Int) = {
    Debug.assert((pos === 0))
    return shebangTriviaRegex.test(text)

  }

  def scanShebangTrivia(text: String, pos: Int) = {
    val shebang = shebangTriviaRegex.exec(text)(0)
    (pos = (pos + shebang.length))
    return pos

  }

  def iterateCommentRanges[T, U](reduce: Boolean, text: String, pos: Int, trailing: Boolean, cb: ((Int, Int, SyntaxKind, Boolean, T, U) => U), state: T, initial: U): U = {
    var pendingPos: Int = zeroOfMyType
    var pendingEnd: Int = zeroOfMyType
    var pendingKind: SyntaxKind = zeroOfMyType
    var pendingHasTrailingNewLine: Boolean = zeroOfMyType
    var hasPendingCommentRange = false
    var collecting = (trailing || (pos === 0))
    var accumulator = initial
    val scan = new scala.util.control.Breaks
    scan.breakable {
      while (((pos >= 0) && (pos < text.length))) {
        {
          val ch = text.charCodeAt(pos)
          ch match {
            case CharacterCodes.carriageReturn =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.lineFeed)) {
                (pos += 1)

              }
            case CharacterCodes.lineFeed =>
              (pos += 1)
              if (trailing) {
                scan.break()

              }
              (collecting = true)
              if (hasPendingCommentRange) {
                (pendingHasTrailingNewLine = true)

              }
              continue
            case CharacterCodes.tab | CharacterCodes.verticalTab | CharacterCodes.formFeed | CharacterCodes.space =>
              (pos += 1)
              continue
            case CharacterCodes.slash =>
              var nextChar = text.charCodeAt((pos + 1))
              var hasTrailingNewLine = false
              if (((nextChar === CharacterCodes.slash) || (nextChar === CharacterCodes.asterisk))) {
                val kind = (if ((nextChar === CharacterCodes.slash)) SyntaxKind.SingleLineCommentTrivia else SyntaxKind.MultiLineCommentTrivia)
                val startPos = pos
                (pos += 2)
                if ((nextChar === CharacterCodes.slash)) {
                  while ((pos < text.length)) {
                    {
                      if (isLineBreak(text.charCodeAt(pos))) {
                        (hasTrailingNewLine = true)
                        break()

                      }
                      (pos += 1)

                    }
                  }

                }
                else {
                  while ((pos < text.length)) {
                    {
                      if (((text.charCodeAt(pos) === CharacterCodes.asterisk) && (text.charCodeAt((pos + 1)) === CharacterCodes.slash))) {
                        (pos += 2)
                        break()

                      }
                      (pos += 1)

                    }
                  }

                }
                if (collecting) {
                  if (hasPendingCommentRange) {
                    (accumulator = cb(pendingPos, pendingEnd, pendingKind, pendingHasTrailingNewLine, state, accumulator))
                    if (((!reduce) && accumulator)) {
                      return accumulator

                    }
                    (hasPendingCommentRange = false)

                  }
                  (pendingPos = startPos)
                  (pendingEnd = pos)
                  (pendingKind = kind)
                  (pendingHasTrailingNewLine = hasTrailingNewLine)
                  (hasPendingCommentRange = true)

                }
                continue

              }
            case _ =>
              if (((ch > CharacterCodes.maxAsciiCharacter) && (isWhiteSpace(ch)))) {
                if ((hasPendingCommentRange && isLineBreak(ch))) {
                  (pendingHasTrailingNewLine = true)

                }
                (pos += 1)
                continue

              }
          }

        }
      }
    }
    if (hasPendingCommentRange) {
      (accumulator = cb(pendingPos, pendingEnd, pendingKind, pendingHasTrailingNewLine, state, accumulator))

    }
    return accumulator

  }

  def forEachLeadingCommentRange[T, U](text: String, pos: Int, cb: ((Int, Int, SyntaxKind, Boolean, T) => U), state: T) = {
    return iterateCommentRanges(false, text, pos, false, cb, state)

  }

  def forEachTrailingCommentRange[T, U](text: String, pos: Int, cb: ((Int, Int, SyntaxKind, Boolean, T) => U), state: T) = {
    return iterateCommentRanges(false, text, pos, true, cb, state)

  }

  def reduceEachLeadingCommentRange[T, U](text: String, pos: Int, cb: ((Int, Int, SyntaxKind, Boolean, T, U) => U), state: T, initial: U) = {
    return iterateCommentRanges(true, text, pos, false, cb, state, initial)

  }

  def reduceEachTrailingCommentRange[T, U](text: String, pos: Int, cb: ((Int, Int, SyntaxKind, Boolean, T, U) => U), state: T, initial: U) = {
    return iterateCommentRanges(true, text, pos, true, cb, state, initial)

  }

  def appendCommentRange(pos: Int, end: Int, kind: SyntaxKind, hasTrailingNewLine: Boolean, _state: Any, comments: Array[CommentRange]) = {
    if ((!comments)) {
      (comments = Array())

    }
    comments.push(Map("pos" -> pos,
      "end" -> end,
      "hasTrailingNewLine" -> hasTrailingNewLine,
      "kind" -> kind))
    return comments

  }

  def getLeadingCommentRanges(text: String, pos: Int): Array[CommentRange] = {
    return reduceEachLeadingCommentRange(text, pos, appendCommentRange, undefined, undefined)

  }

  def getTrailingCommentRanges(text: String, pos: Int): Array[CommentRange] = {
    return reduceEachTrailingCommentRange(text, pos, appendCommentRange, undefined, undefined)

  }

  def getShebang(text: String): String = {
    return (if (shebangTriviaRegex.test(text)) shebangTriviaRegex.exec(text)(0) else undefined)

  }

  def isIdentifierStart(ch: Int, languageVersion: ScriptTarget): Boolean = {
    return ((((((ch >= CharacterCodes.A) && (ch <= CharacterCodes.Z)) || ((ch >= CharacterCodes.a) && (ch <= CharacterCodes.z))) || (ch === CharacterCodes.$)) || (ch === CharacterCodes._underscore_)) || ((ch > CharacterCodes.maxAsciiCharacter) && isUnicodeIdentifierStart(ch, languageVersion)))

  }

  def isIdentifierPart(ch: Int, languageVersion: ScriptTarget): Boolean = {
    return (((((((ch >= CharacterCodes.A) && (ch <= CharacterCodes.Z)) || ((ch >= CharacterCodes.a) && (ch <= CharacterCodes.z))) || ((ch >= CharacterCodes._0) && (ch <= CharacterCodes._9))) || (ch === CharacterCodes.$)) || (ch === CharacterCodes._underscore_)) || ((ch > CharacterCodes.maxAsciiCharacter) && isUnicodeIdentifierPart(ch, languageVersion)))

  }

  def isIdentifierText(name: String, languageVersion: ScriptTarget): Boolean = {
    if ((!isIdentifierStart(name.charCodeAt(0), languageVersion))) {
      return false

    }
    {
      var i = 1
      var n = name.length
      while ((i < n)) {
        {
          if ((!isIdentifierPart(name.charCodeAt(i), languageVersion))) {
            return false

          }

        }
        (i += 1)
      }
    }
    return true

  }

  def createScanner(languageVersion: ScriptTarget, skipTrivia: Boolean, languageVariant: Nothing = LanguageVariant.Standard, text: String, onError: ErrorCallback, start: Int, length: Int): Scanner = {
    var pos: Int = zeroOfMyType
    var end: Int = zeroOfMyType
    var startPos: Int = zeroOfMyType
    var tokenPos: Int = zeroOfMyType
    var token: SyntaxKind = zeroOfMyType
    var tokenValue: String = zeroOfMyType
    var precedingLineBreak: Boolean = zeroOfMyType
    var hasExtendedUnicodeEscape: Boolean = zeroOfMyType
    var tokenIsUnterminated: Boolean = zeroOfMyType
    setText(text, start, length)
    return Map("getStartPos" -> (() => startPos),
      "getTextPos" -> (() => pos),
      "getToken" -> (() => token),
      "getTokenPos" -> (() => tokenPos),
      "getTokenText" -> (() => text.substring(tokenPos, pos)),
      "getTokenValue" -> (() => tokenValue),
      "hasExtendedUnicodeEscape" -> (() => hasExtendedUnicodeEscape),
      "hasPrecedingLineBreak" -> (() => precedingLineBreak),
      "isIdentifier" -> (() => ((token === SyntaxKind.Identifier) || (token > SyntaxKind.LastReservedWord))),
      "isReservedWord" -> (() => ((token >= SyntaxKind.FirstReservedWord) && (token <= SyntaxKind.LastReservedWord))),
      "isUnterminated" -> (() => tokenIsUnterminated),
      "reScanGreaterToken" -> reScanGreaterToken,
      "reScanSlashToken" -> reScanSlashToken,
      "reScanTemplateToken" -> reScanTemplateToken,
      "scanJsxIdentifier" -> scanJsxIdentifier,
      "scanJsxAttributeValue" -> scanJsxAttributeValue,
      "reScanJsxToken" -> reScanJsxToken,
      "scanJsxToken" -> scanJsxToken,
      "scanJSDocToken" -> scanJSDocToken,
      "scan" -> scan,
      "getText" -> getText,
      "setText" -> setText,
      "setScriptTarget" -> setScriptTarget,
      "setLanguageVariant" -> setLanguageVariant,
      "setOnError" -> setOnError,
      "setTextPos" -> setTextPos,
      "tryScan" -> tryScan,
      "lookAhead" -> lookAhead,
      "scanRange" -> scanRange)
    def error(message: DiagnosticMessage, length: Int): Unit = {
      if (onError) {
        onError(message, (length || 0))

      }

    }
    def scanNumber(): String = {
      val start = pos
      while (isDigit(text.charCodeAt(pos))) {
        (pos += 1)
      }
      if ((text.charCodeAt(pos) === CharacterCodes.dot)) {
        (pos += 1)
        while (isDigit(text.charCodeAt(pos))) {
          (pos += 1)
        }

      }
      var end = pos
      if (((text.charCodeAt(pos) === CharacterCodes.E) || (text.charCodeAt(pos) === CharacterCodes.e))) {
        (pos += 1)
        if (((text.charCodeAt(pos) === CharacterCodes.plus) || (text.charCodeAt(pos) === CharacterCodes.minus)))
          (pos += 1)
        if (isDigit(text.charCodeAt(pos))) {
          (pos += 1)
          while (isDigit(text.charCodeAt(pos))) {
            (pos += 1)
          }
          (end = pos)

        }
        else {
          error(Diagnostics.Digit_expected)

        }

      }
      return ("" + (+(text.substring(start, end))))

    }
    def scanOctalDigits(): Int = {
      val start = pos
      while (isOctalDigit(text.charCodeAt(pos))) {
        {
          (pos += 1)

        }
      }
      return (+(text.substring(start, pos)))

    }
    def scanExactNumberOfHexDigits(count: Int): Int = {
      return scanHexDigits(count, false)

    }
    def scanMinimumNumberOfHexDigits(count: Int): Int = {
      return scanHexDigits(count, true)

    }
    def scanHexDigits(minCount: Int, scanAsManyAsPossible: Boolean): Int = {
      var digits = 0
      var value = 0
      while (((digits < minCount) || scanAsManyAsPossible)) {
        {
          val ch = text.charCodeAt(pos)
          if (((ch >= CharacterCodes._0) && (ch <= CharacterCodes._9))) {
            (value = (((value * 16) + ch) - CharacterCodes._0))

          }
          else if (((ch >= CharacterCodes.A) && (ch <= CharacterCodes.F))) {
            (value = ((((value * 16) + ch) - CharacterCodes.A) + 10))

          }
          else if (((ch >= CharacterCodes.a) && (ch <= CharacterCodes.f))) {
            (value = ((((value * 16) + ch) - CharacterCodes.a) + 10))

          }
          else {
            break()

          }
          (pos += 1)
          (digits += 1)

        }
      }
      if ((digits < minCount)) {
        (value = (-1))

      }
      return value

    }
    def scanString(allowEscapes: Nothing = true): String = {
      val quote = text.charCodeAt(pos)
      (pos += 1)
      var result = ""
      var start = pos
      while (true) {
        {
          if ((pos >= end)) {
            (result += text.substring(start, pos))
            (tokenIsUnterminated = true)
            error(Diagnostics.Unterminated_string_literal)
            break()

          }
          val ch = text.charCodeAt(pos)
          if ((ch === quote)) {
            (result += text.substring(start, pos))
            (pos += 1)
            break()

          }
          if (((ch === CharacterCodes.backslash) && allowEscapes)) {
            (result += text.substring(start, pos))
            (result += scanEscapeSequence())
            (start = pos)
            continue

          }
          if (isLineBreak(ch)) {
            (result += text.substring(start, pos))
            (tokenIsUnterminated = true)
            error(Diagnostics.Unterminated_string_literal)
            break()

          }
          (pos += 1)

        }
      }
      return result

    }
    def scanTemplateAndSetTokenValue(): SyntaxKind = {
      val startedWithBacktick = (text.charCodeAt(pos) === CharacterCodes.backtick)
      (pos += 1)
      var start = pos
      var contents = ""
      var resultingToken: SyntaxKind = zeroOfMyType
      while (true) {
        {
          if ((pos >= end)) {
            (contents += text.substring(start, pos))
            (tokenIsUnterminated = true)
            error(Diagnostics.Unterminated_template_literal)
            (resultingToken = (if (startedWithBacktick) SyntaxKind.NoSubstitutionTemplateLiteral else SyntaxKind.TemplateTail))
            break()

          }
          val currChar = text.charCodeAt(pos)
          if ((currChar === CharacterCodes.backtick)) {
            (contents += text.substring(start, pos))
            (pos += 1)
            (resultingToken = (if (startedWithBacktick) SyntaxKind.NoSubstitutionTemplateLiteral else SyntaxKind.TemplateTail))
            break()

          }
          if ((((currChar === CharacterCodes.$) && ((pos + 1) < end)) && (text.charCodeAt((pos + 1)) === CharacterCodes.openBrace))) {
            (contents += text.substring(start, pos))
            (pos += 2)
            (resultingToken = (if (startedWithBacktick) SyntaxKind.TemplateHead else SyntaxKind.TemplateMiddle))
            break()

          }
          if ((currChar === CharacterCodes.backslash)) {
            (contents += text.substring(start, pos))
            (contents += scanEscapeSequence())
            (start = pos)
            continue

          }
          if ((currChar === CharacterCodes.carriageReturn)) {
            (contents += text.substring(start, pos))
            (pos += 1)
            if (((pos < end) && (text.charCodeAt(pos) === CharacterCodes.lineFeed))) {
              (pos += 1)

            }
            (contents += "\n")
            (start = pos)
            continue

          }
          (pos += 1)

        }
      }
      Debug.assert((resultingToken !== undefined))
      (tokenValue = contents)
      return resultingToken

    }
    def scanEscapeSequence(): String = {
      (pos += 1)
      if ((pos >= end)) {
        error(Diagnostics.Unexpected_end_of_text)
        return ""

      }
      val ch = text.charCodeAt(pos)
      (pos += 1)
      ch match {
        case CharacterCodes._0 =>
          return "\0"
        case CharacterCodes.b =>
          return "\b"
        case CharacterCodes.t =>
          return "\t"
        case CharacterCodes.n =>
          return "\n"
        case CharacterCodes.v =>
          return "\u000b"
        case CharacterCodes.f =>
          return "\f"
        case CharacterCodes.r =>
          return "\r"
        case CharacterCodes.singleQuote =>
          return "'"
        case CharacterCodes.doubleQuote =>
          return "\""
        case CharacterCodes.u =>
          if (((pos < end) && (text.charCodeAt(pos) === CharacterCodes.openBrace))) {
            (hasExtendedUnicodeEscape = true)
            (pos += 1)
            return scanExtendedUnicodeEscape()

          }
          return scanHexadecimalEscape(4)
        case CharacterCodes.x =>
          return scanHexadecimalEscape(2)
        case CharacterCodes.carriageReturn =>
          if (((pos < end) && (text.charCodeAt(pos) === CharacterCodes.lineFeed))) {
            (pos += 1)

          }
        case CharacterCodes.lineFeed | CharacterCodes.lineSeparator | CharacterCodes.paragraphSeparator =>
          return ""
        case _ =>
          return String.fromCharCode(ch)
      }

    }
    def scanHexadecimalEscape(numDigits: Int): String = {
      val escapedValue = scanExactNumberOfHexDigits(numDigits)
      if ((escapedValue >= 0)) {
        return String.fromCharCode(escapedValue)

      }
      else {
        error(Diagnostics.Hexadecimal_digit_expected)
        return ""

      }

    }
    def scanExtendedUnicodeEscape(): String = {
      val escapedValue = scanMinimumNumberOfHexDigits(1)
      var isInvalidExtendedEscape = false
      if ((escapedValue < 0)) {
        error(Diagnostics.Hexadecimal_digit_expected)
        (isInvalidExtendedEscape = true)

      }
      else if ((escapedValue > 1114111)) {
        error(Diagnostics.An_extended_Unicode_escape_value_must_be_between_0x0_and_0x10FFFF_inclusive)
        (isInvalidExtendedEscape = true)

      }
      if ((pos >= end)) {
        error(Diagnostics.Unexpected_end_of_text)
        (isInvalidExtendedEscape = true)

      }
      else if ((text.charCodeAt(pos) === CharacterCodes.closeBrace)) {
        (pos += 1)

      }
      else {
        error(Diagnostics.Unterminated_Unicode_escape_sequence)
        (isInvalidExtendedEscape = true)

      }
      if (isInvalidExtendedEscape) {
        return ""

      }
      return utf16EncodeAsString(escapedValue)

    }
    def utf16EncodeAsString(codePoint: Int): String = {
      Debug.assert(((0 <= codePoint) && (codePoint <= 1114111)))
      if ((codePoint <= 65535)) {
        return String.fromCharCode(codePoint)

      }
      val codeUnit1 = (Math.floor((((codePoint - 65536)) / 1024)) + 55296)
      val codeUnit2 = (((((codePoint - 65536)) % 1024)) + 56320)
      return String.fromCharCode(codeUnit1, codeUnit2)

    }
    def peekUnicodeEscape(): Int = {
      if ((((pos + 5) < end) && (text.charCodeAt((pos + 1)) === CharacterCodes.u))) {
        val start = pos
        (pos += 2)
        val value = scanExactNumberOfHexDigits(4)
        (pos = start)
        return value

      }
      return (-1)

    }
    def scanIdentifierParts(): String = {
      var result = ""
      var start = pos
      while ((pos < end)) {
        {
          var ch = text.charCodeAt(pos)
          if (isIdentifierPart(ch, languageVersion)) {
            (pos += 1)

          }
          else if ((ch === CharacterCodes.backslash)) {
            (ch = peekUnicodeEscape())
            if ((!(((ch >= 0) && isIdentifierPart(ch, languageVersion))))) {
              break()

            }
            (result += text.substring(start, pos))
            (result += String.fromCharCode(ch))
            (pos += 6)
            (start = pos)

          }
          else {
            break()

          }

        }
      }
      (result += text.substring(start, pos))
      return result

    }
    def getIdentifierToken(): SyntaxKind = {
      val len = tokenValue.length
      if (((len >= 2) && (len <= 11))) {
        val ch = tokenValue.charCodeAt(0)
        if ((((ch >= CharacterCodes.a) && (ch <= CharacterCodes.z)) && `hasOwnProperty`.call(textToToken, tokenValue))) {
          return (token = textToToken(tokenValue))

        }

      }
      return (token = SyntaxKind.Identifier)

    }
    def scanBinaryOrOctalDigits(base: Int): Int = {
      Debug.assert(((base === 2) || (base === 8)), "Expected either base 2 or base 8")
      var value = 0
      var numberOfDigits = 0
      while (true) {
        {
          val ch = text.charCodeAt(pos)
          val valueOfCh = (ch - CharacterCodes._0)
          if (((!isDigit(ch)) || (valueOfCh >= base))) {
            break()

          }
          (value = ((value * base) + valueOfCh))
          (pos += 1)
          (numberOfDigits += 1)

        }
      }
      if ((numberOfDigits === 0)) {
        return (-1)

      }
      return value

    }
    def scan(): SyntaxKind = {
      (startPos = pos)
      (hasExtendedUnicodeEscape = false)
      (precedingLineBreak = false)
      (tokenIsUnterminated = false)
      while (true) {
        {
          (tokenPos = pos)
          if ((pos >= end)) {
            return (token = SyntaxKind.EndOfFileToken)

          }
          var ch = text.charCodeAt(pos)
          if ((((ch === CharacterCodes.hash) && (pos === 0)) && isShebangTrivia(text, pos))) {
            (pos = scanShebangTrivia(text, pos))
            if (skipTrivia) {
              continue

            }
            else {
              return (token = SyntaxKind.ShebangTrivia)

            }

          }
          ch match {
            case CharacterCodes.lineFeed | CharacterCodes.carriageReturn =>
              (precedingLineBreak = true)
              if (skipTrivia) {
                (pos += 1)
                continue

              }
              else {
                if ((((ch === CharacterCodes.carriageReturn) && ((pos + 1) < end)) && (text.charCodeAt((pos + 1)) === CharacterCodes.lineFeed))) {
                  (pos += 2)

                }
                else {
                  (pos += 1)

                }
                return (token = SyntaxKind.NewLineTrivia)

              }
            case CharacterCodes.tab | CharacterCodes.verticalTab | CharacterCodes.formFeed | CharacterCodes.space =>
              if (skipTrivia) {
                (pos += 1)
                continue

              }
              else {
                while (((pos < end) && isWhiteSpaceSingleLine(text.charCodeAt(pos)))) {
                  {
                    (pos += 1)

                  }
                }
                return (token = SyntaxKind.WhitespaceTrivia)

              }
            case CharacterCodes.exclamation =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                if ((text.charCodeAt((pos + 2)) === CharacterCodes.equals)) {
                  return ((pos += 3), (token = SyntaxKind.ExclamationEqualsEqualsToken))

                }
                return ((pos += 2), (token = SyntaxKind.ExclamationEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.ExclamationToken)
            case CharacterCodes.doubleQuote | CharacterCodes.singleQuote =>
              (tokenValue = scanString())
              return (token = SyntaxKind.StringLiteral)
            case CharacterCodes.backtick =>
              return (token = scanTemplateAndSetTokenValue())
            case CharacterCodes.percent =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.PercentEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.PercentToken)
            case CharacterCodes.ampersand =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.ampersand)) {
                return ((pos += 2), (token = SyntaxKind.AmpersandAmpersandToken))

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.AmpersandEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.AmpersandToken)
            case CharacterCodes.openParen =>
              (pos += 1)
              return (token = SyntaxKind.OpenParenToken)
            case CharacterCodes.closeParen =>
              (pos += 1)
              return (token = SyntaxKind.CloseParenToken)
            case CharacterCodes.asterisk =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.AsteriskEqualsToken))

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.asterisk)) {
                if ((text.charCodeAt((pos + 2)) === CharacterCodes.equals)) {
                  return ((pos += 3), (token = SyntaxKind.AsteriskAsteriskEqualsToken))

                }
                return ((pos += 2), (token = SyntaxKind.AsteriskAsteriskToken))

              }
              (pos += 1)
              return (token = SyntaxKind.AsteriskToken)
            case CharacterCodes.plus =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.plus)) {
                return ((pos += 2), (token = SyntaxKind.PlusPlusToken))

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.PlusEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.PlusToken)
            case CharacterCodes.comma =>
              (pos += 1)
              return (token = SyntaxKind.CommaToken)
            case CharacterCodes.minus =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.minus)) {
                return ((pos += 2), (token = SyntaxKind.MinusMinusToken))

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.MinusEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.MinusToken)
            case CharacterCodes.dot =>
              if (isDigit(text.charCodeAt((pos + 1)))) {
                (tokenValue = scanNumber())
                return (token = SyntaxKind.NumericLiteral)

              }
              if (((text.charCodeAt((pos + 1)) === CharacterCodes.dot) && (text.charCodeAt((pos + 2)) === CharacterCodes.dot))) {
                return ((pos += 3), (token = SyntaxKind.DotDotDotToken))

              }
              (pos += 1)
              return (token = SyntaxKind.DotToken)
            case CharacterCodes.slash =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.slash)) {
                (pos += 2)
                while ((pos < end)) {
                  {
                    if (isLineBreak(text.charCodeAt(pos))) {
                      break()

                    }
                    (pos += 1)

                  }
                }
                if (skipTrivia) {
                  continue

                }
                else {
                  return (token = SyntaxKind.SingleLineCommentTrivia)

                }

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.asterisk)) {
                (pos += 2)
                var commentClosed = false
                while ((pos < end)) {
                  {
                    val ch = text.charCodeAt(pos)
                    if (((ch === CharacterCodes.asterisk) && (text.charCodeAt((pos + 1)) === CharacterCodes.slash))) {
                      (pos += 2)
                      (commentClosed = true)
                      break()

                    }
                    if (isLineBreak(ch)) {
                      (precedingLineBreak = true)

                    }
                    (pos += 1)

                  }
                }
                if ((!commentClosed)) {
                  error(Diagnostics.Asterisk_Slash_expected)

                }
                if (skipTrivia) {
                  continue

                }
                else {
                  (tokenIsUnterminated = (!commentClosed))
                  return (token = SyntaxKind.MultiLineCommentTrivia)

                }

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.SlashEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.SlashToken)
            case CharacterCodes._0 =>
              if ((((pos + 2) < end) && (((text.charCodeAt((pos + 1)) === CharacterCodes.X) || (text.charCodeAt((pos + 1)) === CharacterCodes.x))))) {
                (pos += 2)
                var value = scanMinimumNumberOfHexDigits(1)
                if ((value < 0)) {
                  error(Diagnostics.Hexadecimal_digit_expected)
                  (value = 0)

                }
                (tokenValue = ("" + value))
                return (token = SyntaxKind.NumericLiteral)

              }
              else if ((((pos + 2) < end) && (((text.charCodeAt((pos + 1)) === CharacterCodes.B) || (text.charCodeAt((pos + 1)) === CharacterCodes.b))))) {
                (pos += 2)
                var value = scanBinaryOrOctalDigits(2)
                if ((value < 0)) {
                  error(Diagnostics.Binary_digit_expected)
                  (value = 0)

                }
                (tokenValue = ("" + value))
                return (token = SyntaxKind.NumericLiteral)

              }
              else if ((((pos + 2) < end) && (((text.charCodeAt((pos + 1)) === CharacterCodes.O) || (text.charCodeAt((pos + 1)) === CharacterCodes.o))))) {
                (pos += 2)
                var value = scanBinaryOrOctalDigits(8)
                if ((value < 0)) {
                  error(Diagnostics.Octal_digit_expected)
                  (value = 0)

                }
                (tokenValue = ("" + value))
                return (token = SyntaxKind.NumericLiteral)

              }
              if ((((pos + 1) < end) && isOctalDigit(text.charCodeAt((pos + 1))))) {
                (tokenValue = ("" + scanOctalDigits()))
                return (token = SyntaxKind.NumericLiteral)

              }
            case CharacterCodes._1 | CharacterCodes._2 | CharacterCodes._3 | CharacterCodes._4 | CharacterCodes._5 | CharacterCodes._6 | CharacterCodes._7 | CharacterCodes._8 | CharacterCodes._9 =>
              (tokenValue = scanNumber())
              return (token = SyntaxKind.NumericLiteral)
            case CharacterCodes.colon =>
              (pos += 1)
              return (token = SyntaxKind.ColonToken)
            case CharacterCodes.semicolon =>
              (pos += 1)
              return (token = SyntaxKind.SemicolonToken)
            case CharacterCodes.lessThan =>
              if (isConflictMarkerTrivia(text, pos)) {
                (pos = scanConflictMarkerTrivia(text, pos, error))
                if (skipTrivia) {
                  continue

                }
                else {
                  return (token = SyntaxKind.ConflictMarkerTrivia)

                }

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.lessThan)) {
                if ((text.charCodeAt((pos + 2)) === CharacterCodes.equals)) {
                  return ((pos += 3), (token = SyntaxKind.LessThanLessThanEqualsToken))

                }
                return ((pos += 2), (token = SyntaxKind.LessThanLessThanToken))

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.LessThanEqualsToken))

              }
              if ((((languageVariant === LanguageVariant.JSX) && (text.charCodeAt((pos + 1)) === CharacterCodes.slash)) && (text.charCodeAt((pos + 2)) !== CharacterCodes.asterisk))) {
                return ((pos += 2), (token = SyntaxKind.LessThanSlashToken))

              }
              (pos += 1)
              return (token = SyntaxKind.LessThanToken)
            case CharacterCodes.equals =>
              if (isConflictMarkerTrivia(text, pos)) {
                (pos = scanConflictMarkerTrivia(text, pos, error))
                if (skipTrivia) {
                  continue

                }
                else {
                  return (token = SyntaxKind.ConflictMarkerTrivia)

                }

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                if ((text.charCodeAt((pos + 2)) === CharacterCodes.equals)) {
                  return ((pos += 3), (token = SyntaxKind.EqualsEqualsEqualsToken))

                }
                return ((pos += 2), (token = SyntaxKind.EqualsEqualsToken))

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.greaterThan)) {
                return ((pos += 2), (token = SyntaxKind.EqualsGreaterThanToken))

              }
              (pos += 1)
              return (token = SyntaxKind.EqualsToken)
            case CharacterCodes.greaterThan =>
              if (isConflictMarkerTrivia(text, pos)) {
                (pos = scanConflictMarkerTrivia(text, pos, error))
                if (skipTrivia) {
                  continue

                }
                else {
                  return (token = SyntaxKind.ConflictMarkerTrivia)

                }

              }
              (pos += 1)
              return (token = SyntaxKind.GreaterThanToken)
            case CharacterCodes.question =>
              (pos += 1)
              return (token = SyntaxKind.QuestionToken)
            case CharacterCodes.openBracket =>
              (pos += 1)
              return (token = SyntaxKind.OpenBracketToken)
            case CharacterCodes.closeBracket =>
              (pos += 1)
              return (token = SyntaxKind.CloseBracketToken)
            case CharacterCodes.caret =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.CaretEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.CaretToken)
            case CharacterCodes.openBrace =>
              (pos += 1)
              return (token = SyntaxKind.OpenBraceToken)
            case CharacterCodes.bar =>
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.bar)) {
                return ((pos += 2), (token = SyntaxKind.BarBarToken))

              }
              if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
                return ((pos += 2), (token = SyntaxKind.BarEqualsToken))

              }
              (pos += 1)
              return (token = SyntaxKind.BarToken)
            case CharacterCodes.closeBrace =>
              (pos += 1)
              return (token = SyntaxKind.CloseBraceToken)
            case CharacterCodes.tilde =>
              (pos += 1)
              return (token = SyntaxKind.TildeToken)
            case CharacterCodes.at =>
              (pos += 1)
              return (token = SyntaxKind.AtToken)
            case CharacterCodes.backslash =>
              var cookedChar = peekUnicodeEscape()
              if (((cookedChar >= 0) && isIdentifierStart(cookedChar, languageVersion))) {
                (pos += 6)
                (tokenValue = (String.fromCharCode(cookedChar) + scanIdentifierParts()))
                return (token = getIdentifierToken())

              }
              error(Diagnostics.Invalid_character)
              (pos += 1)
              return (token = SyntaxKind.Unknown)
            case _ =>
              if (isIdentifierStart(ch, languageVersion)) {
                (pos += 1)
                while (((pos < end) && isIdentifierPart((ch = text.charCodeAt(pos)), languageVersion))) {
                  (pos += 1)
                }
                (tokenValue = text.substring(tokenPos, pos))
                if ((ch === CharacterCodes.backslash)) {
                  (tokenValue += scanIdentifierParts())

                }
                return (token = getIdentifierToken())

              }
              else if (isWhiteSpaceSingleLine(ch)) {
                (pos += 1)
                continue

              }
              else if (isLineBreak(ch)) {
                (precedingLineBreak = true)
                (pos += 1)
                continue

              }
              error(Diagnostics.Invalid_character)
              (pos += 1)
              return (token = SyntaxKind.Unknown)
          }

        }
      }

    }
    def reScanGreaterToken(): SyntaxKind = {
      if ((token === SyntaxKind.GreaterThanToken)) {
        if ((text.charCodeAt(pos) === CharacterCodes.greaterThan)) {
          if ((text.charCodeAt((pos + 1)) === CharacterCodes.greaterThan)) {
            if ((text.charCodeAt((pos + 2)) === CharacterCodes.equals)) {
              return ((pos += 3), (token = SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken))

            }
            return ((pos += 2), (token = SyntaxKind.GreaterThanGreaterThanGreaterThanToken))

          }
          if ((text.charCodeAt((pos + 1)) === CharacterCodes.equals)) {
            return ((pos += 2), (token = SyntaxKind.GreaterThanGreaterThanEqualsToken))

          }
          (pos += 1)
          return (token = SyntaxKind.GreaterThanGreaterThanToken)

        }
        if ((text.charCodeAt(pos) === CharacterCodes.equals)) {
          (pos += 1)
          return (token = SyntaxKind.GreaterThanEqualsToken)

        }

      }
      return token

    }
    def reScanSlashToken(): SyntaxKind = {
      if (((token === SyntaxKind.SlashToken) || (token === SyntaxKind.SlashEqualsToken))) {
        var p = (tokenPos + 1)
        var inEscape = false
        var inCharacterClass = false
        while (true) {
          {
            if ((p >= end)) {
              (tokenIsUnterminated = true)
              error(Diagnostics.Unterminated_regular_expression_literal)
              break()

            }
            val ch = text.charCodeAt(p)
            if (isLineBreak(ch)) {
              (tokenIsUnterminated = true)
              error(Diagnostics.Unterminated_regular_expression_literal)
              break()

            }
            if (inEscape) {
              (inEscape = false)

            }
            else if (((ch === CharacterCodes.slash) && (!inCharacterClass))) {
              (p += 1)
              break()

            }
            else if ((ch === CharacterCodes.openBracket)) {
              (inCharacterClass = true)

            }
            else if ((ch === CharacterCodes.backslash)) {
              (inEscape = true)

            }
            else if ((ch === CharacterCodes.closeBracket)) {
              (inCharacterClass = false)

            }
            (p += 1)

          }
        }
        while (((p < end) && isIdentifierPart(text.charCodeAt(p), languageVersion))) {
          {
            (p += 1)

          }
        }
        (pos = p)
        (tokenValue = text.substring(tokenPos, pos))
        (token = SyntaxKind.RegularExpressionLiteral)

      }
      return token

    }
    def reScanTemplateToken(): SyntaxKind = {
      Debug.assert((token === SyntaxKind.CloseBraceToken), "'reScanTemplateToken' should only be called on a '}'")
      (pos = tokenPos)
      return (token = scanTemplateAndSetTokenValue())

    }
    def reScanJsxToken(): SyntaxKind = {
      (pos = (tokenPos = startPos))
      return (token = scanJsxToken())

    }
    def scanJsxToken(): SyntaxKind = {
      (startPos = (tokenPos = pos))
      if ((pos >= end)) {
        return (token = SyntaxKind.EndOfFileToken)

      }
      var char = text.charCodeAt(pos)
      if ((char === CharacterCodes.lessThan)) {
        if ((text.charCodeAt((pos + 1)) === CharacterCodes.slash)) {
          (pos += 2)
          return (token = SyntaxKind.LessThanSlashToken)

        }
        (pos += 1)
        return (token = SyntaxKind.LessThanToken)

      }
      if ((char === CharacterCodes.openBrace)) {
        (pos += 1)
        return (token = SyntaxKind.OpenBraceToken)

      }
      while ((pos < end)) {
        {
          (pos += 1)
          (char = text.charCodeAt(pos))
          if ((((char === CharacterCodes.openBrace)) || ((char === CharacterCodes.lessThan)))) {
            break()

          }

        }
      }
      return (token = SyntaxKind.JsxText)

    }
    def scanJsxIdentifier(): SyntaxKind = {
      if (tokenIsIdentifierOrKeyword(token)) {
        val firstCharPosition = pos
        while ((pos < end)) {
          {
            val ch = text.charCodeAt(pos)
            if (((ch === CharacterCodes.minus) || ((if (((firstCharPosition === pos))) isIdentifierStart(ch, languageVersion) else isIdentifierPart(ch, languageVersion))))) {
              (pos += 1)

            }
            else {
              break()

            }

          }
        }
        (tokenValue += text.substr(firstCharPosition, (pos - firstCharPosition)))

      }
      return token

    }
    def scanJsxAttributeValue(): SyntaxKind = {
      (startPos = pos)
      text.charCodeAt(pos) match {
        case CharacterCodes.doubleQuote | CharacterCodes.singleQuote =>
          (tokenValue = scanString(false))
          return (token = SyntaxKind.StringLiteral)
        case _ =>
          return scan()
      }

    }
    def scanJSDocToken(): SyntaxKind = {
      if ((pos >= end)) {
        return (token = SyntaxKind.EndOfFileToken)

      }
      (startPos = pos)
      (tokenPos = pos)
      val ch = text.charCodeAt(pos)
      ch match {
        case CharacterCodes.tab | CharacterCodes.verticalTab | CharacterCodes.formFeed | CharacterCodes.space =>
          while (((pos < end) && isWhiteSpaceSingleLine(text.charCodeAt(pos)))) {
            {
              (pos += 1)

            }
          }
          return (token = SyntaxKind.WhitespaceTrivia)
        case CharacterCodes.at =>
          (pos += 1)
          return (token = SyntaxKind.AtToken)
        case CharacterCodes.lineFeed | CharacterCodes.carriageReturn =>
          (pos += 1)
          return (token = SyntaxKind.NewLineTrivia)
        case CharacterCodes.asterisk =>
          (pos += 1)
          return (token = SyntaxKind.AsteriskToken)
        case CharacterCodes.openBrace =>
          (pos += 1)
          return (token = SyntaxKind.OpenBraceToken)
        case CharacterCodes.closeBrace =>
          (pos += 1)
          return (token = SyntaxKind.CloseBraceToken)
        case CharacterCodes.openBracket =>
          (pos += 1)
          return (token = SyntaxKind.OpenBracketToken)
        case CharacterCodes.closeBracket =>
          (pos += 1)
          return (token = SyntaxKind.CloseBracketToken)
        case CharacterCodes.equals =>
          (pos += 1)
          return (token = SyntaxKind.EqualsToken)
        case CharacterCodes.comma =>
          (pos += 1)
          return (token = SyntaxKind.CommaToken)
        case CharacterCodes.dot =>
          (pos += 1)
          return (token = SyntaxKind.DotToken)
        case _ =>
      }
      if (isIdentifierStart(ch, ScriptTarget.Latest)) {
        (pos += 1)
        while ((isIdentifierPart(text.charCodeAt(pos), ScriptTarget.Latest) && (pos < end))) {
          {
            (pos += 1)

          }
        }
        return (token = SyntaxKind.Identifier)

      }
      else {
        return ((pos += 1), (token = SyntaxKind.Unknown))

      }

    }
    def speculationHelper[T](callback: (() => T), isLookahead: Boolean): T = {
      val savePos = pos
      val saveStartPos = startPos
      val saveTokenPos = tokenPos
      val saveToken = token
      val saveTokenValue = tokenValue
      val savePrecedingLineBreak = precedingLineBreak
      val result = callback()
      if (((!result) || isLookahead)) {
        (pos = savePos)
        (startPos = saveStartPos)
        (tokenPos = saveTokenPos)
        (token = saveToken)
        (tokenValue = saveTokenValue)
        (precedingLineBreak = savePrecedingLineBreak)

      }
      return result

    }
    def scanRange[T](start: Int, length: Int, callback: (() => T)): T = {
      val saveEnd = end
      val savePos = pos
      val saveStartPos = startPos
      val saveTokenPos = tokenPos
      val saveToken = token
      val savePrecedingLineBreak = precedingLineBreak
      val saveTokenValue = tokenValue
      val saveHasExtendedUnicodeEscape = hasExtendedUnicodeEscape
      val saveTokenIsUnterminated = tokenIsUnterminated
      setText(text, start, length)
      val result = callback()
      (end = saveEnd)
      (pos = savePos)
      (startPos = saveStartPos)
      (tokenPos = saveTokenPos)
      (token = saveToken)
      (precedingLineBreak = savePrecedingLineBreak)
      (tokenValue = saveTokenValue)
      (hasExtendedUnicodeEscape = saveHasExtendedUnicodeEscape)
      (tokenIsUnterminated = saveTokenIsUnterminated)
      return result

    }
    def lookAhead[T](callback: (() => T)): T = {
      return speculationHelper(callback, true)

    }
    def tryScan[T](callback: (() => T)): T = {
      return speculationHelper(callback, false)

    }
    def getText(): String = {
      return text

    }
    def setText(newText: String, start: Int, length: Int) = {
      (text = (newText || ""))
      (end = (if ((length === undefined)) text.length else (start + length)))
      setTextPos((start || 0))

    }
    def setOnError(errorCallback: ErrorCallback) = {
      (onError = errorCallback)

    }
    def setScriptTarget(scriptTarget: ScriptTarget) = {
      (languageVersion = scriptTarget)

    }
    def setLanguageVariant(variant: LanguageVariant) = {
      (languageVariant = variant)

    }
    def setTextPos(textPos: Int) = {
      Debug.assert((textPos >= 0))
      (pos = textPos)
      (startPos = textPos)
      (tokenPos = textPos)
      (token = SyntaxKind.Unknown)
      (precedingLineBreak = false)
      (tokenValue = undefined)
      (hasExtendedUnicodeEscape = false)
      (tokenIsUnterminated = false)

    }

  }
}
