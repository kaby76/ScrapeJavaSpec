using Antlr4.Runtime;
using System.Text;
using HtmlAgilityPack;
using System.Linq;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System;
using Antlr4.Runtime.Tree;
using System.Runtime.ExceptionServices;
using System.Diagnostics.CodeAnalysis;
using System.Net.Mime;

namespace ReadJavaGrammar
{
    class Program
    {
        public static string addr = "https://docs.oracle.com/javase/specs/jls/se8/html/jls-19.html";

        static void Rec(StringBuilder sb, string lhs, List<HtmlNode> rhs_node_children, bool alt = false)
        {
            bool first = true;
            for (int j = 0; j < rhs_node_children.Count; ++j)
            {
                var rhs_s = rhs_node_children[j];
                var rhs = rhs_s.InnerText;
                if (rhs.Trim() == "(one of)")
                {
                    alt = true;
                    ++j;
                    for (; j < rhs_node_children.Count; ++j)
                    {
                        if (rhs_node_children[j].Name == "br")
                            continue;
                        else break;
                    }
                }
                else if (rhs_s.Name == "br")
                {
                    if (j + 1 == rhs_node_children.Count
                        || (rhs_node_children[j + 1].InnerText.Replace(" ", "").Replace("\n", "").Trim() == "" && j+2 == rhs_node_children.Count))
                    {

                    }
                    else
                        sb.Append("| ");
                    // It appears after visiting the spec, a <br> can be in "(one of)"
                    // rules. But, the <br> has no meaning, so continue to consider anything
                    // as alt.
                    // Also, do not add "|" if the last thing on the children list
                    // as in "ConditionalExpression:"
                }
                else if (rhs_s.Name == "#text")
                {
                    sb.Append(rhs);
                }
                else if (rhs_s.Name == "a")
                {
                    if (alt && !first)
                    {
                        sb.Append("| ");
                    }
                    first = false;
                    sb.Append(lc(rhs));
                }
                else if (rhs_s.Name == "code")
                {
                    if (alt && !first)
                    {
                        sb.Append("| ");
                    }
                    first = false;
                    var str = rhs.Trim();
                    str = str.Replace("&lt;", "<");
                    str = str.Replace("&gt;", ">");
                    str = str.Replace("&amp;", "&");
                    rhs = "'" + str + "'";
                    sb.Append(rhs);
                }
                else if (rhs_s.Name == "pre")
                {
                    Rec(sb, lhs, rhs_s.ChildNodes.ToList(), true);
                }
                else
                {
                    sb.Append(rhs);
                }
            }
        }

        static void Main(string[] args)
        {
            StringBuilder sb = new StringBuilder();
            HtmlAgilityPack.HtmlDocument doc = null;
            string local_file_name = "c:/temp/java.gra";
            if (! System.IO.File.Exists(local_file_name))
            {
                HttpWebRequest request = (HttpWebRequest)WebRequest.Create(addr);
                HttpWebResponse response = (HttpWebResponse)request.GetResponse();
                if (response.StatusCode == HttpStatusCode.OK)
                {
                    Stream receiveStream = response.GetResponseStream();
                    StreamReader readStream = null;
                    if (String.IsNullOrWhiteSpace(response.CharacterSet))
                        readStream = new StreamReader(receiveStream);
                    else
                        readStream = new StreamReader(receiveStream, Encoding.GetEncoding(response.CharacterSet));
                    string data = readStream.ReadToEnd();
                    System.IO.File.WriteAllText(local_file_name, data);
                    response.Close();
                    readStream.Close();
                }
            }
            doc = new HtmlAgilityPack.HtmlDocument();
            string htmlString = System.IO.File.ReadAllText(local_file_name);
            doc.LoadHtml(htmlString);

            var lhs_list = doc.DocumentNode.SelectNodes("//div[@class='lhs']").ToList();
            var rhs_list = doc.DocumentNode.SelectNodes("//div[@class = 'rhs']").ToList();
            for (int i = 0; i < lhs_list.Count; ++i)
            {
                var lhs = lhs_list[i].InnerText;
                var rhs_node = rhs_list[i];
                List<HtmlNode> rhs_node_children = rhs_node.ChildNodes.ToList();
                if (i >= 4)
                {
                    sb.AppendLine(lc(lhs));
                    Rec(sb, lhs, rhs_node_children);
                    sb.AppendLine(";");
                }
            }
            var input = sb.ToString();
            var str = new AntlrInputStream(input);
            var lexer = new JavaSpecLexer(str);
            var tokens = new CommonTokenStream(lexer);
            var parser = new JavaSpecParser(tokens);
            var tree = parser.rules();
            var walker = new ParseTreeWalker();
            var listener = new Listen();
            walker.Walk(listener, tree);
            System.Console.WriteLine(listener.sb.ToString());
        }

        static string lc(string id)
        {
            if (id == "Throws") id = "throws_";
            if (id == "Throws:") id = "throws_:";
            if (id == "Finally") id = "finally_";
            if (id == "Finally:") id = "finally_:";
            if (!(id.Contains("IntegerLiteral")
                || id.Contains("CharacterLiteral")
                || id.Contains("FloatingPointLiteral")
                || id.Contains("BooleanLiteral")
                || id.Contains("StringLiteral")
                || id.Contains("NullLiteral")
                ))
            {
                if (id != string.Empty && char.IsUpper(id[0]))
                {
                    id = char.ToLower(id[0]) + id.Substring(1);
                }
            }
            return id;
        }
    }

    class Listen : JavaSpecBaseListener
    {
        public StringBuilder sb = new StringBuilder();

        public Listen()
        {
            sb.AppendLine("grammar Java"
                + (Program.addr.Contains("se8") ? "8" : "unknown")
                + ";");
            sb.AppendLine();
        }

        public override void ExitRule([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.RuleContext context)
        {
            sb.AppendLine(" ;");
        }

        public override void ExitLhs([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.LhsContext context)
        {
            sb.Append(context.ID().GetText() + " :");
        }

        public override void ExitRhs_sym([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Rhs_symContext context)
        {
            if (context.ID() != null) sb.Append(" " + context.ID().GetText());
            else if (context.PIPE() != null) sb.Append(" |");
            else if (context.SL() != null) sb.Append(" " + context.SL().GetText());
        }

        public override void EnterZero_or_one([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_oneContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" (");
            }
        }

        public override void ExitZero_or_one([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_oneContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" )");
            }
            sb.Append("?");
        }

        public override void EnterZero_or_more([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_moreContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" (");
            }
        }

        public override void ExitZero_or_more([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_moreContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" )");
            }
            sb.Append("*");
        }

        public override void ExitRules([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.RulesContext context)
        {
            sb.AppendLine(@"
// LEXER

identifier : Identifier | 'to' | 'module' | 'open' | 'with' | 'provides' | 'uses' | 'opens' | 'requires' | 'exports';

// §3.9 Keywords

ABSTRACT : 'abstract';
ASSERT : 'assert';
BOOLEAN : 'boolean';
BREAK : 'break';
BYTE : 'byte';
CASE : 'case';
CATCH : 'catch';
CHAR : 'char';
CLASS : 'class';
CONST : 'const';
CONTINUE : 'continue';
DEFAULT : 'default';
DO : 'do';
DOUBLE : 'double';
ELSE : 'else';
ENUM : 'enum';
EXTENDS : 'extends';
FINAL : 'final';
FINALLY : 'finally';
FLOAT : 'float';
FOR : 'for';
IF : 'if';
GOTO : 'goto';
IMPLEMENTS : 'implements';
IMPORT : 'import';
INSTANCEOF : 'instanceof';
INT : 'int';
INTERFACE : 'interface';
LONG : 'long';
NATIVE : 'native';
NEW : 'new';
PACKAGE : 'package';
PRIVATE : 'private';
PROTECTED : 'protected';
PUBLIC : 'public';
RETURN : 'return';
SHORT : 'short';
STATIC : 'static';
STRICTFP : 'strictfp';
SUPER : 'super';
SWITCH : 'switch';
SYNCHRONIZED : 'synchronized';
THIS : 'this';
THROW : 'throw';
THROWS : 'throws';
TRANSIENT : 'transient';
TRY : 'try';
VOID : 'void';
VOLATILE : 'volatile';
WHILE : 'while';
UNDER_SCORE : '_';//Introduced in Java 9

// §3.10.1 Integer Literals

IntegerLiteral
	:	DecimalIntegerLiteral
	|	HexIntegerLiteral
	|	OctalIntegerLiteral
	|	BinaryIntegerLiteral
	;

fragment
DecimalIntegerLiteral
	:	DecimalNumeral IntegerTypeSuffix?
	;

fragment
HexIntegerLiteral
	:	HexNumeral IntegerTypeSuffix?
	;

fragment
OctalIntegerLiteral
	:	OctalNumeral IntegerTypeSuffix?
	;

fragment
BinaryIntegerLiteral
	:	BinaryNumeral IntegerTypeSuffix?
	;

fragment
IntegerTypeSuffix
	:	[lL]
	;

fragment
DecimalNumeral
	:	'0'
	|	NonZeroDigit (Digits? | Underscores Digits)
	;

fragment
Digits
	:	Digit (DigitsAndUnderscores? Digit)?
	;

fragment
Digit
	:	'0'
	|	NonZeroDigit
	;

fragment
NonZeroDigit
	:	[1-9]
	;

fragment
DigitsAndUnderscores
	:	DigitOrUnderscore+
	;

fragment
DigitOrUnderscore
	:	Digit
	|	'_'
	;

fragment
Underscores
	:	'_'+
	;

fragment
HexNumeral
	:	'0' [xX] HexDigits
	;

fragment
HexDigits
	:	HexDigit (HexDigitsAndUnderscores? HexDigit)?
	;

fragment
HexDigit
	:	[0-9a-fA-F]
	;

fragment
HexDigitsAndUnderscores
	:	HexDigitOrUnderscore+
	;

fragment
HexDigitOrUnderscore
	:	HexDigit
	|	'_'
	;

fragment
OctalNumeral
	:	'0' Underscores? OctalDigits
	;

fragment
OctalDigits
	:	OctalDigit (OctalDigitsAndUnderscores? OctalDigit)?
	;

fragment
OctalDigit
	:	[0-7]
	;

fragment
OctalDigitsAndUnderscores
	:	OctalDigitOrUnderscore+
	;

fragment
OctalDigitOrUnderscore
	:	OctalDigit
	|	'_'
	;

fragment
BinaryNumeral
	:	'0' [bB] BinaryDigits
	;

fragment
BinaryDigits
	:	BinaryDigit (BinaryDigitsAndUnderscores? BinaryDigit)?
	;

fragment
BinaryDigit
	:	[01]
	;

fragment
BinaryDigitsAndUnderscores
	:	BinaryDigitOrUnderscore+
	;

fragment
BinaryDigitOrUnderscore
	:	BinaryDigit
	|	'_'
	;

// §3.10.2 Floating-Point Literals

FloatingPointLiteral
	:	DecimalFloatingPointLiteral
	|	HexadecimalFloatingPointLiteral
	;

fragment
DecimalFloatingPointLiteral
	:	Digits '.' Digits? ExponentPart? FloatTypeSuffix?
	|	'.' Digits ExponentPart? FloatTypeSuffix?
	|	Digits ExponentPart FloatTypeSuffix?
	|	Digits FloatTypeSuffix
	;

fragment
ExponentPart
	:	ExponentIndicator SignedInteger
	;

fragment
ExponentIndicator
	:	[eE]
	;

fragment
SignedInteger
	:	Sign? Digits
	;

fragment
Sign
	:	[+-]
	;

fragment
FloatTypeSuffix
	:	[fFdD]
	;

fragment
HexadecimalFloatingPointLiteral
	:	HexSignificand BinaryExponent FloatTypeSuffix?
	;

fragment
HexSignificand
	:	HexNumeral '.'?
	|	'0' [xX] HexDigits? '.' HexDigits
	;

fragment
BinaryExponent
	:	BinaryExponentIndicator SignedInteger
	;

fragment
BinaryExponentIndicator
	:	[pP]
	;

// §3.10.3 Boolean Literals

BooleanLiteral
	:	'true'
	|	'false'
	;

// §3.10.4 Character Literals

CharacterLiteral
	:	'\'' SingleCharacter '\''
	|	'\'' EscapeSequence '\''
	;

fragment
SingleCharacter
	:	~['\\\r\n]
	;

// §3.10.5 String Literals

StringLiteral
	:	'""' StringCharacters? '""'
	;

fragment
StringCharacters
	:	StringCharacter+
	;

fragment
StringCharacter
	:	~[""\\\r\n]
	|	EscapeSequence
	;

// §3.10.6 Escape Sequences for Character and String Literals

fragment
EscapeSequence
	:	'\\' [btnfr""'\\]
	|	OctalEscape
    |   UnicodeEscape // This is not in the spec but prevents having to preprocess the input
	;

fragment
OctalEscape
	:	'\\' OctalDigit
	|	'\\' OctalDigit OctalDigit
	|	'\\' ZeroToThree OctalDigit OctalDigit
	;

fragment
ZeroToThree
	:	[0-3]
	;

// This is not in the spec but prevents having to preprocess the input
fragment
UnicodeEscape
    :   '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
    ;

// §3.10.7 The Null Literal

NullLiteral
	:	'null'
	;

// §3.11 Separators

LPAREN : '(';
RPAREN : ')';
LBRACE : '{';
RBRACE : '}';
LBRACK : '[';
RBRACK : ']';
SEMI : ';';
COMMA : ',';
DOT : '.';
ELLIPSIS : '...';
AT : '@';
COLONCOLON : '::';


// §3.12 Operators

ASSIGN : '=';
GT : '>';
LT : '<';
BANG : '!';
TILDE : '~';
QUESTION : '?';
COLON : ':';
ARROW : '->';
EQUAL : '==';
LE : '<=';
GE : '>=';
NOTEQUAL : '!=';
AND : '&&';
OR : '||';
INC : '++';
DEC : '--';
ADD : '+';
SUB : '-';
MUL : '*';
DIV : '/';
BITAND : '&';
BITOR : '|';
CARET : '^';
MOD : '%';
//LSHIFT : '<<';
//RSHIFT : '>>';
//URSHIFT : '>>>';

ADD_ASSIGN : '+=';
SUB_ASSIGN : '-=';
MUL_ASSIGN : '*=';
DIV_ASSIGN : '/=';
AND_ASSIGN : '&=';
OR_ASSIGN : '|=';
XOR_ASSIGN : '^=';
MOD_ASSIGN : '%=';
LSHIFT_ASSIGN : '<<=';
RSHIFT_ASSIGN : '>>=';
URSHIFT_ASSIGN : '>>>=';

// §3.8 Identifiers (must appear after all keywords in the grammar)

Identifier
	:	JavaLetter JavaLetterOrDigit*
	;

fragment
JavaLetter
	:	[a-zA-Z$_] // these are the ""java letters"" below 0x7F
	|	// covers all characters above 0x7F which are not a surrogate
		~[\u0000-\u007F\uD800-\uDBFF]
		{Character.isJavaIdentifierStart(_input.LA(-1))}?
	|	// covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
		[\uD800-\uDBFF] [\uDC00-\uDFFF]
		{Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
	;

fragment
JavaLetterOrDigit
	:	[a-zA-Z0-9$_] // these are the ""java letters or digits"" below 0x7F
	|	// covers all characters above 0x7F which are not a surrogate
		~[\u0000-\u007F\uD800-\uDBFF]
		{Character.isJavaIdentifierPart(_input.LA(-1))}?
	|	// covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
		[\uD800-\uDBFF] [\uDC00-\uDFFF]
		{Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
	;

//
// Whitespace and comments
//

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> channel(HIDDEN)
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> channel(HIDDEN)
    ;
");
        }
    }
}
