{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Char
import Text.Parsec.Language
import Data.List
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)
import GHC.Generics
import Control.Applicative ((<*>),(<$>),(<*),(*>))


type CPPParser a = Parsec [Char] () a

lexer :: T.GenTokenParser [Char] () Identity
lexer = T.makeTokenParser $ emptyDef {
	T.commentStart    = "/*", T.commentEnd = "*/",
	T.commentLine     = "//",
	T.nestedComments  = True,
	T.identStart      = letter <|> char '_',
	T.identLetter     = alphaNum <|> char '_',
	T.reservedNames   = [
		"new","delete",
		"alignas","alignof","asm","auto","bool","break","case","catch","char","char16_t","char32_t",
		"class","const","constexpr","const_cast","continue","decltype","default","delete","dynamic_cast",
		"else","enum","explicit","export","extern","false","float","for","friend","goto","if","inline",
		"int","long","mutable","namespace","noexcept","operator","private","protected","public","register",
		"reinterpret_cast","return","short","signed","sizeof","static","static_assert","static_cast",
		"struct","switch","template","this","thread_local","throw","true","try","typedef","typeid","typename",
		"union","unsigned","using","virtual","void","volatile","wchar_t","while" ],
	T.reservedOpNames = [
		"and","and_eq","bitand","bitor","xor","xor_eq","compl","not","not_eq","or","or_eq",
		"%:%:","...",">>=","<<=","->*","+=","-=","*=","/=","%=","::","<=",">=","&&","||","++","--","->",
		"##","<:",":>","<%","%>","%:",".*","ˆ=","&=","|=","==","!=","<<",">>",
		"!","=","<",">","#",":","?","+","-","*","/","%","ˆ","&","|","~",".","," ],
	T.caseSensitive   = True }

symbol     = T.symbol     lexer
reserved   = T.reserved   lexer
braces     = T.braces     lexer
parens     = T.parens     lexer
semi       = T.semi       lexer
identifier = T.identifier lexer
commaSep1  = T.commaSep1  lexer
comma      = T.comma      lexer


parseTranslUnit :: String -> String -> Either ParseError TranslUnit
parseTranslUnit filename input = runParser (T.whiteSpace lexer *> translation_unit) () filename input

--------- From C++ Standard ISO N 3092
-- http://www.nongnu.org/hcb/

type Identifier = String

data TranslUnit = TranslUnit [Declaration] deriving (Show,Generic)

-- translation-unit:
-- declaration-seqopt

-- declaration-seq:
-- declaration
-- declaration-seq declaration
translation_unit = TranslUnit <$> many declaration

data Declaration = FunctionDef_Decl FunctionDef deriving (Show,Generic)
-- declaration:
-- block-declaration
-- function-definition
-- template-declaration
-- explicit-instantiation
-- explicit-specialization
-- linkage-specification
-- namespace-definition
-- empty-declaration
-- attribute-declaration
declaration =
--	block_declaration       <|>
	FunctionDef_Decl <$> function_definition
--	template_declaration    <|>
--	explicit_instantiation  <|>
--	explicit_specialization <|>
--	linkage_specification   <|>
--	namespace_definition    <|>
--	empty_declaration       <|>
--	attribute_declaration

data FunctionDef = FunctionDef {-(Maybe ?)-} [DeclSpec] Declarator FunctionBody deriving (Show,Generic)
-- function-definition:
-- attribute-specifieropt decl-specifier-seqopt declarator function-body
-- attribute-specifieropt decl-specifier-seqopt declarator = default ;
-- attribute-specifieropt decl-specifier-seqopt declarator = delete ;
function_definition = FunctionDef <$> {-optionMaybe attribute_specifier <*>-} many decl_specifier <*> declarator <*> function_body

data DeclSpec = Type_DeclSpec SimpleType | Friend_DeclSpec | TypeDef_DeclSpec | ConstExpr_DeclSpec deriving (Show,Generic)
-- decl-specifier:
-- storage-class-specifier
-- type-specifier
-- function-specifier
-- friend
-- typedef
-- constexpr
decl_specifier =
-- storage-class-specifier <|>
	Type_DeclSpec      <$> type_specifier      <|>
-- function-specifier <|>
	Friend_DeclSpec    <$ reserved "friend"    <|>
	TypeDef_DeclSpec   <$ reserved "typedef"   <|>
	ConstExpr_DeclSpec <$ reserved "constexpr"

data TypeSpecifier =
	Simple_TypeSpecifier SimpleType
	deriving (Show,Generic)

-- type-specifier:
-- trailing-type-specifier
-- class-specifier
-- enum-specifier
type_specifier =
	trailing_type_specifier
-- class-specifier <|>
-- enum-specifier

-- trailing-type-specifier:
-- simple-type-specifier
-- elaborated-type-specifier
-- typename-specifier
-- cv-qualifier
trailing_type_specifier =
	Simple_TypeSpecifier <$> simple_type_specifier
-- elaborated-type-specifier <|>
-- typename-specifier <|>
-- cv-qualifier

data SimpleType = Char_SimpleType | Int_SimpleType
	deriving (Show,Generic)

-- simple-type-specifier:
-- ::opt nested-name-specifieropt type-name
-- ::opt nested-name-specifier template simple-template-id
-- char
-- char16_t
-- char32_t
-- wchar_t
-- bool
-- short
-- int
-- long
-- signed
-- unsigned
-- float
-- double
-- void
-- auto
-- decltype-specifier
simple_type_specifier =
	Char_SimpleType <$ reserved "char" <|>
	Int_SimpleType  <$ reserved "int"

data Declarator = Declarator Identifier ParamDecls deriving (Show,Generic)
-- declarator:
-- ptr-declarator
-- noptr-declarator parameters-and-qualifiers trailing-return-type
declarator = 
	ptr_declarator
-- noptr-declarator parameters-and-qualifiers trailing-return-type

-- ptr-declarator:
-- noptr-declarator
-- ptr-operator ptr-declarator
ptr_declarator =
	noptr_declarator
-- ptr-operator ptr-declarator

-- noptr-declarator:
-- declarator-id attribute-specifieropt
-- noptr-declarator parameters-and-qualifiers
-- noptr-declarator [ constant-expressionopt ] attribute-specifieropt
-- ( ptr-declarator )
noptr_declarator =
	try (Declarator <$> declarator_id <*> parameters_and_qualifiers) <|>
	Declarator <$> declarator_id <*> pure (ParamDecls [] False)
-- noptr-declarator [ constant-expressionopt ] attribute-specifieropt <|>
-- ( ptr-declarator )

-- declarator-id:
-- ...opt id-expression
-- ::opt nested-name-specifieropt class-name
declarator_id =
	{-...opt-} id_expression
-- ::opt nested-name-specifieropt class-name

-- id-expression:
-- unqualified-id
-- qualified-id
id_expression =
	unqualified_id
--	qualified_id

-- unqualified-id:
-- identifier
-- operator-function-id
-- conversion-function-id
-- literal-operator-id
-- ~ class-name
-- ~ decltype-specifier
-- template-id
unqualified_id =
	identifier
-- operator-function-id
-- conversion-function-id
-- literal-operator-id
-- ~ class-name
-- ~ decltype-specifier
-- template-id

-- parameters-and-qualifiers:
-- ( parameter-declaration-clause ) attribute-specifieropt cv-qualifier-seqopt
-- ref-qualifieropt exception-specificationopt
parameters_and_qualifiers =
	parens parameter_declaration_clause {-attribute-specifieropt cv-qualifier-seqopt-}
-- ref-qualifieropt exception-specificationopt

-- trailing-return-type:
-- -> trailing-type-specifier-seq abstract-declaratoropt

data ParamDecls = ParamDecls [ParamDecl] Bool deriving (Show,Generic)

-- parameter-declaration-clause:
-- parameter-declaration-listopt ...opt
-- parameter-declaration-list , ...
parameter_declaration_clause =
	ParamDecls <$> option [] parameter_declaration_list <*> (isJust <$> optionMaybe (reserved "...")) <|>
	ParamDecls <$> parameter_declaration_list <*> ( True <$ (comma *> reserved "...") )

-- parameter-declaration-list:
-- parameter-declaration
-- parameter-declaration-list , parameter-declaration
parameter_declaration_list = commaSep1 parameter_declaration

data ParamDecl = ParamDecl [DeclSpec] Declarator deriving (Show,Generic)
-- parameter-declaration:
-- attribute-specifieropt decl-specifier-seq declarator
-- attribute-specifieropt decl-specifier-seq declarator = assignment-expression
-- attribute-specifieropt decl-specifier-seq abstract-declaratoropt
-- attribute-specifieropt decl-specifier-seq abstract-declaratoropt = assignment-expression
parameter_declaration =
	{-attribute-specifieropt-} ParamDecl <$> many1 decl_specifier <*> declarator
-- attribute-specifieropt decl-specifier-seq declarator = assignment-expression
-- attribute-specifieropt decl-specifier-seq abstract-declaratoropt
-- attribute-specifieropt decl-specifier-seq abstract-declaratoropt = assignment-expression

data FunctionBody = FunctionBody Statement | Default_FunctionBody | Delete_FunctionBody deriving (Show,Generic)
-- function-body:
-- ctor-initializeropt compound-statement
-- function-try-block
function_body =
-- TODO: ctor-initializeropt
	symbol "=" *> (
		Default_FunctionBody <$ (reserved "default" <* semi) <|>
		Delete_FunctionBody  <$ (reserved "delete"  <* semi) ) <|>
	FunctionBody <$> compound_statement
-- TODO: function-try-block


-- compound-statement:
-- { statement-seqopt }

-- statement-seq:
-- statement
-- statement-seq statement
compound_statement = Compound_Statement <$> braces (many statement)

data Statement =
	Break_Statement |
	Continue_Statement |
	Compound_Statement [Statement] |
	Return_Statement (Maybe Expression) |
	Goto_Statement Identifier
	deriving (Show,Generic)

-- statement:
-- labeled-statement
-- attribute-specifieropt expression-statement
-- attribute-specifieropt compound-statement
-- attribute-specifieropt selection-statement
-- attribute-specifieropt iteration-statement
-- attribute-specifieropt jump-statement
-- declaration-statement
-- attribute-specifieropt try-block
statement =
	jump_statement

-- jump-statement:
-- break ;
-- continue ;
-- return expressionopt ;
-- return braced-init-list ;
-- goto identifier ;
jump_statement =
	Break_Statement    <$  (reserved "break"    *> semi) <|>
	Continue_Statement <$  (reserved "continue" *> semi) <|>
	Return_Statement   <$> (reserved "return"   *> optionMaybe expression <* semi) <|>
-- TODO: return braced-init-list ;
	Goto_Statement     <$> (identifier <* semi)

-- type-specifier-seq:
-- type-specifier attribute-specifieropt
-- type-specifier type-specifier-seq
type_specifier_seq = many1 type_specifier {- TODO: attribute-specifieropt -}

data TypeId = TypeId [TypeSpecifier]
	deriving (Show,Generic)

-- type-id:
-- type-specifier-seq abstract-declaratoropt
type_id = TypeId <$> type_specifier_seq {- TODO: abstract-declaratoropt -}

-- postfix-expression:
-- primary-expression
-- postfix-expression [ expression ]
-- postfix-expression [ braced-init-list ]
-- postfix-expression ( expression-listopt )
-- simple-type-specifier ( expression-listopt )
-- typename-specifier ( expression-listopt )
-- simple-type-specifier braced-init-list
-- typename-specifier braced-init-list
-- postfix-expression . templateopt id-expression
-- postfix-expression -> templateopt id-expression
-- postfix-expression . pseudo-destructor-name
-- postfix-expression -> pseudo-destructor-name
-- postfix-expression ++
-- postfix-expression --
-- dynamic_cast < type-id > ( expression )
-- static_cast < type-id > ( expression )
-- reinterpret_cast < type-id > ( expression )
-- const_cast < type-id > ( expression )
-- typeid ( expression )
-- typeid ( type-id )
postfix_expression =
	primary_expression <|>
	many1
-- postfix-expression ++	
-- postfix-expression --
-- postfix-expression ( expression-listopt )
-- postfix-expression [ expression ]
-- postfix-expression [ braced-init-list ]
-- postfix-expression . templateopt id-expression
-- postfix-expression -> templateopt id-expression
-- postfix-expression . pseudo-destructor-name
-- postfix-expression -> pseudo-destructor-name
-- simple-type-specifier ( expression-listopt )
-- typename-specifier ( expression-listopt )
-- simple-type-specifier braced-init-list
-- typename-specifier braced-init-list
-- dynamic_cast < type-id > ( expression )
-- static_cast < type-id > ( expression )
-- reinterpret_cast < type-id > ( expression )
-- const_cast < type-id > ( expression )
-- typeid ( expression )
-- typeid ( type-id )


-- unary-expression:
-- postfix-expression
-- ++ cast-expression
-- -- cast-expression
-- unary-operator cast-expression
-- sizeof unary-expression
-- sizeof ( type-id )
-- sizeof ... ( identifier )
-- alignof ( type-id )
-- noexcept-expression
-- new-expression
-- delete-expression
unary_expression =
	postfix_expression <|>
	UnaryExpression <$> unary_operator <*> cast_expression
-- TODO: sizeof ( type-id )
-- TODO: sizeof ... ( identifier )
-- TODO: alignof ( type-id )
-- TODO: noexcept-expression
-- TODO: new-expression
-- TODO: delete-expression


data UnaryOperator =
	Indirection | AddressOf | Positive | Negative | Negation | OnesComplement |
	PreIncrement | PreDecrement | PostIncrement | PostDecrement | SizeOfExpr
	deriving (Show,Generic)

-- unary-operator: one of
-- * & + - ! ~
unary_operator =
	Indirection    <$ symbol "*"  <|>
	AddressOf      <$ symbol "&"  <|>
	Positive       <$ symbol "+"  <|>
	Negative       <$ symbol "-"  <|>
	Negation       <$ symbol "!"  <|>
	OnesComplement <$ symbol "~"  <|>
	PreDecrement   <$ symbol "--" <|>
	PreIncrement   <$ symbol "++" <|>
	SizeOfExpr     <$ reserved "sizeof" 

-- cast-expression:
-- unary-expression
-- ( type-id ) cast-expression
cast_expression =
	unary_expression <|>
	CastExpression <$> braces type_id <*> cast_expression

-- pm-expression:
-- cast-expression
-- pm-expression .* cast-expression
-- pm-expression ->* cast-expression
pm_expression = chainl1 cast_expression $
	BinaryExpression PtrToMemberDot  <$ symbol ".*" <|>
	BinaryExpression PtrToMemberArr <$ symbol "->*"

-- multiplicative-expression:
-- pm-expression
-- multiplicative-expression * pm-expression
-- multiplicative-expression / pm-expression
-- multiplicative-expression % pm-expression
multiplicative_expression = chainl1 pm_expression $
	BinaryExpression Mult <$ symbol "*" <|>
	BinaryExpression Div  <$ symbol "/" <|>
	BinaryExpression Mod  <$ symbol "%"

-- additive-expression:
-- multiplicative-expression
-- additive-expression + multiplicative-expression
-- additive-expression - multiplicative-expression
additive_expression = chainl1 multiplicative_expression $
	BinaryExpression Plus  <$ symbol "+" <|>
	BinaryExpression Minus <$ symbol "-"

-- shift-expression:
-- additive-expression
-- shift-expression << additive-expression
-- shift-expression >> additive-expression
shift_expression = chainl1 additive_expression $
	BinaryExpression ShiftLeft  <$ symbol "<<" <|>
	BinaryExpression ShiftRight <$ symbol ">>"	

-- relational-expression:
-- shift-expression
-- relational-expression < shift-expression
-- relational-expression > shift-expression
-- relational-expression <= shift-expression
-- relational-expression >= shift-expression
relational_expression = chainl1 shift_expression $
	BinaryExpression Less      <$ symbol "<" <|>
	BinaryExpression Greater   <$ symbol ">" <|>
	BinaryExpression LessEq    <$ symbol "<=" <|>
	BinaryExpression GreaterEq <$ symbol ">=" <|>

-- equality-expression:
-- relational-expression
-- equality-expression == relational-expression
-- equality-expression != relational-expression
equality_expression = chainl1 relational_expression $
	BinaryExpression Equality   <$ symbol "==" <|>
	BinaryExpression InEquality <$ symbol "!=" 

-- and-expression:
-- equality-expression
-- and-expression & equality-expression
and_expression = chainl1 equality_expression (BinaryExpression And <$ symbol "&")

-- exclusive-or-expression:
-- and-expression
-- exclusive-or-expression ˆ and-expression
exclusive_or_expression = chainl1 and_expression (BinaryExpression ExclusiveOr <$ symbol "^")

-- inclusive-or-expression:
-- exclusive-or-expression
-- inclusive-or-expression | exclusive-or-expression
inclusive_or_expression = chainl1 exclusive_or_expression (BinaryExpression InclusiveOr <$ symbol "|")

-- logical-and-expression:
-- inclusive-or-expression
-- logical-and-expression && inclusive-or-expression
logical_and_expression = chainl1 inclusive_or_expression (BinaryExpression LogicalAnd <$ symbol "&&")

-- logical-or-expression:
-- logical-and-expression
-- logical-or-expression || logical-and-expression
logical_or_expression = chainl1 logical_and_expression (BinaryExpression LogicalOr <$ symbol "||")

-- conditional-expression:
-- logical-or-expression
-- logical-or-expression ? expression : assignment-expression
conditional_expression =
	try ( ConditionalExpression <$> logical_or_expression <*> expression <*> assignment_expression ) <|>
	logical_or_expression

-- assignment-expression:
-- conditional-expression
-- logical-or-expression assignment-operator initializer-clause
-- throw-expression
assignment_expression =
	conditional_expression <|>
	AssignmentExpression <$> logical_or_expression <*> assignment_operator <*> intializer_clause
-- TODO: throw-expression

data AssignmentOperator =
	Assign | MultAssign | DivAssign | ModAssign | PlusAssign | MinusAssign |
	ShiftRightAssign | ShiftLeftAssign | AndAssign | NotAssign | OrAssign
	deriving (Show,Generic)

-- assignment-operator: one of
-- = *= /= %= += -= >>= <<= &= ˆ= |=
assignment_operator =
	Assign           <$ symbol "="   <|>
	MultAssign       <$ symbol "*="  <|>
	DivAssign        <$ symbol "/="  <|>
	ModAssign        <$ symbol "%="  <|>
	PlusAssign       <$ symbol "+="  <|>
	MinusAssign      <$ symbol "-="  <|>
	ShiftRightAssign <$ symbol ">>=" <|>
	ShiftLeftAssign  <$ symbol "<<=" <|>
	AndAssign        <$ symbol "&="  <|>
	NotAssign        <$ symbol "^="  <|>
	OrAssign         <$ symbol "|="  <|>

data BinaryOp =
	LogicalOr | LogicalAnd | InclusiveOr | ExclusiveOr | And | Equality | Inequality |
	Less | Greater | LessEq | GreaterEq | Plus | Minus | ShiftLeft | ShiftRight | Mult | Div | Mod |
	PtrToMemberArr | PtrToMemberDot
	deriving (Show,Generic)

data Expression =
	UnaryExpression UnaryOp Expression |
	BinaryExpression BinaryOp Expression Expression |
	AssignmentExpression Expression AssignmentOperator Expression |
	ConditionalExpression Expression Expression Expression |
	CastExpression TypeId Expression
	deriving (Show,Generic)

-- expression:
-- assignment-expression
-- expression , assignment-expression
expression =
	assignment_expression <|>
	Comma_Expression <$> (expression <* comma) <*> assignment_expression

-- constant-expression:
-- conditional-expression

{-
-- typedef-name:
-- identifier

-- namespace-name:
-- original-namespace-name
-- namespace-alias

-- original-namespace-name:
-- identifier

-- namespace-alias:
-- identifier

-- class-name:
-- identifier
-- template-id

-- enum-name:
-- identifier

-- template-name:
-- identifier

-- hex-quad:
-- hexadecimal-digit hexadecimal-digit hexadecimal-digit hexadecimal-digit

-- universal-character-name:
-- \u hex-quad
-- \U hex-quad hex-quad

-- preprocessing-token:
-- header-name
-- identifier
-- pp-number
-- character-literal
-- user-defined-character-literal
-- string-literal
-- user-defined-string-literal
-- preprocessing-op-or-punc
-- each non-white-space character that cannot be one of the above

-- preprocessing-op-or-punc: one of
-- { } [ ] # ## ( )
-- <: :> <% %> %: %:%: ; : ...
-- new delete ? :: . .*
-- + - * / % ˆ & | ~
-- ! = < > += -= *= /= %=
-- ˆ= &= |= << >> >>= <<= == !=
-- <= >= && || ++ -- , ->* ->
-- and and_eq bitand bitor compl not not_eq
-- or or_eq xor xor_eq

-- token:
-- identifier
-- keyword
-- literal
-- operator
-- punctuator

-- header-name:
-- < h-char-sequence >
-- " q-char-sequence "

-- h-char-sequence:
-- h-char
-- h-char-sequence h-char

-- h-char:
-- any member of the source character set except new-line and >

-- q-char-sequence:
-- q-char
-- q-char-sequence q-char

-- q-char:
-- any member of the source character set except new-line and "

-- pp-number:
-- digit
-- . digit
-- pp-number digit
-- pp-number identifier-nondigit
-- pp-number e sign
-- pp-number E sign
-- pp-number .
-- eliminating left recursion leads to:
-- pp-number  -> { digit | . digit } pp-number'
-- pp-number' -> { digit | identifier-nondigit | { e|E } sign | . } pp-number' | epsilon

-- identifier:
-- identifier-nondigit
-- identifier identifier-nondigit
-- identifier digit

-- identifier-nondigit:
-- nondigit
-- universal-character-name
-- other implementation-defined characters

-- nondigit: one of
-- a b c d e f g h i j k l m
-- n o p q r s t u v w x y z
-- A B C D E F G H I J K L M
-- N O P Q R S T U V W X Y Z _

-- literal:
-- integer-literal
-- character-literal
-- floating-literal
-- string-literal
-- boolean-literal
-- pointer-literal
-- user-defined-literal

-- integer-literal:
-- decimal-literal integer-suffixopt
-- octal-literal integer-suffixopt
-- hexadecimal-literal integer-suffixopt

-- decimal-literal:
-- nonzero-digit
-- decimal-literal digit

-- octal-literal:
-- 0 octal-literal octal-digit

-- hexadecimal-literal:
-- 0x hexadecimal-digit
-- 0X hexadecimal-digit
-- hexadecimal-literal hexadecimal-digit

-- nonzero-digit: one of
-- 1 2 3 4 5 6 7 8 9

-- octal-digit: one of
-- 0 1 2 3 4 5 6 7

-- hexadecimal-digit: one of
-- 0 1 2 3 4 5 6 7 8 9
-- a b c d e f
-- A B C D E F

-- integer-suffix:
-- unsigned-suffix long-suffixopt
-- unsigned-suffix long-long-suffixopt
-- long-suffix unsigned-suffixopt
-- long-long-suffix unsigned-suffixopt

-- unsigned-suffix: one of
-- u U

-- long-suffix: one of
-- l L

-- long-long-suffix: one of
-- ll LL


-- character-literal:
-- ’ c-char-sequence ’
-- u’ c-char-sequence ’
-- U’ c-char-sequence ’
-- L’ c-char-sequence ’

-- c-char-sequence:
-- c-char
-- c-char-sequence c-char

-- c-char:
-- any member of the source character set except
-- the single-quote ’, backslash \, or new-line character
-- escape-sequence
-- universal-character-name

-- escape-sequence:
-- simple-escape-sequence
-- octal-escape-sequence
-- hexadecimal-escape-sequence

-- simple-escape-sequence: one of
-- \’ \" \? \\
-- \a \b \f \n \r \t \v

-- octal-escape-sequence:
-- \ octal-digit
-- \ octal-digit octal-digit
-- \ octal-digit octal-digit octal-digit

-- hexadecimal-escape-sequence:
-- \x hexadecimal-digit
-- hexadecimal-escape-sequence hexadecimal-digit

-- floating-literal:
-- fractional-constant exponent-partopt floating-suffixopt
-- digit-sequence exponent-part floating-suffixopt

-- fractional-constant:
-- digit-sequenceopt . digit-sequence
-- digit-sequence .

-- exponent-part:
-- e signopt digit-sequence
-- E signopt digit-sequence

-- sign: one of
-- + -

-- digit-sequence:
-- digit
-- digit-sequence digit

-- floating-suffix: one of
-- f l F L

-- string-literal:
-- encoding-prefixopt " s-char-sequenceopt "
-- encoding-prefixopt R raw-string

-- encoding-prefix:
-- u8
-- uUL



-- s-char-sequence:
-- s-char
-- s-char-sequence s-char

-- s-char:
-- any member of the source character set except
-- the double-quote ", backslash \, or new-line character
-- escape-sequence
-- universal-character-name

-- raw-string:
-- " d-char-sequenceopt ( r-char-sequenceopt ) d-char-sequenceopt "

-- r-char-sequence:
-- r-char
-- r-char-sequence r-char

-- r-char:
-- any member of the source character set, except
-- a right parenthesis ) followed by the initial d-char-sequence
-- (which may be empty) followed by a double quote ".

-- d-char-sequence:
-- d-char
-- d-char-sequence d-char

-- d-char:

-- any member of the basic source character set except:
-- space, the left parenthesis (, the right parenthesis ), the backslash \,
-- and the control characters representing horizontal tab,
-- vertical tab, form feed, and newline.

-- boolean-literal:
-- false
-- true

-- pointer-literal:
-- nullptr

-- user-defined-literal:
-- user-defined-integer-literal
-- user-defined-floating-literal
-- user-defined-string-literal
-- user-defined-character-literal

-- user-defined-integer-literal:
-- decimal-literal ud-suffix
-- octal-literal ud-suffix
-- hexadecimal-literal ud-suffix

-- user-defined-floating-literal:
-- fractional-constant exponent-partopt ud-suffix
-- digit-sequence exponent-part ud-suffix

-- user-defined-string-literal:
-- string-literal ud-suffix

-- user-defined-character-literal:
-- character-literal ud-suffix

-- ud-suffix:
-- identifier

-- primary-expression:
-- literal
-- this
-- ( expression )
-- id-expression
-- lambda-expression

-- qualified-id:

-- ::opt nested-name-specifier templateopt unqualified-id

-- :: identifier

-- :: operator-function-id

-- :: literal-operator-id

-- :: template-id

-- nested-name-specifier:

-- type-name ::

-- namespace-name ::

-- decltype-specifier ::

-- nested-name-specifier identifier ::

-- nested-name-specifier templateopt simple-template-id ::

-- lambda-expression:
-- lambda-introducer lambda-declaratoropt compound-statement

-- lambda-introducer:
-- [ lambda-captureopt ]

-- lambda-capture:
-- capture-default
-- capture-list
-- capture-default , capture-list

-- capture-default:
-- &=

-- capture-list:
-- capture ...opt
-- capture-list , capture ...opt


-- capture:
-- identifier
-- & identifier
-- this

-- lambda-declarator:
-- ( parameter-declaration-clause ) attribute-specifieropt mutableopt
-- exception-specificationopt trailing-return-typeopt

-- expression-list:
-- initializer-list

-- pseudo-destructor-name:

-- ::opt nested-name-specifieropt type-name ::[CAN] type-name

-- ::opt nested-name-specifier template simple-template-id ::[CAN] type-name

-- ::opt nested-name-specifieropt [CAN] type-name
-- [CAN] decltype-specifier

-- new-expression:

-- ::opt new new-placementopt new-type-id new-initializeropt

-- ::opt new new-placementopt ( type-id ) new-initializeropt


-- new-placement:
-- ( expression-list )

-- new-type-id:
-- type-specifier-seq new-declaratoropt

-- new-declarator:
-- ptr-operator new-declaratoropt
-- noptr-new-declarator

-- noptr-new-declarator:
-- [ expression ] attribute-specifieropt
-- noptr-new-declarator [ constant-expression ] attribute-specifieropt

-- new-initializer:
-- ( expression-listopt )
-- braced-init-list

-- delete-expression:

-- ::opt delete cast-expression

-- ::opt delete [ ] cast-expression

-- noexcept-expression:
-- noexcept ( expression )

-- labeled-statement:

-- attribute-specifieropt identifier : statement

-- attribute-specifieropt case constant-expression : statement

-- attribute-specifieropt default : statement

-- expression-statement:
-- expressionopt ;


-- selection-statement:
-- if ( condition ) statement
-- if ( condition ) statement else statement
-- switch ( condition ) statement

-- condition:
-- expression
-- attribute-specifieropt type-specifier-seq declarator = initializer-clause
-- attribute-specifieropt type-specifier-seq declarator braced-init-list

-- iteration-statement:
-- while ( condition ) statement
-- do statement while ( expression ) ;
-- for ( for-init-statement conditionopt ; expressionopt ) statement

-- for ( for-range-declaration : expression ) statement

-- for-init-statement:
-- expression-statement
-- simple-declaration

-- for-range-declaration:
-- attribute-specifieropt type-specifier-seq declarator

-- declaration-statement:
-- block-declaration
-- A.6 Declarations [gram.dcl]

-- block-declaration:
-- simple-declaration
-- asm-definition
-- namespace-alias-definition
-- using-declaration
-- using-directive
-- static_assert-declaration
-- alias-declaration
-- opaque-enum-declaration

-- alias-declaration:
-- using identifier = type-id ;alias_declaration = AliasDecl <$> string "using" *> identifier <*> ( string "=" *> type_id <* string ";" )

-- simple-declaration:
-- attribute-specifieropt decl-specifier-seqopt init-declarator-listopt ;

-- static_assert-declaration:
-- static_assert ( constant-expression , string-literal ) ;

-- empty-declaration:
-- ;

-- attribute-declaration:
-- attribute-specifier ;

-- decl-specifier-seq:
-- decl-specifier attribute-specifieropt
-- decl-specifier decl-specifier-seq

-- storage-class-specifier:
-- register
-- static
-- thread_local
-- extern
-- mutable

-- function-specifier:
-- inline
-- virtual
-- explicit

-- typedef-name:
-- identifier

-- trailing-type-specifier-seq:
-- trailing-type-specifier attribute-specifieropt
-- trailing-type-specifier trailing-type-specifier-seq


-- type-name:
-- class-name
-- enum-name
-- typedef-name

-- decltype-specifier:
-- decltype ( expression )

-- elaborated-type-specifier:

-- class-key attribute-specifieropt ::opt nested-name-specifieropt identifier

-- class-key ::opt nested-name-specifieropt templateopt simple-template-id

-- enum ::opt nested-name-specifieropt identifier

-- enum-name:
-- identifier

-- enum-specifier:
-- enum-head { enumerator-listopt }
-- enum-head { enumerator-list , }

-- enum-head:
-- enum-key attribute-specifieropt identifieropt enum-baseopt
-- enum-key attribute-specifieropt nested-name-specifier identifier
-- enum-baseopt

-- opaque-enum-declaration:
-- enum-key attribute-specifieropt identifier enum-baseopt ;

-- enum-key:
-- enum
-- enum class
-- enum struct

-- enum-base:

-- : type-specifier-seq

-- enumerator-list:
-- enumerator-definition
-- enumerator-list , enumerator-definition

-- enumerator-definition:
-- enumerator
-- enumerator = constant-expression


-- enumerator:
-- identifier

-- namespace-name:
-- original-namespace-name
-- namespace-alias

-- original-namespace-name:
-- identifier

-- namespace-definition:
-- named-namespace-definition
-- unnamed-namespace-definition

-- named-namespace-definition:
-- original-namespace-definition
-- extension-namespace-definition

-- original-namespace-definition:
-- inlineopt namespace identifier { namespace-body }

-- extension-namespace-definition:
-- inlineopt namespace original-namespace-name { namespace-body }

-- unnamed-namespace-definition:
-- inlineopt namespace { namespace-body }

-- namespace-body:
-- declaration-seqopt

-- namespace-alias:
-- identifier

-- namespace-alias-definition:
-- namespace identifier = qualified-namespace-specifier ;

-- qualified-namespace-specifier:

-- ::opt nested-name-specifieropt namespace-name

-- using-declaration:

-- using typenameopt ::opt nested-name-specifier unqualified-id ;

-- using :: unqualified-id ;

-- using-directive:

-- attribute-specifieropt using namespace ::opt nested-name-specifieropt namespace-name ;

-- asm-definition:
-- asm ( string-literal ) ;

-- linkage-specification:
-- extern string-literal { declaration-seqopt }
-- extern string-literal declaration

-- attribute-specifier:
-- [ [ attribute-list ] ]

-- attribute-list:
-- attributeopt
-- attribute-list , attributeopt
-- attribute ...
-- attribute-list , attribute ...

-- attribute:
-- attribute-token attribute-argument-clauseopt

-- attribute-token:
-- identifier
-- attribute-scoped-token


-- attribute-scoped-token:

-- attribute-namespace :: identifier

-- attribute-namespace:
-- identifier

-- attribute-argument-clause:
-- ( balanced-token-seq )

-- balanced-token-seq:
-- balanced-token
-- balanced-token-seq balanced-token

-- balanced-token:
-- ( balanced-token-seq )
-- [ balanced-token-seq ]
-- { balanced-token-seq }
-- any token other than a parenthesis, a bracket, or a brace
-- A.7 Declarators [gram.decl]

-- init-declarator-list:
-- init-declarator
-- init-declarator-list , init-declarator

-- init-declarator:
-- declarator initializeropt

-- ptr-operator:
-- * attribute-specifieropt cv-qualifier-seqopt
-- & attribute-specifieropt
-- && attribute-specifieropt

-- ::opt nested-name-specifier * attribute-specifieropt cv-qualifier-seqopt

-- cv-qualifier-seq:
-- cv-qualifier cv-qualifier-seqopt

-- cv-qualifier:
-- const
-- volatile

-- ref-qualifier:
-- &
-- &&

-- abstract-declarator:
-- ptr-abstract-declarator
-- noptr-abstract-declaratoropt parameters-and-qualifiers trailing-return-type
-- ...

-- ptr-abstract-declarator:
-- noptr-abstract-declarator
-- ptr-operator ptr-abstract-declaratoropt

-- noptr-abstract-declarator:
-- noptr-abstract-declaratoropt parameters-and-qualifiers
-- noptr-abstract-declaratoropt [ constant-expression ] attribute-specifieropt
-- ( ptr-abstract-declarator )

-- initializer:
-- brace-or-equal-initializer
-- ( expression-list )

-- brace-or-equal-initializer:
-- = initializer-clause
-- braced-init-list

-- initializer-clause:
-- assignment-expression
-- braced-init-list

-- initializer-list:
-- initializer-clause ...opt
-- initializer-list , initializer-clause ...opt

-- braced-init-list:
-- { initializer-list ,opt }
-- { }


-- class-name:
-- identifier
-- simple-template-id

-- class-specifier:
-- class-head { member-specificationopt }

-- class-head:
-- class-key attribute-specifieropt identifieropt base-clauseopt
-- class-key attribute-specifieropt nested-name-specifier identifier base-clauseopt
-- class-key attribute-specifieropt nested-name-specifieropt simple-template-id base-clauseopt

-- class-key:
-- class
-- struct
-- union

-- member-specification:
-- member-declaration member-specificationopt

-- access-specifier : member-specificationopt

-- member-declaration:
-- attribute-specifieropt decl-specifier-seqopt
-- member-declarator-listopt ;
-- function-definition ;opt

-- ::opt nested-name-specifier templateopt unqualified-id ;
-- using-declaration
-- static_assert-declaration
-- template-declaration

-- member-declarator-list:
-- member-declarator
-- member-declarator-list , member-declarator

-- member-declarator:
-- declarator pure-specifieropt
-- declarator brace-or-equal-initializeropt

-- identifieropt attribute-specifieropt : constant-expression

-- pure-specifier:
-- = 0


-- base-clause:

-- : base-specifier-list

-- base-specifier-list:
-- base-specifier ...opt
-- base-specifier-list , base-specifier ...opt

-- base-specifier:
-- attribute-specifieropt base-type-specifier
-- attribute-specifieropt virtual access-specifieropt base-type-specifier
-- attribute-specifieropt access-specifier virtualopt base-type-specifier

-- class-or-decltype:

-- ::opt nested-name-specifieropt class-name
-- decltype-specifier


-- base-type-specifier:
-- class-or-decltype

-- access-specifier:
-- private
-- protected
-- public


-- conversion-function-id:
-- operator conversion-type-id

-- conversion-type-id:
-- type-specifier-seq conversion-declaratoropt

-- conversion-declarator:
-- ptr-operator conversion-declaratoropt

-- ctor-initializer:

-- : mem-initializer-list

-- mem-initializer-list:
-- mem-initializer ...opt
-- mem-initializer , mem-initializer-list ...opt

-- mem-initializer:
-- mem-initializer-id ( expression-listopt )
-- mem-initializer-id braced-init-list

-- mem-initializer-id:
-- class-or-decltype
-- identifier


-- operator-function-id:
-- operator operator

-- operator: one of
-- new delete new[] delete[]
-- + - * / % ˆ & | [CAN]
-- ! = < > += -= *= /= %=
-- ˆ= &= |= << >> >>= <<= == !=
-- <= >= && || ++ -- , ->* ->
-- ( ) [ ]

-- literal-operator-id:
-- operator "" identifier
-- A.12 Templates [gram.temp]

-- template-declaration:
-- template < template-parameter-list > declaration

-- template-parameter-list:
-- template-parameter
-- template-parameter-list , template-parameter

-- template-parameter:
-- type-parameter
-- parameter-declaration


-- type-parameter:
-- class ...opt identifieropt
-- class identifieropt = type-id
-- typename ...opt identifieropt
-- typename identifieropt = type-id
-- template < template-parameter-list > class ...opt identifieropt
-- template < template-parameter-list > class identifieropt = id-expression

-- simple-template-id:
-- template-name < template-argument-listopt >

-- template-id:
-- simple-template-id
-- operator-function-id < template-argument-listopt >
-- literal-operator-id < template-argument-listopt >

-- template-name:
-- identifier

-- template-argument-list:
-- template-argument ...opt
-- template-argument-list , template-argument ...opt

-- template-argument:
-- constant-expression
-- type-id
-- id-expression

-- typename-specifier:

-- typename ::opt nested-name-specifier identifier

-- typename ::opt nested-name-specifier templateopt simple-template-id

-- explicit-instantiation:
-- externopt template declaration

-- explicit-specialization:
-- template < > declaration


-- try-block:
-- try compound-statement handler-seq

-- function-try-block:
-- try ctor-initializeropt compound-statement handler-seq

-- handler-seq:
-- handler handler-seqopt

-- handler:
-- catch ( exception-declaration ) compound-statement

-- exception-declaration:
-- attribute-specifieropt type-specifier-seq declarator
-- attribute-specifieropt type-specifier-seq abstract-declaratoropt
-- ...

-- throw-expression:
-- throw assignment-expressionopt

-- exception-specification:
-- dynamic-exception-specification
-- noexcept-specification


-- dynamic-exception-specification:
-- throw ( type-id-listopt )

-- type-id-list:
-- type-id ...opt
-- type-id-list , type-id ...opt

-- noexcept-specification:
-- noexcept ( constant-expression )
-- noexcept


-- preprocessing-file:
-- groupopt

-- group:
-- group-part
-- group group-part

-- group-part:
-- if-section
-- control-line
-- text-line
-- # non-directive

-- if-section:
-- if-group elif-groupsopt else-groupopt endif-line

-- if-group:
-- # if constant-expression new-line groupopt
-- # ifdef identifier new-line groupopt
-- # ifndef identifier new-line groupopt

-- elif-groups:
-- elif-group
-- elif-groups elif-group

-- elif-group:
-- # elif constant-expression new-line groupopt

-- else-group:
-- # else new-line groupopt

-- endif-line:
-- # endif new-line

-- control-line:
-- # include pp-tokens new-line
-- # define identifier replacement-list new-line
-- # define identifier lparen identifier-listopt ) replacement-list new-line
-- # define identifier lparen ... ) replacement-list new-line
-- # define identifier lparen identifier-list, ... ) replacement-list new-line
-- # undef identifier new-line
-- # line pp-tokens new-line
-- # error pp-tokensopt new-line
-- # pragma pp-tokensopt new-line
-- # new-line

-- text-line:
-- pp-tokensopt new-line


-- non-directive:
-- pp-tokens new-line

-- lparen:
-- a ( character not immediately preceded by white-space

-- identifier-list:
-- identifier
-- identifier-list , identifier

-- replacement-list:
-- pp-tokensopt

-- pp-tokens:
-- preprocessing-token
-- pp-tokens preprocessing-token

-- new-line:
-- the new-line character

-}