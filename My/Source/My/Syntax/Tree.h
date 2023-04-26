#pragma once

#include "My/Base/Core.h"
#include "My/Utils/Utils.h"

#include <functional>

// Forward Declaration
struct TypeSpec;
struct Expression;
struct Statement;
struct Declaration;


#pragma region Tokens
/// <summary>
/// Tokens
/// </summary>
enum class TokenKind
{
	Invalid = -1,
	Eof,
	// Literals
	Int64,
	Uint64,
	Float64,
	String,
	// Symbols
	Plus,
	Dash,
	Star,
	StarStar,
	Slash,
	Percent,
	Caret,
	Tilde,
	Bang,
	And,
	Pipe,
	AndAnd,
	PipePipe,
	Equals,
	BangEquals,
	EqualsEquals,
	Less,
	LessLess,
	LessEquals,
	Greater,
	GreaterGreater,
	GreaterEquals,
	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	Arrow,
	Dot,
	Question,
	Comma,
	Colon,
	Semicolon,
	// Identifiers & Keywords
	Identifier,
	NullKeyword,
	FalseKeyword,
	TrueKeyword,
	NewKeyword,
	VarKeyword,
	ConstKeyword,
	AutoKeyword,
	StaticKeyword,
	InlineKeyword,
	ConstexprKeyword,
	NoGCKeyword,
	CallbackKeyword,
	PodKeyword,

	IfKeyword,
	ElseKeyword,
	ElseifKeyword,
	ForKeyword,
	ForeachKeyword,
	DoKeyword,
	WhileKeyword,
	BreakKeyword,
	ContinueKeyword,
	ReturnKeyword,
	ImportKeyword,
	UsingKeyword,
	ExternKeyword,
	EnumKeyword,
	BflagsKeyword,
	FunctionKeyword,
	StructKeyword
};

struct Token
{
	TokenKind   Kind  = TokenKind::Invalid;
	int32_t     Start = -1;
	int32_t     End   = -1;

	union
	{
		bool        B08;
		int64_t     I64;
		uint64_t    U64;
		double      F64;
		MyString*  Str;
		char*       Id;
	};

	Token();
	Token(TokenKind Kind);
	Token(const Token& Other);
	~Token() { }
	Token& operator=(const Token& Other);

	bool         IsOperator() const noexcept;
	TextLocation Location(uint32_t kLine, char* const lpFilename) const noexcept;
};
#pragma endregion

#pragma region TypeSpecifiers
/// <summary>
/// Type Specifiers
/// </summary>
enum class TypeSpecKind
{
	Invalid = -1,
	Name,
	Array,
	Function,
};

struct NameTypeSpec
{
	Token Name = {};
};

struct ArrayTypeSpec
{
	TypeSpec*    Type          = nullptr;
	Token        LbracketToken = {};
	Expression** Counts        = nullptr;
	Token        RbracketToken = {};
};

struct FunctionTypeSpec
{
	Token      CallbackKeyword = {};
	Token      LparenToken     = {};
	TypeSpec*  Return          = nullptr;
	TypeSpec** Parameters      = nullptr;
	Token      RparenToken     = {};
};

struct TypeSpec
{
	TypeSpecKind Kind = TypeSpecKind::Invalid;

	union
	{
		NameTypeSpec     name;
		ArrayTypeSpec    array;
		FunctionTypeSpec func;
	};

	TypeSpec();
	TypeSpec(TypeSpecKind Kind);
	~TypeSpec() noexcept { }
};
#pragma endregion

#pragma region Expressions
/// <summary>
/// Expressions
/// </summary>
enum class ExpressionKind
{
	Invalid = -1,
	Literal,
	Unary,
	Binary,
	Ternary,
	Parenthesized,
	Name,
	Assignment,
	OperatorNew,
	Cast,
	Call,
	Index,
	Field,
	Array,
};


struct LiteralExpression
{
	Token Literal = {};
};

struct UnaryExpression
{
	Token       Operator = {};
	Expression* Rhs      = nullptr;
};

struct BinaryExpression
{
	Expression* Lhs      = nullptr;
	Token       Operator = {};
	Expression* Rhs      = nullptr;
};

struct TernaryExpression
{
	Expression* Condition     = nullptr;
	Token       QuestionToken = {};
	Expression* Then          = nullptr;
	Token       ColonToken    = {};
	Expression* Else          = nullptr;
};

struct ParenthesizedExpression
{
	Token       LParenToken = {};
	Expression* Expr        = nullptr;
	Token       RParenToken = {};
};

struct NameExpression
{
	Token Identifier;
};

struct AssignmentExpression
{
	Expression* Lhs         = nullptr;
	Token       EqualsToken = {};
	Expression* Rhs         = nullptr;
};

struct FieldInitializer
{
	Token       Name       = {};
	Token       ColonToken = {};
	Expression* Value      = nullptr;
};

struct OperatorNewExpression
{
	Token       NewKeyword  = {};
	TypeSpec*   Type        = nullptr;
	Token       LparenToken = {};
	union
	{
		Expression*       Initializer; // In case of a single expression
		FieldInitializer* Fields;      // In case of struct member initializers
	};
	Token       RparenToken = {};
	bool        HasFieldInitializers = false;
};

struct CastExpression
{
	Token       LparenToken = {};
	TypeSpec*   Type        = nullptr;
	Token       RparenToken = {};
	Expression* Expr        = nullptr;
};

struct CallExpression
{
	Expression*  Callable    = nullptr;
	Token        LparenToken = { };
	Expression** Arguments   = nullptr;
	Token        RparenToken = { };
};

struct IndexExpression
{
	Expression*  Sequence      = nullptr;
	Token        LBracketToken = {};
	Expression** Indices       = nullptr;
	Token        RBracketToken = {};
};

struct FieldExpression
{
	Expression* Object   = nullptr;
	Token	    DotToken = {};
	Token	    Field    = {};
};

struct ArrayExpression
{
	Token        LbraceToken = {};
	Expression** Items       = nullptr;
	Token        RbraceToken = {};
};

struct Expression
{
	ExpressionKind Kind = ExpressionKind::Invalid;

	union
	{
		LiteralExpression       literal;
		UnaryExpression         unary;
		BinaryExpression        binary;
		TernaryExpression       ternary;
		ParenthesizedExpression paren;
		NameExpression          name;
		AssignmentExpression    assign;
		OperatorNewExpression   opnew;
		CastExpression          cast;
		CallExpression          call;
		IndexExpression         index;
		FieldExpression         field;
		ArrayExpression         array;
	};

	Expression();
	Expression(ExpressionKind Kind);
	~Expression() noexcept { };
};
#pragma endregion

#pragma region Statements
/// <summary>
/// Statements
/// </summary>
enum class StatementKind
{
	Invalid = -1,
	Block,
	Expression,
	VariableDeclaration,
	DecomposeDeclaration,
	If,
	For,
	Foreach,
	While,
	DoWhile,
	Break,
	Continue,
	Return
};

struct BlockStatement
{
	Token       LbraceToken = {};
	Statement** Stmts       = nullptr;
	Token       RbraceToken = {};
};

struct ExpressionStatement
{
	Expression* Expr = nullptr;
};

struct VariableDeclarationStatement
{
	Token       VarKeyword  = {}; // or const
	TypeSpec*   Type        = nullptr;
	Token       Identifier  = {};
	Token       EqualsToken = {};
	Expression* Value       = nullptr;
	bool        IsReadonly  = false;
};

struct DecomposeDeclarationStatement
{
	Token       AutoKeyword   = {};
	Token       LbracketToken = {};
	Token*      Identifiers   = nullptr;
	Token       RbracketToken = {};
	Token       EqualsToken   = {};
	Expression* Decomposable  = nullptr;
};

struct IfStatement
{
	struct ElseIfStatement
	{
		Token       ElseifKeyword = {};
		Token       LparenToken   = {};
		Expression* Condition     = nullptr;
		Token       RparenToken   = {};
		Statement*  ElseifBlock   = nullptr;
	};

	Token        IfKeyword    = {};
	Token        LparenToken  = {};
	Expression*  Condition    = nullptr;
	Token        RparenToken  = {};
	Statement*   IfBlock      = nullptr;
	
	ElseIfStatement* ElseIfs      = nullptr;

	Token        ElseKeyword  = {};
	Statement*   ElseBlock    = nullptr;
};

struct ForStatement
{
	Token       ForKeyword  = {}; 
	Token       LparenToken = {};
	Token       Identifier  = {};
	Expression* LowerBound  = nullptr;
	Expression* UpperBound  = nullptr;
	Expression* Step        = nullptr;
	Token       RparenToken = {};
	Statement*  Body        = nullptr;
};

struct ForeachStatement
{
	Token       ForeachKeyword = {};
	Token       LparenToken    = {};
	Token       Identifier     = {};
	Token       ColonToken     = {};
	Expression* Iterable       = nullptr;
	Token       RparenToken    = {};
	Statement*  Body           = nullptr;
};

struct WhileStatement
{
	Token       WhileKeyword = {};
	Token       LparenToken  = {};
	Expression* Condition    = nullptr;
	Token       RparenToken  = {};
	Statement*  Body         = nullptr;
};

struct DoWhileStatement
{
	Token       DoKeyword    = {};
	Statement*  Body         = nullptr;
	Token       WhileKeyword = {};
	Token       LparenToken  = {};
	Expression* Condition    = nullptr;
	Token       RparenToken  = {};
};

struct BreakStatement
{
	Token BreakKeyword = {};
};

struct ContinueStatement
{
	Token ContinueKeyword = {};
};

struct ReturnStatement
{
	Token        ReturnKeyword = {};
	Expression*  Expr          = nullptr;
};

struct Statement
{
	StatementKind Kind = StatementKind::Invalid;

	union
	{
		BlockStatement                block;
		ExpressionStatement           expr;
		VariableDeclarationStatement  var;
		DecomposeDeclarationStatement decomp;
		IfStatement                   ifstmt;
		ForStatement                  forstmt;
		ForeachStatement              foreach;
		WhileStatement                whilestmt;
		DoWhileStatement              dowhile;
		BreakStatement                breakstmt;
		ContinueStatement             continuestmt;
		ReturnStatement               returnstmt;
	};

	Statement();
	Statement(StatementKind Kind);
	~Statement() noexcept { }
};
#pragma endregion

#pragma region Declarations
/// <summary>
/// Declarations
/// </summary>
enum class DeclarationKind
{
	Invalid = -1,
	Import,
	Using,
	Extern,
	Enum,
	BitFlags,
	Variable,
	Function,
	Forward,
	Struct,
};

struct Parameter
{
	Token*    ConstKeyword = nullptr; // Optional
	TypeSpec* Type         = nullptr;
	Token	  Name         = {};
	// Token       EqualsToken = {};
	// Expression* Default     = nullptr;

	Parameter() = default;
	Parameter(TypeSpec* pType, const Token& Name, Token* pConstexprKeyword = nullptr);
};

struct FunctionSignature
{
	Token*     InlineKeyword = nullptr; // Optional
	Token*     StaticKeyword = nullptr; // Optional
	TypeSpec*  Return         = nullptr;
	Token      Name          = {};
	Token      LparenToken   = {};
	Parameter* Params        = nullptr;
	Token      RparenToken   = {};
	Token*     NoGCKeyword   = nullptr; // Optional
	uint32_t   Attributes    = 0u;
};

struct ImportDeclaration
{
	Token ImportKeyword = {};
	Token Filepath      = {};
};

struct UsingDeclaration
{
	Token     UsingKeyword = {};
	Token     Name         = {};
	Token     EqualsToken  = {};
	TypeSpec* Type         = nullptr;
};

struct ExternDeclaration
{
	Token             ExternKeyword   = {};
	Token             FunctionKeyword = {};
	FunctionSignature Signature       = {};
};

struct EnumDeclaration
{
	Token  EnumKeyword = {};
	Token  Name        = {};
	Token  LbraceToken = {};
	Token* Values      = nullptr;
	Token  RbraceToken = {};
};

struct BitFlagsDeclaration
{
	Token       BflagsKeyword = {};
	Token       LparenToken   = {};
	Expression* Base          = nullptr;
	Token       RparenToken   = {};
	Token       Name          = {};
	Token       LbraceToken   = {};
	Token*      Values        = nullptr;
	Token       RbraceToken   = {};
};

using VariableDeclaration = VariableDeclarationStatement;

struct FunctionDeclaration
{
	Token             FunctionKeyword = {};
	FunctionSignature Signature       = {};
	Statement*        Body            = nullptr;
	uint32_t          Attributes      = 0u;
};

struct ForwardDeclaration
{
	Token StructKeyword = {};
	Token Name          = {};
};

struct StructDeclaration
{
	Token         StructKeyword  = {};
	Token*        PodKeyword     = {}; // Optional
	Token         Name           = {};
	Token         LbraceToken    = {};
	Statement**   Members        = nullptr;
	Declaration** Methods        = nullptr;
	Token         RbraceToken    = {};
	uint32_t      Attributes     = 0u;
};

struct Declaration
{
	DeclarationKind Kind = DeclarationKind::Invalid;

	union
	{
		ImportDeclaration   importdecl;
		UsingDeclaration    usingdecl;
		ExternDeclaration   externdecl;
		EnumDeclaration     enumdecl;
		BitFlagsDeclaration bflagsdecl;
		VariableDeclaration vardecl;
		FunctionDeclaration funcdecl;
		ForwardDeclaration  forward;
		StructDeclaration   structdecl;
	};

	Declaration();
	Declaration(DeclarationKind Kind);
	~Declaration() noexcept { }
};
#pragma endregion

/// <summary>
/// Tree representation of the source text
/// </summary>
struct ParseResult
{
	Declaration** Decls    = nullptr;
	Token         EofToken = {};
};

class SyntaxTree
{
public:
	using ParseHandler = std::function<void(MyContext*, SyntaxTree*, ParseResult&, DiagnosticBag&)>;

public:
	static SyntaxTree* Load(MyContext* pContext, const std::string_view& Filename);
	static SyntaxTree* Parse(MyContext* pContext, const std::string_view& Text);

	DiagnosticBag&       GetDiagnostics() noexcept       { return m_Diagnostics; }
	ParseResult&         GetRoot()        noexcept       { return m_Result; }
	SourceText&          GetText()        noexcept       { return m_Text; }
	const DiagnosticBag& GetDiagnostics() const noexcept { return m_Diagnostics; }
	const ParseResult&   GetRoot()		  const noexcept { return m_Result; }
	const SourceText&    GetText()		  const noexcept { return m_Text; }

private:
	static SyntaxTree* Parse(MyContext* pContext, const SourceText& Text);
	static void		   ParseInternal(MyContext* pContext, SyntaxTree* pTree, ParseResult& Root, DiagnosticBag& Diagnostics);
	static Token*      ParseFlattened(MyContext* pContext, const std::string& Text);
	static Token*      ParseFlattened(MyContext* pContext, const SourceText& Text);

private:
	explicit SyntaxTree(MyContext* pContext, const SourceText& Text, const ParseHandler& pfnHandler);

private:
	ParseResult   m_Result      = {};
	SourceText    m_Text;
	DiagnosticBag m_Diagnostics = {};
};


const char* TokenKindString(TokenKind Kind) noexcept;
const char* TypeSpecKindString(TypeSpecKind Kind) noexcept;
const char* ExpressionKindString(ExpressionKind Kind) noexcept;
const char* StatementKindString(StatementKind Kind) noexcept;
const char* DeclarationKindString(DeclarationKind Kind) noexcept;

#ifdef MY_DEBUG
void PrettyPrint(const Token& Token, const std::string& Indent = "") noexcept;
void PrettyPrint(TypeSpec* pType, const std::string& Indent = "") noexcept;
void PrettyPrint(Expression* pExpr, const std::string& Indent = "") noexcept;
void PrettyPrint(Statement* pStmy, const std::string& Indent = "") noexcept;
void PrettyPrint(Declaration* pDecl, const std::string& Indent = "") noexcept;
#endif // MY_DEBUG
