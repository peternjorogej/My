#pragma once

#include "My/Base/Core.h"
#include "My/Object.h"
#include "My/Syntax/Tree.h"

// Forward Declaration
struct MySymbol;
struct BoundExpression;
struct BoundStatement;

#pragma region Symbols
enum class SymbolKind
{
	Invalid = -1,
	GlobalVariable,
	LocalVariable,
	Parameter,
	Function,
	Struct,
};

struct VariableSymbol
{
	MyType* Type       = nullptr;
	bool	IsReadonly = false;
	bool	IsLocal    = true;
};

using ParameterSymbol = VariableSymbol;

struct FunctionSymbol
{
	MyType*      Type     = nullptr;
	MySymbol**   Parameters = nullptr;
	Declaration* Decl       = nullptr;
	bool         IsInline   = false;
	bool         IsStatic   = false;
	bool         IsNoGC     = false;
};

struct FieldSymbol
{
	char*            Name    = nullptr;
	MyType*          Type    = nullptr;
	BoundExpression* Default = nullptr;
};

struct StructSymbol
{
	MyStruct*    Klass  = nullptr;
	FieldSymbol* Fields = nullptr;
	Declaration* Decl   = nullptr;
};

struct MySymbol
{
	SymbolKind Kind = SymbolKind::Invalid;
	char*	   Name = nullptr;
	MyType*    Type = nullptr;

	union
	{
		VariableSymbol  varsym;
		ParameterSymbol paramsym;
		FunctionSymbol  funcsym;
		StructSymbol	structsym;
	};

	MySymbol();
	~MySymbol() noexcept { }

	MySymbol(SymbolKind Kind, char* const lpName, MyType* pType);
};
#pragma endregion

#pragma region BoundExpressions
/// <summary>
/// Expressions
/// </summary>
enum class BoundExpressionKind
{
	Invalid = -1,
	Error,
	Empty, // Used by compile time functions to signify nothing happened
	Literal,
	Unary,
	Binary,
	Ternary,
	Increment,
	Parenthesized,
	Name,
	Assignment,
	OperatorNew,
	Call,
	Index,
	Field,
	Array,
	Instance,

	Conversion
};

enum class BoundUnaryOperatorKind
{
	Invalid = -1,
	Identity,
	Negation,
	LogicalNegation,
	BitwiseNegation
};

enum class BoundBinaryOperatorKind
{
	Invalid = -1,
	Addition,
	Subtraction,
	Multiplication,
	Division,
	Exponentiation,
	Modulo,
	LogicalAnd,
	LogicalOr,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	LeftShift,
	RightShift,
	Equality,
	NonEquality,
	Less,
	LessOrEqual,
	Greater,
	GreaterOrEqual
};

struct BoundUnaryOperator
{
	TokenKind              Kind         = TokenKind::Invalid;
	BoundUnaryOperatorKind OperatorKind = BoundUnaryOperatorKind::Invalid;
	MyType*                RhsType      = nullptr;
	MyType*                ResultType   = nullptr;

	BoundUnaryOperator(TokenKind Kind, BoundUnaryOperatorKind OperatorKind, MyType* pRhsType, MyType* pResultType);
	BoundUnaryOperator(TokenKind Kind, BoundUnaryOperatorKind OperatorKind, MyType* pRhsType);

	static BoundUnaryOperator* Bind(TokenKind Kind, MyType* pRhsType) noexcept;
};

struct BoundBinaryOperator
{
	TokenKind               Kind         = TokenKind::Invalid;
	BoundBinaryOperatorKind OperatorKind = BoundBinaryOperatorKind::Invalid;
	MyType*                 LhsType      = nullptr;
	MyType*                 RhsType      = nullptr;
	MyType*                 ResultType   = nullptr;

	BoundBinaryOperator(
		TokenKind Kind, BoundBinaryOperatorKind OperatorKind, MyType* pLhsType, MyType* pRhsType, MyType* pResultType
	);
	BoundBinaryOperator(TokenKind Kind, BoundBinaryOperatorKind OperatorKind, MyType* pLhsType);
	BoundBinaryOperator(TokenKind Kind, BoundBinaryOperatorKind OperatorKind, MyType* pOperandType, MyType* pResultType);

	static BoundBinaryOperator* Bind(TokenKind Kind, MyType* pLhsType, MyType* pRhsType) noexcept;
};


struct BoundLiteralExpression
{
	MyType* Type  = nullptr;
	MyValue Value = {};
};

struct BoundUnaryExpression
{
	BoundUnaryOperator* Operator = nullptr;
	BoundExpression*    Rhs      = nullptr;
};

struct BoundBinaryExpression
{
	BoundExpression*     Lhs      = nullptr;
	BoundBinaryOperator* Operator = nullptr;
	BoundExpression*     Rhs      = nullptr;
};

struct BoundTernaryExpression
{
	BoundExpression* Condition = nullptr;
	BoundExpression* Then      = nullptr;
	BoundExpression* Else      = nullptr;
};

struct BoundIncrementExpression
{
	MySymbol*        Lvalue = nullptr;
	BoundExpression* Increment = nullptr;
};

struct BoundNameExpression
{
	MySymbol* Symbol = nullptr;
};

struct BoundAssignmentExpression
{
	BoundExpression* Lhs      = nullptr;
	MySymbol*        Variable = nullptr;
	BoundExpression* Rhs      = nullptr;
};

struct BoundOperatorNewExpression
{
	MyType*          Type = nullptr;
	BoundExpression* Expr = nullptr;
};

struct BoundCallExpression
{
	BoundExpression*  Callable  = nullptr;
	MySymbol*         Function  = nullptr;
	BoundExpression** Arguments = nullptr;
};

struct BoundIndexExpression
{
	BoundExpression*  Sequence = nullptr;
	MyType*           Type     = nullptr; // Of elements in sequence
	BoundExpression** Indices  = nullptr;
};

struct BoundFieldExpression
{
	BoundExpression* Object = nullptr;
	MyType*          Type   = nullptr; // Of field being accessed
	char* const      Field  = nullptr;
};

struct BoundArrayExpression
{
	MyType*           Type    = nullptr; // Of the array itself i.e T[...] *?
	BoundExpression*  Default = nullptr; // Used to fill an array that has been new-ed (new T[N]{})
	BoundExpression** Items   = nullptr;
};

struct BoundInstanceExpression
{
	using MemberMap = Pair<char*, BoundExpression*>; 
	using MethodMap = Pair<char*, MySymbol*>; 

	MyType*    Type    = nullptr;
	MemberMap* Members = nullptr;
	MethodMap* Methods = nullptr;
};

struct BoundConversionExpression
{
	MyType*          Type = nullptr;
	BoundExpression* Expr = nullptr;
};

struct BoundExpression
{
	BoundExpressionKind Kind = BoundExpressionKind::Invalid;

	union
	{
		BoundLiteralExpression      literal;
		BoundUnaryExpression        unary;
		BoundBinaryExpression       binary;
		BoundTernaryExpression      ternary;
		BoundIncrementExpression    inc;
		BoundNameExpression         name;
		BoundAssignmentExpression   assign;
		BoundOperatorNewExpression  opnew;
		BoundCallExpression         call;
		BoundIndexExpression        index;
		BoundFieldExpression        field;
		BoundArrayExpression        array;
		BoundInstanceExpression     inst;
		BoundConversionExpression   conv;
	};

	BoundExpression();
	~BoundExpression() noexcept { }

	BoundExpression(BoundExpressionKind Kind);

	MyType* Type() const noexcept;
};

#pragma endregion

#pragma region Statements
/// <summary>
/// Statements
/// </summary>
enum class BoundStatementKind
{
	Invalid = -1,
	Error,
	Block,
	Expression,
	VariableDeclaration,
	DecomposeDeclaration,
	If,
	For,
	Foreach,
	While,
	Return,

	Label,
	Goto,
	ConditionalGoto,

	Nop,
};

struct BoundLabel
{
	char* Label = nullptr;

	constexpr BoundLabel() = default;
	BoundLabel(char* const& lpLabel);

	bool operator==(const BoundLabel& Rhs) const noexcept;
	bool operator!=(const BoundLabel& Rhs) const noexcept;
};

struct BoundBlockStatement
{
	BoundStatement** Statements = nullptr;
};

struct BoundExpressionStatement
{
	BoundExpression* Expr = nullptr;
};

struct BoundVariableDeclarationStatement
{
	MySymbol*        Variable = nullptr;
	BoundExpression* Value    = nullptr;
};

struct BoundDecomposeDeclarationStatement
{
	MySymbol*        Struct       = nullptr;
	MySymbol**       Variables    = nullptr;
	BoundExpression* Decomposable = nullptr;
};

using ElseIfBlock = Pair<BoundExpression*, BoundStatement*>;
struct BoundIfStatement
{
	BoundExpression* IfCondition = nullptr;
	BoundStatement*  IfBlock     = nullptr;
	ElseIfBlock*     ElseIfs     = nullptr;
	BoundStatement*  ElseBlock   = nullptr;
};

struct BoundLoopStatement
{
	BoundStatement* Body          = nullptr;
	BoundLabel      BreakLabel    = {};
	BoundLabel      ContinueLabel = {};
};

struct BoundForStatement
{
	MySymbol*          Variable   = nullptr;
	BoundExpression*   LowerBound = nullptr;
	BoundExpression*   UpperBound = nullptr;
	BoundExpression*   Step       = nullptr;
	BoundLoopStatement Loop       = {};
};

struct BoundForeachStatement
{
	MySymbol*          Variable = nullptr;
	BoundExpression*   Iterable = nullptr;
	BoundLoopStatement Loop     = {};
};

struct BoundWhileStatement
{
	BoundExpression*   Condition = nullptr;
	BoundLoopStatement Loop      = {};
	bool		       IsDoWhile = false;
};

struct BoundReturnStatement
{
	BoundExpression* Expr = nullptr;
};

struct BoundLabelStatement
{
	BoundLabel Label = {};
};

struct BoundGotoStatement
{
	BoundLabel Label = {};
};

struct BoundConditionalGotoStatement
{
	BoundLabel		 Label      = {};
	BoundExpression* Condition  = nullptr;
	bool			 JumpIfTrue = true;
};

struct BoundStatement
{
	BoundStatementKind Kind = BoundStatementKind::Invalid;

	union
	{
		BoundBlockStatement                block;
		BoundExpressionStatement           expr;
		BoundVariableDeclarationStatement  vardecl;
		BoundDecomposeDeclarationStatement decomp;
		BoundIfStatement                   ifstmt;
		BoundForStatement                  forstmt;
		BoundForeachStatement              foreach;
		BoundWhileStatement                whilestmt;
		BoundReturnStatement               ret;
		BoundLabelStatement                label;
		BoundGotoStatement                 gotostmt;
		BoundConditionalGotoStatement      cgotostmt;
	};

	BoundStatement();
	~BoundStatement() { }

	BoundStatement(BoundStatementKind Kind);
};
#pragma endregion

struct BoundGlobalScope
{
	using GlobalMap = Pair<MySymbol*, BoundExpression*>;

	MySymbol**  Variables   = nullptr;
	MySymbol**  Functions   = nullptr;
	MySymbol**  Structs     = nullptr;
	GlobalMap*  Globals     = nullptr;

	DiagnosticBag Diagnostics = { };
};

struct BoundProgram
{
	using BoundVariable = Pair<MySymbol*, BoundExpression*>;
	using BoundFunction = Pair<MySymbol*, BoundStatement*>;
	using GlobalMap     = Pair<char*, BoundVariable>;
	using FunctionMap   = Pair<char*, BoundFunction>;
	using StructMap     = Pair<char*, MyType*>;

	BoundGlobalScope* GlobalScope    = nullptr;
	GlobalMap*        Globals        = nullptr;
	FunctionMap*      FunctionBodies = nullptr;
	StructMap*        StructTypes    = nullptr;

	DiagnosticBag	  Diagnostics    = {};
};

class Binder
{
public:
	static BoundGlobalScope BindGlobalScope(MyContext* pContext, const SyntaxTree* pTree) noexcept;
	static BoundProgram     BindProgram(MyContext* pContext, BoundGlobalScope* pGlobalScope, const SyntaxTree* pTree) noexcept;

	static BoundUnaryOperator* BindUnaryOperator(TokenKind OperatorKind, MyType* pRhsType);
	static BoundBinaryOperator* BindBinaryOperator(TokenKind OperatorKind, MyType* pLhsType, MyType* pRhsType);
};


namespace std
{

	template<>
	struct hash<::BoundLabel>
	{
		size_t operator()(const ::BoundLabel& Label) const noexcept
		{
			return MyHashBytes(Label.Label, strlen(Label.Label));
		}
	};

}


#pragma region Declarations_For_Creator_Function
/// Create Symbols
MySymbol* MakeSymbol_Variable(char* const lpName, MyType* pType, bool bIsReadonly, bool bIsLocal);
MySymbol* MakeSymbol_Parameter(char* const lpName, MyType* pType, bool bIsConstexpr, bool bIsLocal);
MySymbol* MakeSymbol_Function(
	char* const  lpName,
	MyType*      pType,
	MySymbol**   ppParamTypes,
	bool         bIsStatic = false,
	bool         bIsInline = false,
	bool         bIsNoGC   = false,
	Declaration* pDecl     = nullptr
);
MySymbol* MakeSymbol_Struct(
	char* const  lpName,
	MyType*      pType,
	FieldSymbol* pFields,
	Declaration* pDecl      = nullptr
);

/// Create Bound Expressions
BoundExpression* MakeBoundExpression_Error();
BoundExpression* MakeBoundExpression_Empty();
BoundExpression* MakeBoundExpression_Literal(MyType* pType, const MyValue& Value);
BoundExpression* MakeBoundExpression_Unary(BoundUnaryOperator* pOperator, BoundExpression* pRhs);
BoundExpression* MakeBoundExpression_Binary(BoundExpression* pLhs, BoundBinaryOperator* pOperator, BoundExpression* pRhs);
BoundExpression* MakeBoundExpression_Ternary(BoundExpression* pCondition, BoundExpression* pThen, BoundExpression* pElse);
BoundExpression* MakeBoundExpression_Increment(MySymbol* pLvalue, BoundExpression* pIncrement);
BoundExpression* MakeBoundExpression_Name(MySymbol* pSymbol);
BoundExpression* MakeBoundExpression_Assignment(BoundExpression* pLvalue, MySymbol* pVariable, BoundExpression* pRvalue);
BoundExpression* MakeBoundExpression_OperatorNew(MyType* pType, BoundExpression* pInitializer);
BoundExpression* MakeBoundExpression_Call(BoundExpression* pCallable, MySymbol* pFunction, BoundExpression** ppArguments);
BoundExpression* MakeBoundExpression_Call(
	BoundExpression* pCallable,
	MySymbol*        pFunction,
	const std::initializer_list<BoundExpression*>& Arguments
);
BoundExpression* MakeBoundExpression_Index(BoundExpression* pSequence, MyType* pItemType, BoundExpression** ppIndices);
BoundExpression* MakeBoundExpression_Field(BoundExpression* pObject, MyType* pFieldType, char* const lpField);
BoundExpression* MakeBoundExpression_Array(MyType* pType, BoundExpression** ppItems, BoundExpression* pDefault = nullptr);
BoundExpression* MakeBoundExpression_Instance(MyType* pType, BoundInstanceExpression::MemberMap* pMembers);
BoundExpression* MakeBoundExpression_Conversion(MyType* pType, BoundExpression* pExpression);

/// Create Bound Statements
BoundStatement* MakeBoundStatement_Block(BoundStatement** ppStatements);
BoundStatement* MakeBoundStatement_Block(const std::initializer_list<BoundStatement*>& Statements);
BoundStatement* MakeBoundStatement_Expression(BoundExpression* pExpression);
BoundStatement* MakeBoundStatement_VariableDeclaration(MySymbol* pVariable, BoundExpression* pValue);
BoundStatement* MakeBoundStatement_DecomposeDeclaration(MySymbol* pStruct, MySymbol** ppVariables, BoundExpression* pDecomposable);
BoundStatement* MakeBoundStatement_If(BoundExpression* pCondition, BoundStatement*  pIfBlock, ElseIfBlock* pElseifs, BoundStatement* pElseBlock);
BoundStatement* MakeBoundStatement_For(
	MySymbol*         pVariable,
	BoundExpression*  pLowerBound,
	BoundExpression*  pUpperBound,
	BoundExpression*  pStep,
	BoundStatement*   pBody,
	const BoundLabel& BreakLabel,
	const BoundLabel& ContinueLabel
);
BoundStatement* MakeBoundStatement_Foreach(
	MySymbol*         pVariable,
	BoundExpression*  pIterable,
	BoundStatement*   pBody,
	const BoundLabel& BreakLabel,
	const BoundLabel& ContinueLabel
);
BoundStatement* MakeBoundStatement_While(
	BoundExpression*  pCondition,
	BoundStatement*   pBody,
	bool		      bIsDoWhile,
	const BoundLabel& BreakLabel,
	const BoundLabel& ContinueLabel
);
BoundStatement* MakeBoundStatement_Return(BoundExpression* pExpression);
BoundStatement* MakeBoundStatement_Label(const BoundLabel& Label);
BoundStatement* MakeBoundStatement_Goto(const BoundLabel& Label);
BoundStatement* MakeBoundStatement_ConditionalGoto(const BoundLabel& Label, BoundExpression* pCondition, bool bJumpIfTrue);
#pragma endregion


const char* SymbolKindString(SymbolKind Kind) noexcept;
const char* BoundExpressionKindString(BoundExpressionKind Kind) noexcept;
const char* BoundStatementKindString(BoundStatementKind Kind) noexcept;


