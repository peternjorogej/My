#include "BoundTree.h"
#include "ControlFlowGraph.h"
#include "Lowerer.h"
#include "My/Base/IO.h"
#include "stb/stb_ds.h"


static bool _My_ArrayTypesMatch(const MyArrayType& From, const MyArrayType& To) noexcept;
static bool _My_FunctionTypesMatch(const MyFunctionSignature& From, const MyFunctionSignature& To) noexcept;

#pragma region Type Conversion
/// Type Conversion
class TypeConversion
{
public:
	static TypeConversion None;
	static TypeConversion Identity;
	static TypeConversion Implicit;
	static TypeConversion Explicit;

public:
	constexpr TypeConversion(bool Exists, bool IsIdentity, bool IsImplicit)
		: Exists(Exists), IsIdentity(IsIdentity), IsImplicit(IsImplicit)
	{ }

	bool IsExplicit() const noexcept
	{
		return Exists && !IsImplicit;
	}

	static const TypeConversion& Classify(MyType* From, MyType* To) noexcept
	{
		const MyDefaults& md = My_Defaults;

		if (From == To)
			return TypeConversion::Identity;

		if (From != md.VoidType && To == md.ObjectType)
			return TypeConversion::Implicit;

		if (From == md.ObjectType)
			if (To == md.BooleanType || To == md.IntType || To == md.UintType || To == md.FloatType)
				return TypeConversion::Explicit;
			else
				return TypeConversion::Implicit;

		if (From == md.BooleanType || From == md.IntType || From == md.UintType || From == md.FloatType)
			if (To == md.StringType)
				return TypeConversion::Explicit;

		/*if (From == md.StringType)
			if (To == md.BooleanType || To == md.IntType || To == md.UintType || To == md.FloatType)
				return TypeConversion::Explicit;*/

		if (From == md.BooleanType)
			if (To == md.IntType || To == md.UintType || To == md.FloatType)
				return TypeConversion::Implicit;

		if (From == md.IntType)
			if (To == md.BooleanType || To == md.UintType || To == md.FloatType)
				return TypeConversion::Implicit;
		
		if (From == md.UintType)
			if (To == md.BooleanType || To == md.IntType || To == md.FloatType)
				return TypeConversion::Implicit;
		
		if (From == md.FloatType)
			if (To == md.BooleanType || To == md.IntType || To == md.UintType)
				return TypeConversion::Implicit;

		if (From->Kind == MY_TYPE_KIND_ARRAY && To->Kind == MY_TYPE_KIND_ARRAY)
		{
			if (_My_ArrayTypesMatch(*From->Array, *To->Array))
			{
				return TypeConversion::Implicit;
			}
		}
		
		if (From->Kind == MY_TYPE_KIND_FUNCTION && To->Kind == MY_TYPE_KIND_FUNCTION)
		{
			if (_My_FunctionTypesMatch(*From->Signature, *To->Signature))
			{
				return TypeConversion::Implicit;
			}
		}

		return TypeConversion::None;
	}

public:
	bool Exists     = false;
	bool IsIdentity = false;
	bool IsImplicit = false;
};

TypeConversion TypeConversion::None     = TypeConversion(false, false, false);
TypeConversion TypeConversion::Identity = TypeConversion(true, true, true);
TypeConversion TypeConversion::Implicit = TypeConversion(true, false, true);
TypeConversion TypeConversion::Explicit = TypeConversion(true, false, false);
#pragma endregion

/// Symbols
MySymbol::MySymbol()
{
	memset(this, 0, sizeof(MySymbol));
	Kind = SymbolKind::Invalid;
}

MySymbol::MySymbol(SymbolKind Kind, char* const lpName, MyType* pType)
	: MySymbol()
{
	this->Kind = Kind;
	this->Name = lpName;
	this->Type = pType;
}

/// Expressions
#pragma region Operator Binding
BoundUnaryOperator::BoundUnaryOperator(TokenKind Kind, BoundUnaryOperatorKind OperatorKind, MyType* pRhsType, MyType* pResultType)
	: Kind(Kind), OperatorKind(OperatorKind), RhsType(pRhsType), ResultType(pResultType)
{ }

BoundUnaryOperator::BoundUnaryOperator(TokenKind Kind, BoundUnaryOperatorKind OperatorKind, MyType* pRhsType)
	: BoundUnaryOperator(Kind, OperatorKind, pRhsType, pRhsType)
{ }

BoundUnaryOperator* BoundUnaryOperator::Bind(TokenKind Kind, MyType* pRhsType) noexcept
{
	static BoundUnaryOperator s_Operators[] =
	{
		{ TokenKind::Plus,  BoundUnaryOperatorKind::Identity, My_Defaults.IntType },
		{ TokenKind::Dash,  BoundUnaryOperatorKind::Negation, My_Defaults.IntType },
		{ TokenKind::Bang,  BoundUnaryOperatorKind::LogicalNegation, My_Defaults.IntType, My_Defaults.BooleanType },
		{ TokenKind::Tilde, BoundUnaryOperatorKind::BitwiseNegation, My_Defaults.IntType },
		
		{ TokenKind::Plus,  BoundUnaryOperatorKind::Identity, My_Defaults.UintType },
		{ TokenKind::Dash,  BoundUnaryOperatorKind::Negation, My_Defaults.UintType },
		{ TokenKind::Bang,  BoundUnaryOperatorKind::LogicalNegation, My_Defaults.UintType, My_Defaults.BooleanType },
		{ TokenKind::Tilde, BoundUnaryOperatorKind::BitwiseNegation, My_Defaults.UintType },

		{ TokenKind::Plus, BoundUnaryOperatorKind::Identity, My_Defaults.FloatType },
		{ TokenKind::Dash, BoundUnaryOperatorKind::Negation, My_Defaults.FloatType },
		{ TokenKind::Bang, BoundUnaryOperatorKind::LogicalNegation, My_Defaults.FloatType, My_Defaults.BooleanType },

		{ TokenKind::Bang, BoundUnaryOperatorKind::LogicalNegation, My_Defaults.BooleanType  },

	};
	for (BoundUnaryOperator& buo : s_Operators)
	{
		if (buo.Kind == Kind && buo.RhsType == pRhsType)
		{
			return &buo;
		}
	}
	return nullptr;
}


BoundBinaryOperator::BoundBinaryOperator(
	TokenKind               Kind,
	BoundBinaryOperatorKind OperatorKind,
	MyType*                 pLhsType,
	MyType*                 pRhsType,
	MyType*                 pResultType
) : Kind(Kind), OperatorKind(OperatorKind), LhsType(pLhsType), RhsType(pRhsType), ResultType(pResultType)
{ }

BoundBinaryOperator::BoundBinaryOperator(TokenKind Kind, BoundBinaryOperatorKind OperatorKind, MyType* pLhsType)
	: BoundBinaryOperator(Kind, OperatorKind, pLhsType, pLhsType, pLhsType)
{ }

BoundBinaryOperator::BoundBinaryOperator(TokenKind Kind, BoundBinaryOperatorKind OperatorKind, MyType* pOperandType, MyType* pResultType)
	: BoundBinaryOperator(Kind, OperatorKind, pOperandType, pOperandType, pResultType)
{ }

#pragma region Definitions for Operator Binding
#define _Define_My_BoundBinaryOperator_Bind_X64_X64(__x, __TpX) \
	static BoundBinaryOperator* _My_BoundBinaryOperator_Bind_##__x##_##__x(TokenKind Kind, MyType* pLhsType, MyType* pRhsType) noexcept \
	{                                                                                                                \
		static BoundBinaryOperator s_Operators_##__x##_##__x[] =                                                     \
		{                                                                                                            \
			{ TokenKind::Plus,            BoundBinaryOperatorKind::Addition,       __TpX },                          \
			{ TokenKind::Dash,            BoundBinaryOperatorKind::Subtraction,    __TpX },                          \
			{ TokenKind::Star,            BoundBinaryOperatorKind::Multiplication, __TpX },                          \
			{ TokenKind::Slash,           BoundBinaryOperatorKind::Division,       __TpX },                          \
			{ TokenKind::StarStar,        BoundBinaryOperatorKind::Exponentiation, __TpX },                          \
			{ TokenKind::LessLess,		  BoundBinaryOperatorKind::LeftShift,      __TpX },                          \
			{ TokenKind::GreaterGreater,  BoundBinaryOperatorKind::RightShift,     __TpX },                          \
			{ TokenKind::Percent,         BoundBinaryOperatorKind::Modulo,         __TpX },                          \
			{ TokenKind::And,             BoundBinaryOperatorKind::BitwiseAnd,     __TpX },                          \
			{ TokenKind::Pipe,            BoundBinaryOperatorKind::BitwiseOr,      __TpX },                          \
			{ TokenKind::Caret,           BoundBinaryOperatorKind::BitwiseXor,     __TpX },                          \
			{ TokenKind::AndAnd,          BoundBinaryOperatorKind::LogicalAnd,     __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::PipePipe,        BoundBinaryOperatorKind::LogicalOr,      __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::EqualsEquals,    BoundBinaryOperatorKind::Equality,       __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::BangEquals,      BoundBinaryOperatorKind::NonEquality,    __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::Less,		      BoundBinaryOperatorKind::Less,		   __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::LessEquals,      BoundBinaryOperatorKind::LessOrEqual,    __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::Greater,		  BoundBinaryOperatorKind::Greater,		   __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::GreaterEquals,   BoundBinaryOperatorKind::GreaterOrEqual, __TpX, My_Defaults.BooleanType }, \
		};																											 \
		for (BoundBinaryOperator& bbo : s_Operators_##__x##_##__x)                                                   \
		{                                                                                                            \
			if (bbo.Kind == Kind && bbo.LhsType == pLhsType && bbo.RhsType == pRhsType)                              \
			{                                                                                                        \
				return &bbo;                                                                                         \
			}				                                                                                         \
		}                                                                                                            \
		return nullptr;                                                                                              \
	}

#define _Define_My_BoundBinaryOperator_Bind_X64_Y64(__x, __y, __TpX, __TpY) \
	static BoundBinaryOperator* _My_BoundBinaryOperator_Bind_##__x##_##__y(TokenKind Kind, MyType* pLhsType, MyType* pRhsType) noexcept \
	{                                                                                                                     \
		static BoundBinaryOperator s_Operators_##__x##_##__y[] =                                                          \
		{                                                                                                                 \
			{ TokenKind::Plus,	        BoundBinaryOperatorKind::Addition,		 __TpX, __TpY, __TpY },                   \
			{ TokenKind::Dash,	        BoundBinaryOperatorKind::Subtraction,	 __TpX, __TpY, __TpY },                   \
			{ TokenKind::Star,	        BoundBinaryOperatorKind::Multiplication, __TpX, __TpY, __TpY },                   \
			{ TokenKind::Slash,         BoundBinaryOperatorKind::Division,		 __TpX, __TpY, __TpY },                   \
			{ TokenKind::StarStar,      BoundBinaryOperatorKind::Exponentiation, __TpX, __TpY, __TpY },                   \
			{ TokenKind::AndAnd,        BoundBinaryOperatorKind::LogicalAnd,     __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::PipePipe,      BoundBinaryOperatorKind::LogicalOr,      __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::EqualsEquals,  BoundBinaryOperatorKind::Equality,       __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::BangEquals,    BoundBinaryOperatorKind::NonEquality,    __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::Less,		    BoundBinaryOperatorKind::Less,		     __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::LessEquals,    BoundBinaryOperatorKind::LessOrEqual,    __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::Greater,		BoundBinaryOperatorKind::Greater,		 __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::GreaterEquals, BoundBinaryOperatorKind::GreaterOrEqual, __TpX, __TpY, My_Defaults.BooleanType }, \
			{ TokenKind::Plus,	        BoundBinaryOperatorKind::Addition,		 __TpY, __TpX, __TpY },                   \
			{ TokenKind::Dash,	        BoundBinaryOperatorKind::Subtraction,	 __TpY, __TpX, __TpY },                   \
			{ TokenKind::Star,	        BoundBinaryOperatorKind::Multiplication, __TpY, __TpX, __TpY },                   \
			{ TokenKind::Slash,         BoundBinaryOperatorKind::Division,		 __TpY, __TpX, __TpY },                   \
			{ TokenKind::StarStar,      BoundBinaryOperatorKind::Exponentiation, __TpY, __TpX, __TpY },                   \
			{ TokenKind::AndAnd,        BoundBinaryOperatorKind::LogicalAnd,     __TpY, __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::PipePipe,      BoundBinaryOperatorKind::LogicalOr,      __TpY, __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::EqualsEquals,  BoundBinaryOperatorKind::Equality,       __TpY, __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::BangEquals,    BoundBinaryOperatorKind::NonEquality,    __TpY, __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::Less,		    BoundBinaryOperatorKind::Less,		     __TpY, __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::LessEquals,    BoundBinaryOperatorKind::LessOrEqual,    __TpY, __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::Greater,		BoundBinaryOperatorKind::Greater,		 __TpY, __TpX, My_Defaults.BooleanType }, \
			{ TokenKind::GreaterEquals, BoundBinaryOperatorKind::GreaterOrEqual, __TpY, __TpX, My_Defaults.BooleanType }, \
		};                                                                                                                \
		for (BoundBinaryOperator& bbo : s_Operators_##__x##_##__y)                                                        \
		{                                                                                                                 \
			if (bbo.Kind == Kind && bbo.LhsType == pLhsType && bbo.RhsType == pRhsType)									  \
			{                                                                                                             \
				return &bbo;                                                                                              \
			}                                                                                                             \
		}                                                                                                                 \
		return nullptr;                                                                                                   \
	}

_Define_My_BoundBinaryOperator_Bind_X64_X64(Int, My_Defaults.IntType)
_Define_My_BoundBinaryOperator_Bind_X64_X64(Uint, My_Defaults.UintType)

_Define_My_BoundBinaryOperator_Bind_X64_Y64(Int, Uint, My_Defaults.IntType, My_Defaults.UintType)
_Define_My_BoundBinaryOperator_Bind_X64_Y64(Int, Float, My_Defaults.IntType, My_Defaults.FloatType)
_Define_My_BoundBinaryOperator_Bind_X64_Y64(Uint, Float, My_Defaults.UintType, My_Defaults.FloatType)

#undef _Define_My_BoundBinaryOperator_Bind_X64_X64
#undef _Define_My_BoundBinaryOperator_Bind_X64_Y64
#pragma endregion

static BoundBinaryOperator* _My_BoundBinaryOperator_Bind_Float_Float(TokenKind Kind, MyType* pLhsType, MyType* pRhsType) noexcept
{
	static BoundBinaryOperator s_Operators_Float_Float[] =
	{
		{ TokenKind::Plus,	        BoundBinaryOperatorKind::Addition,		 My_Defaults.FloatType },
		{ TokenKind::Dash,	        BoundBinaryOperatorKind::Subtraction,	 My_Defaults.FloatType },
		{ TokenKind::Star,	        BoundBinaryOperatorKind::Multiplication, My_Defaults.FloatType },
		{ TokenKind::Slash,         BoundBinaryOperatorKind::Division,		 My_Defaults.FloatType },
		{ TokenKind::StarStar,      BoundBinaryOperatorKind::Exponentiation, My_Defaults.FloatType },
		{ TokenKind::AndAnd,        BoundBinaryOperatorKind::LogicalAnd,     My_Defaults.FloatType, My_Defaults.BooleanType },
		{ TokenKind::PipePipe,      BoundBinaryOperatorKind::LogicalOr,      My_Defaults.FloatType, My_Defaults.BooleanType },
		{ TokenKind::EqualsEquals,  BoundBinaryOperatorKind::Equality,       My_Defaults.FloatType, My_Defaults.BooleanType },
		{ TokenKind::BangEquals,    BoundBinaryOperatorKind::NonEquality,    My_Defaults.FloatType, My_Defaults.BooleanType },
		{ TokenKind::Less,		    BoundBinaryOperatorKind::Less,		     My_Defaults.FloatType, My_Defaults.BooleanType },
		{ TokenKind::LessEquals,    BoundBinaryOperatorKind::LessOrEqual,    My_Defaults.FloatType, My_Defaults.BooleanType },
		{ TokenKind::Greater,		BoundBinaryOperatorKind::Greater,		 My_Defaults.FloatType, My_Defaults.BooleanType },
		{ TokenKind::GreaterEquals, BoundBinaryOperatorKind::GreaterOrEqual, My_Defaults.FloatType, My_Defaults.BooleanType },
	};
	for (BoundBinaryOperator& bbo : s_Operators_Float_Float)
	{
		if (bbo.Kind == Kind && bbo.LhsType == pLhsType && bbo.RhsType == pRhsType)
		{
			return &bbo;
		}
	}
	return nullptr;
}

BoundBinaryOperator* BoundBinaryOperator::Bind(TokenKind Kind, MyType* pLhsType, MyType* pRhsType) noexcept
{
	// NOTE: This is really one gigantic linear search (probably going to slow down compile times)
	BoundBinaryOperator* pOperator = nullptr;

	static BoundBinaryOperator s_Operators_Other_Other[] =
	{
		{ TokenKind::AndAnd,       BoundBinaryOperatorKind::LogicalAnd,  My_Defaults.BooleanType },
		{ TokenKind::PipePipe,     BoundBinaryOperatorKind::LogicalOr,   My_Defaults.BooleanType },
		{ TokenKind::EqualsEquals, BoundBinaryOperatorKind::Equality,    My_Defaults.BooleanType },
		{ TokenKind::BangEquals,   BoundBinaryOperatorKind::NonEquality, My_Defaults.BooleanType },

		{ TokenKind::LessLess,       BoundBinaryOperatorKind::LeftShift,  My_Defaults.IntType,  My_Defaults.UintType, My_Defaults.UintType },
		{ TokenKind::GreaterGreater, BoundBinaryOperatorKind::RightShift, My_Defaults.IntType,  My_Defaults.UintType, My_Defaults.UintType },
		{ TokenKind::Percent,        BoundBinaryOperatorKind::Modulo,     My_Defaults.IntType,  My_Defaults.UintType, My_Defaults.UintType },
		{ TokenKind::LessLess,       BoundBinaryOperatorKind::LeftShift,  My_Defaults.UintType, My_Defaults.IntType,  My_Defaults.UintType },
		{ TokenKind::GreaterGreater, BoundBinaryOperatorKind::RightShift, My_Defaults.UintType, My_Defaults.IntType,  My_Defaults.UintType },
		{ TokenKind::Percent,        BoundBinaryOperatorKind::Modulo,     My_Defaults.UintType, My_Defaults.IntType,  My_Defaults.UintType },
		
		{ TokenKind::EqualsEquals, BoundBinaryOperatorKind::Equality,    My_Defaults.IntPtrType, My_Defaults.BooleanType },
		{ TokenKind::BangEquals,   BoundBinaryOperatorKind::NonEquality, My_Defaults.IntPtrType, My_Defaults.BooleanType },
		
		{ TokenKind::Plus,         BoundBinaryOperatorKind::Addition,    My_Defaults.StringType },
		{ TokenKind::EqualsEquals, BoundBinaryOperatorKind::Equality,    My_Defaults.StringType, My_Defaults.BooleanType },
		{ TokenKind::BangEquals,   BoundBinaryOperatorKind::NonEquality, My_Defaults.StringType, My_Defaults.BooleanType },

		{ TokenKind::EqualsEquals, BoundBinaryOperatorKind::Equality,    My_Defaults.ObjectType, My_Defaults.BooleanType },
		{ TokenKind::BangEquals,   BoundBinaryOperatorKind::NonEquality, My_Defaults.ObjectType, My_Defaults.BooleanType },
	};

	if ((pOperator = _My_BoundBinaryOperator_Bind_Int_Int(Kind, pLhsType, pRhsType)))
	{
		return pOperator;
	}
	if ((pOperator = _My_BoundBinaryOperator_Bind_Uint_Uint(Kind, pLhsType, pRhsType)))
	{
		return pOperator;
	}
	if ((pOperator = _My_BoundBinaryOperator_Bind_Float_Float(Kind, pLhsType, pRhsType)))
	{
		return pOperator;
	}
	if ((pOperator = _My_BoundBinaryOperator_Bind_Int_Uint(Kind, pLhsType, pRhsType)))
	{
		return pOperator;
	}
	if ((pOperator = _My_BoundBinaryOperator_Bind_Int_Float(Kind, pLhsType, pRhsType)))
	{
		return pOperator;
	}
	if ((pOperator = _My_BoundBinaryOperator_Bind_Uint_Float(Kind, pLhsType, pRhsType)))
	{
		return pOperator;
	}

	for (BoundBinaryOperator& bbo : s_Operators_Other_Other)
	{
		if (bbo.Kind == Kind && bbo.LhsType == pLhsType && bbo.RhsType == pRhsType)
		{
			return &bbo;
		}
	}

	return nullptr;
}
#pragma endregion

BoundExpression::BoundExpression()
{
	memset(this, 0, sizeof(BoundExpression));
	Kind = BoundExpressionKind::Invalid;
}

BoundExpression::BoundExpression(BoundExpressionKind Kind)
	: BoundExpression()
{
	this->Kind = Kind;
}

MyType* BoundExpression::Type() const noexcept
{
	switch (Kind)
	{
		case BoundExpressionKind::Empty:       return My_Defaults.VoidType;
		case BoundExpressionKind::Error:       return My_Defaults.ErrorType;
		case BoundExpressionKind::Literal:     return literal.Type;
		case BoundExpressionKind::Unary:       return unary.Operator->ResultType;
		case BoundExpressionKind::Binary:      return binary.Operator->ResultType;
		case BoundExpressionKind::Ternary:     return ternary.Then->Type();
		case BoundExpressionKind::Increment:   return inc.Lvalue->Type;
		case BoundExpressionKind::Name:        return name.Symbol->Type;
		case BoundExpressionKind::Assignment:  return assign.Variable->Type;
		case BoundExpressionKind::OperatorNew: return opnew.Type;
		case BoundExpressionKind::Call:        return call.Function->Type->Signature->Return;
		case BoundExpressionKind::Index:       return index.Type;
		case BoundExpressionKind::Field:       return field.Type;
		case BoundExpressionKind::Array:       return array.Type;
		case BoundExpressionKind::Cast:        return cast.Type;
		case BoundExpressionKind::Conversion:  return conv.Type;
		case BoundExpressionKind::Instance:    return inst.Type;
		default: break;
	}

	MY_ASSERT(false, "SeverelyFatalError: Should NEVER reach HERE");
	return nullptr;
}

/// Statements
BoundLabel::BoundLabel(char* const& lpLabel)
	: Label(lpLabel)
{ }

bool BoundLabel::operator==(const BoundLabel& Rhs) const noexcept
{
	return Label == Rhs.Label;
}

bool BoundLabel::operator!=(const BoundLabel& Rhs) const noexcept
{
	return !this->operator==(Rhs);
}

BoundStatement::BoundStatement()
{
	memset(this, 0, sizeof(BoundStatement));
	Kind = BoundStatementKind::Invalid;
}

BoundStatement::BoundStatement(BoundStatementKind Kind)
	: BoundStatement()
{
	this->Kind = Kind;
}

#pragma region Internal Binding
/// Scope
struct BoundScope
{
	using SymbolMap = Pair<char*, MySymbol*>;

	BoundScope* Parent    = nullptr;
	SymbolMap*  Variables = nullptr;
	SymbolMap*  Functions = nullptr;
	SymbolMap*  Structs   = nullptr;

	BoundScope()
	{
		static constexpr SymbolMap sm = { nullptr, nullptr };

		stbds_hmdefault(Variables, nullptr);
		stbds_hmdefaults(Variables, sm);
		stbds_hmdefault(Functions, nullptr);
		stbds_hmdefaults(Functions, sm);
		stbds_hmdefault(Structs, nullptr);
		stbds_hmdefaults(Structs, sm);
	}

	BoundScope(BoundScope* pParent)
		: BoundScope()
	{
		Parent = pParent;
	}

	bool DeclareVariable(MySymbol* pVariable) noexcept
	{
		MySymbol* pVar = stbds_hmget(Variables, pVariable->Name);

		if (pVar != nullptr)
		{
			return false;
		}
		if (Parent)
		{
			if (const auto[bFound, pVar] = Parent->LookupVariable(pVariable->Name); bFound)
			{
				return false;
			}
		}
		stbds_hmput(Variables, pVariable->Name, pVariable);
		return true;
	}

	bool DeclareFunction(MySymbol* pFunction) noexcept
	{
		MySymbol* pFunc = stbds_hmget(Functions, pFunction->Name);

		if (pFunc != nullptr)
		{
			return false;
		}
		if (Parent)
		{
			if (const auto [bFound, pFunc] = Parent->LookupFunction(pFunction->Name); bFound)
			{
				return false;
			}
		}
		stbds_hmput(Functions, pFunction->Name, pFunction);
		return true;
	}

	bool DeclareStruct(MySymbol* pStruct) noexcept
	{
		MySymbol* pStrct = stbds_hmget(Structs, pStruct->Name);

		if (pStrct != nullptr)
		{
			return false;
		}
		if (Parent)
		{
			if (const auto [bFound, pStrct] = Parent->LookupStruct(pStruct->Name); bFound)
			{
				return false;
			}
		}
		stbds_hmput(Structs, pStruct->Name, pStruct);
		return true;
	}

	Pair<bool, MySymbol* const&> LookupVariable(char* const lpName) noexcept
	{
		MySymbol* const& pVariable = stbds_hmget(Variables, lpName);

		if (pVariable != nullptr)
		{
			return { true, pVariable };
		}
		if (Parent)
		{
			return Parent->LookupVariable(lpName);
		}
		return { false, nullptr };
	}
	
	Pair<bool, MySymbol* const&> LookupFunction(char* const lpName) noexcept
	{
		MySymbol* const& pFunction = stbds_hmget(Functions, lpName);

		if (pFunction != nullptr)
		{
			return { true, pFunction };
		}
		if (Parent)
		{
			return Parent->LookupFunction(lpName);
		}
		return { false, nullptr };
	}

	Pair<bool, MySymbol* const&> LookupStruct(char* const lpName) noexcept
	{
		MySymbol* const& pStruct = stbds_hmget(Structs, lpName);

		if (pStruct != nullptr)
		{
			return { true, pStruct };
		}
		if (Parent)
		{
			return Parent->LookupStruct(lpName);
		}
		return { false, nullptr };
	}

	MySymbol** GetDeclaredVariables() const noexcept
	{
		MySymbol** ppVars = nullptr;
		for (size_t k = 0; k < stbds_hmlenu(Variables); k++)
		{
			stbds_arrpush(ppVars, Variables[k].value);
		}
		return ppVars;
	}

	MySymbol** GetDeclaredFunctions() const noexcept
	{
		MySymbol** ppFuncs = nullptr;
		for (size_t k = 0; k < stbds_hmlenu(Functions); k++)
		{
			stbds_arrpush(ppFuncs, Functions[k].value);
		}
		return ppFuncs;
	}

	MySymbol** GetDeclaredStructs()   const noexcept
	{
		MySymbol** ppStructs = nullptr;
		for (size_t k = 0; k < stbds_hmlenu(Structs); k++)
		{
			stbds_arrpush(ppStructs, Structs[k].value);
		}
		return ppStructs;
	}
};


class InternalBinder
{
public:
	friend class Binder;

public:
	InternalBinder(MyContext* pContext)
		: m_Tree(), m_Scope(), m_Function(), m_LoopStack(), m_Globals(), m_Diagnostics()
	{
		MyDefaults& md = My_Defaults;

		m_Scope = Allocator::Create<BoundScope>(Allocator::Stage::Binder);
		if (!s_Context)
		{
			s_Context = pContext;
		}
		if (!s_UserDefinedTypes)
		{
			static constexpr Pair<char*, MyType*> kvp = { nullptr, nullptr };
			stbds_shdefault(s_UserDefinedTypes, nullptr);
			stbds_shdefaults(s_UserDefinedTypes, kvp);

			static const auto SetupStruct = [this](MyType* pKlassType) noexcept -> void
			{
				stbds_shput(s_UserDefinedTypes, pKlassType->Klass->Name, pKlassType);

				FieldSymbol* pFieldSymbols = nullptr;
				for (size_t k = 0; k < stbds_arrlenu(pKlassType->Klass->Fields); k++)
				{
					const MyField* const pField = pKlassType->Klass->Fields + k;
					
					FieldSymbol fs = FieldSymbol{ pField->Name, pField->Type, nullptr };
					stbds_arrpush(pFieldSymbols, fs);
				}

				m_Scope->DeclareStruct(MakeSymbol_Struct(pKlassType->Klass->Name, pKlassType, pFieldSymbols, nullptr));
			};

			SetupStruct(md.ObjectType);
			SetupStruct(md.BooleanType);
			SetupStruct(md.IntType);
			SetupStruct(md.UintType);
			SetupStruct(md.IntPtrType);
			SetupStruct(md.FloatType);
			SetupStruct(md.StringType);
			SetupStruct(md.StringBuilderType);
			SetupStruct(md.ComplexType);
			SetupStruct(md.BytesType);
			SetupStruct(md.FileType);
			SetupStruct(md.MathType);

			InitBuiltinMethodSymbols(m_Scope);
		}
		if (!s_ForwardedTypes)
		{
			static constexpr Pair<char*, MyType*> kvp = { nullptr, nullptr };
			stbds_shdefault(s_ForwardedTypes, nullptr);
			stbds_shdefaults(s_ForwardedTypes, kvp);
		}
	}

	InternalBinder(MyContext* pContext, const SyntaxTree* pTree)
		: InternalBinder(pContext)
	{
		m_Tree = pTree;
	}
	
	InternalBinder(MyContext* pContext, MySymbol* pFunction, const SyntaxTree* pTree, BoundGlobalScope* pGlobalScope)
		: InternalBinder(pContext, pTree)
	{
		m_Function = pFunction;

		for (size_t k = 0; k < stbds_arrlenu(pGlobalScope->Variables); k++)
		{
			m_Scope->DeclareVariable(pGlobalScope->Variables[k]);
		}
		for (size_t k = 0; k < stbds_arrlenu(pGlobalScope->Functions); k++)
		{
			m_Scope->DeclareFunction(pGlobalScope->Functions[k]);
		}
		for (size_t k = 0; k < stbds_arrlenu(pGlobalScope->Structs); k++)
		{
			m_Scope->DeclareStruct(pGlobalScope->Structs[k]);
		}

		FunctionSymbol& fs = m_Function->funcsym;
		for (size_t k = 0; k < stbds_arrlenu(fs.Parameters); k++)
		{
			if (fs.Parameters[k]->Type->Kind == MY_TYPE_KIND_FUNCTION)
			{
				m_Scope->DeclareFunction(fs.Parameters[k]);
			}
			else
			{
				m_Scope->DeclareVariable(fs.Parameters[k]);
			}
		}
	}
	
	~InternalBinder() noexcept = default;

	void BindDeclarations(const List<MyStruct*>& UserStructs)
	{
		// These *UserStructs* will be used when we forward declare them. The types will be removed 
		// from s_ForwardedTypes and added to s_UserDefinedTypes in BindForwardDeclaration()
		for (MyStruct* pKlass : UserStructs)
		{
			MyType* pType = MyTypeCreate(MY_TYPE_KIND_STRUCT, pKlass, MY_TYPE_ATTR_NONE);
			stbds_shput(s_ForwardedTypes, pKlass->Name, pType);
		}
		
		Declaration** const& ppDecls = m_Tree->GetRoot().Decls;
		for (size_t k = 0; k < stbds_arrlenu(ppDecls); k++)
		{
			BindDeclaration(ppDecls[k]);
		}
	}

	/// Expressions
	BoundExpression* BindExpression(Expression* pExpression, MyType* pTargetType)
	{
		return BindConversion(pExpression, pTargetType);
	}

	BoundExpression* BindExpression(Expression* pExpression, bool bCanBeVoid = false)
	{
		BoundExpression* pExpr = BindExpressionInternal(pExpression);
		if (!bCanBeVoid && pExpr->Type() == My_Defaults.VoidType)
		{
			m_Diagnostics.ReportInvalidUseOfVoid(GetLocation(pExpression));
			return MakeBoundExpression_Error();
		}
		return pExpr;
	}

	BoundExpression* BindExpressionInternal(Expression* pExpression)
	{
		switch (pExpression->Kind)
		{
			case ExpressionKind::Literal:       return BindLiteralExpression(pExpression);
			case ExpressionKind::Unary:         return BindUnaryExpression(pExpression);
			case ExpressionKind::Binary:        return BindBinaryExpression(pExpression);
			case ExpressionKind::Ternary:       return BindTernaryExpression(pExpression);
			case ExpressionKind::Parenthesized: return BindParenthesizedExpression(pExpression);
			case ExpressionKind::Name:          return BindNameExpression(pExpression);
			case ExpressionKind::Assignment:    return BindAssignmentExpression(pExpression);
			case ExpressionKind::OperatorNew:   return BindOperatorNewExpression(pExpression);
			case ExpressionKind::Call:          return BindCallExpression(pExpression);
			case ExpressionKind::Index:         return BindIndexExpression(pExpression);
			case ExpressionKind::Field:         return BindFieldExpression(pExpression);
			case ExpressionKind::Array:         return BindArrayExpression(pExpression);
			case ExpressionKind::Cast:          return BindCastExpression(pExpression);
			default: break;
		}

		MY_ASSERT(false, "Unexpected expression (kind %d, %s)", pExpression->Kind, ExpressionKindString(pExpression->Kind));
		return nullptr;
	}

	/// Statements
	BoundStatement* BindStatement(Statement* pStatement)
	{
		BoundStatement* pBoundStatement = BindStatementInternal(pStatement);

		if (pBoundStatement->Kind == BoundStatementKind::Expression)
		{
			BoundExpressionStatement& es = pBoundStatement->expr;
			bool bIsAllowedExpression = es.Expr->Kind == BoundExpressionKind::Empty      ||
				                        es.Expr->Kind == BoundExpressionKind::Error      ||
										es.Expr->Kind == BoundExpressionKind::Assignment ||
										es.Expr->Kind == BoundExpressionKind::Call;
			if (!bIsAllowedExpression)
			{
				ExpressionStatement& es = pStatement->expr;
				m_Diagnostics.ReportInvalidExpressionStatement(GetLocation(es.Expr));
			}
		}

		return pBoundStatement;
	}

	BoundStatement* BindStatementInternal(Statement* pStatement)
	{
		switch (pStatement->Kind)
		{
			case StatementKind::Block:                return BindBlockStatement(pStatement);
			case StatementKind::Expression:           return BindExpressionStatement(pStatement);
			case StatementKind::VariableDeclaration:  return BindVariableDeclarationStatement(pStatement);
			case StatementKind::DecomposeDeclaration: return BindDecomposeDeclarationStatement(pStatement);
			case StatementKind::If:                   return BindIfStatement(pStatement);
			case StatementKind::For:                  return BindForStatement(pStatement);
			case StatementKind::Foreach:              return BindForeachStatement(pStatement);
			case StatementKind::While:                return BindWhileStatement(pStatement);
			case StatementKind::DoWhile:              return BindDoWhileStatement(pStatement);
			case StatementKind::Break:                return BindBreakStatement(pStatement);
			case StatementKind::Continue:             return BindContinueStatement(pStatement);
			case StatementKind::Return:               return BindReturnStatement(pStatement);
			default: break;
		}

		MY_ASSERT(false, "Unexpected statement (kind %d, %s)", pStatement->Kind, StatementKindString(pStatement->Kind));
		return nullptr;
	}

	/// Declarations
	void BindDeclaration(Declaration* pDeclaration)
	{
		switch (pDeclaration->Kind)
		{
			case DeclarationKind::Import:   BindImportDeclaration(pDeclaration);   break;
			case DeclarationKind::Using:    BindUsingDeclaration(pDeclaration);    break;
			case DeclarationKind::Extern:   BindExternDeclaration(pDeclaration);   break;
			case DeclarationKind::Enum:     BindEnumDeclaration(pDeclaration);     break;
			case DeclarationKind::Variable: BindVariableDeclaration(pDeclaration); break;
			case DeclarationKind::Function: BindFunctionDeclaration(pDeclaration); break;
			case DeclarationKind::Forward:  BindForwardDeclaration(pDeclaration);   break;
			case DeclarationKind::Struct:   BindStructDeclaration(pDeclaration);   break;
			default:
			{
				MY_ASSERT(false, "Unexpected declaration (kind %u, %s)", pDeclaration->Kind, DeclarationKindString(pDeclaration->Kind));
				break;
			}
		}
	}

	/// Types
	MyType* BindTypeSpec(TypeSpec* pTypeSpec)
	{
		switch (pTypeSpec->Kind)
		{
			case TypeSpecKind::Name:     return BindNameTypeSpecifier(pTypeSpec);
			case TypeSpecKind::Array:    return BindArrayTypeSpecifier(pTypeSpec);
			case TypeSpecKind::Function: return BindFunctionTypeSpecifier(pTypeSpec);
			default: break;
		}

		MY_ASSERT(false, "Unexpected declaration (kind %u, %s)", pTypeSpec->Kind, TypeSpecKindString(pTypeSpec->Kind));
		return nullptr;
	}

	/// Accessors
	const BoundScope*                         GetScope()       const { return m_Scope; }
	const DiagnosticBag&                      GetDiagnostics() const { return m_Diagnostics; }
	Pair<MySymbol*, BoundExpression*>* const& GetGlobals()     const { return m_Globals; }

public:
	static Pair<char*, MyType*>* const& GetUserDefinedTypes() noexcept { return s_UserDefinedTypes; }

private:
	/// Expressions
	BoundExpression* BindLiteralExpression(Expression* pLiteral)
	{
		LiteralExpression& literal = pLiteral->literal;

		MyValue Literal = {};
		MyType* pType   = nullptr;

		switch (literal.Literal.Kind)
		{
			case TokenKind::NullKeyword:  Literal = MakeValue_Null();                       pType = My_Defaults.ObjectType;  break;
			case TokenKind::TrueKeyword:  Literal = MakeValue_Bool(true);                   pType = My_Defaults.BooleanType; break;
			case TokenKind::FalseKeyword: Literal = MakeValue_Bool(false);                  pType = My_Defaults.BooleanType; break;
			case TokenKind::Int64:        Literal = MakeValue_Int64(literal.Literal.I64);   pType = My_Defaults.IntType;     break;
			case TokenKind::Uint64:       Literal = MakeValue_Uint64(literal.Literal.U64);  pType = My_Defaults.UintType;    break;
			case TokenKind::Float64:      Literal = MakeValue_Float64(literal.Literal.F64); pType = My_Defaults.FloatType;   break;
			case TokenKind::String:       Literal = MakeValue_String(literal.Literal.Str);  pType = My_Defaults.StringType;  break;
			default: break;
		}

		MY_ASSERT(pType != nullptr, "Invalid literal");
		return MakeBoundExpression_Literal(pType, Literal);
	}

	BoundExpression* BindUnaryExpression(Expression* pUnary)
	{
		UnaryExpression& unary = pUnary->unary;

		BoundExpression* pRhs = BindExpression(unary.Rhs);
		if (pRhs->Type() == My_Defaults.ErrorType)
		{
			return MakeBoundExpression_Error();
		}

		BoundUnaryOperator* pOperator = BoundUnaryOperator::Bind(unary.Operator.Kind, pRhs->Type());
		if (!pOperator)
		{
			m_Diagnostics.ReportBadUnaryOperator(GetLocation(unary.Operator), unary.Operator, pRhs->Type());
			return MakeBoundExpression_Error();
		}

		return MakeBoundExpression_Unary(pOperator, pRhs);
	}

	BoundExpression* BindBinaryExpression(Expression* pBinary)
	{
		BinaryExpression& binary = pBinary->binary;

		BoundExpression* pLhs = BindExpression(binary.Lhs);
		BoundExpression* pRhs = BindExpression(binary.Rhs);
		if (pLhs->Type() == My_Defaults.ErrorType || pRhs->Type() == My_Defaults.ErrorType)
		{
			return MakeBoundExpression_Error();
		}

		BoundBinaryOperator* pOperator = BoundBinaryOperator::Bind(binary.Operator.Kind, pLhs->Type(), pRhs->Type());
		if (!pOperator)
		{
			m_Diagnostics.ReportBadBinaryOperator(GetLocation(binary.Operator), binary.Operator, pLhs->Type(), pRhs->Type());
			return MakeBoundExpression_Error();
		}

		return MakeBoundExpression_Binary(pLhs, pOperator, pRhs);
	}

	BoundExpression* BindTernaryExpression(Expression* pTernary)
	{
		TernaryExpression& ternary = pTernary->ternary;

		BoundExpression* pCond = BindConversion(ternary.Condition, My_Defaults.BooleanType);
		BoundExpression* pThen = BindExpression(ternary.Then);
		BoundExpression* pElse = BindConversion(ternary.Else, pThen->Type());

		if (pCond->Type() == My_Defaults.ErrorType || pThen->Type() == My_Defaults.ErrorType || pElse->Type() == My_Defaults.ErrorType)
		{
			return MakeBoundExpression_Error();
		}

		return MakeBoundExpression_Ternary(pCond, pThen, pElse);
	}

	BoundExpression* BindParenthesizedExpression(Expression* pParen)
	{
		ParenthesizedExpression& paren = pParen->paren;
		return BindExpression(paren.Expr);
	}

	BoundExpression* BindNameExpression(Expression* pName)
	{
		NameExpression& name = pName->name;

		char* const& lpName = name.Identifier.Id;

		if (const auto[bFound, pVar] = m_Scope->LookupVariable(lpName); bFound)
		{
			return MakeBoundExpression_Name(pVar);
		}
		if (const auto[bFound, pFun] = m_Scope->LookupFunction(lpName); bFound)
		{
			return MakeBoundExpression_Name(pFun);
		}

		if (const auto[bFound, pStr] = m_Scope->LookupStruct(lpName); bFound)
		{
			// For static member access
			return MakeBoundExpression_Name(pStr);
		}
		else
		{
			m_Diagnostics.ReportUndefinedSymbol(GetLocation(name.Identifier), lpName);
		}
		
		return MakeBoundExpression_Error();
	}
	// TODO: Some fixing pending
	BoundExpression* BindAssignmentExpression(Expression* pAssign)
	{
		AssignmentExpression& assign = pAssign->assign;

		BoundExpression* pLhs = BindExpression(assign.Lhs);
		BoundExpression* pRhs = BindExpression(assign.Rhs);

		if (pLhs->Kind == BoundExpressionKind::Name)
		{
			char* const& lpName = pLhs->name.Symbol->Name;
			const auto [bFound, pVariable] = m_Scope->LookupVariable(lpName);

			if (!bFound)
			{
				m_Diagnostics.ReportUndefinedSymbol(GetLocation(assign.Lhs), lpName);
				goto Error;
			}
			if (pVariable->varsym.IsReadonly)
			{
				m_Diagnostics.ReportIllegalAssignment(GetLocation(assign.EqualsToken), lpName);
				goto Error;
			}

			pRhs = BindConversion(GetLocation(assign.Rhs), pRhs, pVariable->Type);
			return MakeBoundExpression_Assignment(pLhs, pVariable, pRhs);
		}
		if (pLhs->Kind == BoundExpressionKind::Index)
		{
			DebugLog::Warn("[DEBUG]: NotProperlyImplementedException: Assigning to index expressions");

			MyType* pSequenceType = pLhs->index.Sequence->Type();
			if (pSequenceType->Kind != MY_TYPE_KIND_ARRAY)
			{
				m_Diagnostics.ReportTypeCannotBeIndexed(GetLocation(assign.Lhs), pSequenceType);
				goto Error;
			}
			MyType* pElementType = GetTypeFromStruct(pSequenceType->Array->Klass);

			pRhs = BindConversion(GetLocation(assign.Rhs), pRhs, pElementType);
			// NOTE: Symbols are unused by indexing expressions, but are required to be able to use assignment expressions
			//       like Print(a[k] = b) (evaluate 'b', then 'a[k] = b', then call 'Print')
			return MakeBoundExpression_Assignment(pLhs, MakeSymbol_Variable("#Index", pElementType, false, true), pRhs);
		}
		if (pLhs->Kind == BoundExpressionKind::Field)
		{
			BoundFieldExpression& fe = pLhs->field;

			MyType* pType = fe.Object->Type();
			MyField* pField = MyStructGetField(pType->Klass, fe.Field);

			if (!pField)
			{
				m_Diagnostics.ReportInvalidKeyOrAttribute(GetLocation(assign.Lhs), fe.Field, pType->Klass);
				goto Error;
			}
			if (pField->Attributes & MY_FIELD_ATTR_CONST)
			{
				m_Diagnostics.ReportIllegalFieldAssignment(GetLocation(assign.EqualsToken), pType->Klass, fe.Field);
				goto Error;
			}

			pRhs = BindConversion(GetLocation(assign.Rhs), pRhs, pField->Type);
			return MakeBoundExpression_Assignment(pLhs, MakeSymbol_Variable(fe.Field, pField->Type, false, true), pRhs);
		}

	Error:
		return MakeBoundExpression_Error();
	}

	BoundExpression* BindOperatorNewExpression(Expression* pOperatorNew)
	{
		OperatorNewExpression& opnew = pOperatorNew->opnew;

		BoundExpression* pExpression = nullptr;
		MyType*         pType       = BindTypeSpec(opnew.Type);

		if (!pType || pType == My_Defaults.ErrorType)
		{
			return MakeBoundExpression_Error();
		}

		if (opnew.HasFieldInitializers)
		{
			const size_t kActualFieldCount = stbds_arrlenu(opnew.Fields);
			const size_t kExpectedFieldCount = MyStructFieldCount(pType->Klass);

			if (kActualFieldCount > kExpectedFieldCount)
			{
				const Token& token = opnew.Fields[kExpectedFieldCount].Name;
				m_Diagnostics.ReportTooManyInitializers(GetLocation(token), pType->Klass, kExpectedFieldCount, kActualFieldCount);
				return MakeBoundExpression_Error();
			}

			BONEFieldInitializer* pInitializers = nullptr;
			for (size_t k = 0; k < kActualFieldCount; k++)
			{
				const FieldInitializer& fi = opnew.Fields[k];
				if (MyField* pField = MyStructGetField(pType->Klass, fi.Name.Id); pField)
				{
					BoundExpression* pValue = BindConversion(fi.Value, pField->Type);
					const BONEFieldInitializer bfi = BONEFieldInitializer{ fi.Name.Id, pValue };
					stbds_arrpush(pInitializers, bfi);
				}
				else
				{
					m_Diagnostics.ReportInvalidKeyOrAttribute(GetLocation(fi.Name), fi.Name.Id, pType->Klass);
					return MakeBoundExpression_Error();
				}
			}

			return MakeBoundExpression_OperatorNew(pType, pInitializers);
		}
		else
		{
			return MakeBoundExpression_OperatorNew(pType, nullptr);
		}
	}
	
	BoundExpression* BindCallExpression(Expression* pCall)
	{
		CallExpression& call = pCall->call;

		// Handle conversion first
		if (call.Callable->Kind == ExpressionKind::Name)
		{
			char* const& lpName = call.Callable->name.Identifier.Id;
			if (MyType* pType = LookupType(lpName); pType && stbds_arrlenu(call.Arguments) == 1ul)
			{
				BoundExpression* pExpr = BindExpression(call.Arguments[0]);
				MyType* pExprType = pExpr->Type();
				TextLocation Location = GetLocation(pCall);

				if (!MyTypeIsReference(pType) && MyTypeIsReference(pExprType))
				{
					// Disallow casting from reference to primitive types
					m_Diagnostics.ReportInvalidTypeConversion(Location, pExprType, pType);
					return MakeBoundExpression_Error();
				}

				return BindConversion(Location, pExpr, pType, true); // <-- bAllowExplicit
			}
		}

		BoundExpression*  pCallable   = nullptr;
		BoundExpression** ppArgs      = nullptr;
		MySymbol*         pFunction   = nullptr;

		bool bIsStaticMethod = false;

		pCallable = BindExpression(call.Callable);
		if (pCallable->Type() == My_Defaults.ErrorType)
		{
			goto Error;
		}

		if (pCallable->Kind == BoundExpressionKind::Name)
		{
			char* const& lpName = pCallable->name.Symbol->Name;
			const Token& Identifier = call.Callable->name.Identifier;

			MyType* pType = pCallable->Type();
			{
				const auto [bIsVar,    v] = m_Scope->LookupVariable(lpName);
				const auto [bIsStruct, s] = m_Scope->LookupStruct(lpName);

				if (bIsVar || bIsStruct)
				{
					m_Diagnostics.ReportNotAFunction(GetLocation(call.Callable), lpName);
					goto Error;
				}
			}
			if (const auto[bFound, pFun] = m_Scope->LookupFunction(lpName); bFound)
			{
				const size_t kParamsLength = stbds_arrlenu(pFun->Type->Signature->Params);
				const size_t kArgsLength = stbds_arrlenu(call.Arguments);

				if (kArgsLength != kParamsLength)
				{
					TextLocation Location = kArgsLength ? GetLocation(stbds_arrlast(call.Arguments)) : GetLocation(pCall);
					m_Diagnostics.ReportInvalidArgumentCount(Location, pFun->Name, kParamsLength, kArgsLength);
					goto Error;
				}

				pFunction = pFun;
			}
			else
			{
				goto Error;
			}
		}
		else if (pCallable->Kind == BoundExpressionKind::Field)
		{
			BoundFieldExpression& field = pCallable->field;

			MyStruct* pKlass = GetStructFromType(field.Object->Type());
			MyMethod* pMethod = MyStructGetMethod(pKlass, field.Field);

			if (pMethod->Flags & MY_FUNC_ATTR_STATIC)
			{
				bIsStaticMethod = true;
			}

			if (const auto[bFound, pFun] = m_Scope->LookupFunction(pMethod->Fullname); bFound)
			{
				const size_t kParamsLength = stbds_arrlenu(pFun->funcsym.Parameters);
				const size_t kArgsLength = stbds_arrlenu(call.Arguments) + (bIsStaticMethod ? 0u : 1u);

				if (kArgsLength != kParamsLength)
				{
					TextLocation Location = call.Arguments ? GetLocation(stbds_arrlast(call.Arguments)) : GetLocation(pCall);
					m_Diagnostics.ReportInvalidArgumentCount(Location, pFun->Name, kParamsLength, kArgsLength);
					goto Error;
				}

				pFunction = pFun;
			}
			else
			{
				goto Error;
			}
		}
		else
		{
			m_Diagnostics.ReportUnknownError(GetLocation(call.Callable), "Invalid object kind for call expression");
			goto Error;
		}

		const size_t kArgsLength = stbds_arrlenu(call.Arguments);
		for (size_t k = 0; k < kArgsLength; k++)
		{
			BoundExpression* pBoundArgument = BindExpression(call.Arguments[k]);
			MyType* pParamType = pFunction->Type->Signature->Params[k];

			pBoundArgument = BindConversion(GetLocation(call.Arguments[k]), pBoundArgument, pParamType);
			stbds_arrpush(ppArgs, pBoundArgument);
		}

		if (bIsStaticMethod)
		{
			pCallable = MakeBoundExpression_Name(pFunction);
		}

		return MakeBoundExpression_Call(pCallable, pFunction, ppArgs);

	Error:
		return MakeBoundExpression_Error();
	}

	BoundExpression* BindIndexExpression(Expression* pIndexExpr)
	{
		IndexExpression& index = pIndexExpr->index;

		BoundExpression* pSequence = BindExpression(index.Sequence);
		BoundExpression** ppIndices = nullptr;

		const size_t kCount = stbds_arrlenu(index.Indices);
		for (size_t k = 0; k < kCount; k++)
		{
			BoundExpression* pIndex = BindExpression(index.Indices[k]);
			stbds_arrpush(ppIndices, pIndex);
		}

		MyType* pValueType = nullptr;

		MyType* pSequenceType = pSequence->Type();
		if (pSequenceType == My_Defaults.ErrorType)
		{
			goto Error;
		}

		// Arrays
		if (pSequenceType->Kind == MY_TYPE_KIND_ARRAY)
		{
			const size_t kExpectedIndexCount = stbds_arrlenu(pSequenceType->Array->Lengths);
			if (kCount != kExpectedIndexCount)
			{
				m_Diagnostics.ReportMismatchedIndexCount(GetLocation(pIndexExpr), kExpectedIndexCount, kCount);
				goto Error;
			}

			for (size_t k = 0; k < stbds_arrlenu(ppIndices); k++)
			{
				if (ppIndices[k]->Type() != My_Defaults.IntType)
				{
					m_Diagnostics.ReportTypeCannotBeUsedAsIndex(
						GetLocation(index.Indices[k]), pSequenceType, ppIndices[k]->Type(), My_Defaults.IntType
					);
					goto Error;
				}
			}

			pValueType = GetTypeFromStruct(pSequenceType->Array->Klass);
		}
		// Error
		else
		{
		m_Diagnostics.ReportTypeCannotBeIndexed(GetLocation(index.Sequence), pSequenceType);
		goto Error;
		}

		return MakeBoundExpression_Index(pSequence, pValueType, ppIndices);

	Error:
		return MakeBoundExpression_Error();
	}

	BoundExpression* BindFieldExpression(Expression* pField)
	{
		FieldExpression& field = pField->field;

		BoundExpression* pObject = BindExpression(field.Object);
		char* const& lpField = field.Field.Id;

		MyType* pObjectType = pObject->Type();
		if (pObjectType == My_Defaults.ErrorType)
		{
			return MakeBoundExpression_Error();
		}

		bool bIsStaticMemberAccess = pObject->Kind == BoundExpressionKind::Name && pObject->name.Symbol->Name == pObjectType->Klass->Name;

		MyField* pStructField = MyStructGetField(pObjectType->Klass, lpField);
		if (pStructField)
		{
			if (bIsStaticMemberAccess && !(pStructField->Attributes & MY_FIELD_ATTR_STATIC))
			{
				m_Diagnostics.ReportNotAStaticMember(GetLocation(field.Field), lpField, pObjectType->Klass);
				return MakeBoundExpression_Error();
			}
			return MakeBoundExpression_Field(pObject, pStructField->Type, lpField);
		}

		MyMethod* pMethod = MyStructGetMethod(pObjectType->Klass, lpField);
		if (pMethod)
		{
			if (bIsStaticMemberAccess && !(pMethod->Flags & MY_FUNC_ATTR_STATIC))
			{
				m_Diagnostics.ReportNotAStaticMethod(GetLocation(field.Field), lpField, pObjectType->Klass);
				return MakeBoundExpression_Error();
			}
			return MakeBoundExpression_Field(pObject, pMethod->Type, lpField);
		}

		m_Diagnostics.ReportInvalidKeyOrAttribute(GetLocation(field.Field), lpField, pObjectType->Klass);
		return MakeBoundExpression_Error();
	}

	BoundExpression* BindArrayExpression(Expression* pArray)
	{
		ArrayExpression& array = pArray->array;

		MyType* pFirstItemType = nullptr;
		BoundExpression** ppItems = nullptr;

		for (size_t k = 0; k < stbds_arrlenu(array.Items); k++)
		{
			BoundExpression* pBoundItem = BindExpression(array.Items[k]);
			if (!pFirstItemType)
			{
				pFirstItemType = pBoundItem->Type();
			}

			pBoundItem = BindConversion(GetLocation(array.Items[k]), pBoundItem, pFirstItemType);
			stbds_arrpush(ppItems, pBoundItem);
		}
		MY_ASSERT(pFirstItemType != nullptr, "How can the first item in the array be of an unknown type?");

		uint32_t* pCounts = nullptr;
		stbds_arrpush(pCounts, (uint32_t)stbds_arrlenu(ppItems));
		MyType* pType = MyTypeCreate(MY_TYPE_KIND_ARRAY, new MyArrayType{ pFirstItemType->Klass, pCounts });

		return MakeBoundExpression_Array(pType, ppItems);
	}

	BoundExpression* BindCastExpression(Expression* pCast)
	{
		CastExpression& cast = pCast->cast;

		MyDefaults& md = My_Defaults;

		MyType* pType = BindTypeSpec(cast.Type);
		if (!pType)
		{
			m_Diagnostics.ReportUnknownType(GetLocation(cast.Type), GetTypeName(cast.Type));
			return MakeBoundExpression_Error();
		}

		BoundExpression* pExpr = BindExpression(cast.Expr);
		MyType* pExprType = pExpr->Type();

		if (!pExpr || pExprType == md.ErrorType)
		{
			return MakeBoundExpression_Error();
		}

		// Cast expressions are valid only when:
		//     1. The expression being cast is of a reference type and the type we are casting to is also
		//        a reference type.
		//     2. Both types are primitive (in which case, we use a type conversion instead).
		//     3. We are casting between primitives and the generic *object* type.
		bool bTypeFromIsRef = MyTypeIsReference(pExprType);
		bool bTypeToIsRef = MyTypeIsReference(pType);
		
		if (bTypeFromIsRef == bTypeToIsRef)
		{
			// Case 1 & 2:
			if (bTypeFromIsRef)
			{
				// Restrict conversions between different reference types
				if (pExprType == md.ObjectType)
				{
					return MakeBoundExpression_Cast(pType, pExpr);
				}
			}
			else
			{
				return BindConversion(GetLocation(cast.Expr), pExpr, pType, true); // <-- bAllowExplicit;
			}
		}

		if ((!bTypeFromIsRef && pType == md.ObjectType) || // primitive type to *object* type
			(!bTypeToIsRef && pExprType == md.ObjectType)) // *object* type to primitive type
		{
			// Case 3:
			return BindConversion(GetLocation(cast.Expr), pExpr, pType, true); // <-- bAllowExplicit;
		}

		m_Diagnostics.ReportInvalidCast(GetLocation(pCast), pExprType, pType);
		return MakeBoundExpression_Error();
	}

	BoundExpression* BindConversion(Expression* pExpression, MyType* pType, bool bAllowExplicit = false)
	{
		BoundExpression* pBoundExpression = BindExpression(pExpression);
		return BindConversion(GetLocation(pExpression), pBoundExpression, pType, bAllowExplicit);
	}

	BoundExpression* BindConversion(const TextLocation& Location, BoundExpression* pExpression, MyType* pType, bool bAllowExplicit = false)
	{
		const TypeConversion& Conversion = TypeConversion::Classify(pExpression->Type(), pType);

		if (!Conversion.Exists)
		{
			if (pExpression->Type() != My_Defaults.ErrorType && pType != My_Defaults.ErrorType)
			{
				m_Diagnostics.ReportIllegalTypeConversion(Location, pExpression->Type(), pType);
			}

			return MakeBoundExpression_Error();
		}

		if (!bAllowExplicit && Conversion.IsExplicit())
		{
			m_Diagnostics.ReportIllegalImplicitTypeConversion(Location, pExpression->Type(), pType);
			return MakeBoundExpression_Error();
		}

		if (Conversion.IsIdentity)
		{
			return pExpression;
		}

		return MakeBoundExpression_Conversion(pType, pExpression);
	}

	/// Statements
	BoundStatement* BindBlockStatement(Statement* pBlock)
	{
		BlockStatement& block = pBlock->block;

		BoundScope* pLocalScope = Allocator::Create<BoundScope>(Allocator::Stage::Binder, m_Scope);
		m_Scope = pLocalScope;
		
		BoundStatement** ppStmts = nullptr;
		for (size_t k = 0; k < stbds_arrlenu(block.Stmts); k++)
		{
			BoundStatement* pStmt = BindStatement(block.Stmts[k]);
			stbds_arrpush(ppStmts, pStmt);
		}

		m_Scope = m_Scope->Parent;
		return MakeBoundStatement_Block(ppStmts);
	}

	BoundStatement* BindExpressionStatement(Statement* pExprStmt)
	{
		ExpressionStatement& expr = pExprStmt->expr;

		BoundExpression* pExpr = BindExpression(expr.Expr, true); // <-- bCanBeVoid
		return MakeBoundStatement_Expression(pExpr);
	}

	BoundStatement* BindVariableDeclarationStatement(Statement* pVarDecl)
	{
		VariableDeclarationStatement& vardecl = pVarDecl->var;

		const auto[pSymbol, pValue] = BindVariableDeclaration(vardecl);
		return MakeBoundStatement_VariableDeclaration(pSymbol, pValue);
	}

	BoundStatement* BindDecomposeDeclarationStatement(Statement* pDecompDecl)
	{
		DecomposeDeclarationStatement& decomp = pDecompDecl->decomp;

		BoundExpression* pDecomposable = BindExpression(decomp.Decomposable);
		MyType* pType = pDecomposable->Type();

		const auto[bFound, pStruct] = m_Scope->LookupStruct(pType->Klass->Name);
		if (!bFound)
		{
			m_Diagnostics.ReportInvalidTypeForDecomposable(GetLocation(decomp.Decomposable), pType);
			goto Error;
		}

		StructSymbol& ss = pStruct->structsym;

		const size_t kExpectedCount = stbds_arrlenu(decomp.Identifiers);
		const size_t kCount = stbds_arrlenu(ss.Fields);
		if (kExpectedCount != kCount)
		{
			m_Diagnostics.ReportInvalidCountForDecomposition(GetLocation(decomp.Decomposable), kExpectedCount, kCount);
			goto Error;
		}

		MySymbol** ppVars = nullptr;
		for (size_t k = 0; k < kExpectedCount; k++)
		{
			const Token& Var = decomp.Identifiers[k];
			FieldSymbol& fs = ss.Fields[k];

			MySymbol* pVar = MakeSymbol_Variable(Var.Id, fs.Type, false, true);
			stbds_arrpush(ppVars, pVar);

			if (!m_Scope->DeclareVariable(pVar))
			{
				m_Diagnostics.ReportSymbolRedeclaration(GetLocation(Var), Var.Id);
			}
		}

		return MakeBoundStatement_DecomposeDeclaration(pStruct, ppVars, pDecomposable);
	
	Error:
		return BindErrorStatement();
	}

	BoundStatement* BindIfStatement(Statement* pIf)
	{
		IfStatement& ifstmt = pIf->ifstmt;

		BoundExpression* pCondition = BindExpression(ifstmt.Condition);
		BoundStatement* pIfBlock = BindStatement(ifstmt.IfBlock);
		ElseIfBlock* pElseifs = nullptr;

		const size_t kCount = stbds_arrlenu(ifstmt.ElseIfs);
		for (size_t k = 0; k < kCount; k++)
		{
			BoundExpression* pElseifCondition = BindExpression(ifstmt.ElseIfs[k].Condition);
			BoundStatement* pElseifBody = BindStatement(ifstmt.ElseIfs[k].ElseifBlock);

			const ElseIfBlock eib = { pElseifCondition, pElseifBody };
			stbds_arrpush(pElseifs, eib);
		}
		
		BoundStatement* pElseBlock = ifstmt.ElseBlock ? BindStatement(ifstmt.ElseBlock) : nullptr;

		return MakeBoundStatement_If(pCondition, pIfBlock, pElseifs, pElseBlock);
	}

	BoundStatement* BindForStatement(Statement* pFor)
	{
		ForStatement& forstmt = pFor->forstmt;

		BoundExpression* pLowerBound = BindExpression(forstmt.LowerBound, My_Defaults.IntType);
		BoundExpression* pUpperBound = BindExpression(forstmt.UpperBound, My_Defaults.IntType);
		BoundExpression* pStep = nullptr;
		{
			if (forstmt.Step)
			{
				pStep = BindExpression(forstmt.Step, My_Defaults.IntType);
			}
			else
			{
				pStep = MakeBoundExpression_Literal(My_Defaults.IntType, MakeValue_Int64(1ll));
			}
		}

		BoundScope* pLocalScope = Allocator::Create<BoundScope>(Allocator::Stage::Binder, m_Scope);
		m_Scope = pLocalScope;

		char* const& lpName = forstmt.Identifier.Id;
		MySymbol* pVariable = MakeSymbol_Variable(lpName, My_Defaults.IntType, false, m_Function != nullptr);

		if (!m_Scope->DeclareVariable(pVariable))
		{
			m_Diagnostics.ReportSymbolRedeclaration(GetLocation(forstmt.Identifier), lpName);
		}

		auto[pBody, BreakLabel, ContinueLabel] = BindLoopBody(forstmt.Body);
		m_Scope = m_Scope->Parent;

		return MakeBoundStatement_For(pVariable, pLowerBound, pUpperBound, pStep, pBody, BreakLabel, ContinueLabel);
	}

	BoundStatement* BindForeachStatement(Statement* pForeach)
	{
		MY_NOT_IMPLEMENTED();
		return nullptr;
	}

	BoundStatement* BindWhileStatement(Statement* pWhile)
	{
		WhileStatement& whilestmt = pWhile->whilestmt;

		BoundExpression* pCondition = BindExpression(whilestmt.Condition);
		auto[pBody, BreakLabel, ContinueLabel] = BindLoopBody(whilestmt.Body);

		return MakeBoundStatement_While(pCondition, pBody, false, BreakLabel, ContinueLabel);
	}
	
	BoundStatement* BindDoWhileStatement(Statement* pDoWhile)
	{
		DoWhileStatement& dowhile = pDoWhile->dowhile;

		BoundExpression* pCondition = BindExpression(dowhile.Condition);
		auto[pBody, BreakLabel, ContinueLabel] = BindLoopBody(dowhile.Body);

		return MakeBoundStatement_While(pCondition, pBody, true, BreakLabel, ContinueLabel);
	}

	BoundStatement* BindBreakStatement(Statement* pBreak)
	{
		if (!stbds_arrlenu(m_LoopStack))
		{
			m_Diagnostics.ReportInvalidBreakOrContinue(GetLocation(pBreak->breakstmt.BreakKeyword), "break");
			return BindErrorStatement();
		}
		const auto&[Break, _] = stbds_arrlast(m_LoopStack);
		return MakeBoundStatement_Goto(Break);
	}

	BoundStatement* BindContinueStatement(Statement* pContinue)
	{
		if (!stbds_arrlenu(m_LoopStack))
		{
			m_Diagnostics.ReportInvalidBreakOrContinue(GetLocation(pContinue->continuestmt.ContinueKeyword), "continue");
			return BindErrorStatement();
		}
		const auto& [_, Continue] = stbds_arrlast(m_LoopStack);
		return MakeBoundStatement_Goto(Continue);
	}

	BoundStatement* BindReturnStatement(Statement* pReturn)
	{
		ReturnStatement& ret = pReturn->returnstmt;

		BoundExpression* pExpression = ret.Expr ? BindExpression(ret.Expr) : nullptr;
		if (!m_Function)
		{
			m_Diagnostics.ReportInvalidReturn(GetLocation(ret.ReturnKeyword));
		}
		else
		{
			FunctionSymbol& fs = m_Function->funcsym;
			MyType* pReturnType = fs.Type->Signature->Return;

			if (pReturnType == My_Defaults.VoidType)
			{
				if (pExpression)
				{
					m_Diagnostics.ReportInvalidReturnExpression(GetLocation(ret.Expr), m_Function->Name);
				}
			}
			else
			{
				if (!pExpression)
				{
					m_Diagnostics.ReportMissingReturnExpression(GetLocation(ret.ReturnKeyword), m_Function->Name);
				}
				else
				{
					pExpression = BindConversion(GetLocation(ret.Expr), pExpression, pReturnType);
				}
			}
		}

		return MakeBoundStatement_Return(pExpression);
	}

	BoundStatement* BindErrorStatement()
	{
		return MakeBoundStatement_Expression(MakeBoundExpression_Error());
	}

	Tuple<BoundStatement*, BoundLabel, BoundLabel> BindLoopBody(Statement* pBody)
	{
		static uint32_t iLoopBodyCounter = 0;

		BoundLabel BreakLabel    = BoundLabel{ MyGetCachedStringV("__break_%u",    iLoopBodyCounter) };
		BoundLabel ContinueLabel = BoundLabel{ MyGetCachedStringV("__continue_%u", iLoopBodyCounter) };
		iLoopBodyCounter++;

		Pair<BoundLabel, BoundLabel> loop = { BreakLabel, ContinueLabel };
		stbds_arrpush(m_LoopStack, loop);
		BoundStatement* pBoundBody = BindStatement(pBody);
		stbds_arrpop(m_LoopStack);

		return Tuple<BoundStatement*, BoundLabel, BoundLabel>{ pBoundBody, BreakLabel, ContinueLabel };
	}

	Tuple<MySymbol*, BoundExpression*> BindVariableDeclaration(VariableDeclaration& VarDecl) noexcept
	{
		const Token& Name = VarDecl.Identifier;
		if (const auto[bFound, pFun] = m_Scope->LookupFunction(Name.Id); bFound)
		{
			m_Diagnostics.ReportSymbolRedeclaredDifferently(GetLocation(Name), Name.Id);
			return {};
		}

		BoundExpression* pValue = nullptr;
		MyType*          pType  = nullptr;
		MySymbol*        pVariable = nullptr;
		const bool       bIsLocal  = m_Function != nullptr;

		if (!(pType = BindTypeSpec(VarDecl.Type)))
		{
			m_Diagnostics.ReportUnknownType(GetLocation(VarDecl.Type), GetTypeName(VarDecl.Type));
			goto Error;
		}

		if (VarDecl.Value)
		{
			pValue = BindExpression(VarDecl.Value);
			pValue = BindConversion(GetLocation(VarDecl.Value), pValue, pType);
		}
		else
		{
			const TextLocation Location = GetLocation(Name);

			if (VarDecl.IsReadonly)
			{
				m_Diagnostics.ReportUninitializedReadonlyVariable(Location, Name);
				goto Error;
			}
			else
			{
				const MyValue Value = MyTypeIsReference(pType) ? MakeValue_Null() : MakeValue_Uint64(0ull);
				pValue = MakeBoundExpression_Literal(pType, Value);
			}
		}
		if (!pValue)
		{
			goto Error;
		}

		pVariable = MakeSymbol_Variable(Name.Id, pType, VarDecl.IsReadonly, bIsLocal);

		if (!m_Scope->DeclareVariable(pVariable))
		{
			m_Diagnostics.ReportSymbolRedeclaration(GetLocation(Name), Name.Id);
			goto Error;
		}

		if (!bIsLocal)
		{
			stbds_hmput(m_Globals, pVariable, pValue);
		}

		return { pVariable, pValue };

	Error:
		return { nullptr, MakeBoundExpression_Error() };
	}

	/// Declarations
	void BindFunctionSignature(const FunctionSignature& Signature, Declaration* pDecl)
	{
		BindFunctionSignature(this, Signature, pDecl);
	}

	void BindImportDeclaration(Declaration* pImport)
	{
		MY_NOT_IMPLEMENTED();
	}

	void BindUsingDeclaration(Declaration* pTypedef)
	{
		UsingDeclaration& usingdecl = pTypedef->usingdecl;

		if (const auto [bFound, pFun] = m_Scope->LookupFunction(usingdecl.Name.Id); bFound)
		{
			m_Diagnostics.ReportSymbolRedeclaredDifferently(GetLocation(usingdecl.Name), usingdecl.Name.Id);
			return;
		}

		char* const& lpName = usingdecl.Name.Id;
		MyType* pType = nullptr;

		if (!(pType = BindTypeSpec(usingdecl.Type)))
		{
			m_Diagnostics.ReportUnknownType(GetLocation(usingdecl.Type), GetTypeName(usingdecl.Type));
			goto Error;
		}

		if (stbds_shget(s_UserDefinedTypes, lpName) != nullptr ||
			stbds_shget(s_TypedefedTypes, lpName) != nullptr)
		{
			m_Diagnostics.ReportSymbolRedeclaration(GetLocation(usingdecl.Name), lpName);
			goto Error;
		}

		stbds_shput(s_TypedefedTypes, lpName, pType);

	Error:
		return;
	}

	void BindExternDeclaration(Declaration* pExtern)
	{
		ExternDeclaration& externdecl = pExtern->externdecl;
		BindFunctionSignature(this, externdecl.Signature, nullptr);
	}

	void BindEnumDeclaration(Declaration* pEnum)
	{
		MY_NOT_IMPLEMENTED();
	}

	void BindBitFlagsDeclaration(Declaration* pBflags)
	{
		MY_NOT_IMPLEMENTED();
	}

	void BindVariableDeclaration(Declaration* pVar)
	{
		const auto[pSymbol, pValue] = BindVariableDeclaration(pVar->vardecl);
		(void)pSymbol;
		(void)pValue;
	}
	
	void BindFunctionDeclaration(Declaration* pFunc)
	{
		FunctionDeclaration& funcdecl = pFunc->funcdecl;
		BindFunctionSignature(this, funcdecl.Signature, pFunc);
	}

	void BindForwardDeclaration(Declaration* pForward)
	{
		ForwardDeclaration& forward = pForward->forward;

		if (const auto [bFound, _] = m_Scope->LookupStruct(forward.Name.Id); bFound)
		{
			m_Diagnostics.ReportSymbolRedeclaration(GetLocation(forward.Name), forward.Name.Id);
			goto Error;
		}
		if (const auto [bFound, _] = m_Scope->LookupFunction(forward.Name.Id); bFound)
		{
			m_Diagnostics.ReportSymbolRedeclaredDifferently(GetLocation(forward.Name), forward.Name.Id);
			goto Error;
		}

		if (MyType* const& pType = stbds_shget(s_ForwardedTypes, forward.Name.Id); pType != nullptr)
		{
			FieldSymbol* pFields = nullptr;
			for (size_t k = 0; k < stbds_arrlenu(pType->Klass->Fields); k++)
			{
				MyField* pField = pType->Klass->Fields + k;
				FieldSymbol fs = { pField->Name, pField->Type, nullptr };
				stbds_arrpush(pFields, fs);
			}

			MySymbol* pSymbol = MakeSymbol_Struct(pType->Klass->Name, pType, pFields, nullptr);

			if (m_Scope->DeclareStruct(pSymbol))
			{
				stbds_shdel(s_ForwardedTypes, pType->Klass->Name);
				stbds_shput(s_UserDefinedTypes, pType->Klass->Name, pType);
			}
		}

	Error:
		return;
	}

	void BindStructDeclaration(Declaration* pStruct)
	{
		// TODO: Not *fully* complete
		StructDeclaration& structdecl = pStruct->structdecl;

		const Token& Name = structdecl.Name;

		if (const auto [bFound, _] = m_Scope->LookupFunction(Name.Id); bFound)
		{
			m_Diagnostics.ReportSymbolRedeclaredDifferently(GetLocation(Name), Name.Id);
			goto Error;
		}

		FieldSymbol* pFields = nullptr;

		uint32_t kAttribs = 0ul;
		if (structdecl.PodKeyword != nullptr)
		{
			kAttribs |= MY_STRUCT_ATTR_POD;
		}

		MyStruct* pKlass = MyStructCreate(s_Context, Name.Id, kAttribs);
		MyType* pType = MyTypeCreate(MY_TYPE_KIND_STRUCT, pKlass, MY_TYPE_ATTR_NONE);

		// Binding the members while binding the struct declaration means that, we can't have fields
		// with types we haven't yet encountered
		for (size_t k = 0; k < stbds_arrlenu(structdecl.Members); k++)
		{
			VariableDeclarationStatement& vds = structdecl.Members[k]->var;
			MyType* pMemberType = BindTypeSpec(vds.Type);
			if (!pMemberType)
			{
				m_Diagnostics.ReportUnknownType(GetLocation(vds.Type), GetTypeName(vds.Type));
				goto Error;
			}
			uint32_t kFieldAttribs = 0ul;
			if (vds.IsReadonly)
			{
				kFieldAttribs |= MY_FIELD_ATTR_CONST;
			}

			MyStruct* pMemberKlass = GetStructFromType(pMemberType);
			MyStructAddField(pKlass, vds.Identifier.Id, pMemberType, kFieldAttribs);

			FieldSymbol fs = { vds.Identifier.Id, pMemberType, nullptr };
			stbds_arrpush(pFields, fs);
		}
		if (!pFields)
		{
			// Empty struct
			char* const lpName = MyGetCachedString("__padding");
			MyStructAddField(pKlass, lpName, My_Defaults.IntType, MY_FIELD_ATTR_CONST);
		}

		// Create the struct symbol and declare it before binding the functions
		{
			MySymbol* pSymbol = MakeSymbol_Struct(Name.Id, pType, pFields, pStruct);
			if (!m_Scope->DeclareStruct(pSymbol))
			{
				m_Diagnostics.ReportSymbolRedeclaration(GetLocation(Name), Name.Id);
				goto Error;
			}

			stbds_shput(s_UserDefinedTypes, Name.Id, pType);
		}
		
		// Step 1: Bind the method signatures
		for (size_t k = 0; k < stbds_arrlenu(structdecl.Methods); k++)
		{
			Declaration* pMethodDecl = structdecl.Methods[k];
			FunctionSignature& fs = pMethodDecl->funcdecl.Signature;

			bool bIsCtor = fs.Name.Id == pKlass->Name;

			MyType* pReturn = BindTypeSpec(fs.Return);
			if (!pReturn)
			{
				m_Diagnostics.ReportUnknownType(GetLocation(fs.Return), GetTypeName(fs.Return));
				return;
			}

			MySymbol** ppParamSyms = nullptr;
			MyType** ppParams = nullptr;
			for (size_t k = 0; k < stbds_arrlenu(fs.Params); k++)
			{
				const Parameter& param = fs.Params[k];
				MyType* pParam = BindTypeSpec(param.Type);
				if (!pParam)
				{
					m_Diagnostics.ReportUnknownType(GetLocation(param.Type), GetTypeName(param.Type));
					return;
				}
				stbds_arrpush(ppParams, pParam);

				MySymbol* pParamSym = MakeSymbol_Parameter(param.Name.Id, pParam, param.ConstKeyword != nullptr, true);
				stbds_arrpush(ppParamSyms, pParamSym);
			}

			uint32_t kMethodFlags = MY_FUNC_ATTR_METHOD;
			if (bIsCtor) { kMethodFlags |= MY_FUNC_ATTR_CTOR; }
			if (fs.StaticKeyword) { kMethodFlags |= MY_FUNC_ATTR_STATIC; }
			if (fs.InlineKeyword) { kMethodFlags |= MY_FUNC_ATTR_INLINE; }
			if (fs.NoGCKeyword) { kMethodFlags |= MY_FUNC_ATTR_NOGC; }

			MyType* pMethodType = MyTypeCreate(
				MY_TYPE_KIND_FUNCTION,
				new MyFunctionSignature{ fs.Name.Id, pReturn, ppParams },
				kMethodFlags
			);

			MyMethod* pMethod = MyMethodCreate(pKlass, fs.Name.Id, pMethodType, kMethodFlags, MY_INVALID_ADDR, bIsCtor); // Adds the method to 'pKlass->Methods'
			{
				if (fs.StaticKeyword == nullptr)
				{
					MySymbol* pSelfParam = MakeSymbol_Parameter(MyGetCachedString("this"), pType, false, true);
					stbds_arrins(ppParamSyms, 0, pSelfParam);
				}

				MySymbol* pFunction = MakeSymbol_Function(
					pMethod->Fullname,
					pMethodType,
					ppParamSyms,
					fs.StaticKeyword != nullptr,
					fs.InlineKeyword != nullptr,
					fs.NoGCKeyword != nullptr,
					pMethodDecl
				);

				if (!m_Scope->DeclareFunction(pFunction))
				{
					m_Diagnostics.ReportSymbolRedeclaration(GetLocation(fs.Name), fs.Name.Id);
					return;
				}
			}
		}

	Error:
		return;
	}

	/// Types
	MyType* BindNameTypeSpecifier(TypeSpec* pTypeSpec)
	{
		NameTypeSpec& nts = pTypeSpec->name;
		return LookupType(nts.Name.Id);
	}

	MyType* BindArrayTypeSpecifier(TypeSpec* pTypeSpec)
	{
		ArrayTypeSpec& ats = pTypeSpec->array;

		MyType* pBaseType = BindTypeSpec(ats.Type);
		if (pBaseType == My_Defaults.ErrorType || (pBaseType->Kind != 0u && pBaseType->Kind != 2u))
		{
			return My_Defaults.ErrorType;
		}

		// TODO: Get array size from constant, and throw error if count cannot be folded
		uint32_t* pLengths = nullptr;
		if (ats.Counts)
		{
			for (size_t k = 0; k < stbds_arrlenu(ats.Counts); k++)
			{
				if (ats.Counts[k])
				{
					BoundExpression* pCount = BindExpression(ats.Counts[k]);
					if (pCount->Kind != BoundExpressionKind::Literal)
					{
						m_Diagnostics.ReportExpectedNumberToken(GetLocation(ats.Counts[k]));
						goto Error;
					}
					if (pCount->Type() != My_Defaults.UintType && pCount->Type() != My_Defaults.IntType)
					{
						m_Diagnostics.ReportIllegalTypeConversion(GetLocation(ats.Counts[k]), pCount->Type(), My_Defaults.UintType);
						goto Error;
					}

					const uint32_t kLength = (uint32_t)pCount->literal.Value.U64;
					stbds_arrpush(pLengths, kLength);
				}
				else
				{
					stbds_arrpush(pLengths, 0ul);
				}
			}
		}
		else
		{
			// It is just a one dimensional array
			stbds_arrpush(pLengths, 0ul);
		}

		return MyTypeCreate(1u, new MyArrayType{ pBaseType->Klass, pLengths }, 0ul);

	Error:
		return My_Defaults.ErrorType;
	}

	MyType* BindFunctionTypeSpecifier(TypeSpec* pTypeSpec)
	{
		//MY_NOT_IMPLEMENTED();
		FunctionTypeSpec& fts = pTypeSpec->func;

		MyType* pReturn = BindTypeSpec(fts.Return);
		if (!pReturn || pReturn == My_Defaults.ErrorType)
		{
			return My_Defaults.ErrorType;
		}

		MyType** ppParameters = nullptr;
		for (size_t k = 0; k < stbds_arrlenu(fts.Parameters); k++)
		{
			MyType* pParameter = BindTypeSpec(fts.Parameters[k]);
			if (!pParameter || pParameter == My_Defaults.ErrorType)
			{
				return My_Defaults.ErrorType;
			}
			stbds_arrpush(ppParameters, pParameter);
		}

		return MyTypeCreate(MY_TYPE_KIND_FUNCTION, new MyFunctionSignature{ nullptr, pReturn, ppParameters });
	}

	MyType* LookupType(char* const& lpName)
	{
		static struct
		{
			const char* const Name = nullptr;
			MyType* const&   Type  = nullptr;
		} s_TypeMap[] =
		{
			{ "void",   My_Defaults.VoidType    },
			{ "object", My_Defaults.ObjectType  },
			{ "bool",   My_Defaults.BooleanType },
			{ "int",    My_Defaults.IntType     },
			{ "uint",   My_Defaults.UintType    },
			{ "intptr", My_Defaults.IntPtrType  },
			{ "float",  My_Defaults.FloatType   },
			{ "string", My_Defaults.StringType  },
		};

		for (const auto& kvp : s_TypeMap)
		{
			if (strcmp(kvp.Name, lpName) == 0)
			{
				return kvp.Type;
			}
		}

		if (MyType* pType = stbds_shget(s_UserDefinedTypes, lpName); pType != nullptr)
		{
			return pType;
		}

		if (MyType* pType = stbds_shget(s_TypedefedTypes, lpName); pType != nullptr)
		{
			return pType;
		}
		
		return nullptr;
	}

	/// Utilities
private:
	TextLocation GetLocation(const Token& Token) noexcept
	{
		const SourceText& Text = m_Tree->GetText();
		return TextLocation(Token.Start, Token.End - Token.Start, Text.GetLineIndex(Token.Start), Text.Filename);
	}

	TextLocation GetLocation(TypeSpec* const& pTypeSpec) noexcept
	{
		const SourceText& Text = m_Tree->GetText();
		int32_t iStart = GetStart(pTypeSpec);
		int32_t iEnd = GetEnd(pTypeSpec);
		return TextLocation(iStart, iEnd - iStart, Text.GetLineIndex(iStart), Text.Filename);
	}

	TextLocation GetLocation(Expression* const& pExpression) noexcept
	{
		const SourceText& Text = m_Tree->GetText();
		int32_t iStart = GetStart(pExpression);
		int32_t iEnd = GetEnd(pExpression);
		return TextLocation(iStart, iEnd - iStart, Text.GetLineIndex(iStart), Text.Filename);
	}

private:
	static void BindFunctionSignature(InternalBinder* const& pBinder, const FunctionSignature& Signature, Declaration* pDecl) noexcept
	{
		if (const auto [bFound, pFun] = pBinder->m_Scope->LookupFunction(Signature.Name.Id); bFound)
		{
			pBinder->m_Diagnostics.ReportSymbolRedeclaredDifferently(pBinder->GetLocation(Signature.Name), Signature.Name.Id);
			return;
		}

		MyType** ppParamTypes = nullptr;
		MySymbol** ppParamSyms = nullptr;
		Pair<char*, bool>* pSeenParams = nullptr;
		{
			static constexpr Pair<char*, bool> sp = {};
			stbds_hmdefault(pSeenParams, false);
			stbds_hmdefaults(pSeenParams, sp);
		}

		MyType* pReturnType = nullptr;
		MySymbol* pFunction = nullptr;

		if (!(pReturnType = pBinder->BindTypeSpec(Signature.Return)))
		{
			pBinder->m_Diagnostics.ReportUnknownType(pBinder->GetLocation(Signature.Return), GetTypeName(Signature.Return));
			return;
		}

		for (size_t k = 0; k < stbds_arrlenu(Signature.Params); k++)
		{
			const Parameter& param = Signature.Params[k];

			MyType* pType = pBinder->BindTypeSpec(param.Type);
			if (!pType)
			{
				pBinder->m_Diagnostics.ReportUnknownType(pBinder->GetLocation(param.Type), GetTypeName(param.Type));
				return;
			}

			MySymbol* pParam = MakeSymbol_Parameter(param.Name.Id, pType, param.ConstKeyword != nullptr, true);

			if (stbds_hmget(pSeenParams, param.Name.Id) == false)
			{
				stbds_hmput(pSeenParams, param.Name.Id, true);
				stbds_arrpush(ppParamTypes, pType);
				stbds_arrpush(ppParamSyms, pParam);
			}
			else
			{
				pBinder->m_Diagnostics.ReportParameterRedeclaration(pBinder->GetLocation(param.Name), param.Name.Id);
				return;
			}
		}

		uint32_t kFlags = 0u;
		if (Signature.InlineKeyword != nullptr) { kFlags |= MY_FUNC_ATTR_INLINE; }
		if (Signature.StaticKeyword != nullptr) { kFlags |= MY_FUNC_ATTR_STATIC; }
		if (Signature.NoGCKeyword != nullptr) { kFlags |= MY_FUNC_ATTR_NOGC; }

		pFunction = MakeSymbol_Function(
			Signature.Name.Id,
			MyTypeCreate(
				MY_TYPE_KIND_FUNCTION,
				new MyFunctionSignature{ Signature.Name.Id, pReturnType, ppParamTypes },
				kFlags
			),
			ppParamSyms,
			Signature.StaticKeyword != nullptr,
			Signature.InlineKeyword != nullptr,
			Signature.NoGCKeyword != nullptr,
			pDecl
		);

		if (!pBinder->m_Scope->DeclareFunction(pFunction))
		{
			pBinder->m_Diagnostics.ReportSymbolRedeclaration(pBinder->GetLocation(Signature.Name), Signature.Name.Id);
		}
	}

	static int32_t GetStart(const TypeSpec* const& pTypeSpec) noexcept
	{
		switch (pTypeSpec->Kind)
		{
			case TypeSpecKind::Name:
				return pTypeSpec->name.Name.Start;
			case TypeSpecKind::Array:
				return GetStart(pTypeSpec->array.Type);
			case TypeSpecKind::Function:
				return pTypeSpec->func.CallbackKeyword.Start;
			default: break;
		}

		MY_ASSERT(false, "SeverelyFatalError: Should NEVER reach HERE");
		return -1;
	}

	static int32_t GetStart(const Expression* const& pExpression) noexcept
	{
		switch (pExpression->Kind)
		{
			case ExpressionKind::Literal:
				return pExpression->literal.Literal.Start;
			case ExpressionKind::Unary:
				return pExpression->unary.Operator.Start;
			case ExpressionKind::Binary:
				return GetStart(pExpression->binary.Lhs);
			case ExpressionKind::Ternary:
				return GetStart(pExpression->ternary.Condition);
			case ExpressionKind::Parenthesized:
				return pExpression->paren.LParenToken.Start;
			case ExpressionKind::Name:
				return pExpression->name.Identifier.Start;
			case ExpressionKind::Assignment:
				return GetStart(pExpression->assign.Lhs);
			case ExpressionKind::OperatorNew:
				return pExpression->opnew.NewKeyword.Start;
			case ExpressionKind::Call:
				return GetStart(pExpression->call.Callable);
			case ExpressionKind::Index:
				return GetStart(pExpression->index.Sequence);
			case ExpressionKind::Field:
				return GetStart(pExpression->field.Object);
			case ExpressionKind::Array:
				return pExpression->array.LbraceToken.Start;
			case ExpressionKind::Cast:
				return pExpression->cast.LparenToken.Start;
			default: break;
		}

		MY_ASSERT(false, "SeverelyFatalError: Should NEVER reach HERE");
		return -1;
	}

	static int32_t GetEnd(const TypeSpec* const& pTypeSpec) noexcept
	{
		switch (pTypeSpec->Kind)
		{
			case TypeSpecKind::Name:
				return pTypeSpec->name.Name.End;
			case TypeSpecKind::Array:
				return pTypeSpec->array.RbracketToken.End;
			case TypeSpecKind::Function:
				return pTypeSpec->func.RparenToken.End;
			default: break;
		}

		MY_ASSERT(false, "SeverelyFatalError: Should NEVER reach HERE");
		return -1;
	}

	static int32_t GetEnd(const Expression* const& pExpression) noexcept
	{
		switch (pExpression->Kind)
		{
			case ExpressionKind::Literal:
				return pExpression->literal.Literal.End;
			case ExpressionKind::Unary:
				return GetEnd(pExpression->unary.Rhs);
			case ExpressionKind::Binary:
				return GetEnd(pExpression->binary.Rhs);
			case ExpressionKind::Ternary:
				return GetEnd(pExpression->ternary.Else);
			case ExpressionKind::Parenthesized:
				return GetEnd(pExpression->paren.Expr);
			case ExpressionKind::Name:
				return pExpression->name.Identifier.End;
			case ExpressionKind::Assignment:
				return GetEnd(pExpression->assign.Rhs);
			case ExpressionKind::OperatorNew:
				return pExpression->opnew.RparenToken.End;
			case ExpressionKind::Call:
				return pExpression->call.RparenToken.End;
			case ExpressionKind::Index:
				return pExpression->index.RBracketToken.End;
			case ExpressionKind::Field:
				return pExpression->field.Field.End;
			case ExpressionKind::Array:
				return pExpression->array.RbraceToken.End;
			case ExpressionKind::Cast:
				return GetEnd(pExpression->cast.Expr);
			default: break;
		}

		MY_ASSERT(false, "SeverelyFatalError: Should NEVER reach HERE");
		return -1;
	}

	static char* const GetTypeName(TypeSpec* const& pTypeSpec) noexcept
	{
		switch (pTypeSpec->Kind)
		{
			case TypeSpecKind::Name:
			{
				return pTypeSpec->name.Name.Id;
			}
			case TypeSpecKind::Array:
			{
				const char* const lpBaseTypename = GetTypeName(pTypeSpec->array.Type);
				const char* lpShape = nullptr;
				switch (stbds_arrlenu(pTypeSpec->array.Counts))
				{
					case 0: lpShape = "[]"; break;
					case 1: lpShape = "[]"; break;
					case 2: lpShape = "[,]"; break;
					case 3: lpShape = "[,,]"; break;
					case 4: lpShape = "[,,,]"; break;
					default: break;
				}
				return MyGetCachedStringV("%s%s", lpBaseTypename, lpShape);
			}
			case TypeSpecKind::Function:
			{
				const char* lpAllParamsString = "";
				const size_t kCount = stbds_arrlenu(pTypeSpec->func.Parameters);
				for (size_t k = 0; k < kCount; k++)
				{
					const char* lpParamString = MyGetCachedStringV("%s%s", GetTypeName(pTypeSpec->func.Parameters[k]), k == kCount - 1 ? "" : ", ");
					lpAllParamsString = MyGetCachedStringV("%s%s", lpAllParamsString, lpParamString);
				}
				return MyGetCachedStringV("%s(%s)", GetTypeName(pTypeSpec->func.Return), lpAllParamsString);
			}
			default: break;
		}
		
		MY_ASSERT(false, "SeverelyFatalError: Should NEVER reach HERE");
		return nullptr;
	}

	static MyType* GetTypeFromStruct(MyStruct* const& pKlass) noexcept
	{
		MyDefaults& md = My_Defaults;

		if (pKlass == md.ObjectStruct)
		{
			return md.ObjectType;
		}
		if (pKlass == md.BooleanStruct)
		{
			return md.BooleanType;
		}
		if (pKlass == md.IntStruct)
		{
			return md.IntType;
		}
		if (pKlass == md.UintStruct)
		{
			return md.UintType;
		}
		if (pKlass == md.IntPtrStruct)
		{
			return md.IntPtrType;
		}
		if (pKlass == md.FloatStruct)
		{
			return md.FloatType;
		}
		if (pKlass == md.ComplexStruct)
		{
			return md.ComplexType;
		}
		if (pKlass == md.StringStruct)
		{
			return md.StringType;
		}
		if (pKlass == md.StringBuilderStruct)
		{
			return md.StringBuilderType;
		}
		if (pKlass == md.FileStruct)
		{
			return md.FileType;
		}
		if (pKlass == md.MathStruct)
		{
			return md.MathType;
		}

		for (size_t k = 0; k < stbds_hmlenu(s_UserDefinedTypes); k++)
		{
			const auto&[lpName, pType] = s_UserDefinedTypes[k];
			if (pKlass == pType->Klass)
			{
				return pType;
			}
		}

		return nullptr;
	}

	static MyStruct* GetStructFromType(MyType* const& pType) noexcept
	{
		switch (pType->Kind)
		{
			case MY_TYPE_KIND_STRUCT:   return pType->Klass;
			case MY_TYPE_KIND_ARRAY:    return pType->Array->Klass;
			case MY_TYPE_KIND_FUNCTION: return MY_NOT_IMPLEMENTED(), nullptr;
			default: break;
		}
		MY_ASSERT(false, "Error: Unknown type (kind: %u)", pType->Kind);
		return nullptr;
	}

private:
	static void InitBuiltinMethodSymbols(BoundScope* pScope) noexcept
	{
		MyDefaults& md = My_Defaults;

		static auto SetupMethod = [&pScope](MyType* pKlassType, const char* lpName, MyType* pReturn, List<Pair<const char*, MyType*>> Params, bool bIsStatic = false) -> void
		{
			static char* const lpThisString = MyGetCachedString("this");

			char* const lpMethodName = MyGetCachedString(lpName);

			MyType** ppParamTypes = nullptr;
			MySymbol** ppParamsSymbols = nullptr;
			if (!bIsStatic)
			{
				stbds_arrpush(ppParamTypes, pKlassType);
				stbds_arrpush(ppParamsSymbols, MakeSymbol_Parameter(lpThisString, pKlassType, true, true));
			}
			for (const auto&[arg, type] : Params)
			{
				char* const lpArgName = MyGetCachedString(arg);
				MySymbol* pSymbol = MakeSymbol_Parameter(lpArgName, type, true, true);

				stbds_arrpush(ppParamTypes, type);
				stbds_arrpush(ppParamsSymbols, pSymbol);
			}

			uint32_t kMethodFlags = MY_FUNC_ATTR_METHOD;
			if (bIsStatic)
			{
				kMethodFlags |= MY_FUNC_ATTR_STATIC;
			}

			MyMethod* pMethod = MyMethodCreate(
				pKlassType->Klass,
				lpMethodName,
				MyTypeCreate(
					MY_TYPE_KIND_FUNCTION,
					new MyFunctionSignature{ lpMethodName, pReturn, ppParamTypes }
				),
				kMethodFlags
			); // Adds the method to 'pKlassType->Klass->Methods'
			

			MySymbol* pMethodSymbol = MakeSymbol_Function(pMethod->Fullname, pMethod->Type, ppParamsSymbols);
			pScope->DeclareFunction(pMethodSymbol);
		};

		static auto ArrayType = [](MyType* pBaseType) -> MyType*
		{
			uint32_t* pLengths = nullptr;
			stbds_arrpush(pLengths, 0ul);

			MyType* pArrayType = MyTypeCreate(MY_TYPE_KIND_ARRAY, new MyArrayType{ pBaseType->Klass, pLengths });
			return pArrayType;
		};

		static MyType* const pObjectArray = ArrayType(md.ObjectType);
		static MyType* const pStringArray = ArrayType(md.StringType);

		// Complex
		{
			//
		}

		/// String
		SetupMethod(md.StringType, "Length",     md.UintType,    { });
		SetupMethod(md.StringType, "Find",       md.UintType,    { { "sSubstr", md.StringType } });
		SetupMethod(md.StringType, "Substr",     md.StringType,  { { "kOffset", md.UintType   }, { "kLength", md.UintType   } });
		SetupMethod(md.StringType, "Split",      pStringArray,   { { "sSep",    md.StringType } });
		SetupMethod(md.StringType, "StartsWith", md.BooleanType, { { "sPrefix", md.StringType } });
		SetupMethod(md.StringType, "EndsWith",   md.BooleanType, { { "sSuffix", md.StringType } });
		SetupMethod(md.StringType, "ToUpper",    md.StringType,  { });
		SetupMethod(md.StringType, "ToLower",    md.StringType,  { });
		SetupMethod(md.StringType, "ParseInt",   md.IntType,     { });
		SetupMethod(md.StringType, "ParseUint",  md.UintType,    { });
		SetupMethod(md.StringType, "ParseFloat", md.FloatType,   { });

		/// StringBuilder
		SetupMethod(md.StringBuilderType, "Init",       md.VoidType,   { });
		SetupMethod(md.StringBuilderType, "Append",     md.VoidType,   { { "sString", md.StringType } });
		SetupMethod(md.StringBuilderType, "AppendV",    md.VoidType,   { { "sFormat", md.StringType }, { "vArgs", pObjectArray } });
		SetupMethod(md.StringBuilderType, "Write",      md.VoidType,   { { "sString", md.StringType } });
		SetupMethod(md.StringBuilderType, "WriteV",     md.VoidType,   { { "sString", md.StringType }, { "vArgs", pObjectArray } });
		SetupMethod(md.StringBuilderType, "WriteLine",  md.VoidType,   { { "sString", md.StringType } });
		SetupMethod(md.StringBuilderType, "WriteLineV", md.VoidType,   { { "sString", md.StringType }, { "vArgs", pObjectArray } });
		SetupMethod(md.StringBuilderType, "ToString",   md.StringType, { });

		// Bytes
		SetupMethod(md.BytesType, "Init",             md.VoidType,   { });
		SetupMethod(md.BytesType, "Free",             md.VoidType,   { });
		SetupMethod(md.BytesType, "AddI32",           md.VoidType,   { { "iValue", md.IntType    } });
		SetupMethod(md.BytesType, "AddU32",           md.VoidType,   { { "kValue", md.UintType   } });
		SetupMethod(md.BytesType, "AddF32",           md.VoidType,   { { "fValue", md.FloatType  } });
		SetupMethod(md.BytesType, "AddI64",           md.VoidType,   { { "iValue", md.IntType    } });
		SetupMethod(md.BytesType, "AddU64",           md.VoidType,   { { "kValue", md.UintType   } });
		SetupMethod(md.BytesType, "AddF64",           md.VoidType,   { { "dValue", md.FloatType  } });
		SetupMethod(md.BytesType, "AddString",        md.VoidType,   { { "sValue", md.StringType } });
		SetupMethod(md.BytesType, "Append",           md.VoidType,   { { "pBytes", md.BytesType  } });
		SetupMethod(md.BytesType, "GetBufferPointer", md.IntPtrType, { });

		// File
		SetupMethod(md.FileType, "Close",      md.VoidType,    { });
		SetupMethod(md.FileType, "IsOpen",     md.BooleanType, { });
		SetupMethod(md.FileType, "Read",       md.StringType,  { { "kLength", md.UintType   } });
		SetupMethod(md.FileType, "ReadBytes",  md.IntPtrType,  { { "kLength", md.UintType   } });
		SetupMethod(md.FileType, "Write",      md.VoidType,    { { "pBuffer", md.IntPtrType }, { "kLength", md.UintType } });
		SetupMethod(md.FileType, "WriteBytes", md.VoidType,    { { "pBuffer", md.IntPtrType }, { "kLength", md.UintType } });

		// (static) Math
		SetupMethod(md.MathType, "Abs",   md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Sin",   md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Cos",   md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Tan",   md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Asin",  md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Acos",  md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Atan",  md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Atan2", md.FloatType, { { "y", md.FloatType }, { "x",    md.FloatType } }, true);
		SetupMethod(md.MathType, "Exp",   md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Log",   md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Log10", md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Logb",  md.FloatType, { { "x", md.FloatType }, { "base", md.FloatType } }, true);
		SetupMethod(md.MathType, "Floor", md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Ceil",  md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Sqrt",  md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Cbrt",  md.FloatType, { { "x", md.FloatType } }, true);
		SetupMethod(md.MathType, "Nthrt", md.FloatType, { { "x", md.FloatType }, { "base", md.FloatType } }, true);
		SetupMethod(md.MathType, "Pow",   md.FloatType, { { "x", md.FloatType }, { "y",    md.FloatType } }, true);
	}

	static void InitModuleStd() noexcept
	{
		MY_NOT_IMPLEMENTED();
	}

	static void InitModuleMath() noexcept
	{
		MY_NOT_IMPLEMENTED();
	}

public:
	struct Module
	{
		Pair<MySymbol*, BoundExpression*>* Globals   = nullptr;
		Pair<char*, MySymbol*>*            Functions = nullptr;
		Pair<char*, MySymbol*>*            Structs   = nullptr;
	};

private:
	static MyContext* s_Context;
	static Pair<char*, MyType*>*   s_UserDefinedTypes;
	static Pair<char*, MyType*>*   s_TypedefedTypes;
	static Pair<char*, MyType*>*   s_ForwardedTypes;
	static Pair<char*, MySymbol*>* s_BuiltinMethods;

private:
	// Source code AST
	const SyntaxTree* m_Tree        = nullptr;
	// Scope of the body
	BoundScope*       m_Scope       = nullptr;
	// Function body being bound
	MySymbol*        m_Function    = nullptr;
	// List
	Pair<BoundLabel, BoundLabel>*       m_LoopStack = nullptr;
	// Dictionary
	Pair<MySymbol*, BoundExpression*>* m_Globals   = nullptr;

	DiagnosticBag     m_Diagnostics = { };
};

MyContext* InternalBinder::s_Context = nullptr;
Pair<char*, MyType*>*   InternalBinder::s_UserDefinedTypes = nullptr;
Pair<char*, MyType*>*   InternalBinder::s_TypedefedTypes   = nullptr;
Pair<char*, MyType*>*   InternalBinder::s_ForwardedTypes   = nullptr;
Pair<char*, MySymbol*>* InternalBinder::s_BuiltinMethods   = nullptr;

#pragma endregion

BoundGlobalScope Binder::BindGlobalScope(MyContext* pContext, const SyntaxTree* pTree, const List<MyStruct*>& UserStructs) noexcept
{
	InternalBinder binder = InternalBinder(pContext, pTree);
	binder.BindDeclarations(UserStructs);

	MySymbol** ppVariables = binder.GetScope()->GetDeclaredVariables();
	MySymbol** ppFunctions = binder.GetScope()->GetDeclaredFunctions();
	MySymbol** ppStructs   = binder.GetScope()->GetDeclaredStructs();

	return BoundGlobalScope{ ppVariables, ppFunctions, ppStructs, binder.GetGlobals(), binder.GetDiagnostics() };
}

BoundProgram Binder::BindProgram(MyContext* pContext, BoundGlobalScope* pGlobalScope, const SyntaxTree* pTree) noexcept
{
	using GMap = BoundProgram::GlobalMap;
	using FMap = BoundProgram::FunctionMap;

	GMap* pGlobals = nullptr;
	FMap* pFunctionBodies = nullptr;
	{
		static constexpr BoundProgram::BoundVariable bv = { nullptr, nullptr };
		stbds_shdefault(pGlobals, bv);
		static constexpr BoundProgram::BoundFunction bf = { nullptr, nullptr };
		stbds_shdefault(pFunctionBodies, bf);
	}

	DiagnosticBag Diagnostics = { };

	for (size_t k = 0; k < stbds_hmlenu(pGlobalScope->Globals); k++)
	{
		const auto& [pVar, pValue] = pGlobalScope->Globals[k];

		const BoundProgram::BoundVariable bv = { pVar, pValue };
		stbds_shput(pGlobals, pVar->Name, bv);
	}

	for (size_t k = 0; k < stbds_arrlenu(pGlobalScope->Functions); k++)
	{
		MySymbol* const& pFunction = pGlobalScope->Functions[k];
		FunctionSymbol& fs = pFunction->funcsym;

		// If fs.Decl is null then the function was declared as extern (i.e it doesn't have a body)
		if (fs.Decl && fs.Decl->funcdecl.Body)
		{
			InternalBinder binder = InternalBinder(pContext, pFunction, pTree, pGlobalScope);
			BoundStatement* pBoundBody = binder.BindStatement(fs.Decl->funcdecl.Body);
			BoundStatement* pLoweredBody = Lowerer::Lower(pFunction, pBoundBody);

			if (fs.Type->Signature->Return != My_Defaults.VoidType && !ControlFlowGraph::AllPathsReturn(pLoweredBody))
			{
				binder.m_Diagnostics.ReportAllPathsMustReturn(binder.GetLocation(fs.Decl->funcdecl.Signature.Name));
			}
			Diagnostics.Extend(binder.GetDiagnostics());

			const BoundProgram::BoundFunction bf = { pFunction, pLoweredBody };
			stbds_shput(pFunctionBodies, pFunction->Name, bf);
		}
		else
		{
			// Declrared extern [extern function T F(Args...)]
			const BoundProgram::BoundFunction bf = { pFunction, nullptr };
			stbds_shput(pFunctionBodies, pFunction->Name, bf);
		}
	}

	BoundProgram::BoundFunction& Main = stbds_shget(pFunctionBodies, "Main");
	if (!Main.key || !Main.value)
	{
		Diagnostics.ReportMainIsUndefined();
	}
	else
	{
		MyFunctionSignature* const& pMainType = Main.key->Type->Signature;
		MyType* const& pParam = pMainType->Params[0];

		const bool bMainHasValidReturnType = pMainType->Return == My_Defaults.IntType;
		const bool bMainHasValidParamCount = stbds_arrlenu(pMainType->Params) == 1ull;
		const bool bMainHasValidParamTypes = pParam->Kind == 1ul && pParam->Array->Klass == My_Defaults.StringStruct;

		if (!bMainHasValidReturnType || !bMainHasValidParamCount || !bMainHasValidParamTypes)
		{
			Diagnostics.ReportMainHasInvalidSignature();
		}
	}

	return BoundProgram{ pGlobalScope, pGlobals, pFunctionBodies, InternalBinder::GetUserDefinedTypes(), Diagnostics };
}

BoundUnaryOperator* Binder::BindUnaryOperator(TokenKind OperatorKind, MyType* pRhsType)
{
	return BoundUnaryOperator::Bind(OperatorKind, pRhsType);
}

BoundBinaryOperator* Binder::BindBinaryOperator(TokenKind OperatorKind, MyType* pLhsType, MyType* pRhsType)
{
	return BoundBinaryOperator::Bind(OperatorKind, pLhsType, pRhsType);
}

#pragma region Definitions for Creator Functions
/// Symbols
MySymbol* MakeSymbol_Variable(char* const lpName, MyType* pType, bool bIsReadonly, bool bIsLocal)
{
	MySymbol* pVariable = Allocator::Create<MySymbol>(
		Allocator::Stage::Binder,
		bIsLocal ? SymbolKind::LocalVariable : SymbolKind::GlobalVariable,
		lpName,
		pType
	);
	pVariable->Name = lpName;
	new(&pVariable->varsym) VariableSymbol{ pType, bIsReadonly, bIsLocal };
	return pVariable;
}

MySymbol* MakeSymbol_Parameter(char* const lpName, MyType* pType, bool bIsConst, bool bIsLocal)
{
	MySymbol* pParameter = Allocator::Create<MySymbol>(Allocator::Stage::Binder, SymbolKind::Parameter, lpName, pType);
	pParameter->Name = lpName;
	new(&pParameter->paramsym) ParameterSymbol{ pType, bIsConst, bIsLocal };
	return pParameter;
}

MySymbol* MakeSymbol_Function(
	char* const  lpName,
	MyType*      pType,
	MySymbol**   ppParamSymbols,
	bool         bIsStatic,
	bool         bIsInline,
	bool         bIsNoGC,
	Declaration* pDecl
)
{
	MySymbol* pFunction = Allocator::Create<MySymbol>(Allocator::Stage::Binder, SymbolKind::Function, lpName, pType);
	pFunction->Name = lpName;
	new(&pFunction->funcsym) FunctionSymbol
	{
		pType,
		ppParamSymbols,
		pDecl,
		bIsInline,
		bIsStatic,
		bIsNoGC
	};
	return pFunction;
}

MySymbol* MakeSymbol_Struct(char* const  lpName, MyType* pType, FieldSymbol* pFields, Declaration* pDecl)
{
	MySymbol* pStruct = Allocator::Create<MySymbol>(Allocator::Stage::Binder, SymbolKind::Struct, lpName, pType);
	pStruct->Name = lpName;
	new(&pStruct->varsym) StructSymbol{ pType->Klass, pFields, pDecl };
	return pStruct;
}

/// Expressions
BoundExpression* MakeBoundExpression_Error()
{
	BoundExpression* pError = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Error);
	return pError;
}

BoundExpression* MakeBoundExpression_Empty()
{
	BoundExpression* pEmpty = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Empty);
	return pEmpty;
}

BoundExpression* MakeBoundExpression_Literal(MyType* pType, const MyValue& Value)
{
	BoundExpression* pLiteral = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Literal);
	new(&pLiteral->literal) BoundLiteralExpression{ pType, Value };
	return pLiteral;
}

BoundExpression* MakeBoundExpression_Unary(BoundUnaryOperator* pOperator, BoundExpression* pRhs)
{
	BoundExpression* pUnary = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Unary);
	new(&pUnary->unary) BoundUnaryExpression{ pOperator, pRhs };
	return pUnary;
}

BoundExpression* MakeBoundExpression_Binary(BoundExpression* pLhs, BoundBinaryOperator* pOperator, BoundExpression* pRhs)
{
	BoundExpression* pBinary = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Binary);
	new(&pBinary->binary) BoundBinaryExpression{ pLhs, pOperator, pRhs };
	return pBinary;
}

BoundExpression* MakeBoundExpression_Ternary(BoundExpression* pCondition, BoundExpression* pThen, BoundExpression* pElse)
{
	BoundExpression* pTernary = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Ternary);
	new(&pTernary->ternary) BoundTernaryExpression{ pCondition, pThen, pElse };
	return pTernary;
}

BoundExpression* MakeBoundExpression_Increment(MySymbol* pLvalue, BoundExpression* pIncrement)
{
	BoundExpression* pIncExpr = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Increment);
	new(&pIncExpr->inc) BoundIncrementExpression{ pLvalue, pIncrement };
	return pIncExpr;
}

BoundExpression* MakeBoundExpression_Name(MySymbol* pSymbol)
{
	BoundExpression* pName = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Name);
	new(&pName->name) BoundNameExpression{ pSymbol };
	return pName;
}

BoundExpression* MakeBoundExpression_Assignment(BoundExpression* pLvalue, MySymbol* pVariable, BoundExpression* pRvalue)
{
	BoundExpression* pAssignment = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Assignment);
	new(&pAssignment->assign) BoundAssignmentExpression{ pLvalue, pVariable, pRvalue };
	return pAssignment;
}

BoundExpression* MakeBoundExpression_OperatorNew(MyType* pType, BoundOperatorNewExpression::FieldInitializer* pInitializers)
{
	BoundExpression* pOperatorNew = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::OperatorNew);
	new(&pOperatorNew->opnew) BoundOperatorNewExpression{ pType, pInitializers };
	return pOperatorNew;
}

BoundExpression* MakeBoundExpression_Call(BoundExpression* pCallable, MySymbol* pFunction, BoundExpression** ppArguments)
{
	BoundExpression* pCall = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Call);
	new(&pCall->call) BoundCallExpression{ pCallable, pFunction, ppArguments };
	return pCall;
}

BoundExpression* MakeBoundExpression_Call(BoundExpression* pCallable, MySymbol* pFunction, const std::initializer_list<BoundExpression*>& Arguments)
{
	BoundExpression** ppArgs = nullptr;
	for (BoundExpression* const& pArg : Arguments)
	{
		stbds_arrpush(ppArgs, pArg);
	}
	return MakeBoundExpression_Call(pCallable, pFunction, ppArgs);
}

BoundExpression* MakeBoundExpression_Index(BoundExpression* pSequence, MyType* pItemType, BoundExpression** ppIndices)
{
	BoundExpression* pIndexExpr = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Index);
	new(&pIndexExpr->index) BoundIndexExpression{ pSequence, pItemType, ppIndices };
	return pIndexExpr;
}

BoundExpression* MakeBoundExpression_Field(BoundExpression* pObject, MyType* pFieldType, char* const lpField)
{
	BoundExpression* pField = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Field);
	new(&pField->field) BoundFieldExpression{ pObject, pFieldType, lpField };
	return pField;
}

BoundExpression* MakeBoundExpression_Array(MyType* pType, BoundExpression** ppItems, BoundExpression* pDefault)
{
	BoundExpression* pArray = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Array);
	new(&pArray->array) BoundArrayExpression{ pType, pDefault, ppItems };
	return pArray;
}

BoundExpression* MakeBoundExpression_Instance(MyType* pType, BoundInstanceExpression::MemberMap* pMembers)
{
	BoundExpression* pInstance = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Instance);
	new(&pInstance->inst) BoundInstanceExpression{ pType, pMembers };
	return pInstance;
}

BoundExpression* MakeBoundExpression_Cast(MyType* pType, BoundExpression* pExpression)
{
	BoundExpression* pCast = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Cast);
	new(&pCast->cast) BoundCastExpression{ pType, pExpression };
	return pCast;
}

BoundExpression* MakeBoundExpression_Conversion(MyType* pType, BoundExpression* pExpression)
{
	BoundExpression* pConversion = Allocator::Create<BoundExpression>(Allocator::Stage::Binder, BoundExpressionKind::Conversion);
	new(&pConversion->conv) BoundConversionExpression{ pType, pExpression };
	return pConversion;
}

BoundStatement* MakeBoundStatement_Block(BoundStatement** ppStatements)
{
	BoundStatement* pBlock = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::Block);
	new(&pBlock->block) BoundBlockStatement{ ppStatements };
	return pBlock;
}

BoundStatement* MakeBoundStatement_Block(const std::initializer_list<BoundStatement*>& Statements)
{
	BoundStatement** ppStmts = nullptr;
	for (BoundStatement* const& pStmt : Statements)
	{
		stbds_arrpush(ppStmts, pStmt);
	}
	return MakeBoundStatement_Block(ppStmts);
}

BoundStatement* MakeBoundStatement_Expression(BoundExpression* pExpression)
{
	BoundStatement* pExpr = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::Expression);
	new(&pExpr->expr) BoundExpressionStatement{ pExpression };
	return pExpr;
}

BoundStatement* MakeBoundStatement_VariableDeclaration(MySymbol* pVariable, BoundExpression* pValue)
{
	BoundStatement* pVarDecl = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::VariableDeclaration);
	new(&pVarDecl->vardecl) BoundVariableDeclarationStatement{ pVariable, pValue };
	return pVarDecl;
}

BoundStatement* MakeBoundStatement_DecomposeDeclaration(MySymbol* pStruct, MySymbol** ppVariables, BoundExpression* pDecomposable)
{
	BoundStatement* pDecompDecl = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::DecomposeDeclaration);
	new(&pDecompDecl->decomp) BoundDecomposeDeclarationStatement{ pStruct, ppVariables, pDecomposable };
	return pDecompDecl;
}

BoundStatement* MakeBoundStatement_If(BoundExpression* pCondition, BoundStatement* pIfBlock, ElseIfBlock* pElseifs, BoundStatement* pElseBlock)
{
	BoundStatement* pIf = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::If);
	new(&pIf->ifstmt) BoundIfStatement{ pCondition, pIfBlock, pElseifs, pElseBlock };
	return pIf;
}

BoundStatement* MakeBoundStatement_For(
	MySymbol*        pVariable,
	BoundExpression*  pLowerBound,
	BoundExpression*  pUpperBound,
	BoundExpression*  pStep,
	BoundStatement*   pBody,
	const BoundLabel& BreakLabel,
	const BoundLabel& ContinueLabel
)
{
	BoundStatement* pFor = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::For);
	new(&pFor->forstmt) BoundForStatement
	{
		pVariable,
		pLowerBound,
		pUpperBound,
		pStep,
		BoundLoopStatement
		{
			pBody,
			BreakLabel,
			ContinueLabel
		}
	};
	return pFor;
}

BoundStatement* MakeBoundStatement_Foreach(
	MySymbol*        pVariable,
	BoundExpression*  pIterable,
	BoundStatement*   pBody,
	const BoundLabel& BreakLabel,
	const BoundLabel& ContinueLabel
)
{
	BoundStatement* pForeach = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::Foreach);
	new(&pForeach->foreach) BoundForeachStatement
	{
		pVariable,
		pIterable,
		BoundLoopStatement
		{
			pBody,
			BreakLabel,
			ContinueLabel
		}
	};
	return pForeach;
}

BoundStatement* MakeBoundStatement_While(
	BoundExpression*  pCondition,
	BoundStatement*   pBody,
	bool		      bIsDoWhile,
	const BoundLabel& BreakLabel,
	const BoundLabel& ContinueLabel
)
{
	BoundStatement* pWhile = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::While);
	new(&pWhile->whilestmt) BoundWhileStatement
	{
		pCondition,
		BoundLoopStatement
		{
			pBody,
			BreakLabel,
			ContinueLabel
		},
		bIsDoWhile
	};
	return pWhile;
}

BoundStatement* MakeBoundStatement_Return(BoundExpression* pExpression)
{
	BoundStatement* pReturn = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::Return);
	new(&pReturn->ret) BoundReturnStatement{ pExpression };
	return pReturn;
}

BoundStatement* MakeBoundStatement_Label(const BoundLabel& Label)
{
	BoundStatement* pLabel = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::Label);
	new(&pLabel->label) BoundLabelStatement{ Label };
	return pLabel;
}

BoundStatement* MakeBoundStatement_Goto(const BoundLabel& Label)
{
	BoundStatement* pGoto = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::Goto);
	new(&pGoto->gotostmt) BoundGotoStatement{ Label };
	return pGoto;
}

BoundStatement* MakeBoundStatement_ConditionalGoto(const BoundLabel& Label, BoundExpression* pCondition, bool bJumpIfTrue)
{
	BoundStatement* pCondGoto = Allocator::Create<BoundStatement>(Allocator::Stage::Binder, BoundStatementKind::ConditionalGoto);
	new(&pCondGoto->cgotostmt) BoundConditionalGotoStatement{ Label, pCondition, bJumpIfTrue };
	return pCondGoto;
}
#pragma endregion


bool _My_ArrayTypesMatch(const MyArrayType& From, const MyArrayType& To) noexcept
{
	if (From.Klass != To.Klass)
	{
		if (To.Klass != My_Defaults.ObjectStruct)
		{
			return false;
		}
	}

	const size_t kLhsRank = stbds_arrlenu(From.Lengths);
	const size_t kRhsRank = stbds_arrlenu(To.Lengths);
	if (kLhsRank != kRhsRank)
	{
		return false;
	}

	for (size_t k = 0; k < kLhsRank; k++)
	{
		const uint64_t& fc = From.Lengths[k];
		const uint64_t& tc = To.Lengths[k];
		if (fc != tc && tc != 0ull)
		{
			return false;
		}
	}

	return true;
}

bool _My_FunctionTypesMatch(const MyFunctionSignature& From, const MyFunctionSignature& To) noexcept
{
	if (From.Return != To.Return)
	{
		return false;
	}

	const size_t kLhsArgc = stbds_arrlenu(From.Params);
	const size_t kRhsArgc = stbds_arrlenu(To.Params);
	if (kLhsArgc != kRhsArgc)
	{
		return false;
	}

	for (size_t k = 0; k < kLhsArgc; k++)
	{
		if (From.Params[k] != To.Params[k])
		{
			return false;
		}
	}

	return true;
}


const char* SymbolKindString(SymbolKind Kind) noexcept
{
	switch (Kind)
	{
		case SymbolKind::GlobalVariable: return "SymbolKind::GlobalVariable";
		case SymbolKind::LocalVariable:  return "SymbolKind::LocalVariable";
		case SymbolKind::Parameter:      return "SymbolKind::Parameter";
		case SymbolKind::Function:       return "SymbolKind::Function";
		case SymbolKind::Struct:         return "SymbolKind::Struct";
		default:                         return "[invalid symbol]";
	}
}

const char* BoundExpressionKindString(BoundExpressionKind Kind) noexcept
{
	switch (Kind)
	{
		case BoundExpressionKind::Error:         return "BoundExpressionKind::Error";
		case BoundExpressionKind::Literal:       return "BoundExpressionKind::Literal";
		case BoundExpressionKind::Unary:         return "BoundExpressionKind::Unary";
		case BoundExpressionKind::Binary:        return "BoundExpressionKind::Binary";
		case BoundExpressionKind::Ternary:       return "BoundExpressionKind::Ternary";
		case BoundExpressionKind::Increment:     return "BoundExpressionKind::Increment";
		case BoundExpressionKind::Parenthesized: return "BoundExpressionKind::Parethesized";
		case BoundExpressionKind::Name:          return "BoundExpressionKind::Name";
		case BoundExpressionKind::Assignment:    return "BoundExpressionKind::Assignment";
		case BoundExpressionKind::OperatorNew:   return "BoundExpressionKind::OperatorNew";
		case BoundExpressionKind::Call:          return "BoundExpressionKind::Call";
		case BoundExpressionKind::Index:         return "BoundExpressionKind::Index";
		case BoundExpressionKind::Field:         return "BoundExpressionKind::Field";
		case BoundExpressionKind::Array:         return "BoundExpressionKind::Array";
		case BoundExpressionKind::Instance:      return "BoundExpressionKind::Instance";
		case BoundExpressionKind::Conversion:    return "BoundExpressionKind::Conversion";
		default:                                 return "[invalid bound expression]";
	}
}

const char* BoundStatementKindString(BoundStatementKind Kind) noexcept
{
	switch (Kind)
	{
		case BoundStatementKind::Error:                return "BoundStatementKind::Error";
		case BoundStatementKind::Block:                return "BoundStatementKind::Block ";
		case BoundStatementKind::Expression:           return "BoundStatementKind::Expression";
		case BoundStatementKind::VariableDeclaration:  return "BoundStatementKind::Variable";
		case BoundStatementKind::DecomposeDeclaration: return "BoundStatementKind::Decompose";
		case BoundStatementKind::If:                   return "BoundStatementKind::If";
		case BoundStatementKind::For:                  return "BoundStatementKind::For";
		case BoundStatementKind::Foreach:              return "BoundStatementKind::Foreach";
		case BoundStatementKind::While:                return "BoundStatementKind::While";
		case BoundStatementKind::Return:               return "BoundStatementKind::Return";
		case BoundStatementKind::Label:                return "BoundStatementKind::Label";
		case BoundStatementKind::Goto:                 return "BoundStatementKind::Goto";
		case BoundStatementKind::ConditionalGoto:      return "BoundStatementKind::ConditionalGoto";
		case BoundStatementKind::Nop:                  return "BoundStatementKind::Nop";
		default:                                       return "[invalid bound statement]";
	}
}

