#include "Tree.h"
#include "My/My.h"
#include "My/Base/IO.h"
#include "My/Object.h"
#include "Stb/stb_ds.h"
#include "Tools/string_builder.h"

/// Keyword Map
using KeywordMap = Pair<const char* const, TokenKind>;

static KeywordMap* s_KeywordMap = nullptr;

static void _My_Initialize_KeywordMap() noexcept;

/// Creators
#pragma region Prototypes_For_Creator_Functions
/// <summary>
/// Create TypeSpec
/// </summary>
static TypeSpec* MakeTypeSpec_Name(const Token& Ident);
static TypeSpec* MakeTypeSpec_Array(TypeSpec* pType, const Token& LbracketToken, Expression** ppCounts, const Token& RbracketToken);
static TypeSpec* MakeTypeSpec_Function(
	const Token& CallbackKeyword,
	const Token& LparenToken0,
	TypeSpec*    pRType,
	const Token& LparenToken1,
	TypeSpec**   ppParamTypes,
	const Token& RparenToken1,
	const Token& RparenToken0
);

/// <summary>
/// Create Expression
/// </summary>
static Expression* MakeExpression_Literal(const Token& Literal);
static Expression* MakeExpression_Unary(const Token& Operator, Expression* pRhs);
static Expression* MakeExpression_Binary(Expression* pLhs, const Token& Operator, Expression* pRhs);
static Expression* MakeExpression_Ternary(
	Expression*  pCondition,
	const Token& QuestionToken,
	Expression*  pThen,
	const Token& ColonToken,
	Expression*  pElse
);
static Expression* MakeExpression_Parenthesized(const Token& LparenToken, Expression* pExpr, const Token& RparenToken);
static Expression* MakeExpression_Name(const Token& Identifier);
static Expression* MakeExpression_Assignment(Expression* pLhs, const Token& EqualsToken, Expression* pRhs);
static Expression* MakeExpression_OperatorNew(
	const Token& NewKeyword,
	TypeSpec*    pType,
	const Token& LbraceToken,
	Expression*  pInitializer,
	const Token& RbraceToken
);
static Expression* MakeExpression_OperatorNew(
	const Token&      NewKeyword,
	TypeSpec*         pType,
	const Token&      LbraceToken,
	FieldInitializer* pFields,
	const Token&      RbraceToken
);
static Expression* MakeExpression_Call(
	Expression*  pCallable,
	const Token& LparenToken,
	Expression** ppArguments,
	const Token& RparenToken
);
static Expression* MakeExpression_Index(
	Expression*  pSequence,
	const Token& LbracketToken,
	Expression** ppIndices,
	const Token& RbracketToken
);
static Expression* MakeExpression_Field(Expression* pObject, const Token& DotToken, const Token& Field);
static Expression* MakeExpression_Array(const Token& LbraceToken, Expression** ppItems, const Token& RbraceToken);

/// <summary>
/// Create Statement
/// </summary>
static Statement* MakeStatement_Block(const Token& LbraceToken, Statement** ppStmts, const Token& RbraceToken);
static Statement* MakeStatement_Expression(Expression* pExpr);
static Statement* MakeStatement_VariableDeclaration(
	const Token& VarKeyword,
	TypeSpec*    pType,
	const Token& Identifier,
	const Token& EqualsToken,
	Expression*  pValue,
	bool         bIsReadonly = false
);
static Statement* MakeStatement_DecomposeDeclaration(
	const Token& AutoKeyword,
	const Token& LbracketToken,
	Token*       pIdentifiers,
	const Token& RbracketToken,
	const Token& EqualsToken,
	Expression*  pDecomposable
);
using ElseIfStatement = IfStatement::ElseIfStatement;
static Statement* MakeStatement_If(
	const Token& IfKeyword,
	const Token& LparenToken,
	Expression*  pCondition,
	const Token& RparenToken,
	Statement*   pIfBlock,
	ElseIfStatement* pElseIfs,
	const Token& ElseKeyword,
	Statement*   pElseBlock
);
static Statement* MakeStatement_For(
	const Token& ForKeyword,
	const Token& LparenToken,
	const Token& Identifier,
	Expression*  pLowerBound,
	Expression*  pUpperBound,
	Expression*  pStep,
	const Token& RparenToken,
	Statement*   pBody
);
static Statement* MakeStatement_Foreach(
	const Token& ForeachKeyword,
	const Token& LparenToken,
	const Token& Identifier,
	const Token& ColonToken,
	Expression*  pIterable,
	const Token& RparenToken,
	Statement*   pBody
);
static Statement* MakeStatement_While(
	const Token& WhileKeyword,
	const Token& LparenToken,
	Expression*  pCondition,
	const Token& RparenToken,
	Statement*   pBody
);
static Statement* MakeStatement_DoWhile(
	const Token& DoKeyword,
	Statement*   pBody,
	const Token& WhileKeyword,
	const Token& LparenToken,
	Expression*  pCondition,
	const Token& RparenToken
);
static Statement* MakeStatement_Break(const Token& BreakKeyword);
static Statement* MakeStatement_Continue(const Token& ContinueKeyword);
static Statement* MakeStatement_Return(const Token& ReturnKeyword, Expression* pExpr);

/// <summary>
/// Create Declaration
/// </summary>
static Declaration* MakeDeclaration_Import(const Token& ImportKeyword, const Token& Name);
static Declaration* MakeDeclaration_Using(
	const Token& UsingKeyword,
	const Token& Name,
	const Token& EqualsToken,
	TypeSpec*    pType
);
static Declaration* MakeDeclaration_Extern(
	const Token&             ExternKeyword,
	const Token&             FunctionKeyword,
	const FunctionSignature& Signature
);
static Declaration* MakeDeclaration_Enum(
	const Token& EnumKeyword,
	const Token& Name,
	const Token& LbraceToken,
	Token*       pValues,
	const Token& RbraceToken
);
static Declaration* MakeDeclaration_BitFlags(
	const Token& BflagsKeyword,
	const Token& LparenToken,
	Expression*  pBase,
	const Token& RparenToken,
	const Token& Name,
	const Token& LbraceToken,
	Token* pValues,
	const Token& RbraceToken
);
static Declaration* MakeDeclaration_Variable(
	Statement* pVarDeclStmt
);
static Declaration* MakeDeclaration_Variable(
	const Token& VarKeyword,
	TypeSpec*    pType,
	const Token& Identifier,
	const Token& EqualsToken,
	Expression*  pValue,
	bool         bIsReadonly = false
);
static Declaration* MakeDeclaration_Function(
	const Token&             FunctionKeyword,
	const FunctionSignature& Signature,
	Statement*               pBody,
	uint32_t                 kAttributes
);
static Declaration* MakeDeclaration_Forward(const Token& StructKeyword, const Token& Name);
static Declaration* MakeDeclaration_Struct(
	const Token&      StructKeyword,
	Token*            pTrivialKeyword,
	const Token&      Name,
	const Token&      LbraceToken,
	Statement**       pMembers,
	Declaration**     pMethods,
	const Token&      RbraceToken,
	uint32_t          kAttributes
);
#pragma endregion

#pragma region Lexer
class InternalLexer
{
public:
	explicit InternalLexer(MyContext* const pContext, SyntaxTree& Tree)
		: m_Context(pContext), m_Text(Tree.GetText()), m_Tree(Tree)
	{
		// Checks to see if *s_KeywordMap* has any items before inserting
		// The table will be initialized once irregardless of how many times InternalLexer is instanciated
		_My_Initialize_KeywordMap();
	}

	~InternalLexer() noexcept = default;

	Token NextToken()
	{
	TOP:
		m_Start = m_Position;
		m_Token = Token(TokenKind::Invalid);

		switch (Current())
		{
			case '\0': m_Token.Kind = TokenKind::Eof; break;
			case '.':  m_Position++, m_Token.Kind = TokenKind::Dot;       break;
			case '%':  m_Position++, m_Token.Kind = TokenKind::Percent;   break;
			case '^':  m_Position++, m_Token.Kind = TokenKind::Caret;     break;
			case '~':  m_Position++, m_Token.Kind = TokenKind::Tilde;     break;
			case '(':  m_Position++, m_Token.Kind = TokenKind::LParen;    break;
			case ')':  m_Position++, m_Token.Kind = TokenKind::RParen;    break;
			case '{':  m_Position++, m_Token.Kind = TokenKind::LBrace;    break;
			case '}':  m_Position++, m_Token.Kind = TokenKind::RBrace;    break;
			case '[':  m_Position++, m_Token.Kind = TokenKind::LBracket;  break;
			case ']':  m_Position++, m_Token.Kind = TokenKind::RBracket;  break;
			case '?':  m_Position++, m_Token.Kind = TokenKind::Question;  break;
			case ',':  m_Position++, m_Token.Kind = TokenKind::Comma;     break;
			case ':':  m_Position++, m_Token.Kind = TokenKind::Colon;     break;
			case ';':  m_Position++, m_Token.Kind = TokenKind::Semicolon; break;
			case '+':  m_Position++, m_Token.Kind = TokenKind::Plus;      break;
			case '/':
				if (Peek(1) == '/')
				{
					ReadSingleLineComment();
				}
				else if (Peek(1) == '*')
				{
					ReadMultiLineComment();
				}
				else
				{
					m_Position++, m_Token.Kind = TokenKind::Slash;
				}
				break;
			case '-':
				m_Position++;
				m_Token.Kind = Current() == '>' ? m_Position++, TokenKind::Arrow : TokenKind::Dash;
				break;
			case '*':
				m_Position++;
				m_Token.Kind = Current() == '*' ? m_Position++, TokenKind::StarStar : TokenKind::Star;
				break;
			case '&':
				m_Position++;
				m_Token.Kind = Current() == '&' ? m_Position++, TokenKind::AndAnd : TokenKind::And;
				break;
			case '|':
				m_Position++;
				m_Token.Kind = Current() == '|' ? m_Position++, TokenKind::PipePipe : TokenKind::Pipe;
				break;
			case '=':
				m_Position++;
				m_Token.Kind = Current() == '=' ? m_Position++, TokenKind::EqualsEquals : TokenKind::Equals;
				break;
			case '!':
				m_Position++;
				m_Token.Kind = Current() == '=' ? m_Position++, TokenKind::BangEquals : TokenKind::Bang;
				break;
			case '<':
				m_Position++;
				switch (Current())
				{
					case '<': m_Position++, m_Token.Kind = TokenKind::LessLess;   break;
					case '=': m_Position++, m_Token.Kind = TokenKind::LessEquals; break;
					default: m_Token.Kind = TokenKind::Less; break;
				}
				break;
			case '>':
				m_Position++;
				switch (Current())
				{
					case '>': m_Position++, m_Token.Kind = TokenKind::GreaterGreater; break;
					case '=': m_Position++, m_Token.Kind = TokenKind::GreaterEquals;	 break;
					default: m_Token.Kind = TokenKind::Greater; break;
				}
				break;
			case '"':
				ReadString();
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				ReadNumber();
				break;
			case ' ':
			case '\t':
			case '\n':
			case '\r':
			case '\v':
				SkipSpace();
				goto TOP;
				break;
			default:
			{
				const char Character = Current();
				if (isalpha(Character) || Character == '_')
				{
					ReadIdentifier();
				}
				else
				{
					TextLocation Location = TextLocation(m_Position, 1, m_Text.GetLineIndex(m_Position), m_Text.Filename);
					m_Diagnostics.ReportBadCharacter(Location, Character);
					m_Position++;
				}
				break;
			}
		}

		m_Token.Start = m_Start;
		m_Token.End   = m_Position;

		return m_Token;
	}

	const DiagnosticBag& GetDiagnostics() const noexcept { return m_Diagnostics; }

private:
	char Peek(uint32_t kOffset = 0u) const noexcept
	{
		const uint32_t kIndex = m_Position + kOffset;
		return kIndex >= m_Text.Length() ? '\0' : m_Text[kIndex];
	}

	char Current() const noexcept
	{
		return Peek();
	}

	void SkipSpace() noexcept
	{
		while (isspace(Current()))
		{
			m_Position++;
		}
	}

	void ReadSingleLineComment() noexcept
	{
		m_Position += 2;
		while (Current() != '\r' && Current() != '\n' && Current() != '\0')
		{
			m_Position++;
		}
	}
	
	void ReadMultiLineComment() noexcept
	{
		m_Position += 2;
		bool bDone = false;

		while (!bDone)
		{
			switch (Current())
			{
				case '\0':
					m_Diagnostics.ReportUnterminatedMultilineComment(TextLocation(m_Start, 2, m_Text.GetLineIndex(m_Start), m_Text.Filename));
					bDone = true;
					break;
				case '*':
					m_Position++;
					if (Current() == '/')
					{
						bDone = true;
						m_Position++;
					}
					break;
				default:
					m_Position++;
					break;
			}
		}
	}
	
	void ReadString() noexcept
	{
		// TODO: Better ReadString() implementation
		static const auto EscapeCharacter = [](char c) noexcept -> char
		{
			switch (c)
			{
				case 'n':  return '\n';
				case 'r':  return '\r';
				case 't':  return '\t';
				case 'v':  return '\v';
				case 'b':  return '\b';
				case 'a':  return '\a';
				case '"':  return '\"';
				case '\\': return '\\';
				case '0':  return '\0';
				default:   return 0;
			}
		};


		m_Position++; // Skip '"'
		StringBuilder sb = SbCreate(8);

		bool bDone = false;
		while (!bDone)
		{
			switch (const char Ch = Current(); Ch)
			{
				case '\0':
				case '\r':
				case '\n':
					m_Diagnostics.ReportUnterminatedString(TextLocation(m_Start, 1, m_Text.GetLineIndex(m_Start), m_Text.Filename));
					bDone = true;
					break;
				case '\\':
				{
					m_Position++;
					char c = Current();
					char e = EscapeCharacter(c);
					if (e == 0 && c != '0')
					{
						m_Diagnostics.ReportInvalidEscapeCharacterInStringLiteral(
							TextLocation(m_Position - 1, 2, m_Text.GetLineIndex(m_Position - 1), m_Text.Filename), c
						);
					}
					else
					{
						SbWriteChar(&sb, e);
					}
					m_Position++;
					break;
				}
				case '"':
					if (Peek(1) == '"')
					{
						m_Position += 2;
					}
					else
					{
						m_Position++;
						SbWriteChar(&sb, 0);
						bDone = true;
					}
					break;
				default:
					SbWriteChar(&sb, Ch);
					m_Position++;
					break;
			}
		}

		m_Token.Kind = TokenKind::String;
		// m_Token.Str  = MyStringNew(m_Context, Str.c_str());
		m_Token.Str  = MyStringNew(m_Context, SbGetString(&sb));
	}

	void ReadNumber() noexcept
	{
		// TODD: Some higher level number parsing
		while (isdigit(Current()))
		{
			m_Position++;
		}

		if (Current() != '.')
		{
			std::string_view Value = m_Text.ToString(m_Start, m_Position - m_Start);
			
			char* lpBegin = const_cast<char*>(Value.data());
			char* lpEnd   = lpBegin + Value.length();

			if (toupper(Current()) == 'U')
			{
				m_Position++;
				m_Token.Kind = TokenKind::Uint64;
				m_Token.U64  = strtoull(lpBegin, &lpEnd, 10);
			}
			else
			{
				m_Token.Kind = TokenKind::Int64;
				m_Token.I64  = strtoll(lpBegin, &lpEnd, 10);
			}
		}
		else
		{
			m_Position++; // Skip '.'
			while (isdigit(Current()))
			{
				m_Position++;
			}

			std::string_view Value = m_Text.ToString(m_Start, m_Position - m_Start);

			char* lpBegin = const_cast<char*>(Value.data());
			char* lpEnd   = lpBegin + Value.length();
			
			m_Token.Kind = TokenKind::Float64;
			m_Token.F64 = strtod(lpBegin, &lpEnd);
		}
	}
	
	void ReadIdentifier() noexcept
	{
		while (isalnum(Current()) || Current() == '_')
		{
			m_Position++;
		}

		m_Token.Id   = MyGetCachedString(m_Text.ToString(m_Start, m_Position - m_Start));
		m_Token.Kind = stbds_shget(s_KeywordMap, m_Token.Id);

		if (m_Token.Kind == TokenKind::Invalid)
		{
			m_Token.Kind = TokenKind::Identifier;
		}
	}

private:
	MyContext* const m_Context     = nullptr;
	SourceText&       m_Text;
	SyntaxTree        m_Tree;
	uint32_t          m_Start       = 0u;
	uint32_t          m_Position    = 0u;
	Token             m_Token       = {};
	DiagnosticBag     m_Diagnostics = {};
};
#pragma endregion

#pragma region Parser
class InternalParser
{
public:
	explicit InternalParser(MyContext* pContext, SyntaxTree& st)
		: m_Tree(st), m_Text(st.GetText())
	{
		InternalLexer lexer = InternalLexer(pContext, m_Tree);
		
		Token  token  = {};
		Token* tokens = nullptr;

		do
		{
			token = lexer.NextToken();
			if (token.Kind != TokenKind::Invalid)
			{
				stbds_arrpush(tokens, token);
			}
		} while (token.Kind != TokenKind::Eof);

		m_Diagnostics.Extend(lexer.GetDiagnostics());

		if (m_Diagnostics.empty())
		{
			m_Tokens = tokens;
		}
	}

	~InternalParser() noexcept = default;

	ParseResult ParseCompilationUnit()
	{
		Declaration** ppDecls = ParseDeclarations();

		ParseResult pr = {};
		if (stbds_arrlenu(ppDecls) < 1u)
		{
			// An error ocuured during parsing, so return nothing
			return pr;
		}
		else
		{
			pr.Decls = ppDecls;
			pr.EofToken = MatchToken(TokenKind::Eof);
			return pr;
		}
	}

	const DiagnosticBag& GetDiagnostics() const { return m_Diagnostics; }

private:
	const Token& Peek(uint32_t kOffset = 0u) const noexcept
	{
		if (m_Tokens == nullptr)
		{
			MY_ASSERT(false, "FatalError");
			return *static_cast<Token*>(nullptr);
		}

		const uint32_t kIndex = m_Position + kOffset;
		const uint32_t kCount = stbds_arrlenu(m_Tokens);
		return kIndex >= kCount ? m_Tokens[kCount - 1] : m_Tokens[kIndex];
	}

	const Token& Current() const noexcept
	{
		return Peek();
	}

	Token& NextToken() noexcept
	{
		Token& current = const_cast<Token&>(Current());
		return m_Position++, current;
	}

	void ReportErrorOnMatchFailed(TokenKind Kind) noexcept
	{
		const Token& current = Current();

		TextLocation Location = current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename);
		switch (Kind)
		{
			case TokenKind::Semicolon:
				m_Diagnostics.ReportExpectedSemicolon(Location, current.Kind);
				break;
			case TokenKind::Greater:
			case TokenKind::RParen:
			case TokenKind::RBrace:
			case TokenKind::RBracket:
				m_Diagnostics.ReportExpectedCommaOrEndingToken(Location, current.Kind, Kind);
				break;
			default:
				m_Diagnostics.ReportUnexpectedToken(Location, current.Kind, Kind);
				break;
		}
	}

	Token MatchToken(TokenKind Kind) noexcept
	{
		if (const Token& current = Current(); current.Kind == Kind)
		{
			NextToken();
			return current;
		}
		else
		{
			ReportErrorOnMatchFailed(Kind);
			return Token(Kind);
		}
	}

	bool CheckAndMatchToken(TokenKind Kind) noexcept
	{
		if (const Token& current = Current(); current.Kind == Kind)
		{
			NextToken();
			return true;
		}
		else
		{
			ReportErrorOnMatchFailed(Kind);
			return false;
		}
	}

	// TypeSpecifiers
	TypeSpec* ParseTypeSpec() noexcept
	{
		TypeSpec*  pTypeSpec  = nullptr;

		// - Types come in basically 3 flavours:
		//       1. a name, say, T - most primitive type
		//       2. a function type, callback(R(TArgs...)) - can be recursive but, that takes care of itself since
		//          we've abstracted parsing function types to its own function ParseFunctionTypeSpec()
		//       3. an array, T[N...] - here T can be either a name or a function type
		// - Since arrays are the most complex, we will parse them last, get the item type, then parse the rank

		if (Current().Kind != TokenKind::Identifier && Current().Kind != TokenKind::CallbackKeyword)
		{
			const Token& current = Current();
			TextLocation Location = current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename);
			m_Diagnostics.ReportMissingTypename(Location);

			goto Error;
		}

		// As stated above, first get type T
		if (Current().Kind == TokenKind::CallbackKeyword)
		{
			pTypeSpec = ParseFunctionTypeSpec();
		}
		else
		{
			pTypeSpec = MakeTypeSpec_Name(NextToken()); // If we got here, the current token is an identifier
		}

		// If we have a '[', then we parse T[N...]
		if (Current().Kind == TokenKind::LBracket)
		{
			const Token& LbracketToken = NextToken(); // [

			Expression** ppCounts = nullptr;
			while (Current().Kind != TokenKind::RBracket && Current().Kind != TokenKind::Eof)
			{
				Expression* pCount = ParseNumberExpression();
				if (!pCount)
				{
					goto Error;
				}
				stbds_arrpush(ppCounts, pCount);

				if (Current().Kind !=TokenKind::RBracket)
				{
					if (!CheckAndMatchToken(TokenKind::Comma))
					{
						goto Error;
					}
				}
			}
			
			const Token& RbracketToken = Current(); // ]
			if (!CheckAndMatchToken(TokenKind::RBracket))
			{
				goto Error;
			}
			
			pTypeSpec = MakeTypeSpec_Array(pTypeSpec, LbracketToken, ppCounts, RbracketToken);
		}

		return pTypeSpec;

	Error:
		return nullptr;
	}

	TypeSpec* ParseFunctionTypeSpec() noexcept
	{
		MY_NOT_IMPLEMENTED();
		// The syntax is weird but it will work for now
		/*TypeSpec*  pType   = nullptr;
		TypeSpec** ppTypes = nullptr;

		const Token& CallbackKeyword = NextToken();
		const Token& LparenToken0 = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}
		if (!(pType = ParseTypeSpec()))
		{
			goto Error;
		}
		const Token& LparenToken1 = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}
		while (Current().Kind != TokenKind::RParen && Current().Kind != TokenKind::Eof)
		{
			TypeSpec* pArgType = ParseTypeSpec();
			if (!pArgType)
			{
				goto Error;
			}
			stbds_arrpush(ppTypes, pArgType);

			if (Current().Kind != TokenKind::RParen)
			{
				if (!CheckAndMatchToken(TokenKind::Comma))
				{
					goto Error;
				}
			}
		}
		const Token& RparenToken1 = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		const Token& RparenToken0 = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		
		return MakeTypeSpec_Function(CallbackKeyword, LparenToken0, pType, LparenToken1, ppTypes, RparenToken1, RparenToken0);

	Error:*/
		return nullptr;
	}

	// Expressions
	Expression* ParseExpression() noexcept
	{
		// return ParseTernaryExpression();
		return ParseAssignmentExpression();
	}

	Expression* ParsePrimaryExpression() noexcept
	{
		switch (Current().Kind)
		{
			case TokenKind::LParen:
				return ParseParenthesizedExpression();
			case TokenKind::LBrace:
				return ParseArrayExpression();
			case TokenKind::NullKeyword:
				return MakeExpression_Literal(MatchToken(TokenKind::NullKeyword));
			case TokenKind::FalseKeyword:
				return MakeExpression_Literal(MatchToken(TokenKind::FalseKeyword));
			case TokenKind::TrueKeyword:
				return MakeExpression_Literal(MatchToken(TokenKind::TrueKeyword));
			case TokenKind::NewKeyword:
				return ParseOperatorNewExpression();
			case TokenKind::Int64:
			case TokenKind::Uint64:
			case TokenKind::Float64:
				return ParseNumberExpression();
			case TokenKind::String:
				return ParseStringExpression();
			case TokenKind::Identifier:
				return ParseNameExpression();
			default:
			{
				const Token& current = Current();
				TextLocation Location = current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename);
				m_Diagnostics.ReportExpectedPrimaryExpression(Location, current.Kind);
				return nullptr;
			}
		}
	}
	
	Expression* ParseIndexExpression(Expression* pExpr)
	{
		if (Current().Kind == TokenKind::LBracket)
		{
			Expression** ppIndices = nullptr;

			const Token& LbracketToken = NextToken();
			while (Current().Kind != TokenKind::RBracket && Current().Kind != TokenKind::Eof)
			{
				Expression* pIndex = ParseExpression();
				if (!pIndex)
				{
					return nullptr;
				}
				stbds_arrpush(ppIndices, pIndex);

				if (Current().Kind != TokenKind::RBracket)
				{
					if (!CheckAndMatchToken(TokenKind::Comma))
					{
						return nullptr;
					}
				}
			}
			const Token& RbracketToken = Current();
			if (!CheckAndMatchToken(TokenKind::RBracket))
			{
				return nullptr;
			}

			return MakeExpression_Index(pExpr, LbracketToken, ppIndices, RbracketToken);
		}

		return pExpr;
	}

	Expression* ParseSecondaryExpression() noexcept
	{
		Expression*  pExpr  = nullptr;
		Expression** ppArgs = nullptr;

		if (!(pExpr = ParsePrimaryExpression()))
		{
			goto Error;
		}

		while (Current().Kind == TokenKind::LParen || Current().Kind == TokenKind::Dot)
		{
			// Call Expression
			if (Current().Kind == TokenKind::LParen)
			{
				const Token& LparenToken = NextToken(); // (
				while (Current().Kind != TokenKind::RParen && Current().Kind != TokenKind::Eof)
				{
					Expression* pArg = ParseExpression();
					if (!pArg)
					{
						goto Error;
					}
					stbds_arrpush(ppArgs, pArg);

					if (Current().Kind != TokenKind::RParen)
					{
						if (!CheckAndMatchToken(TokenKind::Comma))
						{
							goto Error;
						}
					}
				}
				const Token& RparenToken = Current();
				if (!CheckAndMatchToken(TokenKind::RParen))
				{
					goto Error;
				}

				pExpr = MakeExpression_Call(pExpr, LparenToken, ppArgs, RparenToken);
			}
			// (Must be) Field Expression
			else
			{
				const Token& DotToken = Current();
				if (!CheckAndMatchToken(TokenKind::Dot))
				{
					goto Error;
				}

				if (Current().Kind != TokenKind::Identifier)
				{
					const Token& current = Current();
					TextLocation Location = current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename);
					m_Diagnostics.ReportExpectedIdentifierToken(Location);
					goto Error;
				}

				pExpr = MakeExpression_Field(pExpr, DotToken, NextToken());
			}
			
			// Index Expression (coming after call/field expressions)
			Expression* pIndexExpr = ParseIndexExpression(pExpr);
			if (!pIndexExpr)
			{
				goto Error;
			}

			if (pIndexExpr && pIndexExpr != pExpr)
			{
				pExpr = pIndexExpr;
			}
		} // end while

		// Index Expression (coming after name expressions)
		Expression* pIndexExpr = ParseIndexExpression(pExpr);
		if (!pIndexExpr)
		{
			goto Error;
		}

		if (pIndexExpr && pIndexExpr != pExpr)
		{
			pExpr = pIndexExpr;
		}

		return pExpr;

	Error:
		return nullptr;
	}

	Expression* ParseBinaryExpression(int iParentPrecedence = 0) noexcept
	{
		Expression* pLhs = nullptr;
		Expression* pRhs = nullptr;

		int iUnaryOperatorPrecedence = GetUnaryOperatorPrecedence(Current().Kind);
		if (iUnaryOperatorPrecedence && iUnaryOperatorPrecedence >= iParentPrecedence)
		{
			const Token& Operator = NextToken();
			if (!(pRhs = ParseBinaryExpression(iUnaryOperatorPrecedence)))
			{
				goto Error;
			}

			pLhs = MakeExpression_Unary(Operator, pRhs);
		}
		else
		{
			if (!(pLhs = ParseSecondaryExpression()))
			{
				goto Error;
			}
		}

		while (true)
		{
			int iPrecedence = GetBinaryOperatorPrecedence(Current().Kind);
			if (!iPrecedence || iPrecedence <= iParentPrecedence)
			{
				break;
			}

			const Token& Operator = NextToken();
			if (!(pRhs = ParseBinaryExpression(iPrecedence)))
			{
				goto Error;
			}

			pLhs = MakeExpression_Binary(pLhs, Operator, pRhs);
		}

		return pLhs;

	Error:
		return nullptr;
	}

	Expression* ParseTernaryExpression() noexcept
	{
		Expression* pCond = nullptr;
		Expression* pThen = nullptr;
		Expression* pElse = nullptr;

		if (!(pCond = ParseExpression()))
		{
			goto Error;
		}

		if (Current().Kind == TokenKind::Question)
		{
			const Token& QuestionToken = NextToken();
			if (!(pThen = ParseExpression()))
			{
				goto Error;
			}

			const Token& ColonToken = Current();
			if (!CheckAndMatchToken(TokenKind::Colon))
			{
				goto Error;
			}
			if (!(pElse = ParseExpression()))
			{
				goto Error;
			}

			return MakeExpression_Ternary(pCond, QuestionToken, pThen, ColonToken, pElse);
		}

		return pCond;

	Error:
		return nullptr;
	}

	Expression* ParseAssignmentExpression() noexcept
	{
		Expression* pLhs = nullptr;
		Expression* pRhs = nullptr;

		if (!(pLhs = ParseBinaryExpression()))
		{
			goto Error;
		}

		if (Current().Kind == TokenKind::Equals)
		{
			const Token& EqualsToken = NextToken();
			if (!(pRhs = ParseTernaryExpression()))
			{
				goto Error;
			}

			pLhs = MakeExpression_Assignment(pLhs, EqualsToken, pRhs);
		}

		return pLhs;

	Error:
		return nullptr;
	}

	Expression* ParseParenthesizedExpression() noexcept
	{
		Expression* pExpr = nullptr;

		const Token& LparenToken = NextToken();
		if (!(pExpr = ParseExpression()))
		{
			goto Error;
		}

		const Token& RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}

		return MakeExpression_Parenthesized(LparenToken, pExpr, RparenToken);

	Error:
		return nullptr;
	}

	Expression* ParseNumberExpression() noexcept
	{
		// This check is needed since *ParseTypeSpec()* call it directly to parse the dimensions of arrays
		const Token& current = Current();
		if (current.Kind != TokenKind::Int64   && current.Kind != TokenKind::Uint64 && current.Kind != TokenKind::Float64)
		{
			TextLocation Location = current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename);
			m_Diagnostics.ReportExpectedNumberToken(Location);
			return nullptr;
		}

		Token NumberToken = MatchToken(current.Kind);
		return MakeExpression_Literal(NumberToken);
	}

	Expression* ParseStringExpression() noexcept
	{
		Token StringToken = MatchToken(TokenKind::String);
		return MakeExpression_Literal(StringToken);
	}

	Expression* ParseNameExpression() noexcept
	{
		Token Identifier = MatchToken(TokenKind::Identifier);
		return MakeExpression_Name(Identifier);
	}

	Expression* ParseOperatorNewExpression() noexcept
	{
		// MY_NOT_IMPLEMENTED();

		TypeSpec*         pType   = nullptr;
		Expression*       pInit   = nullptr;
		FieldInitializer* pFields = nullptr;

		bool bIsStructInitializer = false;

		const Token& NewKeyword = NextToken();
		if (!(pType = ParseTypeSpec()))
		{
			goto Error;
		}

		const Token& LbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}

		if (Current().Kind != TokenKind::RParen)
		{
			// TODO: StructInitializer to use
			//     1. dot syntax (.Field = Value)
			//     2. colon syntax (Field: Value) *
			//     3. both
#if 0
			if (Peek(0).Kind == TokenKind::Dot && Peek(1).Kind == TokenKind::Identifier && Peek(2).Kind == TokenKind::Equals)
			{
				while (Current().Kind != TokenKind::RParen && Current().Kind != TokenKind::Eof)
				{
					FieldInitializer fi = {};
					NextToken(); // Dot(.) token
					fi.Name = MatchToken(TokenKind::Identifier);
					fi.ColonToken = MatchToken(TokenKind::Equals);
					if (!(fi.Value = ParseExpression()))
					{
						goto Error;
					}

					stbds_arrpush(pFields, fi);

					if (Current().Kind != TokenKind::RParen)
					{
						if (!CheckAndMatchToken(TokenKind::Comma))
						{
							goto Error;
						}
					}
				}
			}
#endif // 0
#if 1
			if (Peek(0).Kind == TokenKind::Identifier && Peek(1).Kind == TokenKind::Colon)
			{
				bIsStructInitializer = true;
				while (Current().Kind != TokenKind::RParen && Current().Kind != TokenKind::Eof)
				{
					FieldInitializer fi = {};
					fi.Name = MatchToken(TokenKind::Identifier);
					fi.ColonToken = MatchToken(TokenKind::Colon);
					if (!(fi.Value = ParseExpression()))
					{
						goto Error;
					}

					stbds_arrpush(pFields, fi);

					if (Current().Kind != TokenKind::RParen)
					{
						if (!CheckAndMatchToken(TokenKind::Comma))
						{
							goto Error;
						}
					}
				}
			}
#endif // 1
			/*else
			{
				pInit = ParseExpression();
			}*/
		} // Current().Kind != TokenKind::RParen

		const Token& RbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		
		if (bIsStructInitializer)
		{
			return MakeExpression_OperatorNew(NewKeyword, pType, LbraceToken, pFields, RbraceToken);
		}
		else
		{
			return MakeExpression_OperatorNew(NewKeyword, pType, LbraceToken, pInit, RbraceToken);
		}

	Error:
		return nullptr;
	}

	Expression* ParseArrayExpression() noexcept
	{
		Expression** ppItems = nullptr; // array initalizer { x, ... }
		
		const Token& LbraceToken = NextToken(); // {
		if (Current().Kind == TokenKind::RBrace)
		{
			const Token& RbraceToken = NextToken();
			return MakeExpression_Array(LbraceToken, ppItems, RbraceToken);// empty array {}
		}

		Expression* pItem = ParseExpression();
		if (!pItem)
		{
			goto Error;
		}

		stbds_arrpush(ppItems, pItem);

		if (Current().Kind != TokenKind::RBrace)
		{
			if (!CheckAndMatchToken(TokenKind::Comma))
			{
				goto Error;
			}

			while (Current().Kind != TokenKind::RBrace && Current().Kind != TokenKind::Eof)
			{
				if (!(pItem = ParseExpression()))
				{
					goto Error;
				}

				stbds_arrpush(ppItems, pItem);

				if (Current().Kind != TokenKind::RBrace)
				{
					if (!CheckAndMatchToken(TokenKind::Comma))
					{
						goto Error;
					}
				}
			}
		}

		const Token& RbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::RBrace))
		{
			goto Error;
		}

		return MakeExpression_Array(LbraceToken, ppItems, RbraceToken);

	Error:
		return nullptr;
	}

	// Statements
	Statement* ParseStatement() noexcept
	{
		switch (Current().Kind)
		{
			case TokenKind::LBrace:
				return ParseBlockStatement();
			case TokenKind::VarKeyword:;
			case TokenKind::ConstKeyword:
				return ParseVariableDeclarationStatement();
			case TokenKind::AutoKeyword:
				return ParseDecomposeDeclarationStatement();
			case TokenKind::IfKeyword:
				return ParseIfStatement();
			case TokenKind::ForKeyword:
				return ParseForStatement();
			case TokenKind::ForeachKeyword:
				return ParseForeachStatement();
			case TokenKind::WhileKeyword:
				return ParseWhileStatement();
			case TokenKind::DoKeyword:
				return ParseDoWhileStatement();
			case TokenKind::BreakKeyword:
				return ParseBreakStatement();
			case TokenKind::ContinueKeyword:
				return ParseContinueStatement();
			case TokenKind::ReturnKeyword:
				return ParseReturnStatement();
			default:
				return ParseExpressionStatement();
		}
	}

	Statement* ParseBlockStatement() noexcept
	{
		Statement** ppStmts = nullptr;

		const Token& LbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::LBrace))
		{
			goto Error;
		}

		while (Current().Kind != TokenKind::RBrace && Current().Kind != TokenKind::Eof)
		{
			Statement* pStmt = ParseStatement();
			if (!pStmt)
			{
				goto Error;
			}

			stbds_arrpush(ppStmts, pStmt);
		}

		const Token& RbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::RBrace))
		{
			goto Error;
		}

		return MakeStatement_Block(LbraceToken, ppStmts, RbraceToken);

	Error:
		return nullptr;
	}

	Statement* ParseExpressionStatement() noexcept
	{
		Expression* pExpr = ParseExpression();
		if (!pExpr)
		{
			goto Error;
		}

		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeStatement_Expression(pExpr);

	Error:
		return nullptr;
	}

	Statement* ParseVariableDeclarationStatement() noexcept
	{
		TokenKind ExpectedKind = Current().Kind == TokenKind::ConstKeyword ? TokenKind::ConstKeyword : TokenKind::VarKeyword;

		TypeSpec*   pType        = nullptr;
		Token		Identifier   = {};
		Token       EqualsToken  = {};
		Expression* pValue       = nullptr;

		const Token& VarKeyword = Current();
		if (!CheckAndMatchToken(ExpectedKind))
		{
			goto Error;
		}

		if (!(pType = ParseTypeSpec()))
		{
			goto Error;
		}

		Identifier = MatchToken(TokenKind::Identifier);

		if (Current().Kind == TokenKind::Equals)
		{
			EqualsToken = NextToken();
			if (!(pValue = ParseExpression()))
			{
				goto Error;
			}
		}

		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeStatement_VariableDeclaration(VarKeyword, pType, Identifier, EqualsToken, pValue, ExpectedKind == TokenKind::ConstKeyword);

	Error:
		return nullptr;
	}

	Statement* ParseDecomposeDeclarationStatement() noexcept
	{
		Token*      pIdentifiers  = nullptr;
		Token       EqualsToken   = {};
		Token       RbracketToken = {};
		Expression* pDecomposable = nullptr;

		const Token& AutoKeyword = NextToken();

		const Token& LbracketToken = Current();
		if (!CheckAndMatchToken(TokenKind::LBracket))
		{
			goto Error;
		}

		if (Current().Kind != TokenKind::Identifier)
		{
			const Token& current = Current();
			TextLocation Location = current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename);
			m_Diagnostics.ReportExpectedIdentifierToken(Location);
			goto Error;
		}

		while (Current().Kind == TokenKind::Identifier && Current().Kind != TokenKind::Eof)
		{
			const Token& Identifier = NextToken();
			stbds_arrpush(pIdentifiers, Identifier);

			if (Current().Kind != TokenKind::RBracket)
			{
				if (!CheckAndMatchToken(TokenKind::Comma))
				{
					goto Error;
				}
			}
		}
		
		RbracketToken = Current();
		if (!CheckAndMatchToken(TokenKind::RBracket))
		{
			goto Error;
		}

		EqualsToken = Current();
		if (!CheckAndMatchToken(TokenKind::Equals))
		{
			goto Error;
		}

		if (!(pDecomposable = ParseExpression()))
		{
			goto Error;
		}

		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeStatement_DecomposeDeclaration(AutoKeyword, LbracketToken, pIdentifiers, RbracketToken, EqualsToken, pDecomposable);

	Error:
		return nullptr;
	}

	Statement* ParseIfStatement() noexcept
	{
		// MY_NOT_IMPLEMENTED();
		Expression*  pIfCondition = nullptr;
		Statement*   pIfBlock     = nullptr;
		ElseIfStatement* pElseifs = nullptr;
		Token        ElseKeyword  = {};
		Statement*   pElseBlock   = nullptr;

		const Token& IfKeyword = NextToken();

		const Token& LparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}
		if (!(pIfCondition = ParseExpression()))
		{
			goto Error;
		}

		const Token& RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		if (!(pIfBlock = ParseStatement()))
		{
			goto Error;
		}

		if (Current().Kind == TokenKind::ElseifKeyword)
		{
			while (Current().Kind == TokenKind::ElseifKeyword && Current().Kind != TokenKind::Eof)
			{
				ElseIfStatement eis = {};
				eis.ElseifKeyword = NextToken();

				eis.LparenToken = Current();
				if (!CheckAndMatchToken(TokenKind::LParen))
				{
					goto Error;
				}

				if (!(eis.Condition = ParseExpression()))
				{
					goto Error;
				}

				eis.RparenToken = Current();
				if (!CheckAndMatchToken(TokenKind::RParen))
				{
					goto Error;
				}

				if (!(eis.ElseifBlock = ParseStatement()))
				{
					goto Error;
				}

				stbds_arrpush(pElseifs, eis);
			}
		}

		if (Current().Kind == TokenKind::ElseKeyword)
		{
			ElseKeyword = NextToken();
			if (!(pElseBlock = ParseStatement()))
			{
				goto Error;
			}
		}

		return MakeStatement_If(IfKeyword, LparenToken, pIfCondition, RparenToken, pIfBlock, pElseifs, ElseKeyword, pElseBlock);

	Error:
		return nullptr;
	}

	Statement* ParseForStatement() noexcept
	{
		Token		Identifier  = {};
		Expression* pLowerBound = nullptr;
		Expression* pUpperBound = nullptr;
		Expression* pStep       = nullptr;
		Statement*  pBody       = nullptr;

		const Token& ForKeyword = NextToken();

		const Token& LparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}

		Identifier = MatchToken(TokenKind::Identifier);
		if (!CheckAndMatchToken(TokenKind::Comma))
		{
			goto Error;
		}
		if (!(pLowerBound = ParseExpression()))
		{
			goto Error;
		}
		if (!CheckAndMatchToken(TokenKind::Comma))
		{
			goto Error;
		}
		if (!(pUpperBound = ParseExpression()))
		{
			goto Error;
		}

		if (Current().Kind != TokenKind::RParen)
		{
			if (!CheckAndMatchToken(TokenKind::Comma))
			{
				goto Error;
			}
			if (!(pStep = ParseExpression()))
			{
				goto Error;
			}
		}

		const Token& RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		if (!(pBody = ParseStatement()))
		{
			goto Error;
		}

		return MakeStatement_For(ForKeyword, LparenToken, Identifier, pLowerBound, pUpperBound, pStep, RparenToken, pBody);

	Error:
		return nullptr;
	}

	Statement* ParseForeachStatement() noexcept
	{
		MY_NOT_IMPLEMENTED();
		/*Token		Identifier  = {};
		Token		ColonToken  = {};
		Expression* pIterable   = nullptr;
		Token		RparenToken = {};
		Statement*  pBody       = nullptr;

		const Token& ForeachKeyword = NextToken();

		const Token& LparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}

		Identifier = MatchToken(TokenKind::Identifier);

		ColonToken = Current();
		if (!CheckAndMatchToken(TokenKind::Colon))
		{
			goto Error;
		}

		if (!(pIterable = ParseExpression()))
		{
			goto Error;
		}

		RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}

		if (!(pBody = ParseStatement()))
		{
			goto Error;
		}

		return MakeStatement_Foreach(ForeachKeyword, LparenToken, Identifier, ColonToken, pIterable, RparenToken, pBody);

	Error:*/
		return nullptr;
	}

	Statement* ParseWhileStatement() noexcept
	{
		Expression* pCond = nullptr;
		Statement*  pBody = nullptr;

		const Token& WhileKeyword = NextToken();

		const Token& LparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}
		if (!(pCond = ParseExpression()))
		{
			goto Error;
		}

		const Token& RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		if (!(pBody = ParseStatement()))
		{
			goto Error;
		}

		return MakeStatement_While(WhileKeyword, LparenToken, pCond, RparenToken, pBody);

	Error:
		return nullptr;
	}

	Statement* ParseDoWhileStatement() noexcept
	{
		Statement*  pBody = nullptr;
		Expression* pCond = nullptr;

		const Token& DoKeyword = NextToken();
		if (!(pBody = ParseStatement()))
		{
			goto Error;
		}
		const Token& WhileKeyword = Current();
		if (!CheckAndMatchToken(TokenKind::WhileKeyword))
		{
			goto Error;
		}
		const Token& LparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}
		if (!(pCond = ParseExpression()))
		{
			goto Error;
		}
		const Token& RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeStatement_DoWhile(DoKeyword, pBody, WhileKeyword, LparenToken, pCond, RparenToken);

	Error:
		return nullptr;
	}

	Statement* ParseBreakStatement() noexcept
	{
		Statement* pBreak = MakeStatement_Break(NextToken());
		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return pBreak;

	Error:
		return nullptr;
	}

	Statement* ParseContinueStatement() noexcept
	{
		Statement* pContinue = MakeStatement_Break(NextToken());
		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return pContinue;

	Error:
		return nullptr;
	}

	Statement* ParseReturnStatement() noexcept
	{
		const Token& ReturnKeyword = NextToken();
		Expression* pExpr = nullptr;

		if (Current().Kind != TokenKind::Semicolon)
		{
			if (!(pExpr = ParseExpression()))
			{
				goto Error;
			}
		}
		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeStatement_Return(ReturnKeyword, pExpr);

	Error:
		return nullptr;
	}

	Declaration*  ParseDeclaration() noexcept
	{
		switch (Current().Kind)
		{
			case TokenKind::ImportKeyword:   return ParseImportDeclaration();
			case TokenKind::UsingKeyword:    return ParseUsingDeclaration();
			case TokenKind::ExternKeyword:	 return ParseExternDeclaration();
			case TokenKind::EnumKeyword:	 return ParseEnumDeclaration();
			case TokenKind::BflagsKeyword:	 return ParseBitFlagsDeclaration();
			case TokenKind::VarKeyword:      return ParseVariableDeclaration();
			case TokenKind::ConstKeyword:    return ParseVariableDeclaration();
			case TokenKind::FunctionKeyword: return ParseFunctionDeclaration();
			case TokenKind::StructKeyword:   return ParseStructDeclaration();
			default: break;
		}

		const Token& current = Current();
		m_Diagnostics.ReportExpectedDeclaration(current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename));
		return nullptr;
	}

	Declaration** ParseDeclarations() noexcept
	{
		Declaration** ppDecls = nullptr;
		
		while (Current().Kind != TokenKind::Eof)
		{
			Declaration* pDecl = ParseDeclaration();
			if (!pDecl)
			{
				goto Error;
			}

			stbds_arrpush(ppDecls, pDecl);
		}

		return ppDecls;

	Error:
		return nullptr;
	}

	Declaration* ParseImportDeclaration() noexcept
	{
		MY_NOT_IMPLEMENTED();
		/*const Token& ImportKeyword = NextToken();

		const Token& ModulePath = Current();
		if (!CheckAndMatchToken(TokenKind::String))
		{
			goto Error;
		}
		
		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeDeclaration_Import(ImportKeyword, ModulePath);

	Error:*/
		return nullptr;
	}

	Declaration* ParseUsingDeclaration() noexcept
	{
		Token     EqualsToken = {};
		TypeSpec* pType       = nullptr;

		const Token& UsingKeyword = NextToken();

		const Token& Name = Current();
		if (!CheckAndMatchToken(TokenKind::Identifier))
		{
			goto Error;
		}

		EqualsToken = Current();
		if (!CheckAndMatchToken(TokenKind::Equals))
		{
			goto Error;
		}

		if (!(pType = ParseTypeSpec()))
		{
			goto Error;
		}

		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeDeclaration_Using(UsingKeyword, Name, EqualsToken, pType);

	Error:
		return nullptr;
	}

	Declaration* ParseExternDeclaration() noexcept
	{
		FunctionSignature Signature = {};

		const Token& ExternKeyword = NextToken();
		const Token& FunctionKeyword = Current();
		if (!CheckAndMatchToken(TokenKind::FunctionKeyword))
		{
			goto Error;
		}

		Signature = ParseFunctionSignature();
		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeDeclaration_Extern(ExternKeyword, FunctionKeyword, Signature);

	Error:
		return nullptr;
	}

	Declaration* ParseEnumDeclaration() noexcept
	{
		MY_NOT_IMPLEMENTED();
		/*Token* pValues     = nullptr;
		Token  LbraceToken = {};
		Token  RbraceToken = {};

		const Token& EnumKeyword = NextToken();

		const Token& Name = Current();
		if (!CheckAndMatchToken(TokenKind::Identifier))
		{
			goto Error;
		}

		LbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::LBrace))
		{
			goto Error;
		}

		if (Current().Kind != TokenKind::RBrace)
		{
			while (Current().Kind == TokenKind::Identifier && Current().Kind != TokenKind::Eof)
			{
				const Token& Value = NextToken();
				stbds_arrpush(pValues, Value);

				if (Current().Kind != TokenKind::RBrace)
				{
					if (!CheckAndMatchToken(TokenKind::Comma))
					{
						goto Error;
					}
				}
			}
		}

		RbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::RBrace))
		{
			goto Error;
		}

		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeDeclaration_Enum(EnumKeyword, Name, LbraceToken, pValues, RbraceToken);

	Error:*/
		return nullptr;
	}
	
	Declaration* ParseBitFlagsDeclaration() noexcept
	{
		MY_NOT_IMPLEMENTED();
		/*Token*      pValues     = nullptr;
		Expression* pBase       = nullptr;
		Token       RparenToken = {};
		Token       Name        = {};
		Token       LbraceToken = {};
		Token       RbraceToken = {};

		const Token& BflagsKeyword = NextToken();

		const Token& LparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}

		if (!(pBase = ParseNumberExpression()))
		{
			goto Error;
		}

		RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}
		
		Name = Current();
		if (!CheckAndMatchToken(TokenKind::Identifier))
		{
			goto Error;
		}
		
		LbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::LBrace))
		{
			goto Error;
		}

		if (Current().Kind != TokenKind::RBrace)
		{
			while (Current().Kind == TokenKind::Identifier && Current().Kind != TokenKind::Eof)
			{
				const Token& Value = NextToken();
				stbds_arrpush(pValues, Value);

				if (Current().Kind != TokenKind::RBrace)
				{
					if (!CheckAndMatchToken(TokenKind::Comma))
					{
						goto Error;
					}
				}
			}
		}

		RbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::RBrace))
		{
			goto Error;
		}

		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeDeclaration_BitFlags(BflagsKeyword, LparenToken, pBase, RparenToken, Name, LbraceToken, pValues, RbraceToken);

	Error:*/
		return nullptr;
	}

	Declaration* ParseVariableDeclaration() noexcept
	{
		Statement* pVarDeclStmt = ParseVariableDeclarationStatement();
		if (!pVarDeclStmt)
		{
			goto Error;
		}
		
		return MakeDeclaration_Variable(pVarDeclStmt);
	
	Error:
		return nullptr;
	}

	Declaration* ParseFunctionDeclaration() noexcept
	{
		Statement* pBody = nullptr;

		const Token& FunctionKeyword = NextToken();
		FunctionSignature Signature = ParseFunctionSignature();
		if (!Signature.Return || Signature.Name.Kind == TokenKind::Invalid)
		{
			goto Error;
		}
		if (!(pBody = ParseStatement()))
		{
			goto Error;
		}
		return MakeDeclaration_Function(FunctionKeyword, Signature, pBody, Signature.Attributes);
		
	Error:
		return nullptr;
	}

	Declaration* ParseStructDeclaration() noexcept
	{
		Token*        pPodKeyword = nullptr;
		Statement**   ppMembers       = nullptr;
		Declaration** ppMethods       = nullptr;
		uint32_t      kAttributes     = 0u;

		const Token& StructKeyword = NextToken();
		if (Current().Kind == TokenKind::PodKeyword)
		{
			pPodKeyword = Allocator::Create<Token>(Allocator::Stage::Parser, NextToken());
			kAttributes |= MY_STRUCT_ATTR_POD;
		}

		const Token& Name = Current();
		if (!CheckAndMatchToken(TokenKind::Identifier))
		{
			goto Error;
		}

		if (Current().Kind == TokenKind::Semicolon)
		{
			// This is a forward declaration
			NextToken();
			return MakeDeclaration_Forward(StructKeyword, Name);
		}
		
		const Token& LbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::LBrace))
		{
			goto Error;
		}

		if (Current().Kind != TokenKind::RBrace)
		{
			while (Current().Kind != TokenKind::RBrace && Current().Kind != TokenKind::Eof)
			{
				switch (Current().Kind)
				{
					case TokenKind::VarKeyword:
					case TokenKind::ConstKeyword:
					{
						Statement* pMemberDecl = ParseVariableDeclarationStatement();
						stbds_arrpush(ppMembers, pMemberDecl);
						break;
					}
					case TokenKind::FunctionKeyword:
					{
						Declaration* pMethodDecl = ParseFunctionDeclaration();
						stbds_arrpush(ppMethods, pMethodDecl);
						break;
					}
					case TokenKind::RBrace:
					{
						break;
					}
					default:
					{
						const Token& current = Current();
						TextLocation Location = current.Location(m_Text.GetLineIndex(current.Start), m_Text.Filename);
						m_Diagnostics.ReportExpectedVariableOrFunctionDeclaration(Location, current);
						goto Error;
						break;
					}
				}
			}
		}
		
		const Token& RbraceToken = Current();
		if (!CheckAndMatchToken(TokenKind::RBrace))
		{
			goto Error;
		}
		if (!CheckAndMatchToken(TokenKind::Semicolon))
		{
			goto Error;
		}

		return MakeDeclaration_Struct(StructKeyword, pPodKeyword, Name, LbraceToken, ppMembers, ppMethods, RbraceToken, kAttributes);

	Error:
		return nullptr;
	}

	FunctionSignature ParseFunctionSignature() noexcept
	{
		// Signature: inline? static? rtype name(constexpr? argtype argname, ...) nogc?
		FunctionSignature fs = {};

		if (Current().Kind == TokenKind::InlineKeyword)
		{
			fs.InlineKeyword = Allocator::Create<Token>(Allocator::Stage::Parser, NextToken());
			fs.Attributes |= MY_FUNC_ATTR_INLINE;
		}
		if (Current().Kind == TokenKind::StaticKeyword)
		{
			fs.StaticKeyword = Allocator::Create<Token>(Allocator::Stage::Parser, NextToken());
			fs.Attributes |= MY_FUNC_ATTR_STATIC;
		}

		if (!(fs.Return = ParseTypeSpec()))
		{
			goto Error;
		}

		fs.Name = MatchToken(TokenKind::Identifier);

		fs.LparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::LParen))
		{
			goto Error;
		}

		if (Current().Kind != TokenKind::RParen)
		{
			while (Current().Kind != TokenKind::RParen && Current().Kind != TokenKind::Eof)
			{
				Parameter param = {};

				if (Current().Kind == TokenKind::ConstKeyword)
				{
					param.ConstKeyword = Allocator::Create<Token>(Allocator::Stage::Parser, NextToken());
				}
				if (!(param.Type = ParseTypeSpec()))
				{
					goto Error;
				}

				param.Name = MatchToken(TokenKind::Identifier);
				stbds_arrpush(fs.Params, param);

				if (Current().Kind != TokenKind::RParen)
				{
					if (!CheckAndMatchToken(TokenKind::Comma))
					{
						goto Error;
					}
				}
			}
		}

		fs.RparenToken = Current();
		if (!CheckAndMatchToken(TokenKind::RParen))
		{
			goto Error;
		}

		if (Current().Kind == TokenKind::NoGCKeyword)
		{
			fs.NoGCKeyword = Allocator::Create<Token>(Allocator::Stage::Parser, NextToken());
			fs.Attributes |= MY_FUNC_ATTR_NOGC;
		}

		return fs;

	Error:
		// Signature is invalid if:
		//     1. FunctionSignature.Name.Kind != Identifier
		//     2. FunctionSignature.Return == nullptr;
		return {};
	}

private:
	static int GetUnaryOperatorPrecedence(TokenKind Kind) noexcept
	{
		switch (Kind)
		{
			case TokenKind::Plus:
			case TokenKind::Dash:
			case TokenKind::Bang:
			case TokenKind::Tilde:
				return 9;
			default: return 0;
		}
	}

	static int GetBinaryOperatorPrecedence(TokenKind Kind) noexcept
	{
		switch (Kind)
		{
			case TokenKind::StarStar:
				return 10;
			case TokenKind::Star:
			case TokenKind::Slash:
				return 8;
			case TokenKind::Plus:
			case TokenKind::Dash:
				return 7;
			case TokenKind::Percent:
				return 6;
			case TokenKind::LessLess:
			case TokenKind::GreaterGreater:
				return 5;
			case TokenKind::And:
			case TokenKind::Pipe:
			case TokenKind::Caret:
				return 4;
			case TokenKind::EqualsEquals:
			case TokenKind::BangEquals:
			case TokenKind::Less:
			case TokenKind::LessEquals:
			case TokenKind::Greater:
			case TokenKind::GreaterEquals:
				return 3;
			case TokenKind::AndAnd:
				return 2;
			case TokenKind::PipePipe:
				return 1;
			default: return 0;
		}
	}

private:
	SyntaxTree&       m_Tree;
	const SourceText& m_Text;
	Token*            m_Tokens      = nullptr;
	uint32_t          m_Position    = 0u;
	DiagnosticBag     m_Diagnostics = { };
};
#pragma endregion

/// Tokens
Token::Token()
{
	memset(this, 0, sizeof(Token));
	this->Kind = TokenKind::Invalid;
}

Token::Token(TokenKind Kind)
	: Token()
{
	this->Kind = Kind;
}

Token::Token(const Token& Other)
	: Token()
{
	*this = Other;
}

Token& Token::operator=(const Token& Rhs)
{
	new(this) Token{};

	Kind  = Rhs.Kind;
	Start = Rhs.Start;
	End   = Rhs.End;

	switch (Kind)
	{
		case TokenKind::TrueKeyword:  B08 = Rhs.B08; break;
		case TokenKind::FalseKeyword: B08 = Rhs.B08; break;
		case TokenKind::Int64:        I64 = Rhs.I64; break;
		case TokenKind::Uint64:       U64 = Rhs.U64; break;
		case TokenKind::Float64:      F64 = Rhs.F64; break;
		case TokenKind::String:       Str = Rhs.Str; break;
		case TokenKind::Identifier:   Id  = Rhs.Id;  break;
		default:                                     break;
	}

	return *this;
}

TextLocation Token::Location(uint32_t kLine, const std::string_view& Filename) const noexcept
{
	return TextLocation(Start, End - Start, kLine, Filename);
}
// Type Specs
TypeSpec::TypeSpec()
{
	memset(this, 0, sizeof(TypeSpec));
	Kind = TypeSpecKind::Invalid;
}

TypeSpec::TypeSpec(TypeSpecKind Kind)
	: TypeSpec()
{
	this->Kind = Kind;
}

// Expressions
Expression::Expression()
{
	memset(this, 0, sizeof(Expression));
	Kind = ExpressionKind::Invalid;
}

Expression::Expression(ExpressionKind Kind)
	: Expression()
{
	this->Kind = Kind;
}

// Statements
Statement::Statement()
{
	memset(this, 0, sizeof(Statement));
	Kind = StatementKind::Invalid;
}

Statement::Statement(StatementKind Kind)
	: Statement()
{
	this->Kind = Kind;
}

// Declarations
Parameter::Parameter(TypeSpec* pType, const Token& Name, Token* pConstKeyword)
	: ConstKeyword(pConstKeyword), Type(pType), Name(Name) /*, Default(pDefault)*/
{ }

Declaration::Declaration()
{
	memset(this, 0, sizeof(Declaration));
	Kind = DeclarationKind::Invalid;
}

Declaration::Declaration(DeclarationKind Kind)
	: Declaration()
{
	this->Kind = Kind;
}

// Syntax Tree
SyntaxTree::SyntaxTree(MyContext* pContext, const SourceText& Text, const ParseHandler& pfnHandler)
	: m_Result(), m_Text(Text)
{
	pfnHandler(pContext, this, m_Result, m_Diagnostics);
}

SyntaxTree* SyntaxTree::Load(MyContext* pContext, const std::string_view& Filename)
{
	const char* const& lpSource = File::ReadAll(Filename);
	SourceText Text = SourceText::From(lpSource, Filename);
	return Parse(pContext, Text);
}

SyntaxTree* SyntaxTree::Parse(MyContext* pContext, const std::string_view& Text)
{
	const SourceText st = SourceText::From(Text, "<__main__>");
	return Parse(pContext, st);
}

SyntaxTree* SyntaxTree::Parse(MyContext* pContext, const SourceText& Text)
{
	return new SyntaxTree(pContext, Text, ParseInternal);
}

void SyntaxTree::ParseInternal(MyContext* pContext, SyntaxTree* pTree, ParseResult& Root, DiagnosticBag& Diagnostics)
{
	InternalParser parser = InternalParser(pContext, *pTree);
	Root = parser.ParseCompilationUnit();
	Diagnostics = parser.GetDiagnostics();
}

Token* SyntaxTree::ParseFlattened(MyContext* pContext, const std::string& Text)
{
	const SourceText st = SourceText::From(Text, "<__main__>");
	return ParseFlattened(pContext, st);
}

Token* SyntaxTree::ParseFlattened(MyContext* pContext, const SourceText& Text)
{
	Token* pTokens = nullptr;

	ParseHandler pfnHandler = [&](MyContext* pContext, SyntaxTree* pTree, ParseResult& Root, DiagnosticBag& Diagnostics) -> void
	{
		if (stbds_arrlenu(pTokens) > 0u)
		{
			stbds_arrfree(pTokens);
			pTokens = nullptr;
		}

		InternalLexer lexer = InternalLexer(pContext, *pTree);
		while (true)
		{
			Token token = lexer.NextToken();
			if (token.Kind == TokenKind::Eof)
			{
				Root = ParseResult{ {}, token };
				break;
			}

			stbds_arrpush(pTokens, token);
		}
		Diagnostics = lexer.GetDiagnostics();
	};

	(void)SyntaxTree(pContext, Text, pfnHandler);
	return pTokens;
}

// Utils
const char* TokenKindString(TokenKind Kind) noexcept
{
	switch (Kind)
	{
		case TokenKind::Invalid:	      return "Invalid";
		case TokenKind::Eof:		      return "Eof";
		case TokenKind::Int64:		      return "Int64";
		case TokenKind::Uint64:		      return "Uint64";
		case TokenKind::Float64:		  return "Float64";
		case TokenKind::String:			  return "String";
		case TokenKind::Plus:		      return "+";
		case TokenKind::Dash:		      return "-";
		case TokenKind::Star:		      return "*";
		case TokenKind::StarStar:         return "**";
		case TokenKind::Slash:		      return "/";
		case TokenKind::Percent:		  return "%";
		case TokenKind::Caret:		      return "^";
		case TokenKind::Tilde:		      return "~";
		case TokenKind::And:		      return "&";
		case TokenKind::AndAnd:			  return "&&";
		case TokenKind::Pipe:		      return "|";
		case TokenKind::PipePipe:	      return "||";
		case TokenKind::Equals:			  return "=";
		case TokenKind::EqualsEquals:     return "==";
		case TokenKind::Bang:		      return "!";
		case TokenKind::BangEquals:       return "!=";
		case TokenKind::Less:		      return "<";
		case TokenKind::LessLess:		  return "<<";
		case TokenKind::LessEquals:       return "<=";
		case TokenKind::Greater:	      return ">";
		case TokenKind::GreaterGreater:   return ">>";
		case TokenKind::GreaterEquals:    return ">=";
		case TokenKind::LParen:	          return "(";
		case TokenKind::RParen:	          return ")";
		case TokenKind::LBrace:	          return "{";
		case TokenKind::RBrace:	          return "}";
		case TokenKind::LBracket:	      return "[";
		case TokenKind::RBracket:	      return "]";
		case TokenKind::Arrow:			  return "->";
		case TokenKind::Dot:			  return ".";
		case TokenKind::Question:	      return "?";
		case TokenKind::Comma:			  return ",";
		case TokenKind::Colon:			  return ":";
		case TokenKind::Semicolon:	      return ";";

		case TokenKind::Identifier:       return "<identifier>";
		case TokenKind::NullKeyword:      return "<keyword: null>";
		case TokenKind::FalseKeyword:     return "<keyword: false>";
		case TokenKind::TrueKeyword:      return "<keyword: true>";
		case TokenKind::NewKeyword:       return "<keyword: new>";
		case TokenKind::VarKeyword:       return "<keyword: var>";
		case TokenKind::ConstKeyword:     return "<keyword: const>";
		case TokenKind::AutoKeyword:      return "<keyword: auto>";
		case TokenKind::StaticKeyword:    return "<keyword: static>";
		case TokenKind::InlineKeyword:    return "<keyword: inline>";
		case TokenKind::ConstexprKeyword: return "<keyword: constexpr>";
		case TokenKind::CallbackKeyword:  return "<keyword: callback>";
		case TokenKind::PodKeyword:       return "<keyword: pod>";

		case TokenKind::IfKeyword:	      return "<keyword: if>";
		case TokenKind::ElseKeyword:      return "<keyword: else>";
		case TokenKind::ElseifKeyword:    return "<keyword: elseif>";
		case TokenKind::ForKeyword:       return "<keyword: for>";
		case TokenKind::ForeachKeyword:   return "<keyword: foreach>";
		case TokenKind::DoKeyword:        return "<keyword: do>";
		case TokenKind::WhileKeyword:     return "<keyword: while>";
		case TokenKind::BreakKeyword:     return "<keyword: break>";
		case TokenKind::ContinueKeyword:  return "<keyword: continue>";
		case TokenKind::ReturnKeyword:    return "<keyword: return>";
		case TokenKind::ImportKeyword:    return "<keyword: import>";
		case TokenKind::UsingKeyword:     return "<keyword: using>";
		case TokenKind::ExternKeyword:    return "<keyword: extern>";
		case TokenKind::EnumKeyword:	  return "<keyword: enum>";
		case TokenKind::FunctionKeyword:  return "<keyword: function>";
		case TokenKind::StructKeyword:    return "<keyword: struct>";
		default:						  return "[invalid tokenkind]";
	}
}

const char* TypeSpecKindString(TypeSpecKind Kind) noexcept
{
	switch (Kind)
	{
		case TypeSpecKind::Name:     return "<typespec: name>";
		case TypeSpecKind::Array:    return "<typespec: array>";
		case TypeSpecKind::Function: return "<typespec: function>";
		default:                     return "[invalid typespec]";
	}
}

const char* ExpressionKindString(ExpressionKind Kind) noexcept
{
	switch (Kind)
	{
		case ExpressionKind::Literal:	    return "<expr: lietral>";
		case ExpressionKind::Unary:		    return "<expr: unary>";
		case ExpressionKind::Binary:	    return "<expr: binary>";
		case ExpressionKind::Ternary:	    return "<expr: ternary>";
		case ExpressionKind::Parenthesized:	return "<expr: parenthesized>";
		case ExpressionKind::Name:		    return "<expr: name>";
		case ExpressionKind::Assignment:	return "<expr: assignment>";
		case ExpressionKind::OperatorNew:   return "<expr: operator-new>";
		case ExpressionKind::Call:		    return "<expr: call>";
		case ExpressionKind::Index:		    return "<expr: index>";
		case ExpressionKind::Field:		    return "<expr: field>";
		case ExpressionKind::Array:		    return "<expr: array>";
		default:						    return "[invalid expression]";
	}
}

const char* StatementKindString(StatementKind Kind) noexcept
{
	switch (Kind)
	{
		case StatementKind::Block:                return "<stmt: block>";
		case StatementKind::Expression:           return "<stmt: expression>";
		case StatementKind::VariableDeclaration:  return "<stmt: variable-declaration>";
		case StatementKind::DecomposeDeclaration: return "<stmt: decompose-declaration>";
		case StatementKind::If:                   return "<stmt: if>";
		case StatementKind::For:                  return "<stmt: for>";
		case StatementKind::Foreach:              return "<stmt: foreach>";
		case StatementKind::While:                return "<stmt: while>";
		case StatementKind::Break:                return "<stmt: break>";
		case StatementKind::Continue:             return "<stmt: continue>";
		case StatementKind::Return:               return "<stmt: return>";
		default:                                  return "[invalid statement]";
	}
}

const char* DeclarationKindString(DeclarationKind Kind) noexcept
{
	switch (Kind)
	{
		case DeclarationKind::Import:     return "<decl: import>";
		case DeclarationKind::Using:      return "<decl: using>";
		case DeclarationKind::Extern:     return "<decl: extern>";
		case DeclarationKind::Enum:       return "<decl: enum>";
		case DeclarationKind::BitFlags:   return "<decl: bitflags>";
		case DeclarationKind::Variable:   return "<decl: variable>";
		case DeclarationKind::Function:   return "<decl: function>";
		case DeclarationKind::Struct:     return "<decl: struct>";
		default:                          return "[invalid declaration]";
	}
}

// Creators
#pragma region Definitions_For_Creator_Functions
TypeSpec* MakeTypeSpec_Name(const Token& Identifier)
{
	TypeSpec* pType = Allocator::Create<TypeSpec>(Allocator::Stage::Parser, TypeSpecKind::Name);
	new(&pType->name) NameTypeSpec{ Identifier };
	return pType;
}

TypeSpec* MakeTypeSpec_Array(TypeSpec* pType, const Token& LbracketToken, Expression** ppCounts, const Token& RbracketToken)
{
	TypeSpec* pArrayType = Allocator::Create<TypeSpec>(Allocator::Stage::Parser, TypeSpecKind::Array);
	new(&pArrayType->array) ArrayTypeSpec{ pType, LbracketToken, ppCounts, RbracketToken };
	return pArrayType;
}

TypeSpec* MakeTypeSpec_Function(
	const Token& CallbackKeyword,
	const Token& LparenToken0,
	TypeSpec*    pRType,
	const Token& LparenToken1,
	TypeSpec**   ppParamTypes,
	const Token& RparenToken1,
	const Token& RparenToken0
)
{
	TypeSpec* pType = Allocator::Create<TypeSpec>(Allocator::Stage::Parser, TypeSpecKind::Function);
	new(&pType->func) FunctionTypeSpec
	{
		CallbackKeyword,
		LparenToken0,
		pRType,
		LparenToken1,
		ppParamTypes,
		RparenToken1,
		RparenToken0,
	};
	return pType;
}

Expression* MakeExpression_Literal(const Token& Literal)
{
	Expression* pLiteral = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Literal);
	new(&pLiteral->literal) LiteralExpression{ Literal };

	switch (Literal.Kind)
	{
		case TokenKind::FalseKeyword: pLiteral->literal.Literal.B08 = false; break;
		case TokenKind::TrueKeyword:  pLiteral->literal.Literal.B08 = true;  break;
		default: break;
	}

	return pLiteral;
}

Expression* MakeExpression_Unary(const Token& Operator, Expression* pRhs)
{
	Expression* pUnary = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Unary);
	new(&pUnary->unary) UnaryExpression{ Operator, pRhs };
	return pUnary;
}

Expression* MakeExpression_Binary(Expression* pLhs, const Token& Operator, Expression* pRhs)
{
	Expression* pUnary = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Binary);
	new(&pUnary->unary) BinaryExpression{ pLhs, Operator, pRhs };
	return pUnary;
}

Expression* MakeExpression_Ternary(Expression* pCondition, const Token& QuestionToken, Expression* pThen, const Token& ColonToken, Expression* pElse)
{
	Expression* pUnary = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Ternary);
	new(&pUnary->unary) TernaryExpression{ pCondition, QuestionToken, pThen, ColonToken, pElse };
	return pUnary;
}

Expression* MakeExpression_Parenthesized(const Token& LparenToken, Expression* pExpr, const Token& RparenToken)
{
	Expression* pParen = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Parenthesized);
	new(&pParen->paren) ParenthesizedExpression{ LparenToken, pExpr, RparenToken };
	return pParen;
}

Expression* MakeExpression_Name(const Token& Identifier)
{
	Expression* pName = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Name);
	new(&pName->name) NameExpression{ Identifier };
	return pName;
}

Expression* MakeExpression_Assignment(Expression* pLhs, const Token& EqualsToken, Expression* pRhs)
{
	Expression* pAssign = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Assignment);
	new(&pAssign->assign) AssignmentExpression{ pLhs, EqualsToken, pRhs };
	return pAssign;
}

Expression* MakeExpression_OperatorNew(const Token& NewKeyword, TypeSpec* pType, const Token& LbraceToken, Expression* pInitializer, const Token& RbraceToken)
{
	Expression* pOperatorNew = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::OperatorNew);
	new(&pOperatorNew->opnew) OperatorNewExpression{ NewKeyword, pType, LbraceToken, nullptr, RbraceToken, false };
	pOperatorNew->opnew.Initializer = pInitializer;
	return pOperatorNew;
}

Expression* MakeExpression_OperatorNew(const Token& NewKeyword, TypeSpec* pType, const Token& LbraceToken, FieldInitializer* pFields, const Token& RbraceToken)
{
	Expression* pOperatorNew = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::OperatorNew);
	new(&pOperatorNew->opnew) OperatorNewExpression{ NewKeyword, pType, LbraceToken, nullptr, RbraceToken, true };
	pOperatorNew->opnew.Fields = pFields;
	return pOperatorNew;
}

Expression* MakeExpression_Call(Expression* pCallable, const Token& LparenToken, Expression** ppArguments, const Token& RparenToken)
{
	Expression* pCall = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Call);
	new(&pCall->call) CallExpression{ pCallable, LparenToken, ppArguments, RparenToken };
	return pCall;
}

Expression* MakeExpression_Index(Expression* pSequence, const Token& LbracketToken, Expression** ppIndices, const Token& RbracketToken)
{
	Expression* pIndexExpr = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Index);
	new(&pIndexExpr->index) IndexExpression{ pSequence, LbracketToken, ppIndices, RbracketToken };
	return pIndexExpr;
}

Expression* MakeExpression_Field(Expression* pObject, const Token& DotToken, const Token& Field)
{
	Expression* pField = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Field);
	new(&pField->field) FieldExpression{ pObject, DotToken, Field };
	return pField;
}

Expression* MakeExpression_Array(const Token& LbraceToken, Expression** ppItems, const Token& RbraceToken)
{
	Expression* pArray = Allocator::Create<Expression>(Allocator::Stage::Parser, ExpressionKind::Array);
	new(&pArray->array) ArrayExpression{ LbraceToken, ppItems, RbraceToken };
	return pArray;
}

Statement* MakeStatement_Block(const Token& LbraceToken, Statement** ppStmts, const Token& RbraceToken)
{
	Statement* pBlock = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::Block);
	new(&pBlock->block) BlockStatement{ LbraceToken, ppStmts, RbraceToken };
	return pBlock;
}

Statement* MakeStatement_Expression(Expression* pExpr)
{
	Statement* pExprStmt = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::Expression);
	new(&pExprStmt->expr) ExpressionStatement{ pExpr };
	return pExprStmt;
}

Statement* MakeStatement_VariableDeclaration(const Token& VarKeyword, TypeSpec* pType, const Token& Identifier, const Token& EqualsToken, Expression* pValue, bool bIsReadonly)
{
	Statement* pVarDecl = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::VariableDeclaration);
	new(&pVarDecl->var) VariableDeclarationStatement{ VarKeyword, pType, Identifier, EqualsToken, pValue, bIsReadonly };
	return pVarDecl;
}

Statement* MakeStatement_DecomposeDeclaration(const Token& AutoKeyword, const Token& LbracketToken, Token* pIdentifiers, const Token& RbracketToken, const Token& EqualsToken, Expression* pDecomposable)
{
	Statement* pDecompDecl = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::DecomposeDeclaration);
	new(&pDecompDecl->decomp) DecomposeDeclarationStatement{ AutoKeyword, LbracketToken, pIdentifiers, RbracketToken, EqualsToken, pDecomposable };
	return pDecompDecl;
}

Statement* MakeStatement_If(const Token& IfKeyword, const Token& LparenToken, Expression* pCondition, const Token& RparenToken, Statement* pIfBlock, ElseIfStatement* pElseIfs, const Token& ElseKeyword, Statement* pElseBlock)
{
	Statement* pIf = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::If);
	new(&pIf->ifstmt) IfStatement
	{
		IfKeyword,
		LparenToken,
		pCondition,
		RparenToken,
		pIfBlock,
		pElseIfs,
		ElseKeyword,
		pElseBlock,
	};
	return pIf;
}

Statement* MakeStatement_For(const Token& ForKeyword, const Token& LparenToken, const Token& Identifier, Expression* pLowerBound, Expression* pUpperBound, Expression* pStep, const Token& RparenToken, Statement* pBody)
{
	Statement* pFor = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::For);
	new(&pFor->forstmt) ForStatement
	{
		ForKeyword,
		LparenToken,
		Identifier,
		pLowerBound,
		pUpperBound,
		pStep,
		RparenToken,
		pBody
	};
	return pFor;
}

Statement* MakeStatement_Foreach(const Token& ForeachKeyword, const Token& LparenToken, const Token& Identifier, const Token& ColonToken, Expression* pIterable, const Token& RparenToken, Statement* pBody)
{
	Statement* pForeach = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::Foreach);
	new(&pForeach->foreach) ForeachStatement
	{
		ForeachKeyword,
		LparenToken,
		Identifier,
		ColonToken,
		pIterable,
		RparenToken,
		pBody,
	};
	return pForeach;
}

Statement* MakeStatement_While(const Token& WhileKeyword, const Token& LparenToken, Expression* pCondition, const Token& RparenToken, Statement* pBody)
{
	Statement* pWhile = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::While);
	new(&pWhile->whilestmt) WhileStatement
	{
		WhileKeyword,
		LparenToken,
		pCondition,
		RparenToken,
		pBody
	};
	return pWhile;
}

Statement* MakeStatement_DoWhile(const Token& DoKeyword, Statement* pBody, const Token& WhileKeyword, const Token& LparenToken, Expression* pCondition, const Token& RparenToken)
{
	Statement* pDoWhile = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::DoWhile);
	new(&pDoWhile->dowhile) DoWhileStatement
	{
		DoKeyword,
		pBody,
		WhileKeyword,
		LparenToken,
		pCondition,
		RparenToken
	};
	return pDoWhile;
}

Statement* MakeStatement_Break(const Token& BreakKeyword)
{
	Statement* pBreak = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::Break);
	new(&pBreak->breakstmt) BreakStatement{ BreakKeyword };
	return pBreak;
}

Statement* MakeStatement_Continue(const Token& ContinueKeyword)
{
	Statement* pContinue = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::Continue);
	new(&pContinue->continuestmt) ContinueStatement{ ContinueKeyword };
	return pContinue;
}

Statement* MakeStatement_Return(const Token& ReturnKeyword, Expression* pExpr)
{
	Statement* pReturn = Allocator::Create<Statement>(Allocator::Stage::Parser, StatementKind::Return);
	new(&pReturn->returnstmt) ReturnStatement{ ReturnKeyword, pExpr };
	return pReturn;
}

Declaration* MakeDeclaration_Import(const Token& ImportKeyword, const Token& Name)
{
	Declaration* pImport = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Import);
	new(&pImport->importdecl) ImportDeclaration{ ImportKeyword, Name };
	return pImport;
}

Declaration* MakeDeclaration_Using(const Token& UsingKeyword, const Token& Name, const Token& EqualsToken, TypeSpec* pType)
{
	Declaration* pUsing = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Using);
	new(&pUsing->usingdecl) UsingDeclaration{ UsingKeyword, Name, EqualsToken, pType };
	return pUsing;
}

Declaration* MakeDeclaration_Extern(const Token& ExternKeyword, const Token& FunctionKeyword, const FunctionSignature& Signature)
{
	Declaration* pExtern = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Extern);
	new(&pExtern->externdecl) ExternDeclaration{ ExternKeyword, FunctionKeyword, Signature };
	return pExtern;
}

Declaration* MakeDeclaration_Enum(const Token& EnumKeyword, const Token& Name, const Token& LbraceToken, Token* pValues, const Token& RbraceToken)
{
	Declaration* pEnum = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Enum);
	new(&pEnum->enumdecl) EnumDeclaration{ EnumKeyword, Name, LbraceToken, pValues, RbraceToken };
	return pEnum;
}

Declaration* MakeDeclaration_BitFlags(const Token& BflagsKeyword, const Token& LparenToken, Expression* pBase, const Token& RparenToken, const Token& Name, const Token& LbraceToken, Token* pValues, const Token& RbraceToken)
{
	Declaration* pBitFlags = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::BitFlags);
	new(&pBitFlags->bflagsdecl) BitFlagsDeclaration
	{
		BflagsKeyword,
		LparenToken,
		pBase,
		RparenToken,
		Name,
		LbraceToken,
		pValues,
		RbraceToken
	};
	return pBitFlags;
}

Declaration* MakeDeclaration_Variable(Statement* pVarDeclStmt)
{
	Declaration* pVarDecl = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Variable);
	new(&pVarDecl->vardecl) VariableDeclaration{ pVarDeclStmt->var };
	return pVarDecl;
}

Declaration* MakeDeclaration_Variable(const Token& VarKeyword, TypeSpec* pType, const Token& Identifier, const Token& EqualsToken, Expression* pValue, bool bIsReadonly)
{
	Declaration* pVarDecl = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Variable);
	new(&pVarDecl->vardecl) VariableDeclaration{ VarKeyword, pType, Identifier, EqualsToken, pValue, bIsReadonly };
	return pVarDecl;
}

Declaration* MakeDeclaration_Function(const Token& FunctionKeyword, const FunctionSignature& Signature, Statement* pBody, uint32_t kAttributes)
{
	Declaration* pFuncDecl = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Function);
	new(&pFuncDecl->funcdecl) FunctionDeclaration{ FunctionKeyword, Signature, pBody, kAttributes };
	return pFuncDecl;
}

Declaration* MakeDeclaration_Forward(const Token& StructKeyword, const Token& Name)
{
	Declaration* pForwardDecl = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Forward);
	new(&pForwardDecl->forward) ForwardDeclaration
	{
		StructKeyword,
		Name,
	};
	return pForwardDecl;
}

Declaration* MakeDeclaration_Struct(const Token& StructKeyword, Token* pTrivialKeyword, const Token& Name, const Token& LbraceToken, Statement** ppMembers, Declaration** ppMethods, const Token& RbraceToken, uint32_t kAttributes)
{
	Declaration* pStructDecl = Allocator::Create<Declaration>(Allocator::Stage::Parser, DeclarationKind::Struct);
	new(&pStructDecl->structdecl) StructDeclaration
	{
		StructKeyword,
		pTrivialKeyword,
		Name,
		LbraceToken,
		ppMembers,
		ppMethods,
		RbraceToken,
		kAttributes
	};
	return pStructDecl;
}
#pragma endregion


void _My_Initialize_KeywordMap() noexcept
{
	if (stbds_shlenu(s_KeywordMap) > 0u)
	{
		return;
	}

	stbds_shdefault(s_KeywordMap, TokenKind::Invalid);

	stbds_shput(s_KeywordMap, "null",  TokenKind::NullKeyword);
	stbds_shput(s_KeywordMap, "false", TokenKind::FalseKeyword);
	stbds_shput(s_KeywordMap, "true",  TokenKind::TrueKeyword);
	stbds_shput(s_KeywordMap, "new",   TokenKind::NewKeyword);

	stbds_shput(s_KeywordMap, "var",       TokenKind::VarKeyword);
	stbds_shput(s_KeywordMap, "const",     TokenKind::ConstKeyword);
	stbds_shput(s_KeywordMap, "auto",      TokenKind::AutoKeyword);
	stbds_shput(s_KeywordMap, "static",    TokenKind::StaticKeyword);
	stbds_shput(s_KeywordMap, "inline",    TokenKind::InlineKeyword);
	stbds_shput(s_KeywordMap, "constexpr", TokenKind::ConstexprKeyword);
	stbds_shput(s_KeywordMap, "nogc",      TokenKind::NoGCKeyword);
	stbds_shput(s_KeywordMap, "callback",  TokenKind::CallbackKeyword);
	stbds_shput(s_KeywordMap, "pod",       TokenKind::PodKeyword);

	stbds_shput(s_KeywordMap, "if",      TokenKind::IfKeyword);
	stbds_shput(s_KeywordMap, "elseif",  TokenKind::ElseifKeyword);
	stbds_shput(s_KeywordMap, "else",    TokenKind::ElseKeyword);
	stbds_shput(s_KeywordMap, "for",     TokenKind::ForKeyword);
	stbds_shput(s_KeywordMap, "foreach", TokenKind::ForeachKeyword);
	stbds_shput(s_KeywordMap, "do",      TokenKind::DoKeyword);
	stbds_shput(s_KeywordMap, "while",   TokenKind::WhileKeyword);

	stbds_shput(s_KeywordMap, "break",    TokenKind::BreakKeyword);
	stbds_shput(s_KeywordMap, "continue", TokenKind::ContinueKeyword);
	stbds_shput(s_KeywordMap, "return",   TokenKind::ReturnKeyword);

	stbds_shput(s_KeywordMap, "import",   TokenKind::ImportKeyword);
	stbds_shput(s_KeywordMap, "using",    TokenKind::UsingKeyword);
	stbds_shput(s_KeywordMap, "extern",   TokenKind::ExternKeyword);
	stbds_shput(s_KeywordMap, "enum",     TokenKind::EnumKeyword);
	stbds_shput(s_KeywordMap, "bflags",   TokenKind::BflagsKeyword);
	stbds_shput(s_KeywordMap, "function", TokenKind::FunctionKeyword);
	stbds_shput(s_KeywordMap, "struct",   TokenKind::StructKeyword);
}

#ifdef MY_DEBUG
template<typename Iterable>
static void _PrettyPrint_Array(Iterable* const& pItems, const char* lpSep) noexcept
{
	const size_t kLength = stbds_arrlen(pItems);
	for (size_t k = 0; k < kLength; k++)
	{
		PrettyPrint(pItems[k]);
		Console::Write(k == kLength-1 ? "" : lpSep);
	}
}

static void _PrettyPrint_Parameters(Parameter* const& pParams) noexcept
{
	const size_t kLength = stbds_arrlen(pParams);
	for (size_t k = 0; k < kLength; k++)
	{
		const Parameter& param = pParams[k];
		if (param.ConstKeyword)
		{
			Console::Write("const ");
		}
		PrettyPrint(param.Type);
		Console::Write(" ");
		PrettyPrint(param.Name);
		Console::Write(k == kLength - 1 ? "" : ", ");
	}
}

static void _PrettyPrint_Signature(const FunctionSignature& Signature) noexcept
{
	if (Signature.InlineKeyword)
	{
		Console::Write("inline ");
	}
	if (Signature.StaticKeyword)
	{
		Console::Write("static ");
	}
	PrettyPrint(Signature.Return);
	Console::Write(" ");
	PrettyPrint(Signature.Name);
	Console::Write("(");
	_PrettyPrint_Parameters(Signature.Params);
	Console::Write(")");
	if (Signature.NoGCKeyword)
	{
		Console::Write(" nogc");
	}
}

void PrettyPrint(const Token& Token, const std::string& Indent) noexcept
{
	switch (Token.Kind)
	{
		case TokenKind::NullKeyword:  Console::Write("null");  break;
		case TokenKind::FalseKeyword: Console::Write("false"); break;
		case TokenKind::TrueKeyword:  Console::Write("true");  break;
		case TokenKind::Int64:		  Console::Write("%I64d",  Token.I64);        break;
		case TokenKind::Uint64:	      Console::Write("%I64u",  Token.U64);        break;
		case TokenKind::Float64:	  Console::Write("%1.15g", Token.F64);        break;
		case TokenKind::String:		  Console::Write("'%s'",   Token.Str->Chars); break;
		case TokenKind::Identifier:   Console::Write("%s",     Token.Id);         break;
		default:
			Console::Write(TokenKindString(Token.Kind));
			break;
	}
}

void PrettyPrint(TypeSpec* pType, const std::string& Indent) noexcept
{
	Console::Write(Indent);
	switch (pType->Kind)
	{
		case TypeSpecKind::Name:
		{
			PrettyPrint(pType->name.Name);
			break;
		}
		case TypeSpecKind::Array:
		{
			PrettyPrint(pType->array.Type);
			Console::Write("[");
			const size_t kRank = stbds_arrlenu(pType->array.Counts);
			for (size_t k = 0; k < kRank; k++)
			{
				PrettyPrint(pType->array.Counts[k]);
				Console::Write(k==kRank-1 ? "" : ", ");
			}
			Console::Write("]");
			break;
		}
		case TypeSpecKind::Function:
		{
			Console::Write("callback(");
			PrettyPrint(pType->func.Return);
			Console::Write("(");
			_PrettyPrint_Array(pType->func.Parameters, ", ");
			Console::Write("))");
			break;
		}
		default: break;
	}
}

void PrettyPrint(Expression* pExpr, const std::string& Indent) noexcept
{
	switch (pExpr->Kind)
	{
		case ExpressionKind::Literal:
		{
			PrettyPrint(pExpr->literal.Literal);
			break;
		}
		case ExpressionKind::Unary:
		{
			Console::Write("(");
			PrettyPrint(pExpr->unary.Operator);
			PrettyPrint(pExpr->unary.Rhs);
			Console::Write(")");
			break;
		}
		case ExpressionKind::Binary:
		{
			Console::Write("(");
			PrettyPrint(pExpr->binary.Lhs);
			PrettyPrint(pExpr->binary.Operator);
			PrettyPrint(pExpr->binary.Rhs);
			Console::Write(")");
			break;
		}
		case ExpressionKind::Ternary:
		{
			PrettyPrint(pExpr->ternary.Condition);
			Console::Write(" ? ");
			PrettyPrint(pExpr->ternary.Then);
			Console::Write(" : ");
			PrettyPrint(pExpr->ternary.Else);
			break;
		}
		case ExpressionKind::Parenthesized:
		{
			Console::Write("(");
			PrettyPrint(pExpr->paren.Expr);
			Console::Write(")");
			break;
		}
		case ExpressionKind::Name:
		{
			PrettyPrint(pExpr->name.Identifier);
			break;
		}
		case ExpressionKind::Assignment:
		{
			PrettyPrint(pExpr->assign.Lhs);
			Console::Write(" = ");
			PrettyPrint(pExpr->assign.Rhs);
			break;
		}
		case ExpressionKind::OperatorNew:
		{
			Console::Write("new ");
			PrettyPrint(pExpr->opnew.Type);
			Console::Write("(");
			if (pExpr->opnew.IsStructInitializer)
			{
				const size_t kLength = stbds_arrlenu(pExpr->opnew.Fields);
				for (size_t k = 0; k < kLength; k++)
				{
					const FieldInitializer& fi = pExpr->opnew.Fields[k];
					PrettyPrint(fi.Name);
					Console::Write(":");
					PrettyPrint(fi.Value);
					Console::Write(k == kLength - 1 ? "" : ", ");
				}
			}
			else
			{
				if (pExpr->opnew.Initializer)
				{
					PrettyPrint(pExpr->opnew.Initializer);
				}
			}
			Console::Write(")");
			break;
		}
		case ExpressionKind::Call:
		{
			PrettyPrint(pExpr->call.Callable);
			Console::Write("(");
			_PrettyPrint_Array(pExpr->call.Arguments, ", ");
			Console::Write(")");
			break;
		}
		case ExpressionKind::Index:
		{
			PrettyPrint(pExpr->index.Sequence);
			Console::Write("[");
			const size_t kCount = stbds_arrlenu(pExpr->index.Indices);
			for (size_t k = 0; k < kCount; k++)
			{
				PrettyPrint(pExpr->index.Indices[k]);
				Console::Write(k == kCount-1 ? "" : ", ");
			}
			Console::Write("]");
			break;
		}
		case ExpressionKind::Field:
		{
			PrettyPrint(pExpr->field.Object);
			Console::Write(".");
			PrettyPrint(pExpr->field.Field);
			break;
		}
		case ExpressionKind::Array:
		{
			Console::Write("{ ");
			_PrettyPrint_Array(pExpr->array.Items, ", ");
			Console::Write(" }");
			break;
		}
		default: break;
	}
}

void PrettyPrint(Statement* pStmt, const std::string& Indent) noexcept
{
	switch (pStmt->Kind)
	{
		case StatementKind::Block:
		{
			Console::WriteLine("%s{", Indent.c_str());
			for (size_t k = 0; k < stbds_arrlenu(pStmt->block.Stmts); k++)
			{
				PrettyPrint(pStmt->block.Stmts[k], Indent + "    ");
			}
			Console::WriteLine("%s}", Indent.c_str());
			break;
		}
		case StatementKind::Expression:
		{
			Console::Write("%s", Indent.c_str());
			PrettyPrint(pStmt->expr.Expr);
			Console::WriteLine(";");
			break;
		}
		case StatementKind::VariableDeclaration:
		{
			Console::Write("%s", Indent.c_str());
			Console::Write("%s ", pStmt->var.IsReadonly ? "const" : "var");
			PrettyPrint(pStmt->var.Type);
			Console::Write(" ");
			PrettyPrint(pStmt->var.Identifier);
			if (pStmt->var.Value)
			{
				Console::Write(" = ");
				PrettyPrint(pStmt->var.Value);
			}
			Console::WriteLine(";");
			break;
		}
		case StatementKind::DecomposeDeclaration:
		{
			Console::Write("%s", Indent.c_str());
			Console::Write("auto [");
			_PrettyPrint_Array(pStmt->decomp.Identifiers, ", ");
			Console::Write("] = ");
			PrettyPrint(pStmt->decomp.Decomposable);
			Console::WriteLine(";");
			break;
		}
		case StatementKind::If:
		{
			Console::Write("%sif (", Indent.c_str());
			PrettyPrint(pStmt->ifstmt.Condition);
			Console::WriteLine(")");
			PrettyPrint(pStmt->ifstmt.IfBlock, Indent);
			const size_t kLength = stbds_arrlenu(pStmt->ifstmt.ElseIfs);
			for (size_t k = 0; k < kLength; k++)
			{
				const IfStatement::ElseIfStatement& eis = pStmt->ifstmt.ElseIfs[k];
				Console::Write("%selseif (", Indent.c_str());
				PrettyPrint(eis.Condition);
				Console::WriteLine(")");
				PrettyPrint(eis.ElseifBlock, Indent);
			}
			if (pStmt->ifstmt.ElseBlock)
			{
				Console::WriteLine("%selse", Indent.c_str());
				PrettyPrint(pStmt->ifstmt.ElseBlock, Indent);
			}
			break;
		}
		case StatementKind::For:
		{
			Console::Write("%sfor (", Indent.c_str());
			PrettyPrint(pStmt->forstmt.Identifier);
			Console::Write(", ");
			PrettyPrint(pStmt->forstmt.LowerBound);
			Console::Write(", ");
			PrettyPrint(pStmt->forstmt.UpperBound);
			Console::Write(", ");
			if (pStmt->forstmt.Step)
			{
				PrettyPrint(pStmt->forstmt.Step);
			}
			else
			{
				Console::Write("1");
			}
			Console::WriteLine(")");
			PrettyPrint(pStmt->forstmt.Body, Indent);
			break;
		}
		case StatementKind::Foreach:
		{
			Console::Write("%sforeach (", Indent.c_str());
			PrettyPrint(pStmt->foreach.Identifier);
			Console::Write(" : ");
			PrettyPrint(pStmt->foreach.Iterable);
			Console::WriteLine(")");
			PrettyPrint(pStmt->foreach.Body, Indent);
			break;
		}
		case StatementKind::While:
		{
			Console::Write("%swhile (", Indent.c_str());
			PrettyPrint(pStmt->whilestmt.Condition);
			Console::WriteLine(")");
			PrettyPrint(pStmt->whilestmt.Body, Indent);
			break;
		}
		case StatementKind::DoWhile:
		{
			Console::WriteLine("%sdo", Indent.c_str());
			PrettyPrint(pStmt->dowhile.Body, Indent);
			Console::Write("%swhile (", Indent.c_str());
			PrettyPrint(pStmt->dowhile.Condition);
			Console::WriteLine(");");
			break;
		}
		case StatementKind::Break:
		{
			Console::WriteLine("%sbreak;", Indent.c_str());
			break;
		}
		case StatementKind::Continue:
		{
			Console::WriteLine("%scontinue;", Indent.c_str());
			break;
		}
		case StatementKind::Return:
		{
			Console::Write("%sreturn ", Indent.c_str());
			PrettyPrint(pStmt->returnstmt.Expr);
			Console::WriteLine(";");
			break;
		}
		default: break;
	}
}

void PrettyPrint(Declaration* pDecl, const std::string& Indent) noexcept
{
	switch (pDecl->Kind)
	{
		case DeclarationKind::Import:
		{
			Console::Write("import ");
			PrettyPrint(pDecl->importdecl.Name);
			Console::WriteLine(";");
			break;
		}
		case DeclarationKind::Using:
		{
			Console::Write("using ");
			PrettyPrint(pDecl->usingdecl.Name);
			Console::Write(" = ");
			PrettyPrint(pDecl->usingdecl.Type);
			Console::WriteLine(";");
			break;
		}
		case DeclarationKind::Extern:
		{
			Console::Write("extern function ");
			_PrettyPrint_Signature(pDecl->externdecl.Signature);
			Console::WriteLine(";");
			break;
		}
		case DeclarationKind::Enum:
		{
			size_t k = 0ul;
			Console::Write("enum ");
			PrettyPrint(pDecl->enumdecl.Name);
			Console::WriteLine("\n{");
			const size_t kLength = stbds_arrlenu(pDecl->enumdecl.Values);
			for (size_t k = 0; k < kLength; k++)
			{
				const Token& Value = pDecl->enumdecl.Values[k];
				Console::Write("    ");
				PrettyPrint(Value);
				Console::WriteLine(k == kLength-1 ? "" : ",");
			}
			Console::WriteLine("};");
			break;
		}
		case DeclarationKind::BitFlags:
		{
			Console::Write("bflags");
			Console::Write("(");
			PrettyPrint(pDecl->bflagsdecl.Base);
			Console::Write(") ");
			PrettyPrint(pDecl->bflagsdecl.Name);
			Console::WriteLine("\n{");
			const size_t kLength = stbds_arrlenu(pDecl->bflagsdecl.Values);
			for (size_t k = 0; k < kLength; k++)
			{
				const Token& Value = pDecl->bflagsdecl.Values[k];
				Console::Write("    ");
				PrettyPrint(Value);
				Console::WriteLine(k == kLength-1 ? "" : ",");
			}
			Console::WriteLine("};");
			break;
		}
		case DeclarationKind::Variable:
		{
			Console::Write("%s ", pDecl->vardecl.IsReadonly ? "const" : "var");
			PrettyPrint(pDecl->vardecl.Type);
			Console::Write(" ");
			PrettyPrint(pDecl->vardecl.Identifier);
			if (pDecl->vardecl.Value)
			{
				Console::Write(" = ");
				PrettyPrint(pDecl->vardecl.Value);
			}
			Console::WriteLine(";");
			break;
		}
		case DeclarationKind::Function:
		{
			if (pDecl->funcdecl.Attributes != 0u)
			{
				Console::Write("%s[", Indent.c_str());
				if (pDecl->funcdecl.Attributes & MY_FUNC_ATTR_INLINE) { Console::Write("FuncAttr.Inline | "); }
				if (pDecl->funcdecl.Attributes & MY_FUNC_ATTR_STATIC) { Console::Write("FuncAttr.Static | "); }
				if (pDecl->funcdecl.Attributes & MY_FUNC_ATTR_CTOR)   { Console::Write("FuncAttr.Ctor | ");   }
				if (pDecl->funcdecl.Attributes & MY_FUNC_ATTR_METHOD) { Console::Write("FuncAttr.Method | "); }
				if (pDecl->funcdecl.Attributes & MY_FUNC_ATTR_NOGC)   { Console::Write("FuncAttr.NoGC | ");   }
				Console::WriteLine("%s] ", "\b\b\b");
			}
			Console::Write("%sfunction ", Indent.c_str());
			_PrettyPrint_Signature(pDecl->funcdecl.Signature);
			Console::WriteLine();
			PrettyPrint(pDecl->funcdecl.Body, Indent);
			break;
		}
		case DeclarationKind::Struct:
		{
			if (pDecl->structdecl.Attributes != 0u)
			{
				Console::Write("[");
				if (pDecl->structdecl.Attributes & MY_STRUCT_ATTR_NONE)    Console::Write("StructAttr.None");
				if (pDecl->structdecl.Attributes & MY_STRUCT_ATTR_POD) Console::Write("StructAttr.Trivial");
				Console::WriteLine("]");
			}
			Console::Write("struct ");
			if (pDecl->structdecl.PodKeyword)
			{
				Console::Write("trivial ");
			}
			PrettyPrint(pDecl->structdecl.Name);
			Console::WriteLine("\n{");
			size_t kLength = stbds_arrlenu(pDecl->structdecl.Members);
			for (size_t k = 0; k < kLength; k++)
			{
				Statement* const& pMember = pDecl->structdecl.Members[k];
				PrettyPrint(pMember, "    ");
			}
			kLength = stbds_arrlenu(pDecl->structdecl.Methods);
			for (size_t k = 0; k < kLength; k++)
			{
				Declaration* const& pMethod = pDecl->structdecl.Methods[k];
				PrettyPrint(pMethod, "    ");
			}
			Console::WriteLine("};");
			break;
		}
		default: break;
	}
}
#endif // MY_DEBUG


