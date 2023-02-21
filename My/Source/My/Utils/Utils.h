#pragma once

#include "My/Base/Core.h"
#include "My/Syntax/Text.h"

#include <vector>

enum class TokenKind;

/// <summary>
/// - This is not an allocator for everything. It creates parse tree and bound tree objects
/// to collect them afterwards. Collection is done after emitting bytecode
/// - Runtime memory allocation is done by the garbage collector
/// </summary>
class Allocator
{
public:
	enum class Stage
	{
		Parser,
		Binder,
		Runtime,
	};

public:
	template<typename Tp, typename... TpArgs>
	static Tp* Create(Stage stage, TpArgs&&... Args) noexcept
	{
		return new(AllocateInternal(sizeof(Tp), stage)) Tp{ std::forward<TpArgs>(Args)... };
	}

	static void Cleanup(Stage stage) noexcept;
	static void FullCleanup() noexcept;

private:
	static void* AllocateInternal(size_t kSize, Stage stage) noexcept;
};


class Random
{
public:
	static int64_t     Int(int64_t iMax = 0ll) noexcept;
	static uint64_t    Uint(uint64_t kMax = 0ull) noexcept;
	static double      Float() noexcept;
	static double      Float(double First, double Last) noexcept;
	template<typename Item>
	static const Item& Choice(const Item* pItems, size_t kLength) noexcept
	{
		return pItems[Uint(kLength)];
	}
};


struct Diagnostic
{
	TextLocation Location;
	std::string  Message = {};

	Diagnostic(const TextLocation& Location, const std::string& Message)
		: Location(Location), Message(Message)
	{ }
};

class DiagnosticBag : public std::vector<Diagnostic>
{
public:
	using Super = std::vector<Diagnostic>;

public:
	DiagnosticBag();
	virtual ~DiagnosticBag() noexcept = default;

	// Other
	void ReportUnknownError(const TextLocation& Location, const std::string& Message = "Unknown Error") noexcept;
	void ReportFeatureNotImplemented(const TextLocation& Location, const char* lpFeature) noexcept;
	void ReportCompilerError(const TextLocation& Location, const std::string& Message) noexcept;
	// Lexing
	void ReportBadCharacter(const TextLocation& Location, char Character) noexcept;
	void ReportInvalidNumber(const TextLocation& Location, const std::string& Text, MyType* pType) noexcept;
	void ReportUnterminatedString(const TextLocation& Location) noexcept;
	void ReportUnterminatedMultilineComment(const TextLocation& Location) noexcept;
	void ReportInvalidEscapeCharacterInStringLiteral(const TextLocation& Location, char Character) noexcept;
	// Parsing
	void ReportUnexpectedToken(const TextLocation& Location, TokenKind Kind, TokenKind ExpectedKind) noexcept;
	void ReportMissingTypename(const TextLocation& Location) noexcept;
	void ReportExpectedIdentifierToken(const TextLocation& Location) noexcept;
	void ReportExpectedNumberToken(const TextLocation& Location) noexcept;
	void ReportExpectedPrimaryExpression(const TextLocation& Location, TokenKind Kind) noexcept;
	void ReportExpectedCommaOrEndingToken(const TextLocation& Location, TokenKind Kind, TokenKind EndTokenKind) noexcept;
	void ReportExpectedComma(const TextLocation& Location, TokenKind Kind) noexcept;
	void ReportExpectedSemicolon(const TextLocation& Location, TokenKind Kind) noexcept;
	void ReportExpectedMatchingEndingToken(const TextLocation& Location, TokenKind Kind, TokenKind OpeningTokenKind, TokenKind EndingTokenKind) noexcept;
	void ReportExpectedVariableOrFunctionDeclaration(const TextLocation& Location, const struct Token& Current) noexcept;
	void ReportExpectedDeclaration(const TextLocation& Location) noexcept;
	// Binding
	void ReportBadUnaryOperator(const TextLocation& Location, const struct Token& Op, MyType* pRhsType) noexcept;
	void ReportBadBinaryOperator(const TextLocation& Location, const struct Token& Op, MyType* pLhsType, MyType* pRhsType) noexcept;
	void ReportTernaryTypeMismatch(const TextLocation& Location, MyType* pThenType, MyType* pElseType) noexcept;
	void ReportNotAVariable(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportNotAFunction(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportUninitializedZeroLengthArray(const TextLocation& Location) noexcept;
	void ReportUninitializedReadonlyVariable(const TextLocation& Location, const struct Token& Name) noexcept;
	void ReportUndefinedSymbol(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportSymbolRedeclaredDifferently(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportSymbolRedeclaration(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportParameterRedeclaration(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportInvalidTypeForDecomposable(const TextLocation& Location, MyType* pType) noexcept;
	void ReportInvalidCountForDecomposition(const TextLocation& Location, size_t kExpectedCount, size_t kCount) noexcept;
	void ReportForeachIterableNotArrayLike(const TextLocation& Location, MyType* pType) noexcept;
	void ReportIllegalTypeConversion(const TextLocation& Location, MyType* pType, MyType* pExpectedType) noexcept;
	void ReportIllegalImplicitTypeConversion(const TextLocation& Location, MyType* pType, MyType* pExpectedType) noexcept;
	void ReportIllegalAssignment(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportIllegalFieldAssignment(const TextLocation& Location, MyStruct* pKlass, char* const& lpField) noexcept;
	void ReportInvalidArgumentCount(const TextLocation& Location, char* const& lpName, size_t ExpectedCount, size_t Count) noexcept;
	void ReportInvalidArgumentType(
		const TextLocation& Location, char* const& lpName, MyType* pExpectedType, MyType* pType, size_t Index
	) noexcept;
	void ReportArgumentNotConstexpr(const TextLocation& Location, char* const& lpFunctionName, char* const& lpArgName, size_t k) noexcept;
	void ReportTypeCannotBeIndexed(const TextLocation& Location, MyType* pType) noexcept;
	void ReportMismatchedIndexCount(const TextLocation& Location, uint32_t kExpectedCount, uint32_t kCount) noexcept;
	void ReportTypeCannotBeUsedAsIndex(const TextLocation& Location, MyType* pObjectType, MyType* pIndexType, MyType* pType) noexcept;
	void ReportInvalidKeyOrAttribute(const TextLocation& Location, char* const& lpField, MyStruct* pKlass) noexcept;
	void ReportTooManyInitializers(const TextLocation& Location, MyStruct* pKlass, size_t kExpectedCount, size_t kCount);
	void ReportTypeDoesNotHaveAttributes(const TextLocation& Location, MyType* pType) noexcept;
	void ReportInvalidUseOfVoid(const TextLocation& Location) noexcept;
	void ReportUnknownType(const TextLocation& Location, char* const& lpTypename) noexcept;
	void ReportUndefinedType(const TextLocation& Location, char* const& lpTypename) noexcept;
	void ReportInvalidExpressionStatement(const TextLocation& Location) noexcept;
	void ReportInvalidBreakOrContinue(const TextLocation& Location, const char* const lpText) noexcept;
	void ReportInvalidReturn(const TextLocation& Location) noexcept;
	void ReportInvalidReturnExpression(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportMissingReturnExpression(const TextLocation& Location, char* const& lpName) noexcept;
	void ReportAllPathsMustReturn(const TextLocation& Location) noexcept;
	void ReportImportedFileDoesNotExist(const TextLocation& Location, MyString* pFilepath) noexcept;
	void ReportMainIsUndefined() noexcept;
	void ReportMainHasInvalidSignature() noexcept;
	// Emitting

	void Extend(const DiagnosticBag& Bag);
	bool Any() const noexcept;

private:
	void Report(const TextLocation& Location, const std::string& Message) noexcept;
};

void PrintDiagnostics(const class SyntaxTree* pTree, const DiagnosticBag& Diagnostics = DiagnosticBag{}) noexcept;


char* MyGetCachedString(const char* lpString, size_t kLength) noexcept;
char* MyGetCachedString(const std::string_view& Str) noexcept;
char* MyGetCachedStringV(const char* lpFormat, ...) noexcept;