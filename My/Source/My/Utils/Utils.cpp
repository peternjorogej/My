#include "Utils.h"
#include "My/Base/IO.h"
#include "My/My.h"
#include "My/Object.h"
#include "My/Syntax/Tree.h"
#include "My/Binding/BoundTree.h"
#include "stb/stb_ds.h"

#include <random>

#pragma region Memory Allocation Cleanup
using AllocationMap = Pair<void*, void*>;

struct Allocation
{
	uint32_t Size   = 0u;
	uint32_t Stage  = 0u;
};

struct AllocationStatistics
{
	uint64_t MaxAllocation    = uint64_t(0u);
	uint64_t MinAllocation    = uint64_t(-1);
	uint64_t AllocatedBytes   = uint64_t(0u);
	uint64_t DeallocatedBytes = uint64_t(0u);
	uint64_t AllocationCount  = uint64_t(0u);
	uint64_t DellocationCount = uint64_t(0u);
};

static AllocationStatistics s_AllocStats = {};
static AllocationMap*       s_Allocs     = nullptr;

static void* _My_Malloc(size_t kSize, uint32_t Stage) noexcept
{
	kSize += sizeof(Allocation);
	// s_AllocatedBytes += kSize;

	if (void* pMemory = ::operator new(kSize); pMemory)
	{
		Allocation* pAlloc = (Allocation*)pMemory;
		pAlloc->Size  = kSize;
		pAlloc->Stage = Stage;

		s_AllocStats.MaxAllocation = std::max(s_AllocStats.MaxAllocation, kSize);
		s_AllocStats.MinAllocation = std::min(s_AllocStats.MinAllocation, kSize);
		s_AllocStats.AllocatedBytes += kSize;
		s_AllocStats.AllocationCount++;

		return (char*)pMemory + sizeof(Allocation);
	}

	return nullptr;
}

static void _My_Free(void* pMemory) noexcept
{
	if (pMemory)
	{
		void* pBlockStart = (char*)pMemory - sizeof(Allocation);
		// s_AllocatedBytes -= ((Allocation*)pBlockStart)->kSize;

		s_AllocStats.DeallocatedBytes += ((Allocation*)pBlockStart)->Size;
		s_AllocStats.DellocationCount++;

		::operator delete(pBlockStart);
		pBlockStart = nullptr;
	}
}


void Allocator::FullCleanup() noexcept
{
	const size_t kLength = stbds_hmlenu(s_Allocs);
	for (size_t k = 0; k < kLength; k++)
	{
		void* pMemory = s_Allocs[k].value;
		_My_Free(pMemory);
	}

	stbds_hmfree(s_Allocs);
	s_Allocs = nullptr;
}

void Allocator::Cleanup(Stage stage) noexcept
{
	// To avoid invalidating iterators, defer the erasure of allocations
	AllocationMap* pTmpAllocs = nullptr;
	for (size_t k = 0; k < stbds_hmlenu(s_Allocs); k++)
	{
		void* pMemory = s_Allocs[k].value;

		Allocation* pAlloc = (Allocation*)((char*)pMemory - sizeof(Allocation));
		if (pAlloc->Stage == uint32_t(stage))
		{
			// Free the memory, but don't erase from s_Allocations
			_My_Free(pMemory);
			// Defer the erasure
			stbds_hmput(pTmpAllocs, pMemory, pMemory);
		}
	}

	for (size_t k = 0; k < stbds_hmlenu(pTmpAllocs); k++)
	{
		void* pMemory = pTmpAllocs[k].value;
		stbds_hmdel(s_Allocs, pMemory);
	}

	stbds_hmfree(pTmpAllocs);
	pTmpAllocs = nullptr;
}

void* Allocator::AllocateInternal(size_t kSize, Stage stage) noexcept
{
	void* pMemory = _My_Malloc(kSize, uint32_t(stage));
	stbds_hmput(s_Allocs, pMemory, pMemory);
	return pMemory;
}
#pragma endregion

#pragma region Heap Memory Management
constexpr Buffer::Buffer(uint8_t* pBuffer)
	: m_Pointer(pBuffer)
{ }

Buffer Buffer::Create(size_t kSize) noexcept
{
	kSize = std::clamp(kSize, MinimumAllocSize, MaximumAllocSize);

	const uint64_t kAllocSize = kSize + sizeof(SizeInfo);
	if (uint8_t* pMemory = (uint8_t*)malloc(kAllocSize); pMemory)
	{
		memset(pMemory, 0, kAllocSize);

		SizeInfo* pSizeInfo = (SizeInfo*)pMemory;
		pSizeInfo->Length = 0ul;
		pSizeInfo->Capacity = kSize;
		pSizeInfo->ROffset = 0ul;

		uint8_t* const pPointer = pMemory + sizeof(SizeInfo);
		return Buffer{ pPointer };
	}

	return Buffer{};
}

void Buffer::Delete(Buffer buffer) noexcept
{
	if (!buffer)
	{
		return;
	}

	uint8_t* pMemory = buffer.m_Pointer - sizeof(SizeInfo);

	SizeInfo* pSizeInfo = (SizeInfo*)pMemory;
	pSizeInfo->Length = 0ul;
	pSizeInfo->Capacity = 0ul;
	pSizeInfo->ROffset = 0ul;

	free(pMemory);
	pMemory = nullptr;
	buffer.m_Pointer = nullptr;
}

void Buffer::Write(const void* pBlock, size_t kBlockSize) noexcept
{
	if (!m_Pointer)
	{
		return;
	}

	SizeInfo* pSizeInfo = (SizeInfo*)(m_Pointer - sizeof(SizeInfo));

	CheckAndResize(kBlockSize);
	memcpy(m_Pointer + pSizeInfo->Length, pBlock, kBlockSize);
	pSizeInfo->Length += kBlockSize;
}

void Buffer::Read(void* pBlock, size_t kBlockSize) noexcept
{
	if (!m_Pointer)
	{
		return;
	}

	SizeInfo* pSizeInfo = (SizeInfo*)(m_Pointer - sizeof(SizeInfo));
	memcpy(pBlock, m_Pointer + pSizeInfo->ROffset, kBlockSize);
	pSizeInfo->ROffset += kBlockSize;
}

void Buffer::Resize(size_t kNewCapacity) noexcept
{
	if (!m_Pointer)
	{
		return;
	}

	uint8_t* pMemory = m_Pointer - sizeof(SizeInfo);

	uint32_t kOldLength = ((SizeInfo*)pMemory)->Length;
	uint32_t kOldCapacity = ((SizeInfo*)pMemory)->Capacity;

	if (kNewCapacity <= kOldCapacity)
	{
		return;
	}

	if (uint8_t* pNewMemory = (uint8_t*)realloc(pMemory, kNewCapacity); pNewMemory)
	{
		SizeInfo* pSizeInfo = (SizeInfo*)pNewMemory;
		pSizeInfo->Length = kOldLength;
		pSizeInfo->Capacity = kNewCapacity;
		m_Pointer = pNewMemory + sizeof(SizeInfo);
		{
			const size_t kAllocatedCapacity = pSizeInfo->Capacity - kOldCapacity;
			memset(m_Pointer + pSizeInfo->Length, 0, kAllocatedCapacity);
		}
	}
	else
	{
		// Reallocation failed. Clean up and invalidate buffer
		Delete(*this);
		m_Pointer = nullptr;
	}
}

void Buffer::CheckAndResize(size_t kRequiredSize) noexcept
{
	if (!m_Pointer)
	{
		return;
	}

	uint8_t* pMemory = m_Pointer - sizeof(SizeInfo);

	uint64_t kCurrentLength = ((SizeInfo*)pMemory)->Length;
	uint64_t kCurrentCapacity = ((SizeInfo*)pMemory)->Capacity;

	uint64_t kAvailCapacity = kCurrentCapacity - kCurrentLength;

	if (kRequiredSize > kAvailCapacity)
	{
		const size_t kNewCapacity = kCurrentCapacity + (kCurrentCapacity / 2ul) + kRequiredSize;
		Resize(kNewCapacity);
	}
}

uint32_t Buffer::Length() const noexcept
{
	return m_Pointer ? ((SizeInfo*)(m_Pointer - sizeof(SizeInfo)))->Length : 0ul;
}

uint32_t Buffer::Capacity() const noexcept
{
	return m_Pointer ? ((SizeInfo*)(m_Pointer - sizeof(SizeInfo)))->Capacity : 0ul;
}

inline Buffer::operator bool() const noexcept
{
	return (bool)m_Pointer;
}

uint8_t* const* Buffer::operator &() const noexcept
{
	return &m_Pointer;
}
#pragma endregion

#pragma region Random Number Generation
static std::mt19937_64 s_RandomEngine = std::mt19937_64(std::random_device{}());
static std::uniform_int_distribution<int64_t>  s_IntDistribution  = {};
static std::uniform_int_distribution<uint64_t> s_UintDistribution = {};
static std::uniform_real_distribution<double>  s_RealDistribution = {};

int64_t Random::Int(int64_t iMax) noexcept
{
	const int64_t p = s_IntDistribution(s_RandomEngine);
	return iMax == 0ll ? p : p % iMax;
}

uint64_t Random::Uint(uint64_t kMax) noexcept
{
	const uint64_t p = s_UintDistribution(s_RandomEngine);
	return kMax == 0ull ? p : p % kMax;
}

double Random::Float() noexcept
{
	return s_RealDistribution(s_RandomEngine) / s_RealDistribution.max();
}

double Random::Float(double First, double Last) noexcept
{
	const double t = Float();
	return First + (Last - First)*t;
}
#pragma endregion

#pragma region Diagnostic Reporting
DiagnosticBag::DiagnosticBag()
	: Super()
{ }

void DiagnosticBag::Extend(const DiagnosticBag& Bag)
{
	insert(end(), Bag.begin(), Bag.end());
}

bool DiagnosticBag::Any() const noexcept
{
	return !empty();
}

void DiagnosticBag::Report(const TextLocation& Location, const std::string& Message) noexcept
{
	emplace_back(Location, Message);
}

void DiagnosticBag::ReportUnknownError(const TextLocation& Location, const std::string& Message) noexcept
{
	Report(Location, Message);
}

void DiagnosticBag::ReportFeatureNotImplemented(const TextLocation& Location, const char* lpFeature) noexcept
{
	const std::string& Message = Console::Format("'%s' is not yet implemented (coming soon, maybe?)", lpFeature);
	Report(Location, Message);
}

void DiagnosticBag::ReportCompilerError(const TextLocation& Location, const std::string& Message) noexcept
{
	Report(Location, Message);
}

// Lexing
void DiagnosticBag::ReportBadCharacter(const TextLocation& Location, char Character) noexcept
{
	const std::string Message = Console::Format("Bad character '%c' in input", Character);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidNumber(const TextLocation& Location, const std::string& Text, MyType* pType) noexcept
{
	MY_NOT_IMPLEMENTED();
}

void DiagnosticBag::ReportUnterminatedString(const TextLocation& Location) noexcept
{
	const std::string Message = "Unterminated std::string literal: ending '\"' not found";
	Report(Location, Message);
}

void DiagnosticBag::ReportUnterminatedMultilineComment(const TextLocation& Location) noexcept
{
	const std::string Message = "Unterminated multiline comment: ending '*/' not found";
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidEscapeCharacterInStringLiteral(const TextLocation& Location, char Character) noexcept
{
	const std::string Message = Console::Format("Invalid escape character in std::string literal: '%c'", Character);
	Report(Location, Message);
}

// Parsing
void DiagnosticBag::ReportUnexpectedToken(const TextLocation& Location, TokenKind Kind, TokenKind ExpectedKind) noexcept
{
	const std::string Message = Console::Format(
		"Unexpected token '%s', expected '%s'", TokenKindString(Kind), TokenKindString(ExpectedKind)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportMissingTypename(const TextLocation& Location) noexcept
{
	const std::string Message = "Explicit type or type specifier is missing";
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedIdentifierToken(const TextLocation& Location) noexcept
{
	const std::string Message = "Expected an identifier";
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedNumberToken(const TextLocation& Location) noexcept
{
	const std::string Message = "Expected Int32, Int64, Float32 or Float64";
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedPrimaryExpression(const TextLocation& Location, TokenKind Kind) noexcept
{
	const std::string Message = Console::Format("Expected a primary expression before '%s'", TokenKindString(Kind));
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedCommaOrEndingToken(const TextLocation& Location, TokenKind Kind, TokenKind EndTokenKind) noexcept
{
	const std::string Message = Console::Format("Expected a ',' or '%s' (not '%s')", TokenKindString(EndTokenKind), TokenKindString(Kind));
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedComma(const TextLocation& Location, TokenKind Kind) noexcept
{
	const std::string Message = Console::Format("Expected a ',' (not '%s')", TokenKindString(Kind));
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedSemicolon(const TextLocation& Location, TokenKind Kind) noexcept
{
	const std::string Message = Console::Format("Expected semicolon at end of statement (not '%s')", TokenKindString(Kind));
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedMatchingEndingToken(const TextLocation& Location, TokenKind Kind, TokenKind OpeningTokenKind, TokenKind EndingTokenKind) noexcept
{
	const std::string Message = Console::Format(
		"Expected matching closing token '%s' (not '%s')", TokenKindString(EndingTokenKind), TokenKindString(Kind)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedVariableOrFunctionDeclaration(const TextLocation& Location, const Token& Current) noexcept
{
	const std::string Message = Console::Format(
		"Invalid token '%s'; expected var/const or function declaration in struct definition", TokenKindString(Current.Kind)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportExpectedDeclaration(const TextLocation& Location) noexcept
{
	const std::string Message = "Only declarations can occur at the top level (imports, typedefs, externs and symbols)";
	Report(Location, Message);
}

// Binding
void DiagnosticBag::ReportBadUnaryOperator(const TextLocation& Location, const Token& Op, MyType* pRhsType) noexcept
{
	const std::string Message = Console::Format("Unary operator '%s' not defined for '%s'", TokenKindString(Op.Kind), MyTypeGetName(pRhsType));
	Report(Location, Message);
}

void DiagnosticBag::ReportBadBinaryOperator(const TextLocation& Location, const Token& Op, MyType* pLhsType, MyType* pRhsType) noexcept
{
	const std::string Message = Console::Format(
		"Binary operator '%s' not defined for '%s' and '%s'", TokenKindString(Op.Kind), MyTypeGetName(pLhsType), MyTypeGetName(pRhsType)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportTernaryTypeMismatch(const TextLocation& Location, MyType* pThenType, MyType* pElseType) noexcept
{
	const std::string Message = Console::Format(
		"Ternary operator has mismatched types for '%s' and '%s'", MyTypeGetName(pThenType), MyTypeGetName(pElseType)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportNotAVariable(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Symbol '%s' is not a variable", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportNotAFunction(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Symbol '%s' is not a function", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportUninitializedZeroLengthArray(const TextLocation& Location) noexcept
{
	const std::string& Message = "Zero length arrays must be initialized";
	Report(Location, Message);
}

void DiagnosticBag::ReportUninitializedReadonlyVariable(const TextLocation& Location, const Token& Name) noexcept
{
	const std::string& Message = Console::Format("Readonly variable '%s' is uninitialized", Name.Id);
	Report(Location, Message);
}

void DiagnosticBag::ReportUndefinedSymbol(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Undefined symbol '%s'", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportSymbolRedeclaredDifferently(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("'%s' redeclared as different symbol", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportSymbolRedeclaration(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Redeclaration of symbol '%s'", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportParameterRedeclaration(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Redeclaration of parameter '%s'", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidTypeForDecomposable(const TextLocation& Location, MyType* pType) noexcept
{
	const std::string Message = Console::Format("Decomposable must be struct type or array-like, not %s", MyTypeGetName(pType));
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidCountForDecomposition(const TextLocation& Location, size_t kExpectedCount, size_t kCount) noexcept
{
	const std::string Message = Console::Format("Expected %I64u elements for decomposition but got %I64u", kExpectedCount, kCount);
	Report(Location, Message);
}

void DiagnosticBag::ReportForeachIterableNotArrayLike(const TextLocation& Location, MyType* pType) noexcept
{
	const std::string Message = Console::Format("Foreach loop expects iterable to be array-like (got type '%s')", MyTypeGetName(pType));
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidCast(const TextLocation& Location, MyType* pTypeFrom, MyType* pTypeTo) noexcept
{
	const std::string Message = Console::Format(
		"Invalid cast from type '%s' to '%s'", MyTypeGetName(pTypeFrom), MyTypeGetName(pTypeTo)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidTypeConversion(const TextLocation& Location, MyType* pTypeFrom, MyType* pTypeTo) noexcept
{
	const std::string Message = Console::Format(
		"Invalid type conversion from type '%s' to '%s'", MyTypeGetName(pTypeFrom), MyTypeGetName(pTypeTo)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportIllegalTypeConversion(const TextLocation& Location, MyType* pType, MyType* pExpectedType) noexcept
{
	const std::string Message = Console::Format(
		"Cannot convert from type '%s' to '%s'", MyTypeGetName(pType), MyTypeGetName(pExpectedType)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportIllegalImplicitTypeConversion(const TextLocation& Location, MyType* pType, MyType* pExpectedType) noexcept
{
	const std::string Message = Console::Format(
		"Cannot implicitly convert from type '%s' to '%s' (are you missing a cast?)", MyTypeGetName(pType), MyTypeGetName(pExpectedType)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportIllegalAssignment(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Illegal assignment to readonly variable '%s'", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportIllegalFieldAssignment(const TextLocation& Location, MyStruct* pKlass, char* const& lpField) noexcept
{
	const std::string Message = Console::Format("Illegal assignment to readonly field '%s.%s'", pKlass->Name, lpField);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidArgumentCount(const TextLocation& Location, char* const& lpName, size_t ExpectedCount, size_t Count) noexcept
{
	const std::string Message = Console::Format(
		"Invalid argument count for function '%s' (expected %I64u, got %I64u)", lpName, ExpectedCount, Count
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidArgumentType(
	const TextLocation& Location, char* const& lpName, MyType* pExpectedType, MyType* pType, size_t Index
) noexcept
{
	const std::string Message = Console::Format(
		"Argument %I64u of '%s' has invalid type '%s'; expected '%s'", Index, lpName, MyTypeGetName(pType), MyTypeGetName(pExpectedType)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportArgumentNotConstexpr(const TextLocation& Location, char* const& lpFunctionName, char* const& lpArgName, size_t k) noexcept
{
	const std::string Format = "Argument '%s' (arg %I64u of function '%s') is not a constant expression, but has been defined as 'constexpr'";
	const std::string Message = Console::Format(Format, lpArgName, k, lpFunctionName);
	Report(Location, Message);
}

void DiagnosticBag::ReportTypeCannotBeIndexed(const TextLocation& Location, MyType* pType) noexcept
{
	const std::string Message = Console::Format("Type '%s' cannot be indexed (expected array)", MyTypeGetName(pType));
	Report(Location, Message);
}

void DiagnosticBag::ReportMismatchedIndexCount(const TextLocation& Location, uint32_t kExpectedCount, uint32_t kCount) noexcept
{
	const std::string Message = Console::Format("Mismatched index count expected %u, but got %u", kExpectedCount, kCount);
	Report(Location, Message);
}

void DiagnosticBag::ReportTypeCannotBeUsedAsIndex(const TextLocation& Location, MyType* pObjectType, MyType* pIndexType, MyType* pType) noexcept
{
	const std::string Message = Console::Format(
		"Type '%s' cannot be used as index to object of type '%s' (expected type '%s')",
		MyTypeGetName(pIndexType), MyTypeGetName(pObjectType), MyTypeGetName(pType)
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportNotAStaticMember(const TextLocation& Location, char* const& lpField, MyStruct* pKlass) noexcept
{
	const std::string Message = Console::Format("Field '%s' is not a static member of '%s'", lpField, pKlass->Name);
	Report(Location, Message);
}

void DiagnosticBag::ReportNotAStaticMethod(const TextLocation& Location, char* const& lpMethod, MyStruct* pKlass) noexcept
{
	const std::string Message = Console::Format("Method '%s' is not a static function of '%s'", lpMethod, pKlass->Name);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidKeyOrAttribute(const TextLocation& Location, char* const& lpField, MyStruct* pKlass) noexcept
{
	const std::string Message = Console::Format("Object of type '%s' has no key/attribute '%s'", pKlass->Name, lpField);
	Report(Location, Message);
}

void DiagnosticBag::ReportTooManyInitializers(const TextLocation& Location, MyStruct* pKlass, size_t kExpectedCount, size_t kCount)
{
	const std::string Message = Console::Format(
		"Too many initializers for '%s' in operator new (expected %I64u, got %I64u)", pKlass->Name, kExpectedCount, kCount
	);
	Report(Location, Message);
}

void DiagnosticBag::ReportTypeDoesNotHaveAttributes(const TextLocation& Location, MyType* pType) noexcept
{
	const std::string Message = Console::Format("Type '%s' does not have attributes", MyTypeGetName(pType));
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidUseOfVoid(const TextLocation& Location) noexcept
{
	const std::string Message = "'void' value cannot be used as an expression";
	Report(Location, Message);
}

void DiagnosticBag::ReportUnknownType(const TextLocation& Location, char* const& lpTypename) noexcept
{
	const std::string Message = Console::Format("Unknown type '%s'", lpTypename);
	Report(Location, Message);
}

void DiagnosticBag::ReportUndefinedType(const TextLocation& Location, char* const& lpTypename) noexcept
{
	const std::string Message = Console::Format("Type '%s' is undefined", lpTypename);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidExpressionStatement(const TextLocation& Location) noexcept
{
	const std::string Message = "Only assignment and call expressions can be used as statements";
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidBreakOrContinue(const TextLocation& Location, const char* const lpText) noexcept
{
	const std::string Message = Console::Format("'%s' statement cannot be used outside a loop", lpText);
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidReturn(const TextLocation& Location) noexcept
{
	const std::string Message = "'return' cannot be used outside a function";
	Report(Location, Message);
}

void DiagnosticBag::ReportInvalidReturnExpression(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Function '%s' returns void, it cannot return a value", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportMissingReturnExpression(const TextLocation& Location, char* const& lpName) noexcept
{
	const std::string Message = Console::Format("Function '%s' does not return void (missing expression after 'return')", lpName);
	Report(Location, Message);
}

void DiagnosticBag::ReportAllPathsMustReturn(const TextLocation& Location) noexcept
{
	const std::string Message = "Not all control paths return a value";
	Report(Location, Message);
}

void DiagnosticBag::ReportImportedFileDoesNotExist(const TextLocation& Location, MyString* pFilepath) noexcept
{
	const std::string Message = Console::Format("File '%s' does not exist (cannot be imported)", pFilepath->Chars);
	Report(Location, Message);
}

void DiagnosticBag::ReportMainIsUndefined() noexcept
{
	const std::string Message = "function 'Main' has not been defined";
	Report(TextLocation(0u, 0u, 0u, {}), Message);
}

void DiagnosticBag::ReportMainHasInvalidSignature() noexcept
{
	const std::string Message = "function 'Main' has invalid signature, expected: 'function int Main(string[])";
	Report(TextLocation(0u, 0u, 0u, {}), Message);
}


void PrintDiagnostics(const class SyntaxTree* pTree, const DiagnosticBag& Diagnostics) noexcept
{
	const SourceText& Text = pTree->GetText();

	const DiagnosticBag& Bag = Diagnostics.empty() ? pTree->GetDiagnostics() : Diagnostics;
	for (const Diagnostic& diagnostic : Bag)
	{
		const uint32_t kErrorLineIndex = Text.GetLineIndex(diagnostic.Location.Start);
		const TextLine& Line = Text.Lines[kErrorLineIndex];

		const std::string_view& ErrorFile = Text.Filename;
		uint32_t kErrorLine = kErrorLineIndex + 1;
		uint32_t kErrorStart = diagnostic.Location.Start - Line.Start;
		uint32_t kErrorEnd = kErrorStart + diagnostic.Location.Length;

		for (uint32_t k = Line.Start; k < Line.End(); k++)
		{
			if (Text[k] == '\t')
			{
				kErrorStart += 7;
				kErrorEnd += 7;
			}
		}

		Console::WriteLine();

		Console::SetColor(Console::Color::Red);
		Console::Write("[Error in '%s' on line %u (%u, %u)]: ", ErrorFile.data(), kErrorLine, kErrorStart+1, kErrorEnd);
		Console::WriteLine(diagnostic.Message);
		Console::ResetColor();

		Console::SetColor(Console::Color::LightGray);
		{
			const std::string_view sv = Text.ToString(Line.Start, Line.Length);
			Console::WriteLine(sv);
		}
		Console::ResetColor();

		for (uint32_t k = 0; k < kErrorStart; k++)
		{
			Console::Write(" ");
		}

		Console::SetColor(Console::Color::Red);
		for (uint32_t k = kErrorStart; k < kErrorEnd; k++)
		{
			Console::Write("^");
		}
		Console::ResetColor();

		Console::WriteLine();
	}
}
#pragma endregion

char* MyGetCachedString(const char* lpString, size_t kLength) noexcept
{
	MyContext* pContext = MyContextGet();
	MY_ASSERT(pContext != nullptr, "Cannot intern strings without an active context");
	MY_ASSERT(lpString != nullptr, "Copying a nullptr");

	if (kLength == size_t(-1))
	{
		kLength = strlen(lpString);
	}

	const uint64_t kHash = MyHashBytes(lpString, kLength, 0ull);
	if (char* lpCachedString = stbds_hmget(pContext->CtCache, kHash); lpCachedString)
	{
		return lpCachedString;
	}
	else
	{
		char* lpStringCopy = new char[kLength + 1];
		strncpy(lpStringCopy, lpString, kLength);
		lpStringCopy[kLength] = 0;
		
		stbds_hmput(pContext->CtCache, kHash, lpStringCopy);
		return lpStringCopy;
	}
}

char* MyGetCachedString(const std::string_view& Str) noexcept
{
	return MyGetCachedString(Str.data(), Str.length());
}

char* MyGetCachedStringV(const char* lpFormat, ...) noexcept
{
	static constexpr uint64_t kSize = 2048ull;
	static char lpBuffer[kSize] = { 0 };

	va_list vArgs;
	va_start(vArgs, lpFormat);
	vsnprintf(lpBuffer, kSize, lpFormat, vArgs);
	va_end(vArgs);

	return MyGetCachedString(lpBuffer, strlen(lpBuffer));
}


