#pragma once

#include "Base/Core.h"

#define MY_INVALID_ADDR uint32_t(-1)

enum tagMyTypeAttributes
{
	MY_TYPE_ATTR_NONE   = 0,
	MY_TYPE_ATTR_CONST  = 1 << 0,
};

enum tagMyTypeKind
{
	MY_TYPE_KIND_STRUCT   = 0,
	MY_TYPE_KIND_ARRAY    = 1,
	MY_TYPE_KIND_FUNCTION = 2,
};

enum tagMyFieldAttributes
{
	MY_FIELD_ATTR_NONE   = 0,
	MY_FIELD_ATTR_CONST  = 1 << 0,
	MY_FIELD_ATTR_STATIC = 1 << 1,
};

enum tagMyFunctionAttributes
{
	MY_FUNC_ATTR_NONE   = 0,
	MY_FUNC_ATTR_INLINE = 1 << 0,
	MY_FUNC_ATTR_STATIC = 1 << 1,
	MY_FUNC_ATTR_CTOR   = 1 << 2,
	MY_FUNC_ATTR_METHOD = 1 << 3,
	MY_FUNC_ATTR_NOGC   = 1 << 4,
};

enum tagMyStructAttributes
{
	MY_STRUCT_ATTR_NONE   = 0,
	MY_STRUCT_ATTR_POD    = 1 << 0,
	MY_STRUCT_ATTR_STATIC = 1 << 1,
};

enum tagMyReturnCode
{
	MY_RC_ERROR = -1,
	MY_RC_SUCCESS = 0,

	MY_RC_STACK_UNDERFLOW,
	MY_RC_STACK_OVERFLOW,
	MY_RC_CALLSTACK_OVERFLOW,
	MY_RC_INVALID_OPCODE,
	MY_RC_INVALID_ADDRESS,

	MY_RC_NULL_REFERENCE,
	MY_RC_DIVISION_BY_ZERO,
	MY_RC_INDEX_OUT_OF_BOUNDS,
	MY_RC_INVALID_CAST,
	MY_RC_INVALID_OPERATION,

	MY_RC_UNDEFINED_BUILTIN,
	MY_RC_UNDEFINED_INTERNAL,
	MY_RC_RECURSION_LIMIT_EXCEEDED,

	_MY_RC_COUNT,
};


struct MyGuid
{
	union
	{
		uint8_t  Data0[16];
		uint64_t Data1[ 2];
	};

	MyGuid();
	~MyGuid() noexcept;
	MyGuid(const MyGuid&) noexcept;
	MyGuid& operator=(const MyGuid&) noexcept;

	bool        IsValid() const noexcept;
	const char* AsString() const noexcept;

	bool operator==(MyGuid& ug) const noexcept;
	bool operator!=(MyGuid& ug) const noexcept;
};

static_assert(sizeof(MyGuid) == 16ull, "Error: Invalid MyGuid size");

struct MyFunctionSignature
{
	char*    Name   = nullptr;
	MyType*  Return = nullptr;
	MyType** Params = nullptr;
};

struct MyFunction
{
	MyFunctionSignature Signature  = {};
	uint32_t Flags   = 0u;
	uint32_t Address = 0u;
};

struct MyMethod
{
	char*     Fullname = nullptr; // Decorated (Fully Qualified) Name (ie. Person__GetName())
	MyStruct* Klass    = nullptr; // Parent struct
	MyType*   Type     = nullptr; // Has the signature
	uint32_t  Flags    = 0ul;
	uint32_t  Address  = 0ul;
	bool      IsCtor   = false;
};

struct MyField
{
	char*       Name       = nullptr;
	MyType*     Type       = nullptr; // Type->Klass to get field's struct
	MyStruct*   Klass      = nullptr; // Parent struct
	const void* Data       = nullptr;
	uint32_t    Offset     = 0ul;     // From the beginning of the object
	uint32_t    Attributes = 0ul;
};

struct MyStruct
{
	char*      Name       = nullptr;
	MyGuid     Guid       = {};
	MyField*   Fields     = nullptr;
	MyMethod** Methods    = nullptr;
	uint32_t   Attributes = 0ul;
	uint32_t   Size       = 0ul;     // Of its instance
};

struct MyArrayType
{
	MyStruct* Klass   = nullptr;
	uint32_t* Lengths = nullptr;
};

struct MyType
{
	union
	{
		MyStruct*            Klass;
		MyArrayType*         Array;
		MyFunctionSignature* Signature;
	};
	uint32_t Flags = 0ul;
	uint8_t  Kind  = 0u;  // MY_TYPE_KIND_X
};

#if 0
struct InternalCallbackContext
{
	MyContext* Context = nullptr;
	uint8_t*    Argv    = nullptr;
	int32_t     Argc    = 0;
	uint8_t*    Out     = nullptr;
	void*       User    = nullptr;
};
using pfnUniInternalFunction = bool(*)(InternalCallbackContext* pCallbackContext);
#endif // 0

using pfnMyInternalFunction = void(*)(MyContext* pContext, MyVM* pVM);

using AddressMap       = Pair<char*, uint32_t>;
using InternalFunction = Pair<char*, pfnMyInternalFunction>;

struct MyArrayShape;

struct MyAssembly
{
	MyInstruction*    Code      = nullptr;
	MyValue*          Constants = nullptr;
	AddressMap*       Globals   = nullptr;
	AddressMap*       Labels    = nullptr;
	AddressMap*       Functions = nullptr;
	AddressMap*       Methods   = nullptr;
	AddressMap*       Variables = nullptr;
	InternalFunction* Internals = nullptr;

	AddressMap*    Fields     = nullptr;
	MyStruct**     Klasses    = nullptr;
	MyArrayShape*  ShapeCache = nullptr;
};

struct MyContext
{
	using CtStringCache   = Pair<uint64_t, char*>;      // Compile time strings (keywords & identifiers)
	using RtStringCache   = Pair<uint64_t, MyString*>; // Runtime strings (literals & string objects)
	using GuidStringCache = Pair<MyGuid, char*>;       // Guid's string interpretation (to avoid regenerating them)
	using TypeIDCache     = Pair<char*, MyGuid>;       // ID for struct types, mapped by type name

	// Runtime
	MyAssembly*     Assembly = nullptr;
	MyVM*           VM       = nullptr;
	// Storage
	GuidStringCache* Guids    = nullptr;
	RtStringCache*   RtCache  = nullptr;
	CtStringCache*   CtCache  = nullptr;
	TypeIDCache*     TidCache = nullptr;
};


struct MyDefaults
{
	MyType* ErrorType   = nullptr;
	MyType* VoidType    = nullptr;
	MyType* ObjectType  = nullptr;
	MyType* BooleanType = nullptr;
	MyType* IntType     = nullptr;
	MyType* UintType    = nullptr;
	MyType* IntPtrType  = nullptr;
	MyType* FloatType   = nullptr;
	MyType* ComplexType = nullptr;
	MyType* StringType  = nullptr;
	MyType* StringBuilderType = nullptr;
	MyType* BytesType   = nullptr;
	MyType* FileType    = nullptr;
	MyType* ConsoleType = nullptr;
	MyType* MathType    = nullptr;

	MyStruct* ErrorStruct   = nullptr;
	MyStruct* VoidStruct    = nullptr;
	MyStruct* ObjectStruct  = nullptr;
	MyStruct* BooleanStruct = nullptr;
	MyStruct* IntStruct     = nullptr;
	MyStruct* UintStruct    = nullptr;
	MyStruct* IntPtrStruct  = nullptr;
	MyStruct* FloatStruct   = nullptr;
	MyStruct* ComplexStruct = nullptr;
	MyStruct* StringStruct  = nullptr;
	MyStruct* StringBuilderStruct = nullptr;
	MyStruct* BytesStruct   = nullptr;
	MyStruct* FileStruct    = nullptr;
	MyStruct* ConsoleStruct = nullptr;
	MyStruct* MathStruct    = nullptr;
};

extern MyDefaults My_Defaults;

MyContext*  MyInitialize();
void        MyInitializeStructs(MyContext* pContext);
void        MyUninitializeStructs();
void        MyUninitialize(MyContext* pContext);

MyFunction* MyFunctionCreate(const MyFunctionSignature& Signature, uint32_t kFlags, uint32_t kAddress = MY_INVALID_ADDR) noexcept;
MyMethod*   MyMethodCreate(
	MyStruct*                  pKlass,
	char*                      lpName,
	MyType*                    pType,
	uint32_t                   kFlags,
	uint32_t                   kAddress = MY_INVALID_ADDR,
	bool                       bIsCtor = false
) noexcept;
MyStruct*   MyStructCreate(MyContext* pContext, const char* lpName, uint32_t kAttribs) noexcept;
bool        MyStructIsReference(MyStruct* pKlass) noexcept;
void        MyStructAddField(MyStruct* pKlass, const char* lpName, MyType* pType, uint32_t kAttribs = MY_FIELD_ATTR_NONE) noexcept;
MyField*    MyStructGetField(MyStruct* pKlass, const char* lpField);
MyMethod*   MyStructGetMethod(MyStruct* pKlass, const char* lpMethod);
size_t      MyStructFieldCount(const MyStruct* pKlass);
MyType*     MyTypeCreate(uint8_t kKind, void* pData, uint32_t kFlags = 0ul) noexcept;
bool        MyTypeIsReference(MyType* pType) noexcept;
const char* MyTypeGetName(const MyType* pType) noexcept;

MyContext*  MyContextGet() noexcept;
MyGuid      MyGuidCreate(MyContext* pContext) noexcept;
MyGuid      MyGuidCreate(MyContext* pContext, uint64_t kValue0, uint64_t kValue1) noexcept;
char*       MyGuidCreateStringRepr(MyContext* pContext, const MyGuid& Guid) noexcept;

void        MyDecompile(const MyAssembly* pAssembly) noexcept; // Defined in VM.cpp
const char* MyReturnCodeString(int64_t iReturnCode) noexcept;

template<typename Item>
uint32_t    MyGetElementIndex(Item* const& pItems, size_t kCount, const Item& it) noexcept
{
	for (size_t k = 0; k < kCount; k++)
	{
		if (pItems[k] == it)
		{
			return (uint32_t)k;
		}
	}
	return uint32_t(-1);
}

