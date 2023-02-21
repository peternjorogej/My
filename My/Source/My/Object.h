#pragma once

#include "My/Base/Core.h"
#include "My/My.h"

enum class MyValueKind : uint8_t
{
	Invalid = 0,
	Null,
	Bool,
	Int64,
	Uint64,
	Float64,
	String,
	Array,
	Object,
};

struct MyValue
{
	MyValueKind Kind = MyValueKind::Invalid;

	union
	{
		bool      B08;
		int64_t   I64;
		uint64_t  U64;
		double    F64;
		MyString* Str;
		MyArray*  Arr;
		MyObject* Obj;
	};

	MyValue();
	MyValue(MyValueKind Kind);
	~MyValue() noexcept;

	operator bool() const noexcept;

	void Print(bool bQuoteString = false) const noexcept;

	template<typename Number>
	Number As() const noexcept;
};


struct MyObject
{
	MyStruct* Klass = nullptr;
	Byte*     Data  = nullptr;
};

struct MyString
{
	MyObject Object = {};
	uint64_t Hash   = 0ull;
	uint64_t Length = 0ull;
	char*    Chars  = nullptr;
};

// This struct can also be represented as uint32_t[8]
struct MyArrayShape
{
	uint32_t Lengths[8] = { 0 };

	constexpr MyArrayShape() = default;
	MyArrayShape(uint32_t kLength0);
	MyArrayShape(uint32_t kLength0, uint32_t kLength1);
	MyArrayShape(uint32_t kLength0, uint32_t kLength1, uint32_t kLength2);
	MyArrayShape(uint32_t kLength0, uint32_t kLength1, uint32_t kLength2, uint32_t kLength3);

	uint32_t CalculateCount() const noexcept;
	uint32_t CalculateRank() const noexcept;
};
static_assert(sizeof(MyArrayShape) == sizeof(uint32_t[8]), "Error: Invalid MyArrayShape size");

struct MyArray
{
	MyObject     Object   = {};
	MyArrayShape Shape    = {};
	uint32_t     Count    = 0ul;
	uint32_t     Rank     = 0ul;
	uint32_t     Capacity = 0ul;
	Byte*        Data     = nullptr;
};



#define MyArrayCount(arr)                     ((arr)->Count)
#define MyArrayPitch(arr)                     ((arr)->Shape.Lengths[0])
#define MyArraySlice(arr)                     ((arr)->Shape.Lengths[0] * (arr)->Shape.Lengths[1])
#define MyArrayAddr(arr,T,index)              ((T*)_MyArrayAddrWithSize(arr, sizeof(T), index))
#define MyArrayAddrByStride(arr,stride,index) ((Byte*)_MyArrayAddrWithSize(arr, stride, index))
#define MyArrayGet(arr,T,index)               (*(T*)MyArrayAddr(arr, T, index)) 
#define MyArraySet(arr,T,index,value)	\
	do {	                                            \
		T*__p = (T*)MyArrayAddr((arr), T, (index));	\
		*__p = (value);	                                \
	} while (0)
#define MyArraySet2(arr,T,index,value)	        (MyArrayGet(arr, T, index) = (value))
#define _MyArrayAddrWithSize(arr,size,index)   ((arr)->Data + (size)*(index))

MyValue     MakeValue_Copy(const MyValue& Value, MyContext* pContext = nullptr);
MyValue     MakeValue_Null();
MyValue     MakeValue_Bool(bool bValue);
MyValue     MakeValue_Int64(int64_t Value);
MyValue     MakeValue_Uint64(uint64_t Value);
MyValue     MakeValue_Float64(double Value);
MyValue     MakeValue_String(MyString* pValue);
MyValue     MakeValue_Array(MyArray* pValue);
MyValue     MakeValue_Object(MyObject* Value);

MyString*   MyStringNew(MyContext* pContext, const char* lpStr);
MyString*   MyStringNew(MyContext* pContext, const char* lpStr, size_t kLength);
MyString*   MyStringNew(MyContext* pContext, const std::string_view& Str);
MyString*   MyStringFormat(MyContext* pContext, const char* lpFmt, ...);
MyString*   MyStringCopy(MyContext* pContext, const MyString* pStr);
MyString*   MyStringIntern(MyString* pStr);
bool        MyStringIsInterned(const MyString* pStr);
bool        MyStringIsEqual(const MyString* pLhsStr, const MyString* pRhsStr);
uint64_t    MyStringGetHash(const MyString* pStr);
uint64_t    MyStringGetLength(const MyString* pStr);
char*       MyStringToUtf8(const MyString* pStr, bool bCopy = false);
wchar_t*    MyStringToUtf16(const MyString* pStr);

MyArray*    MyArrayNew(MyContext* pContext, MyStruct* pKlass, size_t kCount, size_t kCapacity = 16ull);
MyArray*    MyArrayNew(MyContext* pContext, MyStruct* pKlass, const MyArrayShape& Shape, size_t kCapacity = 16ull);
MyArray*    MyArrayCopy(MyContext* pContext, const MyArray* pArray);
bool        MyArrayIsEqual(const MyArray* pLhsStr, const MyArray* pRhsStr);

MyObject*   MyObjectNew(MyContext* pContext, MyStruct* pKlass);
MyObject*   MyObjectCopy(MyContext* pContext, const MyObject* pObject);
MyField*    MyObjectGetField(MyObject* pObject, char* const& lpField);
Byte*       MyObjectFieldGetValue(MyObject* pObject, MyField* pField);
void        MyObjectFieldSetValue(MyObject* pObject, MyField* pField, const void* pData, size_t kSize = 0ull);
template<typename Tp>
Tp&         MyObjectFieldGetValueAs(MyObject* pObject, MyField* pField);
template<typename Tp>
void        MyObjectFieldSetValueAs(MyObject* pObject, MyField* pField, const Tp& Data);

const char* ValueKindString(MyValueKind Kind) noexcept;


template<typename Number>
inline Number MyValue::As() const noexcept
{
	static_assert(std::is_arithmetic<Number>::value, "MyValue::As<T>() expects that T is a numeric type");
	switch (Kind)
	{
		case MyValueKind::Null:    return static_cast<Number>(0);
		case MyValueKind::Bool:    return static_cast<Number>(B08);
		case MyValueKind::Int64:   return static_cast<Number>(I64);
		case MyValueKind::Uint64:  return static_cast<Number>(U64);
		case MyValueKind::Float64: return static_cast<Number>(F64);
		default:                   return static_cast<Number>(0);
	}
}

template<typename Tp>
inline Tp& MyObjectFieldGetValueAs(MyObject* pObject, MyField* pField)
{
	static_assert(std::is_standard_layout<Tp>::value);
	return *reinterpret_cast<Tp*>(MyObjectFieldGetValue(pObject, pField));
}

template<typename Tp>
inline void MyObjectFieldSetValueAs(MyObject* pObject, MyField* pField, const Tp& Data)
{
	static_assert(std::is_standard_layout<Tp>::value);
	
	const MyStruct* pFieldKlass = pField->Type->Klass;
	const uint32_t kKlassSize = pFieldKlass->Attributes & MY_STRUCT_ATTR_POD ? pFieldKlass->Size : sizeof(MyObject*);
	if (sizeof(Tp) != kKlassSize)
	{
		DebugLog::Error("[DEBUG]: Struct sizes mismatched (expected %I64u, got %u)", sizeof(Tp), kKlassSize);
	}

	MyObjectFieldSetValue(pObject, pField, &Data, kKlassSize);
}
