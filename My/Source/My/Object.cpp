#include "Object.h"
#include "My.h"
#include "My/Base/IO.h"
#include "My/VM/VM.h"
#include "My/Utils/Utils.h"
#include "Stb/stb_ds.h"

// Defined as extern
MyDefaults My_Defaults = MyDefaults{};

MyValue::MyValue()
{
    memset(this, 0, sizeof(MyValue));
    Kind = MyValueKind::Invalid;
}

MyValue::MyValue(MyValueKind Kind)
    : MyValue()
{
    this->Kind = Kind;
}

MyValue::~MyValue() noexcept
{ }

MyValue::operator bool() const noexcept
{
    switch (Kind)
    {
        case MyValueKind::Null:    return false;
        case MyValueKind::Bool:    return B08;
        case MyValueKind::Int64:   return bool(I64);
        case MyValueKind::Uint64:  return bool(U64);
        case MyValueKind::Float64: return bool(F64);
        case MyValueKind::String:  return Str != nullptr;
        case MyValueKind::Array:   return Arr != nullptr;
        case MyValueKind::Object:  return Obj != nullptr;
        default: break;
    }

    MY_ASSERT(false, "Error: Cannot convert invalid kind to bool");
    return false;
}

void MyValue::Print(bool bQuoteString) const noexcept
{
    switch (Kind)
    {
        case MyValueKind::Null:     Console::Write("null");                 break;
        case MyValueKind::Bool:     Console::Write(B08 ? "true" : "false"); break;
        case MyValueKind::Int64:    Console::Write("%I64d", I64);           break;
        case MyValueKind::Uint64:   Console::Write("%I64u", U64);           break;
        case MyValueKind::Float64:  Console::Write("%1.9g", F64);           break;
        case MyValueKind::String:
        {
            if (bQuoteString)
            {
                Console::Write("'%s'", Str->Chars);
            }
            else
            {
                Console::Write("%s", Str->Chars);
            }
            break;
        }
        case MyValueKind::Array:
        {
            Console::Write("{ @0x%p, %I64u }", Arr, MyArrayCount(Arr));
            break;
        }
        case MyValueKind::Object:
        {
            Console::Write("%s{ @0x%p }", Obj->Klass->Name, Obj);
            break;
        }
        default:
        {
            MY_ASSERT(false, "Invalid object kind");
            break;
        }
    }
}

MyArrayStride::MyArrayStride(uint32_t kCount)
    : Count(kCount)
{ }

MyArrayStride::MyArrayStride(uint32_t kCount, uint32_t kPitch)
    : Count(kCount), Pitch(kPitch)
{ }

MyArrayStride::MyArrayStride(uint32_t kCount, uint32_t kPitch, uint32_t kSlice)
    : Count(kCount), Pitch(kPitch), Slice(kSlice)
{ }

MyArrayStride::MyArrayStride(uint32_t kCount, uint32_t kPitch, uint32_t kSlice, uint32_t kBlock)
    : Count(kCount), Pitch(kPitch), Slice(kSlice), Block(kBlock)
{ }

MyValue MakeValue_Copy(const MyValue& Value, MyContext* pContext)
{
    switch (Value.Kind)
    {
        case MyValueKind::Null:
        case MyValueKind::Bool:
        case MyValueKind::Int64:
        case MyValueKind::Uint64:
        case MyValueKind::Float64:
            return Value;
        case MyValueKind::String:
        {
            MY_ASSERT(pContext != nullptr, "Invalid context");
            return MakeValue_String(MyStringCopy(pContext, Value.Str));
        }
        case MyValueKind::Array:
        {
            MY_ASSERT(pContext != nullptr, "Invalid context");
            return MakeValue_Array(MyArrayCopy(pContext, Value.Arr));
        }
        case MyValueKind::Object:
        {
            MY_ASSERT(pContext != nullptr, "Invalid context");
            return MakeValue_Object(MyObjectCopy(pContext, Value.Obj));
        }
        default:
        {
            MY_ASSERT(false, "Invalid object kind");
            return MyValue{};
        }
    }
}

MyValue MakeValue_Bool(bool bValue)
{
    MyValue o = MyValue{ MyValueKind::Bool };
    return o.B08 = bValue, o;
}

MyValue MakeValue_Int64(int64_t Value)
{
    MyValue o = MyValue{ MyValueKind::Int64 };
    return o.I64 = Value, o;
}

MyValue MakeValue_Uint64(uint64_t Value)
{
    MyValue o = MyValue{ MyValueKind::Uint64 };
    return o.U64 = Value, o;
}

MyValue MakeValue_Float64(double Value)
{
    MyValue o = MyValue{ MyValueKind::Float64 };
    return o.F64 = Value, o;
}

MyValue MakeValue_String(MyString* pValue)
{
    MyValue o = MyValue{ MyValueKind::String };
    return o.Str = pValue, o;
}

MyValue MakeValue_Array(MyArray* pValue)
{
    MyValue o = MyValue{ MyValueKind::Array };
    return o.Arr = pValue, o;
}

MyValue MakeValue_Object(MyObject* pValue)
{
    MyValue o = MyValue{ MyValueKind::Object };
    return o.Obj = pValue, o;
}

// STRING
MyString* MyStringNew(MyContext* pContext, const char* lpStr)
{
    return MyStringNew(pContext, lpStr, strlen(lpStr));
}

MyString* MyStringNew(MyContext* pContext, const char* lpStr, size_t kLength)
{
    uint64_t kHash = MyHashBytes(lpStr, kLength, 0ul);

    if (MyString* pString = stbds_hmget(pContext->RtCache, kHash); pString != nullptr)
    {
        return pString;
    }
    else
    {
        pString = pContext->VM->GC.CreateString(lpStr, kLength);
        stbds_hmput(pContext->RtCache, kHash, pString);
        return pString;
    }
}

MyString* MyStringNew(MyContext* pContext, const std::string_view& Str)
{
    return MyStringNew(pContext, Str.data(), Str.length());
}

MyString* MyStringFormat(MyContext* pContext, const char* lpFmt, ...)
{
    static constexpr size_t kMaxStringBufferLength = 1024 * 1024; // 1MB
    static char lpBuffer[kMaxStringBufferLength] = { 0 };

    va_list vArgs;
    va_start(vArgs, lpFmt);
    int iResult = vsnprintf(lpBuffer, kMaxStringBufferLength, lpFmt, vArgs); (void)iResult;
    va_end(vArgs);

    return MyStringNew(pContext, lpBuffer);
}

MyString* MyStringCopy(MyContext* pContext, const MyString* pStr)
{
    // FIXME: Do an actual copy. This current implementation doesn't create a new objects,
    //        it returns a cached one
    return MyStringNew(pContext, pStr->Chars);
}

MyString* MyStringIntern(MyString* pStr)
{
    MyContext* pContext = MyContextGet();
    MY_ASSERT(pContext != nullptr, "Cannot intern strings without an active context");

    if (MyString* pCachedStr = stbds_hmget(pContext->RtCache, pStr); pCachedStr)
    {
        return pCachedStr;
    }
    else
    {
        stbds_hmput(pContext->RtCache, pStr->Hash, pStr);
        return pStr;
    }
}

bool MyStringIsInterned(const MyString* pStr)
{
    MyContext* pContext = MyContextGet();
    MY_ASSERT(pContext != nullptr, "Cannot intern strings without an active context");
    return stbds_hmget(pContext->RtCache, pStr->Hash) != nullptr;
}

bool MyStringIsEqual(const MyString* pLhsStr, const MyString* pRhsStr)
{
    // TODO: Assume strings are already interned?
    if (MyStringIsInterned(pLhsStr) && MyStringIsInterned(pRhsStr))
    {
        return pLhsStr == pRhsStr;
    }
    else
    {
        return strncmp(pLhsStr->Chars, pRhsStr->Chars, pLhsStr->Length) == 0;
    }
}

uint64_t MyStringGetHash(const MyString* pStr)
{
    return pStr ? pStr->Hash : 0ul;
}

uint64_t MyStringGetLength(const MyString* pStr)
{
    return pStr ? pStr->Length : 0ul;
}

char* MyStringToUtf8(const MyString* pStr, bool bCopy)
{
    if (!bCopy)
    {
        return pStr->Chars;
    }
    else
    {
        const size_t kLength = pStr->Length + 1;
        return strncpy(new char[kLength]{}, pStr->Chars, kLength);
    }
}

wchar_t* MyStringToUtf16(const MyString* pStr, bool bCopy)
{
    const size_t kLength = pStr->Length + 1;
    wchar_t* lpWstrCopy = new wchar_t[kLength]{};

    for (size_t k = 0; k < kLength-1; k++)
    {
        lpWstrCopy[k] = wchar_t(pStr->Chars[k]);
    }

    return lpWstrCopy;
}

// ARRAY
MyArray* MyArrayNew(MyContext* pContext, MyStruct* pKlass, size_t kStride, size_t kCount, size_t kCapacity)
{
    MY_ASSERT(kCount != 0ull && kCapacity != 0ull, "Error: Invalid array item size");
    MyArrayStride stride;
    stride.Count = kCount;

    return MyArrayNew(pContext, pKlass, stride, kCapacity);
}

MyArray* MyArrayNew(MyContext* pContext, MyStruct* pKlass, const MyArrayStride& Stride, size_t kCapacity)
{
    static constexpr MyArrayStride s_ZeroStride = {};

    MY_ASSERT(memcmp(&Stride, &s_ZeroStride, sizeof(MyArrayStride)) != 0, "Error: Cannot create array with zero stride");
    if (kCapacity < Stride.Count)
    {
        kCapacity = Stride.Count;
    }

    return pContext->VM->GC.CreateArray(pKlass, Stride, kCapacity);
}

MyArray* MyArrayCopy(MyContext* pContext, const MyArray* pArray)
{
    MyArray* pCopy = pContext->VM->GC.CreateArray(pArray->Object.Klass, pArray->Stride, pArray->Stride.Count);

    const uint32_t kSize = pArray->Object.Klass->Size * pCopy->Stride.Count;
    memcpy(pCopy->Data, pArray->Data, kSize);

    return pCopy;
}

bool MyArrayIsEqual(const MyArray* pLhsArray, const MyArray* pRhsArray)
{
    MY_ASSERT(pLhsArray != nullptr && pRhsArray != nullptr, "Invalid arrays");
    if (pLhsArray == pRhsArray)
    {
        // Same object
        return true;
    }
    if (pLhsArray->Data == pRhsArray->Data)
    {
        // If an array has been created, then we are guaranteed that MyArray.Data != nullptr
        // Point to the same block of memory
        return true;
    }
    if (MyArrayCount(pLhsArray) != MyArrayCount(pRhsArray))
    {
        return false;
    }
    /*for (size_t k = 0; k < MyArrayCount(pLhsArray); k++)
    {
        const UniValue& Lhs = pLhsArray->Data[k];
        const UniValue& Rhs = pRhsArray->Data[k];

        if (!Lhs.Equals(Rhs))
        {
            return false;
        }
    }*/
    MY_ASSERT(false, "NotImplemented");

    return true;
}

// INSTANCE
MyObject* MyObjectNew(MyContext* pContext, MyStruct* pKlass)
{
    MyObject* pObject = pContext->VM->GC.CreateObject(pKlass);
    for (size_t k = 0; k < stbds_arrlenu(pKlass->Fields); k++)
    {
        MyField* pField = pKlass->Fields + k;
        pField->Data = &pObject->Data[pField->Offset];
    }
    return pObject;
}

MyObject* MyObjectCopy(MyContext* pContext, const MyObject* pObject)
{
    MyObject* pCopy = pContext->VM->GC.CreateObject(pObject->Klass);
    memcpy(pCopy->Data, pObject->Data, pObject->Klass->Size);
    return pCopy;
}

MyField* MyObjectGetField(MyObject* pObject, char* const& lpField)
{
    return MyStructGetField(pObject->Klass, lpField);
}

Byte* MyObjectFieldGetValue(MyObject* pObject, MyField* pField)
{
    return pObject->Data + pField->Offset;
}

void MyObjectFieldSetValue(MyObject* pObject, MyField* pField, const void* pData, size_t kSize)
{
    memcpy(pObject->Data + pField->Offset, pData, kSize);
}


const char* ValueKindString(MyValueKind Kind) noexcept
{
    switch (Kind)
    {
        case MyValueKind::Null:     return "MyValueKind::Null";
        case MyValueKind::Bool:     return "MyValueKind::Bool";
        case MyValueKind::Int64:    return "MyValueKind::Int64";
        case MyValueKind::Uint64:   return "MyValueKind::Uint64";
        case MyValueKind::Float64:  return "MyValueKind::Float64";
        case MyValueKind::String:   return "MyValueKind::String";
        case MyValueKind::Array:    return "MyValueKind::Array";
        case MyValueKind::Object:   return "MyValueKind::Instance";
        default:                     return "[invalid object kind]";
    }
}
