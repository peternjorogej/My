#include "Builtins.h"
#include "My/Base/IO.h"
#include "My/VM/VM.h"
#include "My/Utils/Utils.h"
#include "stb/stb_ds.h"

#include <stdint.h>
#include <filesystem>

// Core
void _My_Builtin_Equals(MyContext* pContext, MyVM* pVM) noexcept
{
    const uint64_t kRhs = pVM->Stack.PopU64();
    const uint64_t kLhs = pVM->Stack.PopU64();
    pVM->Stack.Push(kLhs == kRhs);
}

void _My_Builtin_CvToInt(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pString = pVM->Stack.PopString();
    const int64_t iValue = strtoll(pString->Chars, nullptr, 10);
    pVM->Stack.Push(iValue);
}

void _My_Builtin_CvToUint(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pString = pVM->Stack.PopString();
    const uint64_t kValue = strtoull(pString->Chars, nullptr, 10);
    pVM->Stack.Push(kValue);
}

void _My_Builtin_CvToFloat(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pString = pVM->Stack.PopString();
    const double dValue = strtod(pString->Chars, nullptr);
    pVM->Stack.Push(dValue);
}

void _My_Builtin_CvIntToString(MyContext* pContext, MyVM* pVM) noexcept
{
    const int64_t iValue = pVM->Stack.PopI64();
    MyString* pString = MyStringNew(pContext, Console::Format("%I64d", iValue));
    pVM->Stack.Push(pString);
}

void _My_Builtin_CvUintToString(MyContext* pContext, MyVM* pVM) noexcept
{
    const uint64_t k = pVM->Stack.PopU64();
    MyString* pString = MyStringNew(pContext, Console::Format("%I64u", k));
    pVM->Stack.Push(pString);
}

void _My_Builtin_CvFloatToString(MyContext* pContext, MyVM* pVM) noexcept
{
    const std::string FloatAsStr = Console::Format("%1.10g", pVM->Stack.PopF64());
    MyString* pString = MyStringNew(pContext, FloatAsStr);
    pVM->Stack.Push(pString);
}

// Std
void _My_Builtin_Write(MyContext* pContext, MyVM* pVM) noexcept
{
    // TODO: Implement properly
    MyArray*  pArgs = pVM->Stack.PopArray();
    MyString* pFmt  = pVM->Stack.PopString();

    if (MyArrayCount(pArgs) == 0ull)
    {
        Console::Write(pFmt->Chars);
        return;
    }

    char*  lpBegin = pFmt->Chars;
    char*  lpEnd   = pFmt->Chars + pFmt->Length;
    size_t kIndex  = 0ull;

    while (true)
    {
        char* pIt = std::find(lpBegin, lpEnd, '%');
        if (pIt == lpEnd)
        {
            Console::Write(lpBegin);
            break;
        }
        else
        {
            if (pIt[1] != '%')
            {
                Console::Write("%.*s", pIt - lpBegin, lpBegin);
            }

            switch (pIt[1])
            {
                case '%':
                    Console::Write("%%");
                    break;
                case 'b':
                    Console::Write("%s", MyArrayGet(pArgs, uint64_t, kIndex++) ? "true" : "false");
                    break;
                case 'd':
                case 'i':
                    Console::Write("%I64d", MyArrayGet(pArgs, int64_t, kIndex++));
                    break;
                case 'f':
                case 'g':
                    Console::Write("%1.9g", MyArrayGet(pArgs, double, kIndex++));
                    break;
                case 'u':
                    Console::Write("%I64u", MyArrayGet(pArgs, uint64_t, kIndex++));
                    break;
                case 's':
                {
                    MyString* const& pString = MyArrayGet(pArgs, MyString*, kIndex++);
                    Console::Write("%s", pString->Chars);
                    break;
                }
                case 'p':
                {
                    void* const& pObject = MyArrayGet(pArgs, void*, kIndex++);
                    Console::Write("0x%p", pObject);
                    break;
                }
                case 'v':
                {
                    MyArray* const& pArray = MyArrayGet(pArgs, MyArray*, kIndex++);
                    Console::Write("{ ");
                    for (size_t k = 0; k < pArray->Count; k++)
                    {
                        const char* const lpSeparator = k == pArray->Count-1 ? "" : ", ";
                        Console::Write("%1.9g%s", MyArrayGet(pArray, double, k), lpSeparator);
                    }
                    Console::Write(" }");
                    break;
                }
                default: break;
            }

            /*if (isdigit(pIt[1]) && pIt[2] == '}')
            {
                Console::Write("%.*s", pIt - lpBegin, lpBegin);
                uint8_t kIndex = pIt[1] - '0';
                Console::Write("(My.Object)");
                pIt += 3;
            }*/

            pIt += 2;
            lpBegin = pIt;
        }
    }
}

void _My_Builtin_WriteLine(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Write(pContext, pVM);
    Console::WriteLine();
}

void _My_Builtin_Read(MyContext* pContext, MyVM* pVM) noexcept
{
    char* const& lpInput = MyGetCachedString(Console::Read());
    MyString* const& pInput = MyStringNew(pContext, lpInput);
    pVM->Stack.Push(pInput);
}

void _My_Builtin_ReadLine(MyContext* pContext, MyVM* pVM) noexcept
{
    char* const& lpInput = MyGetCachedString(Console::ReadLine());
    MyString* const& pInput = MyStringNew(pContext, lpInput);
    pVM->Stack.Push(pInput);

}

void _My_Builtin_ReadInt(MyContext* pContext, MyVM* pVM) noexcept
{
    pVM->Stack.Push(Console::ReadInt64());
}

void _My_Builtin_ReadUint(MyContext* pContext, MyVM* pVM) noexcept
{
    pVM->Stack.Push(Console::ReadUint64());
}

void _My_Builtin_ReadFloat(MyContext* pContext, MyVM* pVM) noexcept
{
    pVM->Stack.Push(Console::ReadFloat64());
}

void _My_Builtin_RandomInt(MyContext* pContext, MyVM* pVM) noexcept
{
    const int64_t iMax = pVM->Stack.PopI64();
    pVM->Stack.Push(Random::Int(iMax));
}

void _My_Builtin_RandomUint(MyContext* pContext, MyVM* pVM) noexcept
{
    const uint64_t kMax = pVM->Stack.PopU64();
    pVM->Stack.Push(Random::Uint(kMax));
}

void _My_Builtin_RandomFloat(MyContext* pContext, MyVM* pVM) noexcept
{
    pVM->Stack.Push(Random::Float());
}

void _My_Builtin_Print(MyContext* pContext, MyVM* pVM) noexcept
{
    // FIXME: Should (realistically) print any object not just strings
    // TODO: Implement properly
    MyString* const& pMessage = pVM->Stack.PopString();
    Console::WriteLine(pMessage->Chars);
}

void _My_Builtin_Length(MyContext* pContext, MyVM* pVM) noexcept
{
    MyArray* const& pArray = pVM->Stack.PopArray();
    pVM->Stack.Push((uint64_t)MyArrayCount(pArray));
}

void _My_Builtin_Clock(MyContext* pContext, MyVM* pVM) noexcept
{
    const double dNow = double(clock()) / double(CLOCKS_PER_SEC);
    pVM->Stack.Push(dNow);
}


void _My_Builtin_HeapAlloc(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kSize = pVM->Stack.PopU64();

    Buffer buffer = Buffer::Create(kSize);

    uint64_t kAddress = buffer ? (uint64_t)(void*)buffer : uint64_t();
    pVM->Stack.Push(kAddress);
}

void _My_Builtin_HeapCopy(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kRhsAddress = pVM->Stack.PopU64();
    uint64_t kLhsAddress = pVM->Stack.PopU64();

    Buffer lbuffer = Buffer{ (uint8_t*)kLhsAddress };
    Buffer rbuffer = Buffer{ (uint8_t*)kRhsAddress };

    if (!lbuffer || !rbuffer)
    {
        return;
    }

    lbuffer.Write((void*)rbuffer, rbuffer.Length());
    
    uint64_t kAddress = lbuffer ? (uint64_t)(void*)lbuffer : uint64_t();
    pVM->Stack.Push(kAddress);
}

void _My_Builtin_HeapResize(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();
    uint64_t kNewSize = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };
    buffer.Resize(kNewSize);

    kAddress = buffer ? (uint64_t)(void*)buffer : uint64_t();
    pVM->Stack.Push(kAddress);
}

void _My_Builtin_HeapFree(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();

    uint8_t* const pAddress = (uint8_t*)kAddress;
    Buffer::Delete(Buffer{ pAddress });
}

void _My_Builtin_Buffer_Length(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();

    const Buffer buffer = Buffer{ (uint8_t*)kAddress };
    pVM->Stack.Push((uint64_t)buffer.Length());
}

void _My_Builtin_Buffer_Capacity(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();

    const Buffer buffer = Buffer{ (uint8_t*)kAddress };
    pVM->Stack.Push((uint64_t)buffer.Capacity());
}

void _My_Builtin_Buffer_WriteI32(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Buffer_WriteU32(pContext, pVM);
}

void _My_Builtin_Buffer_WriteI64(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Buffer_WriteU64(pContext, pVM);
}

void _My_Builtin_Buffer_WriteU32(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kValue32 = pVM->Stack.PopU64();
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    buffer.CheckAndResize(sizeof(uint32_t));
    const uint32_t U32 = (uint32_t)kValue32;
    buffer.Write(&U32, sizeof(uint32_t));
}

void _My_Builtin_Buffer_WriteU64(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kValue64 = pVM->Stack.PopU64();
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    buffer.CheckAndResize(sizeof(uint64_t));
    buffer.Write(&kValue64, sizeof(uint64_t));
}

void _My_Builtin_Buffer_WriteF32(MyContext* pContext, MyVM* pVM) noexcept
{
    double dValue32 = pVM->Stack.PopF64();
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    buffer.CheckAndResize(sizeof(float));
    const float F32 = (float)dValue32;
    buffer.Write(&F32, sizeof(float));
}

void _My_Builtin_Buffer_WriteF64(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Buffer_WriteU64(pContext, pVM);
}

void _My_Builtin_Buffer_WriteString(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* pString = pVM->Stack.PopString();
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    // Write both the string and it's length
    const uint64_t kRequiredSize = sizeof(uint32_t) + pString->Length;
    buffer.CheckAndResize(kRequiredSize);

    // Write the length
    const uint32_t kLength = (uint32_t)pString->Length;
    buffer.Write(&kLength, sizeof(uint32_t));
    // Write the characters
    buffer.Write(pString->Chars, pString->Length);
}

void _My_Builtin_Buffer_Append(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kRhsAddress = pVM->Stack.PopU64();
    uint64_t kLhsAddress = pVM->Stack.PopU64();

    Buffer lbuffer = Buffer{ (uint8_t*)kLhsAddress };
    Buffer rbuffer = Buffer{ (uint8_t*)kRhsAddress };

    if (!lbuffer || !rbuffer)
    {
        return;
    }

    lbuffer.Write((void*)rbuffer, rbuffer.Length());
}

void _My_Builtin_Buffer_ReadI32(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Buffer_ReadU32(pContext, pVM);
}

void _My_Builtin_Buffer_ReadI64(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Buffer_ReadU64(pContext, pVM);
}

void _My_Builtin_Buffer_ReadU32(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    uint32_t kValue32 = 0ul;
    buffer.Read(&kValue32, sizeof(uint32_t));
    pVM->Stack.Push((uint64_t)kValue32);
}

void _My_Builtin_Buffer_ReadU64(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    uint64_t kValue64 = 0ull;
    buffer.Read(&kValue64, sizeof(uint64_t));
    pVM->Stack.Push(kValue64);
}

void _My_Builtin_Buffer_ReadF32(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    float fValue32 = 0ul;
    buffer.Read(&fValue32, sizeof(float));
    pVM->Stack.Push(fValue32);
}

void _My_Builtin_Buffer_ReadF64(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Buffer_ReadU64(pContext, pVM);
}

void _My_Builtin_Buffer_ReadString(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kAddress = pVM->Stack.PopU64();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    if (!buffer)
    {
        return;
    }

    // Read the length
    uint32_t kLength = 0ul;
    buffer.Read(&kLength, sizeof(uint32_t));
    // Read the characters
    Buffer strbuf = Buffer::Create((uint64_t)kLength + 1);
    buffer.Read((void*)strbuf, kLength);

    MyString* pString = MyStringNew(pContext, (char*)strbuf, kLength);
    Buffer::Delete(strbuf);

    pVM->Stack.Push(pString);
}

void _My_Builtin_Buffer_Get(MyContext* pContext, MyVM* pVM) noexcept
{
    MY_NOT_IMPLEMENTED();
}

// Math
void _My_Builtin_Sin(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(sin(x));
}

void _My_Builtin_Cos(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(cos(x));
}

void _My_Builtin_Tan(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(tan(x));
}

void _My_Builtin_Asin(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(asin(x));
}

void _My_Builtin_Acos(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(acos(x));
}

void _My_Builtin_Atan(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(atan(x));
}

void _My_Builtin_Atan2(MyContext* pContext, MyVM* pVM) noexcept
{
    const double y = pVM->Stack.PopF64();
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(atan2(x, y));
}

void _My_Builtin_Log(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(log(x));
}

void _My_Builtin_Log10(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(log10(x));
}

void _My_Builtin_Logb(MyContext* pContext, MyVM* pVM) noexcept
{
    const double y = pVM->Stack.PopF64();
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(log(x) / log(y));
}

void _My_Builtin_Exp(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(exp(x));
}

void _My_Builtin_Floor(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(floor(x));
}

void _My_Builtin_Ceil(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(ceil(x));
}

void _My_Builtin_Sqrt(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(sqrt(x));
}

void _My_Builtin_Cbrt(MyContext* pContext, MyVM* pVM) noexcept
{
    const double x = pVM->Stack.PopF64();
    pVM->Stack.Push(cbrt(x));
}

// String
void _My_Builtin_String_Concat(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pRhs = pVM->Stack.PopString();
    MyString* const& pLhs = pVM->Stack.PopString();

    const char* lpString = MyGetCachedStringV("%s%s", pLhs->Chars, pRhs->Chars);
    MyString* pString = MyStringNew(pContext, lpString);

    pVM->Stack.Push(pString);
}

void _My_Builtin_String_Compare(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pRhs = pVM->Stack.PopString();
    MyString* const& pLhs = pVM->Stack.PopString();
    pVM->Stack.Push(pLhs == pRhs);
}

void _My_Builtin_String_Find(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pSubstr = pVM->Stack.PopString();
    MyString* const& pString = pVM->Stack.PopString();

    const std::string_view sv = std::string_view(pString->Chars);

    pVM->Stack.Push(sv.find(pSubstr->Chars, 0ull));
}

void _My_Builtin_String_Substr(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kLength = pVM->Stack.PopU64();
    uint64_t kStart  = pVM->Stack.PopU64();
    MyString* const& pString = pVM->Stack.PopString();

    if (kStart > pString->Length)
    {
        kStart = 0ull;
    }
    if (kLength == 0ul || kLength > pString->Length)
    {
        kLength = pString->Length;
    }

    const char* lpBegin = pString->Chars + kStart;
    MyString* pSubstr = MyStringNew(pContext, lpBegin, kLength);

    pVM->Stack.Push(pSubstr);
}

void _My_Builtin_String_Split(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pSubstr = pVM->Stack.PopString();
    MyString* const& pString = pVM->Stack.PopString();

    const std::string_view sv = std::string_view(pString->Chars);

    MyString** ppStrings = nullptr;

    uint64_t kNextSubstrStart = 0ull, k = 0ull;
    while (true)
    {
        if (k = sv.find(pSubstr->Chars, k); k != std::string::npos)
        {
            const uint64_t kLength = k - kNextSubstrStart;
            MyString* const pSplit_k = MyStringNew(pContext, pString->Chars + kNextSubstrStart, kLength);
            stbds_arrpush(ppStrings, pSplit_k);

            kNextSubstrStart += kLength + pSubstr->Length;
            k++;
        }
        else
        {
            MyString* const pSplit_k = MyStringNew(pContext, pString->Chars + kNextSubstrStart);
            stbds_arrpush(ppStrings, pSplit_k);

            break;
        }
    }


    const uint64_t kCount = stbds_arrlenu(ppStrings);
    MyArray* pSplits = MyArrayNew(pContext, My_Defaults.StringStruct, kCount, kCount);
    memcpy(pSplits->Data, ppStrings, kCount*sizeof(MyString*));

    stbds_arrfree(ppStrings);

    pVM->Stack.Push(pSplits);
}

void _My_Builtin_String_StartsWith(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pPrefix = pVM->Stack.PopString();
    MyString* const& pString = pVM->Stack.PopString();

    if (pPrefix->Length > pString->Length)
    {
        pVM->Stack.Push(false);
    }
    else
    {
        const bool bStartsWithX = strncmp(pString->Chars, pPrefix->Chars, pPrefix->Length) == 0;
        pVM->Stack.Push(bStartsWithX);
    }
}

void _My_Builtin_String_EndsWith(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pSuffix = pVM->Stack.PopString();
    MyString* const& pString = pVM->Stack.PopString();

    if (pSuffix->Length > pString->Length)
    {
        pVM->Stack.Push(false);
    }
    else
    {
        const uint64_t kOffset = pString->Length - pSuffix->Length;
        const bool bEndsWithX = strncmp(pString->Chars + kOffset, pSuffix->Chars, pSuffix->Length) == 0;
        pVM->Stack.Push(bEndsWithX);
    }
}

void _My_Builtin_String_ToUpper(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pString = pVM->Stack.PopString();

    char* lpUpperCaseString = _strdup(pString->Chars);
    for (size_t k = 0; k < pString->Length; k++)
    {
        lpUpperCaseString[k] = toupper(lpUpperCaseString[k]);
    }

    MyString* pUpper = MyStringNew(pContext, lpUpperCaseString);
    free(lpUpperCaseString), lpUpperCaseString = nullptr;
    pVM->Stack.Push(pUpper);
}

void _My_Builtin_String_ToLower(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pString = pVM->Stack.PopString();

    char* lpLowerCaseString = _strdup(pString->Chars);
    for (size_t k = 0; k < pString->Length; k++)
    {
        lpLowerCaseString[k] = tolower(lpLowerCaseString[k]);
    }

    MyString* pLower = MyStringNew(pContext, lpLowerCaseString);
    free(lpLowerCaseString), lpLowerCaseString = nullptr;
    pVM->Stack.Push(pLower);
}

// StringBuilder
static void StrBldr_CheckAndResizeBuffer(MyObject* pStrBldr, uint64_t kSize) noexcept
{
    char*& pBuffer = MyObjectFieldGetValueAs<char*>(pStrBldr, MyObjectGetField(pStrBldr, "CString"));
    uint64_t& kCapacity = MyObjectFieldGetValueAs<uint64_t>(pStrBldr, MyObjectGetField(pStrBldr, "Capacity"));
    uint64_t kLength = MyObjectFieldGetValueAs<uint64_t>(pStrBldr, MyObjectGetField(pStrBldr, "Length"));

    const uint64_t iAvailCapacity = kCapacity - kLength;
    if (iAvailCapacity < kSize)
    {
        uint64_t kNewCapacity = kCapacity + (kCapacity / 2ul) + kSize;
        char* pNewBuffer = (char*)realloc(pBuffer, kNewCapacity);
        if (pNewBuffer)
        {
            pBuffer = pNewBuffer;
            kCapacity = kNewCapacity;
            
            memset(pBuffer + kLength, 0, kCapacity - kLength);
        }
    }
}

static void StrBldr_Write(MyObject* pStrBldr, const char* lpText, uint64_t kTextLength, bool bNewLine = false) noexcept
{
    char* pBuffer = MyObjectFieldGetValueAs<char*>(pStrBldr, MyObjectGetField(pStrBldr, "CString"));
    uint64_t& kLength = MyObjectFieldGetValueAs<uint64_t>(pStrBldr, MyObjectGetField(pStrBldr, "Length"));

    memcpy(pBuffer + kLength, lpText, kTextLength);
    kLength += kTextLength;

    if (bNewLine)
    {
        pBuffer[kLength++] = '\n';
    }
}

static void StrBldr_Indent(MyObject* pStrBldr) noexcept
{
    static constexpr int s_IndentSize = 4;
    static constexpr const char* s_Space = "                                                                                                                                               ";

    int64_t iIndent = MyObjectFieldGetValueAs<int64_t>(pStrBldr, MyObjectGetField(pStrBldr, "Indent"));
    int64_t iFullIndentLength = iIndent * s_IndentSize;

    StrBldr_CheckAndResizeBuffer(pStrBldr, iFullIndentLength);
    if (iFullIndentLength > 0ll)
    {
        StrBldr_Write(pStrBldr, s_Space, iFullIndentLength);
    }
}

void _My_Builtin_StringBuilder_Init(MyContext* pContext, MyVM* pVM)
{
    MyObject* pStrBldr = pVM->Stack.PopObject();

    uint64_t& kInitialCapacity = MyObjectFieldGetValueAs<uint64_t>(pStrBldr, MyObjectGetField(pStrBldr, "Capacity"));
    if (kInitialCapacity == 0ull)
    {
        kInitialCapacity = 64ull;
    }
    
    if (char* pBuffer = (char*)malloc(kInitialCapacity); pBuffer)
    {
        MyObjectFieldSetValueAs<char*>(pStrBldr, MyObjectGetField(pStrBldr, "CString"), pBuffer);
        MyObjectFieldSetValueAs<uint64_t>(pStrBldr, MyObjectGetField(pStrBldr, "Length"), 0ull);
        MyObjectFieldSetValueAs<int64_t>(pStrBldr, MyObjectGetField(pStrBldr, "Indent"), 0ll);
    }
}

void _My_Builtin_StringBuilder_Append(MyContext* pContext, MyVM* pVM)
{
    MyString* pText = pVM->Stack.PopString();
    MyObject* pStrBldr = pVM->Stack.PopObject();

    if (pText->Length > 0ul)
    {
        StrBldr_CheckAndResizeBuffer(pStrBldr, pText->Length);
        StrBldr_Write(pStrBldr, pText->Chars, pText->Length);
    }
}

void _My_Builtin_StringBuilder_AppendV(MyContext* pContext, MyVM* pVM)
{
    MY_NOT_IMPLEMENTED();
}

void _My_Builtin_StringBuilder_Write(MyContext* pContext, MyVM* pVM)
{
    MyString* pText = pVM->Stack.PopString();
    MyObject* pStrBldr = pVM->Stack.PopObject();

    if (pText->Length > 0ul)
    {
        StrBldr_Indent(pStrBldr);
        StrBldr_CheckAndResizeBuffer(pStrBldr, pText->Length);
        StrBldr_Write(pStrBldr, pText->Chars, pText->Length);
    }
}

void _My_Builtin_StringBuilder_WriteV(MyContext* pContext, MyVM* pVM)
{
    MY_NOT_IMPLEMENTED();
}

void _My_Builtin_StringBuilder_WriteLine(MyContext* pContext, MyVM* pVM)
{
    MyString* pText    = pVM->Stack.PopString();
    MyObject* pStrBldr = pVM->Stack.PopObject();

    if (pText->Length > 0ul)
    {
        StrBldr_Indent(pStrBldr);
        StrBldr_CheckAndResizeBuffer(pStrBldr, pText->Length + 1ul); // +1 for the new line char
        StrBldr_Write(pStrBldr, pText->Chars, pText->Length, true);
    }
}

void _My_Builtin_StringBuilder_WriteLineV(MyContext* pContext, MyVM* pVM)
{
    MY_NOT_IMPLEMENTED();
}

void _My_Builtin_StringBuilder_ToString(MyContext* pContext, MyVM* pVM)
{
    MyObject* pStrBldr = pVM->Stack.PopObject();

    char*& pCString = MyObjectFieldGetValueAs<char*>(pStrBldr, MyObjectGetField(pStrBldr, "CString"));
    MyString* pString = MyStringNew(pContext, pCString);
    
    free(pCString);
    pCString = nullptr;

    pVM->Stack.Push(pString);
}

// Bytes
static void Bytes_CheckAndResizeBuffer(MyObject* pBytes, uint64_t kSize) noexcept
{
    char*& pCBuffer = MyObjectFieldGetValueAs<char*>(pBytes, MyObjectGetField(pBytes, "CBuffer"));
    uint64_t& kCapacity = MyObjectFieldGetValueAs<uint64_t>(pBytes, MyObjectGetField(pBytes, "Capacity"));
    uint64_t kLength = MyObjectFieldGetValueAs<uint64_t>(pBytes, MyObjectGetField(pBytes, "Length"));

    const uint64_t iAvailCapacity = kCapacity - kLength;
    if (iAvailCapacity < kSize)
    {
        uint64_t kNewCapacity = kCapacity + (kCapacity / 2ul) + kSize;
        char* pNewBuffer = (char*)realloc(pCBuffer, kNewCapacity);
        if (pNewBuffer)
        {
            pCBuffer = pNewBuffer;
            kCapacity = kNewCapacity;

            memset(pCBuffer + kLength, 0, kCapacity - kLength);
        }
    }
}

void _My_Builtin_Bytes_Create(MyContext* pContext, MyVM* pVM) noexcept
{
    uint64_t kInitialCapacity = pVM->Stack.PopU64();

    if (kInitialCapacity == 0ull)
    {
        kInitialCapacity = 64ull;
    }

    MyObject* pBytes = MyObjectNew(pContext, My_Defaults.BytesStruct);

    char* const pCBuffer = (char*)malloc(kInitialCapacity);
    MY_ASSERT(pCBuffer != nullptr, "Failed to allocate memory");

    memset(pCBuffer, 0, kInitialCapacity);

    MyObjectFieldSetValueAs<char*>(pBytes, MyObjectGetField(pBytes, "CBuffer"), pCBuffer);
    MyObjectFieldSetValueAs<uint64_t>(pBytes, MyObjectGetField(pBytes, "Capacity"), kInitialCapacity);

    pVM->Stack.Push(pBytes);
}

void _My_Builtin_Bytes_Free(MyContext* pContext, MyVM* pVM) noexcept
{
    MyObject* pBytes = pVM->Stack.PopObject();

    char*& pCBuffer = MyObjectFieldGetValueAs<char*>(pBytes, MyObjectGetField(pBytes, "CBuffer"));

    // NOTE: Check memory of *pCBuffer* in Debug > Windows > Memory 1 (while debugger is running)
    // to confirm that Bytes API works
    free(pCBuffer);
    pCBuffer = nullptr;
}

void _My_Builtin_Bytes_AddInt32(MyContext* pContext, MyVM* pVM) noexcept
{
    MY_NOT_IMPLEMENTED();
}

void _My_Builtin_Bytes_AddInt64(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Bytes_AddUInt64(pContext, pVM);
}

void _My_Builtin_Bytes_AddUInt32(MyContext* pContext, MyVM* pVM) noexcept
{
    MY_NOT_IMPLEMENTED();
}

void _My_Builtin_Bytes_AddUInt64(MyContext* pContext, MyVM* pVM) noexcept
{
    constexpr uint64_t kSize = sizeof(uint64_t);

    uint64_t kValue = pVM->Stack.PopU64();
    MyObject* const pBytes = pVM->Stack.PopObject();

    char* const& pCBuffer = MyObjectFieldGetValueAs<char*>(pBytes, MyObjectGetField(pBytes, "CBuffer"));
    uint64_t& kLength = MyObjectFieldGetValueAs<uint64_t>(pBytes, MyObjectGetField(pBytes, "Length"));

    Bytes_CheckAndResizeBuffer(pBytes, kSize);
    memcpy(pCBuffer + kLength, &kValue, kSize);
    kLength += kSize;
}

void _My_Builtin_Bytes_AddFloat32(MyContext* pContext, MyVM* pVM) noexcept
{
    MY_NOT_IMPLEMENTED();
}

void _My_Builtin_Bytes_AddFloat64(MyContext* pContext, MyVM* pVM) noexcept
{
    _My_Builtin_Bytes_AddUInt64(pContext, pVM);
}

void _My_Builtin_Bytes_AddString(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pStr = pVM->Stack.PopString();
    MyObject* const& pBytes = pVM->Stack.PopObject();

    char* const& pCBuffer = MyObjectFieldGetValueAs<char*>(pBytes, MyObjectGetField(pBytes, "CBuffer"));
    uint64_t& kLength = MyObjectFieldGetValueAs<uint64_t>(pBytes, MyObjectGetField(pBytes, "Length"));

    Bytes_CheckAndResizeBuffer(pBytes, pStr->Length);
    memcpy(pCBuffer + kLength, pStr->Chars, pStr->Length);
    kLength += pStr->Length;
}

void _My_Builtin_Bytes_Append(MyContext* pContext, MyVM* pVM) noexcept
{
    MyObject* const& pOtherBytes = pVM->Stack.PopObject();
    MyObject* const& pThisBytes = pVM->Stack.PopObject();

    char* const& pThisCBuffer = MyObjectFieldGetValueAs<char*>(pThisBytes, MyObjectGetField(pThisBytes, "CBuffer"));
    uint64_t& kThisLength = MyObjectFieldGetValueAs<uint64_t>(pThisBytes, MyObjectGetField(pThisBytes, "Length"));
    
    const char* const pOtherCBuffer = MyObjectFieldGetValueAs<char*>(pOtherBytes, MyObjectGetField(pOtherBytes, "CBuffer"));
    const uint64_t kOtherLength = MyObjectFieldGetValueAs<uint64_t>(pOtherBytes, MyObjectGetField(pOtherBytes, "Length"));

    Bytes_CheckAndResizeBuffer(pThisBytes, kOtherLength);
    memcpy(pThisCBuffer + kThisLength, pOtherCBuffer, kOtherLength);
    kThisLength += kOtherLength;
}

// File
void _My_Builtin_File_Open(MyContext* pContext, MyVM* pVM)
{
    static const auto ValidateOpenMode = [&pContext](MyString* pOpenMode) -> const MyString*
    {
        static MyString* const s_DefaultOpenMode = MyStringNew(pContext, "rt");

        const char* const lpOpenMode = pOpenMode->Chars;

        if (pOpenMode->Length != 2ul)
        {
            return s_DefaultOpenMode;
        }
        if (lpOpenMode[0] != 'r' && lpOpenMode[0] != 'w' && lpOpenMode[0] != 'a')
        {
            return s_DefaultOpenMode;
        }
        if (lpOpenMode[1] != 't' && lpOpenMode[1] != 'b')
        {
            return s_DefaultOpenMode;
        }

        return nullptr;
    };

    MyString* pOpenMode = pVM->Stack.PopString();
    MyString* pFilepath = pVM->Stack.PopString();

    const MyString* pValidOpenMode = ValidateOpenMode(pOpenMode);
    if (pValidOpenMode)
    {
        pOpenMode = (MyString*)pValidOpenMode;
    }
    const char* lpOpenMode = pOpenMode->Chars;
    const bool bReadMode = lpOpenMode[0] == 'r' || lpOpenMode[0] == 'a';

    MyObject* pFile = MyObjectNew(pContext, My_Defaults.FileStruct);
    MyObjectFieldSetValueAs<MyString*>(pFile, MyObjectGetField(pFile, "Filepath"), pFilepath);
    if (const MyString* pNewOpenMode = ValidateOpenMode(pOpenMode); pNewOpenMode)
    {
        lpOpenMode = pNewOpenMode->Chars;
        MyObjectFieldSetValueAs<const MyString*>(pFile, MyObjectGetField(pFile, "OpenMode"), pNewOpenMode);
    }
    else
    {
        lpOpenMode = pOpenMode->Chars;
        MyObjectFieldSetValueAs<const MyString*>(pFile, MyObjectGetField(pFile, "OpenMode"), pOpenMode);
    }
    MyObjectFieldSetValueAs<int64_t>(pFile, MyObjectGetField(pFile, "FileSize"), -1LL);

    if (bReadMode && !std::filesystem::exists(pFilepath->Chars))
    {
        // Read
        pVM->Stack.Push(pFile);
        return;
    }

    if (FILE* pCFile = fopen(pFilepath->Chars, pOpenMode->Chars); pCFile)
    {
        int64_t iFileSize = -1LL;

        if (bReadMode)
        {
            fseek(pCFile, 0, SEEK_END);
            iFileSize = ftell(pCFile);
            fseek(pCFile, 0, SEEK_SET);
        }

        MyObjectFieldSetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"), pCFile);
        MyObjectFieldSetValueAs<int64_t>(pFile, MyObjectGetField(pFile, "FileSize"), iFileSize);
    }

    pVM->Stack.Push(pFile);
}

void _My_Builtin_File_Close(MyContext* pContext, MyVM* pVM)
{
    MyObject* const& pFile = pVM->Stack.PopObject();
    FILE*& pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));
    // TODO: Doesn't work. FWrite works well, but after closing
    // the file, some extra bytes are added to it.
    if (pCFile)
    {
        fclose(pCFile);
        pCFile = nullptr;
    }
}

void _My_Builtin_File_IsOpen(MyContext* pContext, MyVM* pVM)
{
    MyObject* const& pFile = pVM->Stack.PopObject();
    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));
    pVM->Stack.Push(pCFile != nullptr);
}

void _My_Builtin_File_Read(MyContext* pContext, MyVM* pVM)
{
    MyObject* const& pFile = pVM->Stack.PopObject();

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));

    fseek(pCFile, 0, SEEK_END);
    const size_t kLength = ftell(pCFile);
    fseek(pCFile, 0, SEEK_SET);

    MyString* pContents = nullptr;

    if (Buffer buffer = Buffer::Create(kLength + 1); buffer)
    {
        fread((void*)buffer, sizeof(char), kLength, pCFile);
        pContents = MyStringNew(pContext, (char*)buffer, kLength);
        
        Buffer::Delete(buffer);
    }
    else
    {
        pContents = MyStringNew(pContext, "", kLength);
    }

    pVM->Stack.Push(pContents);
}

void _My_Builtin_File_ReadN(MyContext* pContext, MyVM* pVM)
{
    uint64_t kLengthToRead = pVM->Stack.PopU64();
    MyObject* const& pFile = pVM->Stack.PopObject();

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));

    fseek(pCFile, 0, SEEK_END);
    kLengthToRead = std::min(kLengthToRead, (uint64_t)ftell(pCFile));
    fseek(pCFile, 0, SEEK_SET);

    MyString* pContents = nullptr;

    if (Buffer buffer = Buffer::Create(kLengthToRead + 1); buffer)
    {
        fread((void*)buffer, sizeof(char), kLengthToRead, pCFile);
        pContents = MyStringNew(pContext, (char*)buffer, kLengthToRead);

        Buffer::Delete(buffer);
    }
    else
    {
        pContents = MyStringNew(pContext, "");
    }

    pVM->Stack.Push(pContents);
}

void _My_Builtin_File_ReadBytes(MyContext* pContext, MyVM* pVM)
{
    MyObject* const& pFile = pVM->Stack.PopObject();

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));

    fseek(pCFile, 0, SEEK_END);
    const size_t kLength = ftell(pCFile);
    fseek(pCFile, 0, SEEK_SET);

    Buffer buffer = Buffer::Create(kLength);
    if (buffer)
    {
        fread((void*)buffer, sizeof(char), kLength, pCFile);
    }

    pVM->Stack.Push((uint64_t)(void*)buffer);
}

void _My_Builtin_File_ReadNBytes(MyContext* pContext, MyVM* pVM)
{
    uint64_t kLengthToRead = pVM->Stack.PopU64();
    MyObject* const& pFile = pVM->Stack.PopObject();

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));

    fseek(pCFile, 0, SEEK_END);
    kLengthToRead = std::min(kLengthToRead, (uint64_t)ftell(pCFile));
    fseek(pCFile, 0, SEEK_SET);

    Buffer buffer = Buffer::Create(kLengthToRead);
    if (buffer)
    {
        fread((void*)buffer, sizeof(char), kLengthToRead, pCFile);
    }

    pVM->Stack.Push((uint64_t)(void*)buffer);
}

void _My_Builtin_File_Write(MyContext* pContext, MyVM* pVM)
{
    MyString* const& pStr = pVM->Stack.PopString();
    MyObject* const& pFile = pVM->Stack.PopObject();

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));
    fwrite(pStr->Chars, sizeof(char), pStr->Length, pCFile);
}

void _My_Builtin_File_WriteN(MyContext* pContext, MyVM* pVM)
{
    uint64_t kSizeToWrite = pVM->Stack.PopU64();
    MyString* pString = pVM->Stack.PopString();
    MyObject* const& pFile = pVM->Stack.PopObject();

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));
    if (pCFile)
    {
        kSizeToWrite = std::min(kSizeToWrite, pString->Length);
        fwrite(pString->Chars, sizeof(uint8_t), kSizeToWrite, pCFile);
    }
}

void _My_Builtin_File_WriteBytes(MyContext* pContext, MyVM* pVM)
{
    uint64_t kAddress = pVM->Stack.PopU64();
    MyObject* const& pFile = pVM->Stack.PopObject();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));
    if (pCFile)
    {
        fwrite((void*)buffer, sizeof(uint8_t), buffer.Length(), pCFile);
    }
}

void _My_Builtin_File_WriteNBytes(MyContext* pContext, MyVM* pVM)
{
    uint64_t kSizeToWrite = pVM->Stack.PopU64();
    uint64_t kAddress = pVM->Stack.PopU64();
    MyObject* const& pFile = pVM->Stack.PopObject();

    Buffer buffer = Buffer{ (uint8_t*)kAddress };

    FILE* pCFile = MyObjectFieldGetValueAs<FILE*>(pFile, MyObjectGetField(pFile, "CFile"));
    if (pCFile)
    {
        kSizeToWrite = std::min(kSizeToWrite, (uint64_t)buffer.Length());
        fwrite((void*)buffer, sizeof(uint8_t), kSizeToWrite, pCFile);
    }
}

int64_t my_pow(int64_t base, int64_t exponent) noexcept
{
    // Maybe a better implementation *?
    return int64_t(pow(double(base), double(exponent)));
}
