#include "Builtins.h"
#include "My/Base/IO.h"
#include "My/VM/VM.h"
#include "My/Utils/Utils.h"

// Core
void _My_Builtin_Strcat(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pRhs = pVM->Stack.PopString();
    MyString* const& pLhs = pVM->Stack.PopString();

    const char* lpString = UniStrdupV("%s%s", pLhs->Chars, pRhs->Chars);
    MyString* pString = MyStringNew(pContext, lpString);
    
    pVM->Stack.Push(pString);
}

void _My_Builtin_Strcmp(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pRhs = pVM->Stack.PopString();
    MyString* const& pLhs = pVM->Stack.PopString();
    pVM->Stack.Push(pLhs == pRhs);
}

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
    const char* const lpFmt = iValue < 0 ? "%I64d" : "%I64u";
    MyString* pString = MyStringNew(pContext, Console::Format(lpFmt, iValue));
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
    DebugLog::Warn("[DEBUG]: [%s] is not properly implemented", __FUNCTION__);

    MyArray*  pArgs = pVM->Stack.PopArray();
    MyString* pFmt  = pVM->Stack.PopString();

    if (MyArrayCount(pArgs) == 0ull)
    {
        Console::Write(pFmt->Chars);
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
                default: break;
            }

            /*if (isdigit(pIt[1]) && pIt[2] == '}')
            {
                Console::Write("%.*s", pIt - lpBegin, lpBegin);
                uint8_t kIndex = pIt[1] - '0';
                Console::Write("(Uni.Object)");
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
    char* const& lpInput = UniStrdup(Console::Read());
    MyString* const& pInput = MyStringNew(pContext, lpInput);
    pVM->Stack.Push(pInput);
}

void _My_Builtin_ReadLine(MyContext* pContext, MyVM* pVM) noexcept
{
    char* const& lpInput = UniStrdup(Console::ReadLine());
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
    DebugLog::Warn("[DEBUG]: [%s] is not properly implemented", __FUNCTION__);
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

#if 0
// Core
void _My_Builtin_Strcat(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 2, "Invalid argument count");

    MyString* const& pLhs = pICC->Argv[0].Str;
    MyString* const& pRhs = pICC->Argv[1].Str;

    const char* lpString = UniStrdupV("%s%s", pLhs->Chars, pRhs->Chars);
    MyString* pString = MyStringNew(pICC->Context, lpString);
    
    *pICC->Out = MakeValue_String(pString);
    return true;
}

void _My_Builtin_Strcmp(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 2, "Invalid argument count");

    MyString* const& pLhs = pICC->Argv[0].Str;
    MyString* const& pRhs = pICC->Argv[1].Str;

    *pICC->Out = MakeValue_Bool(pLhs == pRhs);
    return true;
}

void _My_Builtin_Equals(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 2, "Invalid argument count");

    const MyObject& Lhs = pICC->Argv[0];
    const MyObject& Rhs = pICC->Argv[1];

    *pICC->Out = MakeValue_Bool(Lhs.Equals(Rhs));
    return true;
}

void _My_Builtin_CvToBool(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    const MyObject& oValue = pICC->Argv[0];

    *pICC->Out = MakeValue_Bool((bool)oValue);
    return true;
}

void _My_Builtin_CvToInt(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    const MyObject& oValue = pICC->Argv[0];

    *pICC->Out = MakeValue_Int64(oValue.As<int64_t>());
    return true;
}

void _My_Builtin_CvToUint(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    const MyObject& oValue = pICC->Argv[0];

    *pICC->Out = MakeValue_Uint64(oValue.As<uint64_t>());
    return true;
}

void _My_Builtin_CvToFloat(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    const MyObject& oValue = pICC->Argv[0];

    *pICC->Out = MakeValue_Float64(oValue.As<double>());
    return true;
}

void _My_Builtin_CvToString(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    const MyObject& oValue = pICC->Argv[0];
    MyString* pString = MyStringNew(pICC->Context, oValue.ToString());
    
    *pICC->Out = MakeValue_String(pString);
    return true;
}

// Std
void _My_Builtin_Write(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 2, "Invalid argument count");

    MyString* pFmt  = pICC->Argv[0].Str;
    MyArray*  pArgs = pICC->Argv[1].Arr;

    if (pArgs->Count == 0ull)
    {
        Console::Write(pFmt->Chars);
        return false;
    }

    char* lpBegin = pFmt->Chars;
    char* lpEnd   = pFmt->Chars + pFmt->Length;

    while (true)
    {
        char* pIt = std::find(lpBegin, lpEnd, '{');
        if (pIt == lpEnd)
        {
            Console::Write(lpBegin);
            break;
        }
        else
        {
            if (isdigit(pIt[1]) && pIt[2] == '}')
            {
                Console::Write("%.*s", pIt - lpBegin, lpBegin);
                uint8_t kIndex = pIt[1] - '0';
                // pArgs->Items[kIndex].Print();
                Console::Write("(Uni.Object)");
                pIt += 3;
            }
            
            lpBegin = pIt;
        }
    }

    return false;
}

void _My_Builtin_WriteLine(InternalCallbackContext* pICC) noexcept
{
    _My_Builtin_Write(pICC);
    Console::WriteLine();
    return false;
}

void _My_Builtin_Read(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 0, "Invalid argument count");

    char* const& lpInput = UniStrdup(Console::Read());
    pICC->Out->Kind = MyValueKind::String;
    pICC->Out->Str  = MyStringNew(pICC->Context, lpInput);

    return true;
}

void _My_Builtin_ReadLine(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 0, "Invalid argument count");

    char* const& lpInput = UniStrdup(Console::ReadLine());
    pICC->Out->Kind = MyValueKind::String;
    pICC->Out->Str  = MyStringNew(pICC->Context, lpInput);

    return true;
}

void _My_Builtin_ReadInt(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 0, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Int64;
    pICC->Out->I64  = Console::ReadInt64();
    return true;
}

void _My_Builtin_ReadUint(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 0, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Uint64;
    pICC->Out->U64  = Console::ReadUint64();
    return true;
}

void _My_Builtin_ReadFloat(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 0, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = Console::ReadFloat64();
    return true;
}

void _My_Builtin_RandomInt(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Int64;
    pICC->Out->I64  = Random::Int(pICC->Argv[0].I64);
    return true;
}

void _My_Builtin_RandomUint(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Uint64;
    pICC->Out->U64  = Random::Uint(pICC->Argv[0].U64);
    return true;
}

void _My_Builtin_RandomFloat(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 0, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = Random::Float();
    return true;
}

void _My_Builtin_Print(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Argv[0].Print();
    Console::WriteLine();
    
    return false;
}

void _My_Builtin_Length(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    *pICC->Out = MakeValue_Uint64(pICC->Argv->Arr->Count);
    return true;
}

void _My_Builtin_Clock(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 0, "Invalid argument count");

    const double dNow = double(clock()) / double(CLOCKS_PER_SEC);

    *pICC->Out = MakeValue_Float64(dNow);
    return true;
}

// Math
void _My_Builtin_Sin(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = sin(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Cos(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = cos(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Tan(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = tan(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Asin(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = asin(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Acos(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = acos(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Atan(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = atan(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Atan2(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 2, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = atan2(pICC->Argv[0].F64, pICC->Argv[1].F64);
    return true;
}

void _My_Builtin_Log(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = log(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Log10(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = log10(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Logb(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 2, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = log(pICC->Argv[0].F64) / log(pICC->Argv[1].F64);
    return true;
}

void _My_Builtin_Exp(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64  = exp(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Floor(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64 = floor(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Ceil(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64 = ceil(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Sqrt(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64 = sqrt(pICC->Argv[0].F64);
    return true;
}

void _My_Builtin_Cbrt(InternalCallbackContext* pICC) noexcept
{
    MY_ASSERT(pICC->Argc == 1, "Invalid argument count");

    pICC->Out->Kind = MyValueKind::Float64;
    pICC->Out->F64 = cbrt(pICC->Argv[0].F64);
    return true;
}
#endif // 0

int64_t my_pow(int64_t base, int64_t exponent) noexcept
{
    // Maybe a better implementation *?
    return int64_t(pow(double(base), double(exponent)));
}
