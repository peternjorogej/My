#include "Builtins.h"
#include "My/Base/IO.h"
#include "My/VM/VM.h"
#include "My/Utils/Utils.h"

// Core
void _My_Builtin_Strcat(MyContext* pContext, MyVM* pVM) noexcept
{
    MyString* const& pRhs = pVM->Stack.PopString();
    MyString* const& pLhs = pVM->Stack.PopString();

    const char* lpString = MyGetCachedStringV("%s%s", pLhs->Chars, pRhs->Chars);
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

int64_t my_pow(int64_t base, int64_t exponent) noexcept
{
    // Maybe a better implementation *?
    return int64_t(pow(double(base), double(exponent)));
}
