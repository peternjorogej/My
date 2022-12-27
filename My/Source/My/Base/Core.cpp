#include "Core.h"
#include "IO.h"
#include "My/Object.h"

static [[nodiscard]] uint64_t _My_InternalHash(const void* pBytes, uint64_t kSize, uint64_t kSeed) noexcept;

void MyAssert(const char* lpFile, int iLine, bool bCondition, const char* lpFormat, ...) noexcept
{
    if (bCondition)
    { }
    else
    {
        Console::SetColor(Console::Color::Red);
        Console::WriteLine("Assertion Failed");
        Console::WriteLine("    [Line %d, File: %s]", iLine, lpFile);
        if (strlen(lpFormat) > 0ull)
        {
            Console::Write("    ");
            
            va_list vArgs;
            va_start(vArgs, lpFormat);
            int iResult = vprintf(lpFormat, vArgs); (void)iResult;
            va_end(vArgs);
            
            Console::WriteLine();
        }
        Console::ResetColor();
#ifdef MY_DEBUG
        __debugbreak();
#else
        exit(-1);
#endif // MY_DEBUG
    }
}

uint64_t MyHashBytes(const void* pBytes, uint64_t kSize, uint64_t kSeed) noexcept
{
#if MY_USE_INTERNAL_HASH
    return _My_InternalHash(pBytes, kSize, kSeed);
#else
  #ifdef MY_MSVC
    return std::_Hash_array_representation(reinterpret_cast<const uint8_t*>(pBytes), kSize);
  #elif defined(MY_GCC)
    return std::_Hash_bytes(pBytes, kSize, kSeed);
  #else
    #error "Unsupported Compiler"
    return uint64_t();
  #endif // MY_MSVC    
#endif // MY_USE_INTERNAL_HASH
}

uint64_t _My_InternalHash(const void* pBytes, uint64_t kSize, uint64_t kSeed) noexcept
{
    MY_NOT_IMPLEMENTED();
    return uint64_t();
}
