#pragma once

#include "Config.h"

#include <stdarg.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <string.h>

#include <string>
#include <tuple>

#ifdef MY_MSVC
  #pragma warning(disable: 4267)
  #define MY_UNUSED     // __declspec(unused) // TODO: Search MSVC's version
#elif defined(MY_GCC)
  #define MY_UNUSED     __attribute__((unused))
#endif // MY_MSVC

#ifdef MY_WIN32
  #define NOMINMAX
  #define WIN32_LEAN_AND_MEAN
  #include <Windows.h>
#elif defined(MY_LINUX)
  #error "TODO: Include Linux specific headers"
#else
  #error "Unsupported Platform"
#endif // MY_WIN32

#ifdef MY_DEBUG
  #define MY_ASSERT(x, ...) ::MyAssert(__FILE__, __LINE__, x, __VA_ARGS__)
#else
  #define MY_ASSERT(x, ...) (void)0
#endif // NDEGUG

#define MY_NOT_IMPLEMENTED() MY_ASSERT(false, "NotImplemented")

#define MY_SAFEDELETE(ptr) delete ptr, ptr = nullptr

#define MY_VERSION "0.0.1"


// Forward Declarations
struct MyInstruction;

struct MyString;
struct MyArray;
struct MyStruct;
struct MyObject;
struct MyValue;

struct MyType;

struct MyAssembly;
struct MyVM;
struct MyContext;

/// <summary>
/// Used by macros defined in UniStbDs.h (previously stb_ds.h)
/// </summary>
/// <typeparam name="K">Key Type</typeparam>
/// <typeparam name="V">Value Type</typeparam>
template<typename K, typename V>
struct Pair
{
	K key   = K{};
	V value = V{};
};


using Byte = uint8_t;

template<typename Tp>
using List  = std::initializer_list<Tp>;

template<typename... TArgs>
using Tuple = std::tuple<TArgs...>;


void     MyAssert(const char* lpFile, int iLine, bool bCondition, const char* lpFmt = "", ...) noexcept;
uint64_t MyHashBytes(const void* pBytes, uint64_t kSize, uint64_t kSeed = 0ull) noexcept;
