#pragma once

#define _CRT_SECURE_NO_WARNINGS

#ifdef _MSC_VER
  #define MY_MSVC
#elif defined(__GNUC__)
  #define MY_GCC
#else
  #error "Unsupported Compiler"
#endif // _MSC_VER

// Switches for choosing...

/// Which hashing algorithm to use. If non-zero, then we use an internal algorithm, otherwise
/// the compilers standard algorithm is used
#define MY_USE_INTERNAL_HASH   0


#ifdef MY_WIN32
  #define MY_SEPARATOR '\\'
#else
  #define MY_SEPARATOR '/'
#endif // MY_WIN32


