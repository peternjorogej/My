#pragma once

#include "My/My.h"

// Core Operations
void _My_Builtin_Equals(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_CvToInt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_CvToUint(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_CvToFloat(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_CvIntToString(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_CvUintToString(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_CvFloatToString(MyContext* pContext, MyVM* pVM) noexcept;

// Std Operations
void _My_Builtin_Write(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_WriteLine(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Read(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_ReadLine(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_ReadInt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_ReadUint(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_ReadFloat(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_RandomInt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_RandomUint(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_RandomFloat(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Print(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Length(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Clock(MyContext* pContext, MyVM* pVM) noexcept;

// Math
void _My_Builtin_Sin(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Cos(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Tan(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Asin(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Acos(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Atan(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Atan2(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Log(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Log10(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Logb(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Exp(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Floor(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Ceil(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Sqrt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Cbrt(MyContext* pContext, MyVM* pVM) noexcept;

// String
void _My_Builtin_Strcat(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Strcmp(MyContext* pContext, MyVM* pVM) noexcept;

// StringBuilder
void _My_Builtin_StringBuilder_Init(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_Indent(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_Dedent(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_Write(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_WriteLine(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_ToString(MyContext* pContext, MyVM* pVM);


#if 0
void _My_Builtin_Strcat(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Strcmp(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Equals(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_CvToBool(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_CvToInt(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_CvToUint(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_CvToFloat(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_CvToString(InternalCallbackContext* pCallbackContext) noexcept;

void _My_Builtin_Write(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_WriteLine(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Read(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_ReadLine(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_ReadInt(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_ReadUint(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_ReadFloat(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_RandomInt(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_RandomUint(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_RandomFloat(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Print(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Length(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Clock(InternalCallbackContext* pCallbackContext) noexcept;

void _My_Builtin_Sin(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Cos(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Tan(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Asin(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Acos(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Atan(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Atan2(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Log(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Log10(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Logb(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Exp(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Floor(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Ceil(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Sqrt(InternalCallbackContext* pCallbackContext) noexcept;
void _My_Builtin_Cbrt(InternalCallbackContext* pCallbackContext) noexcept;
#endif // 0

int64_t my_pow(int64_t base, int64_t exponent) noexcept;
