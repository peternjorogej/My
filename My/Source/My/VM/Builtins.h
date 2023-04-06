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
void _My_Builtin_RandomInt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_RandomUint(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_RandomFloat(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Length(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Clock(MyContext* pContext, MyVM* pVM) noexcept;

void _My_Builtin_HeapAlloc(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_HeapCopy(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_HeapResize(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_HeapFree(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_Length(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_Capacity(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_WriteI32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_WriteI64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_WriteU32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_WriteU64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_WriteF32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_WriteF64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_WriteString(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_Append(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_ReadI32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_ReadI64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_ReadU32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_ReadU64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_ReadF32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_ReadF64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_ReadString(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Buffer_Get(MyContext* pContext, MyVM* pVM) noexcept;

// String
void _My_Builtin_String_Concat(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_Compare(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_Length(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_Find(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_Substr(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_Split(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_StartsWith(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_EndsWith(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_ToUpper(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_ToLower(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_ParseInt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_ParseUint(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_String_ParseFloat(MyContext* pContext, MyVM* pVM) noexcept;

// StringBuilder
void _My_Builtin_StringBuilder_Init(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_Append(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_AppendV(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_Write(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_WriteV(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_WriteLine(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_WriteLineV(MyContext* pContext, MyVM* pVM);
void _My_Builtin_StringBuilder_ToString(MyContext* pContext, MyVM* pVM);

// Bytes
// NOTE: WriteX32, ReadX32 - Hoping for 32-bit types?
void _My_Builtin_Bytes_Init(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_Free(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_AddI32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_AddI64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_AddU32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_AddU64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_AddF32(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_AddF64(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_AddString(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_Append(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Bytes_GetBufferPointer(MyContext* pContext, MyVM* pVM) noexcept;

// File
void _My_Builtin_File_Open(MyContext* pContext, MyVM* pVM);
void _My_Builtin_File_Close(MyContext* pContext, MyVM* pVM);
void _My_Builtin_File_IsOpen(MyContext* pContext, MyVM* pVM);
void _My_Builtin_File_Read(MyContext* pContext, MyVM* pVM);
void _My_Builtin_File_ReadBytes(MyContext* pContext, MyVM* pVM);
void _My_Builtin_File_Write(MyContext* pContext, MyVM* pVM);
void _My_Builtin_File_WriteBytes(MyContext* pContext, MyVM* pVM);

// (static) Console
void _My_Builtin_Console_Print(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Console_Write(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Console_WriteLine(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Console_ReadLine(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Console_ReadInt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Console_ReadUint(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Console_ReadFloat(MyContext* pContext, MyVM* pVM) noexcept;

// (static) Math
void _My_Builtin_Math_Abs(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Sin(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Cos(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Tan(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Asin(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Acos(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Atan(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Atan2(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Log(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Log10(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Logb(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Exp(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Floor(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Ceil(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Sqrt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Cbrt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Nthrt(MyContext* pContext, MyVM* pVM) noexcept;
void _My_Builtin_Math_Pow(MyContext* pContext, MyVM* pVM) noexcept;

