#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

typedef struct
{
    char* Buffer;
    char* Position;
    int   Length;
    int   Capacity;
    int   Indent;
} StringBuilder;

StringBuilder SbCreate(int iInitialCapacity);
void          SbDestroy(StringBuilder* pSb);
void          SbAppendChar(StringBuilder* pSb, char Character); // Does not indent
void          SbAppend(StringBuilder* pSb, const char* lpText); // Does not indent
void          SbAppendV(StringBuilder* pSb, const char* lpFormat, ...); // Does not indent
void          SbWriteChar(StringBuilder* pSb, char Character);
void          SbWrite(StringBuilder* pSb, const char* lpText);
void          SbWriteV(StringBuilder* pSb, const char* lpFormat, ...);
void          SbWriteLine(StringBuilder* pSb, const char* lpText);
void          SbWriteLineV(StringBuilder* pSb, const char* lpFormat, ...);
const char*   SbGetString(const StringBuilder* pSb);
char*         SbToString(StringBuilder* pSb);
void          SbClear(StringBuilder* pSb);

#ifdef __cplusplus
}
#endif // __cplusplus

#ifdef CUTILS_STRING_BUILBER_IMPL

#ifdef _MSC_VER
  #ifndef _CRT_SECURE_NO_WARNINGS
    #define _CRT_SECURE_NO_WARNINGS
  #endif // !_CRT_SECURE_NO_WARNINGS
#endif // _MSC_VER

static void _SbCheckResizeBuffer(StringBuilder* pSb, int iCapacity)
{
    const int iAvailCapacity = pSb->Capacity - pSb->Length;
    if (iAvailCapacity < iCapacity)
    {
        const int iNewCapacity = pSb->Capacity + (pSb->Capacity / 2);
        char* pBuffer = (char*)realloc(pSb->Buffer, iNewCapacity);

        if (pBuffer)
        {
            pSb->Buffer   = pBuffer;
            pSb->Position = pSb->Buffer + pSb->Length;
            pSb->Capacity = iNewCapacity;
            memset(pSb->Position, 0, pSb->Capacity - pSb->Length);
        }
    }
}

static void _SbIndent(StringBuilder* pSb)
{
    static const int iGapSize = 4;
    _SbCheckResizeBuffer(pSb, iGapSize * pSb->Indent);

    for (size_t k = 0; k < pSb->Indent; k++)
    {
        char* pBuffer = (char*)memcpy(pSb->Position, "    ", iGapSize);
        if (pBuffer)
        {
            pSb->Position = pBuffer;
            pSb->Position += iGapSize;
            pSb->Length += iGapSize;
        }
    }
}

StringBuilder SbCreate(int iInitialCapacity)
{
    StringBuilder sb = {};
    memset(&sb, 0, sizeof(StringBuilder));

    if (iInitialCapacity < 1)
    {
        iInitialCapacity = 64;
    }

    char* pBuffer = (char*)malloc(iInitialCapacity);
    if (pBuffer)
    {
        memset(pBuffer, 0, iInitialCapacity);
        sb.Buffer = pBuffer;
        sb.Position = sb.Buffer;
        sb.Length = 0;
        sb.Capacity = iInitialCapacity;
    }

    return sb;
}

void SbDestroy(StringBuilder* pSb)
{
    assert(pSb && pSb->Buffer);
    SbClear(pSb);
    free(pSb->Buffer);
    pSb->Buffer   = NULL;
}

void SbAppendChar(StringBuilder* pSb, char Character)
{
    assert(pSb && pSb->Position);

    _SbCheckResizeBuffer(pSb, 1);

    if ((*pSb->Position++ = Character))
    {
        pSb->Length++;
    }
}

void SbAppend(StringBuilder* pSb, const char* lpText)
{
    assert(pSb && pSb->Position && lpText);

    const int iTextLength = (int)strlen(lpText);
    _SbCheckResizeBuffer(pSb, iTextLength);

    char* pBuffer = (char*)memcpy(pSb->Position, lpText, iTextLength);
    if (pBuffer)
    {
        pSb->Position = pBuffer;
        pSb->Position += iTextLength;
        pSb->Length += iTextLength;
    }
}

void SbAppendV(StringBuilder* pSb, const char* lpFormat, ...)
{
    assert(pSb && pSb->Position && lpFormat);
    static char lpBuffer[2048] = {};

    va_list vArgs;
    va_start(vArgs, lpFormat);
    vsnprintf_s(lpBuffer, 2048ull, 2048ull, lpFormat, vArgs);
    va_end(vArgs);

    SbAppend(pSb, lpBuffer);
}

void SbWriteChar(StringBuilder* pSb, char Character)
{
    assert(pSb && pSb->Position);

    _SbIndent(pSb);
    _SbCheckResizeBuffer(pSb, 1);

    if ((*pSb->Position++ = Character))
    {
        pSb->Length++;
    }
}

void SbWrite(StringBuilder* pSb, const char* lpText)
{
    assert(pSb && pSb->Position && lpText);

    _SbIndent(pSb);
    const int iTextLength = (int)strlen(lpText);
    _SbCheckResizeBuffer(pSb, iTextLength);

    char* pBuffer = (char*)memcpy(pSb->Position, lpText, iTextLength);
    if (pBuffer)
    {
        pSb->Position = pBuffer;
        pSb->Position += iTextLength;
        pSb->Length += iTextLength;
    }
}

void SbWriteV(StringBuilder* pSb, const char* lpFormat, ...)
{
    assert(pSb && pSb->Position && lpFormat);
    static char lpBuffer[2048] = {};

    va_list vArgs;
    va_start(vArgs, lpFormat);
    vsnprintf_s(lpBuffer, 2048ull, 2048ull, lpFormat, vArgs);
    va_end(vArgs);

    SbWrite(pSb, lpBuffer);
}

void SbWriteLine(StringBuilder* pSb, const char* lpText)
{
    SbWriteV(pSb, "%s\n", lpText);
}

void SbWriteLineV(StringBuilder* pSb, const char* lpFormat, ...)
{
    assert(pSb && pSb->Position && lpFormat);
    static char lpBuffer[2048] = {};

    va_list vArgs;
    va_start(vArgs, lpFormat);
    vsnprintf_s(lpBuffer, 2048ull, 2048ull, lpFormat, vArgs);
    va_end(vArgs);

    SbWriteLine(pSb, lpBuffer);
}

const char* SbGetString(const StringBuilder* pSb)
{
    return pSb ? pSb->Buffer : NULL;
}

char* SbToString(StringBuilder* pSb)
{
    char* const lpStringCopy = pSb ? _strdup(SbGetString(pSb)) : NULL;
    return lpStringCopy;
}

void SbClear(StringBuilder* pSb)
{
    assert(pSb);
    if (pSb->Buffer)
    {
        memset(pSb->Buffer, 0, pSb->Capacity);
        pSb->Position = pSb->Buffer;
        pSb->Length = 0;
        pSb->Indent = 0;
    }
}
#endif // CUTILS_STRING_BUILBER_IMPL

