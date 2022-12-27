#pragma once

#include <string.h>
#include <stdint.h>
#include <memory.h>

#include <string>
#include <vector>

#ifdef PNBS_HAVE_ASSERTS
  #include <assert.h>
#else
  #ifndef assert
    #define assert(x) if(!(x)) { printf("[AssertionFailureWarning(%s)]:\n%d: %s\n", #x, __LINE__, __FILE__); exit(-69); }
  #endif // assert
#endif // PNBS_HAVE_ASSERTS

#ifdef PNBS_IMPLEMENTATION
  #define PNBS_API static
#else
  #define PNBS_API extern
#endif // PNBS_IMPLEMENTATION

namespace PnBS
{

    template<typename Byte = char>
    using Buffer = std::vector<Byte>;

    template<typename... TArgs>
    void Assert(bool bExpression, const char* lpMessage, TArgs&&... Args) noexcept;

    template<typename T>
    struct Encode; // Define: void operator()(BufferWriter&, const T&)

    template<typename T>
    struct Decode;  // Define: void operator()(BufferReader&, T&)


    class BufferWriter
    {
    public:
        BufferWriter() = default;

        BufferWriter& operator<<(const char& Value);
        BufferWriter& operator<<(const wchar_t& Value);
        BufferWriter& operator<<(const bool& Value);
        BufferWriter& operator<<(const int8_t& Value);
        BufferWriter& operator<<(const int16_t& Value);
        BufferWriter& operator<<(const int32_t& Value);
        BufferWriter& operator<<(const int64_t& Value);
        BufferWriter& operator<<(const uint8_t& Value);
        BufferWriter& operator<<(const uint16_t& Value);
        BufferWriter& operator<<(const uint32_t& Value);
        BufferWriter& operator<<(const uint64_t& Value);
        BufferWriter& operator<<(const float& Value);
        BufferWriter& operator<<(const double& Value);
        BufferWriter& operator<<(const std::string& Value);

        template<typename T>
        BufferWriter& operator<<(const T& Value)
        {
            Encode<T>{}(*this, Value);
            return *this;
        }

        void Dump(const char* lpFilepath);

    private:
        template<typename T>
        friend struct Encode;

        Buffer<> Data = { };
    };


    class BufferReader
    {
    public:
        BufferReader(const char* lpFilepath);

        BufferReader& operator>>(char& out_Value);
        BufferReader& operator>>(wchar_t& out_Value);
        BufferReader& operator>>(bool& out_Value);
        BufferReader& operator>>(int8_t& out_Value);
        BufferReader& operator>>(int16_t& out_Value);
        BufferReader& operator>>(int32_t& out_Value);
        BufferReader& operator>>(int64_t& out_Value);
        BufferReader& operator>>(uint8_t& out_Value);
        BufferReader& operator>>(uint16_t& out_Value);
        BufferReader& operator>>(uint32_t& out_Value);
        BufferReader& operator>>(uint64_t& out_Value);
        BufferReader& operator>>(float& out_Value);
        BufferReader& operator>>(double& out_Value);
        BufferReader& operator>>(std::string& out_Value);

        template<typename T>
        BufferReader& operator>>(T& out_Value)
        {
            Decode<T>{}(*this, out_Value);
            return *this;
        }

    private:
        void Load(const char* lpFilepath);

    private:
        template<typename T>
        friend struct Decode;

        Buffer<> Data  = { };
        uint32_t Index = 0u;
    };

#ifdef PNBS_IMPLEMENTATION

#ifdef _MSC_VER
  #ifndef _CRT_SECURE_NO_WARNINGS
    #define _CRT_SECURE_NO_WARNINGS
  #endif // !_CRT_SECURE_NO_WARNINGS

  #pragma warning(disable: 4267)
#endif // _MSC_VER

    ///////////////////////////////////////////////////////////////////////
    /// BUFFER WRITER /////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////
    void BufferWriter::Dump(const char* lpFilepath)
    {
        FILE* pFile = fopen(lpFilepath, "wb");
        if (pFile)
        {
            fwrite(Data.data(), sizeof(char), Data.size(), pFile);
            fclose(pFile);
        }
    }

    #define DEFINE_TRIVIAL_ENCODE(__type) \
        BufferWriter& BufferWriter::operator<<(const __type& Value)     \
        {                                                               \
            constexpr size_t Stride = sizeof(__type);                   \
            union { char I8[Stride]; __type V; } __u; __u.V = Value;    \
            Data.insert(Data.end(), __u.I8, __u.I8 + Stride);           \
            return *this;                                               \
        }

    DEFINE_TRIVIAL_ENCODE(char)
    DEFINE_TRIVIAL_ENCODE(wchar_t)
    DEFINE_TRIVIAL_ENCODE(bool)
    DEFINE_TRIVIAL_ENCODE(int8_t)
    DEFINE_TRIVIAL_ENCODE(int16_t)
    DEFINE_TRIVIAL_ENCODE(int32_t)
    DEFINE_TRIVIAL_ENCODE(int64_t)
    DEFINE_TRIVIAL_ENCODE(uint8_t)
    DEFINE_TRIVIAL_ENCODE(uint16_t)
    DEFINE_TRIVIAL_ENCODE(uint32_t)
    DEFINE_TRIVIAL_ENCODE(uint64_t)
    DEFINE_TRIVIAL_ENCODE(float)
    DEFINE_TRIVIAL_ENCODE(double)

    BufferWriter& BufferWriter::operator<<(const std::string& Value)
    {
        (*this) << (uint32_t)Value.length();
        Data.insert(Data.end(), Value.begin(), Value.end());
        return *this;
    }

    ///////////////////////////////////////////////////////////////////////
    /// BUFFER READER /////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////
    BufferReader::BufferReader(const char* lpFilepath)
    {
        Load(lpFilepath);
    }

    void BufferReader::Load(const char* lpFilepath)
    {
        FILE* pFile = fopen(lpFilepath, "rb");
        if (pFile)
        {
            fseek(pFile, 0, SEEK_END);
            const size_t uLength = ftell(pFile);
            fseek(pFile, 0, SEEK_SET);

            Data.resize(uLength);
            fread(Data.data(), sizeof(char), uLength, pFile);

            fclose(pFile);
        }
    }

    #define DEFINE_TRIVIAL_DECODE(__type) \
        BufferReader& BufferReader::operator>>(__type& out) \
        {                                                   \
            constexpr size_t Stride = sizeof(__type);       \
            union { char I8[Stride]; __type V; } __u;       \
            memcpy(__u.I8, &Data[Index], Stride);           \
            Index += Stride;                                \
            out = __u.V;                                    \
            return *this;                                   \
        }

    DEFINE_TRIVIAL_DECODE(char)
    DEFINE_TRIVIAL_DECODE(wchar_t)
    DEFINE_TRIVIAL_DECODE(bool)
    DEFINE_TRIVIAL_DECODE(int8_t)
    DEFINE_TRIVIAL_DECODE(int16_t)
    DEFINE_TRIVIAL_DECODE(int32_t)
    DEFINE_TRIVIAL_DECODE(int64_t)
    DEFINE_TRIVIAL_DECODE(uint8_t)
    DEFINE_TRIVIAL_DECODE(uint16_t)
    DEFINE_TRIVIAL_DECODE(uint32_t)
    DEFINE_TRIVIAL_DECODE(uint64_t)
    DEFINE_TRIVIAL_DECODE(float)
    DEFINE_TRIVIAL_DECODE(double)

    BufferReader& BufferReader::operator>>(std::string& out_Result)
    {
        uint32_t kLength; (*this) >> kLength;
        const char* lpBegin = &Data[Index]; Index += kLength;
        const char* lpEnd   = &Data[Index];
        out_Result = std::string(lpBegin, lpEnd);
        return *this;
    }
#endif // PNBS_IMPLEMENTATION
}
