#pragma once

#include "Core.h"

// A C# like console I/O API
class Console
{
public:
	enum class Color
	{
		White,
		Red,
		Green,
		Blue,
		Yellow,
		Magenta,
		Cyan,
		DarkRed,
		DarkGreen,
		DarkBlue,
		DarkYellow,
		DarkMagenta,
		DarkCyan,
		LightGray,
	};

public:
	// Standard Functions
	static void          Write(const char* lpFmt, ...) noexcept;
	static void          WriteLine() noexcept;
	static void          WriteLine(const char* lpFmt, ...) noexcept;
	static void          Write(const std::string& Fmt, ...) noexcept;
	static void          WriteLine(const std::string& Fmt, ...) noexcept;
	static void          Write(const std::string_view& Fmt, ...) noexcept;
	static void          WriteLine(const std::string_view& Fmt, ...) noexcept;
	static int64_t       ReadInt64() noexcept;
	static uint64_t      ReadUint64() noexcept;
	static double        ReadFloat64() noexcept;
	static std::string   Read() noexcept;
	static std::string   ReadLine() noexcept;
	static char          ReadKey() noexcept;
	static void          Clear() noexcept;
	static std::string   FormatV(const char* lpFmt, ...) noexcept;
	static void          SetColor(Console::Color Color) noexcept;
	static void          ResetColor() noexcept;
	static std::ostream& Out() noexcept;
	static std::istream& In() noexcept;
	// Utility Functions
	template<typename... TArgs>
	static void Write(Console::Color Color, TArgs&&... Args) noexcept
	{
		SetColor(Color);
		Write(std::forward<TArgs>(Args)...);
		ResetColor();
	}
	
	template<typename... TArgs>
	static void WriteLine(Console::Color Color, TArgs&&... Args) noexcept
	{
		SetColor(Color);
		WriteLine(std::forward<TArgs>(Args)...);
		ResetColor();
	}

	template<typename... TArgs>
	static std::string Format(const std::string& Fmt, TArgs&&... Args) noexcept
	{
		if constexpr (sizeof...(TArgs) == 0ull)
		{
			return Fmt;
		}
		else
		{
			return FormatV(Fmt.c_str(), std::forward<TArgs>(Args)...);
		}
	}
};


class DebugLog
{
public:
	static void Error(const char* lpMessage) noexcept;
	static void Error(const std::string_view& Message) noexcept;
	template<typename... TArgs>
	static void Error(const std::string_view& Format, TArgs&&... Args) noexcept
	{
#ifdef MY_DEBUG
		Console::WriteLine(Console::Color::Red, Format, std::forward<TArgs>(Args)...);
#endif // MY_DEBUG
	}

	static void Warn(const char* lpMessage) noexcept;
	static void Warn(const std::string_view& Message) noexcept;
	template<typename... TArgs>
	static void Warn(const std::string_view& Format, TArgs&&... Args) noexcept
	{
#ifdef MY_DEBUG
		Console::WriteLine(Console::Color::Yellow, Format, std::forward<TArgs>(Args)...);
#endif // MY_DEBUG
	}

	static void Okay(const char* lpMessage) noexcept;
	static void Okay(const std::string_view& Message) noexcept;
	template<typename... TArgs>
	static void Okay(const std::string_view& Format, TArgs&&... Args) noexcept
	{
#ifdef MY_DEBUG
		Console::WriteLine(Console::Color::Green, Format, std::forward<TArgs>(Args)...);
#endif // MY_DEBUG
	}

	static void Info(const char* lpMessage) noexcept;
	static void Info(const std::string_view& Message) noexcept;
	template<typename... TArgs>
	static void Info(const std::string_view& Format, TArgs&&... Args) noexcept
	{
#ifdef MY_DEBUG
		Console::WriteLine(Console::Color::LightGray, Format, std::forward<TArgs>(Args)...);
#endif // MY_DEBUG
	}
};


class File
{
public:
	enum class Mode : uint8_t
	{
		Text      = 1,
		Binary    = 2,

		Read      = 4,
		Write     = 8,
		ReadWrite = 16,

		Default   = Read | Text,
	};

public:
	File(const char* lpPath, Mode OpenMode = Mode::Default);
	File(const std::string& Path, Mode OpenMode = Mode::Default);
	File(const std::string_view& Path, Mode OpenMode = Mode::Default);
	File(File&&) noexcept;
	File& operator=(File&&) noexcept;
	~File() noexcept;
	
	File(const File&) = delete;
	File& operator=(const File&) = delete;

	char* Read(size_t kSize = 0ul) noexcept;
	template<typename Tp>
	Tp& Read() noexcept
	{
		static_assert(std::is_standard_layout<Tp>::value);
		return *reinterpret_cast<Tp*>(Read(sizeof(Tp)));
	}
	void  Write(const char* pBuffer, size_t kSize) noexcept;
	template<typename Tp>
	void  Write(const Tp& Value) noexcept
	{
		static_assert(std::is_standard_layout<Tp>::value);
		Write(reinterpret_cast<const char*>(&Value), sizeof(Tp));
	}

	size_t      Size()     const noexcept;
	size_t      Position() const noexcept;
	const char* Path()     const noexcept;
	void*       Handle()   const noexcept;
	bool        IsOpen()   const noexcept;
	void        Close()    noexcept;

public:
	static char* ReadAll(const char* lpPath) noexcept;
	static char* ReadAll(const std::string& Path) noexcept;
	static char* ReadAll(const std::string_view& Path) noexcept;

private:
	Mode   m_Mode     = Mode(0);
	bool   m_Open     = false;
	FILE*  m_Handle   = nullptr;
	char*  m_Path     = nullptr;
	size_t m_Size     = 0ull;
	size_t m_Position = 0ull;
};

