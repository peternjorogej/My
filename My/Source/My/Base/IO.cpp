#include "IO.h"
#include "My/Utils/Utils.h"

#include <iostream>
#include <filesystem>

#pragma region Console
#define _My_IO_Begin_Vprintf(x)     va_list vArgs; va_start(vArgs, x);
#define _My_IO_End_Vprintf()        va_end(vArgs);

void Console::Write(const char* lpFmt, ...) noexcept
{
	_My_IO_Begin_Vprintf(lpFmt);
	int iResult = vprintf(lpFmt, vArgs); (void)iResult;
	_My_IO_End_Vprintf();
}

void Console::WriteLine(const char* lpFmt, ...) noexcept
{
	if (strlen(lpFmt) > 0ul)
	{
		_My_IO_Begin_Vprintf(lpFmt);
		int iResult = vprintf(lpFmt, vArgs); (void)iResult;
		_My_IO_End_Vprintf();
	}
	printf("\n");
}

void Console::Write(const std::string& Fmt, ...) noexcept
{
	_My_IO_Begin_Vprintf(&Fmt);
	int iResult = vprintf_s(Fmt.c_str(), vArgs); (void)iResult;
	_My_IO_End_Vprintf();
}

void Console::WriteLine(const std::string& Fmt, ...) noexcept
{
	if (Fmt.length())
	{
		_My_IO_Begin_Vprintf(&Fmt);
		int iResult = vprintf_s(Fmt.c_str(), vArgs); (void)iResult;
		_My_IO_End_Vprintf();
	}
	printf("\n");	
}

void Console::Write(const std::string_view& Fmt, ...) noexcept
{
	static char lpFormat[2048] = { 0 };
	{
		memset(lpFormat, 0, sizeof(lpFormat));
		strncpy(lpFormat, Fmt.data(), Fmt.length());
		lpFormat[Fmt.length()] = 0;
	}

	_My_IO_Begin_Vprintf(&Fmt);
	int iResult = vprintf_s(lpFormat, vArgs); (void)iResult;
	_My_IO_End_Vprintf();
}

void Console::WriteLine(const std::string_view& Fmt, ...) noexcept
{
	if (Fmt.length())
	{
		static char lpFormat[2048] = { 0 };
		{
			memset(lpFormat, 0, sizeof(lpFormat));
			strncpy(lpFormat, Fmt.data(), Fmt.length());
			lpFormat[Fmt.length()] = 0;
		}

		_My_IO_Begin_Vprintf(&Fmt);
		int iResult = vprintf_s(lpFormat, vArgs); (void)iResult;
		_My_IO_End_Vprintf();
	}
	printf("\n");	
}

#undef _My_IO_Begin_Vprintf
#undef _My_IO_End_Vprintf

void Console::WriteLine() noexcept
{
	printf("\n");
}

int64_t Console::ReadInt64() noexcept
{
	int64_t iValue = 0ll;
	return In() >> iValue, iValue;
}

uint64_t Console::ReadUint64() noexcept
{
	uint64_t kValue = 0ull;
	return In() >> kValue, kValue;
}

double Console::ReadFloat64() noexcept
{
	double dValue = 0.0;
	return In() >> dValue, dValue;
}

std::string Console::ReadLine() noexcept
{
	std::string Input;
	std::getline(In(), Input);
	return Input;
}

char Console::ReadKey() noexcept
{
	return In().get();
}

void Console::Clear() noexcept
{
	system("cls");
}

std::string Console::FormatV(const char* lpFormat, ...) noexcept
{
	constexpr uint64_t kBufferSize = 2048ull;

	static char lpBuffer[kBufferSize] = { 0 };

	va_list vArgs;
	va_start(vArgs, lpFormat);
	memset(lpBuffer, 0, sizeof(lpBuffer));
	int iResult = vsnprintf(lpBuffer, kBufferSize, lpFormat, vArgs); (void)iResult;
	va_end(vArgs);

	return std::string(lpBuffer);
}

void Console::SetColor(Console::Color Color) noexcept
{
#ifdef MY_WIN32
	/*
	enum class Console::Color
	    White (Std) -  7
	    Red         - 12
	    Green       - 10
	    Blue        -  9
	    Yellow      - 14
	    Magenta     - 13
	    Cyan        - 11
	    DarkRed     -  4
	    DarkGreen   -  2
	    DarkBlue    -  1
	    DarkYellow  -  6
	    DarkMagenta -  5
	    DarkCyan    -  3
	    LightGray   -  8
	    White       - 15
	*/
	static constexpr const WORD g_ConsoleColorMap[] = {  7, 12, 10,  9, 14, 13, 11,  4,  2,  1, 6, 5, 3, 8, 15 };
	SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), g_ConsoleColorMap[int(Color)]);
#elif defined(MY_LINUX)
	/*
	enum class Console::Color
	    White (Std) - 0?
	    Red         - 0?
	    Green       - 0?
	    Blue        - 0?
	    Yellow      - 0?
	    Magenta     - 0?
	    Cyan        - 0?
	    DarkRed     - 0?
	    DarkGreen   - 0?
	    DarkBlue    - 0?
	    DarkYellow  - 0?
	    DarkMagenta - 0?
	    DarkCyan    - 0?
	    LightGray   - 0?
	    White       - 0?
	*/
    #error "TODO: Include Linux console coloring support"
#else
	#error "Unsupported Platform"
#endif // MY_WIN32
}

void Console::ResetColor() noexcept
{
#ifdef MY_WIN32
	SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
#elif defined(MY_LINUX)
	#error "TODO: Include Linux console coloring support"
#else
	#error "Unsupported Platform"
#endif // MY_WIN32
}

std::ostream& Console::Out() noexcept
{
	return std::cout;
}

std::istream& Console::In() noexcept
{
	return std::cin;
}
#pragma endregion

#pragma region Debug_Log
void DebugLog::Error(const char* lpMessage) noexcept
{
	const std::string_view svMessage = lpMessage;
	Error(svMessage);
}

void DebugLog::Error(const std::string_view& Message) noexcept
{
#ifdef MY_DEBUG
	Console::SetColor(Console::Color::Red);
	Console::WriteLine(Message);
	Console::ResetColor();
#endif // MY_DEBUG
}

void DebugLog::Warn(const char* lpMessage) noexcept
{
	const std::string_view svMessage = lpMessage;
	Warn(svMessage);
}

void DebugLog::Warn(const std::string_view& Message) noexcept
{
#ifdef MY_DEBUG
	Console::SetColor(Console::Color::Yellow);
	Console::WriteLine(Message);
	Console::ResetColor();
#endif // MY_DEBUG
}

void DebugLog::Okay(const char* lpMessage) noexcept
{
	const std::string_view svMessage = lpMessage;
	Okay(svMessage);
}

void DebugLog::Okay(const std::string_view& Message) noexcept
{
#ifdef MY_DEBUG
	Console::SetColor(Console::Color::Green);
	Console::WriteLine(Message);
	Console::ResetColor();
#endif // MY_DEBUG
}

void DebugLog::Info(const char* lpMessage) noexcept
{
	const std::string_view svMessage = lpMessage;
	Info(svMessage);
}

void DebugLog::Info(const std::string_view& Message) noexcept
{
#ifdef MY_DEBUG
	Console::SetColor(Console::Color::LightGray);
	Console::WriteLine(Message);
	Console::ResetColor();
#endif // MY_DEBUG
}
#pragma endregion

#pragma region File
static inline uint8_t operator|(File::Mode Lhs, File::Mode Rhs) noexcept
{
	return uint8_t(Lhs) | uint8_t(Rhs);
}

static inline uint8_t operator&(File::Mode Lhs, File::Mode Rhs) noexcept
{
	return uint8_t(Lhs) & uint8_t(Rhs);
}

File::File(const std::string_view& Path, Mode OpenMode)
	: m_Mode(OpenMode), m_Open(false), m_Handle(), m_Path(), m_Size(0ull)
{
	if (!std::filesystem::exists(Path))
	{
		MY_ASSERT(false, "File '%.*s' does not exist", (int)Path.length(), Path.data());
		return;
	}

	m_Path = MyGetCachedString(Path);

	char lpMode[3] = { 'r', 't', '\0' };
	if (m_Mode & Mode::Write)  { lpMode[0] = 'w'; }
	if (m_Mode & Mode::Binary) { lpMode[1] = 'b'; }

	m_Handle = fopen(m_Path, lpMode);
	m_Open = true;

	fseek(m_Handle, 0, SEEK_END);
	m_Size = ftell(m_Handle);
	fseek(m_Handle, 0, SEEK_SET);
}

File::File(const std::string& Path, Mode OpenMode)
	: File(std::string_view(Path), OpenMode)
{ }

File::File(const char* lpPath, Mode OpenMode)
	: File(std::string_view(lpPath), OpenMode)
{ }

File::File(File&& Rhs) noexcept
	: m_Mode(), m_Open(), m_Handle(), m_Path(), m_Size()
{
	this->operator=(std::forward<File>(Rhs));
}

File& File::operator=(File&& Rhs) noexcept
{
	m_Mode     = Rhs.m_Mode;     Rhs.m_Mode     = Mode(0);
	m_Open     = Rhs.m_Open;     Rhs.m_Open     = false;
	m_Handle   = Rhs.m_Handle;   Rhs.m_Handle   = nullptr;
	m_Path     = Rhs.m_Path;     Rhs.m_Path     = {};
	m_Size     = Rhs.m_Size;     Rhs.m_Size     = 0ul;
	m_Position = Rhs.m_Position; Rhs.m_Position = 0ul;

	return *this;
}

File::~File()
{
	Close();
}

char* File::Read(size_t kSize) noexcept
{
	if (kSize == 0ul)
	{
		// Read everything
		kSize = m_Size;
	}

	char* pBuffer = nullptr;
	bool bReadable = (m_Position + kSize) <= kSize;

	if (bReadable && m_Mode & Mode::Read)
	{
		if (pBuffer = new char[kSize + 1]{})
		{
			(void)fread(pBuffer, sizeof(char), kSize, m_Handle);
			m_Position += kSize;

			if (m_Mode & Mode::Text)
			{
				// Null termination character if we're dealing with text
				pBuffer[kSize] = '\0';
			}
		}
	}

	return pBuffer;
}

char* File::ReadAll(const char* lpPath) noexcept
{
	return ReadAll(std::string_view(lpPath));
}

char* File::ReadAll(const std::string& Path) noexcept
{
	return ReadAll(std::string_view(Path));
}

char* File::ReadAll(const std::string_view& Path) noexcept
{
	File f = File(Path);
	return f.Read();
}

void File::Write(const char* pBuffer, size_t kSize) noexcept
{
	if (m_Mode & Mode::Write)
	{
		fwrite(pBuffer, sizeof(char), kSize, m_Handle);
		m_Position += kSize;
	}
}

size_t File::Size() const noexcept
{
	return m_Size;
}

size_t File::Position() const noexcept
{
	return m_Position;
}

const char* File::Path() const noexcept
{
	return m_Path;
}

void* File::Handle() const noexcept
{
	return (void*)m_Handle;
}

bool File::IsOpen() const noexcept
{
	return m_Open;
}

void File::Close() noexcept
{
	if (m_Open)
	{
		fclose(m_Handle);
		m_Handle = nullptr;

		m_Open = false;
		m_Position = 0ull;
	}
}
#pragma endregion

