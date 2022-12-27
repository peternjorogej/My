#pragma once

#include "My/Base/Core.h"

struct TextLocation
{
	uint32_t         Start    = 0u;
	uint32_t         Length   = 0u;
	uint32_t         Line     = 0u;
	std::string_view Filename = {};

	TextLocation(uint32_t kStart, uint32_t kLength, uint32_t kiLine, const std::string_view& Filename);

	uint32_t End() const noexcept;

	static TextLocation FromBounds(uint32_t kStart, uint32_t kEnd, uint32_t kLine, const std::string_view& Filename) noexcept;
};

struct TextLine
{
	struct SourceText* Text   = nullptr;
	uint32_t           Start  = 0u;
	uint32_t           Length = 0u;
	uint32_t           LengthIncludingLineBreak = 0u;

	TextLine(struct SourceText* const pText, uint32_t kStart, uint32_t kLength, uint32_t kLengthIncludingLineBreak);

	uint32_t     End() const noexcept;
	TextLocation Location() const noexcept;
	TextLocation LocationIncludingLineBreak() const noexcept;
};

struct SourceText
{
public:
	std::string_view Text     = {};
	std::string_view Filename = {};
	TextLine*  Lines    = nullptr;

	~SourceText() noexcept = default;

	uint32_t         GetLineIndex(uint32_t kPosition) const noexcept;
	size_t           Length() const noexcept;
	std::string_view ToString(uint32_t kStart, uint32_t kCount) const noexcept;
	std::string_view ToString(const TextLocation& Location) const noexcept;
	const char& operator[](size_t Index) const noexcept;

	static SourceText From(const std::string_view& Text, const std::string_view& Filename) noexcept;

private:
	SourceText(const std::string_view& Text, const std::string_view& Filename);

	static TextLine* ParseLines(SourceText* const& pText, const std::string_view& Text);
	static uint32_t  GetLineBreakWidth(const std::string_view& Text, uint32_t kPosition) noexcept;
	static void	     AddLine(TextLine** ppLines, const SourceText* pText, uint32_t kPosition, uint32_t kLineStart, uint32_t kLinebreakWidth) noexcept;
};
