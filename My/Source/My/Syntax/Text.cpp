#include "Text.h"

#include "Stb/stb_ds.h"
#include "My/Utils/Utils.h"

// Text Location
TextLocation::TextLocation(uint32_t kStart, uint32_t kLength, uint32_t kLine, const std::string_view& Filename)
    : Start(kStart), Length(kLength), Line(kLine), Filename(Filename)
{ }

uint32_t TextLocation::End() const noexcept
{
    return Start + Length;
}

TextLocation TextLocation::FromBounds(uint32_t kStart, uint32_t kEnd, uint32_t kLine, const std::string_view& Filename) noexcept
{
    return TextLocation(kStart, kEnd - kStart, kLine, Filename);
}

// Text Line
TextLine::TextLine(SourceText* const pText, uint32_t kStart, uint32_t kLength, uint32_t kLengthIncludingLineBreak)
    : Text(pText), Start(kStart), Length(kLength), LengthIncludingLineBreak(kLengthIncludingLineBreak)
{ }

uint32_t TextLine::End() const noexcept
{
    return Start + Length;
}

TextLocation TextLine::Location() const noexcept
{
    return TextLocation(Start, Length, 0u, Text->Filename);
}

TextLocation TextLine::LocationIncludingLineBreak() const noexcept
{
    return TextLocation(Start, Length, 0u, Text->Filename);
}

// Source Text
uint32_t SourceText::GetLineIndex(uint32_t kPosition) const noexcept
{
    uint32_t kLower = 0;
    uint32_t kUpper = (uint32_t)stbds_arrlenu(Lines) - 1;

    while (kLower <= kUpper)
    {
        uint32_t kIndex = kLower + (kUpper - kLower) / 2;
        uint32_t kStart = Lines[kIndex].Start;

        if (kPosition == kStart)
        {
            return kIndex;
        }
        if (kPosition < kStart)
        {
            kUpper = kIndex - 1;
        }
        else
        {
            kLower = kIndex + 1;
        }
    }

    return kLower - 1;
}

size_t SourceText::Length() const noexcept
{
    return Text.length();
}

std::string_view SourceText::ToString(uint32_t kStart, uint32_t kCount) const noexcept
{
    return Text.substr(kStart, kCount);
}

std::string_view SourceText::ToString(const TextLocation& Location) const noexcept
{
    return Text.substr(Location.Start, Location.Length);
}

const char& SourceText::operator[](size_t Index) const noexcept
{
    MY_ASSERT(Index < Text.length(), "Index '%I64u' out of bounds (0, %I64u)", Text.length()-1);
    return Text.at(Index);
}

SourceText SourceText::From(const std::string_view& Text, const std::string_view& Filename) noexcept
{
    return SourceText(Text, Filename);
}

SourceText::SourceText(const std::string_view& svText, const std::string_view& Filename)
    : Text(svText), Filename(Filename)
{
    Lines = ParseLines(this, Text);
}

TextLine* SourceText::ParseLines(SourceText* const& pText, const std::string_view& Text)
{
    TextLine* pLines = nullptr;

    uint32_t kTextLength = (uint32_t)Text.length();
    uint32_t kPosition   = 0u;
    uint32_t kLineStart  = 0u;

    while (kPosition < kTextLength)
    {
        uint32_t kLinebreakWidth = GetLineBreakWidth(Text, kPosition);

        if (!kLinebreakWidth)
        {
            kPosition++;
        }
        else
        {
            AddLine(&pLines, pText, kPosition, kLineStart, kLinebreakWidth);

            kPosition += kLinebreakWidth;
            kLineStart = kPosition;
        }
    }

    if (kPosition >= kLineStart)
    {
        AddLine(&pLines, pText, kPosition, kLineStart, 0u);
    }

    return pLines;
}

uint32_t SourceText::GetLineBreakWidth(const std::string_view& Text, uint32_t kPosition) noexcept
{
    const char curr = Text[kPosition];
    const char next = (kPosition + 1) >= (uint32_t)Text.length() ? '\0' : Text[(size_t)kPosition + 1u];

    if (curr == '\r' && next == '\n')
    {
        return 2u;
    }
    if (curr == '\r' || curr == '\n')
    {
        return 1u;
    }

    return 0u;
}

void SourceText::AddLine(TextLine** ppLines, const SourceText* pText, uint32_t kPosition, uint32_t kLineStart, uint32_t kLinebreakWidth) noexcept
{
    const uint32_t kLineLength = kPosition - kLineStart;
    const TextLine Line = TextLine(const_cast<SourceText*>(pText), kLineStart, kLineLength, kLineLength + kLinebreakWidth);
    stbds_arrpush(*ppLines, Line);
}


