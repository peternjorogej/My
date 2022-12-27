#pragma once

#include "BoundTree.h"

class Lowerer
{
public:
	static BoundStatement* Lower(MySymbol* pFunction, BoundStatement* pStatement) noexcept;

private:
	Lowerer() = default;
};
