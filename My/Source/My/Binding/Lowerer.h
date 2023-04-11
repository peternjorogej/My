#pragma once

#include "BoundTree.h"

class Lowerer
{
public:
	static BoundStatement* Lower(MySymbol* pFunction, BoundStatement* pStatement) noexcept;

private:
	Lowerer() = default;
};

class ConstantFolding
{
public:
	static BoundExpression* Evaluate(BoundExpression* const pExpression) noexcept;

private:
	ConstantFolding() = default;
};
