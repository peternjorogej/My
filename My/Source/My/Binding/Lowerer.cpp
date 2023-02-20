#include "Lowerer.h"
#include "My/Base/IO.h"
#include "Stb/stb_ds.h"

// NextUpperBoundVariableName(), NextStepVariableName() Used by regular for loops
static char* NextUpperBoundVariableName() noexcept
{
	static int32_t iNextUpperBound = 0;
	return MyGetCachedStringV("__UpperBound%d", iNextUpperBound++);
}

static char* NextStepVariableName() noexcept
{
	static int32_t iNextStep = 0;
	return MyGetCachedStringV("__Step%d", iNextStep++);
}

// NextIterableVariableName(), NextCounterVariableName(), NextLengthVariableName() Used by foreach loops
static char* NextIterableVariableName() noexcept
{
	static int32_t iNextIterable = 0;
	return MyGetCachedStringV("__Iterable%d", iNextIterable++);
}

static char* NextCounterVariableName() noexcept
{
	static int32_t iNextCounter = 0;
	return MyGetCachedStringV("__Counter%d", iNextCounter++);
}

static char* NextLengthVariableName() noexcept
{
	static int32_t iNextLength = 0;
	return MyGetCachedStringV("__Length%d", iNextLength++);
}

// General lables
static BoundLabel NextLabel() noexcept
{
	static int32_t iNextLabel = 0;
	return BoundLabel{ MyGetCachedStringV("__label_%d", iNextLabel++) };
}


/// INTERNAL BOUND TREE REWRITER
class InternalBoundTreeRewriter
{
public:
	virtual ~InternalBoundTreeRewriter() = default;

	virtual BoundExpression* RewriteExpression(BoundExpression* pExpression) noexcept
	{
		switch (pExpression->Kind)
		{
			case BoundExpressionKind::Empty:
				return RewriteEmptyExpression(pExpression);
			case BoundExpressionKind::Error:
				return RewriteErrorExpression(pExpression);
			case BoundExpressionKind::Literal:
				return RewriteLiteralExpression(pExpression);
			case BoundExpressionKind::Unary:
				return RewriteUnaryExpression(pExpression);
			case BoundExpressionKind::Binary:
				return RewriteBinaryExpression(pExpression);
			case BoundExpressionKind::Ternary:
				return RewriteTernaryExpression(pExpression);
			case BoundExpressionKind::Increment:
				return RewriteIncrementExpression(pExpression);
			case BoundExpressionKind::Name:
				return RewriteNameExpression(pExpression);
			case BoundExpressionKind::Assignment:
				return RewriteAssignmentExpression(pExpression);
			case BoundExpressionKind::OperatorNew:
				return RewriteOperatorNewExpression(pExpression);
			case BoundExpressionKind::Call:
				return RewriteCallExpression(pExpression);
			case BoundExpressionKind::Index:
				return RewriteIndexExpression(pExpression);
			case BoundExpressionKind::Field:
				return RewriteFieldExpression(pExpression);
			case BoundExpressionKind::Conversion:
				return RewriteConversionExpression(pExpression);
			case BoundExpressionKind::Array:
				return RewriteArrayExpression(pExpression);
			case BoundExpressionKind::Cast:
				return RewriteCastExpression(pExpression);
			case BoundExpressionKind::Instance:
				return RewriteInstanceExpression(pExpression);
			default:
				MY_ASSERT(false, "FatalError: Unexpected node %s", BoundExpressionKindString(pExpression->Kind));
				return nullptr;
		}
	}
	
	virtual BoundStatement* RewriteStatement(BoundStatement* pStatement) noexcept
	{
		switch (pStatement->Kind)
		{
			case BoundStatementKind::Return:
				return RewriteReturnStatement(pStatement);
			case BoundStatementKind::Expression:
				return RewriteExpressionStatement(pStatement);
			case BoundStatementKind::VariableDeclaration:
				return RewriteVariableDeclarationStatement(pStatement);
			case BoundStatementKind::DecomposeDeclaration:
				return RewriteDecomposeDeclarationStatement(pStatement);

			case BoundStatementKind::If:
				return RewriteIfStatement(pStatement);
			case BoundStatementKind::For:
				return RewriteForStatement(pStatement);
			case BoundStatementKind::Foreach:
				return RewriteForeachStatement(pStatement);
			case BoundStatementKind::While:
				return RewriteWhileStatement(pStatement);
			case BoundStatementKind::Block:
				return RewriteBlockStatement(pStatement);

			case BoundStatementKind::Label:
				return RewriteLabelStatement(pStatement);
			case BoundStatementKind::Goto:
				return RewriteGotoStatement(pStatement);
			case BoundStatementKind::ConditionalGoto:
				return RewriteConditionalGotoStatement(pStatement);

			default:
				MY_ASSERT(false, "FatalError: Unexpected node %s", BoundStatementKindString(pStatement->Kind));
				return nullptr;
		}
	}

protected:
	virtual BoundExpression* RewriteEmptyExpression(BoundExpression* pEmpty) noexcept
	{
		return pEmpty;
	}
	
	virtual BoundExpression* RewriteErrorExpression(BoundExpression* pError) noexcept
	{
		return pError;
	}

	virtual BoundExpression* RewriteLiteralExpression(BoundExpression* pLiteral) noexcept
	{
		return pLiteral;
	}
	
	virtual BoundExpression* RewriteNameExpression(BoundExpression* pName) noexcept
	{
		return pName;
	}
	
	virtual BoundExpression* RewriteUnaryExpression(BoundExpression* pUnary) noexcept
	{
		BoundUnaryExpression& unary = pUnary->unary;

		BoundExpression* pRhs = RewriteExpression(unary.Rhs);
		return pRhs == unary.Rhs ? pUnary : MakeBoundExpression_Unary(unary.Operator, pRhs);
	}

	virtual BoundExpression* RewriteBinaryExpression(BoundExpression* pBinary) noexcept
	{
		BoundBinaryExpression& binary = pBinary->binary;

		BoundExpression* pLhs = RewriteExpression(binary.Lhs);
		BoundExpression* pRhs = RewriteExpression(binary.Rhs);
		return pLhs == binary.Lhs && pRhs == binary.Rhs ? pBinary : MakeBoundExpression_Binary(pLhs, binary.Operator, pRhs);
	}

	virtual BoundExpression* RewriteTernaryExpression(BoundExpression* pTernary) noexcept
	{
		BoundTernaryExpression& ternary = pTernary->ternary;

		BoundExpression* pCondition = RewriteExpression(ternary.Condition);
		BoundExpression* pThen = RewriteExpression(ternary.Then);
		BoundExpression* pElse = RewriteExpression(ternary.Else);

		if (pCondition == ternary.Condition && pThen == ternary.Then && pElse == ternary.Else)
		{
			return pTernary;
		}

		return MakeBoundExpression_Ternary(pCondition, pThen, pElse);
	}

	virtual BoundExpression* RewriteIncrementExpression(BoundExpression* pIncrement) noexcept
	{
		BoundIncrementExpression& inc = pIncrement->inc;

		BoundExpression* pInc = RewriteExpression(inc.Increment);
		return pInc == inc.Increment ? pIncrement : MakeBoundExpression_Increment(inc.Lvalue, pInc);
	}

	virtual BoundExpression* RewriteAssignmentExpression(BoundExpression* pAssignment) noexcept
	{
		BoundAssignmentExpression& assignment = pAssignment->assign;

		BoundExpression* pLhs = RewriteExpression(assignment.Lhs);
		BoundExpression* pRhs = RewriteExpression(assignment.Rhs);

		if (pLhs == assignment.Lhs && pRhs == assignment.Rhs)
		{
			return pAssignment;
		}

		return MakeBoundExpression_Assignment(pLhs, assignment.Variable, pRhs);
	}

	virtual BoundExpression* RewriteOperatorNewExpression(BoundExpression* pOperatorNew) noexcept
	{
		BoundOperatorNewExpression& opnew = pOperatorNew->opnew;

		BONEFieldInitializer* pInitializers = nullptr;

		const size_t kLength = stbds_arrlenu(opnew.Initializers);
		for (size_t k = 0; k < kLength; k++)
		{
			BoundExpression* pOldExpression = opnew.Initializers[k].value;
			BoundExpression* pNewExpression = RewriteExpression(pOldExpression);
			if (pNewExpression != pOldExpression)
			{
				if (stbds_arrlenu(pInitializers) == 0ul)
				{
					stbds_arrsetlen(pInitializers, kLength);
					for (size_t j = 0; j < k; j++)
					{
						pInitializers[j] = opnew.Initializers[j];
					}
				}
			}
			if (stbds_arrlenu(pInitializers) != 0ul)
			{
				pInitializers[k].value = pNewExpression;
			}
		}

		return stbds_arrlenu(pInitializers) == 0ul ? pOperatorNew : MakeBoundExpression_OperatorNew(opnew.Type, pInitializers);

		/*BoundExpression* pExpression = opnew.Expr ? RewriteExpression(opnew.Expr) : nullptr;
		return pExpression == opnew.Expr ? pOperatorNew : MakeBoundExpression_OperatorNew(opnew.Type, pExpression);*/
	}
	
	virtual BoundExpression* RewriteCallExpression(BoundExpression* pCall) noexcept
	{
		BoundCallExpression& call = pCall->call;

		auto[bRewritten, pCallable, ppArgs] = RewriteExpressionWithExpressionList(call.Callable, call.Arguments);
		return !bRewritten ? pCall : MakeBoundExpression_Call(pCallable, call.Function, ppArgs);
	}
	
	virtual BoundExpression* RewriteIndexExpression(BoundExpression* pIndexExpr) noexcept
	{
		BoundIndexExpression& index = pIndexExpr->index;

		auto [bRewritten, pSequence, ppIndices] = RewriteExpressionWithExpressionList(index.Sequence, index.Indices);
		return !bRewritten ? pIndexExpr : MakeBoundExpression_Index(pSequence, index.Type, ppIndices);
	}
	
	virtual BoundExpression* RewriteFieldExpression(BoundExpression* pField) noexcept
	{
		BoundFieldExpression& field = pField->field;

		BoundExpression* pObject = RewriteExpression(field.Object);
		return pObject == field.Object ? pField : MakeBoundExpression_Field(pObject, field.Type, field.Field);
	}
	
	virtual BoundExpression* RewriteCastExpression(BoundExpression* pCast) noexcept
	{
		BoundCastExpression& cast = pCast->cast;

		BoundExpression* pExpression = RewriteExpression(cast.Expr);
		return pExpression == cast.Expr ? pCast : MakeBoundExpression_Cast(cast.Type, pExpression);
	}
	
	virtual BoundExpression* RewriteConversionExpression(BoundExpression* pConversion) noexcept
	{
		BoundConversionExpression& conv = pConversion->conv;

		BoundExpression* pExpression = RewriteExpression(conv.Expr);
		return pExpression == conv.Expr ? pConversion : MakeBoundExpression_Conversion(conv.Type, pExpression);
	}
	
	virtual BoundExpression* RewriteArrayExpression(BoundExpression* pArray) noexcept
	{
		BoundArrayExpression& array = pArray->array;

		BoundExpression** ppItems = nullptr;

		const size_t kLength = stbds_arrlenu(array.Items);
		for (size_t k = 0; k < kLength; k++)
		{
			BoundExpression* pOldExpression = array.Items[k];
			BoundExpression* pNewExpression = RewriteExpression(pOldExpression);
			if (pNewExpression != pOldExpression)
			{
				if (stbds_arrlenu(ppItems) == 0ul)
				{
					stbds_arrsetlen(ppItems, kLength);
					for (size_t j = 0; j < k; j++)
					{
						ppItems[j] = array.Items[j];
					}
				}
			}
			if (stbds_arrlenu(ppItems) != 0ul)
			{
				ppItems[k] = pNewExpression;
			}
		}

		return stbds_arrlenu(ppItems) == 0ul ? pArray : MakeBoundExpression_Array(array.Type, ppItems);
	}
	
	virtual BoundExpression* RewriteInstanceExpression(BoundExpression* pInstance) noexcept
	{
		using MemberMap = BoundInstanceExpression::MemberMap;
		
		BoundInstanceExpression& inst = pInstance->inst;

		MemberMap* pMembers = { };
		for (size_t k = 0; k < stbds_hmlenu(inst.Members); k++)
		{
			MemberMap& mm = inst.Members[k];

			BoundExpression* pNewValue = RewriteExpression(mm.value);
			if (pNewValue != mm.value)
			{
				stbds_hmput(pMembers, mm.key, pNewValue);
			}
		}

		if (stbds_hmlenu(pMembers) == 0ul)
		{
			return pInstance;
		}

		for (size_t k = 0; k < stbds_hmlenu(inst.Members); k++)
		{
			MemberMap& mm = inst.Members[k];

			BoundExpression* pValue = stbds_hmget(pMembers, mm.key);
			if (pValue == nullptr)
			{
				stbds_hmput(pMembers, mm.key, pValue);
			}
		}

		return MakeBoundExpression_Instance(inst.Type, pMembers);
	}

	virtual BoundStatement* RewriteBlockStatement(BoundStatement* pBlock) noexcept
	{
		BoundBlockStatement& block = pBlock->block;

		BoundStatement** ppStmts = nullptr;

		const size_t kLength = stbds_arrlenu(block.Statements);
		for (size_t k = 0; k < kLength; k++)
		{
			BoundStatement* pOldStatement = block.Statements[k];
			BoundStatement* pNewStatement = RewriteStatement(pOldStatement);
			if (pNewStatement != pOldStatement)
			{
				if (stbds_arrlenu(ppStmts) == 0ul)
				{
					stbds_arrsetlen(ppStmts, kLength);
					for (size_t j = 0; j < k; j++)
					{
						ppStmts[j] = block.Statements[j];
					}
				}
			}
			if (stbds_arrlenu(ppStmts) != 0ul)
			{
				ppStmts[k] = pNewStatement;
			}
		}

		return stbds_arrlenu(ppStmts) == 0ul ? pBlock : MakeBoundStatement_Block(ppStmts);
	}

	virtual BoundStatement* RewriteExpressionStatement(BoundStatement* pExpr) noexcept
	{
		BoundExpressionStatement& expr = pExpr->expr;

		BoundExpression* pExpression = RewriteExpression(expr.Expr);
		return pExpression == expr.Expr ? pExpr : MakeBoundStatement_Expression(pExpression);
	}
	
	virtual BoundStatement* RewriteVariableDeclarationStatement(BoundStatement* pVarDecl) noexcept
	{
		BoundVariableDeclarationStatement& vardecl = pVarDecl->vardecl;

		BoundExpression* pValue = RewriteExpression(vardecl.Value);
		return pValue == vardecl.Value ? pVarDecl : MakeBoundStatement_VariableDeclaration(vardecl.Variable, pValue);
	}
	
	virtual BoundStatement* RewriteDecomposeDeclarationStatement(BoundStatement* pDecompDecl) noexcept
	{
		BoundDecomposeDeclarationStatement& decomp = pDecompDecl->decomp;

		BoundExpression* pDecomposable = RewriteExpression(decomp.Decomposable);
		if (pDecomposable == decomp.Decomposable)
		{
			return pDecompDecl;
		}

		return MakeBoundStatement_DecomposeDeclaration(decomp.Struct, decomp.Variables, pDecomposable);
	}
	
	virtual BoundStatement* RewriteIfStatement(BoundStatement* pIf) noexcept
	{
		BoundIfStatement& ifstmt = pIf->ifstmt;

		// If
		BoundExpression* pIfCondition = RewriteExpression(ifstmt.IfCondition);
		BoundStatement* pIfBlock = RewriteStatement(ifstmt.IfBlock);

		// Else If...
		ElseIfBlock* pElseIfs = nullptr;

		const size_t kLength = stbds_arrlenu(ifstmt.ElseIfs);
		for (size_t k = 0; k < kLength; k++)
		{
			const auto&[pOldCondition, pOldBlock] = ifstmt.ElseIfs[k];

			BoundExpression* pNewCondition = RewriteExpression(pOldCondition);
			BoundStatement* pNewBlock = RewriteStatement(pOldBlock);

			if (pNewCondition != pOldCondition || pNewBlock != pOldBlock)
			{
				if (stbds_arrlenu(pElseIfs) == 0ul)
				{
					stbds_arrsetlen(pElseIfs, kLength);
					for (size_t j = 0; j < k; j++)
					{
						pElseIfs[j] = ifstmt.ElseIfs[j];
					}
				}
			}
			if (stbds_arrlenu(pElseIfs) != 0ul)
			{
				pElseIfs[k] = ElseIfBlock{ pNewCondition, pNewBlock };
			}
		}
		// Else
		BoundStatement* pElseBlock = ifstmt.ElseBlock ? RewriteStatement(ifstmt.ElseBlock) : nullptr;

		if (pIfCondition == ifstmt.IfCondition &&
			pIfBlock     == ifstmt.IfBlock     &&
			pElseBlock   == ifstmt.ElseBlock   &&
			stbds_arrlenu(pElseIfs) == 0ul)
		{
			return pIf;
		}

		return MakeBoundStatement_If(pIfCondition, pIfBlock, pElseIfs, pElseBlock);
	}
	
	virtual BoundStatement* RewriteForStatement(BoundStatement* pFor) noexcept
	{
		BoundForStatement& forstmt = pFor->forstmt;

		BoundExpression* pLowerBound = RewriteExpression(forstmt.LowerBound);
		BoundExpression* pUpperBound = RewriteExpression(forstmt.UpperBound);
		BoundExpression* pStep = RewriteExpression(forstmt.Step);
		BoundStatement* pBody  = RewriteStatement(forstmt.Loop.Body);

		if (pLowerBound == forstmt.LowerBound &&
			pUpperBound == forstmt.UpperBound &&
			pStep       == forstmt.Step       &&
			pBody       == forstmt.Loop.Body)
		{
			return pFor;
		}

		return MakeBoundStatement_For(forstmt.Variable, pLowerBound, pUpperBound, pStep, pBody, forstmt.Loop.BreakLabel, forstmt.Loop.ContinueLabel);
	}
	
	virtual BoundStatement* RewriteForeachStatement(BoundStatement* pForeach) noexcept
	{
		BoundForeachStatement& foreach = pForeach->foreach;

		BoundExpression* pIterable = RewriteExpression(foreach.Iterable);
		BoundStatement* pBody = RewriteStatement(foreach.Loop.Body);

		if (pIterable == foreach.Iterable && pBody == foreach.Loop.Body)
		{
			return pForeach;
		}

		return MakeBoundStatement_Foreach(foreach.Variable, pIterable, pBody, foreach.Loop.BreakLabel, foreach.Loop.ContinueLabel);
	}
	
	virtual BoundStatement* RewriteWhileStatement(BoundStatement* pWhile) noexcept
	{
		BoundWhileStatement& whilestmt = pWhile->whilestmt;

		BoundExpression* pCondition = RewriteExpression(whilestmt.Condition);
		BoundStatement* pBody = RewriteStatement(whilestmt.Loop.Body);

		if (pCondition == whilestmt.Condition && pBody == whilestmt.Loop.Body)
		{
			return pWhile;
		}

		return MakeBoundStatement_While(pCondition, pBody, whilestmt.IsDoWhile, whilestmt.Loop.BreakLabel, whilestmt.Loop.ContinueLabel);
	}
	
	virtual BoundStatement* RewriteReturnStatement(BoundStatement* pReturn) noexcept
	{
		BoundReturnStatement& ret = pReturn->ret;

		BoundExpression* pExpression = ret.Expr ? RewriteExpression(ret.Expr) : nullptr;
		return pExpression == ret.Expr ? pReturn : MakeBoundStatement_Return(pExpression);
	}
	
	virtual BoundStatement* RewriteLabelStatement(BoundStatement* pLabel) noexcept
	{
		return pLabel;
	}
	
	virtual BoundStatement* RewriteGotoStatement(BoundStatement* pGoto) noexcept
	{
		return pGoto;
	}
	
	virtual BoundStatement* RewriteConditionalGotoStatement(BoundStatement* pCondGoto) noexcept
	{
		BoundConditionalGotoStatement& cgotostmt = pCondGoto->cgotostmt;

		BoundExpression* pCondition = RewriteExpression(cgotostmt.Condition);
		return pCondition == cgotostmt.Condition ? pCondGoto : MakeBoundStatement_ConditionalGoto(cgotostmt.Label, pCondition, cgotostmt.JumpIfTrue);
	}

private:
	using _RewriteResult = Tuple<bool, BoundExpression*, BoundExpression**>;
	
	_RewriteResult RewriteExpressionWithExpressionList(BoundExpression* pOldParent, BoundExpression** pOldList) noexcept
	{
		BoundExpression* pNewParent = RewriteExpression(pOldParent);
		BoundExpression** pNewList = nullptr;

		const size_t kLength = stbds_arrlenu(pOldList);
		for (size_t k = 0; k < kLength; k++)
		{
			BoundExpression* pOldExpression = pOldList[k];
			BoundExpression* pNewExpression = RewriteExpression(pOldExpression);

			if (pNewExpression != pOldExpression)
			{
				if (stbds_arrlenu(pNewList) == 0ul)
				{
					stbds_arrsetlen(pNewList, kLength);
					for (size_t j = 0; j < k; j++)
					{
						pNewList[j] = pOldList[j];
					}
				}
			}
			if (stbds_arrlenu(pNewList) != 0ul)
			{
				pNewList[k] = pNewExpression;
			}
		}

		if (pNewParent == pOldParent && stbds_arrlenu(pNewList) == 0ul)
			return _RewriteResult{ false, pOldParent, pOldList };

		return _RewriteResult{ true, pNewParent, pNewList };
	}
};

/// INTERNAL LOWERER
class InternalLowerer final : public InternalBoundTreeRewriter
{
public:
	virtual ~InternalLowerer() noexcept = default;

	static BoundStatement* Lower(MySymbol* pFunction, BoundStatement* pStatement) noexcept
	{
		BoundStatement* pLoweredStatement = InternalLowerer{}.RewriteStatement(pStatement);
		return Flatten(pFunction, pLoweredStatement);
	}

protected:
	constexpr InternalLowerer() = default;

	virtual BoundStatement* RewriteIfStatement(BoundStatement* pIf)	 noexcept override
	{
		//
		// if (<cond>)
		//     <then>
		// elseif (<cond...>)
		//	   <then...>
		// else
		//     <else>
		// --->
		// goto-if-false <cond> skip
		// <then>
		// goto end
		// skip:
		// goto-if-false <cond...> skip...
		// <then...>
		// goto end
		// skip...:
		// <else>
		// end:
		//

		BoundIfStatement& ifs = pIf->ifstmt;

		static constexpr uint64_t Placeholder = 0x666DEADBEEF666;
		BoundStatement** ppStatements = nullptr;

		const BoundLabel SkipLabel = NextLabel();
		BoundStatement* pGotoIfFalse = MakeBoundStatement_ConditionalGoto(SkipLabel, ifs.IfCondition, false);
		
		stbds_arrpush(ppStatements, pGotoIfFalse);
		stbds_arrpush(ppStatements, ifs.IfBlock);

		// Labels are supposed to be generated in the order they appear (NOT used); so we can't create an end label
		// since we haven't yet encountered one. We can use a placeholder (gibberish) and then after lowering, reset it to
		// an actual label statement
		BoundStatement* pGotoEnd = (BoundStatement*)Placeholder;
		stbds_arrpush(ppStatements, pGotoEnd);

		BoundStatement* pSkipLabelStatement = MakeBoundStatement_Label(SkipLabel);
		stbds_arrpush(ppStatements, pSkipLabelStatement);

		// ElseIfs
		const size_t kCount = stbds_arrlenu(ifs.ElseIfs);
		for (size_t k = 0; k < kCount; k++)
		{
			const auto&[pCond, pBlock] = ifs.ElseIfs[k];

			const BoundLabel NextSkipLabel = NextLabel();
			BoundStatement* pNextGotoIfFalse = MakeBoundStatement_ConditionalGoto(NextSkipLabel, pCond, false);
			stbds_arrpush(ppStatements, pNextGotoIfFalse);

			stbds_arrpush(ppStatements, pBlock);
			stbds_arrpush(ppStatements, pGotoEnd);

			BoundStatement* pNextSkipLabelStatement = MakeBoundStatement_Label(NextSkipLabel);
			stbds_arrpush(ppStatements, pNextSkipLabelStatement);
		}

		if (ifs.ElseBlock)
		{
			stbds_arrpush(ppStatements, ifs.ElseBlock);
		}

		const BoundLabel EndLabel = NextLabel();
		BoundStatement* pEndLabelStatement = MakeBoundStatement_Label(EndLabel);
		stbds_arrpush(ppStatements, pEndLabelStatement);

		// Resolve the end label
		BoundStatement* pResolvedGotoEnd = MakeBoundStatement_Goto(EndLabel);
		for (size_t k = 0; k < stbds_arrlenu(ppStatements); k++)
		{
			if (ppStatements[k] == (BoundStatement*)Placeholder)
			{
				ppStatements[k] = pResolvedGotoEnd;
			}
		}

		BoundStatement* pLoweredIf = MakeBoundStatement_Block(ppStatements);
		return RewriteStatement(pLoweredIf);
	}

	virtual BoundStatement* RewriteForStatement(BoundStatement* pFor) noexcept override
	{
		//
		// for (<var>, <lower>, <upper>, <step>)
		//     <body>
		// --->
		// {
		//     var   <var> = <lower>
		//     const <con> = <upper>
		//     const <stp> = <step>
		//     while (<var> < <con>) {
		//         <body>
		//		   continue:
		//         <var> = <var> + <stp>
		//     }
		// }
		///

		BoundForStatement& fs = pFor->forstmt;

		MySymbol* pCounterVariable = fs.Variable;
		MySymbol* pUpperBoundVariable = MakeSymbol_Variable(NextUpperBoundVariableName(), My_Defaults.IntType, true, true);
		MySymbol* pStepVariable = MakeSymbol_Variable(NextStepVariableName(), My_Defaults.IntType, true, true);
		BoundBinaryOperator* pOperator = nullptr;

		BoundStatement* pCounterDecl = MakeBoundStatement_VariableDeclaration(pCounterVariable, fs.LowerBound);
		BoundExpression* pCounter = MakeBoundExpression_Name(pCounterVariable);

		BoundStatement* pUpperBoundDecl = MakeBoundStatement_VariableDeclaration(pUpperBoundVariable, fs.UpperBound);
		BoundExpression* pUpperBound = MakeBoundExpression_Name(pUpperBoundVariable);

		BoundStatement* pStepDecl = MakeBoundStatement_VariableDeclaration(pStepVariable, fs.Step);
		BoundExpression* pStep = MakeBoundExpression_Name(pStepVariable);

		pOperator = Binder::BindBinaryOperator(TokenKind::Plus, My_Defaults.IntType, My_Defaults.IntType);

		BoundStatement* pIncrement = nullptr;
		if constexpr (false)
		{
			pIncrement = MakeBoundStatement_Expression(
				MakeBoundExpression_Assignment(
					pCounter,
					pCounterVariable,
					MakeBoundExpression_Binary(
						pCounter,
						pOperator,
						pStep
					)
				)
			);
		}
		else
		{
			pIncrement = MakeBoundStatement_Expression(
				MakeBoundExpression_Increment(pCounterVariable, pStep)
			);
		}

		pOperator = Binder::BindBinaryOperator(TokenKind::Less, My_Defaults.IntType, My_Defaults.IntType);
		BoundExpression* pCondition = MakeBoundExpression_Binary(pCounter, pOperator, pUpperBound);
		BoundStatement* pContinueLabelStatement = MakeBoundStatement_Label(fs.Loop.ContinueLabel);

		BoundStatement* pBlock = MakeBoundStatement_Block({
			pCounterDecl,
			pUpperBoundDecl,
			pStepDecl,
			MakeBoundStatement_While(
				pCondition,
				MakeBoundStatement_Block({
					fs.Loop.Body,
					pContinueLabelStatement,
					pIncrement
				}),
				false,
				fs.Loop.BreakLabel,
				NextLabel()
			)
		});
		return RewriteStatement(pBlock);
	}

	virtual BoundStatement* RewriteForeachStatement(BoundStatement* pForeach) noexcept override
	{

		//
		// foreach (<var> : <iterable>)
		//     <body>
		// --->
		// {
		//     const v = <iterable>
		//     var   k = 0
		//     const n = len(v)
		//     while (k < n) {
		//         var <var> = v[k]
		//         <body>
		//		   continue:
		//         k++
		//     }
		// }
		///

		MY_NOT_IMPLEMENTED();
		return pForeach;

		/*BoundForeachStatement& fes = pForeach->foreach;

		BoundBinaryOperator* pOperator       = nullptr;
		MySymbol*           pLengthFunction = nullptr;

		{
			MySymbol* pParamSym = MakeSymbol_Parameter("vArray", MyType::ObjectArray, false, true);

			MyType** ppParamTypes = nullptr;
			stbds_arrpush(ppParamTypes, pParamSym->Type);

			MySymbol** ppParamSyms = nullptr;
			stbds_arrpush(ppParamSyms, pParamSym);

			pLengthFunction = MakeSymbol_Function(
				MyStrdup("length"),
				MakeType_Function(
					My_Defaults.UintType,
					ppParamTypes
				),
				ppParamSyms,
				false,
				false,
				false,
				nullptr
			);
		}
		
		MySymbol* pIterableVariable = MakeSymbol_Variable(NextIterableVariableName(), fes.Iterable->Type(), true, true);
		MySymbol* pCounterVariable = MakeSymbol_Variable(NextCounterVariableName(), My_Defaults.UintType, false, true);
		MySymbol* pLengthVariable = MakeSymbol_Variable(NextLengthVariableName(), My_Defaults.UintType, true, true);
		MySymbol* pStepVariable = MakeSymbol_Variable(NextStepVariableName(), My_Defaults.UintType, true, true);

		BoundStatement* pIterableDecl = MakeBoundStatement_VariableDeclaration(pIterableVariable, fes.Iterable);
		BoundExpression* pIterable = MakeBoundExpression_Name(pIterableVariable);

		BoundStatement* pCounterDecl = MakeBoundStatement_VariableDeclaration(
			pCounterVariable,
			MakeBoundExpression_Literal(pCounterVariable->Type, MakeValue_Uint64(0ull))
		);
		BoundExpression* pCounter = MakeBoundExpression_Name(pCounterVariable);
		BoundExpression** ppIndices = nullptr;
		stbds_arrpush(ppIndices, pCounter);

		BoundStatement* pLengthDecl = MakeBoundStatement_VariableDeclaration(
			pLengthVariable,
			MakeBoundExpression_Call(
				MakeBoundExpression_Name(pLengthFunction),
				pLengthFunction,
				{ pIterable }
			)
		);
		BoundExpression* pLength = MakeBoundExpression_Name(pLengthVariable);

		BoundStatement* pStepDecl = MakeBoundStatement_VariableDeclaration(
			pStepVariable,
			MakeBoundExpression_Literal(My_Defaults.UintType, MakeValue_Uint64(1ull))
		);
		BoundExpression* pStep = MakeBoundExpression_Name(pStepVariable);

		pOperator = Binder::BindBinaryOperator(TokenKind::Plus, My_Defaults.UintType, My_Defaults.UintType);

		BoundStatement* pIncrement = nullptr;
		if constexpr (false)
		{
			pIncrement = MakeBoundStatement_Expression(
				MakeBoundExpression_Assignment(
					pCounter,
					pCounterVariable,
					MakeBoundExpression_Binary(
						pCounter,
						pOperator,
						pStep
					)
				)
			);
		}
		else
		{
			pIncrement = MakeBoundStatement_Expression(
				MakeBoundExpression_Increment(pCounterVariable, pStep)
			);
		}

		MyType* pArrayItemType = pIterableVariable->Type->arraytype.Type;
		BoundStatement* pVarDecl = MakeBoundStatement_VariableDeclaration(
			fes.Variable,
			MakeBoundExpression_Index(pIterable, pArrayItemType, ppIndices)
		);

		pOperator = Binder::BindBinaryOperator(TokenKind::Less, My_Defaults.UintType, My_Defaults.UintType);
		BoundExpression* pCondition = MakeBoundExpression_Binary(pCounter, pOperator, pLength);
		BoundStatement* pContinueLabelStatement = MakeBoundStatement_Label(fes.Loop.ContinueLabel);

		BoundStatement* pBlock = MakeBoundStatement_Block({
			pIterableDecl,
			pCounterDecl,
			pLengthDecl,
			pStepDecl,
			MakeBoundStatement_While(
				pCondition,
				MakeBoundStatement_Block({
					pVarDecl,
					fes.Loop.Body,
					pContinueLabelStatement,
					pIncrement
				}),
				false,
				fes.Loop.BreakLabel,
				NextLabel()
			)
		});
		return RewriteStatement(pBlock);*/
	}

	virtual BoundStatement* RewriteWhileStatement(BoundStatement* pWhile) noexcept override
	{
		BoundWhileStatement& ws = pWhile->whilestmt;

		if (!ws.IsDoWhile)
		{
			//
			// while (<cond>)
			//     <body>
			// --->
			// goto check
			// continue:
			// <body>
			// check:
			// goto-if-true <cond> continue
			// break:
			///
			const BoundLabel  CheckLabel = NextLabel();

			BoundStatement* pGotoCheck = MakeBoundStatement_Goto(CheckLabel);
			BoundStatement* pContinueLabelStatement = MakeBoundStatement_Label(ws.Loop.ContinueLabel);
			BoundStatement* pCheckLabelStatement = MakeBoundStatement_Label(CheckLabel);
			BoundStatement* pGotoIfTrue = MakeBoundStatement_ConditionalGoto(ws.Loop.ContinueLabel, ws.Condition, true);
			BoundStatement* pBreakLabelStatement = MakeBoundStatement_Label(ws.Loop.BreakLabel);

			BoundStatement* pBlock = MakeBoundStatement_Block({
				pGotoCheck,
				pContinueLabelStatement,
				ws.Loop.Body,
				pCheckLabelStatement,
				pGotoIfTrue,
				pBreakLabelStatement
			});
			return RewriteStatement(pBlock);
		}
		else
		{
			//
			// do
			//     <body>
			// while (<cond>)
			// --->
			// continue
			// <body>
			// goto-if-true <cond> continue
			// break:
			///
			BoundStatement* pContinueLabelStatement = MakeBoundStatement_Label(ws.Loop.ContinueLabel);
			BoundStatement* pGotoIfTrue = MakeBoundStatement_ConditionalGoto(ws.Loop.ContinueLabel, ws.Condition, true);
			BoundStatement* pBreakLabelStatement = MakeBoundStatement_Label(ws.Loop.BreakLabel);

			BoundStatement* pBlock = MakeBoundStatement_Block({
				pContinueLabelStatement,
				ws.Loop.Body,
				pGotoIfTrue,
				pBreakLabelStatement
			});
			return RewriteStatement(pBlock);
		}
	}

	static BoundStatement* Flatten(MySymbol* pFunction, BoundStatement* pStatement)
	{
		BoundStatement** ppStmts = nullptr;
		BoundStatement** ppStack = nullptr;

		stbds_arrpush(ppStack, pStatement);

		while (stbds_arrlenu(ppStack) > 0ul)
		{
			BoundStatement* pCurrent = stbds_arrlast(ppStack); stbds_arrpop(ppStack);
			if (pCurrent->Kind == BoundStatementKind::Block)
			{
				const int32_t kLength = stbds_arrlenu(pCurrent->block.Statements);
				for (int32_t k = kLength-1; k >= 0; k--)
				{
					BoundStatement* const& pStmt = pCurrent->block.Statements[k];
					stbds_arrpush(ppStack, pStmt);
				}
			}
			else
			{
				stbds_arrpush(ppStmts, pCurrent);
			}
		}

		static const auto CanFallThrough = [](BoundStatement* const& pStatement) noexcept -> bool
		{
			return pStatement->Kind != BoundStatementKind::Return &&
				   pStatement->Kind != BoundStatementKind::Goto;
		};

		MyType* pReturnType = pFunction->funcsym.Type->Signature->Return;
		if (pReturnType == My_Defaults.VoidType)
		{
			if (stbds_arrlenu(ppStmts) == 0ul || CanFallThrough(stbds_arrlast(ppStmts)))
			{
				BoundStatement* const pReturn = MakeBoundStatement_Return(nullptr);
				stbds_arrpush(ppStmts, pReturn);
			}
		}

		return MakeBoundStatement_Block(ppStmts);
	}
};

/// LOWERER
BoundStatement* Lowerer::Lower(MySymbol* pFunction, BoundStatement* pStatement) noexcept
{
	return InternalLowerer::Lower(pFunction, pStatement);
}
