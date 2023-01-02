#include "ControlFlowGraph.h"
#include "BoundTree.h"
#include "My/Base/Core.h"
#include "My/Base/IO.h"
#include "Stb/stb_ds.h"


/// HELPER CLASSES
class BasicControlBlockBuilder
{
public:
	BasicControlBlock** Build(BoundStatement* pBlock)
	{
		for (size_t k = 0; k < stbds_arrlenu(pBlock->block.Statements); k++)
		{
			BoundStatement* const& pStatement = pBlock->block.Statements[k];

			switch (pStatement->Kind)
			{
				case BoundStatementKind::Goto:
				case BoundStatementKind::ConditionalGoto:
				case BoundStatementKind::Return:
					stbds_arrpush(m_Statements, pStatement);
					StartBlock();
					break;
				case BoundStatementKind::Label:
					StartBlock();
					stbds_arrpush(m_Statements, pStatement);
					break;
				case BoundStatementKind::Expression:
				case BoundStatementKind::VariableDeclaration:
				case BoundStatementKind::DecomposeDeclaration:
					stbds_arrpush(m_Statements, pStatement);
					break;
				default:
					MY_ASSERT(false, "Unexpected statement: %s", BoundStatementKindString(pStatement->Kind));
					break;
			}
		}

		EndBlock();
		return m_Blocks;
	}

private:
	void StartBlock()
	{
		EndBlock();
	}

	void EndBlock()
	{
		if (size_t kLength = stbds_arrlenu(m_Statements); kLength > 0ul)
		{
			BasicControlBlock* pBlock = Allocator::Create<BasicControlBlock>(Allocator::Stage::Binder);

			BoundStatement** ppStmts = nullptr;
			for (size_t k = 0; k < kLength; k++)
			{
				stbds_arrpush(ppStmts, m_Statements[k]);
			}
			pBlock->Statements = ppStmts;

			stbds_arrpush(m_Blocks, pBlock);
			stbds_arrfree(m_Statements);
		}
	}

private:
	BasicControlBlock*  m_CurrentBlock = nullptr;
	BoundStatement**    m_Statements   = nullptr;
	BasicControlBlock** m_Blocks       = nullptr;
};

class ControlFlowGraphBuilder
{
public:
	using StmtBlockMap  = Pair<BoundStatement*, BasicControlBlock*>;
	using LabelBlockMap = Pair<BoundLabel, BasicControlBlock*>;

public:
	ControlFlowGraphBuilder()
	{
		m_Start = Allocator::Create<BasicControlBlock>(Allocator::Stage::Binder, true);
		m_End   = Allocator::Create<BasicControlBlock>(Allocator::Stage::Binder, false);

		static constexpr StmtBlockMap sbm = { nullptr, nullptr };
		stbds_hmdefault(m_BlockFromStatement, nullptr);
		stbds_hmdefaults(m_BlockFromStatement, sbm);

		static constexpr LabelBlockMap lbm = { {}, nullptr };
		stbds_hmdefault(m_BlockFromLabel, nullptr);
		stbds_hmdefaults(m_BlockFromLabel, lbm);
	}

	~ControlFlowGraphBuilder() noexcept = default;

	ControlFlowGraph Build(BasicControlBlock**& ppBlocks)
	{
		size_t kBlockCount = stbds_arrlenu(ppBlocks);

		if (kBlockCount == 0ul)
		{
			Connect(m_Start, m_End);
		}
		else
		{
			Connect(m_Start, ppBlocks[0]);
		}

		for (size_t k = 0; k < kBlockCount; k++)
		{
			BasicControlBlock* pBlock = ppBlocks[k];

			for (size_t i = 0; i < stbds_arrlenu(pBlock->Statements); i++)
			{
				BoundStatement* const& pStatement = pBlock->Statements[i];
				stbds_hmput(m_BlockFromStatement, pStatement, pBlock);

				if (pStatement->Kind == BoundStatementKind::Label)
				{
					BoundLabelStatement& label = pStatement->label;
					stbds_hmput(m_BlockFromLabel, label.Label, pBlock);
				}
			}
		}

		for (size_t k = 0; k < kBlockCount; k++)
		{
			BasicControlBlock* pBlock = ppBlocks[k];
			BasicControlBlock* pNext = k == (kBlockCount - 1ul) ? m_End : ppBlocks[k + 1];

			for (size_t i = 0; i < stbds_arrlenu(pBlock->Statements); i++)
			{
				BoundStatement* const& pStatement = pBlock->Statements[i];
				Walk(pStatement, pBlock, pNext, pStatement == stbds_arrlast(pBlock->Statements));
			}
		}

	RestartLoop:
		for (size_t k = 0; k < kBlockCount; k++)
		{
			BasicControlBlock* const pBlock = ppBlocks[k];
			if (stbds_arrlenu(pBlock->Incoming) == 0ul)
			{
				RemoveBlock(ppBlocks, pBlock);
				goto RestartLoop;
			}
		}

		stbds_arrins(ppBlocks, 0, m_Start);
		stbds_arrpush(ppBlocks, m_End);

		return ControlFlowGraph(m_Start, m_End, ppBlocks, m_Branches);
	}

private:
	void Connect(BasicControlBlock* pFrom, BasicControlBlock* pTo, BoundExpression* pCondition = nullptr)
	{
		if (pCondition && pCondition->Kind == BoundExpressionKind::Literal)
		{
			if ((bool)pCondition->literal.Value)
				pCondition = nullptr;
			else
				return;
		}

		BasicControlBlockBranch* pBranch = Allocator::Create<BasicControlBlockBranch>(Allocator::Stage::Binder, pFrom, pTo, pCondition);
		stbds_arrpush(pFrom->Outgoing, pBranch);
		stbds_arrpush(pTo->Incoming, pBranch);
		stbds_arrpush(m_Branches, pBranch);
	}

	void Walk(BoundStatement* const& pStatement, BasicControlBlock* pBlock, BasicControlBlock* pNext, bool bIsLast)
	{
		switch (pStatement->Kind)
		{
			case BoundStatementKind::Goto:
				Connect(pBlock, stbds_hmget(m_BlockFromLabel, pStatement->gotostmt.Label));
				break;
			case BoundStatementKind::ConditionalGoto:
			{
				BoundConditionalGotoStatement& cgs = pStatement->cgotostmt;
				BasicControlBlock* pThenBlock = stbds_hmget(m_BlockFromLabel, cgs.Label);
				BasicControlBlock* pElseBlock = pNext;
				BoundExpression* pNegatedCondition = Negate(cgs.Condition);
				BoundExpression* pThenCondition = cgs.JumpIfTrue ? cgs.Condition : pNegatedCondition;
				BoundExpression* pElseCondition = cgs.JumpIfTrue ? pNegatedCondition : cgs.Condition;
				Connect(pBlock, pThenBlock, pThenCondition);
				Connect(pBlock, pElseBlock, pElseCondition);
				break;
			}
			case BoundStatementKind::Return:
				Connect(pBlock, m_End);
				break;
			case BoundStatementKind::VariableDeclaration:
			case BoundStatementKind::DecomposeDeclaration:
			case BoundStatementKind::Label:
			case BoundStatementKind::Expression:
				if (bIsLast)
				{
					Connect(pBlock, pNext);
				}
				break;
			default:
				MY_ASSERT(false, "Unexpected statement: %s", BoundStatementKindString(pStatement->Kind));
				break;
		}
	}

	template<typename T>
	static void Remove(T* const& pItems, const T& Item) noexcept
	{
		int32_t iIndex = -1;
		int32_t iLength = stbds_arrlenu(pItems);
		
		// NOTE: Better (faster) search algorithm
		for (int32_t k = 0; k < iLength; k++)
		{
			if (pItems[k] == Item)
			{
				iIndex = k;
				break;
			}
		}

		if (iIndex >= 0)
		{
			stbds_arrdel(pItems, iIndex);
		}
	}

	void RemoveBlock(BasicControlBlock**& ppBlocks, BasicControlBlock* pBlock)
	{
		for (size_t k = 0; k < stbds_arrlenu(pBlock->Incoming); k++)
		{
			BasicControlBlockBranch* const& pBranch = pBlock->Incoming[k];

			Remove(pBranch->From->Outgoing, pBranch);
			Remove(m_Branches, pBranch);
		}
		
		for (size_t k = 0; k < stbds_arrlenu(pBlock->Outgoing); k++)
		{
			BasicControlBlockBranch* const& pBranch = pBlock->Outgoing[k];

			Remove(pBranch->To->Incoming, pBranch);
			Remove(m_Branches, pBranch);
		}

		Remove(ppBlocks, pBlock);
	}

	BoundExpression* Negate(BoundExpression* pCondition)
	{
		static BoundExpression* pFalse = MakeBoundExpression_Literal(My_Defaults.BooleanType, MakeValue_Bool(false));
		static BoundExpression* pTrue  = MakeBoundExpression_Literal(My_Defaults.BooleanType, MakeValue_Bool(true));

		if (pCondition->Kind == BoundExpressionKind::Literal)
		{
			return (bool)pCondition->literal.Value ? pFalse : pTrue;
		}

		BoundUnaryOperator* pOperator = Binder::BindUnaryOperator(TokenKind::Bang, My_Defaults.BooleanType);
		return MakeBoundExpression_Unary(pOperator, pCondition);
	}

private:
	BasicControlBlock* m_Start = nullptr;
	BasicControlBlock* m_End   = nullptr;
	StmtBlockMap*      m_BlockFromStatement = nullptr;
	LabelBlockMap*     m_BlockFromLabel     = nullptr;
	BasicControlBlockBranch** m_Branches = nullptr;
};

/// DEFINITIONS
BasicControlBlock::BasicControlBlock(bool IsStart)
	: bIsStart(IsStart), bIsEnd(!bIsStart)
{ }

BasicControlBlockBranch::BasicControlBlockBranch(BasicControlBlock* pFrom, BasicControlBlock* pTo, BoundExpression* pCondition)
	: From(pFrom), To(pTo), Condition(pCondition)
{ }

ControlFlowGraph::ControlFlowGraph(
	BasicControlBlock* pStart,
	BasicControlBlock* pEnd,
	BasicControlBlock** const&       ppBlocks,
	BasicControlBlockBranch** const& ppBranches
)
	: m_Start(pStart), m_End(pEnd), m_Blocks(ppBlocks), m_Branches(ppBranches)
{ }


ControlFlowGraph ControlFlowGraph::Create(BoundStatement* pBlock)
{
	BasicControlBlockBuilder BlockBuilder;
	BasicControlBlock** ppBlocks = BlockBuilder.Build(pBlock);

	ControlFlowGraphBuilder GraphBuilder;
	return GraphBuilder.Build(ppBlocks);
}

bool ControlFlowGraph::AllPathsReturn(BoundStatement* pBlock)
{
	ControlFlowGraph Graph = Create(pBlock);

	for (size_t k = 0; k < stbds_arrlenu(Graph.m_End->Incoming); k++)
	{
		BasicControlBlockBranch* const& pBranch = Graph.m_End->Incoming[k];
		BoundStatement** ppStmts = pBranch->From->Statements;

		if (stbds_arrlenu(ppStmts) == 0ul)
		{
			return false;
		}
		if (stbds_arrlast(ppStmts)->Kind != BoundStatementKind::Return)
		{
			return false;
		}
	}

	return true;
}


