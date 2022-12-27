#pragma once

struct BoundExpression;
struct BoundStatement;

class BasicControlBlock;
class BasicControlBlockBranch;

class BasicControlBlock
{
public:
	BasicControlBlock() = default;
	BasicControlBlock(bool bIsStart);

	bool bIsStart = false;
	bool bIsEnd   = false;
	BoundStatement**          Statements = nullptr;
	BasicControlBlockBranch** Incoming   = nullptr;
	BasicControlBlockBranch** Outgoing   = nullptr;
};

class BasicControlBlockBranch
{
public:
	BasicControlBlockBranch(BasicControlBlock* pFrom, BasicControlBlock* pTo, BoundExpression* pCondition);

public:
	BasicControlBlock* From = nullptr;
	BasicControlBlock* To   = nullptr;
	BoundExpression*   Condition = nullptr;
};

class ControlFlowGraph
{
public:
	~ControlFlowGraph() = default;

	static ControlFlowGraph Create(BoundStatement* pBlock);
	static bool				AllPathsReturn(BoundStatement* pBlock);

private:
	friend class ControlFlowGraphBuilder;

	ControlFlowGraph(
		BasicControlBlock* pStart,
		BasicControlBlock* pEnd,
		BasicControlBlock** const&       pBlocks,
		BasicControlBlockBranch** const& pBranches
	);

private:
	BasicControlBlock*        m_Start    = nullptr;
	BasicControlBlock*        m_End      = nullptr;
	BasicControlBlock**	      m_Blocks   = nullptr;
	BasicControlBlockBranch** m_Branches = nullptr;
};
