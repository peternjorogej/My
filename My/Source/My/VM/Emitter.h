#pragma once

#include "My/My.h"
#include "My/Binding/BoundTree.h"


enum class MyOpCode : uint16_t
{
	Invalid = 0,
	// Stack Ops
	Ldc = 1, Ldcf, Ldstr, Ldarr, Ldobj, Ldarg,
	Ldloc, Stloc, Ldglo, Stglo,
	Newarray, Newobj,
	Pop, Dup, Ldelem, Stelem, Ldfld, Stfld,
	Inc, Incf,
	// Math Ops
	Add, Addf, Sub, Subf, Mul, Mulf, Div, Divf, Pow, Powf,
	Lsh, Rsh, Mod, And, Or, Xor, Neg, Negf, Not,
	// Conversion Ops
	Cvtoi, Cvtou, Cvtof, Cvftoi, Cvftou,
	// Comparison Ops
	Ceq, Ceqf, Cneq, Cneqf, Clt, Cltf, Clte, Cltef, Cgt, Cgtf, Cgte, Cgtef,
	// Control Flow Ops
	Label, Jmp, Jnz, Jz,
	// Function Ops
	Func, Call, Ret, Calli,
	// Utility Ops
	Nop, Halt,

	COUNT,
};

struct MyInstruction
{
	MyOpCode Code = MyOpCode::Invalid;
	uint32_t  Arg0 = 0u;
	uint32_t  Arg1 = 0u;
};


class MyBytecodeProcessor
{
public:
	MyBytecodeProcessor(MyContext* pContext);
	~MyBytecodeProcessor() noexcept = default;

	bool Load(const std::string& InputPath);
	bool Dump(const std::string& OutputPath);

	static bool Load(MyAssembly* pAssembly, const std::string& InputPath);
	static bool Dump(const MyAssembly* pAssembly, const std::string& OutputPath);

	MyAssembly* const GetAssembly() noexcept;
	MyAssembly* const GetAssembly() const noexcept;

	void Emit(MyOpCode Code);
	void Emit(MyOpCode Code, uint32_t Arg0, uint32_t Arg1 = 0ul);
	void Emit(MyOpCode Code, int64_t Value);
	void Emit(MyOpCode Code, char* const& lpValue);
	void Emit(MyOpCode Code, const MyString* pValue);
	void Emit(MyOpCode Code, const MySymbol* pVariable);
	void Emit(const MyValue& Value);

	void RegisterStruct(MyStruct* const& pKlass);
	void RegisterFunction(char* const& lpName, pfnMyInternalFunction pfnInternal);
	void RegisterFunction(const InternalFunction& ifunc);
	bool IsRegistered(char* const& lpName);
	void BeginFunction(char* const& lpName);
	void EndFunction();

	void End();
	void Decompile();

private:
	void     EmitUnreferencedAddress(AddressMap** ppLabelMap, AddressMap** ppDeferredMap, char* const& lpLabel, MyOpCode Code);
	void     ResolveDeferredAddress(AddressMap** ppDeferredMap, char* const& lpLabel, uint32_t kAddress);
	uint32_t GetOrSetFieldIndex(char* const& lpField);
	uint32_t FindNativeByName(char* const& lpName);
	uint32_t GetStructIndex(MyStruct* const& pKlass);
	uint32_t GetStructIndex(char* const& lpName);

private:
	MyAssembly*  m_Assembly          = nullptr;
	// These are *arrays* of key-value pairs; since we could have
	// multiple jumps to the same unresolved label
	AddressMap*   m_DeferredLabels    = nullptr;
	AddressMap*   m_DeferredFunctions = nullptr;
};


class Emitter
{
public:
	static MyAssembly* Build(MyContext* pContext, const BoundProgram* pProgram, const List<InternalFunction>& Internals);

private:
	Emitter() = default;
	~Emitter() noexcept = default;

	void EmitProgram(MyBytecodeProcessor& bp, const BoundProgram* pProgram);
	void EmitFunction(MyBytecodeProcessor& bp, const MySymbol* pFunction, BoundStatement* pBody);

	void EmitTypes(MyBytecodeProcessor& bp, const BoundProgram* pProgram) noexcept;
	void EmitInternalFunctions(MyBytecodeProcessor& bp) noexcept;

	void EmitStatement(MyBytecodeProcessor& bp, BoundStatement* pStatement);
	void EmitExpressionStatement(MyBytecodeProcessor& bp, BoundStatement* pExpr);
	void EmitVariableDeclarationStatement(MyBytecodeProcessor& bp, BoundStatement* pVarDecl);
	void EmitDecomposeDeclarationStatement(MyBytecodeProcessor& bp, BoundStatement* pDecompDecl);
	void EmitLabelStatement(MyBytecodeProcessor& bp, BoundStatement* pLabel);
	void EmitGotoStatement(MyBytecodeProcessor& bp, BoundStatement* pGoto);
	void EmitConditionalGotoStatement(MyBytecodeProcessor& bp, BoundStatement* pCondGoto);
	void EmitReturnStatement(MyBytecodeProcessor& bp, BoundStatement* pReturn);

	void EmitExpression(MyBytecodeProcessor& bp, BoundExpression* pExpression);
	void EmitLiteralExpression(MyBytecodeProcessor& bp, BoundExpression* pLiteral);
	void EmitNameExpression(MyBytecodeProcessor& bp, BoundExpression* pName);
	void EmitUnaryExpression(MyBytecodeProcessor& bp, BoundExpression* pUnary);
	void EmitBinaryExpression(MyBytecodeProcessor& bp, BoundExpression* pBinary);
	void EmitTernaryExpression(MyBytecodeProcessor& bp, BoundExpression* pTernary);
	void EmitIncrementExpression(MyBytecodeProcessor& bp, BoundExpression* pIncrement);
	void EmitAssignmentExpression(MyBytecodeProcessor& bp, BoundExpression* pAssignment);
	void EmitCallExpression(MyBytecodeProcessor& bp, BoundExpression* pCall);
	void EmitIndexExpression(MyBytecodeProcessor& bp, BoundExpression* pIndex);
	void EmitFieldExpression(MyBytecodeProcessor& bp, BoundExpression* pField);
	void EmitOperatorNewExpression(MyBytecodeProcessor& bp, BoundExpression* pOperatorNew);
	void EmitConversionExpression(MyBytecodeProcessor& bp, BoundExpression* pConversion);
	void EmitArrayExpression(MyBytecodeProcessor& bp, BoundExpression* pArray);
	void EmitInstanceExpression(MyBytecodeProcessor& bp, BoundExpression* pInstance);

private:
	DiagnosticBag m_Diagnostics = {};
};


const char* OpCodeString(MyOpCode Code) noexcept;

