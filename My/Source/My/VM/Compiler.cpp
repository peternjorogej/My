#include "Compiler.h"
#include "My/My.h"
#include "My/Base/IO.h"
#include "My/Syntax/Tree.h"
#include "My/Binding/BoundTree.h"
#include "My/VM/Emitter.h"
#include "My/VM/VM.h"

static MyAssembly* _My_Compiler_Build(
	MyContext*  pContext,
	SyntaxTree* pTree,
	const List<InternalFunction>& Internals,
	const List<MyStruct*>& UserStructs
) noexcept;


MyAssembly* Compiler::Build(
	MyContext*  pContext,
	const char* lpFilename,
	const List<InternalFunction>& Internals,
	const List<MyStruct*>&        UserStructs
) noexcept
{
	SyntaxTree* pTree = SyntaxTree::Load(pContext, lpFilename);
	return _My_Compiler_Build(pContext, pTree, Internals, UserStructs);
}

MyAssembly* Compiler::BuildSource(
	MyContext*  pContext,
	const char* lpSource,
	const List<InternalFunction>& Internals,
	const List<MyStruct*>&        UserStructs
) noexcept
{
	SyntaxTree* pTree = SyntaxTree::Parse(pContext, lpSource);
	return _My_Compiler_Build(pContext, pTree, Internals, UserStructs);
}

int64_t Compiler::Run(MyContext* pContext, MyAssembly* pAssembly, int iArgc, char** ppArgv)
{
	pContext->VM->Prepare(pAssembly, iArgc, ppArgv);
    return pContext->VM->Run();
}

int64_t Compiler::RunSource(MyContext* pContext, const char* lpSource, int iArgc, char** ppArgv)
{
	MyAssembly* pAssembly = BuildSource(pContext, lpSource);
	pContext->VM->Prepare(pAssembly, iArgc, ppArgv);
	return pContext->VM->Run();
}

MyAssembly* Compiler::Load(MyContext* pContext, const char* lpFilename) noexcept
{
	if (pContext->Assembly)
	{
		delete pContext->Assembly;
		pContext->Assembly = nullptr;
	}

	pContext->Assembly = new MyAssembly{};
	MyBytecodeProcessor::Load(pContext->Assembly, lpFilename);

    return pContext->Assembly;
}

bool Compiler::Dump(MyContext* pContext, const MyAssembly* pAssembly, const char* lpFilename) noexcept
{
    return MyBytecodeProcessor::Dump(pAssembly, lpFilename);
}


MyAssembly* _My_Compiler_Build(
	MyContext*  pContext,
	SyntaxTree* pTree,
	const List<InternalFunction>& Internals,
	const List<MyStruct*>&        UserStructs
) noexcept
{
	if (pTree->GetDiagnostics().Any())
	{
		PrintDiagnostics(pTree);
		return nullptr;
	}

	BoundGlobalScope GlobalScope = Binder::BindGlobalScope(pContext, pTree, UserStructs);
	if (GlobalScope.Diagnostics.Any())
	{
		PrintDiagnostics(pTree, GlobalScope.Diagnostics);
		return nullptr;
	}

	BoundProgram Program = Binder::BindProgram(pContext, &GlobalScope, pTree);
	if (Program.Diagnostics.Any())
	{
		PrintDiagnostics(pTree, Program.Diagnostics);
		return nullptr;
	}

	MyAssembly* pAssembly = Emitter::Build(pContext, &Program, Internals);
	return (pContext->Assembly = pAssembly);
}
