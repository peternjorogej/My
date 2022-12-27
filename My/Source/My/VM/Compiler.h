#pragma once

#include "My/My.h"

class Compiler
{
public:
	static MyAssembly* Build(MyContext* pContext, const char* lpFilename, const List<InternalFunction>& Internals = {}) noexcept;
	static MyAssembly* BuildSource(MyContext* pContext, const char* lpSource, const List<InternalFunction>& Internals = {}) noexcept;
	static int64_t	   Run(MyContext* pContext, MyAssembly* pAssembly, int iArgc, char** ppArgv);
	static int64_t	   RunSource(MyContext* pContext, const char* lpSource, int iArgc, char** ppArgv);
	
	static MyAssembly* Load(MyContext* pContext, const char* lpFilename) noexcept;
	static bool         Dump(MyContext* pContext, const MyAssembly* pAssembly, const char* lpFilename) noexcept;
};

