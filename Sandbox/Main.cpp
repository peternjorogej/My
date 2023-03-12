
/// Major problems that need to be fixed
///  1. Passing functions (pointers/addresses) as parameters to functions so that
///     we can have callbacks
///  2. Proper FFI with C/C++. Something like P/Invoke in C# would (kind of) work,
///     but is there a way to better implement internal calls (defined as extern here)?
///  3. Structs:
///     - Implementing pod structs
///     - Constructors (and maybe Destructors)
///     - Using the defined methods
///  4. Proper type serialization (Maybe Relfection in (distant) future?)
///  5. const should modify the type and not the variable itself. This means that
///     we can't assign to variables that have a const modifier on their types
///  6. Improve some of the instructions (e.g ldfld, stfld, ldelem, stelem), they do
///     a lot and slow down the vm. Also think about inplace operations, especially
///     in loops, e.q clt 0, 1 means cmp less than between local 0 & 1 (not pushing
///     into evaluation stack then setting it back)
///  7. Compile time functions:
///     - static_assert, ...
///  8. Debugging *?
/// 
/// 

#include <My/Base/IO.h>
#include <My/Object.h>
#include <My/VM/VM.h>
#include <My/Utils/Utils.h>
#include <My/VM/Compiler.h>

#include <stb/stb_ds.h>
#include <argparse/argparse.h>

static const char* const lpUsages[] =
{
	"myc [options] [[--] args]",
	NULL,
};


static void PrintDataTypeSizes() noexcept;

static void CppFunction_Native(MyContext* pContext, MyVM* pVM) noexcept
{
	int64_t i = pVM->Stack.PopI64();
	MyString* s = pVM->Stack.PopString();
	Console::WriteLine("%s, %I64d", s->Chars, i);
}

int main(int iArgc, char** ppArgv)
{
	MyContext* pContext = MyInitialize();

	std::vector<char*> Argv(iArgc);
	for (int k = 0; k < iArgc; k++)
	{
		Argv[k] = ppArgv[k];
	}

	int  bPrintDataTypeSizes = false;
	const char* lpBuildFilepath = nullptr;
	const char* lpRunFilepath = nullptr;
	const char* lpRunSourceFilepath = nullptr;
	const char* lpDecompFilepath = nullptr;

	struct argparse_option pOptions[] =
	{
		OPT_HELP(),
		
		OPT_GROUP("Basic options"),
		OPT_STRING('b', "build",  &lpBuildFilepath,     "Build the .MY file to a .MYBC", nullptr, (intptr_t)0, 0),
		OPT_STRING('r', "run",    &lpRunFilepath,       "Run the compiled .MYBC file",   nullptr, (intptr_t)0, 0),
		OPT_STRING('s', "runsrc", &lpRunSourceFilepath, "Run the .MY file",              nullptr, (intptr_t)0, 0),
		OPT_STRING('d', "decomp", &lpDecompFilepath,    "Decompile the .MYBC file",      nullptr, (intptr_t)0, 0),
		
		OPT_BOOLEAN(0, "dtype-sizes", &bPrintDataTypeSizes, "Print the sizes for the data types in My", nullptr, (intptr_t)0, 0),

		OPT_END(),
	};

	struct argparse ArgpParser;
	argparse_init(&ArgpParser, pOptions, lpUsages, 0);
	argparse_describe(&ArgpParser, "\nMy Language Compiler.", nullptr);
	iArgc = argparse_parse(&ArgpParser, iArgc, (const char**)ppArgv);

	List<InternalFunction> Internals =
	{
		{ "CppFunction", CppFunction_Native },
	};

	if (bPrintDataTypeSizes)
	{
		PrintDataTypeSizes();
	}

	if (lpBuildFilepath)
	{
		MyAssembly* pAss = Compiler::Build(pContext, lpBuildFilepath, Internals, {});
		const std::string Filename = Console::Format("%sbc", lpBuildFilepath);
		if (Compiler::Dump(pContext, pAss, Filename.c_str()))
		{
			Console::WriteLine(Console::Color::Green, "Build successful (output: %s)", Filename.c_str());
		}
		else
		{
			Console::WriteLine(Console::Color::Red, "Build failed (could not compile %s)", lpBuildFilepath);
		}
	}
	if (lpRunFilepath)
	{
		MyAssembly* pAss = Compiler::Load(pContext, lpRunFilepath);
		if (pAss)
		{
			int64_t iResult = Compiler::Run(pContext, pAss, (int)Argv.size(), Argv.data());
			Console::Color Color = Console::Color::Green;
			if (iResult != MY_RC_SUCCESS)
			{
				Color = Console::Color::Red;
			}
			Console::WriteLine(Color, "\nProgram exited with code %I64d (%s)\n", iResult, MyReturnCodeString(iResult));
		}
		else
		{
			Console::WriteLine(Console::Color::Red, "Failed to load bytecode file '%s' for execution", lpRunFilepath);
		}
	}
	if (lpRunSourceFilepath)
	{
		MyAssembly* pAss = Compiler::Build(pContext, lpRunSourceFilepath, Internals, {});
		if (pAss)
		{
			int64_t iResult = Compiler::Run(pContext, pAss, (int)Argv.size(), Argv.data());
			Console::Color Color = Console::Color::Green;
			if (iResult != MY_RC_SUCCESS)
			{
				Color = Console::Color::Red;
			}
			Console::WriteLine(Color, "\nProgram exited with code %I64d (%s)\n", iResult, MyReturnCodeString(iResult));
		}
		else
		{
			Console::WriteLine(Console::Color::Red, "Build failed (could not compile %s)", lpRunSourceFilepath);
		}
	}
	if (lpDecompFilepath)
	{
		MyAssembly* pAss = Compiler::Load(pContext, lpDecompFilepath);
		if (pAss)
		{
			MyDecompile(pAss);
			Console::WriteLine("(Successful decompilation)");
		}
		else
		{
			Console::WriteLine(Console::Color::Red, "Failed to load bytecode file '%s' for decompilation", lpDecompFilepath);
		}
	}

	MyUninitialize(pContext);
	return 0;
}

void PrintDataTypeSizes() noexcept
{
	Console::WriteLine(Console::Color::Magenta, "Hello, World from My\n");

	Console::WriteLine("sizeof(MyGuid)              = %I64u", sizeof(MyGuid));
	Console::WriteLine("sizeof(MyAssembly)          = %I64u", sizeof(MyAssembly));
	Console::WriteLine("sizeof(MyGC)                = %I64u", sizeof(MyGC));
	Console::WriteLine("sizeof(MyStack)             = %I64u", sizeof(MyStack));
	Console::WriteLine("sizeof(MyVM)                = %I64u", sizeof(MyVM));
	Console::WriteLine("sizeof(MyContext)           = %I64u", sizeof(MyContext));

	Console::WriteLine("sizeof(MyFunctionSignature) = %I64u", sizeof(MyFunctionSignature));
	Console::WriteLine("sizeof(MyFunction)          = %I64u", sizeof(MyFunction));
	Console::WriteLine("sizeof(MyMethod)            = %I64u", sizeof(MyMethod));
	Console::WriteLine("sizeof(MyField)             = %I64u", sizeof(MyField));
	Console::WriteLine("sizeof(MyStruct)            = %I64u", sizeof(MyStruct));
	Console::WriteLine("sizeof(MyArrayType)         = %I64u", sizeof(MyArrayType));
	Console::WriteLine("sizeof(MyType)              = %I64u", sizeof(MyType));

	Console::WriteLine("sizeof(MyValue)             = %I64u", sizeof(MyValue));
	Console::WriteLine("sizeof(MyObject)            = %I64u", sizeof(MyObject));
	Console::WriteLine("sizeof(MyString)            = %I64u", sizeof(MyString));
	Console::WriteLine("sizeof(MyArrayShape)        = %I64u", sizeof(MyArrayShape));
	Console::WriteLine("sizeof(MyArray)             = %I64u", sizeof(MyArray));
	
	Console::WriteLine();
}

