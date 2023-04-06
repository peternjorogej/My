// Scripting: This projects is for testing how I would use this language
// for scripting. It's a sort of playground
//

#include <My/Base/IO.h>
#include <My/Object.h>
#include <My/VM/Compiler.h>
#include <My/VM/VM.h>

static void CppFunction_Native(MyContext* pContext, MyVM* pVM) noexcept;

int main()
{
	MyContext* pContext = MyInitialize();

	List<InternalFunction> Internals = { { "CppFunction", CppFunction_Native }, };
	List<MyStruct*> UserStructs = {};

	MyAssembly* pAss = Compiler::Build(pContext, "Script.my", Internals, UserStructs);
	if (!pAss)
	{
		goto Error;
	}
	MyDecompile(pAss);
	Console::WriteLine(Console::Color::Green, "Build successful");

	MyStruct* pMySexyStruct = MyContextGetStruct(pContext, "MySexyStruct");
	if (!pMySexyStruct)
	{
		goto Error;
	}

	MyObject* pObject = MyObjectNew(pContext, pMySexyStruct);
	MyVM::Invoke(pContext, pObject, "SexyMethod", { });

	MyVM::Invoke(pContext, "MyFunction", { });
	Console::ReadKey();

Error:
	MyUninitialize(pContext);
}

void CppFunction_Native(MyContext* pContext, MyVM* pVM) noexcept
{
	int64_t iValue = pVM->Stack.PopI64();
	MyString* Text = pVM->Stack.PopString();

	printf("[C++]: Calling '%s'\n", __FUNCTION__);
	printf("%s %I64d\n", Text->Chars, iValue);
}
