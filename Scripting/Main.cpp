// Scripting: This projects is for testing how I would use this language
// for scripting. It's a sort of playground
//

#include <My/Base/IO.h>
#include <My/Object.h>
#include <My/VM/Compiler.h>
#include <My/VM/VM.h>

#include "stb/stb_ds.h"

static void RegisterEntityType_Native(MyContext* pContext, MyVM* pVM) noexcept;

static Pair<char*, MyStruct*>* s_RegisteredEntityTypes = nullptr;

int main()
{
	MyContext* pContext = MyInitialize();

	List<InternalFunction> Internals =
	{
		{ "RegisterEntityType", RegisterEntityType_Native },
	};
	List<MyStruct*> UserStructs = {};

	MyAssembly* pAss = Compiler::Build(pContext, "Script.my", Internals, UserStructs);
	if (!pAss)
	{
		Console::WriteLine(Console::Color::Red, "Build failed");
		goto Error;
	}
	MyDecompile(pAss);
	Console::WriteLine(Console::Color::Green, "Build successful");

	MyVM::Invoke(pContext, "BeginScripting", { });

	static constexpr const char* s_EntityTypeName = "PlayerEntity";

	MyStruct* pEntity = MyContextGetStruct(pContext, s_EntityTypeName);
	if (!pEntity)
	{
		Console::WriteLine(Console::Color::Red, "Entity type '%s' not found", s_EntityTypeName);
		goto Error;
	}

	MyObject* pObject = MyObjectNew(pContext, pEntity);
	MyVM::Invoke(pContext, pObject, "OnCreate", { });
	if (!pContext->VM->Stack.PopU64())
	{
		Console::WriteLine(Console::Color::Red, "%s.OnCreate returned false", s_EntityTypeName);
		goto Error;
	}

	double t = 0.0;
	while (true)
	{
		MyVM::Invoke(pContext, pObject, "OnUpdate", { MakeValue_Float64(1.0 / 30.0) });

		if ((t = MyObjectFieldGetValueAs<double>(pObject, "Value")) > 1.0)
		{
			break;
		}
	}
	for (size_t k = 0; k < stbds_shlenu(s_RegisteredEntityTypes); k++)
	{
		printf("\n[DEBUG]: Registered Entity Type: %s", s_RegisteredEntityTypes[k].key);
	}
	Console::WriteLine("\n[DEBUG]: DONE!");

	Console::ReadKey();

Error:
	MyUninitialize(pContext);
}

void RegisterEntityType_Native(MyContext* pContext, MyVM* pVM) noexcept
{
	MyString* pEntityTypeName = pVM->Stack.PopString();

	printf("[C++]: %s(TypeName: %s)\n", __FUNCTION__, pEntityTypeName->Chars);
	MyStruct* pKlass = MyContextGetStruct(pContext, pEntityTypeName->Chars);
	if (pKlass)
	{
		stbds_shput(s_RegisteredEntityTypes, pKlass->Name, pKlass);
	}

	pContext->VM->Stack.Push((bool)pKlass);
}
