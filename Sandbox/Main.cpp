
/// Major problems that need to be fixed
///  1. Rewriting the parser to remove all the goto statements, and sometimes, to defer
///     error reporting to the later stages of parsing (I want some of the functions to
///     return nullptrs and NOT report errors).
///  2. Passing functions (pointers/addresses) as parameters to functions so that
///     we can have callbacks
///  3. Proper FFI with C/C++. Something like P/Invoke in C# would (kind of) work,
///     but is there a way to implement internal calls (defined as extern here)?
///  4. Structs:
///     - Implementing pod structs
///     - Using the defined methods
///  5. Proper type serialization (Maybe Relfection in (distant) future?)
///  6. const should modify the type and not the variable itself. This means that
///     we can't assign to variables that have a const modifier on their types
///  7. Initializers/(implicit)constructors: sth to initialize struct members
///  8. Improve some of the instructions (e.g ldfld, stfld, ldelem, stelem), they do
///     a lot and slow down the vm. Also think about inplace operations, especially
///     in loops, e.q clt 0, 1 means cmp less than between local 0 & 1 (not pushing
///     into evaluation stack then setting it back)
///  9. Compile time functions:
///     - static_assert, ...
/// 10. Debugging *?
/// 
/// 
/// 
/// 

#include <My/Base/IO.h>
#include <My/Object.h>
#include <My/VM/VM.h>
#include <My/Utils/Utils.h>
#include <Stb/stb_ds.h>
#include <My/VM/Compiler.h>

static void DataTypeSizes() noexcept;
static void TestNewAPI(MyContext* pContext) noexcept;

struct Complex
{
	double Real = 0.0;
	double Imag = 0.0;
};

int main(int iArgc, char** ppArgv)
{
	MyContext* pContext = MyInitialize();

	if constexpr (false)
	{
		DataTypeSizes();
	}
	if constexpr (false)
	{
		TestNewAPI(pContext);
	}

	// Try to register a type natively
	MyStruct* pKlass = MyStructCreate(pContext, "MyStruct", MY_STRUCT_ATTR_NONE);
	MyStructAddFieldAutoOffset(pKlass, "__Dummy0", My_Defaults.IntType, My_Defaults.IntStruct, MY_FIELD_ATTR_NONE);
	MyStructAddFieldAutoOffset(pKlass, "__Dummy1", My_Defaults.IntType, My_Defaults.IntStruct, MY_FIELD_ATTR_NONE);

	MyAssembly* pAss = Compiler::Build(pContext, "Samples/Hello.my", {}, { pKlass });
	if (pAss)
	{
		MyDecompile(pAss);
		int64_t kResult = Compiler::Run(pContext, pAss, iArgc, ppArgv);
		{
			Console::Color Color = Console::Color::Green;
			if (kResult != MY_RC_SUCCESS)
			{
				Color = Console::Color::Red;
			}
			Console::WriteLine(Color, "\nProgram exited with code %I64d (%s)\n", kResult, MyReturnCodeString(kResult));
		}
	}

	MyUninitialize(pContext);
	return 0;
}

void DataTypeSizes() noexcept
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

void TestNewAPI(MyContext* pContext) noexcept
{
	{
		Console::WriteLine(Console::Color::Green, "Testing array of strings");

		MyArrayShape shape = { 3ul, 0ul, 0ul, 0ul };
		MyArray* pNames = MyArrayNew(pContext, My_Defaults.StringStruct, shape);
		MyArraySet2(pNames, MyString*, 0, MyStringNew(pContext, "Peter"));
		MyArraySet2(pNames, MyString*, 1, MyStringNew(pContext, "Njoroge"));
		MyArraySet2(pNames, MyString*, 2, MyStringNew(pContext, "Julius"));

		for (size_t k = 0; k < MyArrayCount(pNames); k++)
		{
			MyString* pName = MyArrayGet(pNames, MyString*, k);
			Console::WriteLine(pName->Chars);
		}
	}
	{
		Console::WriteLine(Console::Color::Green, "Testing array of objects(T = Complex)");

		MyArrayShape shape = { 3ul, 0ul, 0ul, 0ul };
		MyArray* pNums = MyArrayNew(pContext, My_Defaults.ComplexStruct, shape);
		for (size_t k = 0; k < MyArrayCount(pNums); k++)
		{
			const Complex Z = Complex{ Random::Float(1, 10), Random::Float(1, 10) };
			Console::WriteLine("Complex{ %.4g, %.4gi }", Z.Real, Z.Imag);
			MyArraySet2(pNums, Complex, k, Z);
		}

		for (size_t k = 0; k < MyArrayCount(pNums); k++)
		{
			const Complex& Z = MyArrayGet(pNums, Complex, k);
			Console::WriteLine("Complex{ %.4g, %.4gi }", Z.Real, Z.Imag);
		}
	}
	{
		Console::WriteLine(Console::Color::Green, "Testing object(T = File)");

		MyObject* pFile = MyObjectNew(pContext, My_Defaults.FileStruct);
		{
			const uint64_t kHandle = Random::Uint();
			MyString* pPath = MyStringNew(pContext, __FILE__);
			Console::WriteLine("File{ %I64u, %s }", kHandle, pPath->Chars);
			MyObjectFieldSetValueAs<uint64_t>(pFile, MyObjectGetField(pFile, "Handle"), kHandle);
			MyObjectFieldSetValueAs<MyString*>(pFile, MyObjectGetField(pFile, "Filepath"), pPath);
		}
		{
			const uint64_t& kHandle = MyObjectFieldGetValueAs<uint64_t>(pFile, MyObjectGetField(pFile, "Handle"));
			const MyString* const& pPath = MyObjectFieldGetValueAs<MyString*>(pFile, MyObjectGetField(pFile, "Filepath"));
			Console::WriteLine("File{ %I64u, %s }", kHandle, pPath->Chars);
		}
	}
	{
		Console::WriteLine(Console::Color::Green, "Testing type creation & instantiation(T = MyData)");

		MyStruct* pMyDataStruct = MyStructCreate(pContext, "MyData", MY_STRUCT_ATTR_NONE);
		MyStructAddFieldAutoOffset(pMyDataStruct, "ID", My_Defaults.UintType, My_Defaults.UintStruct);
		MyStructAddFieldAutoOffset(pMyDataStruct, "Key", My_Defaults.StringType, My_Defaults.StringStruct);
		MyStructAddFieldAutoOffset(pMyDataStruct, "W", My_Defaults.ComplexType, My_Defaults.ComplexStruct);
		MyStructAddFieldAutoOffset(pMyDataStruct, "Z", My_Defaults.ComplexType, My_Defaults.ComplexStruct);
		Console::WriteLine("struct ['%s'] MyData (%uB):", pMyDataStruct->Guid.AsString(), pMyDataStruct->Size);
		{
			for (size_t k = 0; k < stbds_arrlenu(pMyDataStruct->Fields); k++)
			{
				const MyField& field = pMyDataStruct->Fields[k];
				Console::WriteLine("   - %.2u %s [%s]", field.Offset, field.Name, field.Klass->Name);
			}
		}

		MyObject* pMyData = MyObjectNew(pContext, pMyDataStruct);
		{
			const uint64_t kId = Random::Uint();
			MyString* pKey = MyStringNew(pContext, pMyDataStruct->Guid.AsString());

			MyObjectFieldSetValueAs<uint64_t>(pMyData, MyObjectGetField(pMyData, "ID"), kId);
			MyObjectFieldSetValueAs<MyString*>(pMyData, MyObjectGetField(pMyData, "Key"), pKey);
			MyObjectFieldSetValueAs<Complex>(pMyData, MyObjectGetField(pMyData, "W"), Complex{ 1.234, 5.678 });
			MyObjectFieldSetValueAs<Complex>(pMyData, MyObjectGetField(pMyData, "Z"), Complex{ 9.123, 4.567 });
		}
		{
			const uint64_t& kId = MyObjectFieldGetValueAs<uint64_t>(pMyData, MyObjectGetField(pMyData, "ID"));
			MyString* const& pKey = MyObjectFieldGetValueAs<MyString*>(pMyData, MyObjectGetField(pMyData, "Key"));
			const auto& [Wx, Wy] = MyObjectFieldGetValueAs<Complex>(pMyData, MyObjectGetField(pMyData, "W"));
			const auto& [Zx, Zy] = MyObjectFieldGetValueAs<Complex>(pMyData, MyObjectGetField(pMyData, "Z"));
			Console::WriteLine("MyData{ %I64u, '%s', Complex{ %.4g, %.4g }, Complex{ %.4g, %.4g } }", kId, pKey->Chars, Wx, Wy, Zx, Zy);
}
	}
	Console::WriteLine();
}
