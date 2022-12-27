# **My**
(Still searching for a better name😩; it might also look a bit *derivative*, my apologies in advance😅)

An attempt to write my own fully functional, statically-typed programming language (I am still a long way off though).

At the moment it is usable but lacks many, many features.

## Basic Syntax
1. Comments
```C#
// A single line comment

/* A
Multiline
Comment */
```
2. Variable Declaration
```C#
// Basic types
var int iMyvalue = 0;
var uint kMyValue = 1u;
var float fMyvalue; // Defaults to zero
const string Greeting = "Hello, World"; // Readonly
// Struct types
var MyType mt = new MyType();
// Array types (still incomplete)
var float[] data = new float[5]();
```

3. Function Definition
```C#
function void DoNothing()
{
    // ...
}

function int Main(string[] Args)
{
    // ...
    return 0;
}
```

4. Forward Declarations - forward declared structs are defined natively
```C++
// ...
MyStruct* pKlass = MyStructCreate(pCtx, "FwdDecl", MY_STRUCT_ATTR_NONE);
MyStructAddFieldAutoOffset(pKlass, "Dummy", My_Defaults.IntType, My_Defaults.IntStruct, MY_FIELD_ATTR_NONE); // At least one field must be defined!
// ...
MyAssembly* pAss = Compiler::Build(pCtx, "path/to/script.ns", {}, { pKlass });
// ...
```
then on the scripting side:
```C#
struct FwdDecl;

function int Main(string[] Args)
{
    // ...
    var FwdDecl fd = new FwdDecl();
    // access fd.Dummy as usual
    // ...
}

```

5. Structs
```C#
// POD (Plain Old Data) structs contain no reference types, and *should* be stack allocated (not working at the moment)
struct pod Float2
{
    // Both default to zero
    var float X;   
    var float Y;
};

// Non-POD structs are heap allocated (currently all structs are heap allocated)
struct Person
{
    // When the struct is instantaited, the memory is zero-initialized; so trying to access reference type members will result in crashes
    var int    YOB; 
    var string ID; 
    var string Name; 

    // Structs can have methods but they are neither bound (i.e typechecked) nor emitted
    function int GetYOB()
    {
        return this.YOB;
    }
};
```

## As a Scripting Language
Still working on that 😅

Basic usage:
```C++
#include <My/Base/IO.h>
#include <My/Object.h>
#include <My/VM/Compiler.h>
#include <My/VM/VM.h>
#include <My/Utils/Utils.h>
#include <Stb/stb_ds.h>

int main(int iArgc, char** ppArgv)
{
	MyContext* pCtx = MyInitialize();

    const char* lpPath = "your/path/to/script.ns";
    std::initializer_list<InternalFunction> internals = 
    {
        // ...
    };
	MyAssembly* pAss = Compiler::Build(pCtx, lpPath, internals);
	if (pAss)
	{
		// MyDecompile(pAss);
		int64_t kResult = Compiler::Run(pCtx, pAss, iArgc, ppArgv);
		{
            Console::Color color = Console::Color::Green;
            if (kResult != 0ll)
            {
                color = Console::Color::Red;
            }

			Console::WriteLine(color, "\nProgram exited with code %I64d\n", kResult);
		}
	}

	MyUninitialize(pCtx);
	return 0;
}
```
We can define a function in C++, that will be called internally:
```C++
void CppFunction_Native(MyContext* pCtx, MyVM* pVM) noexcept
{
    // Remember to pop in reverse order
    int64_t i = pVM->Stack.PopI64();
    MyString* s = pVM->Stack.PopString();
    printf("%s, %I64d", s->Chars, i);
}

int main()
{
    // ...
    std::initializer_list<InternalFunction> internals =
    {
        { "CppFunction", CppFunction_Native, -1 };
    };
    MyAssembly* pAss = Compiler::Build(pCtx, lpFilename, internals);
    // ...
}

```
Then on the scripting side:
```C#
extern function void CppFunction(string s, int i);

function int Main(string[] Args)
{
    CppFunction("Hello", 69); // Prints: Hello, 69
    return 0;
}

```

## The Future?
- Proper implementation of array types
- Const-correctness
- Better string management
- Methods
    - Constructors
    - ToString(), Copy(), Write()
    - Overloading  
    - Operator Overloading
- Default arguments and variadic functions
- Better implementation of compile-time functions (static_assert etc)
- Lambdas
- Inheritance, Polymorphism, Encapsulation (visibility)
- Templates (or *Generics*)
- Stack allocation of pod structs
- Shift to using a register-based VM
- Support for 32-bit types
- Better garbage collection
- Proper scripting API
    - FFI (C/C++)
    - Better internal function calls, FFI (with C/C++)
    - Easy, non-complicated interface
- Optimization
    - Codegen
    - Inlining
- API for debugging
- Reflection
- Rewrite in C (usability in both C and C++)
