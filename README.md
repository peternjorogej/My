# **My**
(Still searching for a better name😩)

An attempt to write my own fully functional, statically-typed, compiled programming language (I am still a long way off though).
I always wanted to know how programming languages are made and I
decided to do it just as a fun project.
The language is (kind of) inspired by C# and Mono, and a little bit of Python.

I have learnt a lot of concepts, most of which I give credit to [Immo Landwerth](https://www.youtube.com/@ImmoLandwerth) and
his [Building a Compiler](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y) playlist on YouTube.

At the moment, the language is usable but lacks many, many useful features.

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
var int iMyValue = 0;
var uint kMyValue = 1u;
var float fMyValue; // Defaults to zero
const string Greeting = "Hello, World"; // Readonly
// Struct types
var MyType mt = new MyType();
/* Array types (still incomplete, indexing higher dimensions
does not work as intended */
var float[] data = new float[5]();
//   - 2D (representing a 3x3 matrix)
var float[3, 3] m33 = new float[3, 3](); // Indexing: m33[i, j]

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

4. Typedefs
```C#
// New name for a 4x4 matrix of floats
using Matrix4x4F = float[4, 4];

function int Main(string[] Args)
{
    // ...
    var Matrix4x4F m = new Matrix4x4F();
    // indexing into the array: m[i, j]
    // ...
}

```

5. Forward Declarations - forward declared structs are defined natively (kind of like internal functions)
```C#
// Defined with a single field 'Dummy' of type int
struct FwdDecl;

function int Main(string[] Args)
{
    // ...
    var FwdDecl fd = new FwdDecl();
    // access fd.Dummy as usual
    // ...
}

```

6. Structs
```C#
// POD (Plain Old Data) structs contain no reference types, and *should* be
// stack allocated (not working at the moment)
struct pod Float2
{
    // Both default to zero
    var float X;   
    var float Y;
};

// Non-POD structs are heap allocated (currently all structs are heap allocated)
struct Person
{
    // When the struct is instantaited, the memory is zero-initialized;
    // so trying to access reference type members will result in crashes
    var int    YOB; 
    var string ID; 
    var string Name; 

    // Structs can have methods (API not complete)
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

    const char* lpPath = "your/path/to/script.my";
    std::initializer_list<InternalFunction> internals = 
    {
        // ...
    };
	MyAssembly* pAss = Compiler::Build(pCtx, lpPath, internals);
	if (pAss)
	{
		// MyDecompile(pAss); // To see what the emitted bytecode does
		int64_t iResult = Compiler::Run(pCtx, pAss, iArgc, ppArgv);
        Console::Color color = Console::Color::Green;
        if (iResult != 0ll)
        {
            color = Console::Color::Red;
        }

        Console::WriteLine(color, "\nProgram exited with code %I64d\n", iResult);
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
- [ ] Proper implementation of array types
- [ ] Type casting from `object` type to user defined types
- [ ] Importing files (compiled or not)
- [ ] Const-correctness
- [ ] Better string management
- [ ] More functionality (builtin functions etc)
- [ ] Constant folding
- [ ] Function and operator overloading
- [ ] OOP
	- [ ] Methods (Constructors included)
    - [ ] ToString(), Copy(), Write()
	- [ ] Inheritance and Polymorphism
	- [ ] Encapsulation (*visibility*)
- [ ] Default arguments and variadic functions
- [ ] Better implementation of compile-time functions (static_assert etc)
- [ ] Lambdas
- [ ] Templates (or *Generics*)
- [ ] Stack allocation of pod structs
- [ ] Shift to using a register-based VM
- [ ] Support for 32-bit types
- [ ] Better garbage collection algorithm(s)
- [ ] Proper scripting API
    - [ ] Better internal function calls
    - [ ] FFI (with C/C++)
    - [ ] Easy, non-complicated interface
- [ ] Optimization
    - [ ] Codegen
    - [ ] Inlining
- [ ] API for debugging
- [ ] Reflection
- [ ] Rewrite in C (usability in both C and C++)
