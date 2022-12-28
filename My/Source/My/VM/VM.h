#pragma once

#include "My/Base/Core.h"
#include "My/Object.h"

#define MY_ENCODE_OBJECT_INFO(flags, type, count) flags |= (count << 8u) | uint8_t(type)
#define MY_DECODE_OBJECT_INFO(flags, type, count) count = (flags & 0xFFFFFF00) >> 8u, type = (flags & 0x000000FF)

#define MY_STACK_MAX 1024u * 1024u // 1MB

struct MyGC
{
    struct RuntimeAllocation
    {
        void*        Object = nullptr;
        MyValueKind Kind   = MyValueKind::Invalid;
        bool         Marked = false;
    };

    using MemoryMap = Pair<void*, RuntimeAllocation>;

    MyVM*     VM              = nullptr;
    MemoryMap* Allocations     = nullptr;
    uint32_t   AllocationCount = 0u;
    bool       IsEnabled       = true;

    MyGC();
    ~MyGC() noexcept;

    void Run() noexcept;
    void Mark(uint64_t& kAddress) noexcept;
    void Mark() noexcept;
    void Collect() noexcept;

    MyString*  CreateString(const char* lpString, size_t kLength = 0ull) noexcept;
    MyArray*   CreateArray(MyStruct* Klass, const MyArrayStride& Stride, size_t kCapacity) noexcept;
    MyObject*  CreateObject(MyStruct* pKlass) noexcept;
    void        DestroyObject(MyValueKind Kind, void* pMemory) noexcept;
    
    uint32_t GetAllocationCount() const noexcept;
};


struct MyStack
{
    using Map = Pair<uint32_t, Byte*>;

    Map*     Locals = nullptr;
    Byte*    Stack  = nullptr;
    uint32_t SP     = 0ul; 

    MyStack();
    ~MyStack() noexcept = default;

    void       Push(const MyValue& Value);
    void	   Push(bool Value);
    void	   Push(int64_t Value);
    void	   Push(uint64_t Value);
    void	   Push(double Value);
    void	   Push(MyString* Value);
    void	   Push(MyArray* Value);
    void	   Push(MyObject* Value);
    int64_t    PopI64();
    uint64_t   PopU64();
    double     PopF64();
    MyString*  PopString();
    MyArray*   PopArray();
    MyObject*  PopObject();

    void Clear() noexcept; // Only called within VM

    template<typename Tp>
    inline Tp& Top() noexcept
    {
        return *reinterpret_cast<Tp*>(&Stack[SP - sizeof(Tp)]);
    }
};


struct FunctionCallInfo
{
    MyInstruction* Caller = nullptr;
    MyStack        Locals = {};
#ifdef MY_DEBUG
    char*          Name = "";

    FunctionCallInfo(MyInstruction* pCaller, char* const& lpName);
#endif // QUACK_DEBUG

    FunctionCallInfo(MyInstruction* pCaller);
    ~FunctionCallInfo() noexcept {};
};


struct MyVM
{
    enum class ExecutionMode
    {
        SingleFunction,
        WholeProgram,
    };

    struct CommandLineArgs
    {
        MyArray* Argv = nullptr;
        int64_t   Argc = 0;
    };

    MyContext*       Context   = nullptr;
    MyInstruction*   IP        = nullptr;
    MyAssembly*      Assembly  = nullptr;
    MyStack          Globals   = {};
    MyStack          Stack     = {};
    // uint8_t           Stack[MY_STACK_MAX] = {};
    // uint8_t*          SP        = nullptr;
    FunctionCallInfo* CallStack = nullptr;
    CommandLineArgs   Args      = {};
    MyGC             GC        = {};
    ExecutionMode     Mode      = ExecutionMode::WholeProgram;

    MyVM() = default;
    ~MyVM() noexcept = default;

    void Prepare(MyAssembly* const& pAssembly, int iArgc, char* const* ppArgv);
    void Prepare(const std::string& AssemblyPath, int iArgc, char* const* ppArgv);

    int64_t  Run();
    int64_t  Execute(bool& bRunning);
    
    const uint8_t*          GetStack()     const noexcept;
    const FunctionCallInfo* GetCallStack() const noexcept;

    bool Decompile();

    static int64_t Invoke(MyContext* pContext, const char* lpFunction, const List<MyValue>& Argv);
};
