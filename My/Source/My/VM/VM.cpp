#include "VM.h"
#include "Emitter.h"
#include "My/My.h"
#include "My/Base/IO.h"
#include "My/Utils/Utils.h"
#include "Stb/stb_ds.h"

#include <unordered_set>

#ifdef MY_DEBUG
  #define _My_VM_CheckOverflow(__stk)  if (__stk.SP == MY_STACK_MAX) { return MY_RC_STACK_OVERFLOW; }
  #define _My_VM_CheckUnderflow(__stk, __n) if (__stk.SP < __n) { return MY_RC_STACK_UNDERFLOW; }
#else
  #define _My_VM_CheckOverflow(__stk)
  #define _My_VM_CheckUnderflow(__stk, __n)
#endif // MY_DEBUG

#pragma region Garbage_Collector
MyGC::MyGC()
{
    static constexpr RuntimeAllocation ra = { nullptr, MyValueKind::Invalid, false };
    static constexpr MemoryMap mm = { nullptr, ra };
    stbds_hmdefault(Allocations, ra);
    stbds_hmdefaults(Allocations, mm);
}

MyGC::~MyGC() noexcept
{
    Collect();
    AllocationCount = 0u;
    MY_ASSERT(stbds_hmlenu(Allocations) == 0u, "FatalError: Failed to collect all the objects");

    stbds_hmfree(Allocations);
    Allocations = nullptr;
}

void MyGC::Run() noexcept
{
    if (!IsEnabled)
    {
        return;
    }

    // STAGE 1: MARK
    Mark();

    //// STAGE 2: SWEEP
    Collect();

    //// STAGE 3: UNMARK
    for (size_t k = 0; k < stbds_hmlenu(Allocations); k++)
    {
        auto&[pObj, ra] = Allocations[k];
        ra.Marked = false;
    }

    //// STAGE 4: RESET
    AllocationCount = 0u;
}

void MyGC::Mark(uint64_t& kAddress) noexcept
{
    const void* pObject = (void*)kAddress;
    RuntimeAllocation& ra = stbds_hmget(Allocations, pObject);
    
    if (!ra.Object || ra.Kind == MyValueKind::Invalid)
    {
        // This is not reference object since it is not in the list of allocations (i.e not heap allocated)
        return;
    }

    if (ra.Marked)
    {
        return;
    }
    
    ra.Marked = true;

    switch (ra.Kind)
    {
        case MyValueKind::String:
            break;
        case MyValueKind::Array:
        {
            MyArray* const& pArray = (MyArray*)ra.Object;
            if (MyStructIsReference(pArray->Object.Klass))
            {
                for (size_t k = 0; k < MyArrayCount(pArray); k++)
                {
                    uint64_t& kItem = MyArrayGet(pArray, uint64_t, k);
                    Mark(kItem);
                }
            }
            break;
        }
        case MyValueKind::Object:
        {
            MyObject* const& pObject = (MyObject*)ra.Object;
            for (size_t k = 0; k < stbds_arrlenu(pObject->Klass->Fields); k++)
            {
                MyField* const& pField = pObject->Klass->Fields + k;
                if (MyStructIsReference(pField->Klass))
                {
                    uint64_t& kData = MyObjectFieldGetValueAs<uint64_t>(pObject, pField);
                    Mark(kData);
                }
            }
            break;
        }
        default: break;
    }
}

void MyGC::Mark() noexcept
{
    // Evaluation stack
    {
        constexpr size_t kStep = sizeof(uint64_t);
        const size_t kCount = VM->Stack.SP / kStep;

        for (size_t k = 0; k < kCount; k++)
        {
            Byte* const& pAddress = VM->Stack.Stack + (k * kStep);
            Mark(*reinterpret_cast<uint64_t*>(pAddress));
        }
    }

    // Call stacks (locals)
    for (size_t k = 0; k < stbds_arrlenu(VM->CallStack); k++)
    {
        FunctionCallInfo& fci = VM->CallStack[k];
        for (size_t i = 0; i < stbds_hmlenu(fci.Locals.Locals); i++)
        {
            const auto& [index, addr] = fci.Locals.Locals[i];
            Mark(*reinterpret_cast<uint64_t*>(addr));
        }
    }

    // Globals
    for (size_t k = 0; k < stbds_shlenu(VM->Globals.Locals); k++)
    {
        const auto& [index, addr] = VM->Globals.Locals[k];
        Mark(*reinterpret_cast<uint64_t*>(addr));
    }

    // Constants
    for (size_t k = 0; k < stbds_arrlenu(VM->Assembly->Constants); k++)
    {
        MyValue& value = VM->Assembly->Constants[k];
        switch (value.Kind)
        {
            case MyValueKind::String:
            case MyValueKind::Array:
            case MyValueKind::Object:
                Mark(value.U64);
                break;
            default: break;
        }
    }
}

void MyGC::Collect() noexcept
{
#ifdef MY_DEBUG
    size_t kAllocationsBefore = stbds_hmlenu(Allocations);
#endif // MY_DEBUG

    // STAGE 1: COLLECT
    std::unordered_set<void*> Garbage = {};
    
    for (size_t k = 0; k < stbds_hmlenu(Allocations); k++)
    {
        auto& [pObject, Alloc] = Allocations[k];
        if (!Alloc.Marked)
        {
            MY_ASSERT(pObject == Alloc.Object, "Error: Mismatched objects");
            Garbage.emplace(pObject);
        }
    }
    // STAGE 2: SWEEP
    // for (size_t k = 0; k < Garbage.size(); k++)
    for (void* pMemory : Garbage)
    {
        RuntimeAllocation& ra = stbds_hmget(Allocations, pMemory);
        DestroyObject(ra.Kind, pMemory);
        memset(&ra, 0, sizeof(RuntimeAllocation));

        stbds_hmdel(Allocations, pMemory);
    }
    Garbage.clear();

#ifdef MY_DEBUG
    const size_t kAllocations = stbds_hmlenu(Allocations);
    size_t kCollectedAllocations = kAllocationsBefore - kAllocations;

    Console::WriteLine(Console::Color::Cyan, "[DEBUG]: Collected %I64u object(s); %I64u objects remaining.", kCollectedAllocations, kAllocations);
#endif // MY_DEBUG
}

MyString* MyGC::CreateString(const char* lpString, size_t kLength) noexcept
{
    const size_t kFullLength = sizeof(MyString) + kLength + 1ull; // +1 To account for NTC
    char* pBuffer = new char[kFullLength] {};
    {
        const RuntimeAllocation ra = { pBuffer, MyValueKind::String, false };
        stbds_hmput(Allocations, ra.Object, ra);
        AllocationCount++;
    }

    MyString* pString = reinterpret_cast<MyString*>(pBuffer);
    pString->Object.Klass = My_Defaults.StringStruct;
    pString->Object.Data  = reinterpret_cast<Byte*>(pBuffer);
    pString->Hash = MyHashBytes(lpString, kLength, 0ul);
    pString->Length = kLength;
    pString->Chars = pBuffer + sizeof(MyString);
    strncpy(pString->Chars, lpString, kLength);
    pString->Chars[kLength] = 0;

    return pString;
}

MyArray* MyGC::CreateArray(MyStruct* Klass, const MyArrayStride& Stride, size_t kCapacity) noexcept
{
    const size_t kBufferSize = sizeof(MyArray) + (Klass->Size*kCapacity);

    Byte* pBuffer = reinterpret_cast<Byte*>(::operator new(kBufferSize));
    memset(pBuffer, 0, kBufferSize);
    {
        const RuntimeAllocation ra = { pBuffer, MyValueKind::Array, false };
        stbds_hmput(Allocations, ra.Object, ra);
        AllocationCount++;
    }

    MyArray* pArray = reinterpret_cast<MyArray*>(pBuffer);
    pArray->Object.Klass = Klass;
    pArray->Object.Data  = pBuffer;
    pArray->Capacity = kCapacity;
    pArray->Stride   = Stride;
    pArray->Data     = pBuffer + sizeof(MyArray);

    const uint32_t* pStrides = reinterpret_cast<const uint32_t*>(&Stride);
    for (size_t k = 0; k < 8; k++)
    {
        if (pStrides[k] != 0ull)
        {
            pArray->Rank++;
        }
    }

    return pArray;
}

MyObject* MyGC::CreateObject(MyStruct* pKlass) noexcept
{
    const size_t kBufferSize = sizeof(MyObject) + pKlass->Size;
    Byte* pBlock = reinterpret_cast<Byte*>(::operator new(kBufferSize));
    memset(pBlock, 0, kBufferSize);
    {
        const RuntimeAllocation ra = { pBlock, MyValueKind::Object, false };
        stbds_hmput(Allocations, ra.Object, ra);
        AllocationCount++;
    }

    MyObject* pObject = reinterpret_cast<MyObject*>(pBlock);
    pObject->Klass = pKlass;
    pObject->Data  = pBlock + sizeof(MyObject);

    return pObject;
}

void MyGC::DestroyObject(MyValueKind Kind, void* pMemory) noexcept
{
    switch (Kind)
    {
        case MyValueKind::String:
        {
            MyString* pString = reinterpret_cast<MyString*>(pMemory);
            DebugLog::Info("[Collect:String] '%s'", pString->Chars);
            // Remove it from the map of cached strings before deleting string object
            stbds_hmdel(VM->Context->RtCache, pString->Hash);
            MY_ASSERT(stbds_hmget(VM->Context->RtCache, pString->Hash) == nullptr, "String NOT deleted (%s)", pString->Chars);
            delete[] reinterpret_cast<char*>(pMemory);
            break;
        }
        case MyValueKind::Array:
        {
            MyArray* pArray = reinterpret_cast<MyArray*>(pMemory);
            DebugLog::Info("[Collect:Array] Array(0x%p, %I64u)", pArray->Data, MyArrayCount(pArray));
            // Free items before deleting array object
            // No need to free since we allocated the block, stbds_arrfree(pArray->Items);
            ::operator delete(pMemory);
            break;
        }
        case MyValueKind::Object:
        {
            MyObject* pObject = reinterpret_cast<MyObject*>(pMemory);
            DebugLog::Info("[Collect:Instance] struct ['%s'] %s", pObject->Klass->Guid.AsString(), pObject->Klass->Name);
            // Nothing to do *?
            ::operator delete(pMemory);
            break;
        }
        default: break;
    }
}

uint32_t MyGC::GetAllocationCount() const noexcept
{
    return AllocationCount;
}
#pragma endregion

#pragma region Runtime_Stack
MyStack::MyStack()
{
    Locals = nullptr;
    Stack  = new Byte[MY_STACK_MAX]{ 0u };
    SP     = 0ul;
    
    memset(Stack, 0, MY_STACK_MAX);
}

void MyStack::Push(const MyValue& Value)
{
    switch (Value.Kind)
    {
        case MyValueKind::Bool:    Push(Value.B08); break;
        case MyValueKind::Int64:   Push(Value.I64); break;
        case MyValueKind::Uint64:  Push(Value.U64); break;
        case MyValueKind::Float64: Push(Value.F64); break;
        case MyValueKind::String:  Push(Value.Str); break;
        case MyValueKind::Array:   Push(Value.Arr); break;
        case MyValueKind::Object:  Push(Value.Obj); break;
        default: break;
    }

    MY_ASSERT(false, "Invalid value kind (%u) '%s'", Value.Kind, ValueKindString(Value.Kind));
}

template<typename Tp>
static inline void PushX(MyStack* const& pScope, const Tp& Value) noexcept
{
    static_assert(std::is_standard_layout<Tp>::value || std::is_arithmetic<Tp>::value);
    static constexpr size_t kSize = sizeof(Tp);

    union { uint8_t I8[kSize]; Tp Value; } u;
    u.Value = Value;

    memcpy(pScope->Stack + pScope->SP, u.I8, kSize);
    pScope->SP += kSize;
}

template<typename Tp>
static inline Tp PopX(MyStack* const& pScope) noexcept
{
    static_assert(std::is_standard_layout<Tp>::value || std::is_arithmetic<Tp>::value);
    pScope->SP -= sizeof(Tp);
    return *reinterpret_cast<Tp*>(pScope->Stack + pScope->SP);
}

void MyStack::Push(bool Value)
{
    PushX<int64_t>(this, Value);
}

void MyStack::Push(int64_t Value)
{
    PushX(this, Value);
}

void MyStack::Push(uint64_t Value)
{
    PushX(this, Value);
}

void MyStack::Push(double Value)
{
    PushX(this, Value);
}

void MyStack::Push(MyString* Value)
{
    PushX(this, (uint64_t)Value);
}

void MyStack::Push(MyArray* Value)
{
    PushX(this, (uint64_t)Value);
}

void MyStack::Push(MyObject* Value)
{
    PushX(this, (uint64_t)Value);
}

int64_t MyStack::PopI64()
{
    return PopX<int64_t>(this);
}

uint64_t MyStack::PopU64()
{
    return PopX<uint64_t>(this);
}

double MyStack::PopF64()
{
    return PopX<double>(this);
}

MyString* MyStack::PopString()
{
    return (MyString*)PopX<uint64_t>(this);
}

MyArray* MyStack::PopArray()
{
    return (MyArray*)PopX<uint64_t>(this);
}

MyObject* MyStack::PopObject()
{
    return (MyObject*)PopX<uint64_t>(this);
}

void MyStack::Clear() noexcept
{
    stbds_hmfree(Locals);
    MY_SAFEDELETE(Stack);
    SP = 0ul;
}
#pragma endregion

#ifdef MY_DEBUG
FunctionCallInfo::FunctionCallInfo(MyInstruction* pCaller, char* const& lpName)
    : Caller(pCaller), Locals(), Name(lpName)
{ }
#endif // MY_DEBUG

FunctionCallInfo::FunctionCallInfo(MyInstruction* pCaller)
    : Caller(pCaller), Locals()
{ }


#pragma region Virtual_Machine
#pragma region Virtual_Machine_Utilities
#define _My_VM_BinaryOp(__name, __op) \
    template<typename Number>                                     \
    static inline void __name(MyVM* pVM) noexcept                \
    {                                                             \
        uint64_t Rhs = pVM->Stack.PopU64();                       \
        uint64_t Lhs = pVM->Stack.PopU64();                       \
                                                                  \
        if constexpr (std::is_integral<Number>::value)            \
        {                                                         \
            return pVM->Stack.Push(Lhs __op Rhs);                 \
        }                                                         \
        else                                                      \
        {                                                         \
            const double dLhs = *reinterpret_cast<double*>(&Lhs); \
            const double dRhs = *reinterpret_cast<double*>(&Rhs); \
            return pVM->Stack.Push(dLhs __op dRhs);               \
        }                                                         \
    }

_My_VM_BinaryOp(Add, +)
_My_VM_BinaryOp(Sub, -)
_My_VM_BinaryOp(Mul, *)
_My_VM_BinaryOp(Div, /)
_My_VM_BinaryOp(Mod, %)
_My_VM_BinaryOp(And, &)
_My_VM_BinaryOp(Or, |)
_My_VM_BinaryOp(Xor, ^)
_My_VM_BinaryOp(Lsh, <<)
_My_VM_BinaryOp(Rsh, >>)
_My_VM_BinaryOp(Eq, ==)
_My_VM_BinaryOp(Neq, !=)
_My_VM_BinaryOp(Lt, <)
_My_VM_BinaryOp(Lte, <=)
_My_VM_BinaryOp(Gt, >)
_My_VM_BinaryOp(Gte, >=)

static uint64_t uni_pow(uint64_t base, uint64_t exponent) noexcept
{
    return uint64_t(pow(double(base), double(exponent)));
}

template<typename Number>
static inline void Pow(MyVM* pVM) noexcept
{
    static_assert(std::is_arithmetic<Number>::value, "Pow<T>(MyVM*) expects T to be numeric");

    const uint64_t Rhs = pVM->Stack.PopU64();
    const uint64_t Lhs = pVM->Stack.PopU64();
    
    if constexpr (std::is_integral<Number>::value)
    {
        return pVM->Stack.Push(uni_pow(Lhs, Rhs));
    }
    else                                      
    {
        const double dLhs = *reinterpret_cast<const double*>(&Lhs);
        const double dRhs = *reinterpret_cast<const double*>(&Rhs);
        return pVM->Stack.Push(pow(dLhs, dRhs));
    }                                         
}
#pragma endregion

static uint32_t s_CollectorThreshold = 50ul;
static uint32_t s_RecursionDepth     = 0ul;
static uint32_t s_MaxCallStackSize   = 1'000ul;
static uint32_t s_MaxRecursionLimit  = 1'000ul;


void MyVM::Prepare(MyAssembly* const& pAssembly, int iArgc, char* const* ppArgv)
{
    Assembly = pAssembly;
    IP = &Assembly->Code[0];

    Args.Argc = iArgc;
    Args.Argv = MyArrayNew(Context, My_Defaults.StringStruct, MyArrayStride(iArgc), (size_t)iArgc);

    for (size_t k = 0; k < (size_t)Args.Argc; k++)
    {
        MyString* pArg = MyStringNew(Context, ppArgv[k]);
        MyArraySet(Args.Argv, MyString*, k, pArg);
    }
}

void MyVM::Prepare(const std::string& AssemblyPath, int iArgc, char* const* ppArgv)
{
    MyAssembly* pAssembly = new MyAssembly{};
    if (MyBytecodeProcessor::Load(pAssembly, AssemblyPath))
    {
        Prepare(pAssembly, iArgc, ppArgv);
    }
    else
    {
        MY_ASSERT(false, "Assembly @ '%s' failed to load", AssemblyPath.c_str());
    }
}

int64_t MyVM::Run()
{
    static const auto now = []() noexcept -> double
    {
        return double(clock()) / double(CLOCKS_PER_SEC);
    };

    // Command line arguments
    Stack.Push(Args.Argv);

    bool    bRunning    = true;
    int64_t iReturnCode = 0ll;

    while (bRunning)
    {
        if (GC.GetAllocationCount() >= s_CollectorThreshold)
        {
            const double start = now();
            GC.Run();
            const double dur = (now() - start) * 1000.0;
            Console::WriteLine(Console::Color::DarkMagenta, "GC.Run() took %1.10gms", dur);
        }

        iReturnCode = Execute(bRunning);

        if (iReturnCode != MY_RC_SUCCESS)
        {
            return iReturnCode;
        }
    }

    return Stack.PopI64();
}

int64_t MyVM::Execute(bool& bRunning)
{
    switch (IP->Code)
    {
#pragma region Stack_Operations
        case MyOpCode::Ldc:
        {
            _My_VM_CheckOverflow(Stack);
            Stack.Push(Assembly->Constants[IP->Arg0].U64);
            IP++;
            break;
        }
        case MyOpCode::Ldcf:
        {
            _My_VM_CheckOverflow(Stack);
            Stack.Push(Assembly->Constants[IP->Arg0].F64);
            IP++;
            break;
        }
        case MyOpCode::Ldstr:
        {
            _My_VM_CheckOverflow(Stack);
            Stack.Push(Assembly->Constants[IP->Arg0].Str);
            IP++;
            break;
        }
        case MyOpCode::Ldarg:
        case MyOpCode::Stloc:
        {
            MyStack& scope = stbds_arrlast(CallStack).Locals;
            _My_VM_CheckUnderflow(Stack, 8u);
            _My_VM_CheckOverflow(scope);
            
            stbds_hmput(scope.Locals, IP->Arg0, &scope.Stack[scope.SP]);
            scope.Push(Stack.PopU64());
            
            IP++;
            break;
        }
        case MyOpCode::Ldloc:
        {
            _My_VM_CheckOverflow(Stack);

            MyStack& scope = stbds_arrlast(CallStack).Locals;
            Byte* pAddress = stbds_hmget(scope.Locals, IP->Arg0);
            Stack.Push(*reinterpret_cast<uint64_t*>(pAddress));

            IP++;
            break;
        }
        case MyOpCode::Stglo:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            _My_VM_CheckOverflow(Globals);

            stbds_hmput(Globals.Locals, IP->Arg0, &Globals.Stack[Globals.SP]);
            Globals.Push(Stack.PopU64());
            
            IP++;
            break;
        }
        case MyOpCode::Ldglo:
        {
            _My_VM_CheckOverflow(Stack);

            Byte* pAddress = stbds_hmget(Globals.Locals, IP->Arg0);
            Stack.Push(*reinterpret_cast<uint64_t*>(pAddress));

            IP++;
            break;
        }
        case MyOpCode::Pop:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            Stack.PopU64();
            IP++;
            break;
        }
        case MyOpCode::Dup:
        {
            _My_VM_CheckOverflow(Stack);
            Stack.Push(Stack.Top<uint64_t>());
            IP++;
            break;
        }
#pragma endregion
#pragma region DataStructure_Operations
        case MyOpCode::Newobj:
        {
            MyObject* const& pObject = MyObjectNew(Context, Assembly->Klasses[IP->Arg0]);
            Stack.Push(pObject);
            IP++;
            break;
        }
        case MyOpCode::Newarray:
        {
            MY_NOT_IMPLEMENTED();
            IP++;
            break;
        }
        case MyOpCode::Ldarr:
        {
            // TODO: Will come back to this later (improvement)
            uint8_t  kType = 0u;
            uint32_t kSize = 0ul;
            MY_DECODE_OBJECT_INFO(IP->Arg0, kType, kSize);

            _My_VM_CheckUnderflow(Stack, (8u*kSize));

            MyArray* pArray = MyArrayNew(Context, My_Defaults.ObjectStruct, MyArrayStride(kSize), kSize);
            for (size_t k = 0; k < kSize; k++)
            {
                const size_t kIndex = kSize - k - 1ul;
                MyArraySet2(pArray, uint64_t, kIndex, Stack.PopU64());
            }
            Stack.Push(pArray);
            IP++;
            break;
        }
        case MyOpCode::Ldelem:
        {
            // TODO: Will come back to this later (improvement)
            MyArray* const& pArray = Stack.PopArray();
            uint32_t* pStrides = reinterpret_cast<uint32_t*>(&pArray->Stride);

            size_t kIndex = 0ul;
            for (size_t k = IP->Arg0-1; k > 0u; k--)
            {
                kIndex += pStrides[k]*Stack.PopU64();
            }
            kIndex += Stack.PopU64();

            Stack.Push(MyArrayGet(pArray, uint64_t, kIndex));
            IP++;
            break;
        }
        case MyOpCode::Stelem:
        {
            // TODO: Will come back to this later (improvement)
            MyArray* const& pArray = Stack.PopArray();
            uint32_t* pStrides = reinterpret_cast<uint32_t*>(&pArray->Stride);

            size_t kIndex = 0ul;
            for (size_t k = IP->Arg0 - 1; k > 0u; k--)
            {
                kIndex += pStrides[k] * Stack.PopU64();
            }
            kIndex += Stack.PopU64();

            MyArraySet2(pArray, uint64_t, kIndex, Stack.PopU64());
            IP++;
            break;
        }
        case MyOpCode::Ldfld:
        {
            // TODO: Will come back to this later (improvement)
            MyObject* const& pObject = Stack.PopObject();
            MyField* pField = MyObjectGetField(pObject, Assembly->Fields[IP->Arg0].key);
            Stack.Push(MyObjectFieldGetValueAs<uint64_t>(pObject, pField));
            IP++;
            break;
        }
        case MyOpCode::Stfld:
        {
            // TODO: Will come back to this later (improvement)
            MyObject* pObject = Stack.PopObject();
            MyField* pField = MyObjectGetField(pObject, Assembly->Fields[IP->Arg0].key);
            MyObjectFieldSetValueAs<uint64_t>(pObject, pField, Stack.PopU64());
            IP++;
            break;
        }
#pragma endregion
#pragma region Conversions_Operations
        case MyOpCode::Cvtoi:
        case MyOpCode::Cvtou:
        {
            // Value on stack was already on an integer
            IP++;
            break;
        }
        case MyOpCode::Cvtof:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            Stack.Top<double>() = (double)Stack.Top<uint64_t>();
            IP++;
            break;
        }
        case MyOpCode::Cvftoi:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            Stack.Top<int64_t>() = (int64_t)Stack.Top<double>();
            IP++;
            break;
        }
        case MyOpCode::Cvftou:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            Stack.Top<uint64_t>() = (uint64_t)Stack.Top<double>();
            IP++;
            break;
        }
#pragma endregion
#pragma region Arithmetic_Operations
        case MyOpCode::Add:  Add<int64_t>(this); IP++; break;
        case MyOpCode::Sub:  Sub<int64_t>(this); IP++; break;
        case MyOpCode::Mul:  Mul<int64_t>(this); IP++; break;
        case MyOpCode::Div:  Div<int64_t>(this); IP++; break;
        case MyOpCode::Pow:  Pow<int64_t>(this); IP++; break;
        case MyOpCode::Lsh:  Lsh<int64_t>(this); IP++; break;
        case MyOpCode::Rsh:  Rsh<int64_t>(this); IP++; break;
        case MyOpCode::Mod:  Mod<int64_t>(this); IP++; break;

        case MyOpCode::And:  And<int64_t>(this); IP++; break;
        case MyOpCode::Or:   Or <int64_t>(this); IP++; break;
        case MyOpCode::Xor:  Xor<int64_t>(this); IP++; break;
        case MyOpCode::Ceq:  Eq <int64_t>(this); IP++; break;
        case MyOpCode::Cneq: Neq<int64_t>(this); IP++; break;
        case MyOpCode::Clt:  Lt <int64_t>(this); IP++; break;
        case MyOpCode::Clte: Lte<int64_t>(this); IP++; break;
        case MyOpCode::Cgt:  Gt <int64_t>(this); IP++; break;
        case MyOpCode::Cgte: Gte<int64_t>(this); IP++; break;

        case MyOpCode::Addf: Add<double>(this); IP++; break;
        case MyOpCode::Subf: Sub<double>(this); IP++; break;
        case MyOpCode::Mulf: Mul<double>(this); IP++; break;
        case MyOpCode::Divf: Div<double>(this); IP++; break;
        case MyOpCode::Powf: Pow<double>(this); IP++; break;
        case MyOpCode::Ceqf:  Eq <double>(this); IP++; break; // TODO: Floating point comparison: ==
        case MyOpCode::Cneqf: Neq<double>(this); IP++; break; // TODO: Floating point comparison: !=
        case MyOpCode::Cltf:  Lt <double>(this); IP++; break;
        case MyOpCode::Cltef: Lte<double>(this); IP++; break; // TODO: Floating point comparison: <=
        case MyOpCode::Cgtf:  Gt <double>(this); IP++; break;
        case MyOpCode::Cgtef: Gte<double>(this); IP++; break; // TODO: Floating point compariosn: >=
        case MyOpCode::Neg:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            Stack.Push(-Stack.PopI64());
            IP++;
            break;
        }
        case MyOpCode::Negf:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            Stack.Push(-Stack.PopF64());  
            IP++;
            break;
        }
        case MyOpCode::Not:
        {
            _My_VM_CheckUnderflow(Stack, 8u);
            Stack.Push(~Stack.PopU64());
            IP++;
            break;
        }
        case MyOpCode::Inc:
        {
            const int64_t iStep = Stack.PopI64();
            MyStack& scope = stbds_arrlast(CallStack).Locals;
            *reinterpret_cast<int64_t*>(stbds_hmget(scope.Locals, IP->Arg0)) += iStep;
            IP++;
            break;
        }
        case MyOpCode::Incf:
        {
            const double dStep = Stack.PopF64();
            MyStack& scope = stbds_arrlast(CallStack).Locals;
            *reinterpret_cast<double*>(stbds_hmget(scope.Locals, IP->Arg0)) += dStep;
            IP++;
            break;
        }
#pragma endregion
#pragma region Jump_Operations
        case MyOpCode::Jmp:
        {
            if (IP->Arg0 >= stbds_arrlenu(Assembly->Code))
            {
                return MY_RC_INVALID_ADDRESS;
            }
            IP = &Assembly->Code[IP->Arg0];
            break;
        }
        case MyOpCode::Jnz:
        {
            if (Stack.PopU64() != 0ull)
            {
                if (IP->Arg0 >= stbds_arrlenu(Assembly->Code))
                {
                    return MY_RC_INVALID_ADDRESS;
                }
                IP = &Assembly->Code[IP->Arg0];
            }
            else
            {
                IP++;
            }
            break;
        }
        case MyOpCode::Jz:
        {
            if (Stack.PopU64() == 0ull)
            {
                if (IP->Arg0 >= stbds_arrlenu(Assembly->Code))
                {
                    return MY_RC_INVALID_ADDRESS;
                }
                IP = &Assembly->Code[IP->Arg0];
            }
            else
            {
                IP++;
            }
            break;
        }
        case MyOpCode::Label:
        case MyOpCode::Func:
        {
            IP++;
            break;
        }
        case MyOpCode::Call:
        {
            MyInstruction* const pLastIP = stbds_arrlenu(CallStack) == 0u ? nullptr : stbds_arrlast(CallStack).Caller - 1;
            MyInstruction* const pCaller = IP + 1;

    #ifdef MY_DEBUG
            const FunctionCallInfo fci = FunctionCallInfo(pCaller, nullptr);
            stbds_arrpush(CallStack, fci);
    #else
            const FunctionCallInfo fci = FunctionCallInfo(pCaller);
            stbds_arrpush(CallStack, fci);
    #endif // QUACK_DEBUG

            IP = &Assembly->Code[IP->Arg0];

            if (pLastIP && pLastIP->Arg0 == IP->Arg0)
            {
                s_RecursionDepth++;
            }

            if (stbds_arrlenu(CallStack) > s_MaxCallStackSize)
            {
                return MY_RC_CALLSTACK_OVERFLOW;
            }
            if (s_RecursionDepth > s_MaxRecursionLimit)
            {
                return MY_RC_RECURSION_LIMIT_EXCEEDED;
            }
            break;
        }
        case MyOpCode::Calli:
        {
            if (IP->Arg0 >= stbds_shlenu(Assembly->Internals))
            {
                return MY_RC_UNDEFINED_INTERNAL;
            }

            const InternalFunction& nm = Assembly->Internals[IP->Arg0];
            nm.value(Context, this);

            IP++;
            break;
        }
        case MyOpCode::Ret:
        {
            if (s_RecursionDepth > 0ul)
            {
                s_RecursionDepth--;
            }
            if (s_RecursionDepth == 0ul && Mode == ExecutionMode::SingleFunction)
            {
                bRunning = false;
                return MY_RC_SUCCESS;
            }
            else
            {
                FunctionCallInfo& fci = stbds_arrpop(CallStack);
                IP = fci.Caller;
                fci.Locals.Clear();
            }
            break;
        }
#pragma endregion
#pragma region Misc_Operations
        // Others
        case MyOpCode::Nop:
        {
            IP++;
            break;
        }
        case MyOpCode::Halt:
        {
            bRunning = false;
            if (Mode == ExecutionMode::SingleFunction)
            {
                MY_ASSERT(false, "Shouldn't be getting to this point");
                return MY_RC_INVALID_OPERATION;
            }
            break;
        }
#pragma endregion
        default:
            DebugLog::Error("Unknown/Unhandled instruction '%s'", OpCodeString(IP->Code));
            return MY_RC_INVALID_OPCODE;
    } /* end switch */

    return MY_RC_SUCCESS;
}

const uint8_t* MyVM::GetStack() const noexcept
{
    return Stack.Stack;
}

const FunctionCallInfo* MyVM::GetCallStack() const noexcept
{
    return CallStack;
}

bool MyVM::Decompile()
{
    return Assembly ? MyDecompile(Assembly), true : false;
}

int64_t MyVM::Invoke(MyContext* pContext, const char* lpFunction, const List<MyValue>& Argv)
{
    MY_NOT_IMPLEMENTED();

    char* const& lpName = MyGetCachedString(lpFunction);

    uint32_t kAddress = stbds_shget(pContext->Assembly->Functions, lpName);
    if (kAddress == MY_INVALID_ADDR)
    {
        DebugLog::Error("No function '%s' found", lpName);
        return -1ll;
    }

    // Set up VM
    pContext->VM->Assembly = pContext->Assembly;
    // pContext->VM->IP       = pContext->Assembly->Code + kAddress;
    pContext->VM->Mode     = MyVM::ExecutionMode::SingleFunction;
    
    const FunctionCallInfo fci =
#ifdef MY_DEBUG
        { pContext->VM->IP, lpName };
#else
        { pContext->VM->IP };
#endif // MY_DEBUG
    stbds_arrpush(pContext->VM->CallStack, fci);

    for (const MyValue& oArg : Argv)
    {
        pContext->VM->Stack.Push(oArg);
    }

    int64_t iResult  = 0ll;
    bool    bRunning = true;

    while (bRunning)
    {
        iResult = pContext->VM->Execute(bRunning);
        if (iResult != 0ll)
        {
            break;
        }
    }

    // Reset VM (DO NOT Reset the stack, in case a function you call returns)
    pContext->VM->IP   = pContext->Assembly->Code;
    pContext->VM->Mode = MyVM::ExecutionMode::WholeProgram;
    stbds_arrpop(pContext->VM->CallStack);

    // Call GC *AFTER* invoking function
    if (pContext->VM->GC.GetAllocationCount() >= s_CollectorThreshold)
    {
        pContext->VM->GC.Run();
    }

    return iResult;
}
#pragma endregion


void MyDecompile(const MyAssembly* pAssembly) noexcept
{
    if (!pAssembly || !pAssembly->Code)
    {
        return;
    }

    const MyAssembly& Assembly = *pAssembly;

    /*for (size_t k = 0; k < stbds_arrlenu(Assembly.Types); k++)
    {
        const MyTypeInfo& Info = Assembly.Types[k];
        Console::WriteLine("[%I64u] %s%s", k, Info.Kind <= uint8_t(MyValueKind::String) ? "" : "struct ", Info.Name);

        for (size_t i = 0; i < stbds_shlenu(Info.Fields); i++)
        {
            const auto& [lpName, iIndex] = Info.Fields[i];
            const MyTypeInfo& ti = Assembly.Types[iIndex];

            Console::WriteLine("\t- %s (%s)", lpName, ti.Name);
        }
    }
    Console::WriteLine("\n");*/

    std::string Space = "";

    static const auto Indent = [&]() noexcept -> void
    {
        for (int k = 0; k < 4; k++)
        {
            Space.push_back(' ');
        }
    };
    static const auto Dedent = [&]() noexcept -> void
    {
        if (Space.size())
        {
            for (int k = 0; k < 4; k++)
            {
                Space.pop_back();
            }
        }
    };
    static const auto PrintJumpOp = [&](const char* lpOpName, uint32_t kAddress) -> void
    {
        const char* lpLabel = nullptr;

        for (size_t k = 0; k < stbds_shlenu(Assembly.Labels); k++)
        {
            const auto& [label, addr] = Assembly.Labels[k];
            if (addr == kAddress)
            {
                lpLabel = label;
                break;
            }
        }

        Console::Write(Space);
        Console::Write("%s %s(%u)\n", lpOpName, lpLabel, kAddress);
    };

    const char* const lpFormat = stbds_arrlenu(Assembly.Code) < 10'000ul ? "%.4u: " : "%.6u: ";

    uint32_t IP = 0ul;
    while (true)
    {
        Console::Write(lpFormat, IP);

        const MyInstruction& Inst = Assembly.Code[IP++];
        switch (Inst.Code)
        {
            // Stack
            case MyOpCode::Ldc:
                Console::Write("%sldc, ", Space.c_str());
                Assembly.Constants[Inst.Arg0].Print();
                Console::WriteLine();
                break;
            case MyOpCode::Ldcf:
                Console::Write("%sldcf, ", Space.c_str());
                Assembly.Constants[Inst.Arg0].Print();
                Console::WriteLine();
                break;
            case MyOpCode::Ldstr:
                Console::Write("%sldstr, ", Space.c_str());
                Assembly.Constants[Inst.Arg0].Print(true);
                Console::WriteLine();
                break;
            case MyOpCode::Ldarr:
                /*Console::Write("%sldarr, ", Space.c_str());
                Assembly.Constants[Inst.Arg0].Print(true);
                Console::WriteLine();*/
                Console::WriteLine("%sldarr, [%u, %u]", Space.c_str(), (Inst.Arg0 & 0xFFFFFF00)>>8, (Inst.Arg0 & 0x000000FF));
                break;
            case MyOpCode::Ldobj:
                Console::WriteLine("%sldobj, [%u]", Space.c_str(), (Inst.Arg0 & 0xFFFF));
                break;
            case MyOpCode::Dup:
                Console::WriteLine("%sdup", Space.c_str());
                break;
            case MyOpCode::Pop:
                Console::WriteLine("%spop", Space.c_str());
                break;
            case MyOpCode::Ldarg:
                Console::WriteLine("%sldarg, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Ldloc:
                Console::WriteLine("%sldloc, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Stloc:
                Console::WriteLine("%sstloc, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Ldglo:
                Console::WriteLine("%sldglo, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Stglo:
                Console::WriteLine("%sstglo, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Newarray:
                Console::WriteLine("%snewarray, %I64u", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Newobj:
                Console::WriteLine("%snewobj, [%s]", Space.c_str(), Assembly.Klasses[Inst.Arg0]->Name);
                break;
            case MyOpCode::Ldelem:
                Console::WriteLine("%sldelem, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Stelem:
                Console::WriteLine("%sstelem, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
            case MyOpCode::Ldfld:
                Console::WriteLine("%sldfld, %s", Space.c_str(), Assembly.Fields[Inst.Arg0].key);
                break;
            case MyOpCode::Stfld:
                Console::WriteLine("%sstfld, %s", Space.c_str(), Assembly.Fields[Inst.Arg0].key);
                break;
            case MyOpCode::Inc:
                Console::WriteLine("%sinc, [%I64u]", Space.c_str(), Inst.Arg0);
                break;
                // Conversion
            case MyOpCode::Cvtoi:  Console::WriteLine("%scvtoi",  Space.c_str()); break;
            case MyOpCode::Cvtou:  Console::WriteLine("%scvtou",  Space.c_str()); break;
            case MyOpCode::Cvtof:  Console::WriteLine("%scvtof",  Space.c_str()); break;
            case MyOpCode::Cvftoi: Console::WriteLine("%scvftoi", Space.c_str()); break;
            case MyOpCode::Cvftou: Console::WriteLine("%scvftou", Space.c_str()); break;
                // Arithmetic
            case MyOpCode::Add:  Console::WriteLine("%sadd", Space.c_str()); break;
            case MyOpCode::Addf: Console::WriteLine("%saddf", Space.c_str()); break;
            case MyOpCode::Sub:  Console::WriteLine("%ssub", Space.c_str()); break;
            case MyOpCode::Subf: Console::WriteLine("%ssubf", Space.c_str()); break;
            case MyOpCode::Mul:  Console::WriteLine("%smul", Space.c_str()); break;
            case MyOpCode::Mulf: Console::WriteLine("%smulf", Space.c_str()); break;
            case MyOpCode::Div:  Console::WriteLine("%sdiv", Space.c_str()); break;
            case MyOpCode::Divf: Console::WriteLine("%sdivf", Space.c_str()); break;
            case MyOpCode::Pow:  Console::WriteLine("%spow", Space.c_str()); break;
            case MyOpCode::Powf: Console::WriteLine("%spowf", Space.c_str()); break;
            case MyOpCode::Mod:  Console::WriteLine("%smod", Space.c_str()); break;
            case MyOpCode::Lsh:  Console::WriteLine("%slsh", Space.c_str()); break;
            case MyOpCode::Rsh:  Console::WriteLine("%srsh", Space.c_str()); break;
            case MyOpCode::And:  Console::WriteLine("%sand", Space.c_str()); break;
            case MyOpCode::Or:   Console::WriteLine("%sor", Space.c_str()); break;
            case MyOpCode::Xor:  Console::WriteLine("%sxor", Space.c_str()); break;
            case MyOpCode::Neg:  Console::WriteLine("%sneg", Space.c_str()); break;
            case MyOpCode::Negf: Console::WriteLine("%snegf", Space.c_str()); break;
            case MyOpCode::Not:  Console::WriteLine("%snot", Space.c_str()); break;
                // Comparison
            case MyOpCode::Ceq:   Console::WriteLine("%sceq", Space.c_str()); break;
            case MyOpCode::Ceqf:  Console::WriteLine("%sceqf", Space.c_str()); break;
            case MyOpCode::Cneq:  Console::WriteLine("%scneq", Space.c_str()); break;
            case MyOpCode::Cneqf: Console::WriteLine("%scneqf", Space.c_str()); break;
            case MyOpCode::Clt:   Console::WriteLine("%sclt", Space.c_str()); break;
            case MyOpCode::Cltf:  Console::WriteLine("%scltf", Space.c_str()); break;
            case MyOpCode::Clte:  Console::WriteLine("%sclte", Space.c_str()); break;
            case MyOpCode::Cltef: Console::WriteLine("%scltef", Space.c_str()); break;
            case MyOpCode::Cgt:   Console::WriteLine("%scgt", Space.c_str()); break;
            case MyOpCode::Cgtf:  Console::WriteLine("%scgtf", Space.c_str()); break;
            case MyOpCode::Cgte:  Console::WriteLine("%scgte", Space.c_str()); break;
            case MyOpCode::Cgtef: Console::WriteLine("%scgtef", Space.c_str()); break;
                // Control Flow
            case MyOpCode::Jmp:
                PrintJumpOp("jmp,", Inst.Arg0);
                break;
            case MyOpCode::Jnz:
                PrintJumpOp("jnz,", Inst.Arg0);
                break;
            case MyOpCode::Jz:
                PrintJumpOp("jz,", Inst.Arg0);
                break;
            case MyOpCode::Label:
                PrintJumpOp("label:", Inst.Arg0);
                break;
            case MyOpCode::Func:
                PrintJumpOp("func:", Inst.Arg0);
                Indent();
                break;
                // Functions
            case MyOpCode::Call:
                PrintJumpOp("call", Inst.Arg0);
                break;
            case MyOpCode::Calli:
                if (Inst.Arg0 < stbds_shlenu(Assembly.Internals))
                {
                    const auto& [lpName, pfnInternal] = Assembly.Internals[Inst.Arg0];
                    Console::WriteLine("%scalli %s [0x%p]", Space.c_str(), lpName, pfnInternal);
                }
                else
                {
                    Console::WriteLine("%scalli %s [%s]", Space.c_str(), "<invalid>", "<null>");
                }
                break;
            case MyOpCode::Ret:
                if (Assembly.Code[IP].Code == MyOpCode::Label && Assembly.Code[IP + 1ull].Code == MyOpCode::Func)
                {
                    Dedent();
                }
                if (Assembly.Code[IP].Code == MyOpCode::Func)
                {
                    Dedent();
                }
                if (Assembly.Code[IP + 1ull].Code == MyOpCode::Halt)
                {
                    Dedent();
                }
                Console::Write("%sret\n", Space.c_str());
                break;
            case MyOpCode::Nop:
                Console::WriteLine("%snop", Space.c_str());
                break;
            case MyOpCode::Halt:
                Console::WriteLine("halt\n");
                goto EXIT;
                break;
            default:
                Console::WriteLine("%s[invalid opcode '%s' @ %I64u]", Space.c_str(), OpCodeString(Inst.Code), Inst.Arg0);
                break;
        } /* end switch */
    } /* end while */

EXIT:
    return;
}
