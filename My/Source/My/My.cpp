#include "My.h"
#include "Base/IO.h"
#include "Object.h"
#include "VM/VM.h"
#include "Utils/Utils.h"
#include "Stb/stb_ds.h"

#include <sstream>

static MyContext* s_CurrentContext = nullptr;

static MyContext* _My_ContextCreate() noexcept;
static void       _My_ContextDestroy(MyContext* pContext) noexcept;
static void       _My_StructAddField(MyStruct* pKlass, const char* lpName, MyType* pType, MyStruct* pFieldKlass) noexcept;


MyGuid::MyGuid()
{
    memset(this, 0, sizeof(MyGuid));
}

MyGuid::~MyGuid() noexcept
{ }

MyGuid::MyGuid(const MyGuid& guid) noexcept
{
    *this = guid;
}

MyGuid& MyGuid::operator=(const MyGuid& guid) noexcept
{
    new(this) MyGuid{};
    Data1[0] = guid.Data1[0];
    Data1[1] = guid.Data1[1];
    return *this;
}

bool MyGuid::IsValid() const noexcept
{
    return Data1[0] != 0ul &&
           Data1[1] != 0ul;
}

const char* MyGuid::AsString() const noexcept
{
    MyContext* pContext = MyContextGet();
    MY_ASSERT(pContext != nullptr, "Cannot get guid's string without an active context");
    return stbds_hmget(pContext->Guids, *this);
}

bool MyGuid::operator==(MyGuid& guid) const noexcept
{
    return Data1[0] == guid.Data1[0] &&
           Data1[1] == guid.Data1[1];
}

bool MyGuid::operator!=(MyGuid& guid) const noexcept
{
    return !this->operator==(guid);
}


MyContext* MyInitialize()
{
    MyContext* pContext = _My_ContextCreate();

    MyInitializeStructs(pContext);

    return pContext;
}

void MyUninitialize(MyContext* pContext)
{
    Allocator::FullCleanup();

    _My_ContextDestroy(pContext);
    MyUninitializeStructs();
}

void MyInitializeStructs(MyContext* pContext)
{
    static const auto NextGuid = [&pContext] { return MyGuidCreate(pContext); };

    MyDefaults& ud = My_Defaults;

    // Error
    ud.ErrorStruct = MyStructCreate(pContext, "Error", MY_STRUCT_ATTR_NONE);
    ud.ErrorType = MyTypeCreate(0u, ud.ErrorStruct);
    ud.ErrorStruct->Size = 0ul;
    // Void
    ud.VoidStruct = MyStructCreate(pContext, "Void", MY_STRUCT_ATTR_NONE);
    ud.VoidType = MyTypeCreate(0u, ud.VoidStruct);
    ud.VoidStruct->Size = 0ul;
    // Object
    ud.ObjectStruct = MyStructCreate(pContext, "Object", MY_STRUCT_ATTR_NONE);
    ud.ObjectType = MyTypeCreate(0u, ud.ObjectStruct);
    ud.ObjectStruct->Size = 8ul;
    // Boolean
    ud.BooleanStruct = MyStructCreate(pContext, "Boolean", MY_STRUCT_ATTR_POD);
    ud.BooleanType = MyTypeCreate(0u, ud.BooleanStruct);
    ud.BooleanStruct->Size = 8ul;
    // Int
    ud.IntStruct = MyStructCreate(pContext, "Int", MY_STRUCT_ATTR_POD);
    ud.IntType = MyTypeCreate(0u, ud.IntStruct);
    ud.IntStruct->Size = 8ul;
    // Uint
    ud.UintStruct = MyStructCreate(pContext, "Uint", MY_STRUCT_ATTR_POD);
    ud.UintType = MyTypeCreate(0u, ud.UintStruct);
    ud.UintStruct->Size = 8ul;
    // Float
    ud.FloatStruct = MyStructCreate(pContext, "Float", MY_STRUCT_ATTR_POD);
    ud.FloatType = MyTypeCreate(0u, ud.FloatStruct);
    ud.FloatStruct->Size = 8ul;
    // Complex
    ud.ComplexStruct = MyStructCreate(pContext, "Complex", MY_STRUCT_ATTR_POD);
    ud.ComplexType = MyTypeCreate(0u, ud.ComplexStruct);
    {
        MyStructAddFieldAutoOffset(ud.ComplexStruct, "Real", ud.FloatType, ud.FloatStruct);
        MyStructAddFieldAutoOffset(ud.ComplexStruct, "Imag", ud.FloatType, ud.FloatStruct);
    }
    // String
    ud.StringStruct = MyStructCreate(pContext, "String", MY_STRUCT_ATTR_NONE);
    ud.StringType = MyTypeCreate(0u, ud.StringStruct);
    ud.StringStruct->Size = 8ul;
    // StringBuilder
    ud.StringBuilderStruct = MyStructCreate(pContext, "StringBuilder", MY_STRUCT_ATTR_NONE);
    ud.StringBuilderType = MyTypeCreate(0u, ud.StringBuilderStruct);
    {
        MyStructAddFieldAutoOffset(ud.StringBuilderStruct, "Value", ud.StringType, ud.StringStruct);
    }
    // File
    ud.FileStruct = MyStructCreate(pContext, "File", MY_STRUCT_ATTR_NONE);
    ud.FileType = MyTypeCreate(0u, ud.FileStruct);
    {
        MyStructAddFieldAutoOffset(ud.FileStruct, "Handle",   ud.UintType,   ud.UintStruct, MY_FIELD_ATTR_CONST);
        MyStructAddFieldAutoOffset(ud.FileStruct, "Filepath", ud.StringType, ud.StringStruct);
    }
}

void MyUninitializeStructs()
{
    MyDefaults& ud = My_Defaults;

    MY_SAFEDELETE(ud.ErrorType);
    MY_SAFEDELETE(ud.VoidType);
    MY_SAFEDELETE(ud.ObjectType);
    MY_SAFEDELETE(ud.BooleanType);
    MY_SAFEDELETE(ud.IntType);
    MY_SAFEDELETE(ud.UintType);
    MY_SAFEDELETE(ud.FloatType);
    MY_SAFEDELETE(ud.ComplexType);
    MY_SAFEDELETE(ud.StringType);
    MY_SAFEDELETE(ud.StringBuilderType);
    MY_SAFEDELETE(ud.FileType);

    MY_SAFEDELETE(ud.ErrorStruct);
    MY_SAFEDELETE(ud.VoidStruct);
    MY_SAFEDELETE(ud.ObjectStruct);
    MY_SAFEDELETE(ud.BooleanStruct);
    MY_SAFEDELETE(ud.IntStruct);
    MY_SAFEDELETE(ud.UintStruct);
    MY_SAFEDELETE(ud.FloatStruct);
    MY_SAFEDELETE(ud.ComplexStruct);
    MY_SAFEDELETE(ud.StringStruct);
    MY_SAFEDELETE(ud.StringBuilderStruct);
    MY_SAFEDELETE(ud.FileStruct);
}

MyFunction* MyFunctionCreate(const MyFunctionSignature& Signature, uint32_t kAttribs, uint32_t kAddress) noexcept
{
    MyFunction* pFunc = Allocator::Create<MyFunction>(Allocator::Stage::Runtime);
    pFunc->Signature = Signature;
    pFunc->Address   = kAddress;
    pFunc->Flags     = kAttribs;

    return pFunc;
}

MyMethod* MyMethodCreate(
    const MyFunctionSignature& Signature,
    MyStruct*                  pKlass,
    char*                       lpFullname,
    uint32_t                    kFlags,
    uint32_t                    kAddress,
    bool                        bIsCtor
) noexcept
{
    MyMethod* pMeth = Allocator::Create<MyMethod>(Allocator::Stage::Runtime);
    pMeth->Signature = Signature;
    pMeth->Fullname  = lpFullname;
    pMeth->Klass     = pKlass;
    pMeth->Flags     = kFlags;
    pMeth->Address   = kAddress;
    pMeth->IsCtor    = bIsCtor;

    return pMeth;
}

MyStruct* MyStructCreate(MyContext* pContext, const char* lpName, uint32_t kAttribs) noexcept
{
    MyStruct* pKlass = new MyStruct{};
    pKlass->Name       = UniStrdup(lpName);
    pKlass->Guid       = MyGuidCreate(pContext);
    pKlass->Fields     = nullptr;
    pKlass->Methods    = nullptr;
    pKlass->Attributes = kAttribs;
    pKlass->Size       = 0ul;
    // pKlass->IsInitialized = false;   // ?
    
    return pKlass;
}

bool MyStructIsReference(MyStruct* pKlass) noexcept
{
    MyDefaults& ud = My_Defaults;
    if (pKlass)
    {
        if (pKlass == ud.BooleanStruct ||
            pKlass == ud.IntStruct     ||
            pKlass == ud.UintStruct    ||
            pKlass == ud.FloatStruct)
        {
            return false;
        }
        if (pKlass->Attributes & MY_STRUCT_ATTR_POD)
        {
            return false;
        }

        return true;
    }
    else
    {
        return false;
    }
}

void MyStructAddField(MyStruct* pKlass, const char* lpName, MyType* pType, MyStruct* pFieldKlass, uint32_t kOffset, uint32_t kAttribs) noexcept
{
    MY_ASSERT(kOffset == pKlass->Size, "Error: Invalid field offset");

    _My_StructAddField(pKlass, lpName, pType, pFieldKlass);
    
    MyField& field = stbds_arrlast(pKlass->Fields);
    field.Offset     = kOffset;
    field.Attributes = kAttribs;
    
    pKlass->Size += pFieldKlass->Size;
}

void MyStructAddFieldAutoOffset(MyStruct* pKlass, const char* lpName, MyType* pType, MyStruct* pFieldKlass, uint32_t kAttribs) noexcept
{
    _My_StructAddField(pKlass, lpName, pType, pFieldKlass);
    
    MyField& field = stbds_arrlast(pKlass->Fields);
    field.Offset     = pKlass->Size;
    field.Attributes = kAttribs;

    uint32_t kFieldSize = 0;
    if (pType->Kind == 1u || !(pFieldKlass->Attributes & MY_STRUCT_ATTR_POD))
    {
        kFieldSize = sizeof(void*);
    }
    else
    {
        kFieldSize = pFieldKlass->Size;
    }
    pKlass->Size += kFieldSize;
}

MyField* MyStructGetField(MyStruct* pKlass, const char* lpField)
{
    char* const lpFieldName = UniStrdup(lpField);

    const size_t kCount = stbds_arrlenu(pKlass->Fields);
    for (size_t k = 0; k < kCount; k++)
    {
        MyField* const& pField = pKlass->Fields + k;
        if (pField->Name == lpFieldName)
        {
            return pField;
        }
    }

    return nullptr;
}

MyType* MyTypeCreate(uint8_t kKind, void* pData, uint32_t kFlags) noexcept
{
    MyType* pType = new MyType{};
    pType->Klass = (MyStruct*)pData;
    pType->Flags = kFlags;
    pType->Kind  = kKind;
    
    return pType;
}

const char* MyTypeGetName(const MyType* pType) noexcept
{
    if (pType)
    {
        switch (pType->Kind)
        {
            case 0: return pType->Klass->Name;
            case 1: return pType->Array->Klass->Name;
            case 2: return pType->Signature->Name;
            default: break;
        }
    }

    MY_ASSERT(false, "Error: Invalid type or type kind");
    return nullptr;
}

MyContext* MyContextGet() noexcept
{
    return s_CurrentContext;
}
#if 0
MyGuid MyContextGetTypeID(MyContext* pContext, char* const& lpTypename) noexcept
{
    MY_ASSERT(pContext != nullptr, "Invalid context");
    MyGuid guid = stbds_shget(pContext->TidCache, lpTypename);
    if (guid.IsValid())
    {
        return guid;
    }
    else
    {
        guid = MyGuidCreate(pContext);
        stbds_shput(pContext->TidCache, lpTypename, guid);
        return guid;
    }
}
#endif // 0
MyGuid MyGuidCreate(MyContext* pContext) noexcept
{
    MyGuid guid = MyGuid{};
    guid.Data1[0] = Random::Uint();
    guid.Data1[1] = Random::Uint();

    (void)MyGuidCreateStringRepr(pContext, guid);
    return guid;
}

MyGuid MyGuidCreate(MyContext* pContext, uint64_t kValue0, uint64_t kValue1) noexcept
{
    MyGuid guid = MyGuid{};
    guid.Data1[0] = kValue0;
    guid.Data1[1] = kValue1;

    (void)MyGuidCreateStringRepr(pContext, guid);
    return guid;
}

char* MyGuidCreateStringRepr(MyContext* pContext, const MyGuid& Guid) noexcept
{
    char* lpGuidString = stbds_hmget(pContext->Guids, Guid);
    
    if (lpGuidString != nullptr)
    {
        return lpGuidString;
    }

    // TODO: Consider using a C-Style method instead of relying on std::stringstream?
    {
        std::stringstream ss = {};
        ss << std::hex;
        ss.fill(ss.widen('0'));

        size_t k = 0ull;
        for (const uint8_t& u : Guid.Data0)
        {
            ss.width(2);
            ss << static_cast<uint32_t>(u);
            if (k == 3 || k == 5 || k == 7 || k == 9)
            {
                ss << ss.widen('-');
            }
            k++;
        }

        lpGuidString = UniStrdup(ss.str());
    }
    stbds_hmput(pContext->Guids, Guid, lpGuidString);
    return lpGuidString;
}

MyContext* _My_ContextCreate() noexcept
{
    MyContext* pContext = new MyContext{};
    pContext->VM       = new MyVM{};
    
    {
        static const MyContext::GuidStringCache cache = { MyGuid{}, nullptr };
        stbds_hmdefault(pContext->Guids, nullptr);
        stbds_hmdefaults(pContext->Guids, cache);
    }
    {
        static constexpr MyContext::RtStringCache cache = { 0ull, nullptr };
        stbds_hmdefault(pContext->RtCache, nullptr);
        stbds_hmdefaults(pContext->RtCache, cache);
    }
    {
        static constexpr MyContext::CtStringCache cache = { 0ull, nullptr };
        stbds_hmdefault(pContext->CtCache, nullptr);
        stbds_hmdefaults(pContext->CtCache, cache);
    }

    pContext->VM->Context = pContext;
    pContext->VM->GC.VM   = pContext->VM;

    return (s_CurrentContext = pContext);
}

void _My_ContextDestroy(MyContext* pContext) noexcept
{
    delete pContext->VM;
    pContext->VM = nullptr;

    stbds_hmfree(pContext->Guids);
    pContext->Guids = nullptr;

    stbds_hmfree(pContext->RtCache);
    pContext->RtCache = nullptr;
    
    for (size_t k = 0; k < stbds_hmlenu(pContext->CtCache); k++)
    {
        MyContext::CtStringCache& cache = pContext->CtCache[k];
        delete[] cache.value;
        cache.value = nullptr;
    }
    stbds_hmfree(pContext->CtCache);
    pContext->CtCache = nullptr;

    delete pContext;
}

void _My_StructAddField(MyStruct* pKlass, const char* lpName, MyType* pType, MyStruct* pFieldKlass) noexcept
{
    MyField field = {};
    field.Name   = UniStrdup(lpName);
    field.Type   = pType;
    field.Klass  = pFieldKlass;
    stbds_arrpush(pKlass->Fields, field);
}


const char* ReturnCodeString(int64_t iReturnCode) noexcept
{
    switch (iReturnCode)
    {
        case MY_RC_ERROR:   return "RC_ERROR";
        case MY_RC_SUCCESS: return "RC_SUCCESS";

        case MY_RC_STACK_UNDERFLOW:    return "RC_STACK_UNDERFLOW";
        case MY_RC_STACK_OVERFLOW:     return "RC_STACK_OVERFLOW";
        case MY_RC_CALLSTACK_OVERFLOW: return "RC_CALLSTACK_OVERFLOW";
        case MY_RC_INVALID_OPCODE:     return "RC_INVALID_OPCODE";
        case MY_RC_INVALID_ADDRESS:    return "RC_INVALID_ADDRESS";

        case MY_RC_DIVISION_BY_ZERO:         return "RC_DIVISION_BY_ZERO";
        case MY_RC_INDEX_OUT_OF_BOUNDS:      return "RC_INDEX_OUT_OF_BOUNDS";
        case MY_RC_INVALID_OPERATION:        return "RC_INVALID_OPERATION";
        case MY_RC_UNDEFINED_BUILTIN:        return "RC_UNDEFINED_BUILTIN";
        case MY_RC_UNDEFINED_INTERNAL:       return "RC_UNDEFINED_INTERNAL";
        case MY_RC_RECURSION_LIMIT_EXCEEDED: return "RC_RECURSION_LIMIT_EXCEEDED";
        
        default: return "[Invalid Return Code]";
    }
}