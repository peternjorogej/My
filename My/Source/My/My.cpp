#include "My.h"
#include "Base/IO.h"
#include "Object.h"
#include "VM/VM.h"
#include "Utils/Utils.h"
#include "stb/stb_ds.h"

#include <sstream>

static MyContext* s_CurrentContext = nullptr;

static MyContext* _My_ContextCreate() noexcept;
static void       _My_ContextDestroy(MyContext* pContext) noexcept;


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
    ud.ErrorType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.ErrorStruct);
    ud.ErrorStruct->Size = 0ul;
    // Void
    ud.VoidStruct = MyStructCreate(pContext, "Void", MY_STRUCT_ATTR_NONE);
    ud.VoidType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.VoidStruct);
    ud.VoidStruct->Size = 0ul;
    // Object
    ud.ObjectStruct = MyStructCreate(pContext, "Object", MY_STRUCT_ATTR_NONE);
    ud.ObjectType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.ObjectStruct);
    ud.ObjectStruct->Size = 8ul;
    // Boolean
    ud.BooleanStruct = MyStructCreate(pContext, "Boolean", MY_STRUCT_ATTR_POD);
    ud.BooleanType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.BooleanStruct);
    ud.BooleanStruct->Size = 8ul;
    // Int
    ud.IntStruct = MyStructCreate(pContext, "Int", MY_STRUCT_ATTR_POD);
    ud.IntType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.IntStruct);
    ud.IntStruct->Size = 8ul;
    // Uint
    ud.UintStruct = MyStructCreate(pContext, "Uint", MY_STRUCT_ATTR_POD);
    ud.UintType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.UintStruct);
    ud.UintStruct->Size = 8ul;
    // IntPtr
    ud.IntPtrStruct = MyStructCreate(pContext, "IntPtr", MY_STRUCT_ATTR_POD);
    ud.IntPtrType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.IntPtrStruct);
    ud.IntPtrStruct->Size = 8ul;
    // Float
    ud.FloatStruct = MyStructCreate(pContext, "Float", MY_STRUCT_ATTR_POD);
    ud.FloatType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.FloatStruct);
    ud.FloatStruct->Size = 8ul;
    // Complex
    ud.ComplexStruct = MyStructCreate(pContext, "Complex", MY_STRUCT_ATTR_POD);
    ud.ComplexType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.ComplexStruct);
    {
        MyStructAddField(ud.ComplexStruct, "Real", ud.FloatType);
        MyStructAddField(ud.ComplexStruct, "Imag", ud.FloatType);
    }
    // String
    ud.StringStruct = MyStructCreate(pContext, "String", MY_STRUCT_ATTR_NONE);
    ud.StringType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.StringStruct);
    ud.StringStruct->Size = 8ul;
    // StringBuilder
    ud.StringBuilderStruct = MyStructCreate(pContext, "StringBuilder", MY_STRUCT_ATTR_NONE);
    ud.StringBuilderType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.StringBuilderStruct);
    {
        MyStructAddField(ud.StringBuilderStruct, "CString", ud.IntPtrType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.StringBuilderStruct, "Length", ud.UintType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.StringBuilderStruct, "Capacity", ud.UintType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.StringBuilderStruct, "Indent", ud.IntType);
    }
    // Bytes
    ud.BytesStruct = MyStructCreate(pContext, "Bytes", MY_STRUCT_ATTR_NONE);
    ud.BytesType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.BytesStruct);
    {
        MyStructAddField(ud.BytesStruct, "CBuffer", ud.IntPtrType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.BytesStruct, "Length", ud.UintType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.BytesStruct, "Capacity", ud.UintType, MY_FIELD_ATTR_CONST);
    }
    // File
    ud.FileStruct = MyStructCreate(pContext, "File", MY_STRUCT_ATTR_NONE);
    ud.FileType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.FileStruct);
    {
        MyStructAddField(ud.FileStruct, "CFile",   ud.IntPtrType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.FileStruct, "Filepath", ud.StringType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.FileStruct, "OpenMode", ud.StringType, MY_FIELD_ATTR_CONST);
        MyStructAddField(ud.FileStruct, "FileSize", ud.IntType, MY_FIELD_ATTR_CONST);
    }

    // (static) Math
    ud.MathStruct = MyStructCreate(pContext, "Math", MY_STRUCT_ATTR_STATIC);
    ud.MathType = MyTypeCreate(MY_TYPE_KIND_STRUCT, ud.MathStruct);
    ud.MathStruct->Size = 0ul;
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
    MY_SAFEDELETE(ud.IntPtrType);
    MY_SAFEDELETE(ud.FloatType);
    MY_SAFEDELETE(ud.ComplexType);
    MY_SAFEDELETE(ud.StringType);
    MY_SAFEDELETE(ud.StringBuilderType);
    MY_SAFEDELETE(ud.BytesType);
    MY_SAFEDELETE(ud.FileType);
    MY_SAFEDELETE(ud.MathType);

    MY_SAFEDELETE(ud.ErrorStruct);
    MY_SAFEDELETE(ud.VoidStruct);
    MY_SAFEDELETE(ud.ObjectStruct);
    MY_SAFEDELETE(ud.BooleanStruct);
    MY_SAFEDELETE(ud.IntStruct);
    MY_SAFEDELETE(ud.UintStruct);
    MY_SAFEDELETE(ud.IntPtrStruct);
    MY_SAFEDELETE(ud.FloatStruct);
    MY_SAFEDELETE(ud.ComplexStruct);
    MY_SAFEDELETE(ud.StringStruct);
    MY_SAFEDELETE(ud.StringBuilderStruct);
    MY_SAFEDELETE(ud.BytesStruct);
    MY_SAFEDELETE(ud.FileStruct);
    MY_SAFEDELETE(ud.MathStruct);
}

MyFunction* MyFunctionCreate(const MyFunctionSignature& Signature, uint32_t kAttribs, uint32_t kAddress) noexcept
{
    MyFunction* pFunc = Allocator::Create<MyFunction>(Allocator::Stage::Runtime);
    pFunc->Signature = Signature;
    pFunc->Address   = kAddress;
    pFunc->Flags     = kAttribs;

    return pFunc;
}

// Also adds the method to the struct's method list
MyMethod* MyMethodCreate(
    MyStruct* pKlass,
    char*     lpName,
    MyType*   pType,
    uint32_t  kFlags,
    uint32_t  kAddress,
    bool      bIsCtor
) noexcept
{
    MyMethod* pMeth = Allocator::Create<MyMethod>(Allocator::Stage::Runtime);
    pMeth->Fullname = MyGetCachedStringV("%s__%s", pKlass->Name, lpName);
    pMeth->Klass    = pKlass;
    pMeth->Type     = pType;
    pMeth->Flags    = kFlags;
    pMeth->Address  = kAddress;
    pMeth->IsCtor   = bIsCtor;

    if (pKlass->Attributes & MY_STRUCT_ATTR_STATIC)
    {
        pMeth->Flags |= MY_FUNC_ATTR_STATIC;
    }

    stbds_arrpush(pKlass->Methods, pMeth);
    return pMeth;
}

MyStruct* MyStructCreate(MyContext* pContext, const char* lpName, uint32_t kAttribs) noexcept
{
    MyStruct* pKlass = new MyStruct{};
    pKlass->Name       = MyGetCachedString(lpName);
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
    MyDefaults& md = My_Defaults;
    if (!pKlass)
    {
        return false;
    }

    if (pKlass == md.BooleanStruct ||
        pKlass == md.IntStruct     ||
        pKlass == md.UintStruct    ||
        pKlass == md.IntPtrStruct  ||
        pKlass == md.FloatStruct)
    {
        return false;
    }

    if (pKlass->Attributes & MY_STRUCT_ATTR_POD)
    {
        return false;
    }

    return true;
}

void MyStructAddField(MyStruct* pKlass, const char* lpName, MyType* pType, uint32_t kAttribs) noexcept
{
    MyField field = {};
    field.Name       = MyGetCachedString(lpName);
    field.Type       = pType;
    field.Klass      = pKlass;
    field.Offset     = pKlass->Size;
    field.Attributes = kAttribs;

    if (pKlass->Attributes & MY_STRUCT_ATTR_STATIC)
    {
        field.Attributes |= MY_FIELD_ATTR_STATIC;
    }

    MyStruct* pFieldKlass = field.Type->Klass;

    uint32_t kFieldSize = 0u;
    if (pType->Kind == MY_TYPE_KIND_ARRAY || !(pFieldKlass->Attributes & MY_STRUCT_ATTR_POD))
    {
        kFieldSize = sizeof(void*);
    }
    else
    {
        kFieldSize = pFieldKlass->Size;
    }
    pKlass->Size += kFieldSize;

    stbds_arrpush(pKlass->Fields, field);
}

MyField* MyStructGetField(MyStruct* pKlass, const char* lpField)
{
    char* const lpFieldName = MyGetCachedString(lpField);

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

MyMethod* MyStructGetMethod(MyStruct* pKlass, const char* lpMethod)
{
    char* const lpMethodName = MyGetCachedString(lpMethod);

    const size_t kCount = stbds_arrlenu(pKlass->Methods);
    for (size_t k = 0; k < kCount; k++)
    {
        MyMethod* const& pMethod = pKlass->Methods[k];
        if (pMethod->Type->Signature->Name == lpMethodName)
        {
            return pMethod;
        }
    }

    return nullptr;
}

size_t MyStructFieldCount(const MyStruct* pKlass)
{
    return stbds_arrlenu(pKlass->Fields);
}

MyType* MyTypeCreate(uint8_t kKind, void* pData, uint32_t kFlags) noexcept
{
    MyType* pType = new MyType{};
    pType->Klass = (MyStruct*)pData;
    pType->Flags = kFlags;
    pType->Kind  = kKind;
    
    return pType;
}

bool MyTypeIsReference(MyType* pType) noexcept
{
    switch (pType->Kind)
    {
        case MY_TYPE_KIND_STRUCT:
            return MyStructIsReference(pType->Klass);
        case MY_TYPE_KIND_ARRAY:
            return true;
        default: break;
    }
    
    return false;
}

const char* MyTypeGetName(const MyType* pType) noexcept
{
    if (pType)
    {
        switch (pType->Kind)
        {
            case MY_TYPE_KIND_STRUCT:
            {
                return pType->Klass->Name;
            }
            case MY_TYPE_KIND_ARRAY:
            {
                const char* lpShape = nullptr;
                switch (stbds_arrlenu(pType->Array->Lengths))
                {
                    case 0: lpShape = "[]"; break;
                    case 1: lpShape = "[]"; break;
                    case 2: lpShape = "[,]"; break;
                    case 3: lpShape = "[,,]"; break;
                    case 4: lpShape = "[,,,]"; break;
                    default: break;
                }
                return MyGetCachedStringV("%s%s", pType->Array->Klass->Name, lpShape);
            }
            case MY_TYPE_KIND_FUNCTION:
            {
                const char* lpAllParamsString = "";
                const size_t kCount = stbds_arrlenu(pType->Signature->Params);
                for (size_t k = 0; k < kCount; k++)
                {
                    const char* lpParamString = MyGetCachedStringV("%s%s", MyTypeGetName(pType->Signature->Params[k]), k==kCount-1 ? "" : ", ");
                    lpAllParamsString = MyGetCachedStringV("%s%s", lpAllParamsString, lpParamString);
                }
                return MyGetCachedStringV("%s(%s)", MyTypeGetName(pType->Signature->Return), lpAllParamsString);
            }
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

        lpGuidString = MyGetCachedString(ss.str());
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


const char* MyReturnCodeString(int64_t iReturnCode) noexcept
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

        case MY_RC_NULL_REFERENCE:           return "RC_NULL_REFERENCE";
        case MY_RC_DIVISION_BY_ZERO:         return "RC_DIVISION_BY_ZERO";
        case MY_RC_INDEX_OUT_OF_BOUNDS:      return "RC_INDEX_OUT_OF_BOUNDS";
        case MY_RC_INVALID_CAST:             return "RC_INVALID_CAST";
        case MY_RC_INVALID_OPERATION:        return "RC_INVALID_OPERATION";

        case MY_RC_UNDEFINED_BUILTIN:        return "RC_UNDEFINED_BUILTIN";
        case MY_RC_UNDEFINED_INTERNAL:       return "RC_UNDEFINED_INTERNAL";
        case MY_RC_RECURSION_LIMIT_EXCEEDED: return "RC_RECURSION_LIMIT_EXCEEDED";
        
        default: return "[Invalid Return Code]";
    }
}