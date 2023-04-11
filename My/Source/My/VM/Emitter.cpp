#include "Emitter.h"
#include "VM.h"
#include "Builtins.h"
#include "My/Base/IO.h"
#include "stb/stb_ds.h"
#include "Serializer/PnBinarySerializer.hpp"

static InternalFunction* s_Builtins = nullptr;

static void _My_Emitter_Serialize(const MyAssembly* pAssembly, const std::string& Path) noexcept;
static void _My_Emitter_Deserialize(MyAssembly* pAssembly, const std::string& Path) noexcept;
static void _My_Initialize_BuiltinsMap() noexcept;

#pragma region Bytecode_Generation
MyBytecodeProcessor::MyBytecodeProcessor(MyContext* pContext)
    : m_Assembly()
{
    m_Assembly = new MyAssembly{};

    static constexpr AddressMap am = { nullptr, MY_INVALID_ADDR };

    stbds_shdefault(m_Assembly->Globals, MY_INVALID_ADDR);
    stbds_shdefaults(m_Assembly->Globals, am);
    stbds_shdefault(m_Assembly->Variables, MY_INVALID_ADDR);
    stbds_shdefaults(m_Assembly->Variables, am);

    stbds_shdefault(m_Assembly->Labels, MY_INVALID_ADDR);
    stbds_shdefaults(m_Assembly->Labels, am);
    stbds_shdefault(m_Assembly->Functions, MY_INVALID_ADDR);
    stbds_shdefaults(m_Assembly->Functions, am);

    stbds_shdefault(m_Assembly->Fields, MY_INVALID_ADDR);
    stbds_shdefaults(m_Assembly->Fields, am);

    static constexpr InternalFunction nm = { nullptr, nullptr };
    
    stbds_shdefault(m_Assembly->Internals, nullptr);
    stbds_shdefaults(m_Assembly->Internals, nm);

    if (s_Builtins == nullptr)
    {
        _My_Initialize_BuiltinsMap();
    }
}

void MyBytecodeProcessor::Emit(MyOpCode Code)
{
    const MyInstruction inst = { Code, 0u, 0u };
    stbds_arrpush(m_Assembly->Code, inst);
}

void MyBytecodeProcessor::Emit(MyOpCode Code, uint32_t Arg0, uint32_t Arg1)
{
    const MyInstruction inst = { Code, Arg0, Arg1 };
    stbds_arrpush(m_Assembly->Code, inst);
}

void MyBytecodeProcessor::Emit(MyOpCode Code, int64_t Value)
{
    const MyValue vInt = MakeValue_Int64(Value);
    Emit(vInt);
}

void MyBytecodeProcessor::Emit(MyOpCode Code, const MyString* pValue)
{
    const MyValue vString = MakeValue_String(const_cast<MyString*>(pValue));
    Emit(vString);
}

void MyBytecodeProcessor::Emit(MyOpCode Code, char* const& lpValue)
{
    char* const& lpCachedValue = MyGetCachedString(lpValue);
    switch (Code)
    {
        case MyOpCode::Ldfld:
        {
            Emit(MyOpCode::Ldfld, GetOrSetFieldIndex(lpCachedValue));
            break;
        }
        case MyOpCode::Stfld:
        {
            Emit(MyOpCode::Stfld, GetOrSetFieldIndex(lpCachedValue));
            break;
        }
        case MyOpCode::Newobj:
        {
            Emit(MyOpCode::Newobj, GetStructIndex(lpCachedValue));
            break;
        }
        case MyOpCode::Label:
        {
            const uint32_t kAddress = stbds_arrlenu(m_Assembly->Code);
            ResolveDeferredAddress(&m_DeferredLabels, lpCachedValue, kAddress);

            // We don't need to store the addresses of labels, but they are useful for decompilation (and debugging)
            stbds_shput(m_Assembly->Labels, lpCachedValue, kAddress);
            Emit(MyOpCode::Label, kAddress);
            break;
        }
        case MyOpCode::Jmp:
        case MyOpCode::Jnz:
        case MyOpCode::Jz:
        {
            EmitUnreferencedAddress(&m_Assembly->Labels, &m_DeferredLabels, lpCachedValue, Code);
            break;
        }
        case MyOpCode::Call:
        {
            EmitUnreferencedAddress(&m_Assembly->Functions, &m_DeferredFunctions, lpCachedValue, MyOpCode::Call);
            break;
        }
        case MyOpCode::Calli:
        {
            uint32_t kIndex = FindNativeByName(lpCachedValue);
            MY_ASSERT(kIndex != MY_INVALID_ADDR, "Error: Calling undefined internal function '%s'", lpCachedValue);
            Emit(Code, kIndex);
            break;
        }
        default: break;
    }
}

void MyBytecodeProcessor::Emit(MyOpCode Code, const MySymbol* pVariable)
{
    switch (Code)
    {
        case MyOpCode::Ldarg:
        {
            const uint32_t kIndex = stbds_shlenu(m_Assembly->Variables);
            stbds_shput(m_Assembly->Variables, pVariable->Name, kIndex);
            Emit(MyOpCode::Ldarg, kIndex);
            break;
        }
        case MyOpCode::Stloc:
        {
            uint32_t kAddress = stbds_shget(m_Assembly->Variables, pVariable->Name);
            if (kAddress == MY_INVALID_ADDR)
            {
                kAddress = stbds_shlenu(m_Assembly->Variables);
                stbds_shput(m_Assembly->Variables, pVariable->Name, kAddress);
            }
            Emit(MyOpCode::Stloc, kAddress);
            break;
        }
        case MyOpCode::Ldloc:
        {
            const uint32_t& kIndex = stbds_shget(m_Assembly->Variables, pVariable->Name);
            Emit(MyOpCode::Ldloc, kIndex);
            break;
        }
        case MyOpCode::Stglo:
        {
            uint32_t kAddress = stbds_shget(m_Assembly->Globals, pVariable->Name);
            if (kAddress == MY_INVALID_ADDR)
            {
                kAddress = stbds_shlenu(m_Assembly->Globals);
                stbds_shput(m_Assembly->Globals, pVariable->Name, kAddress);
            }
            Emit(MyOpCode::Stglo, kAddress);
            break;
        }
        case MyOpCode::Ldglo:
        {
            const uint32_t& kIndex = stbds_shget(m_Assembly->Globals, pVariable->Name);
            Emit(MyOpCode::Ldglo, kIndex);
            break;
        }
        case MyOpCode::Inc:
        {
            const uint32_t& kIndex = stbds_shget(m_Assembly->Variables, pVariable->Name);
            Emit(MyOpCode::Inc, kIndex);
            break;
        }
        default: break;
    }
}

void MyBytecodeProcessor::Emit(const MyValue& Value)
{
    MyOpCode Code = MyOpCode::Invalid;
    switch (Value.Kind)
    {
        case MyValueKind::Null:    Code = MyOpCode::Ldc;   break;
        case MyValueKind::Bool:    Code = MyOpCode::Ldc;   break;
        case MyValueKind::Int64:   Code = MyOpCode::Ldc;   break;
        case MyValueKind::Uint64:  Code = MyOpCode::Ldc;   break;
        case MyValueKind::Float64: Code = MyOpCode::Ldcf;  break;
        case MyValueKind::String:  Code = MyOpCode::Ldstr; break;
        case MyValueKind::Array:   Code = MyOpCode::Ldarr; break;
        default:
        {
            MY_ASSERT(false, "Unexpected object type '%s'", ValueKindString(Value.Kind));
            break;
        }
    }

    const uint32_t kIndex = stbds_arrlenu(m_Assembly->Constants);
    Emit(Code, kIndex);
    stbds_arrpush(m_Assembly->Constants, Value);
}

void MyBytecodeProcessor::RegisterStruct(MyStruct* const& pKlass)
{
    using StructMap = Pair<char*, MyStruct*>;

    static StructMap* s_Map = nullptr;
    if (!s_Map)
    {
        static constexpr StructMap sm = { nullptr, nullptr };
        stbds_shdefault(s_Map, nullptr);
        stbds_shdefaults(s_Map, sm);
    }

    if (stbds_shget(s_Map, pKlass->Name) == nullptr)
    {
        stbds_shput(s_Map, pKlass->Name, pKlass);
        stbds_arrpush(m_Assembly->Klasses, pKlass);
    }
}

void MyBytecodeProcessor::RegisterFunction(char* const& lpName, pfnMyInternalFunction pfnInternal)
{
    char* const& lpCachedName = MyGetCachedString(lpName);
    if (!IsRegistered(lpCachedName))
    {
        const InternalFunction ifunc = { lpCachedName, pfnInternal };
        stbds_shputs(m_Assembly->Internals, ifunc);
    }
}

void MyBytecodeProcessor::RegisterFunction(const InternalFunction& ifunc)
{
    RegisterFunction(ifunc.key, ifunc.value);
}

bool MyBytecodeProcessor::IsRegistered(char* const& lpName)
{
    return stbds_shget(m_Assembly->Internals, lpName) != nullptr;
}

void MyBytecodeProcessor::BeginFunction(char* const& lpName)
{
    char* const& lpCachedName = MyGetCachedString(lpName);

    stbds_shfree(m_Assembly->Variables);
    {
        static constexpr AddressMap am = { nullptr, MY_INVALID_ADDR };
        stbds_shdefault(m_Assembly->Variables, MY_INVALID_ADDR);
        stbds_shdefaults(m_Assembly->Variables, am);
    }

    const uint32_t kAddress = stbds_arrlenu(m_Assembly->Code);
    ResolveDeferredAddress(&m_DeferredFunctions, lpCachedName, kAddress);
    
    // We don't need to store the addresses of labels, but they are useful for decompilation (and debugging)
    stbds_shput(m_Assembly->Functions, lpCachedName, kAddress);
    stbds_shput(m_Assembly->Labels, lpCachedName, kAddress);
    Emit(MyOpCode::Func, kAddress);
}

void MyBytecodeProcessor::EndFunction()
{
    // Added for completion; might have something in future
}

void MyBytecodeProcessor::End()
{
    Emit(MyOpCode::Halt);
}

void MyBytecodeProcessor::Decompile()
{
    MyDecompile(m_Assembly);
}

bool MyBytecodeProcessor::Load(const std::string& InputPath)
{
    return Load(m_Assembly, InputPath);
}

bool MyBytecodeProcessor::Dump(const std::string& OutputPath)
{
    return Dump(m_Assembly, OutputPath);
}

bool MyBytecodeProcessor::Load(MyAssembly* pAssembly, const std::string& InputPath)
{
    _My_Emitter_Deserialize(pAssembly, InputPath);
    return true;
}

bool MyBytecodeProcessor::Dump(const MyAssembly* pAssembly, const std::string& OutputPath)
{
    _My_Emitter_Serialize(pAssembly, OutputPath);
    return true;
}

MyAssembly* const MyBytecodeProcessor::GetAssembly() noexcept
{
    return m_Assembly;
}

MyAssembly* const MyBytecodeProcessor::GetAssembly() const noexcept
{
    return m_Assembly;
}

void MyBytecodeProcessor::EmitUnreferencedAddress(AddressMap** ppLabelMap, AddressMap** ppDeferredMap, char* const& lpLabel, MyOpCode Code)
{
    // If the label or function has not yet been defined (i.e its address is ahead of our current position),
    // we store the index of the instruction referencing the label in *pDeferredMap*, and when we eventually
    // get to its definition (basically its label) we will resolve the address
    bool bDeferred = false;

    uint32_t kAddress = stbds_shget(*ppLabelMap, lpLabel);
    if (kAddress == MY_INVALID_ADDR)
    {
        bDeferred = true;
        kAddress = stbds_arrlenu(m_Assembly->Code);
    }

    Emit(Code, kAddress);

    if (bDeferred)
    {
        const AddressMap am = { lpLabel, kAddress };
        stbds_arrpush(*ppDeferredMap, am);
    }
}

void MyBytecodeProcessor::ResolveDeferredAddress(AddressMap** ppDeferredMap, char* const& lpLabel, uint32_t kAddress)
{
    // Resolve the instructions that referenced this label before it was emitted
    for (size_t k = 0; k < stbds_arrlenu(*ppDeferredMap); k++)
    {
        const auto& [name, addr] = (*ppDeferredMap)[k];
        if (name == lpLabel)
        {
            MyInstruction& Inst = m_Assembly->Code[addr];
            Inst.Arg0 = kAddress;
        }
    }
}

uint32_t MyBytecodeProcessor::GetOrSetFieldIndex(char* const& lpField)
{
    uint32_t kIndex = stbds_shget(m_Assembly->Fields, lpField);
    if (kIndex == MY_INVALID_ADDR)
    {
        kIndex = stbds_shlenu(m_Assembly->Fields);
        stbds_shput(m_Assembly->Fields, lpField, kIndex);
    }
    return kIndex;
}

uint32_t MyBytecodeProcessor::FindNativeByName(char* const& lpName)
{
    uint32_t kIndex  = MY_INVALID_ADDR;
    for (size_t k = 0; k < stbds_shlenu(m_Assembly->Internals); k++)
    {
        const InternalFunction& nm = m_Assembly->Internals[k];
        if (nm.key == lpName)
        {
            kIndex = k;
            break;
        }
    }
    return kIndex;
}

uint32_t MyBytecodeProcessor::GetStructIndex(MyStruct* const& pKlass)
{
    return GetStructIndex(pKlass->Name);
}

uint32_t MyBytecodeProcessor::GetStructIndex(char* const& lpName)
{
    for (size_t k = 0; k < stbds_arrlenu(m_Assembly->Klasses); k++)
    {
        if (m_Assembly->Klasses[k]->Name == lpName)
        {
            return (uint32_t)k;
        }
    }

    MY_ASSERT(false, "Struct '%s' not found", lpName);
    return MY_INVALID_ADDR;
}
#pragma endregion

#pragma region Bytecode_Emission
MyAssembly* Emitter::Build(MyContext* pContext, const BoundProgram* pProgram, const List<InternalFunction>& Internals)
{
    Emitter e = Emitter{};
    MyBytecodeProcessor bp = MyBytecodeProcessor{ pContext };

    for (const InternalFunction& ifunc : Internals)
    {
        bp.RegisterFunction(ifunc);
    }

    e.EmitProgram(bp, pProgram);
    Allocator::FullCleanup();

    return bp.GetAssembly();
}

void Emitter::EmitProgram(MyBytecodeProcessor& bp, const BoundProgram* pProgram)
{
    m_FunctionBodies = pProgram->FunctionBodies;

    EmitTypes(bp, pProgram);
    EmitInternalFunctions(bp);

    // Emit the global variables before calling main. They will will be removed from the evaluation stack
    // and stored in the global scope when the 'Stglo' instruction is evaluated
    for (size_t k = 0; k < stbds_shlenu(pProgram->Globals); k++)
    {
        const auto&[lpName, Var] = pProgram->Globals[k];
        const auto&[pVariable, pValue] = Var;

        EmitExpression(bp, pValue);
        bp.Emit(MyOpCode::Stglo, pVariable);
    }

    bp.Emit(MyOpCode::Call, "Main");
    bp.Emit(MyOpCode::Jmp, "__end");

    const size_t kFunctionCount = stbds_shlenu(m_FunctionBodies);

    // m_FunctionBodies is a map so there is the likelihood of attempting 
    // to call an extern-ed function before it has been emitted as an extern.
    // So we do 2 passes: the first to declare the extern-ed functions...
    for (size_t k = 0; k < kFunctionCount; k++)
    {
        const auto& [lpName, Fun] = m_FunctionBodies[k];
        const auto& [pFunction, pBody] = Fun;

        if (!pBody)
        {
            // Since the function was declared as extern, it's definition doesn't exist. In
            // case it was already set as extern before bp.Extern will not add it to the list
            // -------------------------------------------------------------------------------
            // The extern declaration behaves like an on/off switch; if the function doesn't
            // exist in the map, we set the function pointer to null (probably throw an error *?)
            if (bp.IsRegistered(lpName))
            {
                continue;
            }

            if (auto& [name, pfnBuiltin] = stbds_shgets(s_Builtins, lpName); pfnBuiltin != nullptr)
            {
                bp.RegisterFunction(lpName, pfnBuiltin);
            }
            else
            {
                DebugLog::Warn("extern function '%s' not found in the builtins map", lpName);
                bp.RegisterFunction(lpName, nullptr);
            }
        }
    }

    // ...and the second to emit the user defined functions
    for (size_t k = 0; k < kFunctionCount; k++)
    {
        const auto& [lpName, Fun] = m_FunctionBodies[k];
        const auto&[pFunction, pBody] = Fun;

        if (pBody)
        {
            EmitFunction(bp, pFunction, pBody);
        }
    }

    bp.Emit(MyOpCode::Label, "__end");
    bp.End();
}

void Emitter::EmitFunction(MyBytecodeProcessor& bp, const MySymbol* pFunction, BoundStatement* pBody)
{
    const FunctionSymbol& fs = pFunction->funcsym;

    int32_t iCount = 0ull;

    bp.BeginFunction(pFunction->Name);

    iCount = stbds_arrlen(fs.Parameters);
    for (int32_t k = iCount-1; k >= 0; k--)
    {
        bp.Emit(MyOpCode::Ldarg, fs.Parameters[k]);
    }

    EmitFunctionBody(bp, pBody->block.Statements);

    bp.EndFunction();
}

void Emitter::EmitTypes(MyBytecodeProcessor& bp, const BoundProgram* pProgram) noexcept
{
    for (size_t k = 0; k < stbds_shlenu(pProgram->StructTypes); k++)
    {
        MyStruct* const& pKlass = pProgram->StructTypes[k].value->Klass;
        bp.RegisterStruct(pKlass);
    }
}

void Emitter::EmitInternalFunctions(MyBytecodeProcessor& bp) noexcept
{
    for (size_t k = 0; k < stbds_shlenu(s_Builtins); k++)
    {
        const InternalFunction& nm = s_Builtins[k];
        bp.RegisterFunction(nm);
    }
}

void Emitter::EmitStatement(MyBytecodeProcessor& bp, BoundStatement* pStatement)
{
    switch (pStatement->Kind)
    {
        case BoundStatementKind::Expression:
            EmitExpressionStatement(bp, pStatement);
            break;
        case BoundStatementKind::VariableDeclaration:
            EmitVariableDeclarationStatement(bp, pStatement);
            break;
        case BoundStatementKind::DecomposeDeclaration:
            EmitDecomposeDeclarationStatement(bp, pStatement);
            break;
        case BoundStatementKind::Label:
            EmitLabelStatement(bp, pStatement);
            break;
        case BoundStatementKind::Goto:
            EmitGotoStatement(bp, pStatement);
            break;
        case BoundStatementKind::ConditionalGoto:
            EmitConditionalGotoStatement(bp, pStatement);
            break;
        case BoundStatementKind::Return:
            EmitReturnStatement(bp, pStatement);
            break;
        default:
            MY_ASSERT(false, "EmitterError: Unexpected statement kind '%s'\n", BoundStatementKindString(pStatement->Kind));
            break;
    }
}

void Emitter::EmitExpressionStatement(MyBytecodeProcessor& bp, BoundStatement* pExpr)
{
    BoundExpressionStatement& es = pExpr->expr;

    EmitExpression(bp, es.Expr);

    if (es.Expr->Kind == BoundExpressionKind::Assignment || es.Expr->Kind == BoundExpressionKind::Call)
    {
        if (es.Expr->Type() != My_Defaults.VoidType)
        {
            bp.Emit(MyOpCode::Pop);
        }
    }
}

void Emitter::EmitVariableDeclarationStatement(MyBytecodeProcessor& bp, BoundStatement* pVarDecl)
{
    BoundVariableDeclarationStatement& vds = pVarDecl->vardecl;

    EmitExpression(bp, vds.Value);
    bp.Emit(MyOpCode::Stloc, vds.Variable);
}

void Emitter::EmitDecomposeDeclarationStatement(MyBytecodeProcessor& bp, BoundStatement* pDecompDecl)
{
    BoundDecomposeDeclarationStatement& dds = pDecompDecl->decomp;
    StructSymbol& ss = dds.Struct->structsym;

    EmitExpression(bp, dds.Decomposable);

    size_t kCount = stbds_arrlenu(ss.Fields);
    for (size_t k = 0; k < kCount; k++)
    {
        MySymbol* const& pVariable = dds.Variables[k];
        char* const& lpField = ss.Fields[k].Name;

        // We duplicate since Ldfld, pops the object off the evaluation stack
        bp.Emit(MyOpCode::Dup);
        bp.Emit(MyOpCode::Ldfld, lpField);
        bp.Emit(MyOpCode::Stloc, pVariable);
    }
}

void Emitter::EmitLabelStatement(MyBytecodeProcessor& bp, BoundStatement* pLabel)
{
    BoundLabelStatement& ls = pLabel->label;

    const BoundLabel& Label = ls.Label;
    bp.Emit(MyOpCode::Label, Label.Label);
}

void Emitter::EmitGotoStatement(MyBytecodeProcessor& bp, BoundStatement* pGoto)
{
    BoundGotoStatement& gs = pGoto->gotostmt;

    const BoundLabel& Label = gs.Label;
    bp.Emit(MyOpCode::Jmp, Label.Label);
}

void Emitter::EmitConditionalGotoStatement(MyBytecodeProcessor& bp, BoundStatement* pCondGoto)
{
    BoundConditionalGotoStatement& cgs = pCondGoto->cgotostmt;

    EmitExpression(bp, cgs.Condition);

    const BoundLabel& Label = cgs.Label;
    bp.Emit(cgs.JumpIfTrue ? MyOpCode::Jnz : MyOpCode::Jz, Label.Label);
}

void Emitter::EmitReturnStatement(MyBytecodeProcessor& bp, BoundStatement* pReturn)
{
    BoundReturnStatement& rs = pReturn->ret;

    if (rs.Expr)
    {
        EmitExpression(bp, rs.Expr);
    }
    bp.Emit(MyOpCode::Ret);
}

void Emitter::EmitExpression(MyBytecodeProcessor& bp, BoundExpression* pExpression)
{
    switch (pExpression->Kind)
    {
        case BoundExpressionKind::Empty:
            break;
        case BoundExpressionKind::Literal:
            EmitLiteralExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Unary:
            EmitUnaryExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Binary:
            EmitBinaryExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Ternary:
            EmitTernaryExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Increment:
            EmitIncrementExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Name:
            EmitNameExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Assignment:
            EmitAssignmentExpression(bp, pExpression);
            break;
        case BoundExpressionKind::OperatorNew:
            EmitOperatorNewExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Call:
            EmitCallExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Index:
            EmitIndexExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Field:
            EmitFieldExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Conversion:
            EmitConversionExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Array:
            EmitArrayExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Cast:
            EmitCastExpression(bp, pExpression);
            break;
        case BoundExpressionKind::Instance:
            EmitInstanceExpression(bp, pExpression);
            break;
        default:
            MY_ASSERT(false, "EmitterError: Unexpected expression kind '%s'\n", BoundExpressionKindString(pExpression->Kind));
            break;
    }
}

void Emitter::EmitLiteralExpression(MyBytecodeProcessor& bp, BoundExpression* pLiteral)
{
    BoundLiteralExpression& le = pLiteral->literal;
    bp.Emit(le.Value);
}

void Emitter::EmitNameExpression(MyBytecodeProcessor& bp, BoundExpression* pName)
{
    BoundNameExpression& ne = pName->name;

    if (ne.Symbol->Kind == SymbolKind::Function)
    {
        MY_ASSERT(false, "[NotImplemented]: Callable expressions: '%s'", ne.Symbol->Name);
    }
    else
    {
        // NOTE: Are we assuming that name symbols can only be Variables?
        const bool bIsLocal = ne.Symbol->varsym.IsLocal;
        bp.Emit(bIsLocal ? MyOpCode::Ldloc : MyOpCode::Ldglo, ne.Symbol);
    }
}

void Emitter::EmitUnaryExpression(MyBytecodeProcessor& bp, BoundExpression* pUnary)
{
    BoundUnaryExpression& ue = pUnary->unary;

    EmitExpression(bp, ue.Rhs);

    const bool bIsInt = ue.Rhs->Type() == My_Defaults.IntType || ue.Rhs->Type() == My_Defaults.UintType;

    switch (ue.Operator->OperatorKind)
    {
        case BoundUnaryOperatorKind::Negation:
            bp.Emit(bIsInt ? MyOpCode::Neg : MyOpCode::Negf);
            break;
        case BoundUnaryOperatorKind::LogicalNegation:
            bp.Emit(MyOpCode::Ldc, int64_t(0));
            bp.Emit(bIsInt ? MyOpCode::Ceq : MyOpCode::Ceqf);
            break;
        case BoundUnaryOperatorKind::BitwiseNegation:
            bp.Emit(MyOpCode::Not);
            break;
        default:
            MY_ASSERT(false, "Unexpected unary operator '%s'", TokenKindString(ue.Operator->Kind));
            break;
    }
}

void Emitter::EmitBinaryExpression(MyBytecodeProcessor& bp, BoundExpression* pBinary)
{
    static int32_t iNextLogicalOpLabel = 0l;

    BoundBinaryExpression& be = pBinary->binary;

    const BoundBinaryOperator& Op = *be.Operator;

    // Logical Operators (&&, ||)
    if (Op.OperatorKind == BoundBinaryOperatorKind::LogicalAnd || Op.OperatorKind == BoundBinaryOperatorKind::LogicalOr)
    {
        char* const lpJmpLabel = MyGetCachedStringV("__logic_%d", iNextLogicalOpLabel++);

        EmitExpression(bp, be.Lhs);
        bp.Emit(MyOpCode::Dup);
        {
            MyOpCode Code = Op.OperatorKind == BoundBinaryOperatorKind::LogicalAnd ? MyOpCode::Jz : MyOpCode::Jnz;
            bp.Emit(Code, lpJmpLabel);
        }
        EmitExpression(bp, be.Rhs);
        bp.Emit(MyOpCode::Label, lpJmpLabel);

        return;
    }


    EmitExpression(bp, be.Lhs);
    EmitExpression(bp, be.Rhs);

    // +(string, string)
    // ==(string, string)
    // !=(string, string)
    if (be.Lhs->Type() == My_Defaults.StringType && be.Rhs->Type() == My_Defaults.StringType)
    {
        switch (Op.OperatorKind)
        {
            case BoundBinaryOperatorKind::Addition:
                bp.Emit(MyOpCode::Calli, "__strcat");
                break;
            case BoundBinaryOperatorKind::Equality:
                bp.Emit(MyOpCode::Calli, "__strcmp");
                break;
            case BoundBinaryOperatorKind::NonEquality:
                bp.Emit(MyOpCode::Calli, "__strcmp");
                bp.Emit(MyOpCode::Ldc, int64_t(0));
                bp.Emit(MyOpCode::Ceq);
                break;
            default: break;
        }
        return;
    }

    // ==(object, object)
    // !=(object, object)
    if (be.Lhs->Type() == My_Defaults.ObjectType && be.Rhs->Type() == My_Defaults.ObjectType)
    {
        switch (Op.OperatorKind)
        {
            case BoundBinaryOperatorKind::Equality:
                bp.Emit(MyOpCode::Calli, "__equals");
                break;
            case BoundBinaryOperatorKind::NonEquality:
                bp.Emit(MyOpCode::Calli, "__equals");
                bp.Emit(MyOpCode::Ldc, int64_t(0));
                bp.Emit(MyOpCode::Ceq);
                break;
            default: break;
        }
        return;
    }

   
    const bool bIsFloatOperation = be.Lhs->Type() == My_Defaults.FloatType;

    switch (Op.OperatorKind)
    {
        case BoundBinaryOperatorKind::Addition:
            bp.Emit(bIsFloatOperation ? MyOpCode::Addf : MyOpCode::Add);
            break;
        case BoundBinaryOperatorKind::Subtraction:
            bp.Emit(bIsFloatOperation ? MyOpCode::Subf : MyOpCode::Sub);
            break;
        case BoundBinaryOperatorKind::Multiplication:
            bp.Emit(bIsFloatOperation ? MyOpCode::Mulf : MyOpCode::Mul);
            break;
        case BoundBinaryOperatorKind::Division:
            bp.Emit(bIsFloatOperation ? MyOpCode::Divf : MyOpCode::Div);
            break;
        case BoundBinaryOperatorKind::Exponentiation:
            bp.Emit(bIsFloatOperation ? MyOpCode::Powf : MyOpCode::Pow);
            break;
        case BoundBinaryOperatorKind::LeftShift:
            bp.Emit(MyOpCode::Lsh);
            break;
        case BoundBinaryOperatorKind::RightShift:
            bp.Emit(MyOpCode::Rsh);
            break;
        case BoundBinaryOperatorKind::Modulo:
            bp.Emit(MyOpCode::Mod);
            break;
        case BoundBinaryOperatorKind::BitwiseAnd:
            bp.Emit(MyOpCode::And);
            break;
        case BoundBinaryOperatorKind::BitwiseOr:
            bp.Emit(MyOpCode::Or);
            break;
        case BoundBinaryOperatorKind::BitwiseXor:
            bp.Emit(MyOpCode::Xor);
            break;
        case BoundBinaryOperatorKind::Equality:
            bp.Emit(bIsFloatOperation ? MyOpCode::Ceqf : MyOpCode::Ceq);
            break;
        case BoundBinaryOperatorKind::NonEquality:
            bp.Emit(bIsFloatOperation ? MyOpCode::Cneqf : MyOpCode::Cneq);
            break;
        case BoundBinaryOperatorKind::Less:
            bp.Emit(bIsFloatOperation ? MyOpCode::Cltf : MyOpCode::Clt);
            break;
        case BoundBinaryOperatorKind::LessOrEqual:
            bp.Emit(bIsFloatOperation ? MyOpCode::Cltef : MyOpCode::Clte);
            break;
        case BoundBinaryOperatorKind::Greater:
            bp.Emit(bIsFloatOperation ? MyOpCode::Cgtf : MyOpCode::Cgt);
            break;
        case BoundBinaryOperatorKind::GreaterOrEqual:
            bp.Emit(bIsFloatOperation ? MyOpCode::Cgtef : MyOpCode::Cgte);
            break;
        default:
            MY_ASSERT(false, "NotImplementedException*?: '%s'", TokenKindString(Op.Kind));
            break;
    } // end switch
}

void Emitter::EmitTernaryExpression(MyBytecodeProcessor& bp, BoundExpression* pTernary)
{
    static int32_t iNextTernaryLabel = 0;

    BoundTernaryExpression& te = pTernary->ternary;

    char* const& lpEndLabel     = MyGetCachedStringV("__tern_%d", iNextTernaryLabel++);
    char* const& lpIfFalseLabel = MyGetCachedStringV("__tern_%d", iNextTernaryLabel++);

    EmitExpression(bp, te.Condition);
    bp.Emit(MyOpCode::Jz, lpIfFalseLabel);
    EmitExpression(bp, te.Then);
    bp.Emit(MyOpCode::Jmp, lpEndLabel);
    bp.Emit(MyOpCode::Label, lpIfFalseLabel);
    EmitExpression(bp, te.Else);
    bp.Emit(MyOpCode::Label, lpEndLabel);
}

void Emitter::EmitIncrementExpression(MyBytecodeProcessor& bp, BoundExpression* pIncrement)
{
    BoundIncrementExpression& inc = pIncrement->inc;

    const bool bIsIntegerIncrement = inc.Lvalue->Type == My_Defaults.IntType || inc.Lvalue->Type == My_Defaults.UintType;

    EmitExpression(bp, inc.Increment);
    bp.Emit(bIsIntegerIncrement ? MyOpCode::Inc : MyOpCode::Incf, inc.Lvalue);
}

void Emitter::EmitAssignmentExpression(MyBytecodeProcessor& bp, BoundExpression* pAssignment)
{
    // TODO: This probably need some fixing
    BoundAssignmentExpression& ae = pAssignment->assign;

    EmitExpression(bp, ae.Rhs);
    bp.Emit(MyOpCode::Dup);

    switch (ae.Lhs->Kind)
    {
        case BoundExpressionKind::Name:
        {
            bp.Emit(ae.Variable->varsym.IsLocal ? MyOpCode::Stloc : MyOpCode::Stglo, ae.Variable);
            break;
        }
        case BoundExpressionKind::Index:
        {
            MyType* pArrayType = ae.Lhs->index.Sequence->Type();
            MY_ASSERT(pArrayType->Kind == 1u, "Error: Must be an array type");

            const size_t kRank = stbds_arrlenu(pArrayType->Array->Lengths);
            for (size_t k = 0; k < kRank; k++)
            {
                EmitExpression(bp, ae.Lhs->index.Indices[k]);
            }
            EmitExpression(bp, ae.Lhs->index.Sequence);
            bp.Emit(MyOpCode::Stelem, uint32_t(kRank));
            break;
        }
        case BoundExpressionKind::Field:
        {
            EmitExpression(bp, ae.Lhs->field.Object);
            bp.Emit(MyOpCode::Stfld, ae.Lhs->field.Field);
            break;
        }
        default:
        {
            MY_ASSERT(false, "Invalid assignment expression");
            break;
        }
    }
}

void Emitter::EmitCallExpression(MyBytecodeProcessor& bp, BoundExpression* pCall)
{
    BoundCallExpression& ce = pCall->call;

    // Emit object after arguments (will be on top of stack)
    if (ce.Callable->Kind == BoundExpressionKind::Field)
    {
        BoundFieldExpression& fe = ce.Callable->field;
        EmitExpression(bp, fe.Object);
    }

    const size_t kArgc = stbds_arrlenu(ce.Arguments);
    for (size_t k = 0; k < kArgc; k++)
    {
        EmitExpression(bp, ce.Arguments[k]);
    }
    
    char* const& lpName = ce.Function->Name;

    if (bp.IsRegistered(lpName))
    {
        bp.Emit(MyOpCode::Calli, lpName);
    }
    else
    {
        // Inline the functions defined as inline
        FunctionSymbol& fs = ce.Function->funcsym;
        if (fs.IsInline && kArgc == 0u && ce.Callable->Kind != BoundExpressionKind::Field)
        {
            const BoundProgram::BoundFunction& bf = stbds_shget(m_FunctionBodies, lpName);
            const auto&[_, pBody] = bf;
            EmitFunctionBody(bp, pBody->block.Statements, true);
        }
        else
        {
            if (fs.IsInline)
            {
                DebugLog::Warn("[NotImplemented]: Failed to inlining function ('%s') with arguments", lpName);
            }

            bp.Emit(MyOpCode::Call, lpName);
        }
    }
}

void Emitter::EmitIndexExpression(MyBytecodeProcessor& bp, BoundExpression* pIndex)
{
    BoundIndexExpression& ie = pIndex->index;

    MyType* pArrayType = ie.Sequence->Type();
    MY_ASSERT(pArrayType->Kind == 1u, "Error: Must be an array type");

    const size_t kRank = stbds_arrlenu(pArrayType->Array->Lengths);
    for (size_t k = 0; k < kRank; k++)
    {
        EmitExpression(bp, ie.Indices[k]);
    }

    EmitExpression(bp, ie.Sequence);
    bp.Emit(MyOpCode::Ldelem, uint32_t(kRank));
}

void Emitter::EmitFieldExpression(MyBytecodeProcessor& bp, BoundExpression* pField)
{
    BoundFieldExpression& fe = pField->field;

    EmitExpression(bp, fe.Object);
    bp.Emit(MyOpCode::Ldfld, fe.Field);
}

void Emitter::EmitOperatorNewExpression(MyBytecodeProcessor& bp, BoundExpression* pOperatorNew)
{
    // TODO: This probably need some fixing
    BoundOperatorNewExpression& one = pOperatorNew->opnew;

    MyAssembly* const pAssembly = bp.GetAssembly();

    if (one.Type->Kind == MY_TYPE_KIND_STRUCT)
    {
        MyStruct* const& pKlass = one.Type->Klass;
        bp.Emit(MyOpCode::Newobj, pKlass->Name);
        
        if (one.Initializers)
        {
            for (size_t k = 0; k < stbds_arrlenu(one.Initializers); k++)
            {
                const auto&[lpField, pValue] = one.Initializers[k];

                EmitExpression(bp, pValue);
                bp.Emit(MyOpCode::Dup, 1u);
                bp.Emit(MyOpCode::Stfld, lpField);
            }
        }
    }
    else
    {
        MyArrayShape Shape = {};

        for (size_t k = 0; k < stbds_arrlenu(one.Type->Array->Lengths); k++)
        {
            Shape.Lengths[k] = one.Type->Array->Lengths[k];
        }

        const uint32_t kKlassIndex = MyGetElementIndex(pAssembly->Klasses, stbds_arrlenu(pAssembly->Klasses), one.Type->Array->Klass);
        const uint32_t kShapeIndex = stbds_arrlenu(pAssembly->ShapeCache);
        stbds_arrpush(pAssembly->ShapeCache, Shape);

        bp.Emit(MyOpCode::Newarray, kKlassIndex, kShapeIndex);
    }
}

void Emitter::EmitArrayExpression(MyBytecodeProcessor& bp, BoundExpression* pArray)
{
    // MY_NOT_IMPLEMENTED();
    BoundArrayExpression& ae = pArray->array;

    const uint32_t kItemCount = stbds_arrlenu(ae.Items);
    for (size_t k = 0; k < kItemCount; k++)
    {
        EmitExpression(bp, ae.Items[k]);
    }

    uint32_t Flags = 0u;
    MY_ENCODE_OBJECT_INFO(Flags, MyValueKind::Array, kItemCount);

    bp.Emit(MyOpCode::Ldarr, Flags);
    /*BoundArrayExpression& ae = pArray->array;
    const ArrayType& at = ae.Type->arraytype;

    const size_t kItemCount = stbds_arrlenu(ae.Items);
    const size_t kTypeCount = stbds_arrlenu(at.Counts);

    if (kItemCount == kTypeCount)
    {
        for (size_t k = 0; k < kItemCount; k++)
        {
            EmitExpression(bp, ae.Items[k]);
        }

        uint32_t Flags = 0u;
        MY_ENCODE_OBJECT_INFO(Flags, MyObjectKind::Array, kItemCount);

        bp.Emit(MyOpCode::Ldobj, Flags);
    }
    else
    {
        EmitExpression(bp, ae.Default);
        uint64_t kCount = 1ull;
        for (size_t k = 0; k < kTypeCount; k++)
        {
            kCount *= at.Counts[k];
        }

        bp.Emit(MyOpCode::Newarray, uint32_t(kCount));
    }*/
}

void Emitter::EmitCastExpression(MyBytecodeProcessor& bp, BoundExpression* pCast)
{
    BoundCastExpression& cast = pCast->cast;

    MyAssembly* const pAssembly = bp.GetAssembly();

    EmitExpression(bp, cast.Expr);

    const bool bIsArrayType = cast.Type->Kind == MY_TYPE_KIND_ARRAY;
    MyStruct* pKlass = bIsArrayType ? cast.Type->Array->Klass : cast.Type->Klass;

    if (pKlass == My_Defaults.ObjectStruct)
    {
        // No need to emit an instruction for this
        return;
    }

    const uint32_t kKlassIndex = MyGetElementIndex(pAssembly->Klasses, stbds_arrlenu(pAssembly->Klasses), pKlass);

    bp.Emit(MyOpCode::Cast, kKlassIndex, bIsArrayType ? 1u : 0u);
}

void Emitter::EmitConversionExpression(MyBytecodeProcessor& bp, BoundExpression* pConversion)
{
    static const auto ConvToInt = [&bp](MyType* pTypeFrom) -> void
    {
        if (pTypeFrom == My_Defaults.StringType)
        {
            bp.Emit(MyOpCode::Calli, MyGetCachedString("__cvtoint"));
        }
        else if (pTypeFrom == My_Defaults.FloatType)
        {
            bp.Emit(MyOpCode::Cvftoi);
        }
    };
    
    static const auto ConvToUint = [&bp](MyType* pTypeFrom) -> void
    {
        if (pTypeFrom == My_Defaults.StringType)
        {
            bp.Emit(MyOpCode::Calli, MyGetCachedString("__cvtouint"));
        }
        else if (pTypeFrom == My_Defaults.FloatType)
        {
            bp.Emit(MyOpCode::Cvftou);
        }
    };
    
    static const auto ConvToFloat = [&bp](MyType* pTypeFrom) -> void
    {
        if (pTypeFrom == My_Defaults.StringType)
        {
            bp.Emit(MyOpCode::Calli, MyGetCachedString("__cvtofloat"));
        }
        else
        {
            bp.Emit(MyOpCode::Cvtof);
        }
    };

    BoundConversionExpression& ce = pConversion->conv;

    EmitExpression(bp, ce.Expr);
    MyType* pType = ce.Expr->Type();

    if (ce.Type == My_Defaults.ObjectType || pType == My_Defaults.ObjectType)
    { }
    else if (ce.Type->Kind == MY_TYPE_KIND_ARRAY && pType->Kind == MY_TYPE_KIND_ARRAY)
    { }
    else if (ce.Type == My_Defaults.BooleanType)
    {
        ConvToInt(pType);
    }
    else if (ce.Type == My_Defaults.IntType)
    {
        ConvToInt(pType);
    }
    else if (ce.Type == My_Defaults.UintType)
    {
        ConvToUint(pType);
    }
    else if (ce.Type == My_Defaults.FloatType)
    {
        ConvToFloat(pType);
    }
    else if (ce.Type == My_Defaults.StringType)
    {
        if (pType == My_Defaults.FloatType)
        {
            bp.Emit(MyOpCode::Calli, MyGetCachedString("__cvfloattostring"));
        }
        else if (pType == My_Defaults.UintType)
        {
            bp.Emit(MyOpCode::Calli, MyGetCachedString("__cvuinttostring"));
        }
        else if (pType == My_Defaults.BooleanType)
        {
            bp.Emit(MyOpCode::Calli, MyGetCachedString("__cvbooltostring"));
        }
        else
        {
            bp.Emit(MyOpCode::Calli, MyGetCachedString("__cvinttostring"));
        }
    }
    else
    {
        // const char* const& lpTypename0 = ce.Expr->Type()->Fullname;
        // const char* const& lpTypename1 = ce.Type->Fullname;
        // MY_ASSERT(false, "Unexpected conversion from '%s' to '%s'\n", lpTypename0, lpTypename1);
        MY_ASSERT(false, "Error: Invalid conversion");
    }
}

void Emitter::EmitInstanceExpression(MyBytecodeProcessor& bp, BoundExpression* pInstance)
{
    MY_NOT_IMPLEMENTED();
    /*BoundInstanceExpression& ie = pInstance->inst;

    bp.Emit(MyOpCode::Newobj, ie.Type->Fullname);
    bp.Emit(MyOpCode::Call, ie.Type->Fullname);*/
}

void Emitter::EmitFunctionBody(MyBytecodeProcessor& bp, BoundStatement** pStatements, bool bIsInline)
{
    const size_t kCount = stbds_arrlenu(pStatements);
    if (bIsInline)
    {
        for (int32_t k = 0; k < kCount; k++)
        {
            BoundStatement* const& pStmt = pStatements[k];
            switch (pStmt->Kind)
            {
                case BoundStatementKind::Return:
                {
                    BoundReturnStatement& rs = pStmt->ret;
                    if (rs.Expr)
                    {
                        EmitExpression(bp, rs.Expr);
                    }
                    break;
                }
                default:
                {
                    EmitStatement(bp, pStmt);
                    break;
                }
            }
        }
    }
    else
    {
        for (int32_t k = 0; k < kCount; k++)
        {
            BoundStatement* const& pStmt = pStatements[k];
            EmitStatement(bp, pStmt);
        }
    }
}
#pragma endregion

#pragma region Bytecode_Serialization_and_Deserialization
static char OpCodeHasArgument(MyOpCode Code) noexcept
{
    switch (Code)
    {
        case MyOpCode::Cast:
        case MyOpCode::Newarray:
            return 2;
        case MyOpCode::Ldc:
        case MyOpCode::Ldcf:
        case MyOpCode::Ldstr:
        case MyOpCode::Dup:
        case MyOpCode::Ldarr:
        case MyOpCode::Ldobj:
        case MyOpCode::Ldarg:
        case MyOpCode::Ldloc:
        case MyOpCode::Stloc:
        case MyOpCode::Ldglo:
        case MyOpCode::Stglo:
        case MyOpCode::Ldelem:
        case MyOpCode::Stelem:
        case MyOpCode::Ldfld:
        case MyOpCode::Stfld:
        case MyOpCode::Newobj:
        case MyOpCode::Inc:
        case MyOpCode::Incf:
        case MyOpCode::Jmp:
        case MyOpCode::Jnz:
        case MyOpCode::Jz:
        case MyOpCode::Label:
        case MyOpCode::Func:
        case MyOpCode::Call:
        case MyOpCode::Calli:
            return 1;
        // Utility Ops
        default:
            return 0;
    }
}

struct SerializationMetadata
{
    uint64_t    MagicOne = 0ull;
    uint64_t    MagicTwo = 0ull;
    std::string Version  = {};
};

static constexpr uint64_t s_MagicNumber_One = 0xF33D0D3AD0B33Ful;
static constexpr uint64_t s_MagicNumber_Two = 0xB33F0D3AD0F33Dul;

namespace PnBS
{

    // Serialize
    template<>
    struct Encode<::SerializationMetadata>
    {
        void operator()(BufferWriter& Out, const ::SerializationMetadata& Meta) noexcept
        {
            Out << Meta.MagicOne;
            Out << Meta.MagicTwo;
            Out << Meta.Version;
        }
    };

    template<>
    struct Encode<::MyInstruction>
    {
        void operator()(BufferWriter& Out, const ::MyInstruction& Inst) noexcept
        {
            Out << uint8_t(Inst.Code);
            if (char iArgc = OpCodeHasArgument(Inst.Code); iArgc)
            {
                Out << Inst.Arg0;
                if (iArgc == 2)
                {
                    Out << Inst.Arg1;
                }
            }
        }
    };

    template<>
    struct Encode<::MyValue>
    {
        void operator()(BufferWriter& Out, const ::MyValue& Value) noexcept
        {
            Out << uint16_t(Value.Kind);
            switch (Value.Kind)
            {
                case MyValueKind::Null:    Out << uint64_t(); break;
                case MyValueKind::Bool:    Out << Value.B08;  break;
                case MyValueKind::Int64:   Out << Value.I64;  break;
                case MyValueKind::Uint64:  Out << Value.U64;  break;
                case MyValueKind::Float64: Out << Value.F64;  break;
                case MyValueKind::String:
                {
                    Out << std::string(Value.Str->Chars);
                    break;
                }
                case MyValueKind::Array:
                {
                    Out << Value.Arr->Count;
                    for (size_t k = 0; k < Value.Arr->Count; k++)
                    {
                        MY_NOT_IMPLEMENTED();
                        // Out << Value.Arr->Items[k];
                    }
                    break;
                }
                default:
                {
                    MY_ASSERT(false, "We should not serialize a complex object ('%s')", ValueKindString(Value.Kind));
                    break;
                }
            }
        }
    };

    template<>
    struct Encode<::MyArrayShape>
    {
        void operator()(BufferWriter& Out, const ::MyArrayShape& Shape) noexcept
        {
            MyAssembly* Assembly = MyContextGet()->Assembly;
            Out << Shape.Lengths[0] << Shape.Lengths[1] << Shape.Lengths[2] << Shape.Lengths[3] <<
                   Shape.Lengths[4] << Shape.Lengths[5] << Shape.Lengths[6] << Shape.Lengths[7];
        }
    };
    
    template<>
    struct Encode<::MyField>
    {
        void operator()(BufferWriter& Out, const ::MyField& Field) noexcept
        {
            MyAssembly* Assembly = MyContextGet()->Assembly;

            Out << std::string(Field.Name);
            Out << Field.Offset;
            Out << Field.Attributes;
            Out << MyGetElementIndex(Assembly->Klasses, stbds_arrlenu(Assembly->Klasses), Field.Klass);
            // Out << Field.Type;
            // Out << Field.Data;
        }
    };
    
    template<>
    struct Encode<::MyStruct*>
    {
        void operator()(BufferWriter& Out, const ::MyStruct* pKlass) noexcept
        {
            Out << std::string(pKlass->Name);
            Out << pKlass->Attributes;
            Out << pKlass->Size;
            Out << pKlass->Guid.Data1[0];
            Out << pKlass->Guid.Data1[1];

            uint32_t kCount = 0ul;

            kCount = (uint32_t)stbds_arrlenu(pKlass->Fields);
            Out << kCount;
            for (size_t k = 0; k < kCount; k++)
            {
                Out << pKlass->Fields[k];
            }

            /*kCount = (uint32_t)stbds_arrlenu(pKlass->Methods);
            Out << kCount;
            for (size_t k = 0; k < kCount; k++)
            {
                Out << pKlass->Methods[k];
            }*/
        }
    };

    // Deserialize
    template<>
    struct Decode<::SerializationMetadata>
    {
        void operator()(BufferReader& In, ::SerializationMetadata& Meta) noexcept
        {
            In >> Meta.MagicOne;
            In >> Meta.MagicTwo;
            In >> Meta.Version;
        }
    };

    template<>
    struct Decode<::MyInstruction>
    {
        void operator()(BufferReader& In, ::MyInstruction& Inst) noexcept
        {
            uint8_t Code = 0u;

            In >> Code; Inst.Code = MyOpCode(Code);
            if (char iArgc = OpCodeHasArgument(Inst.Code); iArgc)
            {
                In >> Inst.Arg0;
                if (iArgc == 2)
                {
                    In >> Inst.Arg1;
                }
            }
        }
    };

    template<>
    struct Decode<::MyValue>
    {
        void operator()(BufferReader& In, ::MyValue& Value) noexcept
        {
            uint16_t Kind = 0u;
            In >> Kind;
            new(&Value) MyValue{ MyValueKind(Kind)  };
            
            switch (Value.Kind)
            {
                case MyValueKind::Null:    In >> Value.U64; break;
                case MyValueKind::Bool:    In >> Value.B08; break;
                case MyValueKind::Int64:   In >> Value.I64; break;
                case MyValueKind::Uint64:  In >> Value.U64; break;
                case MyValueKind::Float64: In >> Value.F64; break;
                case MyValueKind::String:
                {
                    std::string s = {};
                    In >> s;
                    Value.Str = MyStringNew(MyContextGet(), s);
                    break;
                }
                case MyValueKind::Array:
                {
                    MY_NOT_IMPLEMENTED();
                    /*uint64_t kCount = 0;
                    In >> kCount;
                    MyArray* const& pArray = MyArrayNew(MyContextGet(), { kCount });
                    for (size_t k = 0; k < kCount; k++)
                    {
                        In >> pArray->Items[k];
                    }
                    Value.Arr = pArray;*/
                    break;
                }
                default:
                {
                    MY_ASSERT(false, "We should not deserialize a complex object ('%s')", ValueKindString(Value.Kind));
                    break;
                }
            }
        }
    };

    template<>
    struct Decode<::MyArrayShape>
    {
        void operator()(BufferReader& In, ::MyArrayShape& Shape) noexcept
        {
            MyAssembly* Assembly = MyContextGet()->Assembly;
            In >> Shape.Lengths[0] >> Shape.Lengths[1] >> Shape.Lengths[2] >> Shape.Lengths[3] >>
                  Shape.Lengths[4] >> Shape.Lengths[5] >> Shape.Lengths[6] >> Shape.Lengths[7];
        }
    };
    
    template<>
    struct Decode<::MyField>
    {
        void operator()(BufferReader& In, ::MyField& Field) noexcept
        {
            MyAssembly* Assembly = MyContextGet()->Assembly;

            std::string s = {};
            uint32_t kIndex = 0ul;

            In >> s;
            In >> Field.Offset;
            In >> Field.Attributes;
            In >> kIndex;

            Field.Name = MyGetCachedString(s);
            Field.Klass = Assembly->Klasses[kIndex];
        }
    };

    template<>
    struct Decode<::MyStruct*>
    {
        void operator()(BufferReader& In, ::MyStruct* pKlass) noexcept
        {
            std::string s = {};
            uint64_t kData[2] = {};

            In >> s;
            In >> pKlass->Attributes;
            In >> pKlass->Size;
            In >> kData[0];
            In >> kData[1];

            pKlass->Name = MyGetCachedString(s);
            pKlass->Guid = MyGuidCreate(MyContextGet(), kData[0], kData[1]);

            uint32_t kCount = 0ul;

            In >> kCount;
            for (size_t k = 0; k < kCount; k++)
            {
                MyField field = {};
                In >> field;
                stbds_arrpush(pKlass->Fields, field);
            }

            /*In >> kCount;
            for (size_t k = 0; k < kCount; k++)
            {
                MyMethod method = {};
                In >> method;
                stbds_arrpush(pKlass->Methods, method);
            }*/
        }
    };

}

void _My_Emitter_Serialize(const MyAssembly* pAssembly, const std::string& Path) noexcept
{
    PnBS::BufferWriter Out;
    {
        SerializationMetadata Meta = {};
        Meta.MagicOne = s_MagicNumber_One;
        Meta.MagicTwo = s_MagicNumber_Two;
        Meta.Version = MY_VERSION;

        Out << Meta;
    }

    uint32_t kCount = 0ul;

    /// TYPE INFO
    // Fields
    kCount = stbds_shlenu(pAssembly->Fields);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        const auto& [lpField, kIndex] = pAssembly->Fields[k];
        Out << std::string(lpField);
        Out << kIndex;
    }
    // Structs
    kCount = stbds_arrlenu(pAssembly->Klasses);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        Out << pAssembly->Klasses[k];
    }
    // Array Shapes
    kCount = stbds_arrlenu(pAssembly->ShapeCache);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        Out << pAssembly->ShapeCache[k];
    }

    /// RUNTIME DATA
    // Constants
    kCount = stbds_arrlenu(pAssembly->Constants);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        Out << pAssembly->Constants[k];
    }
    // Globals
    kCount = stbds_shlenu(pAssembly->Globals);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        const auto& [lpName, kIndex] = pAssembly->Globals[k];
        Out << std::string(lpName);
        Out << kIndex;
    }
    // Internal functions
    kCount = stbds_shlenu(pAssembly->Internals);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        const auto& [lpName, pfnNative] = pAssembly->Internals[k];
        Out << std::string(lpName);
        Out << uint64_t(pfnNative);
    }
    // Labels
    kCount = stbds_shlenu(pAssembly->Labels);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        const auto& [lpName, kAddress] = pAssembly->Labels[k];
        Out << std::string(lpName);
        Out << kAddress;
    }
    // Functions
    kCount = stbds_shlenu(pAssembly->Functions);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        const auto& [lpName, kAddress] = pAssembly->Functions[k];
        Out << std::string(lpName);
        Out << kAddress;
    }
    // Bytecode
    kCount = stbds_arrlenu(pAssembly->Code);
    Out << kCount;
    for (size_t k = 0; k < kCount; k++)
    {
        Out << pAssembly->Code[k];
    }

    // WRITE TO FILE
    Out.Dump(Path.c_str());
}

void _My_Emitter_Deserialize(MyAssembly* pAssembly, const std::string& Path) noexcept
{
    stbds_arrfree(pAssembly->Constants);
    stbds_shfree(pAssembly->Globals);
    stbds_shfree(pAssembly->Internals);
    stbds_shfree(pAssembly->Labels);
    stbds_shfree(pAssembly->Functions);
    stbds_arrfree(pAssembly->Code);
    stbds_arrfree(pAssembly->Klasses);
    stbds_arrfree(pAssembly->ShapeCache);
    stbds_shfree(pAssembly->Fields);

    PnBS::BufferReader In = PnBS::BufferReader(Path.c_str());
    {
        SerializationMetadata Meta = {};
        In >> Meta;
        const bool bIsValidBytecodeFile = Meta.MagicOne == s_MagicNumber_One &&
                                          Meta.MagicTwo == s_MagicNumber_Two &&
                                          Meta.Version  == MY_VERSION;
        MY_ASSERT(bIsValidBytecodeFile, "Fatal Error: '%s' is not a valid My# Bytecode File!", Path.c_str());
    }

    uint32_t uSize = 0;

    /// TYPE INFO
    // Fields
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        AddressMap am = {};
        std::string Field;
        In >> Field;
        am.key = MyGetCachedString(Field);
        In >> am.value;
        stbds_shput(pAssembly->Fields, am.key, am.value);
    }
    // Types
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        MyStruct* pKlass = MyStructCreate(MyContextGet(), "", 0ul);
        In >> pKlass;
        stbds_arrpush(pAssembly->Klasses, pKlass);
    }
    // Array Shapes
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        MyArrayShape Shape = {};
        In >> Shape;
        stbds_arrpush(pAssembly->ShapeCache, Shape);
    }

    /// RUNTIME DATA
    // Constants
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        MyValue v = {};
        In >> v;
        stbds_arrpush(pAssembly->Constants, v);
    }
    // Globals
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        AddressMap am = {};
        std::string Name;
        In >> Name;
        am.key = MyGetCachedString(Name);
        In >> am.value;
        stbds_shput(pAssembly->Globals, am.key, am.value);
    }
    // Internal functions
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        InternalFunction ifunc = {};
        std::string Name;
        In >> Name;
        ifunc.key = MyGetCachedString(Name);
        In >> *(uint64_t*)&ifunc.value;
        stbds_shputs(pAssembly->Internals, ifunc);
    }
    // Labels
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        AddressMap am = {};
        std::string Name;
        In >> Name;
        am.key = MyGetCachedString(Name);
        In >> am.value;
        stbds_shput(pAssembly->Labels, am.key, am.value);
    }
    // Functions
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        AddressMap am = {};
        std::string Name;
        In >> Name;
        am.key = MyGetCachedString(Name);
        In >> am.value;
        stbds_shput(pAssembly->Functions, am.key, am.value);
    }
    // Bytecode
    In >> uSize;
    for (size_t k = 0; k < uSize; k++)
    {
        MyInstruction inst = {};
        In >> inst;
        stbds_arrpush(pAssembly->Code, inst);
    }
}


void _My_Initialize_BuiltinsMap() noexcept
{
    static const auto RegisterBuiltin = [&](char* const& lpName, pfnMyInternalFunction pfnBuiltin) -> void
    {
        const InternalFunction ifunc = { MyGetCachedString(lpName), pfnBuiltin };
        stbds_shputs(s_Builtins, ifunc);
    };

    if (s_Builtins == nullptr)
    {
        static constexpr InternalFunction ifunc = { nullptr, nullptr };
        stbds_shdefault(s_Builtins, nullptr);
        stbds_shdefaults(s_Builtins, ifunc);
    }

    // Core
    RegisterBuiltin("__equals",          _My_Builtin_Equals);
    RegisterBuiltin("__cvbooltostring",  _My_Builtin_CvBoolToString);
    RegisterBuiltin("__cvinttostring",   _My_Builtin_CvIntToString);
    RegisterBuiltin("__cvuinttostring",  _My_Builtin_CvUintToString);
    RegisterBuiltin("__cvfloattostring", _My_Builtin_CvFloatToString);
    // Std
    RegisterBuiltin("Length",      _My_Builtin_Length);
    RegisterBuiltin("Clock",       _My_Builtin_Clock);

    RegisterBuiltin("HeapAlloc",         _My_Builtin_HeapAlloc);
    RegisterBuiltin("HeapCopy",          _My_Builtin_HeapCopy);
    RegisterBuiltin("HeapResize",        _My_Builtin_HeapResize);
    RegisterBuiltin("HeapFree",          _My_Builtin_HeapFree);
    RegisterBuiltin("BufferLength",      _My_Builtin_Buffer_Length);
    RegisterBuiltin("BufferCapacity",    _My_Builtin_Buffer_Capacity);
    RegisterBuiltin("BufferWriteI32",    _My_Builtin_Buffer_WriteI32);
    RegisterBuiltin("BufferWriteI64",    _My_Builtin_Buffer_WriteI64);
    RegisterBuiltin("BufferWriteU32",    _My_Builtin_Buffer_WriteU32);
    RegisterBuiltin("BufferWriteU64",    _My_Builtin_Buffer_WriteU64);
    RegisterBuiltin("BufferWriteF32",    _My_Builtin_Buffer_WriteF32);
    RegisterBuiltin("BufferWriteF64",    _My_Builtin_Buffer_WriteF64);
    RegisterBuiltin("BufferWriteString", _My_Builtin_Buffer_WriteString);
    RegisterBuiltin("BufferAppend",      _My_Builtin_Buffer_Append);
    RegisterBuiltin("BufferReadI32",     _My_Builtin_Buffer_ReadI32);
    RegisterBuiltin("BufferReadI64",     _My_Builtin_Buffer_ReadI64);
    RegisterBuiltin("BufferReadU32",     _My_Builtin_Buffer_ReadU32);
    RegisterBuiltin("BufferReadU64",     _My_Builtin_Buffer_ReadU64);
    RegisterBuiltin("BufferReadF32",     _My_Builtin_Buffer_ReadF32);
    RegisterBuiltin("BufferReadF64",     _My_Builtin_Buffer_ReadF64);
    RegisterBuiltin("BufferReadString",  _My_Builtin_Buffer_ReadString);
    RegisterBuiltin("BufferGet",         _My_Builtin_Buffer_Get);
    // String
    RegisterBuiltin("__strcat",           _My_Builtin_String_Concat);
    RegisterBuiltin("__strcmp",           _My_Builtin_String_Compare);
    RegisterBuiltin("String__Length",     _My_Builtin_String_Length);
    RegisterBuiltin("String__Find",       _My_Builtin_String_Find);
    RegisterBuiltin("String__Substr",     _My_Builtin_String_Substr);
    RegisterBuiltin("String__Split",      _My_Builtin_String_Split);
    RegisterBuiltin("String__StartsWith", _My_Builtin_String_StartsWith);
    RegisterBuiltin("String__EndsWith",   _My_Builtin_String_EndsWith);
    RegisterBuiltin("String__ToUpper",    _My_Builtin_String_ToUpper);
    RegisterBuiltin("String__ToLower",    _My_Builtin_String_ToLower);
    RegisterBuiltin("String__ParseInt",   _My_Builtin_String_ParseInt);
    RegisterBuiltin("String__ParseUint",  _My_Builtin_String_ParseUint);
    RegisterBuiltin("String__ParseFloat", _My_Builtin_String_ParseFloat);
    // StringBuilder
    RegisterBuiltin("StringBuilder__Init",       _My_Builtin_StringBuilder_Init);
    RegisterBuiltin("StringBuilder__Append",     _My_Builtin_StringBuilder_Append);
    RegisterBuiltin("StringBuilder__AppendV",    _My_Builtin_StringBuilder_AppendV);
    RegisterBuiltin("StringBuilder__Write",      _My_Builtin_StringBuilder_Write);
    RegisterBuiltin("StringBuilder__WriteV",     _My_Builtin_StringBuilder_WriteV);
    RegisterBuiltin("StringBuilder__WriteLine",  _My_Builtin_StringBuilder_WriteLine);
    RegisterBuiltin("StringBuilder__WriteLineV", _My_Builtin_StringBuilder_WriteLineV);
    RegisterBuiltin("StringBuilder__ToString",   _My_Builtin_StringBuilder_ToString);
    // Bytes
    RegisterBuiltin("Bytes__Init",             _My_Builtin_Bytes_Init);
    RegisterBuiltin("Bytes__Free",             _My_Builtin_Bytes_Free);
    RegisterBuiltin("Bytes__AddI32",           _My_Builtin_Bytes_AddI32);
    RegisterBuiltin("Bytes__AddU32",           _My_Builtin_Bytes_AddU32);
    RegisterBuiltin("Bytes__AddF32",           _My_Builtin_Bytes_AddF32);
    RegisterBuiltin("Bytes__AddI64",           _My_Builtin_Bytes_AddI64);
    RegisterBuiltin("Bytes__AddU64",           _My_Builtin_Bytes_AddU64);
    RegisterBuiltin("Bytes__AddF64",           _My_Builtin_Bytes_AddF64);
    RegisterBuiltin("Bytes__AddString",        _My_Builtin_Bytes_AddString);
    RegisterBuiltin("Bytes__Append",           _My_Builtin_Bytes_Append);
    RegisterBuiltin("Bytes__GetBufferPointer", _My_Builtin_Bytes_GetBufferPointer);
    // File
    RegisterBuiltin("OpenFile",         _My_Builtin_File_Open);
    RegisterBuiltin("File__Close",      _My_Builtin_File_Close);
    RegisterBuiltin("File__IsOpen",     _My_Builtin_File_IsOpen);
    RegisterBuiltin("File__Read",       _My_Builtin_File_Read);
    RegisterBuiltin("File__ReadBytes",  _My_Builtin_File_ReadBytes);
    RegisterBuiltin("File__Write",      _My_Builtin_File_Write);
    RegisterBuiltin("File__WriteBytes", _My_Builtin_File_WriteBytes);

    // (static) Console
    RegisterBuiltin("Console__Print",     _My_Builtin_Console_Print);
    RegisterBuiltin("Console__Write",     _My_Builtin_Console_Write);
    RegisterBuiltin("Console__WriteLine", _My_Builtin_Console_WriteLine);
    RegisterBuiltin("Console__ReadLine",  _My_Builtin_Console_ReadLine);
    RegisterBuiltin("Console__ReadInt",   _My_Builtin_Console_ReadInt);
    RegisterBuiltin("Console__ReadUint",  _My_Builtin_Console_ReadUint);
    RegisterBuiltin("Console__ReadFloat", _My_Builtin_Console_ReadFloat);
    // (static) Math
    RegisterBuiltin("Math__Abs",   _My_Builtin_Math_Abs);
    RegisterBuiltin("Math__Sin",   _My_Builtin_Math_Sin);
    RegisterBuiltin("Math__Cos",   _My_Builtin_Math_Cos);
    RegisterBuiltin("Math__Tan",   _My_Builtin_Math_Tan);
    RegisterBuiltin("Math__Asin",  _My_Builtin_Math_Asin);
    RegisterBuiltin("Math__Acos",  _My_Builtin_Math_Acos);
    RegisterBuiltin("Math__Atan",  _My_Builtin_Math_Atan);
    RegisterBuiltin("Math__Atan2", _My_Builtin_Math_Atan2);
    RegisterBuiltin("Math__Exp",   _My_Builtin_Math_Exp);
    RegisterBuiltin("Math__Log",   _My_Builtin_Math_Log);
    RegisterBuiltin("Math__Log10", _My_Builtin_Math_Log10);
    RegisterBuiltin("Math__Logb",  _My_Builtin_Math_Logb);
    RegisterBuiltin("Math__Floor", _My_Builtin_Math_Floor);
    RegisterBuiltin("Math__Ceil",  _My_Builtin_Math_Ceil);
    RegisterBuiltin("Math__Sqrt",  _My_Builtin_Math_Sqrt);
    RegisterBuiltin("Math__Cbrt",  _My_Builtin_Math_Cbrt);
    RegisterBuiltin("Math__Nthrt", _My_Builtin_Math_Nthrt);
    RegisterBuiltin("Math__Pow",   _My_Builtin_Math_Pow);
    // (static) Random
    RegisterBuiltin("Random__Int",    _My_Builtin_Random_Int);
    RegisterBuiltin("Random__Uint",   _My_Builtin_Random_Uint);
    RegisterBuiltin("Random__Float",  _My_Builtin_Random_Float);
    RegisterBuiltin("Random__String", _My_Builtin_Random_String);
    RegisterBuiltin("Random__Choice", _My_Builtin_Random_Choice);
}
#pragma endregion


const char* OpCodeString(MyOpCode Code) noexcept
{
    switch (Code)
    {
        case MyOpCode::Ldc:      return "Ldc";
        case MyOpCode::Ldcf:     return "Ldcf";
        case MyOpCode::Ldstr:    return "Ldstr";
        case MyOpCode::Ldarr:    return "Ldarr";
        case MyOpCode::Ldobj:    return "Ldobj";
        case MyOpCode::Ldarg:    return "Ldarg";
        case MyOpCode::Ldloc:    return "Ldloc";
        case MyOpCode::Stloc:    return "Stloc";
        case MyOpCode::Ldglo:    return "Ldglo";
        case MyOpCode::Stglo:    return "Stglo";
        case MyOpCode::Newobj:   return "Newobj";
        case MyOpCode::Newarray: return "Newarray";
        case MyOpCode::Pop:      return "Pop";
        case MyOpCode::Dup:      return "Dup";
        case MyOpCode::Ldelem:   return "Ldelem";
        case MyOpCode::Stelem:   return "Stelem";
        case MyOpCode::Ldfld:    return "Ldfld";
        case MyOpCode::Stfld:    return "Stfld";
        case MyOpCode::Inc:      return "Inc";
        case MyOpCode::Incf:     return "Incf";
        case MyOpCode::Add:      return "Add";
        case MyOpCode::Addf:     return "Addf";
        case MyOpCode::Sub:      return "Sub";
        case MyOpCode::Subf:     return "Subf";
        case MyOpCode::Mul:      return "Mul";
        case MyOpCode::Mulf:     return "Mulf";
        case MyOpCode::Div:      return "Div";
        case MyOpCode::Divf:     return "Divf";
        case MyOpCode::Pow:      return "Pow";
        case MyOpCode::Powf:     return "Powf";
        case MyOpCode::Lsh:      return "Lsh";
        case MyOpCode::Rsh:      return "Rsh";
        case MyOpCode::Mod:      return "Mod";
        case MyOpCode::And:      return "And";
        case MyOpCode::Or:       return "Or";
        case MyOpCode::Xor:      return "Xor";
        case MyOpCode::Neg:      return "Neg";
        case MyOpCode::Not:      return "Not";
        case MyOpCode::Ceq:      return "Ceq";
        case MyOpCode::Ceqf:     return "Ceqf";
        case MyOpCode::Cneq:     return "Cneq";
        case MyOpCode::Cneqf:    return "Cneqf";
        case MyOpCode::Clt:      return "Clt";
        case MyOpCode::Cltf:     return "Cltf";
        case MyOpCode::Clte:     return "Clte";
        case MyOpCode::Cltef:    return "Cltef";
        case MyOpCode::Cgt:      return "Cgt";
        case MyOpCode::Cgtf:     return "Cgtf";
        case MyOpCode::Cgte:     return "Cgte";
        case MyOpCode::Cgtef:    return "Cgtef";
        case MyOpCode::Label:    return "Label";
        case MyOpCode::Jmp:      return "Jmp";
        case MyOpCode::Jnz:      return "Jnz";
        case MyOpCode::Jz:       return "Jz";
        case MyOpCode::Func:     return "Func";
        case MyOpCode::Call:     return "Call";
        case MyOpCode::Ret:      return "Ret";
        case MyOpCode::Calli:    return "Calli";
        case MyOpCode::Nop:      return "Nop";
        case MyOpCode::Halt:     return "Halt";
        default:                  return "[invalid opcode]";
    }
}

