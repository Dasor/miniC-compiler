#include "ast.h"

using namespace miniC;
using namespace llvm;

IRGenerator::IRGenerator()
{
    context = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("miniC", *context);
    builder = std::make_unique<llvm::IRBuilder<>>(*context);

    // Create type variables for cleaner code
    llvm::Type *Int32Ty = llvm::Type::getInt32Ty(*context);
    llvm::Type *FloatTy = llvm::Type::getFloatTy(*context);
    llvm::Type *Int8Ty = llvm::Type::getInt8Ty(*context);
    llvm::Type *VoidTy = llvm::Type::getVoidTy(*context);
    llvm::Type *Int8PtrTy = llvm::PointerType::get(Int8Ty, 0);
    llvm::Type *StringTy = llvm::ArrayType::get(Int8Ty, 0);

    namedValues = {};
    namedTypes = {};

    typeMap = {
        {miniC::Type::Int, Int32Ty},
        {miniC::Type::Float, FloatTy},
        {miniC::Type::Char, Int8Ty},
        {miniC::Type::String, StringTy},
        {miniC::Type::Void, VoidTy},
        {miniC::Type::Unknown, nullptr}};

    auto *PrintfFT = llvm::FunctionType::get(
        /*ReturnTy=*/Int32Ty,
        /*Params=*/{Int8PtrTy},
        /*isVarArg=*/true);

    LibCRegistry["printf"] = PrintfFT;

    TheFPM = std::make_unique<llvm::FunctionPassManager>();
    TheLAM = std::make_unique<llvm::LoopAnalysisManager>();
    TheFAM = std::make_unique<llvm::FunctionAnalysisManager>();
    TheCGAM = std::make_unique<llvm::CGSCCAnalysisManager>();
    TheMAM = std::make_unique<llvm::ModuleAnalysisManager>();
    ThePIC = std::make_unique<llvm::PassInstrumentationCallbacks>();

    TheFPM->addPass(InstCombinePass());
    TheFPM->addPass(ReassociatePass());
    TheFPM->addPass(GVNPass());
    TheFPM->addPass(SimplifyCFGPass());

    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

// Accept method implementations for AST nodes
Value *BinaryExpr::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *LiteralExpr::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *VarExpr::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *DefStmt::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *CallExpr::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

llvm::Function *miniC::Function::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

llvm::Function *Prototype::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *BlockStmt::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *ExprStmt::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *IfStmt::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *ForStmt::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *UnaryExpr::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *ArrayAccessExpr::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}

Value *RetStmt::accept(ASTVisitor &visitor)
{
    return visitor.visit(*this);
}
// Visitor method implementations

Value *IRGenerator::visit(LiteralExpr &expr)
{
    switch (expr.type)
    {
    case TokenKind::IntegerLiteral:
        expr.setType(Type::Int);
        return ConstantInt::get(llvm::Type::getInt32Ty(*context), std::stoi(expr.value));
    case TokenKind::FloatingLiteral:
        expr.setType(Type::Float);
        return ConstantFP::get(llvm::Type::getFloatTy(*context), std::stof(expr.value));
    case TokenKind::CharacterLiteral:
        expr.setType(Type::Char);
        return ConstantInt::get(llvm::Type::getInt8Ty(*context), expr.value[1]);
    case TokenKind::StringLiteral: {
        // create a global string constant and return a pointer to it
        expr.setType(Type::String);
        Constant *strConst = ConstantDataArray::getString(*context, expr.value, true);
        GlobalVariable *strVar = new GlobalVariable(
            *module, strConst->getType(), true, llvm::GlobalValue::PrivateLinkage,
            strConst, ".str.");
        // Create a pointer to the string constant
        Value *strPtr = builder->CreateConstGEP2_32(
            strVar->getValueType(), strVar, 0, 0, "strptr");
        // Return the pointer to the string constant
        return strPtr;
    }
    default:
        expr.setType(Type::Unknown);
        return nullptr;
        break; // Ensure proper termination of the default case
    }
}

Value *IRGenerator::visit(VarExpr &expr)
{
    Value *V = namedValues[expr.name];
    llvm::Type *varType = namedTypes[expr.name];
    if (!V || !varType)
    {
        // Variable not found
        return nullptr;
    }
    return V;
}

Value *IRGenerator::visit(DefStmt &stmt)
{
    // Create variable in the symbol table
    llvm::Type *varType = typeMap.at(stmt.var.getType());
    if (!varType)
    {
        return nullptr; // Handle unknown type
    }

    // Create alloca for the variable
    AllocaInst *alloca = nullptr;
    if (stmt.var.isArray())
    {
        auto arrayType = llvm::ArrayType::get(varType, stmt.var.arraySize);
        alloca = builder->CreateAlloca(arrayType, nullptr, stmt.var.name);
        namedTypes[stmt.var.name] = arrayType;
    }
    else
    {
        alloca = builder->CreateAlloca(varType, nullptr, stmt.var.name);
        namedTypes[stmt.var.name] = varType;
    }
    namedValues[stmt.var.name] = alloca;

    // Initialize variable if it has a value
    if (stmt.initValue)
    {
        Value *initValue = stmt.initValue->accept(*this);
        if (!initValue)
        {
            return nullptr; // Initialization failed
        }
        builder->CreateStore(initValue, alloca);
    }

    return alloca;
}

// TODO: This is getting too big and should be refactored
Value *IRGenerator::visit(BinaryExpr &expr)
{
    Value *LHS = expr.lhs->accept(*this);
    Value *RHS = expr.rhs->accept(*this);

    if (!LHS || !RHS)
    {
        return nullptr;
    }

    // Get types of operands
    llvm::Type *lhsType = LHS->getType();
    llvm::Type *rhsType = RHS->getType();

    // If it's a pointer, load the value except for assignment, increase or decrease operations
    if (expr.op != TokenKind::Assign)
    {
        if (auto *allocaLHS = dyn_cast<AllocaInst>(LHS))
        {
            LHS = builder->CreateLoad(allocaLHS->getAllocatedType(), allocaLHS, "loadlhs");
            lhsType = LHS->getType(); // Update type after load
        }
        else if (auto *gepLHS = dyn_cast<GEPOperator>(LHS))
        {
            // If it's a GEP, we need to load the value it points to
            LHS = builder->CreateLoad(gepLHS->getSourceElementType()->getArrayElementType(), LHS, "loadlhs");
            lhsType = LHS->getType(); // Update type after load
        }
        if (auto *allocaRHS = dyn_cast<AllocaInst>(RHS))
        {
            RHS = builder->CreateLoad(allocaRHS->getAllocatedType(), allocaRHS, "loadrhs");
            rhsType = RHS->getType(); // Update type after load
        }
        else if (auto *gepRHS = dyn_cast<GEPOperator>(RHS))
        {
            // If it's a GEP, we need to load the value it points to
            RHS = builder->CreateLoad(gepRHS->getSourceElementType()->getArrayElementType(), RHS, "loadrhs");
            rhsType = RHS->getType(); // Update type after load
        }
    }

    // Type checking and conversion
    if (lhsType != rhsType && !lhsType->isPointerTy())
    {
        // Handle type conversion if needed
        if (lhsType->isFloatTy() || rhsType->isFloatTy())
        {
            if (!lhsType->isFloatTy())
            {
                LHS = builder->CreateSIToFP(LHS, llvm::Type::getFloatTy(*context), "convfloat");
                lhsType = llvm::Type::getFloatTy(*context);
            }
            if (!rhsType->isFloatTy())
            {
                RHS = builder->CreateSIToFP(RHS, llvm::Type::getFloatTy(*context), "convfloat");
                rhsType = llvm::Type::getFloatTy(*context);
            }
        }
        else
        {
            // Type mismatch that can't be converted
            return nullptr;
        }
    }

    // use Strategy pattern for binary operations
    std::unique_ptr<BinaryOp> binOp = BinaryOpBuilder::createBinaryOp(lhsType);
    llvm::Value *res = binOp->perform(LHS, RHS, expr.op, *builder);
    return res ? res : nullptr; // Return result or nullptr on failure
}

Value *IRGenerator::visit(UnaryExpr &expr)
{

    Value *Operand = expr.operand->accept(*this);
    llvm::Type *operandType = Operand->getType();

    // For increment and decrement operations, we need to handle the alloca
    if (expr.op != TokenKind::PlusPlus && expr.op != TokenKind::MinusMinus)
    {
        if (auto *allocaOp = dyn_cast<AllocaInst>(Operand))
        {
            Operand = builder->CreateLoad(allocaOp->getAllocatedType(), allocaOp, "loadlhs");
            operandType = Operand->getType(); // Update type after load
        }
    }

    std::unique_ptr<BinaryOp> binOp = BinaryOpBuilder::createBinaryOp(operandType);
    llvm::Value *res = binOp->perform(Operand, nullptr, expr.op, *builder);
    return res ? res : nullptr; // Return result or nullptr on failure
}

Value *IRGenerator::visit(BlockStmt &stmt)
{
    Value *lastValue = nullptr;
    for (auto &stmtPtr : stmt.statements)
    {
        lastValue = stmtPtr->accept(*this);
        if (!lastValue)
        {
            return nullptr; // Statement failed
        }
        else if (dynamic_cast<RetStmt *>(stmtPtr.get())) // Check if the statement is a return statement
        {
            return lastValue; // If we hit a return statement, we can exit early
        }
    }
    return lastValue; // Return value of last statement
}

Value *IRGenerator::visit(ExprStmt &stmt)
{
    return stmt.expr->accept(*this); // Just evaluate the expression
}

llvm::Function *IRGenerator::emitLibCFunctionIfNeeded(const std::string &name)
{
    // Check if the function is already defined
    if (module->getFunction(name))
        return nullptr;

    // try to get function for registry
    llvm::FunctionType *libcFuncType = LibCRegistry.at(name);
    // Create function
    llvm::Function *libcFunc = llvm::Function::Create(
        libcFuncType, llvm::Function::ExternalLinkage, name, module.get());

    return libcFunc; // Return the created function
}

Value *IRGenerator::visit(CallExpr &expr)
{
    // Get function from module
    llvm::Function *calleeF = module->getFunction(expr.callee);
    if (!calleeF)
    {
        // might be a libc function, try to find it in the symbol table
        calleeF = emitLibCFunctionIfNeeded(expr.callee);
        if (!calleeF)
        {
            fprintf(stderr, "Error: Function '%s' not found.\n", expr.callee.c_str());
            return nullptr; // Function not found
        }
    }

    // Check argument count matches
    if (calleeF->arg_size() != expr.args.size())
    {
        fprintf(stderr, "Error: Function '%s' expects %zu arguments but got %zu.\n",
                expr.callee.c_str(), calleeF->arg_size(), expr.args.size());
        return nullptr; // Argument count mismatch
    }

    // Process arguments with type checking
    std::vector<Value *> argsV;
    for (unsigned i = 0; i < expr.args.size(); i++)
    {
        Value *argV = expr.args[i]->accept(*this);
        if (!argV)
        {
            fprintf(stderr, "Error: Argument %u for function '%s' is invalid.\n", i, expr.callee.c_str());
            return nullptr;
        }

        // Get expected and actual types
        llvm::Type *expectedTy = calleeF->getFunctionType()->getParamType(i);
        llvm::Type *actualTy = argV->getType();

        // load the value if it's an alloca
        if (auto *allocaArg = dyn_cast<AllocaInst>(argV))
        {
            argV = builder->CreateLoad(allocaArg->getAllocatedType(), allocaArg, "loadarg");
            actualTy = argV->getType(); // Update type after load
        }

        // Handle type conversions
        if (actualTy != expectedTy && !actualTy->isArrayTy())
        {
            if (actualTy->isIntegerTy() && expectedTy->isFloatingPointTy())
            {
                argV = builder->CreateSIToFP(argV, expectedTy, "convtmp");
            }
            else if (actualTy->isFloatingPointTy() && expectedTy->isIntegerTy())
            {
                argV = builder->CreateFPToSI(argV, expectedTy, "convtmp");
            }
            else
            {
                fprintf(stderr, "Error: Argument %u for function '%s' has incompatible type: expected %s but got %s.\n",
                        i, expr.callee.c_str(), expectedTy->getStructName().str().c_str(), actualTy->getStructName().str().c_str());
                return nullptr; // Incompatible types
            }
        }
        argsV.push_back(argV);
    }

    expr.setType(Type::Int); // Assume functions return int for now
    return builder->CreateCall(calleeF, argsV, "calltmp");
}

llvm::Function *IRGenerator::visit(Function &func)
{
    // Create function from prototype
    llvm::Function *F = func.proto->accept(*this);
    if (!F)
        return nullptr;

    // Create entry block
    BasicBlock *BB = BasicBlock::Create(*context, "entry", F);
    builder->SetInsertPoint(BB);

    // Add arguments and type to symbol table
    for (auto &Arg : F->args())
    {
        namedValues[std::string(Arg.getName())] = &Arg;
        namedTypes[std::string(Arg.getName())] = Arg.getType();
    }

    // Check if the function has a body
    if (func.body->statements.size() == 0)
    {
        // If no body, return void for void functions
        if (func.proto->returnType == Type::Void)
        {
            builder->CreateRetVoid();
            return F;
        }
        else
        {
            // Error: non-void function without body
            F->eraseFromParent();
            return nullptr;
        }
    }

    // Generate function body

    if (func.body->accept(*this))
    {

        llvm::BasicBlock *currBB = builder->GetInsertBlock();
        if (!currBB->getTerminator())
        {
            if (func.proto->returnType == Type::Void)
            {
                builder->CreateRetVoid();
            }
            else
            {
                // Emit default return value (e.g., 0 for int, null for pointers, etc.)
                llvm::Type *retTy = F->getReturnType();
                llvm::Value *defaultRet;

                if (retTy->isIntegerTy())
                {
                    defaultRet = llvm::ConstantInt::get(retTy, 0);
                }
                else if (retTy->isFloatingPointTy())
                {
                    defaultRet = llvm::ConstantFP::get(retTy, 0.0);
                }
                else if (retTy->isPointerTy())
                {
                    defaultRet = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(retTy));
                }
                else
                {
                    // Unsupported return type
                    F->eraseFromParent();
                    return nullptr;
                }

                builder->CreateRet(defaultRet);
            }
        }

        // Verify function
        if (verifyFunction(*F, &llvm::errs()))
        {
            fprintf(stderr, "Error: Function verification failed for %s\n", F->getName().str().c_str());
            F->print(llvm::errs());
            F->eraseFromParent();
            return nullptr;
        }
        // TheFPM->run(*F, *TheFAM);
        return F;
    }

    // Error occurred
    F->eraseFromParent();
    return nullptr;
}

llvm::Function *IRGenerator::visit(Prototype &proto)
{
    std::vector<llvm::Type *> argTypes;
    for (auto &arg : proto.params)
    {
        llvm::Type *ty = typeMap.at(arg.getType());
        if (!ty)
        {
            return nullptr; // Handle unknown types
        }
        argTypes.push_back(ty);
    }

    llvm::Type *returnType = typeMap.at(proto.returnType);
    if (!returnType)
    {
        return nullptr; // Handle unknown return type
    }
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        returnType, argTypes, false);

    // Create function
    llvm::Function *func = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        proto.name,
        module.get());

    // Set parameter names
    unsigned idx = 0;
    for (auto &arg : func->args())
    {
        arg.setName(proto.params[idx++].name);
    }

    return func;
}

llvm::Value *IRGenerator::visit(IfStmt &stmt)
{
    // Generate code for the condition
    Value *condV = stmt.Cond->accept(*this);
    if (!condV)
    {
        return nullptr; // Condition evaluation failed
    }

    // Convert condition to boolean
    condV = builder->CreateICmpNE(condV, ConstantInt::get(condV->getType(), 0), "ifcond");

    // Create blocks for then and else branches
    llvm::Function *F = builder->GetInsertBlock()->getParent();
    BasicBlock *thenBB = BasicBlock::Create(*context, "then", F);
    BasicBlock *elseBB = BasicBlock::Create(*context, "else");
    BasicBlock *mergeBB = BasicBlock::Create(*context, "ifcont");

    // Create conditional branch
    if (stmt.Else)
    {
        builder->CreateCondBr(condV, thenBB, elseBB);
    }
    else
    {
        builder->CreateCondBr(condV, thenBB, mergeBB);
    }

    // Generate code for the then branch
    builder->SetInsertPoint(thenBB);
    Value *thenV = stmt.Then->accept(*this);
    if (!thenV)
    {
        return nullptr; // Then branch evaluation failed
    }
    // Needed so every branch ends with a return or a jump
    thenBB = builder->GetInsertBlock();
    if (!thenBB->getTerminator())
        builder->CreateBr(mergeBB); // Jump to merge block

    Value *elseV = nullptr;
    F->insert(F->end(), elseBB);
    builder->SetInsertPoint(elseBB);
    if (stmt.Else)
    {

        elseV = stmt.Else->accept(*this);
        if (!elseV)
        {
            return nullptr; // Else branch evaluation failed
        }
    }

    elseBB = builder->GetInsertBlock();
    if (!elseBB->getTerminator())
        builder->CreateBr(mergeBB); // Jump to merge block

    // Set insert point to merge block
    F->insert(F->end(), mergeBB);
    builder->SetInsertPoint(mergeBB);

    // No phi node needed since we aren't using SSA but rather alloca, can return anything (doesn't matter)
    return condV;
}

llvm::Value *IRGenerator::visit(ForStmt &stmt)
{
    // Create a new basic block for the loop
    llvm::Function *F = builder->GetInsertBlock()->getParent();
    BasicBlock *loopBB = BasicBlock::Create(*context, "loop", F);
    BasicBlock *innerBB = BasicBlock::Create(*context, "innerloop", F);
    BasicBlock *afterBB = BasicBlock::Create(*context, "afterloop");

    // Generate code for the initialization
    if (stmt.Init)
    {
        stmt.Init->accept(*this);
    }

    // Create the loop condition block
    builder->CreateBr(loopBB);
    builder->SetInsertPoint(loopBB);

    // Generate code for the condition
    Value *condV = stmt.Cond->accept(*this);
    if (!condV)
    {
        return nullptr; // Condition evaluation failed
    }

    // Convert condition to boolean
    condV = builder->CreateICmpNE(condV, ConstantInt::get(condV->getType(), 0), "forcond");

    // Create conditional branch
    builder->CreateCondBr(condV, innerBB, afterBB);

    loopBB = builder->GetInsertBlock();

    // Create inner code
    builder->SetInsertPoint(innerBB);
    Value *body = stmt.Body->accept(*this);
    if (!body)
    {
        return nullptr; // Body evaluation failed
    }
    innerBB = builder->GetInsertBlock();

    // Generate code for the step
    if (stmt.Step)
    {
        stmt.Step->accept(*this);
    }

    // Jump back to the loop condition
    builder->CreateBr(loopBB);

    // Set insert point to after the loop
    F->insert(F->end(), afterBB);
    builder->SetInsertPoint(afterBB);

    return condV; // No value to return from a for statement
}

Value *IRGenerator::visit(ArrayAccessExpr &expr)
{
    Value *arrayV = expr.array->accept(*this);
    Value *indexV = expr.index->accept(*this);

    if (!arrayV || !indexV)
        return nullptr;

    // if index is a pointer load
    if (auto *allocaIndex = dyn_cast<AllocaInst>(indexV))
    {
        indexV = builder->CreateLoad(allocaIndex->getAllocatedType(), allocaIndex, "loadindex");
    }
    else if (auto *gepIndex = dyn_cast<GEPOperator>(indexV))
    {
        // If it's a GEP, we need to load the value it points to
        indexV = builder->CreateLoad(gepIndex->getSourceElementType()->getArrayElementType(), indexV, "loadindex");
    }

    if (indexV->getType()->isFloatingPointTy())
    {
        indexV = builder->CreateFPToSI(indexV, llvm::Type::getInt32Ty(*context), "convindex");
    }

    llvm::Type *ptrType = arrayV->getType();
    if (!ptrType->isPointerTy())
        return nullptr;

    // Get the element type of the array

    auto *arrayType = namedTypes[expr.array->name];

    llvm::Value *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0);
    llvm::Value *elementPtr = builder->CreateGEP(arrayType, arrayV, {zero, indexV}, "arrayelem");

    return elementPtr;
}

Value *IRGenerator::visit(RetStmt &stmt)
{

    // get current fucntion return type
    llvm::Function *currentFunction = builder->GetInsertBlock()->getParent();
    if (!currentFunction)
    {
        return nullptr; // No current function
    }
    auto currentFunctionReturnType = currentFunction->getReturnType();

    if (stmt.returnValue)
    {
        // Non-void return
        Value *retVal = stmt.returnValue->accept(*this);
        if (!retVal)
        {
            return nullptr; // Return value evaluation failed
        }

        if (retVal->getType()->isPointerTy())
        {
            // If the return value is a pointer, we need to load it
            if (auto *allocaRetVal = dyn_cast<AllocaInst>(retVal))
            {
                retVal = builder->CreateLoad(allocaRetVal->getAllocatedType(), allocaRetVal, "loadret");
            }
            else if (auto *gepRetVal = dyn_cast<GEPOperator>(retVal))
            {
                // If it's a GEP, we need to load the value it points to
                retVal = builder->CreateLoad(gepRetVal->getSourceElementType()->getArrayElementType(), retVal, "loadret");
            }
        }

        // Handle type conversion if needed
        if (retVal->getType() != currentFunctionReturnType)
        {
            // Attempt type conversion
            if (retVal->getType()->isIntegerTy() && currentFunctionReturnType->isFloatingPointTy())
            {
                retVal = builder->CreateSIToFP(retVal, currentFunctionReturnType, "convret");
            }
            else if (retVal->getType()->isFloatingPointTy() && currentFunctionReturnType->isIntegerTy())
            {
                retVal = builder->CreateFPToSI(retVal, currentFunctionReturnType, "convret");
            }
            else
            {
                // Incompatible types
                return nullptr;
            }
        }

        return builder->CreateRet(retVal);
    }
    else
    {
        // Void return
        if (!currentFunctionReturnType->isVoidTy())
        {
            // Error: missing return value in non-void function
            return nullptr;
        }
        return builder->CreateRetVoid();
    }
}

bool VarExpr::typeCheck()
{
    return true;
    /*// Check if variable is defined
    if (namedValues.find(name) == namedValues.end()) {
        std::cerr << "Error: Variable '" << name << "' is not defined." << std::endl;
        return false;
    }

    // Set the type based on the variable's definition
    type = namedValues[name]->getType();
    return true;*/
}

bool LiteralExpr::typeCheck()
{
    // Literal expressions are always valid
    return true;
}

bool CallExpr::typeCheck()
{
    return true;
}

bool BinaryExpr::typeCheck()
{
    // Check if both operands are valid
    if (!lhs->typeCheck() || !rhs->typeCheck())
    {
        return false;
    }

    // Set the type based on the operation
    if (op == TokenKind::Plus || op == TokenKind::Minus || op == TokenKind::Star || op == TokenKind::Slash)
    {
        if (lhs->getType() == Type::Float || rhs->getType() == Type::Float)
        {
            setType(Type::Float);
        }
        else
        {
            setType(Type::Int);
        }
    }
    else
    {
        // Unsupported operation
        return false;
    }

    return true;
}

bool UnaryExpr::typeCheck()
{
    return true;
}

bool ArrayAccessExpr::typeCheck()
{
    return true;
}

void IRGenerator::printIR(){
    module->print(llvm::errs(), nullptr);
}