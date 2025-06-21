#include "ast.h"

using namespace miniC;
using namespace llvm;

IRGenerator::IRGenerator()
{
    context = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("miniC", *context);
    builder = std::make_unique<llvm::IRBuilder<>>(*context);

    namedValues = {};
    namedTypes = {};
    currentFunctionReturnType = nullptr;

    typeMap = {
        {miniC::Type::Int, llvm::Type::getInt32Ty(*context)},
        {miniC::Type::Float, llvm::Type::getFloatTy(*context)},
        {miniC::Type::Char, llvm::Type::getInt8Ty(*context)},
        {miniC::Type::String, llvm::ArrayType::get(llvm::Type::getInt8Ty(*context), 0)},
        {miniC::Type::Void, llvm::Type::getVoidTy(*context)},
        {miniC::Type::Unknown, nullptr}};

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
    case TokenKind::StringLiteral:
        expr.setType(Type::String);
        return ConstantDataArray::getString(*context, expr.value, true);
    default:
        expr.setType(Type::Unknown);
        return nullptr;
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
    }
    if (auto *allocaRHS = dyn_cast<AllocaInst>(RHS))
    {
        RHS = builder->CreateLoad(allocaRHS->getAllocatedType(), allocaRHS, "loadrhs");
        rhsType = RHS->getType(); // Update type after load
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
    }
    return lastValue; // Return value of last statement
}

Value *IRGenerator::visit(ExprStmt &stmt)
{
    return stmt.expr->accept(*this); // Just evaluate the expression
}

Value *IRGenerator::visit(CallExpr &expr)
{
    // Get function from module
    llvm::Function *calleeF = module->getFunction(expr.callee);
    if (!calleeF)
    {
        return nullptr; // Function not found
    }

    // Check argument count matches
    if (calleeF->arg_size() != expr.args.size())
    {
        return nullptr; // Argument count mismatch
    }

    // Process arguments with type checking
    std::vector<Value *> argsV;
    for (unsigned i = 0; i < expr.args.size(); i++)
    {
        Value *argV = expr.args[i]->accept(*this);
        if (!argV)
            return nullptr;

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
        if (actualTy != expectedTy)
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

    // Add arguments to symbol table
    for (auto &Arg : F->args())
    {
        namedValues[std::string(Arg.getName())] = &Arg;
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
    currentFunctionReturnType = typeMap.at(proto.returnType);
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
    builder->CreateBr(mergeBB); // Jump to merge block

    // Set insert point to else block
    thenBB = builder->GetInsertBlock();

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

    builder->CreateBr(mergeBB); // Jump to merge block
    elseBB = builder->GetInsertBlock();

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
    if (!currentFunctionReturnType)
    {
        return nullptr; // No current function context
    }

    if (stmt.returnValue)
    {
        // Non-void return
        Value *retVal = stmt.returnValue->accept(*this);
        if (!retVal)
        {
            return nullptr; // Return value evaluation failed
        }

        // Handle type conversion if needed
        if (currentFunctionReturnType->isVoidTy())
        {
            // Error: returning value from void function
            return nullptr;
        }

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
