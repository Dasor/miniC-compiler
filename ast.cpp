#include "ast.h"

using namespace miniC;
using namespace llvm;

IRGenerator::IRGenerator()
{
    context = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("miniC", *context);
    builder = std::make_unique<llvm::IRBuilder<>>(*context);

    namedValues = {};

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
    if (!V)
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
    AllocaInst *alloca = builder->CreateAlloca(varType, nullptr, stmt.var.name);
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

    // If it's a pointer, load the value except for assignment do not load lhs
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

    // Generate IR based on operation and types
    if (lhsType->isFloatTy())
    {
        switch (expr.op)
        {
        case TokenKind::Plus:
            return builder->CreateFAdd(LHS, RHS, "faddtmp");
        case TokenKind::Minus:
            return builder->CreateFSub(LHS, RHS, "fsubtmp");
        case TokenKind::Star:
            return builder->CreateFMul(LHS, RHS, "fmultmp");
        case TokenKind::Slash:
            return builder->CreateFDiv(LHS, RHS, "fdivtmp");
       default:
            throw std::runtime_error("Unsupported operation for float type");
        }
    }
    else if (lhsType->isIntegerTy())
    {
        switch (expr.op)
        {
        case TokenKind::Plus:
            return builder->CreateAdd(LHS, RHS, "addtmp");
        case TokenKind::Minus:
            return builder->CreateSub(LHS, RHS, "subtmp");
        case TokenKind::Star:
            return builder->CreateMul(LHS, RHS, "multmp");
        case TokenKind::Slash:
            return builder->CreateSDiv(LHS, RHS, "divtmp");
       default:
            throw std::runtime_error("Unsupported operation for int type");
        }
    }else if(lhsType->isPointerTy()){
        switch (expr.op)
        {
        case TokenKind::Assign:
            // For assignment, store the value in the alloca
            builder->CreateStore(RHS, LHS);
            return LHS; // Return the alloca
        case TokenKind::Plus:
        case TokenKind::Minus:
            // Pointer arithmetic is not supported in this context
            throw std::runtime_error("Pointer arithmetic not supported");
        default:
            throw std::runtime_error("Unsupported operation for pointer type");
        }
    }
    else
    {
        throw std::runtime_error("Unsupported operand types for binary operation");
    }

    return nullptr;
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
    if (Value *RetVal = func.body->accept(*this))
    {
        // Create return
        // if it's a pointer use the load instruction and return the value
        if (auto *allocaRetVal = dyn_cast<AllocaInst>(RetVal))
        {
            RetVal = builder->CreateLoad(allocaRetVal->getAllocatedType(), allocaRetVal, "loadret");
        }
        // Check for type mistmatch
        if (typeMap.at(func.proto->returnType) != RetVal->getType())
        {
            if (func.proto->returnType == Type::Void)
            {
                builder->CreateRetVoid();
            }
            else if (typeMap.at(func.proto->returnType) == llvm::Type::getFloatTy(*context) && RetVal->getType()->isIntegerTy())
            {
                // Convert integer to float if needed
                RetVal = builder->CreateSIToFP(RetVal, llvm::Type::getFloatTy(*context), "convfloat");
                builder->CreateRet(RetVal);
            }
            else if (typeMap.at(func.proto->returnType) == llvm::Type::getInt32Ty(*context) && RetVal->getType()->isFloatingPointTy())
            {
                // Convert float to integer if needed
                RetVal = builder->CreateFPToSI(RetVal, llvm::Type::getInt32Ty(*context), "convint");
                builder->CreateRet(RetVal);
            }
            else
            {
                throw std::runtime_error("Return type mismatch in function " + func.proto->name);
            }
        }
        else
        {

            builder->CreateRet(RetVal);
        }

        // Verify function
        if (verifyFunction(*F, &llvm::errs()))
        {
            F->eraseFromParent();
            return nullptr;
        }
        TheFPM->run(*F, *TheFAM);
        return F;
    }

    // Error occurred
    F->eraseFromParent();
    return nullptr;
}

llvm::Function *IRGenerator::visit(Prototype &proto)
{
    // Default implementation does nothing
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
