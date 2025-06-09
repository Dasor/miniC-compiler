#include "ast.h"

using namespace miniC;
using namespace llvm;

// Initialize static members
std::unique_ptr<llvm::LLVMContext> IRGenerator::context = std::make_unique<llvm::LLVMContext>();
std::unique_ptr<llvm::Module> IRGenerator::module = std::make_unique<llvm::Module>("miniC", *IRGenerator::context);
std::unique_ptr<llvm::IRBuilder<>> IRGenerator::builder = std::make_unique<llvm::IRBuilder<>>(*IRGenerator::context);
std::map<std::string, llvm::Value *> IRGenerator::namedValues;
const std::map<miniC::Type, llvm::Type *> IRGenerator::typeMap = {
    {miniC::Type::Int, llvm::Type::getInt32Ty(*IRGenerator::context)},
    {miniC::Type::Float, llvm::Type::getFloatTy(*IRGenerator::context)},
    {miniC::Type::Char, llvm::Type::getInt8Ty(*IRGenerator::context)},
    {miniC::Type::String, llvm::ArrayType::get(llvm::Type::getInt8Ty(*IRGenerator::context), 0)},
    {miniC::Type::Void, llvm::Type::getVoidTy(*IRGenerator::context)},
    {miniC::Type::Unknown, nullptr}};
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

Value *IRGenerator::visit(DefStmt &expr)
{
    // Create variable in the symbol table
    llvm::Type *varType = typeMap.at(expr.type);
    if (!varType)
    {
        return nullptr; // Handle unknown type
    }

    // Create alloca for the variable
    AllocaInst *alloca = builder->CreateAlloca(varType, nullptr, expr.var.name);
    namedValues[expr.var.name] = alloca;

    // Initialize variable if it has a value
    if (expr.initValue)
    {
        Value *initValue = expr.initValue->accept(*this);
        if (!initValue)
        {
            return nullptr; // Initialization failed
        }
        builder->CreateStore(initValue, alloca);
    }

    return alloca;
}

Value *IRGenerator::visit(BinaryExpr &expr)
{
    Value *LHS = expr.lhs->accept(*this);
    Value *RHS = expr.rhs->accept(*this);

    if (!LHS || !RHS)
    {
        return nullptr;
    }

    // Get types of operands
    Type lhsType = expr.lhs->getType();
    Type rhsType = expr.rhs->getType();

    // Type checking and conversion
    if (lhsType != rhsType)
    {
        // Handle type conversion if needed
        if (lhsType == Type::Float || rhsType == Type::Float)
        {
            if (lhsType != Type::Float)
            {
                LHS = builder->CreateSIToFP(LHS, llvm::Type::getFloatTy(*context), "convfloat");
            }
            if (rhsType != Type::Float)
            {
                RHS = builder->CreateSIToFP(RHS, llvm::Type::getFloatTy(*context), "convfloat");
            }
            expr.setType(Type::Float);
        }
        else
        {
            // Type mismatch that can't be converted
            return nullptr;
        }
    }
    else
    {
        expr.setType(lhsType);
    }

    // Generate IR based on operation and types
    switch (expr.op)
    {
    case TokenKind::Plus:
        if (expr.getType() == Type::Float)
        {
            return builder->CreateFAdd(LHS, RHS, "faddtmp");
        }
        else
        {
            return builder->CreateAdd(LHS, RHS, "addtmp");
        }
    case TokenKind::Minus:
        if (expr.getType() == Type::Float)
        {
            return builder->CreateFSub(LHS, RHS, "fsubtmp");
        }
        else
        {
            return builder->CreateSub(LHS, RHS, "subtmp");
        }
    case TokenKind::Star:
        if (expr.getType() == Type::Float)
        {
            return builder->CreateFMul(LHS, RHS, "fmultmp");
        }
        else
        {
            return builder->CreateMul(LHS, RHS, "multmp");
        }
    case TokenKind::Slash:
        if (expr.getType() == Type::Float)
        {
            return builder->CreateFDiv(LHS, RHS, "fdivtmp");
        }
        else
        {
            return builder->CreateSDiv(LHS, RHS, "divtmp");
        }
    default:
        return nullptr;
    }
}

Value *IRGenerator::visit(BlockStmt &stmt) {
    Value *lastValue = nullptr;
    for (auto &stmtPtr : stmt.statements) {
        lastValue = stmtPtr->accept(*this);
        if (!lastValue) {
            return nullptr; // Statement failed
        }
    }
    return lastValue; // Return value of last statement
}

Value *IRGenerator::visit(ExprStmt &stmt) {
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
    if (!func.body)
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
        builder->CreateRet(RetVal);

        // Verify function
        if (verifyFunction(*F, &llvm::errs()))
        {
            F->eraseFromParent();
            return nullptr;
        }
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
