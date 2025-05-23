#include "ast.h"

using namespace miniC;
using namespace llvm;

// Accept method implementations for AST nodes

Value *BinaryExpr::accept(ASTVisitor &visitor)
{
    visitor.visit(*this);
}

Value *LiteralExpr::accept(ASTVisitor &visitor)
{
    visitor.visit(*this);
}

Value *VarExpr::accept(ASTVisitor &visitor)
{
    visitor.visit(*this);
}

Value *CallExpr::accept(ASTVisitor &visitor)
{
    visitor.visit(*this);
}

llvm::Function *miniC::Function::accept(ASTVisitor &visitor)
{
    visitor.visit(*this);
}

llvm::Function *Prototype::accept(ASTVisitor &visitor)
{
    visitor.visit(*this);
}

// Visitor method implementations

Value *ASTVisitor::visit(LiteralExpr &expr)
{
    switch (expr.type)
    {
        {
        case TokenKind::IntegerLiteral:
            // Cast string value to int
            int intValue = std::stoi(expr.value);
            return ConstantInt::get(Type::getInt64Ty(*context), intValue);
            break;
        case TokenKind::FloatingLiteral:
            // Cast string value to float
            float floatValue = std::stof(expr.value);
            return ConstantFP::get(Type::getFloatTy(*context), floatValue);
            break;
        case TokenKind::CharacterLiteral:
            // Cast string value to char
            char charValue = expr.value[1]; // Assuming single character literal
            return ConstantInt::get(Type::getInt8Ty(*context), charValue);
            break;
        case TokenKind::StringLiteral:
            return ConstantDataArray::getString(*context, expr.value, true);
            break;
        default:
            break;
        }
    }
}

    Value *ASTVisitor::visit(VarExpr & expr)
    {
        Value *V = namedValues[expr.name];
        if (!V)
        {
            // Variable not found
            return nullptr;
        }
        return V;
    }

    Value *ASTVisitor::visit(BinaryExpr & expr)
    {
        Value *LHS = expr.lhs->accept(*this);
        Value *RHS = expr.rhs->accept(*this);

        if (!LHS || !RHS)
        {
            // Error handling
            return nullptr;
        }

        switch (expr.op)
        {
        // TODO: add all operators, perhaps expand the def file to use a X macro
        case TokenKind::Plus:
            return builder->CreateAdd(LHS, RHS, "addtmp");
            break;
        case TokenKind::Minus:
            return builder->CreateSub(LHS, RHS, "subtmp");
            break;
        case TokenKind::Star:
            return builder->CreateMul(LHS, RHS, "multmp");
            break;
        case TokenKind::Slash:
            return builder->CreateSDiv(LHS, RHS, "divtmp");
            break;
        default:
            return nullptr; // Unsupported operator
            break;
        }
    }

    Value *ASTVisitor::visit(CallExpr & expr)
    {
        llvm::Function *calleeF = module->getFunction(expr.callee);
        if (!calleeF)
        {
            // Function not found
            return nullptr;
        }
        // Check arguments are correct
        if (calleeF->arg_size() != expr.args.size())
        {
            // Argument count mismatch
            return nullptr;
        }
        // TODO: check argument types

        std::vector<llvm::Value *> argsV;
        for (auto &arg : expr.args)
        {
            llvm::Value *argV = arg->accept(*this);
            if (!argV)
            {
                // Error handling
                return nullptr;
            }
            argsV.push_back(argV);
        }
        return builder->CreateCall(calleeF, argsV, "calltmp");
    }

    llvm::Function *ASTVisitor::visit(Function & func)
    {
        // Default implementation does nothing
    }

    llvm::Function *ASTVisitor::visit(Prototype & proto)
    {
        // Default implementation does nothing
    }
