// ast.h - Abstract Syntax Tree for miniC compiler
#pragma once
#include <vector>
#include <string>
#include <memory>
#include "lexer.h"
#include <map>
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

namespace miniC {

// Forward declarations
class ASTVisitor;

class ASTNode {
public:
    virtual ~ASTNode() = default;
    virtual llvm::Value *accept(ASTVisitor& visitor) = 0;


};

class Expr : public ASTNode {
public:
    virtual ~Expr() = default;
    // Add type information

};

class BinaryExpr : public Expr {
public:
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
    TokenKind op;

    BinaryExpr(std::unique_ptr<Expr> lhs, TokenKind op, std::unique_ptr<Expr> rhs)
        : lhs(std::move(lhs)), op(op), rhs(std::move(rhs)) {}

    llvm::Value *accept(ASTVisitor& visitor) override;
};

class LiteralExpr : public Expr {
public:
    std::string value;
    TokenKind type;

    LiteralExpr(const std::string& value, TokenKind type)
        : value(value), type(type) {}

    llvm::Value *accept(ASTVisitor& visitor) override;
};

class VarExpr : public Expr {
public:
    std::string name;

    explicit VarExpr(const std::string& name) : name(name) {}

    llvm::Value *accept(ASTVisitor& visitor) override;
};

class CallExpr : public Expr {
public:
    std::string callee;
    std::vector<std::unique_ptr<Expr>> args;

    CallExpr(const std::string& callee, std::vector<std::unique_ptr<Expr>> args)
        : callee(callee), args(std::move(args)) {}

    llvm::Value *accept(ASTVisitor& visitor) override;
};

class Prototype : public ASTNode {
public:
    std::string name;
    std::vector<std::string> params;

    Prototype(const std::string& name, std::vector<std::string> params)
        : name(name), params(std::move(params)) {}

   llvm::Function *accept(ASTVisitor& visitor) override;
};

class Function : public ASTNode {
public:
    std::unique_ptr<Prototype> proto;
    std::unique_ptr<Expr> body;

    Function(std::unique_ptr<Prototype> proto, std::unique_ptr<Expr> body)
        : proto(std::move(proto)), body(std::move(body)) {}

    llvm::Function *accept(ASTVisitor& visitor) override;
};

class ASTVisitor {

protected:
    static std::unique_ptr<llvm::LLVMContext> context;
    static std::unique_ptr<llvm::Module> module;
    static std::unique_ptr<llvm::IRBuilder<>> builder;
    static std::map<std::string, llvm::Value*> namedValues;

public:
    virtual ~ASTVisitor() = default;
    virtual llvm::Value *visit(BinaryExpr& expr) = 0;
    virtual llvm::Value *visit(LiteralExpr& expr) = 0;
    virtual llvm::Value *visit(VarExpr& expr) = 0;
    virtual llvm::Value *visit(CallExpr& expr) = 0;
    virtual llvm::Function *visit(Prototype& proto) = 0;
    virtual llvm::Function *visit(Function& func) = 0;
};

} // namespace miniC
