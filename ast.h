// ast.h - Abstract Syntax Tree for miniC compiler
#pragma once
#include <vector>
#include <string>
#include <memory>
#include "lexer.h"
#include "binaryOps.h"
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
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"

// Other features that would be interesting to have: while, arrays, tensors, print, local variables

namespace miniC
{

    // Type system
    enum class Type
    {
        Int,
        Float,
        Char,
        String,
        Void,
        Unknown
    };

    // Forward declarations
    class ASTVisitor;
    class Stmt;
    class BlockStmt;
    class ExprStmt;
    class DefStmt;

    class ASTNode
    {
    public:
        virtual ~ASTNode() = default;
        virtual llvm::Value *accept(ASTVisitor &visitor) = 0;
    };

    class Stmt : public ASTNode
    {
    public:
        virtual ~Stmt() = default;
        virtual llvm::Value *accept(ASTVisitor &visitor) = 0;
    };

    class Expr : public ASTNode
    {
    public:
        virtual ~Expr() = default;
        miniC::Type type = Type::Unknown;

        Expr() = default;
        Expr(Type type) : type(type) {}

        virtual miniC::Type getType() const { return type; }
        virtual void setType(Type t) { type = t; }
        virtual bool typeCheck() = 0;
    };

    class BinaryExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> lhs;
        TokenKind op;
        std::unique_ptr<Expr> rhs;

        BinaryExpr(std::unique_ptr<Expr> lhs, TokenKind op, std::unique_ptr<Expr> rhs)
            : lhs(std::move(lhs)), op(op), rhs(std::move(rhs)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
        bool typeCheck() override;
    };

    class UnaryExpr : public Expr
    {
    public:
        TokenKind op;
        std::unique_ptr<Expr> operand;

        UnaryExpr(TokenKind op, std::unique_ptr<Expr> operand)
            : op(op), operand(std::move(operand)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
        bool typeCheck() override;
    };

    class LiteralExpr : public Expr
    {
    public:
        std::string value;
        TokenKind type;

        LiteralExpr(const std::string &value, TokenKind type)
            : value(value), type(type) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
        bool typeCheck() override;
    };

    class VarExpr : public Expr
    {
    public:
        std::string name;
        int arraySize = 0;

        // For reading a var
        explicit VarExpr(const std::string &name) : name(name) {}
        // For defining a var
        explicit VarExpr(const std::string &name, Type type) : Expr(type), name(name) {}
        // For defining a var with array size
        VarExpr(const std::string &name, Type type, int arraySize)
            : Expr(type), name(name), arraySize(arraySize) {}
        bool isArray() const { return arraySize != 0; }

        llvm::Value *accept(ASTVisitor &visitor) override;
        bool typeCheck() override;
    };

    class ArrayAccessExpr : public Expr
    {
    public:
        std::unique_ptr<VarExpr> array;
        std::unique_ptr<Expr> index;

        ArrayAccessExpr(std::unique_ptr<VarExpr> array, std::unique_ptr<Expr> index)
            : array(std::move(array)), index(std::move(index)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
        bool typeCheck() override;
    };

    class BlockStmt : public Stmt
    {
    public:
        std::vector<std::unique_ptr<Stmt>> statements;

        void addStatement(std::unique_ptr<Stmt> stmt)
        {
            statements.push_back(std::move(stmt));
        }

        llvm::Value *accept(ASTVisitor &visitor) override;
    };

    class ExprStmt : public Stmt
    {
    public:
        std::unique_ptr<Expr> expr;

        explicit ExprStmt(std::unique_ptr<Expr> expr)
            : expr(std::move(expr)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
    };

    class DefStmt : public Stmt
    {
    public:
        VarExpr var;
        std::unique_ptr<Expr> initValue;

        DefStmt(VarExpr var, std::unique_ptr<Expr> initValue)
            : var(std::move(var)), initValue(std::move(initValue)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
    };

    class CallExpr : public Expr
    {
    public:
        std::string callee;
        std::vector<std::unique_ptr<Expr>> args;

        CallExpr(const std::string &callee, std::vector<std::unique_ptr<Expr>> args)
            : callee(callee), args(std::move(args)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
        bool typeCheck() override;
    };

    class Prototype : public ASTNode
    {
    public:
        std::string name;
        std::vector<VarExpr> params;
        miniC::Type returnType;

        Prototype(const std::string &name, std::vector<VarExpr> params, miniC::Type returnType)
            : name(name), params(std::move(params)), returnType(returnType) {}

        llvm::Function *accept(ASTVisitor &visitor) override;
    };

    class Function : public ASTNode
    {
    public:
        std::unique_ptr<Prototype> proto;
        std::unique_ptr<BlockStmt> body;

        Function(std::unique_ptr<Prototype> proto, std::unique_ptr<BlockStmt> body)
            : proto(std::move(proto)), body(std::move(body)) {}

        llvm::Function *accept(ASTVisitor &visitor) override;
    };

    class IfStmt : public Stmt
    {
    public:
        std::unique_ptr<Expr> Cond;
        std::unique_ptr<Stmt> Then, Else;

        IfStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> thenStmt, std::unique_ptr<Stmt> elseStmt)
            : Cond(std::move(cond)), Then(std::move(thenStmt)), Else(std::move(elseStmt)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
    };

    class ForStmt : public Stmt
    {
    public:
        std::unique_ptr<Stmt> Init;
        std::unique_ptr<Expr> Cond, Step;
        std::unique_ptr<BlockStmt> Body;

        ForStmt(std::unique_ptr<Stmt> init, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> step, std::unique_ptr<BlockStmt> body)
            : Init(std::move(init)), Cond(std::move(cond)), Step(std::move(step)), Body(std::move(body)) {}

        llvm::Value *accept(ASTVisitor &visitor) override;
    };

    class ASTVisitor
    {

    public:
        virtual ~ASTVisitor() = default;
        virtual llvm::Value *visit(BinaryExpr &expr) = 0;
        virtual llvm::Value *visit(UnaryExpr &expr) = 0;
        virtual llvm::Value *visit(LiteralExpr &expr) = 0;
        virtual llvm::Value *visit(VarExpr &expr) = 0;
        virtual llvm::Value *visit(CallExpr &expr) = 0;
        virtual llvm::Value *visit(ArrayAccessExpr &expr) = 0;
        virtual llvm::Function *visit(Prototype &proto) = 0;
        virtual llvm::Function *visit(miniC::Function &func) = 0;
        virtual llvm::Value *visit(BlockStmt &stmt) = 0;
        virtual llvm::Value *visit(ExprStmt &stmt) = 0;
        virtual llvm::Value *visit(DefStmt &stmt) = 0;
        virtual llvm::Value *visit(IfStmt &stmt) = 0;
        virtual llvm::Value *visit(ForStmt &stmt) = 0;
    };

    class IRGenerator : public ASTVisitor
    {

    protected:
        std::unique_ptr<llvm::LLVMContext> context;
        std::unique_ptr<llvm::Module> module;
        std::unique_ptr<llvm::IRBuilder<>> builder;
        std::map<std::string, llvm::Value *> namedValues;
        // Maps miniC types to their corresponding LLVM type constructors
        std::map<miniC::Type, llvm::Type *> typeMap;
        // for llvm opt
        // Create new pass and analysis managers.
        std::unique_ptr<llvm::FunctionPassManager> TheFPM;
        std::unique_ptr<llvm::LoopAnalysisManager> TheLAM;
        std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;
        std::unique_ptr<llvm::CGSCCAnalysisManager> TheCGAM;
        std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;
        std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;

    public:
        IRGenerator();
        llvm::Value *visit(BinaryExpr &expr) override;
        llvm::Value *visit(UnaryExpr &expr) override;
        llvm::Value *visit(LiteralExpr &expr) override;
        llvm::Value *visit(VarExpr &expr) override;
        llvm::Value *visit(CallExpr &expr) override;
        llvm::Value *visit(ArrayAccessExpr &expr) override;
        llvm::Function *visit(Prototype &proto) override;
        llvm::Function *visit(miniC::Function &func) override;
        llvm::Value *visit(BlockStmt &stmt) override;
        llvm::Value *visit(ExprStmt &stmt) override;
        llvm::Value *visit(DefStmt &stmt) override;
        llvm::Value *visit(IfStmt &stmt) override;
        llvm::Value *visit(ForStmt &stmt) override;

        // Additional methods for IR generation
        void generateIR(const std::vector<std::unique_ptr<ASTNode>> &astNodes);
    };

} // namespace miniC
