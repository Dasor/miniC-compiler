#pragma once
#include "ast.h"
#include "lexer.h"
#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#define OVERRIDE_BINARY_OPS \
    llvm::Value* add(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* sub(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* mul(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* div(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* mod(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* assign(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* bitwiseAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* bitwiseOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* bitwiseXor(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* lessThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* lessThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* greaterThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* greaterThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* equal(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* notEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* logicalAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* logicalOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) override; \
    llvm::Value* logicalNot(llvm::Value* operand, llvm::IRBuilder<>& builder) override;


// Strategy pattern for binary operations
// base class

class BinaryOp {
public:
    virtual ~BinaryOp() = default;

    // Lookup the operation based on the token kind
    llvm::Value* perform(llvm::Value* lhs, llvm::Value* rhs, miniC::TokenKind op, llvm::IRBuilder<>& builder);
    // Virtual functions for the ops
    virtual llvm::Value* add(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* sub(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* mul(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* div(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* mod(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* assign(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* bitwiseAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* bitwiseOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* bitwiseXor(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* lessThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* lessThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* greaterThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* greaterThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* equal(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* notEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* logicalAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* logicalOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) = 0;
    virtual llvm::Value* logicalNot(llvm::Value* operand, llvm::IRBuilder<>& builder) = 0;
}; 

class BinaryOpBuilder {
public:
    static std::unique_ptr<BinaryOp> createBinaryOp(llvm::Type *type);
};

class IntBinaryOp : public BinaryOp {
    public:
        OVERRIDE_BINARY_OPS
};

class FloatBinaryOp : public BinaryOp {
    public:
        OVERRIDE_BINARY_OPS
};

class PtrBinaryOp : public BinaryOp {
    public:
        OVERRIDE_BINARY_OPS
};
