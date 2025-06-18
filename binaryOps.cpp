#include "binaryOps.h"

using namespace miniC;

llvm::Value* BinaryOp::perform(llvm::Value* lhs, llvm::Value* rhs, miniC::TokenKind op, llvm::IRBuilder<>& builder){

    switch (op)
    {
    case TokenKind::Plus:
        return this->add(lhs, rhs, builder);
        break;
    case TokenKind::Minus:
        return this->sub(lhs, rhs, builder);
        break;
    case TokenKind::Star:
        return this->mul(lhs, rhs, builder);
        break;
    case TokenKind::Slash:
        return this->div(lhs, rhs, builder);
        break;
    case TokenKind::Percent:
        return this->mod(lhs, rhs, builder);
        break;
    case TokenKind::Assign:
        return this->assign(lhs, rhs, builder);
        break;
    case TokenKind::Amp:
        return this->bitwiseAnd(lhs, rhs, builder);
        break;
    case TokenKind::Bar:
        return this->bitwiseOr(lhs, rhs, builder);
        break;
    case TokenKind::Caret:
        return this->bitwiseXor(lhs, rhs, builder);
        break;
    case TokenKind::EqualEqual: 
        return this->equal(lhs, rhs, builder);
        break;
    case TokenKind::NotEqual:  
        return this->notEqual(lhs, rhs, builder);
        break;
    case TokenKind::Less:
        return this->lessThan(lhs, rhs, builder);
        break;
    case TokenKind::LessEqual:
        return this->lessThanEqual(lhs, rhs, builder);
        break;
    case TokenKind::Greater:
        return this->greaterThan(lhs, rhs, builder);
        break;
    case TokenKind::GreaterEqual:
        return this->greaterThanEqual(lhs, rhs, builder);
        break;
    case TokenKind::AmpAmp:
        return this->logicalAnd(lhs, rhs, builder);
        break;
    case TokenKind::BarBar:
        return this->logicalOr(lhs, rhs, builder);
        break;
    case TokenKind::Not:
        return this->logicalNot(lhs, builder);
        break;
    case TokenKind::PlusPlus:
        return this->increase(lhs, builder);
        break;
    default:
        throw std::runtime_error("Unsupported binary operation: " + std::to_string(static_cast<int>(op)));
        break;
    }
}

std::unique_ptr<BinaryOp> BinaryOpBuilder::createBinaryOp(llvm::Type *type) {
    if (type->isIntegerTy()) {
        return std::make_unique<IntBinaryOp>();
    } else if (type->isFloatingPointTy()) {
        return std::make_unique<FloatBinaryOp>();
    } else if (type->isPointerTy()) {
        return std::make_unique<PtrBinaryOp>();
    } else {
        throw std::runtime_error("Unsupported type for binary operation: " + std::to_string(type->getTypeID()));
    }
}

llvm::Value* IntBinaryOp::add(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateAdd(lhs, rhs, "addtmp");
}

llvm::Value* IntBinaryOp::sub(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateSub(lhs, rhs, "subtmp");
}

llvm::Value* IntBinaryOp::mul(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateMul(lhs, rhs, "multmp");
}

llvm::Value* IntBinaryOp::div(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateSDiv(lhs, rhs, "divtmp");
}

llvm::Value* IntBinaryOp::mod(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateSRem(lhs, rhs, "modtmp");
}

llvm::Value* IntBinaryOp::assign(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    builder.CreateStore(rhs, lhs);
    return rhs;
}

llvm::Value* IntBinaryOp::bitwiseAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateAnd(lhs, rhs, "andtmp");
}

llvm::Value* IntBinaryOp::bitwiseOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateOr(lhs, rhs, "ortmp");
}

llvm::Value* IntBinaryOp::bitwiseXor(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateXor(lhs, rhs, "xortmp");
}

llvm::Value* IntBinaryOp::lessThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateICmpSLT(lhs, rhs, "lttmp");
}

llvm::Value* IntBinaryOp::lessThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateICmpSLE(lhs, rhs, "ltetmp");
}

llvm::Value* IntBinaryOp::greaterThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateICmpSGT(lhs, rhs, "gttmp");
}

llvm::Value* IntBinaryOp::greaterThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateICmpSGE(lhs, rhs, "gtetmp");
}

llvm::Value* IntBinaryOp::equal(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateICmpEQ(lhs, rhs, "eqtmp");
}

llvm::Value* IntBinaryOp::notEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateICmpNE(lhs, rhs, "neqtmp");
}

llvm::Value* IntBinaryOp::logicalAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateAnd(builder.CreateICmpNE(lhs, builder.getInt32(0), "lhsbool"),
                            builder.CreateICmpNE(rhs, builder.getInt32(0), "rhsbool"),
                            "andtmp");
}

llvm::Value* IntBinaryOp::logicalOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateOr(builder.CreateICmpNE(lhs, builder.getInt32(0), "lhsbool"),
                           builder.CreateICmpNE(rhs, builder.getInt32(0), "rhsbool"),
                           "ortmp");
}

llvm::Value* IntBinaryOp::logicalNot(llvm::Value* operand, llvm::IRBuilder<>& builder) {
    return builder.CreateICmpEQ(operand, builder.getInt32(0), "nottmp");
}

llvm::Value* IntBinaryOp::increase(llvm::Value* operand, llvm::IRBuilder<>& builder) {
    return builder.CreateAdd(operand, builder.getInt32(1), "incrtmp");
}

// FloatBinaryOp implementations
llvm::Value* FloatBinaryOp::add(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFAdd(lhs, rhs, "faddtmp");
}

llvm::Value* FloatBinaryOp::sub(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFSub(lhs, rhs, "fsubtmp");
}

llvm::Value* FloatBinaryOp::mul(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFMul(lhs, rhs, "fmultmp");
}

llvm::Value* FloatBinaryOp::div(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFDiv(lhs, rhs, "fdivtmp");
}

llvm::Value* FloatBinaryOp::mod(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Modulo operation not supported for floating-point types");
}

llvm::Value* FloatBinaryOp::assign(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    builder.CreateStore(rhs, lhs);
    return rhs;
}

llvm::Value* FloatBinaryOp::bitwiseAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Bitwise operations not supported for floating-point types");
}

llvm::Value* FloatBinaryOp::bitwiseOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Bitwise operations not supported for floating-point types");
}

llvm::Value* FloatBinaryOp::bitwiseXor(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Bitwise operations not supported for floating-point types");
}

llvm::Value* FloatBinaryOp::lessThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFCmpOLT(lhs, rhs, "flttmp");
}

llvm::Value* FloatBinaryOp::lessThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFCmpOLE(lhs, rhs, "fltetmp");
}

llvm::Value* FloatBinaryOp::greaterThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFCmpOGT(lhs, rhs, "fgttmp");
}

llvm::Value* FloatBinaryOp::greaterThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFCmpOGE(lhs, rhs, "fgtetmp");
}

llvm::Value* FloatBinaryOp::equal(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFCmpOEQ(lhs, rhs, "feqtmp");
}

llvm::Value* FloatBinaryOp::notEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    return builder.CreateFCmpONE(lhs, rhs, "fneqtmp");
}

llvm::Value* FloatBinaryOp::logicalAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    auto lhsBool = builder.CreateFCmpONE(lhs, llvm::ConstantFP::get(lhs->getType(), 0.0), "flhsbool");
    auto rhsBool = builder.CreateFCmpONE(rhs, llvm::ConstantFP::get(rhs->getType(), 0.0), "frhsbool");
    return builder.CreateAnd(lhsBool, rhsBool, "fandtmp");
}

llvm::Value* FloatBinaryOp::logicalOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    auto lhsBool = builder.CreateFCmpONE(lhs, llvm::ConstantFP::get(lhs->getType(), 0.0), "flhsbool");
    auto rhsBool = builder.CreateFCmpONE(rhs, llvm::ConstantFP::get(rhs->getType(), 0.0), "frhsbool");
    return builder.CreateOr(lhsBool, rhsBool, "fortmp");
}

llvm::Value* FloatBinaryOp::logicalNot(llvm::Value* operand, llvm::IRBuilder<>& builder) {
    return builder.CreateFCmpOEQ(operand, llvm::ConstantFP::get(operand->getType(), 0.0), "fnottmp");
}

llvm::Value* FloatBinaryOp::increase(llvm::Value* operand, llvm::IRBuilder<>& builder) {
    return builder.CreateFAdd(operand, llvm::ConstantFP::get(operand->getType(), 1.0), "fincrtmp");
}

// PtrBinaryOp implementations
llvm::Value* PtrBinaryOp::add(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::sub(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::mul(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::div(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::mod(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::assign(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    builder.CreateStore(rhs, lhs);
    return rhs;
}

llvm::Value* PtrBinaryOp::bitwiseAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::bitwiseOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::bitwiseXor(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::lessThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::lessThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::greaterThan(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::greaterThanEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::equal(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::notEqual(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::logicalAnd(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::logicalOr(llvm::Value* lhs, llvm::Value* rhs, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::logicalNot(llvm::Value* operand, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}

llvm::Value* PtrBinaryOp::increase(llvm::Value* operand, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported operation for pointer types");
}
