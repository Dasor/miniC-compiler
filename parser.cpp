// parser.cpp - Recursive descent parser implementation
#include "parser.h"
#include <stdexcept>

using namespace miniC;

void Parser::nextToken() {
    currentToken = lexer.gettok();
}

bool Parser::expect(TokenKind kind) {
    if (currentToken.kind == kind) {
        nextToken();
        return true;
    }
    return false;
}

bool Parser::match(TokenKind kind) {
    return currentToken.kind == kind;
}

int Parser::getTokenPrecedence(TokenKind op) {
    switch(op) {
        case TokenKind::Assign:
        case TokenKind::PlusEqual:
        case TokenKind::MinusEqual:
        case TokenKind::StarEqual:
        case TokenKind::SlashEqual:
        case TokenKind::PercentEqual:
        case TokenKind::AmpEqual:
        case TokenKind::BarEqual:
        case TokenKind::CaretEqual: return 10;
        case TokenKind::BarBar: return 20;
        case TokenKind::AmpAmp: return 30;
        case TokenKind::Bar: return 40;
        case TokenKind::Caret: return 50;
        case TokenKind::Amp: return 60;
        case TokenKind::EqualEqual:
        case TokenKind::NotEqual: return 70;
        case TokenKind::Less:
        case TokenKind::LessEqual:
        case TokenKind::Greater:
        case TokenKind::GreaterEqual: return 80;
        case TokenKind::Plus:
        case TokenKind::Minus: return 90;
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent: return 100;
        default: return -1;
    }
}

std::unique_ptr<Expr> Parser::parsePrimary() {
    switch(currentToken.kind) {
        case TokenKind::Identifier:
            return parseIdentifierExpr();
        case TokenKind::IntegerLiteral:
        case TokenKind::FloatingLiteral:
        case TokenKind::CharacterLiteral:
        case TokenKind::StringLiteral:
            return parseLiteralExpr();
        case TokenKind::LParen:
            return parseParenExpr();
        default:
            throw std::runtime_error("Expected primary expression");
    }
}

std::unique_ptr<Expr> Parser::parseIdentifierExpr() {
    std::string name = currentToken.lexeme;
    nextToken();
   //return std::make_unique<VarExpr>(name);
}

std::unique_ptr<Expr> Parser::parseLiteralExpr() {
    auto value = currentToken.lexeme;
    auto type = currentToken.kind;
    nextToken();
    //return std::make_unique<LiteralExpr>(value, type);
}

std::unique_ptr<Expr> Parser::parseParenExpr() {
    nextToken(); // Eat '('
    auto expr = parseExpression();
    if (!expect(TokenKind::RParen)) {
        throw std::runtime_error("Expected ')'");
    }
    return expr;
}

std::unique_ptr<Expr> Parser::parseExpression() {
    auto lhs = parsePrimary();
    return parseBinOpRHS(0, std::move(lhs));
}

std::unique_ptr<Expr> Parser::parseBinOpRHS(int precedence, std::unique_ptr<Expr> lhs) {
    while (true) {
        int currPrec = getTokenPrecedence(currentToken.kind);
        if (currPrec < precedence) return lhs;

        TokenKind op = currentToken.kind;
        nextToken();
        auto rhs = parsePrimary();
        if (!rhs) return nullptr;

        int nextPrec = getTokenPrecedence(currentToken.kind);
        if (currPrec < nextPrec) {
            rhs = parseBinOpRHS(currPrec + 1, std::move(rhs));
            if (!rhs) return nullptr;
        }

        //lhs = std::make_unique<BinaryExpr>(std::move(lhs), op, std::move(rhs));
    }
}
