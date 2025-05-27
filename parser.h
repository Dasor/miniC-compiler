// parser.h - Recursive descent parser for miniC
#pragma once
#include <memory>
#include "ast.h"
#include "lexer.h"

namespace miniC {

class Parser {
public:
    explicit Parser(Lexer &lexer) : lexer(lexer) {}

    // Main parsing interface
    std::unique_ptr<Function> parseFunction();
    std::unique_ptr<Prototype> parsePrototype();
    std::unique_ptr<Expr> parseExpression();

private:
    Lexer &lexer;
    Token currentToken;

    // Helper functions
    void nextToken();
    bool expect(TokenKind kind);
    bool match(TokenKind kind);
    void MainLoop();

    // Expression parsing
    std::unique_ptr<Expr> parsePrimary();
    std::unique_ptr<Expr> parseIdentifierExpr();
    std::unique_ptr<Expr> parseLiteralExpr();
    std::unique_ptr<Expr> parseParenExpr();
    std::unique_ptr<Expr> parseBinOpRHS(int precedence, std::unique_ptr<Expr> lhs);

    // Precedence table
    int getTokenPrecedence(TokenKind op);
};

} // namespace miniC
