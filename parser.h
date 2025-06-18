// parser.h - Recursive descent parser for miniC
#pragma once
#include <memory>
#include "ast.h"
#include "lexer.h"

namespace miniC {

class Parser {
public:
    explicit Parser(Lexer &lexer) : lexer(lexer) {}
    void MainLoop();

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
    bool match(TokenKind lower, TokenKind upper);

    // Expression parsing
    std::unique_ptr<Expr> parsePrimary();
    std::unique_ptr<Expr> parseIdentifierExpr();
    std::unique_ptr<Stmt> parseDefinition();
    std::unique_ptr<Expr> parseLiteralExpr();
    std::unique_ptr<Expr> parseParenExpr();
    std::unique_ptr<Expr> parseBinOpRHS(int precedence, std::unique_ptr<Expr> lhs);
    std::unique_ptr<Stmt> parseStatement();
    std::unique_ptr<BlockStmt> parseBlock();
    std::unique_ptr<IfStmt> parseIfStmt();
    std::unique_ptr<ForStmt> parseForStmt();

    // Precedence table
    int getTokenPrecedence(TokenKind op);
};

} // namespace miniC
