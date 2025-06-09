// parser.cpp - Recursive descent parser implementation
#include "parser.h"
#include "ast.h"
#include <stdexcept>

// define macro for expect semicolon
#define EXPECT_SEMICOLON() \
    if (!expect(TokenKind::Semicolon)) { \
        throw std::runtime_error("Expected ';' after expression"); \
    }

using namespace miniC;

// define IRGenerator visitor
IRGenerator visitor;

void Parser::nextToken()
{
    currentToken = lexer.gettok();
}

bool Parser::expect(TokenKind kind)
{
    if (currentToken.kind == kind)
    {
        nextToken();
        return true;
    }
    return false;
}

bool Parser::match(TokenKind kind)
{
    return currentToken.kind == kind;
}

bool Parser::match(TokenKind lower, TokenKind upper)
{
    return (currentToken.kind >= lower && currentToken.kind <= upper);
}

int Parser::getTokenPrecedence(TokenKind op)
{
    switch (op)
    {
    case TokenKind::Assign:
    case TokenKind::PlusEqual:
    case TokenKind::MinusEqual:
    case TokenKind::StarEqual:
    case TokenKind::SlashEqual:
    case TokenKind::PercentEqual:
    case TokenKind::AmpEqual:
    case TokenKind::BarEqual:
    case TokenKind::CaretEqual:
        return 10;
    case TokenKind::BarBar:
        return 20;
    case TokenKind::AmpAmp:
        return 30;
    case TokenKind::Bar:
        return 40;
    case TokenKind::Caret:
        return 50;
    case TokenKind::Amp:
        return 60;
    case TokenKind::EqualEqual:
    case TokenKind::NotEqual:
        return 70;
    case TokenKind::Less:
    case TokenKind::LessEqual:
    case TokenKind::Greater:
    case TokenKind::GreaterEqual:
        return 80;
    case TokenKind::Plus:
    case TokenKind::Minus:
        return 90;
    case TokenKind::Star:
    case TokenKind::Slash:
    case TokenKind::Percent:
        return 100;
    default:
        return -1;
    }
}

std::unique_ptr<Expr> Parser::parsePrimary()
{
    switch (currentToken.kind)
    {
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

std::unique_ptr<Expr> Parser::parseIdentifierExpr()
{
    std::string name = currentToken.lexeme;
    nextToken();
    return std::make_unique<VarExpr>(name);
}

std::unique_ptr<Stmt> Parser::parseDefinition(){
    auto type = currentToken.kind;
    nextToken(); // Eat the type keyword
    if(!match(TokenKind::Identifier))
    {
        throw std::runtime_error("Expected variable name after type keyword");
    }
    std::string name = currentToken.lexeme;
    nextToken(); // Eat the variable name
    std::unique_ptr<Expr> initValue = nullptr;
    if (match(TokenKind::Assign))
    {
        nextToken(); // Eat '='
        initValue = parseExpression();
        if (!initValue)
        {
            throw std::runtime_error("Expected expression after '='");
        }
    }else{
        EXPECT_SEMICOLON();
    }
    VarExpr var(name);
    return std::make_unique<DefStmt>(var, std::move(initValue));
}

std::unique_ptr<Expr> Parser::parseLiteralExpr()
{
    auto type = currentToken.kind;
    std::string value = currentToken.lexeme;
    nextToken();
    return std::make_unique<LiteralExpr>(value, type);
}

std::unique_ptr<Expr> Parser::parseParenExpr()
{
    nextToken(); // Eat '('
    auto expr = parseExpression();
    if (!expect(TokenKind::RParen))
    {
        throw std::runtime_error("Expected ')'");
    }
    EXPECT_SEMICOLON();
    return expr;
}

std::unique_ptr<Expr> Parser::parseExpression()
{
    // Just eat the return, control flow not yet implemented
    if (currentToken.kind == TokenKind::Kw_Return)
        nextToken();

    auto lhs = parsePrimary();
    auto expr = parseBinOpRHS(0, std::move(lhs));
    return expr;
}

std::unique_ptr<Stmt> Parser::parseStatement() {
    if (currentToken.kind >= TokenKind::Kw_Int && currentToken.kind <= TokenKind::Kw_Void) {
        return parseDefinition();
    } else {
        auto expr = parseExpression();
        EXPECT_SEMICOLON();
        return std::make_unique<ExprStmt>(std::move(expr));
    }
}

std::unique_ptr<BlockStmt> Parser::parseBlock() {
    if (!expect(TokenKind::LBrace)) {
        return nullptr;
    }

    auto block = std::make_unique<BlockStmt>();
    while (!match(TokenKind::RBrace) && !match(TokenKind::EOF_TOK)) {
        if (auto stmt = parseStatement()) {
            block->addStatement(std::move(stmt));
        } else {
            // Error recovery - skip to next statement
            while (!match(TokenKind::Semicolon) && 
                   !match(TokenKind::RBrace) && 
                   !match(TokenKind::EOF_TOK)) {
                nextToken();
            }
            if (match(TokenKind::Semicolon)) nextToken();
        }
    }

    if (!expect(TokenKind::RBrace)) {
        throw std::runtime_error("Expected '}' at end of block");
    }

    return block;
}

std::unique_ptr<Expr> Parser::parseBinOpRHS(int precedence, std::unique_ptr<Expr> lhs)
{
    while (true)
    {
        int currPrec = getTokenPrecedence(currentToken.kind);
        if (currPrec < precedence)
            return lhs;

        TokenKind op = currentToken.kind;
        nextToken();
        auto rhs = parsePrimary();
        if (!rhs)
            return nullptr;

        int nextPrec = getTokenPrecedence(currentToken.kind);
        if (currPrec < nextPrec)
        {
            rhs = parseBinOpRHS(currPrec + 1, std::move(rhs));
            if (!rhs)
                return nullptr;
        }

        lhs = std::make_unique<BinaryExpr>(std::move(lhs), op, std::move(rhs));
    }
}

std::unique_ptr<Prototype> Parser::parsePrototype()
{

    // Parse return type (default to int if not specified)
    Type returnType = Type::Int;
    if (match(TokenKind::Kw_Int, TokenKind::Kw_Void))
    {
        switch (currentToken.kind)
        {
        case TokenKind::Kw_Int:
            returnType = Type::Int;
            break;
        case TokenKind::Kw_Float:
            returnType = Type::Float;
            break;
        case TokenKind::Kw_Char:
            returnType = Type::Char;
            break;
        case TokenKind::Kw_Void:
            returnType = Type::Void;
            break;
        default:
            throw std::runtime_error("Unexpected return type");
        }
    }
    else
    {
        // throw warning
    }

    nextToken(); // Eat the type keyword

    // Parse function name
    std::string name;
    if (currentToken.kind == TokenKind::Identifier)
    {
        name = currentToken.lexeme;
        nextToken();
    }
    else
    {
        throw std::runtime_error("Expected function name");
    }

    // Parse parameters
    std::vector<VarExpr> params;
    if (!expect(TokenKind::LParen))
    {
        throw std::runtime_error("Expected '(' in prototype");
    }

    while (!match(TokenKind::RParen))
    {
        if (currentToken.kind == TokenKind::Identifier)
        {
            params.emplace_back(currentToken.lexeme);
            nextToken();

            if (match(TokenKind::Comma))
            {
                nextToken();
            }
        }
        else
        {
            throw std::runtime_error("Expected parameter name");
        }
    }
    nextToken(); // Eat ')'

    return std::make_unique<Prototype>(name, std::move(params), returnType);
}

std::unique_ptr<Function> Parser::parseFunction()
{
    // Parse prototype
    auto proto = parsePrototype();
    if (!proto)
    {
        return nullptr;
    }

    // Parse function body
    auto body = parseBlock();
    if (!body && proto->returnType != Type::Void)
    {
        throw std::runtime_error("Function with non-void return type cannot have an empty body");
    }

    return std::make_unique<Function>(std::move(proto), std::move(body));
}

/// top ::= definition | expression
void Parser::MainLoop()
{
    while (true)
    {
        nextToken();
        if (currentToken.kind == TokenKind::EOF_TOK)
            break;

        if (currentToken.kind >= TokenKind::Kw_Int && currentToken.kind <= TokenKind::Kw_Void)
        {
            if (auto FnAST = parseFunction())
            {
                if (auto FnIR = FnAST->accept(visitor))
                {
                    // Handle the generated IR for the function
                    fprintf(stderr, "Generated IR for function: %s\n", FnAST->proto->name.c_str());
                    FnIR->print(llvm::errs());
                    fprintf(stderr, "\n");
                }
            }
        }
        else if (currentToken.kind == TokenKind::Identifier || currentToken.kind == TokenKind::IntegerLiteral ||
                 currentToken.kind == TokenKind::FloatingLiteral || currentToken.kind == TokenKind::CharacterLiteral ||
                 currentToken.kind == TokenKind::StringLiteral || currentToken.kind == TokenKind::LParen)
        {
            parseExpression();
        }
        else
        {
            throw std::runtime_error("Unexpected token: " + currentToken.lexeme);
        }
    }
}
