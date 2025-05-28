// parser.cpp - Recursive descent parser implementation
#include "parser.h"
#include "ast.h"
#include <stdexcept>

using namespace miniC;

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

std::unique_ptr<Expr> Parser::parseLiteralExpr()
{
    auto value = currentToken.lexeme;
    auto type = currentToken.kind;
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
    return expr;
}

std::unique_ptr<Expr> Parser::parseExpression()
{
    auto lhs = parsePrimary();
    return parseBinOpRHS(0, std::move(lhs));
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
    if (!expect(TokenKind::LBrace))
    {
        throw std::runtime_error("Expected '{' in function definition");
    }

    if(match(TokenKind::RBrace))
    {
        if(proto->returnType != Type::Void)
        {
            throw std::runtime_error("Function with non-void return type cannot have an empty body");
        }
        nextToken(); // Empty function body
        return std::make_unique<Function>(std::move(proto), nullptr);
    }

    auto body = parseExpression();
    if (!body)
    {
        throw std::runtime_error("Expected function body");
    }

    if (!expect(TokenKind::RBrace))
    {
        throw std::runtime_error("Expected '}' in function definition");
    }

    return std::make_unique<Function>(std::move(proto), std::move(body));
}

/// top ::= definition | expression
void Parser::MainLoop()
{
    while (true)
    {
        fprintf(stderr, ">");
        nextToken();
        if (currentToken.kind == TokenKind::EOF_TOK)
            break;

        if (currentToken.kind >= TokenKind::Kw_Int && currentToken.kind <= TokenKind::Kw_Void)
        {
            parseFunction();
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
