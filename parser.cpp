// parser.cpp - Recursive descent parser implementation
#include "parser.h"
#include "ast.h"
#include <map>
#include <stdexcept>

// define macro for expect semicolon
#define EXPECT_SEMICOLON()                                         \
    if (!expect(TokenKind::Semicolon))                             \
    {                                                              \
        throw std::runtime_error("Expected ';' after expression"); \
    }

using namespace miniC;

// define IRGenerator visitor
IRGenerator visitor = IRGenerator();

static std::map<TokenKind, Type> tokenToType = {
    {TokenKind::Kw_Int, Type::Int},
    {TokenKind::Kw_Float, Type::Float},
    {TokenKind::Kw_Char, Type::Char},
    {TokenKind::Kw_Void, Type::Void}};

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
    case TokenKind::PlusPlus:
    case TokenKind::MinusMinus:
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

    if (!match(TokenKind::LParen) && !match(TokenKind::LBracket))
    {
        // It's a variable, not a function call
        return std::make_unique<VarExpr>(name);
    }

    if (match(TokenKind::LBracket))
    {
        // It's an array access
        nextToken(); // Eat the '['
        auto index = parseExpression();
        if (!index)
        {
            throw std::runtime_error("Expected index expression in array access");
        }
        if (!expect(TokenKind::RBracket))
        {
            throw std::runtime_error("Expected ']' after array index");
        }
        return std::make_unique<ArrayAccessExpr>(std::make_unique<VarExpr>(name), std::move(index));
    }
    nextToken(); // Eat the '('
    std::vector<std::unique_ptr<Expr>> args;
    while (!match(TokenKind::RParen))
    {
        if (currentToken.kind == TokenKind::EOF_TOK)
        {
            throw std::runtime_error("Unexpected end of input in function call");
        }
        args.push_back(parseExpression());
        if (expect(TokenKind::Comma))
        {
        }
        else if (!match(TokenKind::RParen))
        {
            throw std::runtime_error("Expected ',' or ')' in function call arguments");
        }
    }
    nextToken(); // Eat the ')'

    return std::make_unique<CallExpr>(name, std::move(args));
}

std::unique_ptr<Stmt> Parser::parseDefinition()
{
    auto type = currentToken.kind;
    nextToken(); // Eat the type keyword
    if (!match(TokenKind::Identifier))
    {
        throw std::runtime_error("Expected variable name after type keyword");
    }
    std::string name = currentToken.lexeme;
    nextToken(); // Eat the variable name

    // check for array
    int arraySize = 0;
    if (match(TokenKind::LBracket))
    {
        nextToken(); // Eat '['
        arraySize = currentToken.lexeme.empty() ? 0 : std::stoi(currentToken.lexeme);
        if (currentToken.kind != TokenKind::IntegerLiteral)
        {
            throw std::runtime_error("Expected array size after '['");
        }
        nextToken(); // Eat the array size
        if (!expect(TokenKind::RBracket))
        {
            throw std::runtime_error("Expected ']' after array size");
        }
    }

    std::unique_ptr<Expr> initValue = nullptr;
    if (match(TokenKind::Assign))
    {
        nextToken(); // Eat '='
        initValue = parseExpression();
        if (!initValue)
        {
            throw std::runtime_error("Expected expression after '='");
        }
    }
    VarExpr var(name, tokenToType[type], arraySize);
    return std::make_unique<DefStmt>(std::move(var), std::move(initValue));
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
    return expr;
}

std::unique_ptr<Expr> Parser::parseExpression()
{
    // Just eat the return, control flow not yet implemented
    if (currentToken.kind == TokenKind::Kw_Return)
        nextToken();

    auto lhs = parsePrimary();
    // check if the expression is unary
    if (currentToken.kind == TokenKind::PlusPlus || currentToken.kind == TokenKind::MinusMinus)
    {
        auto expr = std::make_unique<UnaryExpr>(currentToken.kind, std::move(lhs));
        // eat Unary op
        nextToken();
        return expr;
        // check if lhs is an array and we are indexing
    }
    auto expr = parseBinOpRHS(0, std::move(lhs));
    return expr;
}

std::unique_ptr<Stmt> Parser::parseStatement()
{
    if (currentToken.kind >= TokenKind::Kw_Int && currentToken.kind <= TokenKind::Kw_Void)
    {
        auto def = parseDefinition();
        EXPECT_SEMICOLON();
        return def;
    }
    else if (currentToken.kind == TokenKind::Kw_If)
    {
        auto ifStmt = parseIfStmt();
        if (!ifStmt)
        {
            throw std::runtime_error("Failed to parse 'if' statement");
        }
        return ifStmt;
    }
    else if (currentToken.kind == TokenKind::Kw_For)
    {
        auto forStmt = parseForStmt();
        if (!forStmt)
        {
            throw std::runtime_error("Failed to parse 'for' statement");
        }
        return forStmt;
    }
    else if (currentToken.kind == TokenKind::Kw_Return)
    {
        auto retStmt = parseRetStmt();
        if (!retStmt)
        {
            throw std::runtime_error("Failed to parse 'return' statement");
        }
        return retStmt;
    }
    else if (currentToken.kind == TokenKind::LBrace)
    {
        return parseBlock();
    }
    else
    {
        auto expr = parseExpression();
        EXPECT_SEMICOLON();
        return std::make_unique<ExprStmt>(std::move(expr));
    }
}

std::unique_ptr<BlockStmt> Parser::parseBlock()
{
    if (!expect(TokenKind::LBrace))
    {
        return nullptr;
    }

    auto block = std::make_unique<BlockStmt>();
    while (!match(TokenKind::RBrace) && !match(TokenKind::EOF_TOK))
    {
        if (auto stmt = parseStatement())
        {
            block->addStatement(std::move(stmt));
        }
        else
        {
            // Error recovery - skip to next statement
            while (!match(TokenKind::Semicolon) &&
                   !match(TokenKind::RBrace) &&
                   !match(TokenKind::EOF_TOK))
            {
                nextToken();
            }
            if (match(TokenKind::Semicolon))
                nextToken();
        }
    }

    if (!expect(TokenKind::RBrace))
    {
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
        returnType = tokenToType[currentToken.kind];
        if (returnType == Type::Unknown)
        {
            throw std::runtime_error("Unknown return type in prototype");
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
        // parse parameter type
        Type paramType = Type::Int; // default to int
        if (match(TokenKind::Kw_Int, TokenKind::Kw_Float))
        {
            paramType = tokenToType[currentToken.kind];
            nextToken(); // Eat the type keyword
        }
        else
        {
            throw std::runtime_error("Expected parameter type");
        }

        if (currentToken.kind == TokenKind::Identifier)
        {
            params.emplace_back(currentToken.lexeme, paramType);
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

    if (match(TokenKind::Semicolon))
    {
        // Function prototype without body
        nextToken();
        return std::make_unique<Function>(std::move(proto), nullptr);
    }

    // Parse function body
    auto body = parseBlock();
    if (!body && proto->returnType != Type::Void)
    {
        throw std::runtime_error("Function with non-void return type cannot have an empty body");
    }

    return std::make_unique<Function>(std::move(proto), std::move(body));
}

std::unique_ptr<IfStmt> Parser::parseIfStmt()
{
    nextToken(); // Eat 'if'
    if (!expect(TokenKind::LParen))
    {
        throw std::runtime_error("Expected '(' after 'if'");
    }

    auto cond = parseExpression();
    if (!cond)
    {
        throw std::runtime_error("Expected condition expression in 'if' statement");
    }

    if (!expect(TokenKind::RParen))
    {
        throw std::runtime_error("Expected ')' after condition in 'if' statement");
    }

    auto thenStmt = parseBlock();
    if (!thenStmt)
    {
        throw std::runtime_error("Expected block after 'if' condition");
    }

    std::unique_ptr<Stmt> elseStmt;
    if (match(TokenKind::Kw_Else))
    {
        nextToken(); // Eat 'else'
        if (match(TokenKind::Kw_If))
        {
            // Nested if statement
            elseStmt = parseIfStmt();
        }
        else
        {
            elseStmt = parseBlock();
        }
        if (!elseStmt)
        {
            throw std::runtime_error("Expected block after 'else'");
        }
    }

    return std::make_unique<IfStmt>(std::move(cond), std::move(thenStmt), std::move(elseStmt));
}

std::unique_ptr<ForStmt> Parser::parseForStmt()
{
    // eat for
    nextToken();

    if (!expect(TokenKind::LParen))
    {
        throw std::runtime_error("Expected '(' after 'for'");
    }
    // Parse initialization
    std::unique_ptr<Stmt> init;
    if (currentToken.kind == TokenKind::Kw_Int || currentToken.kind == TokenKind::Kw_Float ||
        currentToken.kind == TokenKind::Kw_Char)
    {
        init = parseDefinition();
    }

    EXPECT_SEMICOLON();

    // Parse condition
    std::unique_ptr<Expr> cond;
    cond = parseExpression();
    if (!cond)
    {
        throw std::runtime_error("Expected condition expression in 'for' statement");
    }

    EXPECT_SEMICOLON();

    // Parse step
    std::unique_ptr<Expr> step;
    step = parseExpression();
    if (!step)
    {
        throw std::runtime_error("Expected step expression in 'for' statement");
    }
    if (!expect(TokenKind::RParen))
    {
        throw std::runtime_error("Expected ')' after 'for' condition");
    }
    // Parse body
    auto body = parseBlock();
    if (!body)
    {
        throw std::runtime_error("Expected block after 'for' condition");
    }

    return std::make_unique<ForStmt>(std::move(init), std::move(cond), std::move(step), std::move(body));
}

std::unique_ptr<RetStmt> Parser::parseRetStmt()
{
    nextToken(); // Eat 'return'
    std::unique_ptr<Expr> returnValue;
    if (currentToken.kind != TokenKind::Semicolon)
    {
        returnValue = parseExpression();
        if (!returnValue)
        {
            throw std::runtime_error("Expected expression after 'return'");
        }
    }
    EXPECT_SEMICOLON();
    return std::make_unique<RetStmt>(std::move(returnValue));
}

void Parser::MainLoop()
{
    nextToken();
    while (true)
    {
        if (currentToken.kind == TokenKind::EOF_TOK)
            break;

        if (currentToken.kind >= TokenKind::Kw_Int && currentToken.kind <= TokenKind::Kw_Void)
        {
            if (auto FnAST = parseFunction())
            {
                FnAST->accept(visitor); // Generate IR for the function
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
    visitor.printIR(); // Print the generated IR after each function
}
