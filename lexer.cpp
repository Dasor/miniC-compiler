// Lexer.cpp
#include <algorithm>
#include <string>
#include "lexer.h"
#include <cctype>
#include <iostream>

using namespace miniC;

Lexer::Lexer(const std::string& input)
    : source(input), index(0), line(1), column(1) {}

// Create constructor for stdin
Lexer::Lexer()
    : source(""), index(0), line(1), column(1) {
    std::string input;
    std::getline(std::cin, input, '\0'); // Read until EOF
    source = input;
}

char Lexer::currentChar() const {
    return index < source.size() ? source[index] : '\0';
}

void Lexer::advance() {
    if (index < source.size()) {
        ++index;
        ++column;
    }
}

bool Lexer::isAtEnd() const {
    return index >= source.size();
}

// Define gettok and the rest of the methods here...
Token Lexer::gettok() {
    skipWhitespaceAndComments();

    if (isAtEnd()) {
        return Token(TokenKind::EOF_TOK, "", line, column);
    }

    char c = currentChar();

    if (std::isalpha(c) || c == '_') {
        return lexIdentifierOrKeyword();
    }

    if (std::isdigit(c)) {
        return lexNumber();
    }

    if (c == '"') {
        return lexStringLiteral();
    }

    if (c == '\'') {
        return lexCharLiteral();
    }

    return lexOperatorOrPunctuator(); // Handles +, -, ==, etc.
}

void Lexer::skipWhitespaceAndComments() {
    while (!isAtEnd()) {
        char c = currentChar();
        if (std::isspace(c)) {
            if (c == '\n') { ++line; column = 0; }
            advance();
        } else if (c == '/' && source[index + 1] == '/') {
            while (!isAtEnd() && currentChar() != '\n') advance();
        } else if (c == '/' && source[index + 1] == '*') {
            advance(); advance(); // skip /*
            while (!isAtEnd() && !(currentChar() == '*' && source[index + 1] == '/')) {
                if (currentChar() == '\n') { ++line; column = 0; }
                advance();
            }
            if (!isAtEnd()) { advance(); advance(); } // skip */
        } else {
            break;
        }
    }
}

Token Lexer::lexIdentifierOrKeyword() {
    size_t start = index;
    while (std::isalnum(currentChar()) || currentChar() == '_') advance();

    std::string text = source.substr(start, index - start);

    // Check if it's a keyword
    static const std::unordered_map<std::string, TokenKind> keywords = {
        #define KEYWORD(str, enum_name, description) {str, TokenKind::enum_name},
        #include "keywords.def"
        #undef KEYWORD
    };

    auto it = keywords.find(text);
    TokenKind kind = (it != keywords.end()) ? it->second : TokenKind::Identifier;

    Token tok(kind, text, line, column);
    return tok;
}

Token Lexer::lexNumber() {
    size_t start = index;
    bool isFloat = false;

    // Consume digits before the decimal point
    while (std::isdigit(currentChar())) advance();

    // Check for a decimal point
    if (currentChar() == '.') {
        isFloat = true;
        advance(); // Consume the '.'

        // Consume digits after the decimal point
        while (std::isdigit(currentChar())) advance();
    }

    // Check for scientific notation (e.g., 1e10, 3.14E-2)
    if (currentChar() == 'e' || currentChar() == 'E') {
        isFloat = true;
        advance(); // Consume 'e' or 'E'

        // Optional sign after 'e' or 'E'
        if (currentChar() == '+' || currentChar() == '-') {
            advance();
        }

        // Consume digits in the exponent
        while (std::isdigit(currentChar())) advance();
    }

    std::string text = source.substr(start, index - start);

    if (isFloat) {
        Token tok(TokenKind::FloatingLiteral, text, line, column);
        return tok;
    } else {
        Token tok(TokenKind::IntegerLiteral, text, line, column);
        return tok;
    }
}

Token Lexer::lexStringLiteral() {
    advance(); // skip opening quote
    std::string text;
    while (!isAtEnd() && currentChar() != '"') {
        if (currentChar() == '\\') {
            advance(); // skip escape character
            if (!isAtEnd()) {
                switch (currentChar()) {
                    case 'n': text += '\n'; break;
                    case 't': text += '\t'; break;
                    case '\\': text += '\\'; break;
                    case '"': text += '"'; break;
                    default: text += currentChar(); break;
                }
            }
        } else {
            text += currentChar();
        }
        advance();
    }
    advance(); // skip closing quote
    return Token(TokenKind::StringLiteral, text, line, column);
}

Token Lexer::lexCharLiteral() {
    advance(); // skip opening quote
    size_t start = index;
    while (!isAtEnd() && currentChar() != '\'') {
        if (currentChar() == '\\') advance(); // skip escape character
        advance();
    }
    std::string text = source.substr(start, index - start);
    advance(); // skip closing quote
    return Token(TokenKind::CharacterLiteral, text, line, column);
}

Token Lexer::lexOperatorOrPunctuator() {
    // Create operator lookup table from X macros
    static const std::unordered_map<std::string, TokenKind> operators = {
        #define OPERATOR(str, enum_name, description) {str, TokenKind::enum_name},
        #include "operators.def"
        #undef OPERATOR
    };

    // Try matching longest possible operator first (max 3 chars)
    std::string lexeme;
    for (int len = 3; len >= 1; len--) {
        if (index + len > source.size()) continue;
        
        lexeme = source.substr(index, len);
        auto it = operators.find(lexeme);
        if (it != operators.end()) {
            for (int i = 0; i < len; i++) advance();
            return Token(it->second, lexeme, line, column);
        }
    }

    // No match found
    advance();
    return Token(TokenKind::Unknown, std::string(1, currentChar()), line, column);
}
