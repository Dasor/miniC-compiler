// Lexer.h
#pragma once
#include <string>
#include <vector>
#include <unordered_map>

namespace miniC
{

    enum class TokenKind
    {
        // End-of-file
        EOF_TOK,

        // Identifiers and literals
        Identifier,       // e.g. foo, _bar, x123
        IntegerLiteral,   // decimal, octal, hex, (and optionally binary)
        FloatingLiteral,  // e.g. 3.14, .5, 1e9
        CharacterLiteral, // e.g. 'a', '\n'
        StringLiteral,    // e.g. "hello\n"

// Keywords
#define KEYWORD(str, enum_name, description) enum_name,
#include "keywords.def"
#undef KEYWORD

// Operators and Punctuation
#define OPERATOR(str, enum_name, description) enum_name,
#include "operators.def"
#undef OPERATOR

        // Special tokens
        Comment,
        Whitespace,
        Unknown
    };

    struct Token
    {
        TokenKind kind;
        std::string lexeme;
        size_t line, column;
        int64_t intValue;
        std::string stringValue;

        Token() : kind(TokenKind::Unknown), lexeme(""), line(0), column(0), intValue(0) {}

        Token(TokenKind kind, const std::string &lexeme, size_t line, size_t column)
            : kind(kind), lexeme(lexeme), line(line), column(column), intValue(0) {}
    };

    class Lexer
    {
    public:
        Lexer(const std::string &input);
        Lexer();

        Token gettok();

    private:
        std::string source;
        size_t index;
        size_t line, column;

        char currentChar() const;
        void advance();
        bool isAtEnd() const;

        void skipWhitespaceAndComments();
        Token lexIdentifierOrKeyword();
        Token lexNumber();
        Token lexStringLiteral();
        Token lexCharLiteral();
        Token lexOperatorOrPunctuator();
    };

}