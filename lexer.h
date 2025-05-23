// Lexer.h
#pragma once
#include <string>
#include <vector>
#include <unordered_map>

enum class TokenKind {
    // End-of-file
    EOF_TOK,

    // Identifiers and literals
    Identifier,            // e.g. foo, _bar, x123
    IntegerLiteral,        // decimal, octal, hex, (and optionally binary)
    FloatingLiteral,       // e.g. 3.14, .5, 1e9
    CharacterLiteral,      // e.g. 'a', '\n'
    StringLiteral,         // e.g. "hello\n"

    // Keywords
    #define KEYWORD(str,enum_name, description) enum_name,
    #include "keywords.def"
    #undef KEYWORD

    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    PlusPlus,       // ++
    MinusMinus,     // --
    Arrow,          // ->
    Amp,            // &
    Bar,            // |
    Caret,          // ^
    Tilde,          // ~
    Exclaim,        // !
    AmpAmp,         // &&
    BarBar,         // ||
    Question,       // ?
    Colon,          // :
    Assign,         // =
    PlusEqual,      // +=
    MinusEqual,     // -=
    StarEqual,      // *=
    SlashEqual,     // /=
    PercentEqual,   // %=
    AmpEqual,       // &=
    BarEqual,       // |=
    CaretEqual,     // ^=
    Less,           // <
    Greater,        // >
    LessEqual,      // <=
    GreaterEqual,   // >=
    EqualEqual,     // ==
    NotEqual,       // !=

    // Punctuation
    LParen,         // (
    RParen,         // )
    LBracket,       // [
    RBracket,       // ]
    LBrace,         // {
    RBrace,         // }
    Semicolon,      // ;
    Comma,          // ,
    Dot,            // .
    Ellipsis,       // ...

    // Preprocessor (if you want to tokenize directives)
    Hash,           // #
    HashHash,       // ##
    // Note: you may also choose to handle entire preprocessor lines as a single token.

    // (Optional) comments and whitespace if you need to preserve them for macros
    Comment,
    Whitespace,
    Unknown
};

struct Token {
    TokenKind kind;
    std::string lexeme;
    size_t line, column;
    int64_t intValue;
    std::string stringValue;

    Token(TokenKind kind, const std::string& lexeme, size_t line, size_t column)
        : kind(kind), lexeme(lexeme), line(line), column(column), intValue(0) {}
};

class Lexer {
public:
    Lexer(const std::string& input);

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
