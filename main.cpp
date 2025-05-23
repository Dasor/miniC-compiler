#include "lexer.h"
#include <iostream>
#include <string>

int main() {
    // Read from standard input
    std::string code;
    std::string line;
    while (std::getline(std::cin, line)) {
        code += line + "\n";
    }

    // Lexer demonstration
    Lexer lexer(code);
    while (true) {
        Token tok = lexer.gettok();
        std::cout << "Token: " << static_cast<int>(tok.kind) << " '" << tok.lexeme << "'\n";
        if (tok.kind == TokenKind::EOF_TOK) break;
    }
}