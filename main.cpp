#include "lexer.h"
#include <iostream>

int main() {
    std::string code = "int main() { return 42; }";
    Lexer lexer(code);

    while (true) {
        Token tok = lexer.gettok();
        std::cout << "Token: " << static_cast<int>(tok.kind) << " '" << tok.lexeme << "'\n";
        if (tok.kind == TokenKind::EOF_TOK) break;
    }

    return 0;
}
