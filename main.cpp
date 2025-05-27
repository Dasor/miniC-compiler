#include "lexer.h"
#include "parser.h"
#include <iostream>
#include <string>

using namespace miniC;

int main() {
    // Read from standard input
    std::string code;
    std::string line;
    while (std::getline(std::cin, line)) {
        code += line + "\n";
    }

    // Lexer demonstration
    Lexer lexer = Lexer(code);
    Parser parser(lexer);


}