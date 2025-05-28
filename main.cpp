#include "lexer.h"
#include "parser.h"
#include <iostream>
#include <string>

using namespace miniC;

int main() {
    // Read from standard input
    auto code = std::string(std::istreambuf_iterator<char>(std::cin), std::istreambuf_iterator<char>());

    // Lexer demonstration
    Lexer lexer = Lexer(code);
    Parser parser(lexer);
    parser.MainLoop();


}