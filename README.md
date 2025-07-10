# mini-C Compiler

This compiler is inspired by a previous project I worked on [(link here)](https://github.com/Dasor/miniC-llvm/tree/main), as I wanted to continue familiarizing myself with LLVM. I used the [LLVM tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) as a reference but started adapting it to resemble the C language.

It differs from the previous one in many ways: this version is written entirely in C++ and LLVM, without using Flex or Bison. Additionally, it implements types, function calls, and many other features that the earlier compiler lacked.

The project is unfinished and will likely remain that way, as I'm now involved in more serious projects. However, it served as a great warm-up to get more hands-on experience with the LLVM API and better understand both IR and the LLVM infrastructure. I also believe parts of this code, particularly the lexer, parser, and AST can be reused in a future project, using the MLIR API instead, for a new learning experience.

