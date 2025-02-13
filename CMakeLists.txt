cmake_minimum_required(VERSION 3.15)
project(parser C)

set(CMAKE_C_STANDARD 11)

include_directories(/usr/local/Cellar/libantlr3c/3.5.3/include)
link_directories(/usr/local/Cellar/libantlr3c/3.5.3/lib)

add_executable(parser main.c grammar/GrammarLexer.c grammar/GrammarParser.c
        ast/ast.c
        graph/graph.c
        graph/graphStructures.h)

target_link_libraries(parser antlr3c)