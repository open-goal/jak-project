include_directories(tree-sitter/lib/src/)
add_library(tree-sitter
        tree-sitter/lib/src/lib.c
        tree-sitter-opengoal/parser.c)
