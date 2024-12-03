#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <antlr3.h>
#include "CLanguageLexer.h"
#include "CLanguageParser.h"

// Function to generate a unique node ID
static int nodeCounter = 0;

char* generateNodeID() {
    static char buffer[32];
    snprintf(buffer, sizeof(buffer), "node%d", nodeCounter++);
    return buffer;
}

// Recursive function to traverse the tree and generate DOT representation
void generateDOT(pANTLR3_BASE_TREE tree, FILE* output) {
    if (tree == NULL) {
        return;
    }

    // Generate current node
    char* currentNodeID = generateNodeID();
    fprintf(output, "%s [label=\"%s\"];\n", currentNodeID, (char*)tree->toString(tree)->chars);

    // Traverse children
    ANTLR3_UINT32 childCount = tree->getChildCount(tree);
    for (ANTLR3_UINT32 i = 0; i < childCount; i++) {
        pANTLR3_BASE_TREE child = (pANTLR3_BASE_TREE)tree->getChild(tree, i);
        char* childNodeID = generateNodeID();
        fprintf(output, "%s [label=\"%s\"];\n", childNodeID, (char*)child->toString(child)->chars);
        fprintf(output, "%s -> %s;\n", currentNodeID, childNodeID);

        // Recurse on child
        generateDOT(child, output);
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    // Open input file
    pANTLR3_INPUT_STREAM input = antlr3FileStreamNew((pANTLR3_UINT8)argv[1], ANTLR3_ENC_UTF8);
    if (input == NULL) {
        fprintf(stderr, "Failed to open input file: %s\n", argv[1]);
        return 1;
    }

    // Initialize Lexer
    pCLanguageLexer lexer = CLanguageLexerNew(input);
    if (lexer == NULL) {
        fprintf(stderr, "Failed to create lexer\n");
        input->close(input);
        return 1;
    }

    // Initialize Token Stream
    pANTLR3_COMMON_TOKEN_STREAM tokens = antlr3CommonTokenStreamSourceNew(ANTLR3_SIZE_HINT, TOKENSOURCE(lexer));
    if (tokens == NULL) {
        fprintf(stderr, "Failed to create token stream\n");
        lexer->free(lexer);
        input->close(input);
        return 1;
    }

    // Initialize Parser
    pCLanguageParser parser = CLanguageParserNew(tokens);
    if (parser == NULL) {
        fprintf(stderr, "Failed to create parser\n");
        tokens->free(tokens);
        lexer->free(lexer);
        input->close(input);
        return 1;
    }

    // Parse the input (entry point is expr rule)
    CLanguageParser_expr_return result = parser->expr(parser);  // Execute the parsing rule
    if (parser->pParser->rec->state->errorCount > 0) {
        fprintf(stderr, "Parsing failed with errors.\n");
        parser->free(parser);
        tokens->free(tokens);
        lexer->free(lexer);
        input->close(input);
        return 1;
    }

    // Retrieve the parse tree
    pANTLR3_BASE_TREE tree = result.tree;

    if (tree == NULL) {
        fprintf(stderr, "Parse tree is NULL\n");
        parser->free(parser);
        tokens->free(tokens);
        lexer->free(lexer);
        input->close(input);
        return 1;
    }

    // Open DOT file for writing
    FILE* dotFile = fopen("output.dot", "w");
    if (dotFile == NULL) {
        fprintf(stderr, "Failed to open output file\n");
        parser->free(parser);
        tokens->free(tokens);
        lexer->free(lexer);
        input->close(input);
        return 1;
    }

    // Write DOT header
    fprintf(dotFile, "digraph G {\n");

    // Generate DOT representation
    generateDOT(tree, dotFile);

    // Write DOT footer
    fprintf(dotFile, "}\n");

    // Cleanup
    fclose(dotFile);
    parser->free(parser);
    tokens->free(tokens);
    lexer->free(lexer);
    input->close(input);

    printf("DOT file generated: output.dot\n");
    return 0;
}
