#include <string.h>

#include "../grammar/GrammarLexer.h"
#include "../grammar/GrammarParser.h"
#include "antlr3interfaces.h"
#include <antlr3.h>
#include <dirent.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

bool currTreeOn = false;
pANTLR3_BASE_TREE currNode = NULL;
pANTLR3_BASE_TREE sourceNode = NULL;
pANTLR3_BASE_TREE lastArraySuffixNode = NULL;
int counter = 0;

pANTLR3_BASE_TREE rebuildTree(pANTLR3_BASE_TREE tree) {
    if (tree == NULL) {
        return NULL;
    }

    char* currentNode = (char*)tree->toString(tree)->chars;
    unsigned int childCount = tree->getChildCount(tree);

    for (unsigned int i = 0; i < childCount; ++i) {
        pANTLR3_BASE_TREE child = (pANTLR3_BASE_TREE)tree->getChild(tree, i);
        char* childNode = (char*)child->toString(child)->chars;
        printf("%s\n", childNode);
        if (strcmp(currentNode, "Body") == 0) {
            sourceNode = tree;
        }
        if (strcmp(childNode, "ArrayTokenSuffix") == 0 && currTreeOn) {
            if (lastArraySuffixNode == NULL) {
                currNode->addChild(currNode, child);
            } else {
                lastArraySuffixNode->addChild(lastArraySuffixNode, child);
            }
            lastArraySuffixNode = child;
            tree->deleteChild(tree, i);
            --i;
            --childCount;
            continue;
        }
        if (strcmp(childNode, "ArrayToken") == 0 && !currTreeOn) {
            currNode = child;
            currTreeOn = true;
            lastArraySuffixNode = NULL;
            counter = 2;
        } else if (strcmp(childNode, "ArrayToken") != 0 && currTreeOn) {
            counter--;
        } else if (strcmp(childNode, "ArrayToken") != 0 && counter <= 0) {
            counter = 0;
            currTreeOn = false;
        }
        rebuildTree(child);
    }

    return tree;
}


void drawTree(const pGrammarParser parser, pANTLR3_BASE_TREE tree) {
    rebuildTree(tree);
    pANTLR3_STRING output = parser->adaptor->makeDot(parser->adaptor, tree);
    FILE *file = fopen("tree.dot", "w");
    if (file == NULL) {
        perror("Could not open file for writing");
        return;
    }
    fprintf(file, "%s", output->chars);
    fclose(file);

    const char *generateTree = "dot -Tpng tree.dot -o output.png";
    int ret = system(generateTree);
    if (ret != 0) {
        perror("Error generating PNG");
    }
}

void makeTree(char *content, char *filename) {
    const pANTLR3_UINT8 input_string = (pANTLR3_UINT8) content;

    const pANTLR3_INPUT_STREAM input = antlr3StringStreamNew(input_string, ANTLR3_ENC_8BIT,
        strlen(input_string), (pANTLR3_UINT8)"parser");
    const pGrammarLexer lex = GrammarLexerNew(input);
    const pANTLR3_COMMON_TOKEN_STREAM tokens = antlr3CommonTokenStreamSourceNew(ANTLR3_SIZE_HINT, TOKENSOURCE(lex));
    const pGrammarParser parser = GrammarParserNew(tokens);
    const GrammarParser_source_return r = parser->source(parser);
    if (parser->pParser->rec->state->errorCount > 0) {
        parser->free(parser);
        tokens->free(tokens);
        lex->free(lex);
        input->close(input);
        return;
    }

    pANTLR3_BASE_TREE ast = r.tree;
    drawTree(parser, ast);
    printf("end");
    parser->free(parser);
    tokens->free(tokens);
    lex->free(lex);
    input->close(input);
}