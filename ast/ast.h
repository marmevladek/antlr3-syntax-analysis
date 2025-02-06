#include <string.h>

#include "../grammar/GrammarLexer.h"
#include "../grammar/GrammarParser.h"
#include <antlr3treeparser.h>
#include <antlr3.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>

void makeTree(char *content, char *filename);