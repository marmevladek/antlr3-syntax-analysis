#ifndef GRAPHSTRUCTURES_H
#define GRAPHSTRUCTURES_H

#include <stdbool.h>
#include <stdlib.h>
#include <antlr3.h>


typedef struct cfgNode {
    int id;
    pANTLR3_BASE_TREE astNode;
    char *name;
    bool isTraversed;

    struct cfgNode **nextNodes;
} cfgNode;

typedef struct funcNode {
    char *identifier;
    cfgNode *entryNode;
    cfgNode **cfgNodes;
    int nodeCount;
} funcNode;

typedef struct programGraph {
    funcNode **functions;
    int funcCount;
} programGraph;

typedef struct Context {
    cfgNode *currentNode;
    char* currentToken;
    struct Context *parent;
    bool insideLoop;
    bool insideCond;
} context;

typedef struct commonNodes {
    cfgNode **cfgNodes;
} commonNodes;
#endif