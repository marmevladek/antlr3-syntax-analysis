#ifndef GRAPH_STRUCTURES_H
#define GRAPH_STRUCTURES_H

#include <stdbool.h>


struct astNode;

struct cfgNode {
    int id;
    char* name;
    struct astNode* ast;
    struct cfgNode* conditionalBranch;
    struct cfgNode* defaultBranch;
    bool isTraversed;
    bool isProcessed;
    struct astNode* parseTree;
};

struct funcNode {
    char *identifier;
    struct astNode *signature;
    struct astNode *body;
    struct cfgNode *cfgEntry;
    struct cfgNode *cfgExit;
    char *sourceFileName;

    int callNum;
    char **callNames;
    struct funcNode **calls;
};

struct context {
    struct cfgNode *curr;
    struct cfgNode *entryNode;
    struct cfgNode *exitNode;

    int loopDepth;
    struct breakStack *breakStack;
    struct funcNode *function;
};

struct programGraph {
    struct funcNode **functions;
    int funcCount;
};
#endif