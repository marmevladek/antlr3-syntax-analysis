#include <stdio.h>
#include <antlr3.h>

#include "graphStructure.h"

programGraph *globalGraph;
int nodeId = 0;

cfgNode* convertToCfgNode(pANTLR3_BASE_TREE tree) {
    cfgNode *node = malloc(sizeof(cfgNode));
    node->id = nodeId++;
    node->astNode = tree;
    node->name = (char *)tree->toString(tree)->chars;
    node->isTraversed = false;
    node->nextNodes = NULL;
    return node;
}

funcNode* buildFunc(pANTLR3_BASE_TREE tree, char* nodeName) {
    struct funcNode *func = malloc(sizeof(struct funcNode));
    func->identifier = nodeName;
    func->entryNode = convertToCfgNode(tree);
    func->cfgNodes = NULL;
    return func;
}

context* createContext(cfgNode *startNode, char* nodeName) {
    context *ctx = (context*)malloc(sizeof(context));
    ctx->currentNode = startNode;
    ctx->currentToken = nodeName;
    ctx->parent = NULL;
    ctx->insideLoop = (strcmp(nodeName, "LoopToken") == 0);
    ctx->insideCond = (strcmp(nodeName, "CondToken") == 0);
    return ctx;
}

void updateContext(context *ctx, cfgNode *newNode, char* nodeName) {
    ctx->currentNode = newNode;
    ctx->currentToken = nodeName;
    ctx->insideLoop = (strcmp(nodeName, "LoopToken") == 0) || ctx->insideLoop;
    ctx->insideCond = (strcmp(nodeName, "CondToken") == 0) || ctx->insideCond;
}

void processTree(pANTLR3_BASE_TREE tree) {
    funcNode* currentFunc;
    context *ctx = NULL;
    unsigned int childCount = tree->getChildCount(tree);
    for (unsigned int i = 0; i < childCount; ++i) {
        pANTLR3_BASE_TREE child = tree->getChild(tree, i);
        char* currentNode = (char*)tree->toString(tree)->chars;
        if (strcmp(currentNode, "FuncDefToken") == 0) {
            currentFunc = buildFunc(tree, currentNode);
            ctx = createContext(currentFunc->entryNode, currentNode);
        } else if (strcmp(currentNode, "LoopToken") == 0) {
            cfgNode *loopNode = convertToCfgNode(child);
            context *loopCtx = createContext(loopNode, currentNode);
            loopCtx->parent = ctx;
            ctx = loopCtx;
            ctx = ctx->parent;
            free(loopCtx);
        } else if (strcmp(currentNode, "CondToken") == 0) {
            cfgNode *condNode = convertToCfgNode(child);
            updateContext(ctx, condNode, currentNode);
            condNode->nextNodes = malloc(2 * sizeof(cfgNode*));

            condNode->nextNodes[0] = NULL;
            condNode->nextNodes[1] = NULL;

            // TODO: обработать пропуски then+экспрешионв
        }
    }
    if (ctx) {
        free(ctx);
    }
}
