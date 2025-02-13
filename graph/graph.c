#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <antlr3.h>

#include "graphStructures.h"

#include "misc.h"

int nodeId = 0;

static struct cfgNode **allNodes = NULL;
static int allNodesCount = 0;

struct breakStack {
    struct cfgNode *breakTarget;
    struct breakStack *next;
};

static char* safeStrdup(const char *s) {
    return s ? strdup(s) : NULL;
}

static void addNodeToGlobalList(struct cfgNode *node) {
    struct cfgNode **tmp = realloc(allNodes, sizeof(struct cfgNode*)*(allNodesCount+1));
    if (!tmp) {
        fprintf(stderr,"Error: realloc for allNodes failed\n");
        exit(1);
    }
    allNodes = tmp;
    allNodes[allNodesCount++] = node;
}

static bool isNodeInList(struct cfgNode *candidate) {
    if (!candidate) return false;
    for (int i=0; i<allNodesCount; i++) {
        if (allNodes[i] == candidate) {
            return true;
        }
    }
    return false;
}


struct cfgNode* createCfgNode(pANTLR3_BASE_TREE tree) {
    struct cfgNode *node = calloc(1, sizeof(struct cfgNode));
    if (!node) {
        fprintf(stderr,"Error: Allocation for cfgNode failed\n");
        return NULL;
    }
    node->id = nodeId++;
    node->ast = (struct astNode*)tree;
    node->isProcessed = false; // Инициализируем как необработанный
    addNodeToGlobalList(node);
    return node;
}

/*
    WIP
*/
struct funcNode* processFunction(pANTLR3_BASE_TREE funcTree, char* name) {
    struct funcNode *f = calloc(1, sizeof(struct funcNode));
    f->identifier = safeStrdup(name? name : "unknown_func");

    f->cfgEntry = createCfgNode(funcTree);
    f->cfgEntry->name = safeStrdup(f->identifier);

    f->cfgExit = createCfgNode(NULL);
    f->cfgExit->name = safeStrdup("func_exit");

    return f;
}

static void pushBreak(struct context *ctx, struct cfgNode *breakTarget) {
    struct breakStack *bs = malloc(sizeof(struct breakStack));
    bs->breakTarget = breakTarget;
    bs->next = ctx->breakStack;
    ctx->breakStack = bs;
}
static struct cfgNode* topBreak(struct context *ctx) {
    return ctx->breakStack? ctx->breakStack->breakTarget : NULL;
}
static void popBreak(struct context *ctx) {
    if (ctx->breakStack) {
        struct breakStack* tmp = ctx->breakStack;
        ctx->breakStack = tmp->next;
        free(tmp);
    }
}

static bool isControlNode(const char* n) {
    if (!n) return true;
    if (!strcmp(n,"IfStatement"))  return true;
    if (!strcmp(n,"WhileStatement"))  return true;
    if (!strcmp(n,"BreakStatement")) return true;
    if (!strcmp(n,"VarDeclToken")) return true;
    if (!strcmp(n, "DoStatement")) return true;
    return false;
}

static void getExpressionString(pANTLR3_BASE_TREE tree, char* buf, size_t sz) {
    if (!tree || sz<2) return;
    pANTLR3_STRING s = tree->toString(tree);
    const char* nm = (s&&s->chars)? s->chars:"";
    unsigned cc = tree->getChildCount(tree);

    if (isControlNode(nm)) {
        for (unsigned i=0; i<cc; i++) {
            getExpressionString(tree->getChild(tree,i), buf, sz);
        }
        return;
    }

    if (cc==0) {
        if (strlen(buf)+strlen(nm)+2 < sz) {
            strcat(buf,nm);
        }
    } 
    else if (cc==2 && (
             !strcmp(nm,"==")||!strcmp(nm,"!=")||
             !strcmp(nm,"+") ||!strcmp(nm,"-")  ||
             !strcmp(nm,"*") ||!strcmp(nm,"/")  ||
             !strcmp(nm,"%")))
    {
        strcat(buf,"(");
        getExpressionString(tree->getChild(tree,0), buf, sz);
        strcat(buf,nm);
        getExpressionString(tree->getChild(tree,1), buf, sz);
        strcat(buf,")");
    } else {
        for (unsigned i=0; i<cc; i++) {
            getExpressionString(tree->getChild(tree,i), buf, sz);
        }
    }
}

// ------------------------------------------------------------------------
// processTreeNode + обработчики if/while/break/return
// ------------------------------------------------------------------------

static void processTreeNode(pANTLR3_BASE_TREE tree, struct context *ctx);

static void processConditional(pANTLR3_BASE_TREE ifTree, struct context *ctx) {
    struct cfgNode *ifNode = createCfgNode(ifTree);

    char exprBuf[256];
    exprBuf[0] = '\0';
    unsigned cc = ifTree->getChildCount(ifTree);
    pANTLR3_BASE_TREE ifExprChild = NULL;
    pANTLR3_BASE_TREE ifBodyChild  = NULL;
    pANTLR3_BASE_TREE elseBodyChild  = NULL;
    if (cc > 2) ifExprChild = ifTree->getChild(ifTree, 2);
    if (cc > 4) ifBodyChild = ifTree->getChild(ifTree, 4);
    if (cc > 6) elseBodyChild = ifTree->getChild(ifTree, 6);
    if (ifExprChild) {
        getExpressionString(ifExprChild, exprBuf, sizeof(exprBuf));
    }
    if (exprBuf[0]) {
        char tmp[300];
        snprintf(tmp, sizeof(tmp), "if (%s)", exprBuf);
        ifNode->name = safeStrdup(tmp);
    } else {
        ifNode->name = safeStrdup("if (?)");
    }
    if (ctx->curr) {
        ctx->curr->defaultBranch = ifNode;
    }
    ctx->curr = ifNode;

    struct cfgNode* endIf = createCfgNode(NULL);
    endIf->name = safeStrdup("endif");
    
    if (ifBodyChild) {
        ifNode->conditionalBranch = NULL;
        struct cfgNode *bodyStart = createCfgNode(ifBodyChild);
        bodyStart->name = safeStrdup("if_block");
        ifNode->conditionalBranch = bodyStart;
        ctx->curr = bodyStart;
        processTreeNode(ifBodyChild, ctx);
        if (ctx->curr && ctx->curr->defaultBranch==NULL) {
            ctx->curr->defaultBranch = endIf;
        }
    }


    if (elseBodyChild) {
        struct cfgNode* elseStart = createCfgNode(elseBodyChild);
        elseStart->name = safeStrdup("else_block");
        ifNode->defaultBranch = elseStart;
        ctx->curr = elseStart;
        unsigned eCount = elseBodyChild->getChildCount(elseBodyChild);
        for (unsigned i = 0; i < eCount; i++) {
            pANTLR3_BASE_TREE ch = elseBodyChild->getChild(elseBodyChild, i);
            char* childNodeStr = (char*)ch->toString(ch)->chars;
        }

        processTreeNode(elseBodyChild, ctx);

        if (ctx->curr && ctx->curr->defaultBranch==NULL) {
            ctx->curr->defaultBranch = endIf;
        }
    } else {
        if (!ifNode->defaultBranch) {
            ifNode->defaultBranch = endIf;
        }
    }
    ctx->curr = endIf;
}

struct cfgNode* findCfgNodeByAST(pANTLR3_BASE_TREE tree) {
    for (int i = 0; i < allNodesCount; i++) {
        if (allNodes[i]->ast == (struct astNode*)tree) {
            return allNodes[i];
        }
    }
    return NULL;
}


static void processWhile(pANTLR3_BASE_TREE loopTree, struct context *ctx) {
    struct cfgNode *loopNode = createCfgNode(loopTree);

    char exprBuf[256];
    exprBuf[0] = '\0';

    unsigned cc = loopTree->getChildCount(loopTree);
    pANTLR3_BASE_TREE exprChild=NULL, bodyChild=NULL;
    exprChild = loopTree->getChild(loopTree, 2);
    bodyChild = loopTree->getChild(loopTree, 4);

    if (exprChild) {
        getExpressionString(exprChild, exprBuf, sizeof(exprBuf));
    }
    if (exprBuf[0]) {
        char tmp[300];
        snprintf(tmp,sizeof(tmp),"while (%s)", exprBuf);
        loopNode->name = safeStrdup(tmp);
    } else {
        loopNode->name = safeStrdup("while (?)");
    }

    if (ctx->curr) {
        ctx->curr->defaultBranch = loopNode;
    }
    ctx->curr = loopNode;

    struct cfgNode *loopExit = createCfgNode(NULL);
    loopExit->name = safeStrdup("exit");
    pushBreak(ctx, loopExit);

    if (bodyChild) {
        loopNode->conditionalBranch = NULL;
        struct cfgNode *bodyStart = createCfgNode(bodyChild);
        bodyStart->name = safeStrdup("body");
        loopNode->conditionalBranch = bodyStart;
        ctx->curr = bodyStart;
        processTreeNode(bodyChild, ctx);
        if (ctx->curr && ctx->curr->defaultBranch==NULL) {
            ctx->curr->defaultBranch = loopNode;
        }
    }
    loopNode->defaultBranch = loopExit;
    ctx->curr = loopExit;

    popBreak(ctx);
}

static void processBreak(pANTLR3_BASE_TREE bkTree, struct context *ctx) {
    struct cfgNode *bkNode = createCfgNode(bkTree);
    bkNode->name = safeStrdup("break");
    if (ctx->curr) {
        ctx->curr->defaultBranch = bkNode;
    }
    struct cfgNode* exitNode = topBreak(ctx);
    if (exitNode) {
        bkNode->defaultBranch = exitNode;
    } else {
        fprintf(stderr,"Warning: break outside loop\n");
    }
    ctx->curr = bkNode;
}

static void processReturn(pANTLR3_BASE_TREE retTree, struct context *ctx) {
    struct cfgNode* r = createCfgNode(retTree);
    char expr[256]; expr[0]='\0';

    if (retTree->getChildCount(retTree)>0) {
        getExpressionString(retTree->getChild(retTree,0), expr,sizeof(expr));
    }
    if (expr[0]) {
        char tmp[300];
        snprintf(tmp,sizeof(tmp),"return (%s)",expr);
        r->name = safeStrdup(tmp);
    } else {
        r->name = safeStrdup("return");
    }
    if (ctx->curr) {
        ctx->curr->defaultBranch = r;
    }
    ctx->curr = r;
}

static void processDo(pANTLR3_BASE_TREE doTree, struct context *ctx) {
    struct cfgNode *loopNode = createCfgNode(doTree);
    loopNode->name = safeStrdup("do-while");

    if (ctx->curr) {
        ctx->curr->defaultBranch = loopNode;
    }
    ctx->curr = loopNode;

    struct cfgNode *loopExit = createCfgNode(NULL);
    loopExit->name = safeStrdup("exit");
    pushBreak(ctx, loopExit);

    unsigned cc = doTree->getChildCount(doTree);
    pANTLR3_BASE_TREE bodyChild = NULL, exprChild = NULL;
    bodyChild = doTree->getChild(doTree, 1);
    exprChild = doTree->getChild(doTree, 4);

    struct cfgNode *bodyStart = NULL;
    char exprBuf[256] = {0};


    if (bodyChild) {
        bodyStart = createCfgNode(bodyChild);
        bodyStart->name = safeStrdup("body");
        loopNode->defaultBranch = bodyStart;
        ctx->curr = bodyStart;
        processTreeNode(bodyChild, ctx);
    }

    struct cfgNode *condNode = createCfgNode(exprChild);
    
    if (exprChild) {
        getExpressionString(exprChild, exprBuf, sizeof(exprBuf));
    }
    if (exprBuf[0]) {
        char tmp[300];
        snprintf(tmp, sizeof(tmp), "while (%s)", exprBuf);
        condNode->name = safeStrdup(tmp);
    } else {
        condNode->name = safeStrdup("while (?)");
    }

    if (ctx->curr && ctx->curr->defaultBranch == NULL) {
        ctx->curr->defaultBranch = condNode;
    }
    condNode->defaultBranch = loopNode;
    condNode->conditionalBranch = loopExit;

    ctx->curr = loopExit;
    popBreak(ctx);
}

static void checkBinOp(const char* n) {
    if (!strcmp(n, "BinOpAss")) n = "=";
    if (!strcmp(n, "BinOpBinPlus")) n = "+";
    if (!strcmp(n, "BinOpBinAssSum")) n = "-";
}

static bool isTrivialNode(const char* n) {
    if (!n) return true;
    if (!strcmp(n,"Identifier"))    return true;
    if (!strcmp(n,"Literal"))       return true;
    if (!strcmp(n,"Builtin"))       return true;
    if (!strcmp(n,"TypeRef"))  return true;
    if (!strcmp(n,"VarDeclToken"))  return true;
    if (!strcmp(n,"ArrayToken"))    return true;
    if (!strcmp(n,"ArrayTokenSuffix")) return true;
    if (!strcmp(n,"FuncSignature")) return true;
    if (!strcmp(n,"ArgDef"))  return true;
    if (!strcmp(n,"Statement"))         return true;
    if (!strcmp(n, "FuncDef")) return true;
    if (!strcmp(n, "Block")) return true;
    if (!strcmp(n, "Var")) return true;
    if (!strcmp(n, "List")) return true;
    if (!strcmp(n, "IdItem")) return true;
    if (!strcmp(n, "Expr")) return true;
    if (!strcmp(n, "Dec")) return true;
    if (!strcmp(n, "Binary")) return true;
    if (!strcmp(n, "Place")) return true;
    if (!strcmp(n, "Expression")) return true;
    if (!strcmp(n, "BoolStatement")) return true;
    if (!strcmp(n, "CharStatement")) return true;
    if (!strcmp(n, "(")) return true;
    if (!strcmp(n, ")")) return true;
    if (!strcmp(n, ";")) return true;
    if (!strcmp(n, "{")) return true;
    if (!strcmp(n, "}")) return true;
    if (!strcmp(n, ",")) return true;
    return false;
}

static void processGenericNode(pANTLR3_BASE_TREE tree, struct context *ctx) {
    if (!tree) return;
    pANTLR3_STRING s = tree->toString(tree);
    const char* nm = (s && s->chars) ? s->chars : "";

    if (isTrivialNode(nm)) {
        unsigned cc = tree->getChildCount(tree);
        for (unsigned i=0; i<cc; i++) {
            processTreeNode(tree->getChild(tree,i), ctx);
        }
        return;
    }
    struct cfgNode* g = createCfgNode(tree);
    g->name = safeStrdup(nm);

    if (ctx->curr) {
        ctx->curr->defaultBranch = g;
    }
    ctx->curr = g;

    unsigned cc=tree->getChildCount(tree);
    for (unsigned i=0;i<cc;i++) {
        pANTLR3_BASE_TREE ch=tree->getChild(tree,i);
        processTreeNode(ch, ctx);
    }
}

static void processTreeNode(pANTLR3_BASE_TREE tree, struct context *ctx) {
    if (!tree) return;

    pANTLR3_STRING s = tree->toString(tree);
    const char* nm = (s && s->chars) ? s->chars : "";

    // if (!strcmp(nm, "Block")) {
    //     processBlock(tree, ctx);
    // }
    if (!strcmp(nm, "IfStatement")) {
        processConditional(tree, ctx);
    }
    else if (!strcmp(nm, "WhileStatement")) {
        processWhile(tree, ctx);
    }
    else if (!strcmp(nm, "BreakStatement")) {
        processBreak(tree, ctx);
    }
    else if (!strcmp(nm, "ReturnToken")) {
        processReturn(tree, ctx);
    }
    else if (!strcmp(nm, "DoStatement")) {
        processDo(tree, ctx);
    }
    else {
        processGenericNode(tree, ctx);
    }
}


static void resetTraversal(struct cfgNode *node) {
    if (!node) return;
    if (node->isTraversed) return;
    node->isTraversed=true;

    if (node->conditionalBranch && !isNodeInList(node->conditionalBranch)) {
        node->conditionalBranch=NULL;
    }
    if (node->defaultBranch && !isNodeInList(node->defaultBranch)) {
        node->defaultBranch=NULL;
    }
    if (node->conditionalBranch) resetTraversal(node->conditionalBranch);
    if (node->defaultBranch) resetTraversal(node->defaultBranch);

    node->isTraversed=false;
}

static void printCFGNode(FILE *f, struct cfgNode *node) {
    if (!node||node->isTraversed) return;
    node->isTraversed=true;

    fprintf(f,"    Node%d [label=\"%s\"];\n",node->id,node->name?node->name:"");

    if (node->conditionalBranch && !isNodeInList(node->conditionalBranch)) {
        node->conditionalBranch=NULL;
    }
    if (node->defaultBranch && !isNodeInList(node->defaultBranch)) {
        node->defaultBranch=NULL;
    }

    bool isIf=false,isWhile=false;
    if (node->name) {
        if (!strncmp(node->name,"if ((",4))   isIf=true;
        else if (!strncmp(node->name,"while ((",7)) isWhile=true;
    }

    if (node->conditionalBranch) {
        const char* lab = isIf? "then": (isWhile? "true":"");
        fprintf(f,"    Node%d -> Node%d [label=\"%s\"];\n",
                node->id,node->conditionalBranch->id,lab);
        printCFGNode(f,node->conditionalBranch);
    }
    if (node->defaultBranch) {
        const char* lab = isIf? "else": (isWhile? "false":"");
        fprintf(f,"    Node%d -> Node%d [label=\"%s\"];\n",
                node->id,node->defaultBranch->id,lab);
        printCFGNode(f,node->defaultBranch);
    }
}

static void drawCFG(struct programGraph *graph) {
    if (!graph) return;
    for (int i=0; i<graph->funcCount; i++) {
        struct funcNode *fn = graph->functions[i];
        if (!fn || !fn->cfgEntry) continue;

        char fname[256];
        snprintf(fname,sizeof(fname),"%s.dot", fn->identifier);
        FILE *fp = fopen(fname,"w");
        if (!fp) {
            fprintf(stderr,"Error: cannot open %s\n",fname);
            continue;
        }
        resetTraversal(fn->cfgEntry);

        fprintf(fp,"digraph CFG {\n");
        fprintf(fp,"    node [shape=box];\n");
        printCFGNode(fp, fn->cfgEntry);
        fprintf(fp,"}\n");
        fclose(fp);

        char cmd[512];
        snprintf(cmd,sizeof(cmd),"dot -Tpng %s -o %s.png", fname, fn->identifier);
        system(cmd);
    }
}

void processTree(pANTLR3_BASE_TREE tree) {
    struct programGraph *graph = calloc(1, sizeof(struct programGraph));
    unsigned topCount = tree->getChildCount(tree);

    static unsigned funcCounter = 0;

    for (unsigned i = 0; i < topCount; i++) {
        pANTLR3_BASE_TREE child = tree->getChild(tree, i);
        pANTLR3_STRING s = child->toString(child);
        const char *nm = (s && s->chars) ? s->chars : "";
        if (!strcmp(nm, "SourceItem")) {
            char funcName[32];
            snprintf(funcName, sizeof(funcName), "func_%u", funcCounter++);

            struct funcNode *func = processFunction(child, funcName);
            graph->functions = realloc(graph->functions,
                sizeof(struct funcNode *) * (graph->funcCount + 1));
            graph->functions[graph->funcCount++] = func;

            struct context ctx;
            memset(&ctx, 0, sizeof(ctx));
            ctx.curr = func->cfgEntry;
            ctx.entryNode = func->cfgEntry;
            ctx.exitNode = func->cfgExit;
            ctx.loopDepth = 0;
            ctx.breakStack = NULL;
            ctx.function = func;

            unsigned c2 = child->getChildCount(child);
            for (unsigned j = 0; j < c2; j++) {
                pANTLR3_BASE_TREE st = child->getChild(child, j);
                processTreeNode(st, &ctx);
            }

            if (ctx.curr && ctx.curr != func->cfgExit && ctx.curr->defaultBranch == NULL) {
                ctx.curr->defaultBranch = func->cfgExit;
            }
        }
    }

    drawCFG(graph);
}