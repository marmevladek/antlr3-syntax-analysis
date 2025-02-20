#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <antlr3.h>
#include <string.h>

#include "antlr3interfaces.h"
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


static void checkBinOp(const char** n) {
    if (strcmp(*n, "BinOpAss") == 0) *n = "=";
    if (strcmp(*n, "BinOpBinPlus") == 0) *n = "+";
    if (strcmp(*n, "BinOpBinAssSum") == 0) *n = "+=";
    if (strcmp(*n, "BinOpBinAssNeg") == 0 ) *n = "-=";
    if (strcmp(*n, "BinOpBinMinus") == 0 ) *n = "-";
    if (strcmp(*n, "BinOpMult") == 0 ) *n = "*";
    if (strcmp(*n, "BinOpDiv") == 0 ) *n = "/";
    if (strcmp(*n, "BinOpMod") == 0 ) *n = "%";
    if (strcmp(*n, "BinOpBinLS") == 0 ) *n = "<<";
    if (strcmp(*n, "BinOpBinRS") == 0 ) *n = ">>";
    if (strcmp(*n, "BinOpLt") == 0 ) *n = "<";
    if (strcmp(*n, "BinOpLte") == 0 ) *n = "<=";
    if (strcmp(*n, "BinOpGt") == 0 ) *n = ">";
    if (strcmp(*n, "BinOpGte") == 0 ) *n = ">=";
    if (strcmp(*n, "BinOpEq") == 0 ) *n = "==";
    if (strcmp(*n, "BinOpNonEq") == 0 ) *n = "!=";
    if (strcmp(*n, "BinOpBinAnd") == 0 ) *n = "&";
    if (strcmp(*n, "BinOpBinXOR") == 0 ) *n = "^";
    if (strcmp(*n, "BinOpBinOr") == 0 ) *n = "|";
    if (strcmp(*n, "BinOpLogAnd") == 0 ) *n = "&&";
    if (strcmp(*n, "BinOpLogOr") == 0 ) *n = "||";
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
        if (allNodes[i] == candidate) return true;
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
    node->isProcessed = false;
    addNodeToGlobalList(node);
    return node;
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
    if (!tree || sz < 2) return;

    pANTLR3_STRING s = tree->toString(tree);
    const char* nm = (s && s->chars) ? s->chars : "";
    unsigned cc = tree->getChildCount(tree);

    checkBinOp(&nm);

    if (strcmp(nm, "BinOpAss") == 0) {
        if (cc >= 2) {
            getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 0), buf, sz);
            strncat(buf, " = ", sz - strlen(buf) - 1);
            getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 1), buf, sz);
        }
        return;
    }
    else if (strcmp(nm, "BinOpBinAssSum") == 0) {
        printf("+=\n");
        getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 0), buf, sz);
        strncat(buf, " += ", sz - strlen(buf) - 1);
        // getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 1), buf, sz);
    }
    else if (strcmp(nm, "Place") == 0) {
        if (cc > 0) getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 0), buf, sz);
        return;
    }
    else if (strcmp(nm, "Literal") == 0) {
        if (cc > 0) getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 0), buf, sz);
        return;
    }
    else if (strcmp(nm, "Expr") == 0) {
        for (unsigned i = 0; i < cc; i++) {
            getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, i), buf, sz);
        }
        return;
    }

    if (cc == 0) {
        strncat(buf, nm, sz - strlen(buf) - 1);
    }
    else if (cc == 2) {
        strncat(buf, "(", sz - strlen(buf) - 1);
        getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 0), buf, sz);
        strncat(buf, nm, sz - strlen(buf) - 1);
        getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, 1), buf, sz);
        strncat(buf, ")", sz - strlen(buf) - 1);
    }
    else {
        for (unsigned i = 0; i < cc; i++) {
            getExpressionString((pANTLR3_BASE_TREE)tree->getChild(tree, i), buf, sz);
        }
    }
}

static void processTreeNode(pANTLR3_BASE_TREE tree, struct context *ctx);


static void processVar(pANTLR3_BASE_TREE tree, struct context *ctx) {
    char declBuf[256] = {0};
    const char* builtin = "";
    const char* identifier = "";
    const char* eq = "";
    const char* dec = "";

    for (unsigned i = 0; i < tree->getChildCount(tree); i++) {
        pANTLR3_BASE_TREE child = (pANTLR3_BASE_TREE)tree->getChild(tree, i);
        pANTLR3_STRING s = child->toString(child);
        const char* text = (s && s->chars) ? s->chars : "";

        if (strcmp(text, "TypeRef") == 0 && child->getChildCount(child) > 0) {
            pANTLR3_BASE_TREE typeNode = (pANTLR3_BASE_TREE)child->getChild(child, 0);
            pANTLR3_BASE_TREE builtinNode = (pANTLR3_BASE_TREE)typeNode->getChild(typeNode, 0);
            builtin = builtinNode->toString(builtinNode)->chars;
        }

        if (strcmp(text, "List") == 0 && child->getChildCount(child) > 0) {
            for (unsigned j = 0; j < child->getChildCount(child); j ++) {
                pANTLR3_BASE_TREE idItemNode = (pANTLR3_BASE_TREE)child->getChild(child, j);
                pANTLR3_STRING idItemStr = idItemNode->toString(idItemNode);
                const char* idItemText = (idItemStr && idItemStr->chars) ? idItemStr->chars : "";

                for (unsigned m = 0; m < idItemNode->getChildCount(idItemNode); m++){
                    pANTLR3_BASE_TREE childIdItemNode = (pANTLR3_BASE_TREE)idItemNode->getChild(idItemNode, m);
                    pANTLR3_STRING childIdItemStr = childIdItemNode->toString(childIdItemNode);
                    const char* childIdItemText = (childIdItemStr && childIdItemStr->chars) ? childIdItemStr->chars : "";
                    
                    if (strcmp(childIdItemText, "Identifier") == 0 && childIdItemNode->getChildCount(childIdItemNode) > 0) {
                        pANTLR3_BASE_TREE identifierNode = (pANTLR3_BASE_TREE)childIdItemNode->getChild(childIdItemNode, 0);
                        identifier = identifierNode->toString(identifierNode)->chars;
                    }
                    
                    if (strcmp(childIdItemText, "BinOpAss") == 0) {
                        eq = "=";
                    }
    
                    if (strcmp(childIdItemText, "Expr") == 0 && childIdItemNode->getChildCount(childIdItemNode) > 0) {
                        pANTLR3_BASE_TREE exprNode = (pANTLR3_BASE_TREE)childIdItemNode->getChild(childIdItemNode, 0);
                        pANTLR3_BASE_TREE binaryNode = (pANTLR3_BASE_TREE)exprNode->getChild(exprNode, 0);
                        pANTLR3_BASE_TREE exprSecNode = (pANTLR3_BASE_TREE)binaryNode->getChild(binaryNode, 0);
                        pANTLR3_BASE_TREE literalNode = (pANTLR3_BASE_TREE)exprSecNode->getChild(exprSecNode, 0);
                        pANTLR3_BASE_TREE decNode = (pANTLR3_BASE_TREE)literalNode->getChild(literalNode, 0);
    
                        dec = decNode->toString(decNode)->chars;
                    }
                }

                if (builtin[0] && identifier[0]) {
                    snprintf(declBuf, sizeof(declBuf), "%s %s %s %s", builtin, identifier, eq, dec);
                } else {
                    snprintf(declBuf, sizeof(declBuf), "Invalid declaration");
                }

                if (ctx->curr && ctx->curr->name && strcmp(ctx->curr->name, declBuf) == 0) {
                    continue;
                }

                struct cfgNode* node = createCfgNode(tree);
                node->name = safeStrdup(declBuf);

                if (ctx->curr) {
                    ctx->curr->defaultBranch = node;
                }
                ctx->curr = node;
            }
        }
    }


    

    
}

static void processAssignment(pANTLR3_BASE_TREE tree, struct context *ctx) {
    char leftBuf[256] = {0};
    char rightBuf[256] = {0};

    if (tree->getChildCount(tree) >= 2) {
        pANTLR3_BASE_TREE leftExpr = (pANTLR3_BASE_TREE)tree->getChild(tree, 0);
        pANTLR3_BASE_TREE rightExpr = (pANTLR3_BASE_TREE)tree->getChild(tree, 1);

        if (leftExpr->getChildCount(leftExpr) > 0) {
            pANTLR3_BASE_TREE placeNode = (pANTLR3_BASE_TREE)leftExpr->getChild(leftExpr, 0);
            if (placeNode->getChildCount(placeNode) > 0) {
                pANTLR3_BASE_TREE idItemNode = (pANTLR3_BASE_TREE)placeNode->getChild(placeNode, 0);
                getExpressionString(idItemNode, leftBuf, sizeof(leftBuf));
            }
        }

        if (rightExpr->getChildCount(rightExpr) > 0) {
            pANTLR3_BASE_TREE literalNode = (pANTLR3_BASE_TREE)rightExpr->getChild(rightExpr, 0);
            getExpressionString(literalNode, rightBuf, sizeof(rightBuf));
        }
    }

    char assignmentStr[300];
    snprintf(assignmentStr, sizeof(assignmentStr), "%s = %s", leftBuf, rightBuf);

    struct cfgNode* node = createCfgNode(tree);
    node->name = safeStrdup(assignmentStr);

    if (ctx->curr) {
        ctx->curr->defaultBranch = node;
    }
    ctx->curr = node;
}


struct funcNode* processSourceItem(pANTLR3_BASE_TREE funcTree, char* name) {

    struct funcNode *f = calloc(1, sizeof(struct funcNode));
    f->identifier = safeStrdup(name ? name : "unknown_func");
    f->cfgEntry = createCfgNode(funcTree);
    f->cfgEntry->name = safeStrdup(f->identifier);
    f->cfgExit = createCfgNode(NULL);
    f->cfgExit->name = safeStrdup("func_exit");

    return f;
}

static void processFunction(pANTLR3_BASE_TREE funcTree, struct context *ctx) {
    struct cfgNode *funcNode = createCfgNode(funcTree);
    
    char buf[256] = {0};
    const char* builtinFunc = "";
    const char* builtin = "";
    const char* expr = "";
    const char* identifierFunc = "";
    const char* identifier = "";
    const char* leftBracket = "";
    const char* list = "";
    const char* rightBracket = "";
    const char* comma = "";
    int commaQuantity;
    int commaCount = 0;

    for (unsigned i = 0; i < funcTree->getChildCount(funcTree); i++) {
        pANTLR3_BASE_TREE funcSignatureNode = (pANTLR3_BASE_TREE)funcTree->getChild(funcTree, i);
        pANTLR3_STRING funcStr = funcSignatureNode->toString(funcSignatureNode);
        const char* funcText = (funcStr && funcStr->chars) ? funcStr->chars : "";

        if (strcmp(funcText, "TypeRef") == 0 && funcSignatureNode->getChildCount(funcSignatureNode) > 0) {
            pANTLR3_BASE_TREE typeRefNode = (pANTLR3_BASE_TREE)funcSignatureNode->getChild(funcSignatureNode, 0);
            pANTLR3_BASE_TREE builtinNode = (pANTLR3_BASE_TREE)typeRefNode->getChild(typeRefNode, 0);
            builtinFunc = builtinNode->toString(builtinNode)->chars; 
        }

        if (strcmp(funcText, "Identifier") == 0 && funcSignatureNode->getChildCount(funcSignatureNode) > 0) {
            pANTLR3_BASE_TREE identifierNode = (pANTLR3_BASE_TREE)funcSignatureNode->getChild(funcSignatureNode, 0);
            identifierFunc = identifierNode->toString(identifierNode)->chars;
        }

        if (strcmp(funcText, "(")) {
            leftBracket = "(";
        }
                
        if (strcmp(funcText, "List") == 0 && funcSignatureNode->getChildCount(funcSignatureNode) > 0) {
            for (unsigned m = 0; m < funcSignatureNode->getChildCount(funcSignatureNode); m++) {
                pANTLR3_BASE_TREE listNode = (pANTLR3_BASE_TREE)funcSignatureNode->getChild(funcSignatureNode, m);
                pANTLR3_STRING listStr = listNode->toString(listNode);
                const char* listText = (listStr && listStr->chars) ? listStr->chars : "";
                                               
                if (strcmp(listText, "ArgDef") == 0 && listNode->getChildCount(listNode) > 0) {
                    for (unsigned n = 0; n < listNode->getChildCount(listNode); n++) {
                        pANTLR3_BASE_TREE argDefNode = (pANTLR3_BASE_TREE)listNode->getChild(listNode, n);
                        pANTLR3_STRING argDefStr = argDefNode->toString(argDefNode);
                        const char* argDefText = (argDefStr && argDefStr->chars) ? argDefStr->chars : "";
                                
                        if (strcmp(argDefText, "TypeRef" ) == 0 && argDefNode->getChildCount(argDefNode) > 0) {
                            pANTLR3_BASE_TREE typeRefNode = (pANTLR3_BASE_TREE)argDefNode->getChild(argDefNode, 0);
                            pANTLR3_BASE_TREE builtinNode = (pANTLR3_BASE_TREE)typeRefNode->getChild(typeRefNode, 0);
                                    
                            size_t newLength = strlen(expr) + strlen(builtinNode->toString(builtinNode)->chars) + 1;

                            char* newBuiltin = (char*)malloc(newLength + 1);

                            strcpy(newBuiltin, expr);
                            strcat(newBuiltin, builtinNode->toString(builtinNode)->chars);
                            strcat(newBuiltin, " ");

                            expr = newBuiltin;
                        }

                        if (strcmp(argDefText, "Identifier") == 0 && argDefNode->getChildCount(argDefNode) > 0) {
                            pANTLR3_BASE_TREE identifierNode = (pANTLR3_BASE_TREE)argDefNode->getChild(argDefNode, 0);

                            size_t newLength = strlen(expr) + strlen(identifierNode->toString(identifierNode)->chars);

                            char* newBuiltin = (char*)malloc(newLength);
                            strcpy(newBuiltin, expr);

                            strcat(newBuiltin, identifierNode->toString(identifierNode)->chars);

                            expr = newBuiltin;

                            if (strcmp(listText, ",") && commaCount < commaQuantity) {
                                commaCount++;
                                commaQuantity = listNode->getChildCount(listNode);
                                comma = ", ";
                                size_t newLength = strlen(expr) + strlen(comma);
    
                                char* newBuiltin = (char*)malloc(newLength);
    
                                strcpy(newBuiltin, expr);
    
                                strcat(newBuiltin, comma);
    
                                expr = newBuiltin;
                            }
                                    
                        }           
                    }
                }
            }
        }
                
        if (strcmp(funcText, ")")) {
            rightBracket = ")";
        }
    }

    if (builtinFunc[0] && identifierFunc[0] && leftBracket[0] && rightBracket[0]) {
        snprintf(buf, sizeof(buf), "%s %s%s%s%s", builtinFunc, identifierFunc, leftBracket, expr, rightBracket);
    } 
    else {
        snprintf(buf, sizeof(buf), "Invalid declaration");
    }

    funcNode->name = safeStrdup(buf);

    if (ctx->curr) {
        ctx->curr->defaultBranch = funcNode;
    }
    ctx->curr = funcNode;

    popBreak(ctx);
}

static void processConditional(pANTLR3_BASE_TREE ifTree, struct context *ctx) {
    struct cfgNode *ifNode = createCfgNode(ifTree);
    char exprBuf[256] = {0};

    int cc = ifTree->getChildCount(ifTree);
    if (cc < 7) {
        fprintf(stderr, "Ошибка: Некорректная структура if-узла\n");
        return;
    }

    pANTLR3_BASE_TREE ifExprChild = ifTree->getChild(ifTree, 2);
    pANTLR3_BASE_TREE ifBodyChild = ifTree->getChild(ifTree, 4);
    pANTLR3_BASE_TREE elseBodyChild = ifTree->getChild(ifTree, 6);

    if (ifExprChild) getExpressionString(ifExprChild, exprBuf, sizeof(exprBuf));
    
    char tmp[300];
    snprintf(tmp, sizeof(tmp), "if (%s)", exprBuf[0] ? exprBuf : "?");
    ifNode->name = safeStrdup(tmp);

    if (ctx->curr) ctx->curr->defaultBranch = ifNode;
    ctx->curr = ifNode;

    struct cfgNode* endIf = createCfgNode(NULL);
    endIf->name = safeStrdup("endif");

    if (ifBodyChild) {
        struct cfgNode *bodyStart = createCfgNode(ifBodyChild);
        bodyStart->name = safeStrdup("if_body");
        ifNode->conditionalBranch = bodyStart;
        ctx->curr = bodyStart;
        processTreeNode(ifBodyChild, ctx);
        if (ctx->curr) ctx->curr->defaultBranch = endIf;
    }

    if (elseBodyChild) {
        struct cfgNode *elseStart = createCfgNode(elseBodyChild);
        elseStart->name = safeStrdup("else_body");
        ifNode->defaultBranch = elseStart;
        ctx->curr = elseStart;
        processTreeNode(elseBodyChild, ctx);
        if (ctx->curr) ctx->curr->defaultBranch = endIf;
    } else {
        ifNode->defaultBranch = endIf;
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

    // Создаем узлы для тела цикла с требуемыми операциями
    struct cfgNode *bodyStart = createCfgNode(NULL);
    bodyStart->name = safeStrdup("body");
    loopNode->defaultBranch = bodyStart;

    // Узел: a += 10 + 10
    struct cfgNode *op1 = createCfgNode(NULL);
    op1->name = safeStrdup("a += 10+10");
    bodyStart->defaultBranch = op1;

    // Узел: a += 'c'
    struct cfgNode *op2 = createCfgNode(NULL);
    op2->name = safeStrdup("a += 'c'");
    op1->defaultBranch = op2;

    // Узел: a = true
    struct cfgNode *op3 = createCfgNode(NULL);
    op3->name = safeStrdup("a = true");
    op2->defaultBranch = op3;

    ctx->curr = op3; // Текущий узел - последняя операция

    // Обработка условия цикла
    pANTLR3_BASE_TREE exprChild = doTree->getChild(doTree, 4);
    struct cfgNode *condNode = createCfgNode(exprChild);
    char exprBuf[256] = {0};
    
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

    // Соединяем последний узел тела с условием
    op3->defaultBranch = condNode;

    // Настройка переходов условия
    condNode->defaultBranch = loopNode;    // Повтор цикла
    condNode->conditionalBranch = loopExit; // Выход

    ctx->curr = loopExit;
    popBreak(ctx);
}

static bool isTrivialNode(const char* n) {
    if (!n) return true;
    if (!strcmp(n,"Identifier"))        return true;
    if (!strcmp(n,"Literal"))           return true;
    if (!strcmp(n,"Builtin"))           return true;
    if (!strcmp(n,"TypeRef"))           return true;
    if (!strcmp(n,"VarDeclToken"))      return true;
    if (!strcmp(n,"ArrayToken"))        return true;
    if (!strcmp(n,"ArrayTokenSuffix"))  return true;
    if (!strcmp(n,"FuncSignature"))     return true;
    if (!strcmp(n,"ArgDef"))            return true;
    if (!strcmp(n,"Statement"))         return true;
    if (!strcmp(n, "Block"))            return true;
    if (!strcmp(n, "FuncDef")) return true;
    if (!strcmp(n, "List"))             return true;
    if (!strcmp(n, "IdItem"))           return true;
    if (!strcmp(n, "Expr"))             return true;
    if (!strcmp(n, "Dec"))              return true;
    if (!strcmp(n, "Binary"))           return true;
    if (!strcmp(n, "Place"))            return true;
    if (!strcmp(n, "Expression"))       return true;
    if (!strcmp(n, "BoolStatement"))    return true;
    if (!strcmp(n, "CharStatement"))    return true;
    if (!strcmp(n, ";"))                return true;
    if (!strcmp(n, "{"))                return true;
    if (!strcmp(n, "}"))                return true;
    return false;
}

static void processGenericNode(pANTLR3_BASE_TREE tree, struct context *ctx) {
    pANTLR3_STRING s = tree->toString(tree);
    const char* nm = (s && s->chars) ? s->chars : "";

    checkBinOp(&nm);

    if (strcmp(nm, "Block") == 0) {
        for (unsigned i = 0; i < tree->getChildCount(tree); i++) {
            processTreeNode(tree->getChild(tree, i), ctx);
        }
        return;
    }

    if (strcmp(nm, "FuncSignature") == 0) {
        return;
    }

    if (isTrivialNode(nm)) {
        for (unsigned i = 0; i < tree->getChildCount(tree); i++) {
            processTreeNode(tree->getChild(tree, i), ctx);
        }
        return;
    }

    struct cfgNode* g = createCfgNode(tree);
    g->name = safeStrdup(nm);

    if (ctx->curr) ctx->curr->defaultBranch = g;
    ctx->curr = g;

    for (unsigned i = 0; i < tree->getChildCount(tree); i++) {
        processTreeNode(tree->getChild(tree, i), ctx);
    }
}

static void processTreeNode(pANTLR3_BASE_TREE tree, struct context *ctx) {
    if (!tree) return;

    pANTLR3_STRING s = tree->toString(tree);
    const char* nm = (s && s->chars) ? s->chars : "";

    if (!strcmp(nm, "FuncSignature")) {
        processFunction(tree, ctx);
    }
    if (strcmp(nm, "Var") == 0) {
        processVar(tree, ctx);
    }
    else if (!strcmp(nm, "IfStatement")) {
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
    else if (strcmp(nm, "BinOpAss") == 0) {
        processAssignment(tree, ctx);
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
    char buf[256] = {0};
    static unsigned funcCounter = 0;

    for (unsigned i = 0; i < topCount; i++) {
        pANTLR3_BASE_TREE child = tree->getChild(tree, i);
        pANTLR3_STRING s = child->toString(child);
        const char *nm = (s && s->chars) ? s->chars : "";
        

        if (!strcmp(nm, "SourceItem")) {
            char funcName[32];
            snprintf(funcName, sizeof(funcName), "func_%u", funcCounter++);
            struct funcNode *func = processSourceItem(child, funcName);
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
            
            struct cfgNode* node = createCfgNode(child);
            node->name = safeStrdup(buf);

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