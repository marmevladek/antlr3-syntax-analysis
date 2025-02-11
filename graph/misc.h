#define IGNORED_COUNT 5

const char *IgnoredTable[5] = {
    "FuncSignatureToken",
    "sourceToken",
    "ArgListToken",
    "Body",
    "VarDeclToken"
};

bool isIgnoredString(const char *query) {
    for (int i = 0; i < 5; i++) {
        if (strcmp(IgnoredTable[i], query) == 0) {
            return true;
        }
    }
    return false;
}