#include <string.h>

#include <antlr3.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include "ast/ast.h"

int main(void)
{
    char* filename = "input.txt";
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Ошибка при открытии файла");
        return 1;
    }
    size_t buffer_size = 1024;
    char *content = malloc(buffer_size);
    if (content == NULL) {
        perror("Ошибка при выделении памяти");
        fclose(file);
        return 1;
    }
    content[0] = '\0';

    char line[256];
    while (fgets(line, sizeof(line), file) != NULL) {
        if (strlen(content) + strlen(line) + 1 > buffer_size) {
            buffer_size *= 2;
            char *new_content = realloc(content, buffer_size);
            if (new_content == NULL) {
                perror("Ошибка при перераспределении памяти");
                free(content);
                fclose(file);
                return 1;
            }
            content = new_content;
        }
        strcat(content, line);
    }
    makeTree(content, filename);
    fclose(file);
    return 0;
}