// https://www.codewars.com/kata/5208f99aee097e6552000148

#include <stddef.h> // NULL
#include <string.h>
#include <stdlib.h>

char* solution(const char *camelCase) {
    size_t i, spaces = 0;
    for (i = 0; camelCase[i] != '\0'; ++i){
        if (camelCase[i] >= 'A' && camelCase[i] <= 'Z') 
            ++spaces;
    }

    size_t len = 0;
    char *out_buff = calloc(i + spaces + 1, 1);
    for (i = 0; camelCase[i] != '\0'; ++i){
        if (camelCase[i] >= 'A' && camelCase[i] <= 'Z') 
            out_buff[len++] = ' ';

        out_buff[len++] = camelCase[i];
    }
    return out_buff;
}
