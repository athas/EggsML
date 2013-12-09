#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const unsigned int max_instructions = 10000;
static const unsigned int max_output = 300;
static const unsigned int data_size = 10000;

char* readfile(FILE* file) {
    size_t bufsiz = 80, len = 0;
    char* buf = malloc(bufsiz);
    char* p = buf;

    for (;;) {
        int c = fgetc(file);
        if(c == EOF)
            break;

        if (++len == bufsiz) {
            char* newbuf = realloc(buf, bufsiz *= 2);

            p = newbuf + (p - buf);
            buf = newbuf;
        }
        *p++ = c;
    }
    *p = '\0';
    return buf;
}

int main(int argc, char** argv) {
    unsigned char data[data_size];
    memset(data, 0, data_size*sizeof(*data));
    char* code = readfile(stdin);
    char* input = argc > 1 ? argv[1] : "";
    unsigned int datap = 0;
    unsigned int depth = 0;
    unsigned int eggsecuted = 0;
    unsigned int outputted = 0;
    for (int codep = 0; code[codep] != 0; codep++, eggsecuted++) {
        if (eggsecuted >= max_instructions) {
            printf("Program eggsecution halted: %d instruction limit eggceeded.",
                   max_instructions);
            goto stop;
        }
        switch(code[codep]) {
        case '+':
            data[datap]++;
            break;
        case '-':
            data[datap]--;
            break;
        case '>':
            datap++;
            break;
        case '<':
            datap--;
            break;
        case '.':
            if (outputted++ >= max_output) {
                printf("Program eggsecution halted: %d output limit eggceeded.",
                       max_output);
                goto stop;
            }
            putchar(data[datap]);
            break;
        case ',':
            data[datap] = *input;
            if (*input != 0) {
                input++;
            }
            break;
        case '[':
            if (data[datap] == 0) {
                codep++;
                while (depth > 0 || code[codep] != ']') {
                    if (code[codep] == '[') depth++;
                    if (code[codep] == ']') depth--;
                    codep++;
                }
            }
            break;
        case ']':
            if (data[datap] != 0) {
                codep--;
                while (depth > 0 || code[codep] != '[') {
                    if (code[codep] == '[') depth--;
                    if (code[codep] == ']') depth++;
                    codep--;
                }
                codep--;
            }
            break;
        }
    }
 stop:
    puts("\n");
    return 0;
}
