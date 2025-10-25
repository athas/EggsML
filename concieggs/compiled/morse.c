#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

const char* morse_table[256] = {0};

void init_morse_table() {
    morse_table['a'] = "·-";    morse_table['b'] = "-···";  morse_table['c'] = "-·-·";
    morse_table['d'] = "-··";   morse_table['e'] = "·";     morse_table['f'] = "··-·";
    morse_table['g'] = "--·";   morse_table['h'] = "····";  morse_table['i'] = "··";
    morse_table['j'] = "·---";  morse_table['k'] = "-·-";   morse_table['l'] = "·-··";
    morse_table['m'] = "--";    morse_table['n'] = "-·";    morse_table['o'] = "---";
    morse_table['p'] = "·--·";  morse_table['q'] = "--·-";  morse_table['r'] = "·-·";
    morse_table['s'] = "···";   morse_table['t'] = "-";     morse_table['u'] = "··-";
    morse_table['v'] = "···-";  morse_table['w'] = "·--";   morse_table['x'] = "-··-";
    morse_table['y'] = "-·--";  morse_table['z'] = "--··";
    
    morse_table['0'] = "-----"; morse_table['1'] = "·----"; morse_table['2'] = "··---";
    morse_table['3'] = "···--"; morse_table['4'] = "····-"; morse_table['5'] = "·····";
    morse_table['6'] = "-····"; morse_table['7'] = "--···"; morse_table['8'] = "---··";
    morse_table['9'] = "----·";
}

const char* get_morse(const char *str, int *consumed) {
    unsigned char c = tolower((unsigned char)str[0]);
    *consumed = 1;
    
    // Check for UTF-8 encoded Danish characters
    if ((unsigned char)str[0] == 0xc3 && str[1]) {
        unsigned char second = (unsigned char)str[1];
        *consumed = 2;
        if (second == 0xa6 || second == 0x86) return "·-·-"; // æ/Æ
        if (second == 0xb8 || second == 0x98) return "---·"; // ø/Ø
        if (second == 0xa5 || second == 0x85) return "·-";   // å/Å
    }
    
    return morse_table[c];
}

int main() {
    init_morse_table();
    
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    
    while ((read = getline(&line, &len, stdin)) != -1) {
        // Remove trailing newline
        if (read > 0 && line[read-1] == '\n') {
            line[read-1] = '\0';
            read--;
        }
        
        int first_non_space = -1;
        int last_non_space = -1;
        
        // Find first and last non-space characters
        for (int i = 0; i < read; i++) {
            if (!isspace((unsigned char)line[i])) {
                if (first_non_space == -1) first_non_space = i;
                last_non_space = i;
            }
        }
        
        int i = 0;
        while (i < read) {
            unsigned char c = line[i];
            
            // Replace internal spaces with /
            if (isspace(c) && i > first_non_space && i < last_non_space) {
                printf("/");
                i++;
            } else if (!isspace(c)) {
                int consumed;
                const char* morse = get_morse(&line[i], &consumed);
                if (morse) {
                    printf("%s", morse);
                } else {
                    // Print non-morse characters as-is
                    for (int j = 0; j < consumed; j++) {
                        printf("%c", line[i + j]);
                    }
                }
                i += consumed;
            } else {
                i++;
            }
        }
        printf("\n");
    }
    
    free(line);
    return 0;
}
