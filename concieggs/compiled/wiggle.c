#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char *argv[]) {
    if (argc != 5) {
        fprintf(stderr, "Usage: %s <n> <min> <max> <wiggle_factor>\n", argv[0]);
        return 1;
    }
    
    int n = atoi(argv[1]);
    int min_val = atoi(argv[2]);
    int max_val = atoi(argv[3]);
    double wiggle_factor = atof(argv[4]);
    
    srand(time(NULL));
    
    int last_change = 0;
    int offset = 0;
    int wiggle_back_space = 4;
    
    for (int i = 1; i <= n; i++) {
        if ((i - last_change) > wiggle_back_space) {
            if (n - i <= abs(offset) * wiggle_back_space) {
                if (offset > 0) offset--;
                if (offset < 0) offset++;
                last_change = i;
            } else if ((double)rand() / RAND_MAX < wiggle_factor) {
                offset += ((double)rand() / RAND_MAX > 0.5) ? 1 : -1;
                if (offset > max_val) offset = max_val;
                if (offset < min_val) offset = min_val;
                last_change = i;
            }
        }
        
        printf("%d\n", offset);
    }
    
    return 0;
}
