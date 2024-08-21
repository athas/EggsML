#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

void flipline(void) {
  char* line = NULL;
  size_t n;
  ssize_t m;
  while ((m = getline(&line, &n, stdin)) > 0) {
    int i = rand() % m;
    int j = rand() % 8;
    line[i] ^= i<<j;
    fputs(line, stdout);
  }
}

int main(int argc, char* argv[]) {
  srand(getpid());
  if (argc == 1) {
    flipline();
  } else {
    assert(argc==2);
    FILE* f = fopen(argv[1], "r+");
    assert(f != NULL);
    assert(fseek(f, 0, SEEK_END) == 0);
    int size = ftell(f);
    if (size == 0) return 0;
    int i = rand() % size;
    int j = rand() % 8;
    assert(fseek(f, i, SEEK_SET) == 0);
    unsigned char c;
    assert(fread(&c, 1, 1, f) == 1);
    c ^= 1<<j;
    assert(fseek(f, i, SEEK_SET) == 0);
    assert(fwrite(&c, 1, 1, f) == 1);
  }
}
