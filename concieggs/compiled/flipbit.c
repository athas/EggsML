#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
  assert(argc==2);
  FILE* f = fopen(argv[1], "r+");
  srand(getpid());
  assert(f != NULL);
  assert(fseek(f, 0, SEEK_END) == 0);
  int size = ftell(f);
  if (size == 0) return 0;
  int i = rand() % size;
  int j = rand() % 8;
  assert(fseek(f, i, SEEK_SET) == 0);
  unsigned char c;
  assert(fread(&c, 1, 1, f) == 1);
  c ^= (1<<j);
  assert(fseek(f, i, SEEK_SET) == 0);
  assert(fwrite(&c, 1, 1, f) == 1);
}
