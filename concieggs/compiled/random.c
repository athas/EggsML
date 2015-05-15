#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

/* The right thing would be to use arc4random() here, but some people
   might complain if the code runs only on OpenBSD. */

int main(int argc, char** argv) {
  unsigned int seed = 0;
  int min, range;

  if (argc != 3) {
    fprintf(stderr, "Usage: %s <lower> <upper>\n", argv[0]);
    exit(1);
  } else {
    int lower = atoi(argv[1]);
    int upper = atoi(argv[2]);
    range = upper - lower + 1;
    min  = lower;
  }

  seed ^= time(NULL);
  seed ^= getpid();
  seed ^= getppid();
  srand(seed);
  printf("%d\n", min + (rand() % range));
}
