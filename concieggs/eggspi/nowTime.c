#include <sys/time.h>
#include <stdio.h>

int main(int argc, char **argv) {
  struct timeval tv;

  argc=argc; /* Unused */

  if (gettimeofday(&tv, NULL) != 0) {
    perror(argv[0]);
    return 1;
  } else {
    printf("%ld\n", tv.tv_sec);
    return 0;
  }
}
