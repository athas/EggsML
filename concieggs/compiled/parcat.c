/* Stream from multiple fifos.  Whenever a new line is ready from either file,
   print it to standard out.  Assumes there is always an entire line to read
   whenever a read becomes possible (will block until a newline is found). */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>

#define BUFFER_SIZE 768

int main(int argc, char* *argv) {
  int n_fds;
  FILE* *files = (FILE**) (argv + 1);
  fd_set rfds;
  int retval;

  if (argc == 1) {
    exit(EXIT_SUCCESS);
  }

  n_fds = argc - 1;
  FD_ZERO(&rfds);
  int fd_max = 0;
  for (int i = 1; i < argc; i++) {
    int fd = open(argv[i], O_RDWR | O_NONBLOCK);
    assert(fd != -1);
    FILE* f = fdopen(fd, "rw");
    assert(fd < FD_SETSIZE);
    fd_max = fd > fd_max ? fd : fd_max;
    FD_SET(fd, &rfds);
    files[i - 1] = f;
  }
  fd_max++;

  char buffer[BUFFER_SIZE];
  while (true) {
    retval = select(fd_max, &rfds, NULL, NULL, NULL);
    if (retval >= 1) {
      for (int i = 0; i < n_fds; i++) {
        FILE* f = files[i];
        int fd = fileno(f);
        if (FD_ISSET(fd, &rfds)) {
          ssize_t nread;
          do {
            /* Keep reading and writing chunks when they exist. */
            nread = read(fd, buffer, BUFFER_SIZE);
            assert(nread != -1);
            int base = 0;
            int i;
            for (i = base; i < nread; i++) {
              /* Make sure to flush every line to standard out as soon as it is
                 available. */
              if (buffer[i] == '\n') {
                fwrite(buffer + base, i + 1 - base, 1, stdout);
                fflush(stdout);
                base = i + 1;
              }
            }
            if (base != nread - 1) {
              fwrite(buffer + base, nread - base, 1, stdout);
            }
          } while (nread == BUFFER_SIZE);
          if (!(nread >= 0 && buffer[nread - 1] == '\n')) {
            /* Any input we might have had did not end with a newline.  Set our
               read to be blocking and wait for a newline to show. */
            int flags = fcntl(fd, F_GETFL);
            fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
            do {
              nread = read(fd, buffer, 1);
              assert(nread == 1);
              fwrite(buffer, 1, 1, stdout);
            } while (buffer[0] != '\n');
            /* Go back to non-blocking. */
            fcntl(fd, F_SETFL, flags);
            fflush(stdout);
          }
        } else {
          FD_SET(fd, &rfds);
        }
      }
    } else {
      exit(EXIT_FAILURE);
    }
  }

  exit(EXIT_SUCCESS);
}
