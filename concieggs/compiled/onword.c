#define _GNU_SOURCE

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

void on_word(const char *word, int wordlen, const char *cmd) {
  int fds[2];

  pipe(fds);

  int pid;
  if ((pid = fork()) == 0) {
    close(fds[1]);
    dup2(fds[0], 0);
    exit(system(cmd));
  } else {
    close(fds[0]);
    FILE *f = fdopen(fds[1], "w");
    fwrite(word, sizeof(char), wordlen, f);
    fclose(f);
  }
  waitpid(pid, NULL, 0);
}

void on_line(char *line, const char *cmd) {
  while (*line) {
    // Skip to next word.
    while (*line && !isalnum(*line)) {
      fputc(*line++, stdout);
    }
    char *wordstart = line;
    // Skip to end of word.
    while (isalnum(*line)) {
      line++;
    }
    int wordlen = line - wordstart;
    if (wordlen > 0) {
      fflush(stdout);
      on_word(wordstart, wordlen, cmd);
    }
  }
}

int main(int argc, char **argv) {
  char *line = NULL;
  size_t buf;
  ssize_t n;

  while ((n = getline(&line, &buf, stdin)) != -1) {
    on_line(line, argv[1]);
  }
  free(line);
}
