/* The OFFICIAL EggsML line formatter.
 *
 * Does what GNUs 'fmt -s [-w N]' does, but does it cross-platform.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

void write_line(char *line, ssize_t line_len) {
  fwrite(line, line_len, 1, stdout);
  fputc('\n', stdout);
}

void fmt(char *line_full, ssize_t line_remaining_len, int width) {
  char *line = line_full;
  ssize_t line_len;
  int i;
  bool split_on_space = false;
  
  while (line_remaining_len > width) {
    line_len = width;
    for (i = 1; i < line_len; i++) {
      if (line[line_len - i] == ' ') {
        line_len = line_len - i;
        split_on_space = true;
        break;
      }
    }
    write_line(line, line_len);
    if (split_on_space) {
      line_remaining_len--;
      line++;
    }
    line_remaining_len -= line_len;
    line += line_len;
  }
  if (line_remaining_len > 0) {
    write_line(line, line_remaining_len);
  }
}

int main(int argc, char** argv) {
  char *line = NULL;
  size_t line_size = 0;
  ssize_t line_len;
  int width;
  const char *errstr;

  if (argc == 3 && strcmp(argv[1], "-w") == 0) {
    width = atoi(argv[2]); // works, is fine
  }
  else {
    width = 75;
  }

  while ((line_len = getline(&line, &line_size, stdin)) != -1) {
    if (line[line_len - 1] == '\n') {
      line[line_len - 1] = '\0';
      line_len--;
    }
    fmt(line, line_len, width);
  }

  if (ferror(stdin)) {
    perror("getline");
    return 1;
  }

  return 0;
}
