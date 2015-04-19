#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <time.h>

/* TODO: Error detection and handling. */


size_t align(size_t len) {
  return len + sizeof(size_t) - len % sizeof(size_t);
}

typedef struct {
  size_t n_nexts;
  size_t* nexts;
  char* word;
} temp_word_t;

size_t gen_markov(size_t** out_buffer, FILE* read_fd) {
  size_t n_words = 0;
  temp_word_t* temp_words = NULL;
  size_t word_i;
  size_t prev_word_i;

  size_t n_start_words;
  size_t* start_words = NULL;
  bool is_start_word;

  char* line = NULL;
  size_t len = 0;
  ssize_t read;

  /* Build temporary structure. */
  prev_word_i = -1;
  is_start_word = true;
  while ((read = getline(&line, &len, read_fd)) != -1) {
    if (line[read - 1] == '\n') {
      line[read - 1] = '\0';
    }
    if (line[0] == '\0') {
      if (is_start_word) {
        prev_word_i = -1;
      }
      else {
        is_start_word = true;
      }
      continue;
    }

    word_i = -1;
    for (size_t i = 0; i < n_words; i++) {
      if (strcmp(temp_words[i].word, line) == 0) {
        word_i = i;
        break;
      }
    }
    if (word_i == -1) {
      word_i = n_words;
      n_words++;
      temp_words = (temp_word_t*)
        realloc(temp_words, n_words * sizeof(temp_word_t));
      temp_words[word_i].n_nexts = 0;
      temp_words[word_i].nexts = NULL;
      temp_words[word_i].word = strdup(line);
    }

    if (prev_word_i != -1) {
      temp_words[prev_word_i].n_nexts++;
      temp_words[prev_word_i].nexts = (size_t*)
        realloc(temp_words[prev_word_i].nexts,
                temp_words[prev_word_i].n_nexts * sizeof(size_t));
      temp_words[prev_word_i].nexts[temp_words[prev_word_i].n_nexts - 1] = word_i;
    }

    if (is_start_word) {
      for (int i = 0; i < n_start_words; i++) {
        if (strcmp(temp_words[start_words[i]].word, line) == 0) {
          is_start_word = false;
          break;
        }
      }
      if (is_start_word) {
        n_start_words++;
        start_words = realloc(start_words, n_start_words * sizeof(size_t));
        start_words[n_start_words - 1] = word_i;
        is_start_word = false;
      }
    }

    prev_word_i = word_i;
  }
  free(line);

  /* Compactify. */
  size_t* buffer;
  size_t entry_size;
  size_t* offset = (size_t*) malloc(n_words * sizeof(size_t));
  size_t buffer_size = 1 + n_start_words;

  for (size_t i = 0; i < n_words; i++) {
    offset[i] = buffer_size;
    entry_size = (1
                  + temp_words[i].n_nexts
                  + align(strlen(temp_words[i].word) + 1) / sizeof(size_t));
    buffer_size += entry_size;
  }
  buffer = (size_t*) malloc(buffer_size * sizeof(size_t));
  buffer[0] = n_start_words;
  for (int i = 0; i < n_start_words; i++) {
    buffer[1 + i] = offset[start_words[i]];
  }

  for (size_t i = 0; i < n_words; i++) {
    buffer[offset[i]] = temp_words[i].n_nexts;
    for (size_t j = 0; j < temp_words[i].n_nexts; j++) {
      buffer[offset[i] + 1 + j] = offset[temp_words[i].nexts[j]];
    }
    strcpy((char*) &buffer[offset[i] + 1 + temp_words[i].n_nexts],
           temp_words[i].word);
  }

  /* End. */
  for (size_t i = 0; i < n_words; i++) {
    free(temp_words[i].word);
    free(temp_words[i].nexts);
  }
  free(temp_words);
  free(start_words);
  free(offset);

  *out_buffer = buffer;
  return buffer_size;
}

size_t poooooor_random(size_t range) {
  return rand() % range;
}

void gen_words(size_t* buffer, int n_gen_words) {
  size_t n_start_words;
  size_t* start_words;

  size_t* entry;
  size_t n_nexts;
  size_t* nexts;
  char* word;

  n_start_words = buffer[0];
  start_words = &buffer[1];

  if (n_start_words == 0) {
    return;
  }

  entry = &buffer[start_words[poooooor_random(n_start_words)]];
  for (int i = 0; i < n_gen_words; i++) {
    n_nexts = entry[0];
    nexts = &entry[1];
    word = (char*) &entry[1 + n_nexts];
    puts(word);
    if (n_nexts == 0) {
      entry = &buffer[start_words[poooooor_random(n_start_words)]];
    }
    else {
      entry = &buffer[nexts[poooooor_random(n_nexts)]];
    }
  }
}

int main(int argc, char** argv) {
  if (argc < 4) {
    return EXIT_FAILURE;
  }

  srand(time(NULL));

  int n_gen_words = atoi(argv[1]);
  size_t* buffer = NULL;
  size_t buffer_size;
  char* db_dir = getenv("CONCIEGGS_DB_DIR");
  char sub_dir[] = "/markov-cache/";
  char* name = argv[2];
  char* buffer_path = (char*) malloc(strlen(db_dir) + sizeof(sub_dir) + strlen(name));
  buffer_path[0] = '\0';
  strcat(buffer_path, db_dir);
  strcat(buffer_path, sub_dir);
  mkdir(buffer_path, S_IRUSR | S_IWUSR | S_IXUSR);
  strcat(buffer_path, name);

  char* source_path_source;
  char* source_path_data;
  source_path_source = argv[3];
  if (argc == 5) {
    source_path_data = argv[4];
  }
  else {
    source_path_data = source_path_source;
  }

  struct stat attr_source;
  struct stat attr_buffer;
  stat(source_path_source, &attr_source);
  int bufstatret = stat(buffer_path, &attr_buffer);

  int buffd = open(buffer_path, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  FILE* source_fd;

  if (bufstatret == -1 || attr_source.st_mtime > attr_buffer.st_mtime) {
    source_fd = fopen(source_path_data, "r");
    buffer_size = gen_markov(&buffer, source_fd);
    fclose(source_fd);

    write(buffd, buffer, buffer_size * sizeof(size_t));
    gen_words(buffer, n_gen_words);
    free(buffer);
  }
  else {
    buffer_size = attr_buffer.st_size;
    buffer = mmap(NULL, buffer_size * sizeof(size_t), PROT_READ, MAP_PRIVATE, buffd, 0);
    gen_words(buffer, n_gen_words);
    munmap(buffer, buffer_size * sizeof(size_t));
  }

  close(buffd);
  free(buffer_path);
  return EXIT_SUCCESS;
}
