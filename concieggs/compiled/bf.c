/* The OFFICIAL EggsML Brainfuck interpreter.
 *
 * Does some simple optimisation in the form of instruction
 * compression.
 *
 * Don't nest your loops too deeply.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static const int DATASIZE = 30000;

typedef uint8_t byte;
typedef int instruction;

enum instr_type {
  INSTR_ADD = 0,
  INSTR_SUB = 1,
  INSTR_RIGHT = 2,
  INSTR_LEFT = 3,
  INSTR_LOOP = 4,
  INSTR_ENDLOOP = 5,
  INSTR_OUTPUT = 6,
  INSTR_INPUT = 7,
  INSTR_NONE = 8
};

static const int INSTR_TYPE_MASK = 0x7;

static inline int instr_type(instruction instr) {
  return instr & INSTR_TYPE_MASK;
}

static inline int instr_arg(instruction instr) {
  return instr >> 3;
}

static inline void step(byte **datap, instruction **codep) {
  int type = instr_type(**codep);
  int arg = instr_arg(**codep);
  int c;
  switch (type) {
  case INSTR_ADD:
    **datap += arg;
    (*codep)++;
    break;
  case INSTR_SUB:
    **datap -= arg;
    (*codep)++;
    break;
  case INSTR_RIGHT:
    (*datap) += arg;
    (*codep)++;
    break;
  case INSTR_LEFT:
    (*datap) -= arg;
    (*codep)++;
    break;
  case INSTR_LOOP:
    if (**datap == 0) {
      (*codep) += arg;
      (*codep)++;
    } else {
      (*codep)++;
    }
    break;
  case INSTR_ENDLOOP:
    if (**datap != 0) {
      (*codep) -= arg;
      (*codep)++;
    } else {
      (*codep)++;
    }
    break;
  case INSTR_OUTPUT:
    putchar(**datap);
    (*codep)++;
    break;
  case INSTR_INPUT:
    c = getchar();
    if (c == EOF) {
      **datap = 0;
    } else {
      **datap = c;
    }
    (*codep)++;
    break;
  default:
    fprintf(stderr, "Invalid instruction type: %d\n", type);
    exit(1);
    return;
  }
}

int read_instruction(int c) {
  switch (c) {
  case '+': return INSTR_ADD;
  case '-': return INSTR_SUB;
  case '>': return INSTR_RIGHT;
  case '<': return INSTR_LEFT;
  case '[': return INSTR_LOOP;
  case ']': return INSTR_ENDLOOP;
  case '.': return INSTR_OUTPUT;
  case ',': return INSTR_INPUT;
  default: return INSTR_NONE;
  }
}

instruction generate(int constructing, int constructing_arg) {
  return constructing | (constructing_arg << 3);
}

instruction* read_program(size_t *size, FILE* input) {
  int c;
  size_t space = 1024, loopstackspace = 1024;
  instruction *code = malloc(space * sizeof(instruction));
  size_t* loopstack = malloc(loopstackspace * sizeof(size_t));
  size_t loopstacksize = 0;
  int constructing = INSTR_NONE;
  int constructing_arg = 0;
  size_t start;
  *size = 0;
  while ((c = fgetc(input)) != EOF) {
    if (*size == space) {
      space *= 2;
      code = realloc(code, space * sizeof(instruction));
    }
    int current = read_instruction(c);
    if (current == INSTR_NONE) {
      continue;
    }
    if (current == constructing) {
      switch (current) {
      case INSTR_ADD:
      case INSTR_SUB:
      case INSTR_RIGHT:
      case INSTR_LEFT:
        constructing_arg++;
        continue;
      }
    }
    if (constructing != INSTR_NONE) {
      code[*size] = generate(constructing, constructing_arg);
      (*size)++;
      constructing = INSTR_NONE;
    }
    switch (current) {
    case INSTR_ADD:
    case INSTR_SUB:
    case INSTR_RIGHT:
    case INSTR_LEFT:
      constructing = current;
      constructing_arg = 1;
      break;
    case INSTR_LOOP:
      if (loopstacksize == loopstackspace) {
        fprintf(stderr, "Loop stack overflow.\n");
        exit(1);
      }
      loopstack[loopstacksize++] = *size;
      code[*size] = generate(current, 0);
      (*size)++;
      break;
    case INSTR_ENDLOOP:
      start = loopstack[--loopstacksize];
      code[start] = generate(INSTR_LOOP, *size-start);
      code[*size] = generate(current, *size-start);
      (*size)++;
      break;
    case INSTR_INPUT:
    case INSTR_OUTPUT:
      code [*size] = generate(current, 0);
      (*size)++;
      break;
    }
  }
  if (constructing != INSTR_NONE) {
    code[*size] = generate(constructing, constructing_arg);
    (*size)++;
  }
  free(loopstack);
  return code;
}

int main(int argc, char** argv) {
  size_t size;
  FILE* input;
  if (argc == 1) {
    input = stdin;
  } else if (argc == 2) {
    input = fopen(argv[1], "r");
    if (!input) {
      fprintf(stderr, "Failed to open file %s: %s\n", argv[1], strerror(errno));
      exit(1);
    }
  } else {
    fprintf(stderr, "Usage: %s [file]\n", argv[0]);
    exit(1);
  }
  instruction *code = read_program(&size, input);

  byte data[DATASIZE];
  memset(data, 0, DATASIZE);
  instruction *codep = code;
  byte *datap = data;
  int executed = 0;
  while (codep < code + size) {
    step(&datap, &codep);
    if (executed++ > 10000) {
      printf("Eggceeded maximum eggsecution time.\n");
      exit(1);
    }
  }
  return 0;
}
