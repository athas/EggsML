
#define BUFFER_UNIT_T uint8_t
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>
#include <sys/time.h>

#define RETC_PRINT_USAGE     1
#define RETC_PRINT_INFO      2

#define OUTBUFFER_SIZE       (16*1024)
#define INBUFFER_SIZE        (16*1024)
#define INITIAL_BUFFER_SIZE  (4096*8)

#ifdef FLAG_NOINLINE
#define INLINE static
#endif
#ifndef INLINE
#define INLINE static inline
#endif

#ifndef BUFFER_UNIT_T
#warning "BUFFER_UNIT_T not defined. Falling back to default 'uint8_t'"
#define BUFFER_UNIT_T uint8_t
#endif

#ifndef OUTSTREAM
#define OUTSTREAM stdout
#endif

typedef BUFFER_UNIT_T buffer_unit_t;
typedef struct {
  buffer_unit_t *data;
  size_t size;         /* size in bytes */
  size_t bitpos;       /* bit offset from data  */
} buffer_t;

#define BUFFER_UNIT_SIZE (sizeof(buffer_unit_t))
#define BUFFER_UNIT_BITS (BUFFER_UNIT_SIZE * 8)

unsigned char *next;
buffer_t outbuf;
size_t count = 0;

unsigned char inbuf[INBUFFER_SIZE*2];
size_t in_size = 0;
int in_cursor = 0;
#define avail (in_size - in_cursor)

// Program interface

void printCompilationInfo();
void init();
void match();

void buf_flush(buffer_t *buf)
{
  size_t word_index = buf->bitpos / BUFFER_UNIT_BITS;
  // If we do not have a single complete word to flush, return.
  // Not just an optimization! The zeroing logic below assumes word_index > 0.
  if (word_index == 0)
  {
    return;
  }
  if (fwrite(buf->data, BUFFER_UNIT_SIZE, word_index, OUTSTREAM) == -1)
  {
    fprintf(stderr, "Error writing to output stream.\n");
    exit(1);
  }
  // Since partially written words are not flushed, they need to be moved to the
  // beginning of the buffer.
  if (buf->bitpos % BUFFER_UNIT_BITS != 0)
  {
    buf->data[0] = buf->data[word_index];
  }
  else
  {
    // If we flushed everything, re-establish the invariant that the word at the
    // cursor is garbage-free by simply zeroing it.
    buf->data[0] = 0;
  }

  // Rewind cursor
  buf->bitpos = buf->bitpos - word_index * BUFFER_UNIT_BITS;
}

// Write first 'bits' of 'w' to 'buf', starting from the MOST significant bit.
// Precondition: Remaining bits of 'w' must be zero.
INLINE
bool buf_writeconst(buffer_t *buf, buffer_unit_t w, int bits)
{
  size_t word_index = buf->bitpos / BUFFER_UNIT_BITS;
  size_t offset = buf->bitpos % BUFFER_UNIT_BITS;
  size_t bits_available = BUFFER_UNIT_BITS - offset;

#ifdef FLAG_WORDALIGNED
  buf->data[word_index] = w;
#else
  buf->data[word_index] |= w >> offset;
  // test for offset > 0 important; shifting by the word size is undefined behaviour.
  buf->data[word_index+1] = (offset == 0) ? 0 : (w << bits_available);
#endif

  buf->bitpos += bits;

  // Is cursor in last word?
  return (buf->bitpos >= buf->size * 8 - BUFFER_UNIT_BITS);
}

void buf_resize(buffer_t *buf, size_t shift)
{
  size_t new_size = buf->size << shift;
  buffer_unit_t *data2 = calloc(new_size, 1);
  memcpy(data2, buf->data, buf->size);
  free(buf->data);
  buf->data = data2;
  buf->size = new_size;
}

INLINE
void buf_writearray(buffer_t *dst, const buffer_unit_t *arr, int bits)
{
  if (dst->bitpos % BUFFER_UNIT_BITS == 0)
  {
    int count = (bits / BUFFER_UNIT_BITS) + (bits % BUFFER_UNIT_BITS ? 1 : 0);
    memcpy(&dst->data[dst->bitpos / BUFFER_UNIT_BITS], arr, count * BUFFER_UNIT_SIZE);
    dst->bitpos += bits;
    dst->data[dst->bitpos / BUFFER_UNIT_BITS] = 0;
  } else
  {
    int word_index = 0;
    for (word_index = 0; word_index < bits / BUFFER_UNIT_BITS; word_index++)
    {
      buf_writeconst(dst, arr[word_index], BUFFER_UNIT_BITS);
    }

    if (bits % BUFFER_UNIT_BITS != 0)
    {
      buf_writeconst(dst, arr[word_index], bits % BUFFER_UNIT_BITS);
    }
  }
}

INLINE
void reset(buffer_t *buf)
{
  buf->data[0] = 0;
  buf->bitpos = 0;
}

void init_buffer(buffer_t *buf)
{
  buf->data = malloc(INITIAL_BUFFER_SIZE);
  buf->size = INITIAL_BUFFER_SIZE;
  buf->bitpos = 0;
  buf->data[0] = 0;
}

void destroy_buffer(buffer_t *buf)
{
  if (buf->data != NULL)
    free(buf->data);
  buf->data = NULL;
}

INLINE
void outputconst(buffer_unit_t w, int bits)
{
  if (buf_writeconst(&outbuf, w, bits))
  {
    buf_flush(&outbuf);
  }
}

INLINE
void appendarray(buffer_t *dst, const buffer_unit_t *arr, int bits)
{
  size_t total_bits = dst->bitpos + bits;
  if (total_bits >= (dst->size - 1) * BUFFER_UNIT_BITS * BUFFER_UNIT_SIZE)
  {
    size_t shift = 1;
    while (total_bits >= ((dst->size << shift) - 1) * BUFFER_UNIT_BITS * BUFFER_UNIT_SIZE)
    {
      shift++;  
    }
    buf_resize(dst, shift);
  }

  buf_writearray(dst, arr, bits);
}

INLINE
void append(buffer_t *buf, buffer_unit_t w, int bits)
{
  if (buf_writeconst(buf, w, bits))
  {
    buf_resize(buf, 1);
  }  
}

INLINE
void concat(buffer_t *dst, buffer_t *src)
{
  appendarray(dst, src->data, src->bitpos);
}

INLINE
void outputarray(const buffer_unit_t *arr, int bits)
{
  int word_count = bits / BUFFER_UNIT_BITS;
  // Write completed words
  size_t word_index = 0;
  for (word_index = 0; word_index < word_count; word_index++)
  {
    outputconst(arr[word_index], BUFFER_UNIT_BITS);
  }

  int remaining = bits % BUFFER_UNIT_BITS;
  if (remaining != 0)
  {
    outputconst(arr[bits / BUFFER_UNIT_BITS], remaining);
  }
}

INLINE
void output(buffer_t *buf)
{
  outputarray(buf->data, buf->bitpos);
}

INLINE
void consume(int c)
{
  count     += c;
  in_cursor += c;
  next      += c;
}

INLINE
int readnext(int minCount, int maxCount)
{
  if (avail < maxCount)
  {
    int remaining = avail;
    memmove(&inbuf[INBUFFER_SIZE - remaining], &inbuf[INBUFFER_SIZE+in_cursor], remaining);
    in_cursor = -remaining;
    in_size = fread(&inbuf[INBUFFER_SIZE], 1, INBUFFER_SIZE, stdin);
  }
  if (avail < minCount)
  {
    return 0;
  }
  next = &inbuf[INBUFFER_SIZE+in_cursor];
  return 1;
}

INLINE
int cmp(unsigned char *str1, unsigned char *str2, int l)
{
  int i = 0;
  for (i = 0; i < l; i++)
  {
    if (str1[i] != str2[i])
      return 0;
  }
  return 1;
}

void printUsage(char *name)
{
  fprintf(stdout, "Normal usage: %s < infile > outfile\n", name);
  fprintf(stdout, "- \"%s\": reads from stdin and writes to stdout.\n", name);
  fprintf(stdout, "- \"%s -i\": prints compilation info.\n", name);
  fprintf(stdout, "- \"%s -t\": runs normally, but prints timing to stderr.\n", name);
}

void flush_outbuf()
{
  if (outbuf.bitpos % BUFFER_UNIT_BITS != 0)
  {
    outputconst(0, BUFFER_UNIT_BITS);
  }
  buf_flush(&outbuf);
}

void init_outbuf()
{
  outbuf.size = OUTBUFFER_SIZE + BUFFER_UNIT_SIZE;
  outbuf.data = malloc(outbuf.size);
  reset(&outbuf);
}

void run()
{
  match();
  flush_outbuf();
}

#ifndef FLAG_NOMAIN
int main(int argc, char *argv[])
{
  bool do_timing = false;

  if(argc > 2)
  {
    printUsage(argv[0]);
    return RETC_PRINT_USAGE;
  }
  if (argc == 2) 
  {
    if(strcmp("-i", argv[1]) == 0)
    {
      printCompilationInfo();
      return RETC_PRINT_INFO;
    }
    else if(strcmp("-t", argv[1]) == 0)
    {
      do_timing = true;
    }
    else
    {
      printUsage(argv[0]);
      return RETC_PRINT_USAGE;
    }
  }
    
  init_outbuf();
  init();

  if(do_timing)
  {
    struct timeval time_before, time_after, time_result;
    long int millis;
    gettimeofday(&time_before, NULL);
    run();
    gettimeofday(&time_after, NULL);
    timersub(&time_after, &time_before, &time_result);
    // A timeval contains seconds and microseconds.
    millis = time_result.tv_sec * 1000 + time_result.tv_usec / 1000;
    fprintf(stderr, "time (ms): %ld\n", millis);
  }
  else
  {
    run();
  }

  return 0;
}
#endif

const uint8_t tbl[1][256] =
{{ 0x0,  0x1,  0x2,  0x3,  0x4,  0x5,  0x6,  0x7,
   0x8,  0x9,  0xa,  0xb,  0xc,  0xd,  0xe,  0xf,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
  0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
  0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
  0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff}};
buffer_t buf_0;
buffer_t buf_1;
buffer_t buf_2;
buffer_t buf_3;
buffer_t buf_4;
buffer_t buf_5;
buffer_t buf_6;
buffer_t buf_7;
buffer_t buf_8;
buffer_t buf_9;
// 
const buffer_unit_t const_0[0] = {};
// -
const buffer_unit_t const_1[1] = {0x2d};
// D
const buffer_unit_t const_2[1] = {0x44};
// Damnit
const buffer_unit_t const_3[6] = {0x44,0x61,0x6d,0x6e,0x69,0x74};
// Fanden ta'et
const buffer_unit_t const_4[12] = {0x46,0x61,0x6e,0x64,0x65,0x6e,0x20,0x74,0x61,0x27,0x65,0x74};
// P
const buffer_unit_t const_5[1] = {0x50};
// Pawel
const buffer_unit_t const_6[5] = {0x50,0x61,0x77,0x65,0x6c};
// Poul
const buffer_unit_t const_7[4] = {0x50,0x6f,0x75,0x6c};
// Vinter
const buffer_unit_t const_8[6] = {0x56,0x69,0x6e,0x74,0x65,0x72};
// W
const buffer_unit_t const_9[1] = {0x57};
// Winter
const buffer_unit_t const_10[6] = {0x57,0x69,0x6e,0x74,0x65,0x72};
// a
const buffer_unit_t const_11[1] = {0x61};
// admin
const buffer_unit_t const_12[5] = {0x61,0x64,0x6d,0x69,0x6e};
// admini
const buffer_unit_t const_13[6] = {0x61,0x64,0x6d,0x69,0x6e,0x69};
// administrator
const buffer_unit_t const_14[13] = {0x61,0x64,0x6d,0x69,0x6e,0x69,0x73,0x74,0x72,0x61,0x74,0x6f,0x72};
// afluser
const buffer_unit_t const_15[7] = {0x61,0x66,0x6c,0x75,0x73,0x65,0x72};
// aflusning
const buffer_unit_t const_16[9] = {0x61,0x66,0x6c,0x75,0x73,0x6e,0x69,0x6e,0x67};
// anmodning
const buffer_unit_t const_17[9] = {0x61,0x6e,0x6d,0x6f,0x64,0x6e,0x69,0x6e,0x67};
// arbejdslager
const buffer_unit_t const_18[12] = {0x61,0x72,0x62,0x65,0x6a,0x64,0x73,0x6c,0x61,0x67,0x65,0x72};
// b
const buffer_unit_t const_19[1] = {0x62};
// backtracking
const buffer_unit_t const_20[12] = {0x62,0x61,0x63,0x6b,0x74,0x72,0x61,0x63,0x6b,0x69,0x6e,0x67};
// backup
const buffer_unit_t const_21[6] = {0x62,0x61,0x63,0x6b,0x75,0x70};
// bestyrer
const buffer_unit_t const_22[8] = {0x62,0x65,0x73,0x74,0x79,0x72,0x65,0x72};
// best\xc3\xb8vning
const buffer_unit_t const_23[11] = {0x62,0x65,0x73,0x74,0xc3,0xb8,0x76,0x6e,0x69,0x6e,0x67};
// blanktegn
const buffer_unit_t const_24[9] = {0x62,0x6c,0x61,0x6e,0x6b,0x74,0x65,0x67,0x6e};
// blindgydes\xc3\xb8gning
const buffer_unit_t const_25[17] = {0x62,0x6c,0x69,0x6e,0x64,0x67,0x79,0x64,0x65,0x73,0xc3,0xb8,0x67,0x6e,0x69,0x6e,0x67};
// branchandbound
const buffer_unit_t const_26[14] = {0x62,0x72,0x61,0x6e,0x63,0x68,0x61,0x6e,0x64,0x62,0x6f,0x75,0x6e,0x64};
// brandmur
const buffer_unit_t const_27[8] = {0x62,0x72,0x61,0x6e,0x64,0x6d,0x75,0x72};
// bsite
const buffer_unit_t const_28[5] = {0x62,0x73,0x69,0x74,0x65};
// bug
const buffer_unit_t const_29[3] = {0x62,0x75,0x67};
// b\xc3\xa6
const buffer_unit_t const_30[3] = {0x62,0xc3,0xa6};
// b\xc3\xa6rbar
const buffer_unit_t const_31[7] = {0x62,0xc3,0xa6,0x72,0x62,0x61,0x72};
// c
const buffer_unit_t const_32[1] = {0x63};
// capslock
const buffer_unit_t const_33[8] = {0x63,0x61,0x70,0x73,0x6c,0x6f,0x63,0x6b};
// carriagereturn
const buffer_unit_t const_34[14] = {0x63,0x61,0x72,0x72,0x69,0x61,0x67,0x65,0x72,0x65,0x74,0x75,0x72,0x6e};
// central beregningsenhed
const buffer_unit_t const_35[23] = {0x63,0x65,0x6e,0x74,0x72,0x61,0x6c,0x20,0x62,0x65,0x72,0x65,0x67,0x6e,0x69,0x6e,0x67,0x73,0x65,0x6e,0x68,0x65,0x64};
// cloud
const buffer_unit_t const_36[5] = {0x63,0x6c,0x6f,0x75,0x64};
// compiler
const buffer_unit_t const_37[8] = {0x63,0x6f,0x6d,0x70,0x69,0x6c,0x65,0x72};
// computer
const buffer_unit_t const_38[8] = {0x63,0x6f,0x6d,0x70,0x75,0x74,0x65,0x72};
// cpu
const buffer_unit_t const_39[3] = {0x63,0x70,0x75};
// d
const buffer_unit_t const_40[1] = {0x64};
// damnit
const buffer_unit_t const_41[6] = {0x64,0x61,0x6d,0x6e,0x69,0x74};
// database
const buffer_unit_t const_42[8] = {0x64,0x61,0x74,0x61,0x62,0x61,0x73,0x65};
// datafon
const buffer_unit_t const_43[7] = {0x64,0x61,0x74,0x61,0x66,0x6f,0x6e};
// datalager
const buffer_unit_t const_44[9] = {0x64,0x61,0x74,0x61,0x6c,0x61,0x67,0x65,0x72};
// datamat
const buffer_unit_t const_45[7] = {0x64,0x61,0x74,0x61,0x6d,0x61,0x74};
// deadline
const buffer_unit_t const_46[8] = {0x64,0x65,0x61,0x64,0x6c,0x69,0x6e,0x65};
// debugger
const buffer_unit_t const_47[8] = {0x64,0x65,0x62,0x75,0x67,0x67,0x65,0x72};
// debugging
const buffer_unit_t const_48[9] = {0x64,0x65,0x62,0x75,0x67,0x67,0x69,0x6e,0x67};
// dejligt
const buffer_unit_t const_49[7] = {0x64,0x65,0x6a,0x6c,0x69,0x67,0x74};
// del og begr\xc3\xa6ns
const buffer_unit_t const_50[15] = {0x64,0x65,0x6c,0x20,0x6f,0x67,0x20,0x62,0x65,0x67,0x72,0xc3,0xa6,0x6e,0x73};
// del og hersk
const buffer_unit_t const_51[12] = {0x64,0x65,0x6c,0x20,0x6f,0x67,0x20,0x68,0x65,0x72,0x73,0x6b};
// divideandconquer
const buffer_unit_t const_52[16] = {0x64,0x69,0x76,0x69,0x64,0x65,0x61,0x6e,0x64,0x63,0x6f,0x6e,0x71,0x75,0x65,0x72};
// donut
const buffer_unit_t const_53[5] = {0x64,0x6f,0x6e,0x75,0x74};
// d\xc3\xb8dslinje
const buffer_unit_t const_54[10] = {0x64,0xc3,0xb8,0x64,0x73,0x6c,0x69,0x6e,0x6a,0x65};
// e
const buffer_unit_t const_55[1] = {0x65};
// e-
const buffer_unit_t const_56[2] = {0x65,0x2d};
// editor
const buffer_unit_t const_57[6] = {0x65,0x64,0x69,0x74,0x6f,0x72};
// elektropost
const buffer_unit_t const_58[11] = {0x65,0x6c,0x65,0x6b,0x74,0x72,0x6f,0x70,0x6f,0x73,0x74};
// em
const buffer_unit_t const_59[2] = {0x65,0x6d};
// email
const buffer_unit_t const_60[5] = {0x65,0x6d,0x61,0x69,0x6c};
// escape
const buffer_unit_t const_61[6] = {0x65,0x73,0x63,0x61,0x70,0x65};
// escapesequence
const buffer_unit_t const_62[14] = {0x65,0x73,0x63,0x61,0x70,0x65,0x73,0x65,0x71,0x75,0x65,0x6e,0x63,0x65};
// exception
const buffer_unit_t const_63[9] = {0x65,0x78,0x63,0x65,0x70,0x74,0x69,0x6f,0x6e};
// exploit
const buffer_unit_t const_64[7] = {0x65,0x78,0x70,0x6c,0x6f,0x69,0x74};
// f
const buffer_unit_t const_65[1] = {0x66};
// fail
const buffer_unit_t const_66[4] = {0x66,0x61,0x69,0x6c};
// fastpladelager
const buffer_unit_t const_67[14] = {0x66,0x61,0x73,0x74,0x70,0x6c,0x61,0x64,0x65,0x6c,0x61,0x67,0x65,0x72};
// fiasko
const buffer_unit_t const_68[6] = {0x66,0x69,0x61,0x73,0x6b,0x6f};
// firewall
const buffer_unit_t const_69[8] = {0x66,0x69,0x72,0x65,0x77,0x61,0x6c,0x6c};
// flet
const buffer_unit_t const_70[4] = {0x66,0x6c,0x65,0x74};
// flugttegn
const buffer_unit_t const_71[9] = {0x66,0x6c,0x75,0x67,0x74,0x74,0x65,0x67,0x6e};
// flygt
const buffer_unit_t const_72[5] = {0x66,0x6c,0x79,0x67,0x74};
// force
const buffer_unit_t const_73[5] = {0x66,0x6f,0x72,0x63,0x65};
// forkert
const buffer_unit_t const_74[7] = {0x66,0x6f,0x72,0x6b,0x65,0x72,0x74};
// fortolker
const buffer_unit_t const_75[9] = {0x66,0x6f,0x72,0x74,0x6f,0x6c,0x6b,0x65,0x72};
// freefood
const buffer_unit_t const_76[8] = {0x66,0x72,0x65,0x65,0x66,0x6f,0x6f,0x64};
// freesoftware
const buffer_unit_t const_77[12] = {0x66,0x72,0x65,0x65,0x73,0x6f,0x66,0x74,0x77,0x61,0x72,0x65};
// fri software
const buffer_unit_t const_78[12] = {0x66,0x72,0x69,0x20,0x73,0x6f,0x66,0x74,0x77,0x61,0x72,0x65};
// g
const buffer_unit_t const_79[1] = {0x67};
// garbagecollector
const buffer_unit_t const_80[16] = {0x67,0x61,0x72,0x62,0x61,0x67,0x65,0x63,0x6f,0x6c,0x6c,0x65,0x63,0x74,0x6f,0x72};
// gennemtving
const buffer_unit_t const_81[11] = {0x67,0x65,0x6e,0x6e,0x65,0x6d,0x74,0x76,0x69,0x6e,0x67};
// gitrepo
const buffer_unit_t const_82[7] = {0x67,0x69,0x74,0x72,0x65,0x70,0x6f};
// gitrepos
const buffer_unit_t const_83[8] = {0x67,0x69,0x74,0x72,0x65,0x70,0x6f,0x73};
// gitrepository
const buffer_unit_t const_84[13] = {0x67,0x69,0x74,0x72,0x65,0x70,0x6f,0x73,0x69,0x74,0x6f,0x72,0x79};
// gratis mad
const buffer_unit_t const_85[10] = {0x67,0x72,0x61,0x74,0x69,0x73,0x20,0x6d,0x61,0x64};
// greltegn
const buffer_unit_t const_86[8] = {0x67,0x72,0x65,0x6c,0x74,0x65,0x67,0x6e};
// grube
const buffer_unit_t const_87[5] = {0x67,0x72,0x75,0x62,0x65};
// gullaschfunktion
const buffer_unit_t const_88[16] = {0x67,0x75,0x6c,0x6c,0x61,0x73,0x63,0x68,0x66,0x75,0x6e,0x6b,0x74,0x69,0x6f,0x6e};
// gullashetiket
const buffer_unit_t const_89[13] = {0x67,0x75,0x6c,0x6c,0x61,0x73,0x68,0x65,0x74,0x69,0x6b,0x65,0x74};
// h
const buffer_unit_t const_90[1] = {0x68};
// hal
const buffer_unit_t const_91[3] = {0x68,0x61,0x6c};
// haleanmodning
const buffer_unit_t const_92[13] = {0x68,0x61,0x6c,0x65,0x61,0x6e,0x6d,0x6f,0x64,0x6e,0x69,0x6e,0x67};
// harddisk
const buffer_unit_t const_93[8] = {0x68,0x61,0x72,0x64,0x64,0x69,0x73,0x6b};
// hardware
const buffer_unit_t const_94[8] = {0x68,0x61,0x72,0x64,0x77,0x61,0x72,0x65};
// hashfunction
const buffer_unit_t const_95[12] = {0x68,0x61,0x73,0x68,0x66,0x75,0x6e,0x63,0x74,0x69,0x6f,0x6e};
// hashtag
const buffer_unit_t const_96[7] = {0x68,0x61,0x73,0x68,0x74,0x61,0x67};
// hjemmedatamat
const buffer_unit_t const_97[13] = {0x68,0x6a,0x65,0x6d,0x6d,0x65,0x64,0x61,0x74,0x61,0x6d,0x61,0x74};
// hjemmeside
const buffer_unit_t const_98[10] = {0x68,0x6a,0x65,0x6d,0x6d,0x65,0x73,0x69,0x64,0x65};
// hviletilstand
const buffer_unit_t const_99[13] = {0x68,0x76,0x69,0x6c,0x65,0x74,0x69,0x6c,0x73,0x74,0x61,0x6e,0x64};
// h\xc3\xa6gte
const buffer_unit_t const_100[6] = {0x68,0xc3,0xa6,0x67,0x74,0x65};
// i
const buffer_unit_t const_101[1] = {0x69};
// ikke
const buffer_unit_t const_102[4] = {0x69,0x6b,0x6b,0x65};
// interpreter
const buffer_unit_t const_103[11] = {0x69,0x6e,0x74,0x65,0x72,0x70,0x72,0x65,0x74,0x65,0x72};
// istrator
const buffer_unit_t const_104[8] = {0x69,0x73,0x74,0x72,0x61,0x74,0x6f,0x72};
// k
const buffer_unit_t const_105[1] = {0x6b};
// kapl\xc3\xb8bsstrid
const buffer_unit_t const_106[13] = {0x6b,0x61,0x70,0x6c,0xc3,0xb8,0x62,0x73,0x73,0x74,0x72,0x69,0x64};
// kartotek
const buffer_unit_t const_107[8] = {0x6b,0x61,0x72,0x74,0x6f,0x74,0x65,0x6b};
// keyboard
const buffer_unit_t const_108[8] = {0x6b,0x65,0x79,0x62,0x6f,0x61,0x72,0x64};
// kildetekst
const buffer_unit_t const_109[10] = {0x6b,0x69,0x6c,0x64,0x65,0x74,0x65,0x6b,0x73,0x74};
// klejntegn
const buffer_unit_t const_110[9] = {0x6b,0x6c,0x65,0x6a,0x6e,0x74,0x65,0x67,0x6e};
// konsol
const buffer_unit_t const_111[6] = {0x6b,0x6f,0x6e,0x73,0x6f,0x6c};
// kvidr
const buffer_unit_t const_112[5] = {0x6b,0x76,0x69,0x64,0x72};
// kvik
const buffer_unit_t const_113[4] = {0x6b,0x76,0x69,0x6b};
// l
const buffer_unit_t const_114[1] = {0x6c};
// laptop
const buffer_unit_t const_115[6] = {0x6c,0x61,0x70,0x74,0x6f,0x70};
// linjeskriver
const buffer_unit_t const_116[12] = {0x6c,0x69,0x6e,0x6a,0x65,0x73,0x6b,0x72,0x69,0x76,0x65,0x72};
// link
const buffer_unit_t const_117[4] = {0x6c,0x69,0x6e,0x6b};
// lowercase
const buffer_unit_t const_118[9] = {0x6c,0x6f,0x77,0x65,0x72,0x63,0x61,0x73,0x65};
// lus
const buffer_unit_t const_119[3] = {0x6c,0x75,0x73};
// l\xc3\xb8sen
const buffer_unit_t const_120[6] = {0x6c,0xc3,0xb8,0x73,0x65,0x6e};
// m
const buffer_unit_t const_121[1] = {0x6d};
// mail
const buffer_unit_t const_122[4] = {0x6d,0x61,0x69,0x6c};
// mappedatamat
const buffer_unit_t const_123[12] = {0x6d,0x61,0x70,0x70,0x65,0x64,0x61,0x74,0x61,0x6d,0x61,0x74};
// maskinel
const buffer_unit_t const_124[8] = {0x6d,0x61,0x73,0x6b,0x69,0x6e,0x65,0x6c};
// mem
const buffer_unit_t const_125[3] = {0x6d,0x65,0x6d};
// meme
const buffer_unit_t const_126[4] = {0x6d,0x65,0x6d,0x65};
// memer
const buffer_unit_t const_127[5] = {0x6d,0x65,0x6d,0x65,0x72};
// memes
const buffer_unit_t const_128[5] = {0x6d,0x65,0x6d,0x65,0x73};
// merge
const buffer_unit_t const_129[5] = {0x6d,0x65,0x72,0x67,0x65};
// munkering
const buffer_unit_t const_130[9] = {0x6d,0x75,0x6e,0x6b,0x65,0x72,0x69,0x6e,0x67};
// n
const buffer_unit_t const_131[1] = {0x6e};
// namespace
const buffer_unit_t const_132[9] = {0x6e,0x61,0x6d,0x65,0x73,0x70,0x61,0x63,0x65};
// navnerum
const buffer_unit_t const_133[8] = {0x6e,0x61,0x76,0x6e,0x65,0x72,0x75,0x6d};
// nice
const buffer_unit_t const_134[4] = {0x6e,0x69,0x63,0x65};
// not
const buffer_unit_t const_135[3] = {0x6e,0x6f,0x74};
// numlock
const buffer_unit_t const_136[7] = {0x6e,0x75,0x6d,0x6c,0x6f,0x63,0x6b};
// o
const buffer_unit_t const_137[1] = {0x6f};
// opdel
const buffer_unit_t const_138[5] = {0x6f,0x70,0x64,0x65,0x6c};
// opensource
const buffer_unit_t const_139[10] = {0x6f,0x70,0x65,0x6e,0x73,0x6f,0x75,0x72,0x63,0x65};
// overs\xc3\xa6tter
const buffer_unit_t const_140[11] = {0x6f,0x76,0x65,0x72,0x73,0xc3,0xa6,0x74,0x74,0x65,0x72};
// p
const buffer_unit_t const_141[1] = {0x70};
// partitioner
const buffer_unit_t const_142[11] = {0x70,0x61,0x72,0x74,0x69,0x74,0x69,0x6f,0x6e,0x65,0x72};
// password
const buffer_unit_t const_143[8] = {0x70,0x61,0x73,0x73,0x77,0x6f,0x72,0x64};
// pc
const buffer_unit_t const_144[2] = {0x70,0x63};
// pivot
const buffer_unit_t const_145[5] = {0x70,0x69,0x76,0x6f,0x74};
// plaintext
const buffer_unit_t const_146[9] = {0x70,0x6c,0x61,0x69,0x6e,0x74,0x65,0x78,0x74};
// prikkode
const buffer_unit_t const_147[8] = {0x70,0x72,0x69,0x6b,0x6b,0x6f,0x64,0x65};
// printer
const buffer_unit_t const_148[7] = {0x70,0x72,0x69,0x6e,0x74,0x65,0x72};
// programmel
const buffer_unit_t const_149[10] = {0x70,0x72,0x6f,0x67,0x72,0x61,0x6d,0x6d,0x65,0x6c};
// pull
const buffer_unit_t const_150[4] = {0x70,0x75,0x6c,0x6c};
// pullr
const buffer_unit_t const_151[5] = {0x70,0x75,0x6c,0x6c,0x72};
// pullrequest
const buffer_unit_t const_152[11] = {0x70,0x75,0x6c,0x6c,0x72,0x65,0x71,0x75,0x65,0x73,0x74};
// push
const buffer_unit_t const_153[4] = {0x70,0x75,0x73,0x68};
// q
const buffer_unit_t const_154[1] = {0x71};
// qrcode
const buffer_unit_t const_155[6] = {0x71,0x72,0x63,0x6f,0x64,0x65};
// quick
const buffer_unit_t const_156[5] = {0x71,0x75,0x69,0x63,0x6b};
// r
const buffer_unit_t const_157[1] = {0x72};
// racecondition
const buffer_unit_t const_158[13] = {0x72,0x61,0x63,0x65,0x63,0x6f,0x6e,0x64,0x69,0x74,0x69,0x6f,0x6e};
// ram
const buffer_unit_t const_159[3] = {0x72,0x61,0x6d};
// rbar
const buffer_unit_t const_160[4] = {0x72,0x62,0x61,0x72};
// rbare
const buffer_unit_t const_161[5] = {0x72,0x62,0x61,0x72,0x65};
// repository
const buffer_unit_t const_162[10] = {0x72,0x65,0x70,0x6f,0x73,0x69,0x74,0x6f,0x72,0x79};
// request
const buffer_unit_t const_163[7] = {0x72,0x65,0x71,0x75,0x65,0x73,0x74};
// r\xc3\xa5tekst
const buffer_unit_t const_164[8] = {0x72,0xc3,0xa5,0x74,0x65,0x6b,0x73,0x74};
// s
const buffer_unit_t const_165[1] = {0x73};
// scope
const buffer_unit_t const_166[5] = {0x73,0x63,0x6f,0x70,0x65};
// screenshot
const buffer_unit_t const_167[10] = {0x73,0x63,0x72,0x65,0x65,0x6e,0x73,0x68,0x6f,0x74};
// seed
const buffer_unit_t const_168[4] = {0x73,0x65,0x65,0x64};
// shell
const buffer_unit_t const_169[5] = {0x73,0x68,0x65,0x6c,0x6c};
// shellscript
const buffer_unit_t const_170[11] = {0x73,0x68,0x65,0x6c,0x6c,0x73,0x63,0x72,0x69,0x70,0x74};
// sikkerhedskopi
const buffer_unit_t const_171[14] = {0x73,0x69,0x6b,0x6b,0x65,0x72,0x68,0x65,0x64,0x73,0x6b,0x6f,0x70,0x69};
// site
const buffer_unit_t const_172[4] = {0x73,0x69,0x74,0x65};
// sitory
const buffer_unit_t const_173[6] = {0x73,0x69,0x74,0x6f,0x72,0x79};
// skalprogram
const buffer_unit_t const_174[11] = {0x73,0x6b,0x61,0x6c,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d};
// skiftel\xc3\xa5s
const buffer_unit_t const_175[10] = {0x73,0x6b,0x69,0x66,0x74,0x65,0x6c,0xc3,0xa5,0x73};
// skub
const buffer_unit_t const_176[4] = {0x73,0x6b,0x75,0x62};
// skvatgrube
const buffer_unit_t const_177[10] = {0x73,0x6b,0x76,0x61,0x74,0x67,0x72,0x75,0x62,0x65};
// sky
const buffer_unit_t const_178[3] = {0x73,0x6b,0x79};
// sk\xc3\xa6rmbillede
const buffer_unit_t const_179[13] = {0x73,0x6b,0xc3,0xa6,0x72,0x6d,0x62,0x69,0x6c,0x6c,0x65,0x64,0x65};
// smartphone
const buffer_unit_t const_180[10] = {0x73,0x6d,0x61,0x72,0x74,0x70,0x68,0x6f,0x6e,0x65};
// software
const buffer_unit_t const_181[8] = {0x73,0x6f,0x66,0x74,0x77,0x61,0x72,0x65};
// sourcecode
const buffer_unit_t const_182[10] = {0x73,0x6f,0x75,0x72,0x63,0x65,0x63,0x6f,0x64,0x65};
// spildopsamler
const buffer_unit_t const_183[13] = {0x73,0x70,0x69,0x6c,0x64,0x6f,0x70,0x73,0x61,0x6d,0x6c,0x65,0x72};
// spindel
const buffer_unit_t const_184[7] = {0x73,0x70,0x69,0x6e,0x64,0x65,0x6c};
// spindell\xc3\xa6ser
const buffer_unit_t const_185[13] = {0x73,0x70,0x69,0x6e,0x64,0x65,0x6c,0x6c,0xc3,0xa6,0x73,0x65,0x72};
// standby
const buffer_unit_t const_186[7] = {0x73,0x74,0x61,0x6e,0x64,0x62,0x79};
// storage
const buffer_unit_t const_187[7] = {0x73,0x74,0x6f,0x72,0x61,0x67,0x65};
// supercomputer
const buffer_unit_t const_188[13] = {0x73,0x75,0x70,0x65,0x72,0x63,0x6f,0x6d,0x70,0x75,0x74,0x65,0x72};
// svingtap
const buffer_unit_t const_189[8] = {0x73,0x76,0x69,0x6e,0x67,0x74,0x61,0x70};
// t
const buffer_unit_t const_190[1] = {0x74};
// tablet
const buffer_unit_t const_191[6] = {0x74,0x61,0x62,0x6c,0x65,0x74};
// tall\xc3\xa5s
const buffer_unit_t const_192[7] = {0x74,0x61,0x6c,0x6c,0xc3,0xa5,0x73};
// tastatur
const buffer_unit_t const_193[8] = {0x74,0x61,0x73,0x74,0x61,0x74,0x75,0x72};
// tavledatamat
const buffer_unit_t const_194[12] = {0x74,0x61,0x76,0x6c,0x65,0x64,0x61,0x74,0x61,0x6d,0x61,0x74};
// teditor
const buffer_unit_t const_195[7] = {0x74,0x65,0x64,0x69,0x74,0x6f,0x72};
// tekstredigeringsprogram
const buffer_unit_t const_196[23] = {0x74,0x65,0x6b,0x73,0x74,0x72,0x65,0x64,0x69,0x67,0x65,0x72,0x69,0x6e,0x67,0x73,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d};
// tex
const buffer_unit_t const_197[3] = {0x74,0x65,0x78};
// texe
const buffer_unit_t const_198[4] = {0x74,0x65,0x78,0x65};
// texeditor
const buffer_unit_t const_199[9] = {0x74,0x65,0x78,0x65,0x64,0x69,0x74,0x6f,0x72};
// text
const buffer_unit_t const_200[4] = {0x74,0x65,0x78,0x74};
// texteditor
const buffer_unit_t const_201[10] = {0x74,0x65,0x78,0x74,0x65,0x64,0x69,0x74,0x6f,0x72};
// topdatamat
const buffer_unit_t const_202[10] = {0x74,0x6f,0x70,0x64,0x61,0x74,0x61,0x6d,0x61,0x74};
// tupel
const buffer_unit_t const_203[5] = {0x74,0x75,0x70,0x65,0x6c};
// tuple
const buffer_unit_t const_204[5] = {0x74,0x75,0x70,0x6c,0x65};
// tweet
const buffer_unit_t const_205[5] = {0x74,0x77,0x65,0x65,0x74};
// u
const buffer_unit_t const_206[1] = {0x75};
// udgave
const buffer_unit_t const_207[6] = {0x75,0x64,0x67,0x61,0x76,0x65};
// udnyttelse
const buffer_unit_t const_208[10] = {0x75,0x64,0x6e,0x79,0x74,0x74,0x65,0x6c,0x73,0x65};
// undtagelse
const buffer_unit_t const_209[10] = {0x75,0x6e,0x64,0x74,0x61,0x67,0x65,0x6c,0x73,0x65};
// uppercase
const buffer_unit_t const_210[9] = {0x75,0x70,0x70,0x65,0x72,0x63,0x61,0x73,0x65};
// v
const buffer_unit_t const_211[1] = {0x76};
// version
const buffer_unit_t const_212[7] = {0x76,0x65,0x72,0x73,0x69,0x6f,0x6e};
// virkefelt
const buffer_unit_t const_213[9] = {0x76,0x69,0x72,0x6b,0x65,0x66,0x65,0x6c,0x74};
// vognretur
const buffer_unit_t const_214[9] = {0x76,0x6f,0x67,0x6e,0x72,0x65,0x74,0x75,0x72};
// w
const buffer_unit_t const_215[1] = {0x77};
// we
const buffer_unit_t const_216[2] = {0x77,0x65};
// web
const buffer_unit_t const_217[3] = {0x77,0x65,0x62};
// webbrowser
const buffer_unit_t const_218[10] = {0x77,0x65,0x62,0x62,0x72,0x6f,0x77,0x73,0x65,0x72};
// webs
const buffer_unit_t const_219[4] = {0x77,0x65,0x62,0x73};
// website
const buffer_unit_t const_220[7] = {0x77,0x65,0x62,0x73,0x69,0x74,0x65};
// wes
const buffer_unit_t const_221[3] = {0x77,0x65,0x73};
// wesite
const buffer_unit_t const_222[6] = {0x77,0x65,0x73,0x69,0x74,0x65};
// whitespace
const buffer_unit_t const_223[10] = {0x77,0x68,0x69,0x74,0x65,0x73,0x70,0x61,0x63,0x65};
// wrong
const buffer_unit_t const_224[5] = {0x77,0x72,0x6f,0x6e,0x67};
// \xc3\xa5bent stads
const buffer_unit_t const_225[12] = {0xc3,0xa5,0x62,0x65,0x6e,0x74,0x20,0x73,0x74,0x61,0x64,0x73};
void printCompilationInfo()
{
  fprintf(stdout, "No object file generated!\nOptions:\nSST optimization level: 3\nWord size:              UInt8T\n\nTime:        2015-03-31 13:31:23.625482 UTC\nSource file: misc/makeDanish.kex\nSource md5:  2f9905d1fae1c398f21e73b3b1f9b35b\nSST states:  30\n");
}

void init()
{
init_buffer(&buf_1);
init_buffer(&buf_2);
init_buffer(&buf_3);
init_buffer(&buf_4);
init_buffer(&buf_5);
init_buffer(&buf_6);
init_buffer(&buf_7);
init_buffer(&buf_8);
init_buffer(&buf_9);
}

void match()
{
  int i = 0;
goto l0;
l0: if (!readnext(1, 16))
    {
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l4;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
    {
       reset(&buf_5);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'D') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_3,48);
          reset(&buf_6);
          appendarray(&buf_6,const_3,48);
          reset(&buf_3);
          appendarray(&buf_3,const_4,96);
          reset(&buf_2);
          appendarray(&buf_2,const_4,96);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_2,8);
       reset(&buf_1);
       appendarray(&buf_1,const_2,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'P') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_6,40);
          reset(&buf_6);
          appendarray(&buf_6,const_6,40);
          reset(&buf_3);
          appendarray(&buf_3,const_7,32);
          reset(&buf_2);
          appendarray(&buf_2,const_7,32);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_5,8);
       reset(&buf_1);
       appendarray(&buf_1,const_5,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'W') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_10,48);
          reset(&buf_6);
          appendarray(&buf_6,const_10,48);
          reset(&buf_3);
          appendarray(&buf_3,const_8,48);
          reset(&buf_2);
          appendarray(&buf_2,const_8,48);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_9,8);
       reset(&buf_1);
       appendarray(&buf_1,const_9,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'a') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
       {
          consume(5);
          goto l26;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_11,8);
       reset(&buf_1);
       appendarray(&buf_1,const_11,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'b') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
       {
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_20,96);
             reset(&buf_6);
             appendarray(&buf_6,const_20,96);
             reset(&buf_3);
             appendarray(&buf_3,const_25,136);
             reset(&buf_2);
             appendarray(&buf_2,const_25,136);
             consume(12);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_21,48);
             reset(&buf_6);
             appendarray(&buf_6,const_21,48);
             reset(&buf_3);
             appendarray(&buf_3,const_171,112);
             reset(&buf_2);
             appendarray(&buf_2,const_171,112);
             consume(6);
             goto l6;
          }
       }
       if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_26,112);
          reset(&buf_6);
          appendarray(&buf_6,const_26,112);
          reset(&buf_3);
          appendarray(&buf_3,const_50,120);
          reset(&buf_2);
          appendarray(&buf_2,const_50,120);
          consume(14);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_29,24);
          reset(&buf_6);
          appendarray(&buf_6,const_29,24);
          reset(&buf_3);
          appendarray(&buf_3,const_119,24);
          reset(&buf_2);
          appendarray(&buf_2,const_119,24);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
       {
          consume(7);
          goto l24;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_19,8);
       reset(&buf_1);
       appendarray(&buf_1,const_19,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'c') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_33,64);
             reset(&buf_6);
             appendarray(&buf_6,const_33,64);
             reset(&buf_3);
             appendarray(&buf_3,const_175,80);
             reset(&buf_2);
             appendarray(&buf_2,const_175,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_34,112);
             reset(&buf_6);
             appendarray(&buf_6,const_34,112);
             reset(&buf_3);
             appendarray(&buf_3,const_214,72);
             reset(&buf_2);
             appendarray(&buf_2,const_214,72);
             consume(14);
             goto l6;
          }
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_36,40);
          reset(&buf_6);
          appendarray(&buf_6,const_36,40);
          reset(&buf_3);
          appendarray(&buf_3,const_178,24);
          reset(&buf_2);
          appendarray(&buf_2,const_178,24);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_37,64);
             reset(&buf_6);
             appendarray(&buf_6,const_37,64);
             reset(&buf_3);
             appendarray(&buf_3,const_140,88);
             reset(&buf_2);
             appendarray(&buf_2,const_140,88);
             consume(8);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_38,64);
             reset(&buf_6);
             appendarray(&buf_6,const_38,64);
             reset(&buf_3);
             appendarray(&buf_3,const_45,56);
             reset(&buf_2);
             appendarray(&buf_2,const_45,56);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_39,24);
          reset(&buf_6);
          appendarray(&buf_6,const_39,24);
          reset(&buf_3);
          appendarray(&buf_3,const_35,184);
          reset(&buf_2);
          appendarray(&buf_2,const_35,184);
          consume(3);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_32,8);
       reset(&buf_1);
       appendarray(&buf_1,const_32,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'd') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_41,48);
             reset(&buf_6);
             appendarray(&buf_6,const_41,48);
             reset(&buf_3);
             appendarray(&buf_3,const_4,96);
             reset(&buf_2);
             appendarray(&buf_2,const_4,96);
             consume(6);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_42,64);
             reset(&buf_6);
             appendarray(&buf_6,const_42,64);
             reset(&buf_3);
             appendarray(&buf_3,const_107,64);
             reset(&buf_2);
             appendarray(&buf_2,const_107,64);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_46,64);
             reset(&buf_6);
             appendarray(&buf_6,const_46,64);
             reset(&buf_3);
             appendarray(&buf_3,const_54,80);
             reset(&buf_2);
             appendarray(&buf_2,const_54,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_47,64);
                reset(&buf_6);
                appendarray(&buf_6,const_47,64);
                reset(&buf_3);
                appendarray(&buf_3,const_15,56);
                reset(&buf_2);
                appendarray(&buf_2,const_15,56);
                consume(8);
                goto l6;
             }
             if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_48,72);
                reset(&buf_6);
                appendarray(&buf_6,const_48,72);
                reset(&buf_3);
                appendarray(&buf_3,const_16,72);
                reset(&buf_2);
                appendarray(&buf_2,const_16,72);
                consume(9);
                goto l6;
             }
          }
       }
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_52,128);
          reset(&buf_6);
          appendarray(&buf_6,const_52,128);
          reset(&buf_3);
          appendarray(&buf_3,const_51,96);
          reset(&buf_2);
          appendarray(&buf_2,const_51,96);
          consume(16);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_53,40);
          reset(&buf_6);
          appendarray(&buf_6,const_53,40);
          reset(&buf_3);
          appendarray(&buf_3,const_130,72);
          reset(&buf_2);
          appendarray(&buf_2,const_130,72);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_40,8);
       reset(&buf_1);
       appendarray(&buf_1,const_40,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'e') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_57,48);
          reset(&buf_6);
          appendarray(&buf_6,const_57,48);
          reset(&buf_3);
          appendarray(&buf_3,const_196,184);
          reset(&buf_2);
          appendarray(&buf_2,const_196,184);
          consume(6);
          goto l6;
       }
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
       {
          if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_62,112);
             reset(&buf_6);
             appendarray(&buf_6,const_62,112);
             reset(&buf_3);
             appendarray(&buf_3,const_71,72);
             reset(&buf_2);
             appendarray(&buf_2,const_71,72);
             consume(14);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_61,48);
          reset(&buf_6);
          appendarray(&buf_6,const_61,48);
          reset(&buf_3);
          appendarray(&buf_3,const_72,40);
          reset(&buf_2);
          appendarray(&buf_2,const_72,40);
          consume(6);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'x') && 1)))
       {
          if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_63,72);
             reset(&buf_6);
             appendarray(&buf_6,const_63,72);
             reset(&buf_3);
             appendarray(&buf_3,const_209,80);
             reset(&buf_2);
             appendarray(&buf_2,const_209,80);
             consume(9);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_64,56);
             reset(&buf_6);
             appendarray(&buf_6,const_64,56);
             reset(&buf_3);
             appendarray(&buf_3,const_208,80);
             reset(&buf_2);
             appendarray(&buf_2,const_208,80);
             consume(7);
             goto l6;
          }
       }
       consume(1);
       goto l20;
    }
    if (((avail >= 1) && ((next[0] == 'f') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_66,32);
          reset(&buf_6);
          appendarray(&buf_6,const_66,32);
          reset(&buf_3);
          appendarray(&buf_3,const_68,48);
          reset(&buf_2);
          appendarray(&buf_2,const_68,48);
          consume(4);
          goto l6;
       }
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_69,64);
          reset(&buf_6);
          appendarray(&buf_6,const_69,64);
          reset(&buf_3);
          appendarray(&buf_3,const_27,64);
          reset(&buf_2);
          appendarray(&buf_2,const_27,64);
          consume(8);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_73,40);
          reset(&buf_6);
          appendarray(&buf_6,const_73,40);
          reset(&buf_3);
          appendarray(&buf_3,const_81,88);
          reset(&buf_2);
          appendarray(&buf_2,const_81,88);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_76,64);
             reset(&buf_6);
             appendarray(&buf_6,const_76,64);
             reset(&buf_3);
             appendarray(&buf_3,const_85,80);
             reset(&buf_2);
             appendarray(&buf_2,const_85,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_77,96);
             reset(&buf_6);
             appendarray(&buf_6,const_77,96);
             reset(&buf_3);
             appendarray(&buf_3,const_78,96);
             reset(&buf_2);
             appendarray(&buf_2,const_78,96);
             consume(12);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_65,8);
       reset(&buf_1);
       appendarray(&buf_1,const_65,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'g') && 1)))
    {
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_80,128);
          reset(&buf_6);
          appendarray(&buf_6,const_80,128);
          reset(&buf_3);
          appendarray(&buf_3,const_183,104);
          reset(&buf_2);
          appendarray(&buf_2,const_183,104);
          consume(16);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
       {
          consume(7);
          goto l25;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_79,8);
       reset(&buf_1);
       appendarray(&buf_1,const_79,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'h') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_93,64);
                reset(&buf_6);
                appendarray(&buf_6,const_93,64);
                reset(&buf_3);
                appendarray(&buf_3,const_67,112);
                reset(&buf_2);
                appendarray(&buf_2,const_67,112);
                consume(8);
                goto l6;
             }
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_94,64);
                reset(&buf_6);
                appendarray(&buf_6,const_94,64);
                reset(&buf_3);
                appendarray(&buf_3,const_124,64);
                reset(&buf_2);
                appendarray(&buf_2,const_124,64);
                consume(8);
                goto l6;
             }
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_95,96);
                reset(&buf_6);
                appendarray(&buf_6,const_95,96);
                reset(&buf_3);
                appendarray(&buf_3,const_88,128);
                reset(&buf_2);
                appendarray(&buf_2,const_88,128);
                consume(12);
                goto l6;
             }
             if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_96,56);
                reset(&buf_6);
                appendarray(&buf_6,const_96,56);
                reset(&buf_3);
                appendarray(&buf_3,const_89,104);
                reset(&buf_2);
                appendarray(&buf_2,const_89,104);
                consume(7);
                goto l6;
             }
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_90,8);
       reset(&buf_1);
       appendarray(&buf_1,const_90,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'i') && 1)))
    {
       if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_103,88);
          reset(&buf_6);
          appendarray(&buf_6,const_103,88);
          reset(&buf_3);
          appendarray(&buf_3,const_75,72);
          reset(&buf_2);
          appendarray(&buf_2,const_75,72);
          consume(11);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_101,8);
       reset(&buf_1);
       appendarray(&buf_1,const_101,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'k') && 1)))
    {
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_108,64);
          reset(&buf_6);
          appendarray(&buf_6,const_108,64);
          reset(&buf_3);
          appendarray(&buf_3,const_193,64);
          reset(&buf_2);
          appendarray(&buf_2,const_193,64);
          consume(8);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_105,8);
       reset(&buf_1);
       appendarray(&buf_1,const_105,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'l') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_115,48);
          reset(&buf_6);
          appendarray(&buf_6,const_115,48);
          reset(&buf_3);
          appendarray(&buf_3,const_123,96);
          reset(&buf_2);
          appendarray(&buf_2,const_123,96);
          consume(6);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_117,32);
          reset(&buf_6);
          appendarray(&buf_6,const_117,32);
          reset(&buf_3);
          appendarray(&buf_3,const_100,48);
          reset(&buf_2);
          appendarray(&buf_2,const_100,48);
          consume(4);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_118,72);
          reset(&buf_6);
          appendarray(&buf_6,const_118,72);
          reset(&buf_3);
          appendarray(&buf_3,const_110,72);
          reset(&buf_2);
          appendarray(&buf_2,const_110,72);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_114,8);
       reset(&buf_1);
       appendarray(&buf_1,const_114,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'm') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
          {
             if (((avail >= 5) && ((next[4] == 's') && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_128,40);
                reset(&buf_6);
                appendarray(&buf_6,const_128,40);
                reset(&buf_3);
                appendarray(&buf_3,const_127,40);
                reset(&buf_2);
                appendarray(&buf_2,const_127,40);
                consume(5);
                goto l6;
             }
             reset(&buf_7);
             appendarray(&buf_7,const_126,32);
             reset(&buf_6);
             appendarray(&buf_6,const_126,32);
             reset(&buf_3);
             appendarray(&buf_3,const_125,24);
             reset(&buf_2);
             appendarray(&buf_2,const_125,24);
             consume(4);
             goto l6;
          }
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_129,40);
             reset(&buf_6);
             appendarray(&buf_6,const_129,40);
             reset(&buf_3);
             appendarray(&buf_3,const_70,32);
             reset(&buf_2);
             appendarray(&buf_2,const_70,32);
             consume(5);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_121,8);
       reset(&buf_1);
       appendarray(&buf_1,const_121,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'n') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_132,72);
          reset(&buf_6);
          appendarray(&buf_6,const_132,72);
          reset(&buf_3);
          appendarray(&buf_3,const_133,64);
          reset(&buf_2);
          appendarray(&buf_2,const_133,64);
          consume(9);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_134,32);
          reset(&buf_6);
          appendarray(&buf_6,const_134,32);
          reset(&buf_3);
          appendarray(&buf_3,const_49,56);
          reset(&buf_2);
          appendarray(&buf_2,const_49,56);
          consume(4);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_135,24);
          reset(&buf_6);
          appendarray(&buf_6,const_135,24);
          reset(&buf_3);
          appendarray(&buf_3,const_102,32);
          reset(&buf_2);
          appendarray(&buf_2,const_102,32);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_136,56);
          reset(&buf_6);
          appendarray(&buf_6,const_136,56);
          reset(&buf_3);
          appendarray(&buf_3,const_192,56);
          reset(&buf_2);
          appendarray(&buf_2,const_192,56);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_131,8);
       reset(&buf_1);
       appendarray(&buf_1,const_131,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'o') && 1)))
    {
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_139,80);
          reset(&buf_6);
          appendarray(&buf_6,const_139,80);
          reset(&buf_3);
          appendarray(&buf_3,const_225,96);
          reset(&buf_2);
          appendarray(&buf_2,const_225,96);
          consume(10);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_137,8);
       reset(&buf_1);
       appendarray(&buf_1,const_137,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'p') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_142,88);
             reset(&buf_6);
             appendarray(&buf_6,const_142,88);
             reset(&buf_3);
             appendarray(&buf_3,const_138,40);
             reset(&buf_2);
             appendarray(&buf_2,const_138,40);
             consume(11);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_143,64);
             reset(&buf_6);
             appendarray(&buf_6,const_143,64);
             reset(&buf_3);
             appendarray(&buf_3,const_120,48);
             reset(&buf_2);
             appendarray(&buf_2,const_120,48);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_144,16);
          reset(&buf_6);
          appendarray(&buf_6,const_144,16);
          reset(&buf_3);
          appendarray(&buf_3,const_97,104);
          reset(&buf_2);
          appendarray(&buf_2,const_97,104);
          consume(2);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_145,40);
          reset(&buf_6);
          appendarray(&buf_6,const_145,40);
          reset(&buf_3);
          appendarray(&buf_3,const_189,64);
          reset(&buf_2);
          appendarray(&buf_2,const_189,64);
          consume(5);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_146,72);
          reset(&buf_6);
          appendarray(&buf_6,const_146,72);
          reset(&buf_3);
          appendarray(&buf_3,const_164,64);
          reset(&buf_2);
          appendarray(&buf_2,const_164,64);
          consume(9);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_148,56);
          reset(&buf_6);
          appendarray(&buf_6,const_148,56);
          reset(&buf_3);
          appendarray(&buf_3,const_116,96);
          reset(&buf_2);
          appendarray(&buf_2,const_116,96);
          consume(7);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'u') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
          {
             consume(4);
             goto l21;
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_153,32);
             reset(&buf_6);
             appendarray(&buf_6,const_153,32);
             reset(&buf_3);
             appendarray(&buf_3,const_176,32);
             reset(&buf_2);
             appendarray(&buf_2,const_176,32);
             consume(4);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_141,8);
       reset(&buf_1);
       appendarray(&buf_1,const_141,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'q') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_155,48);
          reset(&buf_6);
          appendarray(&buf_6,const_155,48);
          reset(&buf_3);
          appendarray(&buf_3,const_147,64);
          reset(&buf_2);
          appendarray(&buf_2,const_147,64);
          consume(6);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_156,40);
          reset(&buf_6);
          appendarray(&buf_6,const_156,40);
          reset(&buf_3);
          appendarray(&buf_3,const_113,32);
          reset(&buf_2);
          appendarray(&buf_2,const_113,32);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_154,8);
       reset(&buf_1);
       appendarray(&buf_1,const_154,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'r') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_158,104);
             reset(&buf_6);
             appendarray(&buf_6,const_158,104);
             reset(&buf_3);
             appendarray(&buf_3,const_106,104);
             reset(&buf_2);
             appendarray(&buf_2,const_106,104);
             consume(13);
             goto l6;
          }
          if (((avail >= 3) && ((next[2] == 'm') && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_159,24);
             reset(&buf_6);
             appendarray(&buf_6,const_159,24);
             reset(&buf_3);
             appendarray(&buf_3,const_18,96);
             reset(&buf_2);
             appendarray(&buf_2,const_18,96);
             consume(3);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_162,80);
             reset(&buf_6);
             appendarray(&buf_6,const_162,80);
             reset(&buf_3);
             appendarray(&buf_3,const_87,40);
             reset(&buf_2);
             appendarray(&buf_2,const_87,40);
             consume(10);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_163,56);
             reset(&buf_6);
             appendarray(&buf_6,const_163,56);
             reset(&buf_3);
             appendarray(&buf_3,const_17,72);
             reset(&buf_2);
             appendarray(&buf_2,const_17,72);
             consume(7);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_157,8);
       reset(&buf_1);
       appendarray(&buf_1,const_157,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 's') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_166,40);
             reset(&buf_6);
             appendarray(&buf_6,const_166,40);
             reset(&buf_3);
             appendarray(&buf_3,const_213,72);
             reset(&buf_2);
             appendarray(&buf_2,const_213,72);
             consume(5);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_167,80);
             reset(&buf_6);
             appendarray(&buf_6,const_167,80);
             reset(&buf_3);
             appendarray(&buf_3,const_179,104);
             reset(&buf_2);
             appendarray(&buf_2,const_179,104);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_168,32);
          reset(&buf_6);
          appendarray(&buf_6,const_168,32);
          reset(&buf_3);
          appendarray(&buf_3,const_23,88);
          reset(&buf_2);
          appendarray(&buf_2,const_23,88);
          consume(4);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
       {
          if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_170,88);
             reset(&buf_6);
             appendarray(&buf_6,const_170,88);
             reset(&buf_3);
             appendarray(&buf_3,const_174,88);
             reset(&buf_2);
             appendarray(&buf_2,const_174,88);
             consume(11);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_169,40);
          reset(&buf_6);
          appendarray(&buf_6,const_169,40);
          reset(&buf_3);
          appendarray(&buf_3,const_111,48);
          reset(&buf_2);
          appendarray(&buf_2,const_111,48);
          consume(5);
          goto l6;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_180,80);
          reset(&buf_6);
          appendarray(&buf_6,const_180,80);
          reset(&buf_3);
          appendarray(&buf_3,const_43,56);
          reset(&buf_2);
          appendarray(&buf_2,const_43,56);
          consume(10);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'o') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_181,64);
             reset(&buf_6);
             appendarray(&buf_6,const_181,64);
             reset(&buf_3);
             appendarray(&buf_3,const_149,80);
             reset(&buf_2);
             appendarray(&buf_2,const_149,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_182,80);
             reset(&buf_6);
             appendarray(&buf_6,const_182,80);
             reset(&buf_3);
             appendarray(&buf_3,const_109,80);
             reset(&buf_2);
             appendarray(&buf_2,const_109,80);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 't') && 1)))
       {
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_186,56);
             reset(&buf_6);
             appendarray(&buf_6,const_186,56);
             reset(&buf_3);
             appendarray(&buf_3,const_99,104);
             reset(&buf_2);
             appendarray(&buf_2,const_99,104);
             consume(7);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_187,56);
             reset(&buf_6);
             appendarray(&buf_6,const_187,56);
             reset(&buf_3);
             appendarray(&buf_3,const_44,72);
             reset(&buf_2);
             appendarray(&buf_2,const_44,72);
             consume(7);
             goto l6;
          }
       }
       if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_188,104);
          reset(&buf_6);
          appendarray(&buf_6,const_188,104);
          reset(&buf_3);
          appendarray(&buf_3,const_202,80);
          reset(&buf_2);
          appendarray(&buf_2,const_202,80);
          consume(13);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_165,8);
       reset(&buf_1);
       appendarray(&buf_1,const_165,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 't') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_191,48);
          reset(&buf_6);
          appendarray(&buf_6,const_191,48);
          reset(&buf_3);
          appendarray(&buf_3,const_194,96);
          reset(&buf_2);
          appendarray(&buf_2,const_194,96);
          consume(6);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
       {
          consume(3);
          goto l23;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_204,40);
          reset(&buf_6);
          appendarray(&buf_6,const_204,40);
          reset(&buf_3);
          appendarray(&buf_3,const_203,40);
          reset(&buf_2);
          appendarray(&buf_2,const_203,40);
          consume(5);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_205,40);
          reset(&buf_6);
          appendarray(&buf_6,const_205,40);
          reset(&buf_3);
          appendarray(&buf_3,const_112,40);
          reset(&buf_2);
          appendarray(&buf_2,const_112,40);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_190,8);
       reset(&buf_1);
       appendarray(&buf_1,const_190,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'u') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_210,72);
          reset(&buf_6);
          appendarray(&buf_6,const_210,72);
          reset(&buf_3);
          appendarray(&buf_3,const_86,64);
          reset(&buf_2);
          appendarray(&buf_2,const_86,64);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_206,8);
       reset(&buf_1);
       appendarray(&buf_1,const_206,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'v') && 1)))
    {
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_212,56);
          reset(&buf_6);
          appendarray(&buf_6,const_212,56);
          reset(&buf_3);
          appendarray(&buf_3,const_207,48);
          reset(&buf_2);
          appendarray(&buf_2,const_207,48);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_211,8);
       reset(&buf_1);
       appendarray(&buf_1,const_211,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'w') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 3) && ((next[2] == 'b') && 1)))
          {
             if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_218,80);
                reset(&buf_6);
                appendarray(&buf_6,const_218,80);
                reset(&buf_3);
                appendarray(&buf_3,const_185,104);
                reset(&buf_2);
                appendarray(&buf_2,const_185,104);
                consume(10);
                goto l6;
             }
             consume(3);
             goto l7;
          }
          consume(2);
          goto l22;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_223,80);
          reset(&buf_6);
          appendarray(&buf_6,const_223,80);
          reset(&buf_3);
          appendarray(&buf_3,const_24,72);
          reset(&buf_2);
          appendarray(&buf_2,const_24,72);
          consume(10);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_224,40);
          reset(&buf_6);
          appendarray(&buf_6,const_224,40);
          reset(&buf_3);
          appendarray(&buf_3,const_74,56);
          reset(&buf_2);
          appendarray(&buf_2,const_74,56);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_215,8);
       reset(&buf_1);
       appendarray(&buf_1,const_215,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_7);
       append(&buf_7,tbl[0][next[0]],8);
       reset(&buf_3);
       append(&buf_3,tbl[0][next[0]],8);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l18;
    }
    goto fail;
l1: if (!readnext(1, 16))
    {
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
    {
       reset(&buf_5);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'D') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_3,48);
          reset(&buf_6);
          appendarray(&buf_6,const_3,48);
          reset(&buf_3);
          appendarray(&buf_3,const_4,96);
          reset(&buf_2);
          appendarray(&buf_2,const_4,96);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_2,8);
       reset(&buf_1);
       appendarray(&buf_1,const_2,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'P') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_6,40);
          reset(&buf_6);
          appendarray(&buf_6,const_6,40);
          reset(&buf_3);
          appendarray(&buf_3,const_7,32);
          reset(&buf_2);
          appendarray(&buf_2,const_7,32);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_5,8);
       reset(&buf_1);
       appendarray(&buf_1,const_5,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'W') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_10,48);
          reset(&buf_6);
          appendarray(&buf_6,const_10,48);
          reset(&buf_3);
          appendarray(&buf_3,const_8,48);
          reset(&buf_2);
          appendarray(&buf_2,const_8,48);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_9,8);
       reset(&buf_1);
       appendarray(&buf_1,const_9,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'a') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
       {
          consume(5);
          goto l26;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_11,8);
       reset(&buf_1);
       appendarray(&buf_1,const_11,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'b') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
       {
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_20,96);
             reset(&buf_6);
             appendarray(&buf_6,const_20,96);
             reset(&buf_3);
             appendarray(&buf_3,const_25,136);
             reset(&buf_2);
             appendarray(&buf_2,const_25,136);
             consume(12);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_21,48);
             reset(&buf_6);
             appendarray(&buf_6,const_21,48);
             reset(&buf_3);
             appendarray(&buf_3,const_171,112);
             reset(&buf_2);
             appendarray(&buf_2,const_171,112);
             consume(6);
             goto l6;
          }
       }
       if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_26,112);
          reset(&buf_6);
          appendarray(&buf_6,const_26,112);
          reset(&buf_3);
          appendarray(&buf_3,const_50,120);
          reset(&buf_2);
          appendarray(&buf_2,const_50,120);
          consume(14);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_29,24);
          reset(&buf_6);
          appendarray(&buf_6,const_29,24);
          reset(&buf_3);
          appendarray(&buf_3,const_119,24);
          reset(&buf_2);
          appendarray(&buf_2,const_119,24);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
       {
          consume(7);
          goto l24;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_19,8);
       reset(&buf_1);
       appendarray(&buf_1,const_19,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'c') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_33,64);
             reset(&buf_6);
             appendarray(&buf_6,const_33,64);
             reset(&buf_3);
             appendarray(&buf_3,const_175,80);
             reset(&buf_2);
             appendarray(&buf_2,const_175,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_34,112);
             reset(&buf_6);
             appendarray(&buf_6,const_34,112);
             reset(&buf_3);
             appendarray(&buf_3,const_214,72);
             reset(&buf_2);
             appendarray(&buf_2,const_214,72);
             consume(14);
             goto l6;
          }
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_36,40);
          reset(&buf_6);
          appendarray(&buf_6,const_36,40);
          reset(&buf_3);
          appendarray(&buf_3,const_178,24);
          reset(&buf_2);
          appendarray(&buf_2,const_178,24);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_37,64);
             reset(&buf_6);
             appendarray(&buf_6,const_37,64);
             reset(&buf_3);
             appendarray(&buf_3,const_140,88);
             reset(&buf_2);
             appendarray(&buf_2,const_140,88);
             consume(8);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_38,64);
             reset(&buf_6);
             appendarray(&buf_6,const_38,64);
             reset(&buf_3);
             appendarray(&buf_3,const_45,56);
             reset(&buf_2);
             appendarray(&buf_2,const_45,56);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_39,24);
          reset(&buf_6);
          appendarray(&buf_6,const_39,24);
          reset(&buf_3);
          appendarray(&buf_3,const_35,184);
          reset(&buf_2);
          appendarray(&buf_2,const_35,184);
          consume(3);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_32,8);
       reset(&buf_1);
       appendarray(&buf_1,const_32,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'd') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_41,48);
             reset(&buf_6);
             appendarray(&buf_6,const_41,48);
             reset(&buf_3);
             appendarray(&buf_3,const_4,96);
             reset(&buf_2);
             appendarray(&buf_2,const_4,96);
             consume(6);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_42,64);
             reset(&buf_6);
             appendarray(&buf_6,const_42,64);
             reset(&buf_3);
             appendarray(&buf_3,const_107,64);
             reset(&buf_2);
             appendarray(&buf_2,const_107,64);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_46,64);
             reset(&buf_6);
             appendarray(&buf_6,const_46,64);
             reset(&buf_3);
             appendarray(&buf_3,const_54,80);
             reset(&buf_2);
             appendarray(&buf_2,const_54,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_47,64);
                reset(&buf_6);
                appendarray(&buf_6,const_47,64);
                reset(&buf_3);
                appendarray(&buf_3,const_15,56);
                reset(&buf_2);
                appendarray(&buf_2,const_15,56);
                consume(8);
                goto l6;
             }
             if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_48,72);
                reset(&buf_6);
                appendarray(&buf_6,const_48,72);
                reset(&buf_3);
                appendarray(&buf_3,const_16,72);
                reset(&buf_2);
                appendarray(&buf_2,const_16,72);
                consume(9);
                goto l6;
             }
          }
       }
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_52,128);
          reset(&buf_6);
          appendarray(&buf_6,const_52,128);
          reset(&buf_3);
          appendarray(&buf_3,const_51,96);
          reset(&buf_2);
          appendarray(&buf_2,const_51,96);
          consume(16);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_53,40);
          reset(&buf_6);
          appendarray(&buf_6,const_53,40);
          reset(&buf_3);
          appendarray(&buf_3,const_130,72);
          reset(&buf_2);
          appendarray(&buf_2,const_130,72);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_40,8);
       reset(&buf_1);
       appendarray(&buf_1,const_40,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'e') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_57,48);
          reset(&buf_6);
          appendarray(&buf_6,const_57,48);
          reset(&buf_3);
          appendarray(&buf_3,const_196,184);
          reset(&buf_2);
          appendarray(&buf_2,const_196,184);
          consume(6);
          goto l6;
       }
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
       {
          if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_62,112);
             reset(&buf_6);
             appendarray(&buf_6,const_62,112);
             reset(&buf_3);
             appendarray(&buf_3,const_71,72);
             reset(&buf_2);
             appendarray(&buf_2,const_71,72);
             consume(14);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_61,48);
          reset(&buf_6);
          appendarray(&buf_6,const_61,48);
          reset(&buf_3);
          appendarray(&buf_3,const_72,40);
          reset(&buf_2);
          appendarray(&buf_2,const_72,40);
          consume(6);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'x') && 1)))
       {
          if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_63,72);
             reset(&buf_6);
             appendarray(&buf_6,const_63,72);
             reset(&buf_3);
             appendarray(&buf_3,const_209,80);
             reset(&buf_2);
             appendarray(&buf_2,const_209,80);
             consume(9);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_64,56);
             reset(&buf_6);
             appendarray(&buf_6,const_64,56);
             reset(&buf_3);
             appendarray(&buf_3,const_208,80);
             reset(&buf_2);
             appendarray(&buf_2,const_208,80);
             consume(7);
             goto l6;
          }
       }
       consume(1);
       goto l20;
    }
    if (((avail >= 1) && ((next[0] == 'f') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_66,32);
          reset(&buf_6);
          appendarray(&buf_6,const_66,32);
          reset(&buf_3);
          appendarray(&buf_3,const_68,48);
          reset(&buf_2);
          appendarray(&buf_2,const_68,48);
          consume(4);
          goto l6;
       }
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_69,64);
          reset(&buf_6);
          appendarray(&buf_6,const_69,64);
          reset(&buf_3);
          appendarray(&buf_3,const_27,64);
          reset(&buf_2);
          appendarray(&buf_2,const_27,64);
          consume(8);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_73,40);
          reset(&buf_6);
          appendarray(&buf_6,const_73,40);
          reset(&buf_3);
          appendarray(&buf_3,const_81,88);
          reset(&buf_2);
          appendarray(&buf_2,const_81,88);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_76,64);
             reset(&buf_6);
             appendarray(&buf_6,const_76,64);
             reset(&buf_3);
             appendarray(&buf_3,const_85,80);
             reset(&buf_2);
             appendarray(&buf_2,const_85,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_77,96);
             reset(&buf_6);
             appendarray(&buf_6,const_77,96);
             reset(&buf_3);
             appendarray(&buf_3,const_78,96);
             reset(&buf_2);
             appendarray(&buf_2,const_78,96);
             consume(12);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_65,8);
       reset(&buf_1);
       appendarray(&buf_1,const_65,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'g') && 1)))
    {
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_80,128);
          reset(&buf_6);
          appendarray(&buf_6,const_80,128);
          reset(&buf_3);
          appendarray(&buf_3,const_183,104);
          reset(&buf_2);
          appendarray(&buf_2,const_183,104);
          consume(16);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
       {
          consume(7);
          goto l25;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_79,8);
       reset(&buf_1);
       appendarray(&buf_1,const_79,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'h') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_93,64);
                reset(&buf_6);
                appendarray(&buf_6,const_93,64);
                reset(&buf_3);
                appendarray(&buf_3,const_67,112);
                reset(&buf_2);
                appendarray(&buf_2,const_67,112);
                consume(8);
                goto l6;
             }
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_94,64);
                reset(&buf_6);
                appendarray(&buf_6,const_94,64);
                reset(&buf_3);
                appendarray(&buf_3,const_124,64);
                reset(&buf_2);
                appendarray(&buf_2,const_124,64);
                consume(8);
                goto l6;
             }
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_95,96);
                reset(&buf_6);
                appendarray(&buf_6,const_95,96);
                reset(&buf_3);
                appendarray(&buf_3,const_88,128);
                reset(&buf_2);
                appendarray(&buf_2,const_88,128);
                consume(12);
                goto l6;
             }
             if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_96,56);
                reset(&buf_6);
                appendarray(&buf_6,const_96,56);
                reset(&buf_3);
                appendarray(&buf_3,const_89,104);
                reset(&buf_2);
                appendarray(&buf_2,const_89,104);
                consume(7);
                goto l6;
             }
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_90,8);
       reset(&buf_1);
       appendarray(&buf_1,const_90,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'i') && 1)))
    {
       if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_103,88);
          reset(&buf_6);
          appendarray(&buf_6,const_103,88);
          reset(&buf_3);
          appendarray(&buf_3,const_75,72);
          reset(&buf_2);
          appendarray(&buf_2,const_75,72);
          consume(11);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_101,8);
       reset(&buf_1);
       appendarray(&buf_1,const_101,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'k') && 1)))
    {
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_108,64);
          reset(&buf_6);
          appendarray(&buf_6,const_108,64);
          reset(&buf_3);
          appendarray(&buf_3,const_193,64);
          reset(&buf_2);
          appendarray(&buf_2,const_193,64);
          consume(8);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_105,8);
       reset(&buf_1);
       appendarray(&buf_1,const_105,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'l') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_115,48);
          reset(&buf_6);
          appendarray(&buf_6,const_115,48);
          reset(&buf_3);
          appendarray(&buf_3,const_123,96);
          reset(&buf_2);
          appendarray(&buf_2,const_123,96);
          consume(6);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_117,32);
          reset(&buf_6);
          appendarray(&buf_6,const_117,32);
          reset(&buf_3);
          appendarray(&buf_3,const_100,48);
          reset(&buf_2);
          appendarray(&buf_2,const_100,48);
          consume(4);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_118,72);
          reset(&buf_6);
          appendarray(&buf_6,const_118,72);
          reset(&buf_3);
          appendarray(&buf_3,const_110,72);
          reset(&buf_2);
          appendarray(&buf_2,const_110,72);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_114,8);
       reset(&buf_1);
       appendarray(&buf_1,const_114,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'm') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
          {
             if (((avail >= 5) && ((next[4] == 's') && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_128,40);
                reset(&buf_6);
                appendarray(&buf_6,const_128,40);
                reset(&buf_3);
                appendarray(&buf_3,const_127,40);
                reset(&buf_2);
                appendarray(&buf_2,const_127,40);
                consume(5);
                goto l6;
             }
             reset(&buf_7);
             appendarray(&buf_7,const_126,32);
             reset(&buf_6);
             appendarray(&buf_6,const_126,32);
             reset(&buf_3);
             appendarray(&buf_3,const_125,24);
             reset(&buf_2);
             appendarray(&buf_2,const_125,24);
             consume(4);
             goto l6;
          }
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_129,40);
             reset(&buf_6);
             appendarray(&buf_6,const_129,40);
             reset(&buf_3);
             appendarray(&buf_3,const_70,32);
             reset(&buf_2);
             appendarray(&buf_2,const_70,32);
             consume(5);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_121,8);
       reset(&buf_1);
       appendarray(&buf_1,const_121,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'n') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_132,72);
          reset(&buf_6);
          appendarray(&buf_6,const_132,72);
          reset(&buf_3);
          appendarray(&buf_3,const_133,64);
          reset(&buf_2);
          appendarray(&buf_2,const_133,64);
          consume(9);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_134,32);
          reset(&buf_6);
          appendarray(&buf_6,const_134,32);
          reset(&buf_3);
          appendarray(&buf_3,const_49,56);
          reset(&buf_2);
          appendarray(&buf_2,const_49,56);
          consume(4);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_135,24);
          reset(&buf_6);
          appendarray(&buf_6,const_135,24);
          reset(&buf_3);
          appendarray(&buf_3,const_102,32);
          reset(&buf_2);
          appendarray(&buf_2,const_102,32);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_136,56);
          reset(&buf_6);
          appendarray(&buf_6,const_136,56);
          reset(&buf_3);
          appendarray(&buf_3,const_192,56);
          reset(&buf_2);
          appendarray(&buf_2,const_192,56);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_131,8);
       reset(&buf_1);
       appendarray(&buf_1,const_131,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'o') && 1)))
    {
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_139,80);
          reset(&buf_6);
          appendarray(&buf_6,const_139,80);
          reset(&buf_3);
          appendarray(&buf_3,const_225,96);
          reset(&buf_2);
          appendarray(&buf_2,const_225,96);
          consume(10);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_137,8);
       reset(&buf_1);
       appendarray(&buf_1,const_137,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'p') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_142,88);
             reset(&buf_6);
             appendarray(&buf_6,const_142,88);
             reset(&buf_3);
             appendarray(&buf_3,const_138,40);
             reset(&buf_2);
             appendarray(&buf_2,const_138,40);
             consume(11);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_143,64);
             reset(&buf_6);
             appendarray(&buf_6,const_143,64);
             reset(&buf_3);
             appendarray(&buf_3,const_120,48);
             reset(&buf_2);
             appendarray(&buf_2,const_120,48);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_144,16);
          reset(&buf_6);
          appendarray(&buf_6,const_144,16);
          reset(&buf_3);
          appendarray(&buf_3,const_97,104);
          reset(&buf_2);
          appendarray(&buf_2,const_97,104);
          consume(2);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_145,40);
          reset(&buf_6);
          appendarray(&buf_6,const_145,40);
          reset(&buf_3);
          appendarray(&buf_3,const_189,64);
          reset(&buf_2);
          appendarray(&buf_2,const_189,64);
          consume(5);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_146,72);
          reset(&buf_6);
          appendarray(&buf_6,const_146,72);
          reset(&buf_3);
          appendarray(&buf_3,const_164,64);
          reset(&buf_2);
          appendarray(&buf_2,const_164,64);
          consume(9);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_148,56);
          reset(&buf_6);
          appendarray(&buf_6,const_148,56);
          reset(&buf_3);
          appendarray(&buf_3,const_116,96);
          reset(&buf_2);
          appendarray(&buf_2,const_116,96);
          consume(7);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'u') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
          {
             consume(4);
             goto l21;
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_153,32);
             reset(&buf_6);
             appendarray(&buf_6,const_153,32);
             reset(&buf_3);
             appendarray(&buf_3,const_176,32);
             reset(&buf_2);
             appendarray(&buf_2,const_176,32);
             consume(4);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_141,8);
       reset(&buf_1);
       appendarray(&buf_1,const_141,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'q') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_155,48);
          reset(&buf_6);
          appendarray(&buf_6,const_155,48);
          reset(&buf_3);
          appendarray(&buf_3,const_147,64);
          reset(&buf_2);
          appendarray(&buf_2,const_147,64);
          consume(6);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_156,40);
          reset(&buf_6);
          appendarray(&buf_6,const_156,40);
          reset(&buf_3);
          appendarray(&buf_3,const_113,32);
          reset(&buf_2);
          appendarray(&buf_2,const_113,32);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_154,8);
       reset(&buf_1);
       appendarray(&buf_1,const_154,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'r') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_158,104);
             reset(&buf_6);
             appendarray(&buf_6,const_158,104);
             reset(&buf_3);
             appendarray(&buf_3,const_106,104);
             reset(&buf_2);
             appendarray(&buf_2,const_106,104);
             consume(13);
             goto l6;
          }
          if (((avail >= 3) && ((next[2] == 'm') && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_159,24);
             reset(&buf_6);
             appendarray(&buf_6,const_159,24);
             reset(&buf_3);
             appendarray(&buf_3,const_18,96);
             reset(&buf_2);
             appendarray(&buf_2,const_18,96);
             consume(3);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_162,80);
             reset(&buf_6);
             appendarray(&buf_6,const_162,80);
             reset(&buf_3);
             appendarray(&buf_3,const_87,40);
             reset(&buf_2);
             appendarray(&buf_2,const_87,40);
             consume(10);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_163,56);
             reset(&buf_6);
             appendarray(&buf_6,const_163,56);
             reset(&buf_3);
             appendarray(&buf_3,const_17,72);
             reset(&buf_2);
             appendarray(&buf_2,const_17,72);
             consume(7);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_157,8);
       reset(&buf_1);
       appendarray(&buf_1,const_157,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 's') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_166,40);
             reset(&buf_6);
             appendarray(&buf_6,const_166,40);
             reset(&buf_3);
             appendarray(&buf_3,const_213,72);
             reset(&buf_2);
             appendarray(&buf_2,const_213,72);
             consume(5);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_167,80);
             reset(&buf_6);
             appendarray(&buf_6,const_167,80);
             reset(&buf_3);
             appendarray(&buf_3,const_179,104);
             reset(&buf_2);
             appendarray(&buf_2,const_179,104);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_168,32);
          reset(&buf_6);
          appendarray(&buf_6,const_168,32);
          reset(&buf_3);
          appendarray(&buf_3,const_23,88);
          reset(&buf_2);
          appendarray(&buf_2,const_23,88);
          consume(4);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
       {
          if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_170,88);
             reset(&buf_6);
             appendarray(&buf_6,const_170,88);
             reset(&buf_3);
             appendarray(&buf_3,const_174,88);
             reset(&buf_2);
             appendarray(&buf_2,const_174,88);
             consume(11);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_169,40);
          reset(&buf_6);
          appendarray(&buf_6,const_169,40);
          reset(&buf_3);
          appendarray(&buf_3,const_111,48);
          reset(&buf_2);
          appendarray(&buf_2,const_111,48);
          consume(5);
          goto l6;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_180,80);
          reset(&buf_6);
          appendarray(&buf_6,const_180,80);
          reset(&buf_3);
          appendarray(&buf_3,const_43,56);
          reset(&buf_2);
          appendarray(&buf_2,const_43,56);
          consume(10);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'o') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_181,64);
             reset(&buf_6);
             appendarray(&buf_6,const_181,64);
             reset(&buf_3);
             appendarray(&buf_3,const_149,80);
             reset(&buf_2);
             appendarray(&buf_2,const_149,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_182,80);
             reset(&buf_6);
             appendarray(&buf_6,const_182,80);
             reset(&buf_3);
             appendarray(&buf_3,const_109,80);
             reset(&buf_2);
             appendarray(&buf_2,const_109,80);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 't') && 1)))
       {
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_186,56);
             reset(&buf_6);
             appendarray(&buf_6,const_186,56);
             reset(&buf_3);
             appendarray(&buf_3,const_99,104);
             reset(&buf_2);
             appendarray(&buf_2,const_99,104);
             consume(7);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_187,56);
             reset(&buf_6);
             appendarray(&buf_6,const_187,56);
             reset(&buf_3);
             appendarray(&buf_3,const_44,72);
             reset(&buf_2);
             appendarray(&buf_2,const_44,72);
             consume(7);
             goto l6;
          }
       }
       if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_188,104);
          reset(&buf_6);
          appendarray(&buf_6,const_188,104);
          reset(&buf_3);
          appendarray(&buf_3,const_202,80);
          reset(&buf_2);
          appendarray(&buf_2,const_202,80);
          consume(13);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_165,8);
       reset(&buf_1);
       appendarray(&buf_1,const_165,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 't') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_191,48);
          reset(&buf_6);
          appendarray(&buf_6,const_191,48);
          reset(&buf_3);
          appendarray(&buf_3,const_194,96);
          reset(&buf_2);
          appendarray(&buf_2,const_194,96);
          consume(6);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
       {
          consume(3);
          goto l23;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_204,40);
          reset(&buf_6);
          appendarray(&buf_6,const_204,40);
          reset(&buf_3);
          appendarray(&buf_3,const_203,40);
          reset(&buf_2);
          appendarray(&buf_2,const_203,40);
          consume(5);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_205,40);
          reset(&buf_6);
          appendarray(&buf_6,const_205,40);
          reset(&buf_3);
          appendarray(&buf_3,const_112,40);
          reset(&buf_2);
          appendarray(&buf_2,const_112,40);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_190,8);
       reset(&buf_1);
       appendarray(&buf_1,const_190,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'u') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_210,72);
          reset(&buf_6);
          appendarray(&buf_6,const_210,72);
          reset(&buf_3);
          appendarray(&buf_3,const_86,64);
          reset(&buf_2);
          appendarray(&buf_2,const_86,64);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_206,8);
       reset(&buf_1);
       appendarray(&buf_1,const_206,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'v') && 1)))
    {
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_212,56);
          reset(&buf_6);
          appendarray(&buf_6,const_212,56);
          reset(&buf_3);
          appendarray(&buf_3,const_207,48);
          reset(&buf_2);
          appendarray(&buf_2,const_207,48);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_211,8);
       reset(&buf_1);
       appendarray(&buf_1,const_211,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'w') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 3) && ((next[2] == 'b') && 1)))
          {
             if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_218,80);
                reset(&buf_6);
                appendarray(&buf_6,const_218,80);
                reset(&buf_3);
                appendarray(&buf_3,const_185,104);
                reset(&buf_2);
                appendarray(&buf_2,const_185,104);
                consume(10);
                goto l6;
             }
             consume(3);
             goto l7;
          }
          consume(2);
          goto l22;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_223,80);
          reset(&buf_6);
          appendarray(&buf_6,const_223,80);
          reset(&buf_3);
          appendarray(&buf_3,const_24,72);
          reset(&buf_2);
          appendarray(&buf_2,const_24,72);
          consume(10);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_224,40);
          reset(&buf_6);
          appendarray(&buf_6,const_224,40);
          reset(&buf_3);
          appendarray(&buf_3,const_74,56);
          reset(&buf_2);
          appendarray(&buf_2,const_74,56);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_215,8);
       reset(&buf_1);
       appendarray(&buf_1,const_215,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_7);
       append(&buf_7,tbl[0][next[0]],8);
       reset(&buf_5);
       reset(&buf_3);
       append(&buf_3,tbl[0][next[0]],8);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l5;
    }
    goto fail;
l2: if (!readnext(1, 1))
    {
       output(&buf_5);
       output(&buf_7);
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       output(&buf_1);
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))) && 1)))
    {
       reset(&buf_1);
       concat(&buf_1,&buf_3);
       append(&buf_1,tbl[0][next[0]],8);
       output(&buf_5);
       reset(&buf_5);
       concat(&buf_5,&buf_7);
       append(&buf_5,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       append(&buf_7,tbl[0][next[0]],8);
       append(&buf_3,tbl[0][next[0]],8);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l5;
    }
    goto fail;
l3: if (!readnext(1, 1))
    {
       output(&buf_5);
       outputarray(const_17,72);
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       outputarray(const_92,104);
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))) && 1)))
    {
       reset(&buf_1);
       concat(&buf_1,&buf_3);
       append(&buf_1,tbl[0][next[0]],8);
       output(&buf_5);
       reset(&buf_5);
       concat(&buf_5,&buf_9);
       append(&buf_5,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_7);
       concat(&buf_7,&buf_9);
       append(&buf_7,tbl[0][next[0]],8);
       append(&buf_3,tbl[0][next[0]],8);
       reset(&buf_1);
       appendarray(&buf_1,const_92,104);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l5;
    }
    goto fail;
l4: if (!readnext(1, 16))
    {
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l4;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
    {
       reset(&buf_5);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'D') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_3,48);
          reset(&buf_6);
          appendarray(&buf_6,const_3,48);
          reset(&buf_3);
          appendarray(&buf_3,const_4,96);
          reset(&buf_2);
          appendarray(&buf_2,const_4,96);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_2,8);
       reset(&buf_1);
       appendarray(&buf_1,const_2,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'P') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_6,40);
          reset(&buf_6);
          appendarray(&buf_6,const_6,40);
          reset(&buf_3);
          appendarray(&buf_3,const_7,32);
          reset(&buf_2);
          appendarray(&buf_2,const_7,32);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_5,8);
       reset(&buf_1);
       appendarray(&buf_1,const_5,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'W') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_10,48);
          reset(&buf_6);
          appendarray(&buf_6,const_10,48);
          reset(&buf_3);
          appendarray(&buf_3,const_8,48);
          reset(&buf_2);
          appendarray(&buf_2,const_8,48);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_9,8);
       reset(&buf_1);
       appendarray(&buf_1,const_9,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'a') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
       {
          consume(5);
          goto l26;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_11,8);
       reset(&buf_1);
       appendarray(&buf_1,const_11,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'b') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
       {
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_20,96);
             reset(&buf_6);
             appendarray(&buf_6,const_20,96);
             reset(&buf_3);
             appendarray(&buf_3,const_25,136);
             reset(&buf_2);
             appendarray(&buf_2,const_25,136);
             consume(12);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_21,48);
             reset(&buf_6);
             appendarray(&buf_6,const_21,48);
             reset(&buf_3);
             appendarray(&buf_3,const_171,112);
             reset(&buf_2);
             appendarray(&buf_2,const_171,112);
             consume(6);
             goto l6;
          }
       }
       if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_26,112);
          reset(&buf_6);
          appendarray(&buf_6,const_26,112);
          reset(&buf_3);
          appendarray(&buf_3,const_50,120);
          reset(&buf_2);
          appendarray(&buf_2,const_50,120);
          consume(14);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_29,24);
          reset(&buf_6);
          appendarray(&buf_6,const_29,24);
          reset(&buf_3);
          appendarray(&buf_3,const_119,24);
          reset(&buf_2);
          appendarray(&buf_2,const_119,24);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
       {
          consume(7);
          goto l24;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_19,8);
       reset(&buf_1);
       appendarray(&buf_1,const_19,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'c') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_33,64);
             reset(&buf_6);
             appendarray(&buf_6,const_33,64);
             reset(&buf_3);
             appendarray(&buf_3,const_175,80);
             reset(&buf_2);
             appendarray(&buf_2,const_175,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_34,112);
             reset(&buf_6);
             appendarray(&buf_6,const_34,112);
             reset(&buf_3);
             appendarray(&buf_3,const_214,72);
             reset(&buf_2);
             appendarray(&buf_2,const_214,72);
             consume(14);
             goto l6;
          }
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_36,40);
          reset(&buf_6);
          appendarray(&buf_6,const_36,40);
          reset(&buf_3);
          appendarray(&buf_3,const_178,24);
          reset(&buf_2);
          appendarray(&buf_2,const_178,24);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_37,64);
             reset(&buf_6);
             appendarray(&buf_6,const_37,64);
             reset(&buf_3);
             appendarray(&buf_3,const_140,88);
             reset(&buf_2);
             appendarray(&buf_2,const_140,88);
             consume(8);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_38,64);
             reset(&buf_6);
             appendarray(&buf_6,const_38,64);
             reset(&buf_3);
             appendarray(&buf_3,const_45,56);
             reset(&buf_2);
             appendarray(&buf_2,const_45,56);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_39,24);
          reset(&buf_6);
          appendarray(&buf_6,const_39,24);
          reset(&buf_3);
          appendarray(&buf_3,const_35,184);
          reset(&buf_2);
          appendarray(&buf_2,const_35,184);
          consume(3);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_32,8);
       reset(&buf_1);
       appendarray(&buf_1,const_32,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'd') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_41,48);
             reset(&buf_6);
             appendarray(&buf_6,const_41,48);
             reset(&buf_3);
             appendarray(&buf_3,const_4,96);
             reset(&buf_2);
             appendarray(&buf_2,const_4,96);
             consume(6);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_42,64);
             reset(&buf_6);
             appendarray(&buf_6,const_42,64);
             reset(&buf_3);
             appendarray(&buf_3,const_107,64);
             reset(&buf_2);
             appendarray(&buf_2,const_107,64);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_46,64);
             reset(&buf_6);
             appendarray(&buf_6,const_46,64);
             reset(&buf_3);
             appendarray(&buf_3,const_54,80);
             reset(&buf_2);
             appendarray(&buf_2,const_54,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_47,64);
                reset(&buf_6);
                appendarray(&buf_6,const_47,64);
                reset(&buf_3);
                appendarray(&buf_3,const_15,56);
                reset(&buf_2);
                appendarray(&buf_2,const_15,56);
                consume(8);
                goto l6;
             }
             if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_48,72);
                reset(&buf_6);
                appendarray(&buf_6,const_48,72);
                reset(&buf_3);
                appendarray(&buf_3,const_16,72);
                reset(&buf_2);
                appendarray(&buf_2,const_16,72);
                consume(9);
                goto l6;
             }
          }
       }
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_52,128);
          reset(&buf_6);
          appendarray(&buf_6,const_52,128);
          reset(&buf_3);
          appendarray(&buf_3,const_51,96);
          reset(&buf_2);
          appendarray(&buf_2,const_51,96);
          consume(16);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_53,40);
          reset(&buf_6);
          appendarray(&buf_6,const_53,40);
          reset(&buf_3);
          appendarray(&buf_3,const_130,72);
          reset(&buf_2);
          appendarray(&buf_2,const_130,72);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_40,8);
       reset(&buf_1);
       appendarray(&buf_1,const_40,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'e') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_57,48);
          reset(&buf_6);
          appendarray(&buf_6,const_57,48);
          reset(&buf_3);
          appendarray(&buf_3,const_196,184);
          reset(&buf_2);
          appendarray(&buf_2,const_196,184);
          consume(6);
          goto l6;
       }
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
       {
          if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_62,112);
             reset(&buf_6);
             appendarray(&buf_6,const_62,112);
             reset(&buf_3);
             appendarray(&buf_3,const_71,72);
             reset(&buf_2);
             appendarray(&buf_2,const_71,72);
             consume(14);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_61,48);
          reset(&buf_6);
          appendarray(&buf_6,const_61,48);
          reset(&buf_3);
          appendarray(&buf_3,const_72,40);
          reset(&buf_2);
          appendarray(&buf_2,const_72,40);
          consume(6);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'x') && 1)))
       {
          if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_63,72);
             reset(&buf_6);
             appendarray(&buf_6,const_63,72);
             reset(&buf_3);
             appendarray(&buf_3,const_209,80);
             reset(&buf_2);
             appendarray(&buf_2,const_209,80);
             consume(9);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_64,56);
             reset(&buf_6);
             appendarray(&buf_6,const_64,56);
             reset(&buf_3);
             appendarray(&buf_3,const_208,80);
             reset(&buf_2);
             appendarray(&buf_2,const_208,80);
             consume(7);
             goto l6;
          }
       }
       consume(1);
       goto l20;
    }
    if (((avail >= 1) && ((next[0] == 'f') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_66,32);
          reset(&buf_6);
          appendarray(&buf_6,const_66,32);
          reset(&buf_3);
          appendarray(&buf_3,const_68,48);
          reset(&buf_2);
          appendarray(&buf_2,const_68,48);
          consume(4);
          goto l6;
       }
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_69,64);
          reset(&buf_6);
          appendarray(&buf_6,const_69,64);
          reset(&buf_3);
          appendarray(&buf_3,const_27,64);
          reset(&buf_2);
          appendarray(&buf_2,const_27,64);
          consume(8);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_73,40);
          reset(&buf_6);
          appendarray(&buf_6,const_73,40);
          reset(&buf_3);
          appendarray(&buf_3,const_81,88);
          reset(&buf_2);
          appendarray(&buf_2,const_81,88);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_76,64);
             reset(&buf_6);
             appendarray(&buf_6,const_76,64);
             reset(&buf_3);
             appendarray(&buf_3,const_85,80);
             reset(&buf_2);
             appendarray(&buf_2,const_85,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_77,96);
             reset(&buf_6);
             appendarray(&buf_6,const_77,96);
             reset(&buf_3);
             appendarray(&buf_3,const_78,96);
             reset(&buf_2);
             appendarray(&buf_2,const_78,96);
             consume(12);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_65,8);
       reset(&buf_1);
       appendarray(&buf_1,const_65,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'g') && 1)))
    {
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_80,128);
          reset(&buf_6);
          appendarray(&buf_6,const_80,128);
          reset(&buf_3);
          appendarray(&buf_3,const_183,104);
          reset(&buf_2);
          appendarray(&buf_2,const_183,104);
          consume(16);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
       {
          consume(7);
          goto l25;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_79,8);
       reset(&buf_1);
       appendarray(&buf_1,const_79,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'h') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_93,64);
                reset(&buf_6);
                appendarray(&buf_6,const_93,64);
                reset(&buf_3);
                appendarray(&buf_3,const_67,112);
                reset(&buf_2);
                appendarray(&buf_2,const_67,112);
                consume(8);
                goto l6;
             }
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_94,64);
                reset(&buf_6);
                appendarray(&buf_6,const_94,64);
                reset(&buf_3);
                appendarray(&buf_3,const_124,64);
                reset(&buf_2);
                appendarray(&buf_2,const_124,64);
                consume(8);
                goto l6;
             }
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_95,96);
                reset(&buf_6);
                appendarray(&buf_6,const_95,96);
                reset(&buf_3);
                appendarray(&buf_3,const_88,128);
                reset(&buf_2);
                appendarray(&buf_2,const_88,128);
                consume(12);
                goto l6;
             }
             if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_96,56);
                reset(&buf_6);
                appendarray(&buf_6,const_96,56);
                reset(&buf_3);
                appendarray(&buf_3,const_89,104);
                reset(&buf_2);
                appendarray(&buf_2,const_89,104);
                consume(7);
                goto l6;
             }
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_90,8);
       reset(&buf_1);
       appendarray(&buf_1,const_90,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'i') && 1)))
    {
       if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_103,88);
          reset(&buf_6);
          appendarray(&buf_6,const_103,88);
          reset(&buf_3);
          appendarray(&buf_3,const_75,72);
          reset(&buf_2);
          appendarray(&buf_2,const_75,72);
          consume(11);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_101,8);
       reset(&buf_1);
       appendarray(&buf_1,const_101,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'k') && 1)))
    {
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_108,64);
          reset(&buf_6);
          appendarray(&buf_6,const_108,64);
          reset(&buf_3);
          appendarray(&buf_3,const_193,64);
          reset(&buf_2);
          appendarray(&buf_2,const_193,64);
          consume(8);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_105,8);
       reset(&buf_1);
       appendarray(&buf_1,const_105,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'l') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_115,48);
          reset(&buf_6);
          appendarray(&buf_6,const_115,48);
          reset(&buf_3);
          appendarray(&buf_3,const_123,96);
          reset(&buf_2);
          appendarray(&buf_2,const_123,96);
          consume(6);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_117,32);
          reset(&buf_6);
          appendarray(&buf_6,const_117,32);
          reset(&buf_3);
          appendarray(&buf_3,const_100,48);
          reset(&buf_2);
          appendarray(&buf_2,const_100,48);
          consume(4);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_118,72);
          reset(&buf_6);
          appendarray(&buf_6,const_118,72);
          reset(&buf_3);
          appendarray(&buf_3,const_110,72);
          reset(&buf_2);
          appendarray(&buf_2,const_110,72);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_114,8);
       reset(&buf_1);
       appendarray(&buf_1,const_114,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'm') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
          {
             if (((avail >= 5) && ((next[4] == 's') && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_128,40);
                reset(&buf_6);
                appendarray(&buf_6,const_128,40);
                reset(&buf_3);
                appendarray(&buf_3,const_127,40);
                reset(&buf_2);
                appendarray(&buf_2,const_127,40);
                consume(5);
                goto l6;
             }
             reset(&buf_7);
             appendarray(&buf_7,const_126,32);
             reset(&buf_6);
             appendarray(&buf_6,const_126,32);
             reset(&buf_3);
             appendarray(&buf_3,const_125,24);
             reset(&buf_2);
             appendarray(&buf_2,const_125,24);
             consume(4);
             goto l6;
          }
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_129,40);
             reset(&buf_6);
             appendarray(&buf_6,const_129,40);
             reset(&buf_3);
             appendarray(&buf_3,const_70,32);
             reset(&buf_2);
             appendarray(&buf_2,const_70,32);
             consume(5);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_121,8);
       reset(&buf_1);
       appendarray(&buf_1,const_121,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'n') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_132,72);
          reset(&buf_6);
          appendarray(&buf_6,const_132,72);
          reset(&buf_3);
          appendarray(&buf_3,const_133,64);
          reset(&buf_2);
          appendarray(&buf_2,const_133,64);
          consume(9);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_134,32);
          reset(&buf_6);
          appendarray(&buf_6,const_134,32);
          reset(&buf_3);
          appendarray(&buf_3,const_49,56);
          reset(&buf_2);
          appendarray(&buf_2,const_49,56);
          consume(4);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_135,24);
          reset(&buf_6);
          appendarray(&buf_6,const_135,24);
          reset(&buf_3);
          appendarray(&buf_3,const_102,32);
          reset(&buf_2);
          appendarray(&buf_2,const_102,32);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_136,56);
          reset(&buf_6);
          appendarray(&buf_6,const_136,56);
          reset(&buf_3);
          appendarray(&buf_3,const_192,56);
          reset(&buf_2);
          appendarray(&buf_2,const_192,56);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_131,8);
       reset(&buf_1);
       appendarray(&buf_1,const_131,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'o') && 1)))
    {
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_139,80);
          reset(&buf_6);
          appendarray(&buf_6,const_139,80);
          reset(&buf_3);
          appendarray(&buf_3,const_225,96);
          reset(&buf_2);
          appendarray(&buf_2,const_225,96);
          consume(10);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_137,8);
       reset(&buf_1);
       appendarray(&buf_1,const_137,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'p') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_142,88);
             reset(&buf_6);
             appendarray(&buf_6,const_142,88);
             reset(&buf_3);
             appendarray(&buf_3,const_138,40);
             reset(&buf_2);
             appendarray(&buf_2,const_138,40);
             consume(11);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_143,64);
             reset(&buf_6);
             appendarray(&buf_6,const_143,64);
             reset(&buf_3);
             appendarray(&buf_3,const_120,48);
             reset(&buf_2);
             appendarray(&buf_2,const_120,48);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_144,16);
          reset(&buf_6);
          appendarray(&buf_6,const_144,16);
          reset(&buf_3);
          appendarray(&buf_3,const_97,104);
          reset(&buf_2);
          appendarray(&buf_2,const_97,104);
          consume(2);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_145,40);
          reset(&buf_6);
          appendarray(&buf_6,const_145,40);
          reset(&buf_3);
          appendarray(&buf_3,const_189,64);
          reset(&buf_2);
          appendarray(&buf_2,const_189,64);
          consume(5);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_146,72);
          reset(&buf_6);
          appendarray(&buf_6,const_146,72);
          reset(&buf_3);
          appendarray(&buf_3,const_164,64);
          reset(&buf_2);
          appendarray(&buf_2,const_164,64);
          consume(9);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_148,56);
          reset(&buf_6);
          appendarray(&buf_6,const_148,56);
          reset(&buf_3);
          appendarray(&buf_3,const_116,96);
          reset(&buf_2);
          appendarray(&buf_2,const_116,96);
          consume(7);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'u') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
          {
             consume(4);
             goto l21;
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_153,32);
             reset(&buf_6);
             appendarray(&buf_6,const_153,32);
             reset(&buf_3);
             appendarray(&buf_3,const_176,32);
             reset(&buf_2);
             appendarray(&buf_2,const_176,32);
             consume(4);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_141,8);
       reset(&buf_1);
       appendarray(&buf_1,const_141,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'q') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_155,48);
          reset(&buf_6);
          appendarray(&buf_6,const_155,48);
          reset(&buf_3);
          appendarray(&buf_3,const_147,64);
          reset(&buf_2);
          appendarray(&buf_2,const_147,64);
          consume(6);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_156,40);
          reset(&buf_6);
          appendarray(&buf_6,const_156,40);
          reset(&buf_3);
          appendarray(&buf_3,const_113,32);
          reset(&buf_2);
          appendarray(&buf_2,const_113,32);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_154,8);
       reset(&buf_1);
       appendarray(&buf_1,const_154,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'r') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_158,104);
             reset(&buf_6);
             appendarray(&buf_6,const_158,104);
             reset(&buf_3);
             appendarray(&buf_3,const_106,104);
             reset(&buf_2);
             appendarray(&buf_2,const_106,104);
             consume(13);
             goto l6;
          }
          if (((avail >= 3) && ((next[2] == 'm') && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_159,24);
             reset(&buf_6);
             appendarray(&buf_6,const_159,24);
             reset(&buf_3);
             appendarray(&buf_3,const_18,96);
             reset(&buf_2);
             appendarray(&buf_2,const_18,96);
             consume(3);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_162,80);
             reset(&buf_6);
             appendarray(&buf_6,const_162,80);
             reset(&buf_3);
             appendarray(&buf_3,const_87,40);
             reset(&buf_2);
             appendarray(&buf_2,const_87,40);
             consume(10);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_163,56);
             reset(&buf_6);
             appendarray(&buf_6,const_163,56);
             reset(&buf_3);
             appendarray(&buf_3,const_17,72);
             reset(&buf_2);
             appendarray(&buf_2,const_17,72);
             consume(7);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_157,8);
       reset(&buf_1);
       appendarray(&buf_1,const_157,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 's') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_166,40);
             reset(&buf_6);
             appendarray(&buf_6,const_166,40);
             reset(&buf_3);
             appendarray(&buf_3,const_213,72);
             reset(&buf_2);
             appendarray(&buf_2,const_213,72);
             consume(5);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_167,80);
             reset(&buf_6);
             appendarray(&buf_6,const_167,80);
             reset(&buf_3);
             appendarray(&buf_3,const_179,104);
             reset(&buf_2);
             appendarray(&buf_2,const_179,104);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_168,32);
          reset(&buf_6);
          appendarray(&buf_6,const_168,32);
          reset(&buf_3);
          appendarray(&buf_3,const_23,88);
          reset(&buf_2);
          appendarray(&buf_2,const_23,88);
          consume(4);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
       {
          if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_170,88);
             reset(&buf_6);
             appendarray(&buf_6,const_170,88);
             reset(&buf_3);
             appendarray(&buf_3,const_174,88);
             reset(&buf_2);
             appendarray(&buf_2,const_174,88);
             consume(11);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_169,40);
          reset(&buf_6);
          appendarray(&buf_6,const_169,40);
          reset(&buf_3);
          appendarray(&buf_3,const_111,48);
          reset(&buf_2);
          appendarray(&buf_2,const_111,48);
          consume(5);
          goto l6;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_180,80);
          reset(&buf_6);
          appendarray(&buf_6,const_180,80);
          reset(&buf_3);
          appendarray(&buf_3,const_43,56);
          reset(&buf_2);
          appendarray(&buf_2,const_43,56);
          consume(10);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'o') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_181,64);
             reset(&buf_6);
             appendarray(&buf_6,const_181,64);
             reset(&buf_3);
             appendarray(&buf_3,const_149,80);
             reset(&buf_2);
             appendarray(&buf_2,const_149,80);
             consume(8);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_182,80);
             reset(&buf_6);
             appendarray(&buf_6,const_182,80);
             reset(&buf_3);
             appendarray(&buf_3,const_109,80);
             reset(&buf_2);
             appendarray(&buf_2,const_109,80);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 't') && 1)))
       {
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_186,56);
             reset(&buf_6);
             appendarray(&buf_6,const_186,56);
             reset(&buf_3);
             appendarray(&buf_3,const_99,104);
             reset(&buf_2);
             appendarray(&buf_2,const_99,104);
             consume(7);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_187,56);
             reset(&buf_6);
             appendarray(&buf_6,const_187,56);
             reset(&buf_3);
             appendarray(&buf_3,const_44,72);
             reset(&buf_2);
             appendarray(&buf_2,const_44,72);
             consume(7);
             goto l6;
          }
       }
       if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_188,104);
          reset(&buf_6);
          appendarray(&buf_6,const_188,104);
          reset(&buf_3);
          appendarray(&buf_3,const_202,80);
          reset(&buf_2);
          appendarray(&buf_2,const_202,80);
          consume(13);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_165,8);
       reset(&buf_1);
       appendarray(&buf_1,const_165,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 't') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_191,48);
          reset(&buf_6);
          appendarray(&buf_6,const_191,48);
          reset(&buf_3);
          appendarray(&buf_3,const_194,96);
          reset(&buf_2);
          appendarray(&buf_2,const_194,96);
          consume(6);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
       {
          consume(3);
          goto l23;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_204,40);
          reset(&buf_6);
          appendarray(&buf_6,const_204,40);
          reset(&buf_3);
          appendarray(&buf_3,const_203,40);
          reset(&buf_2);
          appendarray(&buf_2,const_203,40);
          consume(5);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_205,40);
          reset(&buf_6);
          appendarray(&buf_6,const_205,40);
          reset(&buf_3);
          appendarray(&buf_3,const_112,40);
          reset(&buf_2);
          appendarray(&buf_2,const_112,40);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_190,8);
       reset(&buf_1);
       appendarray(&buf_1,const_190,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'u') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_210,72);
          reset(&buf_6);
          appendarray(&buf_6,const_210,72);
          reset(&buf_3);
          appendarray(&buf_3,const_86,64);
          reset(&buf_2);
          appendarray(&buf_2,const_86,64);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_206,8);
       reset(&buf_1);
       appendarray(&buf_1,const_206,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'v') && 1)))
    {
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_212,56);
          reset(&buf_6);
          appendarray(&buf_6,const_212,56);
          reset(&buf_3);
          appendarray(&buf_3,const_207,48);
          reset(&buf_2);
          appendarray(&buf_2,const_207,48);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_211,8);
       reset(&buf_1);
       appendarray(&buf_1,const_211,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'w') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 3) && ((next[2] == 'b') && 1)))
          {
             if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_218,80);
                reset(&buf_6);
                appendarray(&buf_6,const_218,80);
                reset(&buf_3);
                appendarray(&buf_3,const_185,104);
                reset(&buf_2);
                appendarray(&buf_2,const_185,104);
                consume(10);
                goto l6;
             }
             consume(3);
             goto l7;
          }
          consume(2);
          goto l22;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_223,80);
          reset(&buf_6);
          appendarray(&buf_6,const_223,80);
          reset(&buf_3);
          appendarray(&buf_3,const_24,72);
          reset(&buf_2);
          appendarray(&buf_2,const_24,72);
          consume(10);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_224,40);
          reset(&buf_6);
          appendarray(&buf_6,const_224,40);
          reset(&buf_3);
          appendarray(&buf_3,const_74,56);
          reset(&buf_2);
          appendarray(&buf_2,const_74,56);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_215,8);
       reset(&buf_1);
       appendarray(&buf_1,const_215,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_7);
       append(&buf_7,tbl[0][next[0]],8);
       reset(&buf_3);
       append(&buf_3,tbl[0][next[0]],8);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l18;
    }
    goto fail;
l5: if (!readnext(1, 16))
    {
       output(&buf_1);
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       output(&buf_1);
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
    {
       reset(&buf_5);
       append(&buf_5,tbl[0][next[0]],8);
       output(&buf_1);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'D') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_3,48);
          reset(&buf_6);
          appendarray(&buf_6,const_3,48);
          reset(&buf_3);
          appendarray(&buf_3,const_4,96);
          reset(&buf_2);
          appendarray(&buf_2,const_4,96);
          output(&buf_1);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_2,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_2,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'P') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_6,40);
          reset(&buf_6);
          appendarray(&buf_6,const_6,40);
          reset(&buf_3);
          appendarray(&buf_3,const_7,32);
          reset(&buf_2);
          appendarray(&buf_2,const_7,32);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_5,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_5,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'W') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_10,48);
          reset(&buf_6);
          appendarray(&buf_6,const_10,48);
          reset(&buf_3);
          appendarray(&buf_3,const_8,48);
          reset(&buf_2);
          appendarray(&buf_2,const_8,48);
          output(&buf_1);
          consume(6);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_9,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_9,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'a') && 1)))
    {
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
       {
          output(&buf_1);
          consume(5);
          goto l26;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_11,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_11,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'b') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
       {
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_20,96);
             reset(&buf_6);
             appendarray(&buf_6,const_20,96);
             reset(&buf_3);
             appendarray(&buf_3,const_25,136);
             reset(&buf_2);
             appendarray(&buf_2,const_25,136);
             output(&buf_1);
             consume(12);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_21,48);
             reset(&buf_6);
             appendarray(&buf_6,const_21,48);
             reset(&buf_3);
             appendarray(&buf_3,const_171,112);
             reset(&buf_2);
             appendarray(&buf_2,const_171,112);
             output(&buf_1);
             consume(6);
             goto l6;
          }
       }
       if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_26,112);
          reset(&buf_6);
          appendarray(&buf_6,const_26,112);
          reset(&buf_3);
          appendarray(&buf_3,const_50,120);
          reset(&buf_2);
          appendarray(&buf_2,const_50,120);
          output(&buf_1);
          consume(14);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_29,24);
          reset(&buf_6);
          appendarray(&buf_6,const_29,24);
          reset(&buf_3);
          appendarray(&buf_3,const_119,24);
          reset(&buf_2);
          appendarray(&buf_2,const_119,24);
          output(&buf_1);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
       {
          output(&buf_1);
          consume(7);
          goto l24;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_19,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_19,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'c') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_33,64);
             reset(&buf_6);
             appendarray(&buf_6,const_33,64);
             reset(&buf_3);
             appendarray(&buf_3,const_175,80);
             reset(&buf_2);
             appendarray(&buf_2,const_175,80);
             output(&buf_1);
             consume(8);
             goto l6;
          }
          if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_34,112);
             reset(&buf_6);
             appendarray(&buf_6,const_34,112);
             reset(&buf_3);
             appendarray(&buf_3,const_214,72);
             reset(&buf_2);
             appendarray(&buf_2,const_214,72);
             output(&buf_1);
             consume(14);
             goto l6;
          }
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_36,40);
          reset(&buf_6);
          appendarray(&buf_6,const_36,40);
          reset(&buf_3);
          appendarray(&buf_3,const_178,24);
          reset(&buf_2);
          appendarray(&buf_2,const_178,24);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_37,64);
             reset(&buf_6);
             appendarray(&buf_6,const_37,64);
             reset(&buf_3);
             appendarray(&buf_3,const_140,88);
             reset(&buf_2);
             appendarray(&buf_2,const_140,88);
             output(&buf_1);
             consume(8);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_38,64);
             reset(&buf_6);
             appendarray(&buf_6,const_38,64);
             reset(&buf_3);
             appendarray(&buf_3,const_45,56);
             reset(&buf_2);
             appendarray(&buf_2,const_45,56);
             output(&buf_1);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_39,24);
          reset(&buf_6);
          appendarray(&buf_6,const_39,24);
          reset(&buf_3);
          appendarray(&buf_3,const_35,184);
          reset(&buf_2);
          appendarray(&buf_2,const_35,184);
          output(&buf_1);
          consume(3);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_32,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_32,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'd') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_41,48);
             reset(&buf_6);
             appendarray(&buf_6,const_41,48);
             reset(&buf_3);
             appendarray(&buf_3,const_4,96);
             reset(&buf_2);
             appendarray(&buf_2,const_4,96);
             output(&buf_1);
             consume(6);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_42,64);
             reset(&buf_6);
             appendarray(&buf_6,const_42,64);
             reset(&buf_3);
             appendarray(&buf_3,const_107,64);
             reset(&buf_2);
             appendarray(&buf_2,const_107,64);
             output(&buf_1);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_46,64);
             reset(&buf_6);
             appendarray(&buf_6,const_46,64);
             reset(&buf_3);
             appendarray(&buf_3,const_54,80);
             reset(&buf_2);
             appendarray(&buf_2,const_54,80);
             output(&buf_1);
             consume(8);
             goto l6;
          }
          if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_47,64);
                reset(&buf_6);
                appendarray(&buf_6,const_47,64);
                reset(&buf_3);
                appendarray(&buf_3,const_15,56);
                reset(&buf_2);
                appendarray(&buf_2,const_15,56);
                output(&buf_1);
                consume(8);
                goto l6;
             }
             if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_48,72);
                reset(&buf_6);
                appendarray(&buf_6,const_48,72);
                reset(&buf_3);
                appendarray(&buf_3,const_16,72);
                reset(&buf_2);
                appendarray(&buf_2,const_16,72);
                output(&buf_1);
                consume(9);
                goto l6;
             }
          }
       }
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_52,128);
          reset(&buf_6);
          appendarray(&buf_6,const_52,128);
          reset(&buf_3);
          appendarray(&buf_3,const_51,96);
          reset(&buf_2);
          appendarray(&buf_2,const_51,96);
          output(&buf_1);
          consume(16);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_53,40);
          reset(&buf_6);
          appendarray(&buf_6,const_53,40);
          reset(&buf_3);
          appendarray(&buf_3,const_130,72);
          reset(&buf_2);
          appendarray(&buf_2,const_130,72);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_40,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_40,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'e') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_57,48);
          reset(&buf_6);
          appendarray(&buf_6,const_57,48);
          reset(&buf_3);
          appendarray(&buf_3,const_196,184);
          reset(&buf_2);
          appendarray(&buf_2,const_196,184);
          output(&buf_1);
          consume(6);
          goto l6;
       }
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
       {
          if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_62,112);
             reset(&buf_6);
             appendarray(&buf_6,const_62,112);
             reset(&buf_3);
             appendarray(&buf_3,const_71,72);
             reset(&buf_2);
             appendarray(&buf_2,const_71,72);
             output(&buf_1);
             consume(14);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_61,48);
          reset(&buf_6);
          appendarray(&buf_6,const_61,48);
          reset(&buf_3);
          appendarray(&buf_3,const_72,40);
          reset(&buf_2);
          appendarray(&buf_2,const_72,40);
          output(&buf_1);
          consume(6);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'x') && 1)))
       {
          if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_63,72);
             reset(&buf_6);
             appendarray(&buf_6,const_63,72);
             reset(&buf_3);
             appendarray(&buf_3,const_209,80);
             reset(&buf_2);
             appendarray(&buf_2,const_209,80);
             output(&buf_1);
             consume(9);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_64,56);
             reset(&buf_6);
             appendarray(&buf_6,const_64,56);
             reset(&buf_3);
             appendarray(&buf_3,const_208,80);
             reset(&buf_2);
             appendarray(&buf_2,const_208,80);
             output(&buf_1);
             consume(7);
             goto l6;
          }
       }
       output(&buf_1);
       consume(1);
       goto l20;
    }
    if (((avail >= 1) && ((next[0] == 'f') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_66,32);
          reset(&buf_6);
          appendarray(&buf_6,const_66,32);
          reset(&buf_3);
          appendarray(&buf_3,const_68,48);
          reset(&buf_2);
          appendarray(&buf_2,const_68,48);
          output(&buf_1);
          consume(4);
          goto l6;
       }
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_69,64);
          reset(&buf_6);
          appendarray(&buf_6,const_69,64);
          reset(&buf_3);
          appendarray(&buf_3,const_27,64);
          reset(&buf_2);
          appendarray(&buf_2,const_27,64);
          output(&buf_1);
          consume(8);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_73,40);
          reset(&buf_6);
          appendarray(&buf_6,const_73,40);
          reset(&buf_3);
          appendarray(&buf_3,const_81,88);
          reset(&buf_2);
          appendarray(&buf_2,const_81,88);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
       {
          if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_76,64);
             reset(&buf_6);
             appendarray(&buf_6,const_76,64);
             reset(&buf_3);
             appendarray(&buf_3,const_85,80);
             reset(&buf_2);
             appendarray(&buf_2,const_85,80);
             output(&buf_1);
             consume(8);
             goto l6;
          }
          if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_77,96);
             reset(&buf_6);
             appendarray(&buf_6,const_77,96);
             reset(&buf_3);
             appendarray(&buf_3,const_78,96);
             reset(&buf_2);
             appendarray(&buf_2,const_78,96);
             output(&buf_1);
             consume(12);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_65,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_65,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'g') && 1)))
    {
       if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_80,128);
          reset(&buf_6);
          appendarray(&buf_6,const_80,128);
          reset(&buf_3);
          appendarray(&buf_3,const_183,104);
          reset(&buf_2);
          appendarray(&buf_2,const_183,104);
          output(&buf_1);
          consume(16);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
       {
          output(&buf_1);
          consume(7);
          goto l25;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_79,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_79,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'h') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
          {
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_93,64);
                reset(&buf_6);
                appendarray(&buf_6,const_93,64);
                reset(&buf_3);
                appendarray(&buf_3,const_67,112);
                reset(&buf_2);
                appendarray(&buf_2,const_67,112);
                output(&buf_1);
                consume(8);
                goto l6;
             }
             if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_94,64);
                reset(&buf_6);
                appendarray(&buf_6,const_94,64);
                reset(&buf_3);
                appendarray(&buf_3,const_124,64);
                reset(&buf_2);
                appendarray(&buf_2,const_124,64);
                output(&buf_1);
                consume(8);
                goto l6;
             }
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_95,96);
                reset(&buf_6);
                appendarray(&buf_6,const_95,96);
                reset(&buf_3);
                appendarray(&buf_3,const_88,128);
                reset(&buf_2);
                appendarray(&buf_2,const_88,128);
                output(&buf_1);
                consume(12);
                goto l6;
             }
             if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_96,56);
                reset(&buf_6);
                appendarray(&buf_6,const_96,56);
                reset(&buf_3);
                appendarray(&buf_3,const_89,104);
                reset(&buf_2);
                appendarray(&buf_2,const_89,104);
                output(&buf_1);
                consume(7);
                goto l6;
             }
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_90,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_90,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'i') && 1)))
    {
       if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_103,88);
          reset(&buf_6);
          appendarray(&buf_6,const_103,88);
          reset(&buf_3);
          appendarray(&buf_3,const_75,72);
          reset(&buf_2);
          appendarray(&buf_2,const_75,72);
          output(&buf_1);
          consume(11);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_101,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_101,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'k') && 1)))
    {
       if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_108,64);
          reset(&buf_6);
          appendarray(&buf_6,const_108,64);
          reset(&buf_3);
          appendarray(&buf_3,const_193,64);
          reset(&buf_2);
          appendarray(&buf_2,const_193,64);
          output(&buf_1);
          consume(8);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_105,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_105,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'l') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_115,48);
          reset(&buf_6);
          appendarray(&buf_6,const_115,48);
          reset(&buf_3);
          appendarray(&buf_3,const_123,96);
          reset(&buf_2);
          appendarray(&buf_2,const_123,96);
          output(&buf_1);
          consume(6);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_117,32);
          reset(&buf_6);
          appendarray(&buf_6,const_117,32);
          reset(&buf_3);
          appendarray(&buf_3,const_100,48);
          reset(&buf_2);
          appendarray(&buf_2,const_100,48);
          output(&buf_1);
          consume(4);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_118,72);
          reset(&buf_6);
          appendarray(&buf_6,const_118,72);
          reset(&buf_3);
          appendarray(&buf_3,const_110,72);
          reset(&buf_2);
          appendarray(&buf_2,const_110,72);
          output(&buf_1);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_114,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_114,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'm') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
          {
             if (((avail >= 5) && ((next[4] == 's') && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_128,40);
                reset(&buf_6);
                appendarray(&buf_6,const_128,40);
                reset(&buf_3);
                appendarray(&buf_3,const_127,40);
                reset(&buf_2);
                appendarray(&buf_2,const_127,40);
                output(&buf_1);
                consume(5);
                goto l6;
             }
             reset(&buf_7);
             appendarray(&buf_7,const_126,32);
             reset(&buf_6);
             appendarray(&buf_6,const_126,32);
             reset(&buf_3);
             appendarray(&buf_3,const_125,24);
             reset(&buf_2);
             appendarray(&buf_2,const_125,24);
             output(&buf_1);
             consume(4);
             goto l6;
          }
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_129,40);
             reset(&buf_6);
             appendarray(&buf_6,const_129,40);
             reset(&buf_3);
             appendarray(&buf_3,const_70,32);
             reset(&buf_2);
             appendarray(&buf_2,const_70,32);
             output(&buf_1);
             consume(5);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_121,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_121,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'n') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_132,72);
          reset(&buf_6);
          appendarray(&buf_6,const_132,72);
          reset(&buf_3);
          appendarray(&buf_3,const_133,64);
          reset(&buf_2);
          appendarray(&buf_2,const_133,64);
          output(&buf_1);
          consume(9);
          goto l6;
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_134,32);
          reset(&buf_6);
          appendarray(&buf_6,const_134,32);
          reset(&buf_3);
          appendarray(&buf_3,const_49,56);
          reset(&buf_2);
          appendarray(&buf_2,const_49,56);
          output(&buf_1);
          consume(4);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_135,24);
          reset(&buf_6);
          appendarray(&buf_6,const_135,24);
          reset(&buf_3);
          appendarray(&buf_3,const_102,32);
          reset(&buf_2);
          appendarray(&buf_2,const_102,32);
          output(&buf_1);
          consume(3);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_136,56);
          reset(&buf_6);
          appendarray(&buf_6,const_136,56);
          reset(&buf_3);
          appendarray(&buf_3,const_192,56);
          reset(&buf_2);
          appendarray(&buf_2,const_192,56);
          output(&buf_1);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_131,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_131,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'o') && 1)))
    {
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_139,80);
          reset(&buf_6);
          appendarray(&buf_6,const_139,80);
          reset(&buf_3);
          appendarray(&buf_3,const_225,96);
          reset(&buf_2);
          appendarray(&buf_2,const_225,96);
          output(&buf_1);
          consume(10);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_137,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_137,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'p') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_142,88);
             reset(&buf_6);
             appendarray(&buf_6,const_142,88);
             reset(&buf_3);
             appendarray(&buf_3,const_138,40);
             reset(&buf_2);
             appendarray(&buf_2,const_138,40);
             output(&buf_1);
             consume(11);
             goto l6;
          }
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_143,64);
             reset(&buf_6);
             appendarray(&buf_6,const_143,64);
             reset(&buf_3);
             appendarray(&buf_3,const_120,48);
             reset(&buf_2);
             appendarray(&buf_2,const_120,48);
             output(&buf_1);
             consume(8);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_144,16);
          reset(&buf_6);
          appendarray(&buf_6,const_144,16);
          reset(&buf_3);
          appendarray(&buf_3,const_97,104);
          reset(&buf_2);
          appendarray(&buf_2,const_97,104);
          output(&buf_1);
          consume(2);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_145,40);
          reset(&buf_6);
          appendarray(&buf_6,const_145,40);
          reset(&buf_3);
          appendarray(&buf_3,const_189,64);
          reset(&buf_2);
          appendarray(&buf_2,const_189,64);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_146,72);
          reset(&buf_6);
          appendarray(&buf_6,const_146,72);
          reset(&buf_3);
          appendarray(&buf_3,const_164,64);
          reset(&buf_2);
          appendarray(&buf_2,const_164,64);
          output(&buf_1);
          consume(9);
          goto l6;
       }
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_148,56);
          reset(&buf_6);
          appendarray(&buf_6,const_148,56);
          reset(&buf_3);
          appendarray(&buf_3,const_116,96);
          reset(&buf_2);
          appendarray(&buf_2,const_116,96);
          output(&buf_1);
          consume(7);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'u') && 1)))
       {
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
          {
             output(&buf_1);
             consume(4);
             goto l21;
          }
          if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_153,32);
             reset(&buf_6);
             appendarray(&buf_6,const_153,32);
             reset(&buf_3);
             appendarray(&buf_3,const_176,32);
             reset(&buf_2);
             appendarray(&buf_2,const_176,32);
             output(&buf_1);
             consume(4);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_141,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_141,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'q') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_155,48);
          reset(&buf_6);
          appendarray(&buf_6,const_155,48);
          reset(&buf_3);
          appendarray(&buf_3,const_147,64);
          reset(&buf_2);
          appendarray(&buf_2,const_147,64);
          output(&buf_1);
          consume(6);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_156,40);
          reset(&buf_6);
          appendarray(&buf_6,const_156,40);
          reset(&buf_3);
          appendarray(&buf_3,const_113,32);
          reset(&buf_2);
          appendarray(&buf_2,const_113,32);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_154,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_154,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'r') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'a') && 1)))
       {
          if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_158,104);
             reset(&buf_6);
             appendarray(&buf_6,const_158,104);
             reset(&buf_3);
             appendarray(&buf_3,const_106,104);
             reset(&buf_2);
             appendarray(&buf_2,const_106,104);
             output(&buf_1);
             consume(13);
             goto l6;
          }
          if (((avail >= 3) && ((next[2] == 'm') && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_159,24);
             reset(&buf_6);
             appendarray(&buf_6,const_159,24);
             reset(&buf_3);
             appendarray(&buf_3,const_18,96);
             reset(&buf_2);
             appendarray(&buf_2,const_18,96);
             output(&buf_1);
             consume(3);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_162,80);
             reset(&buf_6);
             appendarray(&buf_6,const_162,80);
             reset(&buf_3);
             appendarray(&buf_3,const_87,40);
             reset(&buf_2);
             appendarray(&buf_2,const_87,40);
             output(&buf_1);
             consume(10);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_163,56);
             reset(&buf_6);
             appendarray(&buf_6,const_163,56);
             reset(&buf_3);
             appendarray(&buf_3,const_17,72);
             reset(&buf_2);
             appendarray(&buf_2,const_17,72);
             output(&buf_1);
             consume(7);
             goto l6;
          }
       }
       reset(&buf_5);
       appendarray(&buf_5,const_157,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_157,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 's') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'c') && 1)))
       {
          if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_166,40);
             reset(&buf_6);
             appendarray(&buf_6,const_166,40);
             reset(&buf_3);
             appendarray(&buf_3,const_213,72);
             reset(&buf_2);
             appendarray(&buf_2,const_213,72);
             output(&buf_1);
             consume(5);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_167,80);
             reset(&buf_6);
             appendarray(&buf_6,const_167,80);
             reset(&buf_3);
             appendarray(&buf_3,const_179,104);
             reset(&buf_2);
             appendarray(&buf_2,const_179,104);
             output(&buf_1);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_168,32);
          reset(&buf_6);
          appendarray(&buf_6,const_168,32);
          reset(&buf_3);
          appendarray(&buf_3,const_23,88);
          reset(&buf_2);
          appendarray(&buf_2,const_23,88);
          output(&buf_1);
          consume(4);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
       {
          if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_170,88);
             reset(&buf_6);
             appendarray(&buf_6,const_170,88);
             reset(&buf_3);
             appendarray(&buf_3,const_174,88);
             reset(&buf_2);
             appendarray(&buf_2,const_174,88);
             output(&buf_1);
             consume(11);
             goto l6;
          }
          reset(&buf_7);
          appendarray(&buf_7,const_169,40);
          reset(&buf_6);
          appendarray(&buf_6,const_169,40);
          reset(&buf_3);
          appendarray(&buf_3,const_111,48);
          reset(&buf_2);
          appendarray(&buf_2,const_111,48);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_180,80);
          reset(&buf_6);
          appendarray(&buf_6,const_180,80);
          reset(&buf_3);
          appendarray(&buf_3,const_43,56);
          reset(&buf_2);
          appendarray(&buf_2,const_43,56);
          output(&buf_1);
          consume(10);
          goto l6;
       }
       if (((avail >= 2) && ((next[1] == 'o') && 1)))
       {
          if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_181,64);
             reset(&buf_6);
             appendarray(&buf_6,const_181,64);
             reset(&buf_3);
             appendarray(&buf_3,const_149,80);
             reset(&buf_2);
             appendarray(&buf_2,const_149,80);
             output(&buf_1);
             consume(8);
             goto l6;
          }
          if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_182,80);
             reset(&buf_6);
             appendarray(&buf_6,const_182,80);
             reset(&buf_3);
             appendarray(&buf_3,const_109,80);
             reset(&buf_2);
             appendarray(&buf_2,const_109,80);
             output(&buf_1);
             consume(10);
             goto l6;
          }
       }
       if (((avail >= 2) && ((next[1] == 't') && 1)))
       {
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_186,56);
             reset(&buf_6);
             appendarray(&buf_6,const_186,56);
             reset(&buf_3);
             appendarray(&buf_3,const_99,104);
             reset(&buf_2);
             appendarray(&buf_2,const_99,104);
             output(&buf_1);
             consume(7);
             goto l6;
          }
          if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
          {
             reset(&buf_7);
             appendarray(&buf_7,const_187,56);
             reset(&buf_6);
             appendarray(&buf_6,const_187,56);
             reset(&buf_3);
             appendarray(&buf_3,const_44,72);
             reset(&buf_2);
             appendarray(&buf_2,const_44,72);
             output(&buf_1);
             consume(7);
             goto l6;
          }
       }
       if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_188,104);
          reset(&buf_6);
          appendarray(&buf_6,const_188,104);
          reset(&buf_3);
          appendarray(&buf_3,const_202,80);
          reset(&buf_2);
          appendarray(&buf_2,const_202,80);
          output(&buf_1);
          consume(13);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_165,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_165,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 't') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_191,48);
          reset(&buf_6);
          appendarray(&buf_6,const_191,48);
          reset(&buf_3);
          appendarray(&buf_3,const_194,96);
          reset(&buf_2);
          appendarray(&buf_2,const_194,96);
          output(&buf_1);
          consume(6);
          goto l6;
       }
       if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
       {
          output(&buf_1);
          consume(3);
          goto l23;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_204,40);
          reset(&buf_6);
          appendarray(&buf_6,const_204,40);
          reset(&buf_3);
          appendarray(&buf_3,const_203,40);
          reset(&buf_2);
          appendarray(&buf_2,const_203,40);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_205,40);
          reset(&buf_6);
          appendarray(&buf_6,const_205,40);
          reset(&buf_3);
          appendarray(&buf_3,const_112,40);
          reset(&buf_2);
          appendarray(&buf_2,const_112,40);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_190,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_190,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'u') && 1)))
    {
       if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_210,72);
          reset(&buf_6);
          appendarray(&buf_6,const_210,72);
          reset(&buf_3);
          appendarray(&buf_3,const_86,64);
          reset(&buf_2);
          appendarray(&buf_2,const_86,64);
          output(&buf_1);
          consume(9);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_206,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_206,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'v') && 1)))
    {
       if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_212,56);
          reset(&buf_6);
          appendarray(&buf_6,const_212,56);
          reset(&buf_3);
          appendarray(&buf_3,const_207,48);
          reset(&buf_2);
          appendarray(&buf_2,const_207,48);
          output(&buf_1);
          consume(7);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_211,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_211,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 'w') && 1)))
    {
       if (((avail >= 2) && ((next[1] == 'e') && 1)))
       {
          if (((avail >= 3) && ((next[2] == 'b') && 1)))
          {
             if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
             {
                reset(&buf_7);
                appendarray(&buf_7,const_218,80);
                reset(&buf_6);
                appendarray(&buf_6,const_218,80);
                reset(&buf_3);
                appendarray(&buf_3,const_185,104);
                reset(&buf_2);
                appendarray(&buf_2,const_185,104);
                output(&buf_1);
                consume(10);
                goto l6;
             }
             output(&buf_1);
             consume(3);
             goto l7;
          }
          output(&buf_1);
          consume(2);
          goto l22;
       }
       if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_223,80);
          reset(&buf_6);
          appendarray(&buf_6,const_223,80);
          reset(&buf_3);
          appendarray(&buf_3,const_24,72);
          reset(&buf_2);
          appendarray(&buf_2,const_24,72);
          output(&buf_1);
          consume(10);
          goto l6;
       }
       if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_224,40);
          reset(&buf_6);
          appendarray(&buf_6,const_224,40);
          reset(&buf_3);
          appendarray(&buf_3,const_74,56);
          reset(&buf_2);
          appendarray(&buf_2,const_74,56);
          output(&buf_1);
          consume(5);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_215,8);
       output(&buf_1);
       reset(&buf_1);
       appendarray(&buf_1,const_215,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_7);
       append(&buf_7,tbl[0][next[0]],8);
       reset(&buf_5);
       reset(&buf_3);
       append(&buf_3,tbl[0][next[0]],8);
       output(&buf_1);
       reset(&buf_1);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l5;
    }
    goto fail;
l6: if (!readnext(1, 1))
    {
       output(&buf_3);
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       output(&buf_2);
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))) && 1)))
    {
       reset(&buf_5);
       concat(&buf_5,&buf_7);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_1);
       concat(&buf_1,&buf_6);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       append(&buf_6,tbl[0][next[0]],8);
       reset(&buf_5);
       concat(&buf_5,&buf_7);
       append(&buf_5,tbl[0][next[0]],8);
       append(&buf_2,tbl[0][next[0]],8);
       consume(1);
       goto l19;
    }
    goto fail;
l7: if (!readnext(1, 4))
    {
       outputarray(const_184,56);
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       outputarray(const_184,56);
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'r')) || ((('t' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
    {
       reset(&buf_5);
       appendarray(&buf_5,const_217,24);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_1);
       appendarray(&buf_1,const_217,24);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 's') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ite",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_220,56);
          reset(&buf_6);
          appendarray(&buf_6,const_220,56);
          reset(&buf_3);
          appendarray(&buf_3,const_98,80);
          reset(&buf_2);
          appendarray(&buf_2,const_98,80);
          consume(4);
          goto l6;
       }
       reset(&buf_5);
       appendarray(&buf_5,const_219,32);
       reset(&buf_1);
       appendarray(&buf_1,const_219,32);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_6);
       appendarray(&buf_6,const_217,24);
       append(&buf_6,tbl[0][next[0]],8);
       reset(&buf_5);
       appendarray(&buf_5,const_217,24);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_2);
       appendarray(&buf_2,const_184,56);
       append(&buf_2,tbl[0][next[0]],8);
       consume(1);
       goto l19;
    }
    goto fail;
l8: if (!readnext(1, 4))
    {
       output(&buf_5);
       outputarray(const_184,56);
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       output(&buf_1);
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'r')) || ((('t' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
    {
       appendarray(&buf_5,const_217,24);
       append(&buf_5,tbl[0][next[0]],8);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 's') && 1)))
    {
       if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ite",3) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_220,56);
          reset(&buf_3);
          appendarray(&buf_3,const_98,80);
          appendarray(&buf_1,const_172,32);
          consume(4);
          goto l12;
       }
       appendarray(&buf_5,const_219,32);
       appendarray(&buf_1,const_165,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_6);
       append(&buf_6,tbl[0][next[0]],8);
       appendarray(&buf_5,const_217,24);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_2);
       append(&buf_2,tbl[0][next[0]],8);
       consume(1);
       goto l27;
    }
    goto fail;
l9: if (!readnext(1, 6))
    {
       output(&buf_5);
       outputarray(const_177,80);
       goto accept;
    }
    if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
    {
       output(&buf_1);
       outputconst(tbl[0][next[0]],8);
       consume(1);
       goto l1;
    }
    if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'r')) || ((('t' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
    {
       appendarray(&buf_5,const_82,56);
       append(&buf_5,tbl[0][next[0]],8);
       append(&buf_1,tbl[0][next[0]],8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((next[0] == 's') && 1)))
    {
       if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "itory",5) && 1)))
       {
          reset(&buf_7);
          appendarray(&buf_7,const_84,104);
          reset(&buf_3);
          appendarray(&buf_3,const_177,80);
          appendarray(&buf_1,const_173,48);
          consume(6);
          goto l12;
       }
       appendarray(&buf_5,const_83,64);
       appendarray(&buf_1,const_165,8);
       consume(1);
       goto l11;
    }
    if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
    {
       reset(&buf_6);
       append(&buf_6,tbl[0][next[0]],8);
       appendarray(&buf_5,const_82,56);
       append(&buf_5,tbl[0][next[0]],8);
       reset(&buf_2);
       append(&buf_2,tbl[0][next[0]],8);
       consume(1);
       goto l27;
    }
    goto fail;
l10: if (!readnext(1, 8))
     {
        output(&buf_5);
        outputarray(const_22,64);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'h')) || ((('j' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        appendarray(&buf_5,const_12,40);
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'i') && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "strator",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_14,104);
           reset(&buf_3);
           appendarray(&buf_3,const_22,64);
           appendarray(&buf_1,const_104,64);
           consume(8);
           goto l12;
        }
        appendarray(&buf_5,const_13,48);
        appendarray(&buf_1,const_101,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        appendarray(&buf_5,const_12,40);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l11: if (!readnext(1, 1))
     {
        output(&buf_5);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))) && 1)))
     {
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l12: if (!readnext(1, 1))
     {
        output(&buf_5);
        output(&buf_3);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))) && 1)))
     {
        concat(&buf_5,&buf_7);
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        concat(&buf_5,&buf_7);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l13: if (!readnext(1, 5))
     {
        output(&buf_5);
        outputarray(const_55,8);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255)))))))))) && 1)))
     {
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && ((next[0] == '-') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "mail",4) && 1)))
        {
           reset(&buf_5);
           appendarray(&buf_5,const_122,32);
           output(&buf_1);
           outputarray(const_1,8);
           reset(&buf_1);
           appendarray(&buf_1,const_122,32);
           consume(5);
           goto l11;
        }
        output(&buf_1);
        outputarray(const_1,8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'l')) || ((('n' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        appendarray(&buf_5,const_55,8);
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'm') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_60,40);
           reset(&buf_3);
           appendarray(&buf_3,const_58,88);
           appendarray(&buf_1,const_122,32);
           consume(4);
           goto l12;
        }
        appendarray(&buf_5,const_59,16);
        appendarray(&buf_1,const_121,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        appendarray(&buf_5,const_55,8);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l14: if (!readnext(1, 8))
     {
        output(&buf_5);
        outputarray(const_91,24);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= 31)) || ((('!' <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))))) && 1)))
     {
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((next[0] == ' ') || (next[0] == '-')) && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "request",7) && 1)))
        {
           reset(&buf_7);
           for(i = 0; i < 7; i++)
           {  append(&buf_7,tbl[0][next[1 + i]],8);
           }
           reset(&buf_6);
           for(i = 0; i < 7; i++)
           {  append(&buf_6,tbl[0][next[1 + i]],8);
           }
           reset(&buf_3);
           appendarray(&buf_3,const_17,72);
           reset(&buf_2);
           appendarray(&buf_2,const_17,72);
           output(&buf_1);
           outputconst(tbl[0][next[0]],8);
           consume(8);
           goto l6;
        }
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'q')) || ((('s' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        appendarray(&buf_5,const_150,32);
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'r') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "equest",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_152,88);
           reset(&buf_3);
           appendarray(&buf_3,const_92,104);
           appendarray(&buf_1,const_163,56);
           consume(7);
           goto l12;
        }
        appendarray(&buf_5,const_151,40);
        appendarray(&buf_1,const_157,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        appendarray(&buf_5,const_150,32);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l15: if (!readnext(1, 5))
     {
        output(&buf_5);
        outputarray(const_216,16);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'a') || ((('c' <= next[0]) && (next[0] <= 'r')) || ((('t' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))) && 1)))
     {
        appendarray(&buf_5,const_216,16);
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'b') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "site",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_220,56);
           reset(&buf_3);
           appendarray(&buf_3,const_98,80);
           appendarray(&buf_1,const_28,40);
           consume(5);
           goto l12;
        }
        appendarray(&buf_5,const_217,24);
        appendarray(&buf_1,const_19,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ite",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_222,48);
           reset(&buf_3);
           appendarray(&buf_3,const_98,80);
           appendarray(&buf_1,const_172,32);
           consume(4);
           goto l12;
        }
        appendarray(&buf_5,const_221,24);
        appendarray(&buf_1,const_165,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        appendarray(&buf_5,const_216,16);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l16: if (!readnext(1, 7))
     {
        output(&buf_5);
        outputarray(const_197,24);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_1);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'd')) || ((('f' <= next[0]) && (next[0] <= 's')) || ((('u' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))) && 1)))
     {
        appendarray(&buf_5,const_197,24);
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_199,72);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           appendarray(&buf_1,const_57,48);
           consume(6);
           goto l12;
        }
        appendarray(&buf_5,const_198,32);
        appendarray(&buf_1,const_55,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 't') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "editor",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_201,80);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           appendarray(&buf_1,const_195,56);
           consume(7);
           goto l12;
        }
        appendarray(&buf_5,const_200,32);
        appendarray(&buf_1,const_190,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        appendarray(&buf_5,const_197,24);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l17: if (!readnext(1, 16))
     {
        output(&buf_1);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l17;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
     {
        reset(&buf_5);
        append(&buf_5,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'D') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_3,48);
           reset(&buf_6);
           appendarray(&buf_6,const_3,48);
           reset(&buf_3);
           appendarray(&buf_3,const_4,96);
           reset(&buf_2);
           appendarray(&buf_2,const_4,96);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_2,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_2,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'P') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_6,40);
           reset(&buf_6);
           appendarray(&buf_6,const_6,40);
           reset(&buf_3);
           appendarray(&buf_3,const_7,32);
           reset(&buf_2);
           appendarray(&buf_2,const_7,32);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_5,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_5,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'W') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_10,48);
           reset(&buf_6);
           appendarray(&buf_6,const_10,48);
           reset(&buf_3);
           appendarray(&buf_3,const_8,48);
           reset(&buf_2);
           appendarray(&buf_2,const_8,48);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_9,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_9,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'a') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
        {
           output(&buf_1);
           consume(5);
           goto l26;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_11,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_11,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'b') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
        {
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_20,96);
              reset(&buf_6);
              appendarray(&buf_6,const_20,96);
              reset(&buf_3);
              appendarray(&buf_3,const_25,136);
              reset(&buf_2);
              appendarray(&buf_2,const_25,136);
              output(&buf_1);
              consume(12);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_21,48);
              reset(&buf_6);
              appendarray(&buf_6,const_21,48);
              reset(&buf_3);
              appendarray(&buf_3,const_171,112);
              reset(&buf_2);
              appendarray(&buf_2,const_171,112);
              output(&buf_1);
              consume(6);
              goto l6;
           }
        }
        if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_26,112);
           reset(&buf_6);
           appendarray(&buf_6,const_26,112);
           reset(&buf_3);
           appendarray(&buf_3,const_50,120);
           reset(&buf_2);
           appendarray(&buf_2,const_50,120);
           output(&buf_1);
           consume(14);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_29,24);
           reset(&buf_6);
           appendarray(&buf_6,const_29,24);
           reset(&buf_3);
           appendarray(&buf_3,const_119,24);
           reset(&buf_2);
           appendarray(&buf_2,const_119,24);
           output(&buf_1);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
        {
           output(&buf_1);
           consume(7);
           goto l24;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_19,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_19,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'c') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_33,64);
              reset(&buf_6);
              appendarray(&buf_6,const_33,64);
              reset(&buf_3);
              appendarray(&buf_3,const_175,80);
              reset(&buf_2);
              appendarray(&buf_2,const_175,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_34,112);
              reset(&buf_6);
              appendarray(&buf_6,const_34,112);
              reset(&buf_3);
              appendarray(&buf_3,const_214,72);
              reset(&buf_2);
              appendarray(&buf_2,const_214,72);
              output(&buf_1);
              consume(14);
              goto l6;
           }
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_36,40);
           reset(&buf_6);
           appendarray(&buf_6,const_36,40);
           reset(&buf_3);
           appendarray(&buf_3,const_178,24);
           reset(&buf_2);
           appendarray(&buf_2,const_178,24);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_37,64);
              reset(&buf_6);
              appendarray(&buf_6,const_37,64);
              reset(&buf_3);
              appendarray(&buf_3,const_140,88);
              reset(&buf_2);
              appendarray(&buf_2,const_140,88);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_38,64);
              reset(&buf_6);
              appendarray(&buf_6,const_38,64);
              reset(&buf_3);
              appendarray(&buf_3,const_45,56);
              reset(&buf_2);
              appendarray(&buf_2,const_45,56);
              output(&buf_1);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_39,24);
           reset(&buf_6);
           appendarray(&buf_6,const_39,24);
           reset(&buf_3);
           appendarray(&buf_3,const_35,184);
           reset(&buf_2);
           appendarray(&buf_2,const_35,184);
           output(&buf_1);
           consume(3);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_32,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_32,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'd') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_41,48);
              reset(&buf_6);
              appendarray(&buf_6,const_41,48);
              reset(&buf_3);
              appendarray(&buf_3,const_4,96);
              reset(&buf_2);
              appendarray(&buf_2,const_4,96);
              output(&buf_1);
              consume(6);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_42,64);
              reset(&buf_6);
              appendarray(&buf_6,const_42,64);
              reset(&buf_3);
              appendarray(&buf_3,const_107,64);
              reset(&buf_2);
              appendarray(&buf_2,const_107,64);
              output(&buf_1);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_46,64);
              reset(&buf_6);
              appendarray(&buf_6,const_46,64);
              reset(&buf_3);
              appendarray(&buf_3,const_54,80);
              reset(&buf_2);
              appendarray(&buf_2,const_54,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_47,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_47,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_15,56);
                 reset(&buf_2);
                 appendarray(&buf_2,const_15,56);
                 output(&buf_1);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_48,72);
                 reset(&buf_6);
                 appendarray(&buf_6,const_48,72);
                 reset(&buf_3);
                 appendarray(&buf_3,const_16,72);
                 reset(&buf_2);
                 appendarray(&buf_2,const_16,72);
                 output(&buf_1);
                 consume(9);
                 goto l6;
              }
           }
        }
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_52,128);
           reset(&buf_6);
           appendarray(&buf_6,const_52,128);
           reset(&buf_3);
           appendarray(&buf_3,const_51,96);
           reset(&buf_2);
           appendarray(&buf_2,const_51,96);
           output(&buf_1);
           consume(16);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_53,40);
           reset(&buf_6);
           appendarray(&buf_6,const_53,40);
           reset(&buf_3);
           appendarray(&buf_3,const_130,72);
           reset(&buf_2);
           appendarray(&buf_2,const_130,72);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_40,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_40,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_57,48);
           reset(&buf_6);
           appendarray(&buf_6,const_57,48);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           reset(&buf_2);
           appendarray(&buf_2,const_196,184);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
        {
           if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_62,112);
              reset(&buf_6);
              appendarray(&buf_6,const_62,112);
              reset(&buf_3);
              appendarray(&buf_3,const_71,72);
              reset(&buf_2);
              appendarray(&buf_2,const_71,72);
              output(&buf_1);
              consume(14);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_61,48);
           reset(&buf_6);
           appendarray(&buf_6,const_61,48);
           reset(&buf_3);
           appendarray(&buf_3,const_72,40);
           reset(&buf_2);
           appendarray(&buf_2,const_72,40);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'x') && 1)))
        {
           if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_63,72);
              reset(&buf_6);
              appendarray(&buf_6,const_63,72);
              reset(&buf_3);
              appendarray(&buf_3,const_209,80);
              reset(&buf_2);
              appendarray(&buf_2,const_209,80);
              output(&buf_1);
              consume(9);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_64,56);
              reset(&buf_6);
              appendarray(&buf_6,const_64,56);
              reset(&buf_3);
              appendarray(&buf_3,const_208,80);
              reset(&buf_2);
              appendarray(&buf_2,const_208,80);
              output(&buf_1);
              consume(7);
              goto l6;
           }
        }
        output(&buf_1);
        consume(1);
        goto l20;
     }
     if (((avail >= 1) && ((next[0] == 'f') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_66,32);
           reset(&buf_6);
           appendarray(&buf_6,const_66,32);
           reset(&buf_3);
           appendarray(&buf_3,const_68,48);
           reset(&buf_2);
           appendarray(&buf_2,const_68,48);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_69,64);
           reset(&buf_6);
           appendarray(&buf_6,const_69,64);
           reset(&buf_3);
           appendarray(&buf_3,const_27,64);
           reset(&buf_2);
           appendarray(&buf_2,const_27,64);
           output(&buf_1);
           consume(8);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_73,40);
           reset(&buf_6);
           appendarray(&buf_6,const_73,40);
           reset(&buf_3);
           appendarray(&buf_3,const_81,88);
           reset(&buf_2);
           appendarray(&buf_2,const_81,88);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_76,64);
              reset(&buf_6);
              appendarray(&buf_6,const_76,64);
              reset(&buf_3);
              appendarray(&buf_3,const_85,80);
              reset(&buf_2);
              appendarray(&buf_2,const_85,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_77,96);
              reset(&buf_6);
              appendarray(&buf_6,const_77,96);
              reset(&buf_3);
              appendarray(&buf_3,const_78,96);
              reset(&buf_2);
              appendarray(&buf_2,const_78,96);
              output(&buf_1);
              consume(12);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_65,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_65,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'g') && 1)))
     {
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_80,128);
           reset(&buf_6);
           appendarray(&buf_6,const_80,128);
           reset(&buf_3);
           appendarray(&buf_3,const_183,104);
           reset(&buf_2);
           appendarray(&buf_2,const_183,104);
           output(&buf_1);
           consume(16);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
        {
           output(&buf_1);
           consume(7);
           goto l25;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_79,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_79,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'h') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_93,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_93,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_67,112);
                 reset(&buf_2);
                 appendarray(&buf_2,const_67,112);
                 output(&buf_1);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_94,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_94,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_124,64);
                 reset(&buf_2);
                 appendarray(&buf_2,const_124,64);
                 output(&buf_1);
                 consume(8);
                 goto l6;
              }
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_95,96);
                 reset(&buf_6);
                 appendarray(&buf_6,const_95,96);
                 reset(&buf_3);
                 appendarray(&buf_3,const_88,128);
                 reset(&buf_2);
                 appendarray(&buf_2,const_88,128);
                 output(&buf_1);
                 consume(12);
                 goto l6;
              }
              if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_96,56);
                 reset(&buf_6);
                 appendarray(&buf_6,const_96,56);
                 reset(&buf_3);
                 appendarray(&buf_3,const_89,104);
                 reset(&buf_2);
                 appendarray(&buf_2,const_89,104);
                 output(&buf_1);
                 consume(7);
                 goto l6;
              }
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_90,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_90,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'i') && 1)))
     {
        if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_103,88);
           reset(&buf_6);
           appendarray(&buf_6,const_103,88);
           reset(&buf_3);
           appendarray(&buf_3,const_75,72);
           reset(&buf_2);
           appendarray(&buf_2,const_75,72);
           output(&buf_1);
           consume(11);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_101,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_101,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'k') && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_108,64);
           reset(&buf_6);
           appendarray(&buf_6,const_108,64);
           reset(&buf_3);
           appendarray(&buf_3,const_193,64);
           reset(&buf_2);
           appendarray(&buf_2,const_193,64);
           output(&buf_1);
           consume(8);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_105,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_105,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'l') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_115,48);
           reset(&buf_6);
           appendarray(&buf_6,const_115,48);
           reset(&buf_3);
           appendarray(&buf_3,const_123,96);
           reset(&buf_2);
           appendarray(&buf_2,const_123,96);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_117,32);
           reset(&buf_6);
           appendarray(&buf_6,const_117,32);
           reset(&buf_3);
           appendarray(&buf_3,const_100,48);
           reset(&buf_2);
           appendarray(&buf_2,const_100,48);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_118,72);
           reset(&buf_6);
           appendarray(&buf_6,const_118,72);
           reset(&buf_3);
           appendarray(&buf_3,const_110,72);
           reset(&buf_2);
           appendarray(&buf_2,const_110,72);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_114,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_114,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'm') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
           {
              if (((avail >= 5) && ((next[4] == 's') && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_128,40);
                 reset(&buf_6);
                 appendarray(&buf_6,const_128,40);
                 reset(&buf_3);
                 appendarray(&buf_3,const_127,40);
                 reset(&buf_2);
                 appendarray(&buf_2,const_127,40);
                 output(&buf_1);
                 consume(5);
                 goto l6;
              }
              reset(&buf_7);
              appendarray(&buf_7,const_126,32);
              reset(&buf_6);
              appendarray(&buf_6,const_126,32);
              reset(&buf_3);
              appendarray(&buf_3,const_125,24);
              reset(&buf_2);
              appendarray(&buf_2,const_125,24);
              output(&buf_1);
              consume(4);
              goto l6;
           }
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_129,40);
              reset(&buf_6);
              appendarray(&buf_6,const_129,40);
              reset(&buf_3);
              appendarray(&buf_3,const_70,32);
              reset(&buf_2);
              appendarray(&buf_2,const_70,32);
              output(&buf_1);
              consume(5);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_121,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_121,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'n') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_132,72);
           reset(&buf_6);
           appendarray(&buf_6,const_132,72);
           reset(&buf_3);
           appendarray(&buf_3,const_133,64);
           reset(&buf_2);
           appendarray(&buf_2,const_133,64);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_134,32);
           reset(&buf_6);
           appendarray(&buf_6,const_134,32);
           reset(&buf_3);
           appendarray(&buf_3,const_49,56);
           reset(&buf_2);
           appendarray(&buf_2,const_49,56);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_135,24);
           reset(&buf_6);
           appendarray(&buf_6,const_135,24);
           reset(&buf_3);
           appendarray(&buf_3,const_102,32);
           reset(&buf_2);
           appendarray(&buf_2,const_102,32);
           output(&buf_1);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_136,56);
           reset(&buf_6);
           appendarray(&buf_6,const_136,56);
           reset(&buf_3);
           appendarray(&buf_3,const_192,56);
           reset(&buf_2);
           appendarray(&buf_2,const_192,56);
           output(&buf_1);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_131,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_131,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'o') && 1)))
     {
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_139,80);
           reset(&buf_6);
           appendarray(&buf_6,const_139,80);
           reset(&buf_3);
           appendarray(&buf_3,const_225,96);
           reset(&buf_2);
           appendarray(&buf_2,const_225,96);
           output(&buf_1);
           consume(10);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_137,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_137,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'p') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_142,88);
              reset(&buf_6);
              appendarray(&buf_6,const_142,88);
              reset(&buf_3);
              appendarray(&buf_3,const_138,40);
              reset(&buf_2);
              appendarray(&buf_2,const_138,40);
              output(&buf_1);
              consume(11);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_143,64);
              reset(&buf_6);
              appendarray(&buf_6,const_143,64);
              reset(&buf_3);
              appendarray(&buf_3,const_120,48);
              reset(&buf_2);
              appendarray(&buf_2,const_120,48);
              output(&buf_1);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_144,16);
           reset(&buf_6);
           appendarray(&buf_6,const_144,16);
           reset(&buf_3);
           appendarray(&buf_3,const_97,104);
           reset(&buf_2);
           appendarray(&buf_2,const_97,104);
           output(&buf_1);
           consume(2);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_145,40);
           reset(&buf_6);
           appendarray(&buf_6,const_145,40);
           reset(&buf_3);
           appendarray(&buf_3,const_189,64);
           reset(&buf_2);
           appendarray(&buf_2,const_189,64);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_146,72);
           reset(&buf_6);
           appendarray(&buf_6,const_146,72);
           reset(&buf_3);
           appendarray(&buf_3,const_164,64);
           reset(&buf_2);
           appendarray(&buf_2,const_164,64);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_148,56);
           reset(&buf_6);
           appendarray(&buf_6,const_148,56);
           reset(&buf_3);
           appendarray(&buf_3,const_116,96);
           reset(&buf_2);
           appendarray(&buf_2,const_116,96);
           output(&buf_1);
           consume(7);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'u') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
           {
              output(&buf_1);
              consume(4);
              goto l21;
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_153,32);
              reset(&buf_6);
              appendarray(&buf_6,const_153,32);
              reset(&buf_3);
              appendarray(&buf_3,const_176,32);
              reset(&buf_2);
              appendarray(&buf_2,const_176,32);
              output(&buf_1);
              consume(4);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_141,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_141,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'q') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_155,48);
           reset(&buf_6);
           appendarray(&buf_6,const_155,48);
           reset(&buf_3);
           appendarray(&buf_3,const_147,64);
           reset(&buf_2);
           appendarray(&buf_2,const_147,64);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_156,40);
           reset(&buf_6);
           appendarray(&buf_6,const_156,40);
           reset(&buf_3);
           appendarray(&buf_3,const_113,32);
           reset(&buf_2);
           appendarray(&buf_2,const_113,32);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_154,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_154,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'r') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_158,104);
              reset(&buf_6);
              appendarray(&buf_6,const_158,104);
              reset(&buf_3);
              appendarray(&buf_3,const_106,104);
              reset(&buf_2);
              appendarray(&buf_2,const_106,104);
              output(&buf_1);
              consume(13);
              goto l6;
           }
           if (((avail >= 3) && ((next[2] == 'm') && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_159,24);
              reset(&buf_6);
              appendarray(&buf_6,const_159,24);
              reset(&buf_3);
              appendarray(&buf_3,const_18,96);
              reset(&buf_2);
              appendarray(&buf_2,const_18,96);
              output(&buf_1);
              consume(3);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_162,80);
              reset(&buf_6);
              appendarray(&buf_6,const_162,80);
              reset(&buf_3);
              appendarray(&buf_3,const_87,40);
              reset(&buf_2);
              appendarray(&buf_2,const_87,40);
              output(&buf_1);
              consume(10);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_163,56);
              reset(&buf_6);
              appendarray(&buf_6,const_163,56);
              reset(&buf_3);
              appendarray(&buf_3,const_17,72);
              reset(&buf_2);
              appendarray(&buf_2,const_17,72);
              output(&buf_1);
              consume(7);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_157,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_157,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_166,40);
              reset(&buf_6);
              appendarray(&buf_6,const_166,40);
              reset(&buf_3);
              appendarray(&buf_3,const_213,72);
              reset(&buf_2);
              appendarray(&buf_2,const_213,72);
              output(&buf_1);
              consume(5);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_167,80);
              reset(&buf_6);
              appendarray(&buf_6,const_167,80);
              reset(&buf_3);
              appendarray(&buf_3,const_179,104);
              reset(&buf_2);
              appendarray(&buf_2,const_179,104);
              output(&buf_1);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_168,32);
           reset(&buf_6);
           appendarray(&buf_6,const_168,32);
           reset(&buf_3);
           appendarray(&buf_3,const_23,88);
           reset(&buf_2);
           appendarray(&buf_2,const_23,88);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
        {
           if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_170,88);
              reset(&buf_6);
              appendarray(&buf_6,const_170,88);
              reset(&buf_3);
              appendarray(&buf_3,const_174,88);
              reset(&buf_2);
              appendarray(&buf_2,const_174,88);
              output(&buf_1);
              consume(11);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_169,40);
           reset(&buf_6);
           appendarray(&buf_6,const_169,40);
           reset(&buf_3);
           appendarray(&buf_3,const_111,48);
           reset(&buf_2);
           appendarray(&buf_2,const_111,48);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_180,80);
           reset(&buf_6);
           appendarray(&buf_6,const_180,80);
           reset(&buf_3);
           appendarray(&buf_3,const_43,56);
           reset(&buf_2);
           appendarray(&buf_2,const_43,56);
           output(&buf_1);
           consume(10);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'o') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_181,64);
              reset(&buf_6);
              appendarray(&buf_6,const_181,64);
              reset(&buf_3);
              appendarray(&buf_3,const_149,80);
              reset(&buf_2);
              appendarray(&buf_2,const_149,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_182,80);
              reset(&buf_6);
              appendarray(&buf_6,const_182,80);
              reset(&buf_3);
              appendarray(&buf_3,const_109,80);
              reset(&buf_2);
              appendarray(&buf_2,const_109,80);
              output(&buf_1);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 't') && 1)))
        {
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_186,56);
              reset(&buf_6);
              appendarray(&buf_6,const_186,56);
              reset(&buf_3);
              appendarray(&buf_3,const_99,104);
              reset(&buf_2);
              appendarray(&buf_2,const_99,104);
              output(&buf_1);
              consume(7);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_187,56);
              reset(&buf_6);
              appendarray(&buf_6,const_187,56);
              reset(&buf_3);
              appendarray(&buf_3,const_44,72);
              reset(&buf_2);
              appendarray(&buf_2,const_44,72);
              output(&buf_1);
              consume(7);
              goto l6;
           }
        }
        if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_188,104);
           reset(&buf_6);
           appendarray(&buf_6,const_188,104);
           reset(&buf_3);
           appendarray(&buf_3,const_202,80);
           reset(&buf_2);
           appendarray(&buf_2,const_202,80);
           output(&buf_1);
           consume(13);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_165,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_165,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 't') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_191,48);
           reset(&buf_6);
           appendarray(&buf_6,const_191,48);
           reset(&buf_3);
           appendarray(&buf_3,const_194,96);
           reset(&buf_2);
           appendarray(&buf_2,const_194,96);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
        {
           output(&buf_1);
           consume(3);
           goto l23;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_204,40);
           reset(&buf_6);
           appendarray(&buf_6,const_204,40);
           reset(&buf_3);
           appendarray(&buf_3,const_203,40);
           reset(&buf_2);
           appendarray(&buf_2,const_203,40);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_205,40);
           reset(&buf_6);
           appendarray(&buf_6,const_205,40);
           reset(&buf_3);
           appendarray(&buf_3,const_112,40);
           reset(&buf_2);
           appendarray(&buf_2,const_112,40);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_190,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_190,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'u') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_210,72);
           reset(&buf_6);
           appendarray(&buf_6,const_210,72);
           reset(&buf_3);
           appendarray(&buf_3,const_86,64);
           reset(&buf_2);
           appendarray(&buf_2,const_86,64);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_206,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_206,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'v') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_212,56);
           reset(&buf_6);
           appendarray(&buf_6,const_212,56);
           reset(&buf_3);
           appendarray(&buf_3,const_207,48);
           reset(&buf_2);
           appendarray(&buf_2,const_207,48);
           output(&buf_1);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_211,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_211,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'w') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 3) && ((next[2] == 'b') && 1)))
           {
              if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_218,80);
                 reset(&buf_6);
                 appendarray(&buf_6,const_218,80);
                 reset(&buf_3);
                 appendarray(&buf_3,const_185,104);
                 reset(&buf_2);
                 appendarray(&buf_2,const_185,104);
                 output(&buf_1);
                 consume(10);
                 goto l6;
              }
              output(&buf_1);
              consume(3);
              goto l7;
           }
           output(&buf_1);
           consume(2);
           goto l22;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_223,80);
           reset(&buf_6);
           appendarray(&buf_6,const_223,80);
           reset(&buf_3);
           appendarray(&buf_3,const_24,72);
           reset(&buf_2);
           appendarray(&buf_2,const_24,72);
           output(&buf_1);
           consume(10);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_224,40);
           reset(&buf_6);
           appendarray(&buf_6,const_224,40);
           reset(&buf_3);
           appendarray(&buf_3,const_74,56);
           reset(&buf_2);
           appendarray(&buf_2,const_74,56);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_215,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_215,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_8);
        append(&buf_8,tbl[0][next[0]],8);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_4);
        append(&buf_4,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l29;
     }
     goto fail;
l18: if (!readnext(1, 16))
     {
        output(&buf_1);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        reset(&buf_5);
        concat(&buf_5,&buf_3);
        append(&buf_5,tbl[0][next[0]],8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l17;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
     {
        reset(&buf_5);
        append(&buf_5,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'D') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_3,48);
           reset(&buf_6);
           appendarray(&buf_6,const_3,48);
           reset(&buf_3);
           appendarray(&buf_3,const_4,96);
           reset(&buf_2);
           appendarray(&buf_2,const_4,96);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_2,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_2,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'P') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_6,40);
           reset(&buf_6);
           appendarray(&buf_6,const_6,40);
           reset(&buf_3);
           appendarray(&buf_3,const_7,32);
           reset(&buf_2);
           appendarray(&buf_2,const_7,32);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_5,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_5,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'W') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_10,48);
           reset(&buf_6);
           appendarray(&buf_6,const_10,48);
           reset(&buf_3);
           appendarray(&buf_3,const_8,48);
           reset(&buf_2);
           appendarray(&buf_2,const_8,48);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_9,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_9,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'a') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
        {
           output(&buf_1);
           consume(5);
           goto l26;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_11,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_11,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'b') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
        {
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_20,96);
              reset(&buf_6);
              appendarray(&buf_6,const_20,96);
              reset(&buf_3);
              appendarray(&buf_3,const_25,136);
              reset(&buf_2);
              appendarray(&buf_2,const_25,136);
              output(&buf_1);
              consume(12);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_21,48);
              reset(&buf_6);
              appendarray(&buf_6,const_21,48);
              reset(&buf_3);
              appendarray(&buf_3,const_171,112);
              reset(&buf_2);
              appendarray(&buf_2,const_171,112);
              output(&buf_1);
              consume(6);
              goto l6;
           }
        }
        if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_26,112);
           reset(&buf_6);
           appendarray(&buf_6,const_26,112);
           reset(&buf_3);
           appendarray(&buf_3,const_50,120);
           reset(&buf_2);
           appendarray(&buf_2,const_50,120);
           output(&buf_1);
           consume(14);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_29,24);
           reset(&buf_6);
           appendarray(&buf_6,const_29,24);
           reset(&buf_3);
           appendarray(&buf_3,const_119,24);
           reset(&buf_2);
           appendarray(&buf_2,const_119,24);
           output(&buf_1);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
        {
           output(&buf_1);
           consume(7);
           goto l24;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_19,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_19,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'c') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_33,64);
              reset(&buf_6);
              appendarray(&buf_6,const_33,64);
              reset(&buf_3);
              appendarray(&buf_3,const_175,80);
              reset(&buf_2);
              appendarray(&buf_2,const_175,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_34,112);
              reset(&buf_6);
              appendarray(&buf_6,const_34,112);
              reset(&buf_3);
              appendarray(&buf_3,const_214,72);
              reset(&buf_2);
              appendarray(&buf_2,const_214,72);
              output(&buf_1);
              consume(14);
              goto l6;
           }
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_36,40);
           reset(&buf_6);
           appendarray(&buf_6,const_36,40);
           reset(&buf_3);
           appendarray(&buf_3,const_178,24);
           reset(&buf_2);
           appendarray(&buf_2,const_178,24);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_37,64);
              reset(&buf_6);
              appendarray(&buf_6,const_37,64);
              reset(&buf_3);
              appendarray(&buf_3,const_140,88);
              reset(&buf_2);
              appendarray(&buf_2,const_140,88);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_38,64);
              reset(&buf_6);
              appendarray(&buf_6,const_38,64);
              reset(&buf_3);
              appendarray(&buf_3,const_45,56);
              reset(&buf_2);
              appendarray(&buf_2,const_45,56);
              output(&buf_1);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_39,24);
           reset(&buf_6);
           appendarray(&buf_6,const_39,24);
           reset(&buf_3);
           appendarray(&buf_3,const_35,184);
           reset(&buf_2);
           appendarray(&buf_2,const_35,184);
           output(&buf_1);
           consume(3);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_32,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_32,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'd') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_41,48);
              reset(&buf_6);
              appendarray(&buf_6,const_41,48);
              reset(&buf_3);
              appendarray(&buf_3,const_4,96);
              reset(&buf_2);
              appendarray(&buf_2,const_4,96);
              output(&buf_1);
              consume(6);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_42,64);
              reset(&buf_6);
              appendarray(&buf_6,const_42,64);
              reset(&buf_3);
              appendarray(&buf_3,const_107,64);
              reset(&buf_2);
              appendarray(&buf_2,const_107,64);
              output(&buf_1);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_46,64);
              reset(&buf_6);
              appendarray(&buf_6,const_46,64);
              reset(&buf_3);
              appendarray(&buf_3,const_54,80);
              reset(&buf_2);
              appendarray(&buf_2,const_54,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_47,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_47,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_15,56);
                 reset(&buf_2);
                 appendarray(&buf_2,const_15,56);
                 output(&buf_1);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_48,72);
                 reset(&buf_6);
                 appendarray(&buf_6,const_48,72);
                 reset(&buf_3);
                 appendarray(&buf_3,const_16,72);
                 reset(&buf_2);
                 appendarray(&buf_2,const_16,72);
                 output(&buf_1);
                 consume(9);
                 goto l6;
              }
           }
        }
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_52,128);
           reset(&buf_6);
           appendarray(&buf_6,const_52,128);
           reset(&buf_3);
           appendarray(&buf_3,const_51,96);
           reset(&buf_2);
           appendarray(&buf_2,const_51,96);
           output(&buf_1);
           consume(16);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_53,40);
           reset(&buf_6);
           appendarray(&buf_6,const_53,40);
           reset(&buf_3);
           appendarray(&buf_3,const_130,72);
           reset(&buf_2);
           appendarray(&buf_2,const_130,72);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_40,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_40,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_57,48);
           reset(&buf_6);
           appendarray(&buf_6,const_57,48);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           reset(&buf_2);
           appendarray(&buf_2,const_196,184);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
        {
           if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_62,112);
              reset(&buf_6);
              appendarray(&buf_6,const_62,112);
              reset(&buf_3);
              appendarray(&buf_3,const_71,72);
              reset(&buf_2);
              appendarray(&buf_2,const_71,72);
              output(&buf_1);
              consume(14);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_61,48);
           reset(&buf_6);
           appendarray(&buf_6,const_61,48);
           reset(&buf_3);
           appendarray(&buf_3,const_72,40);
           reset(&buf_2);
           appendarray(&buf_2,const_72,40);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'x') && 1)))
        {
           if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_63,72);
              reset(&buf_6);
              appendarray(&buf_6,const_63,72);
              reset(&buf_3);
              appendarray(&buf_3,const_209,80);
              reset(&buf_2);
              appendarray(&buf_2,const_209,80);
              output(&buf_1);
              consume(9);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_64,56);
              reset(&buf_6);
              appendarray(&buf_6,const_64,56);
              reset(&buf_3);
              appendarray(&buf_3,const_208,80);
              reset(&buf_2);
              appendarray(&buf_2,const_208,80);
              output(&buf_1);
              consume(7);
              goto l6;
           }
        }
        output(&buf_1);
        consume(1);
        goto l20;
     }
     if (((avail >= 1) && ((next[0] == 'f') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_66,32);
           reset(&buf_6);
           appendarray(&buf_6,const_66,32);
           reset(&buf_3);
           appendarray(&buf_3,const_68,48);
           reset(&buf_2);
           appendarray(&buf_2,const_68,48);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_69,64);
           reset(&buf_6);
           appendarray(&buf_6,const_69,64);
           reset(&buf_3);
           appendarray(&buf_3,const_27,64);
           reset(&buf_2);
           appendarray(&buf_2,const_27,64);
           output(&buf_1);
           consume(8);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_73,40);
           reset(&buf_6);
           appendarray(&buf_6,const_73,40);
           reset(&buf_3);
           appendarray(&buf_3,const_81,88);
           reset(&buf_2);
           appendarray(&buf_2,const_81,88);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_76,64);
              reset(&buf_6);
              appendarray(&buf_6,const_76,64);
              reset(&buf_3);
              appendarray(&buf_3,const_85,80);
              reset(&buf_2);
              appendarray(&buf_2,const_85,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_77,96);
              reset(&buf_6);
              appendarray(&buf_6,const_77,96);
              reset(&buf_3);
              appendarray(&buf_3,const_78,96);
              reset(&buf_2);
              appendarray(&buf_2,const_78,96);
              output(&buf_1);
              consume(12);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_65,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_65,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'g') && 1)))
     {
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_80,128);
           reset(&buf_6);
           appendarray(&buf_6,const_80,128);
           reset(&buf_3);
           appendarray(&buf_3,const_183,104);
           reset(&buf_2);
           appendarray(&buf_2,const_183,104);
           output(&buf_1);
           consume(16);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
        {
           output(&buf_1);
           consume(7);
           goto l25;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_79,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_79,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'h') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_93,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_93,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_67,112);
                 reset(&buf_2);
                 appendarray(&buf_2,const_67,112);
                 output(&buf_1);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_94,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_94,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_124,64);
                 reset(&buf_2);
                 appendarray(&buf_2,const_124,64);
                 output(&buf_1);
                 consume(8);
                 goto l6;
              }
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_95,96);
                 reset(&buf_6);
                 appendarray(&buf_6,const_95,96);
                 reset(&buf_3);
                 appendarray(&buf_3,const_88,128);
                 reset(&buf_2);
                 appendarray(&buf_2,const_88,128);
                 output(&buf_1);
                 consume(12);
                 goto l6;
              }
              if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_96,56);
                 reset(&buf_6);
                 appendarray(&buf_6,const_96,56);
                 reset(&buf_3);
                 appendarray(&buf_3,const_89,104);
                 reset(&buf_2);
                 appendarray(&buf_2,const_89,104);
                 output(&buf_1);
                 consume(7);
                 goto l6;
              }
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_90,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_90,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'i') && 1)))
     {
        if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_103,88);
           reset(&buf_6);
           appendarray(&buf_6,const_103,88);
           reset(&buf_3);
           appendarray(&buf_3,const_75,72);
           reset(&buf_2);
           appendarray(&buf_2,const_75,72);
           output(&buf_1);
           consume(11);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_101,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_101,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'k') && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_108,64);
           reset(&buf_6);
           appendarray(&buf_6,const_108,64);
           reset(&buf_3);
           appendarray(&buf_3,const_193,64);
           reset(&buf_2);
           appendarray(&buf_2,const_193,64);
           output(&buf_1);
           consume(8);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_105,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_105,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'l') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_115,48);
           reset(&buf_6);
           appendarray(&buf_6,const_115,48);
           reset(&buf_3);
           appendarray(&buf_3,const_123,96);
           reset(&buf_2);
           appendarray(&buf_2,const_123,96);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_117,32);
           reset(&buf_6);
           appendarray(&buf_6,const_117,32);
           reset(&buf_3);
           appendarray(&buf_3,const_100,48);
           reset(&buf_2);
           appendarray(&buf_2,const_100,48);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_118,72);
           reset(&buf_6);
           appendarray(&buf_6,const_118,72);
           reset(&buf_3);
           appendarray(&buf_3,const_110,72);
           reset(&buf_2);
           appendarray(&buf_2,const_110,72);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_114,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_114,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'm') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
           {
              if (((avail >= 5) && ((next[4] == 's') && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_128,40);
                 reset(&buf_6);
                 appendarray(&buf_6,const_128,40);
                 reset(&buf_3);
                 appendarray(&buf_3,const_127,40);
                 reset(&buf_2);
                 appendarray(&buf_2,const_127,40);
                 output(&buf_1);
                 consume(5);
                 goto l6;
              }
              reset(&buf_7);
              appendarray(&buf_7,const_126,32);
              reset(&buf_6);
              appendarray(&buf_6,const_126,32);
              reset(&buf_3);
              appendarray(&buf_3,const_125,24);
              reset(&buf_2);
              appendarray(&buf_2,const_125,24);
              output(&buf_1);
              consume(4);
              goto l6;
           }
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_129,40);
              reset(&buf_6);
              appendarray(&buf_6,const_129,40);
              reset(&buf_3);
              appendarray(&buf_3,const_70,32);
              reset(&buf_2);
              appendarray(&buf_2,const_70,32);
              output(&buf_1);
              consume(5);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_121,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_121,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'n') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_132,72);
           reset(&buf_6);
           appendarray(&buf_6,const_132,72);
           reset(&buf_3);
           appendarray(&buf_3,const_133,64);
           reset(&buf_2);
           appendarray(&buf_2,const_133,64);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_134,32);
           reset(&buf_6);
           appendarray(&buf_6,const_134,32);
           reset(&buf_3);
           appendarray(&buf_3,const_49,56);
           reset(&buf_2);
           appendarray(&buf_2,const_49,56);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_135,24);
           reset(&buf_6);
           appendarray(&buf_6,const_135,24);
           reset(&buf_3);
           appendarray(&buf_3,const_102,32);
           reset(&buf_2);
           appendarray(&buf_2,const_102,32);
           output(&buf_1);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_136,56);
           reset(&buf_6);
           appendarray(&buf_6,const_136,56);
           reset(&buf_3);
           appendarray(&buf_3,const_192,56);
           reset(&buf_2);
           appendarray(&buf_2,const_192,56);
           output(&buf_1);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_131,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_131,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'o') && 1)))
     {
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_139,80);
           reset(&buf_6);
           appendarray(&buf_6,const_139,80);
           reset(&buf_3);
           appendarray(&buf_3,const_225,96);
           reset(&buf_2);
           appendarray(&buf_2,const_225,96);
           output(&buf_1);
           consume(10);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_137,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_137,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'p') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_142,88);
              reset(&buf_6);
              appendarray(&buf_6,const_142,88);
              reset(&buf_3);
              appendarray(&buf_3,const_138,40);
              reset(&buf_2);
              appendarray(&buf_2,const_138,40);
              output(&buf_1);
              consume(11);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_143,64);
              reset(&buf_6);
              appendarray(&buf_6,const_143,64);
              reset(&buf_3);
              appendarray(&buf_3,const_120,48);
              reset(&buf_2);
              appendarray(&buf_2,const_120,48);
              output(&buf_1);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_144,16);
           reset(&buf_6);
           appendarray(&buf_6,const_144,16);
           reset(&buf_3);
           appendarray(&buf_3,const_97,104);
           reset(&buf_2);
           appendarray(&buf_2,const_97,104);
           output(&buf_1);
           consume(2);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_145,40);
           reset(&buf_6);
           appendarray(&buf_6,const_145,40);
           reset(&buf_3);
           appendarray(&buf_3,const_189,64);
           reset(&buf_2);
           appendarray(&buf_2,const_189,64);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_146,72);
           reset(&buf_6);
           appendarray(&buf_6,const_146,72);
           reset(&buf_3);
           appendarray(&buf_3,const_164,64);
           reset(&buf_2);
           appendarray(&buf_2,const_164,64);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_148,56);
           reset(&buf_6);
           appendarray(&buf_6,const_148,56);
           reset(&buf_3);
           appendarray(&buf_3,const_116,96);
           reset(&buf_2);
           appendarray(&buf_2,const_116,96);
           output(&buf_1);
           consume(7);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'u') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
           {
              output(&buf_1);
              consume(4);
              goto l21;
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_153,32);
              reset(&buf_6);
              appendarray(&buf_6,const_153,32);
              reset(&buf_3);
              appendarray(&buf_3,const_176,32);
              reset(&buf_2);
              appendarray(&buf_2,const_176,32);
              output(&buf_1);
              consume(4);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_141,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_141,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'q') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_155,48);
           reset(&buf_6);
           appendarray(&buf_6,const_155,48);
           reset(&buf_3);
           appendarray(&buf_3,const_147,64);
           reset(&buf_2);
           appendarray(&buf_2,const_147,64);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_156,40);
           reset(&buf_6);
           appendarray(&buf_6,const_156,40);
           reset(&buf_3);
           appendarray(&buf_3,const_113,32);
           reset(&buf_2);
           appendarray(&buf_2,const_113,32);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_154,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_154,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'r') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_158,104);
              reset(&buf_6);
              appendarray(&buf_6,const_158,104);
              reset(&buf_3);
              appendarray(&buf_3,const_106,104);
              reset(&buf_2);
              appendarray(&buf_2,const_106,104);
              output(&buf_1);
              consume(13);
              goto l6;
           }
           if (((avail >= 3) && ((next[2] == 'm') && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_159,24);
              reset(&buf_6);
              appendarray(&buf_6,const_159,24);
              reset(&buf_3);
              appendarray(&buf_3,const_18,96);
              reset(&buf_2);
              appendarray(&buf_2,const_18,96);
              output(&buf_1);
              consume(3);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_162,80);
              reset(&buf_6);
              appendarray(&buf_6,const_162,80);
              reset(&buf_3);
              appendarray(&buf_3,const_87,40);
              reset(&buf_2);
              appendarray(&buf_2,const_87,40);
              output(&buf_1);
              consume(10);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_163,56);
              reset(&buf_6);
              appendarray(&buf_6,const_163,56);
              reset(&buf_3);
              appendarray(&buf_3,const_17,72);
              reset(&buf_2);
              appendarray(&buf_2,const_17,72);
              output(&buf_1);
              consume(7);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_157,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_157,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_166,40);
              reset(&buf_6);
              appendarray(&buf_6,const_166,40);
              reset(&buf_3);
              appendarray(&buf_3,const_213,72);
              reset(&buf_2);
              appendarray(&buf_2,const_213,72);
              output(&buf_1);
              consume(5);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_167,80);
              reset(&buf_6);
              appendarray(&buf_6,const_167,80);
              reset(&buf_3);
              appendarray(&buf_3,const_179,104);
              reset(&buf_2);
              appendarray(&buf_2,const_179,104);
              output(&buf_1);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_168,32);
           reset(&buf_6);
           appendarray(&buf_6,const_168,32);
           reset(&buf_3);
           appendarray(&buf_3,const_23,88);
           reset(&buf_2);
           appendarray(&buf_2,const_23,88);
           output(&buf_1);
           consume(4);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
        {
           if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_170,88);
              reset(&buf_6);
              appendarray(&buf_6,const_170,88);
              reset(&buf_3);
              appendarray(&buf_3,const_174,88);
              reset(&buf_2);
              appendarray(&buf_2,const_174,88);
              output(&buf_1);
              consume(11);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_169,40);
           reset(&buf_6);
           appendarray(&buf_6,const_169,40);
           reset(&buf_3);
           appendarray(&buf_3,const_111,48);
           reset(&buf_2);
           appendarray(&buf_2,const_111,48);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_180,80);
           reset(&buf_6);
           appendarray(&buf_6,const_180,80);
           reset(&buf_3);
           appendarray(&buf_3,const_43,56);
           reset(&buf_2);
           appendarray(&buf_2,const_43,56);
           output(&buf_1);
           consume(10);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'o') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_181,64);
              reset(&buf_6);
              appendarray(&buf_6,const_181,64);
              reset(&buf_3);
              appendarray(&buf_3,const_149,80);
              reset(&buf_2);
              appendarray(&buf_2,const_149,80);
              output(&buf_1);
              consume(8);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_182,80);
              reset(&buf_6);
              appendarray(&buf_6,const_182,80);
              reset(&buf_3);
              appendarray(&buf_3,const_109,80);
              reset(&buf_2);
              appendarray(&buf_2,const_109,80);
              output(&buf_1);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 't') && 1)))
        {
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_186,56);
              reset(&buf_6);
              appendarray(&buf_6,const_186,56);
              reset(&buf_3);
              appendarray(&buf_3,const_99,104);
              reset(&buf_2);
              appendarray(&buf_2,const_99,104);
              output(&buf_1);
              consume(7);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_187,56);
              reset(&buf_6);
              appendarray(&buf_6,const_187,56);
              reset(&buf_3);
              appendarray(&buf_3,const_44,72);
              reset(&buf_2);
              appendarray(&buf_2,const_44,72);
              output(&buf_1);
              consume(7);
              goto l6;
           }
        }
        if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_188,104);
           reset(&buf_6);
           appendarray(&buf_6,const_188,104);
           reset(&buf_3);
           appendarray(&buf_3,const_202,80);
           reset(&buf_2);
           appendarray(&buf_2,const_202,80);
           output(&buf_1);
           consume(13);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_165,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_165,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 't') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_191,48);
           reset(&buf_6);
           appendarray(&buf_6,const_191,48);
           reset(&buf_3);
           appendarray(&buf_3,const_194,96);
           reset(&buf_2);
           appendarray(&buf_2,const_194,96);
           output(&buf_1);
           consume(6);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
        {
           output(&buf_1);
           consume(3);
           goto l23;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_204,40);
           reset(&buf_6);
           appendarray(&buf_6,const_204,40);
           reset(&buf_3);
           appendarray(&buf_3,const_203,40);
           reset(&buf_2);
           appendarray(&buf_2,const_203,40);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_205,40);
           reset(&buf_6);
           appendarray(&buf_6,const_205,40);
           reset(&buf_3);
           appendarray(&buf_3,const_112,40);
           reset(&buf_2);
           appendarray(&buf_2,const_112,40);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_190,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_190,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'u') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_210,72);
           reset(&buf_6);
           appendarray(&buf_6,const_210,72);
           reset(&buf_3);
           appendarray(&buf_3,const_86,64);
           reset(&buf_2);
           appendarray(&buf_2,const_86,64);
           output(&buf_1);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_206,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_206,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'v') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_212,56);
           reset(&buf_6);
           appendarray(&buf_6,const_212,56);
           reset(&buf_3);
           appendarray(&buf_3,const_207,48);
           reset(&buf_2);
           appendarray(&buf_2,const_207,48);
           output(&buf_1);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_211,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_211,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'w') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 3) && ((next[2] == 'b') && 1)))
           {
              if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_218,80);
                 reset(&buf_6);
                 appendarray(&buf_6,const_218,80);
                 reset(&buf_3);
                 appendarray(&buf_3,const_185,104);
                 reset(&buf_2);
                 appendarray(&buf_2,const_185,104);
                 output(&buf_1);
                 consume(10);
                 goto l6;
              }
              output(&buf_1);
              consume(3);
              goto l7;
           }
           output(&buf_1);
           consume(2);
           goto l22;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_223,80);
           reset(&buf_6);
           appendarray(&buf_6,const_223,80);
           reset(&buf_3);
           appendarray(&buf_3,const_24,72);
           reset(&buf_2);
           appendarray(&buf_2,const_24,72);
           output(&buf_1);
           consume(10);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_224,40);
           reset(&buf_6);
           appendarray(&buf_6,const_224,40);
           reset(&buf_3);
           appendarray(&buf_3,const_74,56);
           reset(&buf_2);
           appendarray(&buf_2,const_74,56);
           output(&buf_1);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_215,8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_215,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_8);
        append(&buf_8,tbl[0][next[0]],8);
        reset(&buf_5);
        concat(&buf_5,&buf_3);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_4);
        append(&buf_4,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l29;
     }
     goto fail;
l19: if (!readnext(1, 16))
     {
        output(&buf_2);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_2);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
     {
        reset(&buf_5);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        append(&buf_1,tbl[0][next[0]],8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'D') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_3,48);
           reset(&buf_6);
           appendarray(&buf_6,const_3,48);
           reset(&buf_3);
           appendarray(&buf_3,const_4,96);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_4,96);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_2,8);
        reset(&buf_1);
        appendarray(&buf_1,const_2,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'P') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_6,40);
           reset(&buf_6);
           appendarray(&buf_6,const_6,40);
           reset(&buf_3);
           appendarray(&buf_3,const_7,32);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_7,32);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_5,8);
        reset(&buf_1);
        appendarray(&buf_1,const_5,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'W') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_10,48);
           reset(&buf_6);
           appendarray(&buf_6,const_10,48);
           reset(&buf_3);
           appendarray(&buf_3,const_8,48);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_8,48);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_9,8);
        reset(&buf_1);
        appendarray(&buf_1,const_9,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'a') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
        {
           output(&buf_2);
           consume(5);
           goto l26;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_11,8);
        reset(&buf_1);
        appendarray(&buf_1,const_11,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'b') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
        {
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_20,96);
              reset(&buf_6);
              appendarray(&buf_6,const_20,96);
              reset(&buf_3);
              appendarray(&buf_3,const_25,136);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_25,136);
              consume(12);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_21,48);
              reset(&buf_6);
              appendarray(&buf_6,const_21,48);
              reset(&buf_3);
              appendarray(&buf_3,const_171,112);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_171,112);
              consume(6);
              goto l6;
           }
        }
        if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_26,112);
           reset(&buf_6);
           appendarray(&buf_6,const_26,112);
           reset(&buf_3);
           appendarray(&buf_3,const_50,120);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_50,120);
           consume(14);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_29,24);
           reset(&buf_6);
           appendarray(&buf_6,const_29,24);
           reset(&buf_3);
           appendarray(&buf_3,const_119,24);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_119,24);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
        {
           output(&buf_2);
           consume(7);
           goto l24;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_19,8);
        reset(&buf_1);
        appendarray(&buf_1,const_19,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'c') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_33,64);
              reset(&buf_6);
              appendarray(&buf_6,const_33,64);
              reset(&buf_3);
              appendarray(&buf_3,const_175,80);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_175,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_34,112);
              reset(&buf_6);
              appendarray(&buf_6,const_34,112);
              reset(&buf_3);
              appendarray(&buf_3,const_214,72);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_214,72);
              consume(14);
              goto l6;
           }
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_36,40);
           reset(&buf_6);
           appendarray(&buf_6,const_36,40);
           reset(&buf_3);
           appendarray(&buf_3,const_178,24);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_178,24);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_37,64);
              reset(&buf_6);
              appendarray(&buf_6,const_37,64);
              reset(&buf_3);
              appendarray(&buf_3,const_140,88);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_140,88);
              consume(8);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_38,64);
              reset(&buf_6);
              appendarray(&buf_6,const_38,64);
              reset(&buf_3);
              appendarray(&buf_3,const_45,56);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_45,56);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_39,24);
           reset(&buf_6);
           appendarray(&buf_6,const_39,24);
           reset(&buf_3);
           appendarray(&buf_3,const_35,184);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_35,184);
           consume(3);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_32,8);
        reset(&buf_1);
        appendarray(&buf_1,const_32,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'd') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_41,48);
              reset(&buf_6);
              appendarray(&buf_6,const_41,48);
              reset(&buf_3);
              appendarray(&buf_3,const_4,96);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_4,96);
              consume(6);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_42,64);
              reset(&buf_6);
              appendarray(&buf_6,const_42,64);
              reset(&buf_3);
              appendarray(&buf_3,const_107,64);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_107,64);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_46,64);
              reset(&buf_6);
              appendarray(&buf_6,const_46,64);
              reset(&buf_3);
              appendarray(&buf_3,const_54,80);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_54,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_47,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_47,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_15,56);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_15,56);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_48,72);
                 reset(&buf_6);
                 appendarray(&buf_6,const_48,72);
                 reset(&buf_3);
                 appendarray(&buf_3,const_16,72);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_16,72);
                 consume(9);
                 goto l6;
              }
           }
        }
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_52,128);
           reset(&buf_6);
           appendarray(&buf_6,const_52,128);
           reset(&buf_3);
           appendarray(&buf_3,const_51,96);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_51,96);
           consume(16);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_53,40);
           reset(&buf_6);
           appendarray(&buf_6,const_53,40);
           reset(&buf_3);
           appendarray(&buf_3,const_130,72);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_130,72);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_40,8);
        reset(&buf_1);
        appendarray(&buf_1,const_40,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_57,48);
           reset(&buf_6);
           appendarray(&buf_6,const_57,48);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_196,184);
           consume(6);
           goto l6;
        }
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
        {
           if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_62,112);
              reset(&buf_6);
              appendarray(&buf_6,const_62,112);
              reset(&buf_3);
              appendarray(&buf_3,const_71,72);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_71,72);
              consume(14);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_61,48);
           reset(&buf_6);
           appendarray(&buf_6,const_61,48);
           reset(&buf_3);
           appendarray(&buf_3,const_72,40);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_72,40);
           consume(6);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'x') && 1)))
        {
           if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_63,72);
              reset(&buf_6);
              appendarray(&buf_6,const_63,72);
              reset(&buf_3);
              appendarray(&buf_3,const_209,80);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_209,80);
              consume(9);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_64,56);
              reset(&buf_6);
              appendarray(&buf_6,const_64,56);
              reset(&buf_3);
              appendarray(&buf_3,const_208,80);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_208,80);
              consume(7);
              goto l6;
           }
        }
        output(&buf_2);
        consume(1);
        goto l20;
     }
     if (((avail >= 1) && ((next[0] == 'f') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_66,32);
           reset(&buf_6);
           appendarray(&buf_6,const_66,32);
           reset(&buf_3);
           appendarray(&buf_3,const_68,48);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_68,48);
           consume(4);
           goto l6;
        }
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_69,64);
           reset(&buf_6);
           appendarray(&buf_6,const_69,64);
           reset(&buf_3);
           appendarray(&buf_3,const_27,64);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_27,64);
           consume(8);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_73,40);
           reset(&buf_6);
           appendarray(&buf_6,const_73,40);
           reset(&buf_3);
           appendarray(&buf_3,const_81,88);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_81,88);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_76,64);
              reset(&buf_6);
              appendarray(&buf_6,const_76,64);
              reset(&buf_3);
              appendarray(&buf_3,const_85,80);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_85,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_77,96);
              reset(&buf_6);
              appendarray(&buf_6,const_77,96);
              reset(&buf_3);
              appendarray(&buf_3,const_78,96);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_78,96);
              consume(12);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_65,8);
        reset(&buf_1);
        appendarray(&buf_1,const_65,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'g') && 1)))
     {
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_80,128);
           reset(&buf_6);
           appendarray(&buf_6,const_80,128);
           reset(&buf_3);
           appendarray(&buf_3,const_183,104);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_183,104);
           consume(16);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
        {
           output(&buf_2);
           consume(7);
           goto l25;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_79,8);
        reset(&buf_1);
        appendarray(&buf_1,const_79,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'h') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_93,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_93,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_67,112);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_67,112);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_94,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_94,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_124,64);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_124,64);
                 consume(8);
                 goto l6;
              }
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_95,96);
                 reset(&buf_6);
                 appendarray(&buf_6,const_95,96);
                 reset(&buf_3);
                 appendarray(&buf_3,const_88,128);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_88,128);
                 consume(12);
                 goto l6;
              }
              if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_96,56);
                 reset(&buf_6);
                 appendarray(&buf_6,const_96,56);
                 reset(&buf_3);
                 appendarray(&buf_3,const_89,104);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_89,104);
                 consume(7);
                 goto l6;
              }
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_90,8);
        reset(&buf_1);
        appendarray(&buf_1,const_90,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'i') && 1)))
     {
        if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_103,88);
           reset(&buf_6);
           appendarray(&buf_6,const_103,88);
           reset(&buf_3);
           appendarray(&buf_3,const_75,72);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_75,72);
           consume(11);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_101,8);
        reset(&buf_1);
        appendarray(&buf_1,const_101,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'k') && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_108,64);
           reset(&buf_6);
           appendarray(&buf_6,const_108,64);
           reset(&buf_3);
           appendarray(&buf_3,const_193,64);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_193,64);
           consume(8);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_105,8);
        reset(&buf_1);
        appendarray(&buf_1,const_105,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'l') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_115,48);
           reset(&buf_6);
           appendarray(&buf_6,const_115,48);
           reset(&buf_3);
           appendarray(&buf_3,const_123,96);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_123,96);
           consume(6);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_117,32);
           reset(&buf_6);
           appendarray(&buf_6,const_117,32);
           reset(&buf_3);
           appendarray(&buf_3,const_100,48);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_100,48);
           consume(4);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_118,72);
           reset(&buf_6);
           appendarray(&buf_6,const_118,72);
           reset(&buf_3);
           appendarray(&buf_3,const_110,72);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_110,72);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_114,8);
        reset(&buf_1);
        appendarray(&buf_1,const_114,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'm') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
           {
              if (((avail >= 5) && ((next[4] == 's') && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_128,40);
                 reset(&buf_6);
                 appendarray(&buf_6,const_128,40);
                 reset(&buf_3);
                 appendarray(&buf_3,const_127,40);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_127,40);
                 consume(5);
                 goto l6;
              }
              reset(&buf_7);
              appendarray(&buf_7,const_126,32);
              reset(&buf_6);
              appendarray(&buf_6,const_126,32);
              reset(&buf_3);
              appendarray(&buf_3,const_125,24);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_125,24);
              consume(4);
              goto l6;
           }
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_129,40);
              reset(&buf_6);
              appendarray(&buf_6,const_129,40);
              reset(&buf_3);
              appendarray(&buf_3,const_70,32);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_70,32);
              consume(5);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_121,8);
        reset(&buf_1);
        appendarray(&buf_1,const_121,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'n') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_132,72);
           reset(&buf_6);
           appendarray(&buf_6,const_132,72);
           reset(&buf_3);
           appendarray(&buf_3,const_133,64);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_133,64);
           consume(9);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_134,32);
           reset(&buf_6);
           appendarray(&buf_6,const_134,32);
           reset(&buf_3);
           appendarray(&buf_3,const_49,56);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_49,56);
           consume(4);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_135,24);
           reset(&buf_6);
           appendarray(&buf_6,const_135,24);
           reset(&buf_3);
           appendarray(&buf_3,const_102,32);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_102,32);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_136,56);
           reset(&buf_6);
           appendarray(&buf_6,const_136,56);
           reset(&buf_3);
           appendarray(&buf_3,const_192,56);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_192,56);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_131,8);
        reset(&buf_1);
        appendarray(&buf_1,const_131,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'o') && 1)))
     {
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_139,80);
           reset(&buf_6);
           appendarray(&buf_6,const_139,80);
           reset(&buf_3);
           appendarray(&buf_3,const_225,96);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_225,96);
           consume(10);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_137,8);
        reset(&buf_1);
        appendarray(&buf_1,const_137,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'p') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_142,88);
              reset(&buf_6);
              appendarray(&buf_6,const_142,88);
              reset(&buf_3);
              appendarray(&buf_3,const_138,40);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_138,40);
              consume(11);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_143,64);
              reset(&buf_6);
              appendarray(&buf_6,const_143,64);
              reset(&buf_3);
              appendarray(&buf_3,const_120,48);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_120,48);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_144,16);
           reset(&buf_6);
           appendarray(&buf_6,const_144,16);
           reset(&buf_3);
           appendarray(&buf_3,const_97,104);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_97,104);
           consume(2);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_145,40);
           reset(&buf_6);
           appendarray(&buf_6,const_145,40);
           reset(&buf_3);
           appendarray(&buf_3,const_189,64);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_189,64);
           consume(5);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_146,72);
           reset(&buf_6);
           appendarray(&buf_6,const_146,72);
           reset(&buf_3);
           appendarray(&buf_3,const_164,64);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_164,64);
           consume(9);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_148,56);
           reset(&buf_6);
           appendarray(&buf_6,const_148,56);
           reset(&buf_3);
           appendarray(&buf_3,const_116,96);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_116,96);
           consume(7);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'u') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
           {
              output(&buf_2);
              consume(4);
              goto l21;
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_153,32);
              reset(&buf_6);
              appendarray(&buf_6,const_153,32);
              reset(&buf_3);
              appendarray(&buf_3,const_176,32);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_176,32);
              consume(4);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_141,8);
        reset(&buf_1);
        appendarray(&buf_1,const_141,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'q') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_155,48);
           reset(&buf_6);
           appendarray(&buf_6,const_155,48);
           reset(&buf_3);
           appendarray(&buf_3,const_147,64);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_147,64);
           consume(6);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_156,40);
           reset(&buf_6);
           appendarray(&buf_6,const_156,40);
           reset(&buf_3);
           appendarray(&buf_3,const_113,32);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_113,32);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_154,8);
        reset(&buf_1);
        appendarray(&buf_1,const_154,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'r') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_158,104);
              reset(&buf_6);
              appendarray(&buf_6,const_158,104);
              reset(&buf_3);
              appendarray(&buf_3,const_106,104);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_106,104);
              consume(13);
              goto l6;
           }
           if (((avail >= 3) && ((next[2] == 'm') && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_159,24);
              reset(&buf_6);
              appendarray(&buf_6,const_159,24);
              reset(&buf_3);
              appendarray(&buf_3,const_18,96);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_18,96);
              consume(3);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_162,80);
              reset(&buf_6);
              appendarray(&buf_6,const_162,80);
              reset(&buf_3);
              appendarray(&buf_3,const_87,40);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_87,40);
              consume(10);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_163,56);
              reset(&buf_6);
              appendarray(&buf_6,const_163,56);
              reset(&buf_3);
              appendarray(&buf_3,const_17,72);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_17,72);
              consume(7);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_157,8);
        reset(&buf_1);
        appendarray(&buf_1,const_157,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_166,40);
              reset(&buf_6);
              appendarray(&buf_6,const_166,40);
              reset(&buf_3);
              appendarray(&buf_3,const_213,72);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_213,72);
              consume(5);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_167,80);
              reset(&buf_6);
              appendarray(&buf_6,const_167,80);
              reset(&buf_3);
              appendarray(&buf_3,const_179,104);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_179,104);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_168,32);
           reset(&buf_6);
           appendarray(&buf_6,const_168,32);
           reset(&buf_3);
           appendarray(&buf_3,const_23,88);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_23,88);
           consume(4);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
        {
           if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_170,88);
              reset(&buf_6);
              appendarray(&buf_6,const_170,88);
              reset(&buf_3);
              appendarray(&buf_3,const_174,88);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_174,88);
              consume(11);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_169,40);
           reset(&buf_6);
           appendarray(&buf_6,const_169,40);
           reset(&buf_3);
           appendarray(&buf_3,const_111,48);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_111,48);
           consume(5);
           goto l6;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_180,80);
           reset(&buf_6);
           appendarray(&buf_6,const_180,80);
           reset(&buf_3);
           appendarray(&buf_3,const_43,56);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_43,56);
           consume(10);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'o') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_181,64);
              reset(&buf_6);
              appendarray(&buf_6,const_181,64);
              reset(&buf_3);
              appendarray(&buf_3,const_149,80);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_149,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_182,80);
              reset(&buf_6);
              appendarray(&buf_6,const_182,80);
              reset(&buf_3);
              appendarray(&buf_3,const_109,80);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_109,80);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 't') && 1)))
        {
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_186,56);
              reset(&buf_6);
              appendarray(&buf_6,const_186,56);
              reset(&buf_3);
              appendarray(&buf_3,const_99,104);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_99,104);
              consume(7);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_187,56);
              reset(&buf_6);
              appendarray(&buf_6,const_187,56);
              reset(&buf_3);
              appendarray(&buf_3,const_44,72);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_44,72);
              consume(7);
              goto l6;
           }
        }
        if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_188,104);
           reset(&buf_6);
           appendarray(&buf_6,const_188,104);
           reset(&buf_3);
           appendarray(&buf_3,const_202,80);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_202,80);
           consume(13);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_165,8);
        reset(&buf_1);
        appendarray(&buf_1,const_165,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 't') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_191,48);
           reset(&buf_6);
           appendarray(&buf_6,const_191,48);
           reset(&buf_3);
           appendarray(&buf_3,const_194,96);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_194,96);
           consume(6);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
        {
           output(&buf_2);
           consume(3);
           goto l23;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_204,40);
           reset(&buf_6);
           appendarray(&buf_6,const_204,40);
           reset(&buf_3);
           appendarray(&buf_3,const_203,40);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_203,40);
           consume(5);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_205,40);
           reset(&buf_6);
           appendarray(&buf_6,const_205,40);
           reset(&buf_3);
           appendarray(&buf_3,const_112,40);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_112,40);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_190,8);
        reset(&buf_1);
        appendarray(&buf_1,const_190,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'u') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_210,72);
           reset(&buf_6);
           appendarray(&buf_6,const_210,72);
           reset(&buf_3);
           appendarray(&buf_3,const_86,64);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_86,64);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_206,8);
        reset(&buf_1);
        appendarray(&buf_1,const_206,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'v') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_212,56);
           reset(&buf_6);
           appendarray(&buf_6,const_212,56);
           reset(&buf_3);
           appendarray(&buf_3,const_207,48);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_207,48);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_211,8);
        reset(&buf_1);
        appendarray(&buf_1,const_211,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'w') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 3) && ((next[2] == 'b') && 1)))
           {
              if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_218,80);
                 reset(&buf_6);
                 appendarray(&buf_6,const_218,80);
                 reset(&buf_3);
                 appendarray(&buf_3,const_185,104);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_185,104);
                 consume(10);
                 goto l6;
              }
              output(&buf_2);
              consume(3);
              goto l7;
           }
           output(&buf_2);
           consume(2);
           goto l22;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_223,80);
           reset(&buf_6);
           appendarray(&buf_6,const_223,80);
           reset(&buf_3);
           appendarray(&buf_3,const_24,72);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_24,72);
           consume(10);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_224,40);
           reset(&buf_6);
           appendarray(&buf_6,const_224,40);
           reset(&buf_3);
           appendarray(&buf_3,const_74,56);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_74,56);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_215,8);
        reset(&buf_1);
        appendarray(&buf_1,const_215,8);
        output(&buf_2);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_7);
        append(&buf_7,tbl[0][next[0]],8);
        reset(&buf_5);
        reset(&buf_3);
        append(&buf_3,tbl[0][next[0]],8);
        reset(&buf_1);
        append(&buf_1,tbl[0][next[0]],8);
        output(&buf_2);
        consume(1);
        goto l5;
     }
     goto fail;
l20: if (!readnext(1, 5))
     {
        outputarray(const_55,8);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255)))))))))) && 1)))
     {
        outputarray(const_55,8);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && ((next[0] == '-') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "mail",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_122,32);
           reset(&buf_5);
           appendarray(&buf_5,const_56,16);
           reset(&buf_3);
           appendarray(&buf_3,const_122,32);
           reset(&buf_1);
           appendarray(&buf_1,const_58,88);
           consume(5);
           goto l2;
        }
        outputarray(const_56,16);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'l')) || ((('n' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_55,8);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_55,8);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'm') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_60,40);
           reset(&buf_6);
           appendarray(&buf_6,const_60,40);
           reset(&buf_3);
           appendarray(&buf_3,const_58,88);
           reset(&buf_2);
           appendarray(&buf_2,const_58,88);
           consume(4);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_59,16);
        reset(&buf_1);
        appendarray(&buf_1,const_59,16);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_55,8);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_55,8);
        consume(1);
        goto l27;
     }
     goto fail;
l21: if (!readnext(1, 8))
     {
        outputarray(const_91,24);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= 31)) || ((('!' <= next[0]) && (next[0] <= ',')) || ((('.' <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))))) && 1)))
     {
        outputarray(const_91,24);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((next[0] == ' ') || (next[0] == '-')) && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "request",7) && 1)))
        {
           reset(&buf_9);
           for(i = 0; i < 7; i++)
           {  append(&buf_9,tbl[0][next[1 + i]],8);
           }
           reset(&buf_5);
           appendarray(&buf_5,const_91,24);
           append(&buf_5,tbl[0][next[0]],8);
           reset(&buf_3);
           for(i = 0; i < 7; i++)
           {  append(&buf_3,tbl[0][next[1 + i]],8);
           }
           consume(8);
           goto l3;
        }
        outputarray(const_91,24);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'q')) || ((('s' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_150,32);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_150,32);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'r') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "equest",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_152,88);
           reset(&buf_6);
           appendarray(&buf_6,const_152,88);
           reset(&buf_3);
           appendarray(&buf_3,const_92,104);
           reset(&buf_2);
           appendarray(&buf_2,const_92,104);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_151,40);
        reset(&buf_1);
        appendarray(&buf_1,const_151,40);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        appendarray(&buf_6,const_150,32);
        append(&buf_6,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_150,32);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        appendarray(&buf_2,const_91,24);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l19;
     }
     goto fail;
l22: if (!readnext(1, 5))
     {
        outputarray(const_216,16);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        outputarray(const_216,16);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'a') || ((('c' <= next[0]) && (next[0] <= 'r')) || ((('t' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_216,16);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_216,16);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'b') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "site",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_220,56);
           reset(&buf_6);
           appendarray(&buf_6,const_220,56);
           reset(&buf_3);
           appendarray(&buf_3,const_98,80);
           reset(&buf_2);
           appendarray(&buf_2,const_98,80);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_217,24);
        reset(&buf_1);
        appendarray(&buf_1,const_217,24);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ite",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_222,48);
           reset(&buf_6);
           appendarray(&buf_6,const_222,48);
           reset(&buf_3);
           appendarray(&buf_3,const_98,80);
           reset(&buf_2);
           appendarray(&buf_2,const_98,80);
           consume(4);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_221,24);
        reset(&buf_1);
        appendarray(&buf_1,const_221,24);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_216,16);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_216,16);
        consume(1);
        goto l27;
     }
     goto fail;
l23: if (!readnext(1, 7))
     {
        outputarray(const_197,24);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        outputarray(const_197,24);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'd')) || ((('f' <= next[0]) && (next[0] <= 's')) || ((('u' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_197,24);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_197,24);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_199,72);
           reset(&buf_6);
           appendarray(&buf_6,const_199,72);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           reset(&buf_2);
           appendarray(&buf_2,const_196,184);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_198,32);
        reset(&buf_1);
        appendarray(&buf_1,const_198,32);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 't') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "editor",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_201,80);
           reset(&buf_6);
           appendarray(&buf_6,const_201,80);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           reset(&buf_2);
           appendarray(&buf_2,const_196,184);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_200,32);
        reset(&buf_1);
        appendarray(&buf_1,const_200,32);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_197,24);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_197,24);
        consume(1);
        goto l27;
     }
     goto fail;
l24: if (!readnext(1, 1))
     {
        outputarray(const_31,56);
        outputarray(const_0,0);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        outputarray(const_123,96);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'd')) || ((('f' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_160,32);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_160,32);
        append(&buf_1,tbl[0][next[0]],8);
        outputarray(const_30,24);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        reset(&buf_7);
        appendarray(&buf_7,const_161,40);
        reset(&buf_5);
        appendarray(&buf_5,const_30,24);
        reset(&buf_3);
        appendarray(&buf_3,const_161,40);
        reset(&buf_1);
        appendarray(&buf_1,const_123,96);
        consume(1);
        goto l2;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_7);
        appendarray(&buf_7,const_160,32);
        append(&buf_7,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_30,24);
        reset(&buf_3);
        appendarray(&buf_3,const_160,32);
        append(&buf_3,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_123,96);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l5;
     }
     goto fail;
l25: if (!readnext(1, 6))
     {
        outputarray(const_177,80);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        outputarray(const_177,80);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'r')) || ((('t' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_82,56);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_82,56);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "itory",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_84,104);
           reset(&buf_6);
           appendarray(&buf_6,const_84,104);
           reset(&buf_3);
           appendarray(&buf_3,const_177,80);
           reset(&buf_2);
           appendarray(&buf_2,const_177,80);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_83,64);
        reset(&buf_1);
        appendarray(&buf_1,const_83,64);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        appendarray(&buf_6,const_82,56);
        append(&buf_6,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_82,56);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        appendarray(&buf_2,const_177,80);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l19;
     }
     goto fail;
l26: if (!readnext(1, 8))
     {
        outputarray(const_22,64);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        outputarray(const_22,64);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'h')) || ((('j' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_12,40);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_1);
        appendarray(&buf_1,const_12,40);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'i') && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "strator",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_14,104);
           reset(&buf_6);
           appendarray(&buf_6,const_14,104);
           reset(&buf_3);
           appendarray(&buf_3,const_22,64);
           reset(&buf_2);
           appendarray(&buf_2,const_22,64);
           consume(8);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_13,48);
        reset(&buf_1);
        appendarray(&buf_1,const_13,48);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        appendarray(&buf_6,const_12,40);
        append(&buf_6,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_12,40);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        appendarray(&buf_2,const_22,64);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l19;
     }
     goto fail;
l27: if (!readnext(1, 16))
     {
        output(&buf_1);
        output(&buf_6);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_1);
        output(&buf_2);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
     {
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        append(&buf_5,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'D') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_3,48);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_4,96);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_3,48);
           consume(6);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_2,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_2,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'P') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_6,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_7,32);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_6,40);
           consume(5);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_5,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_5,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'W') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_10,48);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_8,48);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_10,48);
           consume(6);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_9,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_9,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'a') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
        {
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_12,40);
           consume(5);
           goto l10;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_11,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_11,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'b') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
        {
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_20,96);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_25,136);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_20,96);
              consume(12);
              goto l12;
           }
           if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_21,48);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_171,112);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_21,48);
              consume(6);
              goto l12;
           }
        }
        if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_26,112);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_50,120);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_26,112);
           consume(14);
           goto l12;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_29,24);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_119,24);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_29,24);
           consume(3);
           goto l12;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
        {
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_30,24);
           consume(7);
           goto l28;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_19,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_19,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'c') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_33,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_175,80);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_33,64);
              consume(8);
              goto l12;
           }
           if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_34,112);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_214,72);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_34,112);
              consume(14);
              goto l12;
           }
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_36,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_178,24);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_36,40);
           consume(5);
           goto l12;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_37,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_140,88);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_37,64);
              consume(8);
              goto l12;
           }
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_38,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_45,56);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_38,64);
              consume(8);
              goto l12;
           }
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_39,24);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_35,184);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_39,24);
           consume(3);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_32,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_32,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'd') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_41,48);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_4,96);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_41,48);
              consume(6);
              goto l12;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_42,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_107,64);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_42,64);
              consume(8);
              goto l12;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_46,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_54,80);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_46,64);
              consume(8);
              goto l12;
           }
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_47,64);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_15,56);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_47,64);
                 consume(8);
                 goto l12;
              }
              if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_48,72);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_16,72);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_48,72);
                 consume(9);
                 goto l12;
              }
           }
        }
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_52,128);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_51,96);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_52,128);
           consume(16);
           goto l12;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_53,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_130,72);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_53,40);
           consume(5);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_40,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_40,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_57,48);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_57,48);
           consume(6);
           goto l12;
        }
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
        {
           if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_62,112);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_71,72);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_62,112);
              consume(14);
              goto l12;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_61,48);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_72,40);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_61,48);
           consume(6);
           goto l12;
        }
        if (((avail >= 2) && ((next[1] == 'x') && 1)))
        {
           if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_63,72);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_209,80);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_63,72);
              consume(9);
              goto l12;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_64,56);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_208,80);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_64,56);
              consume(7);
              goto l12;
           }
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_55,8);
        consume(1);
        goto l13;
     }
     if (((avail >= 1) && ((next[0] == 'f') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_66,32);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_68,48);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_66,32);
           consume(4);
           goto l12;
        }
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_69,64);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_27,64);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_69,64);
           consume(8);
           goto l12;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_73,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_81,88);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_73,40);
           consume(5);
           goto l12;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_76,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_85,80);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_76,64);
              consume(8);
              goto l12;
           }
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_77,96);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_78,96);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_77,96);
              consume(12);
              goto l12;
           }
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_65,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_65,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'g') && 1)))
     {
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_80,128);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_183,104);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_80,128);
           consume(16);
           goto l12;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
        {
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_82,56);
           consume(7);
           goto l9;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_79,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_79,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'h') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_93,64);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_67,112);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_93,64);
                 consume(8);
                 goto l12;
              }
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_94,64);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_124,64);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_94,64);
                 consume(8);
                 goto l12;
              }
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_95,96);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_88,128);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_95,96);
                 consume(12);
                 goto l12;
              }
              if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_96,56);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_89,104);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_96,56);
                 consume(7);
                 goto l12;
              }
           }
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_90,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_90,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'i') && 1)))
     {
        if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_103,88);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_75,72);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_103,88);
           consume(11);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_101,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_101,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'k') && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_108,64);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_193,64);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_108,64);
           consume(8);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_105,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_105,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'l') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_115,48);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_123,96);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_115,48);
           consume(6);
           goto l12;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_117,32);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_100,48);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_117,32);
           consume(4);
           goto l12;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_118,72);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_110,72);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_118,72);
           consume(9);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_114,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_114,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'm') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
           {
              if (((avail >= 5) && ((next[4] == 's') && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_128,40);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_127,40);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_128,40);
                 consume(5);
                 goto l12;
              }
              reset(&buf_7);
              appendarray(&buf_7,const_126,32);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_125,24);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_126,32);
              consume(4);
              goto l12;
           }
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_129,40);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_70,32);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_129,40);
              consume(5);
              goto l12;
           }
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_121,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_121,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'n') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_132,72);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_133,64);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_132,72);
           consume(9);
           goto l12;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_134,32);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_49,56);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_134,32);
           consume(4);
           goto l12;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_135,24);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_102,32);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_135,24);
           consume(3);
           goto l12;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_136,56);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_192,56);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_136,56);
           consume(7);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_131,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_131,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'o') && 1)))
     {
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_139,80);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_225,96);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_139,80);
           consume(10);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_137,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_137,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'p') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_142,88);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_138,40);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_142,88);
              consume(11);
              goto l12;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_143,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_120,48);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_143,64);
              consume(8);
              goto l12;
           }
        }
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_144,16);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_97,104);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_144,16);
           consume(2);
           goto l12;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_145,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_189,64);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_145,40);
           consume(5);
           goto l12;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_146,72);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_164,64);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_146,72);
           consume(9);
           goto l12;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_148,56);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_116,96);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_148,56);
           consume(7);
           goto l12;
        }
        if (((avail >= 2) && ((next[1] == 'u') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
           {
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_150,32);
              consume(4);
              goto l14;
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_153,32);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_176,32);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_153,32);
              consume(4);
              goto l12;
           }
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_141,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_141,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'q') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_155,48);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_147,64);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_155,48);
           consume(6);
           goto l12;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_156,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_113,32);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_156,40);
           consume(5);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_154,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_154,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'r') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_158,104);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_106,104);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_158,104);
              consume(13);
              goto l12;
           }
           if (((avail >= 3) && ((next[2] == 'm') && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_159,24);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_18,96);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_159,24);
              consume(3);
              goto l12;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_162,80);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_87,40);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_162,80);
              consume(10);
              goto l12;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_163,56);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_17,72);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_163,56);
              consume(7);
              goto l12;
           }
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_157,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_157,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_166,40);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_213,72);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_166,40);
              consume(5);
              goto l12;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_167,80);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_179,104);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_167,80);
              consume(10);
              goto l12;
           }
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_168,32);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_23,88);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_168,32);
           consume(4);
           goto l12;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
        {
           if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_170,88);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_174,88);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_170,88);
              consume(11);
              goto l12;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_169,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_111,48);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_169,40);
           consume(5);
           goto l12;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_180,80);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_43,56);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_180,80);
           consume(10);
           goto l12;
        }
        if (((avail >= 2) && ((next[1] == 'o') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_181,64);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_149,80);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_181,64);
              consume(8);
              goto l12;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_182,80);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_109,80);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_182,80);
              consume(10);
              goto l12;
           }
        }
        if (((avail >= 2) && ((next[1] == 't') && 1)))
        {
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_186,56);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_99,104);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_186,56);
              consume(7);
              goto l12;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_187,56);
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              reset(&buf_3);
              appendarray(&buf_3,const_44,72);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_187,56);
              consume(7);
              goto l12;
           }
        }
        if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_188,104);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_202,80);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_188,104);
           consume(13);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_165,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_165,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 't') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_191,48);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_194,96);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_191,48);
           consume(6);
           goto l12;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
        {
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_197,24);
           consume(3);
           goto l16;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_204,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_203,40);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_204,40);
           consume(5);
           goto l12;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_205,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_112,40);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_205,40);
           consume(5);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_190,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_190,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'u') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_210,72);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_86,64);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_210,72);
           consume(9);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_206,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_206,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'v') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_212,56);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_207,48);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_212,56);
           consume(7);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_211,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_211,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'w') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 3) && ((next[2] == 'b') && 1)))
           {
              if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_218,80);
                 reset(&buf_5);
                 concat(&buf_5,&buf_6);
                 reset(&buf_3);
                 appendarray(&buf_3,const_185,104);
                 output(&buf_1);
                 reset(&buf_1);
                 concat(&buf_1,&buf_2);
                 appendarray(&buf_1,const_218,80);
                 consume(10);
                 goto l12;
              }
              reset(&buf_5);
              concat(&buf_5,&buf_6);
              output(&buf_1);
              reset(&buf_1);
              concat(&buf_1,&buf_2);
              appendarray(&buf_1,const_217,24);
              consume(3);
              goto l8;
           }
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_216,16);
           consume(2);
           goto l15;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_223,80);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_24,72);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_223,80);
           consume(10);
           goto l12;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_224,40);
           reset(&buf_5);
           concat(&buf_5,&buf_6);
           reset(&buf_3);
           appendarray(&buf_3,const_74,56);
           output(&buf_1);
           reset(&buf_1);
           concat(&buf_1,&buf_2);
           appendarray(&buf_1,const_224,40);
           consume(5);
           goto l12;
        }
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        appendarray(&buf_5,const_215,8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        appendarray(&buf_1,const_215,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_5);
        concat(&buf_5,&buf_6);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l27;
     }
     goto fail;
l28: if (!readnext(1, 1))
     {
        output(&buf_1);
        outputarray(const_160,32);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        output(&buf_1);
        outputarray(const_160,32);
        outputconst(tbl[0][next[0]],8);
        consume(1);
        goto l1;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'Z')) || ((('a' <= next[0]) && (next[0] <= 'd')) || ((('f' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248)))))) && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_160,32);
        append(&buf_5,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_160,32);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        reset(&buf_5);
        appendarray(&buf_5,const_161,40);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_161,40);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_6);
        append(&buf_6,tbl[0][next[0]],8);
        reset(&buf_5);
        appendarray(&buf_5,const_160,32);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        appendarray(&buf_1,const_160,32);
        consume(1);
        goto l27;
     }
     goto fail;
l29: if (!readnext(1, 16))
     {
        output(&buf_1);
        output(&buf_2);
        goto accept;
     }
     if (((avail >= 1) && ((((0 <= next[0]) && (next[0] <= '/')) || (((':' <= next[0]) && (next[0] <= '@')) || ((('[' <= next[0]) && (next[0] <= '`')) || ((('{' <= next[0]) && (next[0] <= 196)) || (((199 <= next[0]) && (next[0] <= 215)) || (((217 <= next[0]) && (next[0] <= 228)) || (((231 <= next[0]) && (next[0] <= 247)) || ((249 <= next[0]) && (next[0] <= 255))))))))) && 1)))
     {
        reset(&buf_5);
        concat(&buf_5,&buf_4);
        append(&buf_5,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l17;
     }
     if (((avail >= 1) && (((('0' <= next[0]) && (next[0] <= '9')) || ((('A' <= next[0]) && (next[0] <= 'C')) || ((('E' <= next[0]) && (next[0] <= 'O')) || ((('Q' <= next[0]) && (next[0] <= 'V')) || ((('X' <= next[0]) && (next[0] <= 'Z')) || ((next[0] == 'j') || ((('x' <= next[0]) && (next[0] <= 'z')) || (((229 <= next[0]) && (next[0] <= 230)) || (next[0] == 248))))))))) && 1)))
     {
        reset(&buf_5);
        append(&buf_5,tbl[0][next[0]],8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        append(&buf_1,tbl[0][next[0]],8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'D') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "amnit",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_3,48);
           reset(&buf_6);
           appendarray(&buf_6,const_3,48);
           reset(&buf_3);
           appendarray(&buf_3,const_4,96);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_4,96);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_2,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_2,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'P') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "awel",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_6,40);
           reset(&buf_6);
           appendarray(&buf_6,const_6,40);
           reset(&buf_3);
           appendarray(&buf_3,const_7,32);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_7,32);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_5,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_5,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'W') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "inter",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_10,48);
           reset(&buf_6);
           appendarray(&buf_6,const_10,48);
           reset(&buf_3);
           appendarray(&buf_3,const_8,48);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_8,48);
           consume(6);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_9,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_9,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'a') && 1)))
     {
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "dmin",4) && 1)))
        {
           output(&buf_1);
           output(&buf_2);
           consume(5);
           goto l26;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_11,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_11,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'b') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ack",3) && 1)))
        {
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "tracking",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_20,96);
              reset(&buf_6);
              appendarray(&buf_6,const_20,96);
              reset(&buf_3);
              appendarray(&buf_3,const_25,136);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_25,136);
              consume(12);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[4],(unsigned char *) "up",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_21,48);
              reset(&buf_6);
              appendarray(&buf_6,const_21,48);
              reset(&buf_3);
              appendarray(&buf_3,const_171,112);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_171,112);
              consume(6);
              goto l6;
           }
        }
        if (((avail >= 14) && (cmp(&next[1],(unsigned char *) "ranchandbound",13) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_26,112);
           reset(&buf_6);
           appendarray(&buf_6,const_26,112);
           reset(&buf_3);
           appendarray(&buf_3,const_50,120);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_50,120);
           consume(14);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ug",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_29,24);
           reset(&buf_6);
           appendarray(&buf_6,const_29,24);
           reset(&buf_3);
           appendarray(&buf_3,const_119,24);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_119,24);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "\xc3\xa6rbar",6) && 1)))
        {
           output(&buf_1);
           output(&buf_2);
           consume(7);
           goto l24;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_19,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_19,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'c') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "pslock",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_33,64);
              reset(&buf_6);
              appendarray(&buf_6,const_33,64);
              reset(&buf_3);
              appendarray(&buf_3,const_175,80);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_175,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 14) && (cmp(&next[2],(unsigned char *) "rriagereturn",12) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_34,112);
              reset(&buf_6);
              appendarray(&buf_6,const_34,112);
              reset(&buf_3);
              appendarray(&buf_3,const_214,72);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_214,72);
              consume(14);
              goto l6;
           }
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "loud",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_36,40);
           reset(&buf_6);
           appendarray(&buf_6,const_36,40);
           reset(&buf_3);
           appendarray(&buf_3,const_178,24);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_178,24);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "omp",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "iler",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_37,64);
              reset(&buf_6);
              appendarray(&buf_6,const_37,64);
              reset(&buf_3);
              appendarray(&buf_3,const_140,88);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_140,88);
              consume(8);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "uter",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_38,64);
              reset(&buf_6);
              appendarray(&buf_6,const_38,64);
              reset(&buf_3);
              appendarray(&buf_3,const_45,56);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_45,56);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "pu",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_39,24);
           reset(&buf_6);
           appendarray(&buf_6,const_39,24);
           reset(&buf_3);
           appendarray(&buf_3,const_35,184);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_35,184);
           consume(3);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_32,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_32,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'd') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "mnit",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_41,48);
              reset(&buf_6);
              appendarray(&buf_6,const_41,48);
              reset(&buf_3);
              appendarray(&buf_3,const_4,96);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_4,96);
              consume(6);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "tabase",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_42,64);
              reset(&buf_6);
              appendarray(&buf_6,const_42,64);
              reset(&buf_3);
              appendarray(&buf_3,const_107,64);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_107,64);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "adline",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_46,64);
              reset(&buf_6);
              appendarray(&buf_6,const_46,64);
              reset(&buf_3);
              appendarray(&buf_3,const_54,80);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_54,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 6) && (cmp(&next[2],(unsigned char *) "bugg",4) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[6],(unsigned char *) "er",2) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_47,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_47,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_15,56);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_15,56);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 9) && (cmp(&next[6],(unsigned char *) "ing",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_48,72);
                 reset(&buf_6);
                 appendarray(&buf_6,const_48,72);
                 reset(&buf_3);
                 appendarray(&buf_3,const_16,72);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_16,72);
                 consume(9);
                 goto l6;
              }
           }
        }
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "ivideandconquer",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_52,128);
           reset(&buf_6);
           appendarray(&buf_6,const_52,128);
           reset(&buf_3);
           appendarray(&buf_3,const_51,96);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_51,96);
           consume(16);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "onut",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_53,40);
           reset(&buf_6);
           appendarray(&buf_6,const_53,40);
           reset(&buf_3);
           appendarray(&buf_3,const_130,72);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_130,72);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_40,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_40,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'e') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ditor",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_57,48);
           reset(&buf_6);
           appendarray(&buf_6,const_57,48);
           reset(&buf_3);
           appendarray(&buf_3,const_196,184);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_196,184);
           consume(6);
           goto l6;
        }
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "scape",5) && 1)))
        {
           if (((avail >= 14) && (cmp(&next[6],(unsigned char *) "sequence",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_62,112);
              reset(&buf_6);
              appendarray(&buf_6,const_62,112);
              reset(&buf_3);
              appendarray(&buf_3,const_71,72);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_71,72);
              consume(14);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_61,48);
           reset(&buf_6);
           appendarray(&buf_6,const_61,48);
           reset(&buf_3);
           appendarray(&buf_3,const_72,40);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_72,40);
           consume(6);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'x') && 1)))
        {
           if (((avail >= 9) && (cmp(&next[2],(unsigned char *) "ception",7) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_63,72);
              reset(&buf_6);
              appendarray(&buf_6,const_63,72);
              reset(&buf_3);
              appendarray(&buf_3,const_209,80);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_209,80);
              consume(9);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "ploit",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_64,56);
              reset(&buf_6);
              appendarray(&buf_6,const_64,56);
              reset(&buf_3);
              appendarray(&buf_3,const_208,80);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_208,80);
              consume(7);
              goto l6;
           }
        }
        output(&buf_1);
        output(&buf_2);
        consume(1);
        goto l20;
     }
     if (((avail >= 1) && ((next[0] == 'f') && 1)))
     {
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ail",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_66,32);
           reset(&buf_6);
           appendarray(&buf_6,const_66,32);
           reset(&buf_3);
           appendarray(&buf_3,const_68,48);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_68,48);
           consume(4);
           goto l6;
        }
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "irewall",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_69,64);
           reset(&buf_6);
           appendarray(&buf_6,const_69,64);
           reset(&buf_3);
           appendarray(&buf_3,const_27,64);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_27,64);
           consume(8);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "orce",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_73,40);
           reset(&buf_6);
           appendarray(&buf_6,const_73,40);
           reset(&buf_3);
           appendarray(&buf_3,const_81,88);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_81,88);
           consume(5);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ree",3) && 1)))
        {
           if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "food",4) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_76,64);
              reset(&buf_6);
              appendarray(&buf_6,const_76,64);
              reset(&buf_3);
              appendarray(&buf_3,const_85,80);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_85,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "software",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_77,96);
              reset(&buf_6);
              appendarray(&buf_6,const_77,96);
              reset(&buf_3);
              appendarray(&buf_3,const_78,96);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_78,96);
              consume(12);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_65,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_65,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'g') && 1)))
     {
        if (((avail >= 16) && (cmp(&next[1],(unsigned char *) "arbagecollector",15) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_80,128);
           reset(&buf_6);
           appendarray(&buf_6,const_80,128);
           reset(&buf_3);
           appendarray(&buf_3,const_183,104);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_183,104);
           consume(16);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "itrepo",6) && 1)))
        {
           output(&buf_1);
           output(&buf_2);
           consume(7);
           goto l25;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_79,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_79,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'h') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "rd",2) && 1)))
           {
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "disk",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_93,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_93,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_67,112);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_67,112);
                 consume(8);
                 goto l6;
              }
              if (((avail >= 8) && (cmp(&next[4],(unsigned char *) "ware",4) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_94,64);
                 reset(&buf_6);
                 appendarray(&buf_6,const_94,64);
                 reset(&buf_3);
                 appendarray(&buf_3,const_124,64);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_124,64);
                 consume(8);
                 goto l6;
              }
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              if (((avail >= 12) && (cmp(&next[4],(unsigned char *) "function",8) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_95,96);
                 reset(&buf_6);
                 appendarray(&buf_6,const_95,96);
                 reset(&buf_3);
                 appendarray(&buf_3,const_88,128);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_88,128);
                 consume(12);
                 goto l6;
              }
              if (((avail >= 7) && (cmp(&next[4],(unsigned char *) "tag",3) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_96,56);
                 reset(&buf_6);
                 appendarray(&buf_6,const_96,56);
                 reset(&buf_3);
                 appendarray(&buf_3,const_89,104);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_89,104);
                 consume(7);
                 goto l6;
              }
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_90,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_90,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'i') && 1)))
     {
        if (((avail >= 11) && (cmp(&next[1],(unsigned char *) "nterpreter",10) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_103,88);
           reset(&buf_6);
           appendarray(&buf_6,const_103,88);
           reset(&buf_3);
           appendarray(&buf_3,const_75,72);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_75,72);
           consume(11);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_101,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_101,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'k') && 1)))
     {
        if (((avail >= 8) && (cmp(&next[1],(unsigned char *) "eyboard",7) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_108,64);
           reset(&buf_6);
           appendarray(&buf_6,const_108,64);
           reset(&buf_3);
           appendarray(&buf_3,const_193,64);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_193,64);
           consume(8);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_105,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_105,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'l') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "aptop",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_115,48);
           reset(&buf_6);
           appendarray(&buf_6,const_115,48);
           reset(&buf_3);
           appendarray(&buf_3,const_123,96);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_123,96);
           consume(6);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ink",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_117,32);
           reset(&buf_6);
           appendarray(&buf_6,const_117,32);
           reset(&buf_3);
           appendarray(&buf_3,const_100,48);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_100,48);
           consume(4);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "owercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_118,72);
           reset(&buf_6);
           appendarray(&buf_6,const_118,72);
           reset(&buf_3);
           appendarray(&buf_3,const_110,72);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_110,72);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_114,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_114,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'm') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "me",2) && 1)))
           {
              if (((avail >= 5) && ((next[4] == 's') && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_128,40);
                 reset(&buf_6);
                 appendarray(&buf_6,const_128,40);
                 reset(&buf_3);
                 appendarray(&buf_3,const_127,40);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_127,40);
                 consume(5);
                 goto l6;
              }
              reset(&buf_7);
              appendarray(&buf_7,const_126,32);
              reset(&buf_6);
              appendarray(&buf_6,const_126,32);
              reset(&buf_3);
              appendarray(&buf_3,const_125,24);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_125,24);
              consume(4);
              goto l6;
           }
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "rge",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_129,40);
              reset(&buf_6);
              appendarray(&buf_6,const_129,40);
              reset(&buf_3);
              appendarray(&buf_3,const_70,32);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_70,32);
              consume(5);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_121,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_121,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'n') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "amespace",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_132,72);
           reset(&buf_6);
           appendarray(&buf_6,const_132,72);
           reset(&buf_3);
           appendarray(&buf_3,const_133,64);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_133,64);
           consume(9);
           goto l6;
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "ice",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_134,32);
           reset(&buf_6);
           appendarray(&buf_6,const_134,32);
           reset(&buf_3);
           appendarray(&buf_3,const_49,56);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_49,56);
           consume(4);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ot",2) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_135,24);
           reset(&buf_6);
           appendarray(&buf_6,const_135,24);
           reset(&buf_3);
           appendarray(&buf_3,const_102,32);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_102,32);
           consume(3);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "umlock",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_136,56);
           reset(&buf_6);
           appendarray(&buf_6,const_136,56);
           reset(&buf_3);
           appendarray(&buf_3,const_192,56);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_192,56);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_131,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_131,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'o') && 1)))
     {
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "pensource",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_139,80);
           reset(&buf_6);
           appendarray(&buf_6,const_139,80);
           reset(&buf_3);
           appendarray(&buf_3,const_225,96);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_225,96);
           consume(10);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_137,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_137,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'p') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 11) && (cmp(&next[2],(unsigned char *) "rtitioner",9) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_142,88);
              reset(&buf_6);
              appendarray(&buf_6,const_142,88);
              reset(&buf_3);
              appendarray(&buf_3,const_138,40);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_138,40);
              consume(11);
              goto l6;
           }
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ssword",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_143,64);
              reset(&buf_6);
              appendarray(&buf_6,const_143,64);
              reset(&buf_3);
              appendarray(&buf_3,const_120,48);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_120,48);
              consume(8);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_144,16);
           reset(&buf_6);
           appendarray(&buf_6,const_144,16);
           reset(&buf_3);
           appendarray(&buf_3,const_97,104);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_97,104);
           consume(2);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "ivot",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_145,40);
           reset(&buf_6);
           appendarray(&buf_6,const_145,40);
           reset(&buf_3);
           appendarray(&buf_3,const_189,64);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_189,64);
           consume(5);
           goto l6;
        }
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "laintext",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_146,72);
           reset(&buf_6);
           appendarray(&buf_6,const_146,72);
           reset(&buf_3);
           appendarray(&buf_3,const_164,64);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_164,64);
           consume(9);
           goto l6;
        }
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "rinter",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_148,56);
           reset(&buf_6);
           appendarray(&buf_6,const_148,56);
           reset(&buf_3);
           appendarray(&buf_3,const_116,96);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_116,96);
           consume(7);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'u') && 1)))
        {
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "ll",2) && 1)))
           {
              output(&buf_1);
              output(&buf_2);
              consume(4);
              goto l21;
           }
           if (((avail >= 4) && (cmp(&next[2],(unsigned char *) "sh",2) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_153,32);
              reset(&buf_6);
              appendarray(&buf_6,const_153,32);
              reset(&buf_3);
              appendarray(&buf_3,const_176,32);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_176,32);
              consume(4);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_141,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_141,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'q') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "rcode",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_155,48);
           reset(&buf_6);
           appendarray(&buf_6,const_155,48);
           reset(&buf_3);
           appendarray(&buf_3,const_147,64);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_147,64);
           consume(6);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uick",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_156,40);
           reset(&buf_6);
           appendarray(&buf_6,const_156,40);
           reset(&buf_3);
           appendarray(&buf_3,const_113,32);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_113,32);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_154,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_154,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'r') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'a') && 1)))
        {
           if (((avail >= 13) && (cmp(&next[2],(unsigned char *) "cecondition",11) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_158,104);
              reset(&buf_6);
              appendarray(&buf_6,const_158,104);
              reset(&buf_3);
              appendarray(&buf_3,const_106,104);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_106,104);
              consume(13);
              goto l6;
           }
           if (((avail >= 3) && ((next[2] == 'm') && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_159,24);
              reset(&buf_6);
              appendarray(&buf_6,const_159,24);
              reset(&buf_3);
              appendarray(&buf_3,const_18,96);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_18,96);
              consume(3);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "pository",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_162,80);
              reset(&buf_6);
              appendarray(&buf_6,const_162,80);
              reset(&buf_3);
              appendarray(&buf_3,const_87,40);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_87,40);
              consume(10);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "quest",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_163,56);
              reset(&buf_6);
              appendarray(&buf_6,const_163,56);
              reset(&buf_3);
              appendarray(&buf_3,const_17,72);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_17,72);
              consume(7);
              goto l6;
           }
        }
        reset(&buf_5);
        appendarray(&buf_5,const_157,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_157,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 's') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'c') && 1)))
        {
           if (((avail >= 5) && (cmp(&next[2],(unsigned char *) "ope",3) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_166,40);
              reset(&buf_6);
              appendarray(&buf_6,const_166,40);
              reset(&buf_3);
              appendarray(&buf_3,const_213,72);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_213,72);
              consume(5);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "reenshot",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_167,80);
              reset(&buf_6);
              appendarray(&buf_6,const_167,80);
              reset(&buf_3);
              appendarray(&buf_3,const_179,104);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_179,104);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 4) && (cmp(&next[1],(unsigned char *) "eed",3) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_168,32);
           reset(&buf_6);
           appendarray(&buf_6,const_168,32);
           reset(&buf_3);
           appendarray(&buf_3,const_23,88);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_23,88);
           consume(4);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "hell",4) && 1)))
        {
           if (((avail >= 11) && (cmp(&next[5],(unsigned char *) "script",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_170,88);
              reset(&buf_6);
              appendarray(&buf_6,const_170,88);
              reset(&buf_3);
              appendarray(&buf_3,const_174,88);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_174,88);
              consume(11);
              goto l6;
           }
           reset(&buf_7);
           appendarray(&buf_7,const_169,40);
           reset(&buf_6);
           appendarray(&buf_6,const_169,40);
           reset(&buf_3);
           appendarray(&buf_3,const_111,48);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_111,48);
           consume(5);
           goto l6;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "martphone",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_180,80);
           reset(&buf_6);
           appendarray(&buf_6,const_180,80);
           reset(&buf_3);
           appendarray(&buf_3,const_43,56);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_43,56);
           consume(10);
           goto l6;
        }
        if (((avail >= 2) && ((next[1] == 'o') && 1)))
        {
           if (((avail >= 8) && (cmp(&next[2],(unsigned char *) "ftware",6) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_181,64);
              reset(&buf_6);
              appendarray(&buf_6,const_181,64);
              reset(&buf_3);
              appendarray(&buf_3,const_149,80);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_149,80);
              consume(8);
              goto l6;
           }
           if (((avail >= 10) && (cmp(&next[2],(unsigned char *) "urcecode",8) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_182,80);
              reset(&buf_6);
              appendarray(&buf_6,const_182,80);
              reset(&buf_3);
              appendarray(&buf_3,const_109,80);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_109,80);
              consume(10);
              goto l6;
           }
        }
        if (((avail >= 2) && ((next[1] == 't') && 1)))
        {
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "andby",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_186,56);
              reset(&buf_6);
              appendarray(&buf_6,const_186,56);
              reset(&buf_3);
              appendarray(&buf_3,const_99,104);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_99,104);
              consume(7);
              goto l6;
           }
           if (((avail >= 7) && (cmp(&next[2],(unsigned char *) "orage",5) && 1)))
           {
              reset(&buf_7);
              appendarray(&buf_7,const_187,56);
              reset(&buf_6);
              appendarray(&buf_6,const_187,56);
              reset(&buf_3);
              appendarray(&buf_3,const_44,72);
              output(&buf_1);
              output(&buf_2);
              reset(&buf_2);
              appendarray(&buf_2,const_44,72);
              consume(7);
              goto l6;
           }
        }
        if (((avail >= 13) && (cmp(&next[1],(unsigned char *) "upercomputer",12) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_188,104);
           reset(&buf_6);
           appendarray(&buf_6,const_188,104);
           reset(&buf_3);
           appendarray(&buf_3,const_202,80);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_202,80);
           consume(13);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_165,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_165,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 't') && 1)))
     {
        if (((avail >= 6) && (cmp(&next[1],(unsigned char *) "ablet",5) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_191,48);
           reset(&buf_6);
           appendarray(&buf_6,const_191,48);
           reset(&buf_3);
           appendarray(&buf_3,const_194,96);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_194,96);
           consume(6);
           goto l6;
        }
        if (((avail >= 3) && (cmp(&next[1],(unsigned char *) "ex",2) && 1)))
        {
           output(&buf_1);
           output(&buf_2);
           consume(3);
           goto l23;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "uple",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_204,40);
           reset(&buf_6);
           appendarray(&buf_6,const_204,40);
           reset(&buf_3);
           appendarray(&buf_3,const_203,40);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_203,40);
           consume(5);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "weet",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_205,40);
           reset(&buf_6);
           appendarray(&buf_6,const_205,40);
           reset(&buf_3);
           appendarray(&buf_3,const_112,40);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_112,40);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_190,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_190,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'u') && 1)))
     {
        if (((avail >= 9) && (cmp(&next[1],(unsigned char *) "ppercase",8) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_210,72);
           reset(&buf_6);
           appendarray(&buf_6,const_210,72);
           reset(&buf_3);
           appendarray(&buf_3,const_86,64);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_86,64);
           consume(9);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_206,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_206,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'v') && 1)))
     {
        if (((avail >= 7) && (cmp(&next[1],(unsigned char *) "ersion",6) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_212,56);
           reset(&buf_6);
           appendarray(&buf_6,const_212,56);
           reset(&buf_3);
           appendarray(&buf_3,const_207,48);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_207,48);
           consume(7);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_211,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_211,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((next[0] == 'w') && 1)))
     {
        if (((avail >= 2) && ((next[1] == 'e') && 1)))
        {
           if (((avail >= 3) && ((next[2] == 'b') && 1)))
           {
              if (((avail >= 10) && (cmp(&next[3],(unsigned char *) "browser",7) && 1)))
              {
                 reset(&buf_7);
                 appendarray(&buf_7,const_218,80);
                 reset(&buf_6);
                 appendarray(&buf_6,const_218,80);
                 reset(&buf_3);
                 appendarray(&buf_3,const_185,104);
                 output(&buf_1);
                 output(&buf_2);
                 reset(&buf_2);
                 appendarray(&buf_2,const_185,104);
                 consume(10);
                 goto l6;
              }
              output(&buf_1);
              output(&buf_2);
              consume(3);
              goto l7;
           }
           output(&buf_1);
           output(&buf_2);
           consume(2);
           goto l22;
        }
        if (((avail >= 10) && (cmp(&next[1],(unsigned char *) "hitespace",9) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_223,80);
           reset(&buf_6);
           appendarray(&buf_6,const_223,80);
           reset(&buf_3);
           appendarray(&buf_3,const_24,72);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_24,72);
           consume(10);
           goto l6;
        }
        if (((avail >= 5) && (cmp(&next[1],(unsigned char *) "rong",4) && 1)))
        {
           reset(&buf_7);
           appendarray(&buf_7,const_224,40);
           reset(&buf_6);
           appendarray(&buf_6,const_224,40);
           reset(&buf_3);
           appendarray(&buf_3,const_74,56);
           output(&buf_1);
           output(&buf_2);
           reset(&buf_2);
           appendarray(&buf_2,const_74,56);
           consume(5);
           goto l6;
        }
        reset(&buf_5);
        appendarray(&buf_5,const_215,8);
        output(&buf_1);
        output(&buf_2);
        reset(&buf_1);
        appendarray(&buf_1,const_215,8);
        consume(1);
        goto l11;
     }
     if (((avail >= 1) && ((((197 <= next[0]) && (next[0] <= 198)) || (next[0] == 216)) && 1)))
     {
        reset(&buf_8);
        append(&buf_8,tbl[0][next[0]],8);
        reset(&buf_5);
        concat(&buf_5,&buf_4);
        append(&buf_5,tbl[0][next[0]],8);
        reset(&buf_4);
        append(&buf_4,tbl[0][next[0]],8);
        output(&buf_1);
        reset(&buf_1);
        concat(&buf_1,&buf_2);
        reset(&buf_2);
        append(&buf_2,tbl[0][next[0]],8);
        consume(1);
        goto l29;
     }
     goto fail;
  accept:
    return;
  fail:
    fprintf(stderr, "Match error at input symbol %zu!\n", count);
    exit(1);
}
