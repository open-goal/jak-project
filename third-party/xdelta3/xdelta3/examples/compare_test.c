/* xdelta3 - delta compression tools and library
   Copyright 2016 Joshua MacDonald

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <assert.h>

#include "xdelta3.h"

#define NUM (1<<20)
#define ITERS 100

/* From wikipedia on RDTSC */
inline uint64_t rdtsc() {
  uint32_t lo, hi;
  asm volatile ("rdtsc" : "=a" (lo), "=d" (hi));
  return (uint64_t)hi << 32 | lo;
}

typedef int (*test_func)(const char *s1, const char *s2, int n);

void run_test(const char *buf1, const char *buf2,
	      const char *name, test_func func) {
  uint64_t start, end;
  uint64_t accum = 0;
  int i, x;

  for (i = 0; i < ITERS; i++) {
    start = rdtsc();
    x = func(buf1, buf2, NUM);
    end = rdtsc();
    accum += end - start;
    assert(x == NUM - 1);
  }

  accum /= ITERS;

  printf("%s : %qu cycles\n", name, accum);
}

/* Build w/ -fno-builtin for this to be fast, this assumes that there
 * is a difference at s1[n-1] */
int memcmp_fake(const char *s1, const char *s2, int n) {
  int x = memcmp(s1, s2, n);
  return x < 0 ? n - 1 : n + 1;
}

#define UNALIGNED_OK 1
static inline int
test2(const char *s1c, const char *s2c, int n)
{
  int i = 0;
#if UNALIGNED_OK
  int nint = n / sizeof(int);

  if (nint >> 3)
    {
      int j = 0;
      const int *s1 = (const int*)s1c;
      const int *s2 = (const int*)s2c;
      int nint_8 = nint - 8;

      while (i <= nint_8 &&
	     s1[i++] == s2[j++] &&
	     s1[i++] == s2[j++] &&
	     s1[i++] == s2[j++] &&
	     s1[i++] == s2[j++] &&
	     s1[i++] == s2[j++] &&
	     s1[i++] == s2[j++] &&
	     s1[i++] == s2[j++] &&	 
	     s1[i++] == s2[j++]) { }

      i = (i - 1) * sizeof(int);
    }
#endif

  while (i < n && s1c[i] == s2c[i])
    {
      i++;
    }
  return i;
}

static inline int
test1(const char *s1c, const char *s2c, int n) {
  int i = 0;
  while (i < n && s1c[i] == s2c[i])
    {
      i++;
    }
  return i;
}

int main(/*int argc, char **argv*/) {
  char *buf1 = malloc(NUM+1);
  char *buf2 = malloc(NUM+1);
  int i;

  for (i = 0; i < NUM; i++) {
    buf1[i] = buf2[i] = rand();
  }

  buf2[NUM-1]++;

  printf ("ALIGNED\n");

  run_test(buf1, buf2, "memcmp", &memcmp_fake);
  run_test(buf1, buf2, "test1", &test1);
  run_test(buf1, buf2, "test2", &test2);

  for (i = 0; i < NUM; i++) {
    buf1[i] = buf2[i+1] = rand();
  }

  buf2[NUM]++;

  printf ("UNALIGNED\n");

  run_test(buf1, buf2+1, "memcmp", &memcmp_fake);
  run_test(buf1, buf2+1, "test1", &test1);
  run_test(buf1, buf2+1, "test2", &test2);

  return 0;
}
