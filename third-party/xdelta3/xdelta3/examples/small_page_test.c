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

#define PAGE_SIZE 4096

#define SPACE_MAX 131072   // how much memory per process
#define OUTPUT_MAX 1024    // max size for output
#define XD3_ALLOCSIZE 256  // internal size for various buffers
#define IOPT_SIZE 128      // instruction buffer

// SPACE_MAX of 32K is sufficient for most inputs with XD3_COMPLEVEL_1
// XD3_COMPLEVEL_9 requires about 4x more space than XD3_COMPLEVEL_1

#include "xdelta3.h"
#include "xdelta3.c"

typedef struct _context {
  uint8_t *buffer;
  int allocated;
} context_t;

static int max_allocated = 0;

void*
process_alloc (void* opaque, usize_t items, usize_t size)
{
  context_t *ctx = (context_t*) opaque;
  usize_t t = items * size;
  void *ret;

  if (ctx->allocated + t > SPACE_MAX)
    {
      return NULL;
    }

  ret = ctx->buffer + ctx->allocated;
  ctx->allocated += t;
  return ret;
}

void
process_free (void* opaque, void *ptr)
{
}

int
process_page (int            is_encode,
	      int          (*func) (xd3_stream *),
	      const uint8_t *input,
	      usize_t        input_size,
	      const uint8_t *source,
	      uint8_t       *output,
	      usize_t       *output_size,
	      usize_t        output_size_max,
	      int            flags) {

  /* On my x86 this is 1072 of objects on the stack */
  xd3_stream stream;
  xd3_config config;
  xd3_source src;
  context_t *ctx = calloc(SPACE_MAX, 1);
  int ret;

  memset (&config, 0, sizeof(config));

  if (ctx == NULL)
    {
      printf("calloc failed\n");
      return -1;
    }

  ctx->buffer = (uint8_t*)ctx;
  ctx->allocated = sizeof(*ctx);

  config.flags = flags;
  config.winsize = PAGE_SIZE;
  config.sprevsz = PAGE_SIZE;
  config.srcwin_maxsz = PAGE_SIZE;
  config.iopt_size = IOPT_SIZE;
  config.alloc = &process_alloc;
  config.freef = &process_free;
  config.opaque = (void*) ctx;

  src.blksize = PAGE_SIZE;
  src.onblk = PAGE_SIZE;
  src.curblk = source;
  src.curblkno = 0;

  if ((ret = xd3_config_stream (&stream, &config)) != 0 ||
      (ret = xd3_set_source_and_size (&stream, &src, PAGE_SIZE)) != 0 ||
      (ret = xd3_process_stream (is_encode,
				 &stream,
				 func, 1,
				 input, input_size,
				 output, output_size,
				 output_size_max)) != 0)
    {
      if (stream.msg != NULL)
	{
	  fprintf(stderr, "stream message: %s\n", stream.msg);
	}
    }

  xd3_free_stream (&stream);
  if (max_allocated < ctx->allocated)
    {
      max_allocated = ctx->allocated;
      fprintf(stderr, "max allocated %d\n", max_allocated);
    }

  free(ctx);
  return ret;
}

int test(int stride, int encode_flags)
{
  uint8_t frompg[PAGE_SIZE];
  uint8_t topg[PAGE_SIZE];
  uint8_t output[OUTPUT_MAX];
  uint8_t reout[PAGE_SIZE];
  usize_t output_size;
  usize_t re_size;
  int i, j, ret;

  for (i = 0; i < PAGE_SIZE; i++)
    {
      topg[i] = frompg[i] = (rand() >> 3 ^ rand() >> 6 ^ rand() >> 9);
    }

  // change 1 byte every stride
  if (stride > 0)
    {
      for (j = stride; j <= PAGE_SIZE; j += stride)
	{
	  topg[j - 1] ^= 0xff;
	}
    }

  if ((ret = process_page (1, xd3_encode_input,
			   topg, PAGE_SIZE,
			   frompg, output,
			   &output_size, OUTPUT_MAX,
			   encode_flags)) != 0)
    {
      fprintf (stderr, "encode failed: stride %u flags 0x%x\n",
	       stride, encode_flags);
      return ret;
    }

  if ((ret = process_page (0, xd3_decode_input,
			   output, output_size,
			   frompg, reout,
			   &re_size, PAGE_SIZE,
			   0)) != 0)
    {
      fprintf (stderr, "decode failed: stride %u output_size %u flags 0x%x\n",
	       stride, output_size, encode_flags);
      return ret;
    }

  if (output_size > OUTPUT_MAX || re_size != PAGE_SIZE)
    {
      fprintf (stderr, "internal error: %u != %u\n", output_size, re_size);
      return -1;
    }

  for (i = 0; i < PAGE_SIZE; i++)
    {
      if (reout[i] != topg[i])
	{
	  fprintf (stderr, "encode-decode error: position %d\n", i);
	  return -1;
	}
    }

  fprintf(stderr, "stride %d flags 0x%x size %u ",
	  stride, encode_flags, output_size);
  fprintf(stderr, "%s\n", (ret == 0) ? "OK" : "FAIL");

  return 0;
}

int main()
{
  int stride;
  int level;

  for (level = 1; level < 10; level = (level == 1 ? 3 : level + 3))
    {
      int lflag = level << XD3_COMPLEVEL_SHIFT;

      for (stride = 2; stride <= PAGE_SIZE; stride += 2)
	{
	  test(stride, lflag);
	  test(stride, lflag | XD3_SEC_DJW);
	}
    }

  return 0;
}
