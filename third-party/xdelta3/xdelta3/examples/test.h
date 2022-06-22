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

#define NOT_MAIN 1

#include "xdelta3.h"
#include "xdelta3.c"

static int read_whole_file(const char *name,
			   uint8_t **buf_ptr,
			   size_t *buf_len) {
  main_file file;
  int ret;
  xoff_t len;
  usize_t nread;
  main_file_init(&file);
  file.filename = name;
  ret = main_file_open(&file, name, XO_READ);
  if (ret != 0) {
    fprintf(stderr, "open failed\n");
    goto exit;
  }
  ret = main_file_stat(&file, &len);
  if (ret != 0) {
    fprintf(stderr, "stat failed\n");
    goto exit;
  }
  
  (*buf_len) = (size_t)len;
  (*buf_ptr) = (uint8_t*) main_malloc(*buf_len);
  ret = main_file_read(&file, *buf_ptr, *buf_len, &nread,
		       "read failed");
  if (ret == 0 && *buf_len == nread) {
    ret = 0;
  } else {
    fprintf(stderr, "invalid read\n");
    ret = XD3_INTERNAL;
  }
 exit:
  main_file_cleanup(&file);
  return ret;
}

