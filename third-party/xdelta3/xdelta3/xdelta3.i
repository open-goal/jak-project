%module xdelta3
%import cstring.i
%import argcargv.i
%{
#include "xdelta3.h"

int xd3_main_cmdline (int ARGC, char **ARGV);
%}

%cstring_input_binary(const char *input, unsigned int input_size);
%cstring_input_binary(const char *source, unsigned int source_size);

%define %max_output_withsize(TYPEMAP, SIZE, MAXSIZE)
%typemap(in) MAXSIZE (unsigned int alloc_size) {
  $1 = alloc_size = PyInt_AsLong(obj2);
}
%typemap(in,numinputs=0) (TYPEMAP, SIZE) {
}
%typemap(check) (TYPEMAP, SIZE) {
  // alloc_size input is #7th position in xd3_xxcode_memory()
  $1 = malloc(alloc_size7);
  $2 = &alloc_size7;
}
%typemap(argout,fragment="t_output_helper") (TYPEMAP, SIZE) {
  if (result == 0) {
    PyObject *o;
    // alloc_size7 now carries actual size
    o = PyString_FromStringAndSize($1,alloc_size7);
    $result = t_output_helper($result,o);
  } else {
    $result = t_output_helper($result,Py_None);
  }
  free($1);
}
%typemap(default) int flags {
  $1 = 0;
}
%enddef

%max_output_withsize(char *output_buf, unsigned int *output_size, unsigned int max_output);

int     xd3_encode_memory (const uint8_t *input,
			   usize_t        input_size,
			   const uint8_t *source,
			   usize_t        source_size,
			   uint8_t       *output_buffer,
			   usize_t       *output_size,
			   usize_t        avail_output,
			   int            flags);

int     xd3_decode_memory (const uint8_t *input,
			   usize_t        input_size,
			   const uint8_t *source,
			   usize_t        source_size,
			   uint8_t       *output_buf,
			   usize_t       *output_size,
			   usize_t        avail_output,
			   int            flags);

int     xd3_main_cmdline (int ARGC, char **ARGV);

/* Is this the right way? */
enum {
  /*XD3_JUST_HDR,*/
  /*XD3_SKIP_WINDOW,*/
  /*XD3_SKIP_EMIT,*/
  /*XD3_FLUSH,*/
  XD3_SEC_DJW,
  XD3_SEC_FGK,
  /*XD3_SEC_TYPE,*/
  XD3_SEC_NODATA,
  XD3_SEC_NOINST,
  XD3_SEC_NOADDR,
  /*XD3_SEC_OTHER,*/
  XD3_ADLER32,
  XD3_ADLER32_NOVER,
  XD3_NOCOMPRESS,
  XD3_BEGREEDY,
  XD3_COMPLEVEL_SHIFT,
  XD3_COMPLEVEL_MASK,
  XD3_COMPLEVEL_1,
  XD3_COMPLEVEL_3,
  XD3_COMPLEVEL_6,
  XD3_COMPLEVEL_9,
};
