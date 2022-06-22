#include "xdelta3.h"

extern int VVV;

int VVV;

void use(int r)
{
  VVV = r;
}

int main() {
  xd3_config config;
  xd3_stream stream;
  xd3_source source;

  xd3_init_config (& config, 0);
  use (xd3_config_stream (&stream, &config));
  use (xd3_close_stream (&stream));
  xd3_abort_stream (&stream);
  xd3_free_stream (&stream);
  
  xd3_avail_input (& stream, NULL, 0);
  xd3_consume_output (& stream);
  
  use (xd3_set_source (& stream, & source));
  xd3_set_flags (& stream, 0);
  
  use (xd3_decode_stream (& stream, NULL, 0, NULL, NULL, 0));
  use (xd3_decode_input (&stream));
  use (xd3_get_appheader (& stream, NULL, NULL));
  
#if XD3_ENCODER
  use (xd3_encode_input (&stream));
  use (xd3_encode_stream (& stream, NULL, 0, NULL, NULL, 0));
  use (xd3_set_appheader (& stream));
  use (xd3_encoder_used_source (& stream));
  use (xd3_encoder_srcbase (& stream));
  use (xd3_encoder_srclen (& stream));
#endif
  return 0;
}
