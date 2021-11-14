
// hides warnings
#ifdef __linux__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#endif

#define STB_IMAGE_IMPLEMENTATION
#include "third-party/stb_image.h"

#ifdef __linux__
#pragma GCC diagnostic pop
#endif