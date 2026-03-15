#pragma once

#ifndef __aarch64__
#include "xmmintrin.h"
#else
#include "third-party/sse2neon/sse2neon.h"
#endif