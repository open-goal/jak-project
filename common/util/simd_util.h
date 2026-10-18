#pragma once

#ifndef __aarch64__
#include <immintrin.h>
#else
#include "third-party/sse2neon/sse2neon.h"
#endif