#pragma once

#include <cstddef>

// Note: these are not implemented on windows and will return zero.
size_t get_peak_rss();