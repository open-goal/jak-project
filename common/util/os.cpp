#include "os.h"

#ifdef __linux__

#include <sys/resource.h>

size_t get_peak_rss() {
  rusage x;
  getrusage(RUSAGE_SELF, &x);
  return x.ru_maxrss * 1024;
}

#else
int get_current_rss() {
  return 0;
}

int get_peak_rss() {
  return 0;
}
#endif