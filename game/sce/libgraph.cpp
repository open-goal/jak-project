#include "libgraph.h"

#include "common/log/log.h"

namespace ee {
void sceGsResetPath() {}
void sceGsResetGraph(int mode, int inter, int omode, int ffmode) {
  lg::warn("sceGsResetGraph: mode {} inter {} omode {} fmode {}", mode, inter, omode, ffmode);
}
}  // namespace ee
