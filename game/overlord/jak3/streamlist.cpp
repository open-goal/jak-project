#include "streamlist.h"
namespace jak3 {

List g_RequestedStreamsList;
void jak3_overlord_init_globals_streamlist() {
  g_RequestedStreamsList = {};
}
}  // namespace jak3