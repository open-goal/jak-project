#include "dvd_driver.h"

#include "common/util/Assert.h"

#include <memory>

namespace jak3 {

std::unique_ptr<CDvdDriver> g_DvdDriver;

void jak3_overlord_init_globals_dvd_driver() {
  g_DvdDriver = std::make_unique<CDvdDriver>();
}

CDvdDriver* get_driver() {
  return g_DvdDriver.get();
}

void CDvdDriver::Initialize() {
  ASSERT_NOT_REACHED();
}

}  // namespace jak3