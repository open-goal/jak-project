#include "basefilesystem.h"

#include "game/sce/iop.h"
#include "common/util/Assert.h"

using namespace iop;
namespace jak3 {
void jak3_overlord_init_globals_basefilesystem() {}

CBaseFileSystem::CBaseFileSystem() {
  for (auto& sema : m_Sema) {
    sema = -1;

    SemaParam param;
    param.max_count = 1;
    param.attr = 0;
    param.init_count = 1;
    param.option = 0;
    sema = CreateSema(&param);
    if (sema < 0) {
      ASSERT_NOT_REACHED();
    }
  }
}
}