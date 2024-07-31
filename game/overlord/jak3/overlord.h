#pragma once

#include <cstdlib>

#include "common/common_types.h"
#include "common/log/log.h"

namespace jak3 {

/*!
 * External entry point for the game to start Overlord. This assumes that the IOP Kernel
 * is at least initialized, then sets up all overlord threads/RPCs. Once this returns,
 * it's safe to call overlord functions.
 */
int start_overlord_wrapper(bool* signal);
void jak3_overlord_init_globals_overlord();
char* strncpyz(char* dst, const char* src, size_t n);

extern int g_nServerThreadID;
extern int g_nPlayerThreadID;
extern int g_nLoaderThreadID;

enum class LogCategory {
  PAGING,
  FILESYSTEM,
  WARN,
  SPU_DMA_STR,
  EE_DMA,
  ISO_QUEUE,
  VAG_SETUP,
  DGO,
  RPC,
  STR_RPC,
  PLAYER_RPC,
  DRIVER,
  NUM_CATETORIES
};

constexpr bool g_OverlordLogEnable[(int)LogCategory::NUM_CATETORIES] = {
    false,  // paging: cpage's, page manager, page crossing, etc
    true,   // filesystem: opening/finding files
    true,   // warning: something weird
    false,  // spu dma streaming: vag streaming, clocks, spu, dma
    true,   // ee dma: sending stuff to the ee (dgo, etc)
    true,   // iso queue: message queuing
    true,   // vag setup: creation of vag commands (lists, etc)
    false,  // dgo
    true,   // rpc in general
    true,   // str rpc
    false,  // PLAYER
    false,  // driver
};

template <typename... Args>
void ovrld_log(LogCategory category, const std::string& format, Args&&... args) {
  if (g_OverlordLogEnable[(int)category]) {
    lg::info(format, std::forward<Args>(args)...);
  }
}

}  // namespace jak3