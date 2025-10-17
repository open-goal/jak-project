
#include "game/overlord/jakx/basefile.h"
#include "game/overlord/jakx/basefilesystem.h"
#include "game/overlord/jakx/dma.h"
#include "game/overlord/jakx/dvd_driver.h"
#include "game/overlord/jakx/iso.h"
#include "game/overlord/jakx/iso_api.h"
#include "game/overlord/jakx/iso_cd.h"
#include "game/overlord/jakx/iso_queue.h"
#include "game/overlord/jakx/isocommon.h"
#include "game/overlord/jakx/list.h"
#include "game/overlord/jakx/overlord.h"
#include "game/overlord/jakx/pagemanager.h"
#include "game/overlord/jakx/ramdisk.h"
#include "game/overlord/jakx/sbank.h"
#include "game/overlord/jakx/soundcommon.h"
#include "game/overlord/jakx/spustreams.h"
#include "game/overlord/jakx/srpc.h"
#include "game/overlord/jakx/ssound.h"
#include "game/overlord/jakx/stream.h"
#include "game/overlord/jakx/streamlist.h"
#include "game/overlord/jakx/vag.h"
#include "game/overlord/jakx/vblank_handler.h"

namespace jakx {
void jakx_overlord_init_globals_all() {
  jakx_overlord_init_globals_overlord();
  jakx_overlord_init_globals_pagemanager();
  jakx_overlord_init_globals_iso_cd();
  jakx_overlord_init_globals_dma();
  jakx_overlord_init_globals_iso();
  jakx_overlord_init_globals_iso_queue();
  jakx_overlord_init_globals_srpc();
  jakx_overlord_init_globals_vag();
  jakx_overlord_init_globals_ssound();
  jakx_overlord_init_globals_iso_api();
  jakx_overlord_init_globals_spustreams();
  jakx_overlord_init_globals_list();
  jakx_overlord_init_globals_vblank_handler();
  jakx_overlord_init_globals_dvd_driver();
  jakx_overlord_init_globals_basefile();
  jakx_overlord_init_globals_basefilesystem();
  jakx_overlord_init_globals_ramdisk();
  jakx_overlord_init_globals_isocommon();
  jakx_overlord_init_globals_stream();
  jakx_overlord_init_globals_sbank();
  jakx_overlord_init_globals_soundcommon();
  jakx_overlord_init_globals_streamlist();
}
}  // namespace jakx