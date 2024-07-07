
#include "game/overlord/jak3/basefile.h"
#include "game/overlord/jak3/basefilesystem.h"
#include "game/overlord/jak3/dma.h"
#include "game/overlord/jak3/dvd_driver.h"
#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/iso_cd.h"
#include "game/overlord/jak3/iso_queue.h"
#include "game/overlord/jak3/isocommon.h"
#include "game/overlord/jak3/list.h"
#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/pagemanager.h"
#include "game/overlord/jak3/ramdisk.h"
#include "game/overlord/jak3/sbank.h"
#include "game/overlord/jak3/soundcommon.h"
#include "game/overlord/jak3/spustreams.h"
#include "game/overlord/jak3/srpc.h"
#include "game/overlord/jak3/ssound.h"
#include "game/overlord/jak3/stream.h"
#include "game/overlord/jak3/streamlist.h"
#include "game/overlord/jak3/vag.h"
#include "game/overlord/jak3/vblank_handler.h"

namespace jak3 {
void jak3_overlord_init_globals_all() {
  jak3_overlord_init_globals_overlord();
  jak3_overlord_init_globals_pagemanager();
  jak3_overlord_init_globals_iso_cd();
  jak3_overlord_init_globals_dma();
  jak3_overlord_init_globals_iso();
  jak3_overlord_init_globals_iso_queue();
  jak3_overlord_init_globals_srpc();
  jak3_overlord_init_globals_vag();
  jak3_overlord_init_globals_ssound();
  jak3_overlord_init_globals_iso_api();
  jak3_overlord_init_globals_spustreams();
  jak3_overlord_init_globals_list();
  jak3_overlord_init_globals_vblank_handler();
  jak3_overlord_init_globals_dvd_driver();
  jak3_overlord_init_globals_basefile();
  jak3_overlord_init_globals_basefilesystem();
  jak3_overlord_init_globals_ramdisk();
  jak3_overlord_init_globals_isocommon();
  jak3_overlord_init_globals_stream();
  jak3_overlord_init_globals_sbank();
  jak3_overlord_init_globals_soundcommon();
  jak3_overlord_init_globals_streamlist();
}
}  // namespace jak3