#include "game/kernel/jak1/JaksBoot.h"

#include "game/kernel/jak1/kboot.h"
#include "game/kernel/jak1/klisten.h"
#include "game/kernel/jak1/kscheme.h"
#include "game/kernel/jak2/kboot.h"
#include "game/kernel/jak2/klisten.h"
#include "game/kernel/jak2/kscheme.h"
#include "game/runtime.h"

s32 JaksBoot::goal_main(int argc, const char* const* argv) {
  switch (g_game_version) {
    case GameVersion::Jak1:
      jak1::goal_main(argc, argv);
      break;
    case GameVersion::Jak2:
      jak2::goal_main(argc, argv);
      break;
    default:
      ASSERT_MSG(false, "Unsupported game version");
  }
}
void JaksBoot::init_globals(void) {
  jak1::kboot_init_globals();
  jak2::kboot_init_globals();

  jak1::kscheme_init_globals();
  jak2::kscheme_init_globals();

  jak1::klisten_init_globals();
  jak2::klisten_init_globals();
}
