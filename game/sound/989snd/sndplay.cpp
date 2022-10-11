#include "player.h"

#include "common/log/log.h"

int main(int argc, char* argv[]) {
  snd::player player;
  unsigned bankid = 0;

  fs::path file = argv[1];

  if (argc > 2) {
    bankid = player.load_bank(file, 0);
    unsigned sound = player.play_sound(bankid, atoi(argv[2]), 0x400, 0, 0, 0);
    lg::info("sound {} started", sound);
  }

  while (true) {
#ifdef __linux
    timespec rqtp{}, rmtp{};
    rqtp.tv_nsec = 0;
    rqtp.tv_sec = 1;
    if (nanosleep(&rqtp, &rmtp) == -1) {
      break;
    }
#endif
  }

  return 0;
}
