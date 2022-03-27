#include "player.h"
#include <filesystem>

int main(int argc, char* argv[]) {
  snd::player player;
  unsigned bankid = 0;

  std::filesystem::path file = argv[1];

  if (argc > 2) {
    bankid = player.load_bank(file, 0);
    unsigned sound = player.play_sound(bankid, atoi(argv[2]), 0x400, 0, 0, 0);
    fmt::print("sound {} started\n", sound);
  }

  while (true) {
    timespec rqtp{}, rmtp{};
    rqtp.tv_nsec = 0;
    rqtp.tv_sec = 1;
#ifdef __linux
    if (nanosleep(&rqtp, &rmtp) == -1) {
      break;
    }
#endif
  }

  return 0;
}
