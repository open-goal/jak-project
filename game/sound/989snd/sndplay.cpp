#include "player.h"

int main(int argc, char* argv[]) {
  snd::player player;
  unsigned bankid = 0;

  std::filesystem::path file = argv[1];

  if (argc > 1) {
    bankid = player.load_bank(file, 0);
    unsigned sound = player.play_sound(bankid, 0);
    fmt::print("sound {} started\n", sound);
  }

  while (true) {
    timespec rqtp{}, rmtp{};
    rqtp.tv_nsec = 0;
    rqtp.tv_sec = 1;
    if (nanosleep(&rqtp, &rmtp) == -1) {
      break;
    }
  }

  // for (auto& b : gBanks) {
  //     fmt::print("Bank {:.4}\n", (char*)&b.data.BankID);

  //    fmt::print("Songs:\n");
  //    for (auto& s : b.sounds) {
  //        fmt::print("type {}\n", s.Type);
  //        fmt::print("bank {:.4}\n", (char*)&s.Bank);
  //        fmt::print("midi {:.4}\n", (char*)&s.MIDIID);
  //        fmt::print("unk {:.4}\n", (char*)&s.OrigBank);
  //        fmt::print("{}\n", s.Index);
  //    }
  //}

  return 0;
}
