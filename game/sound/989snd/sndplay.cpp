#include <filesystem>
#include <iostream>
#include <sstream>

#ifdef _WIN32
#include <windows.h>
#define sleep(n) Sleep(n * 1000)
#endif

#include "player.h"

#include "common/log/log.h"

int main(int argc, char* argv[]) {
  snd::player player;
  unsigned bankid = 0;

  fs::path file = argv[1];
  bankid = player.load_bank(file, 0);

  if (argc > 2) {
    unsigned sound = player.play_sound(bankid, atoi(argv[2]), 0x400, 0, 0, 0);
    lg::info("sound {} started", sound);
  }

  printf("commands:\n");
  printf(" play [id]\n");
  printf(" stop\n");

  while (true) {
    printf("> ");
    std::string command;
    std::getline(std::cin, command);

    std::stringstream ss(command);
    std::string tmp;
    std::vector<std::string> parts;

    while (std::getline(ss, tmp, ' ')) {
      parts.push_back(tmp);
    }

    if (parts[0] == "play") {
      if (parts.size() < 2) {
        printf("invalid args\n");
      } else {
        auto id = player.play_sound(bankid, std::atoi(parts[1].c_str()), 0x400, 0, 0, 0);
        printf("sound handle %d started\n", id);
      }
    }

    if (parts[0] == "playall") {
      auto idx = 0;
      auto id = player.play_sound(bankid, idx, 0x400, 0, 0, 0);
      while (true) {
        if (player.sound_still_active(id)) {
          sleep(1);
        } else {
          idx++;
          id = player.play_sound(bankid, idx, 0x400, 0, 0, 0);
        }
      }
    }

    if (parts[0] == "setreg") {
      if (parts.size() < 3) {
        printf("invalid args\n");
      } else {
        player.set_sound_reg(std::atoi(parts[1].c_str()), std::atoi(parts[2].c_str()),
                             std::atoi(parts[3].c_str()));
      }
    }

    if (parts[0] == "stop") {
      printf("stopping all sounds\n");
      player.stop_all_sounds();
    }
  }

  return 0;
}
