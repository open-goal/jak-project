#include <filesystem>
#include <iostream>
#include <sstream>

#include "common/util/FileUtil.h"

#ifdef _WIN32
#include <windows.h>
#define sleep(n) Sleep(n * 1000)
#endif

#include "player.h"

#include "common/log/log.h"

int main(int argc, char* argv[]) {
  snd::Player player;

  fs::path file = argv[1];
  auto file_buf = file_util::read_binary_file(file);
  auto bankid = player.LoadBank(file_buf);

  if (argc > 2) {
    unsigned sound = player.PlaySound(bankid, atoi(argv[2]), 0x400, 0, 0, 0);
    lg::info("sound {} started", sound);
  }

  printf("commands:\n");
  printf(" play [id]\n");
  printf(" stop\n");
  printf(" dump-info\n");

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
        auto id = player.PlaySound(bankid, std::atoi(parts[1].c_str()), 0x400, 0, 0, 0);
        printf("sound handle %d started\n", id);
      }
    }

    if (parts[0] == "playall") {
      auto idx = 0;
      auto id = player.PlaySound(bankid, idx, 0x400, 0, 0, 0);
      while (true) {
        if (player.SoundStillActive(id)) {
          sleep(1);
        } else {
          idx++;
          id = player.PlaySound(bankid, idx, 0x400, 0, 0, 0);
        }
      }
    }

    if (parts[0] == "setreg") {
      if (parts.size() < 3) {
        printf("invalid args\n");
      } else {
        player.SetSoundReg(std::atoi(parts[1].c_str()), std::atoi(parts[2].c_str()),
                           std::atoi(parts[3].c_str()));
      }
    }

    if (parts[0] == "stop") {
      printf("stopping all sounds\n");
      player.StopAllSounds();
    }

    if (parts[0] == "dump-info") {
      player.DebugPrintAllSoundsInBank(bankid);
    }
  }

  return 0;
}
