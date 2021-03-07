#include "ReplUtils.h"

#include "common/util/FileUtil.h"
#include "third-party/replxx/include/replxx.hxx"

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"

using Replxx = replxx::Replxx;

void ReplWrapper::clear_screen() {
  repl.clear_screen();
}

void ReplWrapper::set_history_max_size( size_t len) {
  repl.set_max_history_size(len);
}

const char* ReplWrapper::readline( const std::string& prompt) {
  return repl.input(prompt);
}

void ReplWrapper::add_to_history( const std::string& line) {
  repl.history_add(line);
}

void ReplWrapper::save_history() {
  // NOTE - library doesn't seem unicode safe on windows
  std::filesystem::path path = file_util::get_user_home_dir();
  if (std::filesystem::exists(path)) {
    repl.history_save((path / ".opengoal.repl.history").string());
  }
}

void ReplWrapper::load_history() {
  // NOTE - library doesn't seem unicode safe on windows
  std::filesystem::path path = file_util::get_user_home_dir();
  if (std::filesystem::exists(path)) {
    repl.history_load((path / ".opengoal.repl.history").string());
  }
}

void ReplWrapper::print_help_message() {
  fmt::print(fmt::emphasis::bold, "\nREPL Controls:\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(e)\n");
  fmt::print(" - Exit the compiler once the current REPL command is finished\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(lt [ip-address] [port-number])\n");
  fmt::print(
      " - Connect the listener to a running target. The IP address defaults to `127.0.0.1` and the "
      "port to `8112`\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(r [ip-address] [port-number])\n");
  fmt::print(
      " - Attempt to reset the target and reconnect. After this, the target will have nothing "
      "loaded.\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(:status)\n");
  fmt::print(" - Send a ping-like message to the target. Requires the target to be connected\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(shutdown-target)\n");
  fmt::print(" - If the target is connected, make it exit\n");

  fmt::print(fmt::emphasis::bold, "\nCompiling & Building:\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow), "(m \"filename\")\n");
  fmt::print(" - Compile an OpenGOAL source file\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow), "(ml \"filename\")\n");
  fmt::print(" - Compile and Load an OpenGOAL source file\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow), "(build-game)\n");
  fmt::print(" - Loads and builds all game files and rebuilds DGOs\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow), "(build-kernel)\n");
  fmt::print(" - Similar to (build-game) but only the kernel files\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow), "(blg)\n");
  fmt::print(" - Performs a (build-game) and then loads all CGOs\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow),
             "(build-dgos \"path/to/dgos/description/file\")\n");
  fmt::print(
      " - Builds all the DGO files described in the DGO description file. See "
      "`goal_src/builds/dgos.txt` for an example.\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow),
             "(asm-data-file tool-name \"file-name\")\n");
  fmt::print(
      " - Build a data file. The `tool-name` refers to which data building tool should be used.\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::yellow), "(build-data)\n");
  fmt::print(" - Macro for rebuilding all data files\n");

  fmt::print(fmt::emphasis::bold, "\nOther:\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::magenta), "(gs)\n");
  fmt::print(" - Enter a GOOS REPL\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::magenta),
             "(set-config! config-name config-value)\n");
  fmt::print(" - Used to set compiler configuration\n");
}
