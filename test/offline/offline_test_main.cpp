#include <future>
#include <string>

#include "common/log/log.h"
#include "common/util/term_util.h"
#include "common/util/unicode_util.h"

#include "config/config.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "framework/file_management.h"
#include "framework/orchestration.h"

#include "third-party/CLI11.hpp"
#include "third-party/fmt/format.h"

int main(int argc, char* argv[]) {
  ArgumentGuard u8_guard(argc, argv);

  bool dump_current_output = false;
  std::string iso_data_path;
  std::string game_name;
  // Useful for testing in debug mode (dont have to wait for everything to finish)
  int max_files = -1;
  std::string single_file = "";
  uint32_t num_threads = 1;
  std::string project_path;
  bool fail_on_cmp = false;
  bool pretty_print = false;

  CLI::App app{"OpenGOAL - Offline Reference Test Runner"};
  app.add_option("--iso_data_path", iso_data_path, "The path to the folder with the ISO data files")
      ->check(CLI::ExistingPath)
      ->required();
  app.add_option("--game", game_name, "The game name, for example 'jak1'")->required();
  app.add_flag("-d,--dump_current_output", dump_current_output,
               "Output the current output to a folder, use in conjunction with the reference test "
               "files update script");
  app.add_option("-m,--max_files", max_files,
                 "Limit the amount of files ran in a single test, picks the first N");
  app.add_option("-t,--num_threads", num_threads,
                 "The number of threads to partition the offline test work between");
  app.add_option("-f,--file", single_file,
                 "Limit the offline test routine to a single file to decompile/compile -- useful "
                 "when you are just iterating on a single file");
  app.add_flag("--fail-on-cmp", fail_on_cmp, "Fail the tests immediately if the comparison fails");
  app.add_flag("-p,--pretty-print", pretty_print,
               "Use the condensed and progress-indicating printing format");
  app.add_option("--proj-path", project_path, "Project path");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  if (pretty_print) {
    lg::set_stdout_level(lg::level::off_unless_die);
    term_util::clear();
  }
  lg::initialize();

  std::optional<fs::path> pp;
  if (!project_path.empty()) {
    pp = project_path;
  }
  if (!file_util::setup_project_path(pp)) {
    lg::error("Couldn't setup project path, tool is supposed to be ran in the jak-project repo!");
    return 1;
  }

  // Setup environment, fetch files
  auto config = OfflineTestConfig(game_name, iso_data_path, num_threads, dump_current_output,
                                  fail_on_cmp, false, pretty_print);

  lg::info("Finding files...");
  auto source_files = find_source_files(game_name, config.dgos, single_file);
  if (max_files > 0 && max_files < (int)source_files.size()) {
    source_files.erase(source_files.begin() + max_files, source_files.end());
  }

  // Figure out the number of threads, prepare their statuses and start printing it
  if (num_threads < 1) {
    num_threads = 1;
  } else if (num_threads > 1) {
    num_threads = std::min(num_threads, std::thread::hardware_concurrency());
  }

  // Distribute the work amongst the threads, partitioning by DGO
  decompiler::init_opcode_info();
  auto workers = distribute_work(config, source_files);

  // summarize results:
  OfflineTestThreadResult total;
  for (auto& worker : workers) {
    auto ret = worker.get();
    total.add(ret);
  }

  if (!total.compare.total_pass) {
    lg::error("Comparison failed.");
    for (auto& f : total.compare.failing_files) {
      fmt::print("{}\n", f.filename);
      fmt::print("{}\n", f.diff);
    }
    lg::error("Failing files:");
    for (auto& f : total.compare.failing_files) {
      lg::error("- {}", f.filename);
    }
  }

  if (!total.compile.ok) {
    for (auto& f : total.compile.failing_files) {
      lg::error("{}", f.filename);
      fmt::print("{}\n", f.error);
    }
  }

  fmt::print("Compiled {} lines in {:.3f}s ({} lines/sec)\n", total.compile.num_lines,
             total.time_spent_compiling,
             (int)(total.compile.num_lines / total.time_spent_compiling));

  if (!total.exit_code) {
    fmt::print("pass!\n");
  }

  return total.exit_code;
}
