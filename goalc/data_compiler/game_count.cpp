#include "game_count.h"

#include <vector>

#include "DataObjectGenerator.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"

void compile_game_count(const std::string& input_filename, const std::string& output_prefix) {
  struct Count {
    int buzzer;
    int money;
  };

  std::vector<Count> counts;
  int unknowns[2] = {0, 0};

  goos::Reader reader;
  auto code = reader.read_from_file({input_filename});
  int len = goos::list_length(code) - 1;
  int i = 0;
  std::string err;

  // parser
  goos::for_each_in_list(code.as_pair()->cdr, [&](const goos::Object& obj) {
    if (obj.is_pair()) {
      if (i == len - 1) {
        // last entry should be the unknowns.
        goos::Arguments args;
        if (!goos::get_va(obj, &err, &args)) {
          throw std::runtime_error(err);
        }
        if (!goos::va_check(args, {},
                            {{"unknown-1", {true, goos::ObjectType::INTEGER}},
                             {"unknown-2", {true, goos::ObjectType::INTEGER}}},
                            &err)) {
          throw std::runtime_error(err);
        }
        unknowns[0] = args.get_named("unknown-1").as_int();
        unknowns[1] = args.get_named("unknown-2").as_int();
      } else {
        goos::Arguments args;
        if (!goos::get_va(obj, &err, &args)) {
          throw std::runtime_error(err);
        }
        if (!goos::va_check(args, {},
                            {{"buzzer", {true, goos::ObjectType::INTEGER}},
                             {"money", {true, goos::ObjectType::INTEGER}}},
                            &err)) {
          throw std::runtime_error(err);
        }
        Count c;
        c.buzzer = args.get_named("buzzer").as_int();
        c.money = args.get_named("money").as_int();
        counts.push_back(c);
      }
    } else {
      throw std::runtime_error("Invalid game count file.");
    }
    i++;
  });

  // compiler
  DataObjectGenerator gen;
  gen.add_type_tag("game-count-info");
  gen.add_word(counts.size());
  for (auto& x : counts) {
    gen.add_word(x.money);
    gen.add_word(x.buzzer);
  }
  gen.add_word(unknowns[0]);
  gen.add_word(unknowns[1]);
  auto result = gen.generate_v4();

  file_util::write_binary_file(
      file_util::get_jak_project_dir() / "out" / output_prefix / "obj" / "game-cnt.go",
      result.data(), result.size());
}
