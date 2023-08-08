

namespace term_util {
void clear();
int row_count();
int col_count();

#define define_common_cli_arguments(cli_app_name) \
  bool _cli_flag_disable_ansi = false;            \
  cli_app_name.add_flag("--disable-ansi", _cli_flag_disable_ansi, "Disable ANSI colors");
}  // namespace term_util
