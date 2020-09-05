#include <stdexcept>
#include "Logger.h"

void Logger::close() {
  if (fp) {
    fclose(fp);
  }
}

void Logger::set_file(std::string filename) {
  if (fp) {
    fclose(fp);
  }

  fp = fopen(filename.c_str(), "w");
  if (!fp) {
    throw std::runtime_error("invalid file name " + filename + " in logger");
  }
}

void Logger::log(LoggerMessageKind kind, const char* format, ...) {
  FILE* dest = nullptr;
  auto& settings = config[kind];
  switch (settings.kind) {
    case LOG_STDERR:
      dest = stderr;
      break;
    case LOG_STDOUT:
      dest = stdout;
      break;
    case LOG_IGNORE:
      dest = nullptr;
      break;
    case LOG_FILE:
      dest = fp;
      break;
    default:
      throw std::runtime_error("unknown log destination in log");
  }

  if (!dest)
    return;

  if (!settings.prefix.empty()) {
    fprintf(dest, "%s", settings.prefix.c_str());
  }

  if (settings.color != COLOR_NORMAL) {
    const char* color_codes[] = {"", "[0;31m", "[0;32m", "[0;36m"};
    printf("\033%s", color_codes[settings.color]);
  }

  va_list arglist;
  va_start(arglist, format);
  vfprintf(dest, format, arglist);
  va_end(arglist);

  if (settings.color != COLOR_NORMAL) {
    printf("\033[0m");
  }

  // todo, does this make things slow?
  if (settings.kind == LOG_FILE) {
    fflush(fp);
  }
}

Logger gLogger;