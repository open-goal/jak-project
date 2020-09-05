#ifndef JAK_LOGGER_H
#define JAK_LOGGER_H

#include <string>
#include <cstdarg>
#include <unordered_map>

enum LoggerColor { COLOR_NORMAL, COLOR_RED, COLOR_GREEN, COLOR_BLUE };

enum LoggerDestKind { LOG_STDOUT, LOG_STDERR, LOG_FILE, LOG_IGNORE };

struct LoggerDest {
  LoggerDestKind kind = LOG_STDOUT;
  LoggerColor color = COLOR_NORMAL;
  std::string prefix;
};

enum LoggerMessageKind {
  MSG_GOAL,
  MSG_ICE,
  MSG_ERR,
  MSG_COLOR,
  MSG_EMIT,
  MSG_DEBUG,
  MSG_WARN,
  MSG_TGT,
  MSG_TGT_INFO,
};

class Logger {
 public:
  void set_file(std::string filename);
  void log(LoggerMessageKind kind, const char* format, ...);
  std::unordered_map<LoggerMessageKind, LoggerDest> config;
  void close();

 private:
  FILE* fp = nullptr;
};

extern Logger gLogger;

#endif  // JAK_LOGGER_H
