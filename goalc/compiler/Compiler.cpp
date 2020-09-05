#include "Compiler.h"
#include "goalc/logger/Logger.h"

Compiler::Compiler() {
  init_logger();
  m_ts.add_builtin_types();
}

void Compiler::execute_repl() {}

Compiler::~Compiler() {
  gLogger.close();
}

void Compiler::init_logger() {
  gLogger.set_file("compiler.txt");
  gLogger.config[MSG_COLOR].kind = LOG_FILE;
  gLogger.config[MSG_DEBUG].kind = LOG_IGNORE;
  gLogger.config[MSG_TGT].color = COLOR_GREEN;
  gLogger.config[MSG_TGT_INFO].color = COLOR_BLUE;
  gLogger.config[MSG_WARN].color = COLOR_RED;
  gLogger.config[MSG_ICE].color = COLOR_RED;
  gLogger.config[MSG_ERR].color = COLOR_RED;
}