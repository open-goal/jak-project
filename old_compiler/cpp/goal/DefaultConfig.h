/*!
 * @file DefaultConfig.h
 * The default configuration of the compiler.
 * These configuration settings are used when the compiler is first loading.
 * As goal-lib is loaded, it will override these values.
 */

#ifndef JAK_V2_DEFAULTCONFIG_H
#define JAK_V2_DEFAULTCONFIG_H

#include <utility>
#include <string>

static std::pair<std::string, std::string> default_config[] = {{"debug-print-ir", "#f"},
                                                               {"debug-print-obj", "#f"},
                                                               {"print-asm-file-time", "#f"}};

#endif  // JAK_V2_DEFAULTCONFIG_H
