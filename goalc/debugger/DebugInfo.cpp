#include "DebugInfo.h"

#include <utility>

DebugInfo::DebugInfo(std::string obj_name) : m_obj_name(std::move(obj_name)) {}
