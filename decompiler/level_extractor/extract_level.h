#pragma once

#include <vector>

#include "common/math/Vector.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {



void extract_from_level(ObjectFileDB& db, const std::string& dgo_name);
}

