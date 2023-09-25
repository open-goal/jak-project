#pragma once

#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {

std::string extract_actors_to_json(const level_tools::DrawableInlineArrayActor& actors);

}