#pragma once

#include "TypeSystem.h"
#include "common/goos/Object.h"

TypeSpec parse_deftype(const goos::Object& deftype, TypeSystem* ts);
