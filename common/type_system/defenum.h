#pragma once

/*!
 * @file defenum.h
 * Parser for the GOAL "defenum" form.
 * This is used both in the compiler and in the decompiler for the type definition file.
 */

#include "TypeSystem.h"
#include "Enum.h"
#include "common/goos/Object.h"

void parse_defenum(const goos::Object& defenum, TypeSystem* ts, GoalEnum& goalenum);
