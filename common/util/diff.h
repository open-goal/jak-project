#pragma once

#include <string>

/*!
 * Diff two strings. This uses the code from gtest's diff implementation.
 */
std::string diff_strings(const std::string& lhs, const std::string& rhs);