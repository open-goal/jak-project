#pragma once

#include <string>
#include <vector>

/*!
 * Diff two strings. This uses the code from gtest's diff implementation.
 */
std::string diff_strings(const std::string& lhs, const std::string& rhs);

std::vector<std::string> split_string(const ::std::string& str, char delimiter = '\n');
