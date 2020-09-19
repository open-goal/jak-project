#pragma once

#ifndef JAK_V2_FILEIO_H
#define JAK_V2_FILEIO_H

#include <string>
#include <vector>

std::string combine_path(const std::string& parent, const std::string& child);
std::string base_name(const std::string& filename);
void init_crc();
uint32_t crc32(const uint8_t* data, size_t size);
uint32_t crc32(const std::vector<uint8_t>& data);

#endif  // JAK_V2_FILEIO_H
