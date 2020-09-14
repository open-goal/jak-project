#pragma once

/*!
 * @file LinkedObjectFileCreation.h
 * Create a LinkedObjectFile from raw object file data.
 * This implements a decoder for the GOAL linking format.
 */

#ifndef NEXT_LINKEDOBJECTFILECREATION_H
#define NEXT_LINKEDOBJECTFILECREATION_H

#include "LinkedObjectFile.h"

LinkedObjectFile to_linked_object_file(const std::vector<uint8_t>& data, const std::string& name);

#endif  // NEXT_LINKEDOBJECTFILECREATION_H
