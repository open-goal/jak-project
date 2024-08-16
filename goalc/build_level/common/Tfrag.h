#pragma once

#include <string>

#include "gltf_mesh_extract.h"

#include "common/custom_data/Tfrag3Data.h"

class DataObjectGenerator;

class DrawableTreeTfrag {
  // "drawable-tree-tfrag"
  // "drawable-inline-array-tfrag"
 public:
  DrawableTreeTfrag(const std::string& type, const std::string& array_type)
      : m_type(type), m_array_type(array_type) {}
  size_t add_to_object_file(DataObjectGenerator& gen) const;

 private:
  std::string m_type;
  std::string m_array_type;
};

void tfrag_from_gltf(const gltf_mesh_extract::TfragOutput& mesh_extract_out,
                     std::vector<tfrag3::TfragTree>& out_pc);