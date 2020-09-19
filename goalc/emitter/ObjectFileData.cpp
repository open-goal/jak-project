#include "ObjectFileData.h"

namespace emitter {
std::vector<u8> ObjectFileData::to_vector() const {
  std::vector<uint8_t> result;
  // header
  result.insert(result.end(), header.begin(), header.end());

  // link tables
  for (int seg = N_SEG; seg-- > 0;) {
    result.insert(result.end(), link_tables[seg].begin(), link_tables[seg].end());
  }

  // data (code + static objects, by segment)
  for (int seg = N_SEG; seg-- > 0;) {
    result.insert(result.end(), segment_data[seg].begin(), segment_data[seg].end());
    //    printf("seg %d data\n", seg);
    //    for (auto x : segment_data[seg]) {
    //      printf("%02x ", x);
    //    }
    //    printf("\n");
  }
  return result;
}
}  // namespace emitter