#include "third-party/fmt/format.h"
#include "dma_reader.h"
#include "common/util/assert.h"


//
///*!
// * "Flattening" DMA compacts it into a single stream.
// * Tag transfers are padded with 64-bits of 0's to keep things qw aligned.
// * It will be a nop for the VIF anyway.
// * Note that flattening may make the DMA stream larger, as it will repeat data.
// */
//std::vector<u64> flatten_dma(const DmaReadState& in) {
//  DmaReadState state = in;
//  std::vector<u64> result;
//  while (!state.ended) {
//    auto read_result = next_dma(&state);
//    result.push_back(0);  // tag transfer padding
//    result.push_back(read_result.transferred_tag);
//    for (u32 i = 0; i < 2 * read_result.qwc; i++) {
//      result.push_back(state.read<u64>(read_result.addr + 8 * i));
//    }
//  }
//  return result;
//}
//
//void debug_print_dma(const DmaReadState& in) {
//  DmaReadState state = in;
//  std::vector<u64> result;
//  while (!state.ended) {
//    u64 tag_data = state.read<u64>(state.tag_addr);
//    DmaTag tag(tag_data);
//    fmt::print("@ 0x{:8x} {}", state.tag_addr, tag.print());
//    next_dma(&state);
//  }
//}
//
//std::vector<std::vector<u64>> flatten_dma_per_bucket(const DmaReadState& in) {
//  // we rely on the fact that there is a separate buffer for buckets, and that each bucket is just a
//  // 16-byte tag.
//  u32 next_bucket = in.tag_addr + 16;
//  std::vector<std::vector<u64>> flattened_buckets;
//  std::vector<u64> current_bucket_data;
//  DmaReadState state = in;
//
//  while (!state.ended) {
//    if (state.tag_addr == next_bucket) {
//      flattened_buckets.push_back(current_bucket_data);
//      current_bucket_data.clear();
//      next_bucket += 16;
//    }
//    current_bucket_data.push_back(0);  // tag transfer padding
//    auto read_result = next_dma(&state);
//    current_bucket_data.push_back(read_result.transferred_tag);
//    for (u32 i = 0; i < 2 * read_result.qwc; i++) {
//      current_bucket_data.push_back(state.read<u64>(read_result.addr + 8 * i));
//    }
//  }
//  return flattened_buckets;
//}
//
//std::vector<PerBucketInfo> find_buckets(const DmaReadState& in) {
//  // we rely on the fact that there is a separate buffer for buckets, and that each bucket is just a
//  // 16-byte tag.
//  u32 next_bucket = in.tag_addr + 16;
//  std::vector<PerBucketInfo> buckets;
//  PerBucketInfo current_bucket_data;
//  current_bucket_data.start_addr = in.tag_addr;
//  current_bucket_data.end_addr = in.tag_addr + 16;
//  DmaReadState state = in;
//
//  while (!state.ended) {
//    if (state.tag_addr == next_bucket) {
//      buckets.push_back(current_bucket_data);
//      current_bucket_data = PerBucketInfo();
//      current_bucket_data.start_addr = state.tag_addr;
//      current_bucket_data.end_addr = state.tag_addr + 16;
//      next_bucket += 16;
//    }
//
//    auto read_result = next_dma(&state);
//    current_bucket_data.tag_count++;
//    current_bucket_data.qwc += read_result.qwc;
//  }
//  return buckets;
//}