//#pragma once
//
//#include <string>
//
//#include <cstring>
//
//#include "common/common_types.h"
//
///*!
// * @file dma_reader.h
// * This file contains utilities for reading and printing DMA data.
// */
//
//struct DmaData {
//  u32 addr = 0;
//  u32 qwc = 0;
//  u64 transferred_tag = 0;
//};
//
//DmaData next_dma(DmaReadState* state);
//
//std::vector<u64> flatten_dma(const DmaReadState& in);
//std::vector<std::vector<u64>> flatten_dma_per_bucket(const DmaReadState& in);
//void debug_print_dma(const DmaReadState& in);
//
//struct PerBucketInfo {
//  u32 start_addr = 0;
//  u32 end_addr = 0;
//  u32 qwc = 0;
//  u32 tag_count = 0;
//};
//
///*!
// * Note:
// *  this finds 71 buckets, even though there are only 69.
// *  There's a setup before the buckets and an end after the buckets.
// */
//std::vector<PerBucketInfo> find_buckets(const DmaReadState& in);