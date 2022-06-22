/* xdelta3 - delta compression tools and library -*- Mode: C++ -*-
   Copyright 2016 Joshua MacDonald

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

class Segment {
 public:
  Segment(size_t size, MTRandom *rand)
    : size_(size),
      seed_(rand->Rand32()),
      seed_offset_(0),
      data_(NULL) {
    CHECK_GT(size_, 0);
  }

  Segment(size_t size, uint32_t seed)
    : size_(size),
      seed_(seed),
      seed_offset_(0),
      data_(NULL) {
    CHECK_GT(size_, 0);
  }

  Segment(size_t size, uint8_t *data)
    : size_(size),
      seed_(0),
      seed_offset_(0),
      data_(data) {
    CHECK_GT(size_, 0);
  }

  size_t Size() const {
    return size_;
  }

  Segment Subseg(size_t start, size_t size) const {
    CHECK_LE(start + size, size_);
    if (data_) {
      return Segment(size, data_ + start);
    } else {
      return Segment(size, seed_, seed_offset_ + start);
    }
  }

  void Fill(size_t seg_offset, size_t size, uint8_t *data) const {
    CHECK_LE(seg_offset + size, size_);
    if (data_) {
      memcpy(data, data_ + seg_offset, size);
    } else {
      size_t skip = seg_offset + seed_offset_;
      MTRandom gen(seed_);
      MTRandom8 gen8(&gen);
      while (skip--) {
	gen8.Rand8();
      }
      for (size_t i = 0; i < size; i++) {
	data[i] = gen8.Rand8();
      }
    }
  }

  string ToString() const {
    string r;
    if (data_) {
      for (size_t i = 0; i < size_; i++) {
	char buf[10];
	sprintf(buf, "%02x ", data_[i]);
	r.append(buf);
      }
    } else {
      char buf[256];
      sprintf(buf, "size=%ld,seed=%ud,skip=%ld", size_, seed_, seed_offset_);
      r.append(buf);
    }
    return r;
  }

private:
  // Used by Subseg()
  Segment(size_t size, uint32_t seed, size_t seed_offset)
    : size_(size),
      seed_(seed),
      seed_offset_(seed_offset),
      data_(NULL) {
    CHECK_GT(size_, 0);
  }

  size_t size_;  // Size of this segment

  // For random segments
  uint32_t seed_;  // Seed used for generating byte sequence
  size_t seed_offset_;  // Seed positions the sequence this many bytes
                        // before its beginning.

  // For literal segments (data is not owned)
  uint8_t *data_;
};

typedef map<xoff_t, Segment> SegmentMap;
typedef typename SegmentMap::const_iterator ConstSegmentMapIterator;
typedef typename SegmentMap::iterator SegmentMapIterator;
