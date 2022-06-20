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

template <typename T, typename U>
class SizeIterator {
 public:
  SizeIterator(MTRandom *rand, size_t howmany)
    : rand_(rand),
      count_(0),
      fixed_(U::sizes),
      fixed_size_(SIZEOF_ARRAY(U::sizes)),
      howmany_(howmany) { }

  T Get() {
    if (count_ < fixed_size_) {
      return fixed_[count_];
    }
    return rand_->Rand<T>() % U::max_value;
  }

  bool Done() {
    return count_ >= fixed_size_ && count_ >= howmany_;
  }

  void Next() {
    count_++;
  }

 private:
  MTRandom *rand_;
  size_t count_;
  T* fixed_;
  size_t fixed_size_;
  size_t howmany_;
};

// Small sizes
class SmallSizes {
public:
  static size_t sizes[];
  static size_t max_value;
};

size_t SmallSizes::sizes[] = {
  0, 1, 128 / 4, 3333, 
  128 - (128 / 3),
  128,
  128 + (128 / 3),
  2 * 128 - (128 / 3),
  2 * 128,
  2 * 128 + (128 / 3),
};

size_t SmallSizes::max_value = 128 * 3;

// Large sizes
class LargeSizes {
public:
  static size_t sizes[];
  static size_t max_value;
};

size_t LargeSizes::sizes[] = {
  1 << 20,
  1 << 18,
  1 << 16,
};

size_t LargeSizes::max_value = 1<<20;

// Base constants
struct BaseConstants {
  static const size_t TEST_ROUNDS;
};

const size_t BaseConstants::TEST_ROUNDS = 10;

// Regtest<> arguments
struct SmallBlock : public BaseConstants {
  static const xoff_t BLOCK_SIZE;
  static const size_t WINDOW_SIZE;
  typedef SmallSizes Sizes;
};

const xoff_t SmallBlock::BLOCK_SIZE = 1<<7;
const size_t SmallBlock::WINDOW_SIZE = 1<<7;

struct LargeBlock : public BaseConstants {
  static const xoff_t BLOCK_SIZE;
  static const size_t WINDOW_SIZE;
  typedef LargeSizes Sizes;
};

const xoff_t LargeBlock::BLOCK_SIZE = (1 << 13);
const size_t LargeBlock::WINDOW_SIZE = (1 << 13);

struct MixedBlock : public BaseConstants {
  static const xoff_t BLOCK_SIZE;
  static const size_t WINDOW_SIZE;
  typedef SmallSizes Sizes;
};

const xoff_t MixedBlock::BLOCK_SIZE = 1<<7;
const size_t MixedBlock::WINDOW_SIZE = 1<<8;

struct OversizeBlock : public BaseConstants {
  static const xoff_t BLOCK_SIZE;
  static const size_t WINDOW_SIZE;
  typedef SmallSizes Sizes;
};

const xoff_t OversizeBlock::BLOCK_SIZE = 1<<8;
const size_t OversizeBlock::WINDOW_SIZE = 1<<7;
