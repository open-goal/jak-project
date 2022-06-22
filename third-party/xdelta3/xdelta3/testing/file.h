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
class Block;
class BlockIterator;
class TmpFile;

class Block {
public:
  Block()
    : data_(NULL),
      data_size_(0),
      size_(0) { }

  ~Block() {
    if (data_) {
      delete [] data_;
    }
  }

  size_t Size() const {
    return size_;
  }

  uint8_t operator[](size_t i) const {
    CHECK_LT(i, size_);
    return data_[i];
  }

  uint8_t* Data() const {
    if (data_ == NULL) {
      CHECK_EQ(0, size_);
      data_size_ = 1;
      data_ = new uint8_t[1];
    }
    return data_;
  }

  // For writing to blocks
  void Append(const uint8_t *data, size_t size) {
    if (data_ == NULL) {
      CHECK_EQ(0, size_);
      CHECK_EQ(0, data_size_);
      data_ = new uint8_t[Constants::BLOCK_SIZE];
      data_size_ = Constants::BLOCK_SIZE;
    }

    if (size_ + size > data_size_) {
      uint8_t *tmp = data_;
      while (size_ + size > data_size_) {
	data_size_ *= 2;
      }
      data_ = new uint8_t[data_size_];
      memcpy(data_, tmp, size_);
      delete [] tmp;
    }

    memcpy(data_ + size_, data, size);
    size_ += size;
  }

  // For cleaing a block
  void Reset() {
    size_ = 0;
  }

  // Note: This does not benefit from -Wformat= checking, due to the
  // enclosing template. Further, it was not used.
  // void Print() const {
  //   xoff_t pos = 0;
  //   for (size_t i = 0; i < Size(); i++) {
  //     if (pos % 16 == 0) {
  // 	DP(RINT "%5" Q "x: ", pos);
  //     }
  //     DP(RINT "%02x ", (*this)[i]);
  //     if (pos % 16 == 15) {
  // 	DP(RINT "\n");
  //     }
  //     pos++;
  //   }
  //   DP(RINT "\n");
  // }

  void WriteTmpFile(TmpFile *f) const {
    f->Append(this);
  }

  void SetSize(size_t size) {
    uint8_t *t = NULL;
    if (data_size_ < size) {
      if (data_) {
	t = data_;
      }
      data_ = new uint8_t[size];
      data_size_ = size;
    }
    if (t && size < size_) {
      memcpy(data_, t, size);
    }
    delete [] t;
    size_ = size;
  }

private:
  friend class BlockIterator;

  mutable uint8_t *data_;
  mutable size_t data_size_;
  size_t size_;
};

class FileSpec {
 public:
  FileSpec(MTRandom *rand)
    : rand_(rand) {
  }

  // Generates a file with a known size
  void GenerateFixedSize(xoff_t size) {
    Reset();

    for (xoff_t p = 0; p < size; ) {
      xoff_t t = min(Constants::BLOCK_SIZE, size - p);
      table_.insert(make_pair(p, Segment(t, rand_)));
      p += t;
    }
  }

  // Generates a file with exponential-random distributed size
  void GenerateRandomSize(xoff_t mean) {
    GenerateFixedSize(rand_->ExpRand(mean));
  }

  // Returns the size of the file
  xoff_t Size() const {
    if (table_.empty()) {
      return 0;
    }
    ConstSegmentMapIterator i = --table_.end();
    return i->first + i->second.Size();
  }

  // Returns the number of blocks
  xoff_t Blocks(size_t blksize = Constants::BLOCK_SIZE) const {
    if (table_.empty()) {
      return 0;
    }
    return ((Size() - 1) / blksize) + 1;
  }

  // Returns the number of segments
  xoff_t Segments() const {
    return table_.size();
  }

  // Create a mutation according to "what".
  void ModifyTo(const Mutator &mutator,
		FileSpec *modify) const {
    modify->Reset();
    mutator.Mutate(&modify->table_, &table_, rand_);
    modify->CheckSegments();
  }

  void CheckSegments() const {
    for (ConstSegmentMapIterator iter(table_.begin());
	 iter != table_.end(); ) {
      ConstSegmentMapIterator iter0(iter++);
      if (iter == table_.end()) {
	break;
      }
      CHECK_EQ(iter0->first + iter0->second.Size(), iter->first);
    }
  }

  void Reset() {
    table_.clear();
  }

  void Print() const {
    for (ConstSegmentMapIterator iter(table_.begin());
	 iter != table_.end();
	 ++iter) {
      const Segment &seg = iter->second;
      cerr << "Segment at " << iter->first
	   << " (" << seg.ToString() << ")" << endl;
    }
  }

  void PrintData() const {
    Block block;
    for (BlockIterator iter(*this); !iter.Done(); iter.Next()) {
      iter.Get(&block);
      block.Print();
    }
  }

  void WriteTmpFile(TmpFile *f) const {
    Block block;
    for (BlockIterator iter(*this); !iter.Done(); iter.Next()) {
      iter.Get(&block);
      f->Append(&block);
    }
  }

  void Get(Block *block, xoff_t offset, size_t size) const {
    size_t got = 0;
    block->SetSize(size);

    ConstSegmentMapIterator pos = table_.upper_bound(offset);
    if (pos == table_.begin()) {
      CHECK_EQ(0, Size());
      return;
    }
    --pos;

    while (got < size) {
      CHECK(pos != table_.end());
      CHECK_GE(offset, pos->first);

      const Segment &seg = pos->second;

      // The position of this segment may start before this block starts,
      // and then the position of the data may be offset from the seeding
      // position.
      size_t seg_offset = offset - pos->first;
      size_t advance = min(seg.Size() - seg_offset,
			   size - got);

      seg.Fill(seg_offset, advance, block->Data() + got);

      got += advance;
      offset += advance;
      ++pos;
    }
  }

  typedef BlockIterator iterator;

 private:
  friend class BlockIterator;

  MTRandom *rand_;
  SegmentMap table_;
};

class BlockIterator {
public:
  explicit BlockIterator(const FileSpec& spec)
    : spec_(spec),
      blkno_(0),
      blksize_(Constants::BLOCK_SIZE) { }

  BlockIterator(const FileSpec& spec,
		size_t blksize)
    : spec_(spec),
      blkno_(0),
      blksize_(blksize) { }

  bool Done() const {
    return blkno_ >= spec_.Blocks(blksize_);
  }

  void Next() {
    blkno_++;
  }

  xoff_t Blkno() const {
    return blkno_;
  }

  xoff_t Blocks() const {
    return spec_.Blocks(blksize_);
  }

  xoff_t Offset() const {
    return blkno_ * blksize_;
  }

  void SetBlock(xoff_t blkno) {
    CHECK_LE(blkno, Blocks());
    blkno_ = blkno;
  }

  void Get(Block *block) const {
    spec_.Get(block, blkno_ * blksize_, BytesOnBlock());
  }

  size_t BytesOnBlock() const {
    xoff_t blocks = spec_.Blocks(blksize_);
    xoff_t size = spec_.Size();

    DCHECK((blkno_ < blocks) ||
	   (blkno_ == blocks && size % blksize_ == 0));

    if (blkno_ == blocks) {
      return 0;
    }
    if (blkno_ + 1 == blocks) {
      return ((size - 1) % blksize_) + 1;
    }
    return blksize_;
  }

  size_t BlockSize() const {
    return blksize_;
  }

private:
  const FileSpec& spec_;
  xoff_t blkno_;
  size_t blksize_;
};

class ExtFile {
public:
  ExtFile() {
    static int static_counter = 0;
    pid_t pid = getpid();
    char buf[64];
    xoff_t xpid = pid;
    snprintf(buf, 64, "/tmp/regtest.%" Q "u.%d", xpid, static_counter++);
    filename_.append(buf);
    unlink(filename_.c_str());
  }

  ~ExtFile() {
    unlink(filename_.c_str());
  }

  const char* Name() const {
    return filename_.c_str();
  }

  // Check whether a real file matches a file spec.
  bool EqualsSpec(const FileSpec &spec) const {
    main_file t;
    main_file_init(&t);
    CHECK_EQ(0, main_file_open(&t, Name(), XO_READ));

    Block tblock;
    Block sblock;
    for (BlockIterator iter(spec); !iter.Done(); iter.Next()) {
      iter.Get(&sblock);
      tblock.SetSize(sblock.Size());
      size_t tread;
      CHECK_EQ(0, main_file_read(&t,
				 tblock.Data(),
				 tblock.Size(), &tread, "read failed"));
      CHECK_EQ(0, CmpDifferentBlockBytes(tblock, sblock));
    }

    CHECK_EQ(0, main_file_close(&t));
    main_file_cleanup(&t);
    return true;
  }

protected:
  string filename_;
};

class TmpFile : public ExtFile {
public:
  TmpFile() {
    main_file_init(&file_);
    CHECK_EQ(0, main_file_open(&file_, Name(), XO_WRITE));
  }

  ~TmpFile() {
    main_file_cleanup(&file_);
  }

  void Append(const Block *block) {
    CHECK_EQ(0, main_file_write(&file_,
				block->Data(), block->Size(),
				"tmpfile write failed"));
  }

  const char* Name() const {
    if (main_file_isopen(&file_)) {
      CHECK_EQ(0, main_file_close(&file_));
    }
    return ExtFile::Name();
  }

private:
  mutable main_file file_;
};
