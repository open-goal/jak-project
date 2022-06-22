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

#include "test.h"
#include <assert.h>
#include <list>
#include <vector>
#include <algorithm>

#include "../cpp-btree/btree_map.h"

extern "C" {
uint32_t xd3_large32_cksum_old (xd3_hash_cfg *cfg, const uint8_t *base, const usize_t look);
uint32_t xd3_large32_cksum_update_old (xd3_hash_cfg *cfg, uint32_t cksum, 
				       const uint8_t *base, const usize_t look);

uint64_t xd3_large64_cksum_old (xd3_hash_cfg *cfg, const uint8_t *base, const usize_t look);
uint64_t xd3_large64_cksum_update_old (xd3_hash_cfg *cfg, uint64_t cksum, 
				       const uint8_t *base, const usize_t look);
}

using btree::btree_map;
using std::list;
using std::vector;

// MLCG parameters
// a, a*
uint32_t good_32bit_values[] = {
  1597334677U, // ...
  741103597U, 887987685U,
};

// a, a*
uint64_t good_64bit_values[] = {
  1181783497276652981ULL, 4292484099903637661ULL,
  7664345821815920749ULL, // ...
};

void print_header() {
  static int hdr_cnt = 0;
  if (hdr_cnt++ % 20 == 0) {
    printf("%-32sConf\t\tCount\tUniq\tFull\tCover\tColls"
	   "\tMB/s\tIters\t#Colls\n", "Name");
  }
}

struct true_type { };
struct false_type { };

template <typename Word>
usize_t bitsof();

template<>
usize_t bitsof<unsigned int>() {
  return sizeof(unsigned int) * 8;
}

template<>
usize_t bitsof<unsigned long>() {
  return sizeof(unsigned long) * 8;
}

template<>
usize_t bitsof<unsigned long long>() {
  return sizeof(unsigned long long) * 8;
}

template <typename Word>
struct hhash {  // shift "s" bits leaving the high bits as a hash value for
		// this checksum, which are the most "distant" in terms of the
		// spectral test for the rabin_karp MLCG.  For short windows,
		// the high bits aren't enough, XOR "mask" worth of these in.
  Word operator()(const Word t, const Word s, const Word mask) {
    return (t >> s) ^ (t & mask);
  }
};

template <typename Word>
Word good_word();

template<>
uint32_t good_word<uint32_t>() {
  return good_32bit_values[0];
}

template<>
uint64_t good_word<uint64_t>() {
  return good_64bit_values[0];
}

// CLASSES

#define SELF Word, CksumSize, CksumSkip, Hash, Compaction
#define MEMBER template <typename Word,		\
			 int CksumSize,		\
			 int CksumSkip,		\
			 typename Hash,		\
                         int Compaction>

MEMBER
struct cksum_params {
  typedef Word word_type;
  typedef Hash hash_type;

  static const int cksum_size = CksumSize;
  static const int cksum_skip = CksumSkip;
  static const int compaction = Compaction;
};

MEMBER
struct rabin_karp : public cksum_params<SELF> {
  // (a^cksum_size-1 c_0) + (a^cksum_size-2 c_1) ...
  rabin_karp()
    : powers(make_powers()),
      product(powers[0] * good_word<Word>()),
      incr_state(0) { }

  static Word* make_powers() {
    Word *p = new Word[CksumSize];
    p[CksumSize - 1] = 1;
    for (int i = CksumSize - 2; i >= 0; i--) {
      p[i] = p[i + 1] * good_word<Word>();
    }
    return p;
  }

  ~rabin_karp() {
    delete [] powers;
  }

  Word step(const uint8_t *ptr) {
    Word h = 0;
    for (int i = 0; i < CksumSize; i++) {
      h += (ptr[i]) * powers[i];
    }
    return h;
  }

  Word state0(const uint8_t *ptr) {
    incr_state = step(ptr);
    return incr_state;
  }

  Word incr(const uint8_t *ptr) {
    incr_state = good_word<Word>() * incr_state -
      product * (ptr[-1]) + (ptr[CksumSize - 1]);
    return incr_state;
  }

  const Word *const powers;
  const Word  product;
  Word        incr_state;
};

MEMBER
struct with_stream : public cksum_params<SELF> {
  xd3_stream stream;

  with_stream()
  {
    xd3_config cfg;
    memset (&stream, 0, sizeof (stream));
    xd3_init_config (&cfg, 0);
    cfg.smatch_cfg = XD3_SMATCH_SOFT;
    cfg.smatcher_soft.large_look = CksumSize;
    cfg.smatcher_soft.large_step = CksumSkip;
    cfg.smatcher_soft.small_look = 4;
    cfg.smatcher_soft.small_chain = 4;
    cfg.smatcher_soft.small_lchain = 4;
    cfg.smatcher_soft.max_lazy = 4;
    cfg.smatcher_soft.long_enough = 4;
    CHECK_EQ(0, xd3_config_stream (&stream, &cfg));

    CHECK_EQ(0, xd3_size_hashtable (&stream,
				    1<<10 /* ignored */,
				    stream.smatcher.large_look,
				    & stream.large_hash));
  }
  ~with_stream() 
  {
    xd3_free_stream (&stream);
  }
};

MEMBER
struct large_cksum : public with_stream<SELF> {
  Word step(const uint8_t *ptr) {
    return xd3_large_cksum (&this->stream.large_hash, ptr, CksumSize);
  }

  Word state0(const uint8_t *ptr) {
    incr_state = step(ptr);
    return incr_state;
  }

  Word incr(const uint8_t *ptr) {
    incr_state = xd3_large_cksum_update (&this->stream.large_hash, 
					 incr_state, ptr - 1, CksumSize);
    return incr_state;
  }

  Word incr_state;
};

#if SIZEOF_USIZE_T == 4
#define xd3_large_cksum_old         xd3_large32_cksum_old
#define xd3_large_cksum_update_old  xd3_large32_cksum_update_old
#elif SIZEOF_USIZE_T == 8
#define xd3_large_cksum_old         xd3_large64_cksum_old
#define xd3_large_cksum_update_old  xd3_large64_cksum_update_old
#endif

MEMBER
struct large_cksum_old : public with_stream<SELF> {
  Word step(const uint8_t *ptr) {
    return xd3_large_cksum_old (&this->stream.large_hash, ptr, CksumSize);
  }

  Word state0(const uint8_t *ptr) {
    incr_state = step(ptr);
    return incr_state;
  }

  Word incr(const uint8_t *ptr) {
    incr_state = xd3_large_cksum_update_old (&this->stream.large_hash, 
					     incr_state, ptr - 1, CksumSize);
    return incr_state;
  }

  Word incr_state;
};

// TESTS

template <typename Word>
struct file_stats {
  typedef const uint8_t* ptr_type;
  typedef Word word_type;
  typedef btree::btree_multimap<word_type, ptr_type> table_type;
  typedef typename table_type::iterator table_iterator;

  usize_t cksum_size;
  usize_t cksum_skip;
  usize_t unique;
  usize_t unique_values;
  usize_t count;
  table_type table;

  file_stats(usize_t size, usize_t skip)
    : cksum_size(size),
      cksum_skip(skip),
      unique(0),
      unique_values(0),
      count(0) {
  }

  void reset() {
    unique = 0;
    unique_values = 0;
    count = 0;
    table.clear();
  }

  void update(word_type word, ptr_type ptr) {
    table_iterator t_i = table.find(word);

    count++;
    if (t_i != table.end()) {
      int collisions = 0;
      for (table_iterator p_i = t_i;
	   p_i != table.end() && p_i->first == word;
	   ++p_i) {
	if (memcmp(p_i->second, ptr, cksum_size) == 0) {
	  return;
	}
	collisions++;
      }
      if (collisions >= 1000) {
	fprintf(stderr, "Something is not right, lots of collisions=%d\n", 
		collisions);
	abort();
      }
    } else {
      unique_values++;
    }
    unique++;
    table.insert(std::make_pair(word, ptr));
    return;
  }

  void freeze() {
    table.clear();
  }
};

struct test_result_base;

static vector<test_result_base*> all_tests;

struct test_result_base {
  virtual ~test_result_base() {
  }
  virtual void reset() = 0;
  virtual void print() = 0;
  virtual void get(const uint8_t* buf, const size_t buf_size, 
		   usize_t iters) = 0;
  virtual void stat() = 0;
  virtual usize_t count() = 0;
  virtual usize_t dups() = 0;
  virtual double uniqueness() = 0;
  virtual double fullness() = 0;
  virtual double collisions() = 0;
  virtual double coverage() = 0;
  virtual double compression() = 0;
  virtual double time() = 0;
  virtual double total_time() = 0;
  virtual usize_t total_count() = 0;
  virtual usize_t total_dups() = 0;
};

template <typename Checksum>
struct test_result : public test_result_base {
  Checksum cksum;
  const char *test_name;
  file_stats<typename Checksum::word_type> fstats;
  usize_t test_size;
  usize_t n_steps;
  usize_t n_incrs;
  typename Checksum::word_type s_bits;
  typename Checksum::word_type s_mask;
  usize_t t_entries;
  usize_t h_bits;
  usize_t h_buckets_full;
  char *hash_table;
  long accum_millis;
  usize_t accum_iters;

  // These are not reset
  double accum_time;
  usize_t accum_count;
  usize_t accum_dups;
  usize_t accum_colls;
  size_t accum_size;

  test_result(const char *name)
    : test_name(name),
      fstats(Checksum::cksum_size, Checksum::cksum_skip),
      hash_table(NULL),
      accum_millis(0),
      accum_iters(0),
      accum_time(0.0),
      accum_count(0),
      accum_dups(0),
      accum_colls(0),
      accum_size(0) {
    all_tests.push_back(this);
  }

  ~test_result() {
    reset();
  }

  void reset() {
    // size of file
    test_size = 0;

    // count
    n_steps = 0;
    n_incrs = 0;

    // four values used by new_table()/summarize_table()
    s_bits = 0;
    s_mask = 0;
    t_entries = 0;
    h_bits = 0;
    h_buckets_full = 0;

    accum_millis = 0;
    accum_iters = 0;

    fstats.reset();

    // temporary
    if (hash_table) {
      delete(hash_table);
      hash_table = NULL;
    }
  }

  usize_t count() {
    if (Checksum::cksum_skip == 1) {
      return n_incrs;
    } else {
      return n_steps;
    }
  }

  usize_t dups() {
    return fstats.count - fstats.unique;
  }

  /* Fraction of distinct strings of length cksum_size which are not
   * represented in the hash table. */
  double collisions() {
    return (fstats.unique - fstats.unique_values) / (double) fstats.unique;
  }
  usize_t colls() {
    return (fstats.unique - fstats.unique_values);
  }

  double uniqueness() {
    return 1.0 - (double) dups() / count();
  }

  double fullness() {
    return (double) h_buckets_full / (1 << h_bits);
  }

  double coverage() {
    return (double) h_buckets_full / uniqueness() / count();
  }

  double compression() {
    return 1.0 - coverage();
  }

  double time() {
    return (double) accum_millis / accum_iters;
  }

  double total_time() {
    return accum_time;
  }

  usize_t total_count() {
    return accum_count;
  }

  usize_t total_dups() {
    return accum_dups;
  }

  usize_t total_colls() {
    return accum_dups;
  }

  void stat() {
    accum_time += time();
    accum_count += count();
    accum_dups += dups();
    accum_colls += colls();
    accum_size += test_size;
  }

  void print() {
    if (fstats.count != count()) {
      fprintf(stderr, "internal error: %" W "d != %" W "d\n", fstats.count, count());
      abort();
    }
    print_header();
    printf("%-32s%d/%d 2^%" W "u\t%" W "u\t%0.4f\t%.4f\t%.4f\t%.1e\t%.2f\t"
	   "%" W "u\t%" W "u\n",
	   test_name,
	   Checksum::cksum_size,
	   Checksum::cksum_skip,
	   h_bits,
	   count(),
	   uniqueness(),
	   fullness(),
	   coverage(),
	   collisions(),
	   0.001 * accum_iters * test_size / accum_millis,
	   accum_iters,
	   colls());
  }

  usize_t size_log2 (usize_t slots) {
    usize_t bits = bitsof<typename Checksum::word_type>() - 1;
    usize_t i;

    for (i = 3; i <= bits; i += 1) {
      if (slots <= (1U << i)) {
	return i - Checksum::compaction;
      }
    }

    return bits;
  }

  void new_table(usize_t entries) {
    t_entries = entries;
    h_bits = size_log2(entries);

    usize_t n = 1 << h_bits;

    s_bits = bitsof<typename Checksum::word_type>() - h_bits;
    s_mask = n - 1U;

    hash_table = new char[n / 8];
    memset(hash_table, 0, n / 8);
  }

  int get_table_bit(usize_t i) {
    return hash_table[i/8] & (1 << i%8);
  }

  int set_table_bit(usize_t i) {
    return hash_table[i/8] |= (1 << i%8);
  }

  void summarize_table() {
    usize_t n = 1 << h_bits;
    usize_t f = 0;
    for (usize_t i = 0; i < n; i++) {
      if (get_table_bit(i)) {
	f++;
      }
    }
    h_buckets_full = f;
  }

  void get(const uint8_t* buf, const size_t buf_size, usize_t test_iters) {
    typename Checksum::hash_type hash;
    const uint8_t *ptr;
    const uint8_t *end;
    usize_t periods;
    int64_t last_offset;
    int64_t stop;

    test_size = buf_size;
    last_offset = buf_size - Checksum::cksum_size;

    if (last_offset < 0) {
      periods = 0;
      n_steps = 0;
      n_incrs = 0;
      stop = -Checksum::cksum_size;
    } else {
      periods = last_offset / Checksum::cksum_skip;
      n_steps = periods + 1;
      n_incrs = last_offset + 1;
      stop = last_offset - (periods + 1) * Checksum::cksum_skip;
    }

    // Compute file stats once.
    if (fstats.unique_values == 0) {
      if (Checksum::cksum_skip == 1) {
	for (size_t i = 0; i <= buf_size - Checksum::cksum_size; i++) {
	  fstats.update(hash(cksum.step(buf + i), s_bits, s_mask), buf + i);
	}
      } else {
	ptr = buf + last_offset;
	end = buf + stop;

	for (; ptr != end; ptr -= Checksum::cksum_skip) {
	  fstats.update(hash(cksum.step(ptr), s_bits, s_mask), ptr);
	}
      }
      fstats.freeze();
    }

    long start_test = get_millisecs_now();

    if (Checksum::cksum_skip != 1) {
      new_table(n_steps);

      for (usize_t i = 0; i < test_iters; i++) {
	ptr = buf + last_offset;
	end = buf + stop;

	for (; ptr != end; ptr -= Checksum::cksum_skip) {
	  set_table_bit(hash(cksum.step(ptr), s_bits, s_mask));
	}
      }

      summarize_table();
    }

    stop = buf_size - Checksum::cksum_size + 1;
    if (stop < 0) {
      stop = 0;
    }

    if (Checksum::cksum_skip == 1) {
      new_table(n_incrs);

      for (usize_t i = 0; i < test_iters; i++) {
	ptr = buf;
	end = buf + stop;

	if (ptr != end) {
	  set_table_bit(hash(cksum.state0(ptr++), s_bits, s_mask));
	}

	for (; ptr != end; ptr++) {
	  typename Checksum::word_type w = cksum.incr(ptr);
	  CHECK_EQ(w, cksum.step(ptr));
	  set_table_bit(hash(w, s_bits, s_mask));
	}
      }

      summarize_table();
    }

    accum_iters += test_iters;
    accum_millis += get_millisecs_now() - start_test;
  }
};

static int read_whole_file(const char *name,
			   uint8_t **buf_ptr,
			   size_t *buf_len) {
  main_file file;
  int ret;
  xoff_t len;
  size_t nread;
  main_file_init(&file);
  file.filename = name;
  ret = main_file_open(&file, name, XO_READ);
  if (ret != 0) {
    fprintf(stderr, "open failed\n");
    goto exit;
  }
  ret = main_file_stat(&file, &len);
  if (ret != 0) {
    fprintf(stderr, "stat failed\n");
    goto exit;
  }
  
  (*buf_len) = (size_t)len;
  (*buf_ptr) = (uint8_t*) main_malloc(*buf_len);
  ret = main_file_read(&file, *buf_ptr, *buf_len, &nread,
		       "read failed");
  if (ret == 0 && *buf_len == nread) {
    ret = 0;
  } else {
    fprintf(stderr, "invalid read\n");
    ret = XD3_INTERNAL;
  }
 exit:
  main_file_cleanup(&file);
  return ret;
}

int main(int argc, char** argv) {
  int i;
  uint8_t *buf = NULL;
  size_t buf_len = 0;
  int ret;

  if (argc <= 1) {
    fprintf(stderr, "usage: %s file ...\n", argv[0]);
    return 1;
  }

// TODO: The xdelta3-hash.h code is identical now; add sameness test.
// using rabin_karp<> template.
#define TEST(T,Z,S,C)					\
  test_result<large_cksum<T,Z,S,hhash<T>,C>>		\
    _xck_ ## T ## _ ## Z ## _ ## S ## _ ## C		\
    ("xck_" #T "_" #Z "_" #S "_" #C);			\
  test_result<large_cksum_old<T,Z,S,hhash<T>,C>>	\
    _old_ ## T ## _ ## Z ## _ ## S ## _ ## C		\
    ("old_" #T "_" #Z "_" #S "_" #C)

#define TESTS(SIZE, SKIP)	 \
  TEST(usize_t, SIZE, SKIP, 1);  \
  TEST(usize_t, SIZE, SKIP, 2)
   
  TESTS(5, 1);
  TESTS(6, 1);
  TESTS(7, 1);
  TESTS(8, 1);
  TESTS(9, 1);
  TESTS(10, 1);
  TESTS(11, 1);
  TESTS(12, 1);
  TESTS(13, 1);
  TESTS(14, 1);
  TESTS(15, 1);
  TESTS(16, 1);
  TESTS(17, 1);
  TESTS(18, 1);
  TESTS(19, 1);
  TESTS(20, 1);
  TESTS(21, 1);
  TESTS(22, 1);
  TESTS(23, 1);
  TESTS(24, 1);
  TESTS(25, 1);
  TESTS(26, 1);
  TESTS(27, 1);
  TESTS(28, 1);
  TESTS(29, 1);
  TESTS(30, 1);
  TESTS(31, 1);
  TESTS(32, 1);
  TESTS(33, 1);
  TESTS(34, 1);
  TESTS(35, 1);
  TESTS(36, 1);
  TESTS(37, 1);
  TESTS(38, 1);
  TESTS(39, 1);


  for (i = 1; i < argc; i++) {
    if ((ret = read_whole_file(argv[i],
			       & buf,
			       & buf_len))) {
      return 1;
    }

    fprintf(stderr, "file %s is %zu bytes\n",
	    argv[i], buf_len);

    double min_time = -1.0;
    double min_compression = 0.0;

    for (vector<test_result_base*>::iterator iter = all_tests.begin();
	 iter != all_tests.end(); ++iter) {
      test_result_base *test = *iter;
      test->reset();

      usize_t iters = 1;
      long start_test = get_millisecs_now();

      do {
	test->get(buf, buf_len, iters);
	iters *= 3;
	iters /= 2;
      } while (get_millisecs_now() - start_test < 2000);

      test->stat();

      if (min_time < 0.0) {
	min_compression = test->compression();
	min_time = test->time();
      }

      if (min_time > test->time()) {
	min_time = test->time();
      }

      if (min_compression > test->compression()) {
	min_compression = test->compression();
      }

      test->print();
    }

    main_free(buf);
    buf = NULL;
  }

  return 0;      
}
