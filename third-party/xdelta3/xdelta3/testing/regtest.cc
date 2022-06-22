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
#include "random.h"
#include "sizes.h"

template <typename Constants>
class Regtest {
public:
  typedef typename Constants::Sizes Sizes;

  struct Options {
    Options()
      : encode_srcwin_maxsz(1<<20),
	block_size(Constants::BLOCK_SIZE),
	window_size(Constants::WINDOW_SIZE),
	size_known(false),
	iopt_size(XD3_DEFAULT_IOPT_SIZE),
	smatch_cfg(XD3_SMATCH_DEFAULT) { }

    xoff_t encode_srcwin_maxsz;
    size_t block_size;
    xoff_t window_size;
    bool size_known;
    usize_t iopt_size;
    xd3_smatch_cfg smatch_cfg;
  };

#include "segment.h"
#include "modify.h"
#include "file.h"
#include "cmp.h"
#include "delta.h"

  void InMemoryEncodeDecode(const FileSpec &source_file,
			    const FileSpec &target_file,
			    Block *coded_data,
			    const Options &options) {
    xd3_stream encode_stream;
    xd3_config encode_config;
    xd3_source encode_source;

    xd3_stream decode_stream;
    xd3_config decode_config;
    xd3_source decode_source;
    xoff_t verified_bytes = 0;
    xoff_t encoded_bytes = 0;

    if (coded_data) {
      coded_data->Reset();
    }

    memset(&encode_stream, 0, sizeof (encode_stream));
    memset(&encode_source, 0, sizeof (encode_source));

    memset(&decode_stream, 0, sizeof (decode_stream));
    memset(&decode_source, 0, sizeof (decode_source));

    xd3_init_config(&encode_config, XD3_ADLER32);
    xd3_init_config(&decode_config, XD3_ADLER32);

    encode_config.winsize = options.window_size;
    encode_config.iopt_size = options.iopt_size;
    encode_config.smatch_cfg = options.smatch_cfg;

    CHECK_EQ(0, xd3_config_stream (&encode_stream, &encode_config));
    CHECK_EQ(0, xd3_config_stream (&decode_stream, &decode_config));

    encode_source.blksize = options.block_size;
    decode_source.blksize = options.block_size;

    encode_source.max_winsize = options.encode_srcwin_maxsz;
    decode_source.max_winsize = options.encode_srcwin_maxsz;

    if (!options.size_known)
      {
	xd3_set_source (&encode_stream, &encode_source);
	xd3_set_source (&decode_stream, &decode_source);
      }
    else
      {
	xd3_set_source_and_size (&encode_stream, &encode_source,
				 source_file.Size());
	xd3_set_source_and_size (&decode_stream, &decode_source,
				 source_file.Size());
      }

    BlockIterator source_iterator(source_file, options.block_size);
    BlockIterator target_iterator(target_file, Constants::WINDOW_SIZE);
    Block encode_source_block, decode_source_block;
    Block decoded_block, target_block;
    bool encoding = true;
    bool done = false;
    bool done_after_input = false;

    IF_DEBUG1 (XPR(NTR "source %" Q "u[%" Z "u] target %" Q "u winsize %" Z "u\n",
		   source_file.Size(), options.block_size,
		   target_file.Size(),
		   Constants::WINDOW_SIZE));

    while (!done) {
      target_iterator.Get(&target_block);

      xoff_t blks = target_iterator.Blocks();

      IF_DEBUG2(XPR(NTR "target in %s: %" Q "u[%" Z "u] %" Q "u(%" Q "u) "
		    "verified %" Q "u\n",
		    encoding ? "encoding" : "decoding",
		    target_iterator.Offset(),
		    target_block.Size(),
		    target_iterator.Blkno(),
		    blks,
		    verified_bytes));

      if (blks == 0 || target_iterator.Blkno() == (blks - 1)) {
	xd3_set_flags(&encode_stream, XD3_FLUSH | encode_stream.flags);
      }

      xd3_avail_input(&encode_stream, target_block.Data(), target_block.Size());
      encoded_bytes += target_block.Size();

    process:
      int ret;
      const char *msg;
      if (encoding) {
	ret = xd3_encode_input(&encode_stream);
	msg = encode_stream.msg;
      } else {
	ret = xd3_decode_input(&decode_stream);
	msg = decode_stream.msg;
      }
      (void) msg;

      switch (ret) {
      case XD3_OUTPUT:
	if (encoding) {
	  if (coded_data != NULL) {
	    // Optional encoded-output to the caller
	    coded_data->Append(encode_stream.next_out,
			       encode_stream.avail_out);
	  }
	  // Feed this data to the decoder.
	  xd3_avail_input(&decode_stream,
			  encode_stream.next_out,
			  encode_stream.avail_out);
	  xd3_consume_output(&encode_stream);
	  encoding = false;
	} else {
	  decoded_block.Append(decode_stream.next_out,
			       decode_stream.avail_out);
	  xd3_consume_output(&decode_stream);
	}
	goto process;

      case XD3_GETSRCBLK: {
	xd3_source *src = (encoding ? &encode_source : &decode_source);
	Block *block = (encoding ? &encode_source_block : &decode_source_block);
	if (encoding) {
	  IF_DEBUG2(XPR(NTR "[srcblock] %" Q "u last srcpos %" Q "u "
			"encodepos %" Q "u\n",
			encode_source.getblkno,
			encode_stream.match_last_srcpos,
			encode_stream.input_position + encode_stream.total_in));
	}

	source_iterator.SetBlock(src->getblkno);
	source_iterator.Get(block);
	src->curblkno = src->getblkno;
	src->onblk = block->Size();
	src->curblk = block->Data();

	goto process;
      }

      case XD3_INPUT:
	if (!encoding) {
	  encoding = true;
	  goto process;
	} else {
	  if (done_after_input) {
	    done = true;
	    continue;
	  }

	  if (target_block.Size() < target_iterator.BlockSize()) {
	    encoding = false;
	  } else {
	    target_iterator.Next();
	  }
	  continue;
	}

      case XD3_WINFINISH:
	if (encoding) {
	  if (encode_stream.flags & XD3_FLUSH) {
	    done_after_input = true;
	  }
	  encoding = false;
	} else {
	 CHECK_EQ(0, CmpDifferentBlockBytesAtOffset(decoded_block,
						    target_file,
						    verified_bytes));
	 verified_bytes += decoded_block.Size();
	 decoded_block.Reset();
	 encoding = true;
       }
       goto process;

     case XD3_WINSTART:
     case XD3_GOTHEADER:
       goto process;

     default:
       XPR(NTR "%s = %s %s\n", encoding ? "E " : " D",
	   xd3_strerror(ret),
	   msg == NULL ? "" : msg);

       CHECK_EQ(0, ret);
       CHECK_EQ(-1, ret);
     }
   }

   CHECK_EQ(target_file.Size(), encoded_bytes);
   CHECK_EQ(target_file.Size(), verified_bytes);
   CHECK_EQ(0, xd3_close_stream(&decode_stream));
   CHECK_EQ(0, xd3_close_stream(&encode_stream));
   xd3_free_stream(&encode_stream);
   xd3_free_stream(&decode_stream);
 }

  void MainEncodeDecode(const TmpFile &source_file,
			const TmpFile &target_file,
			ExtFile *coded_data,
			const Options &options) {
    vector<const char*> ecmd;
    char bbuf[16];
    snprintf(bbuf, sizeof(bbuf), "-B%" Q "u", options.encode_srcwin_maxsz);
    ecmd.push_back("xdelta3");
    ecmd.push_back(bbuf);
    ecmd.push_back("-s");
    ecmd.push_back(source_file.Name());
    ecmd.push_back(target_file.Name());
    ecmd.push_back(coded_data->Name());
    ecmd.push_back(NULL);

    CHECK_EQ(0, xd3_main_cmdline(ecmd.size() - 1,
				 const_cast<char**>(&ecmd[0])));

    vector<const char*> dcmd;
    ExtFile recon_file;
    dcmd.push_back("xdelta3");
    ecmd.push_back(bbuf);
    dcmd.push_back("-d");
    dcmd.push_back("-s");
    dcmd.push_back(source_file.Name());
    dcmd.push_back(coded_data->Name());
    dcmd.push_back(recon_file.Name());
    dcmd.push_back(NULL);

    CHECK_EQ(0, xd3_main_cmdline(dcmd.size() - 1,
				 const_cast<char**>(&dcmd[0])));

    CHECK_EQ(0, test_compare_files(recon_file.Name(),
				   target_file.Name()));
  }

  // Similar to xd3_process_memory, with support for test Options.
  // Exercises xd3_process_stream.
  int TestProcessMemory (int            is_encode,
			 int          (*func) (xd3_stream *),
			 const uint8_t *input,
			 usize_t        input_size,
			 const uint8_t *source,
			 usize_t        source_size,
			 uint8_t       *output,
			 usize_t       *output_size,
			 usize_t        output_size_max,
			 const Options &options) {
    xd3_stream stream;
    xd3_config config;
    xd3_source src;
    int ret;

    memset (& stream, 0, sizeof (stream));
    memset (& config, 0, sizeof (config));

    if (is_encode)
      {
	config.winsize = input_size;
	config.iopt_size = options.iopt_size;
	config.sprevsz = xd3_pow2_roundup (config.winsize);
      }

    if ((ret = xd3_config_stream (&stream, &config)) != 0)
      {
	goto exit;
      }

    if (source != NULL)
      {
	memset (& src, 0, sizeof (src));

	src.blksize = source_size;
	src.onblk = source_size;
	src.curblk = source;
	src.curblkno = 0;
	src.max_winsize = source_size;

	if ((ret = xd3_set_source_and_size (&stream, &src, source_size)) != 0)
	  {
	    goto exit;
	  }
      }

    if ((ret = xd3_process_stream (is_encode,
				   & stream,
				   func, 1,
				   input, input_size,
				   output,
				   output_size,
				   output_size_max)) != 0)
      {
	goto exit;
      }

  exit:
    if (ret != 0)
      {
	IF_DEBUG2 (DP(RINT "test_process_memory: %d: %s\n", ret, stream.msg));
      }
    xd3_free_stream(&stream);
    return ret;
  }

  void EncodeDecodeAPI(const FileSpec &spec0, const FileSpec &spec1, 
		       Block *delta, const Options &options) {
    Block from;
    Block to;
    spec0.Get(&from, 0, spec0.Size());
    spec1.Get(&to, 0, spec1.Size());

    delta->SetSize(to.Size() * 1.5);
    usize_t out_size;
    int enc_ret = TestProcessMemory(true,
				    &xd3_encode_input,
				    to.Data(),
				    to.Size(),
				    from.Data(),
				    from.Size(),
				    delta->Data(),
				    &out_size,
				    delta->Size(),
				    options);
    CHECK_EQ(0, enc_ret);
    delta->SetSize(out_size);

    Block recon;
    recon.SetSize(to.Size());
    usize_t recon_size;
    int dec_ret = xd3_decode_memory(delta->Data(),
				    delta->Size(),
				    from.Data(),
				    from.Size(),
				    recon.Data(),
				    &recon_size,
				    recon.Size(),
				    0);
    CHECK_EQ(0, dec_ret);
    CHECK_EQ(0, CmpDifferentBlockBytes(to, recon));
  }

//////////////////////////////////////////////////////////////////////

void TestPrintf() {
  char buf[64];
  xoff_t x = XOFF_T_MAX;
  snprintf_func (buf, sizeof(buf), "%" Q "u", x);
  const char *expect = XD3_USE_LARGEFILE64 ?
    "18446744073709551615" : "4294967295";
  XD3_ASSERT(strcmp (buf, expect) == 0);
}

void TestRandomNumbers() {
  MTRandom rand;
  int rounds = 1<<20;
  uint64_t usum = 0;
  uint64_t esum = 0;

  for (int i = 0; i < rounds; i++) {
    usum += rand.Rand32();
    esum += rand.ExpRand32(1024);
  }

  double allowed_error = 0.01;

  uint32_t umean = usum / rounds;
  uint32_t emean = esum / rounds;

  uint32_t uexpect = UINT32_MAX / 2;
  uint32_t eexpect = 1024;

  if (umean < uexpect * (1.0 - allowed_error) ||
      umean > uexpect * (1.0 + allowed_error)) {
    XPR(NT "uniform mean error: %u != %u\n", umean, uexpect);
    abort();
  }

  if (emean < eexpect * (1.0 - allowed_error) ||
      emean > eexpect * (1.0 + allowed_error)) {
    XPR(NT "exponential mean error: %u != %u\n", emean, eexpect);
    abort();
  }
}

void TestRandomFile() {
  MTRandom rand1;
  FileSpec spec1(&rand1);
  BlockIterator bi(spec1);

  spec1.GenerateFixedSize(0);
  CHECK_EQ(0, spec1.Size());
  CHECK_EQ(0, spec1.Segments());
  CHECK_EQ(0, spec1.Blocks());
  bi.SetBlock(0);
  CHECK_EQ(0, bi.BytesOnBlock());

  spec1.GenerateFixedSize(1);
  CHECK_EQ(1, spec1.Size());
  CHECK_EQ(1, spec1.Segments());
  CHECK_EQ(1, spec1.Blocks());
  bi.SetBlock(0);
  CHECK_EQ(1, bi.BytesOnBlock());

  spec1.GenerateFixedSize(Constants::BLOCK_SIZE);
  CHECK_EQ(Constants::BLOCK_SIZE, spec1.Size());
  CHECK_EQ(1, spec1.Segments());
  CHECK_EQ(1, spec1.Blocks());
  bi.SetBlock(0);
  CHECK_EQ(Constants::BLOCK_SIZE, bi.BytesOnBlock());
  bi.SetBlock(1);
  CHECK_EQ(0, bi.BytesOnBlock());

  spec1.GenerateFixedSize(Constants::BLOCK_SIZE + 1);
  CHECK_EQ(Constants::BLOCK_SIZE + 1, spec1.Size());
  CHECK_EQ(2, spec1.Segments());
  CHECK_EQ(2, spec1.Blocks());
  bi.SetBlock(0);
  CHECK_EQ(Constants::BLOCK_SIZE, bi.BytesOnBlock());
  bi.SetBlock(1);
  CHECK_EQ(1, bi.BytesOnBlock());

  spec1.GenerateFixedSize(Constants::BLOCK_SIZE * 2);
  CHECK_EQ(Constants::BLOCK_SIZE * 2, spec1.Size());
  CHECK_EQ(2, spec1.Segments());
  CHECK_EQ(2, spec1.Blocks());
  bi.SetBlock(0);
  CHECK_EQ(Constants::BLOCK_SIZE, bi.BytesOnBlock());
  bi.SetBlock(1);
  CHECK_EQ(Constants::BLOCK_SIZE, bi.BytesOnBlock());
}

void TestFirstByte() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);

  spec0.GenerateFixedSize(0);
  spec1.GenerateFixedSize(1);
  CHECK_EQ(0, CmpDifferentBytes(spec0, spec0));
  CHECK_EQ(0, CmpDifferentBytes(spec1, spec1));
  CHECK_EQ(1, CmpDifferentBytes(spec0, spec1));
  CHECK_EQ(1, CmpDifferentBytes(spec1, spec0));

  spec0.GenerateFixedSize(1);
  spec0.ModifyTo(Modify1stByte(), &spec1);
  CHECK_EQ(1, CmpDifferentBytes(spec0, spec1));

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE + 1);
  spec0.ModifyTo(Modify1stByte(), &spec1);
  CHECK_EQ(1, CmpDifferentBytes(spec0, spec1));

  SizeIterator<size_t, Sizes> si(&rand, Constants::TEST_ROUNDS);

  for (; !si.Done(); si.Next()) {
    size_t size = si.Get();
    if (size == 0) {
      continue;
    }
    spec0.GenerateFixedSize(size);
    spec0.ModifyTo(Modify1stByte(), &spec1);
    InMemoryEncodeDecode(spec0, spec1, NULL, Options());
  }
}

void TestModifyMutator() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE * 3);

  struct {
    size_t size;
    size_t addr;
  } test_cases[] = {
    { Constants::BLOCK_SIZE, 0 },
    { Constants::BLOCK_SIZE / 2, 1 },
    { Constants::BLOCK_SIZE, 1 },
    { Constants::BLOCK_SIZE * 2, 1 },
  };

  for (size_t i = 0; i < SIZEOF_ARRAY(test_cases); i++) {
    ChangeList cl1;
    cl1.push_back(Change(Change::MODIFY, test_cases[i].size,
			 test_cases[i].addr));
    spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
    CHECK_EQ(spec0.Size(), spec1.Size());

    size_t diff = CmpDifferentBytes(spec0, spec1);
    CHECK_LE(diff, test_cases[i].size);

    // There is a 1/256 probability of the changed byte matching the
    // original value.  The following allows double the probability to
    // pass.
    CHECK_GE(diff, test_cases[i].size - (2 * test_cases[i].size / 256));

    InMemoryEncodeDecode(spec0, spec1, NULL, Options());
  }
}

void TestAddMutator() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE * 2);
  // TODO: fix this test (for all block sizes)!  it's broken because
  // the same byte could be added?

  struct {
    size_t size;
    size_t addr;
    size_t expected_adds;
  } test_cases[] = {
    { 1, 0,                         2 /* 1st byte, last byte (short block) */ },
    { 1, 1,                         3 /* 1st 2 bytes, last byte */ },
    { 1, Constants::BLOCK_SIZE - 1, 2 /* changed, last */ },
    { 1, Constants::BLOCK_SIZE,     2 /* changed, last */ },
    { 1, Constants::BLOCK_SIZE + 1, 3 /* changed + 1st of 2nd block, last */ },
    { 1, 2 * Constants::BLOCK_SIZE, 1 /* last byte */ },
  };

  for (size_t i = 0; i < SIZEOF_ARRAY(test_cases); i++) {
    ChangeList cl1;
    cl1.push_back(Change(Change::ADD, test_cases[i].size, test_cases[i].addr));
    spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
    CHECK_EQ(spec0.Size() + test_cases[i].size, spec1.Size());

    Block coded;
    InMemoryEncodeDecode(spec0, spec1, &coded, Options());

    Delta delta(coded);
    CHECK_EQ(test_cases[i].expected_adds,
	     delta.AddedBytes());
  }
}

void TestDeleteMutator() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE * 4);

  struct {
    size_t size;
    size_t addr;
  } test_cases[] = {
    // Note: an entry { Constants::BLOCK_SIZE, 0 },
    // does not work because the xd3_srcwin_move_point logic won't
    // find a copy if it occurs >= double its size into the file.
    { Constants::BLOCK_SIZE / 2, 0 },
    { Constants::BLOCK_SIZE / 2, Constants::BLOCK_SIZE / 2 },
    { Constants::BLOCK_SIZE, Constants::BLOCK_SIZE / 2 },
    { Constants::BLOCK_SIZE * 2, Constants::BLOCK_SIZE * 3 / 2 },
    { Constants::BLOCK_SIZE, Constants::BLOCK_SIZE * 2 },
  };

  for (size_t i = 0; i < SIZEOF_ARRAY(test_cases); i++) {
    ChangeList cl1;
    cl1.push_back(Change(Change::DELRANGE, test_cases[i].size,
			 test_cases[i].addr));
    spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
    CHECK_EQ(spec0.Size() - test_cases[i].size, spec1.Size());

    Block coded;
    InMemoryEncodeDecode(spec0, spec1, &coded, Options());

    Delta delta(coded);
    CHECK_EQ(0, delta.AddedBytes());
  }
}

void TestCopyMutator() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE * 3);

  struct {
    size_t size;
    size_t from;
    size_t to;
  } test_cases[] = {
    // Copy is difficult to write tests for because where Xdelta finds
    // copies, it does not enter checksums.  So these tests copy data from
    // later to earlier so that checksumming will start.
    { Constants::BLOCK_SIZE / 2, Constants::BLOCK_SIZE / 2, 0 },
    { Constants::BLOCK_SIZE, 2 * Constants::BLOCK_SIZE,
      Constants::BLOCK_SIZE, },
  };

  for (size_t i = 0; i < SIZEOF_ARRAY(test_cases); i++) {
    ChangeList cl1;
    cl1.push_back(Change(Change::COPY, test_cases[i].size,
			 test_cases[i].from, test_cases[i].to));
    spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
    CHECK_EQ(spec0.Size() + test_cases[i].size, spec1.Size());

    Block coded;
    InMemoryEncodeDecode(spec0, spec1, &coded, Options());

    Delta delta(coded);
    CHECK_EQ(0, delta.AddedBytes());
  }
}

void TestMoveMutator() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE * 3);

  struct {
    size_t size;
    size_t from;
    size_t to;
  } test_cases[] = {
    // This is easier to test than Copy but has the same trouble as Delete.
    { Constants::BLOCK_SIZE / 2, Constants::BLOCK_SIZE / 2, 0 },
    { Constants::BLOCK_SIZE / 2, 0, Constants::BLOCK_SIZE / 2 },
    { Constants::BLOCK_SIZE, Constants::BLOCK_SIZE, 2 *
      Constants::BLOCK_SIZE },
    { Constants::BLOCK_SIZE, 2 * Constants::BLOCK_SIZE,
      Constants::BLOCK_SIZE },
    { Constants::BLOCK_SIZE * 3 / 2, Constants::BLOCK_SIZE,
      Constants::BLOCK_SIZE * 3 / 2 },

    // This is a no-op
    { Constants::BLOCK_SIZE, Constants::BLOCK_SIZE * 2,
      3 * Constants::BLOCK_SIZE },
  };

  for (size_t i = 0; i < SIZEOF_ARRAY(test_cases); i++) {
    ChangeList cl1;
    cl1.push_back(Change(Change::MOVE, test_cases[i].size,
			 test_cases[i].from, test_cases[i].to));
    spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
    CHECK_EQ(spec0.Size(), spec1.Size());

    Block coded;
    InMemoryEncodeDecode(spec0, spec1, &coded, Options());

    Delta delta(coded);
    CHECK_EQ(0, delta.AddedBytes());
  }
}

void TestOverwriteMutator() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE);

  ChangeList cl1;
  cl1.push_back(Change(Change::COPYOVER, 10, 0, 20));
  spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
  CHECK_EQ(spec0.Size(), spec1.Size());

  Block b0, b1;
  BlockIterator(spec0).Get(&b0);
  BlockIterator(spec1).Get(&b1);

  CHECK(memcmp(b0.Data(), b1.Data() + 20, 10) == 0);
  CHECK(memcmp(b0.Data(), b1.Data(), 20) == 0);
  CHECK(memcmp(b0.Data() + 30, b1.Data() + 30,
	       Constants::BLOCK_SIZE - 30) == 0);

  xoff_t zero = 0;
  cl1.clear();
  cl1.push_back(Change(Change::COPYOVER, 10, 20, zero));
  spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
  CHECK_EQ(spec0.Size(), spec1.Size());

  BlockIterator(spec0).Get(&b0);
  BlockIterator(spec1).Get(&b1);

  CHECK(memcmp(b0.Data() + 20, b1.Data(), 10) == 0);
  CHECK(memcmp(b0.Data() + 10, b1.Data() + 10,
	       Constants::BLOCK_SIZE - 10) == 0);
}

// Note: this test is written to expose a problem, but the problem was
// only exposed with BLOCK_SIZE = 128.
void TestNonBlocking() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);
  FileSpec spec2(&rand);

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE * 3);

  // This is a lazy target match
  Change ct(Change::COPYOVER, 22,
	    Constants::BLOCK_SIZE + 50,
	    Constants::BLOCK_SIZE + 20);

  // This is a source match just after the block boundary, shorter
  // than the lazy target match.
  Change cs1(Change::COPYOVER, 16,
	     Constants::BLOCK_SIZE + 51,
	     Constants::BLOCK_SIZE - 1);

  // This overwrites the original source bytes.
  Change cs2(Change::MODIFY, 108,
	     Constants::BLOCK_SIZE + 20);

  // This changes the first blocks
  Change c1st(Change::MODIFY, Constants::BLOCK_SIZE - 2, 0);

  ChangeList csl;
  csl.push_back(cs1);
  csl.push_back(cs2);
  csl.push_back(c1st);

  spec0.ModifyTo(ChangeListMutator(csl), &spec1);

  ChangeList ctl;
  ctl.push_back(ct);
  ctl.push_back(c1st);

  spec0.ModifyTo(ChangeListMutator(ctl), &spec2);

  InMemoryEncodeDecode(spec1, spec2, NULL, Options());
}

void TestEmptyInMemory() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);
  Block block;

  spec0.GenerateFixedSize(0);
  spec1.GenerateFixedSize(0);

  InMemoryEncodeDecode(spec0, spec1, &block, Options());

  Delta delta(block);
  CHECK_LT(0, block.Size());
  CHECK_EQ(1, delta.Windows());
}

void TestBlockInMemory() {
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);
  Block block;

  spec0.GenerateFixedSize(Constants::BLOCK_SIZE);
  spec1.GenerateFixedSize(Constants::BLOCK_SIZE);

  InMemoryEncodeDecode(spec0, spec1, &block, Options());

  Delta delta(block);
  CHECK_EQ(spec1.Blocks(Constants::WINDOW_SIZE), delta.Windows());
}

void TestSmallStride() {
  MTRandom rand;
  FileSpec spec0(&rand);
  usize_t size = Constants::BLOCK_SIZE * 4;
  spec0.GenerateFixedSize(size);

  // Note: Not very good performance due to hash collisions, note 3x
  // multiplier below.
  for (int s = 15; s < 101; s++) {
    usize_t changes = 0;
    ChangeList cl;
    for (usize_t j = s; j < size; j += s, ++changes)
      {
	cl.push_back(Change(Change::MODIFY, 1, j));
      }

    FileSpec spec1(&rand);
    spec0.ModifyTo(ChangeListMutator(cl), &spec1);

    Options options;
    options.encode_srcwin_maxsz = size;
    options.iopt_size = 128;
    options.smatch_cfg = XD3_SMATCH_SLOW;
    options.size_known = false;

    Block block;
    InMemoryEncodeDecode(spec0, spec1, &block, options);
    Delta delta(block);

    IF_DEBUG1(DP(RINT "[stride=%d] changes=%" W "u adds=%" Q "u\n",
		 s, changes, delta.AddedBytes()));
    double allowance = Constants::BLOCK_SIZE < 8192 || s < 30 ? 3.0 : 1.1;
    CHECK_GE(allowance * changes, (double)delta.AddedBytes());
  }
}

void TestCopyWindow() {
  // Construct an input that has many copies, to fill the IOPT buffer
  // and force a source window decision.  "srclen" may be set to a
  // value that goes beyond the end-of-source.
  const int clen = 16;
  const int size = 4096;
  const int nmov = size / clen;
  const int iters = 16;
  uint32_t added_01 = 0;
  uint32_t added_10 = 0;
  for (int i = 1; i <= iters; i++) {
    MTRandom rand(MTRandom::TEST_SEED1 * i);
    FileSpec spec0(&rand);
    ChangeList cl;

    spec0.GenerateFixedSize(size);

    for (int j = 0; j < nmov; j += 2)
      {
	cl.push_back(Change(Change::MOVE,
			    clen, (j + 1) * clen, j * clen));
      }

    FileSpec spec1(&rand);
    spec0.ModifyTo(ChangeListMutator(cl), &spec1);

    Options options;
    options.encode_srcwin_maxsz = size;
    options.iopt_size = 128;
    options.smatch_cfg = XD3_SMATCH_SLOW;

    Block block1;
    InMemoryEncodeDecode(spec0, spec1, &block1, options);
    Delta delta1(block1);
    // Allow one missed window (e.g., hash collisions)
    added_01 += delta1.AddedBytes();

    Block block2;
    InMemoryEncodeDecode(spec1, spec0, &block2, options);
    Delta delta2(block2);
    // Allow one missed window (e.g., hash collisions)
    added_10 += delta2.AddedBytes();

    Block block3;
    Block block4;
    EncodeDecodeAPI(spec0, spec1, &block3, options);
    EncodeDecodeAPI(spec1, spec0, &block4, options);
  }
  // Average less than 0.5 misses (of length clen) per iteration.
  CHECK_GE(clen * iters / 2, added_01);
  CHECK_GE(clen * iters / 2, added_10);
}

void TestCopyFromEnd() {
  // Copies from the end of the source buffer, which reach a block
  // boundary end-of-file.
  const int size = 4096;
  const int clen = 16;
  const int nmov = (size / 2) / clen;
  const int iters = 16;
  uint32_t added_01 = 0;
  uint32_t added_10 = 0;
  for (int i = 1; i <= iters; i++) {
    MTRandom rand(MTRandom::TEST_SEED1 * i);
    FileSpec spec0(&rand);
    ChangeList cl;

    spec0.GenerateFixedSize(size);

    cl.push_back(Change(Change::MODIFY, 2012, 2048));

    for (int j = 0; j < nmov; j += 2)
      {
	cl.push_back(Change(Change::MOVE,
			    clen, (j + 1) * clen, j * clen));
      }

    cl.push_back(Change(Change::COPYOVER, 28, 4068, 3000));
    cl.push_back(Change(Change::COPYOVER, 30, 4066, 3100));
    cl.push_back(Change(Change::COPYOVER, 32, 4064, 3200));

    FileSpec spec1(&rand);
    spec0.ModifyTo(ChangeListMutator(cl), &spec1);

    Options options;
    options.encode_srcwin_maxsz = size;
    options.iopt_size = 128;
    options.smatch_cfg = XD3_SMATCH_SLOW;

    Block block1;
    InMemoryEncodeDecode(spec0, spec1, &block1, options);
    Delta delta1(block1);
    added_01 += delta1.AddedBytes();

    Block block2;
    InMemoryEncodeDecode(spec1, spec0, &block2, options);
    Delta delta2(block2);
    added_10 += delta2.AddedBytes();

    Block block3;
    Block block4;
    EncodeDecodeAPI(spec0, spec1, &block3, options);
    EncodeDecodeAPI(spec1, spec0, &block4, options);
  }
  CHECK_GE(2000 * iters, added_01);
  CHECK_LE(2000 * iters, added_10);
}

void TestHalfBlockCopy() {
  // Create a half-block copy, 7.5 blocks apart, in a pair of files:
  //       0             1     ...     6             7
  // spec0 [bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb][ccccc][bbbb_]
  // spec1 [aaaaa][ccccc][aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa_]
  // where stage=
  // 0: the final block is full
  //   a. (source)spec1->(target)spec0 copies block C: reads 8 source
  //      blocks during target block 0.
  //   b. (source)spec0->(target)spec1 does not copy block C b/c attempt
  //      to read past EOF empties block 0 from (virtual) block cache
  // 1: the final block is less than full.
  //   a. (same) copies block C
  //   b. (same) copies block C, unlike 0a, no attempt to read past EOF
  //
  // "virtual" above refers to XD3_TOOFARBACK, since there is no caching
  // in the API, there is simply a promise not to request blocks that are
  // beyond source->max_winsize from the last known source file position.
  for (int stage = 0; stage < 2; stage++)
    {
      IF_DEBUG1 (DP(RINT "half_block_copy stage %d\n", stage));

      MTRandom rand;
      FileSpec spec0(&rand);
      FileSpec spec1(&rand);

      spec0.GenerateFixedSize(Constants::BLOCK_SIZE * 8 - stage);

      ChangeList cl1;
      cl1.push_back(Change(Change::MODIFY,
			   Constants::BLOCK_SIZE / 2,  // size
			   0));
      cl1.push_back(Change(Change::COPYOVER,
			   Constants::BLOCK_SIZE / 2,  // size
			   Constants::BLOCK_SIZE * 7,  // offset
			   Constants::BLOCK_SIZE / 2));
      cl1.push_back(Change(Change::MODIFY,
			   Constants::BLOCK_SIZE * 7,
			   Constants::BLOCK_SIZE - stage));
      spec0.ModifyTo(ChangeListMutator(cl1), &spec1);

      Options options;
      options.encode_srcwin_maxsz = Constants::BLOCK_SIZE * 8;

      Block block0;
      Block block1;
      InMemoryEncodeDecode(spec0, spec1, &block0, options);
      InMemoryEncodeDecode(spec1, spec0, &block1, options);

      Delta delta0(block0);
      Delta delta1(block1);

      const int yes =
	Constants::BLOCK_SIZE * 8 - Constants::BLOCK_SIZE / 2;
      const int no =
	Constants::BLOCK_SIZE * 8 - Constants::BLOCK_SIZE / 2;

      if (stage == 0)
	{
	  CHECK_EQ(yes, delta0.AddedBytes());
	  CHECK_EQ(no, delta1.AddedBytes());
	}
      else
	{
	  CHECK_EQ(yes, delta0.AddedBytes());
	  CHECK_EQ(yes, delta1.AddedBytes());
	}
    }
}

void FourWayMergeTest(const FileSpec &spec0,
		      const FileSpec &spec1,
		      const FileSpec &spec2,
		      const FileSpec &spec3) {
  TmpFile f0, f1, f2, f3;
  ExtFile d01, d12, d23;
  Options options;
  options.encode_srcwin_maxsz =
    std::max(spec0.Size(), options.encode_srcwin_maxsz);

  spec0.WriteTmpFile(&f0);
  spec1.WriteTmpFile(&f1);
  spec2.WriteTmpFile(&f2);
  spec3.WriteTmpFile(&f3);

  MainEncodeDecode(f0, f1, &d01, options);
  MainEncodeDecode(f1, f2, &d12, options);
  MainEncodeDecode(f2, f3, &d23, options);

  // Merge 2
  ExtFile out;
  vector<const char*> mcmd;
  mcmd.push_back("xdelta3");
  mcmd.push_back("merge");
  mcmd.push_back("-m");
  mcmd.push_back(d01.Name());
  mcmd.push_back(d12.Name());
  mcmd.push_back(out.Name());
  mcmd.push_back(NULL);

  // XPR(NTR "Running one merge: %s\n", CommandToString(mcmd).c_str());
  CHECK_EQ(0, xd3_main_cmdline(mcmd.size() - 1,
			       const_cast<char**>(&mcmd[0])));

  ExtFile recon;
  vector<const char*> tcmd;
  tcmd.push_back("xdelta3");
  tcmd.push_back("-d");
  tcmd.push_back("-s");
  tcmd.push_back(f0.Name());
  tcmd.push_back(out.Name());
  tcmd.push_back(recon.Name());
  tcmd.push_back(NULL);

  // XPR(NTR "Running one recon! %s\n", CommandToString(tcmd).c_str());
  CHECK_EQ(0, xd3_main_cmdline(tcmd.size() - 1,
			       const_cast<char**>(&tcmd[0])));
  // XPR(NTR "Should equal! %s\n", f2.Name());

  CHECK(recon.EqualsSpec(spec2));

  // Merge 3
  ExtFile out3;
  vector<const char*> mcmd3;
  mcmd3.push_back("xdelta3");
  mcmd3.push_back("merge");
  mcmd3.push_back("-m");
  mcmd3.push_back(d01.Name());
  mcmd3.push_back("-m");
  mcmd3.push_back(d12.Name());
  mcmd3.push_back(d23.Name());
  mcmd3.push_back(out3.Name());
  mcmd3.push_back(NULL);

  // XPR(NTR "Running one 3-merge: %s\n", CommandToString(mcmd3).c_str());
  CHECK_EQ(0, xd3_main_cmdline(mcmd3.size() - 1,
			       const_cast<char**>(&mcmd3[0])));

  ExtFile recon3;
  vector<const char*> tcmd3;
  tcmd3.push_back("xdelta3");
  tcmd3.push_back("-d");
  tcmd3.push_back("-s");
  tcmd3.push_back(f0.Name());
  tcmd3.push_back(out3.Name());
  tcmd3.push_back(recon3.Name());
  tcmd3.push_back(NULL);

  // XPR(NTR "Running one 3-recon %s\n", CommandToString(tcmd3).c_str());
  CHECK_EQ(0, xd3_main_cmdline(tcmd3.size() - 1,
			       const_cast<char**>(&tcmd3[0])));
  // XPR(NTR "Should equal %s\n", f3.Name());

  CHECK(recon3.EqualsSpec(spec3));
}

void TestMergeCommand1() {
  /* Repeat random-input testing for a number of iterations.
   * Test 2, 3, and 4-file scenarios (i.e., 1, 2, and 3-delta merges). */
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);
  FileSpec spec2(&rand);
  FileSpec spec3(&rand);

  SizeIterator<size_t, Sizes> si0(&rand, 10);

  for (; !si0.Done(); si0.Next()) {
    size_t size0 = si0.Get();

    SizeIterator<size_t, Sizes> si1(&rand, 10);
    for (; !si1.Done(); si1.Next()) {
      size_t change1 = si1.Get();

      if (change1 == 0) {
	continue;
      }

      // XPR(NTR "S0 = %lu\n", size0);
      // XPR(NTR "C1 = %lu\n", change1);
      // XPR(NTR ".");

      size_t add1_pos = size0 ? rand.Rand32() % size0 : 0;
      size_t del2_pos = size0 ? rand.Rand32() % size0 : 0;

      spec0.GenerateFixedSize(size0);

      ChangeList cl1, cl2, cl3;

      size_t change3 = change1;
      size_t change3_pos;

      if (change3 >= size0) {
	change3 = size0;
	change3_pos = 0;
      } else {
	change3_pos = rand.Rand32() % (size0 - change3);
      }

      cl1.push_back(Change(Change::ADD, change1, add1_pos));
      cl2.push_back(Change(Change::DELRANGE, change1, del2_pos));
      cl3.push_back(Change(Change::MODIFY, change3, change3_pos));

      spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
      spec1.ModifyTo(ChangeListMutator(cl2), &spec2);
      spec2.ModifyTo(ChangeListMutator(cl3), &spec3);

      FourWayMergeTest(spec0, spec1, spec2, spec3);
      FourWayMergeTest(spec3, spec2, spec1, spec0);
    }
  }
}

void TestMergeCommand2() {
  /* Same as above, different mutation pattern. */
  /* TODO: run this with large sizes too */
  /* TODO: run this with small sizes too */
  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);
  FileSpec spec2(&rand);
  FileSpec spec3(&rand);

  SizeIterator<size_t, Sizes> si0(&rand, 10);
  for (; !si0.Done(); si0.Next()) {
    size_t size0 = si0.Get();

    SizeIterator<size_t, Sizes> si1(&rand, 10);
    for (; !si1.Done(); si1.Next()) {
      size_t size1 = si1.Get();

      SizeIterator<size_t, Sizes> si2(&rand, 10);
      for (; !si2.Done(); si2.Next()) {
	size_t size2 = si2.Get();

	SizeIterator<size_t, Sizes> si3(&rand, 10);
	for (; !si3.Done(); si3.Next()) {
	  size_t size3 = si3.Get();

	  // We're only interested in three sizes, strictly decreasing. */
	  if (size3 >= size2 || size2 >= size1 || size1 >= size0) {
	    continue;
	  }

	  // XPR(NTR "S0 = %lu\n", size0);
	  // XPR(NTR "S1 = %lu\n", size1);
	  // XPR(NTR "S2 = %lu\n", size2);
	  // XPR(NTR "S3 = %lu\n", size3);
	  // XPR(NTR ".");

	  spec0.GenerateFixedSize(size0);

	  ChangeList cl1, cl2, cl3;

	  cl1.push_back(Change(Change::DELRANGE, size0 - size1, 0));
	  cl2.push_back(Change(Change::DELRANGE, size0 - size2, 0));
	  cl3.push_back(Change(Change::DELRANGE, size0 - size3, 0));

	  spec0.ModifyTo(ChangeListMutator(cl1), &spec1);
	  spec0.ModifyTo(ChangeListMutator(cl2), &spec2);
	  spec0.ModifyTo(ChangeListMutator(cl3), &spec3);

	  FourWayMergeTest(spec0, spec1, spec2, spec3);
	  FourWayMergeTest(spec3, spec2, spec1, spec0);
	}
      }
    }
  }
}

void TestLastFrontierBlock() {
  // This test constructs an input that can expose
  // https://github.com/jmacd/xdelta/issues/188
  // when run through the command-line with source via a FIFO.
  // That is not tested here, but the test stays.
  if (Constants::WINDOW_SIZE < XD3_ALLOCSIZE)
    {
      return;
    }

  MTRandom rand;
  FileSpec spec0(&rand);
  FileSpec spec1(&rand);
  const xoff_t size = XD3_ALLOCSIZE * 64;  // == XD3_MINSRCWINSZ * 2
  const xoff_t edit = XD3_ALLOCSIZE;

  Options options;
  options.encode_srcwin_maxsz = XD3_MINSRCWINSZ;
  options.block_size = XD3_ALLOCSIZE;
  options.window_size = XD3_MINSRCWINSZ;
  options.size_known = false;

  spec0.GenerateFixedSize(size);

  ChangeList cl;

  // Modify the 0th byte in order to induce indexing of subsequent
  // bytes, but allow copying most of the file to keep the test fast.
  cl.push_back(Change(Change::MODIFY, 1, edit * 31));
  cl.push_back(Change(Change::COPYOVER, edit, edit * 31, edit * 63));

  spec0.ModifyTo(ChangeListMutator(cl), &spec1);

  Block noblock;
  InMemoryEncodeDecode(spec0, spec1, &noblock, options);
  InMemoryEncodeDecode(spec1, spec0, &noblock, options);
}

};  // class Regtest<Constants>

#define TEST(x) XPR(NTR #x "...\n"); regtest.x()

// These tests are primarily tests of the testing framework itself.
template <class T>
void UnitTest() {
  Regtest<T> regtest;
  TEST(TestPrintf);
  TEST(TestRandomNumbers);
  TEST(TestRandomFile);
  TEST(TestFirstByte);
  TEST(TestModifyMutator);
  TEST(TestAddMutator);
  TEST(TestDeleteMutator);
  TEST(TestCopyMutator);
  TEST(TestMoveMutator);
  TEST(TestOverwriteMutator);
}

// These are Xdelta tests.
template <class T>
void MainTest() {
  XPR(NT "Blocksize %" Q "u windowsize %" Z "u\n",
      T::BLOCK_SIZE, T::WINDOW_SIZE);
  Regtest<T> regtest;
  TEST(TestEmptyInMemory);
  TEST(TestBlockInMemory);
  TEST(TestSmallStride);
  TEST(TestCopyWindow);
  TEST(TestCopyFromEnd);
  TEST(TestNonBlocking);
  TEST(TestHalfBlockCopy);
  TEST(TestLastFrontierBlock);
  TEST(TestMergeCommand1);
  TEST(TestMergeCommand2);
}

#undef TEST

int main(int argc, char **argv)
{
  vector<const char*> mcmd;
  string pn;
  const char *sp = strrchr(argv[0], '/');
  if (sp != NULL) {
    pn.append(argv[0], sp - argv[0] + 1);
  }
  pn.append("xdelta3");
  mcmd.push_back(pn.c_str());
  mcmd.push_back("test");
  mcmd.push_back(NULL);

  UnitTest<SmallBlock>();
  MainTest<SmallBlock>();
  MainTest<MixedBlock>();
  MainTest<OversizeBlock>();
  MainTest<LargeBlock>();

  CHECK_EQ(0, xd3_main_cmdline(mcmd.size() - 1,
  			       const_cast<char**>(&mcmd[0])));

  return 0;
}

