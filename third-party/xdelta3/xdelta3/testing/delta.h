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

class Delta {
public:
  Delta(const Block &block) {
    int ret;
    xd3_config config;
    memset(&stream_, 0, sizeof (stream_));
    memset(&config, 0, sizeof (config));

    xd3_init_config(&config, XD3_SKIP_EMIT | XD3_ADLER32_NOVER);

    CHECK_EQ(0, xd3_config_stream (&stream_, &config));

    xd3_avail_input (&stream_, block.Data(), block.Size());

    bool done = false;
    while (!done) {
      ret = xd3_decode_input(&stream_);
    
      switch (ret) {
      case XD3_INPUT:
	done = true;
	break;
      case XD3_OUTPUT:
	CHECK_EQ(0, xd3_whole_append_window (&stream_));
	break;
      case XD3_GOTHEADER:
      case XD3_WINSTART:
      case XD3_WINFINISH:
	break;
      default:
	cerr << "decode: " << done;
	abort();
      }
    }
  }

  ~Delta() {
    xd3_free_stream(&stream_);
  }

  xoff_t AddedBytes() const {
    return stream_.whole_target.addslen;
  }

  xoff_t Windows() const {
    return stream_.whole_target.wininfolen;
  }

// Note: This does not benefit from -Wformat= checking, due to the
// enclosing template. Further, it was not used.
// void Print() const {
//     for (size_t i = 0; i < stream_.whole_target.instlen; i++) {
//       xd3_winst &winst = stream_.whole_target.inst[i];
//       switch (winst.type) {
//       case XD3_RUN: 
// 	DP(RINT, "%" Q "u run %" W "u\n", winst.position, winst.size);
// 	break;
//       case XD3_ADD: 
// 	DP(RINT "%" Q "u add %" W "u\n", winst.position, winst.size);
// 	break;
//       default:
// 	DP(RINT "%" Q "u copy %" W "u @ %" Q "u (mode %u)\n", 
// 	   winst.position, winst.size, winst.addr, winst.mode);
// 	break;
//       }
//     }
//   }

private:
  xd3_stream stream_;
};
