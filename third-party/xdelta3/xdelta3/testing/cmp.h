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
static size_t CmpDifferentBlockBytes(const Block &a, const Block &b) {
  size_t total = 0;
  size_t i = 0; 
  size_t m = min(a.Size(), b.Size());

  for (; i < m; i++) {
    if (a[i] != b[i]) {
      total++;
    }
  }

  total += a.Size() - i;
  total += b.Size() - i;

  return total;
}

static xoff_t CmpDifferentBytes(const FileSpec &a, const FileSpec &b) {
  Block block_a, block_b;
  xoff_t total = 0;
  typename FileSpec::iterator a_i(a), b_i(b);

  for (; !a_i.Done() && !b_i.Done(); a_i.Next(), b_i.Next()) {

    a_i.Get(&block_a);
    b_i.Get(&block_b);

    total += CmpDifferentBlockBytes(block_a, block_b);
  }

  for (; !a_i.Done(); a_i.Next()) {
    total += a_i.BytesOnBlock();
  }
  for (; !b_i.Done(); b_i.Next()) {
    total += b_i.BytesOnBlock();
  }

  return total;
}

static size_t CmpDifferentBlockBytesAtOffset(const Block &a,
					     const FileSpec &b_spec,
					     xoff_t offset) {
  Block b;
  size_t size = a.Size();
  CHECK_LE(offset, b_spec.Size());
  if (b_spec.Size() < offset + size) {
    size = b_spec.Size() - offset;
  }
  b_spec.Get(&b, offset, size);
  return CmpDifferentBlockBytes(a, b);
}
