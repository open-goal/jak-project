#!/usr/bin/python2.7
# xdelta3 - delta compression tools and library -*- Mode: C++ -*-
# Copyright 2016 Joshua MacDonald
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import xdelta3

# the test data section is expected to be len('target')
source = 'source source input0 source source'
target = 'source source target source source'

#
#

print 'encode: basic ...'
result, patch = xdelta3.xd3_encode_memory(target, source, 50)

assert result == 0
assert len(patch) < len(source)

print 'encode: adler32 ...'
result, patch_adler32 = xdelta3.xd3_encode_memory(target, source, 50,
                                                  xdelta3.XD3_ADLER32)

assert result == 0
assert len(patch_adler32) < len(source)
assert len(patch_adler32) > len(patch)

print 'encode: secondary ...'
result, patch_djw = xdelta3.xd3_encode_memory(target, source, 50,
                                              xdelta3.XD3_SEC_DJW)

assert result == 0
# secondary compression doesn't help
assert len(patch_djw) > len(patch)

print 'encode: exact ...'
result, ignore = xdelta3.xd3_encode_memory(target, source, len(patch))

assert result == 0
assert len(ignore) < len(source)

print 'encode: out of space ...'
result, ignore = xdelta3.xd3_encode_memory(target, source, len(patch) - 1)

assert result == 28
assert ignore == None

print 'encode: zero space ...'
result, ignore = xdelta3.xd3_encode_memory(target, source, 0)

assert result == 28
assert ignore == None

print 'encode: no source ...'
result, zdata = xdelta3.xd3_encode_memory(target, None, 50)

assert result == 0
assert len(zdata) > len(patch)

print 'encode: no input ...'
result, ignore = xdelta3.xd3_encode_memory(None, None, 50)

assert result != 0

print 'decode: basic ...'
result, target1 = xdelta3.xd3_decode_memory(patch, source, len(target))

assert result == 0
assert len(target1) == len(target)
assert target1 == target

print 'decode: out of space ...'
result, ignore = xdelta3.xd3_decode_memory(patch, source, len(target) - 1)

assert result == 28
assert ignore == None

print 'decode: zero space ...'
result, ignore = xdelta3.xd3_decode_memory(patch, source, 0)

assert result == 28
assert ignore == None

print 'decode: single byte error ...'
# a few expected single-byte errors, e.g., unused address cache bits, see
# xdelta3-test.h's single-bit error tests
extra_count = 4
noverify_count = 0
for corrupt_pos in range(len(patch_adler32)):
    input = ''.join([j == corrupt_pos and '\xff' or patch_adler32[j]
                     for j in range(len(patch_adler32))])

    result, ignore = xdelta3.xd3_decode_memory(input, source, len(target), 0)
    assert result == -17712
    assert ignore == None

    # without adler32 verification, the error may be in the data section which
    # in this case is 6 bytes 'target'
    result, corrupt = xdelta3.xd3_decode_memory(input, source, len(target),
                                                xdelta3.XD3_ADLER32_NOVER)
    if result == 0:
        noverify_count = noverify_count + 1
        #print "got %s" % corrupt
    #end
#end
assert noverify_count == len('target') + extra_count

print 'decode: no source ...'
result, target2 = xdelta3.xd3_decode_memory(zdata, None, len(target))

assert result == 0
assert target == target2

# Test compression level setting via flags.  assumes a 9 byte checksum
# and that level 9 steps 2, level 1 steps 15:
#         01234567890123456789012345678901
# level 1 only indexes 2 checksums "abcdefghi" and "ABCDEFGHI"
# outputs 43 vs. 23 bytes
print 'encode: compression level ...'

source = '_la_la_abcdefghi_la_la_ABCDEFGHI'
target = 'la_la_ABCDEFGH__la_la_abcdefgh__'

result1, level1 = xdelta3.xd3_encode_memory(target, source, 50, xdelta3.XD3_COMPLEVEL_1)
result9, level9 = xdelta3.xd3_encode_memory(target, source, 50, xdelta3.XD3_COMPLEVEL_9)

assert result1 == 0 and result9 == 0
assert len(level1) > len(level9)

#
# Issue 65
print 'encode: 65 ...'
source = 'Hello World' 
target = 'Hello everyone' 
result, patch = xdelta3.xd3_encode_memory(target, source, len(target))
assert result != 0

result, patch = xdelta3.xd3_encode_memory(target, source, 2 * len(target))
assert result == 0

print 'PASS'
