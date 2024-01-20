## Finding the Normals
We're going to assume that the generic tie math in Jak 1 is the same as ETIE as Jak 2. This could be wrong, but it should be easy to verify.

The tricky part is finding the normals. These are needed for ETIE, but not plain TIE.

For looking through the types, it seems like generic TIE uses the same normals as ETIE:
```lisp
(deftype generic-tie-normal (structure)
  ((x     int8  :offset-assert 0)
   (y     int8  :offset-assert 1)
   (z     int8  :offset-assert 2)
   (dummy int8  :offset-assert 3) ;; was 0 in ETIE normals.
   )
  :method-count-assert 9
  :size-assert         #x4
  :flag-assert         #x900000004
  )
```

Searching around for normal
```lisp
(deftype generic-tie-header (structure)
  ((effect              uint8              :offset-assert 0)
   (interp-table-size   uint8              :offset-assert 1)
   (num-bps             uint8              :offset-assert 2)
   (num-ips             uint8              :offset-assert 3)
   (tint-color          uint32             :offset-assert 4)
   (index-table-offset  uint16             :offset-assert 8)
   (kick-table-offset   uint16             :offset-assert 10)
   (normal-table-offset uint16             :offset-assert 12) ;; here it is!
   (interp-table-offset uint16             :offset-assert 14)
   (gsf-header          gsf-header :inline :offset-assert 16)
   )
  :method-count-assert 9
  :size-assert         #x20
  :flag-assert         #x900000020
  )
```

My first guess is that the generic data for a TIE fragment is just a header, and these `uint16`s are byte-offsets for the normal data. I'd guess that there's a header per-fragment - they used `uint8`'s for `num-bps`/`num-ips` (base points, interpolated points). A single proto might have more than 255 points, but a fragment won't.

My guess is that `generic-ref` in a `tie-fragment` points to some data that starts with `generic-tie-header`.
```lisp
(deftype tie-fragment (drawable)
  ((gif-ref       (inline-array adgif-shader) :offset 4)
   (point-ref     uint32         :offset 8)
   (color-index   uint16         :offset 12)
   (base-colors   uint8          :offset 14)
   (tex-count     uint16         :offset-assert 32)
   (gif-count     uint16         :offset-assert 34)
   (vertex-count  uint16         :offset-assert 36)
   (color-count   uint16         :offset-assert 38)
   (num-tris      uint16         :offset-assert 40)
   (num-dverts    uint16         :offset-assert 42)
   (dp-ref        uint32         :offset-assert 44)
   (dp-qwc        uint32         :offset-assert 48)
   (generic-ref   uint32         :offset-assert 52) ;; the data we want
   (generic-count uint32         :offset-assert 56)
   (debug-lines   (array vector-array)  :offset-assert 60)
   )
  :method-count-assert 18
  :size-assert         #x40
  :flag-assert         #x1200000040
  )
```

Extract the data from the file
```cpp
    u16 generic_qwc = read_plain_data_field<u32>(ref, "generic-count", dts);
    if (generic_qwc) {
      generic_data.resize(16 * generic_qwc);
      auto generic_data_ref = deref_label(get_field_ref(ref, "generic-ref", dts));
      memcpy_plain_data((u8*)generic_data.data(), generic_data_ref, generic_qwc * 16);
```

The data made sense. There were some cases where the `interp-table` and the `normal-table` appeared to be on top of each other, but this only occured when `num-ips` was 0 and `interp-table-size` was 0 too.

Treating `normal-table-offset` as a byte-offset from the start of the header seemed to work.

One fear I had is that the mesh for generic/non-generic is somehow different, so I sanity checked that `num-ips` + `num-bps` from the generic header matched the total unique vertex count from the TIE unpacker.

## Finding the Envmap shader
We need an additional shader for the environment map draw. Fortunately, this seems the same as Jak 2 - there's a field in `prototype-bucket-tie` for it. I turned on that code for Jak 1, and it found envmap shaders on reasonable things.

## Finding the tint color
This is a little trickier. In jak 2, the tint color was specified per bucket, but in Jak 1, it's specified per fragment. Luckily my data format for ETIE can support this, but `extract_tie.cpp` needs some refactoring to actually generate it.

## Normal Bugs

Some very small number of fragments appear totally wrong.

The most likely explanation is the normals are in the wrong order. The normals seems like valid normals, but looking at the mesh, they are clearly wrong.

I made the assumption that the order of normals would match the order of points, but could this be wrong?

There's this table called `index_table`:
```
0 2 32 33
1 3 16 17
34 35 4 5
6 8 36 37
7 9 18 20
38 39 19 21
10 11 40 41
22 23 12 13
42 43 24 25
14 15 44 45
26 27 28 29
46 47 30 31
```
but going through extremely carefully showed nothing interesting... It all matches up perfectly

Next step: maybe the normal matrix is wrong... let's try to actually find the mystery scaling factor:

```
vmulx.xyz vf16, vf10, vf14
```
the `vf14.x` here.

```
lui t6, 16256
mtc1 f1, t6 ;; 1.0

qmfc2.i s1, vf10
mtc1 f12, s1
dsra32 s2, s1, 0
mtc1 f13, s2
pextuw s2, r0, s2
mtc1 f14, s2
mula.s f12, f12
madda.s f13, f13
madd.s f15, f14, f14
rsqrt.s f15, f1, f15
mfc1 s1, f15
qmtc2.i vf14, s1
vmulx.xyz vf16, vf10, vf14
```