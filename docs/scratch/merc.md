Loop over effect (skip those flagged with use-mercneric)

Loop over fragments

First thing is the "row" data.
row.x/y is st-vif-add from the merc-ctrl-header.
row.z = 0x47800000, row.w = 0x4b010000


Next is the unsigned-four.
This is unpacked with unpack8.
It always goes to 140 in the VU memory.
The source data is just the merc-fragment (I believe it includes merc-byte-header)

After is the lump-four. This has a STMOD enabled and is unpack8.
It goes after the unsigned-four (variable size) in VU memory.
The source data is after. Rounding in static data is ((u4c + 3) >> 2) << 4.

After is the Floating Point data. This is copied as unpack32.

_only_ on the first fragment of an effect, there's an upload to 132 of 8 qw:
the first 7 are lights.
the final is the first quadword of the merc-ctrl-header (xyz-scale, st-magic, st-out-a, st-out-b)

there are secrets hidden in the lights:
- light 0's w is some flag with ignore alpha in it.


Next is (optional) matrix uploads.
There is a loop of transfers. These are all size 7 qw.

Next is the MSCAL!
It has a different number of the first fragment of an effect.
This tells merc to load the light stuff.

End Loop over fragments

Increment effect
Increment effect info
Decrement effect count

update some next-merc thing in the scratchpad


# Merc Renderer


