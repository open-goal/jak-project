# BLERC
"blerc" = "merc blend shape" is the face animation program.

It works by updating the vertices used by merc.

It begine with a call to `(blerc-init)`, which resets the list of things to blerc.

Then, each process-drawable calls `merc-blend-shape` (if needed), which figures out the blend shape coefficients for the current frame, and passes it to `setup-blerc-chains`. This builds a DMA chain of all blercs.

However, this DMA chain doesn't get put in a VU1 bucket. Instead, after the full chain is build, `(blerc-execute)` will do a bunch of math and update the merc vertex data.

This was checked by running `(-> (the process-drawable (process-by-name "sidekick" *active-pool*)) draw mgeo effect 0 frag-geo)` and seeing that the `spad_from_dma_no_sadr_off` in `merc_blend_shape.cpp` was writing to this data.

There is a system to avoid doing blerc computations when not needed. Once blerc is disabled, it does a single final run with all 0's, which puts everything "back to normal", and then blerc no longer runs.

# Determining which fragments can be blerc'd
In the PC port, we will need to update the vertices. We want to update as few vertices as possible at runtime because this will likely be slow.  Additionally, we need metadata to figure out how to go from modified merc data back to PC-port format. If we know which vertices can possibly be modified by blerc, we can skip including the metadata for the rest. Only a small number of merc vertices are faces, so this could be a big win for data size.

As far as I can tell, there's a `merc-blend-ctrl` per each fragment. It's size is `2 + merc_ctrl.header.blend_target_count`. If the `blend-vtx-count` field of a merc-blend-ctrl is 0, then this fragment has no blerc.

Checking some common models:
(showing number of effects, frags, lump4 bytes that have possible blerc)
```
BLERC: eichar-lod0, 3/4 e, 15/85 f, 1737/9984 v
BLERC: sidekick-lod0, 3/4 e, 9/35 f, 957/3933 v
```

# PC draw lists:
The plan is to make 3 sets of draws:
- Normal draw list
- Draw list that touches no blerc vertices
- Alternate draw list for blerc vertices

The first list is what is normally used. This may contain draws that mix together blerc and non-blerc vertices.

The second draw list can be used when blerc is active for this character for all the non-blerc vertices.

The third draw list will contain the blerc vertices, but will do indexing slightly differently. Instead of indexing into the giant array of all merc vertices, it will index into the yet-to-be-filled blerc index buffer for this character.

This approach gives us:
- the same performance as before if blerc is off
- easy way to do per-character blerc'd vertex uploads
- possibility to do per-frame blerc'd vertex uploads with some clever indexing.
- per-fragment granularity for blerc on/off