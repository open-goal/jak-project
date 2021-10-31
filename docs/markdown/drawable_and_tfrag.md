## Drawable Trees
At the highest level is the level file itself, which is a `bsp-header`.  It contains a `drawable-tree-array`, which contains a small number of `drawable-tree`s. (~1 to 15?).

Each of these `drawable-tree`s is a different kind, distinguished by its type. Different types of trees go to different renderers. For example, there is a `drawable-tree-lowres-tfrag`, a `drawable-tree-instance-tie`, and even a `drawable-tree-actor`.  It is possible to have multiple trees of the same type, as some trees seem to have a maximum size - for example a `drawable-tree-instance-tie` can have only 43 different prototypes, so it is common to see a few of these.  These trees are the thing passed to the appropriate renderers. All the trees for all the levels get put in the `background-work` structure, which is then read by the renderers.

The `drawable-tree-tfrag` contains all the "normal" tfrag data.  There are other trees like `trans`, `dirt`, `ice`, `lowres`, and `lowres-trans`, which are only present if the level actually has this type of geometry.  As far as I can tell, the special ones follow the same format, but I haven't checked this in detail.

The `drawable-tree-tfrag` contains a small number (1 to 6?) of `drawable-inline-array`.   They all refer to the same set of `tfragment`s (the actual things to draw), but have a different tree structure.

The `drawable-inline-array` contains `draw-node`s.  Each `draw-node` contains between 0 and 8 children. The children are represented by a pointer to an inline array of children.  The children can be either `draw-node`s or `tfragment`s.  All the children are always the same type.

The first `drawable-inline-array` in the `drawable-tree-tfrag` is the full tree structure. It starts with some number of children that is smaller than 8.  And from there on, all children are `draw-node` (always 8 or fewer children) or a `tfragment` directly. So this is the deepest possible tree. I believe the max depth seen is 6?

The next `drawable-inline-array` starts with a list of all nodes at with a depth of 1 from one of the top-level nodes of the first.  So this has at most 64 entries.  Like the previous tree, all the children from here on have 8 or fewer children.

This pattern continues. The n-th `drawable-inline-array` is a list of all nodes at depth n in the "real" tree.

There are two tricks to this:
First, if the `drawable-inline-array` contains `draw-node`s, the type is actually `drawable-inline-array-node`.  Unlike a `draw-node`, which can only contain 8 children, a `drawable-inline-array` can contain a huge number of children.  The final `drawable-inline-array` is a list of a all children at the final depth, so it's always an array of `tfragment`. In this case, the type of the `drawable-inline-array` is a `drawable-inline-array-tfrag`, and it's just a giant list of all the `tfragment`s in the level.

The second trick is that the `draw-node`s and `tfragment`s are stored only once, even if they appear in multiple `drawable-inline-array`s.  They used the weird "node = length + pointer to inline array of children" format and sorted nodes by depth to enable this.  


## Tfrag renderers
The `tfrag-methods.gc` file has the methods of `drawable` that call the actual "draw" implementations in `tfrag.gc`.  There is a near and "normal" version of the renderer. So far, it seems like trans/low-res, ice, dirt, etc don't have separate rendering code (or are part of the main `tfrag` program).

It looks possible for `tfragment`s to be drawn using the generic renderer, but so far I can't find the code where this happens.

## Tfrag data
Tfrag also uses the `adgif-shader` idea. I believe the shaders are per-`tfragment` (though some may share, or there may be tricks to avoid resending the shader if consecutive `tfragment`s use the same settings).

I don't know if the `adgif-shader` is always 5 quadwords, like for sprite.  It seems possible to use the `adgif-shader` just to set up texturing, but also have some other stuff.  I believe that we currently log in these shaders and link to textures, so we can probably learn something from inspecting these.

There are 4 sets of data in `tfragment`: base, common, level0, level1.  Each has its own DMA transfer (consisting of a address to data, plus a length).  The details of which goes where is not clear yet.  I _think_ that sometimes not all 4 are valid. There are only 3 start addresses stored, and the three DMA chains may be the same, or overlap.

The DMA data itself seems to only be loading data.  It uses `unpack` (V4-16, V4-32, V3-32, V4-8) and `STROW`, `STMOD`, `STCYCL` to set up fancy unpacking tricks.  No other VIFcodes have been found in any level.

Additionally, there are some color palettes that use the time-of-day system.