# Graphics

There are three frames in flight at a time:
- One frame being processed by the CPU, building DMA lists. This is called the "calc frame"
- One frame being processed by the GPU, going through the DMA lists and drawing triangles to one frame buffer. I will call this the "draw" frame.
- One frame being displayed on the screen, stored in the other frame buffer.  This is called the "on-screen" frame.



## Synchronization

The PC Port only synchronizes on vsync. This waits for the "draw" frame to finish rendering, and for the buffers to be swapped.

The game's code for this is kind of messy and confusing. On the PS2, you make sure rendering is done with `sync-path`, which waits for the DMA chain to finish.  But they call this earlier than I think they need to, and I don't really understand why.  I don't see any place where they read back from the finished frame or depth buffer.  Or where they would overwrite the memory. There's a second call to `sync-path` right where you would expected, right before the `syncv`.  After `syncv`, they call some Sony library function to actually display the correct framebuffer, then immediately start sending the next DMA chain.

The stuff between `sync-path` and `syncv` is:
- depth cue "calc" (seems fast)
- screen filter "calc" (very fast, just DMA for a single quad)
- calc for letterbox bars and blackout
- debug draw
- draw profiler (look at all the stuff that happens after!)
- deci count draw
- file info draw
- stdcon text draw
- iop/memcard info
- tie buffer init
- merc buffer init
- post draw hook (does nothing)
- bucket patching
- cache flush
- a second `sync-path`

I'm really not sure why they have the first `sync-path` there. One theory is that they didn't want debug code and non-debug rendering running at the same time - the debug code will compete with the rendering to use the main bus, and will make the rendering slower.

For now, the PC Port doesn't do anything on `sync-path`. I think there's two ways this could be an issue in the future:
- If they reuse the DMA buffer after `sync-path`, and our renderer is still reading from it. Currently not an issue because of dma copy, described later.
- If they need to read the completed depth buffer or frame buffer after `sync-path`. I don't see any example of this.



## DMA Copy



## Buckets

## Textures

## Shaders

## Direct Renderer

## Mysteries

