# Graphics

There are three frames in flight at a time:
- One frame being processed by the CPU, building DMA lists. This is called the "calc frame"
- One frame being processed by the GPU, going through the DMA lists and drawing triangles to one frame buffer. I will call this the "draw" frame.
- One frame being displayed on the screen, stored in the other frame buffer.  This is called the "on-screen" frame.



## Synchronization

The PC Port synchronizes on `syncv` and on `sync-path`. The `syncv` waits for an actual buffer swap and `sync-path` waits for the renderer to finish.

The game's code for this is kind of messy and confusing, and calls `sync-path` twice. On the PS2, you make sure rendering is done with `sync-path`, which waits for the DMA chain to finish.  But they call this earlier than I think they need to, and I don't really understand why.  I don't see any place where they read back from the finished frame or depth buffer.  Or where they would overwrite the memory. There's a second call to `sync-path` right where you would expect, imeediately before the `syncv`.  After `syncv`, they call some Sony library function to actually display the correct framebuffer, then immediately start sending the next DMA chain.

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

According to the Performance Analyzer, this takes about 1% to 2% of a frame.  They subtract off 4% of a frame from the profile bar so that 100% there is really around 96% of a frame, I guess to account for this extra time.

I'm really not sure why they have the first `sync-path` there.  It makes some sense in debug mode so that you can draw the profile bar for the GPU after it has finished.  Another theory is that they didn't want debug code and non-debug rendering running at the same time - the debug code will compete with the rendering to use the main bus, and will make the rendering slower.  But it seems like you don't want this in the release version.

For now, the PC Port does sync on `sync-path`, but it probably doesn't need to.

## DMA Copy

Starting the main graphics DMA chain is intercepted by the PC Port code.  In the GOAL thread, it iterates through the entire DMA chain and creates a copy. Then, it sends this copy to the graphics thread.

There are two reasons for copying:
- If there is a memory bug in the game that corrupts the DMA buffer, it will not cause the renderer to crash. This is nice for debugging.
- It will be easy to save this copied DMA chain to a file for debugging later.

The DMA copier attempts to reduce the amount of memory used. It divides the 128MB of RAM into 128 kB chunks, marks which ones contain DMA data, then only copies those chunks. The chunks are compacted and the DMA pointers are updated to point to the relocated chunks.

This operation might be expensive and we might need to get rid of it later. But it would be easy to get rid of - the renderers themselves just read from a DMA chain and don't care if it is a copy or not.

## DMA Chain Iteration

On the C++ side, you can iterate through DMA with the `DmaFollower` class.  Here is an example of flattening a DMA chain to a sequence of bytes:
```cpp
std::vector<u8> flatten_dma() {
  DmaFollower state(memory, start_tag_addr);
  std::vector<u8> result;
  while (!state.ended()) {
  	// tag value:
  	state.current_tag();
  	// tag address
  	state.current_tag_offset();

  	// DMA data
    auto read_result = state.read_and_advance();
    // this is the transferred tag (u64 after dma tag, usually 2x vif tags)
    u64 tag = read_result.transferred_tag;

    // the actual bytes (pointer to data in the input chain)
    result.insert(result.end(), read_result.data, read_result.data + read_result.size_bytes);
  }
  return result;
}
```

This will take care of following `call` and `ret` and those details.


## Buckets
The game builds the DMA chain in 69 buckets. Each bucket corresponds to a rendering pass.  In the OpenGLRenderer, you can designate a specific renderer for each bucket.

This can be set up in `init_bucket_renderers`, and you can pass arguments to the renderers:
```cpp
/*!
 * Construct bucket renderers.  We can specify different renderers for different buckets
 */
void OpenGLRenderer::init_bucket_renderers() {

  // create a DirectRenderer for each of these two buckets
  init_bucket_renderer<DirectRenderer>("debug-draw-0", BucketId::DEBUG_DRAW_0, 1024 * 8);
  init_bucket_renderer<DirectRenderer>("debug-draw-1", BucketId::DEBUG_DRAW_1, 1024 * 8);

}
```

Each bucket renderer will have access to shared data. For now, this is textures and shaders.

## Textures
Currently, the only textures supported are those uploaded with `upload-now!`, which does an immediate upload of the texture. The `TexturePool` receives the actual GOAL `texture-page` object, and uses the metadata there to figure out how to convert the texture to RGBA8888.

It maintains a lookup table of converted textures by TBP which is used for fast lookup in the renderers.

This system isn't great yet, I think we will need to improve it for time-of-day textures and textures that are swapped in and out of VRAM.

## Shaders
The shaders themselves are stored in `opengl_renderer/shaders`. They should be named with `.frag` and `.vert` extensions.

To add a new shader, add it to `ShaderId` in `Shader.h` and give it a name in `Shader.cpp`. It will be compiled when the graphics system is loaded.

## Direct Renderer
The direct renderer interprets GIF data directly. It is for rendering stuff that was set up entirely on the CPU, with no VU1 use.

Currently, the only known things that use this are:
- debug stuff
- font
- screen filter/blackout/depth-cue
- the progress menu

All of these cases are pretty simple, and this is nowhere near a full GS emulation. 

It does a single pass through the DMA chain and creates arrays of triangles. It is designed to reduce the number of OpenGL draw calls when consecutive primitives are drawn in the same mode.

## Mysteries

Why did they put the first call to `sync-path` in?

How does the `upload-now!` during texture login for near textures work? It seems like it writes too much and might write over the other level's texture.

