# How to replace textures
## Release build
Create a folder called `texture_replacements` inside the `data` directory. The directory structure should be
```
data/texture_replacements/page_name/texture_name.png
```
Where `page_name` is the name of the folder in `data/assets/textures` and `texture_name.png` is the name of the texture.

## From source
Textures to be replaced should be saved in
```
jak-project/texture_replacements/page_name/texture_name.png
```
Where `page_name` is the name of the folder in `assets/textures` and `texture_name.png` is the name of the texture. You'll have to create the `texture_replacements` folder yourself.

# Recommended use
To make this easier to set up, you can copy the default textures from `assets`, and then modify those.

For example, you can copy the `common` folder from `assets/textures` to `texture_replacements`.  Then you can modify the png files in `texture_replacements/common`

# Rebuilding the game with modified textures
Run the decompiler/extractor again to rebuild with modified textures.

If it worked, you will see:
```
Replacing jak-project/texture_replacements/common/jng-precursor-metal-plain-01-lores.png
```
in part of the output.

# Restrictions
Do not change the resolution of the sky, clouds, or eye textures. Other textures should let you change the size.  Using extremely large textures will use more VRAM and will load slower.

The PNG file should have an alpha channel. Some textures use their alpha channels for transparency, or for indicating which parts should have environment mapping applied. It may be useful to look at how the original texture uses the alpha channel first, especially for particle effects.
