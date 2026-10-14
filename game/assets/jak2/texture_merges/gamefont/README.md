# Cyrillic glyphs for the Jak 2 font

These images are merged over the original `gamefont` textures (`font.24lo`, `font.12lo`, `font.12hi`)
by the decompiler's `texture_merges` feature. Only the new glyphs are drawn, everything else is transparent.

- 66 letters (А-Я, а-я, Ё, ё) placed in unused cells 174-239 (character codes `0xB4`-`0xF5`).
- Mappings are in `common/util/font/dbs/font_db_jak2.cpp`, glyph widths in `goal_src/jak2/engine/data/font-data.gc`.
- The glyphs were drawn from scratch based on the Jost font (SIL Open Font License 1.1, see `Jost_OFL.txt`).
  Nothing was copied or traced from the Jak 2 or Jak 3 font textures.
