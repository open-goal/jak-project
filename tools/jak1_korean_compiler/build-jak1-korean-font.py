#!/usr/bin/env python3
"""Build the Jak 1 composited Hangul jamo bank and font atlases."""

from __future__ import annotations

import json
import re
from pathlib import Path

from PIL import Image, ImageChops, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[2]
TEXT_FILE = ROOT / "game/assets/jak1/text/game_custom_text_ko-KR.json"
SUBTITLE_FILE = ROOT / "game/assets/jak1/subtitle/subtitle_lines_ko-KR.json"
KOREAN_DB = ROOT / "game/assets/fonts/jak2_jak3_korean_db.json"
JAMO_CATALOG = ROOT / "scripts/jamos.png"
SOURCE_DIR = ROOT / "decompiler_out/jak1/textures/gamefontnew"
OUTPUT_DIR = ROOT / "custom_assets/jak1/texture_replacements/gamefontnew"
GOAL_TABLES = ROOT / "goal_src/jak1/engine/gfx/korean-font.gc"
MAP_FILE = ROOT / "game/assets/jak1/korean_glyph_map.json"

# 0 is unsafe inside a C string.  0x7e starts a Jak format command, and some
# translated strings are passed through format after lookup and expansion.
# Korean glyphs use marker 0x02, so their
# payload bytes can reuse every other extension value without colliding with
# the original marker 0x01 Japanese glyphs.  The high bit selects the hi page.
# Payload 0x80 has a zero low-page index and would address the cell before
# the atlas bank.  Leave it unused on both pages.
RESERVED_PAYLOADS = {0x00, 0x7E, 0x80}
AVAILABLE_PAYLOADS = [value for value in range(256) if value not in RESERVED_PAYLOADS]


def write_text_if_changed(path: Path, content: str) -> None:
    if path.exists() and path.read_text(encoding="utf-8") == content:
        return
    path.write_text(content, encoding="utf-8", newline="\n")


def korean_syllables() -> set[str]:
    menu_data = json.loads(TEXT_FILE.read_text(encoding="utf-8"))
    subtitle_data = json.loads(SUBTITLE_FILE.read_text(encoding="utf-8"))

    strings = list(menu_data.values())
    strings.extend(subtitle_data.get("speakers", {}).values())
    for section in ("cutscenes", "hints"):
        for lines in subtitle_data.get(section, {}).values():
            strings.extend(lines)
    return {char for text in strings for char in text if "가" <= char <= "힣"}


def syllable_glyphs(char: str, db: dict) -> list[str]:
    syllable = ord(char) - 0xAC00
    initial = chr(0x1100 + syllable // (21 * 28))
    median = chr(0x1161 + (syllable % (21 * 28)) // 28)
    final = chr(0x11A7 + syllable % 28)
    has_final = final != "\u11a7"
    vertical = ord(median) in {*range(0x1161, 0x1169), 0x1175}
    horizontal = ord(median) in {0x1169, *range(0x116D, 0x116F), *range(0x1172, 0x1174)}
    orientation = (3 if has_final else 0) + (0 if vertical else 1 if horizontal else 2)
    jamos = [initial, median] + ([final] if has_final else [])
    glyphs: list[str] = []
    for index, jamo in enumerate(jamos):
        context = ",".join(jamos[:index] + ["<G>"] + jamos[index + 1 :])
        entry = db[jamo][orientation]
        selected = entry["alternatives"].get(context, entry["defaultGlyph"])
        glyphs.extend(selected.split(","))
    return list(dict.fromkeys(glyphs))


def required_jamo_glyphs(chars: set[str]) -> list[str]:
    db = json.loads(KOREAN_DB.read_text(encoding="utf-8"))
    glyphs = {glyph for char in chars for glyph in syllable_glyphs(char, db)}
    return sorted(glyphs, key=lambda glyph: (glyph.startswith("extra_"), int(glyph.rsplit("0x", 1)[1], 16)))


def all_jamo_glyphs() -> list[str]:
    """Return every contextual component referenced by the Jak 2/3 Korean DB."""
    db = json.loads(KOREAN_DB.read_text(encoding="utf-8"))
    glyphs: set[str] = set()
    for orientations in db.values():
        for entry in orientations:
            if entry is None:
                continue
            glyphs.update(entry["defaultGlyph"].split(","))
            for alternative in entry["alternatives"].values():
                glyphs.update(alternative.split(","))
    return sorted(glyphs, key=lambda glyph: (glyph.startswith("extra_"), int(glyph.rsplit("0x", 1)[1], 16)))


def make_mapping(glyphs: list[str]) -> dict[str, int]:
    capacity = len(AVAILABLE_PAYLOADS)
    if len(glyphs) > capacity:
        raise RuntimeError(f"Korean glyph bank is full: {len(glyphs)} jamo glyphs, capacity {capacity}")
    return {glyph: AVAILABLE_PAYLOADS[index] for index, glyph in enumerate(glyphs)}


VECTOR_RE = re.compile(
    r"\(new 'static 'vector :x ([^ ]+) :y ([^ ]+) :z ([^ ]+) :w ([^)]+)\)"
)


def original_metrics(table_name: str, next_marker: str) -> list[tuple[float, float, float, float]]:
    source = (ROOT / "goal_src/jak1/engine/gfx/font.gc").read_text(encoding="utf-8")
    start = source.index(f"(define *{table_name}*")
    end = source.index(next_marker, start)
    return [tuple(map(float, match)) for match in VECTOR_RE.findall(source[start:end])]


def expanded_metrics(
    original: list[tuple[float, float, float, float]], small: bool
) -> list[tuple[float, float, float, float]]:
    # The replacement atlas is twice as wide.  Existing glyphs stay in its
    # left half, so all of their U coordinates are halved.
    metrics = [(x * 0.5, y, z, w) for x, y, z, w in original]
    default_width = 12.0 if small else 24.0
    # draw-string's extension path adds 255, then loads at byte offset -256.
    # Since each metric is a 16-byte vector, payload N reads table entry
    # N + 239 (not N + 255).
    while len(metrics) < 367:
        index = len(metrics)
        mask = index - 255
        # Restore the intended continuation of the Japanese extension grid.
        if not small and 34 <= mask <= 49:
            extension_slot = mask + 98
            col, row = extension_slot % 10, extension_slot // 10
            metrics.append(((0.0039 + col * 0.09375) * 0.5, 0.0019 + row * 0.0625, 1.0, 24.0))
        else:
            metrics.append((0.000975, 0.0019, 1.0, default_width))

    # Each 128-payload page has 127 usable cells, in a 15-column grid.
    # Both pages share the same UV layout in their new right-hand half.
    for mask in range(1, 128):
        slot = mask - 1
        col, row = slot % 15, slot // 15
        original_width = 128 if small else 256
        # GL samples texture pixels at their centers. Starting at the cell
        # boundary blends edge strokes with the transparent gutter, making
        # the first column look clipped and the opposite edge look dirty.
        x = (original_width + 1.5 + col * (8 if small else 16)) / (original_width * 2)
        y = (1.5 + row * (12 if small else 24)) / (256 if small else 512)
        metrics[239 + mask] = (
            x,
            y,
            1.0,
            8.0 if small else 16.0,
        )
    return metrics


def native_expanded_metrics(original: list[tuple[float, float, float, float]]) -> list[tuple[float, float, float, float]]:
    """Preserve Jak 1 extension UVs when Korean glyphs reuse their indices."""
    metrics = [(x * 0.5, y, z, w) for x, y, z, w in original]
    while len(metrics) < 367:
        metrics.append((0.000975, 0.0019, 1.0, 24.0))
    return metrics


def emit_goal_table(name: str, metrics: list[tuple[float, float, float, float]]) -> str:
    lines = [
        f"(define *{name}*\n",
        "  (new 'static\n",
        "       'inline-array\n",
        "       vector\n",
        f"       {len(metrics)}\n",
    ]
    for x, y, z, w in metrics:
        lines.append(
            f"       (new 'static 'vector :x {x:.7f} :y {y:.7f} :z {z:.1f} :w {w:.7f})\n"
        )
    lines[-1] = lines[-1].rstrip("\n") + "))\n"
    return "".join(lines)


def emit_mapping_table(name: str, mapping: dict[str, int], extra: bool) -> str:
    values = [0] * 256
    for glyph, payload in mapping.items():
        if glyph.startswith("extra_") != extra:
            continue
        values[int(glyph.rsplit("0x", 1)[1], 16)] = payload
    body = " ".join(f"#x{value:02x}" for value in values)
    return f"(define *{name}* (new 'static 'boxed-array :type uint8 {body}))\n"


def write_goal_tables(mapping: dict[str, int]) -> None:
    font12 = original_metrics("font12-table", "(define *font24-table*")
    font24 = original_metrics("font24-table", ";; we have both")
    if len(font12) != 250 or len(font24) != 289:
        raise RuntimeError(f"Unexpected base table sizes: {len(font12)}, {len(font24)}")
    content = (
        ";;-*-Lisp-*-\n"
        ";; Generated by tools/jak1_korean_compiler/build-jak1-korean-font.py.\n"
        "(in-package goal)\n"
        '(require "engine/gfx/font-h.gc")\n\n'
        + emit_goal_table("font12-table-korean", expanded_metrics(font12, True))
        + "\n"
        + emit_goal_table("font24-table-korean", expanded_metrics(font24, False))
        + "\n"
        + emit_goal_table("font12-table-native-expanded", native_expanded_metrics(font12))
        + "\n"
        + emit_goal_table("font24-table-native-expanded", native_expanded_metrics(font24))
        + "\n"
        + emit_mapping_table("korean-jamo-map", mapping, False)
        + emit_mapping_table("korean-extra-jamo-map", mapping, True)
    )
    write_text_if_changed(GOAL_TABLES, content)


def load_original_atlas(source: Path, destination: Path, expected_width: int) -> Image.Image:
    if source.exists():
        return Image.open(source).convert("RGBA")
    if destination.exists():
        expanded = Image.open(destination).convert("RGBA")
        if expanded.width == expected_width:
            return expanded
        if expanded.width >= expected_width * 2:
            return expanded.crop((0, 0, expected_width, expanded.height))
    raise FileNotFoundError(
        f"Missing {source} and {destination}. Run extraction with save_texture_pngs enabled once."
    )


def catalog_glyph(glyph: str) -> Image.Image:
    catalog = Image.open(JAMO_CATALOG).convert("RGBA")
    code = int(glyph.rsplit("0x", 1)[1], 16)
    # The catalog is a ten-column contact sheet. Green labels are removed below.
    if glyph.startswith("extra_"):
        offset = code - 0x7E
        row, col = divmod(offset, 10)
        # The secondary-page labels are above their glyphs in this catalog.
        y = 1646 + row * 65
    else:
        offset = code - 0x06
        row, col = divmod(offset, 10)
        y = row * 64
    x = col * 49
    # The contact sheet has a 49 px pitch, but several drawings extend across
    # that boundary. A hard 49 px crop both adds the next drawing's left edge
    # to this glyph and cuts that edge off the next glyph. Read a margin and
    # assign connected strokes to the cell containing most of their pixels.
    margin = 8
    padded = catalog.crop((x - margin, y, x + 49 + margin, y + 64))
    pixels = padded.load()
    for py in range(padded.height):
        for px in range(padded.width):
            r, g, b, _ = pixels[px, py]
            alpha = 0 if g > r * 1.35 and g > b * 1.35 else max(r, g, b)
            pixels[px, py] = (255, 255, 255, alpha)

    alpha = padded.getchannel("A")
    unvisited = {(px, py) for py in range(64) for px in range(padded.width)
                 if alpha.getpixel((px, py)) >= 12}
    selected: list[tuple[int, int]] = []
    while unvisited:
        seed = unvisited.pop()
        component = [seed]
        pending = [seed]
        while pending:
            px, py = pending.pop()
            for dy in (-1, 0, 1):
                for dx in (-1, 0, 1):
                    neighbor = (px + dx, py + dy)
                    if neighbor in unvisited:
                        unvisited.remove(neighbor)
                        pending.append(neighbor)
                        component.append(neighbor)
        inside = sum(margin <= px < margin + 49 for px, _ in component)
        if inside * 2 > len(component):
            selected.extend(component)

    if not selected:
        raise RuntimeError(f"No pixels found for {glyph}")
    # Keep the source composition coordinates unless a stroke crosses the
    # border; then move the whole drawing just enough to fit inside its cell.
    left = min(px for px, _ in selected)
    right = max(px for px, _ in selected)
    if right - left >= 49:
        raise RuntimeError(f"Source glyph {glyph} is wider than its atlas cell")
    shift = max(0, margin - left)
    shift = min(shift, margin + 48 - right)
    crop = Image.new("RGBA", (49, 64), (0, 0, 0, 0))
    out = crop.load()
    for px, py in selected:
        dest_x = px + shift - margin
        if 0 <= dest_x < 49:
            out[dest_x, py] = pixels[px, py]
    if glyph.startswith("extra_"):
        # These six secondary-page drawings are all final consonants for
        # combined vowels. The contact sheet places them at the top of the
        # cell, while Jak's full-cell overlay needs them in the bottom third.
        lowered = Image.new("RGBA", crop.size, (0, 0, 0, 0))
        lowered.alpha_composite(crop, (0, 31))
        crop = lowered
    if not crop.getchannel("A").getbbox():
        raise RuntimeError(f"No pixels found for {glyph}")
    return crop


def jak1_catalog_glyph(glyph: str) -> Image.Image:
    """Fit the few Jak 2/3 contextual forms that collide in Jak 1's 16 px cell."""
    if glyph == "0x21":
        # The narrow native double-siot form leaves room for the right-side
        # vowel in syllables such as 쌓 without merging their strokes.
        source = catalog_glyph("0x4c").resize((41, 64), Image.Resampling.LANCZOS)
        fitted = Image.new("RGBA", (49, 64), (0, 0, 0, 0))
        fitted.alpha_composite(source)
        return fitted
    if glyph == "0x89":
        # This contextual ㅜ drawing already contains a final ㄴ. Jak 1 also
        # overlays the separately selected final, producing two ㄴs in 분.
        return catalog_glyph("0x85")
    source = catalog_glyph(glyph)
    if glyph == "0x08":
        # Keep ㄴ's top aligned with ㅣ and extend its baseline downward.
        # Translating the whole glyph hides its upper stroke in 니.
        return source.resize((49, 96), Image.Resampling.LANCZOS).crop((0, 0, 49, 64))
    if glyph in ("0xa2", "0xa4"):
        # The ㅗ half of ㅘ meets the leading consonant too high in Jak 1's
        # compact cell. Lower both the final and no-final contextual forms.
        fitted = Image.new("RGBA", source.size, (0, 0, 0, 0))
        fitted.alpha_composite(source, (0, 6))
        return fitted
    if glyph == "extra_0x86":
        # Combined-vowel final ㄹ overlaps the vowel's lower stroke in 훨.
        # Five source pixels move it below the vowel without clipping.
        fitted = Image.new("RGBA", source.size, (0, 0, 0, 0))
        fitted.alpha_composite(source, (0, 5))
        return fitted
    if glyph == "extra_0x8a":
        # The combined-vowel final ㅆ sits two atlas rows above the other
        # finals. Six source pixels lower reaches the baseline without clipping.
        fitted = Image.new("RGBA", source.size, (0, 0, 0, 0))
        fitted.alpha_composite(source, (0, 6))
        return fitted
    if glyph == "0x5c":
        # The ㅔ form used by 헤 overlaps the right side of ㅎ at Jak 1 size.
        # Move it right within the 49-pixel source cell before downsampling.
        fitted = Image.new("RGBA", source.size, (0, 0, 0, 0))
        fitted.alpha_composite(source, (6, 0))
        return fitted
    if glyph == "0x68":
        # The two ㄱ forms have only a one-pixel gap in the source. Open that
        # gap and shorten their stems so they stay distinct from ㅜ/ㅡ at
        # subtitle scale in 꾼, 끈, and 끝.
        separated = Image.new("RGBA", source.size, (0, 0, 0, 0))
        separated.alpha_composite(source.crop((0, 0, 20, 64)), (0, 0))
        separated.alpha_composite(source.crop((20, 0, 44, 64)), (25, 0))
        upper = separated.crop((0, 6, 49, 29)).resize((49, 18), Image.Resampling.LANCZOS)
        fitted = Image.new("RGBA", source.size, (0, 0, 0, 0))
        fitted.alpha_composite(upper, (0, 2))
        return fitted
    if glyph == "0xe1":
        # The source cell for final ㄴ includes one stray row from its neighbor.
        # Remove that row so it cannot cross the leading consonant in 꾼.
        fitted = source.copy()
        fitted.paste((0, 0, 0, 0), (0, 0, fitted.width, 1))
        return fitted
    if glyph == "0x93":
        # The wide contextual ㄷ starts too far right when paired with ㅝ.
        fitted = Image.new("RGBA", source.size, (0, 0, 0, 0))
        fitted.alpha_composite(source, (-6, 0))
        return fitted
    return source


def draw_atlas(source: Path, destination: Path, mapping: dict[str, int], small: bool, high_page: bool) -> None:
    cell_w, cell_h = (8, 12) if small else (16, 24)
    content_w, content_h = (7, 11) if small else (15, 23)
    original = load_original_atlas(source, destination, 128 if small else 256)
    output = Image.new("RGBA", (original.width * 2, original.height), (0, 0, 0, 0))
    output.alpha_composite(original, (0, 0))
    for glyph, payload in mapping.items():
        if bool(payload & 0x80) != high_page:
            continue
        mask = payload & 0x7F
        slot = mask - 1
        col, row = slot % 15, slot // 15
        x = original.width + 1 + col * cell_w
        y = 1 + row * cell_h
        source_glyph = jak1_catalog_glyph(glyph)
        source_glyph = source_glyph.resize((content_w, content_h), Image.Resampling.LANCZOS)
        # Lanczos can leave nearly transparent ringing around the strokes.
        # Removing only those tiny values prevents isolated specks without
        # losing the useful anti-aliasing on the glyph edge.
        alpha = source_glyph.getchannel("A").point(lambda value: 0 if value < 12 else value)
        source_glyph.putalpha(alpha)
        output.alpha_composite(source_glyph, (x, y))
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.exists():
        with Image.open(destination) as existing:
            current = existing.convert("RGBA")
        if current.size == output.size and ImageChops.difference(current, output).getbbox() is None:
            return
    output.save(destination, optimize=True)


def write_atlases(mapping: dict[str, int]) -> None:
    for name in ("ascii.12lo.png", "ascii.12hi.png", "ascii.24lo.png", "ascii.24hi.png"):
        source = SOURCE_DIR / name
        draw_atlas(source, OUTPUT_DIR / name, mapping, ".12" in name, "hi" in name)


def main() -> None:
    chars = korean_syllables()
    required_glyphs = required_jamo_glyphs(chars)
    glyphs = all_jamo_glyphs()
    mapping = make_mapping(glyphs)
    write_text_if_changed(
        MAP_FILE,
        json.dumps({glyph: f"02 {payload:02x}" for glyph, payload in mapping.items()}, ensure_ascii=False, indent=2)
        + "\n",
    )
    write_goal_tables(mapping)
    write_atlases(mapping)
    print(
        f"Generated all {len(glyphs)} composited jamo glyphs "
        f"({len(required_glyphs)} currently used by {len(chars)} syllables, capacity {len(AVAILABLE_PAYLOADS)})."
    )


if __name__ == "__main__":
    main()
