#!/usr/bin/env python3
"""Generate and install one mips2c function from an object file."""

import argparse
import json
import re
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GAMES = {"jak1": "jak1", "jak2": "jak2", "jak3": "jak3", "jakx": "jakx"}


def matching_brace(text, open_index):
    depth = 0
    for index in range(open_index, len(text)):
        if text[index] == "{":
            depth += 1
        elif text[index] == "}":
            depth -= 1
            if depth == 0:
                return index
    raise ValueError("Unmatched opening brace")


def add_function_to_hacks(path, function):
    text = path.read_text()
    match = re.search(r'"mips2c_functions_by_name"\s*:\s*\[', text)
    if not match:
        raise ValueError(f"No mips2c_functions_by_name array in {path}")
    start = match.end() - 1
    depth = 0
    end = None
    for i in range(start, len(text)):
        if text[i] == "[":
            depth += 1
        elif text[i] == "]":
            depth -= 1
            if depth == 0:
                end = i
                break
    array_text = text[start : end + 1]
    if re.search(r'"' + re.escape(function) + r'"', array_text):
        return
    close = end
    prior = text[:close].rstrip()
    comma = "" if prior.endswith("[") or prior.endswith(",") else ","
    text = text[:close] + f'{comma}\n    "{function}"\n  ' + text[close:]
    path.write_text(text)


def namespace_for_goal_function(function):
    method = re.fullmatch(r"\(method\s+(\d+)\s+([^)]+)\)", function)
    if method:
        return f"method_{method.group(1)}_{method.group(2).replace('-', '_')}"
    return function.replace("-", "_").replace("!", "").replace("<", "").replace(">", "")


def extract_generated_function(output_path, function):
    text = output_path.read_text()
    markers = list(re.finditer(r";;-\*-MIPS2C-Start-\*-\s*\n(.*?)\n;;-\*-MIPS2C-End-\*-", text, re.S))
    if not markers:
        raise ValueError(f"No generated MIPS2C output found in {output_path}")
    wanted = namespace_for_goal_function(function)
    for marker in markers:
        block = marker.group(1).strip() + "\n"
        namespace = re.search(r"namespace Mips2C::\w+\s*\{\s*namespace (\w+)\s*\{", block)
        if namespace and wanted == namespace.group(1):
            return namespace.group(1), block
    raise ValueError(f"No generated C++ namespace matching {function!r} in {output_path}")


def stack_size_for_function(block):
    adjustments = list(re.finditer(
        r"//\s*(?:daddiu|addiu)\s+sp,\s*sp,\s*(-?\d+)|"
        r"//\s*daddu\s+sp,\s*sp,\s*r0\b",
        block,
    ))
    if not adjustments:
        raise ValueError("Could not determine stack adjustment from generated MIPS2C function")

    last_adjustment = adjustments[-1]
    adjustment_value = last_adjustment.group(1)
    used_bytes = abs(int(adjustment_value)) if adjustment_value else 0
    target = used_bytes * 2
    if target <= 256:
        return 256
    lower = 1 << (target.bit_length() - 1)
    upper = lower * 2
    # On an exact tie, choose the larger size so the stack allocation is safe.
    return max(256, upper if target - lower >= upper - target else lower)


def upsert_cpp(path, namespace, block):
    path.parent.mkdir(parents=True, exist_ok=True)
    text = path.read_text() if path.exists() else ""
    target = re.compile(
        r"namespace Mips2C::\w+\s*\{\s*namespace " + re.escape(namespace) + r"\s*\{"
    )
    match = target.search(text)
    replacement = block.rstrip() + "\n"
    if match:
        start = text.rfind("//--------------------------MIPS2C", 0, match.start())
        if start < 0:
            start = text.rfind("\n", 0, match.start()) + 1
        function_end = text.find("\n} // namespace Mips2C", match.end())
        if function_end < 0:
            raise ValueError(f"Could not find end of existing namespace {namespace} in {path}")
        function_end += len("\n} // namespace Mips2C")
        next_function = text.find("//--------------------------MIPS2C", function_end)
        end = next_function if next_function >= 0 else len(text)
        text = text[:start] + replacement + text[end:]
    else:
        text = text.rstrip() + ("\n\n" if text.strip() else "") + replacement
    path.write_text(text)


def update_table(path, game, source_file, namespace):
    text = path.read_text()
    # Add declaration inside the game's namespace, if absent.
    game_ns = re.search(r"namespace " + re.escape(game) + r"\s*\{", text)
    if not game_ns:
        raise ValueError(f"No namespace for {game} in {path}")
    ns_open = text.index("{", game_ns.start())
    ns_close = matching_brace(text, ns_open)
    decl = f"namespace {namespace} {{ extern void link(); }}"
    if decl not in text[ns_open:ns_close]:
        text = text[:ns_close] + "\n" + decl + text[ns_close:]

    # Locate the game's callback map by its section label and insert/update its object row.
    label = {"jak1": "JAK 1", "jak2": "JAK 2", "jak3": "JAK 3", "jakx": "JAK X"}[game]
    section = re.search(r"//+\s*" + re.escape(label), text)
    if not section:
        raise ValueError(f"No {label} callback section in {path}")
    next_section = re.search(r"//+\s*JAK (?:1|2|3|X)", text[section.end():])
    section_end = section.end() + next_section.start() if next_section else text.index("};", section.end())
    segment = text[section.end():section_end]
    row = re.search(r'\{"' + re.escape(source_file) + r'"\s*,\s*\{([^}]*)\}\}', segment)
    callback = f"{game}::{namespace}::link"
    if row:
        callbacks = row.group(1)
        if callback not in callbacks:
            insertion = row.start(1) + len(callbacks.rstrip())
            comma = ", " if callbacks.strip() else ""
            segment = segment[:insertion] + comma + callback + segment[insertion:]
    else:
        initializer = segment.find("{{")
        if initializer < 0:
            raise ValueError(f"Could not find callback map initializer for {game}")
        insertion = initializer + 2
        segment = segment[:insertion] + '\n     {"' + source_file + '", {' + callback + '}},' + segment[insertion:]
    text = text[:section.end()] + segment + text[section_end:]
    path.write_text(text)


def remove_cpp_function(path, game, namespace):
    text = path.read_text()
    target = re.compile(
        r"namespace Mips2C::" + re.escape(game) + r"\s*\{\s*namespace " +
        re.escape(namespace) + r"\s*\{"
    )
    removed = 0
    while True:
        match = target.search(text)
        if not match:
            break
        start = text.rfind("//--------------------------MIPS2C", 0, match.start())
        if start < 0:
            start = text.rfind("\n", 0, match.start()) + 1
        function_end = text.find("\n} // namespace Mips2C", match.end())
        if function_end < 0:
            raise ValueError(f"Could not find end of {namespace} in {path}")
        function_end += len("\n} // namespace Mips2C")
        next_function = text.find("//--------------------------MIPS2C", function_end)
        end = next_function if next_function >= 0 else len(text)
        text = text[:start] + text[end:]
        removed += 1
    if removed:
        path.write_text(text)
    return removed


def remove_table_references(path, game, namespace):
    text = path.read_text()
    game_ns = re.search(r"namespace " + re.escape(game) + r"\s*\{", text)
    if not game_ns:
        raise ValueError(f"No namespace for {game} in {path}")
    ns_open = text.index("{", game_ns.start())
    ns_close = matching_brace(text, ns_open)
    declaration = re.compile(
        r"^\s*namespace " + re.escape(namespace) + r"\s*\{\s*extern void link\(\);\s*\}\s*\n?",
        re.M,
    )
    text = text[:ns_open + 1] + declaration.sub("", text[ns_open + 1:ns_close]) + text[ns_close:]

    label = {"jak1": "JAK 1", "jak2": "JAK 2", "jak3": "JAK 3", "jakx": "JAK X"}[game]
    section = re.search(r"//+\s*" + re.escape(label), text)
    if not section:
        raise ValueError(f"No {label} callback section in {path}")
    next_section = re.search(r"//+\s*JAK (?:1|2|3|X)", text[section.end():])
    section_end = section.end() + next_section.start() if next_section else text.index("};", section.end())
    segment = text[section.end():section_end]
    row_pattern = re.compile(r'\s*\{"([^"\n]+)"\s*,\s*\{([^}]*)\}\}\s*,?')
    callback = f"{game}::{namespace}::link"
    rows = list(row_pattern.finditer(segment))
    for row in reversed(rows):
        callbacks = [item.strip() for item in row.group(2).split(",") if item.strip()]
        remaining = [item for item in callbacks if item != callback]
        if len(remaining) == len(callbacks):
            continue
        if remaining:
            body_start, body_end = row.span(2)
            segment = segment[:body_start] + ", ".join(remaining) + segment[body_end:]
        else:
            segment = segment[:row.start()] + segment[row.end():]
    text = text[:section.end()] + segment + text[section_end:]
    path.write_text(text)


def remove_function(game, function):
    namespace = namespace_for_goal_function(function)
    cpp_folder = ROOT / "game/mips2c" / f"{game}_functions"
    removed_from = []
    if cpp_folder.exists():
        for cpp in cpp_folder.glob("*.cpp"):
            if remove_cpp_function(cpp, game, namespace):
                removed_from.append(cpp)
    remove_table_references(
        ROOT / "game/mips2c/mips2c_table.cpp", game, namespace
    )
    return namespace, removed_from


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--game", required=True, choices=GAMES)
    parser.add_argument("--file", help="Object file name, with or without .gc")
    parser.add_argument("--function", required=True, help="GOAL function name")
    parser.add_argument(
        "--remove", action="store_true", help="Remove this function without decompiling"
    )
    parser.add_argument(
        "--cpp-file",
        help="C++ file under game/mips2c/<game>_functions to append to (defaults to <object>.cpp)",
    )
    parser.add_argument("--version", default="ntsc_v1")
    parser.add_argument("--decompiler", help="Path to the decompiler executable")
    args = parser.parse_args()

    if args.remove:
        namespace, removed_from = remove_function(args.game, args.function)
        files = ", ".join(str(path) for path in removed_from) or "no C++ definition found"
        print(f"Removed {args.function} ({namespace}): {files}; updated mips2c_table.cpp")
        return
    if not args.file:
        parser.error("--file is required unless --remove is used")

    filename = Path(args.file).stem
    game = args.game
    hacks = ROOT / "decompiler/config" / game / args.version / "hacks.jsonc"
    if not hacks.exists():
        raise SystemExit(f"Hacks config not found: {hacks}")
    decompiler = args.decompiler
    if not decompiler:
        candidates = [ROOT / "out/build/Release/bin/decompiler", ROOT / "build/decompiler/decompiler"]
        decompiler = next((str(p) for p in candidates if p.exists()), None)
    if not decompiler:
        raise SystemExit("Decompiler not found; pass --decompiler")

    add_function_to_hacks(hacks, args.function)
    override = json.dumps(
        {"decompile_code": True, "levels_extract": False, "allowed_objects": [filename]}
    )
    subprocess.run([decompiler, str(ROOT / "decompiler/config" / game / f"{game}_config.jsonc"),
                    str(ROOT / "iso_data"), str(ROOT / "decompiler_out"), "--version", args.version,
                    "--config-override", override], cwd=ROOT, check=True)

    output = ROOT / "decompiler_out" / game / f"{filename}_ir2.asm"
    namespace, block = extract_generated_function(output, args.function)
    stack_size = stack_size_for_function(block)
    block = block.replace("PUT_STACK_SIZE_HERE", str(stack_size))
    cpp_filename = Path(args.cpp_file).name if args.cpp_file else f"{filename}.cpp"
    if not cpp_filename.endswith(".cpp"):
        cpp_filename += ".cpp"
    cpp = ROOT / "game/mips2c" / f"{game}_functions" / cpp_filename
    upsert_cpp(cpp, namespace, block)
    update_table(ROOT / "game/mips2c/mips2c_table.cpp", game, filename, namespace)
    print(f"Updated {cpp} and mips2c_table.cpp for {args.function}")


if __name__ == "__main__":
    main()
