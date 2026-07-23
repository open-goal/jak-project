#!/usr/bin/env python3

import difflib
import os
import re
import sys
import termios
import tty
from dataclasses import dataclass
from io import StringIO


OLD_FILE = "ir2-jak3-test_ir2.asm"
NEW_FILE = "ir2-jakx-test_ir2.asm"


# ============================================================================
# Colors
# ============================================================================

class Color:
    RESET = "\033[0m"
    RED = "\033[31m"
    GREEN = "\033[32m"
    YELLOW = "\033[33m"
    CYAN = "\033[36m"
    DIM = "\033[2m"
    BOLD = "\033[1m"


def color(text, c):
    return f"{c}{text}{Color.RESET}"


# ============================================================================
# Data Structures
# ============================================================================

@dataclass
class Operation:
    number: int
    asm: str
    mnemonic: str
    normalized: str


@dataclass
class Function:
    name: str
    stack_total: int
    operations: list[Operation]
    has_errors: bool = False


# ============================================================================
# Parsing
# ============================================================================

FUNC_RE = re.compile(r";\s*\.function\s+(.+)")
STACK_RE = re.compile(r";stack:\s*total\s+(0x[0-9a-fA-F]+|\d+)")
OP_RE = re.compile(r"^\s*(.*?)\s*;;\s*\[\s*(\d+)\]")
ERROR_RE = re.compile(r";;\s*ERROR:\s*")


REGISTER_RE = re.compile(
    r"\b("
    r"r0|at|v[01]|a[0-3]|t[0-9]|s[0-7]|gp|sp|fp|ra|hi|lo"
    r")\b"
)


def normalize_registers(asm):

    mapping = {}
    next_id = 0

    def repl(match):

        nonlocal next_id

        reg = match.group(1)

        if reg not in mapping:
            mapping[reg] = f"R{next_id}"
            next_id += 1

        return mapping[reg]

    return REGISTER_RE.sub(repl, asm)


def parse_file(path):

    functions = {}

    current = None

    with open(path) as f:

        for line in f:

            m = FUNC_RE.search(line)

            if m:

                if current:
                    functions[current.name] = current

                current = Function(
                    name=m.group(1),
                    stack_total=0,
                    operations=[],
                )

                continue


            if current is None:
                continue


            if ERROR_RE.search(line):
                current.has_errors = True


            m = STACK_RE.search(line)

            if m:
                current.stack_total = int(m.group(1), 0)
                continue


            m = OP_RE.match(line)

            if m:

                asm = m.group(1).rstrip()

                current.operations.append(
                    Operation(
                        number=int(m.group(2)),
                        asm=asm,
                        mnemonic=asm.split()[0] if asm else "",
                        normalized=normalize_registers(asm),
                    )
                )


    if current:
        functions[current.name] = current


    return functions


# ============================================================================
# Analysis
# ============================================================================

def similarity(a, b):
    return difflib.SequenceMatcher(None, a, b).ratio()


def is_decompiled(func):
    return not func.has_errors


def compare_exact(old, new):

    return (
        len(old.operations) == len(new.operations)
        and all(
            a.asm == b.asm
            for a, b in zip(
                old.operations,
                new.operations
            )
        )
    )


def classify(old, new):

    if compare_exact(old, new):
        return "IDENTICAL"


    mnemonic_score = similarity(
        [x.mnemonic for x in old.operations],
        [x.mnemonic for x in new.operations],
    )

    normalized_score = similarity(
        [x.normalized for x in old.operations],
        [x.normalized for x in new.operations],
    )


    if normalized_score > .995:
        return "REGISTER CHANGES"

    if mnemonic_score > .98:
        return "NEAR MATCH"

    if mnemonic_score > .75:
        return "MODIFIED"

    return "DIFFERENT"


CLASS_COLOR = {
    "IDENTICAL": Color.GREEN,
    "REGISTER CHANGES": Color.GREEN,
    "NEAR MATCH": Color.YELLOW,
    "MODIFIED": Color.YELLOW,
    "DIFFERENT": Color.RED,
}


CLASS_ORDER = {
    "DIFFERENT": 0,
    "MODIFIED": 1,
    "NEAR MATCH": 2,
    "REGISTER CHANGES": 3,
    "IDENTICAL": 4,
}


# ============================================================================
# Diff
# ============================================================================

def print_diff(old, new, out):

    matcher = difflib.SequenceMatcher(
        None,
        [x.normalized for x in old.operations],
        [x.normalized for x in new.operations],
    )


    print("Diff:\n", file=out)


    for tag, i1, i2, j1, j2 in matcher.get_opcodes():

        if tag == "equal":
            continue


        print(
            color(
                f"@@ {tag} @@",
                Color.CYAN
            ),
            file=out
        )


        if tag in ("replace", "delete"):

            for op in old.operations[i1:i2]:

                print(
                    color(
                        f"- [{op.number:4}] {op.asm}",
                        Color.RED
                    ),
                    file=out
                )


        if tag in ("replace", "insert"):

            for op in new.operations[j1:j2]:

                print(
                    color(
                        f"+ [{op.number:4}] {op.asm}",
                        Color.GREEN
                    ),
                    file=out
                )


        print(file=out)



def analyze(old, new):

    out = StringIO()

    def p(*args):
        print(*args, file=out)


    classification = classify(old,new)


    p("=" * 100)
    p(old.name)
    p("=" * 100)
    p()


    p(
        "Stack:"
    )

    stack_match = old.stack_total == new.stack_total

    p(f"  old : 0x{old.stack_total:x}")
    p(f"  new : 0x{new.stack_total:x}")
    p(
        " ",
        color(
            "MATCH" if stack_match else "DIFFERENT",
            Color.GREEN if stack_match else Color.RED
        )
    )

    p()


    p("Operations:")
    p(f"  old : {len(old.operations)}")
    p(f"  new : {len(new.operations)}")
    p(f"  diff: {len(new.operations)-len(old.operations):+}")


    mnemonic = similarity(
        [x.mnemonic for x in old.operations],
        [x.mnemonic for x in new.operations],
    )

    normalized = similarity(
        [x.normalized for x in old.operations],
        [x.normalized for x in new.operations],
    )


    p()

    p("Similarity:")
    p(f"  Exact Assembly      : {compare_exact(old,new)}")
    p(f"  Mnemonic            : {mnemonic:.3%}")
    p(f"  Register Normalized : {normalized:.3%}")


    p()

    p(
        "Classification:",
        color(
            classification,
            CLASS_COLOR[classification]
        )
    )


    if classification != "IDENTICAL":

        p()

        print_diff(
            old,
            new,
            out
        )


    return out.getvalue()



# ============================================================================
# Pager
# ============================================================================

def clear_screen():

    os.system(
        "clear" if os.name != "nt"
        else "cls"
    )


def read_key():

    fd = sys.stdin.fileno()

    old = termios.tcgetattr(fd)

    try:

        tty.setraw(fd)

        key = sys.stdin.read(1)

        if key == "\x1b":
            key += sys.stdin.read(2)

        return key

    finally:

        termios.tcsetattr(
            fd,
            termios.TCSADRAIN,
            old
        )



# ============================================================================
# Main
# ============================================================================

old_functions = parse_file(OLD_FILE)
new_functions = parse_file(NEW_FILE)


functions = []

for name in set(old_functions) & set(new_functions):

    old = old_functions[name]
    new = new_functions[name]

    functions.append(
        (
            old,
            new,
            analyze(old,new)
        )
    )


functions.sort(
    key=lambda x:
        (
            1 if is_decompiled(x[0]) else 0,
            CLASS_ORDER.get(
                classify(x[0],x[1]),
                99
            ),
            x[0].name
        )
)


show_decompiled = False
index = 0


while True:

    visible = [
        f for f in functions
        if show_decompiled or not is_decompiled(f[0])
    ]


    if not visible:
        break


    index = min(index, len(visible)-1)


    clear_screen()


    print(
        color(
            f"Functions: {len(functions)} | "
            f"Review: {len(functions)-sum(is_decompiled(x[0]) for x in functions)} | "
            f"Decompiled: {sum(is_decompiled(x[0]) for x in functions)}",
            Color.CYAN
        )
    )

    print()


    for i,(old,new,_) in enumerate(visible):

        cls = classify(old,new)

        marker = "> " if i == index else "  "

        print(
            marker
            + f"{old.name:<40}"
            + color(
                cls,
                CLASS_COLOR[cls]
            )
        )


    print()
    print(visible[index][2])


    print(
        color(
            "[↑↓/jk] move  [d] show decompiled  [q] quit",
            Color.DIM
        )
    )


    key = read_key()


    if key.lower() == "q":
        break

    elif key in ("\x1b[A","k","K"):
        index = max(index-1,0)

    elif key in ("\x1b[B","j","J"):
        index = min(index+1,len(visible)-1)

    elif key.lower() == "d":
        show_decompiled = not show_decompiled
        index = 0