from pathlib import Path

import json
import os
import subprocess
import shutil

# assume this is available/importable
from utils import get_ref_path_from_filename

BASE_DIR = "./"
OUT_DIR = os.path.join(BASE_DIR, "decompiler_out", "jakx")
REF_BASE = os.path.join(BASE_DIR, "test", "decompiler", "reference")


def run_decomp(name: str):
    env = os.environ.copy()
    env["FILE"] = name
    env["GAME"] = "jakx"

    subprocess.run(
        ["task", "decomp-file"],
        env=env,
        cwd=BASE_DIR,
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )


def copy_to_ref(name: str):
    src = os.path.join(OUT_DIR, f"{name}_disasm.gc")

    if not os.path.exists(src):
        print("couldn't copy to refs, file not found")
        return  # or raise

    ref_path = get_ref_path_from_filename("jakx", name, REF_BASE)

    # ensure directory exists
    os.makedirs(os.path.dirname(ref_path), exist_ok=True)

    # enforce renamed filename
    dst = os.path.join(os.path.dirname(ref_path), f"{name}_REF.gc")

    shutil.copyfile(src, dst)


todo_path = Path("./scripts/gsrc/_decomp-plan-todo.json")

with todo_path.open("r", encoding="utf-8") as f:
    entries = json.load(f)

for i, file_name in enumerate(entries):
    print(f"[{i}/{len(entries)}] {file_name}")
    run_decomp(file_name)
    copy_to_ref(file_name)
