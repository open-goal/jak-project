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
    )


def copy_to_ref(name: str):
    src = os.path.join(OUT_DIR, f"{name}_disasm.gc")

    if not os.path.exists(src):
        return  # or raise

    ref_path = get_ref_path_from_filename("jakx", name, REF_BASE)

    # ensure directory exists
    os.makedirs(os.path.dirname(ref_path), exist_ok=True)

    # enforce renamed filename
    dst = os.path.join(os.path.dirname(ref_path), f"{name}_REF.gc")

    shutil.copyfile(src, dst)


with open("./scripts/gsrc/decomp-plan.json", "r") as f:
    data = json.load(f)

for entry in data:
    name = entry.get("name")
    done = entry.get("done")

    if done and name:
        run_decomp(name)
        copy_to_ref(name)
        # exit(1)
