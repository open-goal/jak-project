import glob
from pathlib import Path
import subprocess
import time
from colorama import just_fix_windows_console, Fore, Back, Style

just_fix_windows_console()

files = glob.glob("./goal_src/jak1/**/*.gc", recursive=True)
total_ms = 0
for file in files:
    start_time = time.perf_counter()
    subprocess.run(["./out/build/Release/bin/formatter", "--write", "--file", file])
    elapsed_ms = (time.perf_counter() - start_time) * 1000
    total_ms = total_ms + elapsed_ms
    print(f"Formatted .../{Path(file).stem} in {Fore.CYAN} {elapsed_ms:.2f}ms {Fore.RESET}")

print(f"In total that took {total_ms}ms for {len(files)} files!")

subprocess.run(["git", "diff", "--shortstat", "origin/master"])
