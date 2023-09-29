# Gets the diff between this PR and `origin/master`
# If there are any 'og:preserve-this' lines removed, the script returns 1

import unidiff
from colorama import Fore

try:
    with open("lint-changes.diff", encoding="utf-8") as f:
        diff = f.read()
except:
    print("Unable to read lint-changes.diff, exiting without failing, must be some odd changes in the diff!.")
    exit(0)

patch_set = unidiff.PatchSet.from_string(diff)

flagged_deletions = []

for patched_file in patch_set:
    file_path = patched_file.path
    for hunk in patched_file:
        for line in hunk:
            if line.is_removed and "og:preserve-this" in line.value.strip():
                flagged_deletions.append(
                    {
                        "file": file_path,
                        "line_num": line.source_line_no,
                        "deletion": line.value.strip(),
                    }
                )

print(flagged_deletions)

if len(flagged_deletions) > 0:
    print(
        Fore.RED
        + "Flagged goal_src code has been deleted, either you made a mistake or you better know what you're doing!" + Fore.RESET
    )
    for flagged_deletion in flagged_deletions:
        print(
            "  - {}{}{}:{}{}{} - {}{}{}".format(
                Fore.CYAN,
                flagged_deletion["file"],
                Fore.RESET,
                Fore.GREEN,
                flagged_deletion["line_num"],
                Fore.RESET,
                Fore.RED,
                flagged_deletion["deletion"],
                Fore.RESET,
            )
        )
    print(Fore.RESET)
    exit(1)
print("No flagged goal_src code was deleted (hopefully!)")
exit(0)
