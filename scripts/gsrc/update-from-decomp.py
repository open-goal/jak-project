# Updates a gsrc file from it's respective decompiled output
# Tries it's very best to:
# - retain all comments (100% of them)
# - place them in either the exact same spot, or roughly the same spot
# - retain decomp deviations (marked with ;; decomp deviation comment guards) and try to place them back
#   - this will also automate the process of knowing if a file was modified or not if actually used
#
# This is done with a variety of heuristics and can obviously only be so
# accurate on unstructured data like code
#
# Assumes this is ran from the root dir of the repository

# Overview of the process:
# - Collect all comments, associate as much metadata with them as possible so we can reposition them in the new output
# - Potential metadata that will help (from most to least accurate)
#   - symbol it is directly before/after (symbol names don't change, so this is VERY accurate (ie. a function name / type name))
#   - forms/variable names it is directly before/after (these may change, might be the whole reason you are decompiling again!)
#   - line number into a form (may also change, but independent of output)
#   - containing form (if all else fails, we can atleast put it _near_ where it should go)
# Update the decompilation
# Attempt to add back all of the comment lines
# - adjust indentation if needed
#
# 1. Get metadata from original file so we can reconstruct it
# 2. Cleanup disasm file (get rid of most comments, etc)
# 3. Add back all comment blocks to modified original file
# 4. hope for the best...

# Known Issues:
# - use defuns as symbols (consistent names), but account for padding properly
# - padding after decomp deviation blocks / blocks in general is wrong
# - blocks starting inline (ie '(define foo 'bar) #|start of a block comment that continues on...)
# - decomp deviation blocks inside forms can cause paren counting issues
# -
# - codes a mess, as one would probably expect for something as miserable as this, it needs a refactor
# - there are likely ways to make this more efficient

import argparse
from code_retention.all_types_retention import update_alltypes_named_blocks
from code_retention.code_retention import is_line_start_of_form, has_form_ended
from utils import get_gsrc_path_from_filename

parser = argparse.ArgumentParser("update-from-decomp")
parser.add_argument("--game", help="The name of the game", type=str)
parser.add_argument("--file", help="The name of the file", type=str)
parser.add_argument(
    "--debug", help="Output debug metadata on every block", action="store_true"
)
parser.add_argument(
    "--clearDebug", help="Clear debug metadata", action="store_true"
)  # TODO - implement!
args = parser.parse_args()

gsrc_path = get_gsrc_path_from_filename(args.game, args.file)

# Step 1 - Find and update all named blocks from all-types (useful for enums)
update_alltypes_named_blocks(args.game)

comments = []
debug_lines = []
decomp_ignore_forms = ["defmethod inspect"]
decomp_ignore_errors = False

with open(gsrc_path) as f:
    lines_temp = f.readlines()
    lines = []
    # Get rid of debug lines, this is so i can re-run without having to reset the file
    for line in lines_temp:
        if "[DEBUG]" in line:
            continue
        # Check for comment annotate overrides / settings, this is the "nicest" place to shove this
        if "og:ignore-errors" in line and "true" in line:
            decomp_ignore_errors = True
        if "og:ignore-form" in line:
            decomp_ignore_forms.append(line.partition("ignore-form:")[2].strip())
        lines.append(line)

if args.debug:
    with open(gsrc_path, "w") as f:
        f.writelines(debug_lines)
    exit(0)

# Step 2: Cleanup the decomp output

lines_to_ignore = [
    ";;-*-Lisp-*-",
    "(in-package goal)",
    ";; definition",
    ";; INFO:",
    ";; failed to figure",
    ";; Used lq/sq",
    ";; this part is debug only",
    ";; WARN: Return type mismatch int vs none",
    ";; WARN: Stack slot offset",
]

if decomp_ignore_errors:
    lines_to_ignore.append(";; ERROR:")
    lines_to_ignore.append(";; WARN:")

decomp_lines = []
# cache all form definition lines from the incoming decompilation
# this way, we can "quickly" figure out which form is the most relevant
decomp_form_def_lines = []


def should_ignore_line(line):
    for ignore_line in lines_to_ignore:
        if line.lower().startswith(ignore_line.lower()):
            return True
    return False


# TODO - ignore brackets inside strings!

decomp_file_path = "./decompiler_out/{}/{}_disasm.gc".format(args.game, args.file)
with open(decomp_file_path) as f:
    lines = f.readlines()
    i = 0
    decomp_form_paren_stack = []
    decomp_within_form = None
    while i < len(lines):
        line = lines[i]
        if should_ignore_line(line):
            i = i + 1
            continue
        decomp_within_form = is_line_start_of_form(line)
        # Check if we should ignore the form
        if decomp_within_form is not None:
            # See if we should skip it
            skip_form = False
            for form_to_ignore in decomp_ignore_forms:
                if form_to_ignore in decomp_within_form:
                    skip_form = True
                    break
            if has_form_ended(decomp_form_paren_stack, line):
                decomp_within_form = None
                decomp_form_paren_stack = []
                if not skip_form:
                    decomp_lines.append(line)
                i = i + 1
            else:
                if not skip_form:
                    decomp_form_def_lines.append(decomp_within_form)
                    decomp_lines.append(line)
                while i + 1 < len(lines):
                    i = i + 1
                    line = lines[i]
                    if not skip_form and not should_ignore_line(line):
                        decomp_lines.append(line)
                    if has_form_ended(decomp_form_paren_stack, line):
                        decomp_within_form = None
                        decomp_form_paren_stack = []
                        i = i + 1
                        break
        else:
            decomp_lines.append(line)
            i = i + 1


# Step 3: Start merging the new code + comments
final_lines = []
with open(gsrc_path) as f:
    lines = f.readlines()
    for line in lines:
        final_lines.append(line)
        if line.lower().startswith(";; decomp begins"):
            break
    for line in decomp_lines:
        final_lines.append(line)

# Step 4.a: Remove excessive new-lines from the end of the output, only leave a single empty new-line
lines_to_ignore = 0
i = len(final_lines) - 1
while i > 0 and (final_lines[i] == "\n" or final_lines[i] == "0\n"):
    lines_to_ignore = lines_to_ignore + 1
    i = i - 1

print("ignoring - {}".format(lines_to_ignore))

# Step 4.b: Write it out
with open(gsrc_path, "w") as f:
    i = 0
    while i + lines_to_ignore < len(final_lines):
        f.write(final_lines[i])
        i = i + 1
