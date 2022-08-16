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
# - block quotes padding amount includes the included lines
# - there are likely ways to make this more efficient

import re
from rapidfuzz import fuzz


class CommentMeta:
    def __init__(self):
        self.data = ""
        self.symbol_before = None
        self.symbol_after = None
        self.symbol_padding_before = None
        self.symbol_padding_after = None
        # NOTE - maybe holding more than just 1 line before/after might help?
        self.code_before = None
        self.code_after = None
        self.code_padding_before = None
        self.code_padding_after = None
        self.line_num_in_form = None  # None == top level
        self.containing_form = None  # none - top level
        self.inline = False
        self.code_in_line = None  # only for inline comments
        self.line_in_file = None  # a worst-case scenario fallback

    def __str__(self):
        return "{}:{}:{}".format(self.data, self.symbol_before, self.symbol_after)


# TODO - make this work for multiple files
comments = []

# returns (symbol | None, padding)
def backtrack_for_symbol(lines, index):
    padding = 0
    for i in range(index - 1, 0, -1):
        tline = lines[i].strip()
        matches = re.search(
            r"(?:define|define-extern|defun|defstate|deftype)\s+([^\s]*)\s", tline
        )
        if matches is not None:
            return matches.group(1), padding
        elif (
            not tline.strip() == "" and not tline.strip().startswith(";")
        ) or "decomp begins" in tline.lower():
            # we hit a non empty line (but it wasn't a symbol!)
            return None, padding
        elif tline.strip() == "":
            padding = padding + 1
    return None, padding


def lookahead_for_symbol(lines, index):
    padding = 0
    for i in range(index + 1, len(lines), 1):
        tline = lines[i].lstrip()
        matches = re.search(
            r"(?:define|define-extern|defun|defstate|deftype)\s+([^\s]*)\s", tline
        )
        if matches is not None:
            return matches.group(1), padding
        elif (
            not tline.strip() == "" and not tline.strip().startswith(";")
        ) or "decomp begins" in tline.lower():
            # we hit a non empty line (but it wasn't a symbol!)
            return None, padding
        elif tline.strip() == "":
            padding = padding + 1
    return None, padding


def backtrack_for_code(lines, index):
    padding = 0
    for i in range(index - 1, 0, -1):
        line = lines[i]
        if line.strip() == "":
            padding = padding + 1
            continue
        elif "decomp begins" in line.lower():
            return None, padding
        elif line.lstrip().startswith(";"):
            continue
        return line, padding


def lookahead_for_code(lines, index):
    padding = 0
    for i in range(index + 1, len(lines), 1):
        line = lines[i]
        if line.strip() == "":
            padding = padding + 1
            continue
        elif "decomp begins" in line.lower():
            return None, padding
        elif line.lstrip().startswith(";"):
            continue
        return line, padding


# returns form, or none
def is_line_start_of_form(line):
    matches = re.search(r"\(\s*([^\s.]*)\s+", line)
    if matches is not None:
        return line
    return None


debug_lines = []


def debug_nice_formatted_code(val):
    if val is None:
        return None
    return val.strip()[0:20]


def has_form_ended(stack, line):
    # if the stack is empty, return true
    for char in line:
        if char == "(":
            stack.append(char)
        elif char == ")":
            stack.pop()
            if len(stack) == 0:
                return True
    return False


with open("./goal_src/jak2/kernel/gkernel.gc") as f:
    lines_temp = f.readlines()
    lines = []
    # Get rid of debug lines, this is so i can re-run without having to reset the file
    for line in lines_temp:
        if "[DEBUG]" in line:
            continue
        lines.append(line)
    # track if we are inside a define*/defun/defmethod/deftype/defstate
    within_form = None
    line_num_in_form = None
    form_paren_stack = []
    found_output = False
    i = 0
    while i < len(lines):
        debug_lines.append(lines[i])
        tline = lines[i].lstrip()
        if "decomp begins" in tline.lower():
            found_output = True
            i = i + 1
            continue
        if not found_output:
            i = i + 1
            continue
        # actually process code
        if within_form is None:
            # lets see if we are now in one
            within_form = is_line_start_of_form(lines[i])
            if within_form is not None:
                line_num_in_form = 0
                if has_form_ended(form_paren_stack, lines[i]):
                    within_form = None
                    form_paren_stack = []
        elif within_form is not None:
            # check if the form has ended by counting parens
            if has_form_ended(form_paren_stack, lines[i]):
                within_form = None
                form_paren_stack = []
                line_num_in_form = 0
            else:
                line_num_in_form = line_num_in_form + 1

        if tline.startswith(";"):
            # treat decomp deviation blocks as essentially comments as well, so include them in a block comment if appropriate
            # this is done because there is nothing to match them against (if a comment is inside them for example)
            # so we have to copy them in full
            in_deviation_block = False
            if "decomp deviation" in tline.lower():
                in_deviation_block = True
            current_comment = CommentMeta()
            current_comment.line_in_file = i
            current_comment.data = lines[i]
            (
                current_comment.symbol_before,
                current_comment.symbol_padding_before,
            ) = backtrack_for_symbol(lines, i)
            (
                current_comment.symbol_after,
                current_comment.symbol_padding_after,
            ) = lookahead_for_symbol(lines, i)
            (
                current_comment.code_before,
                current_comment.code_padding_before,
            ) = backtrack_for_code(lines, i)
            (
                current_comment.code_after,
                current_comment.code_padding_after,
            ) = lookahead_for_code(lines, i)
            current_comment.containing_form = within_form
            current_comment.line_num_in_form = line_num_in_form
            current_comment.inline = False
            # look ahead to handle block comments
            if i + 1 < len(lines):
                next_line = lines[i + 1]
            if "decomp deviation" in next_line.lower():
                in_deviation_block = False
            while i + 1 < len(lines) and (
                in_deviation_block or next_line.lstrip().startswith(";")
            ):
                debug_lines.append(lines[i + 1])
                i = i + 1
                current_comment.data = current_comment.data + next_line
                if i + 1 < len(lines):
                    next_line = lines[i + 1]
                if "decomp deviation" in next_line.lower():
                    in_deviation_block = False
            comments.append(current_comment)
            debug_lines.append(
                ";; [DEBUG]: sym - {}:{} | {}:{} || code - {}...:{} | {}...:{}\n".format(
                    current_comment.symbol_before,
                    current_comment.symbol_padding_before,
                    current_comment.symbol_after,
                    current_comment.symbol_padding_after,
                    debug_nice_formatted_code(current_comment.code_before),
                    current_comment.code_padding_before,
                    debug_nice_formatted_code(current_comment.code_after),
                    current_comment.code_padding_after,
                )
            )
            debug_lines.append(
                ";; [DEBUG]: in_form - {}...:{}\n".format(
                    debug_nice_formatted_code(current_comment.containing_form),
                    current_comment.line_num_in_form,
                )
            )
        # inline comments
        # TODO - cleanup duplication
        elif ";" in tline:
            current_comment = CommentMeta()
            current_comment.data = ";" + tline.split(";")[1]
            (
                current_comment.symbol_before,
                current_comment.symbol_padding_before,
            ) = backtrack_for_symbol(lines, i)
            (
                current_comment.symbol_after,
                current_comment.symbol_padding_after,
            ) = lookahead_for_symbol(lines, i)
            (
                current_comment.code_before,
                current_comment.code_padding_before,
            ) = backtrack_for_code(lines, i)
            (
                current_comment.code_after,
                current_comment.code_padding_after,
            ) = lookahead_for_code(lines, i)
            current_comment.containing_form = within_form
            current_comment.line_num_in_form = line_num_in_form
            current_comment.inline = True
            current_comment.code_in_line = tline.split(";")[0]
            comments.append(current_comment)
            debug_lines.append(
                ";; [DEBUG]: sym - {}:{} | {}:{} || code - {}...:{} | {}...:{}\n".format(
                    current_comment.symbol_before,
                    current_comment.symbol_padding_before,
                    current_comment.symbol_after,
                    current_comment.symbol_padding_after,
                    debug_nice_formatted_code(current_comment.code_before),
                    current_comment.code_padding_before,
                    debug_nice_formatted_code(current_comment.code_after),
                    current_comment.code_padding_after,
                )
            )
            debug_lines.append(
                ";; [DEBUG]: in_form - {}...:{} || inline_code - {}...\n".format(
                    debug_nice_formatted_code(current_comment.containing_form),
                    current_comment.line_num_in_form,
                    debug_nice_formatted_code(current_comment.code_in_line),
                )
            )
        i = i + 1


# TODO - gate this behind a flag, debug mode, prints out info in the file itself next to the comments
# with open("./goal_src/jak2/kernel/gkernel.gc", "w") as f:
#     f.writelines(debug_lines)
# exit(0)

# Step 2: Cleanup the decomp output

lines_to_ignore = [
    ";;-*-Lisp-*-",
    "(in-package goal)",
    ";; definition",
    ";; INFO:",
    ";; failed to figure",
    ";; Used lq/sq",
]

decomp_lines = []


def should_ignore_line(line):
    for ignore_line in lines_to_ignore:
        if line.lower().startswith(ignore_line.lower()):
            return True
    return False


with open("./decompiler_out/jak2/gkernel_disasm.gc") as f:
    lines = f.readlines()
    for line in lines:
        if should_ignore_line(line):
            continue
        decomp_lines.append(line)

# Step 3: Start merging the new code + comments
final_lines = []
decomp_started = False


def get_symbol_at_line(line):
    tline = line.strip()
    matches = re.search(
        r"(?:define|define-extern|defun|defstate|deftype)\s+([^\s]*)\s", tline
    )
    if matches is not None:
        return matches.group(1)
    return None


def relevant_symbol_comments_for_line_before(line, within_form):
    if within_form is not None:
        return []
    symbol = get_symbol_at_line(line)
    if symbol is None:
        return []
    # Loop through comments, finding any that match the symbol
    # they WILL be placed, so we can remove them from our list now
    i = 0
    relevant_comments = []
    while i < len(comments):
        comment = comments[i]
        if comment.symbol_after == symbol:
            relevant_comments.append(comment)
            comments.pop(i)
        else:
            i = i + 1
    return relevant_comments


def padding_before_comment(comment):
    if comment.containing_form is None:
        if comment.symbol_after is not None:
            return "\n" * comment.symbol_padding_after
    return ""


def padding_after_comment(comment):
    if comment.containing_form is None:
        if comment.symbol_before is not None:
            return "\n" * comment.symbol_padding_before
    return ""


def relevant_symbol_comments_for_line_after(line, within_form):
    if within_form is not None:
        return []
    symbol = get_symbol_at_line(line)
    if symbol is None:
        return []
    # Loop through comments, finding any that match the symbol
    # they WILL be placed, so we can remove them from our list now
    i = 0
    relevant_comments = []
    while i < len(comments):
        comment = comments[i]
        # if we can, we prefer to put comments before not after (more accurate re-creation)
        if comment.symbol_after is None and comment.symbol_before == symbol:
            relevant_comments.append(comment)
            comments.pop(i)
        else:
            i = i + 1
    return relevant_comments


# the first half of the defmethod/etc lines (before arg list) is less likely to change
# so we want to split it to weight it more heavily
def split_def_line(line):
    first_part = ""
    second_part = ""
    for index, char in enumerate(line):
        if char == "(":
            if index == 0:
                first_part = first_part + char
            else:
                second_part = second_part + char
            continue
        else:
            if second_part != "":
                second_part = second_part + char
            else:
                first_part = first_part + char
    return first_part, second_part


def get_relevant_form_comments(form_def_line):
    threshold = 90.0
    relevant_comments = []
    code_def_part, code_rest = split_def_line(form_def_line)
    # TODO - what if there is no rest!
    i = 0
    while i < len(comments):
        comment = comments[i]
        if comment.containing_form is None:
            i = i + 1
            continue
        comment_def_part, comment_rest = split_def_line(comment.containing_form)
        def_score = fuzz.ratio(code_def_part, comment_def_part) * 0.65
        rest_score = fuzz.ratio(code_rest, comment_rest) * 0.35

        if def_score + rest_score >= threshold:
            relevant_comments.append(comment)
            comments.pop(i)
        else:
            i = i + 1
    return relevant_comments


with open("./goal_src/jak2/kernel/gkernel.gc") as f:
    lines = f.readlines()
    within_form = None
    line_num_in_form = None
    form_paren_stack = []
    for line in lines:
        if "[DEBUG]" in line:
            continue
        if line.lower().lstrip().startswith(";; decomp begins"):
            decomp_started = True
            final_lines.append(line)
            break
        if not decomp_started:
            final_lines.append(line)
            continue

    i = 0
    while i < len(decomp_lines):
        line = decomp_lines[i]
        # Otherwise, its a part of the output we have to be more careful about
        # For every line in the decompiled output, we scan our comment list to see if anything matches
        # if it does, we insert it appropriately and remove the comment from the list
        #
        # This is the main source of inefficiency, but the process gets progressively faster as comments are eliminated
        if within_form is None:
            # lets see if we are now in one
            within_form = is_line_start_of_form(line)
            # TODO - check line for symbol matches?
            if within_form is not None:
                line_num_in_form = 0
                if has_form_ended(form_paren_stack, line):
                    within_form = None
                    form_paren_stack = []
                else:
                    # Get all of the lines of the form at once
                    form_start = decomp_lines[i]
                    form_lines = [form_start]
                    while i < len(decomp_lines):
                        i = i + 1
                        line = decomp_lines[i]
                        if has_form_ended(form_paren_stack, line):
                            within_form = None
                            form_paren_stack = []
                            break
                        else:
                            form_lines.append(line)
                    # Add any comments needed to the form contents
                    # - first we get all comments that have match well with the form's start line (ie. defmethod ....)
                    form_comments = get_relevant_form_comments(form_start)
                    # - for each comment, let's find which line matches it the best,
                    # if NONE exceed the threshold (if both match the same, pick the first), we default to the line offset
                    highest_score = -1
                    index_to_insert = -1
                    threshold = 50.0
                    place_comment_after = True
                    for comment in form_comments:
                        for index, form_line in enumerate(form_lines):
                            # skip any comments that were previously added
                            if form_line.lstrip().startswith(";"):
                                continue
                            if comment.code_before is not None:
                                score = fuzz.ratio(form_line, comment.code_before)
                                if score >= threshold and score > threshold:
                                    index_to_insert = index
                                    highest_score = score
                                    place_comment_after = True
                            if comment.code_after is not None:
                                score = fuzz.ratio(form_line, comment.code_after)
                                if score >= threshold and score > threshold:
                                    index_to_insert = index
                                    highest_score = score
                                    place_comment_after = False
                        # add the comment!
                        if index_to_insert == -1:
                            form_lines.insert(comment.line_num_in_form, comment.data)
                        elif place_comment_after:
                            form_lines.insert(
                                index_to_insert,
                                padding_before_comment(comment) + comment.data,
                            )
                        else:
                            form_lines.insert(
                                index_to_insert,
                                padding_after_comment(comment) + comment.data,
                            )
                    # Add the lines to the final output
                    for form_line in form_lines:
                        final_lines.append(form_line)

        # Otherwise, we are at the top-level!
        if within_form is None:
            # TODO - handle non-symbol associated comments at the top level
            before_comments = relevant_symbol_comments_for_line_before(
                line, within_form
            )
            for comment in before_comments:
                final_lines.append(padding_before_comment(comment) + comment.data)
            final_lines.append(line)
            after_comments = relevant_symbol_comments_for_line_after(line, within_form)
            for comment in after_comments:
                final_lines.append(padding_after_comment(comment) + comment.data)
        # TODO - test comments above defuns!
        i = i + 1


# Step 4: Write it out
with open("./goal_src/jak2/kernel/gkernel.gc", "w") as f:
    f.writelines(final_lines)
