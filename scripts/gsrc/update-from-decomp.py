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
# - there are likely ways to make this more efficient
# - codes a mess, as one would probably expect for something as miserable as this
# - use defuns as symbols (consistent names), but account for padding properly
# - padding after decomp deviation blocks / blocks in general is wrong
# - blocks starting inline (ie '(define foo 'bar) #|start of a block comment that continues on and on...)

# Potential Improvements:
# - decomp deviation with meta - specify a form to ignore

import re
from rapidfuzz import fuzz


class CommentMeta:
    def __init__(self):
        self.data = ""
        self.symbol_before = None
        self.symbol_inline = None
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
        self.containing_form_kind = None  # function|method|behaviour
        self.containing_form_func_name = None  # or the method/behaviour
        self.containing_form_type = None
        self.inline = False
        self.code_in_line = None  # only for inline comments
        self.line_in_file = None  # a worst-case scenario fallback

    def __str__(self):
        return "{}:{}:{}".format(self.data, self.symbol_before, self.symbol_after)


# TODO - make this work for multiple files
comments = []
debug_lines = []


def debug_nice_formatted_code(val):
    if val is None:
        return None
    return val.strip()[0:20]


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


def symbol_on_line(line):
    tline = line.lstrip()
    matches = re.search(
        r"(?:define|define-extern|defun|defstate|deftype)\s+([^\s]*)\s", tline
    )
    if matches is not None:
        return matches.group(1)
    return None


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


def has_form_ended(stack, line):
    # if the stack is empty, return true
    line_before_comment = line.partition(";")[0]
    for char in line_before_comment:
        if char == "(":
            stack.append(char)
        elif char == ")":
            if len(stack) == 0:
                # unbalanced parens?
                return True
            stack.pop()
            if len(stack) == 0:
                return True
        else:
            if len(stack) == 0:
                return True
    return False


def append_form_metadata(comment, form_start_line):
    func_matches = re.search(r"\(defun(?:-debug)? ([^\s]*)", form_start_line)
    if func_matches is not None:
        comment.containing_form_kind = "function"
        comment.containing_form_func_name = func_matches.group(1)
        comment.containing_form_type = None
        return
    behavior_matches = re.search(
        r"\((?:defbehavior) ([^\s]*) ([^\s]*)", form_start_line
    )
    if behavior_matches is not None:
        comment.containing_form_kind = "behavior"
        comment.containing_form_func_name = behavior_matches.group(1)
        comment.containing_form_type = behavior_matches.group(2)
        return
    method_matches = re.search(r"\((?:defmethod) ([^\s]*) ([^\s]*)", form_start_line)
    if method_matches is not None:
        comment.containing_form_kind = "method"
        comment.containing_form_func_name = method_matches.group(1)
        comment.containing_form_type = method_matches.group(2)
        return
    comment.containing_form_kind = "unknown"
    comment.containing_form_func_name = None
    comment.containing_form_type = None


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
        if "*kernel-context*" in tline:
          print("ye")
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
                if "kernel-dispatcher" in within_form:
                  print("ye")
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

        if tline.startswith(";") or tline.startswith("#|"):
            # treat decomp deviation blocks as essentially comments as well, so include them in a block comment if appropriate
            # this is done because there is nothing to match them against (if a comment is inside them for example)
            # so we have to copy them in full
            in_deviation_block = False
            if "decomp deviation" in tline.lower() or tline.startswith("#|"):
                in_deviation_block = True
            current_comment = CommentMeta()
            current_comment.line_in_file = i
            current_comment.data = lines[i]
            (
                current_comment.symbol_before,
                current_comment.symbol_padding_before,
            ) = backtrack_for_symbol(lines, i)
            (
                current_comment.code_before,
                current_comment.code_padding_before,
            ) = backtrack_for_code(lines, i)
            current_comment.containing_form = within_form
            if within_form is not None:
                append_form_metadata(current_comment, within_form)
            current_comment.line_num_in_form = line_num_in_form
            current_comment.inline = False
            # look ahead to handle block comments
            if i + 1 < len(lines):
                next_line = lines[i + 1]
            if "decomp deviation" in next_line.lower() or next_line.startswith("|#"):
                in_deviation_block = False
            while i + 1 < len(lines) and (
                in_deviation_block or next_line.lstrip().startswith(";") or next_line.lstrip().startswith("|#")
            ):
                debug_lines.append(lines[i + 1])
                i = i + 1
                current_comment.data = current_comment.data + next_line
                if i + 1 < len(lines):
                    next_line = lines[i + 1]
                if "decomp deviation" in next_line.lower() or next_line.startswith("|#"):
                    in_deviation_block = False
            (
                current_comment.symbol_after,
                current_comment.symbol_padding_after,
            ) = lookahead_for_symbol(lines, i)
            (
                current_comment.code_after,
                current_comment.code_padding_after,
            ) = lookahead_for_code(lines, i)
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
            current_comment.line_in_file = i
            current_comment.data = ";" + tline.partition(";")[2]
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
            if within_form is not None:
                append_form_metadata(current_comment, within_form)
            current_comment.line_num_in_form = line_num_in_form
            current_comment.symbol_inline = symbol_on_line(tline)
            current_comment.inline = True
            current_comment.code_in_line = tline.partition(";")[0]
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


def relevant_symbol_comments_for_line_before(line):
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


def relevant_symbol_comments_for_inline(line):
    symbol = get_symbol_at_line(line)
    if symbol is None:
        return []
    # Loop through comments, finding any that match the symbol
    # they WILL be placed, so we can remove them from our list now
    i = 0
    relevant_comments = []
    while i < len(comments):
        comment = comments[i]
        if comment.symbol_inline == symbol:
            relevant_comments.append(comment)
            comments.pop(i)
        else:
            i = i + 1
    return relevant_comments


def padding_after_comment(comment):
    if comment.containing_form is None:
        if comment.symbol_before is not None:
            return "\n" * comment.symbol_padding_before
    return ""


def relevant_symbol_comments_for_line_after(line):
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


def get_form_metadata(form_def_line):
    func_matches = re.search(r"\(defun(?:-debug)? ([^\s]*)", form_def_line)
    if func_matches is not None:
        return "function", func_matches.group(1), None
    behavior_matches = re.search(r"\((?:defbehavior) ([^\s]*) ([^\s]*)", form_def_line)
    if behavior_matches is not None:
        return "behavior", behavior_matches.group(1), behavior_matches.group(2)
    method_matches = re.search(r"\((?:defmethod) ([^\s]*) ([^\s]*)", form_def_line)
    if method_matches is not None:
        return "method", method_matches.group(1), method_matches.group(2)
    return "unknown", None, None


built_in_method_names = [
    "new",
    "delete",
    "print",
    "inspect",
    "length",
    "asize-of",
    "copy",
    "relocate",
    "memusage",
]


def different_method_names(form_func_name, comment_form_func_name):
    if (
        comment_form_func_name not in built_in_method_names
        and form_func_name not in built_in_method_names
    ):
        return False
    return form_func_name != comment_form_func_name


def get_most_relevant_containing_form(form_def_line, decomp_lines):
    form_kind, form_func_name, form_type = get_form_metadata(form_def_line)
    # TODO - what if there is no rest!
    code_def_part, code_rest = split_def_line(form_def_line)
    threshold = 50.0
    highest_score = -1
    highest_form = ""
    for comment in comments:
        if comment.containing_form is None:
            continue
        if comment.containing_form_kind != "unknown":
            if comment.containing_form_kind != form_kind:
                continue
            elif (
                form_kind == "function"
                and comment.containing_form_func_name != form_func_name
            ):
                continue
            elif (
                form_kind == "behavior"
                and comment.containing_form_func_name != form_func_name
            ):
                continue
            elif form_kind == "method" and (
                comment.containing_form_type != form_type
                or different_method_names(
                    form_func_name, comment.containing_form_func_name
                )
            ):
                continue
        comment_def_part, comment_rest = split_def_line(comment.containing_form)
        def_score = fuzz.ratio(code_def_part, comment_def_part) * 0.65
        if def_score == 65.0 and form_kind != "unknown":
            return comment.containing_form.strip()
        rest_score = fuzz.ratio(code_rest, comment_rest) * 0.35
        combined_score = def_score + rest_score
        if combined_score >= threshold and combined_score > highest_score:
            highest_score = combined_score
            highest_form = comment.containing_form.strip()
    if highest_score == -1:
        return None
    # Now check that there is no better form in existance.  If there is, then that means this form has NO comments
    # because we were able to find a better match (the above loop only checks form def lines associated with comments)
    for line in decomp_lines:
        within_form = is_line_start_of_form(line)
        if within_form is not None:
            line_form_kind, line_form_func_name, line_form_type = get_form_metadata(
                within_form
            )
            if form_kind != "unknown":
                if form_kind != line_form_kind:
                    continue
                elif form_kind == "function" and line_form_func_name != form_func_name:
                    continue
                elif form_kind == "behavior" and line_form_func_name != form_func_name:
                    continue
                elif form_kind == "method" and (
                    line_form_type != form_type
                    or different_method_names(form_func_name, line_form_func_name)
                ):
                    continue
            def_part, rest = split_def_line(within_form)
            def_score = fuzz.ratio(code_def_part, def_part) * 0.65
            if def_score == 65.0 and form_kind != "unknown":
                return line.strip()
            rest_score = fuzz.ratio(code_rest, rest) * 0.35
            combined_score = def_score + rest_score
            if combined_score > highest_score:
                return None
    return highest_form


def get_relevant_form_comments(form_def_line, decomp_lines):
    if "defun kernel-dispatcher" in form_def_line:
        print("ye")
    relevant_comments = []
    # First, find the most relevant form and use that, not all comments that match closely with
    # an existing form
    best_matching_form = get_most_relevant_containing_form(form_def_line, decomp_lines)
    if best_matching_form is None:
        return relevant_comments
    i = 0
    while i < len(comments):
        comment = comments[i]
        if comment.containing_form is None:
            i = i + 1
            continue
        score = score_alg(comment.containing_form, best_matching_form)
        if score > 90.0:
            relevant_comments.append(comment)
            comments.pop(i)
        else:
            i = i + 1
    return relevant_comments


# Simple fuzz ratio, but removes obvious outliers like empty lines / lines with only a paren
def score_alg(line1, line2):
    tline1 = line1.strip()
    tline2 = line2.strip()
    if tline1 == "" or tline1 == ")" or tline1 == "(":
        return -1
    if tline2 == "" or tline2 == ")" or tline2 == "(":
        return -1
    return fuzz.ratio(tline1, tline2)


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
                    form_comments = get_relevant_form_comments(form_start, decomp_lines)
                    # - for each comment, let's find which line matches it the best,
                    # if NONE exceed the threshold (if both match the same, pick the first), we default to the line offset
                    for comment in form_comments:
                        highest_score = -1
                        index_to_insert = -1
                        threshold = 50.0
                        place_comment_after = True
                        for index, form_line in enumerate(form_lines):
                            # skip any comments that were previously added
                            if form_line.lstrip().startswith(";"):
                                continue
                            if comment.code_in_line is not None:
                                score = score_alg(form_line, comment.code_in_line)
                                if score >= threshold and score > highest_score:
                                    index_to_insert = index
                                    highest_score = score
                            if comment.code_before is not None:
                                score = score_alg(form_line, comment.code_before)
                                if score >= threshold and score > highest_score:
                                    index_to_insert = index
                                    highest_score = score
                                    place_comment_after = True
                            if comment.code_after is not None:
                                score = score_alg(form_line, comment.code_after)
                                if score >= threshold and score > highest_score:
                                    index_to_insert = index
                                    highest_score = score
                                    place_comment_after = False
                        # add the comment!
                        if index_to_insert == -1:
                            if comment.inline:
                                form_lines[comment.line_num_in_form] = (
                                    form_lines[index_to_insert].rstrip()
                                    + " "
                                    + comment.data
                                )
                            else:
                                form_lines.insert(
                                    comment.line_num_in_form, comment.data
                                )
                        elif comment.inline:
                            form_lines[index_to_insert] = (
                                form_lines[index_to_insert].rstrip()
                                + " "
                                + comment.data
                            )
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
            if "*kernel-context*" in line:
                print("ye")
            before_comments = relevant_symbol_comments_for_line_before(line)
            for comment in before_comments:
                final_lines.append(padding_before_comment(comment) + comment.data)
            inline_comments = relevant_symbol_comments_for_inline(line)
            if len(inline_comments) > 0:
                comment_str = ""
                for comment in inline_comments:
                    comment_str = comment_str + comment.data.strip()
                comment_str = comment_str.replace(";", "")
                final_lines.append(
                    "{} ;; {}".format(line.rstrip(), comment_str.strip())
                )
            else:
                final_lines.append(line)
            after_comments = relevant_symbol_comments_for_line_after(line)
            for comment in after_comments:
                final_lines.append(padding_after_comment(comment) + comment.data)
        # next line
        i = i + 1

# Step 3.b: Handle any remaining top level comments
# If we can't find a code line that meets a threshold, default to their line number
# - Why is this done after: if a comment is associated with nothing but code, we have no
#   guarantee where it should go, so we have to wait until all code is populated
# This is SUPER inefficient, so hopefully we've processed nearly all comments by this point
for comment in comments:
    within_form = None
    line_num_in_form = None
    form_paren_stack = []
    found_output = True
    i = 0
    index_to_insert = -1
    highest_score = -1
    place_comment_after = True
    threshold = 50.0
    while i < len(final_lines):
        debug_lines.append(final_lines[i])
        tline = final_lines[i].lstrip()
        if "decomp begins" in tline.lower():
            found_output = True
            i = i + 1
            continue
        if not found_output:
            i = i + 1
            continue
        line = final_lines[i]
        # We can try to claw back a bit of efficiency by skipping the inside of forms
        if within_form is not None:
            # check if the form has ended by counting parens
            if has_form_ended(form_paren_stack, line):
                if comment.code_in_line is not None:
                    score = score_alg(line, comment.code_in_line)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                if comment.code_before is not None:
                    score = score_alg(line, comment.code_before)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                        place_comment_after = True
                if comment.code_after is not None:
                    score = score_alg(line, comment.code_after)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                        place_comment_after = False
                within_form = None
                form_paren_stack = []
                line_num_in_form = 0
            else:
                line_num_in_form = line_num_in_form + 1
        else:
            # lets see if we are now in a form
            within_form = is_line_start_of_form(line)
            if within_form is not None:
                if comment.code_in_line is not None:
                    score = score_alg(line, comment.code_in_line)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                if comment.code_before is not None:
                    score = score_alg(line, comment.code_before)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                        place_comment_after = True
                if comment.code_after is not None:
                    score = score_alg(line, comment.code_after)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                        place_comment_after = False
                line_num_in_form = 0
            else:
                # just normal code, check it
                if comment.code_in_line is not None:
                    score = score_alg(line, comment.code_in_line)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                if comment.code_before is not None:
                    score = score_alg(line, comment.code_before)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                        place_comment_after = True
                if comment.code_after is not None:
                    score = score_alg(line, comment.code_after)
                    if score >= threshold and score > highest_score:
                        index_to_insert = i
                        highest_score = score
                        place_comment_after = False
        i = i + 1
    # end of while loop
    # add the comment!
    if index_to_insert == -1:
        if comment.inline:
            final_lines[comment.line_in_file] = (
                final_lines[comment.line_in_file].rstrip() + " " + comment.data
            )
        else:
            final_lines.insert(comment.line_in_file, comment.data)
    elif comment.inline:
        final_lines[index_to_insert] = (
            final_lines[index_to_insert].rstrip() + " " + comment.data
        )
    elif place_comment_after:
        final_lines.insert(
            index_to_insert,
            padding_before_comment(comment) + comment.data,
        )
    else:
        final_lines.insert(
            index_to_insert,
            padding_after_comment(comment) + comment.data,
        )

# Step 4: Write it out
with open("./goal_src/jak2/kernel/gkernel.gc", "w") as f:
    f.writelines(final_lines)
