import re
from rapidfuzz import fuzz

# TODO - rename and refactor all usages, it's not _always_ a comment anymore!
# RetainedCode or something
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
    if line.lstrip().startswith(";") or "(when *debug-segment*" in line:
        return None
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


def process_original_lines(lines):
    comments = []
    debug_lines = []
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
                in_deviation_block
                or next_line.lstrip().startswith(";")
                or next_line.lstrip().startswith("|#")
            ):
                debug_lines.append(lines[i + 1])
                i = i + 1
                current_comment.data = current_comment.data + next_line
                if i + 1 < len(lines):
                    next_line = lines[i + 1]
                if "decomp deviation" in next_line.lower() or next_line.startswith(
                    "|#"
                ):
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
    return comments, debug_lines


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


def get_relevant_form_comments(form_def_line):
    form_kind, form_func_name, form_type = get_form_metadata(form_def_line)
    code_def_part, code_rest = split_def_line(form_def_line)
    relevant_comments = []
    i = 0
    while i < len(comments):
        comment = comments[i]
        if comment.containing_form is None:
            i = i + 1
            continue
        (
            comment_form_kind,
            comment_form_func_name,
            comment_form_type,
        ) = get_form_metadata(comment.containing_form)
        # First disqualify the form if it's obviously unrelated
        if comment_form_kind != "unknown":
            if form_kind != comment_form_kind:
                i = i + 1
                continue
            elif form_kind == "function" and comment_form_func_name != form_func_name:
                i = i + 1
                continue
            elif form_kind == "behavior" and comment_form_func_name != form_func_name:
                i = i + 1
                continue
            elif form_kind == "method" and (
                comment_form_type != form_type
                or different_method_names(form_func_name, comment_form_func_name)
            ):
                i = i + 1
                continue
        # Evaluate it's score (comments and current def line)
        def_part, rest = split_def_line(comment.containing_form)
        def_score = fuzz.ratio(code_def_part, def_part) * 0.65
        if def_score == 65.0 and form_kind != "unknown":
            relevant_comments.append(comment)
            comments.pop(i)
            continue
        rest_score = fuzz.ratio(code_rest, rest) * 0.35
        combined_score = def_score + rest_score
        threshold = 50.0
        if combined_score < threshold:
            i = i + 1
            continue
        # Now, let's look at ALL other def lines yet to come from the decomp output
        # if any are a better match, don't add the comment yet -- we'll add it when we get there!
        # TODO - remove lines from the list as we find them so speed this up
        found_better_form = False
        for decomp_def_line in decomp_form_def_lines:
            line_form_kind, line_form_func_name, line_form_type = get_form_metadata(
                decomp_def_line
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
                def_part, rest = split_def_line(decomp_def_line)
                def_score = fuzz.ratio(code_def_part, def_part) * 0.65
                if def_score == 65.0 and form_kind != "unknown":
                    found_better_form = True
                    break
                rest_score = fuzz.ratio(code_rest, rest) * 0.35
                if combined_score < def_score + rest_score:
                    found_better_form = True
                    break
            # TODO otherwise? still test?
        if found_better_form:
            i = i + 1
            continue
        relevant_comments.append(comment)
        comments.pop(i)
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


# TODO - improvement on comparison - a higher score on a longer line == better? some sort of weighting approach here too?
def merge_retained_code_and_new_code(gsrc_path, decomp_lines, final_lines):
    decomp_started = False
    with open(gsrc_path) as f:
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
                        for comment in form_comments:
                            highest_score = -1
                            index_to_insert = -1
                            threshold = 50.0
                            place_kind = None
                            for index, form_line in enumerate(form_lines):
                                # skip any comments that were previously added
                                if form_line.lstrip().startswith(";"):
                                    continue
                                if comment.code_in_line is not None:
                                    score = score_alg(form_line, comment.code_in_line)
                                    if score >= threshold and score > highest_score:
                                        index_to_insert = index
                                        highest_score = score
                                        place_kind = "inline"
                                if comment.code_before is not None:
                                    score = score_alg(form_line, comment.code_before)
                                    if score >= threshold and score > highest_score:
                                        index_to_insert = index
                                        highest_score = score
                                        place_comment_after = True
                                        place_kind = "next_line"
                                if comment.code_after is not None:
                                    score = score_alg(form_line, comment.code_after)
                                    if score >= threshold and score > highest_score:
                                        index_to_insert = index
                                        highest_score = score
                                        place_comment_after = False
                                        place_kind = "before_line"
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
                                form_index = index_to_insert
                                if place_kind == "next_line":
                                    form_index = index_to_insert + 1
                                form_lines[form_index] = (
                                    form_lines[form_index].rstrip() + " " + comment.data
                                )
                            elif place_kind == "next_line":
                                form_lines.insert(
                                    index_to_insert + 1,
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


def handle_dangling_blocks(comments, final_lines, debug_lines):
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
                index_to_insert + 1,
                padding_before_comment(comment) + comment.data,
            )
        else:
            final_lines.insert(
                index_to_insert,
                padding_after_comment(comment) + comment.data,
            )
