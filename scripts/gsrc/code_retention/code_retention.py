import re

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
