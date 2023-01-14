import re
import argparse
from utils import get_gsrc_path_from_filename
from colorama import just_fix_windows_console, Fore, Back, Style

just_fix_windows_console()

parser = argparse.ArgumentParser("lint-gsrc-file")
parser.add_argument("--game", help="The name of the game", type=str)
parser.add_argument("--file", help="The name of the file", type=str)
args = parser.parse_args()


class LintMatch:
    def __init__(self, src_path, offending_lineno, context):
        self.src_path = src_path
        self.offending_lineno = offending_lineno
        self.context = context

    def __str__(self):
        output = (
            Style.BRIGHT
            + Fore.MAGENTA
            + "@ {}:{}\n".format(self.src_path, self.offending_lineno)
            + Fore.RESET
            + Style.RESET_ALL
        )
        for line in self.context:
            # skip lines that are just brackets
            if line.strip() == ")" or line.strip() == "(":
                continue
            output += "\t{}\n".format(line)
        return output


class LinterRule:
    def __init__(self, level, rule_name, regex_pattern, context_size):
        self.level = level
        self.rule_name = rule_name
        self.regex_pattern = regex_pattern
        self.context_size = context_size
        self.matches = []

    def __str__(self):
        level_color = Fore.LIGHTBLUE_EX
        if self.level == "WARN":
            level_color = Fore.YELLOW
        elif self.level == "ERROR":
            level_color = Fore.RED
        return (
            level_color
            + "[{}]{} - {} - {}/{}/g".format(
                self.level,
                Fore.RESET,
                level_color + self.rule_name + Fore.RESET,
                Fore.CYAN,
                self.regex_pattern.pattern,
            )
            + Fore.RESET
            + ":"
        )


# Construct all rules
linter_rules = []
# Infos
# Warnings
linter_rules.append(
    LinterRule("WARN", "method_splits", re.compile("method-of-(?:type|object)"), 3)
)
linter_rules.append(
    LinterRule("WARN", "func_splits", re.compile("\(t9-\d+(?:\s+[^\s]+\s*)?\)"), 3)
)
linter_rules.append(
    LinterRule("WARN", "missing_arg", re.compile("local-vars.*[at].*\s+none\)"), 1)
)
# Errors
linter_rules.append(LinterRule("ERROR", "missing_res_tag", re.compile(".pcpyud"), 1))
linter_rules.append(LinterRule("ERROR", "decomp_error", re.compile(";; ERROR"), 1))
linter_rules.append(
    LinterRule(
        "ERROR", "casting_stack_var", re.compile("the-as\s+[^\s]*\s+.*\(new 'stack"), 2
    )
)

src_path = get_gsrc_path_from_filename(args.game, args.file)

# Iterate through the file line by line, check against each rule
# if the rule is violated (it matches) then we append the match with useful details

print("Linting GOAL_SRC File...")


def get_context(lines, match_span, idx, amount_inclusive):
    lines_grabbed = []
    # Strip left pad, while maintaining indent
    last_line_indent_width = -1
    last_line_indent = -1
    while len(lines_grabbed) < amount_inclusive and len(lines) > idx + len(
        lines_grabbed
    ):
        # TODO - first line, colorize the match
        # if len(lines_grabbed) == 0:
        #   line = lines[idx + len(lines_grabbed)]
        #   line = line[:match_span[0]] + Back.RED + line[:match_span[1]] + Back.RESET + line[match_span[1]:]
        #   line = line.rstrip()
        line = lines[idx + len(lines_grabbed)].rstrip()
        indent_width = len(line) - len(line.lstrip())
        if last_line_indent_width == -1:
            lines_grabbed.append(line.lstrip())
        elif last_line_indent == -1:
            # calculate the difference
            indent_diff = indent_width - last_line_indent_width
            last_line_indent = indent_diff
            stripped_line = line.lstrip()
            lines_grabbed.append(stripped_line.rjust(indent_diff + len(stripped_line)))
        else:
            stripped_line = line.lstrip()
            lines_grabbed.append(
                stripped_line.rjust(last_line_indent + len(stripped_line))
            )
        last_line_indent_width = indent_width
    return lines_grabbed


with open(src_path) as f:
    src_lines = f.readlines()
    for lineno, line in enumerate(src_lines):
        adjusted_lineno = lineno + 1
        for rule in linter_rules:
            match = rule.regex_pattern.search(line)
            if match:
                rule.matches.append(
                    LintMatch(
                        src_path,
                        adjusted_lineno,
                        get_context(src_lines, match.span(), lineno, rule.context_size),
                    )
                )

# Iterate through all our linter rules, printing nicely in groups with the
# context surrounding the match
#
# If we find any violations at warning or above, we will ultimately return exit(1)
throw_error = False
for rule in linter_rules:
    # Iterate through violations
    if len(rule.matches) > 0:
        print(rule)
        for match in rule.matches:
            if rule.level == "ERROR" or rule.level == "WARN":
                throw_error = True
            print(match)

if throw_error:
    print(Fore.RED + "Found potential problems, exiting with code 1!" + Fore.RESET)
    exit(1)
else:
    print(Fore.GREEN + "Looks good!" + Fore.RESET)
