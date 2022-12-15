import re
import argparse
from utils import get_gsrc_path_from_filename

parser = argparse.ArgumentParser("lint-gsrc-file")
parser.add_argument("--game", help="The name of the game", type=str)
parser.add_argument("--file", help="The name of the file", type=str)
args = parser.parse_args()

throw_error = False

# TODO - if more code is added here overtime, be smarter about this / group errors
method_split_pattern = re.compile('method-of-(?:type|object)')
function_split_pattern = re.compile('\(t9-\d+(?:\s+[^\s]+\s*)?\)')
missing_res_tag_pattern = re.compile('.pcpyud')
decompiler_error_pattern = re.compile(';; ERROR')
missing_arg = re.compile('local-vars.*[at].*\s+none\)')
casting_stack_var = re.compile('the-as\s+[^\s]*\s+.*\(new \'stack')

src_path = get_gsrc_path_from_filename(args.game, args.file)

print("Linting GOAL_SRC File...")

with open(src_path) as f:
  for lineno, line in enumerate(f):
    method_split_match = method_split_pattern.search(line)
    if method_split_match:
      print("method_split - {}:{}".format(src_path, lineno + 1))
      throw_error = True
      continue
    function_split_match = function_split_pattern.search(line)
    if function_split_match:
      print("function_split - {}:{}".format(src_path, lineno + 1))
      throw_error = True
      continue
    missing_res_tag_match = missing_res_tag_pattern.search(line)
    if missing_res_tag_match:
      print("missing_res_tag - {}:{}".format(src_path, lineno + 1))
      throw_error = True
      continue
    decompiler_error_match = decompiler_error_pattern.search(line)
    if decompiler_error_match:
      print("decompiler_error - {}:{}".format(src_path, lineno + 1))
      throw_error = True
      continue
    missing_arg_match = missing_arg.search(line)
    if missing_arg_match:
      print("missing_arg - {}:{}".format(src_path, lineno + 1))
      throw_error = True
      continue
    casting_stack_var_match = casting_stack_var.search(line)
    if casting_stack_var_match:
      print("casting stack var - {}:{}".format(src_path, lineno + 1))
      throw_error = True
      continue

if throw_error:
  print("Found potential problems, exiting with code 1!")
  exit(1)
else:
  print("Looks good!")
