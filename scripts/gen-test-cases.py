import re

# Quick and dirty script to generate decompiler test cases from text file format

with open("test-cases.txt") as f:
    content = f.readlines()
content = [x.strip() for x in content]

test_cases = {}

# TODO - there is a bug in this code if we add test-cases with multiple lists of registers.
# currently, its going to split too much breaking fragile assumptions.  Fix when required

for case in content:
  if not case:
    continue

  args = re.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", case)
  assembly_lines = args[0].replace("\\n\"", "").replace("\"", "").strip().split("\\n")

  instruction_summary = ""
  instructions = []
  for line in assembly_lines:
    if not re.match("^L\d*:\s*$", line):
      instructions.append(line.strip().split(" ")[0].upper().replace(".", ""))
  instruction_summary = "_".join(instructions)

  assembly_lines = "{{{}}}".format(", ".join(["\"{}\"".format(x.replace("\\n", "").strip()) for x in assembly_lines]))
  output_lines = args[1].replace("\\n", "").strip()
  write_regs = "{{{}}}".format(args[2].replace("\\n", "").strip().replace("\"\"", "").replace(" ", "\",\""))
  read_regs = "{{{}}}".format(args[3].replace("\\n", "").strip().replace("\"\"", "").replace(" ", "\",\""))
  clob_regs = "{{{}}}".format(args[4].replace("\\n", "").strip().replace("\"\"", "").replace(" ", "\",\""))

  test_case = "test_case(assembly_from_list({}), {}, {}, {}, {});".format(assembly_lines, output_lines, write_regs, read_regs, clob_regs);

  if instruction_summary in test_cases:
    test_cases[instruction_summary].append(test_case)
  else:
    test_cases[instruction_summary] = []
    test_cases[instruction_summary].append(test_case)

import os
if os.path.exists("test-cases.cpp"):
  os.remove("test-cases.cpp")

with open("test-cases.cpp", "a") as f:
  instructions = test_cases.keys()
  instructions = sorted(instructions)
  for instr in instructions:
    f.write("TEST(DecompilerAtomicOpBuilder, {}) {{".format(instr))
    for case in test_cases[instr]:
      f.write(case)
    f.write("}\n\n")

