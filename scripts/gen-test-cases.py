import re

# Quick and dirty script to generate decompiler test cases from text file format

with open("test-cases.txt") as f:
    content = f.readlines()
content = [x.strip() for x in content]

test_cases = {}

for case in content:
  args = re.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", case)
  assembly_lines = args[0].replace("\"", "").strip().split("\\n")
  main_instruction = assembly_lines[0].split(" ")[0]
  if re.match("^L\d*:\s*$", main_instruction):
    main_instruction = assembly_lines[1].strip().split(" ")[0]
  main_instruction = main_instruction.upper().replace(".", "_")
  assembly_lines = "{{{}}}".format(", ".join(["\"{}\"".format(x.replace("\\n", "").strip()) for x in assembly_lines]))
  output_lines = args[1].replace("\\n", "").strip()
  write_regs = "{{{}}}".format(args[2].replace("\\n", "").strip().replace(" ", "\",\""))
  read_regs = "{{{}}}".format(args[3].replace("\\n", "").strip().replace(" ", "\",\""))
  clob_regs = "{{{}}}".format(args[4].replace("\\n", "").strip().replace(" ", "\",\""))

  test_case = "test_case(assembly_from_list({}), {}, {}, {}, {});".format(assembly_lines, output_lines, write_regs, read_regs, clob_regs);

  if main_instruction in test_cases:
    test_cases[main_instruction].append(test_case)
  else:
    test_cases[main_instruction] = []
    test_cases[main_instruction].append(test_case)

with open("test-cases.cpp", "a") as f:
  instructions = test_cases.keys()
  instructions = sorted(instructions)
  for instr in instructions:
    f.write("TEST(DecompilerAtomicOpBuilder, {}) {{".format(instr))
    for case in test_cases[instr]:
      f.write(case)
    f.write("}\n\n")

