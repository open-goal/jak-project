# load test.gc into a list of string
with open("test.gc", "r") as file:
    lines = file.readlines()

new_lines = []
line_no = 0
while line_no < len(lines):
    line = lines[line_no]
    if ":methods" in line:
        # keep iterating until we find a line with only a closing paren `)`
        line_no += 1
        states = []
        methods = []
        while line_no < len(lines):
            method_entry = lines[line_no]
            if ")" == method_entry.strip():
                if len(states) > 0:
                  new_lines.append("  (:state-methods\n")
                  for state in states:
                    new_lines.append(state)
                  new_lines.append("    )\n")
                if len(methods) > 0:
                  new_lines.append("  (:methods\n")
                  for method in methods:
                    new_lines.append(method)
                  new_lines.append(method_entry.split(")")[0] + ")\n")
                break
            else:
              if ":state" in method_entry and not ":states" in method_entry:
                  if "()" in method_entry.split(";")[0]:
                      states.append("    {}\n".format(method_entry.strip().split(" ")[0].replace("(", "").replace(")", "")))
                  else:
                      states.append("    ({} {})\n".format(method_entry.strip().split(" ")[0].replace("(", "").replace(")", ""), method_entry.strip().split(" ")[1].replace("(", "").replace(")", "")))
              else:
                  methods.append(method_entry)
            line_no += 1
    else:
        new_lines.append(line)
    line_no += 1

# write contents back out
with open("test.gc", "w") as file:
    file.writelines(new_lines)
