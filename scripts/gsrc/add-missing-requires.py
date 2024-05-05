import json
import os
import re

with open('./jak1-missing-requires.json', 'r') as f:
  missing_requires = json.load(f)

from pathlib import Path
from collections import defaultdict
import networkx as nx
import glob

# 1. in `goalc` right now atleast, I can't seem to be able to accurately detect:
# - macro usages
# - virtual state usages
# This is because tracking their definition locations is difficult, due to it coming from
# a macro environment, which is not tracked.
# 
# This can probably be solved, but for now, I'll bite the bullet and statically analyze the code
#
# Find all macro definition locations in `goal_src` as well as all virtual `defstate`s.
# Then we can find all spots where these are used and ensure they are within the missing-require json.
macro_definitions = {}
virtual_state_definitions = {}

# find definitions
files = glob.glob("../../goal_src/jak1/**/*.gc", recursive=True)
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()
        for line_no, line in enumerate(lines):
            if line.lstrip().startswith("(defmacro "):
                name = line.split("(defmacro ")[1].split(" ")[0]
                macro_definitions[name] = file
            elif line.lstrip().startswith("(defstate ") and line_no + 1 < len(lines) and ":virtual #t" in lines[line_no + 1]:
                name = line.split("(defstate ")[1].split(" ")[0]
                proc_name = line.split("(defstate ")[1].split("(")[1].split(")")[0]
                virtual_state_definitions[f"{name}-{proc_name}"] = file

print(len(macro_definitions))
print(len(virtual_state_definitions))

# now look for all usages
for file_no, file in enumerate(files):
    print(f"{file_no}/{len(files)} - {Path(file).stem}")
    with open(file, 'r') as f:
        lines = f.readlines()
        for line_no, line in enumerate(lines):
            if '(go-virtual ' in line:
                state_name = line.split("(go-virtual ")[1].split(" ")[0]
                # find the process name by winding back up the line count until we find:
                # an defstate/defbehavior line
                # an empty line (give up)
                temp_line_no = line_no
                proc_name = ""
                while temp_line_no > 0:
                    temp_line = lines[temp_line_no]
                    if temp_line.lstrip().startswith("(defstate "):
                        proc_name = temp_line.split("(defstate ")[1].split("(")[1].split(")")[0]
                        break
                    elif temp_line.lstrip().startswith("(defbehavior "):
                        proc_name = temp_line.split("(defbehavior ")[1].split(" ")[1]
                        break
                    elif temp_line.strip() == "":
                        break
                    temp_line_no = temp_line_no - 1
                if proc_name != "":
                    # find the state's definition and ensure we require it
                    if f"{name}-{proc_name}" in virtual_state_definitions.keys():
                        state_definition_file = virtual_state_definitions[f"{name}-{proc_name}"]
                        curr_file = Path(file).stem
                        if curr_file not in missing_requires.keys():
                            missing_requires[curr_file] = [state_definition_file]
                        else:
                            missing_requires[curr_file].append(state_definition_file)
            # ... check all macros
            else:
                for macro_name, macro_definition_file in macro_definitions.items():
                    if f"({macro_name} " in line:
                        curr_file = Path(file).stem
                        if curr_file not in missing_requires.keys():
                            missing_requires[curr_file] = [macro_definition_file]
                        else:
                            missing_requires[curr_file].append(macro_definition_file)

# reduce dependencies to just stems because currently files have to be globally unique, so this is fine
# also dedupe them
for file_name, deps in missing_requires.items():
    new_deps = set()
    for dep in deps:
        stem = Path(dep).stem
        if stem != file_name:
            new_deps.add(stem)
    missing_requires[file_name] = list(new_deps)

output_file = "jak1-missing-requires-final.json"
with open(output_file, "w") as f:
    json.dump(missing_requires, f, indent=2)

def create_graph(dependencies):
    graph = nx.DiGraph()
    for key, files in dependencies.items():
        for file in files:
            graph.add_edge(key, Path(file).stem)
    return graph

def transitive_reduction(graph):
    transitive_reduced_graph = nx.transitive_reduction(graph)
    return transitive_reduced_graph

graph = create_graph(missing_requires)
# print(nx.find_cycle(graph)) DEBUGGING CYCLES
reduced_graph = transitive_reduction(graph)

# Convert reduced graph to dictionary
reduced_dependencies = {}
for node, successors in reduced_graph.adjacency():
    reduced_dependencies[node] = list(successors.keys())

output_file = "transitive_reduced_dependencies.json"
with open(output_file, "w") as f:
    json.dump(reduced_dependencies, f, indent=4)

print(f"Transitive reduced dependencies written to {output_file}")

# Figure out packages by looking at .gd files
file_bundles = {}
gd_files = glob.glob("../../goal_src/jak1/**/*.gd", recursive=True)
for file in gd_files:
    with open(file, 'r') as f:
        lines = f.readlines()
        bundle_name = lines[0].replace("(", "").replace(")", "").replace("\"", "").strip()
        for line_no, line in enumerate(lines):
           if line_no == 0:
              continue
           stripped_line = line.replace("(", "").replace(")", "").replace("\"", "").strip()
           if '.o' in stripped_line:
              file_name = stripped_line.replace(".o", "")
              if file_name not in file_bundles.keys():
                file_bundles[file_name] = [f"\"{bundle_name}\""]
              elif f"\"{bundle_name}\"" not in file_bundles[file_name]:
                file_bundles[file_name].append(f"\"{bundle_name}\"")

# Go and append the require statements to the beginning of all files
for file in files:
    with open(file, 'r+') as f:
        lines = f.readlines()
        new_lines = []
        for line in lines:
           if "(in-package " in line:
              new_lines.append(line)
              file_stem = Path(file).stem
              # add in bundle and require statements
              if file_stem in file_bundles.keys():
                new_lines.append(f"(bundles {" ".join(file_bundles[file_stem])})\n")
              if file_stem in reduced_dependencies.keys():
                new_lines.append("\n")
                for dep in reduced_dependencies[file_stem]:
                   # Find the file's full path
                   minimized_path = ""
                   for temp_file in files:
                     if Path(temp_file).stem == dep:
                        minimized_path = os.path.normpath(temp_file).replace(os.sep, '/').split("goal_src/jak1/")[1]
                        break
                   new_lines.append(f"(require \"{minimized_path}\")\n")
                new_lines.append("\n")
           else:
               new_lines.append(line)
            
        # Move the file pointer to the beginning
        f.seek(0)
        # Write the modified lines back to the file
        f.writelines(new_lines)
        # Truncate any remaining content (if the new content is shorter than the old)
        f.truncate()
