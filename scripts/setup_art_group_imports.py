
import os
import glob
import argparse
import re
import json

def get_goal_files(root_dir, ext = "*.gc"):
    """Get all GOAL source files under root_dir."""
    return [goal_file for file in os.walk(root_dir) for goal_file in glob.glob(os.path.join(file[0], ext))]

def get_sgs(goal_file):
    """Get a list of all the skel groups defined in the file, excluding the -sg and *'s."""
    with open(goal_file, "r") as f:
        text = f.read()
    # given "(defskelgroup *foo* bar", will match "bar"
    matches = re.findall(r'\(defskelgroup \*[\w-]+-sg\* ([\w-]+)', text)
    return matches


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(dest='goal_src', help='the goal_src folder')
    args = parser.parse_args()
    all_import_files = get_goal_files(os.path.join(args.goal_src, "import"))
    all_files = get_goal_files(args.goal_src)
    all_non_import_files = list(set(all_files) - set(all_import_files))

    output_json = {}
    to_modify = {}

    import_map = {}
    for import_file in all_import_files:
        base = os.path.basename(import_file)
        # make sure extention is -ag.gc
        assert base[-6:] == "-ag.gc"
        base_no_extension = base[:-6]
        import_map[base_no_extension] = "goal_src/import/" + import_file.split("import/")[1]
    # print(import_map)

    # sg_locations = {}
    for source_file in all_non_import_files:
        sgs = get_sgs(source_file)
        deps = set()
        for sg in sgs:
            if sg not in import_map:
                print("missing: ", sg)
            else:
                deps.add(import_map[sg])
        if len(deps) > 0:
            output_json[os.path.basename(source_file)[:-3]] = list(deps)
            to_modify[source_file] = deps

    # uncomment to modify files
    # for file, deps in to_modify.items():
    #     print("modifying ", file, deps)
    #     with open(file, "r") as f:
    #         lines = f.readlines()
    #     to_add = [] # ["\n"]
    #     for dep in deps:
    #         to_add.append("(import \"{}\")\n".format(dep))
    #     print(to_add)
    #     added = False
    #     for i, line in enumerate(lines):
    #         if ";; decomp begins" in line.lower():
    #             lines[i+1:i+1] = to_add
    #             added = True
    #             break
    #     if not added:
    #         lines[6:6] = to_add
    #     assert lines[1] == "(in-package goal)\n"
    #     with open(file, "w") as f:
    #         f.writelines(lines)

    # uncomment to print json.
    # print(json.dumps(output_json, indent=4))

if __name__ == "__main__":
    main()