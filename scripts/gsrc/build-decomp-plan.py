import json

with open("./matching-functions.json") as f:
    target_analysis = json.load(f)

with open("../../goal_src/jakx/build/all_objs.json") as f:
    target_build_order = json.load(f)

# We want a list of files, in the build order, that either:
# - are header files (easy)
# - have matching code with jak3 (easy)

plan = []
for file_info in target_build_order:
    file_name = file_info[0]
    if file_name.endswith("-h") or (
        file_name in target_analysis and len(target_analysis[file_name]) > 0
    ):
        if file_name in target_analysis and len(target_analysis[file_name]) > 0:
            plan.append({"name": file_name, "matching": target_analysis[file_name]})
        else:
            plan.append({"name": file_name, "matching": []})

with open("./decomp-plan.json", "w") as f:
    json.dump(plan, f)
