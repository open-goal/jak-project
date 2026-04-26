import json

with open("../../decompiler_out/jak3/_func_metadata.json") as f:
    other_analysis = json.load(f)

with open("../../decompiler_out/jakx/_func_metadata.json") as f:
    target_analysis = json.load(f)

matching_results = {}
total_func_count = 0

for file, functions in target_analysis.items():
    total_func_count = total_func_count + len(functions)
    if file not in other_analysis:
        continue
    matching_results[file] = {}
    other_funcs = other_analysis[file]
    for func, hash in functions.items():
        if func in other_funcs and hash == other_funcs[func]:
            print(f"{file}:{func} is the same")
            matching_results[file][func] = True

print(total_func_count)

with open("./matching-functions.json", "w+") as f:
    f.write(json.dumps(matching_results))
