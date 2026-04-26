import json
import os
import subprocess
from multiprocessing import process

# TODO NOW after doing this, we will decomp the entire game, and copy over any files that are done / have no errors
# then make sure to enable offline tests

processed_file_list = []
if os.path.isfile("./process-file-list.json"):
    with open("./process-file-list.json") as f:
        processed_file_list = json.load(f)

target_game = "jakx"
other_game = "jak3"


def copy_casts(file, function):
    # anonymous_function_types.jsonc
    if "anon-function" in function:
        print("do the anonymous functions yourself")
    # label_types.jsonc (if top level is the same)
    # if "top-level-login" in function:
    #     with open(
    #         f"../../decompiler/config/{other_game}/ntsc_v1/label_types.jsonc"
    #     ) as f:
    #         other_casts = json.load(f)
    #     if file in other_casts:
    #         with open(
    #             f"../../decompiler/config/{target_game}/ntsc_v1/label_types.jsonc", "w"
    #         ) as f:
    #             target_casts = json.load(f)
    #             target_casts[file] = other_casts[file]
    #             json.dump(target_casts, f)
    # stack_structures.jsonc
    with open(
        f"../../decompiler/config/{other_game}/ntsc_v1/stack_structures.jsonc"
    ) as f:
        other_casts = json.load(f)
    if function in other_casts:
        path = f"../../decompiler/config/{target_game}/ntsc_v1/stack_structures.jsonc"
        with open(path) as f:
            target_casts = json.load(f)
        target_casts[function] = other_casts[function]
        with open(path, "w") as f:
            json.dump(target_casts, f, indent=2)
    # type_casts.jsonc
    with open(f"../../decompiler/config/{other_game}/ntsc_v1/type_casts.jsonc") as f:
        other_casts = json.load(f)
    if function in other_casts:
        path = f"../../decompiler/config/{target_game}/ntsc_v1/type_casts.jsonc"
        with open(path) as f:
            target_casts = json.load(f)
        target_casts[function] = other_casts[function]
        with open(path, "w") as f:
            json.dump(target_casts, f, indent=2)
    # var_names.jsonc
    with open(f"../../decompiler/config/{other_game}/ntsc_v1/var_names.jsonc") as f:
        other_casts = json.load(f)
    if function in other_casts:
        path = f"../../decompiler/config/{target_game}/ntsc_v1/var_names.jsonc"
        with open(path) as f:
            target_casts = json.load(f)
        target_casts[function] = other_casts[function]
        with open(path, "w") as f:
            json.dump(target_casts, f, indent=2)


def process_entry(file, matching_funcs):
    print(f"Processing [{file}]")
    # clean decomp folder
    subprocess.run(
        ["task", "decomp-clean"],  # or your command
        cwd="../../",  # working directory
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    for func_name, is_matching in matching_funcs.items():
        # copy all casts for all related functions
        print(f"- {func_name}")
        copy_casts(file, func_name)
    # then decompile the file
    env = os.environ.copy()
    env["FILE"] = file
    subprocess.run(
        ["task", "decomp-file"],  # or your command
        env=env,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        cwd="../../",  # working directory
    )
    # then the user can view it and compare


def main():
    with open("./matching-functions.json", "r") as f:
        matching_func_info = json.load(f)

    for file, matching_funcs in matching_func_info.items():
        if len(matching_funcs) == 0:
            continue
        process_entry(file, matching_funcs)
        while True:
            process_entry(file, matching_funcs)
            user_input = input("[n]ext / [r]erun: ").strip().lower()
            if user_input == "n":
                processed_file_list.append(file)
                break
            elif user_input == "r":
                continue
            else:
                print("Invalid input. Use 'n' or 'r'.")


if __name__ == "__main__":
    try:
        main()
        with open("./process-file-list.json") as f:
            json.dump(processed_file_list, f)
    except KeyboardInterrupt:
        print("\nExiting.")
