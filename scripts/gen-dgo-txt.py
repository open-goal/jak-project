import json
import argparse
import os, shutil, sys
from glob import glob
from pathlib import Path

# generate dgo.txt and gd files out of ps3 test level dgo folders
# usage: python gen-dgo-txt.py <path to dgo folder>

dgo_json = {}

def get_dgo_files(path):
    return glob(path + "/**/*.go", recursive=True)

def remove_extensions(file_list):
    for file in file_list:
        if (file.endswith("-bt.go")):
            os.rename(file, file[:-3].replace("-bt", "-vis"))
        elif file.endswith(".go"):
            os.rename(file, file[:-3])

def move_to_root_folder(root_path, cur_path):
    for filename in os.listdir(cur_path):
        if os.path.isfile(os.path.join(cur_path, filename)):
            shutil.move(os.path.join(cur_path, filename), os.path.join(root_path, filename))
        elif os.path.isdir(os.path.join(cur_path, filename)):
            move_to_root_folder(root_path, os.path.join(cur_path, filename))
        else:
            sys.exit("Should never reach here.")
    # remove empty folders
    if cur_path != root_path:
        os.rmdir(cur_path)

def gen_json_for_dgo(folder_name, files):
    dgo_name = os.path.basename(Path(folder_name)) + ".DGO"
    dgo = {
        "file_name": dgo_name,
        "internal_name": dgo_name,
        "objects": []
    }

    for file in files:
        internal_name = ""
        if "." in file:
            internal_name = Path(file).name.split(".")[0]
        else:
            internal_name = file
        unique_name = internal_name
        if (internal_name.endswith("-ag")):
            internal_name = internal_name.replace("-ag", "")
        elif (internal_name.endswith("-bt")):
            internal_name = internal_name.replace("-bt", "-vis")
            unique_name = unique_name.replace("-bt", "-vis")
        dgo["objects"].append({
            "internal_name": internal_name,
            "unique_name": unique_name
        })
    return dgo

def gen_gd_file(dgo_name, file_list):
    # ("SKATEPARK.DGO"
    #  ("drill-turret-ext-ag.go"
    #   "drill-turret-int-ag.go"
    #   "jak-pole+0-ag.go"
    #   "lazerfence-ag.go"
    #   "lazerpit-ag.go"
    #   "mech-ag.go"
    #   "tpage-243.go"
    #   "tpage-354.go"
    #   "tpage-3279.go"
    #   "skatepark-bt.go"
    #  ))
    file_names = []
    for file in file_list:
        file = file.replace("-bt", "-vis")
        file_names.append(os.path.basename(Path(file)))
    buffer = ""
    buffer += "(" + "\"" + os.path.basename(Path(dgo_name)) + ".DGO" + "\"" + "\n  (\n"
    for file in file_names:
        buffer += "    \"" + file + "\"\n"
    buffer += "  ))\n"
    # print("dgo " + os.path.basename(Path(dgo_name)) + " gd:\n" + buffer + "\n")
    with open(dgo_name + "/" + os.path.basename(Path(dgo_name)).lower() + ".gd", "a+") as f:
        f.write(buffer)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("dgo_folder", type=str)
    args = parser.parse_args()

    # generate gd files for all dgo folders in current directory
    # folders = [name for name in os.listdir(os.getcwd()) if os.path.isdir(os.path.join(os.getcwd(), name))]
    # for folder in folders:
    #     gen_gd_file(folder, get_dgo_files(folder))

    files = get_dgo_files(args.dgo_folder)
    gen_gd_file(args.dgo_folder, files)
    dgo_json = gen_json_for_dgo(args.dgo_folder, files)
    # print(dgo_json)
    with open (args.dgo_folder + "/" + os.path.basename(Path(args.dgo_folder)) + ".DGO.txt", "a+") as f:
        json.dump(dgo_json, f, indent=4)
    move_to_root_folder(args.dgo_folder, args.dgo_folder)
    file_list_after = glob(args.dgo_folder + "/**/*.go", recursive=True)
    remove_extensions(file_list_after)

if __name__ == "__main__":
    main()