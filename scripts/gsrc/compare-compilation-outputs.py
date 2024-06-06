# Simple script that compares every file in `out/game/obj` with a base directory
# This is useful for when you expect your compilation output to be identical, ie. when you've just made formatting only changes
# If every file matches...you should be able to be confident that you have broken nothing!

import os
import hashlib

def hash_file(filepath):
    """Returns the MD5 hash of the file."""
    hasher = hashlib.md5()
    with open(filepath, 'rb') as f:
        buf = f.read()
        hasher.update(buf)
    return hasher.hexdigest()

def compare_directories(base_dir, compare_dir):
    """Compares files in two directories based on their MD5 hash."""
    mismatched_files = []
    missing_files = []

    # Iterate through files in the base directory
    for root, _, files in os.walk(base_dir):
        for file in files:
            base_file_path = os.path.join(root, file)
            relative_path = os.path.relpath(base_file_path, base_dir)
            compare_file_path = os.path.join(compare_dir, relative_path)

            if os.path.exists(compare_file_path):
                base_file_hash = hash_file(base_file_path)
                compare_file_hash = hash_file(compare_file_path)
                if base_file_hash != compare_file_hash:
                    mismatched_files.append(relative_path)
            else:
                missing_files.append(relative_path)

    # Report results
    if not mismatched_files and not missing_files:
        print("All files matched successfully.")
    else:
        if mismatched_files:
            print("Mismatched files:")
            for file in mismatched_files:
                print(f" - {file}")
        if missing_files:
            print("Missing files:")
            for file in missing_files:
                print(f" - {file}")

# Usage example
base_directory = './out/jak1/obj'
compare_directory = './out/jak1/obj_master'
print(f'Comparing {base_directory} with {compare_directory}')
compare_directories(base_directory, compare_directory)
