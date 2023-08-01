from utils import get_gsrc_path_from_filename, get_ref_path_from_filename, get_file_list
import os

# TODO - hard-coded to jak 2

# Get all the gsrc files, if they aren't empty -- log if they aren't added to the reference tests as well
file_list = get_file_list("jak2")

# TODO - function for getting just the names
missing_files = []
for file in file_list:
  file_name = ""
  if file[2] != 3:
    continue
  else:
    file_name = file[0]

  # check gsrc
  gsrc_path = get_gsrc_path_from_filename("jak2", file_name)
  if gsrc_path:
    gsrc_length = 0
    with open(gsrc_path, 'r') as fp:
      gsrc_length = len(fp.readlines())

    if gsrc_length > 15:
      if file_name == "enemy-h":
        print(file_name)
      # check if ref exists
      ref_path = get_ref_path_from_filename("jak2", file_name, "./test/decompiler/reference/")
      if not os.path.exists(ref_path):
        missing_files.append(file_name)

print(missing_files)
