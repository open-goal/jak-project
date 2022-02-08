import zipfile

p2s_name = input("Select savestate (.p2s) file (drag and drop):")
out_dir = input("Select output directory:")

if (p2s_name.startswith('"')):
  p2s_name = p2s_name[1:len(p2s_name)-1]
if (out_dir.startswith('"')):
  out_dir = out_dir[1:len(out_dir)-1]

with zipfile.ZipFile(p2s_name, 'r') as p2s:
  p2s.extractall(out_dir + "/savestate_out")

print("Savestate extracted")
