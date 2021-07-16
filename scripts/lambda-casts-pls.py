import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--file")
args = parser.parse_args()

if args.file:
  lines = []
  with open("./decompiler_out/jak1/{}_ir2.asm".format(args.file)) as f:
    content = f.readlines()
    label_searching = False
    for line in content:
      if line.startswith("; .function (anon-function"):
        # find the label
        label_searching = True
      if label_searching and line.startswith("L"):
        label_searching = False
        lines.append("[\"{}\", \"_lambda_\", true],".format(line.strip().replace(":", "")))
  print("Here you Go:\n\n\n")
  for line in lines:
    print(line)
  print("\n\n\nGo paste them")

