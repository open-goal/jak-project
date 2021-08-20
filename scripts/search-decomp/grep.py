import re
import os
import argparse
import time

parser = argparse.ArgumentParser('pygrep')
parser.add_argument('-d', '--directory', type=str, required=True, help='root directory to recursively search')
parser.add_argument('-s', '--search_term_file', type=str, required=True, help='search term file')
parser.add_argument('-o', '--output_file', type=str, required=True, help='output file name')
args = parser.parse_args()

summary_results = {}
results = {}
search_terms = []

with open(args.search_term_file, "r") as f:
  print("Initializing Search Term File")
  for line in f:
    token = line.strip()
    # VU INSTRUCTION ADDITION. Appends all combinations of `dest` to replace `{DEST}`
    vuDestCombinations = ["x", "xy", "xz", "xw", "xyz", "xzw", "xyzw", "y", "yz", "yw", "yzw", "z", "zw", "w"]
    if "{DEST}" in token:
      for combination in vuDestCombinations:
        tempToken = token.replace("{DEST}", combination)
        search_terms.append({
          "term": tempToken
        })
        summary_results[tempToken.lower()] = 0
        results[tempToken.lower()] = []
    else:
      search_terms.append({
        "term": token
      })
      summary_results[token.lower()] = 0
      results[token.lower()] = []

print("Searching for {} tokens...".format(len(search_terms)))

totalTimeStart = time.time()
for index, search_term in enumerate(search_terms):
  start = time.time()
  term = search_term["term"].lower()
  print("[{:.2f}%] - Searching for - {}...".format((index/len(search_terms) * 100), term), end="")
  pattern = re.compile(re.escape(term) + "\s+")
  for path, _, files in os.walk(args.directory):
    for fn in files:
      filepath = os.path.join(path, fn)
      with open(filepath) as handle:
        for lineno, line in enumerate(handle):
          mo = pattern.search(line)
          if mo:
            result = "{}:{}:{}".format(filepath,
                    lineno,
                    line)
            summary_results[term] = summary_results[term] + 1
            results[term].append(result.strip())
  print("Took {:.2f} seconds, Found - {} occurences.".format(time.time() - start, summary_results[term]))
print("Took {} seconds in total".format(time.time() - totalTimeStart))

if os.path.exists(args.output_file):
  os.remove(args.output_file)

with open(args.output_file, "w") as f:
  print("Outputting Report")
  f.write("USAGE SUMMARY\n")
  for key in sorted(summary_results, key=summary_results.get, reverse=True):
    f.write("{} - {}\n".format(key, summary_results[key]))
  f.write("\nOCCURENCES\n")
  for key, value in results.items():
    f.write("{}\n".format(key))
    for occurence in value:
      f.write("- {}\n".format(occurence))
