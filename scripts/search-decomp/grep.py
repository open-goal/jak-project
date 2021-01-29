import re
import os
import argparse

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
    line = line.strip()
    tokens = line.split(":")
    search_terms.append({
      "term": tokens[0],
      "description": tokens[1]
    })
    summary_results[tokens[0].lower()] = 0
    results[tokens[0].lower()] = []

for search_term in search_terms:
  term = search_term["term"].lower()
  print("Searching for - {}".format(term))
  pattern = re.compile(term)
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
