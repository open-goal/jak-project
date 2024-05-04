import json

with open('./jak1-missing-requires.json', 'r') as f:
  missing_requires = json.load(f)

from pathlib import Path
from collections import defaultdict
import networkx as nx

dependencies = {
    "air": [
        "C:/Users/xtvas/Repositories/opengoal/jak-project/goal_src/jak1/kernel-defs.gc",
        "C:/Users/xtvas/Repositories/opengoal/jak-project/goal_src/jak1/engine/math/vector-h.gc",
        "C:/Users/xtvas/Repositories/opengoal/jak-project/goal_src/jak1/levels/beach/air-h.gc",
        "C:/Users/xtvas/Repositories/opengoal/jak-project/goal_src/jak1/engine/debug/debug-h.gc",
        "C:/Users/xtvas/Repositories/opengoal/jak-project/goal_src/jak1/engine/math/math.gc"
    ],
    "air-h": [
        "C:/Users/xtvas/Repositories/opengoal/jak-project/goal_src/jak1/engine/math/vector-h.gc",
        "C:/Users/xtvas/Repositories/opengoal/jak-project/goal_src/jak1/kernel-defs.gc"
    ],
}

def extract_stem(file_paths):
    return [Path(file).stem for file in file_paths]

def create_graph(dependencies):
    graph = nx.DiGraph()
    for key, files in dependencies.items():
        for file in files:
            graph.add_edge(key, Path(file).stem)
    return graph

def transitive_reduction(graph):
    transitive_reduced_graph = nx.transitive_reduction(graph)
    return transitive_reduced_graph

graph = create_graph(missing_requires)
reduced_graph = transitive_reduction(graph)

# Convert reduced graph to dictionary
reduced_dependencies = {}
for node, successors in reduced_graph.adjacency():
    reduced_dependencies[node] = list(successors.keys())

output_file = "transitive_reduced_dependencies.json"
with open(output_file, "w") as f:
    json.dump(reduced_dependencies, f, indent=4)

print(f"Transitive reduced dependencies written to {output_file}")