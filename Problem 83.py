from collections import deque
from heapdict import heapdict


class Graph(object):
    def __init__(self):
        self.nodes = set()
        self.edges = heapdict()
        self.distances = {}

    def addNode(self, value):
        self.nodes.add(value)

    def addEdge(self, from_node, to_node, distance):
        self.edges.setdefault(from_node,[]).append(to_node)
        self.edges.setdefault(to_node,[]).append(from_node)
        self.distances[(from_node, to_node)] = distance


def dijkstra(graph, initial):
    visited = {initial: 0}
    path = {}
    nodes = set(graph.nodes)

    while nodes:
        u = None
        for node in nodes:
            if node in visited and (u is None or visited[node] < visited[u]):
                u = node
        if u is None:
            break
        nodes.remove(u)
        current_weight = visited[u]

        for edge in graph.edges[u]:
            try:
                weight = current_weight + graph.distances[(u, edge)]
            except:
                continue
            if edge not in visited or weight < visited[edge]:
                visited[edge] = weight
                path[edge] = u

    return visited, path


def shortestPath(graph, origin, destination):
    if origin == destination:
        return 0, list(origin)
    visited, paths = dijkstra(graph, origin)
    full_path = deque()
    if destination not in paths:
        return 0, origin
    new_destination = paths[destination]

    while new_destination != origin:
        full_path.appendleft(new_destination)
        new_destination = paths[new_destination]

    full_path.appendleft(origin)
    full_path.append(destination)

    return visited[destination], list(full_path)

matrix = []
with open("Problem 83 Matrix.txt") as file:
    for line in file:
        line = line.split(",")
        line = [int(x) for x in line]
        matrix.append(line)

g = Graph()
g.addNode((-1,-1))
for y in range(len(matrix)):
    for x in range(len(matrix[y])):
        g.addNode((y,x))
        g.addEdge((y-1,x),(y,x),matrix[y][x])
        g.addEdge((y+1,x),(y,x),matrix[y][x])
        g.addEdge((y,x-1),(y,x),matrix[y][x])
        g.addEdge((y,x+1),(y,x),matrix[y][x])

g.addEdge((-1,-1),(0,0),matrix[0][0])

print(shortestPath(g,(-1,-1),(len(matrix)-1,len(matrix)-1))[0])
