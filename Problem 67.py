matrix = []
with open("Problem 67 Tree.txt") as file:
    for line in file:
        matrix.append([int(x) for x in line.split(" ")])
new = [[0 for _ in line] for line in matrix]
new[0][0] = matrix[0][0]

def printMatrix(toPrint):
    for line in toPrint:
        thing = ""
        for n in line:
            if n < 10:
                thing += "0"
            thing += str(n)+" "
        print(thing[:-1])

for y in range(len(matrix)-1):
    for x in range(len(matrix[y])):
        new[y+1][x] = max(new[y][x]+matrix[y+1][x],new[y+1][x])
        new[y+1][x+1] = max(new[y][x]+matrix[y+1][x+1],new[y+1][x+1])

print(max(new[-1]))
