matrix = []
with open("Problem 81 Matrix.txt") as file:
    for line in file:
        temp = []
        for number in line.split(","):
            temp.append(int(number))
        matrix.append(temp)
new = [[0 for _ in row] for row in matrix]
new[0][0] = matrix[0][0]
def printMatrix(toPrint):
    for line in toPrint:
        temp = ""
        for number in line:
            if(number < 1000):
                temp += "0"
            if (number < 100):
                temp += "0"
            if (number < 10):
                temp += "0"
            temp += str(number)+" "
        print(temp[:-1])

for i in range(min(len(matrix), len(matrix[0]))):
    for x in range(i, len(matrix[0])-1):
        y = i
        if new[y][x+1] != 0:
            new[y][x+1] = min(matrix[y][x+1]+new[y][x],new[y][x+1])
        else:
            new[y][x+1] = matrix[y][x+1]+new[y][x]
        if new[y+1][x+1] != 0:
            new[y+1][x+1] = min(new[y][x+1]+matrix[y+1][x+1],new[y+1][x+1])
        else:
            new[y+1][x+1] = new[y][x+1]+matrix[y+1][x+1]
    for y in range(i, len(matrix)-1):
        x = i
        if new[y+1][x] != 0:
            new[y+1][x] = min(matrix[y+1][x]+new[y][x],new[y+1][x])
        else:
            new[y+1][x] = matrix[y+1][x]+new[y][x]
        if new[y+1][x+1] != 0:
            new[y+1][x+1] = min(new[y+1][x]+matrix[y+1][x+1],new[y+1][x+1])
        else:
            new[y+1][x+1] = new[y+1][x]+matrix[y+1][x+1]
print(new[-1][-1])
