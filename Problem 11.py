matrix = []
with open("Problem 11 Matrix.txt") as file:
    for line in file:
        matrix.append([int(n) for n in line.split(" ")])
def printMatrix():
    for line in matrix:
        s = ""
        for n in line:
            if n < 10:
                s += "0"
            s += str(n)+" "
        print(s[:-1])

answer = 0
for y in range(len(matrix)):
    for x in range(len(matrix[y])):
        if x+4 <= len(matrix[y]):
            result = 1
            for i in range(4):
                result *= matrix[y][x+i]
            if result > answer:
                answer = result
        if y+4 <= len(matrix):
            result = 1
            for i in range(4):
                result *= matrix[y+i][x]
            if result > answer:
                answer = result
        if y+4 <= len(matrix) and x+4 <= len(matrix[y]):
            result = 1
            for i in range(4):
                result *= matrix[y+i][x+i]
            if result > answer:
                answer = result
        if y+4 <= len(matrix) and x-3 >= 0:
            result = 1
            for i in range(4):
                result *= matrix[y+i][x-i]
            if result > answer:
                answer = result
print(answer)
