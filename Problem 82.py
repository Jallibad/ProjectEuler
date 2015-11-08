matrix = []
with open("Problem 82 Matrix Test.txt") as file:
    string = file.read()
    for line in string.split("\n"):
        answer = []
        for number in line.split(","):
            answer.append(int(number))
        matrix.append(answer)

def printMatrix(m):
    for line in m:
        answer = ""
        for number in line:
            if number < 100:
                answer += "0"+str(number)+" "
            else:
                answer += str(number)+" "
        print(answer)

printMatrix(matrix)
