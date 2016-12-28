chains = {1: False, 89: True}

def digitSquare(n):
    answer = 0
    for x in str(n):
        answer += int(x) ** 2
    return answer

def insertN(n):
    if n not in chains:
        chains[n] = insertN(digitSquare(n))
    return chains[n]

answer = 0
for i in range(1, 10**7):
    if insertN(i):
        answer += 1
print(answer)
