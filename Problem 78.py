memoize = {};
def partitions(number):
    return countSumsR(1, number);
def countSumsR(m, n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    elif (m, n) in memoize:
        return memoize[(m, n)]
    else:
        answer = 0
        for i in range(m, n+1):
            answer += countSumsR(i, n-i)
        memoize[(m, n)] = answer
        return answer
k = 4
while True:
    current = partitions(k)
    if current % (10**3) == 0:
        break
    print(current)
    k+=5
print(k)
