memoize = {};
def partitions(n, m=1):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    elif (m, n) in memoize:
        return memoize[(m, n)]
    else:
        answer = 0
        for i in range(m, n+1):
            answer += partitions(n-i, i)
        memoize[(m, n)] = answer
        return answer
k = 4
for i in range(10):
    print(partitions(i))
print(k)
