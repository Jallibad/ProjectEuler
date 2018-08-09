memoize = {}
def tiles(n):
    if n < 50:
        return 1
    if n not in memoize:
        answer = tiles(n-1)
        for i in range(50, n+1):
            if i < n:
                answer += tiles(n-i-1)
            else:
                answer += tiles(n-i)
        memoize[n] = answer
    return memoize[n]

answer = 1
while tiles(answer) < 10**6:
    answer += 1
print(answer)
