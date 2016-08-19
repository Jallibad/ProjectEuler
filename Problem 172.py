memoize = {}

def f(n):
    answer = 0
    threeList = [3]*10
    for i in range(1,10):
        m = threeList.copy()
        m[i] -= 1
        answer += fw(n, m)
    return answer
def fw(n, m):
    if n == 0:
        return 1
    t = tuple(m)
    if (n,t) in memoize:
        return memoize[(n,t)]
    answer = 0
    for i in range(10):
        if m[i] > 0:
            newM = m.copy()
            newM[i] -= 1
            answer += fw(n-1, newM)
    memoize[(n,t)] = answer
    return answer

print(f(17))
