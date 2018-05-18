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
    if (n,t) not in memoize:
        memoize[(n,t)] = 0
        for i in range(10):
            if m[i] > 0:
                newM = m.copy()
                newM[i] -= 1
                memoize[(n,t)] += fw(n-1, newM)
    return memoize[(n,t)]

print(f(17))
