memoize = {}
def tiles(n):
    if n < 3:
        return 1
    if n not in memoize:
        answer = tiles(n-1)
        for i in range(3, n+1):
            if i < n:
                answer += tiles(n-i-1)
            else:
                answer += tiles(n-i)
        memoize[n] = answer
    return memoize[n]

print(tiles(50))
