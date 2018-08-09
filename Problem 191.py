memoize = {}

def f(n, a=0, l=False):
    if n==0:
        return 1
    if (n, a, l) not in memoize:
        answer = f(n-1, 0, l)
        if not l:
            answer += f(n-1, 0, True)
        if a != 2:
            answer += f(n-1, a+1, l)
        memoize[(n, a, l)] = answer
    return memoize[(n, a, l)]

print(f(30))
