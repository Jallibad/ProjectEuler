memoize = {}

def f(n, a=0, l=False):
    if n==0:
        return 1
    if (n, a, l) in memoize:
        return memoize[(n, a, l)]
    answer = 0
    if not l:
        answer += f(n-1, 0, True)
    answer += f(n-1, 0, l)
    if a != 2:
        answer += f(n-1, a+1, l)
    memoize[(n, a, l)] = answer
    return answer

print(f(30))
