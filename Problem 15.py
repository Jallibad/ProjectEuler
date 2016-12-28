memoize = {(0,0): 1}
def path(x, y):
    if (x,y) in memoize:
        return memoize[(x,y)]
    answer = 0
    if x > 0:
        answer += path(x-1,y)
    if y > 0:
        answer += path(x,y-1)
    memoize[(x,y)] = answer
    return answer

print(path(20,20))
