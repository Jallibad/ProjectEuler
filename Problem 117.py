memoize = {0: 1}
def tiles(n):
    if n < 0:
        return 0
    if n not in memoize:
        memoize[n] = tiles(n-1) + tiles(n-2) + tiles(n-3) + tiles(n-4)
    return memoize[n]

print(tiles(50))
