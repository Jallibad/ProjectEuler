memoize = {}
def countSums(number, currMax=None):
    if number==0:
        return 1
    if currMax is None:
        currMax = number-1
    if (number, currMax) not in memoize:
        memoize[(number, currMax)] = 0
        for x in range(1, min(number, currMax)+1):
            memoize[(number, currMax)] += countSums(number-x, min(x,number-x))
    return memoize[(number, currMax)]
print(countSums(100))
