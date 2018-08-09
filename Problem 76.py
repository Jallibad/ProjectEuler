memoize = {}
def countSums(number):
    return countSumsR(number, number-1)
def countSumsR(number, currMax):
    if number==0:
        return 1
    elif (number, currMax) not in memoize:
        memoize[(number, currMax)] = 0
        for x in range(1, min(number, currMax)+1):
            memoize[(number, currMax)] += countSumsR(number-x, min(x,number-x))
    return memoize[(number, currMax)]
print(countSums(100))
