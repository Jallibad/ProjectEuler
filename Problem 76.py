memoize = {}
def countSums(number):
    return countSumsR(number, number-1)
def countSumsR(number, currMax):
    if number==0:
        return 1
    elif (number, currMax) in memoize:
        return memoize[(number, currMax)]
    else:
        answer = 0
        for x in range(1, min(number, currMax)+1):
            answer += countSumsR(number-x, min(x,number-x))
        memoize[(number, currMax)] = answer
        return answer
print(countSums(100))
