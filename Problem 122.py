##def efficient(current, power, options):
##    print(current, power, options)
##    if current == power:
##        return 0
##    minimum = -1
##    for x in options:
##        if x > power-current:
##            continue
##        nextOptions = list(options)
##        nextOptions.append(current+x)
##        temp = efficient(current+x, power, nextOptions)+1
##        if temp < minimum or minimum == -1:
##            minimum = temp
##    return minimum
##
##print(efficient(1, 15, [1]))

answers = {}
upperBound = 5

def update(key, value):
    if key not in answers or answers[key] > value:
        answers[key] = value

##for i in pastResults:
##    curr = x+i
##    if curr <= upperBound:
##        if curr in answers and curr
##    else:
##        break
