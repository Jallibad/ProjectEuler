import math

digitFactorialChains = {}

def digitFactorial(digit):
    result = 0
    for letter in str (digit):
        result += math.factorial(int(letter))
    return result
    
def digitFactorialChain(number):
    result = 1
    previous = [number]
    current = digitFactorial(number)
    global digitFactorialChains
    while not (current in previous):
        if current in digitFactorialChains:
            return result + digitFactorialChains[current]
        else:
            result += 1
            previous.append(current)
            current = digitFactorial(current)
    digitFactorialChains[number] = result
    return result

answer = 0
for x in range(10**6):
    if (digitFactorialChain(x) == 60):
        answer+=1
print(answer)
