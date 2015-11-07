memoize = {1: 1}

def collatzR(n):
    if n in memoize:
        return memoize[n]
    else:
        answer = 0
        if n%2 == 0:
            answer = collatzR(int(n/2))+1
        else:
            answer = collatzR(3*n+1)+1
        memoize[n] = answer
        return answer
    
maximum = (0, 0) #(number, amount)
for i in range(1,10**6-1):
    temp = collatzR(i)
    if temp > maximum[1]:
        maximum = (i, temp)
print(maximum[0])
