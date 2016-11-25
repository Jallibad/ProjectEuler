memoize = {1: 1}

def collatzR(n):
    if n not in memoize:
        if n%2 == 0:
            memoize[n] = collatzR(int(n/2))+1
        else:
            memoize[n] = collatzR(3*n+1)+1
    return memoize[n]
    
maximum = (0, 0) #(number, amount)
for i in range(1,10**6):
    temp = collatzR(i)
    if temp > maximum[1]:
        maximum = (i, temp)
print(maximum[0])
