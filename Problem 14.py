memoize = {1: 1}

def collatz(n):
    if n not in memoize:
        if n%2 == 0:
            memoize[n] = collatz(int(n/2))+1
        else:
            memoize[n] = collatz(3*n+1)+1
    return memoize[n]
    
maximum = (0, 0) #(number, amount)
for i in range(1,10**6):
    temp = collatz(i)
    if temp > maximum[1]:
        maximum = (i, temp)
print(maximum[0])
