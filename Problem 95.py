def factors(n):
    answer = []
    for i in range(1, int(n/2)+1):
        if n % i == 0:
            answer.append(i)
    return answer

def amicable(n):
    return sum(factors(n))

def amicableChain(n):
    return amicableChainR([n])

def amicableChainR(n):
    x = amicable(n[len(n)-1])
    if x not in n:
        changed = n[:]
        changed.append(x)
        return 1+amicableChainR(changed)
    else:
        return 1
print(amicableChain(12496))
