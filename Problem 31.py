memoize = {} # (number, max): possibilities

def coinSums(n, maximum=200):
    if n == 0:
        return 1
    if (n, maximum) in memoize:
        return memoize[n, maximum]
    else:
        answer = 0
        for coin in [i for i in [1,2,5,10,20,50,100,200] if i <= n and i <= maximum]:
            answer += coinSums(n-coin, coin)
        memoize[n, maximum] = answer
        return answer
print(coinSums(200))
