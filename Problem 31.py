memoize = {} # (number, max): possibilities

def coinSums(n, maximum=200):
    if n == 0:
        return 1
    if (n, maximum) not in memoize:
        memoize[n, maximum] = 0
        for coin in [i for i in [1,2,5,10,20,50,100,200] if i <= n and i <= maximum]:
            memoize[n, maximum] += coinSums(n-coin, coin)
    return memoize[n, maximum]
print(coinSums(200))
