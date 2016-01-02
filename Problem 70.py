import collections
import time

def totients(MAX):
    phi = list(range(0, MAX))
    for i in range(2, MAX):
        if (phi[i] != i):
            continue
        for j in range (i, MAX, i):
            phi[j] = int(phi[j] / i * (i - 1))
    return phi

start_time = time.time()

MAX = 10**7

phis = totients(MAX)
minimum = 0
answer = 0
for i in range(2,MAX):
    if collections.Counter(str(phis[i])) == collections.Counter(str(i)) and (i/phis[i] < minimum or answer == 0):
        answer = i
        minimum = i/phis[i]
print(answer)
print(time.time() - start_time)
