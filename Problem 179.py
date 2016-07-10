def factorsSieve(upperBound):
    answer = list([1]*upperBound)
    for i in range(2, upperBound):
        for n in range(i, upperBound, i):
            answer[n] += 1
    return answer[1:]
factors = factorsSieve(10**7-1)
answer = 0
for i in range(len(factors)-1):
    if factors[i] == factors[i+1]:
        answer += 1
print(answer)
