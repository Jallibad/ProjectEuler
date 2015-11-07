def nextLexicographicPermutation(toOrder):
    answer = list(toOrder)
    k = None
    for i in range(len(toOrder)-1):
        if toOrder[i] < toOrder[i+1]:
            k = i
    if k is None:
        return None
    l = k+1
    for i in range(k+1, len(toOrder)):
        if toOrder[k] < toOrder[i]:
            l = i
    answer[k] = toOrder[l]
    answer[l] = toOrder[k]
    return answer[:k+1]+answer[:k:-1]

perm = [0,1,2,3,4,5,6,7,8,9]
for i in range(10**6-1):
    perm = nextLexicographicPermutation(perm)
print(perm)
