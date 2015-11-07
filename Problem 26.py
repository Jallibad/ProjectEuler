def longDivision(dividend, divisor):
    thing = 0
    result = []
    i = 0
    while True:
        if i < len(str(dividend)):
            thing += int(str(dividend)[i])
        whole = thing // divisor
        current = (whole, thing)
        if current in result:
            return i - result.index(current) # The length of the cycle
            #return [x[0] for x in result] # Result
        result.append(current)
        thing = (thing-whole*divisor)*10
        i += 1
print(max([(longDivision(1, i), i) for i in range(2,1000)], key=lambda x: x[0])[1])
