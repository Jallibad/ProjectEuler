import math

def isPrime(number):
    if number == 1:
        return False
    for i in range(2, int(math.sqrt(number))):
        if number%i == 0:
            return False
    return True

def factors(number):
    answers = {}
    for i in range(1, int(math.sqrt(number))):
        if number%i == 0:
            answers
    return answers
