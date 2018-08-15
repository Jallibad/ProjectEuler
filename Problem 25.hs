import MathFunctions (fibs)

fib = (fibs !!)

main = print $ until ((==1000) . length . show . fib) succ 0