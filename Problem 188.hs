{-# LANGUAGE DataKinds #-}

import MathFunctions ((↑↑))
import ModularArithmetic (Mod)

main = print (1777 ↑↑ 1855 :: Mod Int 100000000)