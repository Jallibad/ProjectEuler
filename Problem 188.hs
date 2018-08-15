tetrate :: Integral a => a -> a -> a -> a
tetrate a 1 _ = a
tetrate a k b = modular_pow a (tetrate a (k-1) b) b

mult x y b = (x*y) `mod` b

modular_pow _ 0 _ = 1
modular_pow base exponent modulus = if odd exponent then mult result base modulus else result
	where result = modular_pow (mult base base modulus) (exponent `div` 2) modulus

main = print $ tetrate 1777 1855 $ 10^8