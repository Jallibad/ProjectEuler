{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}

module ModularArithmetic where

import Control.Arrow ((***))
import Data.Proxy
import Data.Ratio ((%))
import GHC.TypeLits

newtype Mod a (m :: Nat) = Mod a deriving (Eq, Ord)

instance Show a => Show (Mod a m) where
    show (Mod a) = show a

instance (Integral a, KnownNat m) => Enum (Mod a m) where
	fromEnum (Mod a) = fromIntegral a
	toEnum = fromIntegral

instance (Integral a, KnownNat m) => Num (Mod a m) where
	(Mod a) + (Mod b) = fromIntegral $ a+b
	(Mod a) * (Mod b) = fromIntegral $ a*b
	abs (Mod a) = Mod $ abs a
	signum (Mod a) = Mod $ signum a
	fromInteger = Mod . fromInteger . (`mod` (natVal (Proxy :: Proxy m)))
	negate (Mod a) = Mod $ (fromInteger $ natVal (Proxy :: Proxy m))-a

instance (Integral a, Ord a, KnownNat m) => Real (Mod a m) where
	toRational (Mod a) = (toInteger a) % 1

instance (Integral a, KnownNat m) => Integral (Mod a m) where
	(Mod a) `quotRem` (Mod b) = (Mod *** Mod) $ a `quotRem` b
	toInteger (Mod n) = toInteger n