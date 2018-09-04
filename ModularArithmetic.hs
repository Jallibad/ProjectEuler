{-# LANGUAGE KindSignatures, ScopedTypeVariables, DataKinds #-}

module ModularArithmetic where

import Control.Arrow ((***))
import Data.Function (on)
import Data.Proxy
import Data.Ratio ((%))
import GHC.TypeLits (Nat, natVal, KnownNat)

newtype Mod a (m :: Nat) = Mod a deriving (Eq, Ord)

unMod :: Mod a m -> a
unMod (Mod a) = a

modMap :: (a -> a) -> Mod a m -> Mod a m
modMap f = Mod . f . unMod

instance Show a => Show (Mod a m) where
	show = show . unMod

instance (Integral a, KnownNat m) => Enum (Mod a m) where
	fromEnum = fromIntegral . unMod
	toEnum = fromIntegral

instance (Integral a, KnownNat m) => Num (Mod a m) where
	(+) = (fromIntegral .) . ((+) `on` unMod)
	(*) = (fromIntegral .) . ((*) `on` unMod)
	abs = modMap abs
	signum = modMap signum
	fromInteger = Mod . fromInteger . flip mod (natVal (Proxy :: Proxy m))
	negate = modMap $ flip subtract $ fromInteger $ natVal (Proxy :: Proxy m)

instance (Integral a, Ord a, KnownNat m) => Real (Mod a m) where
	toRational = (% 1) . toInteger . unMod

instance (Integral a, KnownNat m) => Integral (Mod a m) where
	quotRem = ((Mod *** Mod) .) . (quotRem `on` unMod)
	toInteger = toInteger . unMod