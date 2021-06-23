module Task3_Nat where

import           Test.Tasty.HUnit
import           Test.QuickCheck

data Nat = Z | S Nat
  deriving Show

-- instance Arbitrary Nat where
--   arbitrary = intToNat . getNonNegative <$> arbitrary

intToNat :: Integer -> Nat
intToNat = undefined

natToInt :: Nat -> Integer
natToInt = undefined

-- prop_nat_int_roundtrip1 :: Nat -> Property
-- prop_nat_int_roundtrip1 n = intToNat (natToInt n) === n

-- prop_nat_int_roundtrip2 :: Integer -> Property
-- prop_nat_int_roundtrip2 i = i >= 0 ==> natToInt (intToNat i) === i
