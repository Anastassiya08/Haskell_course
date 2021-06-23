module Task8_Fix where

import           Test.QuickCheck
import           Data.Function
import           Test.Tasty.HUnit

iterateElement :: a -> [a]
iterateElement x = fix (x:)

prop_iterateElement :: Integer -> Property
prop_iterateElement x = take 7 (iterateElement x) === [x, x, x, x, x, x, x]

fibonacci :: Integer -> Integer
fibonacci = fix (\f n -> case n of 1 -> 0
                                   2 -> 1
                                   _ -> f (n - 1) + f (n - 2))

unit_fibonacci :: Assertion
unit_fibonacci = fibonacci 14 @?= 233

factorial :: Integer -> Integer
factorial = fix (\f n -> if n <= 1 then 1 else n * f (n - 1))

unit_factorial :: Assertion
unit_factorial = factorial 7 @?= 5040

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\my_map f xs -> case xs of [] -> []
                                         x1:xs1 -> f x1 : my_map f xs1)
                                    
prop_mapFix_eq_map :: Blind (Integer -> Char) -> [Integer] -> Property
prop_mapFix_eq_map (Blind f) xs = mapFix f xs === map f xs
