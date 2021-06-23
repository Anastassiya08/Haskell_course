module StringSum where

import           Test.QuickCheck
import           Text.Read

-- | 1
stringSum :: String -> Maybe Int
stringSum str = fmap sum (traverse readMaybe (words str))

-- | 2
prop_stringSum_dummy_1 :: Property
prop_stringSum_dummy_1 = stringSum "1 2  3  4 5" === Just 15

prop_stringSum_dummy_2 :: Property
prop_stringSum_dummy_2 = stringSum "7 6  5 x 4   3 2 1" === Nothing

prop_stringSum_dummy_3 :: Property
prop_stringSum_dummy_3 = stringSum "3 4.0 5" === Nothing

prop_stringSum_dummy_4 :: Property
prop_stringSum_dummy_4 = stringSum "7   8 9 10" === Just 34

