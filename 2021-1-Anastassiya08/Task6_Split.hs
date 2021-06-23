module Task6_Split where

import           Test.QuickCheck
import           Test.Tasty.HUnit
import           Data.List.NonEmpty

customSplit :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
customSplit ch cur (l :| ls)
  | ch /= cur = (cur : l) :| ls
  | otherwise = [] :| (l : ls)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn ch = foldr (customSplit ch) ([] :| [])

testConvertNE (Just a) = a
unit_splitOn :: Assertion
unit_splitOn = splitOn '/' "path/to/file" @?= (testConvertNE . nonEmpty) ["path", "to", "file"]

customJoin :: Eq a => a -> [a] -> [a] -> [a]
customJoin _ cur []     = cur
customJoin ch cur l = cur ++ (ch : l)

joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith ch = foldr (customJoin ch) []

unit_joinWith :: Assertion
unit_joinWith = joinWith '/' ("path" :| ["to", "file"]) @?= "path/to/file"

prop_splitOn_joinWith :: Char -> String -> Property
prop_splitOn_joinWith c s = joinWith c (splitOn c s) === s
