module Tasks4_5_Tree where

-- import           Data.Foldable
-- import           Data.List (sort)
-- import           Test.QuickCheck

data Tree a

instance Foldable Tree where
  foldMap = undefined
  -- OR
  foldr = undefined

fromList :: [a] -> Tree a
fromList = undefined

-- prop_roundtrip_sort :: [Integer] -> Property
-- prop_roundtrip_sort = toList . fromList === sort
