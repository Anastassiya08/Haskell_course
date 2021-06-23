module Task7_Monoid where

import           Test.QuickCheck

associativity :: (Eq a, Semigroup a, Show a) => a -> a -> a -> Property
associativity a b c = (a <> b) <> c === a <> (b <> c)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = undefined

data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Semigroup (NonEmpty a) where
  (<>) = undefined

data ThisOrThat a b = This a | That b | Both a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (ThisOrThat a b) where
  arbitrary =
    oneof
      [This <$> arbitrary, That <$> arbitrary, Both <$> arbitrary <*> arbitrary]

instance Semigroup (ThisOrThat a b) where
  (<>) = undefined

-- prop_NonEmpty_associativity
--   :: NonEmpty Integer -> NonEmpty Integer -> NonEmpty Integer -> Property
-- prop_NonEmpty_associativity = associativity

-- prop_ThisOrThat_associativity
--   :: ThisOrThat Integer Char
--   -> ThisOrThat Integer Char
--   -> ThisOrThat Integer Char
--   -> Property
-- prop_ThisOrThat_associativity = associativity
