module NonEmpty where

import           Control.Applicative
import           Data.Foldable

import           Data.Functor.Compose
import           Data.Functor.Identity

import           Test.QuickCheck
import           Test.QuickCheck.Poly

data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

-- | 3
instance Functor NonEmpty where
  fmap f (x :| xs) = (f x) :| (fmap f xs)

prop_NonEmpty_fmaps_as_a_list :: Fun A B -> NonEmpty A -> Property
prop_NonEmpty_fmaps_as_a_list (Fun _ f) x =
  toList (fmap f x) === fmap f (toList x)
  
prop_Functor_Identity :: NonEmpty A -> Property
prop_Functor_Identity x =
  fmap id x === x
  
prop_Functor_Composition :: Fun B C -> Fun A B -> NonEmpty A -> Property
prop_Functor_Composition (Fun _ f) (Fun _ g) x =
  fmap (f . g) x === (fmap f . fmap g) x
  

-- | 4
instance Applicative NonEmpty where
  pure a = a :| []
  f' <*> x' = do { f <- f'
                 ; x <- x'
                 ; return (f x) }
  
prop_Applicative_Identity :: NonEmpty A -> Property
prop_Applicative_Identity v =
  ((pure id) <*> v) === v

prop_Applicative_Composition :: NonEmpty (Fun B C) -> NonEmpty (Fun A B) -> NonEmpty A -> Property
prop_Applicative_Composition u' v' w =
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))
  where
    u = applyFun <$> u'
    v = applyFun <$> v'

prop_Applicative_Homomorphism :: Fun A B -> A -> Property
prop_Applicative_Homomorphism (Fun _ f) x =
  (pure f <*> pure x) === (pure (f x) :: NonEmpty B)

prop_Applicative_Interchange :: NonEmpty (Fun A B) -> A -> Property
prop_Applicative_Interchange u' y =
  (u <*> pure y) === (pure ($ y) <*> u)
  where
    u = applyFun <$> u'
    

-- | 5
instance Monad NonEmpty where
  (x :| xs) >>= f = 
    let y :| ys = f x
        l (z :| zs) = z : zs
    in
    y :| (ys ++ (xs >>= l . f))
        
prop_Monad_LeftIdentity :: A -> Fun A (NonEmpty B) -> Property
prop_Monad_LeftIdentity a (Fun _ k) =
  (return a >>= k) === k a

prop_Monad_RightIdentity :: NonEmpty B -> Property
prop_Monad_RightIdentity m =
  (m >>= return) === m

prop_Monad_Associativity :: NonEmpty A -> Fun A (NonEmpty B) -> Fun B (NonEmpty C) -> Property
prop_Monad_Associativity m (Fun _ k) (Fun _ h) =
  (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

-- | 6
instance Foldable NonEmpty where
  foldMap f (x :| xs) = (f x) <> (foldMap f xs)

-- | 7
instance Traversable NonEmpty where
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)
  
prop_traverse_Identity :: NonEmpty A -> Property
prop_traverse_Identity x =
  traverse Identity x === Identity x

prop_traverse_Composition :: Fun A (F B) -> Fun B (G C) -> NonEmpty A -> Property
prop_traverse_Composition (Fun _ f) (Fun _ g) x =
  traverse (Compose . fmap g . f) x
    === (Compose . fmap (traverse g) . traverse f) x

prop_sequenceA_Identity :: NonEmpty A -> Property
prop_sequenceA_Identity x =
  (sequenceA . fmap Identity) x === Identity x

prop_sequenceA_Composition :: NonEmpty (F (G A)) -> Property
prop_sequenceA_Composition x =
  (sequenceA . fmap Compose) x === (Compose . fmap sequenceA . sequenceA) x
  
type F = Maybe

type G = Either String
