module Data.Functor.WeightedSet where

-- containers
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Data.Functor.Classes

newtype WeightedSet w a = WeightedSet { getWeightedSet :: Map a w }
  deriving (Show)

-- FIXME this doesn't show the constructor name
instance Show2 WeightedSet where
  liftShowsPrec2 a b c d e = liftShowsPrec2 c d a b e . getWeightedSet

instance Show w => Show1 (WeightedSet w) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Ord a, Semigroup w) => Semigroup (WeightedSet w a) where
  weightedSet1 <> weightedSet2 = WeightedSet $ unionWith (<>) (getWeightedSet weightedSet1) (getWeightedSet weightedSet2)

instance (Ord a, Semigroup w) => Monoid (WeightedSet w a) where
  mempty = WeightedSet empty

instance Foldable (WeightedSet w) where
  foldMap f = foldMap f . keys . getWeightedSet

singleton :: Monoid w => a -> WeightedSet w a
singleton a = WeightedSet $ Map.singleton a mempty

-- FIXME Generalise to (w -> w) ? And then special cases left mappend and right mappend?
-- In fact this is more of a monoid action!
mappendWeightedSet :: Semigroup w => w -> WeightedSet w a -> WeightedSet w a
mappendWeightedSet w = WeightedSet . fmap (w <>) . getWeightedSet

runWeightedSet :: WeightedSet w a -> [(a, w)]
runWeightedSet = toList . getWeightedSet

fromList :: Ord a => [(a, w)] -> WeightedSet w a
fromList = WeightedSet . Map.fromList

foldWeightedSets :: (Foldable f, Functor f, Ord a, Monoid w) => f (WeightedSet w a, w) -> WeightedSet w a
foldWeightedSets = foldMap (uncurry $ flip mappendWeightedSet)

joinWeightedSet :: (Monoid w, Ord a) => WeightedSet w (WeightedSet w a) -> WeightedSet w a
joinWeightedSet = foldWeightedSets . runWeightedSet

map :: (Ord b, Monoid w) => (a -> b) -> WeightedSet w a -> WeightedSet w b
map f = WeightedSet . mapKeysWith mappend f . getWeightedSet
