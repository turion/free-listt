module Data.Functor.WeightedSet.Coyoneda where

-- base
import Control.Arrow (first)

-- containers
import Data.Map.Strict (fromDistinctAscList)

-- kan-extensions
import Data.Functor.Coyoneda

-- free-listt
import qualified Data.Functor.WeightedSet as WeightedSet
import Data.Functor.Classes (Show1 (..))

fromWeightedSet :: WeightedSet.WeightedSet w a -> WeightedSet w a
fromWeightedSet = WeightedSet . liftCoyoneda

newtype WeightedSet w a = WeightedSet { getWeightedSet :: Coyoneda (WeightedSet.WeightedSet w) a }
  deriving (Functor, Foldable)


-- FIXME I cannot make a Show1 without hiding the value of the Coyoneda function and the internal elements

instance (Show w, Monoid w) => Show1 (WeightedSet w) where
  -- FIXME this discards w
  liftShowsPrec showsPrec' showList' n' = liftShowsPrec (\n (a, _w) -> showsPrec' n a) (showList' . map fst) n' . runWeightedSetInternal

-- FIXME this collapses the set somewhat. ok?
instance (Show a, Show w, Ord a, Monoid w) => Show (WeightedSet w a) where
  showsPrec n = showsPrec n . runWeightedSet

instance (Ord a, Monoid w) => Semigroup (WeightedSet w a) where
  cmap1 <> cmap2 = fromWeightedSet $ runWeightedSet cmap1 <> runWeightedSet cmap2

instance (Ord a, Monoid w) => Monoid (WeightedSet w a) where
  mempty = fromWeightedSet mempty

singleton :: Monoid w => a -> WeightedSet w a
singleton a = WeightedSet $ Coyoneda (const a) $ WeightedSet.singleton ()

runWeightedSet :: (Ord a, Monoid w) => WeightedSet w a -> WeightedSet.WeightedSet w a
runWeightedSet (WeightedSet (Coyoneda f weightedSet)) = WeightedSet.map f weightedSet

fromList :: Ord a => [(a, w)] -> WeightedSet w a
fromList = fromWeightedSet . WeightedSet.fromList

factorFoldWeightedSets :: (Foldable f, Functor f, Ord a, Monoid w) => f (WeightedSet w a, w) -> WeightedSet w a
factorFoldWeightedSets = fromWeightedSet . WeightedSet.foldWeightedSets . fmap (first runWeightedSet)

joinWeightedSet :: (Ord a, Monoid w) => WeightedSet w (WeightedSet w a) -> WeightedSet w a
joinWeightedSet = fromWeightedSet . WeightedSet.foldWeightedSets . runWeightedSetInternal . fmap runWeightedSet

runWeightedSetInternal :: WeightedSet w a -> [(a, w)]
runWeightedSetInternal (WeightedSet (Coyoneda f as)) = map (first f) $ WeightedSet.runWeightedSet as

-- FIXME Traversable?
-- FIXME Alternative? Seems to need Ord a in WeightedSet context

instance Monoid w => Applicative (WeightedSet w) where
  WeightedSet (Coyoneda f1 map1) <*> WeightedSet (Coyoneda f2 map2) = WeightedSet $ Coyoneda
    (\(a1, a2) -> f1 a1 $ f2 a2) $
    WeightedSet.WeightedSet $ fromDistinctAscList $ do -- FIXME proptest whether fromDistinctAscList is ok here
      (value1, prob1) <- WeightedSet.runWeightedSet map1
      (value2, prob2) <- WeightedSet.runWeightedSet map2
      return ((value1, value2), prob1 `mappend` prob2)
  pure = singleton
