module Control.Monad.Trans.Set where

{-# LANGUAGE ExistentialQuantification #-}

-- containers
import Data.Map.Strict ()

-- transformers
import Control.Monad.Trans.Class

-- free
import Control.Monad.Trans.Free.Ap

-- kan-extensions
import Data.Functor.WeightedSet.Coyoneda hiding (fromWeightedSet)

-- free-listt
import qualified Data.Functor.WeightedSet as WeightedSet
import Control.Arrow (second)

-- | The monad instance is much slower than Applicative, so make sure you have ApplicativeDo activated all the time!
newtype WeightedSetT w m a = WeightedSetT { getWeightedSetT :: FreeT (WeightedSet w) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, Show)

{-
runWeightedSetTWeightedSet :: (Ord a, Monad m) => WeightedSetT w m a -> m (WeightedSet w a)
runWeightedSetTWeightedSet = fmap runFreeWeightedSet . joinFreeT . getWeightedSetT
  where
    runFreeWeightedSet :: Ord a => Free WeightedSet w a -> WeightedSet w a
    runFreeWeightedSet = runFreeFWeightedSet . runFree
    runFreeFWeightedSet :: Ord a => FreeF WeightedSet w a (Free WeightedSet w a) -> WeightedSet w a
    runFreeFWeightedSet (Pure a) = pure a
    runFreeFWeightedSet (Free cmap) = joinWeightedSet $ runFreeWeightedSet <$> cmap
runWeightedSetTWeightedSet :: (Ord a, Monad m) => WeightedSetT w m a -> m (WeightedSet w a)
runWeightedSetTWeightedSet = _ . getWeightedSetT
  where
    runFreeTWeightedSet :: (Ord a, Monad m) => FreeT WeightedSet m a -> m (WeightedSet w a)
    runFreeTWeightedSet = join . fmap _ . runFreeT
    runFreeFWeightedSet :: Ord a => FreeF WeightedSet w a (Free WeightedSet w a) -> WeightedSet w a
    runFreeFWeightedSet (Pure a) = pure a
    runFreeFWeightedSet (Free cmap) = joinWeightedSet $ runFreeWeightedSet <$> cmap
-}
runWeightedSetTWeightedSet :: (Ord a, Monad m, Monoid w) => WeightedSetT w m a -> m (WeightedSet w a)
runWeightedSetTWeightedSet = iterT (fmap factorFoldWeightedSets . mapM strength . runWeightedSetInternal) . fmap pure . getWeightedSetT
  where
    strength :: Functor m => (m a, b) -> m (a, b)
    strength (ma, b) = ( , b) <$> ma

pushM :: (Monad m) => m (WeightedSetT w m b) -> WeightedSetT w m b
pushM = WeightedSetT . FreeT . ((runFreeT . getWeightedSetT) =<<)

fromWeightedSet :: (Monad m, Monoid w) => WeightedSet w a -> WeightedSetT w m a
fromWeightedSet = WeightedSetT . liftF

instance (Ord a, Monad m, Monoid w) => Semigroup (WeightedSetT w m a) where
  weightedSet1 <> weightedSet2 = pushM $ fmap fromWeightedSet $ (<>) <$> runWeightedSetTWeightedSet weightedSet1 <*> runWeightedSetTWeightedSet weightedSet2

instance (Ord a, Monad m, Monoid w) => Monoid (WeightedSetT w m a) where
  mempty = fromWeightedSet mempty

-- Internal use only, because it exposes the structure of the map and doesn't delete duplicates
runWeightedSetTInternal :: (Monoid w, Monad m) => WeightedSetT w m b -> m [(b, w)]
runWeightedSetTInternal = iterT runWeightedSetInternalM . fmap (\b -> [(b, mempty)]) . getWeightedSetT
  where
    runWeightedSetInternalM :: (Monoid w, Monad m) => WeightedSet w (m [(a, w)]) -> m [(a, w)]
    runWeightedSetInternalM = fmap Prelude.concat . mapM (\(as, w) -> map (second (mappend w)) <$> as) . runWeightedSetInternal

-- | Removes duplicates and re-sorts the internal structure.
--   O(n * log (n)) in the current size of the list.
--   Can save space, and time in future calculations.
optimize :: (Ord b, Monad m, Monoid w) => WeightedSetT w m b -> WeightedSetT w m b
optimize = pushM . fmap fromWeightedSet . runWeightedSetTWeightedSet

-- | Efficiently concatenates the sets while removing duplicates as much and early as possible
concat :: (Ord b, Monad m, Monoid w) => WeightedSetT w m (WeightedSetT w m b) -> WeightedSetT w m b
concat = optimize . (optimize =<<)

runWeightedSetT :: (Ord b, Monad m, Monoid w) => WeightedSetT w m b -> m [(b, w)]
runWeightedSetT = fmap (WeightedSet.runWeightedSet . runWeightedSet) . runWeightedSetTWeightedSet

{-
Generalisation:

* Weighted by binary number probability (find library for that) gives a compression scheme for the type
  * Yet to figure out how to sensibly encode floats and integers
  * Time series inference gives time series compression
-}
-- FIXME Aliases (also Coyoneda) for
-- * w = (): Regular set
-- * Num w (only sum? not product?): Counting set (multiset?)
