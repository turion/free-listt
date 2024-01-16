-- | Free Tree monad transformer
module Control.Monad.Trans.Tree where

-- base
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Classes

-- transformers
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Error.Class
import Control.Monad.RWS (MonadRWS, MonadReader, MonadState)
import Control.Monad.Writer

-- exceptions
import Control.Monad.Catch (MonadCatch, MonadThrow)

-- free
import Control.Monad.Trans.Free

-- free-listt
import Control.Applicative.Trans.List qualified as Applicative

{- | The tree monad transformer.

It is implemented as a rose tree (see https://en.wikipedia.org/wiki/Rose_tree)
of computations in @m@.
Each layer of the tree is a list of computations.
In many situations, this type can be used as a drop-in replacement for a list monad transformer.
-}
newtype TreeT m a = TreeT {getTreeT :: FreeT [] m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , MonadIO
    , MonadFail
    , MonadTrans
    , Eq1
    , Ord1
    , Read1
    , Show1
    , Eq
    , Ord
    , Read
    , Show
    , MonadError e
    , MonadState s
    , MonadReader r
    , MonadRWS r w s
    , MonadWriter w
    , MonadCatch
    , MonadThrow
    )

-- | Flatten all layers of computations to a single one.
flatten :: (Monad m) => TreeT m a -> Applicative.ListT m a
flatten = Applicative.ListT . runTreeT

-- | Like 'flatten', but remove the 'Applicative.ListT' wrapper.
runTreeT :: (Monad m) => TreeT m a -> m [a]
runTreeT = iterT (fmap concat . sequence) . fmap pure . getTreeT

-- | Construct a 'TreeT' from an effectful list (a single layer).
treeT :: (Monad m) => m [a] -> TreeT m a
treeT = TreeT . FreeT . fmap (Free . fmap pure)

-- | Construct a 'TreeT' from a single effectful value.
singleton :: (Functor m) => m a -> TreeT m a
singleton = TreeT . FreeT . fmap Pure
