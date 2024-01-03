-- | Free list monad transformer
module Control.Monad.Trans.List where

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

{- | The free list monad transformer.

It is implemented as a rose tree (see https://en.wikipedia.org/wiki/Rose_tree)
of computations in @m@.
-}
newtype ListT m a = ListT {getListT :: FreeT [] m a}
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
flatten :: (Monad m) => ListT m a -> Applicative.ListT m a
flatten = Applicative.ListT . runListT

-- | Like 'flatten', but remove the 'Applicative.ListT' wrapper.
runListT :: (Monad m) => ListT m a -> m [a]
runListT = iterT (fmap concat . sequence) . fmap pure . getListT

-- | Construct a 'ListT' from an effectful list (a single layer).
listT :: (Monad m) => m [a] -> ListT m a
listT = ListT . FreeT . fmap (Free . fmap pure)

-- | Construct a 'ListT' from a single effectful value.
singleton :: (Functor m) => m a -> ListT m a
singleton = ListT . FreeT . fmap Pure
