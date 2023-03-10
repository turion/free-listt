module Control.Monad.Trans.List where

-- base
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

-- transformers
import Control.Monad.Trans.Class

-- free
import Control.Monad.Trans.Free.Ap

newtype ListT m a = ListT { getListT :: FreeT [] m a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO, MonadTrans, Eq1, Ord1, Read1, Show1, Eq, Ord, Read, Show)
    -- FIXME derive more type classes from mtl, exceptions? See https://hackage.haskell.org/package/free-5.1.3/docs/Control-Monad-Trans-Free-Ap.html#t:FreeT
