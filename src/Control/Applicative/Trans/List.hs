module Control.Applicative.Trans.List where

-- base
import Data.Functor.Compose

newtype ListT f a = ListT {getListT :: f [a]}
    deriving (Functor)
    deriving
        (Applicative)
        via (Compose f [])
