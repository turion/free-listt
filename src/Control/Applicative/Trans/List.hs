-- | The applicative list transformer
module Control.Applicative.Trans.List where

-- base
import Data.Functor.Compose

{- | The 'Applicative' list transformer.

This is isomorphic to the "old" @ListT@ transformer.
It is not a monad, but a lawful 'Applicative'.
-}
newtype ListT f a = ListT {getListT :: f [a]}
  deriving (Functor)
  deriving
    (Applicative)
    via (Compose f [])
