module Control.Monad.Trans.BinaryTree where

-- transformers
-- FIXME Strict or CPS?
import Control.Monad.Trans.Writer (WriterT)

-- freet
import Control.Monad.Trans.Free

data Branch a = Branch { leftBranch :: a, rightBranch :: a }

newtype BinaryTreeT m a = BinaryTreeT { getBinaryTreeT :: FreeT Branch m a }

-- FIXME See https://arxiv.org/ftp/arxiv/papers/2202/2202.09230.pdf and implement standard operations
newtype SimplicialSetT m a = SimplicialSetT { getSimplicialSetT :: BinaryTreeT (WriterT Bool m) a }
