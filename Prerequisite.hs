module Prerequisite (
      module Data.StateVar
    , module Data.IORef
    , newStateVar, Prerequisite.show
)where

import Data.IORef

import System.IO
import Control.Monad

newStateVar :: a -> IO (StateVar a)
newStateVar x = do xref <- newIORef x
                   return (StateVar (readIORef xref) (writeIORef xref)) 

show :: Show a => StateVar a -> IO String
show state = do val <- get state
                return (Prelude.show val)