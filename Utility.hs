module Utility (
    module Data.IORef
  , module Data.StateVar
  , module Control.Monad
  , IOShow, showIO
  , newStateVar
)where

import Data.IORef
import Data.StateVar
import Control.Monad
import System.IO

class IOShow a where
    showIO :: a -> IO ()

instance IOShow Int where
    showIO = putStrLn . show

instance (Show a, IOShow a) => IOShow (StateVar a) where
    showIO state = do val <- get state
                      putStrLn (show val)

newStateVar :: a -> IO (StateVar a)
newStateVar x = do xref <- newIORef x
                   return (StateVar (readIORef xref) (writeIORef xref)) 