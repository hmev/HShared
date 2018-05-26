{-# LANGUAGE ScopedTypeVariables#-}
module Data.Map.MMap where

import qualified Data.Map      as Map
import           Data.IORef
import           Data.StateVar
import           Control.Monad
import           Control.Exception (catch, SomeException)

type MNode  a = StateVar a
type MMap k a = StateVar (Map.Map k (MNode a))

newStateVar :: a -> IO (StateVar a)
newStateVar x = do xref <- newIORef x
                   return (StateVar (readIORef xref) (writeIORef xref)) 

new :: (Ord k) => IO (MMap k a)
new = newStateVar (Map.empty :: Map.Map k (StateVar a))

insert :: (Ord k) => MMap k a -> k -> a -> IO ()
insert mmap k a = do leaf <- newStateVar a
                     mmap $~ (Map.insert k leaf)    

delete :: (Ord k) => MMap k a -> k -> IO ()
delete mmap k   = do mmap $~ (Map.delete k)

lookup :: (Ord k) => MMap k a -> k -> IO (Maybe a)
lookup mmap k   = catch (do map <- get mmap
                            let Just node = Map.lookup k map
                            val <- get node
                            return $ Just val
                        )
                        (
                            \(e::SomeException) -> return Nothing
                        )

lookupForEdit :: (Ord k) => MMap k a -> k -> IO (Maybe (MNode a))
lookupForEdit mmap k   = do map <- get mmap
                            return (Map.lookup k map)

(!)    :: (Ord k) => MMap k a -> k -> IO a
(!)    mmap k   = do Just node <- Data.Map.MMap.lookup mmap k
                     return node
                     
member :: (Ord k) => MMap k a -> k -> IO Bool
member mmap k   = do map <- get mmap 
                     return (Map.member k map)

update :: (Ord k) => MMap k a -> (a -> Maybe a) -> k -> IO ()
update mmap f k = do Just node <- Data.Map.MMap.lookupForEdit mmap k
                     val <- get node
                     Just newVal <- return (f val)
                     node $= newVal

(!=)   :: (Ord k) => MMap k a -> (k, a) -> IO ()
(!=)  mmap (k,a)= do Just node <- Data.Map.MMap.lookupForEdit mmap k
                     node $= a

unbox :: (k, MNode a) -> IO (k, a)
unbox (k, mNode) = do v <- get mNode
                      return (k, v)