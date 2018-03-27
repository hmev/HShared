module Data.Map.MMap where

import qualified Data.Map      as Map
import           GHC.IORef
import           Data.StateVar
import           Control.Monad

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

lookup :: (Ord k) => MMap k a -> k -> IO (Maybe (MNode a))
lookup mmap k   = do map <- get mmap
                     return (Map.lookup k map)

member :: (Ord k) => MMap k a -> k -> IO Bool
member mmap k   = do map <- get mmap 
                     return (Map.member k map)

update :: (Ord k) => MMap k a -> (a -> Maybe a) -> k -> IO ()
update mmap f k = do Just node <- Data.Map.MMap.lookup mmap k
                     val <- get node
                     case f val of 
                        Nothing -> delete mmap k
                        Just newval -> do node $= newval 

showIO :: (Ord k, Show a) => MMap k a -> IO ()
showIO mmap = do map <- get mmap
                 mapM_ (putStrLn <=< (return . show) <=< get) map