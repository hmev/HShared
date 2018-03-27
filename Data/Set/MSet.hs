module Data.Set.MSet where

import qualified Data.Set      as Set
import           GHC.IORef
import           Data.StateVar
import           Control.Monad

type MNode k = k
type MSet  k = StateVar (Set.Set k)

newStateVar :: a -> IO (StateVar a)
newStateVar x = do xref <- newIORef x
                   return (StateVar (readIORef xref) (writeIORef xref)) 

new :: (Ord k) => IO (MSet k)
new = newStateVar (Set.empty :: Set.Set k)

insert :: (Ord k) => MSet k -> k -> IO ()
insert mset k = do mset $~ (Set.insert k)    

delete :: (Ord k) => MSet k -> k -> IO ()
delete mset k = do mset $~ (Set.delete k)

member :: (Ord k) => MSet k -> k -> IO Bool
member mset k = do (return . (member k)) <=< (get mset) 

showIO :: (Ord k) => MSet k -> IO ()
showIO mset = do set <- get mset
                 mapM_ (putStrLn <=< (return . show) <=< get) set