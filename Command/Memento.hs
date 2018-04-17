module Command.Memento where

import           Element.Element
import qualified Data.List.MQueue as MQueue
import           Data.List.MQueue   (MQueue, (!), (!=))
import           Data.StateVar
import           GHC.IORef
import           Data.StateVar
import           Control.Monad
import           Command.Command

data Memento = Memento  Command Mem
--             | Mementos [Memento]

data MementoManager = MementoManager {
    get' :: MQueue Memento
  , curr :: StateVar Int
}

data Mem = Empty
         | ElemMem ElementId Element

newStateVar :: a -> IO (StateVar a)
newStateVar x = do xref <- newIORef x
                   return (StateVar (readIORef xref) (writeIORef xref)) 

new :: IO MementoManager
new = do mems <- MQueue.new
         curr <- newStateVar 0
         return (MementoManager mems curr)

addMemento :: MementoManager -> Memento -> IO ()
addMemento mgr mem
    = do clearFuture mgr
         MQueue.push_back (get' mgr) mem

moveForward :: MementoManager -> IO ()
moveForward (MementoManager mems curr)
    = do sz <- MQueue.size mems
         cr <- get curr
         if (cr < sz) 
            then curr $~ (+1)
            else error "operation could not complete."

moveBackward :: MementoManager -> IO ()
moveBackward (MementoManager mems curr)
    = do cr <- get curr
         if (cr > 0) 
            then curr $~ ((-) 1)
            else error "operation could not complete."

clearFuture :: MementoManager -> IO ()
clearFuture (MementoManager mems curr) 
    = do sz <- MQueue.size mems
         cr <- get curr
         forM_ [1..(sz-cr)] (\_ -> 
                MQueue.pop_back  mems
            )

clearPast   :: MementoManager -> IO ()
clearPast   (MementoManager mems curr)
    = do cr <- get curr
         forM_ [0..(cr-1)]  (\_ ->
                MQueue.pop_front mems 
            )

getUndo :: MementoManager -> IO Memento
getUndo (MementoManager mems curr) 
    = do cr <- get curr
         mems ! (cr - 1)
         
getRedo :: MementoManager -> IO Memento
getRedo (MementoManager mems curr) 
    = do cr <- get curr
         mems ! cr

getUndoCommand :: MementoManager -> IO Command
getUndoCommand mgr = do Memento cmd _ <- getUndo mgr
                        return cmd

getRedoCommand :: MementoManager -> IO Command
getRedoCommand mgr = do Memento cmd _ <- getRedo mgr
                        return cmd        

