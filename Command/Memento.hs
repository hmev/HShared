module Command.Memento where

import           Utility
import           Element.Element
import qualified Data.List.MQueue as MQueue
import           Data.List.MQueue   (MQueue, (!), (!=))
import           Data.StateVar
import           Data.IORef
import           Control.Monad
import           Command.Command

data Memento = Memento  Command Mem
--             | Mementos [Memento]
               deriving (Show, Read, Eq)

data MementoManager = MementoManager {
    get' :: MQueue Memento
  , curr :: Int
}

data Mem = Empty
         | ElemMem ElementId Element
         deriving (Show, Read, Eq)

new :: IO MementoManager
new = do mems <- MQueue.new
         curr <- return 0
         return (MementoManager mems curr)

addMemento :: MementoManager -> Memento -> IO MementoManager
addMemento memM mem
    = do MementoManager mems curr <- clearFuture memM
         MQueue.push_back mems mem
         return $ MementoManager mems (curr + 1)

moveForward :: MementoManager -> IO MementoManager
moveForward (MementoManager mems curr)
    = do bound <- get . MQueue.bound $ mems
         if (curr <= snd bound) 
            then return $ MementoManager mems (curr+1)
            else error "operation could not complete."

moveBackward :: MementoManager -> IO MementoManager
moveBackward (MementoManager mems curr)
    = do bound <- get . MQueue.bound $ mems
         if (curr > fst bound) 
            then return $ MementoManager mems (curr-1)
            else error "operation could not complete."

clearFuture :: MementoManager -> IO MementoManager
clearFuture (MementoManager mems curr) 
    = do sz <- MQueue.size mems
         forM_ [1..(sz-curr)] (\_ -> 
                MQueue.pop_back  mems
            )
         bd <- get (MQueue.bound mems)
         return $ MementoManager mems ((snd bd) + 1)

clearPast   :: MementoManager -> IO MementoManager
clearPast   (MementoManager mems curr)
    = do forM_ [0..(curr-1)]  (\_ ->
                MQueue.pop_front mems 
            )
         bd <- get (MQueue.bound mems)
         return $ MementoManager mems (fst bd)

getUndo :: MementoManager -> IO Memento
getUndo (MementoManager mems curr) 
    = do mems ! (curr - 1)
         
getRedo :: MementoManager -> IO Memento
getRedo (MementoManager mems curr) 
    = do mems ! curr

getUndoCommand :: MementoManager -> IO Command
getUndoCommand mgr = do Memento cmd _ <- getUndo mgr
                        return cmd

getRedoCommand :: MementoManager -> IO Command
getRedoCommand mgr = do Memento cmd _ <- getRedo mgr
                        return cmd        

---

instance IOShow MementoManager where
    showIO (MementoManager mem curr) 
        = do get (MQueue.bound mem) >>= (\bd -> putStrLn ("bound = " ++ show bd) )
             putStrLn ("curr = " ++ show curr)
             MQueue.showIO mem