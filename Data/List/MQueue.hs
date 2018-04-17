module Data.List.MQueue where

import qualified Data.List.MList as MList
import           Data.List.MList   (MList)
import           Data.Ix           (inRange, rangeSize)
import           GHC.IORef
import           Data.StateVar
import           Control.Monad

data MQueue a = MQueue {
    list :: MList a
  , bound :: StateVar (Int, Int)
}

newStateVar :: a -> IO (StateVar a)
newStateVar x = do xref <- newIORef x
                   return (StateVar (readIORef xref) (writeIORef xref)) 

new :: IO (MQueue a)
new = do l    <- MList.new :: (IO (MList a))
         sz   <- get . MList.size $ l
         bd   <- newStateVar (0, sz-1)
         return $ MQueue l bd

(!) :: MQueue a -> Int -> IO a
(!) mQueue i 
    = do bd <- get . bound $ mQueue
         if inRange bd i
            then (list mQueue) MList.! i
            else error "Out of Range!"

(!=) :: MQueue a -> (Int, a) -> IO ()
(!=) mQueue (i,e)
    = do bd <- get . bound $ mQueue
         if inRange bd i
            then (list mQueue) MList.!= (i,e)
            else error "Out of Rangsize e!"

push_back :: MQueue a -> a -> IO ()
push_back (MQueue l bd) e
    = do MList.push_back l e
         bd $~ (\(a,b) -> (a,b+1))

pop_back :: MQueue a -> IO ()
pop_back (MQueue l bd)
    = do (MList.size l) $~ ((-) 1)
         bd $~ (\(a,b) -> (a, b-1))

pop_front :: MQueue a -> IO ()
pop_front (MQueue l bd)
    = do bd $~ (\(a,b) -> (a+1, b))

size :: MQueue a -> IO Int
size = (return . rangeSize) <=< (get . bound) 

head :: MQueue a -> IO a
head (MQueue lst bd) 
    = do (l, _) <- get bd
         lst MList.! l

tail :: MQueue a -> IO a
tail (MQueue lst bd)
    = do (_, r) <- get bd
         lst MList.! r