module Data.List.MList where

import qualified Data.Array.IO    as IOArray
import           Data.Array.IO      (IOArray)
import           Data.IORef
import           Data.StateVar
import           Control.Monad

data MList a = MList {
    array :: StateVar (IOArray Int a)
  , size  :: StateVar Int
  , realsize :: StateVar Int
}

initialSize :: Int
initialSize = 3

newStateVar :: a -> IO (StateVar a)
newStateVar x = do xref <- newIORef x
                   return (StateVar (readIORef xref) (writeIORef xref)) 

new :: IO (MList a)
new = do size_ <- newStateVar 0
         realsize_ <- newStateVar initialSize
         array_ <- (IOArray.newArray_ (0, initialSize-1)) >>= newStateVar
         return (MList array_ size_ realsize_)

newWithSize :: Int -> IO (MList a)
newWithSize sz = do size_ <- newStateVar 0
                    realsize_ <- newStateVar sz
                    array_ <- (IOArray.newArray_ (0, sz-1)) >>= newStateVar
                    return (MList array_ size_ realsize_)

fromList :: [a] -> IO (MList a)
fromList l = do sz <- return (length l)
                size_ <- newStateVar sz
                realsize_ <- newStateVar sz
                array_ <- (IOArray.newArray_ (0, sz-1)) >>= newStateVar
                arr <- get array_
                forM_ [0..(sz-1)] 
                    (\i -> do e <- return (l !! i)
                              IOArray.writeArray arr i e 
                    )
                return (MList array_ size_ realsize_)         

(!)       :: MList a -> Int -> IO a
(!) mList i = do sz  <- get . size $ mList
                 if (i >= sz) 
                    then error "Out of Range!"
                    else do arr <- get . array $ mList
                            IOArray.readArray arr i

(!=)      :: MList a -> (Int, a) -> IO ()
(!=) mList (i, e) = do sz  <- get . size $ mList
                       if (i >= sz) 
                          then error "Out of Range!"
                          else do arr <- get . array $ mList
                                  IOArray.writeArray arr i e

push_back :: MList a -> a -> IO ()
push_back mList e
    = do checkscale mList 1
         (size mList) $~ (+1)
         s <- get . size $ mList
         mList != (s-1, e)
        
-- private 

rescale   :: MList a -> Int -> IO ()
rescale mList newSize 
    = do sz <- get . realsize $ mList
         if (sz < newSize) 
            then do arr <- get . array $ mList
                    newarr <- IOArray.newArray_ (0, newSize-1)
                    realsize mList $= newSize
                    copyTo arr newarr
                    array mList $= newarr
            else return ()

checkscale :: MList a -> Int -> IO ()
checkscale mList addSize
    = do sz   <- get .     size $ mList
         rlsz <- get . realsize $ mList
         if (sz + addSize <= rlsz)
            then return ()
            else if (sz + addSize <= rlsz * 2)
                    then rescale mList (rlsz * 2)
                    else rescale mList (rlsz * 2 + addSize)

copyTo :: IOArray Int a -> IOArray Int a -> IO ()
copyTo arr1 arr2 
    = do sz1 <- (return . IOArray.rangeSize) <=< IOArray.getBounds $ arr1
         forM_ [0..(sz1-1)] (\i -> do
                e <- IOArray.readArray arr1 i
                IOArray.writeArray arr2 i e 
            )