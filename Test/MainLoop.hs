import qualified Data.Map.MMap        as MMap
import           Command.Command         as CMMD
import           Command.Manager         as Mgr
import           Command.Switcher

test :: IO ()
test = do mgr <- new :: IO Manager
          loop mgr
          return ()

loop :: Manager -> IO Manager
loop mgr
     = do commands <- getLine >>= (return . words)
          command  <- (return . read . head $ commands) :: IO Command
          if command == Quit then return mgr
                             else do todo command (tail commands) mgr
                                     loop mgr

-- fromFile :: Manager -> IO Manager
-- fromFile mgr = do handle <- openFile "\Test\TestFile" ReadMode
--                   let operateOnLine = do line <- hGetLine handle >>= (return . words)
--                                          command <- (return . read . head $ line) :: IO Command
--                                          | command == Quit -> return mgr
--                                          | otherwise -> todo command (tail commands) mgr