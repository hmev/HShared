import           Control.Monad          (foldM, (>>))
import           Control.Exception      (catch, SomeException)

import qualified Data.Map.MMap        as MMap
import           Command.Command      as CMMD
import           Command.Manager      as Mgr
import           Command.Switcher

import           Persistence.Command
import           System.IO

operateLine :: Manager -> (Command, CommandParams) -> IO Manager
operateLine mgr (command, params)
    = do case command of 
              Quit -> return mgr
              otherwise -> 
                 do case command of 
                         Undo      -> do undo mgr
                         Redo      -> do redo mgr
                         otherwise -> do putStrLn ""
                                         putStrLn ("Command : " ++ show command)
                                         todo command params mgr

-- test :: IO ()
-- test = do mgr <- new :: IO Manager
--           loop mgr
--           return ()

-- loop :: Manager -> IO Manager
-- loop mgr
--      = do commands <- getLine >>= (return . words)
--           command  <- catch ((return . read . head $ commands) :: IO Command)
--                             (ioError)
--           mgr <- operate mgr (command, tail commands)
--           loop mgr

handler :: SomeException -> IO [(Command, CommandParams)]
handler e = (putStrLn "File is corrupted.") 
         >> (return ([]::[(Command, CommandParams)]) )

testFromFile :: IO ()
testFromFile = do mgr <- new :: IO Manager
                  handle <- openFile "Test/TestFile" ReadMode
                  commands <- catch (deserialize handle)
                                    handler
                  foldM operateLine mgr commands
                  return ()

                  