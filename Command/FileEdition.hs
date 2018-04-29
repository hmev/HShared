module Command.FileEdition where
import Command.Command
import Command.Manager

import Utility
import Element.ElementManager
import Element.EpisodeManager

-- create  :: ModelPath -> IO Manager
-- create  = undefined

-- archive :: IO Manager -> ModelPath -> IO ()
-- archive = undefined

-- sync    :: IO Manager -> Central   -> IO ()
-- sync    = undefined


todo :: Command -> CommandParams -> Manager -> IO Manager
todo Show _ mgr@(Manager mElemTable       _ memM)
    = do putStrLn ""
         putStrLn "-- start to show --"
         putStrLn "Element Table :"
         showIO mElemTable
         putStrLn ""
         putStrLn "Memento Manager :"
         showIO memM
         putStrLn "-- end showing --"
         putStrLn ""
         return mgr

todo Save _ mgr@(Manager          _ epsM memM)
    = do EpisodeManager curr <- get epsM
         epsM $= EpisodeManager (curr+1)
         putStrLn . show $ curr
         return mgr