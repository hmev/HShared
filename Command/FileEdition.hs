module Command.FileEdition where
import Command.Command
import Command.Manager

import Utility
import Element.ElementManager
import Element.IntervalManager

-- create  :: ModelPath -> IO Manager
-- create  = undefined

-- archive :: IO Manager -> ModelPath -> IO ()
-- archive = undefined

-- sync    :: IO Manager -> Central   -> IO ()
-- sync    = undefined


todo :: Command -> CommandParams -> Manager -> IO Manager
todo Show _ mgr@(Manager elemM _ memM)
    = do putStrLn ""
         putStrLn "-- start to show --"
         putStrLn "Element Table :"
         showIO elemM
         putStrLn ""
         putStrLn "Memento Manager :"
         showIO memM
         putStrLn "-- end showing --"
         putStrLn ""
         return mgr

todo Save _ mgr@(Manager elemM epsM memM)
    = do let curr = current epsM
         let epsM' = IntervalManager (get' epsM) (curr+1)
         putStrLn . show $ curr
         return (Manager   elemM
                           epsM'
                           memM
                )