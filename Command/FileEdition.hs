module Command.FileEdition where
import Command.Command
import Command.Manager

import Element.ElementTable (showIO)
import Element.EpisodeManager
import Data.StateVar

type ModelPath = String

-- create  :: ModelPath -> IO Manager
-- create  = undefined

-- archive :: IO Manager -> ModelPath -> IO ()
-- archive = undefined

-- sync    :: IO Manager -> Central   -> IO ()
-- sync    = undefined


todo :: Command -> CommandParams -> Manager -> IO ()
todo Show _ (Manager mElemTable       _ _)
    = do showIO mElemTable

todo Save _ (Manager          _ mEpsMgr _)
    = do EpisodeManager curr <- get mEpsMgr
         mEpsMgr $= EpisodeManager (curr+1)
         putStrLn . show $ curr