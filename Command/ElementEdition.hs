module Command.ElementEdition where
import qualified Data.Map.MMap          as MMap
import qualified Element.Element        as Elem
import qualified Element.ElementTable   as ElemT
import           Element.Episode        as Epsd
import           Element.EpisodeManager as EpsMgr

import           Data.StateVar
import           Control.Monad

import           Command.Manager

parse :: [String] -> Manager -> IO ()
parse ("new" :ss) (Manager mElemTable mEpsMgr)
    = do let id = read . head $ ss
             name = head . tail $ ss
         EpisodeManager curr <- get mEpsMgr
         MMap.insert mElemTable id (Elem.Element name curr)

parse ("edit":ss) (Manager mElemTable mEpsMgr)
    = do let id = read . head $ ss
             newname = head . tail $ ss
         EpisodeManager curr <- get mEpsMgr
         MMap.update mElemTable (\_ -> Just (Elem.Element newname curr)) id

parse ("show":ss) (Manager mElemTable       _) 
    = do MMap.showIO mElemTable

parse ("save":ss) (Manager          _ mEpsMgr)
    = do EpisodeManager curr <- get mEpsMgr
         mEpsMgr $= EpisodeManager (curr+1)
         putStrLn . show $ curr
         
parse _ _ 
    = do return ()