import qualified Data.Map.MMap        as MMap
import qualified Element.Element      as Elem
import qualified Element.ElementTable as ElemT
import qualified Element.EpisodeManager  as EpsMgr
import           Command.Manager         as Mgr
import qualified Command.ElementEdition  as CMMD

test :: IO ()
test = do mElemT <- MMap.new 
          mEpsMgr <- EpsMgr.new
          loop (Manager mElemT mEpsMgr)
          return ()

loop :: Manager -> IO Manager
loop mgr@(Manager mElemTable mEpiMgr) 
     = do commandline <- getLine
          case words commandline of
             "quit":ss -> return mgr
             commands -> do CMMD.parse commands (Manager mElemTable mEpiMgr) 
                            loop (Manager mElemTable mEpiMgr)