module Command.Manager where

import           Element.ElementTable      (MElemTable)
import           Element.EpisodeManager    (MEpisodeManager)
import           Command.Memento           (MementoManager)

import qualified Element.ElementTable   as MElemTable
import qualified Element.EpisodeManager as MEpisodeManager
import qualified Command.Memento        as MementoManager

data Manager = Manager {
      getElemT  :: MElemTable
    , getEpsMgr :: MEpisodeManager
    , getMemMgr :: MementoManager
}

new :: IO Manager 
new = do elemT  <- MElemTable.new
         epsMgr <- MEpisodeManager.new
         memMgr <- MementoManager.new
         return (Manager elemT epsMgr memMgr)