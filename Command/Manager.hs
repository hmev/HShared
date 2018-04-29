module Command.Manager where

import           Element.ElementManager      (ElementManager)
import           Element.EpisodeManager    (MEpisodeManager)
import           Command.Memento           (MementoManager)

import qualified Element.ElementManager   as ElementManager
import qualified Element.EpisodeManager as MEpisodeManager
import qualified Command.Memento        as MementoManager

data Manager = Manager {
      getElemT  :: ElementManager
    , getEpsMgr :: MEpisodeManager
    , getMemMgr :: MementoManager
}

new :: IO Manager 
new = do elemM <- ElementManager.new
         epsM  <- MEpisodeManager.new
         memM  <- MementoManager.new
         return (Manager elemM epsM memM)