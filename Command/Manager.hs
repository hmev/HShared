module Command.Manager where

import Element.ElementTable   (MElemTable)
import Element.EpisodeManager (MEpisodeManager)

data Manager = Manager {
      getElemT  :: MElemTable
    , getEpsMgr :: MEpisodeManager
}