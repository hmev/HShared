module Command.Manager where

import           Element.ElementManager   (ElementManager)
import           Element.IntervalManager  (IntervalManager)
import           Command.Memento          (MementoManager)

import qualified Element.ElementManager   as ElementManager
import qualified Element.IntervalManager  as IntervalManager
import qualified Command.Memento          as MementoManager

data Manager = Manager {
      getElemT  :: ElementManager
    , getEpsMgr :: IntervalManager
    , getMemMgr :: MementoManager
}

new :: IO Manager 
new = do elemM <- ElementManager.new
         epsM  <- IntervalManager.new
         memM  <- MementoManager.new
         return (Manager elemM epsM memM)