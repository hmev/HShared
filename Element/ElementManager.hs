module Element.ElementManager where
import Data.Map.MMap as MMap

import Utility
import Element.Element
import Control.Monad 

data ElementManager = ElementManager {
    getTable :: MMap.MMap ElementId Element
  , getNextId  :: ElementId
}

instance IOShow ElementManager where
    showIO et = do map <- get . getTable $ et
                   mapM_ (putStrLn <=< (return . show) <=< get) map

new :: IO ElementManager
new = do table <- MMap.new
         let nextId = 0
         return $ ElementManager table nextId

getElement :: ElementManager -> ElementId -> IO Element
getElement = getElementById

getElementById :: ElementManager -> ElementId -> IO Element
getElementById mgr id = getTable mgr ! id

addElement :: ElementManager -> Element -> IO ElementManager
addElement mgr elem
    = do insert (getTable mgr) (getNextId mgr) elem
         return $ ElementManager (getTable mgr) (getNextId mgr + 1)

addElementById :: ElementManager -> ElementId -> Element -> IO ElementManager
addElementById mgr id elem
    = do insert (getTable mgr) id elem
         let nextId = case (getNextId mgr < id) of
                        True -> id
                        False -> getNextId mgr
         return $ ElementManager (getTable mgr) nextId

delElement :: ElementManager -> ElementId -> IO ElementManager
delElement mgr id
    = do delete (getTable mgr) id
         return mgr

modElement :: ElementManager -> ElementId -> Element -> IO ElementManager
modElement mgr id elem = (getTable mgr != (id, elem)) >> return mgr