module Element.ElementManager where
import qualified Data.Map.MMap as MMap

import Utility
import Element.Element
import Control.Monad 

data ElementManager = ElementManager {
    table :: MMap.MMap ElementId Element
}

instance IOShow ElementManager where
    showIO et = do map <- get . table $ et
                   mapM_ (putStrLn <=< (return . show) <=< get) map

new :: IO ElementManager
new = do table <- MMap.new
         return $ ElementManager table