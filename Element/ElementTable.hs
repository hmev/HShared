module Element.ElementTable where
import qualified Data.Map.MMap as MMap

import Element.Element

import Data.StateVar (get)
import Control.Monad 

type MElemTable = MMap.MMap ElementId Element

showIO :: MElemTable -> IO ()
showIO table = do map <- get table
                  mapM_ (putStrLn <=< (return . show) <=< get) map

new :: IO MElemTable
new = MMap.new