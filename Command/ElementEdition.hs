module Command.ElementEdition where
import qualified Data.Map.MMap          as MMap
import qualified Data.List.MQueue       as MQueue
import           Element.Element        as Elem
import qualified Element.ElementTable   as ElemT
import           Element.Episode        as Eps
import           Element.EpisodeManager as EpsMgr
import           Command.Command        as CMMD
import           Command.Memento        as Mem

import           Data.StateVar
import           Control.Monad

import           Command.Manager

-- parse :: [String] -> Manager -> IO ()
-- parse ("new" :ss) (Manager mElemTable mEpsMgr)
--     = do let id = read . head $ ss
--              name = head . tail $ ss
--          EpisodeManager curr <- get mEpsMgr
--          MMap.insert mElemTable id (Elem.Element name curr)

-- parse ("edit":ss) (Manager mElemTable mEpsMgr)
--     = do let id = read . head $ ss
--              newname = head . tail $ ss
--          EpisodeManager curr <- get mEpsMgr
--          MMap.update mElemTable (\_ -> Just (Elem.Element newname curr)) id

-- parse ("del":ss)  (Manager mElemTable mEpsMgr)
--     = do let id = read . head $ ss
--          EpisodeManager curr <- get mEpsMgr
--          MMap.delete mELemTable id

-- parse ("show":ss) (Manager mElemTable       _) 
--     = do MMap.showIO mElemTable

-- parse ("save":ss) (Manager          _ mEpsMgr)
--     = do EpisodeManager curr <- get mEpsMgr
--          mEpsMgr $= EpisodeManager (curr+1)
--          putStrLn . show $ curr
         
-- parse _ _ 
--     = do return ()

-- todo --

todo :: Command -> CommandParams -> Manager -> IO ()
todo Create (sId:sName:[]) (Manager mElemTable mEpsMgr mMemMgr)
    = do id   <- return . read $ sId
         eps  <- (return . current) <=< get $ mEpsMgr
         elem <- return (Element sName (ElementHistory eps))
         -- add element
         MMap.insert mElemTable id elem
         -- add memento
         mem <- return $ Memento Create (ElemMem id elem)
         addMemento mMemMgr mem

todo Edit   (sId:sNewName:[]) (Manager mElemTable mEpsMgr mMemMgr)
    = do id   <- return . read $ sId
         eps  <- (return . current) <=< get $ mEpsMgr
         elem <- return $ Element sNewName (ElementHistory eps)
         -- add memento
         oldElem <- mElemTable MMap.! id
         mem  <- return $ Memento Edit (ElemMem id oldElem)
         addMemento mMemMgr mem
         -- edit element
         mElemTable MMap.!= (id, elem)

todo Delete (sId:[]) (Manager mElemTable mEpsMgr mMemMgr)
    = do id <- return . read $ sId
         -- add memento 
         oldElem <- mElemTable MMap.! id
         mem <- return $ Memento Delete (ElemMem id oldElem)
         addMemento mMemMgr mem
         -- delete element
         MMap.delete mElemTable id

-- undo --

undo :: Command -> Manager -> IO ()
undo Create (Manager mElemTable mEpsMgr mMemMgr)
    = do Memento Create (ElemMem id _) <- getUndo mMemMgr
         -- delete element
         MMap.delete mElemTable id
         -- deal with memento 
         moveBackward mMemMgr

undo Edit   (Manager mElemTable mEpsMgr mMemMgr)
    = do Memento Edit (ElemMem id oldElem) <- getUndo mMemMgr
         elem <- mElemTable MMap.! id
         newMem <- return $ Memento Edit (ElemMem id elem)
         -- roll back element
         mElemTable MMap.!= (id, oldElem)
         -- revise memento
         moveBackward mMemMgr
         cr <- get . curr $ mMemMgr
         (get' mMemMgr) MQueue.!= (cr, newMem)

undo Delete (Manager mElemTable mEpsMgr mMemMgr)
    = do Memento Delete (ElemMem id elem) <- getUndo mMemMgr
         -- add back element
         MMap.insert mElemTable id elem
         -- revise memento 
         moveBackward mMemMgr

-- -- redo --

redo :: Command -> Manager -> IO ()
redo Create (Manager mElemTable mEpsMgr mMemMgr)
    = do Memento Create (ElemMem id elem) <- getRedo mMemMgr
         -- delete element
         MMap.insert mElemTable id elem
         -- deal with memento 
         moveForward mMemMgr

redo Edit   (Manager mElemTable mEpsMgr mMemMgr)
    = do Memento Edit (ElemMem id elem) <- getRedo mMemMgr
         oldElem <- mElemTable MMap.! id
         newMem <- return $ Memento Edit (ElemMem id oldElem)
         -- roll back element
         mElemTable MMap.!= (id, oldElem)
         -- revise memento
         cr <- (get . curr $ mMemMgr)
         (get' mMemMgr) MQueue.!= (cr, newMem)
         moveForward mMemMgr

redo Delete (Manager mElemTable mEpsMgr mMemMgr)
    = do Memento Delete (ElemMem id _) <- getRedo mMemMgr
         -- add back element
         MMap.delete mElemTable id
         -- revise memento 
         moveForward mMemMgr