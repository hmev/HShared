module Command.ElementEdition where
import qualified Data.Map.MMap          as MMap
import qualified Data.List.MQueue       as MQueue
import           Element.Element        as Elem
import           Element.ElementManager   as ElemT
import           Element.Episode        as Eps
import           Element.EpisodeManager as EpsMgr
import           Command.Command        as CMMD
import           Command.Memento        as Mem

import           Data.StateVar
import           Control.Monad

import           Command.Manager

import           Utility

-- todo --

todo :: Command -> CommandParams -> Manager -> IO Manager
todo Create (sId:sName:[]) mgr@(Manager elemM epsM memM)
    = do id   <- return . read $ sId
         eps  <- (return . current) <=< get $ epsM
         let elem  = Element sName (ElementHistory eps)
         -- add element
         MMap.insert (table elemM) id elem
         -- add memento
         let mem = Memento Create (ElemMem id elem)
         memM' <- addMemento memM mem
         return (Manager   elemM
                           epsM
                           memM'
                )

todo Edit   (sId:sNewName:[]) mgr@(Manager elemM epsM memM)
    = do id   <- return . read $ sId
         eps  <- (return . current) <=< get $ epsM
         let elem = Element sNewName (ElementHistory eps)
         -- add memento
         oldElem <- (table elemM) MMap.! id
         let mem = Memento Edit (ElemMem id oldElem)
         memM' <- addMemento memM mem
         -- edit element
         (table elemM) MMap.!= (id, elem)
         return (Manager   elemM
                           epsM
                           memM'
                )

todo Delete (sId:[]) mgr@(Manager elemM epsM memM)
    = do id <- return . read $ sId
         -- add memento 
         oldElem <- (table elemM) MMap.! id
         let mem =  Memento Delete (ElemMem id oldElem)
         memM' <- addMemento memM mem
         -- delete element
         MMap.delete (table elemM) id
         return (Manager   elemM
                           epsM
                           memM'
                )

-- undo --

undo :: Command -> Manager -> IO Manager
undo Create mgr@(Manager elemM epsM memM)
    = do Memento Create (ElemMem id _) <- getUndo memM
         -- delete element
         MMap.delete (table elemM) id
         -- deal with memento 
         memM' <- moveBackward memM
         return (Manager   elemM
                           epsM
                           memM'
                )

undo Edit   mgr@(Manager elemM epsM memM)
    = do Memento Edit (ElemMem id oldElem) <- getUndo memM
         elem <- (table elemM) MMap.! id
         let newMem = Memento Edit (ElemMem id elem)
         -- roll back element
         (table elemM) MMap.!= (id, oldElem)
         -- revise memento
         memM' <- moveBackward memM
         (get' memM') MQueue.!= (curr memM', newMem)
         return (Manager   elemM
                           epsM
                           memM'
                )

undo Delete mgr@(Manager elemM epsM memM)
    = do Memento Delete (ElemMem id elem) <- getUndo memM
         -- add back element
         MMap.insert (table elemM) id elem
         -- revise memento 
         memM' <- moveBackward memM
         return (Manager   elemM
                           epsM
                           memM'
                )

-- -- redo --

redo :: Command -> Manager -> IO Manager
redo Create mgr@(Manager elemM epsM memM)
    = do Memento Create (ElemMem id elem) <- getRedo memM
         -- delete element
         MMap.insert (table elemM) id elem
         -- deal with memento 
         memM' <- moveForward memM
         return (Manager elemM
                         epsM
                         memM'
                )

redo Edit   mgr@(Manager elemM epsM memM)
    = do Memento Edit (ElemMem id newElem) <- getRedo memM
         oldElem <- (table elemM) MMap.! id
         putStrLn . show $ oldElem
         let newMem = Memento Edit (ElemMem id oldElem)
         -- roll back element
         (table elemM) MMap.!= (id, newElem)
         -- revise memento
         (get' memM) MQueue.!= (curr memM, newMem)
         memM' <- moveForward memM
         return (Manager   elemM
                           epsM
                           memM'
                )

redo Delete mgr@(Manager elemM epsM memM)
    = do Memento Delete (ElemMem id _) <- getRedo memM
         -- add back element
         MMap.delete (table elemM) id
         -- revise memento 
         memM'<- moveForward memM
         return (Manager   elemM
                           epsM
                           memM'
                )