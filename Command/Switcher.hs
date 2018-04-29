module Command.Switcher where
import Command.Command
import Command.Manager
import qualified Command.FileEdition as CMMDFile
import qualified Command.ElementEdition as CMMDElement
import qualified Command.Memento as Mem

todo :: Command -> CommandParams -> Manager -> IO Manager
todo Create = CMMDElement.todo Create
todo Delete = CMMDElement.todo Delete
todo Edit   = CMMDElement.todo Edit

todo Save   = CMMDFile.todo Save
todo Show   = CMMDFile.todo Show

undo :: Manager -> IO Manager
undo mgr = do command <- Mem.getUndoCommand . getMemMgr $ mgr
              putStrLn ("Command : Undo "  ++ show command)
              if (isElementCommand command)
                 then CMMDElement.undo command mgr
                 else do error "operation could not complete"
                         return mgr

redo :: Manager -> IO Manager
redo mgr = do command <- Mem.getRedoCommand . getMemMgr $ mgr
              putStrLn ("Command : Redo "  ++ show command)
              if (isElementCommand command)
                 then CMMDElement.redo command mgr
                 else do error "operation could not complete"
                         return mgr