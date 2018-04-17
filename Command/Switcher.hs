module Command.Switcher where
import Command.Command
import Command.Manager
import qualified Command.FileEdition as CMMDFile
import qualified Command.ElementEdition as CMMDElement

todo :: Command -> CommandParams -> Manager -> IO ()
todo Create = CMMDElement.todo Create
todo Delete = CMMDElement.todo Delete
todo Edit   = CMMDElement.todo Edit

todo Save   = CMMDFile.todo Save
todo Show   = CMMDFile.todo Show