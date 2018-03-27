module Command.FileEdition where
import Command.Manager

type ModelPath = String

create  :: ModelPath -> IO Manager
create  = undefined

archive :: IO Manager -> ModelPath -> IO ()
archive = undefined

