module Command.Command where

data Command = Create
             | Delete 
             | Edit
             -- file command
             | Save
             | Show
             | Quit
             -- memento command
             | Undo
             | Redo
             deriving (Show, Read, Eq)

type CommandParams = [String]

isElementCommand Create = True
isElementCommand Delete = True
isElementCommand Edit   = True
isElementCommand _      = True