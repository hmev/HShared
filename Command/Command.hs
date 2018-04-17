module Command.Command where

data Command = Create
             | Delete 
             | Edit
             | Save
             | Show
             | Quit
             deriving (Show, Read, Eq)

type CommandParams = [String]