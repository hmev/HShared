module Element.Element where

type ElementId = Int
type Episode = Int

data Element = Element {
    name :: String
  , modified :: Episode
} deriving (Show, Read, Eq)
