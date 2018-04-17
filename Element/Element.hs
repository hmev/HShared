module Element.Element where

type ElementId = Int
type Episode = Int

data ElementHistory = ElementHistory {
    modified :: Episode
} deriving (Show, Read, Eq)

data Element = Element {
    name :: String
  , epis :: ElementHistory
} deriving (Show, Read, Eq)
