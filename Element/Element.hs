module Element.Element where
import Element.Interval

type ElementId = Int

data ElementHistory = ElementHistory {
    modified :: IntervalId
} deriving (Show, Read, Eq)

data Element = Element {
    name :: String
  , epis :: ElementHistory
} deriving (Show, Read, Eq)
