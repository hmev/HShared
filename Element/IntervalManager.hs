module Element.IntervalManager where
import Utility
import Element.Interval
import Data.Map.MMap as MMap
import Data.UUID
import Data.UUID.V1

data IntervalManager = IntervalManager {
    get'    :: MMap IntervalId Interval
  , current :: IntervalId
}

new :: IO IntervalManager
new = do map <- MMap.new
         endAndNewInterval $ IntervalManager map (-1)

endAndNewInterval :: IntervalManager -> IO IntervalManager
endAndNewInterval mgr = do Just uuid <- nextUUID
                           insert (get' mgr) (current mgr + 1) uuid
                           return $ IntervalManager (get' mgr) (current mgr + 1)