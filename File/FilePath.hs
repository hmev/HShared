module Persistence.Storage where
import System.IO (Handle)

data FilePath = LocalPath {
                  perfix :: String
                , path   :: String
                }
              -- | CloudPath 

data File = File FilePath Handle