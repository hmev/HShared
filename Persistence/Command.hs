module Persistence.Command where
import Command.Command

import System.IO (Handle, hIsEOF, hPutStrLn, hGetLine)

serialize :: Handle -> [(Command, CommandParams)] -> IO ()
serialize h commands = mapM_ (\(cm, params) -> hPutStrLn h (show cm ++ " " ++ unwords params)) commands

deserialize   :: Handle -> IO [(Command, CommandParams)]
deserialize   h = do line <- deserializeLine h
                     eof  <- hIsEOF h
                     if eof then return [line]
                            else do lines <- deserialize h
                                    return (line:lines)

deserializeLine :: Handle -> IO (Command, CommandParams)
deserializeLine h = hGetLine h >>= (return . words) 
                               >>= (\(cm:params) -> return (read cm :: Command, params))