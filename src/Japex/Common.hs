module Japex.Common
    (
        Command(Command, commandFunc, helpFunc)
    ) where

data Command = Command {
        commandFunc :: [String] -> IO()
        , helpFunc :: IO ()
    }
