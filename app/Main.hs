module Main (main) where

import qualified Application.App as App
import System.Environment (getArgs)

main :: IO ()
main = do
    -- command line argument for root directory
    args <- getArgs
    case args of
        [root] -> App.run root
