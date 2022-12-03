module Lib.Utils where

import Data.Text qualified as T
import System.Environment

getFile :: MonadFail m => [a] -> m a
getFile [file] = return file
getFile _ = fail "pass in file arg"

getArgFile :: IO T.Text
getArgFile = do
    file <- getArgs >>= getFile
    fmap T.pack $ readFile file
