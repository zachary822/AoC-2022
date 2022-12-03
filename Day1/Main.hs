{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Ord
import Data.Text qualified as T
import Lib.Utils

main :: IO ()
main = do
    text <- getArgFile

    let elf = T.splitOn "\n\n" text
    let elfCals = [sum (map (read . T.unpack :: T.Text -> Int) $ filter (not . T.null) $ T.splitOn "\n" e) | e <- elf]

    print $ sum $ take 3 $ sortBy (comparing Down) elfCals
