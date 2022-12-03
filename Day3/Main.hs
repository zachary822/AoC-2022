{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.List
import Data.Maybe
import Data.Text qualified as T
import Lib.Utils

getPriority :: Char -> Maybe Int
getPriority c
    | c >= 'A' && c <= 'Z' = Just $ ord c - 38
    | c >= 'a' && c <= 'z' = Just $ ord c - 96
    | otherwise = Nothing

getErrant :: (Eq a) => [a] -> a
getErrant xs = head $ intersect c d
  where
    l = length xs
    (c, d) = splitAt (l `div` 2) xs

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = (take n xs) : (chunkList n $ drop n xs)

getBadge :: (Eq a) => [[a]] -> a
getBadge xs = head $ foldl1 intersect xs

main :: IO ()
main = do
    text <- getArgFile

    let ruckSacks = map T.unpack $ filter (not . T.null) $ T.splitOn "\n" text

    print $ "Q1: " ++ (show . sum . mapMaybe (getPriority . getErrant) $ ruckSacks)

    print $ "Q2: " ++ (show . sum . mapMaybe (getPriority . getBadge) $ chunkList 3 ruckSacks)
