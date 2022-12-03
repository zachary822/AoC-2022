{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text qualified as T
import Lib.Utils

data Hand = A | B | C deriving (Read, Show, Eq)
data Result = X | Y | Z deriving (Read, Show)

--            0   3   6

beat :: Hand -> Hand -> Result
beat A C = Z
beat B A = Z
beat C B = Z
beat A B = X
beat B C = X
beat C A = X
beat _ _ = Y

scoreHand :: Hand -> Int
scoreHand A = 1
scoreHand B = 2
scoreHand C = 3

scoreResult :: Result -> Int
scoreResult X = 0
scoreResult Y = 3
scoreResult Z = 6

parseHand :: [String] -> (Hand, Result)
parseHand (a : b : _) = (read a, read b)

getHand :: Hand -> Result -> Hand
getHand A X = C
getHand B X = A
getHand C X = B
getHand A Z = B
getHand B Z = C
getHand C Z = A
getHand a Y = a

score :: Hand -> Result -> Int
score a b = scoreHand a + scoreResult b

main :: IO ()
main = do
    text <- getArgFile
    let games = map parseHand $ map (map T.unpack . T.splitOn " ") $ filter ((== 3) . T.length) $ T.splitOn "\n" text

    print $ sum $ map (\(a, b) -> score (getHand a b) b) games
