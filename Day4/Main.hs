{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib.Utils
import Data.Text qualified as T

readPair :: T.Text -> [Int]
readPair t = map (read . T.unpack) $ T.split ((||) <$> (== '-') <*> (== ',')) t

checkPair :: T.Text -> Bool
checkPair t = (a1 <= b1 && a2 >= b2) || (b1 <= a1 && b2 >= a2)
  where
    a1:a2:b1:b2:[] = readPair t

checkPair' :: T.Text -> Bool
checkPair' t = (a2 >= b1 && a2 <= b2) || (b2 >= a1 && b2 <= a2)
  where
    a1:a2:b1:b2:[] = readPair t

main :: IO ()
main = do
  text <- getArgFile

  let pairs = T.splitOn "\n" $ T.strip text

  print $ "Q1: " ++ (show . foldl (\acc b -> if b then acc + 1 else acc) 0 $ map checkPair pairs)
  print $ "Q2: " ++ (show . foldl (\acc b -> if b then acc + 1 else acc) 0 $ map checkPair' pairs)
