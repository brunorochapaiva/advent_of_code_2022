module Day01 (printDay01) where

import Data.List (tails, sort)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

getTotals :: String -> Maybe [Int]
getTotals = traverse
            ( fmap sum
            . traverse readMaybe
            . takeWhile (not . null)
            . tail
            )
          . filter (or . fmap null . listToMaybe)
          . tails
          . ("" :)
          . lines

part1 :: String -> Maybe Int
part1 = fmap maximum . getTotals

part2 :: String -> Maybe Int
part2 = fmap (sum . take 3 . reverse . sort) . getTotals

printDay01 :: IO ()
printDay01 = do
  file <- readFile "data/day01"
  putStrLn "-- DAY 01 --"
  putStr "  Part 1: "
  putStrLn $ maybe "something went wrong :(" show (part1 file)
  putStr "  Part 2: "
  putStrLn $ maybe "Something went wrong :(" show (part2 file)
