module Day03 (printDay03)  where

import Data.Maybe (listToMaybe)
import Data.List (intersect, elemIndex)

import Control.Arrow ((&&&), second)
import Control.Monad ((<=<))

priority :: Char -> Maybe Int
priority = fmap (+1) . flip elemIndex (['a' .. 'z'] ++ ['A' .. 'Z'])

part1 :: String -> Maybe Int
part1 = fmap sum
      . traverse
        ( (priority <=< listToMaybe)
        . uncurry intersect
        . uncurry splitAt
        . (((flip div 2) . length) &&& id)
        )
      . lines

part2 :: String -> Maybe Int
part2 = fmap sum
      . traverse
        (   priority
        <=< listToMaybe
        <=< (fmap (uncurry intersect . second (uncurry intersect)) . listToMaybe)
        ) 
      . takeWhile (not . null)
      . iterate (drop 3)
      . uncurry zip
      . fmap (uncurry zip)
      . (id &&& (drop 1 &&& drop 2))
      . lines

printDay03 :: IO ()
printDay03 = do
  file <- readFile "data/day03"
  putStrLn "-- DAY 03 --"
  putStr "  Part 1: "
  putStrLn $ maybe "something went wrong :(" show (part1 file)
  putStr "  Part 2: "
  putStrLn $ maybe "something went wrong :(" show (part2 file)
