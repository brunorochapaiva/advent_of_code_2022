module Day02 (printDay02)  where

import Data.Maybe (listToMaybe)
import Data.List (elemIndex)

import Control.Arrow ((***), (&&&), second)
import Control.Monad ((>=>))
import Control.Monad.Zip (mzip)

playToPoints :: Char -> Maybe Int
playToPoints = fmap ((+1) . flip mod 3) . flip elemIndex ['A', 'B', 'C', 'X', 'Y', 'Z']

outcome :: (Int, Int) -> Int
outcome = (*3) . flip mod 3 . (+1) . (uncurry subtract)

unoutcome :: (Int, Int) -> Int
unoutcome = (+1) . flip mod 3 . uncurry (+) . ((subtract 1) *** (flip mod 3 . subtract 1))

part1 :: String -> Maybe Int
part1 = fmap sum
      . traverse
        ( fmap (uncurry (+) . (outcome &&& snd))
        . uncurry mzip
        . ((listToMaybe >=> playToPoints) &&& ((listToMaybe >=> playToPoints) . tail . tail))
        )
      . lines

part2 :: String -> Maybe Int
part2 = fmap sum
      . traverse
        ( fmap (uncurry (+) . (unoutcome &&& ((*3) . snd)) . second (subtract 1))
        . uncurry mzip
        . ((listToMaybe >=> playToPoints) &&& ((listToMaybe >=> playToPoints) . tail . tail))
        )
      . lines

printDay02 :: IO ()
printDay02 = do
  file <- readFile "data/day02"
  putStrLn "-- DAY 02 --"
  putStr "  Part 1: "
  putStrLn $ maybe "something went wrong :(" show (part1 file)
  putStr "  Part 2: "
  putStrLn $ maybe "something went wrong :(" show (part2 file)
