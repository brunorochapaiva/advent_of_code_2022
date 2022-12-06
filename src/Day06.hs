module Day06 (printDay06)  where

import Data.List (tails, nub, findIndex)
import Control.Arrow ((***), (&&&), first, second)

findDistinct :: (Int, String) -> Maybe Int
findDistinct = uncurry fmap
             . first (+)
             . (fst &&& ( findIndex id
                        . uncurry (zipWith ($))
                        . ((fmap (==) . repeat) *** (fmap (length . nub)))
                        ))
             . (fst &&& (uncurry (zipWith take) . first repeat))
             . second tails
{--
So this was actually quite a readable solution, but to avoid duplicating
code (and keep stuff point free), I decided to come up with the ungodly
findDistinct function. Still though, I wanted to keep the nice solution around.

part1 :: String -> Maybe Int
part1 = fmap (+4)
      . findIndex
        ( (==4)
        . length
        . nub
        )
      . fmap (take 4)
      . tails
--}

part1 :: String -> Maybe Int
part1 = curry findDistinct 4

part2 :: String -> Maybe Int
part2 = curry findDistinct 14

printDay06 :: IO ()
printDay06 = do
  file <- readFile "data/day06"
  putStrLn "-- DAY 06 --"
  putStr "  Part 1: "
  putStrLn $ maybe "something went wrong :(" show $ part1 file
  putStr "  Part 2: "
  putStrLn $ maybe "something went wrong :(" show $ part2 file
