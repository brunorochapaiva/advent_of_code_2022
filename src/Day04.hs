module Day04 (printDay04)  where

import Text.Read (readMaybe)
import Data.Function ((&))

import Control.Arrow ((***), (&&&), second)
import Control.Monad.Zip (mzip)

haveContainment :: ((Int, Int), (Int, Int)) -> Bool
haveContainment = or
                . uncurry fmap
                . ( (&) &&& const
                    [ uncurry (&&) . ((uncurry (<=) . (fst *** fst)) &&& (uncurry (>=) . (snd *** snd)))
                    , uncurry (&&) . ((uncurry (>=) . (fst *** fst)) &&& (uncurry (<=) . (snd *** snd)))
                    ]
                  )

haveOverlap :: ((Int, Int), (Int, Int)) -> Bool
haveOverlap = not . or
            . uncurry fmap
            . ( (&) &&& const
                [ uncurry (<) . (snd *** fst)
                , uncurry (>) . (fst *** snd)
                ]
              )

part1 :: String -> Maybe Int
part1 = fmap (length . filter haveContainment)
      . traverse
        ( uncurry mzip
        . ((uncurry mzip . (readMaybe *** readMaybe)) *** (uncurry mzip . (readMaybe *** readMaybe)))
        . ((second tail . span (/='-')) *** (second tail . span (/='-') . tail))
        . span (/=',')
        )
      . lines

part2 :: String -> Maybe Int
part2 = fmap (length . filter haveOverlap)
      . traverse
        ( uncurry mzip
        . ((uncurry mzip . (readMaybe *** readMaybe)) *** (uncurry mzip . (readMaybe *** readMaybe)))
        . ((second tail . span (/='-')) *** (second tail . span (/='-') . tail))
        . span (/=',')
        )
      . lines

printDay04 :: IO ()
printDay04 = do
  file <- readFile "data/day04"
  putStrLn "-- DAY 04 --"
  putStr "  Part 1: "
  putStrLn $ maybe "something went wrong :(" show $ part1 file
  putStr "  Part 2: "
  putStrLn $ maybe "something went wrong :(" show $ part2 file
