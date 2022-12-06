module Day05 (printDay05)  where

import Data.List (uncons)
import Control.Monad ((<=<), guard)
import Control.Arrow ((***), (&&&), first, second)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Maybe

isChar :: (Char, String) -> Maybe String
isChar =   uncurry ($>)
       .   first guard
       <=< uncurry fmap
       .   (first . (==) *** uncons)

parseCrate :: String -> Maybe (Maybe Char, String)
parseCrate =   uncurry fmap
           .   ((,) . Just *** curry isChar ']')
           <=< uncons
           <=< (curry isChar '[')

parseEmpty :: String -> Maybe (Maybe Char, String)
parseEmpty =   fmap ((,) Nothing)
           .   (curry isChar ' ')
           <=< (curry isChar ' ')
           <=< (curry isChar ' ')

parseCrateOrEmpty :: String -> Maybe (Maybe Char, String)
parseCrateOrEmpty = uncurry (<|>)
                  . (parseCrate &&& parseEmpty)

parseCrateOrEmptyWithSpace :: String -> Maybe (Maybe Char, String)
parseCrateOrEmptyWithSpace =   uncurry fmap
                           .   ((,) *** (curry isChar ' '))
                           <=< parseCrateOrEmpty

parseLine :: String -> [Maybe Char]
parseLine = _
          . takeWhile isJust 
          . tail
          . iterate (parseCrateOrEmptyWithSpace . snd  =<<)
          . Just
          . (,) Nothing

part1 :: String -> Maybe Int
part1 = const Nothing

part2 :: String -> Maybe Int
part2 = const Nothing

printDay05 :: IO ()
printDay05 = do
  file <- readFile "data/day05"
  putStrLn "-- DAY 05 --"
  putStr "  Part 1: "
  putStrLn $ maybe "something went wrong :(" show $ part1 file
  putStr "  Part 2: "
  putStrLn $ maybe "something went wrong :(" show $ part2 file
