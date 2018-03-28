{-
median.hs
Dakota Crowder
2018 March 27th
CSCE 331 Programming Language Concepts
-}

module Main where

import System.IO
import Data.List
import Data.Maybe
-- main, is the main hub for the program, runs "run" and "median"
main = do
  putStrLn "This program will calculate the median of an integer list"
  putStrLn "Please enter in the numbers in the list one at a time"
  let a = run
  a >>= median
  putStrLn "Compute another median? [y/n]"
  hFlush stdout
  line <- getLine
  if line == "y"
    then do main
    else do putStrLn "Goodbye"
-- run, runs the input of the program, converts it to an integer list
run = do
  putStrLn "Enter Integer (Nothing to stop input): "
  hFlush stdout
  line <- getLine
  if line == ""
    then do return []
    else do
      let n = read line
      rest <- run
      return ((toInteger n):rest)

-- median, takes the output from "getMedian" and converts it to IO
median :: (Ord a, Integral a, Show a) => [a] -> IO ()
median a = do
  let b = sort a
  putStrLn "The median of the list is "
  let c = getMedian b
  if c == Nothing then do
    putStrLn "Empty list has no median"
  else do
    print (fromMaybe 0 c)
-- getMedian, returns the median value from an integer list
getMedian [] = Nothing
getMedian a
        | odd (length a) = Just( a !! mid )
        | even (length a) = Just( ((a !! mid) + ((a !! mid) + 1)) `div` 2) where
          mid = (length a) `div` 2
