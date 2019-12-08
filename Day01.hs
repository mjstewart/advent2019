module Day01 where

import Common

solvePart1 :: IO Int
solvePart1 =
  sum . map mass <$> fileToLinesInt "inputs/Day01.txt"

solvePart2 :: IO Int
solvePart2 =
  sum . map f <$> fileToLinesInt "inputs/Day01.txt"
  where
    f = sum . drop 1 . takeWhile (>0) . iterate mass

mass :: Int -> Int
mass x = (x `div` 3) - 2
