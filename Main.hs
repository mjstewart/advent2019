module Main where

import qualified Data.Text as T

main :: IO [T.Text]
main = T.lines . T.pack <$> readFile "inputs/Day01.txt"
