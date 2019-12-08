{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Text.Read as TextRead
import qualified Data.Maybe as Maybe

linesToText :: FilePath -> IO [Text.Text]
linesToText =
  (Text.lines <$>) . TextIO.readFile

linesToInt :: FilePath -> IO [Int]
linesToInt =
  (Maybe.catMaybes <$>) . ints . linesToText
  where
    ints = (fmap.fmap) (\x -> TextRead.readMaybe (Text.unpack x) :: Maybe Int)

csvToInts :: FilePath -> IO [Int]
csvToInts =
  (ints <$>) . TextIO.readFile
  where
    ints text =
      Maybe.catMaybes $ (\x -> TextRead.readMaybe (Text.unpack x) :: Maybe Int)
      <$> Text.splitOn "," text
