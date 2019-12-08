{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Common

data Inst =
  Next Int (Map Int Int)
  | Halt Int (Map Int Int)
  | Error Text
  deriving (Show, Eq)

solvePart1 :: IO Inst
solvePart1 =
  runInstruction . Next 0 . setup . Map.fromList . zip [0..] <$> Common.csvToInts "inputs/Day02.txt"
  where
    setup state = foldr (\(k, v) state' -> Map.insert k v state') state [(1, 12), (2, 2)]

solver :: [Int] -> Inst
solver =
  runInstruction . Next 0 . Map.fromList . zip [0..]

runInstruction :: Inst -> Inst
runInstruction (Next i m) =
  runInstruction $ runNext i m
runInstruction ins = ins

runNext ::
  Int
  -> Map Int Int
  -> Inst
runNext i state =
  maybe (Error $ Text.pack ("index " <> show i <> " doesnt exist")) f (Map.lookup i state)
  where
    f 1  = binOp i (+) state
    f 2  = binOp i (*) state
    f 99 = Halt i state
    f n  = Error $ Text.pack ("Unexpected op code found at index " <> show n)

binOp ::
  Int
  -> (Int -> Int -> Int)
  -> Map Int Int
  -> Inst
binOp start f state =
  let outcome = liftA3
       (\a b target -> Map.insert target (f a b) state)
       (resolve (start + 1) state)
       (resolve (start + 2) state)
       (Map.lookup (start + 3) state)
  in maybe
     (Error $ Text.pack ("Couldnt resolve binary op starting from index: " <> show start))
     (Next $ start + 4)
     outcome

resolve ::
  Int
  -> Map Int Int
  -> Maybe Int
resolve key state =
  Map.lookup key state >>= flip Map.lookup state
