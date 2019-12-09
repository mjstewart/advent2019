{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day02 where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Common
import Control.Monad.Identity
import Data.Bool

data Inst =
  Next Int (Map Int Int)
  | Halt Int (Map Int Int)
  | Error Text
  deriving (Show, Eq)

solvePart1 :: IO Inst
solvePart1 =
  runSolverM [(1, 12), (2, 2)] $ Common.csvToInts "inputs/Day02.txt"

solvePart2 :: IO Inst
solvePart2 =
  runPart2Solver <$> Common.csvToInts "inputs/Day02.txt"

solvePart2ByMapping :: IO [Inst]
solvePart2ByMapping = runPart2SolverMapping <$> Common.csvToInts "inputs/Day02.txt"

-- A different strategy - use map + filter
-- This is slower though since foldr short circuits when the result doesnt depend on the
-- accumulator as per https://wiki.haskell.org/Foldr_Foldl_Foldl'
runPart2SolverMapping :: [Int] -> [Inst]
runPart2SolverMapping xs =
  filter f $ map (\(noun, verb) -> solver [(1, noun), (2, verb)] xs)
    [(noun, verb) | noun <- [0..99], verb <- [0..99]]
  where
    f (Halt _ state) = maybe False (==19690720) $ Map.lookup 0 state
    f _ = False

runPart2Solver :: [Int] -> Inst
runPart2Solver xs =
  foldr
    (\(noun, verb) inst ->
      case inst of
        (Halt _ state) -> check inst state noun verb
        _              -> inst
    ) (Halt 0 (Map.fromList [(0, 0)]))
    [(noun, verb) | noun <- [0..99], verb <- [0..99]]
    where
      check inst state noun verb =
        bool (solver [(1, noun), (2, verb)] xs) inst
        $ maybe False (== 19690720) $ Map.lookup 0 state

{-
By passing in a Monad, instead of using IO, we can pass in
the Identity Monad to test the behaviour without worrying about
doing any IO but still testing the implementation.

runSolverM [] $ Identity [1,9,10,3,2,3,11,0,99,30,40,50]
-}
runSolverM :: Monad m =>
  [(Int, Int)]
  -> m [Int]
  -> m Inst
runSolverM replacements = (solver replacements <$>)

solver ::
  [(Int, Int)]
  -> [Int]
  -> Inst
solver replacements =
  runInstruction . Next 0 . setup . Map.fromList . zip [0..]
  where
    setup state = foldr (\(k, v) state' -> Map.insert k v state') state replacements

runInstruction :: Inst -> Inst
runInstruction (Next i state) =
  runInstruction $ runNext i state
runInstruction inst = inst

runNext ::
  Int
  -> Map Int Int
  -> Inst
runNext i state =
  maybe (Error $ Text.pack ("index " <> show i <> " doesnt exist")) f (Map.lookup i state)
  where
    f 1  = binaryOp i (+) state
    f 2  = binaryOp i (*) state
    f 99 = Halt i state
    f n  = Error $ Text.pack ("Unexpected op code found at index " <> show n)

binaryOp ::
  Int
  -> (Int -> Int -> Int)
  -> Map Int Int
  -> Inst
binaryOp start f state =
  maybe
    (Error $ Text.pack ("Couldnt resolve binary op starting from index: " <> show start))
    (Next $ start + 4)
    $ liftA3
       (\a b target -> Map.insert target (f a b) state)
       (resolve (start + 1) state)
       (resolve (start + 2) state)
       (Map.lookup (start + 3) state)

resolve ::
  Int
  -> Map Int Int
  -> Maybe Int
resolve key state =
  Map.lookup key state >>= flip Map.lookup state
