{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day02 where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Common
import Control.Monad.Identity (Identity)
import qualified Control.Monad as Monad
import Data.Bool

data Inst =
  Next Int (Map Int Int)
  | Halt Int (Map Int Int)
  | Error Text
  deriving (Show, Eq)

solvePart1 :: IO Inst
solvePart1 =
  runSolverM [(1, 12), (2, 2)] $ Common.csvToInts "inputs/Day02.txt"

-- solvePart2 :: IO Inst
-- solvePart2 =
  -- runSolverUntil <$> Common.csvToInts "inputs/Day02.txt"
  -- where
    -- isAnswer state = maybe False (==19690720) Map.lookup 0 state
    -- foldM (\(noun, verb) inst ->
      -- case inst of
        -- (Next i state) -> isAnswer state inst
    -- runSolverM [(1, noun), (2, verb)]
    -- ) Next 0
-- (noun, verb) | noun <- [0..99], verb <- [0..99]]

runX :: IO Inst
runX =
  runSolverUntil <$> Common.csvToInts "inputs/Day02.txt"

{-

-}
runSolverUntil :: [Int] -> Inst
runSolverUntil xs =
  foldr (\(noun, verb) inst ->
    case inst of
      (Error msg) -> Error msg
      (Halt i state) -> runNextOrStop inst state noun verb
      (Next i state) -> bool (solver [(1, noun), (2, verb)] xs) inst $ isEq inst
    ) (Next 0 (Map.fromList [(0, 0)])) [(noun, verb) | noun <- [0..99], verb <- [0..99]]
    where
      runNextOrStop inst state noun verb = bool
        (solver [(1, noun), (2, verb)] xs)
        inst
        $ maybe False (== 19690720) $ Map.lookup 0 state




isEq :: Inst -> Bool
isEq = \case
  (Halt _ state) -> f state
  (Next _ state) -> f state
  _              -> False
  where
    f state = maybe False (== 19690720) $ Map.lookup 0 state

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
