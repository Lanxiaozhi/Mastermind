module Guess (
  GameState, 
  initialize, 
  guess,
  refine,
  meval,
  judge,
  prune,
  mvalid,
  distinct,
  select
) where

import System.Random
import Data.List
import Data.Ord
import Data.Monoid
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Text.Printf

-- your GameState, default to (), can be modified
-- type GameState = ()
data GameState = State {guessNum :: Int, guessEval :: (Int, Int), currentRange :: [Int]} deriving (Show)

-- your initializer for GateState, can be modified
initialize :: (Int -> Bool) -> GameState
initialize _ = State { guessNum = 123, guessEval = (0,0), currentRange = distinct [0..9999]}

-- your guess function, implement it
-- guess g s = (fst $ randomR (0000, 9999) g, s)
guess ::  RandomGen g => g -> GameState -> (Int, GameState)
guess g s = (y, s) where
  y = select $ currentRange s

-- notify the result of the last guess and refine the game state
refine :: (Int, GameState) -> (Int, Int) -> GameState
refine (y, s) (a, b) = s' where
  newRange = prune y (a, b) (currentRange s)
  s' = State { guessNum = y, guessEval = (a,b), currentRange = newRange}

meval :: Int -> Int -> (Int, Int)
meval e x = (appeared, inPosition) where
  show04 :: Int -> String
  show04 = printf "%04d"
  de = show04 e
  dx = show04 x
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)

judge :: Int -> (Int, Int) -> Int -> [Int]
judge s (a,b) x = if meval s x == (a, b) then [x] else []

prune :: Int -> (Int, Int) -> [Int] -> [Int]
prune s (a, b) [x] = judge s (a,b) x
prune s (a, b) (x:xs) = judge s (a, b) x ++ (prune s (a,b) xs)

mvalid :: Int -> Bool
mvalid n = 100 <= n && n <= 9999 && noDup n where
  show04 :: Int -> String
  show04 = printf "%04d"
  noDup x = length (Data.List.nub (show04 x)) == length (show04 x)

distinct :: [Int] -> [Int]
distinct [x] = if mvalid x then [x] else []
distinct (x:xs) = if mvalid x then x : (distinct xs) else distinct xs

select :: [Int] -> Int
select = head