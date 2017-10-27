module TrueValue.Goal
where

import qualified TrueValue.Transitions as T
import qualified TrueValue.Match as M
import qualified TrueValue.Outcome as Outcome
import qualified TrueValue.League as League

import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.List
import Debug.Trace

type ScoreTuple = (Int, Int)

data Scorer = Home
            | Away
            deriving (Show, Eq)

data Goal = Goal T.Score Scorer T.Time
  deriving (Show, Eq)


home :: T.Score -> T.Time -> Goal
home score time = Goal score Home time

away :: T.Score -> T.Time -> Goal
away score time = Goal score Away time

value :: ScoreTuple -> Scorer -> Int -> Float
value currentScore scorer t =
  value' $ mkGoal currentScore scorer t

mkGoal :: ScoreTuple -> Scorer -> Int -> Goal
mkGoal scoreTuple scorer t =
  Goal score scorer time
  where
    score = tupleToScore scoreTuple
    time  = intToTime t 

value' :: Goal -> Float
value' goal = v
  where
    foldFun match acc = acc + valueMatch' match goal
    v = (Map.foldr foldFun 0.0 League.results) / fromIntegral (Map.size League.results)


values :: ScoreTuple -> Scorer -> [Int] -> [(Int, Float)]
values currentScore scorer minutes =
  [ (minute, value currentScore scorer minute)
  | minute <- minutes ]

values' :: (T.Time -> Goal) -> [T.Time] -> [(T.Time, Float)]
values' goalMaker times = vs
  where
    f time = value' $ goalMaker time
    vals = map f times
    vs = zip times vals
  

valueMatch :: M.Match -> ScoreTuple -> Scorer -> Int -> Float
valueMatch match scoreTuple scorer t =
  valueMatch' match $ mkGoal scoreTuple scorer t
    

valueMatch' :: M.Match -> Goal -> Float
valueMatch' match (Goal curSc scorer time) = v
  where
    newSc = nextScore curSc scorer
    curNode = M.Standing (time, curSc)
    newNode = M.Standing (time, newSc)
    oldPoints = Outcome.points $ M.getNodeOutcome match curNode
    newPoints = Outcome.points $ M.getNodeOutcome match newNode
    v = points oldPoints newPoints scorer

valuesMatch :: M.Match -> ScoreTuple -> Scorer -> [Int] -> [(Int, Float)]
valuesMatch match scoreTuple scorer ts = vals'
  where
    score = tupleToScore scoreTuple
    times = map intToTime ts
    goalMaker = Goal score scorer 
    vals =  valuesMatch' match goalMaker times 
    vals' = map (\(t, v) -> (timeToInt t, v)) vals

valuesMatch' :: M.Match -> (T.Time -> Goal) -> [T.Time] -> [(T.Time, Float)]
valuesMatch' match goalMaker minutes = vs 
  where
    f time = valueMatch' match (goalMaker time)  
    vals = map f minutes
    vs = zip minutes vals

points (old, _) (new, _) Home = new - old
points (_, old) (_, new) Away = new - old


tupleToScore :: ScoreTuple -> T.Score
tupleToScore (0,0) = T.NilNil
tupleToScore (1,0) = T.OneNil
tupleToScore (0,1) = T.NilOne
tupleToScore (n,m)
  | n == m = T.Even
  | n > m  && ((n-m) <= M.maxDelta) = T.Plus $ n-m
  | n < m  && ((m-n) <= M.maxDelta) = T.Minus $ m-n
  | otherwise = error "too big a difference in goals scored"
  
nextScore :: T.Score -> Scorer -> T.Score
nextScore score Home = T.updateScore score T.Home
nextScore score Away = T.updateScore score T.Away

intToTime :: Int -> T.Time
intToTime n
  | n < 46 = T.FirstHalf n
  | otherwise = T.SecondHalf n

timeToInt :: T.Time -> Int
timeToInt (T.FirstHalf n) = min n 45
timeToInt (T.SecondHalf n) = min n 90


minutesStep :: Int -> [Int]
minutesStep 1 = [1..97]
minutesStep n = ns
  where
    base = [n, 2*n .. 96]
    ns = (1:base) ++ [97]
  
