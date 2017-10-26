module TrueValue.Outcome
  where

import TrueValue.Transitions as T
import qualified Data.Map.Strict as Map

type Outcome = Map.Map T.Score Float

outcome :: T.Score -> Float -> Outcome
outcome score probability = Map.singleton score probability

final :: T.Score -> Outcome
final score =
  outcome score 1.0

empty :: Outcome
empty = Map.empty

add :: [Outcome] -> Outcome
add outcomes =
  Map.unionsWith (+) outcomes

multiply :: Outcome -> Float -> Outcome
multiply outcome x =
  Map.map (* x) outcome

points :: Outcome -> (Float, Float)
points outcome = (hp, ap)
  where
    dist = distribution outcome
    draws = dist Map.! T.Draw
    homewins = dist Map.! T.HomeWin
    awaywins = dist Map.! T.AwayWin
    hp = 3 * homewins + draws
    ap = 3 * awaywins + draws
    
   

distribution :: Outcome -> Map.Map T.Result Float
distribution outcome = d
  where
    xs = Map.toList outcome
    results = map (\(score, prob) -> (T.result score, prob)) xs
    resultMaps = map ( uncurry Map.singleton) results
    d = Map.unionsWith (+) resultMaps
    
