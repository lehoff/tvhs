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
    draws = Map.findWithDefault 0.0 T.Draw dist
    homewins = Map.findWithDefault 0.0 T.HomeWin dist
    awaywins = Map.findWithDefault 0.0 T.AwayWin dist 
    hp = 3 * homewins + draws
    ap = 3 * awaywins + draws
    
   

distribution :: Outcome -> Map.Map T.Result Float
distribution outcome = d
  where
    xs = Map.toList outcome
    results = map (\(score, prob) -> (T.result score, prob)) xs
    resultMaps = map ( uncurry Map.singleton) results
    d = Map.unionsWith (+) resultMaps
    
-- mapLookup key map =
--   case Map.lookup key map of
--     Nothing ->
--       error "mapLookup failed on " ++ show key
--     Just v ->
--       v
