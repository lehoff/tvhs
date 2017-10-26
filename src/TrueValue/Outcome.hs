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


