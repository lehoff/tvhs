module Main where

import TrueValue.Transitions as T
import TrueValue.Match as M
import TrueValue.Goal as Goal

main :: IO ()
main = print $ "v1: " ++ show v1 ++ " v2: " ++ show v2
  where
    v1 = Goal.value (0,0) Goal.Home 39
    v2 = Goal.value (2,1) Goal.Away 50
