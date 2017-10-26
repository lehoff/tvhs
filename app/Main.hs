module Main where

import TrueValue.Transitions as T
import TrueValue.Match as M

main :: IO ()
main = print $ M.getNodeContext match lastMinute -- (length $ nodes match)
  where
    match = M.computeMatch T.liverpool T.leicester
    lastMinute = M.Standing (T.SecondHalf 45,T.NilNil)
