module Main where

import TrueValue.Transitions as T
import TrueValue.Match as M
main :: IO ()
main = print match -- (length $ nodes match)
  where
    match = M.computeMatch T.liverpool T.leicester
    
