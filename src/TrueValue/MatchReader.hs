module TrueValue.MatchReader
  where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Text.Parsec 
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.String.Utils

import TrueValue.Goal as Goal

data GoalScore = GoalScore Int Int String Int 
               | OwnGoal Int Int Int 
               deriving (Show)
type Goals = Maybe [GoalScore]





--------------------------------------------------------------------------------
-- The parser
--------------------------------------------------------------------------------
goals = none 
     <|> allGoals


none = do
    string "none"
    return Nothing 
allGoals = do 
    gs <- many1 (goalLine)
    return $ Just gs

--goalLine = ownGoalLine <|> goalScoreLine               
goalLine = ownGoal
        <|> goalScoreLine

ownGoal = Text.Parsec.try (do
    goal <- goalScore
    spaces
    string "(o.g.)"
    optional endOfLine
    return $ goalScoreToOwnGoal goal)
goalScoreToOwnGoal (GoalScore home away _ m) =
    OwnGoal home away m
goalScoreLine = do
    score <- goalScore
    optional penalty
    optional endOfLine
    return score
goalScore = do
    home <- nat
    spaces
    char ':'
    spaces
    away <- nat
    spaces
    scorer <- name
    spaces 
    m <- minute
    return $ GoalScore home away scorer m
penalty = string "(pen)"
name = do 
    parts <- many1 (namePart <* optional (char ' ')) 
    return $ join " " parts
namePart = do
    first <- letter
    rest <- many (noneOf " \n")
    return $ first:rest
minute = do
    n <- nat 
    char '\''
    return n





convertGoals Nothing = []
convertGoals (Just gs) = goals
  where 
    goalTuples = convert (0,0) gs
    goals = [ Goal.mkGoal sc who when | (sc, who, when) <- goalTuples ]

convert sc ((OwnGoal h a _):rest) = convert (h,a) rest
convert sc ((GoalScore h a name m):rest) = g : convert newSc rest
  where
    newSc = (h, a)
    who = scorer sc newSc
    g = (sc, who, m)

scorer (h,_) (h',_)
  | h == h' = Goal.Away
  | otherwise = Goal.Home 

    

