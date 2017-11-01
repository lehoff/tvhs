{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}


module TrueValue.Transitions
where

import Data.Binary
import GHC.Generics




data WhistleTime = WHalftime | WFulltime deriving (Show)

data Action = Whistle WhistleTime 
            | Both
            | Home
            | Away
            | None
            deriving (Show)

data Time = FirstHalf Int
          | Halftime
          | SecondHalf Int
          | Fulltime
          deriving (Eq, Ord, Show, Generic)
instance Binary Time

-- type Score = (Int, Int)

data Score = NilNil
                | OneNil
                | NilOne
                | Even
                | Plus Int
                | Minus Int
                deriving (Eq, Ord, Show, Generic)
instance Binary Score



data TeamStrength =  TeamStrength { attack :: Float,
                                    defense :: Float
                                  } deriving (Show)

type Standing = (Time, Score) 

data WhistleProbability = NoWhistle
                        | Perhaps Float WhistleTime
                        | Certain WhistleTime
                        deriving (Show)

initialStanding :: Standing
initialStanding = (FirstHalf(0), NilNil)

pointsHome :: Score -> Int
pointsHome NilNil = 1
pointsHome Even = 1
pointsHome OneNil = 3
pointsHome (Plus _) = 3
pointsHome _ = 0

pointsAway :: Score -> Int
pointsAway NilNil = 1
pointsAway Even = 1
pointsAway NilOne = 3
pointsAway (Minus _) = 3
pointsAway _ = 0

data Result = HomeWin
            | Draw
            | AwayWin
            deriving (Eq, Ord, Show)

result :: Score -> Result
result OneNil = HomeWin
result (Plus _) = HomeWin
result NilOne = AwayWin
result (Minus _) = AwayWin
result _ = Draw 

liverpool :: TeamStrength
liverpool = TeamStrength {attack = 3.216, defense = 0.355}

leicester :: TeamStrength
leicester = TeamStrength {attack = 1.735, defense = 0.533}

nextStanding :: Standing -> Action -> Standing
nextStanding (_, score) (Whistle WHalftime) = (Halftime, score)
nextStanding (_, score) (Whistle WFulltime) = (Fulltime, score)
nextStanding (time, score) goal_action =
  (nextMinute time, updateScore score goal_action)

nextMinute :: Time -> Time
nextMinute (FirstHalf m) = FirstHalf $ m + 1
nextMinute (SecondHalf m) = SecondHalf $ m + 1
nextMinute (Halftime)  = SecondHalf 45


updateScore :: Score -> Action -> Score
updateScore score None = score

updateScore NilNil Both = Even
updateScore NilNil Home = OneNil
updateScore NilNil Away = NilOne

updateScore OneNil Both = Plus 1
updateScore OneNil Home = Plus 2
updateScore OneNil Away = Even

updateScore NilOne Both = Minus 1
updateScore NilOne Home = Even
updateScore NilOne Away = Minus 2

updateScore Even Both = Even
updateScore Even Home = Plus 1
updateScore Even Away = Minus 1

updateScore sc@(Plus _) Both = sc
updateScore (Plus 1) Away = Even
updateScore (Plus n) Away = Plus $ n-1
updateScore (Plus n) Home = Plus $ n+1

updateScore sc@(Minus _) Both = sc
updateScore (Minus 1) Home = Even
updateScore (Minus n) Home = Minus $ n-1
updateScore (Minus n) Away = Minus $ n+1

updateScore score action = error ("wrong updateScore args: " ++ (show score) ++ " " ++ (show action))

next :: TeamStrength -> TeamStrength -> Standing -> [(Action, Float)]
next home away (Halftime, score) = next home away (nextMinute Halftime, score) 
next home away standing =
  adjustGoalTransitionsWithWhistle whistleProb gt
  where
    whistleProb = whistleProbability standing
    gt = goalTransistions home away standing

adjustGoalTransitionsWithWhistle :: WhistleProbability -> [(Action, Float)] -> [(Action, Float)]
adjustGoalTransitionsWithWhistle NoWhistle  gt = gt
adjustGoalTransitionsWithWhistle (Certain half) gt = [(Whistle half, 1.0)]
adjustGoalTransitionsWithWhistle (Perhaps prob half) gt =
  (Whistle half, prob) : (multiply gt (1 - prob))

multiply gt p =
  map (\(action, p0) -> (action, p*p0)) gt
  
   
  
  


whistleProbability :: Standing -> WhistleProbability
whistleProbability ((FirstHalf m), _) =
  whistleProbabilityInternal extras (m - 45) WHalftime
  where
    extras = map (\m -> m/100) [1, 37, 37, 17, 5, 3]
whistleProbability ((SecondHalf m), _) =
  whistleProbabilityInternal extras (m - 90) WFulltime
  where
    extras = map (\m -> m/100) [1, 1, 3, 25, 37, 22, 7, 3, 1]
whistleProbability st = error $ "Calling whistleProbablility with wrong standing" ++ (show st)

whistleProbabilityInternal :: [Float] -> Int -> WhistleTime -> WhistleProbability
whistleProbabilityInternal extras stoppage half
  | stoppage < 0 = NoWhistle
  | (stoppage+1) >= length(extras) = Certain half
  | otherwise = Perhaps prop half
  where
    prop = f / s
    f : fs = reverse (take (stoppage+1) extras)
    s' = (1 - sum fs)
    s = fromInteger (round $ s' * 100) / 100

goalTransistions :: TeamStrength -> TeamStrength -> Standing -> [(Action, Float)]
goalTransistions home away (time, score) =
  [(Both, probHome * probAway),
   (Home, probHome * (1 - probAway)),
   (Away, probAway * (1 - probHome)),
   (None, (1 - probHome) * (1 - probAway))]
  where
    m = minute time
    probHome = lambda lambda0 score m
    probAway = mu mu0 score m
    (lambda0, mu0) = baseLambdaAndMu home away

baseLambdaAndMu :: TeamStrength -> TeamStrength -> (Float, Float)
baseLambdaAndMu home away =
  ((attack home * defense away) / 90, (attack away * defense home) / 90)
  

minute :: Time -> Int
minute (FirstHalf m) = m
minute (SecondHalf m) = m

lambda :: Float -> Score -> Int -> Float
lambda base score m =
  base * (lambda_xy score) * (xi_1 m)
  where
    lambda_xy OneNil = 0.86
    lambda_xy NilOne = 1.10
    lambda_xy (Plus _) = 1.01
    lambda_xy (Minus _) = 1.13
    lambda_xy _ = 1.0
    xi_1 :: Int -> Float  
    xi_1 m = (xi1 + fromIntegral m)/90.0
    xi1 = 0.67



mu :: Float -> Score -> Int -> Float
mu base score m =
  base * (mu_xy score) * (xi_2 m)
  where
    mu_xy OneNil = 1.33
    mu_xy NilOne = 1.07
    mu_xy (Plus _) = 1.53
    mu_xy (Minus _) = 1.16
    mu_xy _ = 1.0
    xi_2 m = (xi2 + fromIntegral m)/90
    xi2 = 0.47

 
