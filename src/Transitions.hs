module Transitions
where

data Action = HalftimeWhistle
            | FulltimeWhistle
            | Both
            | Home
            | Away
            | None
            deriving (Show)

data Time = FirstHalf Int
          | SecondHalf Int
          | Halftime
          | Fulltime
                      deriving (Show)

type Score = (Int, Int)

data TeamStrength =  TeamStrength { attack :: Float,
                                    defense :: Float
                                  } deriving (Show)

type Standing = (Time, Score)

initialStanding :: Standing
initialStanding = (FirstHalf(0), (0,0))

liverpool :: TeamStrength
liverpool = TeamStrength {attack = 3.216, defense = 0.355}

leicester :: TeamStrength
leicester = TeamStrength {attack = 1.735, defense = 0.533}

nextStanding :: Standing -> Action -> Standing
nextStanding (_, score) HalftimeWhistle = (Halftime, score)

nextStanding (_, score) FulltimeWhistle = (Fulltime, score)

nextStanding (time, score) goal_action =
  (nextMinute time, updateScore score goal_action)

nextMinute :: Time -> Time
nextMinute (FirstHalf m) = FirstHalf $ m + 1
nextMinute (SecondHalf m) = SecondHalf $ m + 1
nextMinute Halftime = SecondHalf 45

updateScore :: Score -> Action -> Score
updateScore score None = score
updateScore (h, a) Both = (h+1, a+1)
updateScore (h, a) Home = (h+1, a)
updateScore (h, a) Away = (h, a+1)

next :: TeamStrength -> TeamStrength -> Standing -> [(Action, Float)]
next home away standing =
  adjustGoalTransitionsWithWhistle whistleProb half gt
  where
    (whistleProb, half) = whistleProbability standing
    gt = goalTransistions home away standing

adjustGoalTransitionsWithWhistle prob half gt = gt


whistleProbability :: Standing -> (Float, Time)
whistleProbability ((FirstHalf m), _) =
  whistleProbabilityInternal extras (m - 45) Halftime
  where
    extras = map (\m -> m/100) [1, 37, 37, 17, 5, 3]
whistleProbability ((SecondHalf m), _) =
  whistleProbabilityInternal extras (m - 45) Fulltime
  where
    extras = map (\m -> m/100) [1, 1, 3, 25, 37, 22, 7, 3, 1]

whistleProbabilityInternal :: [Float] -> Int -> Time -> (Float, Time)
whistleProbabilityInternal extras stoppage half =
  (f / s, half)
  where
    f : fs = reverse (take (stoppage+1) extras)
    s' = (1 - sum fs)
    s = fromInteger (round $ s * 100) / 100

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
    lambda_xy (1, 0) = 0.86
    lambda_xy (0, 1) = 1.10
    lambda_xy (x, y)
      | x == y = 1.0
      | x > y && x > 1 = 1.53
      | otherwise = 1.13
    xi_1 :: Int -> Float  
    xi_1 m = (xi1 + fromIntegral m)/90.0
    xi1 = 0.67

mu :: Float -> Score -> Int -> Float
mu base score m =
  base * (mu_xy score) * (xi_2 m)
  where
    mu_xy (1,0) = 1.33
    mu_xy (0,1) = 1.07
    mu_xy (x,y)
      | x == y = 1.00
      | x > y && x > 1 = 1.53
      | otherwise = 1.16
    xi_2 m = (xi2 + fromIntegral m)/90
    xi2 = 0.47

 
