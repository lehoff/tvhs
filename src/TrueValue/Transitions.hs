module TrueValue.Transitions
where

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
          deriving (Eq, Ord, Show)

type Score = (Int, Int)

data TeamStrength =  TeamStrength { attack :: Float,
                                    defense :: Float
                                  } deriving (Show)

type Standing = (Time, Score) 

data WhistleProbability = NoWhistle
                        | Perhaps Float WhistleTime
                        | Certain WhistleTime
                        deriving (Show)

initialStanding :: Standing
initialStanding = (FirstHalf(0), (0,0))

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
updateScore (h, a) Both = (h+1, a+1)
updateScore (h, a) Home = (h+1, a)
updateScore (h, a) Away = (h, a+1)

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

 
