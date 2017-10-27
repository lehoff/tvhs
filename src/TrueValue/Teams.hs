module TrueValue.Teams
  where

import TrueValue.Transitions as T
import qualified Data.Map.Strict as Map


--- computations from the Elixir team_strength library

type TeamName = String

teams= ["AFC Bournemouth", "Arsenal FC", "Burnley FC", "Chelsea FC", "Crystal Palace",
        "Everton FC", "Hull City", "Leicester City", "Liverpool FC", "Manchester City",
        "Manchester United", "Middlesbrough FC", "Southampton FC", "Stoke City",
        "Sunderland AFC", "Swansea City", "Tottenham Hotspur", "Watford FC",
        "West Bromwich Albion", "West Ham United"]


strengthsList = 
  [("AFC Bournemouth",      ts(2.081, 0.567)),
   ("Arsenal FC",           ts(3.167, 0.372)),
   ("Burnley FC",           ts(1.291, 0.465)),
   ("Chelsea FC",           ts(3.561, 0.279)),
   ("Crystal Palace",       ts(1.834, 0.533)),
   ("Everton FC",           ts(2.426, 0.372)),
   ("Hull City",            ts(1.193, 0.677)),
   ("Leicester City",       ts(1.735, 0.533)),
   ("Liverpool FC",         ts(3.216, 0.355)),
   ("Manchester City",      ts(3.315, 0.33)),
   ("Manchester United",    ts(2.032, 0.245)),
   ("Middlesbrough FC",     ts(0.699, 0.448)),
   ("Southampton FC",       ts(1.39, 0.406)),
   ("Stoke City",           ts(1.39, 0.474)),
   ("Sunderland AFC",       ts(0.798, 0.584)),
   ("Swansea City",         ts(1.587, 0.592)),
   ("Tottenham Hotspur",    ts(3.611, 0.22)),
   ("Watford FC",           ts(1.341, 0.575)),
   ("West Bromwich Albion", ts(1.489, 0.431)),
   ("West Ham United",      ts(1.686, 0.541))]
  where
    ts (a,d) = T.TeamStrength {attack = a, defense = d}


strengths :: Map.Map String T.TeamStrength
strengths = Map.fromList strengthsList


strength :: TeamName -> T.TeamStrength
strength teamname =
  case Map.lookup teamname strengths of
    Nothing ->
      error $ "strength failed on " ++ teamname
    Just s ->
      s
  
