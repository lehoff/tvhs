module TrueValue.League
  where

import qualified TrueValue.Teams as Teams
import qualified TrueValue.Match as M

import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.List


--- This is where we compute all matches and have them available for others to use.

type Fixture = (Teams.TeamName,Teams.TeamName)

fixtures :: [Fixture]
fixtures = [(home, away)
           | home <- Teams.teams, away <- Teams.teams, home /= away ]


playMatch :: Fixture -> M.Match
playMatch (home, away) = match
  where
    homeStrength = Teams.strength home
    awayStrength = Teams.strength away
    match = M.computeMatch homeStrength awayStrength


results :: Map.Map Fixture M.Match
results =
  Map.fromList [ (fixture, playMatch fixture) | fixture <- fixtures ]
            
