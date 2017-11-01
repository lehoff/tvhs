{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module TrueValue.League
  where

import qualified TrueValue.Teams as Teams
import qualified TrueValue.Match as M

import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.List

import Data.Binary
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BS

--- This is where we compute all matches and have them available for others to use.

type Fixture = (Teams.TeamName,Teams.TeamName)
type Results =  Map.Map Fixture M.Match


fixtures :: [Fixture]
fixtures = [(home, away)
           | home <- Teams.teams, away <- Teams.teams, home /= away ]


playMatch :: Fixture -> M.Match
playMatch (home, away) = match
  where
    homeStrength = Teams.strength home
    awayStrength = Teams.strength away
    match = M.computeMatch homeStrength awayStrength


results :: Results
results =
  Map.fromList [ (fixture, playMatch fixture) | fixture <- fixtures ]
            
-- instance Binary (Map.Map Fixture M.Match)


saveResults filename = BS.writeFile filename (encode results)

loadResults filename =
  BS.readFile filename >>= return . decode :: IO Results
  
  
