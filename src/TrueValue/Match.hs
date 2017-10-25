module TrueValue.Match
  where


import TrueValue.Transitions

import qualified Data.Graph.Inductive as Graph
import TrueValue.Transitions as T
import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.List
import Debug.Trace


data NodeName = OutOfBounds
              | Final T.Score
              | Standing T.Standing
              deriving (Eq, Ord, Show)

data Match = Match 
  { finalScores :: [NodeName]
  , nodes :: Map.Map NodeName Int
  , done :: [NodeName]
  , graph :: Graph.Gr NodeName Float}
  deriving (Show)
 
initialMatch = Match { finalScores = finals,
                       nodes = ns,
                       done = [],
                       graph = graph'}
  where 
    validResults = [(h,a) | h <- [0..10], a <- [0..10] ]
    fulltime = zip [1..] [Standing (T.Fulltime, score) | score <- validResults]
    graph = foldl (\g node -> Graph.insNode node g) Graph.empty fulltime
    graph' = Graph.insNode (0, OutOfBounds) graph 
    ns =  (Map.fromList $ (OutOfBounds, 0) : (map swap fulltime))
    finals = map snd fulltime
    
    
computeMatch :: T.TeamStrength -> T.TeamStrength -> Match
computeMatch home away = forward
  where
    forward = computeForward home away initialMatch

computeForward :: T.TeamStrength -> T.TeamStrength -> Match -> Match
computeForward home away match = computeForwardLoop [Standing T.initialStanding] home away match 

computeForwardLoop :: [NodeName] -> T.TeamStrength -> T.TeamStrength -> Match -> Match
computeForwardLoop [] _ _ match = match
computeForwardLoop (OutOfBounds:rest) home away match =
  computeForwardLoop rest home away match
computeForwardLoop ((Final _):rest) home away match =
  computeForwardLoop rest home away match
computeForwardLoop (nodeName:rest) home away match =
  case elem nodeName $ done match of
    True ->
      computeForwardLoop rest home away match
    False ->
      let (match', newNodes) = executeForward nodeName home away match in
        computeForwardLoop (newNodes ++ rest) home away match'
        
-- only to be called if the node isn't part of done yet
executeForward :: NodeName -> T.TeamStrength -> T.TeamStrength -> Match -> (Match, [NodeName])
executeForward nodeName@(Standing standing) home away match = (match'''', succNodeNames)
  where 
    match' = insertNode nodeName match
    actions = T.next home away standing
    nextNode = restrictStanding . T.nextStanding standing 
    succNodes = map (\(action, prob) -> (nextNode action, prob)) actions
    succNodeNames = map fst succNodes
    match'' = trace (show succNodeNames) $ foldl (\m node -> insertNode node m) match' succNodeNames
    match''' = foldl (\m (node,p) -> insertEdge m nodeName node p) match'' succNodes
    match'''' = match''' { done = nodeName : done match'''}
--    match'''' = trace (show succNodeNames) $ computeForwardLoop (succNodeNames ++ rest) home away match'''     

restrictStanding:: T.Standing -> NodeName
restrictStanding (T.Fulltime, score) = Final score
restrictStanding st@(_, (h,a)) 
  | h > 10 || a > 10 = OutOfBounds
  | otherwise = Standing st

insertNode :: NodeName -> Match -> Match
insertNode OutOfBounds match = match
insertNode (Final _) match = match 
insertNode nodeName match =
  case Map.lookup nodeName $ nodes match of
    Nothing ->
      match'
      where
        index = 1 + maxIndex match
        node = (index, nodeName)
        graph' = Graph.insNode node $ graph match
        nodes' = Map.insert nodeName index $ nodes match
        match' = match { nodes = nodes',
                         graph = graph'}
    Just index ->
      match
  
-- if we use the invariant that nodes are numbered 0.. we could just use
-- Graph.noNodes here.
maxIndex match = max
  where
    (_, max) = Graph.nodeRange $ graph match

insertEdge :: Match -> NodeName -> NodeName -> Float -> Match
insertEdge match name1 name2 prob = match'
  where
    node1 = lookupNode name1 match
    node2 = lookupNode name2 match
    edge = (node1, node2, prob)
    graph' = Graph.insEdge edge $ graph match
    match' = match { graph = graph' }

lookupNode :: NodeName -> Match -> Int
lookupNode name match =
   nodes match Map.! name
