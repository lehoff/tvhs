module TrueValue.Match
  where


import TrueValue.Transitions

import qualified Data.Graph.Inductive as Graph
import TrueValue.Transitions as T
import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.List

data NodeName = OutOfBounds
              | Final T.Score
              | Standing T.Standing
              deriving (Eq, Ord, Show)

data Match = Match 
  { finalScores :: [NodeName]
  , nodes :: Map.Map NodeName Int
  , graph :: Graph.Gr NodeName Float}
  deriving (Show)
 
initialMatch = Match { finalScores = finals,
                       nodes = ns,
                       graph = graph'}
  where 
    validResults = [(h,a) | h <- [0..1], a <- [0..1] ]
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
computeForward home away match = computeForwardLoop (Standing T.initialStanding) home away match 

computeForwardLoop :: NodeName -> T.TeamStrength -> T.TeamStrength -> Match -> Match
computeForwardLoop OutOfBounds _ _ match = match
computeForwardLoop nodeName@(Standing standing) home away match = match'''
  where 
    match' = insertNode nodeName match
    actions = T.next home away standing
    nextNode = restrictStanding . T.nextStanding standing 
    succNodes = map (\(action, prob) -> (nextNode action, prob)) actions
    -- now we just recurse to ensure that all successors are in the match
    match'' = foldl (\m (s,_) -> computeForwardLoop s home away m) match' succNodes
    -- and then we add edges to the successors, which now know are in the graph
    match''' = foldl (\m (node,p) -> insertEdge m nodeName node p) match'' succNodes
    

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
