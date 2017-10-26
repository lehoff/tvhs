module TrueValue.Match
  where


import qualified Data.Graph.Inductive as Graph
import TrueValue.Transitions as T
import TrueValue.Outcome as Outcome
import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.List
import Debug.Trace


data NodeName = OutOfBounds
              | Final T.Score
              | Standing T.Standing
              deriving (Eq, Ord, Show)

type Label = (NodeName, Outcome.Outcome)

data Match = Match 
  { finalScores :: [NodeName]
  , nodes :: Map.Map NodeName Int
  , done :: [NodeName]
  , graph :: Graph.Gr Label Float}
  deriving (Show)

type Context = Graph.Context Label Float

initialMatch = Match { finalScores = finals,
                       nodes = ns,
                       done = [],
                       graph = graph'}
  where 
    validResults = [(h,a) | h <- [0..10], a <- [0..10] ]
    fulltime = zip [1..] [finalNode score | score <- validResults]
    graph = foldl (\g node -> Graph.insNode node g) Graph.empty fulltime
    graph' = Graph.insNode (0, (OutOfBounds, Outcome.empty)) graph 
    ns =  Map.fromList $ ((OutOfBounds,0) : (map (\(i, (n,_)) -> (n,i)) fulltime))
    finals = map (\(_,(n,_)) -> n) fulltime
    
finalNode :: T.Score -> (NodeName, Outcome.Outcome)
finalNode score =
  (Final score, Outcome.final score)

    
computeMatch :: T.TeamStrength -> T.TeamStrength -> Match
computeMatch home away = final
  where
    forward = computeForward home away initialMatch
    final = computeBackward forward 

--------------------------------------------------------------------------------
-- helper functions to express things in terms of NodeNames
--------------------------------------------------------------------------------
getNodeContext :: Match -> NodeName -> Context
getNodeContext match nodename =
  Graph.context (graph match) (lookupNode nodename match)

updateNode:: Context -> Match -> Match
updateNode context match = match'
  where
    g = graph match 
    g0 = Graph.delNode (Graph.node' context) g
    g' = context Graph.& g0
    match' = match { graph = g' }

preNodes :: Match -> NodeName -> [NodeName]
preNodes match nodename = pre 
  where
    context = getNodeContext match nodename
    pre = map (nodenameForIndex match) $ Graph.pre' context

sucNodes :: Match -> NodeName -> [NodeName]
sucNodes match nodename = suc
  where
    context = getNodeContext match nodename
    suc = map (nodenameForIndex match) $ Graph.suc' context

isDone :: Match -> NodeName -> Bool
isDone _ OutOfBounds = True
isDone _ (Final _)  = True
isDone match nodeName =
  elem nodeName $ done match
  
getNodeOutcome :: Match -> NodeName -> Outcome.Outcome
getNodeOutcome match nodename = outcome
  where
    context = getNodeContext match nodename
    outcome = outcomeFromLabel $ Graph.lab' context

--------------------------------------------------------------------------------
-- raw compute functions
--------------------------------------------------------------------------------

computeForward :: T.TeamStrength -> T.TeamStrength -> Match -> Match
computeForward home away match = computeForwardLoop [Standing T.initialStanding] home away match 

computeBackward :: Match -> Match
computeBackward match = match'
  where
    preFinals = concatMap (preNodes match)  $ finalScores match
    preOutOfBounds =  preNodes match OutOfBounds 
    match' = computeBackwardLoop (preFinals ++ preOutOfBounds) match { done = [] }

computeBackwardLoop :: [NodeName] -> Match -> Match
computeBackwardLoop [] match = match
computeBackwardLoop (nodename:nodenames) match = 
  -- trace (show nodename) $
  case all (isDone match) $ sucNodes match nodename of
    True ->
      computeBackwardLoop nodenames $ calculateNodeOutcome nodename match
    False ->
      computeBackwardLoop (nodenames ++ [nodename]) match 
    
calculateNodeOutcome :: NodeName -> Match -> Match
calculateNodeOutcome nodename match = match' { done = nodename : (done match') }
  where
    context = getNodeContext match nodename
    (preAdj, node, label, sucAdj) = context
    g = graph match
    outcomesAndProbs = map (\(prob, node) -> (outcomeForIndex node g, prob)) sucAdj
    scaledOutcomes = (map (\(out, prob) -> Outcome.multiply out prob) outcomesAndProbs)
    outcome = Outcome.add scaledOutcomes
    newContext = (preAdj, node, (nodename, outcome), sucAdj)
    match' = -- trace (show newContext) $
      updateNode newContext match 
                           
    
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
    match'' =  foldl (\m node -> insertNode node m) match' succNodeNames
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
        node = (index, (nodeName, Outcome.empty))
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

nodenameForIndex :: Match -> Int -> NodeName
nodenameForIndex match index = name
  where
    context = Graph.context (graph match) index
    name = nodenameFromLabel $ Graph.lab' context


nodenameFromLabel :: Label -> NodeName
nodenameFromLabel = fst 

outcomeFromLabel :: Label -> Outcome.Outcome
outcomeFromLabel = snd


outcomeForIndex :: Int -> Graph.Gr Label Float -> Outcome.Outcome
outcomeForIndex i g =
  case Graph.lab g i of
    Nothing ->
      error ("should have had an outcome for node" ++ (show i))
    Just label ->
      outcomeFromLabel label
