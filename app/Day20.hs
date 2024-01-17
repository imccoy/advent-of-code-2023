{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Day20 (day20) where
import Part (Part (Part1, Part2))

import Control.Monad (void, forM_, forM, when)
import Data.Maybe (fromMaybe, fromJust)
import Text.Parsec
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy (State, runState, MonadState (put, get))
import qualified Control.Monad.State.Lazy as State
import Debug.Trace (trace)
import Control.Monad (unless)
import Data.Foldable (foldrM)
import Data.List (sort)
import qualified Data.GraphViz.Types.Graph as GraphViz
import qualified Data.Text.Lazy as T
import Data.GraphViz.Attributes.Complete (Attribute(Label), Label(StrLabel))
import Data.GraphViz (printDotGraph)
import qualified Data.Set as Set

type Parsed = [Module]

data ModuleType = Broadcaster | Conj | FlipFlop
  deriving (Show, Ord, Eq)
data Module = Module { moduleType :: ModuleType, moduleName :: String, moduleRecipients :: [String] }
  deriving (Show, Ord, Eq)

data States = FlipFlopState { flipFlopOn :: !Bool } | ConjunctionState { remembered :: Map.Map String Pulse } | BroadcasterState
  deriving (Show, Ord, Eq)

data Pulse = Low | High
  deriving (Show, Ord, Eq)

parseModule = do (moduleType,name) <- try (char '&' >> ((Conj,) <$> many alphaNum)) <|>
                                      try (char '%' >> ((FlipFlop,) <$> many alphaNum)) <|>
                                      (string "broadcaster" >> pure (Broadcaster, "broadcaster"))
                 string " -> "
                 recipients <- sepBy (many1 alphaNum) (string ", ")
                 newline
                 pure $ Module moduleType name recipients

parseMachine = manyTill parseModule (void newline <|> eof)

parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseMachine () "none"

toDotVizGraph gr = let nodeNumbers :: Map.Map String Int
                       nodeNumbers = Map.fromList $ zip (Map.keys gr) [0..]
                    in GraphViz.mkGraph [GraphViz.DotNode number [Label $ StrLabel . T.pack $ show (moduleType mod) ++ ": " ++ name]
                                        | (name,number) <- Map.toList nodeNumbers
                                        , let Just mod = Map.lookup name gr
                                        ]
                                        [GraphViz.DotEdge (fromJust $ Map.lookup a nodeNumbers) (fromJust $ Map.lookup b nodeNumbers) []
                                        | (a,Module _ _ recip) <- Map.toList gr, b <- recip
                                        ]


machineMap = Map.insert "rx" (Module Broadcaster "rx" []) . Map.fromList . map (\m -> (moduleName m, m))

moduleInputs modules mod = [ mod'
                           | mod' <- Map.elems modules
                           , moduleName mod `elem` moduleRecipients mod']


initialStates :: Map.Map String Module -> Map.Map String States
initialStates modules = fmap (\mod ->
                                case moduleType mod of
                                  Broadcaster -> BroadcasterState
                                  FlipFlop -> FlipFlopState False
                                  Conj -> ConjunctionState $ Map.fromList [(moduleName mod',Low)
                                                                          | mod' <- moduleInputs modules mod]
                             ) modules

incrementLowCount = do (mods,l,h,s,q) <- get
                       let l' = l+1
                       l' `seq` put (mods,l',h,s,q)

incrementHighCount = do (mods,l,h,s,q) <- get
                        let h' = h + 1
                        h' `seq` put (mods,l,h',s,q)

type SType = (Map.Map String Module,Int,Int,Map.Map String States,[(Module,Pulse)])

getModStates :: State.State SType (Map.Map String States)
getModStates = do (_,_,_,s,_) <- get
                  pure s

getModState modName = fromJust . Map.lookup modName <$> getModStates

putModStates s = do (mods,l,h,_,q) <- get
                    put (mods,l,h,s,q)

putModState modName s = do states <- getModStates
                           let s' = Map.insert modName s states
                           s' `seq` putModStates s'

mapModStateM modName f = do state <- getModState modName
                            s' <- f state
                            putModState modName s'

getMods = do (mods,_,_,_,_) <- get
             pure mods

getMod modName = Map.lookup modName <$> getMods

getQueue = do (_,_,_,_,q) <- get
              pure q

putQueue q = do (mods,l,h,s,_) <- get
                put (mods,l,h,s,q)

sendPulse :: String -> String -> Pulse -> State.State SType ()
sendPulse fromModName toModName pulse =
  do maybeMod <- getMod toModName
     case maybeMod of
       Nothing -> pure ()
       Just mod ->
         case moduleType mod of
           Broadcaster -> enqueue mod pulse
           FlipFlop -> if pulse == High
                         then pure ()
                         else mapModStateM toModName $ \s -> do
                                enqueue mod (if flipFlopOn s then Low else High)
                                let s' = FlipFlopState (not $ flipFlopOn s)
                                s' `seq` pure s'
           Conj -> mapModStateM toModName $ \(ConjunctionState r) -> do
                     let r' = Map.insert fromModName pulse r
                     enqueue mod (if all (==High) . Map.elems $ r' then Low else High)
                     r' `seq` pure (ConjunctionState r')

runQueue :: State.State SType (Bool, [(Module,Pulse)])
runQueue = do q <- getQueue
              putQueue []
              forM (reverse q) $ uncurry sendToRecipients
              pure (null q, [(m, p) | (m,p) <- reverse q])

enqueue :: Module -> Pulse -> State.State SType ()
enqueue mod pulse = do q <- getQueue
                       putQueue $ (mod,pulse):q

sendToRecipients :: Module -> Pulse -> State.State SType ()
sendToRecipients mod pulse = go (moduleRecipients mod)
  where go [] = pure ()
        go (recipient:rs) = do if pulse == Low then incrementLowCount else incrementHighCount
                               sendPulse (moduleName mod) recipient pulse
                               go rs

runLoop :: State.State SType [(Module,Pulse)]
runLoop = do (qEmpty, pulses) <- runQueue
             if qEmpty
               then pure pulses
               else do next <- runLoop
                       pure $ pulses ++ next

hitButton :: State.State SType (Map.Map String States, [(Module,Pulse)])
hitButton = do incrementLowCount
               sendPulse "" "broadcaster" Low
               pulses <- runLoop
               stateAfter <- getModStates
               pure (stateAfter,pulses)

hitButtonMany n = do (_,sigs) <- hitButton
                     if any (\(mod,pulse) -> pulse == Low && moduleName mod == "rx") sigs
                       then pure n
                     else
                       do let n' = n + 1
                          n' `seq` hitButtonMany n'

hitButtonManyMany = do (sts,pulses) <- hitButton
                       next <- hitButtonManyMany
                       pure $ (sts,pulses):next


part1 :: Parsed -> IO ()
part1 mods0 = do let mods = machineMap mods0
                 print mods0
                 let ((),(_,low,high,_,_)) = runState (forM_ [1..1000] $ \n -> do
                                                         void hitButton
                                                      ) (mods,0,0,initialStates mods,[])
                 print (low,high,low*high)


assignSections :: Map.Map String Module -> Map.Map String [Int]
assignSections gr = let initials = zip (moduleRecipients $ fromJust $ Map.lookup "broadcaster" gr) (pure <$> [1..])
                     in go (Set.fromList $ map fst initials) (Map.fromList initials)
  where go :: Set.Set String -> Map.Map String [Int] -> Map.Map String [Int]
        go q sections = case Set.minView q of
                          Nothing -> sections
                          Just (name, q') -> let hereSections = fromJust $ Map.lookup name sections
                                                 nexts = [(next, needed) |
                                                            next <- moduleRecipients $ fromJust $ Map.lookup name gr,
                                                            let nextSections = fromMaybe [] $ Map.lookup next sections,
                                                            let needed = filter (`notElem` nextSections) hereSections,
                                                            not $ null needed]
                                              in go (Set.union (Set.fromList $ fst <$> nexts) q')
                                                    (Map.unionWith (++) sections (Map.fromList nexts))

sectionGraphs :: Map.Map String Module -> Map.Map String [Int] -> [Map.Map String Module]
sectionGraphs gr sections = [sectionGraph n | n <- allSectionNumbers]
  where allSectionNumbers = Set.toList . Set.fromList . concat . Map.elems $ sections
        sectionGraph n = let gr' = Map.filterWithKey (\k _ -> k == "broadcaster" || Map.lookup k sections == Just [n]) gr
                          in Map.map (\mod -> mod { moduleRecipients = filter (`Map.member` gr') (moduleRecipients mod) }) gr'

part2 :: Parsed -> IO ()
part2 mods0 = do let mods = machineMap mods0
                 writeFile "20.dot" (T.unpack $ printDotGraph (toDotVizGraph mods))
                 -- it turns out the machine has this structure where there's a bunch of
                 -- sections, each of which flows into an penultimate node that then goes into rx
                 let sections = assignSections mods
                 forM_ [1..10] $ \section ->
                   forM_ (Map.toList sections) $ \(name,sectionNumbers) ->
                     when (section `elem` sectionNumbers) $ print (name,sectionNumbers)

                 let rxInputs = moduleInputs mods $ fromJust $ Map.lookup "rx" mods
                 let rx1Inputs = concatMap (moduleInputs mods) rxInputs
                 print rx1Inputs
                 ts <- forM (sectionGraphs mods sections) $ \sectionMods -> do
                         let (sts,_) = runState hitButtonManyMany (sectionMods,0,0,initialStates sectionMods,[])
                         let (n,n',_,pulses) = findStateLoop sts
                         print (n, n', [p | rx1Input <- rx1Inputs, p <- fromMaybe [] $ Map.lookup (moduleName rx1Input) pulses])
                         pure n
                 -- it turns out that the penultimate nodes that go into rx all loop within about
                 -- 4000 iterations, and they'll send a high signal immediately before their state
                 -- returns to the initial state, and they'll send a low signal on every other button
                 -- press. That means we can do this:

                 print $ foldr (lcm . (+ (-1))) 1 ts


findStateLoop :: [(Map.Map String States, [(Module, Pulse)])] -> (Int,Int,Map.Map (Map.Map String States) Int, Map.Map String [(Int,Pulse)])
findStateLoop = go Map.empty Map.empty 1
  where go sts pss n ((st,ps):stspss)
          = case Map.lookup st sts of
              Just n' -> (n, n', sts, pss)
              Nothing -> go (Map.insert st n sts)
                            (Map.unionWith (++)
                                           (Map.unionsWith
                                              (++)
                                              [Map.singleton (moduleName m) [(n,p)]
                                              |(m,p) <- ps
                                              ])
                                           pss)
                            (n+1)
                            stspss


day20 part args = do let filename = case args of
                                      [] -> "inputs/day20"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
