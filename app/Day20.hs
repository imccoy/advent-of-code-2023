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

hitButtonManyMany = do (_,pulses) <- hitButton
                       next <- hitButtonManyMany
                       pure $ pulses:next


part1 :: Parsed -> IO ()
part1 mods0 = do let mods = machineMap mods0
                 print mods0
                 let ((),(_,low,high,_,_)) = runState (forM_ [1..1000] $ \n -> do
                                                         void hitButton
                                                      ) (mods,0,0,initialStates mods,[])
                 print (low,high,low*high)

data Loop = Loop { loopDuration :: Integer, loopPulses :: [(Integer, Pulse)] }
  deriving (Show)

{-
findPattern :: Map.Map String Module -> Map.Map String States -> Map.Map String Loop -> String -> Loop
findPattern mods states patternsIn here = let mod = fromJust $ Map.lookup here mods
                                              initialState = fromJust $ Map.lookup here states
                                              duration = foldr gcd 0 . fmap (loopDuration . fromJust . (`Map.lookup` patternsIn) . moduleName) . moduleInputs mods $ mod
                                              inPulses = [ [(loopDuration loop * c + t, inputName, p) | c <- [0..], (t,p) <- loopPulses loop]
                                                         | inputName <- moduleName <$> moduleInputs mods mod
                                                         , let loop = fromJust . Map.lookup inputName $ patternsIn
                                                         ]
                                              allPulses :: [[(Integer,String,Pulse)]] -> [(Integer,String,Pulse)]
                                              allPulses ps = let (p,others) = findMin (head ps) (tail ps) []
                                                              in p:allPulses others

                                              findMin :: [(Integer,String,Pulse)] -> [[(Integer,String,Pulse)]] -> [[(Integer,String,Pulse)]] -> ((Integer,String,Pulse),[[(Integer,String,Pulse)]])
                                              findMin (pMin:psMins) [] ps = (pMin, psMins:ps)
                                              findMin pMin@((t,_,_):psMin) (pHere@((tHere,_,_):_):ps) ps'
                                                | tHere < t = findMin pHere ps (pMin:ps')
                                                | otherwise = findMin pMin ps (pHere:ps')

                                              threshold dur ((tNow,_,_):(tNext,_,_):_) = (dur > tNow && dur < tNext, if tNext > dur then dur + duration else dur)
                                              go s0 dur s (p:ps) = let (s', ps') = go1 s p
                                                                       (onThreshold, nextDuration) = threshold dur (p:ps)
                                                                       (finalDuration, finalSignals) = if onThreshold && s' == s0 then (dur, ps') else go s0 nextDuration s' ps
                                                                    in (finalDuration, ps' ++ finalSignals)
                                              go1 s (t,source,pulse) = case moduleType mod of
                                                                         FlipFlop -> if pulse == High
                                                                                       then (s,[])
                                                                                       else (FlipFlopState (not $ flipFlopOn s), [(t,if flipFlopOn s then Low else High)])
                                                                         Conj -> let s' = Map.insert source pulse (remembered s)
                                                                                  in (ConjunctionState s',[(t, if all (==High) . Map.elems $ s' then Low else High)])

                                           in uncurry Loop $ go initialState duration initialState $ allPulses inPulses

mkPatterns mods = let patterns = Map.fromList [(moduleName mod, findPattern mods (initialStates mods) patterns (moduleName mod)) | mod <- Map.elems mods]
                   in patterns
-}


part2 :: Parsed -> IO ()
part2 mods0 = do error "nope!!!!"
                 let mods = machineMap mods0
                 let (sts,_) = runState hitButtonManyMany (mods,0,0,initialStates mods,[])

                 forM_ (Map.elems mods) $ \mod -> do
                   putStr $ if moduleName mod == "broadcaster" then "BC   " else moduleName mod ++ "   "
                 putStrLn ""
                 forM_ (zip [1..] $ take 100 sts) $ \(n, sigs) -> do
                   forM_ (Map.elems mods) $ \mod -> do
                     let modSigs = [pulse | (mod',pulse) <- sigs, mod' == mod]
                     let highs = length (filter (==High) modSigs)
                     let lows = length (filter (==Low) modSigs)
                     let s = (if highs == 0 then "  " else show highs ++ "H") ++ (if lows == 0 then "  " else show lows ++ "L")
                     putStr $ s ++ " "
                   print n
                 --  
                 putStrLn ""
                 --forM (Map.elems mods) $ \mod -> do
                 --  putStrLn $ moduleName mod ++ " -> " ++ show (moduleRecipients mod) ++ "[<- " ++ show (fmap moduleName $ moduleInputs mods mod) ++ "]"
                 --  forM sts' $ \(st,sigs) -> do
                 --    putStrLn $ "  " ++ show (fromJust $ Map.lookup (moduleName mod) st) ++ " " ++ show [pulse | (mod', pulse) <- sigs, mod' == mod]
                 --  
                 --print . snd . head $ sts

                 --let patterns = mkPatterns mods
                 --print $ Map.lookup "ft" patterns


day20 part args = do let filename = case args of
                                      [] -> "inputs/day20"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
