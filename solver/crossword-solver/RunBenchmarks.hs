{-# LANGUAGE BangPatterns #-}

--import Benchmarks.Guardian25989
--import Benchmarks.Everyman
--import Benchmarks.Cryptonite
--import Benchmarks.Rufus
--import Benchmarks.Paul
--import Benchmarks.Araucaria
--import Benchmarks.Gordius
--import Benchmarks.Pasquale
--import Benchmarks.Telegraph0to10000
import Benchmarks.Telegraph100000to109999
import Benchmarks.Times100000to109999
import Data.Char
import Parser
import Evaluation
import Types
import System.IO
import CacheFunctions
import ParseTreeFilters
import Debug.Trace
import Data.List
import Constants
import Data.Time.Clock

head' []
  = []
head' sols
  = [head sols]

solveAll :: [(Clue, String)] -> Int -> Int -> Bool -> Bool -> String -> IO()
solveAll clues m n useAnswer evCacheOn filename 
  = do
      h <- openFile filename WriteMode
      t <- getCurrentTime
      let cs = filter clueOK (zip [m..] (take (n - m + 1) (drop m clues)))
      let ncs = length cs
      (lens, sumN, dep, sumV, sumI, times) <- processClues h ([], 0, 0, zeroVec, zeroVec, []) cs
      t' <- getCurrentTime
      hPutStrLn h ("Solve time: " ++ show (diffUTCTime t' t))
      hPutStrLn h (show (zip [minBound :: IndicatorTypes .. maxBound] sumI))
      hPutStrLn h (show (zip [minBound :: IndicatorTypes .. maxBound] sumV))
      hPutStrLn h (show lens)
      hPutStrLn h (show times)
      hPutStrLn h ("Av. depth = " ++ show (fromIntegral dep / fromIntegral sumN))
      hPutStrLn h ("Solved: " ++ show sumN ++ "/" ++ show ncs)
      hClose h
  where
    --
    -- The various checks force evaluation so as to avoid space leaks
    --
    processClues h acc []
      = return acc
    processClues h acc@(lens, m, dep, v, i, ts) (clue : clues)
      | not (null lens) && toInt (head lens) < 0 ||
          m < 0 ||
          dep < 0 ||
          sum v < 0 ||
          sum i < 0 ||
          not (null ts) && head ts < 0
        = return acc
      | otherwise
        = do
          res <- solve h useAnswer evCacheOn clue
          let acc' = update acc res
          processClues h acc' clues
      where
        toInt (Left x) = x
        toInt (Right x) = x
        update acc@(lens, m, dep, v, i, ts) ((len, n, dep', v', i'), t)
          = if n == 0
            then (Left len : lens, m, dep, v, i, t : ts)
            else (Right len : lens, m + n, dep + dep', addV v v',
              if i' == dummyTopLevel
              then i
              else addV i (makeVec i'), t : ts)

    clueOK :: (Int, (Clue, String)) -> Bool
    clueOK (_, ((ctext, _), _))
      = len <= 12 && not (len == 2 && w == "see" && isDigit (head w'))
      where
        ws = words ctext
        len = length ws
        (w : w' : _) = ws

makeVec ind
  = [if ind == i then 1 else 0 | i <- [minBound :: IndicatorTypes .. maxBound]]

addV
  = zipWith (+)

type Vec = [Int]

solve :: Handle -> Bool -> Bool -> (Int, (Clue, String)) -> IO ((Int, Int, Int, Vec, IndicatorTypes), NominalDiffTime)
solve h useAnswer evCacheOn (n, (clue, answer)) 
  = do t <- getCurrentTime
       sol <- solveClue
       t' <- getCurrentTime
       return (sol, diffUTCTime t' t)
  where
    solveClue
      = trace (show n) $ do
          showSol
      where
        len = length (words (fst (clue)))
        showSol
          | null sols
            = do 
                hPutStrLn h (details ++ " ---")
                return (len, 0, 0, zeroVec, dummyTopLevel)
          | not (null validSols)
            = do 
                hPutStrLn h (details ++ " " ++ "SOLVED")
                case validSols of
                  !(!h : _) -> return (len, 1, depth h, analyseSolution h, topLevel h)
          | not (null validSynSols)
            = do
                hPutStrLn h (details ++ " " ++ "SYN-SOLVED")
                case validSynSols of
                  !(!h : _) -> return (len, 1, depth h, oneVec h, topLevel h)
          | otherwise
            = do 
                hPutStrLn h (details ++ " ---")
                return (len, 0, 0, zeroVec, dummyTopLevel)
          where
            validSols = filter isValidSol sols
            validSynSols = filter isValidSynSol sols
            details = show n ++ " (" ++ show (length ps) ++ ")" 
            answer' = map toLower answer 
            getTree (_, _, pt, _) = pt
            isValidSol (_, _, pt, R (sol, _))
              = not (isSynonymTree pt) && 
                not (isHyponymTree pt) &&
                filter (/=' ') sol == answer' 
            isValidSynSol (_, _, pt, R (sol, _))
              = (isSynonymTree pt || isHyponymTree pt) && 
                filter (/=' ') sol == answer' 
                && len <= 4
            (ps, pCache) = prunedParses clue answer
            sols = evaluate ps pCache clue (if useAnswer then answer' else "") evCacheOn

main 
  = do
      -- solveAll (map getClue times) 0 9999 True False "times-withanswer-100000-109999"
      solveAll (map getClue telegraph) 0 9999 False False "telegraph-withoutanswer-100000-109999"
      -- solveAll (map getClue telegraph) 0 9999 True False "telegraph-withanswer-100000-109999"
      -- solveAll (map getClue guardian) 15000 19999 True True "guardian-withanswer-15000-24999-noIndirect-cacheOn"
      -- solveAll (map getClue everyman) 7000 16999 True False "everyman-withanswer-7000-16999-noIndirect"
      -- solveAll (map getClue everyman) 0 9999 True False "everyman-withanswer-0-9999-noIndirect-2"
      -- solveAll (map getClue times) 0 999 True False "times-withanswer-0-999-indirect"
      -- solveAll (map getClue telegraph) 0 9000 True False "telegraph-withanswer-0-9000-2"
      -- solveAll (map getClue telegraph) 0 9000 True False "telegraph-withanswer-0-9000"
      -- solveAll (map getClue araucaria) 0 2499 False False "araucaria-withoutanswer-0-2499"
      -- solveAll (map getClue araucaria) 0 2499 True False "araucaria-withanswer-0-2499"
      -- solveAll (map getClue paul) 9201 11700 True False "paul-withanswer-9201-11700-2"
      -- solveAll (map getClue paul) 9201 11700 False False "paul-withoutanswer-9201-11700-2"
      -- solveAll (map getClue paul) 9201 11700 False False "paul-withoutanswer-9201-11700"
      -- solveAll (map getClue paul) 0 2499 True False "paul-withanswer-0-2499"
      -- solveAll (map getClue paul) 9201 11700 True False "paul-withanswer-9201-11700"
      -- solveAll (map getClue rufus) 15287 17786 True False "rufus-withanswer-15287-17786-2"
      -- solveAll (map getClue rufus) 15287 17786 True False "rufus-withanswer-15287-17786"
      -- solveAll (map getClue rufus) 15287 17786 False False "rufus-withoutanswer-15287-17786-2"
      -- solveAll (map getClue rufus) 15287 17786 False False "rufus-withoutanswer-15287-17786"
      -- solveAll (map getClue everyman) 0 1000 "everyman-withoutanswer-0-1000" 
      -- solveAll (map getClue everyman) 0 1000 "everyman-withanswer-0-1000-2"
      -- solveAll (map getClue everyman) 0 1000 "everyman-withanswer-0-1000-4"
      -- solveAll (map getClue everyman) 1000 10999 True "everyman-withanswer-1000-10999"
      -- solveAll (map getClue everyman) 1000 10999 False "everyman-withoutanswer-1000-10999"
      -- solveAll (map getClue everyman) 2000 2999 "everyman-withoutanswer-2000-2999-compound"
      -- solveAll (map getClue everyman) 0 17595 "everyman-withanswer-0-17595-2"
      -- solveAll (map getClue everyman) 2000 2999 "everyman-withoutanswer-4-2000-2999"
      -- solveAll (map getClue guardian) 0 9999 False "guardian-withoutanswer-0-9999"
      -- solveAll (map getClue guardian) 0 1000 "guardian-withoutanswer-0-1000"
      -- solveAll (map getClue guardian) 1000 10999 True False "guardian-withanswer-1000-10999-8"
      -- solveAll (map getClue guardian) 0 1000 "guardian-withanswer-0-1000"
      -- solveAll (map getClue guardian) 1000 10999 False True "guardian-withoutanswer-1000-10999-EvOn"
      -- solveAll (map getClue guardian) 1554 1554 False True "guardian-withoutanswer-1554-1554-EvOn"
      -- solveAll (map getClue guardian) 2305 2305 True "guardian-withanswer-2305-2305"
      -- solveAll (map getClue guardian) 2305 2305 False "guardian-withoutanswer-10315-10315"
      -- solveAll (map getClue guardian) 10315 10315 False "guardian-withoutanswer-10315-10315-EvOn"
      -- solveAll (map getClue cryptonite) 1000 1999 True "cryponite-withanswer-1000-1999" 
      -- solveAll (map getClue cryptonite) 1000 1999 "cryponite-withoutanswer-1000-1999"
      -- solveAll (map getClue cryptonite) 100 199 "cryponite-withoutanswer-100-199"
      -- solveAll (map getClue cryptonite) 0 1000 "cryponite-withoutanswer-0-1000"
      -- solveAll (map getClue cryptonite) 1000 10999 True "cryponite-withanswer-1000-10999" 
      -- solveAll (map getClue cryptonite) 1000 10999 False "cryponite-withoutanswer-1000-10999" 
      -- solveAll (map getClue cryptonite) 1000 1099 "cryponite-withanswer-1000-1099"
      -- solveAll (map getClue cryptonite) 5000 6000 "cryponite-withanswer-5000-6000"
      -- solveAll (map getClue cryptonite) 1001 10000 "cryponite-withanswer-1001-10000"
  where
    getClue (_, c, n) = (c, n)

zeroVec :: [Int]
zeroVec 
  = map (const 0) [minBound :: IndicatorTypes ..  maxBound]

oneVec :: Solution -> [Int]
oneVec (_, _, ((_, (Synonym _, _))), _)
  = [if treeType == DefIndicator then 1 else 0 | 
       treeType <- [minBound ..  maxBound]]
oneVec (_, _, ((_, (Hyponym _ _, _))), _)
  = [if treeType == HyponymInd then 1 else 0 | 
       treeType <- [minBound ..  maxBound]]
oneVec _
  = zeroVec

analyseSolution :: Solution -> [Int]
analyseSolution (_, _, pt, _)
  = [count treeType (analyseSolution' pt) | treeType <- [minBound .. maxBound]]
  where
    count x xs = length (filter (== x) xs)

analyseSolution' :: ParseTree -> [IndicatorTypes]
analyseSolution' (_, (SpareWord s i, _))
  = [CharadeIndL2]
analyseSolution' (_, (Synonym (i, j), _))
  = [DefIndicator]
analyseSolution' (_, (Hyponym i j, _))
  = [HyponymInd]
analyseSolution' (_, (Anagram i t, _))
  = AnagramInd : analyseSolution' t
analyseSolution' (_, (AnagramInf i t1 t2, _))
  = AnagramInfInd : analyseSolution' t1 ++ analyseSolution' t2
analyseSolution' (_, (Abbreviation i j, _))
  = [AbbreviationsInd]
analyseSolution' (_, (Odds i j, _))
  = [OddsInd]
analyseSolution' (_, (Evens i j, _))
  = [EvensInd]
analyseSolution' (_, (FirstLetters i t, _))
  = FirstLettersInd : analyseSolution' t
analyseSolution' (_, (LastLetters i t, _))
  = LastLettersInd : analyseSolution' t
analyseSolution' (_, (MiddleLetters i t, _))
  = MiddleLettersInd : analyseSolution' t
analyseSolution' (_, (EndLetters i t, _))
  = EndLettersInd : analyseSolution' t
analyseSolution' (_, (Duplicate i s t, _))
  = DuplicateInd : analyseSolution' t
analyseSolution' (_, (Homophone i t, _))
  = HomophoneInd : analyseSolution' t
analyseSolution' (_, (Reversal i t, _))
  = ReversalInd : analyseSolution' t
analyseSolution' (_, (Rotation i t, _))
  = RotationInd : analyseSolution' t
analyseSolution' (_, (Insertion i i' t t', _))
  | elem '.' i' = InsertionIndL2 : analyseSolution' t ++ analyseSolution' t'
  | otherwise = InsertionIndL1 : analyseSolution' t ++ analyseSolution' t'
analyseSolution' (_, (Subtraction i t t', _))
  = SubtractionIndL1 : analyseSolution' t ++ analyseSolution' t'
analyseSolution' (_, (Charade i ts, _))
  = CharadeIndL1 : concatMap analyseSolution' ts

dummyTopLevel :: IndicatorTypes
dummyTopLevel
  = SubtractionIndL2

topLevel :: Solution -> IndicatorTypes
topLevel (_, _, pt, _)
  = topLevel' pt

topLevel' :: ParseTree -> IndicatorTypes
topLevel' (_, (SpareWord _ _, _))
  = CharadeIndL2
topLevel' (_, (Synonym (i, j), _))
  = DefIndicator
topLevel' (_, (Hyponym i j, _))
  = HyponymInd
topLevel' (_, (Anagram i t, _))
  = AnagramInd
topLevel' (_, (AnagramInf i t1 t2, _))
  = AnagramInfInd
topLevel' (_, (Abbreviation i j, _))
  = AbbreviationsInd
topLevel' (_, (Odds i j, _))
  = OddsInd
topLevel' (_, (Evens i j, _))
  = EvensInd
topLevel' (_, (FirstLetters i t, _))
  = FirstLettersInd
topLevel' (_, (LastLetters i t, _))
  = LastLettersInd
topLevel' (_, (MiddleLetters i t, _))
  = MiddleLettersInd
topLevel' (_, (EndLetters i t, _))
  = EndLettersInd
topLevel' (_, (Duplicate i s t, _))
  = DuplicateInd
topLevel' (_, (Homophone i t, _))
  = HomophoneInd
topLevel' (_, (Reversal i t, _))
  = ReversalInd
topLevel' (_, (Rotation i t, _))
  = RotationInd
topLevel' (_, (Insertion i i' t t', _))
  | elem '.' i' = InsertionIndL2 
  | otherwise = InsertionIndL1 
topLevel' (_, (Subtraction i t t', _))
  = SubtractionIndL1
topLevel' (_, (Charade i ts, _))
  = CharadeIndL1

depth :: Solution -> Int
depth (_, _, pt, _)
  = depth' pt

depth' :: ParseTree -> Int
depth' ((_, d), _)
  = d
{-
  = 1
depth' (_, (Synonym (i, j), _))
  = 1
depth' (_, (Hyponym i j, _))
  = 1
depth' (_, (Anagram i t, _))
  = 1 + depth' t
depth' (_, (AnagramInf i t t', _))
  = 1 + max (depth' t) (depth' t')
depth' (_, (Abbreviation i j, _))
  = 1
depth' (_, (Odds i j, _))
  = 1
depth' (_, (Evens i j, _))
  = 1
depth' (_, (FirstLetters i t, _))
  = 1 + depth' t
depth' (_, (LastLetters i t, _))
  = 1 + depth' t
depth' (_, (MiddleLetters i t, _))
  = 1 + depth' t
depth' (_, (EndLetters i t, _))
  = 1 + depth' t
depth' (_, (Duplicate i s t, _))
  = 1 + depth' t
depth' (_, (Homophone i t, _))
  = 1 + depth' t
depth' (_, (Reversal i t, _))
  = 1 + depth' t
depth' (_, (Rotation i t, _))
  = 1 + depth' t
depth' (_, (Insertion i i' t t', _))
  = 1 + max (depth' t) (depth' t')
depth' (_, (Subtraction i t t', _))
  = 1 + max (depth' t) (depth' t')
depth' (_, (Charade i ts, _))
  = 1 + maximum (map depth' ts)
-}
