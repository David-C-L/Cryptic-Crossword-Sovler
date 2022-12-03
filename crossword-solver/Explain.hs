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
--import Benchmarks.Times0to9999
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
import ShowFunctions

explainAll :: [(Clue, String)] -> Int -> Int -> String -> IO()
explainAll clues m n filename 
  = do
      h <- openFile filename WriteMode
      mapM_ (explain h)
            (zip [m..] cs)
      hPutStrLn h ("Clues = " ++ show (length cs))
      hClose h
  where
    cs = filter clueOK (take (n - m + 1) (drop m clues))
    clueOK ((ctext, _), _) 
      = len <= 12 && not (len == 2 && w == "see" && isDigit (head w'))
      where
        ws = words ctext
        len = length ws
        (w : w' : _) = ws

explain :: Handle -> (Int, (Clue, String)) -> IO ()
explain h (n, (clue@(text, clen), answer)) 
  = do 
      trace (show n) $ explainSol
  where
    len = length (words (fst (clue)))
    explainSol
      | null sols
        = return ()
      | not (null validSols)
        = do 
            showClue
            hPutStrLn h (showSol pCache (head validSols))
            hPutStrLn h ""
      | not (null validSynSols)
        = do
            showClue
            hPutStrLn h (showSol pCache (head validSynSols))
            hPutStrLn h ""
      | otherwise
        = return ()

    showClue = hPutStrLn h (text ++ " (" ++ show clen ++ ")")

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
    sols = evaluate ps pCache clue answer' False
    
main 
  = do
      explainAll (map getClue telegraph) 0 9999 "explain-telegraph-100000-109999"
      -- explainAll (map getClue everyman) 7000 16999 "explain-everyman-7000-16999" 
      -- explainAll (map getClue times) 0 9999 "explain-times-0-9999-noindirect"
      -- explainAll (map getClue rufus) 0 10000 "explain-rufus-0-10000-noindirect"
      -- explainAll (map getClue telegraph) 0 9999   "explain-telegraph-0-9999"
      -- explainAll (map getClue everyman) 0 17500 "explain-everyman-0-17500"
      -- explainAll (map getClue cryptonite) 0 500 "explain-cryptonite-0-500"
      -- explainAll (map getClue cryptonite) 0 20000 "explain-cryptonite-0-20000"
      -- explainAll (map getClue rufus) 15287 15386 "explain-rufus-15287-17786"
      -- solveAll (map getClue araucaria) 0 2499 False False "araucaria-withoutanswer-0-2499"
      -- solveAll (map getClue araucaria) 0 2499 True False "araucaria-withanswer-0-2499"
      -- solveAll (map getClue paul) 9201 11700 True False "paul-withanswer-9201-11700-2"
      -- solveAll (map getClue paul) 9201 11700 False False "paul-withoutanswer-9201-11700-2"
      -- solveAll (map getClue paul) 9201 11700 False False "paul-withoutanswer-9201-11700"
      -- solveAll (map getClue paul) 0 2499 True False "paul-withanswer-0-2499"
      -- solveAll (map getClue paul) 9201 11700 True False "paul-withanswer-9201-11700"
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
topLevel' (_, (Insertion i i' t t', _))
  | elem '.' i' = InsertionIndL2 
  | otherwise = InsertionIndL1 
topLevel' (_, (Subtraction i t t', _))
  = SubtractionIndL1
topLevel' (_, (Charade i ts, _))
  = CharadeIndL1

