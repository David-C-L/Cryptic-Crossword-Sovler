module Solver where 

import State
import Debug.Trace
import Data.List  
import Data.Char
import Control.Monad
import Data.Array.IArray

import Matches
import Indicators
import IndicatorPredicates
import Clues
import Parser
import Types
import Evaluation
import Databases
import Utilities
import CacheFunctions
import Constants
import Stemmer
--import Benchmarks.Cryptonite
import Benchmarks.Everyman
--import Benchmarks.Rufus
import Benchmarks.Telegraph100000to109999
--import Benchmarks.Times0to9999
import ShowFunctions

sol n = let (_,c,s) = telegraph!!n in trace s $ solveWithAnswer c (map toLower s)
-------------------------------------------------------------------------
-- Top-level solver functions.
--
-- Most of this is I/O.
--

head' []
  = []
head' sols
  = [head sols]

solve clue 
  = showSolutions False clue "" id head' False

solveFull clue 
  = showSolutions True clue "" id head' False

solveAllFull clue 
  = showSolutions True clue "" id id False

solveWithCache clue 
  = showSolutions False clue "" id head' True

solveWithAnswer clue answer
  = showSolutions False clue answer id head' False

solveOne n clue
  = showSolutions True clue "" (\ps -> [ps !! n]) head' False

solveOneWithAnswer n clue answer
  = showSolutions False clue answer (\ps -> [ps !! n]) head' False

solveAll clue 
  = showSolutions False clue "" id id False

solveAllWithCache clue 
  = showSolutions False clue "" id id True

solveAllWithAnswer clue answer
  = showSolutions False clue answer id id False

--
-- The preFilter allows seleced parse trees to be picked out for
-- evaluation. The postProcess can be used to select either the 
-- first solution, all solutions etc. If the answer is provided
-- (answer not "") then the evaluator essentially discards the
-- definition and matches the parse tree(s) against the given 
-- string. This checks the solver's ability to wolve the wordplay
-- alone.
--
showSolutions displayFull clue answer prefilter postProcess evalCacheOn
  | null sols' = putStrLn ("The clue is: " ++ show clue ++ 
                          "\nSorry, but I couldn't solve it\n")
  | displayFull = mapM_ (display clue pCache) sols'
  | otherwise = mapM_ (putStrLn . showSol pCache) sols'
  where
    acceptAnyDef = not (null answer)
    (parses, pCache) = if null answer
                       then prunedParses clue
                       else prunedParsesWithAnswer clue answer
    parses' = prefilter parses
    sols = evaluate parses' pCache clue answer evalCacheOn
    sols' = postProcess sols

dumpIndicators :: (IndicatorPredicateSet, String, [String]) -> IO ()
dumpIndicators (inds, textString, stemmedTextWords)
  = mapM_ showInd [i | i <- [minBound .. maxBound], inds ! i]
  where
    showInd c
      = putStrLn $ show c ++ ": " ++ textString ++ " (" ++ show stemmedTextWords ++ ")\n"

-------------------------------------------------------------------------
--
-- The display functions. This could be tidied up...
--

quote s
  = "\"" ++ s ++ "\""

quote' s 
  = "\"" ++ unwords s ++ "\""

indent n k s
  = putStrLn ((replicate (n * k) ' ') ++ s)

getRes (R (s, _)) = s

display :: Clue -> ParserCache -> Solution -> IO ()
display clue@(ctxt, clen) pCache (d, i, t, rt@(R (sol, _)))
  = do 
      putStrLn ("The clue is: " ++ ctxt ++ " (" ++ show clen ++ ")")
      putStrLn ("I think the solution is " ++ 
                "\"" ++ map toUpper (filter (/=' ') sol) ++ "\"\n")
      putStrLn ("Let me explain...\nI think the answer is supposed to mean " ++
                quote (getString pCache d))
      when (i /= noInd) $ 
        putStrLn (quoteIndex i ++ " separates the definition from the wordplay")
      displayTree t rt 0 1
      putStrLn "-----------"
  where
    showSyn txt s n k
      = do
          indent n k (message arg s)
      where
        arg = getString pCache txt

    showSyn' txt s n k
      = do
          indent n k (message txt s)

    message arg s
      | arg == s  = quote s ++ " is verbatim text"
      | otherwise = "The text " ++ quote arg ++ " resolves to " ++ quote s

    quoteIndex index
      = quote (getString pCache index)

    quoteBinaryIndex index index'
      | index' == noInd = quoteIndex index
      | otherwise = quote (getString pCache index ++ "..." ++
                           getString pCache index')

    displayQuote :: String -> Indicator -> ClueText -> String -> Int -> Int -> 
                    IO()
    displayQuote desc i txt s n k
      = do
          indent n k (quote ind ++ " indicates " ++ desc ++ 
                      " with " ++ quote arg ++ " as the target")
          indent n 1 ("The result is " ++ quote s)
      where
        ind = getString pCache i
        arg = getString pCache txt

    -- 
    -- n is the indentation offset. k is a binary scaling factor: 0 means do not 
    -- indent the first line as we're sitting after a putStr; 1 means indent
    -- as specified by the n. It's just for tidying up the formatting.
    -- Note that s may be different to txt if it txt contains apostrophes.
    --

    --
    -- When we take out from raw text we don't recursively evaluate the
    -- subtree. Instead we use getTextStrings. In that case rts is null so 
    -- we have to assemble the explanation from the parse tree and two strings
    -- (take-out target and result). There is some redundancy, but it
    -- simplifies the evaluator.
    --
    displayTakeout i q t rt@(R (s', rts)) s n k
      = do
          indent n k ("I think " ++ quoteIndex i ++ " indicates " ++ q)
          when (null rts) (displayTakeoutArg t s s' (n + 3) 1)
          when (not (null rts)) (displayTree t rt (n + 3) 1)
          indent n 1 ("The result of the take-out is " ++ quote s)
    displayTakeoutArg (_, (Synonym txt, _)) s s' n k
      = showSyn txt s' n k
    displayTakeoutArg (_, (Hyponym i txt, _)) s s' n k
      = do
          showSyn txt s n k
          indent n k ("(" ++ quoteIndex i ++
                      " indicates a specific example of something)")
    displayTakeoutArg (_, (Reversal i t, _)) s s' n k
      = do
          indent n k ("I think " ++ quoteIndex i ++ " indicates " ++
                      "a reversal")
          indent n k ("The target is the text " ++ quote (reverse s'))
          indent n 1 ("Reversing this gives " ++ quote s')
    displayTakeoutArg t@(_, (Charade i ts, _)) s s' n k
      = do return ()

    displayTree :: ParseTree -> ResultTree -> Int -> Int -> IO ()
    displayTree (_, t) r n k
      = displayTree' t r n k
      where
        displayTree' (SpareWord sw _, _) (R (s, _)) n k
          = showSyn' sw s n k
        displayTree' (Synonym txt, _) (R (s, _)) n k
          = showSyn txt s n k
        displayTree' (Hyponym i txt, _) (R (s, _)) n k
          = do
              showSyn txt s n k
              indent n 1 ("(" ++ quoteIndex i ++
                          " indicates a specific example of something)")
        displayTree' (Abbreviation i i', _) (R (s', [t@(R (s'', _))])) n k
          | i == noInd
            = do
                indent n k ("The word " ++ quoteIndex i' ++ " abbreviates to " ++ quote s')
          | otherwise
            = do
                indent n k ("I think " ++ quoteIndex i ++ " indicates an abbreviation")
                displayTree' (Synonym i', undefined) t (n + 3) 1
                indent (n + 3) 1 ("The word " ++ quote s'' ++ " abbreviates to " ++ quote s')
        --
        -- Some text may indicate reversals or anagrams. We pick the former
        -- if there is ambiguity (an edge case for a better explanation).
        --
        displayTree' (Anagram i t@(_, (SpareWord _ i',_)), _) res@(R (s, [rt@(R (s', _))])) n k
            = do 
                indent n k ("I think " ++ quote i' ++ 
                            " indicates an anagram")
                displayTree t rt (n + 3) 1
                indent n 1 ("The required anagram is " ++ quote s)
        displayTree' (Anagram i t, bs) res@(R (s, [rt@(R (s', _))])) n k
{-
          | getIndicatorPredicateSet pCache i ! ReversalInd &&
            s == reverse s'
            = displayTree' (Reversal i t, bs) res n k
          | otherwise
-}
            = do 
                indent n k ("I think " ++ quoteIndex i ++ 
                            " indicates an anagram")
                displayTree t rt (n + 3) 1
                indent n 1 ("The required anagram is " ++ quote s)
        displayTree' (AnagramInf i t t', _) (R (s, [rt, rt'])) n k
          = do 
              indent n k ("I think " ++ quoteIndex i ++ 
                          " indicates an anagram of two sub-clues:")
              displayTrees (n + 3) [t, t'] [rt, rt']
              indent n 1 ("The required anagram is " ++ quote s)
        displayTree' (Odds i txt, _) (R (s, _)) n k
          = displayQuote "'take odd-numbered letters'" i txt s n k
        displayTree' (Evens i txt, _) (R (s, _)) n k
          = displayQuote "'take even-numbered letters'" i txt s n k
        displayTree' (FirstLetters i t, _) (R (s, [rt])) n k
          = displayTakeout i "a first-letters take-out" t rt s n k
        displayTree' (LastLetters i t, _) (R (s, [rt])) n k
          = displayTakeout i "a last-letters take-out" t rt s n k
        displayTree' (MiddleLetters i t, _) (R (s, [rt])) n k
          = displayTakeout i "a middle-letters take-out" t rt s n k
        displayTree' (EndLetters i t, _) (R (s, [rt])) n k
          = displayTakeout i "an end-letters take-out" t rt s n k
        displayTree' (Duplicate i w t, _) (R (s, [rt, rt'])) n k
          | s1 == s2
            = do
                indent n k ("I think " ++ quoteIndex i ++ " indicates " ++
                            "a duplication")
                displayTree t rt (n + 3) 1
                indent n 1 ("Duplicating this we get " ++ quote (removeSpaces s))
          | otherwise 
            = do
                indent n k ("I think " ++ quoteIndex i ++ " indicates " ++
                            "a duplication with " ++ quote w ++
                            " as the target")
                indent (n + 3) 1 ("The first resolves to " ++ quote s1)
                indent (n + 3) 1 ("The second resolves to " ++ quote s2)
                indent n 1 ("Combining them we get " ++ quote s)
          where
            s1 = getRes rt
            s2 = getRes rt'
        displayTree' (Homophone i t, _) (R (s, [rt@(R (s', _))])) n k
          = do
              indent n k ("I think " ++ quoteIndex i ++ " indicates " ++
                        "a homophone")
              displayTree t rt (n + 3) 1
              indent n 1 ("The homophone of " ++ 
                          quote s' ++ " is " ++
                          quote s)
        displayTree' (Reversal i t@(_, (SpareWord _ i',_)), _) (R (s, [rt])) n k
          = do
              indent n k ("I think " ++ quote i' ++ " indicates " ++
                        "a reversal")
              displayTree t rt (n + 3) 1
              indent n 1 ("Reversing this gives " ++ quote s)
        displayTree' (Reversal i t, _) (R (s, [rt])) n k
          = do
              indent n k ("I think " ++ quoteIndex i ++ " indicates " ++
                        "a reversal")
              displayTree t rt (n + 3) 1
              indent n 1 ("Reversing this gives " ++ quote s)
        displayTree' (Insertion i ind t t', _) (R (s, [rt, rt'])) n k
          = do
              indent n k ("I think " ++ quote ind ++ " indicates " ++ 
                        "the insertion of one word into another")
              displayTrees (n + 3) [t, t'] [rt, rt']
              indent n 1 ("For the insertion, " ++ quote (getRes rt) ++ 
                        " needs to be inserted into " ++
                        quote (getRes rt'))
              indent n 1 ("The result is " ++ quote s)
        displayTree' (Subtraction i t t', _) (R (s, [rt, rt'])) n k
          = do
              indent n k ("I think " ++ quoteIndex i ++ " indicates " ++ 
                        "the removal of one word from another")
              displayTrees (n + 3) [t, t'] [rt, rt']
              indent n 1 ("For the removal, " ++ quote (getRes rt) ++ 
                        " needs to be removed from " ++ 
                        quote (getRes rt'))
              indent n 1 ("The result is " ++ quote s)
        -- If there is no indicator then this is the start of a Charade tree...
        displayTree' t@(Charade i ts, _) (R (s, rts)) n k
          | i == noInd 
            = do
                indent n k ("I think we have the concatenation of " ++ 
                            show (length ts) ++ " sub-clues:")
                displayTrees (n + 3) ts rts
                indent n 1 ("Joining the results together we get " ++ quote s)
        displayTree' (Charade i ts, _) (R (s, rts)) n k
          = do
              indent n k ("I think " ++ quoteIndex i ++ " indicates " ++
                        "a charade (juxtaposition) of " ++
                        show (length ts) ++ " sub-clues:")
              displayTrees (n + 3) ts rts
              indent n 1 ("Joining the results together we get " ++ quote s)

        displayTrees n ts rts
          = mapM_ displayOneTree (zip [1..] (zip ts rts))
          where
            displayOneTree (i, (t, rt))
              = do
                  putStr (replicate n ' ' ++ show i ++ ": ")
                  displayTree t rt (n + 3) 0

