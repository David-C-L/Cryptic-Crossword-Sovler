module CacheFunctions where

import Data.List
import Data.Maybe
import Data.Array.IArray
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import Debug.Trace
import Data.Set hiding ((\\), null, map, filter, foldr)

import Types
import Stemmer
import Databases
import Utilities
import Matches
--import CStrings
import Constants
import BoundsFunctions
import IndicatorPredicates

-----------------------------------------------------------------------------

-- Various functions for building and indexing the parser and evaluation 
-- caches. 
-- The module also includes the filters used to build subtrees for
-- each construction during parsing. The idea is to start with ALL trees
-- for a given piece of clue text (indexed by a pair of integers) and then
-- filter out the ones you don't want. This is much tidier that building
-- bespoke parsers for each construction.
--
--
-- The parser cache is indexed by a pair of word indices. It caches the text
-- string, its text synonyms, plain synonyms, a separate singular
-- synonym of each word and the list of all parse trees arising from
-- that index. The singular synonym table is needed because singular versions of
-- a word are not part of the clue text, e.g. when solving "two trees"
-- we need synonyms of "tree", not "trees".
-- Note: parseAllClueTypes has no charades at the topmost level, so
-- makeCharades completes the set. This guarantees that all parse trees are
-- structurally unique.
--
-- When you're building a Charade node in makeCharades the argument trees
-- should contain only non-Charades. But those will already be cached,
-- so we can just look them up (getNonCharadeTrees).
-- partitions returns all subtext partitions; from these we can form
-- all possible concatenations using sequence (Data.List).
-- For each concatenation we build a Charade tree.
-- Note: index (0, n-1) is not valid, as there has to be a definition 
-- somewhere.
--
 
makeParseCache :: [String] -> 
                  (Index -> [ParseTree]) ->
                  ParserCache
makeParseCache clueText parseWordplay
  = array ((0, 0),(clen - 1, clen - 1))
          (map makeEntry (substringIndices (0, clen - 1) \\ [(0, clen - 1)])) 
  where
    clen = length clueText
    stemmedText = map stem clueText
    makeEntry index
      = (index, CacheEntry 
                  textString
                  stemmedTextWords
                  textSynTable
                  synTable
                  singularSynTable
                  indPreds
                  nTrees
                  allParseTrees
                  parseTreeArray)
        where
          --
          -- We give both synonym tables the same bounds to avoid
          -- potential array bounds errors when evaluating synonyms.
          -- E.g. a Synonym node may show a lower bound of 2 because
          -- of a singular synonym, whereas the plain synonym has a
          -- bound of 3. 
          -- parseWordplay generates the parse trees for the given 
          -- index.
          -- The parse tree array is simply to speed up indexing when
          -- using an evaluation cache.
          --
          -- The null check avoids taking the min(max)imum of an empty list.
          -- nullBounds is defined in the Constants module.
          --
          -- StemCache is an array mapping an index to a list of stemmed words
          -- corresponding to that index.
          --
          
          computeBounds xs
            | null xs   = nullBounds
            | otherwise = (minimum xs, maximum xs)

          textString = unwords (indexList index clueText)
          stemmedTextWords = indexList index stemmedText
          textSyns = textExpansions textString
          textLengths = map length textSyns
          (minL, maxL) = computeBounds textLengths
          textSynTable = makeSynonymTable textSyns minL maxL
          plainSyns = synonyms textString
          singularSyns = synonyms (makeSingular textString)
          synLengths = map length (plainSyns ++ singularSyns)
          (minL', maxL') = computeBounds synLengths
          synTable = makeSynonymTable plainSyns minL' maxL'
          singularSynTable = makeSynonymTable singularSyns minL' maxL'
          indPreds = buildIndicatorPredicateSet stemmedTextWords
          allParseTrees = parseWordplay index 
          nTrees = length allParseTrees
          parseTreeArray :: Array Int ParseTree
          parseTreeArray = listArray (0, nTrees - 1) allParseTrees

--
-- Used in the evaluator to get all candidate solutions from
-- a fragment of text, given by def, and also when building an evaluation
-- cache (experimental -- see below). Note that def is an Index, 
-- i.e. a pair of indices into the clue text. That text is copied
-- in here, as we need to check that a candidate isn't one of
-- the words already in the clue. 
-- The parser cache (pCache) also includes a cache of the synonyms
-- of words/phrases in the clue text, indexed (<!>) by length.
--
getCandidates :: ParserCache -> Index -> Int -> String -> [String] -> [String]
getCandidates pCache def n answer textWords
  = [s | s <- syns, not (elem s textWords)]
  where
    syns = if null answer
           then (getSynonymTable pCache def) <!> n
           else [answer]

--
-- The evaluation cache is experimental. The Matches module computes all
-- substring indices that might be used during backwards evaluation.
-- The idea is to cache the result of the evaluation when those indices 
-- are applied to a concrete candidate string. The cache is indexed by the 
-- parse tree id (an (i, j, k) triple) and the string subtext. Another
-- experiment encodes small strings (<= 23 characters) as a pair of 64-bit 
-- ints and uses the two ints instead of the raw stsring subtext.
-- That is used here, although a small fix to the types and Map below
-- makes it easy to revert. 
-- Another experiment would involve replacing all strings during evaluation
-- with the 128-bit encoded strings, which should reduce memory pressure.
-- The performance gains of cacheing are minimal, but the code is retained. 
-- If you turn the cache off during evalution (both ON and OFF versions are 
-- supplied in the evaluation module) then this code is not used.
--
makeEvalCache :: Clue -> String -> [(Parse, String, [String])] -> ParserCache -> Evaluator -> EvalCache
makeEvalCache (c, n) answer ps pCache evaluator
  = trace (show allCandidates) $ array ((0, 0), (clen - 1, clen - 1))
           (map makeEntry (substringIndices (0, clen - 1) \\ [(0, clen - 1)]))
  where
    clen = length clueText
    clueText = words c

    getParseTree ((_, _, t), _, _)
      = t

    getCands (_, _, cands)
      = cands

    getIndices :: TreeId -> [(TreeId, [Int])] -> [[Int]]
    getIndices index hashlist
      = [indices | (i, indices) <- hashlist, i == index]

    makeStrings :: String -> [[Int]] -> [String]
    makeStrings s indices
      = map makeString indices
      where
        makeString is = [s !! n | n <- is]

    ms = matches [0 .. n - 1] (map getParseTree ps)
    es = Data.Set.toList ms

    allCandidates
      = concatMap getCands ps

    makeEntry :: Index ->
                 (Index, (Array Int
                                (Map.Map String [ResultTree])))
    makeEntry index@(i, j)
      = (index, array (0, nTrees - 1) (map makeEntry' [0 .. nTrees - 1]))
      where
        nTrees = getNumberOfTrees pCache index
        makeEntry' :: Int -> (Int, (Map.Map String [ResultTree]))
        makeEntry' k
          = (k, Map.fromList (concatMap makeEntries allCandidates))
          where
            t = getTree pCache (i, j) k
            allIndices = getIndices (i, j, k) es
            makeEntries cand
              = map f substrings
              where
                substrings = makeStrings cand allIndices
                nullText = array (0, -1) []
                f substr = (substr,
                            evaluator substr t UseAllSyns True nullText)

indexEvalCache :: EvalCache -> Int -> Int -> Int -> String -> [ResultTree]
indexEvalCache evCache i j k s
  = fromMaybe err (Map.lookup s ((evCache ! (i, j)) ! k))
  where
    err = error ("Evaluation cache lookup error: " ++ show (i, j, k) ++ " " ++ s)

--
-- Builds a table (Int -> String) of the synonyms of ONE piece of text
-- Synonyms comprise only single words.
-- We only store words up to a maximum length (a Constant).
--
makeSynonymTable :: [String] -> Int -> Int -> SynonymTable
makeSynonymTable ws mn mx
  = ((mn, min mx maxWordLength), makeSynonymArray sortedSyns)
  where
    ws' = nub $ filter (not . elem ' ') ws
    sortedSyns = sortOn fst (zip (map length ws') ws')

    makeSynonymArray :: [(Int, String)] -> 
                        Array Int (Set.Set String, [String])
    makeSynonymArray syns
      = listArray (0, maxWordLength) (map makeArrayEntry wordLists)
      where
        makeArrayEntry wl = (Set.fromList wl, wl)
        wordLists = map nub (makeWordLists (groupBy eqLen syns) 0)
        eqLen (k, _) (k', _) = k == k'
        makeWordLists _ k
          | k > maxWordLength = []
        makeWordLists [] k
          = [] : makeWordLists [] (k + 1)
        makeWordLists syns@(wl@((len, _) : _) : syns') k
          | len == k  = map snd wl : makeWordLists syns' (k + 1)
          | otherwise = [] : makeWordLists syns (k + 1)
 
isEmpty :: SynonymTable -> Bool
isEmpty (bs, table)
  = bs == nullBounds

-- (<!>) :: (a, Array Int [String]) -> Int -> [String]
(<!>) :: SynonymTable -> Int -> [String]
infix 1 <!>
(_, table) <!> index 
  = if index > maxWordLength
    then []
    else snd (table ! index)

(<!!>) :: SynonymTable -> Int -> Set.Set String
infix 1 <!!>
(_, table) <!!> index 
  = if index > maxWordLength
    then Set.empty
    else fst (table ! index)

--
-- Parser pCache getter functions...
-- NOte: the functions on the RHSs are record projectors
-- in the CacheEntry data type.
--
getWords :: ParserCache -> Index -> [String]
getWords pCache index
  = words (getString pCache index)

getString :: ParserCache -> Index -> String
getString pCache index
  = clueText (pCache ! index)

getStemmedWords :: ParserCache -> Index -> [String]
getStemmedWords pCache index
  = stemmedWords (pCache ! index)

getMergedString :: ParserCache -> Index -> String
getMergedString pCache index
  = filter (/= ' ') (getString pCache index)

getStringLength :: ParserCache -> Index -> Int
getStringLength pCache index
  = length (getMergedString pCache index)

getTextSynonymTable :: ParserCache -> Index -> SynonymTable
getTextSynonymTable pCache index
  = textSynonyms (pCache ! index)

getSynonymTable :: ParserCache -> Index -> SynonymTable
getSynonymTable pCache index
  = plainSynonyms (pCache ! index)

getSingularSynonymTable :: ParserCache -> Index -> SynonymTable
getSingularSynonymTable pCache index
  = singularSynonyms (pCache ! index)

getIndicatorPredicateSet :: ParserCache -> Index -> IndicatorPredicateSet
getIndicatorPredicateSet pCache index
  = indicatorPredicates (pCache ! index)

getNumberOfTrees :: ParserCache -> Index -> Int
getNumberOfTrees pCache index
  = numSubtrees (pCache ! index)

getAllTrees :: ParserCache -> Index -> [ParseTree]
getAllTrees pCache index
  = parseTrees (pCache ! index)

getTree :: ParserCache -> Index -> Int -> ParseTree
getTree pCache index k
  = parseTreeArray (pCache ! index) ! k

