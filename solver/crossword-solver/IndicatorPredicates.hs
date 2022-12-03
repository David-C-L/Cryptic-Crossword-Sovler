module IndicatorPredicates where

import Data.Array.IArray
import Types
import qualified Data.Set as Set
import Stemmer
import Utilities
import Data.List
import Stemmer
import Indicators
import Constants
import Debug.Trace

--
-- Usef for command-line debugging
--
checkInd :: IndicatorTypes ->  String -> Bool
checkInd indPred s
  | length ws > maxIndicatorWords = False
  | otherwise = predSet ! indPred
  where
    ws = map stem (words s)
    predSet = buildIndicatorPredicateSet ws

-- The code here implements indicators manually, rather than by a
-- suitably-trained ANN, for example. The rules are somewhat ad hoc, but the
-- results are effective.
--
-- Stemmed noise comprises words that can safely be removed before checking for 
-- key indicator words. The noise words are specific to the indicator, e.g.
-- "up" may be noise for an angram, but a key indicator word for a reversal.
--
-- Stemmed Ind1 words are *single* words (stemmed) that can be taken as
-- indicators in their own right, but that might also qualify a second word or 
-- phrase, e.g. changing shape. Here "change" is an Ind1 word and "shape" an
-- Ind2 word. Again the Ind1 and Ind2 words are specific to the indicator.
-- Ind1 words are removed from Ind2 words, if there is an overlap.

-- WARNING: It is easy to increase massively the number of parses if you're not
-- careful. It is very easy for common words, e.g. "to" to end up being 
-- recognised as an indicator.
-- You must remember that Ind1 words get stripped from the Ind2 words, so it's
-- easy for, e.g. "to make" to become "to" if "make" is in Ind1.
-- Short plurals, e.g. "cars" get stemmed to "car" and is confused with "care", 
-- "caring" etc.
-- As an edge case, "cars" is a special case in the Stemmer.
--

stemAll
  = nub . map stem

removeWords :: [String] -> [String] -> [String]
removeWords stemmedWords
  = filter (not . (`elem` stemmedWords))

--
-- Remove any ind1 words from ws and ind2
--
removeWords' :: [String] -> [String] -> [[String]] -> ([String], [[String]])
removeWords' ws ind1 ind2
  = foldr f (ws, ind2) ind1
  where
    f ind1Word (ws, ind2) 
      | elem ind1Word ws = (ws \\ [ind1Word], 
                            [ind2Words \\ [ind1Word] | ind2Words <- ind2])
      | otherwise        = (ws, ind2)

-- First remove the noise. If there is nothing left it's not an indicator.
-- Then remove any ind1 words from both the candidate indicator (ws) and
-- ind2 words. If there is nothing left we've found an indicator, e.g. 
-- "back" or "is back" for a reversal. Otherwise if what's
-- left (ws') appears in ind2 then we've also succeeded, e.g. "is looking
-- "back", "wiped head" etc.
-- Note: all arguments have been stemmed by this point.
--

isIndicator :: [String] -> [String] -> [[String]] -> [String] -> Bool
isIndicator noise ind1 ind2 ws
  = tryInd1 (removeWords noise ws)
  where
    tryInd1 [] 
      = False
    tryInd1 ws' 
      = tryInd2 (removeWords' ws' ind1 ind2)
    tryInd2 (ws', ind2') 
      = null ws' || elem ws' ind2'

stemInd1 ind1 noise
  = removeWords noise (stemAll ind1)

stemInd2 ind2 noise
  =  map (stemAll . removeWords noise . words) ind2

defNoiseStemmed
  = stemAll defNoise
defRNoiseStemmed
  = stemAll defRNoise
defInd1Stemmed
  = stemInd1 defInd1 defNoiseStemmed
defInd1RStemmed
  = stemInd1 defInd1R defRNoiseStemmed
defInd2Stemmed
  = stemInd2 defInd2 defNoiseStemmed

anagramNoiseStemmed
  = stemAll anagramNoise
anagramInd1Stemmed
  = stemInd1 anagramInd1 anagramNoiseStemmed
anagramInd2Stemmed
  = stemInd2 anagramInd2 anagramNoiseStemmed

anagramInfNoiseStemmed
  = stemAll anagramInfNoise
anagramInfInd1Stemmed
  = stemInd1 anagramInfInd1 anagramInfNoiseStemmed
anagramInfInd2Stemmed
  = stemInd2 anagramInfInd2 anagramInfNoiseStemmed

abbreviationsNoiseStemmed
  = stemAll abbreviationsNoise
abbreviationsInd1Stemmed
  = stemInd1 abbreviationsInd1 abbreviationsNoiseStemmed
abbreviationsInd2Stemmed
  = stemInd2 abbreviationsInd2 abbreviationsNoiseStemmed

oddsNoiseStemmed
  = stemAll oddsNoise
oddsInd1Stemmed
  = stemInd1 oddsInd1 oddsNoiseStemmed
oddsInd2Stemmed
  = stemInd2 oddsInd2 oddsNoiseStemmed

evensNoiseStemmed
  = stemAll evensNoise
evensInd1Stemmed
  = stemInd1 evensInd1 evensNoiseStemmed
evensInd2Stemmed
  = stemInd2 evensInd2 evensNoiseStemmed

hyponymNoiseStemmed
  = stemAll hyponymNoise
hyponymInd1Stemmed
  = stemInd1 hyponymInd1 hyponymNoiseStemmed
hyponymInd2Stemmed
  = stemInd2 hyponymInd2 hyponymNoiseStemmed

rotationNoiseStemmed
  = stemAll rotationNoise
rotationInd1Stemmed
  = stemInd1 rotationInd1 rotationNoiseStemmed
rotationInd2Stemmed
  = stemInd2 rotationInd2 rotationNoiseStemmed

reversalNoiseStemmed
  = stemAll reversalNoise
reversalInd1Stemmed
  = stemInd1 reversalInd1 reversalNoiseStemmed
reversalInd2Stemmed
  = stemInd2 reversalInd2 reversalNoiseStemmed

firstLettersNoiseStemmed
  = stemAll firstLettersNoise
firstLettersInd1Stemmed
  = stemInd1 firstLettersInd1 firstLettersNoiseStemmed
firstLettersInd2Stemmed
  = stemInd2 firstLettersInd2 firstLettersNoiseStemmed

lastLettersNoiseStemmed
  = stemAll lastLettersNoise
lastLettersInd1Stemmed
  = stemInd1 lastLettersInd1 lastLettersNoiseStemmed
lastLettersInd2Stemmed
  = stemInd2 lastLettersInd2 lastLettersNoiseStemmed

endLettersNoiseStemmed
  = stemAll endLettersNoise
endLettersInd1Stemmed
  = stemInd1 endLettersInd1 endLettersNoiseStemmed
endLettersInd2Stemmed
  = stemInd2 endLettersInd2 endLettersNoiseStemmed

middleLettersNoiseStemmed
  = stemAll middleLettersNoise
middleLettersInd1Stemmed
  = stemInd1 middleLettersInd1 middleLettersNoiseStemmed
middleLettersInd2Stemmed
  = stemInd2 middleLettersInd2 middleLettersNoiseStemmed

duplicateNoiseStemmed
  = stemAll duplicateNoise
duplicateInd1Stemmed
  = stemInd1 duplicateInd1 duplicateNoiseStemmed
duplicateInd2Stemmed
  = stemInd2 duplicateInd2 duplicateNoiseStemmed

homophoneNoiseStemmed
  = stemAll homophoneNoise
homophoneInd1Stemmed
  = stemInd1 homophoneInd1 homophoneNoiseStemmed
homophoneInd2Stemmed
  = stemInd2 homophoneInd2 homophoneNoiseStemmed

insertionWordsNoiseStemmed
  = stemAll insertionWordsNoise
insertionWordsInd1Stemmed
  = stemInd1 insertionWordsInd1 insertionWordsNoiseStemmed
surroundingWordsNoiseStemmed
  = stemAll surroundingWordsNoise
surroundingWordsInd1Stemmed
  = stemInd1 surroundingWordsInd1 surroundingWordsNoiseStemmed
insertionInd2L1Stemmed
  = stemInd2 insertionInd2L1 surroundingWordsNoiseStemmed
insertionInd2L2Stemmed
  = stemInd2 insertionInd2L2 insertionWordsNoiseStemmed
insertionInd2C1Stemmed
  = stemInd2 insertionInd2C1 insertionWordsNoiseStemmed
insertionInd2C2Stemmed
  = stemInd2 insertionInd2C2 surroundingWordsNoiseStemmed
insertionInd2R1Stemmed
  = stemInd2 insertionInd2R1 surroundingWordsNoiseStemmed
insertionInd2R2Stemmed
  = stemInd2 insertionInd2R2 insertionWordsNoiseStemmed

leavingWordsNoiseStemmed
  = stemAll leavingWordsNoise
leavingWordsInd1Stemmed
  = stemInd1 leavingWordsInd1 leavingWordsNoiseStemmed
subtractionWordsNoiseStemmed
  = stemAll subtractionWordsNoise
subtractionWordsInd1Stemmed
  = stemInd1 subtractionWordsInd1 subtractionWordsNoiseStemmed
subtractionInd2L1Stemmed
  = stemInd2 subtractionInd2L1 subtractionWordsNoiseStemmed
subtractionInd2L2Stemmed
  = stemInd2 subtractionInd2L2 leavingWordsNoiseStemmed
subtractionInd2C1Stemmed
  = stemInd2 subtractionInd2C1 leavingWordsNoiseStemmed
subtractionInd2C2Stemmed
  = stemInd2 subtractionInd2C2 subtractionWordsNoiseStemmed
subtractionInd2R1Stemmed
  = stemInd2 subtractionInd2R1 leavingWordsNoiseStemmed
subtractionInd2R2Stemmed
  = stemInd2 subtractionInd2R2 subtractionWordsNoiseStemmed

beforeWordsNoiseStemmed
  = stemAll beforeWordsNoise
beforeWordsInd1Stemmed
  = stemAll beforeWordsInd1
afterWordsNoiseStemmed
  = stemAll afterWordsNoise
afterWordsInd1Stemmed
  = stemAll afterWordsInd1
charadeInd2L1Stemmed
  = stemInd2 charadeInd2L1 afterWordsNoiseStemmed
charadeInd2L2Stemmed
  = stemInd2 charadeInd2L2 beforeWordsNoiseStemmed
charadeInd2C1Stemmed
  = stemInd2 charadeInd2C1 beforeWordsNoiseStemmed
charadeInd2C2Stemmed
  = stemInd2 charadeInd2C2 afterWordsNoiseStemmed
charadeInd2R1Stemmed
  = stemInd2 charadeInd2R1 afterWordsNoiseStemmed
charadeInd2R2Stemmed
  = stemInd2 charadeInd2R2 beforeWordsNoiseStemmed
--
-- E.g. dropping guts, losing the heart
--
isCompoundIndicator p1 p2 text
  = or [p1 s1 && p2 s2 | (s1, s2) <- split2' text]
    
--
-- TO DO... Build compound indicator predicates to handle, e.g.
-- case dismissed, ripping out insides etc.
--
-- This is used to build and cache all the indicator predicate
-- results once the clue text is known.
-- Note that the same text can appear in many parse trees, 
-- so these have to be cached independently of those trees.
-- E.g. X mixing Y in Z where we have at least trees for
-- (X mixing) Y in Z and X (mixing Y in Z). They independently
-- ask what mixing might mean.
--
buildIndicatorPredicateSet :: [String] -> IndicatorPredicateSet
buildIndicatorPredicateSet stemmedText 
  = listArray (minBound, maxBound)
      (map ($ stemmedText) predicateList)
  where
    --
    -- We pick this out as it is used also to build
    -- the end letters predicate
    --
    subtractionL1Predicate
      = isIndicator subtractionWordsNoiseStemmed
                     subtractionWordsInd1Stemmed
                     subtractionInd2L1Stemmed
    firstLettersPredicate
      = isIndicator firstLettersNoiseStemmed
                     firstLettersInd1Stemmed
                     firstLettersInd2Stemmed
    lastLettersPredicate
      = isIndicator lastLettersNoiseStemmed
                     lastLettersInd1Stemmed
                     lastLettersInd2Stemmed
    middleLettersPredicate
      = isIndicator middleLettersNoiseStemmed
                     middleLettersInd1Stemmed
                     middleLettersInd2Stemmed
    endLettersPredicate text
      = isIndicator endLettersNoiseStemmed
                     endLettersInd1Stemmed
                     endLettersInd2Stemmed text
    predicateList
      = [isIndicator defNoiseStemmed
                      defInd1Stemmed
                      defInd2Stemmed,

         isIndicator defRNoiseStemmed
                      defInd1RStemmed
                      defInd2Stemmed,

         isIndicator hyponymNoiseStemmed
                      hyponymInd1Stemmed
                      hyponymInd2Stemmed,

         isIndicator abbreviationsNoiseStemmed
                      abbreviationsInd1Stemmed
                      abbreviationsInd2Stemmed,

         isIndicator anagramNoiseStemmed
                      anagramInd1Stemmed
                      anagramInd2Stemmed,

         isIndicator anagramInfNoiseStemmed
                      anagramInfInd1Stemmed
                      anagramInfInd2Stemmed,

         isIndicator oddsNoiseStemmed
                      oddsInd1Stemmed
                      oddsInd2Stemmed,

         isIndicator evensNoiseStemmed
                      evensInd1Stemmed
                      evensInd2Stemmed,

         \t -> firstLettersPredicate t ||
               isCompoundIndicator subtractionL1Predicate
                                   lastLettersPredicate
                                   t,

         \t -> lastLettersPredicate t ||
               isCompoundIndicator subtractionL1Predicate
                                   firstLettersPredicate
                                   t,

         \t -> middleLettersPredicate t ||
               isCompoundIndicator subtractionL1Predicate
                                   endLettersPredicate
                                   t,

         \t -> endLettersPredicate t ||
               isCompoundIndicator subtractionL1Predicate
                                   middleLettersPredicate
                                   t,

         isIndicator duplicateNoiseStemmed
                      duplicateInd1Stemmed
                      duplicateInd2Stemmed,

         isIndicator homophoneNoiseStemmed
                      homophoneInd1Stemmed
                      homophoneInd2Stemmed,

         isIndicator rotationNoiseStemmed
                      rotationInd1Stemmed
                      rotationInd2Stemmed,

         isIndicator reversalNoiseStemmed
                      reversalInd1Stemmed
                      reversalInd2Stemmed,

         isIndicator surroundingWordsNoiseStemmed
                      surroundingWordsInd1Stemmed
                      insertionInd2L1Stemmed,
         isIndicator insertionWordsNoiseStemmed
                      insertionWordsInd1Stemmed
                      insertionInd2L2Stemmed,
         isIndicator insertionWordsNoiseStemmed
                      insertionWordsInd1Stemmed
                      insertionInd2C1Stemmed,
         isIndicator surroundingWordsNoiseStemmed
                      surroundingWordsInd1Stemmed
                      insertionInd2C2Stemmed,
         isIndicator surroundingWordsNoiseStemmed
                      surroundingWordsInd1Stemmed
                      insertionInd2R1Stemmed,
         isIndicator insertionWordsNoiseStemmed
                      insertionWordsInd1Stemmed
                      insertionInd2R2Stemmed,
         -- Extras...
         -- Around
         isIndicator surroundingWordsNoiseStemmed
                     []
                     (stemInd2 surroundingWordsInd1 []),
         -- Into
         isIndicator insertionWordsNoiseStemmed
                     []
                     (stemInd2 insertionWordsInd1 []),
         -- With
         isIndicator []
                     []
                     (stemInd2 with []),
         -- WithHas
         isIndicator []
                     []
                     (stemInd2 withHas []),
         -- Locate
         isIndicator insertionWordsNoiseStemmed
                     []
                     (stemInd2 locationWords []),
         -- Inject
         isIndicator insertionWordsNoiseStemmed
                     []
                     (stemInd2 injectionWords []),
         -- Surround
         isIndicator surroundingWordsNoiseStemmed
                     []
                     (stemInd2 envelopingWords []),

         subtractionL1Predicate,
         isIndicator leavingWordsNoiseStemmed
                      leavingWordsInd1Stemmed
                      subtractionInd2L2Stemmed,
         isIndicator leavingWordsNoiseStemmed
                      leavingWordsInd1Stemmed
                      subtractionInd2C1Stemmed,
         isIndicator subtractionWordsNoiseStemmed
                      subtractionWordsInd1Stemmed
                      subtractionInd2C2Stemmed,
         isIndicator subtractionWordsNoiseStemmed
                      subtractionWordsInd1Stemmed
                      subtractionInd2R1Stemmed,
         isIndicator subtractionWordsNoiseStemmed
                      subtractionWordsInd1Stemmed
                      subtractionInd2R2Stemmed,

         isIndicator afterWordsNoiseStemmed
                      afterWordsInd1Stemmed
                      charadeInd2L1Stemmed,
         isIndicator beforeWordsNoiseStemmed
                      beforeWordsInd1Stemmed
                      charadeInd2L2Stemmed,
         isIndicator beforeWordsNoiseStemmed
                      beforeWordsInd1Stemmed
                      charadeInd2C1Stemmed,
         isIndicator afterWordsNoiseStemmed
                      afterWordsInd1Stemmed
                      charadeInd2C2Stemmed,
         isIndicator afterWordsNoiseStemmed
                      afterWordsInd1Stemmed
                      charadeInd2R1Stemmed,
         isIndicator beforeWordsNoiseStemmed
                      beforeWordsInd1Stemmed
                      charadeInd2R2Stemmed
        ]
