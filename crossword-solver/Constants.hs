module Constants where
 
import Types

--
-- Used when there is no indicator
--
noInd :: Index
noInd
  = (-1, -1)

--
-- Maximum word length for evaluation
--
maxWordLength :: Int
maxWordLength
  = 16

--
-- Maximum number of words in a definition
--
maxDefLength :: Int
maxDefLength
  = 7

--
-- Maximum number of active words in definition when the answer
-- is given
--
maxNoOfActiveWords :: Int
maxNoOfActiveWords
  = 3

--
-- Maximum number of words in an indicator
--
maxIndicatorWords :: Int
maxIndicatorWords
  = 5

--
-- Maximum depth of parse tree
--
maxTreeDepth :: Int
maxTreeDepth
  = 4

--
-- Maximum number of subtrees. Allows up to 4-letter
-- take-outs. Should be enough...
--
maxNumCharadeSubtrees :: Int
maxNumCharadeSubtrees
  = 4

--
-- Maximum number of subtrees under a subtraction charade
-- (second argument).
--
maxNumSubtractionSubtrees :: Int
maxNumSubtractionSubtrees
  = 2

--
-- Determines the size of the text you are allowed to remove
-- from a target string.
--
subtractionLengthLimits :: Bounds
subtractionLengthLimits
  = (1, 5)

--
-- You wouldn't make an anagram from "a", for example.
--
minAnagramTextLength :: Int
minAnagramTextLength
  = 2

--
-- For forwards evaluation of anagrams we have to compute the
-- angrams explicitly. This sets a limit on when to cop out.
--
maxLengthForAnagrams :: Int
maxLengthForAnagrams
  = 9

--
-- For subtext rules that span multiple words (essentially a map)
--
maxWordSpan :: Int
maxWordSpan
  = 5

--
-- We allow synonyms under subtext (e.g. prefix), but they 
-- must have a minimum size and with only up to a specified number
-- discarded. len is the length of the target string (candidate
-- solution fragment). m and n are the synonym length bounds.
-- E.g. tOy, RESt, OFFendERS, but not LItigatiON.
-- This avoids stupid solutions like leading light producing I
-- from Illumination etc.
-- We eliminate long words completely, in part to avoid
-- strange solutions with indirect takeouts, e.g. 
-- part stretchiness -> part ELASTICATion -> ELASTICAT. 
-- But something like first attempts -> TRIE seems OK.
--
getSubtextSynLengthBounds :: Int -> Bounds
getSubtextSynLengthBounds len 
  | len <= 7  = (len + 1, 2 * len)
  | otherwise = (0, 0)

--
-- Maximum number of subtrees that we can synonym-expand under
-- subtext. Otherwise it gets too expensive.
--
maxNumSubtextSubtrees :: Int
maxNumSubtextSubtrees
  = 2

-- 
-- Middle letters must return a certain fraction of the target word.
-- len is the length of the word, or its maximum.
--
minMiddleLetterLength :: Int -> Int
minMiddleLetterLength len
  = len `div` 3

--
-- This variant is used when the string is bang in the middle
-- of the target text, e.g. matches p memphis -> True
--
minMiddleLetterLength' :: Int -> Int
minMiddleLetterLength' len
  = len `div` 4

