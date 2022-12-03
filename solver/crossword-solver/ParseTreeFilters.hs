module ParseTreeFilters where

import Types
import CacheFunctions
import Constants
import Indicators

--
-- Contains prdedicates used to filter parse trees during
-- clue parsing. The parserCache stores all parse trees for
-- all indices and each parsing rule picks out the required
-- cases using these filters.
--

anyOf :: [a -> Bool] -> a -> Bool
anyOf ps t
  = or [p t | p <- ps]

allBut :: [a -> Bool] -> a -> Bool
allBut ps t
  = not (anyOf ps t)

--
-- Tree type predicates...
--
isSynonymTree (_, (Synonym _, _))
  = True
isSynonymTree _
  = False

isHyponymTree :: ParseTree -> Bool
isHyponymTree  (_, (Hyponym _ _, _))
  = True
isHyponymTree _
  = False

isAbbreviationTree :: ParseTree -> Bool
isAbbreviationTree (_, (Abbreviation _ _, _))
  = True
isAbbreviationTree _
  = False

isAnagramTree :: ParseTree -> Bool
isAnagramTree  (_, (Anagram _ _, _))
  = True
isAnagramTree _
  = False

isAnagramInfTree :: ParseTree -> Bool
isAnagramInfTree  (_, (AnagramInf _ _ _, _))
  = True
isAnagramInfTree _
  = False

isOddsTree :: ParseTree -> Bool
isOddsTree  (_, (Odds _ _, _))
  = True
isOddsTree _
  = False

isEvensTree :: ParseTree -> Bool
isEvensTree  (_, (Evens _ _, _))
  = True
isEvensTree _
  = False

isFirstLettersTree :: ParseTree -> Bool
isFirstLettersTree  (_, (FirstLetters _ _, _))
  = True
isFirstLettersTree _
  = False

isLastLettersTree :: ParseTree -> Bool
isLastLettersTree  (_, (LastLetters _ _, _))
  = True
isLastLettersTree _
  = False

isMiddleLettersTree :: ParseTree -> Bool
isMiddleLettersTree  (_, (MiddleLetters _ _, _))
  = True
isMiddleLettersTree _
  = False

isEndLettersTree :: ParseTree -> Bool
isEndLettersTree  (_, (EndLetters _ _, _))
  = True
isEndLettersTree _
  = False

isSubtextTree :: ParseTree -> Bool
isSubtextTree 
  = anyOf [isFirstLettersTree, isLastLettersTree,
           isMiddleLettersTree, isEndLettersTree]

isDuplicateTree :: ParseTree -> Bool
isDuplicateTree  (_, (Duplicate _ _ _, _))
  = True
isDuplicateTree _
  = False

isHomophoneTree :: ParseTree -> Bool
isHomophoneTree  (_, (Homophone _ _, _))
  = True
isHomophoneTree _
  = False

isReversalTree :: ParseTree -> Bool
isReversalTree  (_, (Reversal _ _, _))
  = True
isReversalTree _
  = False

isInsertionTree :: ParseTree -> Bool
isInsertionTree  (_, (Insertion _ _ _ _, _))
  = True
isInsertionTree _
  = False

isSubtractionTree :: ParseTree -> Bool
isSubtractionTree  (_, (Subtraction _ _ _, _))
  = True
isSubtractionTree _
  = False

isCharadeTree :: ParseTree -> Bool
isCharadeTree  (_, (Charade _ _, _))
  = True
isCharadeTree _
  = False

-- 
-- The filters...
--

applyFilter :: (ParseTree -> Bool) -> ParserCache -> Index -> [ParseTree]
applyFilter p pCache index
  = filter p (getAllTrees pCache index)

--
-- Charades of text/abbreviations/subtext/duplicates are supported through
-- makeCharades. For this reason you should only parse single-word 
-- text here, otherwise you can get two equivalent 2-word text 
-- parses, e.g. one from w1 w2 directly and one from a charade of w1 and w2. 
-- So, don't pick up *all* Text nodes - let charades do the work.
--
-- NOTE: Literal subtext (First, Last, Middle, End) is allowed in order
-- for clues like Harry Potter to work - here you need "not initially" 
-- to mean "n".
--
-- Indicated charades are also allowed, as in ("mix ac with tr vehicle", 4) 
-- where "ac with tr" builds a labelled charade node.
--
-- Subtext cases all have synonym subtrees, so there's no need to
-- check the subtree type. They may also include reversals, but 
-- we don't check for that. ANAG (SUBTEXT (REV ...)) is therefore
-- possible, but unlikely.
--
-- Subtractions under an anagram are restricted to synonyms and 
-- charades thereof.
--

--
-- A useful function to identify arbitrary charades of synonyms.
-- It's used in anagrams and subtractions...
--
isSynonymCharadeTree :: ParseTree -> Bool
isSynonymCharadeTree (_, (Charade _ ts, _))
  = all isSynonymCharadeTree ts
isSynonymCharadeTree t
  = anyOf [isSynonymTree, isHyponymTree] t

--
-- Anagrams can be applied to 'basic' constructions involving
-- text, abbreviations, subtext and subtractions involving synonym
-- charades.
--
isAnagramArgTree :: ParseTree -> Bool
isAnagramArgTree (_, (Subtraction _ t t', _))
  = isSynonymCharadeTree t && isSynonymCharadeTree t'
isAnagramArgTree (_, (Charade _ ts, _))
  | all isAnagramArgTree  ts
  = True
isAnagramArgTree t
  = anyOf [isSynonymTree, isAbbreviationTree, isSubtextTree,
           isDuplicateTree] t

getAnagramArgTrees :: ParserCache -> Index -> [ParseTree]
getAnagramArgTrees pCache index 
  = applyFilter isAnagramArgTree pCache index

-- 
-- For subtext we want:
--   1. Synonyms - these will be evaluated as both raw text 
--      and synonyms; the latter is subject to strict
--      length constraints - see the evaluation rules.
--      If the text spans more than one word, e.g. cash
--      register, we must include it if it has a synonym,
--      e.g. till. 
--   2. Charades of single-word text, e.g. for "rich people" 
--      we want Synonym "rich" + Synonym "people".
--   3. Reversals of  *charades* of the above - you don't reverse
--      a synonym and then take out; you take out and then 
--      reverse.
--   4. Top-level examples. These will only evaluate to a 
--      synonym, e.g. hilary maybe -> term, so the raw text 
--      string will be "" (see the evaluator). Examples 
--      can not appear under a charade or reversal.
--
isSubtextArgTree :: ParserCache -> ParseTree -> Bool
isSubtextArgTree _ (_, (Hyponym _ _, _))
  = True
isSubtextArgTree pCache (_, (Reversal _ t, _))
  = isSubtextReversalArg pCache t
isSubtextArgTree pCache t
  = isSubtextArgTree' pCache t

isSubtextArgTree' :: ParserCache -> ParseTree -> Bool
isSubtextArgTree' pCache (_, (SpareWord sw i, _))
  = True
isSubtextArgTree' pCache (_, (Synonym index@(m, n), _))
  = m == n || not (isEmpty (getSynonymTable pCache index))
isSubtextArgTree' pCache (_, (Charade _ ts, _))
  = all (isSubtextArgTree' pCache) ts
isSubtextArgTree' _ _
  = False

isSubtextReversalArg :: ParserCache -> ParseTree -> Bool
isSubtextReversalArg pCache (_, (Charade _ ts, _))
  = all (isSubtextArgTree' pCache) ts
isSubtextReversalArg _ _
  = False

getSubtextArgTrees :: ParserCache -> Index -> [ParseTree]
getSubtextArgTrees pCache index
  = applyFilter (isSubtextArgTree pCache) pCache index

getDuplicateArgTrees :: ParserCache -> Index -> [ParseTree]
getDuplicateArgTrees 
  = applyFilter (anyOf [isSynonymTree, 
                        isHyponymTree]) 

getHomophoneArgTrees :: ParserCache -> Index -> [ParseTree]
getHomophoneArgTrees 
  = getSynonymTrees

getRotationArgTrees :: ParserCache -> Index -> [ParseTree]
getRotationArgTrees 
  = getSynonymTrees

getReversalArgTrees :: ParserCache -> Index -> [ParseTree]
getReversalArgTrees pCache index
  = applyFilter (allBut [isAnagramTree, 
                         isAnagramInfTree, 
                         isReversalTree]) pCache index

--
-- Anything can be inserted but the target excludes obvious
-- things like anagrams and other (nested) insertions. In
-- particular (X in Y) in Z is allowed (but will never be seen!)
-- but X in (Y in Z) is not. The former (if ever seen) 
-- will generate the same solutions as the latter, so this is
-- a useful optimisation.
-- X in Y in Z will be taken as (X in Y) in Z, so we disallow
-- insertions in the second argument.
-- Inserting into a charade is (currently) allowed, as
-- there may be the odd case where it is useful, e.g. everyman
-- 1041. But is arguably a daft construction.
--
getInsertionArg1Trees :: ParserCache -> Index -> [ParseTree]
getInsertionArg1Trees pCache index
  = applyFilter (const True) pCache index

getInsertionArg2Trees :: ParserCache -> Index -> [ParseTree]
getInsertionArg2Trees pCache index
  = applyFilter (allBut [isInsertionTree]) pCache index

--
-- The charade leaves test also counts nested *indicated*
-- charades, which may sit under t, e.g. in binary tree form.
-- Update: This version disallows subtractions from charades,
-- similar to insertions.
--
isSubtractionArg2Tree :: ParseTree -> Bool
{-
isSubtractionArg2Tree t@(_, (Charade _ ts, _))
  | numCharadeLeaves t > maxNumSubtractionSubtrees 
  = False
  | all isSubtractionArg2Tree' ts
  = True
  where
    numCharadeLeaves (_, (Charade _ ts, _))
      = sum (map numCharadeLeaves ts)
    numCharadeLeaves _
      = 1
-}
isSubtractionArg2Tree t
  = isSubtractionArg2Tree' t

isSubtractionArg2Tree' :: ParseTree -> Bool
{-
isSubtractionArg2Tree' t@(_, (Charade _ ts, _))
  | all isSubtractionArg2Tree' ts
  = True
-}
isSubtractionArg2Tree' t
  = anyOf [isSynonymTree, isHyponymTree] t

--
-- We don't want anagrams, reversals etc. under subtractions
-- as we want those operators to sit at the top of the tree.
-- E.g. "mix a man without energy" should be ANAG (SUB ...) not
-- SUB (SYN energy) (ANAG ...).
--
getSubtractionArg1Trees :: ParserCache -> Index -> [ParseTree]
getSubtractionArg1Trees pCache index
  = applyFilter (anyOf [isSynonymCharadeTree, 
                        isAbbreviationTree, 
                        isOddsTree,
                        isEvensTree,
                        isDuplicateTree,
                        isSubtextTree]) pCache index

-- 
-- Note: changing this to getAllTrees can massively slow down some
-- clues, e.g. 33 unless evaluation works only with TextSyns.
-- Doing that means that clue 37 can't be solved.
-- Using both AllTrees and evaluating with AllSyns is a bad
-- combination.
--
getSubtractionArg2Trees :: ParserCache -> Index -> [ParseTree]
getSubtractionArg2Trees pCache index
  = applyFilter isSubtractionArg2Tree pCache index

--
-- These are used to parse charades whilst avoiding multiple trees
-- with the same meaning (see parsing rules)...
--
isUnindicatedCharadeTree :: ParseTree -> Bool
isUnindicatedCharadeTree (_, (Charade ind _, _))
  = ind == noInd
isUnindicatedCharadeTree _
  = False

isIndicatedCharadeTree :: ParseTree -> Bool
isIndicatedCharadeTree (_, (Charade ind _, _))
  = ind /= noInd
isIndicatedCharadeTree _
  = False

--
-- We can prune all charades to the left of a topmost
-- charade UNLESS the charade flips its arguments. In that
-- case no other combination (parse) will lead to the correct
-- reading, e.g. "one following bishop and doctor round" must
-- involve an indicated charade to the left of an indicated 
-- charade somewhere. Otherwise you can never construct the
-- solution "bimbo". 
-- Otherwise, (X + Y) + Z = X + (Y + Z) and the former can 
-- be discarded.
-- Note: for the clue ...he volunteers after short time, you
-- end up with short time +[after] (he + volunteers) and you 
-- have to retain the rightmost tree. So you can prune anything
-- to the right of a +.
--
getIndicatedCharadeArg1Trees :: ParserCache -> Index -> [ParseTree]
getIndicatedCharadeArg1Trees pCache index
  = applyFilter p pCache index
  where
    p (_, (Charade ind _, _))
      | ind == noInd = False
      | otherwise    = elem indStr afterWords
      where
        indStr = getString pCache ind
    p _
      = True

getIndicatedCharadeArg2Trees :: ParserCache -> Index -> [ParseTree]
getIndicatedCharadeArg2Trees pCache index
  = applyFilter (const True) pCache index

getNonCharadeTrees :: ParserCache -> Index -> [ParseTree]
getNonCharadeTrees pCache index 
  = applyFilter (not . isUnindicatedCharadeTree) pCache index

--
-- Used to parse definitions
--
getSynonymTrees :: ParserCache -> Index -> [ParseTree]
getSynonymTrees pCache index
  = applyFilter isSynonymTree pCache index

getHyponymTrees :: ParserCache -> Index -> [ParseTree]
getHyponymTrees pCache index
  = applyFilter isHyponymTree pCache index


